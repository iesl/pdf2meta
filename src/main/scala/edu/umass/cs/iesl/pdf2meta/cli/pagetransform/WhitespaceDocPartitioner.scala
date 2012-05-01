package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import scala.util.Random
import scala._
import collection.mutable
import collection.mutable.{Buffer, PriorityQueue}

object WhitespaceDocPartitioner
	{
	val MIN_QUALITY = 4000
	}

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
class WhitespaceDocPartitioner extends DocTransformer with Logging
	{

	import WhitespaceDocPartitioner._

	def apply(doc: DocNode): DocNode =
		{
		val pages = doc.getPages

		val newPages = pages.map(partitionPage)
		doc.create(newPages)
		}

	def partitionPage(page: PageNode): DocNode =
		{
		val leaves = page.leaves

		val obstacles: mutable.Set[Rectangle] = mutable.Set() ++ leaves.map(_.rectangle.get)
		val whitespaceNodes: Buffer[WhitespaceBox] = Buffer[WhitespaceBox]()
		var area = Double.MaxValue



		val qualityFunction: (Rectangle) => Double = (x: Rectangle) => x.area   //** could consider aspect ratio etc. here
		val pivotPicker: (Set[Rectangle]) => Rectangle = (s: Set[Rectangle]) => s.toSeq(Random.nextInt(s.size)) //perf?
		def configuredFindWhitespace(bound: Rectangle, allObstacles: Set[Rectangle]) = find_whitespace(qualityFunction, pivotPicker)(bound, allObstacles)
		//: (Rectangle, Seq[Rectangle]) => Option[Rectangle]
		var isNotDone = true
		// perf for now just restart the algorithm from scratch.  Later, reuse the queue as Breuel suggests.
		while (isNotDone)
			{
			//** for now just choose a random pivot; later could try to get a central one
			// actually why not just take the largest?  That way we prune as fast as possible?
			val max_whitespace: Option[Rectangle] = configuredFindWhitespace(page.rectangle.get, obstacles.toSet)
			isNotDone = max_whitespace.map(x =>
				                               {
				                               area = x.area
				                               obstacles += x

				                               val rr = RectangleOnPage(page.page, x).get

				                               whitespaceNodes += new WhitespaceBox(page.id + "w" + whitespaceNodes.size, rr)
				                               true
				                               }).getOrElse(false)
			}

		// now whitespaceNodes contains the selected WhitespaceBoxes, but these don't actually partition anything yet.
		// but it doesn't matter because the reading order algorithm can take it from here.

		page.create(leaves ++ whitespaceNodes)
		}

	// direct translation of Breuel's pseudocode
	def find_whitespace(quality: Rectangle => Double, pick: Set[Rectangle] => Rectangle)(bound: Rectangle, allObstacles: Set[Rectangle]): Option[Rectangle] =
		{
		type PRect = (Double, Rectangle, Set[Rectangle]) //  quality, rectangle, obstacles
		implicit val ord: Ordering[PRect] = new Ordering[PRect]
				{
				def compare(x: PRect, y: PRect) =
					{
					x._1.compare(y._1) match
					{
						case 0 => 0 //** could prefer portrait/landscape, or higher/lefter, etc.
						case x => x
					}
					}
				}

		val queue: PriorityQueue[PRect] = new PriorityQueue[PRect]
		queue.enqueue((quality(bound), bound, allObstacles))

		while (!queue.isEmpty)
			{
			val (q, r, obstacles) = queue.dequeue()

			if (obstacles.isEmpty)
				{
				assert(allObstacles.filter(_.overlaps(r)).isEmpty)
				return Some(r)
				}
			val pivot = pick(obstacles).intersection(r).get // the pivot may extend beyond the bounds, in which case just use the inside part
			assert(pivot.area > 0)
			val r0 = Rectangle(pivot.right, r.bottom, r.right, r.top)
			val r1 = Rectangle(r.left, r.bottom, pivot.left, r.top)
			val r2 = Rectangle(r.left, pivot.top, r.right, r.top)
			val r3 = Rectangle(r.left, r.bottom, r.right, pivot.bottom)
			val nonEmptySubrectangles = Set(r0, r1, r2, r3).flatten // drop empty rectangles
			// drop any subrectangles that exactly match an obstacle
			//val subrectangles = nonEmptySubrectangles.diff(obstacles)  // can't do it this way because equals isn't defined that way
			val subrectangles = nonEmptySubrectangles.filterNot(s => allObstacles.exists(o => o.fullyContains(s)))

			for (sub_r <- subrectangles)
				{
				val sub_q = quality(sub_r)
				if (sub_q >= MIN_QUALITY) // ** Add MIN_HEIGHT, MIN_WIDTH?
					{
					val sub_obstacles = obstacles.filter(_.overlaps(sub_r)) // "not overlaps" in Breuel's pseudocode is a bug
					//assert(!sub_obstacles.contains(pivot)) // sanity check
					assert(!sub_obstacles.contains(sub_r)) //** is equality a problem here?

					queue.enqueue((sub_q, sub_r, sub_obstacles))
					}
				}
			}
		//throw new DocTransformerException("Impossible?  Found no maximal whitespace")
		None
		}
	}

/*
@INPROCEEDINGS{Breuel02twogeometric,
    author = {Thomas M. Breuel},
    title = {Two Geometric Algorithms for Layout Analysis},
    booktitle = {In Workshop on Document Analysis Systems},
    year = {2002},
    pages = {188--199},
    publisher = {Springer-Verlag}
}

def find_whitespace(bound,rectangles):
    queue.enqueue(quality(bound),bound,rectangles)
    while not queue.is_empty():
        (q,r,obstacles) = queue.dequeue_max()
        if obstacles==[]:
            return r
        pivot = pick(obstacles)
        r0 = (pivot.x1,r.y0,r.x1,r.y1)
        r1 = (r.x0,r.y0,pivot.x0,r.y1)
        r2 = (r.x0,pivot.y1,r.x1,r.y1)
        r3 = (r.x0,r.y0,r.x1,pivot.y0)
        subrectangles = [r0,r1,r2,r3]
        for sub_r in subrectangles:
            sub_q = quality(sub_r)
            sub_obstacles =
                [list of u in obstacles if not overlaps(u,sub_r)]
            queue.enqueue(sub_q,sub_r,sub_obstacles)
            */
