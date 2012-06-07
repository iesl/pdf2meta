package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import com.weiglewilczek.slf4s.Logging
import scala._
import collection.mutable.Buffer
import util.Random
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import scala.Predef._
import edu.umass.cs.iesl.scalacommons.collections._

object WhitespaceDocPartitioner
	{
	val MIN_QUALITY = 3000
	//5000
	val MIN_HEIGHT  = 10
	val MIN_WIDTH   = 10

	val MIN_AREA        = 3000
	val MAX_WHITESPACES = 12

	//type PRect = (Double, Rectangle, Set[Rectangle], Int)
	final class PRect(val quality: Double, val rect: Rectangle, val obstacles: Set[Rectangle], val generation: Int)
		{
		// make equality depend only on the rectangle, so that there is only one representative at a time in the priority queue.
		override def equals(other: Any): Boolean = other match
		{
			case that: PRect => this.rect == that.rect
			case _ => false
		}

		override def hashCode: Int = rect.hashCode

		override def toString() = quality + "(" + generation + ") : " + obstacles.size + " : " + rect

		val remainingArea = rect.area - obstacles.map(_.area).sum // assume that the obstacles have no portion outside of x,
		}
	//  quality, rectangle, obstacles, generation
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
		//val obstacles: mutable.Set[Rectangle] = mutable.Set() ++ leaves.map(_.rectangle.get)
		//var area = Double.MaxValue
		//val qualityFunction: (Rectangle, Set[Rectangle]) => Double = (x: Rectangle, subobstacles: Set[Rectangle]) => x.area //** could consider aspect ratio
		// etc. here
		// ?? this turns out to be horribly slow (probably due to considering nodes in the wrong order and pruning too little) ??
		def qualityFunction(x: Rectangle, obstacles: Set[Rectangle]) =
			{
			val remainingArea = x.area - obstacles.map(_.area).sum // assume that the obstacles have no portion outside of x,
			// and that they don't overlap with each other

			if (x.height < MIN_HEIGHT || x.width < MIN_WIDTH || remainingArea < MIN_AREA) 0.0f
			else
				{
				// ** could consider aspect ratio etc. here.  Do we prefer square?  etc...
				// or just consider the longest boundary
				// scala.math.max(x.height, x.width)


				// or combine them (very fuzzy manual heuristic)
				// OK, try the sum of the dimensions of a rectangle of the same length,
				// but of the width that results from excluding the area due to obstacles
				//val maxDim = scala.math.max(x.height, x.width)
				//val effectiveMinDim = remainingArea / maxDim
				//maxDim + effectiveMinDim

				remainingArea
				}
			}


		val pivotPicker: (Set[Rectangle]) => Rectangle = (s: Set[Rectangle]) => s.toSeq(Random.nextInt(s.size)) //perf?
		//def pivotPicker(s: Set[Rectangle]) = s.map(x => (x, x.area)).toSeq.sortBy(_._2).head._1 //perf; pick largest
		def configuredFindWhitespace(bound: Rectangle, allObstacles: Set[Rectangle]) = find_whitespaces(qualityFunction, pivotPicker)(bound, allObstacles)


		// restarting the algorithm from scratch is too slow.  Instead, reuse the queue as Breuel suggests.
		val max_whitespaces: Seq[Rectangle] = configuredFindWhitespace(page.rectangle.get, leaves.map(_.rectangle.get).toSet)
		val whitespaceNodes = max_whitespaces.zipWithIndex.map((z: (Rectangle, Int)) => z match
		{
			case (x: Rectangle, i: Int) =>
				{
				//area = x.area
				//obstacles += x
				val rr = RectangleOnPage(page.page, x).get

				new WhitespaceBox(page.id + "w" + i, rr)
				}
		})


		// now whitespaceNodes contains the selected WhitespaceBoxes, but these don't actually partition anything yet.
		// but it doesn't matter because the reading order algorithm can take it from here.

		page.create(leaves ++ whitespaceNodes)
		}

	// originally a direct translation of Breuel's pseudocode, but evolved a bit
	def find_whitespaces(quality: (Rectangle, Set[Rectangle]) => Double, pick: Set[Rectangle] => Rectangle)
	                    (bound: Rectangle, baseObstacles: Set[Rectangle]): Seq[Rectangle] =
		{

		val nonOverlappingObstacles = Rectangle.decomposeToNonOverlapping(baseObstacles)

		val ord: Ordering[PRect] = new Ordering[PRect]
			{
			def compare(x: PRect, y: PRect) =
				{
				x.quality.compare(y.quality)

				//quality(x).compare(quality(y))
				/*
			 x._1.compare(y._1) match
			 {
				 case 0 => 0 // ** when areas are equal, could prefer portrait/landscape, or higher/lefter, etc.
				 case c => c
			 }
			 */
				}
			}

		val queue: PrioritySet[PRect] = new PrioritySet[PRect](ord)
		queue.enqueue(new PRect(quality(bound, nonOverlappingObstacles), bound, nonOverlappingObstacles, 0))

		val whitespaces: Buffer[Rectangle] = Buffer[Rectangle]()

		while (!queue.isEmpty && whitespaces.length < MAX_WHITESPACES)
			{
			val p = queue.dequeue()
			assert(!queue.toSet.exists(_.quality > p.quality))

			val currentGeneration: Int = whitespaces.length

			def processRect(prect: PRect)
				{
				if (prect.obstacles.isEmpty)
					whitespaces += prect.rect
				else
					{
					val r = prect.rect

					val pivot = pick(prect.obstacles).intersection(r).get // the pivot may extend beyond the bounds, in which case just use the inside part
					assert(pivot.area > 0)
					val r0 = Rectangle(pivot.right, r.bottom, r.right, r.top)
					val r1 = Rectangle(r.left, r.bottom, pivot.left, r.top)
					val r2 = Rectangle(r.left, pivot.top, r.right, r.top)
					val r3 = Rectangle(r.left, r.bottom, r.right, pivot.bottom)

					// drop empty rectangles
					val nonEmptySubrectangles = Set(r0, r1, r2, r3).flatten

					// drop any subrectangles that exactly match an obstacle
					val subrectangles = nonEmptySubrectangles.filterNot(s => prect.obstacles.exists(o => o.fullyContains(s)))

					for (subR <- subrectangles)
						{
						//val sub_obstacles = obstacles.filter(_.overlaps(sub_r)) // "not overlaps" in Breuel's pseudocode is a bug
						val subObstacles = prect.obstacles.flatMap(_.intersection(subR)) // limit to portion within sub_r
						val subQ = quality(subR, subObstacles)
						if (subQ >= MIN_QUALITY)
							{
							//assert(!sub_obstacles.contains(pivot)) // sanity check
							assert(!subObstacles.contains(subR)) //** is equality a problem here?

							queue.enqueue(new PRect(subQ, subR, subObstacles, currentGeneration))
							}
						}
					}
				}

			if (p.generation < currentGeneration) // whitespaces have been added, recompute
				{
				//val newObstacles = nonOverlappingObstacles.filter(_.overlaps(r)) ++ whitespaces.filter(_.overlaps(r))
				val newWhitespaces = whitespaces.drop(p.generation)
				val newObstacles: Buffer[Rectangle] = newWhitespaces.flatMap(_.intersection(p.rect))
				if (newObstacles.isEmpty)
					{
					// the score was OK as is, so this really is the top of the queue
					processRect(p)
					}
				else
					{
					//perf
					for (i <- p.obstacles; j <- newObstacles if j != i)
						{
						assert(!i.overlaps(j))
						}
					val allObstacles = p.obstacles ++ newObstacles

					val newQ: Double = quality(p.rect, allObstacles)


					if (newQ == p.quality)
						{
						// the score was OK as is, so this really is the top of the queue
						val newPrect: PRect = new PRect(newQ, p.rect, allObstacles, currentGeneration)
						processRect(newPrect)
						}
					else if (newQ >= MIN_QUALITY)
						{
						val newPrect: PRect = new PRect(newQ, p.rect, allObstacles, currentGeneration)
						queue.enqueue(newPrect)
						}
					}
				}
			else // score is legit
				{
				processRect(p)
				}
			}
		//throw new DocTransformerException("Impossible?  Found no maximal whitespace")
		whitespaces.toSeq
		}
	}

// originally a direct translation of Breuel's pseudocode, but evolved a bit
/*	def find_whitespaces(quality: (Rectangle, Set[Rectangle]) => Double, pick: Set[Rectangle] => Rectangle)
	                    (bound: Rectangle, baseObstacles: Set[Rectangle]): Seq[Rectangle] =
		{

		/**
		 * given a set of nonoverlapping rectangles and one new rectangle which may overlap the others, return a new set of nonoverlapping rectangles tiling
		 * the same space.  In other words, add to the set of nonoverlapping rectangles some new ones covering whatever portions of r were not already
		 * accounted for.
		 *
		 * @param fixed
		 * @param r
		 * @return
		 */
		def makeNonOverlapping(fixed: Set[Rectangle], r: Rectangle): Set[Rectangle] =
			{
			val overlaps = fixed.flatMap(_.intersection(r)).toSeq
			val decomposed: Set[Rectangle] = r - overlaps
			fixed ++ decomposed
			}
		val nonOverlappingObstacles = baseObstacles.foldLeft[Set[Rectangle]](Set())(makeNonOverlapping)

		def conditionalQuality(r: Rectangle, rs: Set[Rectangle]): Option[Double] =
			{
			val y = quality(r, rs)
			if (y >= MIN_QUALITY) Some(y) else None
			}
		val queue: LazyRecomputingPrioritySet[Rectangle, Double] = new LazyRecomputingPrioritySet[Rectangle,
				Double](quality) with ConditionalPrioritySet[Rectangle] {

		}
		queue.enqueue(bound)

		val obstacleMap = collection.mutable.Map[Rectangle, Set[Rectangle]]()
		obstacleMap.put(bound, nonOverlappingObstacles)

		val whitespaces: Buffer[Rectangle] = Buffer[Rectangle]()

		while (!queue.isEmpty)
			{
			val r = queue.dequeue() // this will automatically recompute invalid priorities!
			val obstacles = obstacleMap.remove(r).get // None is impossible

			if (obstacles.isEmpty)
				whitespaces += r
			else
				{
				val pivot = pick(obstacles).intersection(r).get // the pivot may extend beyond the bounds, in which case just use the inside part
				assert(pivot.area > 0)
				val r0 = Rectangle(pivot.right, r.bottom, r.right, r.top)
				val r1 = Rectangle(r.left, r.bottom, pivot.left, r.top)
				val r2 = Rectangle(r.left, pivot.top, r.right, r.top)
				val r3 = Rectangle(r.left, r.bottom, r.right, pivot.bottom)

				// drop empty rectangles
				val nonEmptySubrectangles = Set(r0, r1, r2, r3).flatten

				// drop any subrectangles that exactly match an obstacle
				val subrectangles = nonEmptySubrectangles.filterNot(s => obstacles.exists(o => o.fullyContains(s)))

				for (subR <- subrectangles)
					{
					//val sub_obstacles = obstacles.filter(_.overlaps(sub_r)) // "not overlaps" in Breuel's pseudocode is a bug
					val subObstacles = obstacles.flatMap(_.intersection(subR)) // limit to portion within sub_r

					if (queue.enqueueIfAccepted(subR))
						{
						//assert(!sub_obstacles.contains(pivot)) // sanity check
						assert(!subObstacles.contains(subR)) //** is equality a problem here?
						obstacleMap.put(subR, subObstacles)
						}
					}
				}
			}
		//throw new DocTransformerException("Impossible?  Found no maximal whitespace")
		whitespaces.toSeq
		}
	}*/

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
