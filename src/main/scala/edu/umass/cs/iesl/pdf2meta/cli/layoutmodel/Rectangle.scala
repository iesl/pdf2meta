package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.pdf2meta.cli.readingorder.OverlapRatios
import scala.Float

object Rectangle extends Logging
	{
	def encompassing(rects: Seq[Rectangle], padding: Int): Option[Rectangle] =
		{
		// PERF
		val realRects = rects.filter(_.isReal)
		if (realRects.isEmpty)
			{
			None
			}
		else
			{
			val bottom = realRects.map(r => r.bottom).min - padding
			val left = realRects.map(r => r.left).min - padding
			val top = realRects.map(r => r.top).max + padding
			val right = realRects.map(r => r.right).max + padding
			Some(new RealRectangle(left, bottom, right, top))
			}
		}

	val BoundingBoxRE = """(.*),(.*),(.*),(.*)""".r

	// doesn't enforce number format
	def apply(coordinates: String): Option[Rectangle] =
		{
		if (coordinates.trim.isEmpty) throw new Error("empty coordinate string")
		else
			{
			lazy val BoundingBoxRE(x0s, y0s, x1s, y1s) = coordinates
			lazy val left = x0s.toFloat;
			lazy val bottom = y0s.toFloat;
			lazy val right = x1s.toFloat;
			lazy val top = y1s.toFloat;
			Rectangle(left, bottom, right, top)
			}
		}

	/**
	 * negative extents are not allowed, but empty rectangles are
	 * @param left
	 * @param bottom
	 * @param right
	 * @param top
	 * @return
	 */
	def apply(left: Float, bottom: Float, right: Float, top: Float): Option[Rectangle] =
		{
		//try
		//{

		if (top < bottom || (right < left)) None
		else Some(new RealRectangle(left, bottom, right, top))
		//}
		//catch
		//{case e: IllegalArgumentException => None}
		}

	/**
	 * given a set of nonoverlapping rectangles and one new rectangle which may overlap the others, return a new set of nonoverlapping rectangles tiling
	 * the same space.  In other words, add to the set of nonoverlapping rectangles some new ones covering whatever portions of r were not already
	 * accounted for.
	 *
	 * @param fixed
	 * @param r
	 * @return
	 */
	private final def addNonOverlappingRegions[T <: Rectangle](fixed: Set[T], r: Rectangle): Set[Rectangle] =
		{
		val overlaps = fixed.flatMap(_.intersection(r)).toSeq
		val decomposed: Set[Rectangle] = r - overlaps
		fixed ++ decomposed
		}

	def decomposeToNonOverlapping[T <: Rectangle](orig: Set[T]): Set[Rectangle] =
		{
		val result = orig.foldLeft[Set[Rectangle]](Set())(addNonOverlappingRegions)
		//perf
		for (i <- result; j <- result if j != i)
			{
			assert(!i.overlaps(j))
			}
		result
		}
	}

trait Rectangle
	{

	/**
	 * A set of nonoverlapping rectangles tiling the space left when the arguments are removed from this.  Strategy: find horizontal intervals containing no
	 * holes, and make full-height rectangles out of those.  If there are none, do the same thing for vertical intervals.  Recurse.
	 * @param holes
	 * @return
	 */
	/*
	def -(holes: Seq[Rectangle]): Set[Rectangle] =
		{
		val h = holes.map(r => (r.left, r.right))
		val nonOverlappingH = FloatIntervals.union(h)
		val horizontalNoHoles = FloatIntervals.invert(nonOverlappingH, left, right)
		val horizontalSolids = horizontalNoHoles.map(_ match
		                      {
			                      case (hl, hr) => Rectangle(hl, bottom, hr, top)
		                      })

		if (horizontalSolids.isEmpty)
			{

			}

		blah blah
		}
		*/
	// that was too much work, just subtract one at a time and fold.

	def -(holes: Seq[Rectangle]): Set[Rectangle] =
		{
		def subtractOneFromAll(bases: Set[Rectangle], hole: Rectangle): Set[Rectangle] =
			{
			bases.flatMap(_ - hole)
			}

		holes.foldLeft(Set(this))(subtractOneFromAll)
		}

	def -(hole: Rectangle): Set[Rectangle] =
		{
		if (!overlaps(hole)) Set(this)
		else
			{
			val above = this.above(hole.top)
			//val middleY = this.below(hole.top).above(hole.bottom)
			val below = this.below(hole.bottom)

			val left = this.leftOf(hole.left)
			val middleX = this.rightOf(hole.left).leftOf(hole.right)
			val right = this.rightOf(hole.right)

			//val aboveLeft = above.intersection(left)
			val aboveMiddle = above.intersection(middleX)
			//val aboveRight = above.intersection(right)
			//val middleLeft = middleY.intersection(left)
			//val middleMiddle = middleY.intersection(middleX)
			//val middleRight = middleY.intersection(right)
			//val belowLeft = below.intersection(left)
			val belowMiddle = below.intersection(middleX)
			//val belowRight = below.intersection(right)
			//Set(aboveLeft,aboveMiddle,aboveRight,middleLeft,middleRight,belowLeft,belowMiddle,belowRight).flatten
			val leftO = if (left.isEmpty) None else Some(left)
			val rightO = if (right.isEmpty) None else Some(right)
			Set(leftO, aboveMiddle, belowMiddle, rightO).flatten
			}
		}

	def topEdge: Rectangle = Rectangle(left, top, right, top).get

	def bottomEdge: Rectangle = Rectangle(left, bottom, right, bottom).get

	def horizontalMiddle = (right + left) / 2f

	def isLeftOf(d: Float) = (right <= d)

	def isRightOf(d: Float) = (left >= d)

	def isAbove(d: Float) = (bottom >= d)

	def isBelow(d: Float) = (top <= d)

	def isBelowAndAbove(a: Float, b: Float): Boolean = (isBelow(a) && isAbove(b))

	def isMostlyBelow(r: Rectangle) = new OverlapRatios(this, r).mostlyBelow

	val left  : Float
	val bottom: Float
	val right : Float
	val top   : Float

	def equalBounds(that: Rectangle): Boolean =
		{
		left == that.left && bottom == that.bottom && right == that.right && top == that.top
		}

	// inclusive
	lazy val horizontalInterval = (left, right)
	lazy val verticalInterval   = (bottom, top)

	def width = right - left

	def height = (top - bottom).abs

	lazy val area = width * height

	def aspectRatio = width / height

	def isLandscape = width > height

	def isPortrait = height > width

	def isEmpty = area == 0

	/*
   def isHighlyLandscape = width > 4*height
   def isSquare = width == height
 */
	def isBogus = (height == 0 && width == 0)

	def isReal = (height != 0 || width != 0)

	def overlaps(that: Rectangle): Boolean =
		{
		if (this.left > that.right || that.left > this.right || this.bottom > that.top || that.bottom > this.top)
			false
		else
			intersection(that).isDefined
		}

	/*
   def overlapsInclusive(that: Rectangle): Boolean =
	   {
	   containsCornerInclusive(that) || that.containsCornerInclusive(this)
	   }*/
	def fullyContains(that: Rectangle): Boolean =
		{
		left <= that.left && bottom <= that.bottom && right >= that.right && top >= that.top
		}

	def boundsequal(that: Rectangle): Boolean =
		{
		left == that.left && bottom == that.bottom && right == that.right && top == that.top
		}

	def containsCornerInclusive(that: Rectangle): Boolean =
		{
		containsPointInclusive(that.left, that.bottom) || containsPointInclusive(that.left, that.top) ||
		containsPointInclusive(that.right, that.bottom) ||
		containsPointInclusive(that.right, that.top)
		}

	def containsPointInclusive(x: Float, y: Float): Boolean =
		{
		left <= x && x <= right && bottom <= y && y <= top
		}

	/*
   // doesn't work right if the corners lie on the borders but not at the corners

   def overlapsExclusive(that: Rectangle): Boolean =
	   {
	   containsCornerExclusive(that) || that.containsCornerExclusive(this) || (this==that)
	   }

   def containsCornerExclusive(that: Rectangle): Boolean =
	   {
	   containsPointExclusive(that.left, that.bottom) || containsPointExclusive(that.left, that.top) ||
	   containsPointExclusive(that.right, that.bottom) ||
	   containsPointExclusive(that.right, that.top)
	   }

   def containsPointExclusive(x: Float, y: Float): Boolean =
	   {
	   left < x && x < right && bottom < y && y < top
	   }*/
	def leftOf(x: Float): Rectangle = Rectangle(left.min(x), bottom, right.min(x), top).get

	def rightOf(x: Float): Rectangle = Rectangle(left.max(x), bottom, right.max(x), top).get

	def below(y: Float): Rectangle = Rectangle(left, bottom.min(y), right, top.min(y)).get

	def above(y: Float): Rectangle = Rectangle(left, bottom.max(y), right, top.max(y)).get

	def intersection(that: Rectangle): Option[Rectangle] =
		{
		//if (overlapsInclusive(that))
		//	{
		// the Rectangle factory returns None for negative extents, but allows zero extents.
		val or = Rectangle(left.max(that.left), bottom.max(that.bottom), right.min(that.right), top.min(that.top))
		//	}
		or.flatMap(r =>
			           {
			           if (r.area > 0) Some(r)
			           else None
			           })
		}

	override def toString = "(" + left + "," + bottom + "), (" + right + "," + top + ")"
	}

case class RealRectangle(override val left: Float, override val bottom: Float, override val right: Float, override val top: Float) extends Rectangle
	{
	require(top >= bottom)
	require(right >= left)
	}

case class Page(pagenum: Int, rectangle: Rectangle)

//** should be rectangleOnPages, with startPage and endPage.  In this case pages shourd be considered laid out diagonally,
// to cover the case of a right olumn continuing to a left column
trait RectangleOnPage extends Rectangle
	{
	val page: Page

	def overlaps(that: RectangleOnPage): Boolean =
		{
		if (page == that.page)
			{super.overlaps(that)}
		else false
		}

	def fullyContains(that: RectangleOnPage): Boolean =
		{
		if (page == that.page)
			{super.fullyContains(that)}
		else false
		}

	def boundsequal(that: RectangleOnPage): Boolean =
		{
		if (page == that.page)
			{super.boundsequal(that)}
		else false
		}

	def containsCorner(that: RectangleOnPage): Boolean =
		{
		if (page == that.page)
			{super.containsCornerInclusive(that)}
		else false
		}

	override def leftOf(x: Float): RectangleOnPage = RectangleOnPage(page, left.min(x), bottom, right.min(x), top).get

	override def rightOf(x: Float): RectangleOnPage = RectangleOnPage(page, left.max(x), bottom, right.max(x), top).get

	override def below(y: Float): RectangleOnPage = RectangleOnPage(page, left, bottom.min(y), right, top.min(y)).get

	override def above(y: Float): RectangleOnPage = RectangleOnPage(page, left, bottom.max(y), right, top.max(y)).get

	def intersection(that: RectangleOnPage): Option[RectangleOnPage] =
		{
		if (overlaps(that))
			{
			RectangleOnPage(page, left.max(that.left), bottom.max(that.bottom), right.min(that.right), top.min(that.top))
			}
		else None
		}

	override def toString = "" + page + " (" + left + "," + bottom + "), (" + right + "," + top + ")"
	}

private case class RealRectangleOnPage(page: Page, left: Float, bottom: Float, right: Float, top: Float) extends RectangleOnPage
	{
	require(top >= bottom)
	require(right >= left)
	}

object RectangleOnPage extends Logging
	{
	def encompassing(rects: Seq[RectangleOnPage], padding: Int): Option[RectangleOnPage] =
		{
		val pages = rects.map(_.page).distinct
		pages.length match
		{
			case 1 =>
				{
				val rect = Rectangle.encompassing(rects, padding)
				rect match
				{
					case None    =>
						{
						logger.warn("Can't create rectangle encompassing None underlying")
						None
						}
					case Some(r) => RectangleOnPage(pages(0), r)
				}
				}
			case _ =>
				{
				logger.warn("Can't create rectangle spanning multiple pages")
				None
				}
		}
		}

	val BoundingBoxRE = """(.*),(.*),(.*),(.*)""".r

	// doesn't enforce number format
	def apply(page: Page, coordinates: String): Option[RectangleOnPage] =
		{
		if (coordinates.trim.isEmpty) throw new Error("empty coordinate string")
		else
			{
			lazy val BoundingBoxRE(x0s, y0s, x1s, y1s) = coordinates
			lazy val left = x0s.toFloat;
			lazy val bottom = y0s.toFloat;
			lazy val right = x1s.toFloat;
			lazy val top = y1s.toFloat;
			RectangleOnPage(page, left, bottom, right, top)
			}
		}

	def apply(page: Page, rect: Rectangle): Option[RectangleOnPage] =
		{
		Some(new RealRectangleOnPage(page, rect.left, rect.bottom, rect.right, rect.top))
		}

	def apply(page: Page, left: Float, bottom: Float, right: Float, top: Float): Option[RectangleOnPage] =
		{
		Some(new RealRectangleOnPage(page, left, bottom, right, top))
		}
	}

