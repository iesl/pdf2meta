package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.pdf2meta.cli.readingorder.OverlapRatios

object Rectangle extends Logging
  {
  def encompassing(rects: Seq[Rectangle], padding: Int): Option[Rectangle] =
    {
    // PERF
    val realRects = rects.filter(_.isReal)
    if (realRects.isEmpty)
      {
      None //BogusRectangle
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

  //def encompassing(rects: Seq[Rectangular]): Rectangle = encompassing(rects.map(x=>x.rectangle))
  val BoundingBoxRE = """(.*),(.*),(.*),(.*)""".r // doesn't enforce number format
  /*  def apply(coordinates: String): Option[Rectangle] =
    {
    if (coordinates.trim.isEmpty) throw new Error("empty coordinate string")
    else
      {
      lazy val BoundingBoxRE(x0s, y0s, x1s, y1s) = coordinates
      lazy val left = x0s.toDouble;
      lazy val bottom = y0s.toDouble;
      lazy val right = x1s.toDouble;
      lazy val top = y1s.toDouble;
      Rectangle(left, bottom, right, top)
      }
    }
*/
  def apply(coordinates: String): Option[Rectangle] =
    {
    if (coordinates.trim.isEmpty) throw new Error("empty coordinate string")
    else
      {
      lazy val BoundingBoxRE(x0s, y0s, x1s, y1s) = coordinates
      lazy val left = x0s.toDouble;
      lazy val bottom = y0s.toDouble;
      lazy val right = x1s.toDouble;
      lazy val top = y1s.toDouble;
      Rectangle(left, bottom, right, top)
      }
    }

  def apply(left: Double, bottom: Double, right: Double, top: Double): Option[Rectangle] =
    {
    //if (left == 0 && bottom == 0 && top == 0 && right == 0) Some(new BogusRectangle)
    // else
    Some(new RealRectangle(left, bottom, right, top))
    }

  /*  def apply(left: Double, bottom: Double, right: Double, top: Double, container: Rectangle): Option[Rectangle] =
      {
      //if (left == 0 && bottom == 0 && top == 0 && right == 0) Some(new BogusRectangle)
      // else
      Some(new NestedRealRectangle(left, bottom, right, top, container))
      }*/
  }


trait Rectangle
  {
  def topEdge: Rectangle = Rectangle(left, top, right, top).get

  def bottomEdge: Rectangle = Rectangle(left, bottom, right, bottom).get

  //def container: Rectangle
  def horizontalMiddle = (right + left) / 2.


  def isLeftOf(d: Double) = (right <= d)
  def isRightOf(d: Double) = (left >= d)
  def isAbove(d: Double) = (bottom >= d)
  def isBelow(d: Double) = (top <= d)

  def isBelowAndAbove(a: Double, b: Double): Boolean = (isBelow(a) && isAbove(b))

  def isMostlyBelow(r: Rectangle) = new OverlapRatios(this, r).mostlyBelow

  val left: Double
  val bottom: Double
  val right: Double
  val top: Double

  // inclusive
  lazy val horizontalInterval = (left, right)
  lazy val verticalInterval = (bottom, top)

  def width = right - left
  def height = (top - bottom).abs
  def area = width * height

  def aspectRatio = width / height

  def isLandscape = width > height
  def isPortrait = height > width
  /*
  def isHighlyLandscape = width > 4*height
  def isSquare = width == height
*/
  def isBogus = (height == 0 && width == 0)
  def isReal = (height != 0 || width != 0)

  def overlaps(that: Rectangle): Boolean =
    {
    //fullyContains(that) || that.fullyContains(this) ||
    containsCorner(that) || that.containsCorner(this)
    }


  def fullyContains(that: Rectangle): Boolean =
    {
    left <= that.left && bottom <= that.bottom && right >= that.right && top >= that.top
    }

  def boundsequal(that: Rectangle): Boolean =
    {
    left == that.left && bottom == that.bottom && right == that.right && top == that.top
    }

  def containsCorner(that: Rectangle): Boolean =
    {
    containsPoint(that.left, that.bottom) || containsPoint(that.left, that.top) ||
    containsPoint(that.right, that.bottom) ||
    containsPoint(that.right, that.top)
    }

  def containsPoint(x: Double, y: Double): Boolean =
    {
    left <= x && x <= right && bottom <= y && y <= top
    }

  def leftOf(x: Double): Rectangle = Rectangle(left.min(x), bottom, right.min(x), top).get
  def rightOf(x: Double): Rectangle = Rectangle(left.max(x), bottom, right.max(x), top).get

  def below(y: Double): Rectangle = Rectangle(left, bottom.min(y), right, top.min(y)).get
  def above(y: Double): Rectangle = Rectangle(left, bottom.max(y), right, top.max(y)).get

  def intersection(that: Rectangle): Option[Rectangle] =
    {
    if (overlaps(that))
      {
      Rectangle(left.max(that.left), bottom.max(that.bottom), right.min(that.right), top.min(that.top))
      }
    else None
    }

  override def toString = "(" + left + "," + bottom + "), (" + right + "," + top + ")"
  }


private class RealRectangle(val left: Double, val bottom: Double, val right: Double, val top: Double) extends Rectangle
  {
  require(top >= bottom)
  require(right >= left)
  //  def container : Rectangle = this
  }

/*
private class NestedRealRectangle(override val left: Double,override val bottom: Double, override val right: Double,
override val top: Double,
                                  override val container: Rectangle) extends RealRectangle(left, bottom, right, top)
*/
case class Page(pagenum: Int, rectangle: Rectangle)

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
      {super.containsCorner(that)}
    else false
    }


  override def leftOf(x: Double): RectangleOnPage = RectangleOnPage(page, left.min(x), bottom, right.min(x), top).get
  override def rightOf(x: Double): RectangleOnPage = RectangleOnPage(page, left.max(x), bottom, right.max(x), top).get

  override def below(y: Double): RectangleOnPage = RectangleOnPage(page, left, bottom.min(y), right, top.min(y)).get
  override def above(y: Double): RectangleOnPage = RectangleOnPage(page, left, bottom.max(y), right, top.max(y)).get

  def intersection(that: RectangleOnPage): Option[RectangleOnPage] =
    {
    if (overlaps(that))
      {
      RectangleOnPage(page, left.max(that.left), bottom.max(that.bottom), right.min(that.right), top.min(that.top))
      }
    else None
    }

  override def toString = "" + page + " (" + left + "," + bottom + "), (" + right + "," + top + ")"

  // The superclass intersection should call the subclass overlaps(), right?
  /*  def intersection(that: Rectangle): Option[Rectangle] =
      {
      if (overlaps(that))
        {
        Rectangle(left.max(that.left), bottom.max(that.bottom), right.min(that.right), top.min(that.top))
        }
      else None
      }*/
  }

private class RealRectangleOnPage(val page: Page, val left: Double, val bottom: Double, val right: Double, val top: Double) extends RectangleOnPage
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
          case None =>
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

  /*def encompassing(rects: Seq[RectangleOnPage], padding: Int): Option[RectangleOnPage] =
    {
    // PERF
    val realRects = rects.filter(_.isReal)
    if (realRects.isEmpty)
      {
      None //BogusRectangle
      }
    else
      {
      val bottom = realRects.map(r => r.bottom).min - padding
      val left = realRects.map(r => r.left).min - padding
      val top = realRects.map(r => r.top).max + padding
      val right = realRects.map(r => r.right).max + padding
      Some(new RealRectangle(left, bottom, right, top))
      }
    }*/
  //def encompassing(rects: Seq[Rectangular]): Rectangle = encompassing(rects.map(x=>x.rectangle))
  val BoundingBoxRE = """(.*),(.*),(.*),(.*)""".r // doesn't enforce number format
  /*  def apply(coordinates: String): Option[Rectangle] =
    {
    if (coordinates.trim.isEmpty) throw new Error("empty coordinate string")
    else
      {
      lazy val BoundingBoxRE(x0s, y0s, x1s, y1s) = coordinates
      lazy val left = x0s.toDouble;
      lazy val bottom = y0s.toDouble;
      lazy val right = x1s.toDouble;
      lazy val top = y1s.toDouble;
      Rectangle(left, bottom, right, top)
      }
    }
*/
  def apply(page: Page, coordinates: String): Option[RectangleOnPage] =
    {
    if (coordinates.trim.isEmpty) throw new Error("empty coordinate string")
    else
      {
      lazy val BoundingBoxRE(x0s, y0s, x1s, y1s) = coordinates
      lazy val left = x0s.toDouble;
      lazy val bottom = y0s.toDouble;
      lazy val right = x1s.toDouble;
      lazy val top = y1s.toDouble;
      RectangleOnPage(page, left, bottom, right, top)
      }
    }

  def apply(page: Page, rect: Rectangle): Option[RectangleOnPage] =
    {
    Some(new RealRectangleOnPage(page, rect.left, rect.bottom, rect.right, rect.top))
    }

  def apply(page: Page, left: Double, bottom: Double, right: Double, top: Double): Option[RectangleOnPage] =
    {
    //if (left == 0 && bottom == 0 && top == 0 && right == 0) Some(new BogusRectangle)
    // else
    Some(new RealRectangleOnPage(page, left, bottom, right, top))
    }

  /*  def apply(left: Double, bottom: Double, right: Double, top: Double, container: Rectangle): Option[Rectangle] =
      {
      //if (left == 0 && bottom == 0 && top == 0 && right == 0) Some(new BogusRectangle)
      // else
      Some(new NestedRealRectangle(left, bottom, right, top, container))
      }*/
  }


/*private class BogusRectangle() extends Rectangle
  {
  val bottom = _
  val left = _
  val right = _
  val top = _
  }
*/
// This is more like null than like zero, in that a rectangle of zero area is still Real.
// Could have used an Option for it I guess...
// private class BogusRectangle extends RealRectangle(0, 0, 0, 0)
/*
object NestedRectangular
  {
  val errors = new ArrayBuffer[String]
  val boxorder = new RectangularReadingOrder
    {
    def handleOrderingError(message: String)
      {errors += message}
    }
  }

trait NestedRectangular[+T <: NestedRectangular] extends OrderedTreeNode[T] with Rectangular
  {

  }
*/
//class ReadingOrderException(message: String) extends Exception(message)
