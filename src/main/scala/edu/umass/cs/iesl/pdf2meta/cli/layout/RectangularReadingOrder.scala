package edu.umass.cs.iesl.pdf2meta.cli.layout


/*
 * Created by IntelliJ IDEA.
 * User: lorax
 * Date: 9/8/11
 * Time: 10:13 AM
 */
trait RectangularReadingOrder extends Ordering[Rectangular]
  {
  def handleOrderingError(message: String)

  //= new ArrayBuffer[String]
  private class OverlapRatios(a: Rectangle, basis: Rectangle)
    {
    val above = a.above(basis.top)
    val middleY = a.below(basis.top).above(basis.bottom)
    val below = a.below(basis.top)

    val left = a.leftOf(basis.left)
    val middleX = a.rightOf(basis.left).leftOf(basis.right)
    val right = a.rightOf(basis.right)

    val aboveLeft = above.intersection(left)
    val aboveMiddle = above.intersection(middleX)
    val aboveRight = above.intersection(right)
    val middleLeft = middleY.intersection(left)
    val middleMiddle = middleY.intersection(middleX)
    val middleRight = middleY.intersection(left)
    val belowLeft = below.intersection(left)
    val belowMiddle = below.intersection(middleX)
    val belowRight = below.intersection(right)

    //def fullyContained = that.area == middleMiddle.map(_.area).getOrElse(Double.NegativeInfinity)
    def fullyContained = a.area == a.intersection(basis).map(_.area).getOrElse(Double.NegativeInfinity)
    //def fullyContains = basis.area == a.intersection(basis).map(_.area).getOrElse(Double.NegativeInfinity)
    def fullyLeftOf = a.area == left.area
    def fullyRightOf = a.area == right.area
    def fullyAbove = a.area == above.area
    def fullyBelow = a.area == below.area


    def mostlyLeftOf = left.area > a.area * .5 // more than half of a is to the left of the basis left border
    def mostlyRightOf = right.area > a.area * .5

    def mostlyAbove = above.area > a.area * .5
    def mostlyBelow = below.area > a.area * .5
    }

  private val BEFORE = -1
  private val SAME = 0
  private val AFTER = 1

  override def compare(wrappedA: Rectangular, wrappedBasis: Rectangular): Int =
    {

    // terminology: position of a relative to basis
    // e.g. a is above basis, a is left of basis, a is before basis, etc.
    val a = wrappedA.rectangle
    val basis = wrappedBasis.rectangle
    val overlaps = new OverlapRatios(a, basis)
    val reverseOverlaps = new OverlapRatios(basis, a) // careful to make everything symmetric
    // if a is fully to the left of that, it comes first, regardless of y position (think columns)
    // note we have to consider the extents of the rectangles, not just the centers

    (overlaps, reverseOverlaps) match
    {
      case (o, r) if r.fullyContained => BEFORE
      case (o, r) if o.fullyContained => AFTER
      case (o, r) if o.fullyLeftOf => BEFORE
      case (o, r) if o.fullyRightOf => AFTER

      // these cases already covered
      // case (o,r) if r.fullyLeftOf => AFTER
      // case (o,r) if r.fullyRightOf => BEFORE
      // these kick in only if the horizontal matches failed, i.e. there is at least some horizontal overlap
      case (o, r) if o.fullyAbove => BEFORE
      case (o, r) if o.fullyBelow => AFTER

      // these cases already covered
      // case (o,r) if r.fullyAbove => AFTER
      // case (o,r) if r.fullyBelow => BEFORE
      // there is some horizontal and some vertical overlap, but not full containment
      // these cases could be done by comparing a.center to basis borders
      case (o, r) if o.mostlyLeftOf => BEFORE
      case (o, r) if o.mostlyRightOf => AFTER
      case (o, r) if r.mostlyLeftOf => AFTER
      case (o, r) if r.mostlyRightOf => BEFORE
      case (o, r) if o.mostlyAbove => BEFORE
      case (o, r) if o.mostlyBelow => AFTER
      case (o, r) if r.mostlyAbove => AFTER
      case (o, r) if r.mostlyBelow => BEFORE

      // a.center is within basis, and vice versa
      // just vote among the overhangs
      case (o, r) =>
        {
        val beforeVotes = o.left.area + o.above.area + r.right.area + r.below.area
        val afterVotes = o.right.area + o.below.area + r.left.area + r.above.area
        (beforeVotes - afterVotes) match
        {
          case d if (d > 0) => BEFORE
          case d if (d < 0) => AFTER
          case _ => SAME
        }
        }
    }
    }


  /*
      if (a boundsequal that)
        {
        0
        }
      else if (a overlaps that)
             {
             handleOrderingError("" + this + " overlaps " + that);
             val t = a.top compare that.top
             if (t != 0) t
             else
               {
               a.left compare that.left
               }
             //throw new ReadingOrderException("" + this + " overlaps " + that);
             }
      else if (a.left < that.left)
             {
             if (a.top >= that.bottom || a.right <= that.left) -1 else 1
             }
      else if (a.left == that.left)
             {
             if (a.top > that.top) -1 else 1
             }
      else
        {
        if (a.bottom >= that.top && a.left < that.right) -1 else 1
        }
      }*/
  }

/*class PageReadingOrder(errors: ArrayBuffer[String]) extends Ordering[TextBox]
  {
  val boxorder = new RectangularReadingOrder(errors) {}

  override def compare(a: TextBox, b: TextBox): Int =
    {
    if (a.page < b.page) -1
    else if (a.page == b.page) boxorder.compare(a.bbox, b.bbox)
    else 1



    /*  try
    {
    ((a.page < b.page) || (a.page == b.page && (a.bbox < b.bbox))
    }
    catch
    {
    case e: ReadingOrderException => (true, Some("Page " + a.page + ": " + e.getMessage))
    }
    }*/
    }
  }*/
