package edu.umass.cs.iesl.pdf2meta.cli.readingorder

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{DocNode, Rectangle}

/*class RectangularReadingOrderFactory extends ReadingOrderFactory
  {
  def apply(handleOrderingError: ((String) => Unit)): Ordering[Rectangular] =
    {new RectangularReadingOrder(handleOrderingError)}
  }
*/
class OverlapRatios(a: Rectangle, basis: Rectangle)
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

  def startsAboveLeft = a.left < basis.left && a.top > basis.top
  def startsBelowRight = a.left > basis.left && a.top > basis.top


  def mostlyLeftOf = left.area > a.area * .5 // more than half of a is to the left of the basis left border
  def mostlyRightOf = right.area > a.area * .5

  def mostlyAbove = above.area > a.area * .5
  def mostlyBelow = below.area > a.area * .5
  }

/*
 * Created by IntelliJ IDEA.
 * User: lorax
 * Date: 9/8/11
 * Time: 10:13 AM
 */
//(val handleOrderingError: ((String) => Unit))
object RectangularReadingOrder extends Ordering[DocNode]
  {
  //def handleOrderingError(message: String)
  //= new ArrayBuffer[String]
  private val BEFORE = -1
  private val SAME = 0
  private val AFTER = 1

  override def compare(wrappedA: DocNode, wrappedBasis: DocNode): Int =
    {


  (wrappedA.rectangle, wrappedBasis.rectangle) match
    {
      case (None, None) => SAME
      case (Some(x), None) => BEFORE
      case (None, Some(y)) => AFTER
      case (Some(aa), Some(basisb)) =>
        {
        // terminology: position of a relative to basis
        // e.g. a is above basis, a is left of basis, a is before basis, etc.

        (aa, basisb) match
        {
          case (a, basis) if (a.page.pagenum < basis.page.pagenum) => BEFORE
          case (a, basis) if (a.page.pagenum > basis.page.pagenum) => AFTER
          case (a, basis) =>
            {
            // if there is a main rectangle, there must also be a core rectangle (maybe just the same)
            val coreA = wrappedA.coreRectangle
            val coreB = wrappedBasis.coreRectangle
            compareRectanglesSolid(a,basis).
            getOrElse(compareRectanglesSolid(coreA.get, coreB.get).
            getOrElse(compareRectanglesFuzzy(coreA.get,coreB.get).    // note order
                      getOrElse(compareRectanglesFuzzy(a, basis).
                      getOrElse(SAME))))


            }
        }
        }
    }
    }

  private def compareRectanglesSolid(a: Rectangle, basis: Rectangle): Option[Int] =
    {
     val overlaps = new OverlapRatios(a, basis)
            val reverseOverlaps = new OverlapRatios(basis, a)

            // careful to make everything symmetric
            // if a is fully to the left of that, it comes first, regardless of y position (think columns)
            // note we have to consider the extents of the rectangles, not just the centers

            (overlaps, reverseOverlaps) match
            {
              case (o, r) if o.fullyLeftOf => Some(BEFORE)
              case (o, r) if o.fullyRightOf => Some(AFTER)

              // these cases already covered
              // case (o,r) if r.fullyLeftOf => AFTER
              // case (o,r) if r.fullyRightOf => BEFORE
              // these kick in only if the horizontal matches failed, i.e. there is at least some horizontal overlap
              case (o, r) if o.fullyAbove => Some(BEFORE)
              case (o, r) if o.fullyBelow => Some(AFTER)

              // these cases already covered
              // case (o,r) if r.fullyAbove => AFTER
              // case (o,r) if r.fullyBelow => BEFORE

              case (o, r) => None
            }
    }

// assume the solid comparison has already failed

  private def compareRectanglesFuzzy(a: Rectangle, basis: Rectangle): Option[Int] =
    {
     val overlaps = new OverlapRatios(a, basis)
            val reverseOverlaps = new OverlapRatios(basis, a)

            // careful to make everything symmetric
            // if a is fully to the left of that, it comes first, regardless of y position (think columns)
            // note we have to consider the extents of the rectangles, not just the centers

            (overlaps, reverseOverlaps) match
            {

              case (o, r) if r.fullyContained => Some(BEFORE)
              case (o, r) if o.fullyContained => Some(AFTER)
              // here's where it gets tricky
              // there is some horizontal and some vertical overlap, but not full containment
              // these cases could be done by comparing a.center to basis borders

              case (o, r) if o.mostlyAbove => Some(BEFORE)
              case (o, r) if o.mostlyBelow => Some(AFTER)
              case (o, r) if r.mostlyAbove => Some(AFTER)
              case (o, r) if r.mostlyBelow => Some(BEFORE)
              case (o, r) if o.mostlyLeftOf => Some(BEFORE)
              case (o, r) if o.mostlyRightOf => Some(AFTER)
              case (o, r) if r.mostlyLeftOf => Some(AFTER)
              case (o, r) if r.mostlyRightOf => Some(BEFORE)

              // a.center is within basis, and vice versa
              // just vote among the overhangs
              case (o, r) =>
                {
                val beforeVotes = o.left.area + o.above.area + r.right.area + r.below.area
                val afterVotes = o.right.area + o.below.area + r.left.area + r.above.area
                (beforeVotes - afterVotes) match
                {
                  case d if (d > 0) => Some(BEFORE)
                  case d if (d < 0) => Some(AFTER)
                  case _ => None
                }
                }
            }
    }
  }
