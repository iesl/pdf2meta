package edu.umass.cs.iesl.pdf2meta.cli.readingorder

import org.scalatest.PrivateMethodTester._
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.Rectangle
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

class RectangularReadingOrderTest extends Spec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  val compareRectanglesSolid = PrivateMethod[Option[Int]]('compareRectanglesSolid)

  val rectangleGen: Gen[Rectangle] = for {
    b <- Gen.choose(0.0, 1000.0)
    l <- Gen.choose(0.0, 1000.0)
    r <- Gen.choose(l, 1000.0)
    t <- Gen.choose(b, 1000.0)
  } yield new Rectangle() {
      val bottom = b
      val right = r
      val top = t
      val left = l
    }

  def pairGen[T](g: Gen[T]): Gen[(T, T)] = for {
    n <- g
    m <- g
  } yield (n, m)

  describe("A reading order") {
    it("should order a pair of rectangles symmetrically") {


      forAll(pairGen(rectangleGen)) {
        case (x, y) => {
          val forward = RectangularReadingOrder invokePrivate compareRectanglesSolid(x, y)
          val reverse = RectangularReadingOrder invokePrivate compareRectanglesSolid(y, x)
          assert(forward === reverse.map(x => -x))
        }

      }
    }
  }
}
