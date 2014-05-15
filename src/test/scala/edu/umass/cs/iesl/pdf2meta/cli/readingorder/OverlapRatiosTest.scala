package edu.umass.cs.iesl.pdf2meta.cli.readingorder

import org.scalatest.{FunSpec, BeforeAndAfter}
import org.scalatest.Matchers
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.RealRectangle


/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

class OverlapRatiosTest extends FunSpec with Matchers with BeforeAndAfter {

  val basis = new RealRectangle(10, 20, 100, 200)

  describe("A fully-above fully-left-of rectangle") {
    val fullyAboveLeft = new RealRectangle(basis.left - 2, basis.top + 1, basis.left - 1, basis.top + 2)
    val o = new OverlapRatios(fullyAboveLeft, basis)

    it("produces the expected above rectangle") {
      o.above shouldEqual fullyAboveLeft
    }

    it("produces the expected middleY rectangle") {
      o.middleY shouldEqual new RealRectangle(8, 200, 9, 200)
    }

    it("produces the expected below rectangle") {
      o.below shouldEqual new RealRectangle(8, 20, 9, 20)
    }

    it("produces the expected left rectangle") {
      o.left shouldEqual fullyAboveLeft
    }

    it("produces the expected middleX rectangle") {
      o.middleX shouldEqual new RealRectangle(10, 201, 10, 202)
    }

    it("produces the expected right rectangle") {
      o.right shouldEqual new RealRectangle(100, 201, 100, 202)
    }

    it("has the expected fully contained relationship with the basis") {
      o.fullyContained shouldEqual false
    }

    it("has the expected fully relationships with the basis") {
      o.fullyAbove shouldEqual true
      o.fullyLeftOf shouldEqual true
      o.fullyBelow shouldEqual false
      o.fullyRightOf shouldEqual false
    }

    it("has the expected mostly relationships with the basis") {
      o.mostlyAbove shouldEqual true
      o.mostlyLeftOf shouldEqual true
      o.mostlyBelow shouldEqual false
      o.mostlyRightOf shouldEqual false
    }

    it("has the expected starts relationships with the basis") {
      o.startsAboveLeft shouldEqual true
      o.startsBelowRight shouldEqual false
    }

    /*
      val aboveLeft = above.intersection(left)
  val aboveMiddle = above.intersection(middleX)
  val aboveRight = above.intersection(right)
  val middleLeft = middleY.intersection(left)
  val middleMiddle = middleY.intersection(middleX)
  val middleRight = middleY.intersection(left)
  val belowLeft = below.intersection(left)
  val belowMiddle = below.intersection(middleX)
  val belowRight = below.intersection(right)
     */

    it("produces the expected above-left rectangle") {
      o.aboveLeft shouldEqual Some(fullyAboveLeft)
    }
    it("produces the expected above-middle rectangle") {
      o.aboveMiddle shouldEqual None
    }
    it("produces the expected above-right rectangle") {
      o.aboveRight shouldEqual None
    }
    it("produces the expected middle-left rectangle") {
      o.middleLeft shouldEqual None
    }
    it("produces the expected middle-middle rectangle") {
      o.middleMiddle shouldEqual None
    }
    it("produces the expected middle-right rectangle") {
      o.middleRight shouldEqual None
    }
    it("produces the expected below-left rectangle") {
      o.belowLeft shouldEqual None
    }
    it("produces the expected below-middle rectangle") {
      o.belowMiddle shouldEqual None
    }
    it("produces the expected below-right rectangle") {
      o.belowRight shouldEqual None
    }
  }


  describe("A partly-above partly-left-of rectangle") {
    val partlyAboveLeft = new RealRectangle(basis.left - 2, basis.top - 1, basis.left + 1, basis.top + 2)
    val o = new OverlapRatios(partlyAboveLeft, basis)

    it("produces the expected above rectangle") {
      o.above shouldEqual new RealRectangle(8, 200, 11, 202)
    }

    it("produces the expected middleY rectangle") {
      o.middleY shouldEqual new RealRectangle(8, 199, 11, 200)
    }

    it("produces the expected below rectangle") {
      o.below shouldEqual new RealRectangle(8, 20, 11, 20)
    }

    it("produces the expected left rectangle") {
      o.left shouldEqual new RealRectangle(8, 199, 10, 202)
    }

    it("produces the expected middleX rectangle") {
      o.middleX shouldEqual new RealRectangle(10, 199, 11, 202)
    }

    it("produces the expected right rectangle") {
      o.right shouldEqual new RealRectangle(100, 199, 100, 202)
    }

    it("has the expected fully contained relationship with the basis") {
      o.fullyContained shouldEqual false
    }

    it("has the expected fully relationships with the basis") {
      o.fullyAbove shouldEqual false
      o.fullyLeftOf shouldEqual false
      o.fullyBelow shouldEqual false
      o.fullyRightOf shouldEqual false
    }

    it("has the expected mostly relationships with the basis") {
      o.mostlyAbove shouldEqual true
      o.mostlyLeftOf shouldEqual true
      o.mostlyBelow shouldEqual false
      o.mostlyRightOf shouldEqual false
    }

    it("has the expected starts relationships with the basis") {
      o.startsAboveLeft shouldEqual true
      o.startsBelowRight shouldEqual false
    }

    /*
      val aboveLeft = above.intersection(left)
  val aboveMiddle = above.intersection(middleX)
  val aboveRight = above.intersection(right)
  val middleLeft = middleY.intersection(left)
  val middleMiddle = middleY.intersection(middleX)
  val middleRight = middleY.intersection(left)
  val belowLeft = below.intersection(left)
  val belowMiddle = below.intersection(middleX)
  val belowRight = below.intersection(right)
     */

    it("produces the expected above-left rectangle") {
      o.aboveLeft shouldEqual Some(new RealRectangle(8, 200, 10, 202))
    }
    it("produces the expected above-middle rectangle") {
      o.aboveMiddle shouldEqual  Some(new RealRectangle(10, 200, 11, 202))
    }
    it("produces the expected above-right rectangle") {
      o.aboveRight shouldEqual None
    }
    it("produces the expected middle-left rectangle") {
      o.middleLeft shouldEqual   Some(new RealRectangle(8, 199, 10, 200))
    }
    it("produces the expected middle-middle rectangle") {
      o.middleMiddle shouldEqual   Some(new RealRectangle(10, 199, 11, 200))
    }
    it("produces the expected middle-right rectangle") {
      o.middleRight shouldEqual None
    }
    it("produces the expected below-left rectangle") {
      o.belowLeft shouldEqual None
    }
    it("produces the expected below-middle rectangle") {
      o.belowMiddle shouldEqual None
    }
    it("produces the expected below-right rectangle") {
      o.belowRight shouldEqual None
    }
  }

}
