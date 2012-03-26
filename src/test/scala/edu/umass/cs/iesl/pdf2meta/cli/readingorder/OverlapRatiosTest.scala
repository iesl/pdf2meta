package edu.umass.cs.iesl.pdf2meta.cli.readingorder

import org.scalatest.{BeforeAndAfter, Spec}
import org.scalatest.matchers.ShouldMatchers
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.RealRectangle


/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

class OverlapRatiosTest extends Spec with ShouldMatchers with BeforeAndAfter {

  val basis = new RealRectangle(10, 20, 100, 200)

  describe("A fully-above fully-left-of rectangle") {
    val fullyAboveLeft = new RealRectangle(basis.left - 2, basis.top + 1, basis.left - 1, basis.top + 2)
    val o = new OverlapRatios(fullyAboveLeft, basis)

    it("produces the expected above rectangle") {
      o.above should be === fullyAboveLeft
    }

    it("produces the expected middleY rectangle") {
      o.middleY should be === new RealRectangle(8, 200, 9, 200)
    }

    it("produces the expected below rectangle") {
      o.below should be === new RealRectangle(8, 20, 9, 20)
    }

    it("produces the expected left rectangle") {
      o.left should be === fullyAboveLeft
    }

    it("produces the expected middleX rectangle") {
      o.middleX should be === new RealRectangle(10, 201, 10, 202)
    }

    it("produces the expected right rectangle") {
      o.right should be === new RealRectangle(100, 201, 100, 202)
    }

    it("has the expected fully contained relationship with the basis") {
      o.fullyContained should be === false
    }

    it("has the expected fully relationships with the basis") {
      o.fullyAbove should be === true
      o.fullyLeftOf should be === true
      o.fullyBelow should be === false
      o.fullyRightOf should be === false
    }

    it("has the expected mostly relationships with the basis") {
      o.mostlyAbove should be === true
      o.mostlyLeftOf should be === true
      o.mostlyBelow should be === false
      o.mostlyRightOf should be === false
    }

    it("has the expected starts relationships with the basis") {
      o.startsAboveLeft should be === true
      o.startsBelowRight should be === false
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
      o.aboveLeft should be === Some(fullyAboveLeft)
    }
    it("produces the expected above-middle rectangle") {
      o.aboveMiddle should be === None
    }
    it("produces the expected above-right rectangle") {
      o.aboveRight should be === None
    }
    it("produces the expected middle-left rectangle") {
      o.middleLeft should be === None
    }
    it("produces the expected middle-middle rectangle") {
      o.middleMiddle should be === None
    }
    it("produces the expected middle-right rectangle") {
      o.middleRight should be === None
    }
    it("produces the expected below-left rectangle") {
      o.belowLeft should be === None
    }
    it("produces the expected below-middle rectangle") {
      o.belowMiddle should be === None
    }
    it("produces the expected below-right rectangle") {
      o.belowRight should be === None
    }
  }


  describe("A partly-above partly-left-of rectangle") {
    val partlyAboveLeft = new RealRectangle(basis.left - 2, basis.top - 1, basis.left + 1, basis.top + 2)
    val o = new OverlapRatios(partlyAboveLeft, basis)

    it("produces the expected above rectangle") {
      o.above should be === new RealRectangle(8, 200, 11, 202)
    }

    it("produces the expected middleY rectangle") {
      o.middleY should be === new RealRectangle(8, 199, 11, 200)
    }

    it("produces the expected below rectangle") {
      o.below should be === new RealRectangle(8, 20, 11, 20)
    }

    it("produces the expected left rectangle") {
      o.left should be === new RealRectangle(8, 199, 10, 202)
    }

    it("produces the expected middleX rectangle") {
      o.middleX should be === new RealRectangle(10, 199, 11, 202)
    }

    it("produces the expected right rectangle") {
      o.right should be === new RealRectangle(100, 199, 100, 202)
    }

    it("has the expected fully contained relationship with the basis") {
      o.fullyContained should be === false
    }

    it("has the expected fully relationships with the basis") {
      o.fullyAbove should be === false
      o.fullyLeftOf should be === false
      o.fullyBelow should be === false
      o.fullyRightOf should be === false
    }

    it("has the expected mostly relationships with the basis") {
      o.mostlyAbove should be === true
      o.mostlyLeftOf should be === true
      o.mostlyBelow should be === false
      o.mostlyRightOf should be === false
    }

    it("has the expected starts relationships with the basis") {
      o.startsAboveLeft should be === true
      o.startsBelowRight should be === false
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
      o.aboveLeft should be === Some(new RealRectangle(8, 200, 10, 202))
    }
    it("produces the expected above-middle rectangle") {
      o.aboveMiddle should be ===  Some(new RealRectangle(10, 200, 11, 202))
    }
    it("produces the expected above-right rectangle") {
      o.aboveRight should be === None
    }
    it("produces the expected middle-left rectangle") {
      o.middleLeft should be ===   Some(new RealRectangle(8, 199, 10, 200))
    }
    it("produces the expected middle-middle rectangle") {
      o.middleMiddle should be ===   Some(new RealRectangle(10, 199, 11, 200))
    }
    it("produces the expected middle-right rectangle") {
      o.middleRight should be === None
    }
    it("produces the expected below-left rectangle") {
      o.belowLeft should be === None
    }
    it("produces the expected below-middle rectangle") {
      o.belowMiddle should be === None
    }
    it("produces the expected below-right rectangle") {
      o.belowRight should be === None
    }
  }

}
