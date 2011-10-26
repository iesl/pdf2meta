package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._

trait ScoringModelComponent
  {
  val scoringFunctions: List[ScoringFunction]
  }

object ScoringModel extends Logging
  {

  // features should be normalized to [0,1] so that the later weighted mixtures are interpretable
  // previously we normalized to [-1, 1] because some features have opposites; now those are split in two features
  // it's OK to use negative weights in the scoring phase, i.e. a title should not be highly punctuated
  def linearBetween(min: Double, max: Double)(x: Double) =
    {
    x match
    {
      case x if x > max => 1.0
      case x if x < min => 0.0
      case x => (x - min) / (max - min)
    }
    }

  val nearPageTop = LocalFeature("nearTop", (box: DocNode) =>
    {
    box.rectangle match
    {
      case None => 0
      case Some(r) =>
        {
        val pageRect: Rectangle = box.rectangle.get.page.rectangle
        linearBetween(pageRect.height * 0.5, pageRect.height * 0.8)(r.top)
        }
    }
    })

  val marginPercentageMin = 0.05
  val marginPercentageMax = 0.1

  val topMargin = LocalFeature("topMargin", (box: DocNode) =>
    {
    val pageRect: Rectangle = box.rectangle.get.page.rectangle
    box.rectangle match
    {
      case None => 0
      case Some(r) => linearBetween(pageRect.height * (1 - marginPercentageMax), pageRect.height * (1 - marginPercentageMin))(r.bottom)
    }
    })

  val bottomMargin = LocalFeature("bottomMargin", (box: DocNode) =>
    {
    val pageRect: Rectangle = box.rectangle.get.page.rectangle
    box.rectangle match
    {
      case None => 0
      case Some(r) => 1 - linearBetween(pageRect.height * marginPercentageMin, pageRect.height * marginPercentageMax)(r.top)
    }
    })
  val rightMargin = LocalFeature("rightMargin", (box: DocNode) =>
    {
    val pageRect: Rectangle = box.rectangle.get.page.rectangle
    box.rectangle match
    {
      case None => 0
      case Some(r) => linearBetween(pageRect.width * (1 - marginPercentageMax), pageRect.width * (1 - marginPercentageMin))(r.left)
    }
    })
  val leftMargin = LocalFeature("leftMargin", (box: DocNode) =>
    {
    val pageRect: Rectangle = box.rectangle.get.page.rectangle
    box.rectangle match
    {
      case None => 0
      case Some(r) => 1 - linearBetween(pageRect.width * marginPercentageMin, pageRect.width * marginPercentageMax)(r.right)
    }
    })

  val notInMargin = LocalFeature("notInMargin", (box: DocNode) =>
    {
    val pageRect: Rectangle = box.rectangle.get.page.rectangle
    box.rectangle match
    {
      case None => 0
      case Some(r) =>
        {
        val inAnyMargin =
          (1 - linearBetween(pageRect.width * marginPercentageMin, pageRect.width * marginPercentageMax)(r.right)) +
          linearBetween(pageRect.width * (1 - marginPercentageMax), pageRect.width * (1 - marginPercentageMin))(r.left) +
          (1 - linearBetween(pageRect.height * marginPercentageMin, pageRect.height * marginPercentageMax)(r.top)) +
          linearBetween(pageRect.height * (1 - marginPercentageMax), pageRect.height * (1 - marginPercentageMin))(r.bottom)
        1 - inAnyMargin
        }
    }
    })

  val nearBeginning = ContextFeature("nearBeginning", (doc: DocNode, box: DocNode) =>
    {
    val csp: Map[DocNode, (Double, Double)] = doc.charSpanProportional
    val result = 1 - linearBetween(0.0, 0.1)(csp(box)._1)
    result
    })

  val notNearBeginning = ContextFeature("notNearBeginning", (doc: DocNode, box: DocNode) =>
    {
    val csp: Map[DocNode, (Double, Double)] = doc.charSpanProportional
    val result = linearBetween(0.1, 0.2)(csp(box)._1)
    result
    })

  val nearEnd = ContextFeature("nearEnd", (doc: DocNode, box: DocNode) =>
    {
    val csp: Map[DocNode, (Double, Double)] = doc.charSpanProportional
    val result = linearBetween(0.7, 0.9)(csp(box)._1)
    result
    })

  //val nearEnd = ContextFeature("nearEnd", (doc: DocNode, box: DocNode) => 1 - nearBeginning(doc, box))
  //val nearPageBottom = LocalFeature("nearTop", (box: DocNode) => 1 - nearPageTop(box))
  val highlyLandscape = LocalFeature("highlyLandscape", (box: DocNode) =>
    {
    box.rectangle match
    {
      case None => 0
      case Some(r) => linearBetween(4, 6)(r.aspectRatio)
    }
    })

  /*
  val unusualFont = ContextFeature("unusualFont", (doc:DocNode,box: DocNode) =>
    {
    if (box.dominantFont != doc.dominantFont) 1
    else -1
    })
*/
  val largeFont = ContextFeature("largeFont", (doc: DocNode, box: DocNode) =>
    {
    box.dominantFont match
    {
      case None => 0
      case Some(x) =>
        if (x.height > doc.dominantFont.get.height) 1
        else 0
    }
    })

  val veryLargeFont = ContextFeature("veryLargeFont", (doc: DocNode, box: DocNode) =>
    {
    box.dominantFont match
    {
      case None => 0
      case Some(x) => linearBetween(doc.dominantFont.get.height * 1.2, doc.dominantFont.get.height * 1.8)(x.height)
    }
    })

  val smallFont = ContextFeature("smallFont", (doc: DocNode, box: DocNode) =>
    {
    box.dominantFont match
    {
      case None => 0
      case Some(x) =>
        if (x.height < doc.dominantFont.get.height) 1
        else 0
    }
    })

  val dominantFont = ContextFeature("dominantFont", (doc: DocNode, box: DocNode) =>
    {
    box.dominantFont match
    {
      case None => 0
      case Some(x) =>
        if (x == doc.dominantFont.get) 1
        else 0
    }
    })

  /*  val nonDominantFont = ContextFeature("nonDominantFont", (doc: DocNode, box: DocNode) =>
      {
      box.dominantFont match
      {
        case None => 0
        case Some(x) =>
          if (x == doc.dominantFont.get) 0
          else 1
      }
      })*/
  /*  val unusualFontId = ContextFeature("unusualFontId", (doc: DocNode, box: DocNode) =>
        {
        box.dominantFont match
        {
          case None => 0
          case Some(x) =>
            if (x.fontid != doc.dominantFont.get.fontid) 1
            else -1
        }
        })*/
  val highlyPunctuated = LocalFeature("highlyPunctuated", (box: TextBox) =>
    {
    val l: Double = box.text.length()
    val punct: Double = l - box.text.filter(c => (c.isLetterOrDigit || c.isSpaceChar)).length()
    //logger.debug("Box " + box + " has punctuation ratio " + (punct / l))
    //if (punct / l > .05) 1 else -1
    linearBetween(.04, .08)(punct / l)
    })

  val notHighlyPunctuated = LocalFeature("notHighlyPunctuated", (box: TextBox) =>
    {
    val l: Double = box.text.length()
    val punct: Double = l - box.text.filter(c => (c.isLetterOrDigit || c.isSpaceChar)).length()
    //logger.debug("Box " + box + " has punctuation ratio " + (punct / l))
    //if (punct / l > .05) 1 else -1
    linearBetween(.04, .08)(punct)
    })

  //val short = TextFeature("short", (box: TextBox) => if (box.text.length() < 5) 1 else -1)
  val startsWithDelimiting = LocalFeature("startWithDelimiting", (box: DocNode) =>
    {
    box.allNodes match
    {
      case Nil => 0
      case (x: DelimitingBox) :: y => 1
      case _ => 0
    }
    })

  def lengthBetween(min: Int, max: Int) =
    {
    LocalFeature("lengthBetween (" + min + ", " + max + ")", (box: TextBox) =>
      {
      if (box.text.length >= min &&
          box.text.length <= max) 1
      else 0
      })
    }


  def lengthAbove(max: Int) =
    {
    LocalFeature("lengthAbove (" + max + ")", (box: TextBox) =>
      {
      if (box.text.length > max) 1
      else 0
      })
    }


  def startsWith(s: String) =
    {
    LocalFeature("startsWith (" + s + ")", (box: TextBox) =>
      {
      if (box.text.matches("^" + s + ".*")) 1 else 0
      })
    }

  val emailRE = ".*@.*\\.(com|org|net|edu|..)"
  val doiRE = "10\\.\\d+\\/\\d+"
  val initialsRE = " [A-Z]\\.? "

  def countContains(regex: String) =
    {
    LocalFeature("countContains (" + regex + ")", (box: TextBox) =>
      {
      box.text.split(regex).length - 1
      })
    }


  def textProportion(ss: Seq[String], min: Double, max: Double) =
    {
    LocalFeature("textProportion (" + ss.mkString(", ") + ")", (box: TextBox) =>
      {
      val t: String = box.text

      // remove all of the requested strings and see how many characters remain
      val prop = (t.length - t.split(ss.mkString("|")).mkString.length) / t.length.toDouble
      linearBetween(min, max)(prop)
      })
    }

  val university = textProportion(List("University", "Department", "Institute", "Center", "Laboratory"), .05, .1)

  val initials = textProportion(List(initialsRE), .06, .15)
  val correspondence = countContains("whom correspondence")
  val abstractText = startsWith("Abstract")
  val referencesText = startsWith("References")
  val figureText = startsWith("(Figure|Fig\\.|Table)")

  val scoringFunctions =
    List(ScoringFunction("title", nearBeginning -> 2, notNearBeginning -> -2, nearPageTop -> 1, largeFont -> 1, veryLargeFont -> 2, highlyPunctuated -> -1, lengthBetween(30, 200) -> 2,
                         lengthAbove(200) -> -2),
         ScoringFunction("authors", nearBeginning -> 1.5, nearPageTop -> .5, largeFont -> .5, dominantFont -> -1, highlyPunctuated -> 2, lengthBetween(10, 1000) -> 1, initials -> 5),
         ScoringFunction("affiliations", nearBeginning -> 1.4, dominantFont -> -1, highlyPunctuated -> 2, lengthBetween(10, 1000) -> 1, university -> 2),
         ScoringFunction("contactinfo", correspondence -> 3, countContains(emailRE) -> 3, dominantFont -> -1, highlyPunctuated -> 2, lengthBetween(10, 1000) -> 1),
         ScoringFunction("abstract", abstractText -> 10, nearBeginning -> 2, nearPageTop -> .5, largeFont -> .2, dominantFont -> -1, highlyPunctuated -> -2, lengthBetween(100, 5000) -> 1,
                         initials -> -5),
         ScoringFunction("caption", figureText -> 10, nearBeginning -> -1, smallFont -> 1, dominantFont -> -1, lengthBetween(100, 5000) -> 1, startsWithDelimiting -> .5),
         ScoringFunction("heading", nearBeginning -> -.5, lengthBetween(5, 100) -> 1, lengthAbove(150) -> -5, largeFont -> 2, veryLargeFont -> -2, highlyPunctuated -> -2),
         ScoringFunction("body", dominantFont -> 5, highlyPunctuated -> -2),
         ScoringFunction("references", referencesText -> 10, nearEnd -> 2, nearBeginning -> -2, dominantFont -> -1, smallFont -> 2, highlyPunctuated -> 2),
         ScoringFunction("header", topMargin -> 3, notInMargin -> -4, lengthBetween(10, 200) -> 1, highlyLandscape -> 1, veryLargeFont -> -2),
         ScoringFunction("footnote", bottomMargin -> 3, smallFont -> 1, highlyPunctuated -> -1, lengthBetween(10, 200) -> 1, startsWithDelimiting -> .1),
         ScoringFunction("footer", bottomMargin -> 3, notInMargin -> -4, lengthBetween(10, 200) -> 1, highlyLandscape -> 1), ScoringFunction("metadata", countContains(doiRE) -> 10),
         ScoringFunction("discard", lengthBetween(0, 5) -> 10))
  }
