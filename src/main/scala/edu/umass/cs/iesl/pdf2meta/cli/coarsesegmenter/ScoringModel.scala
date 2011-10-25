package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._

trait ScoringModelComponent
  {
  val scoringFunctions: List[ScoringFunction]
  }

object ScoringModel extends Logging
  {

  // features should be normalized to [-1,1] so that the later weighted mixtures are interpretable
  val nearPageTop = LocalFeature("nearTop", (box: DocNode) =>
    {
    val pageRect: Rectangle = box.rectangle.get.page.rectangle
    box.rectangle match
    {
      case None => 0
      case Some(r) =>
        (((r.top.toDouble) / pageRect.height) - 0.5) * 2 // scale to [-1,1]
    }
    })

  val nearBeginning = ContextFeature("nearBeginning", (doc: DocNode, box: DocNode) =>
    {
    val csp: Map[DocNode, (Double, Double)] = doc.charSpanProportional
    val result = 1 - (csp(box)._1 * 2)
    result
    })

  //val nearEnd = ContextFeature("nearEnd", (doc: DocNode, box: DocNode) => 1 - nearBeginning(doc, box))
  //val nearPageBottom = LocalFeature("nearTop", (box: DocNode) => 1 - nearPageTop(box))

  val highlyLandscape = LocalFeature("nearTop", (box: DocNode) =>
    {
    box.rectangle match
    {
      case None => -1
      case Some(r) => if (r.isHighlyLandscape) 1 else -1
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
        else -1
    }
    })

  val veryLargeFont = ContextFeature("veryLargeFont", (doc: DocNode, box: DocNode) =>
    {
    box.dominantFont match
    {
      case None => 0
      case Some(x) =>
        if (x.height > doc.dominantFont.get.height * 1.5) 1
        else -1
    }
    })

  val smallFont = ContextFeature("smallFont", (doc: DocNode, box: DocNode) =>
    {
    box.dominantFont match
    {
      case None => 0
      case Some(x) =>
        if (x.height < doc.dominantFont.get.height) 1
        else -1
    }
    })

  val dominantFont = ContextFeature("dominantFont", (doc: DocNode, box: DocNode) =>
    {
    box.dominantFont match
    {
      case None => 0
      case Some(x) =>
        if (x == doc.dominantFont.get) 1
        else -1
    }
    })

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
    if (punct / l > .05) 1 else -1
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
    LocalFeature("lengthBetween", (box: TextBox) =>
      {
      if (box.text.length >= min &&
          box.text.length <= max) 1
      else -1
      })
    }


  val scoringFunctions = List(ScoringFunction("title", nearBeginning -> 1, nearPageTop -> 1, largeFont -> 1, veryLargeFont -> 2, dominantFont -> -2, highlyPunctuated -> -1,
                                              lengthBetween(10, 200) -> 2),
                              ScoringFunction("authors", nearPageTop -> .5, largeFont -> .5, dominantFont -> -1, highlyPunctuated -> 2, lengthBetween(10, 1000) -> 1),
                              ScoringFunction("affiliations", nearBeginning -> -2, dominantFont -> -1, highlyPunctuated -> 2, lengthBetween(10, 1000) -> 1),
                              ScoringFunction("abstract", startsWith("Abstract") -> 10, nearBeginning -> 2,nearPageTop -> .5, largeFont -> .2, dominantFont -> -1, highlyPunctuated -> -2, lengthBetween(100, 5000) -> 1),
                              ScoringFunction("caption", nearBeginning -> -1, smallFont -> 1, dominantFont -> -1, lengthBetween(100, 5000) -> 1,startsWithDelimiting -> .5),
                              ScoringFunction("heading", nearBeginning -> -.5, lengthBetween(5, 100) -> 1, largeFont -> 2, veryLargeFont -> -2, highlyPunctuated -> -2),
                              ScoringFunction("body", dominantFont -> 5, highlyPunctuated -> -2),
                              ScoringFunction("references", nearBeginning -> -2, nearPageTop -> -.2, dominantFont -> -1, smallFont -> 2, highlyPunctuated -> 2),
                              ScoringFunction("header", nearPageTop -> 3, lengthBetween(10, 200) -> 1, highlyLandscape -> 1),
                              ScoringFunction("footnote", nearPageTop -> -3, smallFont -> 1, highlyPunctuated -> -1, lengthBetween(10, 200) -> 1, startsWithDelimiting -> .1),
                              ScoringFunction("footer", nearPageTop -> -4, highlyPunctuated -> 1, lengthBetween(10, 200) -> 1, highlyLandscape -> 1),
                              ScoringFunction("discard", lengthBetween(0, 5) -> 10))
  }
