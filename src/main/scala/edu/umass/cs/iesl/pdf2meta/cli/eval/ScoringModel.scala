package edu.umass.cs.iesl.pdf2meta.cli.eval

import com.weiglewilczek.slf4s.Logging

import edu.umass.cs.iesl.pdf2meta.cli.layout.{LayoutRectangle, TextBox,Page}

/*
 * Created by IntelliJ IDEA.
 * User: lorax
 * Date: 9/12/11
 * Time: 2:29 PM
 */
object ScoringModel extends Logging
  {

  // features should be normalized to [-1,1] so that the later weighted mixtures are interpretable
  val nearTop = Feature("nearTop", (page: Page, box: LayoutRectangle) =>
    {
    ((page.rectangle.height - box.rectangle.top).toDouble) /
    page.rectangle.height
    })

  val largeFont = TextFeature("largeFont", (page: Page, box: TextBox) => if (box.dominantFont._2 > 14) 1 else -1)

  val highlyPunctuated = TextFeature("highlyPunctuated", (page: Page, box: TextBox) =>
    {
    val l: Double = box.text.length()
    val punct: Double = l - box.text.filter(c => (c.isLetterOrDigit || c.isSpaceChar)).length()
    //logger.debug("Box " + box + " has punctuation ratio " + (punct / l))
    if (punct / l > .05) 1 else -1
    })

  val short = TextFeature("short", (page: Page, box: TextBox) => if (box.text.length() < 5) 1 else -1)

  val scoringFunctions = List(ScoringFunction("title", nearTop -> .5, largeFont -> 1, highlyPunctuated -> -1),
                              ScoringFunction("authors", nearTop -> .2, largeFont -> .5, highlyPunctuated -> 1),
                              ScoringFunction("abstract", nearTop -> .1, largeFont -> .2, highlyPunctuated -> -1),
                              ScoringFunction("body", nearTop -> 0, largeFont -> -1, highlyPunctuated -> -1),
                              ScoringFunction("discard", short -> 10))
  }
