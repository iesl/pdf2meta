package edu.umass.cs.iesl.pdf2meta.cli.config

import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter._

/*
trait ScoringModelComponent
  {
  val scoringFunctions: List[ScoringFunction]
  }
*/
object StandardScoringModel extends ScoringModel with Logging
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

  val sideways = LocalFeature("sideways", (box: DocNode) =>
    {
    box match
    {
      case b: AnnotatedDocNode => if (b.annotations.contains(("sideways"))) 1.0 else 0.0
      case _ => 0
    }
    })

  val marginPercentageMin = 0.05
  val marginPercentageMax = 0.1

  val topMargin = LocalFeature("topMargin", (box: DocNode) =>
    {
    box.rectangle match
    {
      case None => 0
      case Some(r) =>
        {
        val pageRect: Rectangle = box.rectangle.get.page.rectangle
        linearBetween(pageRect.height * (1 - marginPercentageMax), pageRect.height * (1 - marginPercentageMin))(r.bottom)
        }
    }
    })


  val bottomMargin = LocalFeature("bottomMargin", (box: DocNode) =>
    {
    box.rectangle match
    {
      case None => 0
      case Some(r) =>
        {
        val pageRect: Rectangle = box.rectangle.get.page.rectangle
        1 - linearBetween(pageRect.height * marginPercentageMin, pageRect.height * marginPercentageMax)(r.top)
        }
    }
    })

  val rightMargin = LocalFeature("rightMargin", (box: DocNode) =>
    {
    box.rectangle match
    {
      case None => 0
      case Some(r) =>
        {
        val pageRect: Rectangle = box.rectangle.get.page.rectangle
        linearBetween(pageRect.width * (1 - marginPercentageMax), pageRect.width * (1 - marginPercentageMin))(r.left)
        }
    }
    })

  val leftMargin = LocalFeature("leftMargin", (box: DocNode) =>
    {
    box.rectangle match
    {
      case None => 0
      case Some(r) =>
        {
        val pageRect: Rectangle = box.rectangle.get.page.rectangle
        1 - linearBetween(pageRect.width * marginPercentageMin, pageRect.width * marginPercentageMax)(r.right)
        }
    }
    })

  val notInMargin = LocalFeature("notInMargin", (box: DocNode) =>
    {
    box.rectangle match
    {
      case None => 0
      case Some(r) =>
        {
        {
        val pageRect: Rectangle = box.rectangle.get.page.rectangle
        val inAnyMargin =
          (1 - linearBetween(pageRect.width * marginPercentageMin, pageRect.width * marginPercentageMax)(r.right)) +
          linearBetween(pageRect.width * (1 - marginPercentageMax), pageRect.width * (1 - marginPercentageMin))(r.left) +
          (1 - linearBetween(pageRect.height * marginPercentageMin, pageRect.height * marginPercentageMax)(r.top)) +
          linearBetween(pageRect.height * (1 - marginPercentageMax), pageRect.height * (1 - marginPercentageMin))(r.bottom)
        1 - inAnyMargin
        }
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


  val lastNode = ContextFeature("lastNode", (doc: DocNode, box: DocNode) =>
    {
    val csp: Map[DocNode, (Double, Double)] = doc.charSpanProportional
    val endPosition: Double = csp(box)._2
    val result = linearBetween(1, 1)(endPosition)
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

  val isDelimiter = LocalFeature("isDelimiter", (box: DocNode) =>
    {
    box match
    {
      case x: DelimitingBox => 1
      case _ => 0
    }
    })


  val isFullPageDelimiter = LocalFeature("isFullPageDelimiter", (box: DocNode) =>
    {
    box match
    {
      case x: DelimitingBox =>
        {
        if (x.theRectangle.width / x.theRectangle.page.rectangle.width > 0.7) 1 else 0
        }
      case _ => 0
    }
    })

  val isNonMarginFullPageDelimiter = isFullPageDelimiter.multiply("isNonMarginFullPageDelimiter", notInMargin)

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
        if (x.height > doc.dominantFontHeight) 1
        else 0
    }
    })

  val veryLargeFont = ContextFeature("veryLargeFont", (doc: DocNode, box: DocNode) =>
    {
    box.dominantFont match
    {
      case None => 0
      case Some(x) => linearBetween(doc.dominantFontHeight * 1.2, doc.dominantFontHeight * 1.8)(x.height)
    }
    })

  val smallFont = ContextFeature("smallFont", (doc: DocNode, box: DocNode) =>
    {
    box.dominantFont match
    {
      case None => 0
      case Some(x) =>
        if (x.height < doc.dominantFontHeight) 1
        else 0
    }
    })

  val dominantFont = ContextFeature("dominantFont", (doc: DocNode, box: DocNode) =>
    {
    doc.dominantFont.map(dominant =>
        box.dominantFont.map(local => if (local == dominant) 1.0 else 0.0).getOrElse(0.0)).getOrElse(0.0)
  /*
    box.dominantFont match
    {
      case None => 0
      case Some(x) =>
        if (x == doc.dominantFont.get) 1
        else 0
    }*/

    })


  val dominantColumnWidth = ContextFeature("dominantColumnWidth ", (doc: DocNode, box: DocNode) =>
    {
    box.rectangle match
    {
      case None => 0
      case Some(x) =>
        {
        val width: Option[Double] = doc.dominantTextLineWidth
        width.map(w =>
                    {
                    if ((((x.width / 10.0).ceil) * 10.0) == w) 1.0 else 0.0
                    }).getOrElse(0.0)
        }
    }
    })


  val longColumnWidth = ContextFeature("longColumnWidth ", (doc: DocNode, box: DocNode) =>
    {
    box.rectangle match
    {
      case None => 0
      case Some(x) =>
        if (x.width > doc.dominantTextLineWidth.getOrElse(Double.MaxValue)) 1
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
    l match
    {
      case 0 => 0
      case _ =>
        {
        val punct: Double = l - box.text.filter(c => (c.isLetterOrDigit || c.isSpaceChar)).length()
        //logger.debug("Box " + box + " has punctuation ratio " + (punct / l))
        //if (punct / l > .05) 1 else -1
        linearBetween(.04, .08)(punct / l)
        }
    }
    })

  val notHighlyPunctuated = LocalFeature("notHighlyPunctuated", (box: TextBox) =>
    {
    val l: Double = box.text.length()
    l match
    {
      case 0 => 0
      case _ =>
        {
        val punct: Double = l - box.text.filter(c => (c.isLetterOrDigit || c.isSpaceChar)).length()
        //logger.debug("Box " + box + " has punctuation ratio " + (punct / l))
        //if (punct / l > .05) 1 else -1
        linearBetween(.04, .08)(punct)
        }
    }
    })

  //val short = TextFeature("short", (box: TextBox) => if (box.text.length() < 5) 1 else -1)
  val startsWithDelimiting = LocalFeature("startWithDelimiting", (box: DocNode) =>
    {
    box.allLeaves match
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


  def startsWith(name: String, s: String) =
    {
    LocalFeature(name, (box: TextBox) =>
      {
      if (box.text.matches("^" + s + ".*")) 1 else 0
      })
    }

  val emailRE = ".*@.*\\.(com|org|net|edu|..)"
  val doiRE = "10\\.\\d+\\/.+"
  val initialsRE = " [A-Z]\\.? "
  val digitsRE = "[0-9]*"
  val dateRE1 = "((0?[1-9]|^1[0-2])\\/(0?[1-9]|[1-2][0-9]|3[0-1])\\/(19|20)?[0-9][0-9])"
  val monthRE = "((jan(uary)?|feb(ruary)?|mar(ch)?|apr(il)?|may|jun(e)?|jul(y)?|aug(ust)?|sept(ember)?|oct(ober)?|nov(ember)?|dec(ember)?)"
  val dateRE2 = monthRE + " +([1-2][0-9]|3[0-1]|0?[1-9]),? +(19|20)[0-9][0-9])"
  val dateRE3 = "([1-2][0-9]|3[0-1]|0?[1-9]) +" + monthRE + ",? +(19|20)[0-9][0-9])"

  def contains(name: String, regex: String) =
    {
    LocalFeature(name, (box: TextBox) =>
      {
      val count = box.text.split(regex).length - 1
      count match
      {
        case 0 => 0.0
        case _ => 1.0
      }
      })
    }

  def containsAny(name: String, regexes: Seq[String]) =
    {
    LocalFeature(name, (box: TextBox) =>
      {
      val lc: String = box.text
      val count = regexes.map(lc.split(_).length - 1).sum
      count match
      {
        case 0 => 0.0
        case _ => 1.0
      }
      })
    }

  def containsAnyLC(name: String, regexes: Seq[String]) =
    {
    LocalFeature(name, (box: TextBox) =>
      {
      val lc: String = box.text.toLowerCase
      val count = regexes.map(lc.split(_).length - 1).sum
      count match
      {
        case 0 => 0.0
        case _ => 1.0
      }
      })
    }

  def textProportion(name: String, ss: Seq[String], min: Double, max: Double) = regexProportion(name, ss.mkString("|"), min, max)


  def regexProportion(name: String, regex: String, min: Double, max: Double) =
    {
    LocalFeature(name, (box: TextBox) =>
      {
      val t: String = box.text
      t.length match
      {
        case 0 => 0
        case _ =>
          {
          // remove all of the requested strings and see how many characters remain
          val prop = (t.length - t.split(regex).mkString.length) / t.length.toDouble
          linearBetween(min, max)(prop)
          }
      }
      })
    }


  def lexiconProportion(name: String, lex: Lexicon, min: Double, max: Double) =
    {
    LocalFeature(name, (box: TextBox) =>
      {
      val t: String = box.text
      t.length match
      {
        case 0 => 0
        case _ =>
          {
          val m = lex.matches(box.text.split("\\W")) // not .toLowerCase
          val prop = m.mkString("").length.toDouble / t.length.toDouble
          linearBetween(min, max)(prop)
          }
      }
      })
    }


  val university = textProportion("AffiliationTerms", List("University", "Department", "Institute", "Center", "Laboratory"), .05, .1)

  val initials = textProportion("Initials", List(initialsRE), 0.05, 0.15)
  val digits = textProportion("Digits", List(digitsRE), 0.05, 0.15)
  val correspondence = contains("CorrespondenceTerms", "whom correspondence")
  val metadataText = contains("MetadataTerms", "ISSN|issn|\bDOI\b|\bdoi\b|[Cc]opyright")
  val abstractText = startsWith("AbstractTerms", "Abstract")
  val referencesText = startsWith("ReferencesTerms", "References")
  val http = contains("http", "http://")
  val figureText = startsWith("FigureTerms", "(Figure|Fig\\.|Table)")
  val headingText = startsWith("HeadingTerms", "Introduction|Background|Results|Materials|Methods|Discussion|Conclusion")
  val email = contains("Email", emailRE)
  val date = containsAnyLC("Date", Seq(dateRE1, dateRE2, dateRE3))
  val doi = contains("DOI", doiRE)

  /*
  def countLexiconMatches(name: String, lex: Lexicon) =
    {
    LocalFeature(name, (box: TextBox) =>
      {
      lex.countMatches(box.text.toLowerCase.split(" "))
      })
    }
*/
  val firstname = new CompoundFeature("FirstName",
                                      List(lexiconProportion("FirstNameMed", Lexicon.firstnameMed, 0.0, 0.2) -> 0.3, lexiconProportion("FirstNameHigh", Lexicon.firstnameHigh, 0.0, 0.2) -> 0.6,
                                           lexiconProportion("FirstNameHighest", Lexicon.firstnameHighest, 0.0, 0.2) -> 1.0).toMap)
  val lastname = new CompoundFeature("LastName", List(lexiconProportion("LastNameMed", Lexicon.lastnameMed, 0.0, 0.2) -> 0.3, lexiconProportion("LastNameHigh", Lexicon.lastnameHigh, 0.0, 0.2) -> 0.6,
                                                      lexiconProportion("LastNameHighest", Lexicon.lastnameHighest, 0.0, 0.2) -> 1.0).toMap)
  val noLastName = new FunctionFeature("NoLastName", lastname,
                                       {x =>
                                         {
                                         linearBetween(.9, .95)(1 - x)
                                         }
                                       })


  val cities = lexiconProportion("cities", Lexicon.cities, 0.05, 0.1)
  val states = lexiconProportion("states", Lexicon.states, 0.05, 0.1)

  val scoringFunctions =
    List(ScoringFunction("title", nearBeginning -> 2, notNearBeginning -> -2, nearPageTop -> 1, largeFont -> 1, veryLargeFont -> 2, highlyPunctuated -> -1, lengthBetween(30, 200) -> 2,
                         lengthAbove(200) -> -2, longColumnWidth -> 2),
         ScoringFunction("authors", firstname -> 2, noLastName -> -100, nearBeginning -> 1.5, nearPageTop -> .5, largeFont -> .5, dominantFont -> -1, highlyPunctuated -> 2,
                         lengthBetween(10, 1000) -> 1, initials -> 5, digits -> -10, longColumnWidth -> 2),
         ScoringFunction("affiliations", cities -> 1, states -> 1, nearBeginning -> 1.4, dominantFont -> -1, highlyPunctuated -> 2, lengthBetween(10, 1000) -> 1, university -> 5),
         ScoringFunction("contactinfo", correspondence -> 3, email -> 10, dominantFont -> -1, highlyPunctuated -> 2, lengthBetween(10, 1000) -> 1),
         ScoringFunction("abstract", abstractText -> 10, nearBeginning -> 2, nearPageTop -> .5, largeFont -> .2, dominantFont -> -1, highlyPunctuated -> -2, lengthBetween(100, 5000) -> 1,
                         initials -> -5, longColumnWidth -> 2),
         ScoringFunction("caption", figureText -> 10, nearBeginning -> -1, smallFont -> 1, dominantFont -> -1, lengthBetween(100, 5000) -> 1, startsWithDelimiting -> .5),
         ScoringFunction("heading", nearBeginning -> -.5, bottomMargin -> -10, lengthBetween(5, 100) -> 1, lengthAbove(150) -> -5, largeFont -> 2, veryLargeFont -> -2, highlyPunctuated -> -2,
                         headingText -> 10), ScoringFunction("body", dominantFont -> 3, dominantColumnWidth -> 1, highlyPunctuated -> -2),
         ScoringFunction("references", noLastName -> -100, lastname -> 2, referencesText -> 110, nearEnd -> 2, nearBeginning -> -2, dominantFont -> -1, smallFont -> 2, highlyPunctuated -> 2,
                         http -> 1), ScoringFunction("codeOrData", lastname -> -2, dominantFont -> -1, highlyPunctuated -> 4),
         ScoringFunction("header", topMargin -> 3, notInMargin -> -4, lengthBetween(10, 200) -> 1, highlyLandscape -> 1, veryLargeFont -> -2),
         ScoringFunction("footnote", bottomMargin -> 3, smallFont -> 1, highlyPunctuated -> -1, lengthBetween(10, 200) -> 1, startsWithDelimiting -> .1),
         ScoringFunction("footer", bottomMargin -> 3, notInMargin -> -4, lengthBetween(10, 200) -> 1, highlyLandscape -> 1),
         ScoringFunction("metadata", doi -> 10, metadataText -> 10, date -> 1, http -> 1), ScoringFunction("discard", sideways -> 10, lengthBetween(0, 5) -> 10),
         ScoringFunction("end", lastNode -> 15, isNonMarginFullPageDelimiter -> 15)) //nearEnd -> .1, //isDelimiter -> 3,
  }

