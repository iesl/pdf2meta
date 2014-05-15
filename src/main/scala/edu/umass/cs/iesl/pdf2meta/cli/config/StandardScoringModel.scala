package edu.umass.cs.iesl.pdf2meta.cli.config

import com.typesafe.scalalogging.slf4j.Logging
import scala.Predef._
import scala.{Float, Some}
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter._
import edu.umass.cs.iesl.scalacommons.Lexicon
import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter.Lexicon

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
	def linearBetween(min: Float, max: Float)(x: Float) =
		{
		x match
		{
			case x if x > max => 1.0f
			case x if x < min => 0.0f
			case x            => (x - min) / (max - min)
		}
		}

	val nearPageTop = LocalFeature("nearTop", (box: DocNode) =>
		{
		box.rectangle match
		{
			case None    => 0
			case Some(r) =>
				{
				val pageRect: Rectangle = box.rectangle.get.page.rectangle
				linearBetween(pageRect.height * 0.5f, pageRect.height * 0.8f)(r.top)
				}
		}
		})

	val sideways = LocalFeature("sideways", (box: DocNode) =>
		{
		box match
		{
			case b: AnnotationNode => if (b.annotations.contains(("sideways"))) 1.0f else 0.0f
			case _                 => 0f
		}
		})

	val marginPercentageMin = 0.05f
	val marginPercentageMax = 0.1f

	val topMargin = LocalFeature("topMargin", (box: DocNode) =>
		{
		box.rectangle match
		{
			case None    => 0
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
			case None    => 0
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
			case None    => 0
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
			case None    => 0
			case Some(r) =>
				{
				val pageRect: Rectangle = box.rectangle.get.page.rectangle
				1 - linearBetween(pageRect.width * marginPercentageMin, pageRect.width * marginPercentageMax)(r.right)
				}
		}
		})

	val notInMargin = LocalFeature[DocNode]("notInMargin", (box: DocNode) =>
		{
		box.rectangle match
		{
			case None    => 0
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

	val nearBeginning = ContextFeature("nearBeginning", (doc: DocNode, box: LeafNode) =>
		{
		val csp: Map[LeafNode, (Float, Float)] = doc.charSpanProportional
		val result = 1 - linearBetween(0.0f, 0.1f)(csp(box)._1)
		result
		})

	val notNearBeginning = ContextFeature("notNearBeginning", (doc: DocNode, box: LeafNode) =>
		{
		val csp: Map[LeafNode, (Float, Float)] = doc.charSpanProportional
		val result = linearBetween(0.1f, 0.2f)(csp(box)._1)
		result
		})

	val nearEnd = ContextFeature("nearEnd", (doc: DocNode, box: LeafNode) =>
		{
		val csp: Map[LeafNode, (Float, Float)] = doc.charSpanProportional
		val result = linearBetween(0.7f, 0.9f)(csp(box)._1)
		result
		})

	val lastNode = ContextFeature("lastNode", (doc: DocNode, box: LeafNode) =>
		{
		val csp: Map[LeafNode, (Float, Float)] = doc.charSpanProportional
		val endPosition: Float = csp(box)._2
		val result = linearBetween(1, 1)(endPosition)
		result
		})

	//val nearEnd = ContextFeature("nearEnd", (doc: DocNode, box: DocNode) => 1 - nearBeginning(doc, box))
	//val nearPageBottom = LocalFeature("nearTop", (box: DocNode) => 1 - nearPageTop(box))
	val highlyLandscape = LocalFeature("highlyLandscape", (box: DocNode) =>
		{
		box.rectangle match
		{
			case None    => 0
			case Some(r) => linearBetween(4, 6)(r.aspectRatio)
		}
		})

	val isDelimiter = LocalFeature("isDelimiter", (box: DocNode) =>
		{
		box match
		{
			case x: DelimitingBox => 1
			case _                => 0
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
			case _                => 0
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
			case None    => 0
			case Some(x) =>
				{
				if (x.height > doc.dominantFontHeight) 1
				else 0
				}
		}
		})

	val veryLargeFont = ContextFeature("veryLargeFont", (doc: DocNode, box: DocNode) =>
		{
		box.dominantFont match
		{
			case None    => 0
			case Some(x) => linearBetween(doc.dominantFontHeight * 1.2f, doc.dominantFontHeight * 1.8f)(x.height)
		}
		})

	val smallFont = ContextFeature("smallFont", (doc: DocNode, box: DocNode) =>
		{
		box.dominantFont match
		{
			case None    => 0
			case Some(x) =>
				{
				if (x.height < doc.dominantFontHeight) 1
				else 0
				}
		}
		})

	val dominantFont = ContextFeature("dominantFont", (doc: DocNode, box: DocNode) =>
		{
		doc.dominantFont.map(dominant =>
			                     box.dominantFont.map(local => if (local == dominant) 1.0f else 0.0f).getOrElse(0.0f)).getOrElse(0.0f)
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
			case None    => 0f
			case Some(x) =>
				{
				val width: Option[Float] = doc.dominantTextLineWidth
				width.map(w =>
					          {
					          if ((((x.width / 10.0f).ceil) * 10.0f) == w) 1.0f else 0.0f
					          }).getOrElse(0.0f)
				}
		}
		})

	val longColumnWidth = ContextFeature("longColumnWidth ", (doc: DocNode, box: DocNode) =>
		{
		box.rectangle match
		{
			case None    => 0
			case Some(x) =>
				{
				if (x.width > doc.dominantTextLineWidth.getOrElse(Float.MaxValue)) 1
				else 0
				}
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
		val l: Float = box.text.length()
		l match
		{
			case 0 => 0
			case _ =>
				{
				val punct: Float = l - box.text.filter(c => (c.isLetterOrDigit || c.isSpaceChar)).length()
				//logger.debug("Box " + box + " has punctuation ratio " + (punct / l))
				//if (punct / l > .05) 1 else -1
				linearBetween(.04f, .08f)(punct / l)
				}
		}
		})

	val notHighlyPunctuated = LocalFeature("notHighlyPunctuated", (box: TextBox) =>
		{
		val l: Float = box.text.length()
		l match
		{
			case 0 => 0
			case _ =>
				{
				val punct: Float = l - box.text.filter(c => (c.isLetterOrDigit || c.isSpaceChar)).length()
				//logger.debug("Box " + box + " has punctuation ratio " + (punct / l))
				//if (punct / l > .05) 1 else -1
				linearBetween(.04f, .08f)(punct)
				}
		}
		})

	//val short = TextFeature("short", (box: TextBox) => if (box.text.length() < 5) 1 else -1)
	val startsWithDelimiting = LocalFeature("startWithDelimiting", (box: DocNode) =>
		{
		box match
		{
			case b: DelimitingBox   => 1
			case b: InternalDocNode =>
				{
				b.leaves match
				{
					case Nil                     => 0
					case (x: DelimitingBox) :: y => 1
					case _                       => 0
				}
				}
			case b                  => 0
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

	val emailRE    = ".*@.*\\.(com|org|net|edu|..)"
	val doiRE      = "10\\.\\d+\\/.+"
	val initialsRE = " [A-Z]\\.? "
	val digitsRE   = "[0-9]*"
	val dateRE1    = "((0?[1-9]|^1[0-2])\\/(0?[1-9]|[1-2][0-9]|3[0-1])\\/(19|20)?[0-9][0-9])"
	val monthRE    = "((jan(uary)?|feb(ruary)?|mar(ch)?|apr(il)?|may|jun(e)?|jul(y)?|aug(ust)?|sept(ember)?|oct(ober)?|nov(ember)?|dec(ember)?)"
	val dateRE2    = monthRE + " +([1-2][0-9]|3[0-1]|0?[1-9]),? +(19|20)[0-9][0-9])"
	val dateRE3    = "([1-2][0-9]|3[0-1]|0?[1-9]) +" + monthRE + ",? +(19|20)[0-9][0-9])"

	def contains(name: String, regex: String) =
		{
		LocalFeature(name, (box: TextBox) =>
			{
			val count = box.text.split(regex).length - 1
			count match
			{
				case 0 => 0.0f
				case _ => 1.0f
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
				case 0 => 0.0f
				case _ => 1.0f
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
				case 0 => 0.0f
				case _ => 1.0f
			}
			})
		}

	def textProportion(name: String, ss: Seq[String], min: Float, max: Float) = regexProportion(name, ss.mkString("|"), min, max)

	def regexProportion(name: String, regex: String, min: Float, max: Float) =
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
					val prop = (t.length - t.split(regex).mkString.length) / t.length.toFloat
					linearBetween(min, max)(prop)
					}
			}
			})
		}

	def lexiconProportion(name: String, lex: Lexicon, min: Float, max: Float) =
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
					val prop = m.mkString("").length.toFloat / t.length.toFloat
            
					linearBetween(min, max)(prop)
					}
			}
			})
		}

	val university = textProportion("AffiliationTerms", List("University", "Department", "Institute", "Center", "Laboratory"), .05f, .1f)

	val initials                  = textProportion("Initials", List(initialsRE), 0.05f, 0.15f)
	val digits                    = textProportion("Digits", List(digitsRE), 0.05f, 0.15f)
	val correspondence            = contains("CorrespondenceTerms", "whom correspondence")
	val metadataText              = contains("MetadataTerms", "ISSN|issn|\bDOI\b|\bdoi\b|[Cc]opyright")
	val abstractText              = startsWith("AbstractTerms", "Abstract")
	val referencesHeaderText      = contains("ReferencesHeaderTerms", "References|Literature|Cited")
	val acknowledgementHeaderText = contains("AcknowledgementHeaderTerms", "Acknowledgments|Acknowledgements")
	val referencesText            = contains("ReferencesTerms", "Technical report")
	val http                      = contains("http", "http://")
	val figureText                = startsWith("FigureTerms", "(Figure|Fig\\.|Table)")
	val headingText               = startsWith("HeadingTerms", "Introduction|Background|Results|Materials|Methods|Discussion|Conclusion")
	val email                     = contains("Email", emailRE)
	val date                      = containsAnyLC("Date", Seq(dateRE1, dateRE2, dateRE3))
	val doi                       = contains("DOI", doiRE)

	/*
   def countLexiconMatches(name: String, lex: Lexicon) =
	 {
	 LocalFeature(name, (box: TextBox) =>
	   {
	   lex.countMatches(box.text.toLowerCase.split(" "))
	   })
	 }
 */
	val firstname  = new CompoundFeature("FirstName", List(lexiconProportion("FirstNameMed", Lexicon.firstnameMed, 0.0f, 0.2f) -> 0.3f,
	                                                       lexiconProportion("FirstNameHigh", Lexicon.firstnameHigh, 0.0f, 0.2f) -> 0.6f,
	                                                       lexiconProportion("FirstNameHighest", Lexicon.firstnameHighest, 0.0f, 0.2f) -> 1.0f).toMap)
	val lastname   = new CompoundFeature("LastName", List(lexiconProportion("LastNameMed", Lexicon.lastnameMed, 0.0f, 0.2f) -> 0.3f,
	                                                      lexiconProportion("LastNameHigh", Lexicon.lastnameHigh, 0.0f, 0.2f) -> 0.6f,
	                                                      lexiconProportion("LastNameHighest", Lexicon.lastnameHighest, 0.0f, 0.2f) -> 1.0f).toMap)
	val noLastName = new FunctionFeature("NoLastName", lastname,
	                                     {
	                                     x =>
		                                     {
		                                     linearBetween(.95f, .99f)(1f - x)
		                                     }
	                                     })

	val cities = lexiconProportion("cities", Lexicon.cities, 0.05f, 0.1f)
	val states = lexiconProportion("states", Lexicon.states, 0.05f, 0.1f)

	val scoringFunctions =
		List(ScoringFunction("title", nearBeginning -> 2, notNearBeginning -> -2, nearPageTop -> 1, largeFont -> 1, veryLargeFont -> 2,
		                     highlyPunctuated -> -1,
		                     lengthBetween(30, 200) -> 2, lengthAbove(200) -> -2, longColumnWidth -> 2),
		     ScoringFunction("authors", firstname -> 2, lastname -> 4, noLastName -> -2, nearBeginning -> 1.5f, nearPageTop -> .5f, largeFont -> .5f,
		                     dominantFont -> -1, highlyPunctuated -> 2, lengthBetween(10, 1000) -> 1, initials -> 5, digits -> -10, longColumnWidth -> 2),
		     ScoringFunction("affiliations", cities -> 1, states -> 1, nearBeginning -> 1.4f, dominantFont -> -1, highlyPunctuated -> 2,
		                     lengthBetween(10, 1000) -> 1, university -> 5, referencesText -> -5),
		     ScoringFunction("contactinfo", correspondence -> 3, email -> 10, dominantFont -> -1, highlyPunctuated -> 2, lengthBetween(10, 1000) -> 1),
		     ScoringFunction("abstract", abstractText -> 10, nearBeginning -> 2, nearPageTop -> .5f, largeFont -> .2f, dominantFont -> -1,
		                     highlyPunctuated -> -2, lengthBetween(100, 5000) -> 1, initials -> -5, longColumnWidth -> 2),
		     ScoringFunction("caption", figureText -> 10, nearBeginning -> -1, smallFont -> 1, dominantFont -> -1, lengthBetween(100, 5000) -> 1,
		                     startsWithDelimiting -> .5f),
		     ScoringFunction("heading", nearBeginning -> -.5f, bottomMargin -> -10, lengthBetween(5, 100) -> 1, lengthAbove(150) -> -5, largeFont -> 2,
		                     veryLargeFont -> -2, highlyPunctuated -> -2, headingText -> 10),
		     ScoringFunction("body", dominantFont -> 3, dominantColumnWidth -> 1, highlyPunctuated -> -2),
		     ScoringFunction("references", firstname -> 2, lastname -> 4, noLastName -> -2, referencesHeaderText -> 110, referencesText -> 5, nearEnd -> 2,
		                     nearBeginning -> -2, dominantFont -> -1, smallFont -> 2, highlyPunctuated -> 2, http -> 1),
		     ScoringFunction("codeOrData", lastname -> -2, dominantFont -> -1, highlyPunctuated -> 4),
		     ScoringFunction("header", topMargin -> 3, notInMargin -> -4, lengthBetween(10, 200) -> 1, highlyLandscape -> 1, veryLargeFont -> -2),
		     ScoringFunction("footnote", bottomMargin -> 3, smallFont -> 1, highlyPunctuated -> -1, lengthBetween(10, 200) -> 1, startsWithDelimiting -> .1f),
		     ScoringFunction("footer", bottomMargin -> 3, notInMargin -> -4, lengthBetween(10, 200) -> 1, highlyLandscape -> 1),
		     ScoringFunction("acknowledgements", acknowledgementHeaderText -> 100, nearEnd -> 1, nearBeginning -> -1, lengthBetween(10, 200) -> 1,
		                     firstname -> 2, lastname -> 2, referencesText -> -10),
		     ScoringFunction("metadata", doi -> 10, metadataText -> 10, date -> 1, http -> 1),
		     ScoringFunction("discard", sideways -> 10, lengthBetween(0, 5) -> 10),
		     ScoringFunction("end", lastNode -> 15, isNonMarginFullPageDelimiter -> 15)) //nearEnd -> .1, //isDelimiter -> 3,
	}

