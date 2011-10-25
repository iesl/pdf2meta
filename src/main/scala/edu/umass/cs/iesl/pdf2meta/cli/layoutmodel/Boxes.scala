package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

import collection.Seq

trait TextContainer
  {
  def text: String
  }

// font id, height
class FontWithHeight(val fontid: String, rawheight: Double)
  {
  val height = (rawheight * 10.0).round / 10.0
  override def equals(p1: Any) =
    {
    p1 match
    {
      case x: FontWithHeight => (fontid == x.fontid && height == x.height)
      case _ => false
    }
    }

  override def hashCode : Int = 41 * (41 + fontid.hashCode) + height.hashCode()

  def equalsWithinOneQuantum(p1: FontWithHeight) : Boolean =
    {
    p1 match
    {
      case x: FontWithHeight => (fontid == x.fontid && (height - x.height).abs <= 0.1)
      case _ => false
    }
    }
  override def toString = fontid + " " + height
  }

trait HasFontInfo extends TextContainer
  {
  def dominantFont: Option[FontWithHeight]
  //def partitionByFont(boxorder: Ordering[Rectangular]): Seq[TextContainer with HasFontInfo]
  }


class DelimitingBox(id: String, val theRectangle: RectangleOnPage)
        extends DocNode(id, Seq.empty, None, None,true)
  {
  override def computeRectangle = Some(theRectangle)
  //  override def computePage = Some(thePage)
  }

class RectBox(id: String, override val theRectangle: RectangleOnPage)
        extends DelimitingBox(id, theRectangle)

class CurveBox(id: String, override val theRectangle: RectangleOnPage)
        extends DelimitingBox(id, theRectangle)

class FigureBox(id: String, override val theRectangle: RectangleOnPage)
        extends DelimitingBox(id, theRectangle)



