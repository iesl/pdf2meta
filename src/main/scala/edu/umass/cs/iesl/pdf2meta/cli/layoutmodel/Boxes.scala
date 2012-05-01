package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

import collection.Seq

trait TextContainer {
  def text: String = mkString(" ")

  def mkString(d: String): String
}

class FontWithHeight(val fontid: String, rawheight: Float) {
  val height = (rawheight * 10.0f).round / 10.0f

  override def equals(p1: Any) = {
    p1 match {
      case x: FontWithHeight => (fontid == x.fontid && height == x.height)
      case _ => false
    }
  }

  override def hashCode: Int = 41 * (41 + fontid.hashCode) + height.hashCode()

  def equalsWithinOneQuantum(p1: FontWithHeight): Boolean = {
    p1 match {
      case x: FontWithHeight => (fontid == x.fontid && (height - x.height).abs <= 0.1)
      case _ => false
    }
  }


  def sizeEqualsWithinOneQuantum(p1: FontWithHeight): Boolean = sizeEqualsWithin(0.1f)(p1)

  def sizeEqualsWithin(epsilon: Float)(p1: FontWithHeight): Boolean = {
    p1 match {
      case x: FontWithHeight => ((height - x.height).abs <= epsilon)
      case _ => false
    }
  }

  override def toString = fontid + " " + height
}

trait HasFontInfo extends TextContainer {
  def dominantFont: Option[FontWithHeight]
}


class DelimitingBox(id: String,  val theRectangle: RectangleOnPage)
  extends LeafNode(id,None, None, Some(theRectangle)) {

  override def create(childrenA: Seq[DocNode]) = {
    assert(childrenA.isEmpty)
    this
  }
override def printTree(prefix: String): String = prefix + "DELIMITER\n"
}

class RectBox(id: String, override val theRectangle: RectangleOnPage)
  extends DelimitingBox(id, theRectangle)

class CurveBox(id: String, override val theRectangle: RectangleOnPage)
  extends DelimitingBox(id, theRectangle)

class FigureBox(id: String, override val theRectangle: RectangleOnPage)
  extends DelimitingBox(id, theRectangle)

class WhitespaceBox(id: String, override val theRectangle: RectangleOnPage) extends DelimitingBox(id, theRectangle) {

override def printTree(prefix: String): String =
		{
		val buf = new StringBuilder(prefix)
		val w: Float = this.rectangle.get.width
		val h: Float = this.rectangle.get.height
		buf.append(if (w > h) "HORIZONTAL" else "VERTICAL")
		buf.append(" WHITESPACE: " + w + " x " + h + " : " + this.rectangle.get + "\n")
		buf.toString()
		}

  override def create(childrenA: Seq[DocNode]) = {
    assert(childrenA.isEmpty)
    this
  }

}


