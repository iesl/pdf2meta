package edu.umass.cs.iesl.pdf2meta.cli.layout

import collection.Seq

trait TextContainer
  {
  def text: String
  }

trait HasFontInfo extends TextContainer
  {
  // font id, height
  type FontWithHeight = (String, Double);
  def dominantFont: FontWithHeight
  def partitionByFont(boxorder: Ordering[Rectangular]): Seq[TextContainer with HasFontInfo]
  }


class RectBox(id: String, override val rectangle: Rectangle) extends LayoutRectangle(id, Seq.empty)

class CurveBox(id: String, override val rectangle: Rectangle) extends LayoutRectangle(id, Seq.empty)



