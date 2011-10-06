package edu.umass.cs.iesl.pdf2meta.cli.layout

import collection.mutable.ArrayBuffer
import collection.Seq

class Page(val pagenumber: Int,
           val layoutRects: Seq[LayoutRectangle], // val rects: Seq[RectBox], val curves: Seq[CurveBox],
           override val rectangle: Rectangle, val errors: ArrayBuffer[String])
        extends TextBox(pagenumber.toString, layoutRects) with Ordered[Page]
  {

  def allLayoutRects: scala.Seq[LayoutRectangle] =
    {
    allNodes.collect({case x: TextLine => None; case x: TextBox => None; case x: LayoutRectangle => Some(x)}).flatten
    }
  def textBoxes: scala.Seq[TextBox] =
    {
    allNodes.collect({case x: TextLine => None; case x: TextBox => Some(x)}).flatten
    }
  def rects: scala.Seq[RectBox] = allNodes.collect({case x: RectBox => x})

  def compare(that: Page) = pagenumber compare that.pagenumber

  def this(id: Int, textBoxes: Seq[LayoutRectangle], rectangle: Rectangle) =
    {
    this (id, textBoxes, rectangle, new ArrayBuffer[String])
    }

  val boxorder = new RectangularReadingOrder
    {
    def handleOrderingError(message: String)
      {errors += message}
    }
  val soup = LayoutRectangle(pagenumber.toString, allLeaves)
  val orderedBoxes = soup.partition(boxorder) //partitionByDelimiters(boxorder)
  //.partitionByLayout(boxorder)
  //.partitionByFont(boxorder)
  def regroup: Page = new Page(pagenumber, orderedBoxes.children, rectangle, errors) //rects, curves,
  }

















