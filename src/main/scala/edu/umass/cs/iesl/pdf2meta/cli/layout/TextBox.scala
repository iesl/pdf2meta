package edu.umass.cs.iesl.pdf2meta.cli.layout

import collection.Seq
import edu.umass.cs.iesl.pdf2meta.webapp.lib.pdf.util.Util

class TextBox(id: String, children: Seq[LayoutRectangle])
        extends LayoutRectangle(id, children) with HasFontInfo
  {

  override def toString = text.substring(text.length.max(30).min(0))

  def text: String =
    {
    (for (x <- children.collect
               {case x: HasFontInfo => x}) yield
      {x.text}).mkString(" ")
    }
  def dominantFont: FontWithHeight =
    {
    val fontCounts = (for (x <- children.collect
                                {case x: HasFontInfo => x}) yield
      {x.dominantFont}) groupBy (identity) map
                     {case (c, cs) => (c, cs.size)};
    fontCounts.toSeq.sortWith((a, b) => a._2 > b._2).head._1
    }


  // first put all the font blocks from the children on equal footing
  override def partitionByFont(boxorder: Ordering[Rectangular]): Seq[TextBox] = // children.map(_.splitByFont).flatten
    {
    // ignore any child that isn't a TextBox.
    // it never happens otherwise anyway, and if it did, the layout partitioning should cover that case
    val c: Seq[TextBox] = children.collect({case x: TextBox => x})
    val s: Seq[TextBox] = c.map(_.partitionByFont(boxorder)).flatten
    val fontSplitChildren: Seq[TextBox] = s.map(_.children).flatten.collect(
    {case x: TextBox => x}) // when TextBoxes were collections, this was s.flatten.collect
    // then order them
    val sortedChildren = fontSplitChildren.sorted(boxorder)

    // then regroup them
    // merge if the blocks sort adjacently AND are in the same column
    // we don't want blocks in two columns to merge because the encompassing box will be interpreted wrongly wrt
    // ordering one level up
    type FontAndLeft = (FontWithHeight, Double)
    val fontBlocks: List[(((String, Double), List[TextBox]), Int)] =
      {
      val fontAndLeftMatch: (TextBox => (String, Double)) = ((x: TextBox) =>
        {
        (x.dominantFont._1, x.rectangle.left) // ignore the font height
        })
      // HasFontInfo
      val runs: List[((String, Double), List[TextBox])] = Util.contiguousRuns(sortedChildren.toList)(fontAndLeftMatch)
      runs.zipWithIndex
      };

    fontBlocks.map({
                   case ((key, blocks), index) => new TextBox(id + "." + index, blocks)
                   })

    // the regrouped boxes are still in order
    }
  }

















