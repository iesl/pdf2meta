package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

import collection.Seq
import edu.umass.cs.iesl.scalacommons.{ListUtils, StatsUtils}

trait TextBox extends HasFontInfo
  {
  self: DocNode =>

  override def toString = text.substring(text.length.max(30).min(0))

  def spanText =
    {
    if (text.length < 80)
      {text}
    else
      {
      text.substring(0, 37) + " ... " + text.substring(text.length - 37, text.length)
      }
    }

  // todo make lazy
  def mkString(d: String): String =
    {
    (for (x <- secretChildren.collect
               {case x: HasFontInfo => x}) yield
      {x.text}).mkString(d)
    }

  lazy val allFonts: Seq[(FontWithHeight, Int)] =
    {
    if (secretChildren.isEmpty)
      {
      // Must be a RectBox or something, with no fonts in it
      Seq()
      }
    else
      {
      assert(!isSecretLeaf)
      val leafFonts = allSecretLeaves.filter(_.dominantFont.isDefined).map(leaf => (leaf.dominantFont.get, leaf.text.size))
      StatsUtils.histogramAccumulate(leafFonts).toSeq
      }
    }

  lazy val dominantFont: Option[FontWithHeight] =
    {
    allFonts.length match
    {
      case 0 => None
      case _ =>
        {
        val bestFont = allFonts.toSeq.sortWith((a, b) => a._2 > b._2).head
        if (bestFont._2 > text.length() * .5)
          {
          Some(bestFont._1)
          }
        else None
        }
    }
    }

  lazy val dominantFontHeight = {
    dominantFont.map(_.height).getOrElse(9.0)
  }

  lazy val allTextLineWidths: Seq[(Double, Int)] =
    {
    if (secretChildren.isEmpty)
      {
      // Must be a RectBox or something, with no fonts in it
      Seq()
      }
    else
      {
      // round up to the nearest 10; this is just a rough estimate of column width
      val lineWidthsWithLength = textLines.map(t => (t.rectangle.map(r => ((r.width / 10.0).ceil * 10.0)).getOrElse(0.0), t.text.length))
      StatsUtils.histogramAccumulate(lineWidthsWithLength).toSeq
      }
    }

  lazy val dominantTextLineWidth: Option[Double] =
    {
    textLines.length match
    {
      case 0 => None
      case _ =>
        {
        val sortedTextLineWidths: Seq[(Double, Int)] = allTextLineWidths.toSeq.sortWith((a, b) => a._2 > b._2)
        if (sortedTextLineWidths.isEmpty)
          {
          None
          }
        else
          {
          val bestLineWidth = sortedTextLineWidths.head
          if (bestLineWidth._2 > text.length() * .5)
            {
            Some(bestLineWidth._1)
            }
          else None
          }
        }
    }
    }


  // first put all the font blocks from the children on equal footing
  def partitionByFont(boxorder: Ordering[RectangularOnPage]): Seq[DocNode] =
    {

    val s: Seq[DocNode] = children.map(_.partitionByFont(boxorder)).flatten
    val fontSplitChildren: Seq[DocNode] = s.map(_.children).flatten
    val sortedChildren = fontSplitChildren.sorted(boxorder)

    // then regroup them
    // merge if the blocks sort adjacently AND are in the same column
    // we don't want blocks in two columns to merge because the encompassing box will be interpreted wrongly wrt
    // ordering one level up
    type FontAndLeft = (FontWithHeight, Double)
    val fontBlocks: List[(((String, Double), List[DocNode]), Int)] =
      {
      val fontAndLeftMatch: (DocNode => (String, Double)) = ((x: DocNode) =>
        {
        (x.dominantFont.get.fontid, x.rectangle.get.left) // ignore the font height
        })

      val runs: List[((String, Double), List[DocNode])] = ListUtils.contiguousRuns(sortedChildren.toList)(fontAndLeftMatch)
      runs.zipWithIndex
      };

    fontBlocks.map({
                   case ((key, blocks), index) => DocNode(id + "." + index, blocks, None, None)
                   })

    // the regrouped boxes are still in order, but now only TextBoxes are provided
    }
  }

















