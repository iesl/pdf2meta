package edu.umass.cs.iesl.pdf2meta.cli.layout

import collection.Seq
import edu.umass.cs.iesl.pdf2meta.webapp.lib.pdf.util.Util

class TextLine(override val id: String, override val text: String, fonts: Seq[String], rectangles: List[Rectangle])
        extends TextBox(id, Seq.empty)
  {
  override lazy val rectangle: Rectangle = Rectangle.encompassing(rectangles, 0)
                                           .getOrElse(throw new Error("TextLine without rectangle"))
  private lazy val heights = rectangles.map(x => x.height)
  lazy val fontsWithHeights: List[FontWithHeight] =
    {
    if (fonts.length == 1)
      {
      val uniqueHeights = Set(heights)
      require(uniqueHeights.size == 1)
      List((fonts.head, heights.head))
      }
    else
      {
      require(fonts.length == text.length)
      require(fonts.length == heights.length)
      (fonts zip heights).toList
      }
    };

  override lazy val dominantFont: (String, Double) =
    {
    if (fontsWithHeights.isEmpty) ("None", 0)
    else
      {
      val fontCounts = fontsWithHeights groupBy (identity) map
                       {case (c, cs) => (c, cs.size)}
      fontCounts.toSeq.sortWith((a, b) => a._2 > b._2).head._1
      }
    }
  private def runsToTextLines(runs: List[(FontWithHeight, List[FontWithHeight])]) =
    {
    var index = 0
    var start = 0
    for (q <- runs) yield
      {
      val subid = id + "." + index
      val (c, charFonts) = q
      val end = start + charFonts.length
      val substring = text.substring(start, end)
      val subfonts = fonts.slice(start, end)
      val subrects = rectangles.slice(start, end)
      index += 1
      start = end
      new TextLine(subid, substring, subfonts, subrects)
      }
    }
  /*
  def splitByFontRaw =
    {
    val runs = Util.contiguousRuns(fonts.split(" ").toList)(x => x)
    runsToTextLines(runs)
    }
*/
  override def partitionByFont(boxorder: Ordering[Rectangular]) =
    {
    if (fontsWithHeights.isEmpty)
      {
      require(text.isEmpty)
      List()
      }

    if (fontsWithHeights.length == 1)
      {
      List(this)
      }
    else
      {
      // if there are more than 5 characters in a row of the same font, make a separate TextBox containing just those
      // characters, with a newly computed bounding box.
      // any characters in font-blocks <= 5 characters are included in the following block
      val runs = Util.collapseShortRuns(Util.contiguousRuns(fontsWithHeights.toList)(x => x), 5)
      runsToTextLines(runs)
      // keep these in the preexisting order
      }
    }

  // this might be thrown off by superscripts etc.
  def fontSize = rectangle.height
  }

















