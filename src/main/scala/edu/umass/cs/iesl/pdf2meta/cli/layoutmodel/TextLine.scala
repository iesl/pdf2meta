package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

import collection.Seq
import edu.umass.cs.iesl.pdf2meta.cli.util.Util

class TextLine(override val id: String, val theText: String, fonts: Seq[String], rectangles: List[RectangleOnPage])
        extends DocNode(id, Seq.empty, None, None, true)
  {

  override def mkString(d: String) = theText

  override lazy val rectangle: Option[RectangleOnPage] = RectangleOnPage.encompassing(rectangles, 0)
  //.getOrElse(throw new Error("TextLine without rectangle"))
  // round heights to nearest .05 points
  private lazy val heights = rectangles.map(_.height) //rectangles.map(x => (x.height * 20.0).round / 20.0)
  lazy val fontsWithHeights: List[FontWithHeight] =
    {
    if (fonts.length == 1)
      {
      val uniqueHeights = Set(heights)
      require(uniqueHeights.size == 1)
      List(new FontWithHeight(fonts.head, heights.head))
      }
    else
      {
      require(fonts.length == text.length)
      require(fonts.length == heights.length)
      val t: List[(String, Double)] = (fonts zip heights).toList
      t.map(x => new FontWithHeight(x._1, x._2))
      }
    };

  override lazy val dominantFont: Option[FontWithHeight] =
    {
    if (fontsWithHeights.isEmpty) Some(new FontWithHeight("None", 0))
    else
      {
      val fontCounts = Util.histogram(fontsWithHeights)

      Some(fontCounts.toSeq.sortWith((a, b) => a._2 > b._2).head._1)
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
  override def partitionByFont(boxorder: Ordering[RectangularOnPage]) =
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
  def fontSize = rectangle.get.height

  override def create(childrenA: Seq[DocNode]) =
    {
    new TextLine(id, text, fonts, rectangles)
    }
  }

















