package edu.umass.cs.iesl.pdf2meta.cli.extract

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._

import com.typesafe.scalalogging.slf4j.Logging
import scala.Function1
import scala.sys.process.{Process, ProcessIO}
import java.util.Date
import tools.nsc.io.JFile
import edu.umass.cs.iesl.scalacommons.{ListUtils, StatsUtils, Workspace, IOUtils}

object PdfMinerExtractor {
  lazy val executable = {
    val tempPythonLocation = "/tmp/pdf2meta/rexapdfminer.py"
    val py = getClass.getResourceAsStream("/rexapdfminer.py")
    val tempPythonFile = new JFile(tempPythonLocation)
    tempPythonFile.getParentFile.mkdirs()
    IOUtils.copy(py, tempPythonFile)
    tempPythonFile.setExecutable(true)
    tempPythonLocation
  }
}

class PdfMinerExtractor extends PdfExtractor with Logging with (Workspace => DocNode) {

  def apply(w: Workspace): DocNode = {
    lazy val command = PdfMinerExtractor.executable + " --file " + w.file  // python + " " +
    lazy val output = {
      logger.debug("Starting PdfMiner...")
      val startTime = new Date
      logger.debug("Running PdfMiner in " + w.dir.jfile)

      val pb = Process(command, w.dir.jfile)
      val outSB = StringBuilder.newBuilder
      val errSB = StringBuilder.newBuilder
      val pio = new ProcessIO(_ => (),
        stdout => scala.io.Source.fromInputStream(stdout).getLines().foreach(outSB append _ append "\n"),
        stderr => scala.io.Source.fromInputStream(stderr).getLines().foreach(errSB append _ append "\n"))
      val p = pb.run(pio)
      val exitCode = p.exitValue()

      logger.debug("PdfMiner done ")
      val endTime = new Date()

      logger.debug("PdfMiner took " + ((endTime.getTime - startTime.getTime)) + " milliseconds")
      val out = outSB toString()
      val err = errSB toString()
      if (!err.isEmpty) {
        logger.warn(err)
      }
      logger.debug(out)
      out
    }

    lazy val outfile = {
      output // just trigger the lazy evaluation

      //w.dir + "/" + w.filename + ".d/pdfminer/" + w.file.segments.last + ".pdfminer.xml"
      w.dir + "/" + w.file.segments.last + ".pdfminer.xml"
    }

    lazy val xml = scala.xml.XML.loadFile(outfile)

    val lengthR = """(.*),(.*),(.*)""".r
    val cid = """\(cid:.*?\)"""


    val rawPages = {
      logger.debug("Starting XML parsing...")
      val startTime = new Date()
      logger.debug("Starting XML parsing...")

      val result = for (page <- xml \\ "page") yield {
        val pageid = (page \ "@pageid").text
        val pagenum = pageid.toInt

        val pageRect: Rectangle = Rectangle((page \ "@bbox").text).getOrElse(throw new Error("Page without bounding box, abort"))
        val thePage = new Page(pagenum, pageRect)

        val textboxes = for (box <- page \ "textbox") yield {
          val textboxid = pageid + "." + (box \ "@index").text
          var lineid = 0
          val lines =
            for (line <- box \ "textline") yield {
              val fonts = (line \ "@fonts").text
              val bboxes = (line \ "@bboxes").text
              val lengthStr = (line \ "@length").text

              if (bboxes.isEmpty) {
                None
              }
              else {
                val bboxRects = bboxes.split(" ").map(RectangleOnPage(thePage, _)).toList.flatten
                if (bboxRects.isEmpty) {
                  // this happens if there were bounding boxes, but all of them turned out to be bogus (hence None)
                  None
                }
                else {
                  val id = textboxid + "." + lineid
                  lineid += 1

                  val (text: String, length: Int) =
                    try {
                      val lengthR(ccs, bbs, ffs) = lengthStr
                      // if we got here, it's because there was a cid thingy

                      (line.text.replaceAll(cid, "*"), bbs.toInt)
                    }
                    catch {
                      case e: MatchError => (line.text, lengthStr.toInt)
                    }

                  require(length == text.length)
                  require(!text.contains("(cid"))
                  Some(new PdfMinerTextLine(id, text, fonts.split(" "), bboxRects))
                }
              }
            }
          val lf: Seq[DocNode] = lines.flatten

          lf match {
            case Nil => None;
            case _ => Some(new InternalDocNode(textboxid, lf, None, None))
          }
        }
        var rectid = 0
        val rects = (for (rect <- page \ "rect") yield {
          rectid += 1
          val bbox: String = (rect \ "@bbox").text
          if (bbox.trim().isEmpty) None
          else RectangleOnPage(thePage, bbox).map((r => new RectBox("r-" + pageid + "." + rectid.toString, r)))
        })
        var curveid = 0
        val curves = (for (curve <- page \ "curve") yield {
          curveid += 1
          val bbox: String = (curve \ "@bbox").text
          if (bbox.trim().isEmpty) None
          else RectangleOnPage(thePage, bbox).map(r => new CurveBox("c-" + pageid + "." + curveid.toString, r))
        })
        var figureid = 0
        val figures = (for (figure <- page \ "figure") yield {
          figureid += 1
          val bbox: String = (figure \ "@bbox").text
          if (bbox.trim().isEmpty) None
          else RectangleOnPage(thePage, bbox).map(r => new FigureBox("f-" + pageid + "." + figureid.toString, r))
        })

        //val topOfPageRect =new RectBox("top-"+pageid, RectangleOnPage(thePage,pageRect.topEdge).get)
        //val bottomOfPageRect = new RectBox("bottom-"+pageid,RectangleOnPage(thePage,pageRect.bottomEdge).get)

        new InternalDocNode(pageid, (//topOfPageRect :: bottomOfPageRect ::
          textboxes.flatten.toList :::
            rects.flatten.toList :::
            curves.flatten.toList ::: figures.flatten.toList), None, None) {
          override lazy val rectangle: Option[RectangleOnPage] = RectangleOnPage(thePage, pageRect)
        }
      }
      logger.debug("XML parsing done...")
      val parseTime = new Date()
      logger.debug("XML parsing took " + ((parseTime.getTime - startTime.getTime)) + " milliseconds")
      result
    }

    //need to add an empty node, but only to the last page
    val allPagesWithEmptyEndNode = {
      val r = rawPages.reverse
      val lastPage = r.head
      val lastPageRect: RectangleOnPage = lastPage.rectangle.get
      val bottomOfPageRect = new RectBox("bottom-" + lastPage.id, RectangleOnPage(lastPageRect.page, lastPageRect.bottomEdge).get)
      ((lastPage :+ bottomOfPageRect) :: r.tail.toList).reverse
    }

    InternalDocNode(w.filename, allPagesWithEmptyEndNode, Some(List(command, output).iterator), None)
  }
}


/**
 * A TextLine where the constituent atoms may have already been lost (i.e., PdfMiner has already merged them)
 * @param id
 * @param theText
 * @param fonts
 * @param rectangles
 */
class PdfMinerTextLine(override val id: String, override val theText: String, fonts: Seq[String], rectangles: List[RectangleOnPage])
		extends TextLine(id, theText,RectangleOnPage.encompassing(rectangles, 0))
	{

	// by encompassing only rectangles for the dominant-font glyphs we avoid problems with ordering the rectangles when there are drop caps, superscripts,
	// etc.
	// we may introduce other ordering problems, though.  Let's try it...
	override lazy val coreRectangle: Option[RectangleOnPage] = RectangleOnPage.encompassing(dominantFontRectangles, 0)

	private lazy  val heights                              = rectangles.map(_.height)
	override lazy val allFonts: Seq[(FontWithHeight, Int)] =
		{
		if (fonts.length == 1)
			{
			val uniqueHeights = Set(heights)
			require(uniqueHeights.size == 1)
			if (heights.head == 0.0)
				{List()}
			else
				{
				List((new FontWithHeight(fonts.head, heights.head), text.length))
				}
			}
		else
			{
			require(fonts.length == text.length)
			require(fonts.length == heights.length)
			val t: List[(String, Float)] = (fonts zip heights).toList
			t.filter(_._2 != 0.0).map(x => (new FontWithHeight(x._1, x._2), 1))
			}
		};

	lazy val dominantFontRectangles: List[RectangleOnPage] = dominantFont.map(dFont => rectanglesWithFonts.filter(_._2 == dFont).map(_._1))
	                                                         .getOrElse(rectangles)

	override lazy val dominantFont: Option[FontWithHeight] =
		{
		if (allFonts.isEmpty) None
		else
			{
			val fontCounts = StatsUtils.histogramAccumulate(allFonts)

			Some(fontCounts.toSeq.sortWith((a, b) => a._2 > b._2).head._1)
			}
		}

	lazy val rectanglesWithFonts: List[(RectangleOnPage, FontWithHeight)] =
		{
		if (allFonts.length == 0)
			{List()}
		else if (allFonts.length == 1)
			{rectangles.map((_, allFonts.head._1))}
		else
			{
			rectangles.zip(allFonts.map(_._1))
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
			new PdfMinerTextLine(subid, substring, subfonts, subrects)
			}
		}

	override def partitionByFont(boxorder: Ordering[RectangularOnPage]) =
		{
		if (allFonts.isEmpty)
			{
			require(text.isEmpty)
			List()
			}
		else if (allFonts.length == 1)
			{
			List(this)
			}
		else
			{
			// if there are more than 5 characters in a row of the same font,
			// make a separate TextBox containing just those
			// characters, with a newly computed bounding box.
			// any characters in font-blocks <= 5 characters are included in the following block
			val runs = ListUtils.collapseShortRuns(ListUtils.contiguousRuns(allFonts.map(_._1).toList)(x => x), 5)
			runsToTextLines(runs)
			// keep these in the preexisting order
			}
		}

	// this might be thrown off by superscripts etc.
	def fontSize = rectangle.get.height

	override def create(childrenA: Seq[DocNode]) =
		{
		assert(childrenA.isEmpty)
		this
		}
	}





