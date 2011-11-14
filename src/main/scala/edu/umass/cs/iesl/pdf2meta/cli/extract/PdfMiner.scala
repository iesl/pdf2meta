package edu.umass.cs.iesl.pdf2meta.cli.extract

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._

import com.weiglewilczek.slf4s.Logging
import scala.Function1
import scala.sys.process.{Process, ProcessIO}
import java.util.Date
import edu.umass.cs.iesl.pdf2meta.cli.util.{Util, Workspace}
import tools.nsc.io.JFile

object PdfMiner
  {
  lazy val executable =
    {
    val tempPythonLocation = "/tmp/pdf2meta/rexapdfminer.py"
    val py = getClass.getResourceAsStream("/rexapdfminer.py")
    val tempPythonFile = new JFile(tempPythonLocation)
    tempPythonFile.getParentFile.mkdirs()
    Util.copy(py, tempPythonFile)
    tempPythonFile.setExecutable(true)
    tempPythonLocation
    }
  }

class PdfMiner extends XmlExtractor with Logging with Function1[Workspace, DocNode]
  {

  def apply(w: Workspace): DocNode =
    {
    lazy val command = PdfMiner.executable + " --file " + w.file
    lazy val output =
      {
      logger.debug("Starting PdfMiner...")
      val startTime = new Date
      logger.debug("Running PdfMiner...")

      val pb = Process(command)
      val sb = StringBuilder.newBuilder
      val pio = new ProcessIO(_ => (), stdout => scala.io.Source.fromInputStream(stdout).getLines().foreach(sb append _), _ => ())
      val p = pb.run(pio)
      val exitCode = p.exitValue()

      logger.debug("PdfMiner done ")
      val endTime = new Date()

      logger.debug("PdfMiner took " + ((endTime.getTime - startTime.getTime)) + " milliseconds")
      sb toString()
      }

    lazy val outfile =
      {
      output // just trigger the lazy evaluation
      w.dir + "/" + w.filename + ".d/pdfminer/" + w.file.segments.last + ".pdfminer.xml"
      }

    lazy val xml = scala.xml.XML.loadFile(outfile)

    val lengthR = """(.*),(.*),(.*)""".r
    val cid = """\(cid:.*?\)"""


    val rawPages =
      {
      logger.debug("Starting XML parsing...")
      val startTime = new Date()
      logger.debug("Starting XML parsing...")

      val result = for (page <- xml \\ "page") yield
        {
        val pageid = (page \ "@pageid").text
        val pagenum = pageid.toInt

        val pageRect: Rectangle = Rectangle((page \ "@bbox").text).getOrElse(throw new Error("Page without bounding box, abort"))
        val thePage = new Page(pagenum, pageRect)

        val textboxes = for (box <- page \ "textbox") yield
          {
          val textboxid = pageid + "." + (box \ "@index").text
          var lineid = 0
          val lines =
            for (line <- box \ "textline") yield
              {
              val fonts = (line \ "@fonts").text
              val bboxes = (line \ "@bboxes").text
              val lengthStr = (line \ "@length").text

              if (bboxes.isEmpty)
                {
                None
                }
              else
                {
                val bboxRects = bboxes.split(" ").map(RectangleOnPage(thePage, _)).toList.flatten
                if (bboxRects.isEmpty)
                  {
                  // this happens if there were bounding boxes, but all of them turned out to be bogus (hence None)
                  None
                  }
                else
                  {
                  val id = textboxid + "." + lineid
                  lineid += 1

                  val (text: String, length: Int) =
                    try
                    {
                    val lengthR(ccs, bbs, ffs) = lengthStr
                    // if we got here, it's because there was a cid thingy

                    (line.text.replaceAll(cid, "*"), bbs.toInt)
                    }
                    catch
                    {
                    case e: MatchError => (line.text, lengthStr.toInt)
                    }

                  require(length == text.length)
                  require(!text.contains("(cid"))
                  Some(new TextLine(id, text, fonts.split(" "), bboxRects))
                  }
                }
              }
          val lf: Seq[DocNode] = lines.flatten

          lf match
          {
            case Nil => None;
            case _ => Some(new DocNode(textboxid, lf, None, None))
          }
          }
        var rectid = 0
        val rects = (for (rect <- page \ "rect") yield
          {
          rectid += 1
          val bbox: String = (rect \ "@bbox").text
          if (bbox.trim().isEmpty) None
          else RectangleOnPage(thePage, bbox).map((r => new RectBox("r-" + pageid + "." + rectid.toString, r)))
          })
        var curveid = 0
        val curves = (for (curve <- page \ "curve") yield
          {
          curveid += 1
          val bbox: String = (curve \ "@bbox").text
          if (bbox.trim().isEmpty) None
          else RectangleOnPage(thePage, bbox).map(r => new CurveBox("c-" + pageid + "." + curveid.toString, r))
          })
        var figureid = 0
        val figures = (for (figure <- page \ "figure") yield
          {
          figureid += 1
          val bbox: String = (figure \ "@bbox").text
          if (bbox.trim().isEmpty) None
          else RectangleOnPage(thePage, bbox).map(r => new FigureBox("f-" + pageid + "." + figureid.toString, r))
          })

        //val topOfPageRect =new RectBox("top-"+pageid, RectangleOnPage(thePage,pageRect.topEdge).get)
        //val bottomOfPageRect = new RectBox("bottom-"+pageid,RectangleOnPage(thePage,pageRect.bottomEdge).get)

        new DocNode(pageid, (//topOfPageRect :: bottomOfPageRect ::
                            textboxes.flatten.toList :::
                            rects.flatten.toList :::
                            curves.flatten.toList ::: figures.flatten.toList), None, None)
          {
          override lazy val rectangle: Option[RectangleOnPage] = RectangleOnPage(thePage, pageRect)
          }
        }
      logger.debug("XML parsing done...")
      val parseTime = new Date()
      logger.debug("XML parsing took " + ((parseTime.getTime - startTime.getTime)) + " milliseconds")
      result
      }

    //need to add an empty node, but only to the last page
    val allPagesWithEmptyEndNode =
      {
      val r = rawPages.reverse
      val lastPage = r.head
      val lastPageRect: RectangleOnPage = lastPage.rectangle.get
      val bottomOfPageRect = new RectBox("bottom-" + lastPage.id, RectangleOnPage(lastPageRect.page, lastPageRect.bottomEdge).get)
      ((lastPage :+ bottomOfPageRect) :: r.tail.toList).reverse
      }

    DocNode(w.filename, allPagesWithEmptyEndNode, Some(List(command, output).iterator), None)
    }
  }
