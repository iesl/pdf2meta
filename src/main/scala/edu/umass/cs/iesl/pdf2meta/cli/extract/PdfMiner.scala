package edu.umass.cs.iesl.pdf2meta.cli.extract

import sys.process.{ProcessIO, Process}
import com.weiglewilczek.slf4s.Logging
import collection.immutable.Seq
import scala.Predef._
import edu.umass.cs.iesl.pdf2meta.cli.util.Workspace
import edu.umass.cs.iesl.pdf2meta.cli.layout._

/*
 * Created by IntelliJ IDEA.
 * User: lorax
 * Date: 9/2/11
 * Time: 11:50 AM
 */
class PdfMiner(w: Workspace) extends Logging
  {


  lazy val command = "/Users/lorax/iesl/rexa-textmill/bin/rexapdfminer.py --file " + w.file
  lazy val output =
    {
    val pb = Process(command)
    val sb = StringBuilder.newBuilder
    val pio = new ProcessIO(_ => (), stdout => scala.io.Source.fromInputStream(stdout).getLines().foreach(sb append _),
                            _ => ())
    val p = pb.run(pio)
    val exitCode = p.exitValue()
    sb toString()
    }

  lazy val outfile =
    {
    output // just trigger the lazy evaluation
    w.file + ".d/pdfminer/" + w.file.segments.last + ".pdfminer.xml"
    }

  // lazy val outlines = scala.io.Source.fromFile(outfile).mkString
  lazy val xml = scala.xml.XML.loadFile(outfile)

  val lengthR = """(.*),(.*),(.*)""".r
  //  val cid = """(.*)\(cid:.*\)(.*)""" .r
  val cid = """\(cid:.*?\)"""


  //val pageorder = new PageReadingOrder(errors)
  //var startline = 0;
  val rawPages = for (page <- xml \\ "page") yield
    {
    //val errors = boxorder.errors
    val pageid = (page \ "@pageid").text
    val textboxes = for (box <- page \ "textbox") yield
      {
      val textboxid = pageid + "." + (box \ "@index").text
      var lineid = 0
      val lines =
        for (line <- box \ "textline") yield
          {
          //val bbox = new BoundingBox((line \ "@bbox").text)
          //val bbox = (line \ "@bbox").text
          val fonts = (line \ "@fonts").text
          val bboxes = (line \ "@bboxes").text
          val lengthStr = (line \ "@length").text

          if (bboxes.isEmpty)
            {
            None
            }
          else
            {
            //val bboxRect = Rectangle(bbox)
            val bboxRects = bboxes.split(" ").map(Rectangle(_)).toList.flatten
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
      // val bbox = new BoundingBox((box \ "@bbox").text) //with BoxReadingOrder
      // val bbox = (box \ "@bbox").text
      // val bboxRect = Rectangle(bbox)
      val lf: Seq[LayoutRectangle] = lines.flatten

      lf match
      {
        case Nil => None;
        case _ => Some(new TextBox(textboxid, lf))
      }
      }
    var rectid = 0
    val rects = (for (rect <- page \ "rect") yield
      {
      rectid += 1
      val bbox: String = (rect \ "@bbox").text
      if (bbox.trim().isEmpty) None
      else Rectangle(bbox).map((r => new RectBox("r-" + pageid + "." + rectid.toString, r)))
      })
    var curveid = 0
    val curves = (for (curve <- page \ "curve") yield
      {
      curveid += 1
      val bbox: String = (curve \ "@bbox").text
      if (bbox.trim().isEmpty) None
      else Rectangle(bbox).map(r => new CurveBox("c-" + pageid + "." + curveid.toString, r))
      })

    Rectangle((page \ "@bbox").text)
    .map(r => new Page(pageid.toInt, (textboxes.flatten.toList ::: rects.flatten.toList ::: curves.flatten.toList), r))
    .getOrElse(throw new Error("Page without bounding box, abort"))
    }

  val pages: List[Page] = rawPages.map(_.regroup).toList

  // couple ways to do this:
  // 1.  assemble all the raw strings, then zipWithIndex
  // 2.  grab the already-numbered textLines from each page, and merge them
  // 3.  for-comprehension as follows
  // val numberedTextLines = for (page <- pages.sorted; line <- page.textLines) yield line
  //val numberedTextLinesByPage =
  //val textBoxes = pages.map(x=>x.textBoxes.map(y => (x, y))).flatten
  }
