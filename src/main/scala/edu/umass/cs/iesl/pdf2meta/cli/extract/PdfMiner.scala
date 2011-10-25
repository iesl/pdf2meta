package edu.umass.cs.iesl.pdf2meta.cli.extract

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._

import com.weiglewilczek.slf4s.Logging
import scala.Function1
import edu.umass.cs.iesl.pdf2meta.cli.util.Workspace

import scala.sys.process.{Process, ProcessIO}

class PdfMiner extends XmlExtractor with Logging with Function1[Workspace, DocNode]
  {
  def apply(w: Workspace): DocNode =
    {
    lazy val command = "/Users/lorax/iesl/rexa-textmill/bin/rexapdfminer.py --file " + w.file
    lazy val output =
      {
      val pb = Process(command)
      val sb = StringBuilder.newBuilder
      val pio = new ProcessIO(_ => (),
                              stdout => scala.io.Source.fromInputStream(stdout).getLines().foreach(sb append _),
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
      val pagenum = pageid.toInt

      val pageRect: Rectangle = Rectangle((page \ "@bbox").text)
                                .getOrElse(throw new Error("Page without bounding box, abort"))
      val thePage = new Page(pagenum, pageRect)

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
        // val bbox = new BoundingBox((box \ "@bbox").text) //with BoxReadingOrder
        // val bbox = (box \ "@bbox").text
        // val bboxRect = Rectangle(bbox)
        val lf: Seq[DocNode] = lines.flatten

        lf match
        {
          case Nil => None;
          case _ => Some(new DocNode(textboxid, lf, None, None,false))
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
        else RectangleOnPage(thePage, bbox).map(r => new CurveBox("c-" + pageid + "." + curveid.toString,  r))
        })
      var figureid = 0
      val figures = (for (figure <- page \ "figure") yield
        {
        figureid += 1
        val bbox: String = (figure \ "@bbox").text
        if (bbox.trim().isEmpty) None
        else RectangleOnPage(thePage, bbox).map(r => new FigureBox("f-" + pageid + "." + figureid.toString,  r))
        })

      new DocNode(pageid, (textboxes.flatten.toList :::
                           rects.flatten.toList :::
                           curves.flatten.toList ::: figures.flatten.toList), None, None,false)
        {
        override lazy val rectangle : Option[RectangleOnPage] = RectangleOnPage(thePage, pageRect)
//        override lazy val page = Some(pagenum)
        }
      }


    // couple ways to do this:
    // 1.  assemble all the raw strings, then zipWithIndex
    // 2.  grab the already-numbered textLines from each page, and merge them
    // 3.  for-comprehension as follows
    // val numberedTextLines = for (page <- pages.sorted; line <- page.textLines) yield line
    //val numberedTextLinesByPage =
    //val textBoxes = pages.map(x=>x.textBoxes.map(y => (x, y))).flatten

    DocNode(w.filename, rawPages.toList, Some(List(command, output).iterator), None,false)
    }
  }
