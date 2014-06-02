package edu.umass.cs.iesl.pdf2meta.cli.extract.metatagger

import edu.umass.cs.iesl.scalacommons.Workspace
import com.typesafe.scalalogging.slf4j.Logging
import org.apache.pdfbox.exceptions.InvalidPasswordException
import org.apache.pdfbox.pdmodel.{PDPage, PDDocument}
import collection.mutable.Buffer
import edu.umass.cs.iesl.pdf2meta.cli.extract.{MetataggerExtractor, PdfExtractorException, PdfExtractor}
import org.apache.pdfbox.pdmodel.common.{PDRectangle, PDStream}
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import edu.umass.cs.iesl.pdf2meta.cli.extract.pdfbox.LayoutItemsToDocNodes
import java.awt.Dimension
import scala.xml._
/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 *  kzaporojets: adapting to metatagger
 */
class MetataggerBoxExtractor extends MetataggerExtractor with Logging with Function1[Workspace, DocNode] {
  def apply(v1: Workspace) = {
    //here xml

    println ("path to xml: " + v1.file.path.toString)
    val documentMT: Elem = XML.loadFile(v1.file.path.toString)
    //    		try
    //    		{
    //		if (document.isEncrypted())
    //			{
    //			try
    //			{
    //			document.decrypt("")
    //			}
    //			catch
    //			{
    //			case e: InvalidPasswordException => throw new PdfExtractorException("Error: Document is encrypted with a password.")
    //			}
    //			}

    //documentMT.text
    //allTextBoxes: Seq[DocNode]
    println ("trying to get llx from the header:")
//    (documentMT \\ "headers").foreach{header =>
//      println((header \ "@llx").text)
//    }
    val docNodes:Seq[DocNode] = processXMLRecursive(documentMT \\ "headers", "")
//    recursiveXMLProcess(documentMT)
    println ("end of trying to get llx from the header")
    val internalDoc:InternalDocNode = new InternalDocNode("id_val", docNodes, Some((List("val")).toIterator), Some((List("val")).toIterator))

    internalDoc
/*    val document: PDDocument = PDDocument.load(v1.file.bufferedInput())
    //      new InternalDocNode("root", new List(new DocNode("1",None, None), None, None))
    import collection.JavaConversions._
    val allPagesB: Buffer[_] = document.getDocumentCatalog.getAllPages;
    val allPages: Seq[PDPage] = allPagesB.toSeq.asInstanceOf[Seq[PDPage]]



    val allPageNodes = for ((thepage: PDPage, pageNum: Int) <- allPages.zipWithIndex) yield {
      //logger.info("Processing page: " + page.);
      val contents: PDStream = thepage.getContents
      val mbox: PDRectangle = thepage.getMediaBox

      // note that zipWithIndex naturally started at 0, but page numbering should be 1-based.
      val pPage = new Page(pageNum + 1, new RealRectangle(mbox.getLowerLeftX, mbox.getLowerLeftY, mbox.getUpperRightX, mbox.getUpperRightY))

      if (contents != null) {
        val printer: LayoutItemsToDocNodes = new {
          val pdpage = thepage
          private val box: PDRectangle = pdpage.findMediaBox()
          val pageSize = new Dimension(box.getWidth.asInstanceOf[Int], box.getHeight.asInstanceOf[Int]);
          val docPage = pPage
        } with LayoutItemsToDocNodes

        printer.processStream(thepage, thepage.findResources(), thepage.getContents().getStream());
        Some(printer.getRootDocNode());
      }
      else None
    }

    new InternalDocNode("root", allPageNodes.flatten, None, None)
    //    }
    //		finally
    //			{
    //			document.close();
    //			}*/
  }


  def processXMLRecursive(node:Seq[Node], parentName:String):Seq[DocNode] =
  {
//    val ptrn = ".*".r
    val ptrn = "([0-9].*)".r
//    val Pattern = """([0-9])(.*)""".r
//    println(ptrn.pattern.matcher("hello").find())
    val seqDocNode:Seq[DocNode] = Seq()
//    node.foreach{currentNode =>
    val res = for(currentNode <- node)
      yield
      { (currentNode \ "@llx").text
        match
        {
          case ptrn(_) =>
            //          println(parentName + currentNode.label + ": " + (currentNode \ "@llx").text)
            println(currentNode.label + ": " + (currentNode \ "@llx").text)
            //(id: String,  val theRectangle: RectangleOnPage)
            val currNode: DocNode = new DelimitingBox((currentNode \ "@llx").text + (currentNode \ "@lly").text +
              (currentNode \ "@urx").text + (currentNode \ "@ury").text, new RectangleOnPage {
              override val page: Page = new Page(1,
                new Rectangle {
                  override val bottom: Float = 1000.0f
                  override val top: Float = 1.0f
                  override val left: Float = 1.0f
                  override val right: Float = 800.0f
                })
              override val bottom: Float = (currentNode \ "@lly").text.toFloat
              override val top: Float = (currentNode \ "@ury").text.toFloat
              override val left: Float = (currentNode \ "@llx").text.toFloat
              override val right: Float = (currentNode \ "@urx").text.toFloat
            })

            //seqDocNode :+ currNode
            (seqDocNode ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + "->")) :+ currNode

          case _ =>
            seqDocNode
          //println("Not match: " + parentName + currentNode.label + ":" + (currentNode \ "@llx").text)
        }
      }
      res.flatten
 //   seqDocNode
  }
}

class MetataggerBoxTextAtom(override val id: String, override val theText: String,  font: String,  fontHeight: Float, val rect: RectangleOnPage,
                     val charWidths : Array[Float])
		extends TextAtom(id, theText,Some(rect))
	{
	override lazy val dominantFont : Option[FontWithHeight]             = Some(new FontWithHeight(font, fontHeight))
	override lazy val allFonts     : Seq[(FontWithHeight, Int)] = Seq((dominantFont.get, theText.length))

	override def partitionByFont(boxorder: Ordering[RectangularOnPage]) = Seq(this)

	//def fontSize = fontHeight

	override def create(childrenA: Seq[DocNode]) =
		{
		assert(childrenA.isEmpty)
		this
		}
	}
