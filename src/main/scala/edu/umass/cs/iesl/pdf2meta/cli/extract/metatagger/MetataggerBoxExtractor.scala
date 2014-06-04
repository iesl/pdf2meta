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
import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter.{LocalFeature, Feature, ClassifiedRectangle, ClassifiedRectangles}
import edu.umass.cs.iesl.scalacommons.collections.WeightedSet
import edu.umass.cs.iesl.pdf2meta.cli.config.StandardScoringModel
/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 *  kzaporojets: adapted to metatagger
 */
class MetataggerBoxExtractor extends MetataggerExtractor with Logging with Function1[Workspace, (DocNode, ClassifiedRectangles)] {

  //TODO: read from properties file
  val mapAcceptedLabels:Map[String, String] = Map("CONTENT -> HEADERS -> TITLE" -> "HEADERS -> TITLE",
                                              "CONTENT -> HEADERS -> AUTHORS" -> "HEADERS -> AUTHORS",
                                              "CONTENT -> HEADERS -> INSTITUTION" -> "HEADERS -> INSTITUTION",
                                              "CONTENT -> HEADERS -> ADDRESS" -> "HEADERS -> ADDRESS",
                                              "CONTENT -> HEADERS -> EMAIL" -> "HEADERS -> EMAIL",
                                              "CONTENT -> HEADERS -> ABSTRACT" -> "HEADERS -> ABSTRACT")
  def apply(v1: Workspace) = {
    //here xml

    println ("path to xml: " + v1.file.path.toString)
    val documentMT: Elem = XML.loadFile(v1.file.path.toString)

    //documentMT.text
    //allTextBoxes: Seq[DocNode]
    println ("trying to get llx from the header:")
//    (documentMT \\ "headers").foreach{header =>
//      println((header \ "@llx").text)
//    }
    val docNodes:(Seq[DocNode], Seq[ClassifiedRectangle]) = processXMLRecursive(documentMT \\ "content", "",
    //TODO: encode the size of the page inside xml and read from there
     new Rectangle {
        override val bottom: Float = 0f
        override val top: Float = 792.0f
        override val left: Float = 0f
        override val right: Float = 612.0f
      })
//    recursiveXMLProcess(documentMT)
    println ("end of trying to get llx from the header")
    val internalDoc:InternalDocNode = new InternalDocNode("id_val", docNodes._1, None, None) //Some((List("val")).toIterator), Some((List("val")).toIterator))
    //(0.0,0.0), (612.0,792.0)
    //Page(1,(0.0,0.0), (612.0,792.0))
    val classifiedRectangles:ClassifiedRectangles = new ClassifiedRectangles(docNodes._2)
    (internalDoc, classifiedRectangles)
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


  def processXMLRecursive(node:Seq[Node], parentName:String, pageDimensions:Rectangle):(Seq[DocNode], Seq[ClassifiedRectangle]) =
  {
    val ptrn = "([0-9].*)".r
    val seqDocNode:Seq[DocNode] = Seq()
    val seqClassifiedRectangle:Seq[ClassifiedRectangle] = Seq()


    val res = for(currentNode <- node)
      yield
      { (currentNode \ "@llx").text
        match
        {
          case ptrn(_) =>
            //println(currentNode.label + ": " + (currentNode \ "@pageNum").text)
            //(id: String,  val theRectangle: RectangleOnPage)
            val currNode: DocNode = new DelimitingBox(/*(currentNode \ "@llx").text + (currentNode \ "@lly").text +
              (currentNode \ "@urx").text + (currentNode \ "@ury").text + */parentName + currentNode.label, new RectangleOnPage {
              override val page: Page = new Page(Integer.valueOf((currentNode \ "@pageNum").text),pageDimensions)
              override val bottom: Float = (currentNode \ "@lly").text.toFloat
              override val top: Float = (currentNode \ "@ury").text.toFloat
              override val left: Float = (currentNode \ "@llx").text.toFloat
              override val right: Float = (currentNode \ "@urx").text.toFloat
            })
            val f:Feature = LocalFeature("dumbfeature", (box: DocNode) =>
                              {
                            box match
                            {
                              case _                 => 0f
                            }
                          })
//StandardScoringModel.sideways
            val weightedFeatureSet:WeightedSet[Feature] = new WeightedSet[Feature]{
              val asMap = Map[Feature, Double]()
            }
            val weightedStringSet:WeightedSet[String] = new WeightedSet[String]{
              val asMap = Map[String, Double]()
            }

            if(mapAcceptedLabels.keys.exists(x => x==(parentName + currentNode.label).toUpperCase()))
            {
              val currClassifiedRectangle: ClassifiedRectangle =
                      new ClassifiedRectangle(new MetataggerBoxTextAtom(currNode.id,
                        mapAcceptedLabels.get((parentName + currentNode.label).toUpperCase()).get, "Font", 0.0f,
                           currNode.rectangle.get, Array[Float](0f))//currNode
                , weightedFeatureSet, weightedStringSet, None)

              ((seqDocNode ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",pageDimensions)._1) :+ currNode,
                (seqClassifiedRectangle ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",pageDimensions)._2) :+ currClassifiedRectangle)
            }
            else
            {
              ((seqDocNode ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",pageDimensions)._1),
                (seqClassifiedRectangle ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",pageDimensions)._2))
            }

          case _ =>
           // println ("not matched: " + currentNode.label)
            ((seqDocNode ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",pageDimensions)._1),
              (seqClassifiedRectangle ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",pageDimensions)._2))

//            (seqDocNode, seqClassifiedRectangle)
        }
      }


      (res.map{t:((Seq[DocNode], Seq[ClassifiedRectangle])) => t._1}.flatten,
        res.map{t:((Seq[DocNode], Seq[ClassifiedRectangle])) => t._2}.flatten)
  }
}

class MetataggerBoxTextAtom(override val id: String, override val theText: String,  font: String,  fontHeight: Float, val rect: RectangleOnPage,
                     val charWidths : Array[Float])
		extends TextAtom(id, theText,Some(rect))
	{
	override lazy val dominantFont : Option[FontWithHeight]             = Some(new FontWithHeight(font, fontHeight))
	override lazy val allFonts     : Seq[(FontWithHeight, Int)] = Seq((dominantFont.get, theText.length))

	override def partitionByFont(boxorder: Ordering[RectangularOnPage]) = Seq(this)


	override def create(childrenA: Seq[DocNode]) =
		{
		assert(childrenA.isEmpty)
		this
		}
	}
