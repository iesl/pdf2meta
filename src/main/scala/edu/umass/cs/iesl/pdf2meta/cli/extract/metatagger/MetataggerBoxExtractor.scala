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
    "CONTENT -> HEADERS -> ABSTRACT" -> "HEADERS -> ABSTRACT",
    "CONTENT -> BIBLIO -> REFERENCE -> CONFERENCE" -> "REFERENCES -> CONFERENCE",
    "CONTENT -> BIBLIO -> REFERENCE -> ADDRESS" -> "REFERENCES -> ADDRESS",
    "CONTENT -> BIBLIO -> REFERENCE -> PUBLISHER" -> "REFERENCES -> PUBLISHER",
    "CONTENT -> BIBLIO -> REFERENCE -> ADDRESS" -> "REFERENCES -> ADDRESS",
    "CONTENT -> BIBLIO -> REFERENCE -> REF-MARKER" -> "REFERENCES -> REF-MARKER",
    "CONTENT -> BIBLIO -> REFERENCE -> AUTHORS" -> "REFERENCES -> AUTHORS",
    "CONTENT -> BIBLIO -> REFERENCE" -> "REFERENCES",
    "CONTENT -> BIBLIO -> REFERENCE -> TITLE" -> "REFERENCES -> TITLE",
    "CONTENT -> BIBLIO -> REFERENCE -> JOURNAL" -> "REFERENCES -> JOURNAL",
    "CONTENT -> BIBLIO -> REFERENCE -> VOLUME" -> "REFERENCES -> VOLUME",
    "CONTENT -> BIBLIO -> REFERENCE -> NUMBER" -> "REFERENCES -> NUMBER",
    "CONTENT -> BIBLIO -> REFERENCE -> PAGES" -> "REFERENCES -> PAGES",
    "CONTENT -> BIBLIO -> REFERENCE -> DATE" -> "REFERENCES -> DATE",
    "CONTENT -> BIBLIO -> REFERENCE -> BOOKTITLE" -> "REFERENCES -> BOOKTITLE",
    "CONTENT -> BIBLIO -> REFERENCE -> NOTE" -> "REFERENCES -> NOTE")

  //for recursive content such as authors that itself can be composed of firstname, lastname, etc.
  val recursiveExtraction:List[String] = List("CONTENT -> BIBLIO -> REFERENCE -> AUTHORS")

  def apply(v1: Workspace) = {
    //here xml

    println ("path to xml: " + v1.file.path.toString)
    val documentMT: Elem = XML.loadFile(v1.file.path.toString)


    println ("trying to get llx from the header:")

    val docNodes:(Seq[DocNode], Seq[ClassifiedRectangle]) = processXMLRecursive(documentMT \\ "content", "", "",
    //TODO: encode the size of the page inside xml and read from there
     new Rectangle {
        override val bottom: Float = 0f
        override val top: Float = 792.0f
        override val left: Float = 0f
        override val right: Float = 612.0f
      })
//    recursiveXMLProcess(documentMT)
    println ("end of trying to get llx from the header")
    val internalDoc:InternalDocNode = new InternalDocNode("id_val", docNodes._1, None, None)
    val classifiedRectangles:ClassifiedRectangles = new ClassifiedRectangles(docNodes._2)
    (internalDoc, classifiedRectangles)

  }


  def processXMLRecursive(node:Seq[Node], parentName:String, parentId:String, pageDimensions:Rectangle):(Seq[DocNode], Seq[ClassifiedRectangle]) =
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
            if((parentName + currentNode.label).toUpperCase().contains("REFERENCE") &&
                 Math.abs((currentNode \ "@lly").text.toFloat -
                      (currentNode \ "@ury").text.toFloat)>400)
            {
              ((seqDocNode ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ", parentId, pageDimensions)._1),
                (seqClassifiedRectangle ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ", parentId, pageDimensions)._2))
            }
            else
            {
              def returnParentId(lblName:String, rect:RectangleOnPage, parentId:String)={if(lblName.toUpperCase()=="REFERENCE"){"REFERENCE_" + rect.top.toInt + "_" +
                rect.left.toInt + "_" + rect.bottom.toInt + "_" + rect.right.toInt + "_" + rect.page.pagenum +  "_"}else{parentId}}

              val rectOnPage:RectangleOnPage = new RectangleOnPage {
                override val page: Page = new Page(Integer.valueOf((currentNode \ "@pageNum").text),pageDimensions)
                override val bottom: Float = (currentNode \ "@lly").text.toFloat
                override val top: Float = (currentNode \ "@ury").text.toFloat
                override val left: Float = (currentNode \ "@llx").text.toFloat
                override val right: Float = (currentNode \ "@urx").text.toFloat
              }
              val currNode: DocNode = new DelimitingBox(/*(currentNode \ "@llx").text + (currentNode \ "@lly").text +
                (currentNode \ "@urx").text + (currentNode \ "@ury").text + */

                {if((parentName + currentNode.label).toUpperCase().contains("REFERENCE"))
                  {
                    returnParentId(currentNode.label, rectOnPage,"") + parentId + parentName + currentNode.label /* + (currentNode \ "@llx").text + (currentNode \ "@lly").text +
                    (currentNode \ "@urx").text + (currentNode \ "@ury").text*/
                  }
                  else
                  {
                    returnParentId(currentNode.label, rectOnPage,"") + parentId + parentName + currentNode.label  /*+ (currentNode \ "@llx").text + (currentNode \ "@lly").text +
                      (currentNode \ "@urx").text + (currentNode \ "@ury").text*/
                  }
                }
                , rectOnPage)
              val f:Feature = LocalFeature("dumbfeature", (box: DocNode) =>
                                {
                              box match
                              {
                                case _                 => 0f
                              }
                            })
              val weightedFeatureSet:WeightedSet[Feature] = new WeightedSet[Feature]{
                val asMap = Map[Feature, Double]()
              }
              val weightedStringSet:WeightedSet[String] = new WeightedSet[String]{
                val asMap = Map[String, Double]()
              }


              if(mapAcceptedLabels.keys.exists(x => x==(parentName + currentNode.label).toUpperCase()))
              {
                def getContent(currentNode:scala.xml.Node, currNode:DocNode, completePath:String):String =
                                    {if(!currentNode.text.toString().contains("\n") && currNode.id.toUpperCase().contains("REFERENCE"))
                                    {
                                          ": " + currentNode.text.toString()
                                    }
                                    else if(recursiveExtraction.exists(x => x == completePath.toUpperCase()))
                                    {
                                        ": " + getRecursiveContent(currentNode)
                                    }
                                    else
                                    {
                                       ""
                                    }}
                def getRecursiveContent(currentNode:scala.xml.Node):String =
                {
                  if(currentNode.text.toString().contains("\n"))
                  {

                    currentNode.child.map(x=> getRecursiveContent(x)).mkString(" ")
                  }
                  else
                  {
                    currentNode.text
                  }
                }
                val currClassifiedRectangle: ClassifiedRectangle =
                        new ClassifiedRectangle(new MetataggerBoxTextAtom(currNode.id,
                          mapAcceptedLabels.get((parentName + currentNode.label).toUpperCase()).get + getContent(currentNode,currNode,parentName + currentNode.label), "Font", 0.0f,
                             currNode.rectangle.get, Array[Float](0f))//currNode
                  , weightedFeatureSet, weightedStringSet, None)

                ((seqDocNode ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",returnParentId(currentNode.label, currNode.rectangle.get,parentId),pageDimensions)._1) :+ currNode,
                  (seqClassifiedRectangle ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",returnParentId(currentNode.label, currNode.rectangle.get,parentId),pageDimensions)._2) :+ currClassifiedRectangle)
              }
              else
              {
                ((seqDocNode ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",returnParentId(currentNode.label, currNode.rectangle.get,parentId),pageDimensions)._1),
                  (seqClassifiedRectangle ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",returnParentId(currentNode.label, currNode.rectangle.get,parentId),pageDimensions)._2))
              }
            }
          case _ =>
            ((seqDocNode ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",parentId,pageDimensions)._1),
              (seqClassifiedRectangle ++ processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",parentId,pageDimensions)._2))
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
