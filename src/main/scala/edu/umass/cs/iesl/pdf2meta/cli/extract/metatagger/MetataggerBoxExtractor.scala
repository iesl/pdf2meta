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
import net.liftweb.http.S
import edu.umass.cs.iesl.pdf2meta.cli.lib.MapToProperties
/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 *  kzaporojets: adapted to metatagger
 */
class MetataggerBoxExtractor extends MetataggerExtractor with Logging with Function1[Workspace,ClassifiedRectangles /*(DocNode, ClassifiedRectangles)*/] {

  //TODO: read from properties file
  val mapAcceptedLabels:Map[String, String] = Map("CONTENT -> HEADERS -> TITLE" -> "HEADERS -> TITLE",
    "CONTENT -> HEADERS -> AUTHORS" -> "HEADERS -> AUTHORS",
    "CONTENT -> HEADERS -> INSTITUTION" -> "HEADERS -> INSTITUTION",
    "CONTENT -> HEADERS -> ADDRESS" -> "HEADERS -> ADDRESS",
    "CONTENT -> HEADERS -> EMAIL" -> "HEADERS -> EMAIL",
    "CONTENT -> HEADERS -> ABSTRACT" -> "HEADERS -> ABSTRACT",
    "CONTENT -> HEADERS -> EDITOR" -> "HEADERS -> EDITOR",
    "CONTENT -> HEADERS -> PAGES" -> "HEADERS -> PAGES",
    "CONTENT -> HEADERS -> CONFERENCE" -> "HEADERS -> CONFERENCE",
    "CONTENT -> HEADERS -> NOTE -> CONFERENCE" -> "HEADERS -> NOTE -> CONFERENCE",
   // "CONTENT -> HEADERS -> NOTE" -> "HEADERS -> NOTE",
    "CONTENT -> HEADERS -> NOTE -> DATE" -> "HEADERS -> NOTE -> DATE",
    "CONTENT -> HEADERS -> NOTE -> INSTITUTION" -> "HEADERS -> NOTE -> INSTITUTION",
    "CONTENT -> HEADERS -> NOTE -> ADDRESS" -> "HEADERS -> NOTE -> ADDRESS",
    "CONTENT -> HEADERS -> NOTE -> JOURNAL" -> "HEADERS -> NOTE -> JOURNAL",
    "CONTENT -> HEADERS -> NOTE -> PAGES" -> "HEADERS -> NOTE -> PAGES",
    "CONTENT -> HEADERS -> NOTE -> TECH" -> "HEADERS -> NOTE -> TECH",
    "CONTENT -> HEADERS -> NOTE -> VOLUME" -> "HEADERS -> NOTE -> VOLUME",
    "CONTENT -> HEADERS -> NOTE -> NUMBER" -> "HEADERS -> NOTE -> NUMBER",
    "CONTENT -> HEADERS -> NOTE -> PUBLISHER" -> "HEADERS -> NOTE -> PUBLISHER",
    "CONTENT -> HEADERS -> NOTE -> BOOKTITLE" -> "HEADERS -> NOTE -> BOOKTITLE",
    "CONTENT -> HEADERS -> DATE" -> "HEADERS -> DATE",
    "CONTENT -> HEADERS -> KEYWORD" -> "HEADERS -> KEYWORD",
    "CONTENT -> HEADERS -> TECH" -> "HEADERS -> TECH",
    "CONTENT -> HEADERS -> WEB" -> "HEADERS -> WEB",
    "CONTENT -> HEADERS -> PHONE" -> "HEADERS -> PHONE",
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
    "CONTENT -> BIBLIO -> REFERENCE -> NOTE" -> "REFERENCES -> NOTE",
    "CONTENT -> BIBLIO -> REFERENCE -> INSTITUTION" -> "REFERENCES -> INSTITUTION",
    "CONTENT -> BIBLIO -> REFERENCE -> WEB" -> "REFERENCES -> WEB",
    "CONTENT -> BIBLIO -> REFERENCE -> TECH" -> "REFERENCES -> TECH",
    "CONTENT -> BIBLIO -> REFERENCE -> AUTHOR" -> "REFERENCES -> AUTHOR",
    "CONTENT -> HEADERS -> AUTHORS -> AUTHOR -> NOTE" -> "HEADERS -> AUTHORS -> NOTE",
    "CONTENT -> BODY -> COLUMN" -> "BODY -> COLUMN"
    )



  val authorExtraction:List[String] = List("CONTENT -> BIBLIO -> REFERENCE -> AUTHORS",
     "CONTENT -> HEADERS -> AUTHORS"
  )

  val siblingsHerarchyExtraction:List[String] = List("CONTENT -> HEADERS -> INSTITUTION",
    "CONTENT -> HEADERS -> ADDRESS",
    "CONTENT -> HEADERS -> NOTE -> INSTITUTION",
    "CONTENT -> HEADERS -> NOTE -> ADDRESS",
    "CONTENT -> HEADERS -> EMAIL"
    //,
    //"CONTENT -> HEADERS -> NOTE -> DATE"
  )

  //for recursive content such as authors that itself can be composed of firstname, lastname, etc.
  val recursiveExtraction:List[String] = List("CONTENT -> BIBLIO -> REFERENCE -> AUTHORS")

//for the text content that must be merged and not be represented by different labels (ex: sometimes the tag regarding the conference appear several times)
//however, the position of the textboxes may be different
  val mergeContent:List[String] = List("CONTENT -> BIBLIO -> REFERENCE -> CONFERENCE",
                                  "CONTENT -> BIBLIO -> REFERENCE -> DATE",
                                  "CONTENT -> BIBLIO -> REFERENCE -> ADDRESS",
                                  "CONTENT -> BIBLIO -> REFERENCE -> TITLE",
                                  //****************************************//
                                  "CONTENT -> HEADERS -> TITLE",
//                                  "CONTENT -> HEADERS -> INSTITUTION",
//                                  "CONTENT -> HEADERS -> ADDRESS",
//                                  "CONTENT -> HEADERS -> EMAIL",
                                  "CONTENT -> HEADERS -> NOTE -> DATE",
//                                  "CONTENT -> HEADERS -> NOTE -> INSTITUTION",
 //                                 "CONTENT -> HEADERS -> NOTE -> ADDRESS",
                                  "CONTENT -> HEADERS -> NOTE -> JOURNAL",
                                  "CONTENT -> HEADERS -> NOTE -> PAGES",
                                  "CONTENT -> HEADERS -> DATE",
                                  "CONTENT -> HEADERS -> KEYWORD",
                                  "CONTENT -> HEADERS -> NOTE -> PUBLISHER",
                                  "CONTENT -> HEADERS -> NOTE -> TECH",
                                  "CONTENT -> HEADERS -> NOTE -> VOLUME",
                                  "CONTENT -> HEADERS -> NOTE -> NUMBER",
                                  "CONTENT -> HEADERS -> NOTE -> CONFERENCE",
                                  "CONTENT -> HEADERS -> ABSTRACT",
                                  "CONTENT -> HEADERS -> EDITOR",
                                  "CONTENT -> HEADERS -> NOTE -> BOOKTITLE",
                                  "CONTENT -> HEADERS -> TECH",
                                  "CONTENT -> HEADERS -> WEB",
                                  "CONTENT -> HEADERS -> PHONE",
                                  "CONTENT -> HEADERS -> PAGES",
                                  "CONTENT -> HEADERS -> AUTHORS -> AUTHOR -> NOTE"
)

  def apply(v1: Workspace) = {
    //here xml

    val documentMT: Elem = XML.loadFile(v1.file.path.toString)

    val props:MapToProperties = new MapToProperties()
    val propertiesFilename:String =
      S.get("propertiesFile").openOrThrowException("err while obtaining properties file")

    val properties:Map[String,String] = props.readPropertiesFile(propertiesFilename)
    val docNodes:Seq[ClassifiedRectangle] =
      processXMLRecursiveV4(documentMT \\ "content", "", "",
     new Rectangle {
        override val bottom: Float = 0f
        override val top: Float = properties.get("height").get.toFloat //java.lang.Float.valueOf(S.get("height").openOrThrowException("err in width"))
       //792.0f
        override val left: Float = 0f
        override val right: Float = properties.get("width").get.toFloat //java.lang.Float.valueOf(S.get("width").openOrThrowException("err in height")) //612.0f
      },false)
    val classifiedRectangles:ClassifiedRectangles = new ClassifiedRectangles(docNodes)
    classifiedRectangles
  }




  def getConcatenation(mapData:Seq[Node]):String =
  {
    mapData.map(x=>
      x.text).mkString(" ")
  }



  def processXMLRecursiveV4(node:Seq[Node], parentName:String, parentId:String, pageDimensions:Rectangle, justContent:Boolean):Seq[ClassifiedRectangle] =
  {

//    println("parent data: " + parentName + " " + parentId)

    val ptrn = "([0-9].*)".r
    //    val seqDocNode:Seq[DocNode] = Seq()
    val seqClassifiedRectangle:Seq[ClassifiedRectangle] = Seq()

    def returnParentId(lblName:String, rect:RectangleOnPage, parentId:String)={if(lblName.toUpperCase()=="REFERENCE"){"REFERENCE_" + rect.top.toInt + "_" +
      rect.left.toInt + "_" + rect.bottom.toInt + "_" + rect.right.toInt + "_" + rect.page.pagenum +  "_"}else{parentId}}

    val res = for(currentNode <- node.groupBy(_.label))
    yield
    { (currentNode._2(0) \ "@llx").text
    match {
      case ptrn(_) =>
        println("change")

        def getRectangleOnPage(cNode:Node):RectangleOnPage = {
          val rectOnPage:RectangleOnPage = new RectangleOnPage {
            override val page: Page = new Page(Integer.valueOf((cNode \ "@pageNum").text),pageDimensions)
            override val bottom: Float = (cNode \ "@lly").text.toFloat + 2.0f
            override val top: Float = (cNode \ "@ury").text.toFloat + 2.0f
            override val left: Float = (cNode \ "@llx").text.toFloat - 7.0f
            override val right: Float = (cNode \ "@urx").text.toFloat - 7.0f
          }
          rectOnPage
        }

        if((parentName + currentNode._1).toUpperCase().contains("REFERENCE") &&
          Math.abs((currentNode._2(0) \ "@lly").text.toFloat - (currentNode._2(0) \ "@ury").text.toFloat)> 400 /*400*/)
        {
          val recRes:Seq[ClassifiedRectangle] = processXMLRecursiveV4(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ",parentId,pageDimensions,false)
          val recSiblings:Seq[ClassifiedRectangle] = {if(currentNode._2.size>1){processXMLRecursiveV4(currentNode._2.tail, parentName,parentId,pageDimensions,false)}
          else
          {(seqClassifiedRectangle)}}

          ((seqClassifiedRectangle ++ recSiblings) ++ recRes)
        }
        else
        {
          def mergeIsApplicable() = {
            if(mergeContent.exists(x => x == (parentName + currentNode._1).toUpperCase() ))
            {
              true
            }
            else{false}
          }
          def siblingHierarchyApplicable() =
          {
            if(siblingsHerarchyExtraction.exists(x => x == (parentName + currentNode._1).toUpperCase() ))
            {
              true
            }
            else{false}
          }
          val rectOnPage:RectangleOnPage = getRectangleOnPage(currentNode._2(0))

          val currNode: DocNode = new DelimitingBox(
            returnParentId(currentNode._2(0).label, rectOnPage,"") + parentId + parentName + currentNode._2(0).label
            , rectOnPage)
          val weightedFeatureSet:WeightedSet[Feature] = new WeightedSet[Feature]{
            val asMap = Map[Feature, Double]()
          }
          val weightedStringSet:WeightedSet[String] = new WeightedSet[String]{
            val asMap = Map[String, Double]()
          }


          if(mapAcceptedLabels.keys.exists(x => x==(parentName + currentNode._2(0).label).toUpperCase()))
          {
            def getContent(currntNode:scala.xml.Node, currNode:DocNode, completePath:String, includePref:Boolean):String =
            {
              val pref = if(includePref){": "} else {""}
              if(mergeIsApplicable())
              {
                pref + getConcatenation(currentNode._2 )
              }
              else if(!currntNode.text.toString().contains("\n") && currNode.id.toUpperCase().contains("REFERENCE"))
              {
                pref + currntNode.text.toString()
              }
              else if(recursiveExtraction.exists(x => x == completePath.toUpperCase()))
              {
                pref + getRecursiveContent(currntNode)
              }
              else if(siblingHierarchyApplicable())
              {
                pref + currntNode.text.toString()
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


            //def extractSiblings()

            //for extracting the author data in a particular node
            def extractAuthors(currentNode:Seq[scala.xml.Node], currentAuthorId:Int):Seq[ClassifiedRectangle] =
            {

              val currentAuthor = currentNode.head

              if(currentAuthor.label.toUpperCase()!="AUTHOR")
              {
                if(currentNode.size>1)
                {
                  extractAuthors(currentNode.tail,currentAuthorId)
                }
                else
                {
                  List()
                }
              }
              else
              {
                //for each of the authors, extract the first, middle and last names
                def getXAuthorPart(currAuthor:Node, part:String, acronym:String):String = {
                   val firstName = currAuthor.child.filter(x=> x.label==part)
                   if(firstName.size>0)
                   {
                     acronym + ": " + firstName(0).text + " "
                   }
                   else
                   {
                     ""
                   }
                }
                val currentAuthorText = getXAuthorPart(currentAuthor, "author-first", "FN") +
                                            getXAuthorPart(currentAuthor, "author-middle","MN") +
                                            getXAuthorPart(currentAuthor, "author-last", "LN")
                val rectOnPage = getRectangleOnPage(currentAuthor)

                val currClassifiedRectangle: ClassifiedRectangle =
                  new ClassifiedRectangle(new MetataggerBoxTextAtom(currNode.id + "_author_" + currentAuthorId,
                    currentAuthorText, "Font", 0.0f,
                    rectOnPage, Array[Float](0f))//currNode
                    , weightedFeatureSet, weightedStringSet, None, List())
                if(currentNode.size>1)
                {
                  currClassifiedRectangle +: extractAuthors(currentNode.tail, currentAuthorId+1)
                }
                else
                {
                  List(currClassifiedRectangle)
                }
              }
            }

            //gets the children structure that corresponds
            def getChildren(currentNode:scala.xml.Node, path:String, siblings:Seq[ClassifiedRectangle], currRect:List[ClassifiedRectangle]):Seq[ClassifiedRectangle] =
            {
              //based on the path, decides what children should be created
              if(authorExtraction.exists(x => x==path.toUpperCase()))
              {
                extractAuthors(currentNode.child,1)
              }
              else if(siblingsHerarchyExtraction.exists(x => x==path.toUpperCase()))
              {
                //d
                //currRect ++ siblings
                currRect ++ siblings.reverse
              }
              else
              {
                List()
              }
            }

            //def getHierarchicalStructure()

            val recRes = processXMLRecursiveV4(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ",returnParentId(currentNode._2(0).label, currNode.rectangle.get,parentId),pageDimensions,false)

            val recSiblings:Seq[ClassifiedRectangle] = {if(currentNode._2.size>1){processXMLRecursiveV4(currentNode._2.tail, parentName,parentId,pageDimensions,
              if(siblingHierarchyApplicable()){true}else{false})}
            else
            {(seqClassifiedRectangle)}}

            if(siblingHierarchyApplicable())
            {
              println("hierarchy applicable")
            }
            val currClassifiedRectangleP: ClassifiedRectangle =
              new ClassifiedRectangle(new MetataggerBoxTextAtom(currNode.id,
                {if(!justContent){mapAcceptedLabels.get((parentName + currentNode._2(0).label).toUpperCase()).get}else{""}} +
                {if(!justContent && siblingHierarchyApplicable()){""}else{getContent(currentNode._2(0),currNode,parentName + currentNode._2(0).label,!justContent)}}, "Font", 0.0f,
                currNode.rectangle.get, Array[Float](0f))//currNode
                , weightedFeatureSet, weightedStringSet, None, List())


            val currClassifiedRectangle = currClassifiedRectangleP.copy(children = getChildren(currentNode._2(0), parentName + currentNode._2(0).label,recSiblings,

              if(!justContent){List(currClassifiedRectangleP.copy(node = new MetataggerBoxTextAtom(currNode.id,
                      getContent(currentNode._2(0),currNode,parentName + currentNode._2(0).label,false), "Font", 0.0f,
                    currNode.rectangle.get, Array[Float](0f))))}else{List()}

            ))


            (((seqClassifiedRectangle) ++ {if(mergeIsApplicable()){
                recSiblings
            }else{if(!siblingHierarchyApplicable() || justContent){recSiblings}else{List()}}}) ++ recRes) :+ currClassifiedRectangle

//            currClassifiedRectangle +: (((seqClassifiedRectangle) ++ {if(mergeIsApplicable()){
//                recSiblings
//            }else{if(!siblingHierarchyApplicable() || justContent){recSiblings}else{List()}}}) ++ recRes)
          }
          else
          {
            val recRes:(Seq[ClassifiedRectangle]) = processXMLRecursiveV4(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ",returnParentId(currentNode._2(0).label, currNode.rectangle.get,parentId),pageDimensions,false)
            val recSiblings:( Seq[ClassifiedRectangle]) = {if(currentNode._2.size>1){processXMLRecursiveV4(currentNode._2.tail, parentName,parentId,pageDimensions,false)}
            else
            {(seqClassifiedRectangle)}}

            (((seqClassifiedRectangle ++ recSiblings) ++ recRes))
          }
        }
      case _ =>
        val recRes = processXMLRecursiveV4(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ", parentId
          ,pageDimensions,false)
        (seqClassifiedRectangle ++ recRes)
    }
    }

    res.flatten.toSeq
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
