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
class MetataggerBoxExtractor extends MetataggerExtractor with Logging with Function1[Workspace,ClassifiedRectangles /*(DocNode, ClassifiedRectangles)*/] {

  //TODO: read from properties file
  val mapAcceptedLabels:Map[String, String] = Map("CONTENT -> HEADERS -> TITLE" -> "HEADERS -> TITLE",
    "CONTENT -> HEADERS -> AUTHORS" -> "HEADERS -> AUTHORS",
//    "CONTENT -> HEADERS -> AUTHORS -> AUTHOR" -> "HEADERS -> AUTHOR",
//    "CONTENT -> HEADERS -> AUTHORS -> AUTHOR -> AUTHOR-FIRST" -> "HEADERS -> AUTHOR-FIRST",
//    "CONTENT -> HEADERS -> AUTHORS -> AUTHOR -> AUTHOR-LAST" -> "HEADERS -> AUTHOR-LAST",
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
    "CONTENT -> BIBLIO -> REFERENCE -> NOTE" -> "REFERENCES -> NOTE",
    "CONTENT -> BIBLIO -> REFERENCE -> INSTITUTION" -> "REFERENCES -> INSTITUTION",
//    "CONTENT -> BIBLIO -> REFERENCE -> AUTHORS -> AUTHOR" -> "REFERENCES -> AUTHOR",
//    "CONTENT -> BIBLIO -> REFERENCE -> AUTHORS -> AUTHOR -> AUTHOR-FIRST" -> "REFERENCES -> AUTHOR-FIRST",
//    "CONTENT -> BIBLIO -> REFERENCE -> AUTHORS -> AUTHOR -> AUTHOR-LAST" -> "REFERENCES -> AUTHOR-LAST",
//    "CONTENT -> BIBLIO -> REFERENCE -> AUTHORS -> AUTHOR -> AUTHOR-MIDDLE" -> "REFERENCES -> AUTHOR-MIDDLE",
    "CONTENT -> BIBLIO -> REFERENCE -> WEB" -> "REFERENCES -> WEB")



  val authorExtraction:List[String] = List("CONTENT -> BIBLIO -> REFERENCE -> AUTHORS",
     "CONTENT -> HEADERS -> AUTHORS"
  )

  val mapFnLnLabels:Map[String, String] = Map(
    "CONTENT -> HEADERS -> AUTHORS -> AUTHOR -> AUTHOR-FIRST" -> "FN",
    "CONTENT -> HEADERS -> AUTHORS -> AUTHOR -> AUTHOR-LAST" -> "LN",
    "CONTENT -> BIBLIO -> REFERENCE -> AUTHORS -> AUTHOR -> AUTHOR-FIRST" -> "FN",
    "CONTENT -> BIBLIO -> REFERENCE -> AUTHORS -> AUTHOR -> AUTHOR-LAST" -> "LN",
    "CONTENT -> BIBLIO -> REFERENCE -> AUTHORS -> AUTHOR -> AUTHOR-MIDDLE" -> "MN"
  )
//  val firstNamePar:String = "CONTENT -> HEADERS -> AUTHORS -> AUTHOR -> AUTHOR-FIRST"
//  val lastNamePar:String = "CONTENT -> HEADERS -> AUTHORS -> AUTHOR -> AUTHOR-LAST"

  //for recursive content such as authors that itself can be composed of firstname, lastname, etc.
  val recursiveExtraction:List[String] = List("CONTENT -> BIBLIO -> REFERENCE -> AUTHORS")

//for the text content that must be merged and not be represented by different labels (ex: sometimes the tag regarding the conference appear several times)
//however, the position of the textboxes may be different
  val mergeContent:List[String] = List("CONTENT -> BIBLIO -> REFERENCE -> CONFERENCE",
                                  "CONTENT -> BIBLIO -> REFERENCE -> DATE",
                                  "CONTENT -> BIBLIO -> REFERENCE -> ADDRESS")

  def apply(v1: Workspace) = {
    //here xml

    println ("path to xml: " + v1.file.path.toString)
    val documentMT: Elem = XML.loadFile(v1.file.path.toString)


    println ("trying to get llx from the header:")

//    processXMLRecursiveV2(documentMT \\ "content", "", "",
//        //TODO: encode the size of the page inside xml and read from there
//        new Rectangle {
//          override val bottom: Float = 0f
//          override val top: Float = 792.0f
//          override val left: Float = 0f
//          override val right: Float = 612.0f
//        })

//    val docNodes:(Seq[DocNode], Seq[ClassifiedRectangle]) =
//      processXMLRecursiveV2(documentMT \\ "content", "", "",
//    //TODO: encode the size of the page inside xml and read from there
//     new Rectangle {
//        override val bottom: Float = 0f
//        override val top: Float = 792.0f
//        override val left: Float = 0f
//        override val right: Float = 612.0f
//      })

    println ("end of trying to get llx from the header")
//    val internalDoc:InternalDocNode = new InternalDocNode("id_val", docNodes._1, None, None)
//    val classifiedRectangles:ClassifiedRectangles = new ClassifiedRectangles(docNodes._2)
//    (internalDoc, classifiedRectangles)

    val docNodes:Seq[ClassifiedRectangle] =
      processXMLRecursiveV3(documentMT \\ "content", "", "",
    //TODO: encode the size of the page inside xml and read from there
     new Rectangle {
        override val bottom: Float = 0f
        override val top: Float = 792.0f
        override val left: Float = 0f
        override val right: Float = 612.0f
      })
    val classifiedRectangles:ClassifiedRectangles = new ClassifiedRectangles(docNodes)
    classifiedRectangles
  }


//  def processXMLRecursive(node:Seq[Node], parentName:String, parentId:String, pageDimensions:Rectangle):(Seq[DocNode], Seq[ClassifiedRectangle]) =
//  {
//    val ptrn = "([0-9].*)".r
//    val seqDocNode:Seq[DocNode] = Seq()
//    val seqClassifiedRectangle:Seq[ClassifiedRectangle] = Seq()
//
//
//    val res = for(currentNode <- node)
//      yield
//      { (currentNode \ "@llx").text
//        match
//        {
//          case ptrn(_) =>
//            //println(currentNode.label + ": " + (currentNode \ "@pageNum").text)
//            //(id: String,  val theRectangle: RectangleOnPage)
//
//
//
//            if((parentName + currentNode.label).toUpperCase().contains("REFERENCE") &&
//                 Math.abs((currentNode \ "@lly").text.toFloat -
//                      (currentNode \ "@ury").text.toFloat)>400)
//            {
//              val recRes = processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ", parentId, pageDimensions)
//              ((seqDocNode ++ recRes._1),
//                (seqClassifiedRectangle ++ recRes._2))
//            }
//            else
//            {
//              def returnParentId(lblName:String, rect:RectangleOnPage, parentId:String)={if(lblName.toUpperCase()=="REFERENCE"){"REFERENCE_" + rect.top.toInt + "_" +
//                rect.left.toInt + "_" + rect.bottom.toInt + "_" + rect.right.toInt + "_" + rect.page.pagenum +  "_"}else{parentId}}
//
//              val rectOnPage:RectangleOnPage = new RectangleOnPage {
//                override val page: Page = new Page(Integer.valueOf((currentNode \ "@pageNum").text),pageDimensions)
//                override val bottom: Float = (currentNode \ "@lly").text.toFloat + 5.0f
//                override val top: Float = (currentNode \ "@ury").text.toFloat + 5.0f
//                override val left: Float = (currentNode \ "@llx").text.toFloat + 5.0f
//                override val right: Float = (currentNode \ "@urx").text.toFloat + 5.0f
//              }
//
//
//              val currNode: DocNode = new DelimitingBox(
//                    returnParentId(currentNode.label, rectOnPage,"") + parentId + parentName + currentNode.label
//                , rectOnPage)
//              val weightedFeatureSet:WeightedSet[Feature] = new WeightedSet[Feature]{
//                val asMap = Map[Feature, Double]()
//              }
//              val weightedStringSet:WeightedSet[String] = new WeightedSet[String]{
//                val asMap = Map[String, Double]()
//              }
//
//
//              if(mapAcceptedLabels.keys.exists(x => x==(parentName + currentNode.label).toUpperCase()))
//              {
//                def getContent(currentNode:scala.xml.Node, currNode:DocNode, completePath:String):String =
//                                    {if(!currentNode.text.toString().contains("\n") && currNode.id.toUpperCase().contains("REFERENCE"))
//                                    {
//                                        ": " + currentNode.text.toString()
//                                    }
//                                    else if(recursiveExtraction.exists(x => x == completePath.toUpperCase()))
//                                    {
//                                        ": " + getRecursiveContent(currentNode)
//                                    }
//                                    else
//                                    {
//                                        ""
//                                    }}
//                def getRecursiveContent(currentNode:scala.xml.Node):String =
//                {
//                  if(currentNode.text.toString().contains("\n"))
//                  {
//
//                    currentNode.child.map(x=> getRecursiveContent(x)).mkString(" ")
//                  }
//                  else
//                  {
//                    currentNode.text
//                  }
//                }
//
//
//                val currClassifiedRectangle: ClassifiedRectangle =
//                        new ClassifiedRectangle(new MetataggerBoxTextAtom(currNode.id,
//                          mapAcceptedLabels.get((parentName + currentNode.label).toUpperCase()).get + getContent(currentNode,currNode,parentName + currentNode.label), "Font", 0.0f,
//                             currNode.rectangle.get, Array[Float](0f))//currNode
//                  , weightedFeatureSet, weightedStringSet, None, List())
//
//                val recRes = processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",returnParentId(currentNode.label, currNode.rectangle.get,parentId),pageDimensions)
//
//                ((seqDocNode ++ recRes._1) :+ currNode,
//                  (seqClassifiedRectangle ++ recRes._2) :+ currClassifiedRectangle)
//              }
//              else
//              {
//                val recRes = processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",returnParentId(currentNode.label, currNode.rectangle.get,parentId),pageDimensions)
//                ((seqDocNode ++ recRes._1),
//                  (seqClassifiedRectangle ++ recRes._2))
//              }
//            }
//          case _ =>
//            val recRes = processXMLRecursive(currentNode.child, parentName + currentNode.label + " -> ",parentId,pageDimensions)
//            ((seqDocNode ++ recRes._1),
//              (seqClassifiedRectangle ++ recRes._2))
//        }
//      }
//
//
//      (res.map{t:((Seq[DocNode], Seq[ClassifiedRectangle])) => t._1}.flatten,
//        res.map{t:((Seq[DocNode], Seq[ClassifiedRectangle])) => t._2}.flatten)
//  }




  def getConcatenation(mapData:Seq[Node]):String =
  {
    mapData.map(x=>
      x.text).mkString(" ")
  }
//
//  def processXMLRecursiveV2(node:Seq[Node], parentName:String, parentId:String, pageDimensions:Rectangle):(Seq[DocNode], Seq[ClassifiedRectangle]) =
//  {
//    val ptrn = "([0-9].*)".r
//    val seqDocNode:Seq[DocNode] = Seq()
//    val seqClassifiedRectangle:Seq[ClassifiedRectangle] = Seq()
//
//    def returnParentId(lblName:String, rect:RectangleOnPage, parentId:String)={if(lblName.toUpperCase()=="REFERENCE"){"REFERENCE_" + rect.top.toInt + "_" +
//      rect.left.toInt + "_" + rect.bottom.toInt + "_" + rect.right.toInt + "_" + rect.page.pagenum +  "_"}else{parentId}}
//
//    val res = for(currentNode <- node.groupBy(_.label))
//    yield
//    { (currentNode._2(0) \ "@llx").text
//    match {
//      case ptrn(_) =>
//
//        if((parentName + currentNode._1).toUpperCase().contains("REFERENCE") &&
//          Math.abs((currentNode._2(0) \ "@lly").text.toFloat - (currentNode._2(0) \ "@ury").text.toFloat)>400)
//        {
//          val recRes:(Seq[DocNode], Seq[ClassifiedRectangle]) = processXMLRecursiveV2(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ",parentId,pageDimensions)
//          val recSiblings:(Seq[DocNode], Seq[ClassifiedRectangle]) = {if(currentNode._2.size>1){processXMLRecursiveV2(currentNode._2.tail, parentName,parentId,pageDimensions)}
//          else
//          {(seqDocNode,seqClassifiedRectangle)}}
//
//          (((seqDocNode ++ recSiblings._1) ++ recRes._1),
//            ((seqClassifiedRectangle ++ recSiblings._2) ++ recRes._2))
//        }
//        else
//        {
//          def mergeIsApplicable() = {
//            if(mergeContent.exists(x => x == (parentName + currentNode._1).toUpperCase() ))
//            {
//              true
//            }
//            else{false}
//          }
//          val rectOnPage:RectangleOnPage = new RectangleOnPage {
//            override val page: Page = new Page(Integer.valueOf((currentNode._2(0) \ "@pageNum").text),pageDimensions)
//            override val bottom: Float = (currentNode._2(0) \ "@lly").text.toFloat + 2.0f
//            override val top: Float = (currentNode._2(0) \ "@ury").text.toFloat + 2.0f
//            override val left: Float = (currentNode._2(0) \ "@llx").text.toFloat - 7.0f
//            override val right: Float = (currentNode._2(0) \ "@urx").text.toFloat - 7.0f
//          }
//          val currNode: DocNode = new DelimitingBox(
//            returnParentId(currentNode._2(0).label, rectOnPage,"") + parentId + parentName + currentNode._2(0).label
//            , rectOnPage)
//          val weightedFeatureSet:WeightedSet[Feature] = new WeightedSet[Feature]{
//            val asMap = Map[Feature, Double]()
//          }
//          val weightedStringSet:WeightedSet[String] = new WeightedSet[String]{
//            val asMap = Map[String, Double]()
//          }
//
//
//          if(mapAcceptedLabels.keys.exists(x => x==(parentName + currentNode._2(0).label).toUpperCase()))
//          {
//            def getContent(currntNode:scala.xml.Node, currNode:DocNode, completePath:String):String =
//            {
//              if(mergeIsApplicable())
//              {
//                ": " + getConcatenation(currentNode._2 )
//              }
//              else if(!currntNode.text.toString().contains("\n") && currNode.id.toUpperCase().contains("REFERENCE"))
//              {
//                ": " + currntNode.text.toString()
//              }
//              else if(recursiveExtraction.exists(x => x == completePath.toUpperCase()))
//              {
//                ": " + getRecursiveContent(currntNode)
//              }
//              else
//              {
//                ""
//              }}
//
//            def getRecursiveContent(currentNode:scala.xml.Node):String =
//            {
//              if(currentNode.text.toString().contains("\n"))
//              {
//
//                currentNode.child.map(x=> getRecursiveContent(x)).mkString(" ")
//              }
//              else
//              {
//                currentNode.text
//              }
//            }
//
//            val recRes = processXMLRecursiveV2(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ",returnParentId(currentNode._2(0).label, currNode.rectangle.get,parentId),pageDimensions)
//
//            val recSiblings = {if(currentNode._2.size>1){processXMLRecursiveV2(currentNode._2.tail, parentName,parentId,pageDimensions)}
//            else
//            {(seqDocNode,seqClassifiedRectangle)}}
//
//
//            val currClassifiedRectangle: ClassifiedRectangle =
//              new ClassifiedRectangle(new MetataggerBoxTextAtom(currNode.id,
//                {mapAcceptedLabels.get((parentName + currentNode._2(0).label).toUpperCase()).get} +
//                  getContent(currentNode._2(0),currNode,parentName + currentNode._2(0).label), "Font", 0.0f,
//                currNode.rectangle.get, Array[Float](0f))//currNode
//                , weightedFeatureSet, weightedStringSet, None, List())
//
//
//            ((((seqDocNode) ++ recSiblings._1) ++ recRes._1) :+ currNode,
//              (((seqClassifiedRectangle) ++ {if(mergeIsApplicable()){
//                recSiblings._2.map(x=>currClassifiedRectangle)
//              }else{recSiblings._2}}) ++ recRes._2):+ currClassifiedRectangle )
//          }
//          else
//          {
//            val recRes:(Seq[DocNode], Seq[ClassifiedRectangle]) = processXMLRecursiveV2(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ",returnParentId(currentNode._2(0).label, currNode.rectangle.get,parentId),pageDimensions)
//            val recSiblings:(Seq[DocNode], Seq[ClassifiedRectangle]) = {if(currentNode._2.size>1){processXMLRecursiveV2(currentNode._2.tail, parentName,parentId,pageDimensions)}
//            else
//            {(seqDocNode,seqClassifiedRectangle)}}
//
//            (((seqDocNode ++ recSiblings._1) ++ recRes._1),
//              ((seqClassifiedRectangle ++ recSiblings._2) ++ recRes._2))
//          }
//        }
//      case _ =>
//        val recRes = processXMLRecursiveV2(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ", parentId
//          ,pageDimensions)
//        ((seqDocNode ++ recRes._1),
//          (seqClassifiedRectangle ++ recRes._2))
//    }
//    }
//
//    (res.toSeq.map{t:((Seq[DocNode], Seq[ClassifiedRectangle])) => t._1}.flatten,
//      res.toSeq.map{t:((Seq[DocNode], Seq[ClassifiedRectangle])) => t._2}.flatten)
//  }


  def processXMLRecursiveV3(node:Seq[Node], parentName:String, parentId:String, pageDimensions:Rectangle):Seq[ClassifiedRectangle] =
  {

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

        if((parentName + currentNode._1).toUpperCase().contains("REFERENCE") &&
          Math.abs((currentNode._2(0) \ "@lly").text.toFloat - (currentNode._2(0) \ "@ury").text.toFloat)>400)
        {
          val recRes:Seq[ClassifiedRectangle] = processXMLRecursiveV3(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ",parentId,pageDimensions)
          val recSiblings:Seq[ClassifiedRectangle] = {if(currentNode._2.size>1){processXMLRecursiveV3(currentNode._2.tail, parentName,parentId,pageDimensions)}
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
          val rectOnPage:RectangleOnPage = new RectangleOnPage {
            override val page: Page = new Page(Integer.valueOf((currentNode._2(0) \ "@pageNum").text),pageDimensions)
            override val bottom: Float = (currentNode._2(0) \ "@lly").text.toFloat + 2.0f
            override val top: Float = (currentNode._2(0) \ "@ury").text.toFloat + 2.0f
            override val left: Float = (currentNode._2(0) \ "@llx").text.toFloat - 7.0f
            override val right: Float = (currentNode._2(0) \ "@urx").text.toFloat - 7.0f
          }
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
            def getContent(currntNode:scala.xml.Node, currNode:DocNode, completePath:String):String =
            {
              if(mergeIsApplicable())
              {
                ": " + getConcatenation(currentNode._2 )
              }
              else if(!currntNode.text.toString().contains("\n") && currNode.id.toUpperCase().contains("REFERENCE"))
              {
                ": " + currntNode.text.toString()
              }
              else if(recursiveExtraction.exists(x => x == completePath.toUpperCase()))
              {
                ": " + getRecursiveContent(currntNode)
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

            val recRes = processXMLRecursiveV3(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ",returnParentId(currentNode._2(0).label, currNode.rectangle.get,parentId),pageDimensions)

            val recSiblings:Seq[ClassifiedRectangle] = {if(currentNode._2.size>1){processXMLRecursiveV3(currentNode._2.tail, parentName,parentId,pageDimensions)}
            else
            {(seqClassifiedRectangle)}}


            val currClassifiedRectangle: ClassifiedRectangle =
              new ClassifiedRectangle(new MetataggerBoxTextAtom(currNode.id,
                {mapAcceptedLabels.get((parentName + currentNode._2(0).label).toUpperCase()).get} +
                  getContent(currentNode._2(0),currNode,parentName + currentNode._2(0).label), "Font", 0.0f,
                currNode.rectangle.get, Array[Float](0f))//currNode
                , weightedFeatureSet, weightedStringSet, None, List())


              (((seqClassifiedRectangle) ++ {if(mergeIsApplicable()){
                recSiblings.map(x=>currClassifiedRectangle)
              }else{recSiblings}}) ++ recRes) :+ currClassifiedRectangle
          }
          else
          {
            val recRes:(Seq[ClassifiedRectangle]) = processXMLRecursiveV3(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ",returnParentId(currentNode._2(0).label, currNode.rectangle.get,parentId),pageDimensions)
            val recSiblings:( Seq[ClassifiedRectangle]) = {if(currentNode._2.size>1){processXMLRecursiveV3(currentNode._2.tail, parentName,parentId,pageDimensions)}
            else
            {(seqClassifiedRectangle)}}

            (((seqClassifiedRectangle ++ recSiblings) ++ recRes))
          }
        }
      case _ =>
        val recRes = processXMLRecursiveV3(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ", parentId
          ,pageDimensions)
          (seqClassifiedRectangle ++ recRes)
    }
    }

      res.flatten.toSeq
  }







  def processXMLRecursiveV4(node:Seq[Node], parentName:String, parentId:String, pageDimensions:Rectangle):Seq[ClassifiedRectangle] =
  {

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
          Math.abs((currentNode._2(0) \ "@lly").text.toFloat - (currentNode._2(0) \ "@ury").text.toFloat)>400)
        {
          val recRes:Seq[ClassifiedRectangle] = processXMLRecursiveV4(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ",parentId,pageDimensions)
          val recSiblings:Seq[ClassifiedRectangle] = {if(currentNode._2.size>1){processXMLRecursiveV4(currentNode._2.tail, parentName,parentId,pageDimensions)}
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
//          val rectOnPage:RectangleOnPage = new RectangleOnPage {
//            override val page: Page = new Page(Integer.valueOf((currentNode._2(0) \ "@pageNum").text),pageDimensions)
//            override val bottom: Float = (currentNode._2(0) \ "@lly").text.toFloat + 2.0f
//            override val top: Float = (currentNode._2(0) \ "@ury").text.toFloat + 2.0f
//            override val left: Float = (currentNode._2(0) \ "@llx").text.toFloat - 7.0f
//            override val right: Float = (currentNode._2(0) \ "@urx").text.toFloat - 7.0f
//          }
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
            def getContent(currntNode:scala.xml.Node, currNode:DocNode, completePath:String):String =
            {
              if(mergeIsApplicable())
              {
                ": " + getConcatenation(currentNode._2 )
              }
              else if(!currntNode.text.toString().contains("\n") && currNode.id.toUpperCase().contains("REFERENCE"))
              {
                ": " + currntNode.text.toString()
              }
              else if(recursiveExtraction.exists(x => x == completePath.toUpperCase()))
              {
                ": " + getRecursiveContent(currntNode)
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
            //for extracting the author data in a particular node
            def extractAuthors(currentNode:Seq[scala.xml.Node], currentAuthorId:Int):Seq[ClassifiedRectangle] =
            {
              val currentAuthor = currentNode.head
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

            val recRes = processXMLRecursiveV4(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ",returnParentId(currentNode._2(0).label, currNode.rectangle.get,parentId),pageDimensions)

            val recSiblings:Seq[ClassifiedRectangle] = {if(currentNode._2.size>1){processXMLRecursiveV4(currentNode._2.tail, parentName,parentId,pageDimensions)}
            else
            {(seqClassifiedRectangle)}}


            val currClassifiedRectangle: ClassifiedRectangle =
              new ClassifiedRectangle(new MetataggerBoxTextAtom(currNode.id,
                {mapAcceptedLabels.get((parentName + currentNode._2(0).label).toUpperCase()).get} +
                  getContent(currentNode._2(0),currNode,parentName + currentNode._2(0).label), "Font", 0.0f,
                currNode.rectangle.get, Array[Float](0f))//currNode
                , weightedFeatureSet, weightedStringSet, None, List())


            (((seqClassifiedRectangle) ++ {if(mergeIsApplicable()){
              recSiblings.map(x=>currClassifiedRectangle)
            }else{recSiblings}}) ++ recRes) :+ currClassifiedRectangle
          }
          else
          {
            val recRes:(Seq[ClassifiedRectangle]) = processXMLRecursiveV4(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ",returnParentId(currentNode._2(0).label, currNode.rectangle.get,parentId),pageDimensions)
            val recSiblings:( Seq[ClassifiedRectangle]) = {if(currentNode._2.size>1){processXMLRecursiveV4(currentNode._2.tail, parentName,parentId,pageDimensions)}
            else
            {(seqClassifiedRectangle)}}

            (((seqClassifiedRectangle ++ recSiblings) ++ recRes))
          }
        }
      case _ =>
        val recRes = processXMLRecursiveV4(currentNode._2(0).child, parentName + currentNode._2(0).label + " -> ", parentId
          ,pageDimensions)
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
