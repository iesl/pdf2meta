package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{DelimitingBox, Page, DocNode}
import edu.umass.cs.iesl.scalacommons.collections.WeightedSet
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import collection.Seq


trait CoarseSegmenter extends Function1[DocNode, ClassifiedRectangles]

case class ClassifiedRectangle(node: DocNode, featureWeights: WeightedSet[Feature], labelWeights: WeightedSet[String], basedOn: Option[ClassifiedRectangle])
  {
  def callLabel(os: Option[String]): ClassifiedRectangle =
    {

    os.map(s =>
             {
             val newWeights = new WeightedSet[String]()
               {
               def asMap = Map((s, 1.0))
               }

             new ClassifiedRectangle(node, featureWeights, newWeights.normalized, Some(this))
             }).getOrElse(this)
    }

  lazy val label: Option[String] = labelWeights.unambiguousBest(0.9) match
  {
    case Some(x) => Some(x)
    case None => basedOn.flatMap(_.label)
  }

  def discarded = label.map(_.equalsIgnoreCase("discard")).getOrElse(false)
  }

class ClassifiedRectangles(val raw: Seq[ClassifiedRectangle]) // extends CitationMention
  {

  def legit = raw.filter(_.label.map(s => !s.equalsIgnoreCase("discard")).getOrElse(true))
  def discarded = raw.filter(_.discarded)
  val delimiters = raw.map(_.node).filter({case x: DelimitingBox => true; case _ => false})

  def onPage(page: Page) =
    {
    new ClassifiedRectangles(raw.filter(_.node.rectangle match
                                        {
                                          case Some(x) => x.page == page;
                                          case None => false;
                                        }))
    }


  val docid = "bogus"
  val sourcefile = "bogus"
  val body = {
    val bodyNodes: Seq[ClassifiedRectangle] = raw.filter(_.label.map(_.equals("body")).getOrElse(false))
    val bodyNodeTexts: Seq[String] = bodyNodes.map(_.node.text)
    bodyNodeTexts.mkString(" ")
  }
  val title = {
    val titleNodes: Seq[ClassifiedRectangle] = raw.filter(_.label.map(_.equals("title")).getOrElse(false))
    val titleNodeTexts: Seq[String] = titleNodes.map(_.node.text)
    titleNodeTexts.mkString(" ")
  }
  val paperAbstract = raw.filter(_.label.map(_.equals("abstract")).getOrElse(false)).map(_.node.text).mkString(" ")
    val authors = raw.filter(_.label.map(_.equals("authors")).getOrElse(false)).map(_.node.text).mkString(" ")
  val referenceStrings = Nil //raw.filter(_._2 != "references").flatMap(_._2).mkString(" ")
  val referenceIds = Nil
  val venue = None
  val year = None
  }

object ClassifiedRectangles
{
  implicit def classifiedRectanglesToCitationMention : (ClassifiedRectangles) => StructuredCitation = cr =>
  {
    new StructuredCitation {
      override val title : Option[String] = cr.title
      override val doctype : Option[DocType] = JournalArticle
      override val abstractText: Option[String] = Some(cr.paperAbstract)
      override val bodyText: Option[String] = Some(cr.body)
      override val authors = {
        val a = cr.authors.split("and|,").map(fullname => new AuthorInRole(new Person() {
          override val name = Some(fullname)
        },Nil)).toList
        a
      }
    }
  }
}
