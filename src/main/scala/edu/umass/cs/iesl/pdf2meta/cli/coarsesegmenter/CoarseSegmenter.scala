package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{DelimitingBox, Page, DocNode}
import edu.umass.cs.iesl.scalacommons.collections.WeightedSet
import edu.umass.cs.iesl.bibmogrify.model._


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
  val body = raw.filter(_.label != "body").flatMap(_.label).mkString(" ")
  val title = raw.filter(_.label != "title").flatMap(_.label).mkString(" ")
  val paperAbstract = raw.filter(_.label != "abstract").flatMap(_.label).mkString(" ")
  val referenceStrings = Nil //raw.filter(_._2 != "references").flatMap(_._2).mkString(" ")
  val referenceIds = Nil
  val venue = None
  val authors = Nil //"not implemented" //raw.filter(_._2 != "authors").flatMap(_._2).mkString(" ")
  val year = None
  }

object ClassifiedRectangles
{
  implicit def classifiedRectanglesToCitationMention : (ClassifiedRectangles) => CitationMention = cr =>
  {
    new CitationMention {
      override val title = cr.title
      override val doctype = JournalArticle
      override val abstractText: Option[String] = Some(cr.paperAbstract)
      override val bodyText: Option[String] = Some(cr.body)
    }
  }
}
