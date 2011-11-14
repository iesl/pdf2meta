package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import edu.umass.cs.iesl.pdf2meta.cli.metadatamodel.MetadataModel
import edu.umass.cs.iesl.pdf2meta.cli.util.WeightedSet
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{DelimitingBox, Page, DocNode}


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

class ClassifiedRectangles(val raw: Seq[ClassifiedRectangle])
  {

  def legit = raw.filter(_.label.map(s => !s.equalsIgnoreCase("discard")).getOrElse(true))
  def discarded = raw.filter(_.discarded)
  val delimiters = raw.map(_.node).filter({case x : DelimitingBox => true; case _ => false})

  def onPage(page: Page) =
    {
    new ClassifiedRectangles(raw.filter(_.node.rectangle match
                                        {
                                          case Some(x) => x.page == page;
                                          case None => false;
                                        }))
    }

  def toMetadataModel: MetadataModel =
    {
    new MetadataModel
      {
      val body = raw.filter(_.label != "body").flatMap(_.label).mkString(" ")
      val title = raw.filter(_.label != "title").flatMap(_.label).mkString(" ")
      val paperabstract = raw.filter(_.label != "abstract").flatMap(_.label).mkString(" ")
      val referenceStrings = List("not implemented") //raw.filter(_._2 != "references").flatMap(_._2).mkString(" ")
      val authors = List.empty //"not implemented" //raw.filter(_._2 != "authors").flatMap(_._2).mkString(" ")
      }
    }
  }
