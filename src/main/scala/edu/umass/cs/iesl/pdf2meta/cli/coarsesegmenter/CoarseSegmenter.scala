package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import edu.umass.cs.iesl.pdf2meta.cli.metadatamodel.MetadataModel
import edu.umass.cs.iesl.pdf2meta.cli.util.WeightedSet
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{DelimitingBox, Page, DocNode}

/*
trait CoarseSegmenterComponent
  {
  // because this component has no dependencies, we don't need a self type here, and the service class needn't be
  // inner.
  def coarseSegmenter: CoarseSegmenter
  }
*/

trait CoarseSegmenter extends Function1[DocNode, ClassifiedRectangles]


/*object CoarseSegmenterTypes
  {
  type ClassifiedRectangle = (DocNode, String, WeightedSet[Feature], WeightedSet[String])
  }*/
case class ClassifiedRectangle(node: DocNode, featureWeights: WeightedSet[Feature], labelWeights: WeightedSet[String], basedOn: Option[ClassifiedRectangle])
  {
  //lazy val bestLabel : Option[String] = labelWeights.unambiguousBest(.9)
  def callLabel(os: Option[String]): ClassifiedRectangle =
    {
    /*os.map(s =>
             {
             // the idea here is that we add weight 0.5005 for the called label, and scale down the rest
             // that guarantees that the called label is the highest-weighted one (probably by a substantial margin), while preserving info about the other label weights
             val newWeights = labelWeights.normalized |+| new WeightedSet[String]()
               {
               def asMap = Map((s, 1.01))
               }
             new ClassifiedRectangle(node, featureWeights, newWeights.normalized)
             }).getOrElse(this)*/
    //val s = os.getOrElse("[Called None]")
    // the idea here is that we add weight 0.5005 for the called label, and scale down the rest
    // that guarantees that the called label is the highest-weighted one (probably by a substantial margin), while preserving info about the other label weights
    /* val newWeights = labelWeights.normalized |+| new WeightedSet[String]()
          {
          def asMap = Map((s.toUpperCase, 1.01))
          }*/

    os.map(s =>
             {
             val newWeights = new WeightedSet[String]()
               {
               def asMap = Map((s, 1.0)) // s.toUpperCase
               }

             new ClassifiedRectangle(node, featureWeights, newWeights.normalized, Some(this))
             }).getOrElse(this)
    }

  lazy val label: Option[String] = labelWeights.unambiguousBest(0.9) match
  {
    case Some(x) => Some(x)
    case None => basedOn.flatMap(_.label) // "[ambiguous]"
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
  /*
  //def removeRedundant(a:ClassifiedRectangle, b:ClassifiedRectangle) : ClassifiedRectangle = {}
  def legitNonRedundant(current: String, l: List[ClassifiedRectangle]): List[ClassifiedRectangle] =
    {
    l match
    {
      case (x: ClassifiedRectangle) :: t if (x.bestLabel == current) => ClassifiedRectangle(x, f, s) :: legitNonRedundant(current, t)
      case ClassifiedRectangle(x, f, s) :: t => ClassifiedRectangle(x, f, s) :: legitNonRedundant(y, t)
      case Nil => Nil
    }
    }

  def legitNonRedundant: List[ClassifiedRectangle] = legitNonRedundant("Bogus", legit.toList)
*/
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
