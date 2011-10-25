package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import edu.umass.cs.iesl.pdf2meta.cli.metadatamodel.MetadataModel
import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter.CoarseSegmenterTypes._
import edu.umass.cs.iesl.pdf2meta.cli.util.WeightedSet
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{RectangleOnPage, Page, DocNode}

trait CoarseSegmenterComponent
  {
  // because this component has no dependencies, we don't need a self type here, and the service class needn't be
  // inner.
  def coarseSegmenter: CoarseSegmenter
  }

trait CoarseSegmenter extends Function1[DocNode, ClassifiedRectangles]


object CoarseSegmenterTypes
  {
  type ClassifiedRectangle = (DocNode, String, WeightedSet[Feature], WeightedSet[String])
  }


class ClassifiedRectangles(val raw: Seq[ClassifiedRectangle])
  {

  def legit = raw.filter(_._2 != "discard")
  def discarded = raw.filter(_._2 == "discard")

  def onPage(page : Page) = new ClassifiedRectangles(raw.filter(_._1.rectangle match { case Some(x) => x.page == page; case None => false; }))

  //def removeRedundant(a:ClassifiedRectangle, b:ClassifiedRectangle) : ClassifiedRectangle = {}
  def legitNonRedundant(current: String, l: List[ClassifiedRectangle]): List[ClassifiedRectangle] =
    {
    l match
    {
      case (x, y, f, s) :: t if (y == current) => (x, "", f, s) :: legitNonRedundant(current, t)
      case (x, y, f, s) :: t => (x, y, f, s) :: legitNonRedundant(y, t)
      case Nil => Nil
    }
    }

  def legitNonRedundant: List[ClassifiedRectangle] = legitNonRedundant("Bogus", legit.toList)

  def toMetadataModel: MetadataModel =
    {
    new MetadataModel
      {
      val body = raw.filter(_._2 != "body").flatMap(_._2).mkString(" ")
      val title = raw.filter(_._2 != "title").flatMap(_._2).mkString(" ")
      val paperabstract = raw.filter(_._2 != "abstract").flatMap(_._2).mkString(" ")
      val referenceStrings = List("not implemented") //raw.filter(_._2 != "references").flatMap(_._2).mkString(" ")
      val authors = List.empty //"not implemented" //raw.filter(_._2 != "authors").flatMap(_._2).mkString(" ")
      }
    }
  }
