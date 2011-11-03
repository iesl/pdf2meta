package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

import collection.Seq
import edu.umass.cs.iesl.pdf2meta.cli.util.OrderedTreeNode

/*
object DocNode
  {
  def apply(id: String, children: Seq[DocNode]): DocNode =
    {
    if (children.exists({case x: TextBox => true; case _ => false}))
      {new TextBox(id, children)}
    else new DocNode(id, children)
    }
  }
class DocNode(val id: String, override val children: Seq[DocNode])
        extends DocNode(id, children) //with Rectangular with OnPage
  {

  /*  def create(idA: String, childrenA: Seq[DocNode]) =
      {
      DocNode(idA, childrenA)
      }*/
  def :+(r: DocNode): DocNode =
    {
    new DocNode(id + "+" + r.id, children :+ r)
    }
  }

*/
object DocNode
  {
  def apply(id: String, children: Seq[DocNode], localInfo: Option[Iterator[String]], localErrors: Option[Iterator[String]], isAtomic: Boolean): DocNode =
    {
    /* if (children.exists({case x: TextContainer => true; case _ => false}))
          {new DocNode(id, children)}
        else */

    new DocNode(id, children, localInfo, localErrors, isAtomic)
    }

  val begin = new DocNode("begin", Nil, None, None, false)

  //def apply(id: String, children: Seq[DocNode]) = DocNode(id, children, None, None);
  }

object PartitionedDocNode
  {
  def apply(id: String, children: Seq[DocNode], localInfo: Option[Iterator[String]], localErrors: Option[Iterator[String]]): DocNode =
    {
    /* if (children.exists({case x: TextContainer => true; case _ => false}))
          {new DocNode(id, children)}
        else */

    new PartitionedDocNode(id, children, localInfo, localErrors)
    }

  //def apply(id: String, children: Seq[DocNode]) = DocNode(id, children, None, None);
  }


// these are now represented as optional members on DocNode
trait RectangularOnPage
  {
  def rectangle: Option[RectangleOnPage]
  }

/*trait OnPage
  {
  def page: Option[Int]
  }
*/
/**
 * A node whose children represent established partitions
 */
class PartitionedDocNode(override val id: String, override val children: Seq[DocNode], override val localInfo: Option[Iterator[String]], override val localErrors: Option[Iterator[String]])
        extends DocNode(id, children, localInfo, localErrors, false)

class DocNode(val id: String, val children: Seq[DocNode], val localInfo: Option[Iterator[String]], val localErrors: Option[Iterator[String]], val isAtomic: Boolean)
        extends OrderedTreeNode[DocNode] with RectangularOnPage with TextBox
  {


lazy val charSpanProportional: Map[DocNode, (Double, Double)] =
    {
    def appendEnd(l: List[(DocNode, Int)], n: DocNode): List[(DocNode, Int)] =
      {
      (n, l.head._2 + n.text.length) :: l
      }

    val charEnds: List[(DocNode, Int)] = allAtoms.foldLeft(List[(DocNode, Int)]((DocNode.begin, 0)))(appendEnd).reverse

    def selfzip(l: List[(DocNode, (Int, Int))], e: (DocNode, Int)): List[(DocNode, (Int, Int))] =
      {
      (e._1, (l.head._2._2, e._2)) :: l
      }
    val charBeginEnds: List[(DocNode, (Int, Int))] = charEnds.foldLeft(List[(DocNode, (Int, Int))]((DocNode.begin, (0, 0))))(selfzip).reverse
    val total = text.length.toDouble
    val proportional = charBeginEnds.map((x: (DocNode, (Int, Int))) => (x._1, (x._2._1 / total, x._2._2 / total)))
    proportional.toMap
    }

  // the deepest ancestor with a rectangle, if any
  // def onpage: Option[DocNode]
  lazy val rectangle: Option[RectangleOnPage] = computeRectangle

  def computeRectangle: Option[RectangleOnPage] =
    {
    val childRects: Seq[Option[RectangleOnPage]] = children.map((x: DocNode) => x.rectangle)
    if (childRects.exists(x => (x == None)))
      {
      None
      }
    else
      {
      RectangleOnPage.encompassing(childRects.flatten, 0)
      }
    }


  lazy val errors: Option[Iterator[String]] =
    {
    val r: Iterator[String] = children.map(_.errors).flatten.foldLeft[Iterator[String]](localErrors.getOrElse(Iterator.empty))((a: Iterator[String], b: Iterator[String]) =>
                                                                                                                                 {
                                                                                                                                 a ++
                                                                                                                                 b
                                                                                                                                 })
    if (r.isEmpty) None else Some(r)
    }


  lazy val info: Option[Iterator[String]] =
    {
    val r: Iterator[String] = children.map(_.info).flatten.foldLeft[Iterator[String]](localInfo.getOrElse(Iterator.empty))((a: Iterator[String], b: Iterator[String]) =>
                                                                                                                             {
                                                                                                                             a ++
                                                                                                                             b
                                                                                                                             })
    if (r.isEmpty) None else Some(r)
    }

  /*  lazy val page: Option[Int] = computePage
def computePage: Option[Int] =
    {
    val d = children.map(_.page).distinct
    d.length match
    {
      case 1 => d(0)
      case _ => None
    }
    }
*/
  /**
   * recursively collect all nodes in breadth-first order
   */
  def allNodes: List[DocNode] =
    {
    val childNodeLists: List[List[DocNode]] = children.map(_.allNodes).toList
    val f: List[DocNode] = childNodeLists.flatten
    children.toList ::: f
    }

  def allLeaves: List[DocNode] = allNodes.filter(_.children.isEmpty)

  // collect the shallowest set of nodes that are marked "atomic" and span the doc
  def allAtoms: List[DocNode] =
    {
    if (isAtomic)
      {List(this)}
    else
      {
      val childNodeLists: List[List[DocNode]] = children.map(_.allAtoms).toList
      val f: List[DocNode] = childNodeLists.flatten
      //children.toList ::: f
      f
      }
    }


  /*  def allDocNodes: scala.Seq[DocNode] =
        {
        allNodes.collect({case x: TextLine => None; case x: TextBox => None; case x: DocNode => Some(x)}).flatten
        }*/
/*  def textBoxes: Seq[DocNode] =
    {
    allNodes.collect({
                     //case x: TextLine => None;
                     case x: DelimitingBox => None;
                     case x: TextBox => Some(x)
                     }).flatten
    }
  def textBoxChildren: Seq[DocNode] =
    {
    children.collect({
                     //case x: TextLine => None;
                     case x: DelimitingBox => None;
                     case x: TextBox => Some(x)
                     }).flatten
    }
*/
  def delimitingBoxes: scala.Seq[DelimitingBox] =
    {
    val all = allNodes
    val result = all.collect({
                             case x: DelimitingBox => x
                             //                     case x: RectBox => throw new Error("RectBox is a DelimitingBox")
                             })
    result
    }

  // for debugging
  val delimiters = delimitingBoxes.length

  def create(childrenA: Seq[DocNode]) =
    {
    DocNode(id, childrenA, localInfo, localErrors, false)
    }

  def :+(r: DocNode): DocNode =
    {
    if (children.isEmpty)
      {
      DocNode(id + "+" + r.id, List(this, r), None, None, true)
      }
    else
      {
      DocNode(id + "+" + r.id, children :+ r, localInfo, localErrors, true)
      }
    }
  }

















