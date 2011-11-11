package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

import collection.Seq
import edu.umass.cs.iesl.pdf2meta.cli.util.OrderedTreeNode
import com.weiglewilczek.slf4s.Logging

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
  def apply(id: String, children: Seq[DocNode], localInfo: Option[Iterator[String]], localErrors: Option[Iterator[String]], isAtomic: Boolean, isMergeable: Boolean): DocNode =
    {
    /* if (children.exists({case x: TextContainer => true; case _ => false}))
          {new DocNode(id, children)}
        else */

    new DocNode(id, children, localInfo, localErrors, isAtomic, isMergeable)
    }

  val begin = new DocNode("begin", Nil, None, None, false, false)

  //def apply(id: String, children: Seq[DocNode]) = DocNode(id, children, None, None);
  }

/*object PartitionedDocNode
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
*/
object AnnotatedDocNode
  {
  def apply(id: String, children: Seq[DocNode], localInfo: Option[Iterator[String]], localErrors: Option[Iterator[String]], isAtomic: Boolean, isMergeable: Boolean,
            annotations: Seq[String]): DocNode =
    {
    /* if (children.exists({case x: TextContainer => true; case _ => false}))
          {new DocNode(id, children)}
        else */

    new AnnotatedDocNode(id, children, localInfo, localErrors, isAtomic, isMergeable, annotations)
    }

  //def apply(id: String, children: Seq[DocNode]) = DocNode(id, children, None, None);
  }

class AnnotatedDocNode(override val id: String, override val children: Seq[DocNode], override val localInfo: Option[Iterator[String]], override val localErrors: Option[Iterator[String]],
                       override val isAtomic: Boolean, isMergeable: Boolean, val annotations: Seq[String])
        extends DocNode(id, children, localInfo, localErrors, isAtomic, isMergeable)
  {

  override def create(childrenA: Seq[DocNode]) =
    {
    AnnotatedDocNode(id, childrenA, localInfo, localErrors, isAtomic, isMergeable, annotations)
    }

  override def makeAtomic =
    {
    if (isAtomic) this
    else
      AnnotatedDocNode(id, children, localInfo, localErrors, true, isMergeable, annotations)
    }
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
/*class PartitionedDocNode(override val id: String, override val children: Seq[DocNode], override val localInfo: Option[Iterator[String]], override val localErrors: Option[Iterator[String]])
        extends DocNode(id, children, localInfo, localErrors, false)
  {

  override def create(childrenA: Seq[DocNode]) =
    {
    PartitionedDocNode(id, childrenA, localInfo, localErrors)
    }
  override def makeAtomic = this
  }
*/
class DocNode(val id: String, val children: Seq[DocNode], val localInfo: Option[Iterator[String]], val localErrors: Option[Iterator[String]], val isAtomic: Boolean, val isMergeable: Boolean)
        extends OrderedTreeNode[DocNode] with RectangularOnPage with TextBox with Logging
  {


  lazy val charSpanProportional: Map[DocNode, (Double, Double)] =
    {
    def appendEnd(l: List[(DocNode, Int)], n: DocNode): List[(DocNode, Int)] =
      {
      // add one to the length to account for the space that gets added between adjacent nodes, at TextContainer.text
      (n, l.head._2 + n.text.length + 1) :: l
      }

    val charEnds: List[(DocNode, Int)] = spanningAtoms.foldLeft(List[(DocNode, Int)]((DocNode.begin, 0)))(appendEnd).reverse

    def selfzip(l: List[(DocNode, (Int, Int))], e: (DocNode, Int)): List[(DocNode, (Int, Int))] =
      {
      (e._1, (l.head._2._2, e._2)) :: l
      }
    val charBeginEnds: List[(DocNode, (Int, Int))] = charEnds.foldLeft(List[(DocNode, (Int, Int))]((DocNode.begin, (0, 0))))(selfzip).reverse
    val total = text.length.toDouble
    val proportional = charBeginEnds.map((x: (DocNode, (Int, Int))) => (x._1, (x._2._1 / total, x._2._2 / total)))
    proportional.toMap
    }


  // a rectangle may be None if the node has children on multiple pages.
  // this may not be the best way to handle this situation, as we frequently do foobar.rectangle.get.height or whatever, which is error-prone, and handling Otions in every case is a hassle.
  lazy val rectangle: Option[RectangleOnPage] = computeRectangle

  // allow backing off to a "core" of the content for ordering when it's ambiguous using the full rectangle
  def coreRectangle = rectangle

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
   * excludes the root, but we don't care
   */
  def allNodesBreadthFirst: List[DocNode] =
    {
    val childNodeLists: List[List[DocNode]] = children.map(_.allNodesBreadthFirst).toList
    val f: List[DocNode] = childNodeLists.flatten
    children.toList ::: f
    }

  /*
  def allNodesBreadthFirstExceptAtomDescendants: List[DocNode] =
    {
    if (isAtomic)
      {
      List(this)
      }
    else
      {
      val childNodeLists: List[List[DocNode]] = children.map(_.allNodesBreadthFirst).toList
      val f: List[DocNode] = childNodeLists.flatten
      children.toList ::: f
      }
    }
*/
  /**
   * recursively collect all nodes in preorder depth-first order
   */
  def allNodesDepthFirst: List[DocNode] =
    {
    val childNodeLists: List[List[DocNode]] = children.map(_.allNodesDepthFirst).toList
    val f: List[DocNode] = childNodeLists.flatten
    this :: f
    }


  def allLeaves: Seq[DocNode] = if (children.isEmpty) List(this) else children.flatMap(_.allLeaves)

  // collect the shallowest set of nodes that are marked "atomic" and span the doc
  def spanningAtoms: List[DocNode] =
    {
    if (isAtomic)
      {
      /*this match
      {
        case ana: AnnotatedDocNode =>
          {
          logger.debug("sideways atom")
          }
        case _ =>
      }
      this match
      {
        case x: DelimitingBox =>
          {
          logger.debug("Atomic DelimitingNode");
          }
        case _ =>
      }*/
      List(this)
      }
    else
      {
      /*this match
      {
        case ana: AnnotatedDocNode =>
          {
          logger.debug("sideways nonatom")
          }
        case _ =>
      }*/
      val childNodeLists: List[List[DocNode]] = children.map(_.spanningAtoms).toList
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
    val all = allNodesDepthFirst
    val result = all.collect({
                             case x: DelimitingBox => x
                             //                     case x: RectBox => throw new Error("RectBox is a DelimitingBox")
                             })
    result
    }


  def textLines: scala.Seq[TextLine] =
    {
    val all = allNodesDepthFirst
    val result = all.collect({
                             case x: TextLine => x
                             //                     case x: RectBox => throw new Error("RectBox is a DelimitingBox")
                             })
    result
    }
  /*
def partitionedChildren : Seq[PartitionedDocNode] =
        {
        children.collect({case x: PartitionedDocNode => x })
        }
*/
  // for debugging
  val delimiters = delimitingBoxes.length

  def create(childrenA: Seq[DocNode]) =
    {
    DocNode(id, childrenA, localInfo, localErrors, isAtomic, isMergeable)
    }
  def makeAtomic =
    {
    if (isAtomic) this
    else
      DocNode(id, children, localInfo, localErrors, true, isMergeable)
    }

  // join two mergeable atoms into one mergeable atom
  def :+(r: DocNode): DocNode =
    {
    require(!(this.isInstanceOf[AnnotatedDocNode] || r.isInstanceOf[AnnotatedDocNode]))
    //require(isMergeable && isAtomic)
    //require(r.isMergeable && r.isAtomic)

    val atomic = isMergeable && r.isMergeable

    if (children.isEmpty)
      {
      DocNode(id + "+" + r.id, List(this, r), None, None, atomic, true)
      }
    else
      {
      DocNode(id + "+" + r.id, children :+ r, localInfo, localErrors, atomic, true)
      }
    }

  /* override def toString =
      {
      "DocNode with " + children.length + " children, " + allNodesDepthFirst.length + " nodes, " + spanningAtoms.length + " atoms, " + allLeaves.length + " leaves, and " + delimiters +
      " delimiters"
      }*/
  def printTree(prefix: String): String =
    {
    val buf = new StringBuilder(prefix)
    buf.append(if (isMergeable) " MERGABLE " else " FIXED ")
    this match
    {
      case x: DelimitingBox => buf.append("DELIMITER\n")
      case x if x.isAtomic => buf.append("ATOM: " + this.spanText + "\n")
      case x =>
        {
        buf.append("GROUP " + id + " : " + children.length + " children, " + allNodesDepthFirst.length + " nodes, " + spanningAtoms.length + " atoms, " + allLeaves.length + " leaves, " + delimiters +
                   " delimiters\n")

        for (c <- children)
          {buf.append(c.printTree(prefix + "   |"))}
        }
    }

    buf.toString()
    }

  def filterDeep(filt: (DocNode) => Boolean): Option[DocNode] =
    {
    if (filt(this))
      {
      val newChildren = children.map(_.filterDeep(filt)).flatten
      Some(create(newChildren))
      }
    else None
    }
  }








