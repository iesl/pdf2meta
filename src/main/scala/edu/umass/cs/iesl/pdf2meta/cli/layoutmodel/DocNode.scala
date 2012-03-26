package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

import collection.Seq
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.scalacommons.OrderedTreeNode

trait RectangularOnPage
{
  def rectangle: Option[RectangleOnPage]
}

object DocNode
  {
  def apply(id: String, children: Seq[DocNode], localInfo: Option[Iterator[String]], localErrors: Option[Iterator[String]]): DocNode = //, isMergeable: Boolean
    {
    new DocNode(id, children, localInfo, localErrors)
    }

  val begin = new DocNode("begin", Nil, None, None)
  }


class DocNode(val id: String, val children: Seq[DocNode], val localInfo: Option[Iterator[String]], val localErrors: Option[Iterator[String]]) //, val isMergeable: Boolean)
        extends OrderedTreeNode[DocNode] with RectangularOnPage with TextBox with Logging
  {
  def isLeaf = children.length == 0
  def isSecretLeaf = secretChildren.length == 0

  // allow computing internal things based on a set of children that are not the same as the public ones
  // by default just use the same ones
  def secretChildren = children

  lazy val charSpanProportional: Map[DocNode, (Double, Double)] =
    {
    def appendEnd(l: List[(DocNode, Int)], n: DocNode): List[(DocNode, Int)] =
      {
      // add one to the length to account for the space that gets added between adjacent nodes, at TextContainer.text
      (n, l.head._2 + n.text.length + 1) :: l
      }

    val charEnds: List[(DocNode, Int)] = allLeaves.foldLeft(List[(DocNode, Int)]((DocNode.begin, 0)))(appendEnd).reverse

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
  // todo this may not be the best way to handle this situation, as we frequently do foobar.rectangle.get.height or whatever,
  // which is error-prone, and handling Options in every case is a hassle.
  lazy val rectangle: Option[RectangleOnPage] = computeRectangle

  // allow backing off to a "core" of the content for ordering when it's ambiguous using the full rectangle
  def coreRectangle = rectangle

  def computeRectangle: Option[RectangleOnPage] =
    {
    val childRects: Seq[Option[RectangleOnPage]] = secretChildren.map((x: DocNode) => x.rectangle)
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
    val r: Iterator[String] = secretChildren.map(_.errors).flatten.foldLeft[Iterator[String]](localErrors.getOrElse(Iterator.empty))((a: Iterator[String], b: Iterator[String]) =>
                                                                                                                                       {
                                                                                                                                       a ++
                                                                                                                                       b
                                                                                                                                       })
    if (r.isEmpty) None else Some(r)
    }


  lazy val info: Option[Iterator[String]] =
    {
    val r: Iterator[String] = secretChildren.map(_.info).flatten.foldLeft[Iterator[String]](localInfo.getOrElse(Iterator.empty))((a: Iterator[String], b: Iterator[String]) =>
                                                                                                                                   {
                                                                                                                                   a ++
                                                                                                                                   b
                                                                                                                                   })
    if (r.isEmpty) None else Some(r)
    }

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

  /**
   * recursively collect all nodes in preorder depth-first order
   */
  def allNodesDepthFirst: List[DocNode] =
    {
    val childNodeLists: List[List[DocNode]] = children.map(_.allNodesDepthFirst).toList
    val f: List[DocNode] = childNodeLists.flatten
    this :: f
    }

// { if(this.isInstanceOf[WhitespaceBox]) Nil else List(this) }
  def allLeaves: Seq[DocNode] = if (isLeaf) List(this) else children.flatMap(_.allLeaves)

  def allSecretLeaves: Seq[DocNode] = if (isSecretLeaf) List(this) else secretChildren.flatMap(_.allSecretLeaves)


  def delimitingBoxes: scala.Seq[DelimitingBox] =
    {
    val all = allNodesDepthFirst
    val result = all.collect({
                             case x: DelimitingBox => x
                             //                     case x: RectBox => throw new Error("RectBox is a DelimitingBox")
                             })
    result
    }



  def whitespaceBoxes: scala.Seq[WhitespaceBox] =
  {
    val all = allNodesDepthFirst
    val result = all.collect({
      case x: WhitespaceBox => x
      //                     case x: RectBox => throw new Error("RectBox is a DelimitingBox")
    })
    result
  }


  def allPartitions: scala.Seq[PartitionedDocNode] =
    {
    val all = allNodesDepthFirst
    val result = all.collect({
                             case x: PartitionedDocNode => x
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


  // for debugging
  val delimiters = delimitingBoxes.length

  def create(childrenA: Seq[DocNode]) =
    {
    if (childrenA.length == 1) childrenA(0)
    else if (childrenA == children) this
    else
      DocNode(id, childrenA, localInfo, localErrors)
    }


  // aggregate nodes into a group
  def :+(r: DocNode): DocNode =
    {
    require(!(this.isInstanceOf[AnnotatedDocNode] || r.isInstanceOf[AnnotatedDocNode]))
    if (children.isEmpty)
      {
      DocNode(id + "+" + r.id, List(this, r), None, None)
      }
    else
      {
      DocNode(id + "+" + r.id, children :+ r, localInfo, localErrors)
      }
    }

  // aggregate nodes into a leaf group
  def :++(r: DocNode): DocNode =
    {
    require(!(this.isInstanceOf[AnnotatedDocNode] || r.isInstanceOf[AnnotatedDocNode]))

    if (children.isEmpty)
      {
      LeafDocNode(id + "+" + r.id, List(this, r), None, None)
      }
    else
      {
      LeafDocNode(id + "+" + r.id, children :+ r, localInfo, localErrors)
      }
    }
  def printTree(prefix: String): String =
    {
    val buf = new StringBuilder(prefix)
    this match
    {
      case x: DelimitingBox => buf.append("DELIMITER\n")
      case x: WhitespaceBox => buf.append("WHITESPACE: " + this.rectangle.get.width + " x " + this.rectangle.get.height + "\n")
      case x if x.isLeaf => buf.append("LEAF: " + this.spanText + "\n")
      case x =>
        {
        this match
        {
          case y: WhitespacePartitionedDocNode => buf.append("WHITESPACE PARTITION (" + y.strength + ") ")
          case y: LinePartitionedDocNode => buf.append("LINE PARTITION (" + y.strength + ") ")
          case y => buf.append("GROUP ")
        }
        buf.append(id + " : " + children.length + " children, " + allNodesDepthFirst.length + " nodes, " + allLeaves.length + " leaves, " + delimiters +
                   " delimiters\n")

        for (c <- children)
          {buf.append(c.printTree(prefix + "   |"))}
        }
    }

    buf.toString()
    }

  /**
   * Filter the children of this node, recursively; preorder means any immediate children failing the filter don't get recursed.
   * Note this node itself doesn't get filtered
   */
  def filterDeep(filt: (DocNode) => Boolean): DocNode = preOrderApply(n => Some(create(children.filter(filt)))).get
  }





