package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

import scala.collection.Seq
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.scalacommons.OrderedTreeNode
import scala.Predef._
import scala._

trait RectangularOnPage
	{
	def rectangle: Option[RectangleOnPage]

	// allow backing off to a "core" of the content for ordering when it's ambiguous using the full rectangle
	def coreRectangle: Option[RectangleOnPage] = rectangle
	}

abstract class DocNode(val id: String, val localInfo: Option[Iterator[String]], val localErrors: Option[Iterator[String]]) extends
OrderedTreeNode[DocNode] with RectangularOnPage with TextBox with Logging
	{
	def getPages: Seq[PageNode] =
		{
		// perf can prune when a page is found
		val all = allNodesDepthFirst
		val result = all.collect({
		                         case x: PageNode => x
		                         //                     case x: RectBox => throw new Error("RectBox is a DelimitingBox")
		                         })
		result
		}

	lazy val errors: Option[Iterator[String]] = localErrors

	lazy val info: Option[Iterator[String]] = localInfo

	def allNodesBreadthFirst: List[DocNode] = List(this)

	def allNodesDepthFirst: List[DocNode] = List(this)

	def printTree(prefix: String): String

	def leaves: Seq[LeafNode]

	lazy val charSpanProportional: Map[LeafNode, (Float, Float)] =
		{
		def appendEnd(l: List[(LeafNode, Int)], n: LeafNode): List[(LeafNode, Int)] =
			{
			// add one to the length to account for the space that gets added between adjacent nodes, at TextContainer.text
			(n, l.head._2 + n.text.length + 1) :: l
			}

		val charEnds: List[(LeafNode, Int)] = leaves.foldLeft(List[(LeafNode, Int)]((LeafNode.begin, 0)))(appendEnd).reverse

		def selfzip(l: List[(LeafNode, (Int, Int))], e: (LeafNode, Int)): List[(LeafNode, (Int, Int))] =
			{
			(e._1, (l.head._2._2, e._2)) :: l
			}
		val charBeginEnds: List[(LeafNode, (Int, Int))] = charEnds.foldLeft(List[(LeafNode, (Int, Int))]((LeafNode.begin, (0, 0))))(selfzip).reverse
		val total = text.length.toFloat
		val proportional = charBeginEnds.map((x: (LeafNode, (Int, Int))) => (x._1, (x._2._1 / total, x._2._2 / total)))
		proportional.toMap
		}

	//def allSecretLeaves: Seq[DocNode] = if (isSecretLeaf) List(this) else secretChildren.flatMap(_.allSecretLeaves)
	def delimitingBoxes: Seq[DelimitingBox] =
		{
		val all = allNodesDepthFirst
		val result = all.collect({
		                         case x: DelimitingBox => x
		                         //                     case x: RectBox => throw new Error("RectBox is a DelimitingBox")
		                         })
		result
		}

	def whitespaceBoxes: Seq[WhitespaceBox] =
		{
		val all = allNodesDepthFirst
		val result = all.collect({
		                         case x: WhitespaceBox => x
		                         //                     case x: RectBox => throw new Error("RectBox is a DelimitingBox")
		                         })
		result
		}

	def allPartitions: Seq[PartitionedDocNode] =
		{
		val all = allNodesDepthFirst
		val result = all.collect({
		                         case x: PartitionedDocNode => x
		                         //                     case x: RectBox => throw new Error("RectBox is a DelimitingBox")
		                         })
		result
		}

	def textLines: Seq[TextLine] =
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

	/**
	 * create a leaf node, but keep the detailed contents; also drop empty nodes
	 * @return
	 */
	def bounceRetainingChildren(separator: String = " "): DocNode =
		{
		if (delimitingBoxes.isEmpty)
			{
			new DerivedNode(this)
				{override val text = derivedFrom.mkString(separator)}
			}
		else this // can't bounce a node with delimiters inside
		}

	/*
	   // aggregate nodes into a leaf group
	   def :++(r: DocNode): DocNode =
		   {
		   //require(!(this.isInstanceOf[AnnotationNode] || r.isInstanceOf[AnnotationNode]))
		   // current children and secretChildren are all combined into secretChildren, because we're making Leaf nodes
		   if (children.isEmpty)
			   {
			   InternalDocNode(id + "+" + r.id, List(this, r), None, None, true)
			   }
		   else
			   {
			   InternalDocNode(id + "+" + r.id, children :+ r, localInfo, localErrors, true)
			   }
		   }
		   */
	}

object LeafNode
	{
	val begin = new LeafNode("begin", None, None, None)
	}

class LeafNode(override val id: String, override val localInfo: Option[Iterator[String]], override val localErrors: Option[Iterator[String]],
               override val rectangle: Option[RectangleOnPage])
		extends DocNode(id, localInfo, localErrors)
	{
	def children = Nil

	/**maybe a preorder traversal expands a node
	 *
	 * @param childrenA
	 * @return
	 */
	def create(childrenA: Seq[DocNode]) =
		{
		if (childrenA.length == 1) childrenA(0)
		else if (childrenA == children) this
		else
			InternalDocNode(id, childrenA, localInfo, localErrors)
		}

	def printTree(prefix: String): String =
		{
		if (spanText.length > 0)
			{
			val buf = new StringBuilder(prefix)
			buf.append("LEAF: " + spanText + "\n")

			buf.toString()
			}
		else ""
		}

	def leaves: Seq[LeafNode] = List(this)
	}

class DerivedNode(val derivedFrom: DocNode) extends LeafNode(derivedFrom.id, derivedFrom.localInfo, derivedFrom.localErrors, derivedFrom.rectangle)
	{
	//override val text          = derivedFrom.mkString("")
	override val coreRectangle = derivedFrom.coreRectangle

	override lazy val allFonts              = derivedFrom.allFonts
	override lazy val allTextLineWidths     = derivedFrom.allTextLineWidths
	override lazy val dominantFont          = derivedFrom.dominantFont
	override lazy val dominantFontHeight    = derivedFrom.dominantFontHeight
	override lazy val dominantTextLineWidth = derivedFrom.dominantTextLineWidth

	override def mkString(d: String) = text

	override def partitionByFont(boxorder: Ordering[RectangularOnPage]) = Seq(this)

	// aggregate nodes into a group
	//override def :+(r: DocNode): DocNode = (derivedFrom :+ r).bounceRetainingChildren()
	override def bounceRetainingChildren(separator: String) = this // ignore request for alternate separator
	}

class AnnotationNode(val derivedFrom: DocNode, val annotations: Seq[String])
		extends LeafNode(derivedFrom.id, derivedFrom.localInfo, derivedFrom.localErrors, derivedFrom.rectangle)





