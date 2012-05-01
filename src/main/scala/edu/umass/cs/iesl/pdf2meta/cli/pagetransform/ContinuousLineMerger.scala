package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import edu.umass.cs.iesl.scalacommons.ListUtils

class ContinuousLineMerger extends SimilarityDocMerger
	{
	/*
	 override def applyLocalOnly(node: DocNode) =
		 {
		 val intermediateChildrenR: List[DocNode] = node.children.foldLeft(List[DocNode]())(merge)
		 val intermediateChildren: List[DocNode] = intermediateChildrenR.reverse
 /*
		 val finalChildren : List[DocNode] = intermediateChildren flatMap
							 {
							 //case l if l.isClassificationLeaf => l.bounce  // no because SpaceEstimator comes next?
							 case l           => Some(l)
							 }
 */
		 Some(node.create(intermediateChildren.map(_.bounceRetainingChildren)))
		 }
 */
	// we're traversing the input left to right, but it gets output right to left
	/*	override def merge(precontext: List[DocNode], r: DocNode): List[DocNode] =
		 {
		 precontext match
		 {
			 case Nil    => List(r)
			 case h :: t =>
				 {
				 if (similar(h, r))
					 {(h.bounceRetainingChildren() :+ r) :: t} // merge things together into InternalDocNodes
				 else
					 {r :: precontext}
				 }
		 }
		 }
 */
	// note relationship with RectangularReadingOrder
	def similar(an: DocNode, bn: DocNode): Boolean =
		{
		if (an.isInstanceOf[DelimitingBox] || bn.isInstanceOf[DelimitingBox]) false
		else
			{
			(an.rectangle, bn.rectangle) match
			{
				case (Some(a), Some(b)) =>
					{
					// extremely simplistic to start
					// ignore font, due to italics, superscripts, and such
					/*
					   val sameFont = (an.dominantFont, bn.dominantFont) match
					   {
						 case (Some(af), Some(bf)) => af.equalsWithinOneQuantum(bf)
						 case _ => false
					   }*/
					//val aOneLine = an.dominantFont.map(a.height <= _.height * 1.5).getOrElse(false)
					//val bOneLine = bn.dominantFont.map(b.height <= _.height * 1.5).getOrElse(false)
					// allow overlap
					// ** superscripts, subscripts, etc.
					val sameLine = (a.page == b.page) && b.isRightOf(a.left) && ((a.bottom - b.bottom).abs < 1)

					//sameFont &&
					//aOneLine && bOneLine &&
					sameLine
					}
				case _                  => false
			}
			}
		}
	}

/**
 * Join individual words and phrases based on matching baseline; doesn't handle superscripts etc.
 * This is not a SimilarityDocMerger because it is intended to operate only on nodes containing leaves (e.g., pages containing glyphs),
 * not the leaves themselves or other internal nodes.
 */
/*class SimpleSortingLineMerger extends SimpleLineMerger
	{

	def sortAndGroupLine(chars: Seq[DocNode]): Seq[DocNode] =
		{
		val sortedByX = chars.sortBy(c => c.rectangle.get.left)

		val intermediateChildrenR: List[DocNode] = sortedByX.foldLeft(List[DocNode]())(merge)
		val intermediateChildren: List[DocNode] = intermediateChildrenR.reverse
		//Some(chars(0).create(intermediateChildren))
		//intermediateChildren.flatMap(_.bounce) // drop empty groups
		// don't bounce; SpaceEstimator needs the details

		intermediateChildren.map(_.bounceRetainingChildren(""))
		}

	override def applyLocalOnly(node: DocNode) =
		{

		val groupedByY = node.children.groupBy(n => n.rectangle.get.bottom).toSeq
		val sortedByY = groupedByY.sortBy(-_._1)
		val linesInOrder = sortedByY.map(_._2)

		val result = linesInOrder.flatMap(sortAndGroupLine)

		Some(result)
		}
	}
*/
class SimpleLineMerger
	{

	// we're traversing the input left to right, but it gets output right to left
	/*	def merge(precontext: List[DocNode], r: DocNode): List[DocNode] =
		 {
		 precontext match
		 {
			 case Nil    => List(r)
			 case h :: t =>
				 {
				 if (similar(h, r))
					 {(h :+ r) :: t}
				 else
					 {r :: precontext}
				 }
		 }
		 }
 */
	// note relationship with RectangularReadingOrder
	def similar(an: DocNode, bn: DocNode): Boolean =
		{
		if (an.isInstanceOf[DelimitingBox] || bn.isInstanceOf[DelimitingBox]) false
		else
			{
			(an.rectangle, bn.rectangle) match
			{
				case (Some(a), Some(b)) =>
					{
					// extremely simplistic to start
					// ignore font, due to italics, superscripts, and such
					/*
					   val sameFont = (an.dominantFont, bn.dominantFont) match
					   {
						 case (Some(af), Some(bf)) => af.equalsWithinOneQuantum(bf)
						 case _ => false
					   }*/
					//val aOneLine = an.dominantFont.map(a.height <= _.height * 1.5).getOrElse(false)
					//val bOneLine = bn.dominantFont.map(b.height <= _.height * 1.5).getOrElse(false)
					// allow overlap
					// ** superscripts, subscripts, etc.
					val sameLine = (a.page == b.page) && b.isRightOf(a.left) && ((a.bottom - b.bottom).abs < 1)
					val abutting = b.left - a.right < 2 //(a.right - b.left).abs < 2
					//sameFont &&
					//aOneLine && bOneLine &&
					sameLine && abutting
					}
				case _                  => false
			}
			}
		}

	def groupLines(chars: Seq[DocNode]): Seq[DocNode] =
		{
		val runs = ListUtils.groupContiguousSimilar[DocNode](similar)(chars.toList)
		val intermediateChildren: List[DocNode] = runs.map(children => InternalDocNode(children.map(_.id).mkString("+"), children, None, None))

		/*
		// assume chars are in order
		val intermediateChildrenR: List[DocNode] = chars.foldLeft(List[DocNode]())(merge)
		val intermediateChildren: List[DocNode] = intermediateChildrenR.reverse
		//Some(chars(0).create(intermediateChildren))
		//intermediateChildren.flatMap(_.bounce) // drop empty groups
		// don't bounce; SpaceEstimator needs the details
*/

		intermediateChildren.map(_.bounceRetainingChildren(""))
		}

	def applyLocalOnly(node: DocNode) =
		{
		val runs = ListUtils.groupContiguousSimilar[DocNode](similar)(node.children.toList)
		val intermediateChildren: List[DocNode] = runs.map(children => InternalDocNode(children.map(_.id).mkString("+"), children, None, None))

		val grouped = intermediateChildren.map(_.bounceRetainingChildren(""))


		Some(grouped)
		}
	}
