package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.scalacommons.ListUtils
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{InternalDocNode, AnnotationNode, DocNode}

/**
 * Group consecutive children that have appropriate similarity
 */
abstract class SimilarityDocMerger extends PreOrderDocTransformer with Logging with SimilarityMerger
	{

	/*	def applyLocalOnly(node: DocNode) =
		 {
		 val intermediateChildrenR: List[DocNode] = node.children.foldLeft(List[DocNode]())(merge)
		 val intermediateChildren: List[DocNode] = intermediateChildrenR.reverse


		 val intermed
		 Some(node.create(intermediateChildren))
		 }
 */
	def applyLocalOnly(node: DocNode) =
		{
		val runs = ListUtils.groupContiguousSimilar[DocNode](similar)(node.children.toList)

		// if a run contains more than one node, make an internal node to represent it
		val intermediateChildren: List[DocNode] = runs.map(children => children match
		{
			case c :: t if t == Nil => c
			case c        => InternalDocNode(children.map(_.id).mkString("+"), children, None, None).bounceRetainingChildren("")
		})

		//val grouped = intermediateChildren.map(_.bounceRetainingChildren(""))

		Some(node.create(intermediateChildren))
		}
	}

//** refactor as generic to scalacommons, but careful with :++
trait SimilarityMerger
	{
	// we're traversing the input left to right, but it gets output right to left
	/*def merge(precontext: List[DocNode], r: DocNode): List[DocNode] =
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
		}*/
	def similar(an: DocNode, bn: DocNode): Boolean
	}

/*
@deprecated // see IndentedParagraphsMerger
class ParagraphMerger extends SimilarityDocMerger with Logging
  {
  // note relationship with RectangularReadingOrder
  def similar(an: DocNode, bn: DocNode): Boolean =
    {
    an match
    {
      case ana: AnnotatedDocNode =>
        {
        logger.debug("sideways")
        }
      case _ =>
    }

    (an.rectangle, bn.rectangle) match
    {
      case (Some(a), Some(b)) =>
        {
        val sameFont = (an.dominantFont, bn.dominantFont) match
        {
          case (Some(af), Some(bf)) => af.equalsWithinOneQuantum(bf)
          case _ => false
        }


        val sameColumn = (a.page == b.page) && b.isBelow(a.top) && ((a.left - b.left).abs < 5) // && ((a.right - b.right).abs < 5)
        // includes hanging indent
        val sameColumnWithIndent = b.isBelow(a.top) && ((a.left - b.left).abs < 20) && ((a.right - b.right).abs < 5)

        sameFont && (sameColumn || sameColumnWithIndent
        )
        }
      case _ => false
    }
    }
  }
*/
/**
 * Just merge any consecutive runs of characters that seem to be on the same line.
 * The result will have no spaces, so we'll have to estimate those later.
 */
/**
 * Group consecutive children that have appropriate similarity
 */
class LineMerger extends SimilarityDocMerger
	{
	// note relationship with RectangularReadingOrder
	def similar(an: DocNode, bn: DocNode): Boolean =
		{
		(an.rectangle, bn.rectangle) match
		{
			case (Some(a), Some(b)) =>
				{
				// ignore font, due to italics, superscripts, and such
				/*
						val sameFont = (an.dominantFont, bn.dominantFont) match
						{
						  case (Some(af), Some(bf)) => af.equalsWithinOneQuantum(bf)
						  case _ => false
						}*/
				val aOneLine = an.dominantFont.map(a.height <= _.height * 1.5).getOrElse(false)
				val bOneLine = bn.dominantFont.map(b.height <= _.height * 1.5).getOrElse(false)

				// allow overlap
				val sameLine = (a.page == b.page) && b.isRightOf(a.left) && ((a.top - b.top).abs < 3) && ((a.bottom - b.bottom).abs < 3)

				//sameFont &&
				aOneLine && bOneLine && sameLine
				}
			case _                  => false
		}
		}
	}

class SidewaysLineMerger extends SimilarityDocMerger
	{
	def similar(an: DocNode, bn: DocNode): Boolean =
		{

		def checkOneCharColumn: Boolean =
			{
			(an.rectangle, bn.rectangle) match
			{
				case (Some(a), Some(b)) =>
					{
					/*val sameFont = (an.dominantFont, bn.dominantFont) match
				 {
				   case (Some(af), Some(bf)) => af.equalsWithinOneQuantum(bf)
				   case _ => false
				 }
	 */
					val sameColumn = (a.page == b.page) && b.isBelow(a.top) && ((a.left - b.left).abs < 5)

					// sameFont &&
					sameColumn
					}
				case _                  => false
			}
			}

		(an.text.length, bn.text.length) match
		{
			case (1, 1) => checkOneCharColumn
			case (x, 1) =>
				{
				an match
				{
					case ana: AnnotationNode =>
						{if (ana.annotations.contains("sideways")) checkOneCharColumn else false}
					case _                   => false
				}
				}
			case _      => false
		}
		}

	/*
   override def merge(precontext: List[DocNode], r: DocNode): List[DocNode] =
	   {
	   precontext match
	   {
		   case Nil    => List(r)
		   case h :: t =>
			   {
			   if (similar(h, r))
				   {
				   val merged = new AnnotationNode(InternalDocNode(h.id + "+" + r.id, h.children :+ r, h.localInfo, h.localErrors), Seq("sideways"))
				   merged :: t
				   }
			   else
				   {r :: precontext}
			   }
	   }
	   }*/
	override def applyLocalOnly(node: DocNode) =
		{
		val runs = ListUtils.groupContiguousSimilar[DocNode](similar)(node.children.toList)

		// if a run contains more than one node, make an internal node to represent it
		val intermediateChildren: List[DocNode] = runs.map(children => children match
		{
			case c :: Nil => c
			case c        => new AnnotationNode(InternalDocNode(children.map(_.id).mkString("+"), children, None, None).bounceRetainingChildren(""), Seq("sideways"))
		})

		//val grouped = intermediateChildren.map(_.bounceRetainingChildren(""))

		Some(node.create(intermediateChildren))
		}
	}

// SidewaysParagraphMerger: ignore
