package edu.umass.cs.iesl.pdf2meta.cli.extract.metatagger

import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import edu.umass.cs.iesl.scalacommons.{StatsUtils, ListUtils}
import edu.umass.cs.iesl.pdf2meta.cli.pagetransform.PreOrderDocTransformer

/**
 * Convert a sequence of characters constituting a line into a single node, inserting spaces as appropriate
 *
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
class MetataggerExtractor extends PreOrderDocTransformer with Logging
	{
	/*
	 private class WordMerger(threshold: Float)
		 {
		 // note relationship with RectangularReadingOrder
		 def similar(an: DocNode, bn: DocNode): Boolean =
			 {
			 (an.rectangle, bn.rectangle) match
			 {
				 case (Some(a), Some(b)) =>  (a.page == b.page) && (a.left - b.right < threshold)
				 case _                  => false
			 }
			 }
		 }
 */
	private def partitionToWords(chars: Seq[DocNode]): DocNode =
		{
		//val chars: Seq[DocNode] = charNode.secretChildren
		//if (chars.map(_.theText).contains(" "))
		val runs = ListUtils.contiguousRuns(chars.toList)(_.isInstanceOf[TextAtom])
		val runsWithFonts = for ((t, r) <- runs) yield
			{
			if (t)
				{
				val castChars = r.map(_.asInstanceOf[TextAtom])

				// there may be space characters present; that's OK
				val glyphSizes = castChars.map(_.charWidths).flatten

				// distribution of distances between one char and the next
				// actually this is not informative, since a line with no spaces or a line with spaces between each character are legit
				//val deltas = for(l <- chars.sliding(2)) yield { (l.head.rectangle.get.right - l.tail.head.rectangle.get.left)}
				val minimumSpaceSize = (glyphSizes.sum / glyphSizes.length) * 0.3f

				val deltaWithinThreshold: (DocNode, DocNode) => Boolean =
					{
					(a: DocNode, b: DocNode) =>
						{
						b.rectangle.get.left - a.rectangle.get.right < minimumSpaceSize
						}
					}

				val words = ListUtils.groupContiguousSimilar(deltaWithinThreshold)(chars.toList)

				// join word characters without spaces, and words with spaces
				val line = (for (word <- words) yield word.mkString("")).mkString(" ")
				val leafFonts = castChars.map(c => (c.dominantFont.get, c.text.length))
				(line, leafFonts)
				}
			else
				{
				// assume things that are not PdfBoxTextLine should be separated by spaces
				(r.mkString(" "), Nil)
				}
			//charNode.create(words.map((l: List[DocNode]) => charNode.create(l)))
			}
		val rect = Rectangle.encompassing(chars.flatMap(_.rectangle), 0)
		val page = chars.head.rectangle.map(_.page).get


		val allRuns = runsWithFonts.map(_._1)
		val leafFonts = runsWithFonts.flatMap(_._2)

		// drop all children, secret or otherwise, and just create a leaf node  (see also DocNode.bounce)
		new TextLine(chars.head.id, allRuns.mkString(" "), rect.flatMap(RectangleOnPage(page, _)))
			{
			override lazy val allFonts: Seq[(FontWithHeight, Int)] =
				{
				StatsUtils.histogramAccumulate(leafFonts).toSeq
				}

			override def create(childrenA: Seq[DocNode]) =
				{
				assert(childrenA.isEmpty)
				this
				}
			}
		}

	override def applyLocalOnly(node: DocNode) =
		{
		val result =
			node match
			{
				case n: DerivedNode => partitionToWords(n.derivedFrom.leaves)
				case n              => n
			}

		// take this opportunity to remove empty nodes
		// if (result.text.isEmpty) None else Some(result)

		Some(result)
		}
	}
