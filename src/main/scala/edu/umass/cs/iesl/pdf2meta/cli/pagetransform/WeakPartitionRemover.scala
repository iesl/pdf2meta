package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import com.weiglewilczek.slf4s.Logging
import collection.Seq
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{InternalDocNode, WhitespacePartitionedDocNode, DocNode}

/**
 * Remove "weak" partitions, i.e. those that are likely not real.  This covers mostly double-spacing.
 * The strategy is to find the median partition height (the whitespace between lines), and remove all
 * partitions that are smaller or not much larger than that.
 *
 * However: it should remove only empty partitions, not those containing lines.
 * That is achieved by setting the partition strength to 1000 in SlicingDocPartitioner.
 */
class WeakPartitionRemover extends DocTransformer with Logging
	{
	override def apply(v1: DocNode) = v1 match
	{
		case v: InternalDocNode =>
			{
			new SpecificWeakPartitionRemover(v).apply(v1)
			}
		case v                  => v
	}

	class SpecificWeakPartitionRemover(root: InternalDocNode) extends PreOrderDocTransformer with Logging
		{

		lazy val strengthThreshold: Option[Float] =
			{
			val strengths: Seq[Float] = root.allPartitions.map(_.strength)
			val strengthsS = strengths.sorted

			// if there are too many partitionings (i.e. due to doublespacing), find the height threshold that eliminates most of them

			if (strengthsS.length > 50)
				{
				// assume that there is a long plateau of short delimiters; the median surely is part of that
				val median = strengthsS((strengthsS.length / 2).round)

				// capture only the illegitimate heights
				val t: Float = strengths.filter(_ <= (median * 1.10)).max

				logger.debug("Set strength threshold " + t)

				Some(t)
				}
			else None
			}

		override def apply(v1: DocNode) =
			{
			strengthThreshold.map(t => super.apply(v1)).getOrElse(v1)
			}

		def applyLocalOnly(node: DocNode) =
			{
			node match
			{
				case x: WhitespacePartitionedDocNode if x.maxContainedStrength < strengthThreshold.get => Some(x.makeNonPartition)
				case x                                                                                 => Some(x)
			}
			}
		}

	}
