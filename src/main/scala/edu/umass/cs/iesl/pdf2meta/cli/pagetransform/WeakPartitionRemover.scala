package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import com.weiglewilczek.slf4s.Logging
import collection.Seq
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{PartitionedDocNode, DocNode}


class WeakPartitionRemover extends DocTransformer with Logging
  {


  override def apply(v1: DocNode) = new SpecificWeakPartitionRemover(v1).apply(v1)

  //def applyLocalOnly(node: DocNode) = throw new Error("applyLocalOnly should never be called because apply is overridden")

  class SpecificWeakPartitionRemover(root: DocNode) extends PreOrderDocTransformer with Logging
    {

    lazy val strengthThreshold: Option[Double] =
      {
      val strengths: Seq[Double] = root.allPartitions.map(_.strength)
      //val heightHistogram = Util.histogram(heights)
      val strengthsS = strengths.sorted

      // if there are too many partitionings (i.e. due to doublespacing), find the height threshold that eliminates most of them

      if (strengthsS.length > 50)
        {
        // assume that there is a long plateau of short delimiters; the median surely is part of that
        val median = strengthsS((strengthsS.length / 2).round)

        // capture only the illegitimate heights
        val t: Double = strengths.filter(_ <= (median * 1.10)).max

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
        case x: PartitionedDocNode if x.strength < strengthThreshold.get => Some(x.makeNonPartition)
        case x => Some(x)
      }
      }


    /* def apply(root: DocNode) =
            {

            val strengths: Seq[Double] = root.allPartitions.map(_.strength)
            //val heightHistogram = Util.histogram(heights)
            val strengthsS = strengths.sorted

            // if there are too many partitionings (i.e. due to doublespacing), find the height threshold that eliminates most of them

            if (strengthsS.length > 50)
              {
              // assume that there is a long plateau of short delimiters; the median surely is part of that
              val median = strengthsS((strengthsS.length / 2).round)

              // capture only the illegitimate heights
              val doubleSpaceThreshold = strengths.filter(_ <= (median * 1.10)).max

              // remove any partitions that fail the threshold, thereby allowing adjacent runs to be merged by the paragraph merger
              // the root must pass the filter
              root.filterDeep({
                              case x: PartitionedDocNode => x.applyThreshold(doubleSpaceThreshold)
                              case _ => true
                              }).get
              }
            else root
            }*/
    }

  }
