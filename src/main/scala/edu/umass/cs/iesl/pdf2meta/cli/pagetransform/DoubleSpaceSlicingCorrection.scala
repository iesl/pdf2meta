package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import com.weiglewilczek.slf4s.Logging
import collection.Seq
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{DelimitingBox, DocNode}

class DoubleSpaceSlicingCorrection extends DocTransformer with Logging
  {
  def apply(root: DocNode) =
    {

    val heights: Seq[Double] = root.delimitingBoxes.map(_.theRectangle.height)
    //val heightHistogram = Util.histogram(heights)
    val heightsS = heights.sorted

    // if there are too many delimitingBoxes (i.e. due to doublezpacing), find the height threshold that eliminates most of them

    if (heightsS.length > 50)
      {
      // assume that there is a long plateau of short delimiters; the median surely is part of that
      val median = heightsS((heightsS.length / 2).round)

      // capture only the illegitimate heights
      val doubleSpaceThreshold = heights.filter(_ <= (median * 1.10)).max

      // remove any partitions that fail the threshold, thereby allowing adjacent runs to be merged by the paragraph merger
      // the root must pass the filter
      root.filterDeep({
                      case x: DelimitingBox => x.rectangle.get.height > doubleSpaceThreshold
                      case _ => true
                      }).get
      }
    else root
    }
  }
