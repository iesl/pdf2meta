package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import edu.umass.cs.iesl.scalacommons.collections.WeightedSet
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{InternalDocNode, DocNode}

class StubCoarseSegmenter extends CoarseSegmenter
  {
  def apply(v1: DocNode) =
    {
    val noFeatures = WeightedSet[Feature](Seq.empty);
    val noLabels = WeightedSet[String](Seq.empty);
    val crs = v1.leaves.map(x => ClassifiedRectangle(x, noFeatures, noLabels, None))
    new ClassifiedRectangles(crs)
    }
  }
