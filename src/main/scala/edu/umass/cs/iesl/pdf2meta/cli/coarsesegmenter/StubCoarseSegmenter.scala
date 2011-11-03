package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode
import edu.umass.cs.iesl.pdf2meta.cli.util.WeightedSet

class StubCoarseSegmenter extends CoarseSegmenter
  {
  def apply(v1: DocNode) =
    {
    val noFeatures = WeightedSet[Feature](Seq.empty);
    val noLabels = WeightedSet[String](Seq.empty);
    val crs = v1.allAtoms.map(x => ClassifiedRectangle(x, noFeatures, noLabels, None))
    new ClassifiedRectangles(crs)
    }
  }
