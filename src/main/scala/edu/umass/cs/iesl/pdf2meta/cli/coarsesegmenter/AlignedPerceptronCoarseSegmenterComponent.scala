package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import edu.umass.cs.iesl.pdf2meta.cli.segmentsmoother.SegmentSmoother
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{InternalDocNode, DocNode}

trait AlignedPerceptronCoarseSegmenterComponent
        extends CoarseSegmenter
  {
  val perceptronPhase: CoarseSegmenter
  val segmentSmoother: SegmentSmoother

  def apply(tree: DocNode) =
    {
    val localClassified: ClassifiedRectangles = perceptronPhase.apply(tree)
    val alignmentClassified: ClassifiedRectangles = segmentSmoother.apply(localClassified)
    alignmentClassified
    }
  }
