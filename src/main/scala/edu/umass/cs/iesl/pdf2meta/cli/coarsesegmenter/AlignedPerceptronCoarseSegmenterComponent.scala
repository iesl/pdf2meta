package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode
import collection.mutable.LinkedHashMap
import edu.umass.cs.iesl.pdf2meta.cli.segmentsmoother.SegmentSmoother


trait AlignedPerceptronCoarseSegmenterComponent
        extends CoarseSegmenter
  {

  val perceptronPhase: CoarseSegmenter
  val segmentSmoother: SegmentSmoother //= new BestCoarseLabelModelAligner
  // for now we don't "rescue" labels if the alignment overruled them
  //val labelRescuer = new LabelRescuer(List("affiliations","contactinfo","metadata"))
  //val labelPattern = List("title", "authors", "abstract", "body", "references", "end")
  //val ignoredLabels = List("")
  def apply(atoms: DocNode) =
    {
    val localClassified: ClassifiedRectangles = perceptronPhase.apply(atoms)

    val alignmentClassified: ClassifiedRectangles = segmentSmoother.apply(localClassified)

    alignmentClassified

    //    val reclassified = localClassified.raw.map(r => alignmentMap.get(r).map(r.callLabel(_)).getOrElse(r))
    // val reclassified = alignment.map(t=> t._1.map(_.callLabel(t._2)))
    //val reclassified = for (t <- alignment) yield t._1.map(_.callLabel(t._2))
    //val rescued = labelRescuer.apply(reclassified.flatten)
    //    new ClassifiedRectangles(reclassified) //rescued
    }
  }


/*
class WiredAligner(val coarseLabels : Seq[String]) extends SegmentSmoother
  {
  val sequenceAligner = new PdfSegmentLabelAligner(coarseLabels)
def apply(v1: ClassifiedRectangles) = {
sequenceAligner.apply(v1.raw,coarseLabels)
}
}
*/
