package edu.umass.cs.iesl.pdf2meta.cli.segmentsmoother

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode
import collection.mutable.LinkedHashMap
import edu.umass.cs.iesl.pdf2meta.cli.config.{LetterCoarseLabelModel, StandardCoarseLabelModel}
import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter.{ClassifiedRectangle, ClassifiedRectangles}

trait BestCoarseLabelModelAligner extends SegmentSmoother
  {

  val coarseLabelModels: List[CoarseLabelModel] // = List(new StandardCoarseLabelModel, new LetterCoarseLabelModel)


  def tryModel(model: CoarseLabelModel, input: ClassifiedRectangles): (Double, ClassifiedRectangles) =
    {
    // collapse some labels
    val coarseLocalClassified: Seq[ClassifiedRectangle] = model.labelCollapser.apply(input.raw).raw

    // first just call those classifications that don't depend on alignment, and hold those out so as not to confuse the aligner
    val (definiteClassifications: Map[ClassifiedRectangle, String], alignableRemainder: Seq[ClassifiedRectangle]) = model.definiteClassifier.apply(coarseLocalClassified)

    // do the alignment
    val aligner = new PdfSegmentLabelAligner(model)
    val (score: Double, alignment: Seq[(Option[ClassifiedRectangle], Option[String])]) = aligner.apply(alignableRemainder, model.coarseLabels)

    val alignedRects = alignment.map({ case (Some(r),x) => Some(r)
    case _ => None}).flatten
    val firstRect : ClassifiedRectangle = alignedRects.head
    val lastRect : ClassifiedRectangle = alignedRects.last

    val firstRectI = coarseLocalClassified.indexOf(firstRect)
    val lastRectI = coarseLocalClassified.indexOf(lastRect)
    val coarseLocalClassifiedInsideAlignment = coarseLocalClassified.slice(firstRectI, lastRectI+1)


    // flatten the alignment (i.e., list only matches)
    val alignmentMap: Map[ClassifiedRectangle, String] = alignment.map(pair =>
                                                                         {
                                                                         val result: Option[(ClassifiedRectangle, String)] = pair match
                                                                         {
                                                                           case (Some(r), Some(l)) => Some(r -> l)
                                                                           //case (Some(r), None) => Some(r, None)
                                                                           case _ => None
                                                                         }
                                                                         result
                                                                         }).flatten.toMap

    // merge the aligned with the unaligned classifications, maintaining order
    // todo really use coarseLocalClassifiedInsideAlignment
    val reclassified: Seq[ClassifiedRectangle] = coarseLocalClassified.map(r => r.callLabel(Some(definiteClassifications.getOrElse(r, alignmentMap.getOrElse(r, "discardX")))))
    //val reclassified : Seq[ClassifiedRectangle] = input.raw.map(r => definiteClassifications.getOrElse(r, alignmentMap.getOrElse(r, r.callLabel(Some("[ambiguous]")))))
    //val reclassified = alignmentMap.map(t => t._1.callLabel(t._2))
    // reinstate classifications made prior to label collapsing, for labels that were not to be aligned anyway
    //val rescued = model.labelRescuer.apply(reclassified)

    (score, new ClassifiedRectangles(reclassified.toSeq))
    }

  def apply(input: ClassifiedRectangles) =
    {
    val results: Seq[(Double, ClassifiedRectangles)] = coarseLabelModels.map(tryModel(_, input))
    results.sortBy(x => -x._1).head._2 // choose best score
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
