package edu.umass.cs.iesl.pdf2meta.cli.segmentsmoother

import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter.{ClassifiedRectangle, ClassifiedRectangles}

trait BestCoarseLabelModelAligner extends SegmentSmoother
  {

  val coarseLabelModels: List[CoarseLabelModel]
  def tryModel(model: CoarseLabelModel, input: ClassifiedRectangles): (Double, ClassifiedRectangles) =
    {
    // collapse some labels
    val coarseLocalClassified: Seq[ClassifiedRectangle] = model.labelCollapser.apply(input.raw).raw

    // first just call those classifications that don't depend on alignment, and hold those out so as not to confuse the aligner
    val (definiteClassifications: Map[ClassifiedRectangle, String], alignableRemainder: Seq[ClassifiedRectangle]) = model.definiteClassifier.apply(coarseLocalClassified)

    // do the alignment
    val aligner = new PdfSegmentLabelAligner(model)
    val (score: Double, alignment: Seq[(Option[ClassifiedRectangle], Option[String])]) = aligner.apply(alignableRemainder, model.coarseLabels)

    val alignedRects = alignment.map({
                                     case (Some(r), x) => Some(r)
                                     case _ => None
                                     }).flatten
    val firstRect: ClassifiedRectangle = alignedRects.head
    val lastRect: ClassifiedRectangle = alignedRects.last

    val firstRectI = coarseLocalClassified.indexOf(firstRect)
    val lastRectI = coarseLocalClassified.indexOf(lastRect)
    val coarseLocalClassifiedInsideAlignment = coarseLocalClassified.slice(firstRectI, lastRectI + 1)
    val coarseLocalClassifiedBeforeAlignment: Seq[ClassifiedRectangle] = coarseLocalClassified.slice(0, firstRectI)
    val coarseLocalClassifiedAfterAlignment: Seq[ClassifiedRectangle] = coarseLocalClassified.slice(lastRectI + 1, coarseLocalClassified.length)


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


    val reclassified: Seq[ClassifiedRectangle] = coarseLocalClassifiedBeforeAlignment.map(r => r.callLabel(Some("discard"))).toList :::
     coarseLocalClassifiedInsideAlignment.map(r => r.callLabel(Some(definiteClassifications.getOrElse(r, alignmentMap.getOrElse(r, "discard"))))).toList ::: coarseLocalClassifiedAfterAlignment.map(r => r.callLabel(Some("discard"))).toList

    (score, new ClassifiedRectangles(reclassified.toSeq))
    }

  def apply(input: ClassifiedRectangles) =
    {
    val results: Seq[(Double, ClassifiedRectangles)] = coarseLabelModels.map(tryModel(_, input))
    results.sortBy(x => -x._1).head._2 // choose best score
    }
  }

