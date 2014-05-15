package edu.umass.cs.iesl.pdf2meta.cli.segmentsmoother

import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter.ClassifiedRectangle


// todo figure out where the comments in this class were supposed to go, since they apparently got scrambled
class PdfSegmentLabelAligner(val model: CoarseLabelModel) extends ExtendedMatchDPAligner[ClassifiedRectangle, String]
  {
  def createCell(x: Option[ClassifiedRectangle], y: Option[String], prefix: Option[DPCell[ClassifiedRectangle, String]], score: Double): LengthDPCell[ClassifiedRectangle, String] =
    {
    new LengthDPCell[ClassifiedRectangle, String](x, y, prefix, score, x.map(_.node.text.length).getOrElse(0))
    }

  val labelExtendThreshold = 0.1
  override val beforeStartTextSkipPenalty: Double = -0.05
  override val afterEndTextSkipPenalty: Double = -0.05
  override val lightTextSkipPenalty: Double = -0.1
  override val heavyTextSkipPenalty: Double = -5.0

   val mismatchPenalty: Double = -5.0


  // dropping an expected label is very bad.  Note this constant is not normalized re the expected total score
  override val beforeStartLabelSkipPenalty: Double = -100.0
  override val labelSkipPenalty: Double = -100.0
  override val afterEndLabelSkipPenalty: Double = -100.0


  val matchMultiplier = 10.0

  val alignableLabels: Seq[String] = model.coarseLabels

  def scoreDelete(i: Seq[ClassifiedRectangle], j: Seq[String], prefix: Option[DPCell[ClassifiedRectangle, String]]): (Option[ClassifiedRectangle], Option[String], Double) =
    {
    // beforeStartLabelSkipPenalty is dealt with in the DP initialization in the superclass
    // TODO allow afterEndLabelSkipPenalty when all the text has already been consumed
    // i.e., i.length == allText.length
    // for now, just use the same labelSkipPenalty either way
    (None, Some(j.head), labelSkipPenalty)
    }
  def scoreInsert(i: Seq[ClassifiedRectangle], j: Seq[String], prefix: Option[DPCell[ClassifiedRectangle, String]]): (Option[ClassifiedRectangle], Option[String], Double) =
    {
    val classifiedRectangle: ClassifiedRectangle = i.head

    // see if we'd like to extend the previous label
    // note findLastMatch ignores skips, so we can still get e.g. body-footnote-body
    val label: String = prefix.flatMap(_.findLastMatch).flatMap(_.y).getOrElse("Bogus")
    val localCompatibility = classifiedRectangle.labelWeights(label)
    val localLabel = classifiedRectangle.label

    /*
    Four cases:
    1) the previous match is a good match here, so extend
    2) the previous match is a poor match here, but we're past the end: light penalty
    3) the previous match is a poor match here, and the local prediction is of an alignable kind: heavy penalty.
        Can we just test if the label has already been aligned?
        No, because the local label may be something from a later part of the label sequence!
    4) the previous match is a poor match here, but the local prediction is the unalignable kind: trust the local prediction with a light penalty
     */
    // note emitting "None" as the aligned symbol produces a skip, which will later be overridden with the local label

      lazy val alignedLabels: List[String] = List() // TODO prefix.map(_.traceback).map(_.map(c => c.y)).flatten.toList.flatten

    val (previousLabelBlocks: Int, previousLabelTextLength: Int) = prefix.head match
    {
      case x: LengthDPCell[ClassifiedRectangle, String] => x.matched
      case _ => (0,0)
    }
    val totalLabelTextLength = classifiedRectangle.node.text.length + previousLabelTextLength
    val totalLabelTextBlocks = 1 + previousLabelBlocks

    val allowableLabelTextLength = model.get(label).map(_.maxChars).getOrElse(Int.MaxValue)
    val allowableLabelTextBlocks = model.get(label).map(_.maxBlocks).getOrElse(Int.MaxValue)

    if (localCompatibility > labelExtendThreshold && (totalLabelTextLength <= allowableLabelTextLength) && (totalLabelTextBlocks <= allowableLabelTextBlocks))
      {
      val score = localLabel match
         {
           case Some(x) if x == label => localCompatibility * matchMultiplier
           case _ => mismatchPenalty
         }

      (Some(classifiedRectangle), Some(label), score)
      }
    else if (alignedLabels.contains("end"))
           {
           (None, Some("discard"), afterEndTextSkipPenalty)
           }
    else if (alignableLabels.contains(localLabel.getOrElse("Bogus")))
           {
           (Some(classifiedRectangle), None, heavyTextSkipPenalty)
           }
    else
      {
      (Some(classifiedRectangle), None, lightTextSkipPenalty)
      }
    }
  def scoreMatch(i: Seq[ClassifiedRectangle], j: Seq[String], prefix: Option[DPCell[ClassifiedRectangle, String]]): (Option[ClassifiedRectangle], Option[String], Double) =
    {
    val classifiedRectangle: ClassifiedRectangle = i.head
    val label: String = j.head
    val localLabel = classifiedRectangle.label
    val localCompatibility = classifiedRectangle.labelWeights(label)
    val score = localLabel match
    {
      case Some(x) if x == label => localCompatibility * matchMultiplier
      case _ => mismatchPenalty //localCompatibility
    }

    (Some(classifiedRectangle), Some(label), score)
    }
  }
