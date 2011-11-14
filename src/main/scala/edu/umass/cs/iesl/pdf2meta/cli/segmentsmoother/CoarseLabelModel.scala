package edu.umass.cs.iesl.pdf2meta.cli.segmentsmoother

import collection.mutable.LinkedHashMap
import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter.{LabelFixer, LabelTransformer}

trait CoarseLabelModel
  {
  val coarseLabelMap: LinkedHashMap[String, CoarseAlignmentConstraint]
  val definiteLabels: Seq[String]

  case class CoarseAlignmentConstraint(coarseLabel: String, minChars: Int, maxChars: Int, minBlocks: Int, maxBlocks: Int, fineLabels: Seq[String])

  lazy val coarseLabelsReverse: Map[String, String] = (for (t <- coarseLabelMap.values; l <- t.fineLabels) yield (l, t.coarseLabel)).toMap
  lazy val coarseLabels = coarseLabelMap.keys.toSeq

  lazy val labelCollapser = new LabelTransformer(coarseLabelsReverse)

  lazy val definiteClassifier = new LabelFixer(definiteLabels)

  def get(x: String): Option[CoarseAlignmentConstraint] = coarseLabelMap.get(x)
  }




