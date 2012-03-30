package edu.umass.cs.iesl.pdf2meta.cli.config

import collection.mutable.LinkedHashMap
import edu.umass.cs.iesl.pdf2meta.cli.segmentsmoother.CoarseLabelModel
import scala.Int

class StandardCoarseLabelModel extends CoarseLabelModel {
  override val coarseLabelMap: LinkedHashMap[String, CoarseAlignmentConstraint] = LinkedHashMap(
    "title" -> CoarseAlignmentConstraint("title", 30, 200, 1, 5, List("title")),
    "authors" -> CoarseAlignmentConstraint("authors", 10, 10000, 1, 10, List("authors")),
    "abstract" -> CoarseAlignmentConstraint("abstract", 100, 5000, 1, 5, List("abstract")),
    "body" -> CoarseAlignmentConstraint("body", 500, Int.MaxValue, 5, Int.MaxValue, List("body", "heading", "footnote", "codeOrData")),
    "acknowledgements" -> CoarseAlignmentConstraint("acknowledgements", 100, 5000, 0, 5, List("acknowledgements")),  // acks may come before or after refs; let the alignment decide.
    "references" -> CoarseAlignmentConstraint("references", 0, Int.MaxValue, 0, Int.MaxValue, List("references")),
    "acknowledgements" -> CoarseAlignmentConstraint("acknowledgements", 100, 5000, 0, 5, List("acknowledgements")),  // doesn't specify "but not both"; for that we need alternate models
    "end" -> CoarseAlignmentConstraint("end", 0, 0, 1, 1, List("end"))
  )

  override val definiteLabels = List("contactinfo", "affiliations", "header", "footer", "caption", "metadata", "discard") //, "discardX", "discardY")
}






