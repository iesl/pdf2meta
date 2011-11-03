package edu.umass.cs.iesl.pdf2meta.cli.config

import collection.mutable.LinkedHashMap
import edu.umass.cs.iesl.pdf2meta.cli.segmentsmoother.CoarseLabelModel
import scala.Int

class StandardCoarseLabelModel extends CoarseLabelModel
  {
  override val coarseLabelMap: LinkedHashMap[String, L] = LinkedHashMap("title" -> L("title", 30, 200, 1, 5, List("title")), "authors" -> L("authors", 10, 10000, 1,10, List("authors")),
                                                                        "abstract" -> L("abstract", 100, 5000, 1, 5, List("abstract")),
                                                                        "body" -> L("body", 500, Int.MaxValue, 5, Int.MaxValue, List("body", "header", "footnote", "caption")),
                                                                        "references" -> L("references", 0, Int.MaxValue, 0,Int.MaxValue,List("references")), "end" -> L("end", 0, 0, 1,1,List("end")))
  override val definiteLabels = List("contactinfo", "affiliations", "header", "footer", "metadata", "discard", "discardX", "discardY")
  }






