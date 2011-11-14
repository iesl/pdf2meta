package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode

class DocDeepSorter(val ordering: Ordering[DocNode]) extends PostOrderDocTransformer
  {
  override def applyLocalOnly(v1: DocNode) = Some(v1.create(v1.children.toSeq.sorted(ordering)))
  }
