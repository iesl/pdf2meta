package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode

class DocDeepSorter(val ordering: Ordering[DocNode]) extends PostOrderDocTransformer
  {

/*
  def apply(rect: DocNode): DocNode =
    {
    val result = rect.deepSorted(ordering)
    result
    /*val children = for (c <- rect.children) yield (apply(c))

    DocNode(rect.id, children.sorted(ordering), rect.localInfo, rect.localErrors)*/
    }*/
override def applyLocalOnly(v1: DocNode) = Some(v1.create(v1.children.toSeq.sorted(ordering)))
}
