package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode

class DocDeepSorter(val ordering: Ordering[DocNode]) extends DocTransformer
  {
  def apply(rect: DocNode): DocNode =
    {
    val result = rect.deepSorted(ordering)
    result
    /*val children = for (c <- rect.children) yield (apply(c))

    DocNode(rect.id, children.sorted(ordering), rect.localInfo, rect.localErrors)*/
    }
  }
