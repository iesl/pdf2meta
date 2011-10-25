package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{PartitionedDocNode, DocNode}

class StarDocPartitioner extends DocTransformer
  {
  def apply(rect: DocNode): DocNode =
    {
    // just ignore any existing groupings
    val result = DocNode(rect.id, rect.allLeaves, rect.localInfo, rect.localErrors,false)
    result
    }
  }

class AtomStarDocPartitioner extends DocTransformer
  {
  def apply(rect: DocNode): DocNode =
    {
    // just ignore any existing groupings, up to the point of nodes that have been declared "atomic"
    val result = DocNode(rect.id, rect.allAtoms, rect.localInfo, rect.localErrors,false)
    result
    }
  }

class PageStarDocPartitioner extends DocTransformer
  {
  def apply(doc: DocNode): DocNode =
    {
    val regroupedPages = doc.children.map(page => DocNode(page.id, page.allLeaves, page.localInfo, page.localErrors,false))
    // just ignore any existing groupings below the first level
    val result = PartitionedDocNode(doc.id, regroupedPages, doc.localInfo, doc.localErrors)
    result
    }
  }
