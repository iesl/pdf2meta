package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode

class DocFlattener extends DocTransformer
  {
  def apply(rect: DocNode): DocNode =
    {
    // just ignore any existing groupings, but maintain order
    val result = DocNode(rect.id, rect.allLeaves, rect.localInfo, rect.localErrors)
    result
    }
  }

/**
 * Ignore any existing groupings below the first level
 */
class PageHonoringDocFlattener extends DocTransformer
  {
  def apply(doc: DocNode): DocNode =
    {
    val regroupedPages = doc.children.map(page => DocNode(page.id, page.allLeaves, page.localInfo, page.localErrors))
    val result = doc.create(regroupedPages)
    result
    }
  }
