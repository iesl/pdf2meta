package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode
import collection.Seq

class DocFlattener extends DocTransformer
  {
  def apply(rect: DocNode): DocNode =
    {
    // just ignore any existing groupings, but maintain order
    val result = DocNode(rect.id, rect.allLeaves, rect.localInfo, rect.localErrors, false,false)
    result
    }
  }


class PartitionsOrAtomsDocValidator extends DocTransformer
  {
  def apply(rect: DocNode): DocNode =
    {
    rect.allNodesBreadthFirst.map({
                                                       case x if (!x.isAtomic && x.isMergeable) =>
                                                         {
                                                         throw new Error("non-atomic, mergable node")
                                                         }
                                                       case _ =>
                                                       })

    rect
    }
  }

/**
 * For each node, consider the list of children
 * if there is a child that is mergable but not atomic, flatten it
 *
 */
class PartitionHonoringDocFlattener extends DocTransformer
  {
  def apply(rect: DocNode): DocNode =
    {
    val origText = rect.text


    val result = rect match
    {
      case x if x.isAtomic => x
 /*
 // doesn't matter if this node is mergeable
      case x if ! x.isMergeable =>
        {
        // don't flatten the items at this level, just recurse
        val flattenedChildren: Seq[DocNode] = rect.children.map(apply(_))
        DocNode(rect.id, flattenedChildren, rect.localInfo, rect.localErrors, false, false)
        }
*/
      case x =>  {
        // recurse but flatten the child lists into one node here, since this node is not Partitioned
        val flattenedChildren: Seq[DocNode] = rect.children.map(apply(_))
        val newChildren: Seq[DocNode] = flattenedChildren.flatMap({

                                                                  case y if ((! y.isAtomic) && y.isMergeable) => y.children
                                                                  case y  => List(y)
                                                                  })
        DocNode(rect.id, newChildren, rect.localInfo, rect.localErrors, false, rect.isMergeable)
        }

    }

    // just ignore any existing groupings, but maintain order; and retain PartitionDocNodes.
    // val result = DocNode(rect.id, rect.allLeaves, rect.localInfo, rect.localErrors, false)
    val resultText: String = result.text
    assert(resultText == origText)

    result
    }
  }


class AtomDocFlattener extends DocTransformer
  {
  def apply(rect: DocNode): DocNode =
    {
    // just ignore any existing groupings, up to the point of nodes that have been declared "atomic"
    val result = rect.create(rect.spanningAtoms) //DocNode(rect.id, rect.allAtoms, rect.localInfo, rect.localErrors, false,false)
    result
    }
  }

class PageHonoringDocFlattener extends DocTransformer
  {
  def apply(doc: DocNode): DocNode =
    {
    val regroupedPages = doc.children.map(page => DocNode(page.id, page.allLeaves, page.localInfo, page.localErrors, false,false))
    // just ignore any existing groupings below the first level
    val result = doc.create(regroupedPages) // DocNode(doc.id, regroupedPages, doc.localInfo, doc.localErrors,false,false)
    result
    }
  }
