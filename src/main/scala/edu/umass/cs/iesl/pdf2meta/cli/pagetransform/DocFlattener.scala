package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import collection.Seq
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{PartitionedDocNode, DocNode}

class DocFlattener extends DocTransformer
  {
  def apply(rect: DocNode): DocNode =
    {
    // just ignore any existing groupings, but maintain order
    val result = DocNode(rect.id, rect.allLeaves, rect.localInfo, rect.localErrors)
    result
    }
  }

/*
class PartitionsOrAtomsDocValidator extends DocTransformer
  {
  def apply(rect: DocNode): DocNode =
    {
    rect.allNodesBreadthFirst.map({
                                                       case x if (!x.isAtomic) =>   // && x.isMergeable
                                                         {
                                                         throw new Error("non-atomic, mergable node")
                                                         }
                                                       case _ =>
                                                       })

    rect
    }
  }
  */

/*
/**
 * For each node, consider the list of children
 * if there is a child that is mergable but not atomic, flatten it
 *
 */
class PartitionHonoringDocFlattener extends PreOrderDocTransformer
  {
 def applyLocalOnly(node: DocNode) =
  {
  node match {
    case x : PartitionedDocNode => Some(x)
    case x =>
  }
  }
  }

/*
def apply(node: DocNode): DocNode =
    {
    val origText = node.text


    val result = node match
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
        val flattenedChildren: Seq[DocNode] = node.children.map(apply(_))
        val newChildren: Seq[DocNode] = flattenedChildren.flatMap({

                                                                  case y if ((! y.isAtomic)) => y.children  // && y.isMergeable
                                                                  case y  => List(y)
                                                                  })
        DocNode(node.id, newChildren, node.localInfo, node.localErrors, false)
        }

    }

    // just ignore any existing groupings, but maintain order; and retain PartitionDocNodes.
    // val result = DocNode(rect.id, rect.allLeaves, rect.localInfo, rect.localErrors, false)
    val resultText: String = result.text
    assert(resultText == origText)

    result
    }
  }
*/
*/

class AtomDocFlattener extends DocTransformer
  {
  def apply(node: DocNode): DocNode =
    {
    // just ignore any existing groupings, up to the point of nodes that have been declared "atomic"
    val result = node.create(node.allLeaves) //DocNode(rect.id, rect.allAtoms, rect.localInfo, rect.localErrors, false,false)
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
