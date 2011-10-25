package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import collection.Seq
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{RectangleOnPage, DocNode}


/*class ReferencesSectionMerger extends SimilarityDocMerger
  {
  // note relationship with RectangularReadingOrder
  def similar(an: DocNode, bn: DocNode): Boolean =
    {
    (an.rectangle, bn.rectangle) match
    {
      case (Some(a), Some(b)) =>
        {
        val sameColumn =  b.isBelow(a.top) && (a.left == left1 || a.left == left2) && (b.left == left1 || b.left == left2)
        sameColumn
        }
      case _ => false
    }
    }
  }*/


/**
 * Find a hanging-indent situation, assuming that there is already a node encompassing the set
 */
@deprecated
class ReferencesMerger extends DocTransformer
  {
  def apply(rect: DocNode): DocNode =
    {

    if (rect.children.length == 0)
      {
      rect
      }
    else
      {
      val rects: Seq[RectangleOnPage] = rect.children.map(_.rectangle).flatten
      val lefts = rects.map(_.left).distinct.sorted

      val intermediateChildren = if (lefts.length == 2)
                                   {
                                   val start = lefts(0);
                                   val continue = lefts(1);

                                   def merge(precontext: List[DocNode], r: DocNode): List[DocNode] =
                                     {
                                     precontext match
                                     {
                                       case Nil => List(r)
                                       case h :: t => if (similar(h, r))
                                                        {(h :+ r) :: t}
                                                      else
                                                        {r :: precontext}
                                     }
                                     }

                                   def similar(an: DocNode, bn: DocNode): Boolean =
                                     {
                                     (an.rectangle, bn.rectangle) match
                                     {
                                       case (Some(a), Some(b)) => b.left == continue // ignore fonts-- lots of italics etc.  // could check for size, though?
                                       case _ => false
                                     }
                                     }

                                   // following matches SimilarityDocMerger, but here the merge and similar functions have to be inside the apply
                                   // reverse recursion: first merge the items at this level; then merge within the _new_ children
                                   val intermediateChildrenR: List[DocNode] = rect.children.foldLeft(List[DocNode]())(merge)
                                   val intermediateChildren: List[DocNode] = intermediateChildrenR.reverse
                                   intermediateChildren
                                   }
                                 else
                                   {
                                   rect.children
                                   }

      val newChildren: Seq[DocNode] =
        if (intermediateChildren.length == 1)
          {
          // this node is already a single item block, so all of the children got merged into a new node.  Recursing this won't terminate.
          // instead recurse into the existing children
          rect.children
          }
        else
          {
          intermediateChildren.map(apply(_))
          }
      val result = DocNode(rect.id, newChildren, rect.localInfo, rect.localErrors,true)
      result
      }
    }
  }
