package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode

/**
 * Group consecutive children that have appropriate similarity
 */
abstract class SimilarityDocMerger extends DocTransformer
  {
  def apply(rect: DocNode): DocNode =
    {
    if (rect.children.length == 0)
      {
      rect
      }
    else if (rect.children.length == 1)
           {
           // recurse, but declare nonatomic
           DocNode(rect.id, rect.children.map(apply(_)), rect.localInfo, rect.localErrors, false)
           }
    else
      {
      // reverse recursion: first merge the items at this level; then merge within the _new_ children
      val intermediateChildrenR: List[DocNode] = rect.children.foldLeft(List[DocNode]())(merge)
      val intermediateChildren: List[DocNode] = intermediateChildrenR.reverse
      val result =
        if (intermediateChildren.length == 1)
          {
          // there were more children before and they all got merged, so the result is an atom
          // this node is already a single item block, so all of the children got merged into a new node.  Recursing this won't terminate.
          // instead recurse into the existing children
          // actually don't bother, since this is an atom; just return the existing children
          DocNode(rect.id, rect.children, rect.localInfo, rect.localErrors, true)
          }
        else
          {
          // the intermediate children may be atoms (which the merging taxes care of)
          // but this one may not be
          val newChildren = intermediateChildren.map(apply(_))
          DocNode(rect.id, newChildren, rect.localInfo, rect.localErrors, false)
          }
      result
      }
    }

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

  def similar(an: DocNode, bn: DocNode): Boolean
  }


class ParagraphMerger extends SimilarityDocMerger
  {
  // note relationship with RectangularReadingOrder
  def similar(an: DocNode, bn: DocNode): Boolean =
    {
    (an.rectangle, bn.rectangle) match
    {
      case (Some(a), Some(b)) =>
        {
        val sameFont = (an.dominantFont, bn.dominantFont) match
        {
          case (Some(af), Some(bf)) => af.equalsWithinOneQuantum(bf)
          case _ => false
        }


        val sameColumn = b.isBelow(a.top) && ((a.left - b.left).abs < 5) // && ((a.right - b.right).abs < 5)
        // includes hanging indent
        val sameColumnWithIndent = b.isBelow(a.top) && ((a.left - b.left).abs < 20) && ((a.right - b.right).abs < 5)

        sameFont && (sameColumn || sameColumnWithIndent)
        }
      case _ => false
    }
    }
  }

/**
 * Group consecutive children that have appropriate similarity
 */
class LineMerger extends SimilarityDocMerger
  {
  // note relationship with RectangularReadingOrder
  def similar(an: DocNode, bn: DocNode): Boolean =
    {
    (an.rectangle, bn.rectangle) match
    {
      case (Some(a), Some(b)) =>
        {
        // ignore font, due to italics, superscripts, and such
        /*
        val sameFont = (an.dominantFont, bn.dominantFont) match
        {
          case (Some(af), Some(bf)) => af.equalsWithinOneQuantum(bf)
          case _ => false
        }*/
        // allow overlap
        val sameLine = b.isRightOf(a.left) && ((a.top - b.top).abs < 2) && ((a.bottom - b.bottom).abs < 2)

        //sameFont &&
        sameLine
        }
      case _ => false
    }
    }
  }

