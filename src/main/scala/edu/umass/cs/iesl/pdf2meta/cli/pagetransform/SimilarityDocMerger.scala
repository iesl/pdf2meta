package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{AnnotatedDocNode, DocNode}
import com.weiglewilczek.slf4s.Logging

/**
 * Group consecutive children that have appropriate similarity
 */
abstract class SimilarityDocMerger extends PreOrderDocTransformer with Logging
  {


 def applyLocalOnly(node: DocNode) =
  {
  val intermediateChildrenR: List[DocNode] = node.children.foldLeft(List[DocNode]())(merge)
      val intermediateChildren: List[DocNode] = intermediateChildrenR.reverse
  Some(node.create(intermediateChildren))

  }

/*
  def apply(rect: DocNode): DocNode =
    {
    if (rect.children.length == 0)
      {
      rect
      }
    else if (rect.children.length == 1)
           {
           // just recurse, don't flatten yet
           rect.create(rect.children.map(apply(_)))
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
          //DocNode(rect.id, rect.children, rect.localInfo, rect.localErrors, true)
          rect.makeAtomic
          }
        else
          {
          // the intermediate children may be atoms (which the merging takes care of)
          // but this one may not be
          val newChildren = intermediateChildren.map(apply(_))
          this match
          {
            case x: AnnotatedDocNode =>
              {
              logger.error("wtf")
              }
            case _ =>
          }
          //DocNode(rect.id, newChildren, rect.localInfo, rect.localErrors, false)
          rect.create(newChildren)
          }
      result
      }

    }*/
def merge(precontext: List[DocNode], r: DocNode): List[DocNode] =
    {
    precontext match
    {
      case Nil => List(r)
      case h :: t => if (similar(h, r))  //h.isMergeable && r.isMergeable &&
                       {(h :++ r) :: t}
                     else
                       {r :: precontext}
    }
    }

  def similar(an: DocNode, bn: DocNode): Boolean
  }

@deprecated // see IndentedParagraphsMerger
class ParagraphMerger extends SimilarityDocMerger with Logging
  {
  // note relationship with RectangularReadingOrder
  def similar(an: DocNode, bn: DocNode): Boolean =
    {
    an match
    {
      case ana: AnnotatedDocNode =>
        {
        logger.debug("sideways")
        }
      case _ =>
    }

    (an.rectangle, bn.rectangle) match
    {
      case (Some(a), Some(b)) =>
        {
        val sameFont = (an.dominantFont, bn.dominantFont) match
        {
          case (Some(af), Some(bf)) => af.equalsWithinOneQuantum(bf)
          case _ => false
        }


        val sameColumn = (a.page == b.page) && b.isBelow(a.top) && ((a.left - b.left).abs < 5) // && ((a.right - b.right).abs < 5)
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
        val sameLine = (a.page == b.page) && b.isRightOf(a.left) && ((a.top - b.top).abs < 3) && ((a.bottom - b.bottom).abs < 3)

        //sameFont &&
        sameLine
        }
      case _ => false
    }
    }
  }


class SidewaysLineMerger extends SimilarityDocMerger
  {
  def similar(an: DocNode, bn: DocNode): Boolean =
    {

    def checkOneCharColumn: Boolean =
      {
      (an.rectangle, bn.rectangle) match
      {
        case (Some(a), Some(b)) =>
          {
          /*val sameFont = (an.dominantFont, bn.dominantFont) match
            {
              case (Some(af), Some(bf)) => af.equalsWithinOneQuantum(bf)
              case _ => false
            }
*/
          val sameColumn = (a.page == b.page) && b.isBelow(a.top) && ((a.left - b.left).abs < 5)

          // sameFont &&
          sameColumn
          }
        case _ => false
      }
      }

    (an.text.length, bn.text.length) match
    {
      case (1, 1) => checkOneCharColumn
      case (x, 1) =>
        {
        an match
        {
          case ana: AnnotatedDocNode =>
            {if (ana.annotations.contains("sideways")) checkOneCharColumn else false}
          case _ => false
        }
        }
      case _ => false
    }
    }


  override def merge(precontext: List[DocNode], r: DocNode): List[DocNode] =
    {
    precontext match
    {
      case Nil => List(r)
      case h :: t => if (similar(h, r))  //h.isMergeable && r.isMergeable &&
                       {
                       val merged = AnnotatedDocNode(h.id + "+" + r.id, h.children :+ r, h.localInfo, h.localErrors, Seq("sideways"))
                       merged :: t
                       }
                     else
                       {r :: precontext}
    }
    }
  }

// SidewaysParagraphMerger: ignore
