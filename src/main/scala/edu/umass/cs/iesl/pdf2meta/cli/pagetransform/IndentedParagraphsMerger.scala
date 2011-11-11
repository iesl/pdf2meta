package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import collection.immutable
import runtime.FractionalProxy
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{RectangleOnPage, DocNode}

object IndentedParagraphsMerger
  {

  val slop: Double = 5

  class ApproximateDouble(val self: Double) extends FractionalProxy[Double]
    {
    protected val integralNum = Numeric.DoubleAsIfIntegral
    def =~(x: Double): Boolean = doubleWrapper(x - self).abs <= slop
    }

  implicit def toApproximateDouble(d: Double): ApproximateDouble =
    {
    new ApproximateDouble(d)
    }
  }

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
 * Find a hanging-indent situation.
 * The strategy is to make a list of nodes that could possibly be the start of a references section, because the node the following node starts either at the same X coordinate or indented from there.
 * Each such node get stored with the indentation level.
 * Then we find strings of consecutive nodes with the same indent distance (to cover the case that references continue in the next column or on the next page).
 */
class IndentedParagraphsMerger extends DocTransformer
  {
  val MaxIndent = 20

  import IndentedParagraphsMerger.toApproximateDouble

  case class IndentationModel(firstLineX: Double, continuationLineX: Option[Double])
    {
    def indentDistance: Option[Double] = continuationLineX.map(_ - firstLineX)
    }

  // based on Util.contiguousRuns
  def contiguousIndentRuns(s: List[DocNode]): immutable.List[(IndentationModel, List[DocNode])] =
    {
    if (s.isEmpty) Nil
    else
      {
      val suffix: List[(IndentationModel, List[DocNode])] = contiguousIndentRuns(s.tail)

      val a: DocNode = s.head
      val aLeft = a.rectangle.get.left

      if (suffix.isEmpty)
        {
        List((new IndentationModel(aLeft, None), List(a)))
        }
      else
        {
        val (currentIndent: IndentationModel, currentNodes) = suffix.head
        val nextNode = currentNodes.head // shouldn't be empty
        val aRect: RectangleOnPage = a.rectangle.get
        val nextRect: RectangleOnPage = nextNode.rectangle.get

        // font information is a mess in references sections -- e.g. italic glyphs can be much taller than roman, etc.
        val sameFont = (a.dominantFont, nextNode.dominantFont) match
        {
          case (Some(af), Some(bf)) => af.sizeEqualsWithin(1.0)(bf) // use only font size, not font ID, because references have a lot of italic/bold variation
          case _ => false
        }

        // how about: blocks are mergeable if there is some non-zero font size in common
        val minimalFontAgreement: Boolean =
          {
          val aFontSizes = a.allFonts.map(_._2)
          val bFontSizes = nextNode.allFonts.map(_._2)

          // note font sizes are already quantized
          ! aFontSizes.intersect(bFontSizes).filterNot(_ == 0.0).isEmpty

          }

        if ((!a.isMergeable) || (!nextNode.isMergeable) || !minimalFontAgreement || aRect.page != nextRect.page || !nextRect.isBelow(aRect.top)) // || !sameFont
          {
          // start a new block
          (IndentationModel(aLeft, None), List(a)) :: suffix
          }
        else
          {
          // we're potentially in the same column
          currentIndent match
          {
            case IndentationModel(x: Double, None) =>
              {
              val indentDistance = x - aLeft

              indentDistance match
              {
                case d if d > MaxIndent || d < -MaxIndent => (IndentationModel(aLeft, None), List(a)) :: suffix // not same column; start a new block
                case d if d =~ 0 => (currentIndent, a :: currentNodes) :: suffix.tail // add this line to the current indent block
                case d if d > 0 => (IndentationModel(aLeft, Some(x)), a :: currentNodes) :: suffix.tail // the head is a hanging indent of the tail
                case d if d < 0 => (IndentationModel(x, Some(aLeft)), a :: currentNodes) :: suffix.tail // the head could be a continuation line where the tail has only start lines
              }
              }
            case IndentationModel(x, Some(c)) =>
              {
              if (aLeft =~ x || aLeft =~ c)
                {
                (currentIndent, a :: currentNodes) :: suffix.tail // add this line to the current indent block
                }
              else // if (aLeft != x && aLeft != c)
                {
                // this line is not part of the current block; start a new one
                (IndentationModel(aLeft, None), List(a)) :: suffix
                }
              }
          }
          }
        }
      }
    }

  def apply(rect: DocNode): DocNode =
    {
    // merge the children recursively, and merge at this level, but don't flatten
    // also don't treat PartitionedDocNode specially, since we can just run PartitionHonoringDocFlattener first.

    rect match
    {
      case x if x.isAtomic => x
      case x =>
        {
        val ch = x.children.map(apply(_))
        val runs = contiguousIndentRuns(ch.toList)
        val newChildren = for (r <- runs)
        yield
          {
          r match
          {
            case (im, mergeNodes) if mergeNodes.length == 1 => mergeNodes.head
            case (im, mergeNodes) =>
              {
              // declare the merged node atomic iff all of the children were atoms
              val nonAtomicChildren: List[DocNode] = mergeNodes.filter(!_.isAtomic)
              val allChildrenAtomic: Boolean = nonAtomicChildren.length == 0
              DocNode((mergeNodes map (_.id)).mkString("+"), mergeNodes, None, None, allChildrenAtomic, false)
              }
          }
          }

        rect.create(newChildren)
        }
    }
    }

  /*    if (rect.children.length == 0)
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
      }*/
  }


/*
class ReferencesDPCell[S, L](x: Option[S], y: Option[L], prefix: Option[DPCell[S, L]], score: Double) extends DPCell[S, L](x, y, prefix, score)
  {
val firstLineX : Double
val continuationLineX : Double
}

class ReferencesAligner extends ExtendedMatchDPAligner[DocNode, String]
  {
val afterEndLabelSkipPenalty = 0
val afterEndTextSkipPenalty = 0
val beforeStartLabelSkipPenalty = 0
val beforeStartTextSkipPenalty = 0
def createCell(x: Option[DocNode], y: Option[String], prefix: Option[DPCell[DocNode, String]], score: Double) = new ReferencesDPCell[DocNode,String](x,y,prefix,score)
val heavyTextSkipPenalty = _
val labelSkipPenalty = _
val lightTextSkipPenalty = _
def scoreDelete(i: Seq[DocNode], j: Seq[String], prefix: Option[DPCell[DocNode, String]]) = null
def scoreInsert(i: Seq[DocNode], j: Seq[String], prefix: Option[DPCell[DocNode, String]]) = null
def scoreMatch(i: Seq[DocNode], j: Seq[String], prefix: Option[DPCell[DocNode, String]]) = null
}
*/
