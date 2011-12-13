package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import collection.immutable
import runtime.FractionalProxy
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{LeafDocNode, RectangleOnPage, DocNode}

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

/**
 * Find a hanging-indent situation.
 * The strategy is to make a list of nodes that could possibly be the start of a references section, because the node the following node starts either at the same X coordinate or indented from there.
 * Each such node get stored with the indentation level.
 * Then we find strings of consecutive nodes with the same indent distance (to cover the case that references continue in the next column or on the next page).
 */
class IndentedParagraphsMerger extends PostOrderDocTransformer
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
          val aFontSizes = a.allFonts.map(_._1.height)
          val bFontSizes = nextNode.allFonts.map(_._1.height)

          // note font sizes are already quantized
          !aFontSizes.intersect(bFontSizes).filterNot(_ == 0.0).isEmpty
          }

        if (!minimalFontAgreement || aRect.page != nextRect.page || !nextRect.isBelow(aRect.top)) // || !sameFont
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
              else
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


  def applyLocalOnly(node: DocNode) =
    {

    // the children have already been processed
    val runs = contiguousIndentRuns(node.children.toList)
    val newChildren = for (r <- runs)
    yield
      {
      r match
      {
        case (im, mergeNodes) if mergeNodes.length == 1 => mergeNodes.head
        case (im, mergeNodes) =>
          {
          // declare the merged node atomic iff all of the children were atoms
          val nonAtomicChildren: List[DocNode] = mergeNodes.filter(!_.isLeaf)
          val allChildrenAtomic: Boolean = nonAtomicChildren.length == 0
          if (allChildrenAtomic)
            {
            LeafDocNode((mergeNodes map (_.id)).mkString("+"), mergeNodes, None, None)
            }
          else
            {
            DocNode((mergeNodes map (_.id)).mkString("+"), mergeNodes, None, None)
            }
          }
      }
      }
    Some(node.create(newChildren))


    }

  }

