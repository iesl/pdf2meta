package edu.umass.cs.iesl.pdf2meta.cli.segmentsmoother

import com.typesafe.scalalogging.slf4j.Logging
import collection.Seq
import edu.umass.cs.iesl.scalacommons.Memoize1

trait SequenceAligner[S, L] extends ((Seq[S], Seq[L]) => (Double, Seq[(Option[S], Option[L])]))

// return the backtrace paths, in reverse order, with scores
class DPCell[S, L](val x: Option[S], val y: Option[L], val prefix: Option[DPCell[S, L]], val score: Double) extends Ordered[DPCell[S, L]]
  {

  def traceback: List[DPCell[S, L]] = this :: prefix.map(_.traceback).getOrElse(Nil)

  override def compare(that: DPCell[S, L]) = score.compare(that.score)

  lazy val findLastMatch: Option[DPCell[S, L]] =
    {
    (x, y) match
    {
      case (Some(q), Some(z)) => Some(this);
      case _ => prefix.map(_.findLastMatch).getOrElse(None)
    }
    }
  }

class LengthDPCell[S, L](x: Option[S], y: Option[L], prefix: Option[DPCell[S, L]], score: Double, val length: Int) extends DPCell[S, L](x, y, prefix, score)
  {
  // (blocks, chars)
  lazy val prevMatched: (Int, Int) =
    {
    val t: (Option[L], Option[DPCell[S, L]]) = (y, prefix)
    val result = t match
    {
      case (Some(yy), Some(ll: LengthDPCell[S, L])) => if (yy == ll.y.getOrElse("Bogus")) ll.matched else (0,0)
      case _ => (0,0)
    }
    result
    }

  lazy val matched: (Int,Int) =
    {
    val (blocks, chars) = prevMatched
    val newChars = length + chars
    val newBlocks = blocks+1
    (newBlocks, newChars)
    }
  }


/**
 * A dynamic programming aligner (e.g., Smith-Waterman etc.) that allows symbols from the second set
 * (e.g. "Labels") to match multiple contiguous symbols from the first set (e.g. "Words", "Text segments", etc.)
 */
trait ExtendedMatchDPAligner[S, L] extends SequenceAligner[S, L] with Logging
  {
  def createCell(x: Option[S], y: Option[L], prefix: Option[DPCell[S, L]], score: Double): DPCell[S, L]

  /**
   * Skipping "text" at the beginning or end incurs a penalty for global alignment, not for local
   */
  val beforeStartTextSkipPenalty: Double
  val afterEndTextSkipPenalty: Double

  /**
   * Skipping text with a preliminary label that we're not trying to align anyway incurs a light penalty
   */
  val lightTextSkipPenalty: Double

  /**
   * Skipping text that ought to be aligned with a known label incurs a heavier penalty
   */
  val heavyTextSkipPenalty: Double

  /**
   * Skipping a label likely incurs a very high penalty, whether at the ends or internally
   */
  val beforeStartLabelSkipPenalty: Double

  val labelSkipPenalty: Double
  val afterEndLabelSkipPenalty: Double


  // the scoring functions return not only the score but also the matched symbols
  // this allows extending a "match" through an insert or delete, based on some criterion
  def scoreMatch(i: Seq[S], j: Seq[L], prefix: Option[DPCell[S, L]]): (Option[S], Option[L], Double)
  def scoreInsert(i: Seq[S], j: Seq[L], prefix: Option[DPCell[S, L]]): (Option[S], Option[L], Double)
  def scoreDelete(i: Seq[S], j: Seq[L], prefix: Option[DPCell[S, L]]): (Option[S], Option[L], Double)

  // basics:
  // counting is 0-based
  // (i, j) means x,y, where the actual text is x and the labels are y
  // An "insert" means there is extra unlabelled text.  That's horizontal, so i advances but j does not
  // A "delete" means an expected label is missing.  That's vertical, so j advances but i does not.
  // i and j are actually reverse lists of the symbols consumed so far
  def h(t: (Seq[S], Seq[L]), memoh: ((Seq[S], Seq[L])) => Option[DPCell[S, L]]): Option[DPCell[S, L]] =
    {
    //val (i,j)=t
    val result = t match
    {
      case (Nil, Nil) => None
      case (i, Nil) =>
        {
        val insertPrefix: (Option[DPCell[S, L]], Double) = memoh(i.tail -> Nil).map((c: DPCell[S, L]) => (Some(c), c.score)).getOrElse((None, 0d))
        Some(createCell(None, None, insertPrefix._1, i.length * beforeStartTextSkipPenalty))
        }
      case (Nil, j) =>
        {
        val deletePrefix: (Option[DPCell[S, L]], Double) = memoh(Nil -> j.tail).map((c: DPCell[S, L]) => (Some(c), c.score)).getOrElse((None, 0d))
        Some(createCell(None, None, deletePrefix._1, j.length * beforeStartLabelSkipPenalty))
        }
      case (i, j) =>
        {
        //logger.debug("Computing DP cell: " + i.length + ", " + j.length)
        val matchPrefix: (Option[DPCell[S, L]], Double) = memoh(i.tail -> j.tail).map((c: DPCell[S, L]) => (Some(c), c.score)).getOrElse((None, 0d))
        val matchMove = scoreMatch(i, j, matchPrefix._1)
        val matchScore: Double = matchPrefix._2 + matchMove._3

        val insertPrefix: (Option[DPCell[S, L]], Double) = memoh(i.tail -> j).map((c: DPCell[S, L]) => (Some(c), c.score)).getOrElse((None, 0d))
        val insertMove = scoreInsert(i, j, insertPrefix._1)
        val insertScore = insertPrefix._2 + insertMove._3

        val deletePrefix: (Option[DPCell[S, L]], Double) = memoh(i -> j.tail).map((c: DPCell[S, L]) => (Some(c), c.score)).getOrElse((None, 0d))
        val deleteMove = scoreDelete(i, j, deletePrefix._1)
        val deleteScore = deletePrefix._2 + deleteMove._3

        val score = matchScore.max(insertScore).max(deleteScore)

        score match
        {
          //case s if (s <= 0.0) => None // local alignment, reset
          case s if (s == matchScore) => Some(createCell(matchMove._1, matchMove._2, matchPrefix._1, matchScore))
          case s if (s == insertScore) => Some(createCell(insertMove._1, insertMove._2, insertPrefix._1, insertScore))
          case s if (s == deleteScore) => Some(createCell(deleteMove._1, deleteMove._2, deletePrefix._1, deleteScore))
        }
        }
    }
    result
    }

  def apply(textSymbols: Seq[S], labels: Seq[L]) =
    {
    val mh = Memoize1.Y[(Seq[S], Seq[L]), Option[DPCell[S, L]]](h)

    // global alignment.  Local and semilocal etc. can be produced by setting zero end gap penalties
    val bestCell: DPCell[S, L] = mh((textSymbols.reverse, labels.reverse)).getOrElse(throw new Error("DP failed: No best cell found"))

    val alignment: List[(Option[S], Option[L])] = for (c: DPCell[S, L] <- bestCell.traceback.reverse) yield (c.x, c.y)

    def debug() {
      val tr = bestCell.traceback
      val tabTable: String = (for (j <- Range(0, labels.length + 1)) yield
        {
        val lPrefix: Seq[L] = labels.slice(0, j).reverse
        (for (i <- Range(0, textSymbols.length + 1)) yield
          {
          val sPrefix: Seq[S] = textSymbols.slice(0, i).reverse
          val result: Option[DPCell[S, L]] = mh((sPrefix, lPrefix))
          result.map(x => (("%3.2f" format x.score) + " (" + x.y.getOrElse("") + ")" + (if (tr.contains(x)) "*" else ""))).getOrElse("")
          }).mkString("\t")
        }).mkString("\n")
      logger.debug("\n" + tabTable)
      }

    debug

    (bestCell.score, alignment.toSeq)
    }
  }
