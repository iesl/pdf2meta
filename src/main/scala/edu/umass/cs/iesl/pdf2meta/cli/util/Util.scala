package edu.umass.cs.iesl.pdf2meta.webapp.lib.pdf.util

import collection.{immutable, Seq}
import immutable.Map

object Util
  {
  def histogram[T](list: Seq[T]): Map[T, Int] =
    {
    /*val histEntries = for
    {key <- list.distinct
     count = list.count(_ == key)} yield
      {
      (key -> count)
      }
    val counts = Map(histEntries: _*)
*/

    val groups: Map[T, Seq[T]] = list groupBy (identity)  // groupby does not keep multiple identical items!
    val counts = groups map
                 {case (c, cs) => (c, cs.length)}

    counts
    }

  def histogramAccumulate[T](weightedList: Seq[(T, Int)]): Map[T, Int] =
    {
    val groups: Map[T, Seq[(T, Int)]] = weightedList groupBy (x => x._1)  // does not work !?
    val counts = groups map
                 {
                 case (q, blocks) => (q, blocks.foldLeft(0)((acc, x) => acc + x._2))
                 }

    counts
    }


  import scala.math._

  def squaredDifference(value1: Double, value2: Double) = pow(value1 - value2, 2.0)

  def stdDev(list: List[Double], average: Double) =
    {
    list.isEmpty match
    {
      case false =>
        val squared = list.foldLeft(0.0)(_ + squaredDifference(_, average))
        sqrt(squared / list.length.toDouble)
      case true => 0.0
    }
    }

  /*  def contiguousRuns[K](s: Seq[A])(f: A => K): immutable.Seq[Tuple2[K, Int]] =
    {
    //val m = mutable.Seq.empty[K, Builder[Tuple2[K, Int]]]

    val b = immutable.Seq.newBuilder[Tuple2[K, Int], Seq[Tuple2[K, Int]]]

    var last: K = Null
    var count: Int = 0
    for (elem <- s)
      {
      val key = f(elem)
      key match
      {
        case `last` => count = 1
        case _ => {

        b += ((last,count))
        last = key
        count = 1
        }
      }


      }
     b += ((last,count))

    b.result
    }
  */
  /* def contiguousRuns[K, A](s: List[A])(f: A => K): immutable.List[(K, List[A])] =
      {
      contiguousRunsReversed(s.reverse)(f)
      }*/
  def contiguousRuns[K, A](s: List[A])(f: A => K): immutable.List[(K, List[A])] =
    {
    if (s.isEmpty) Nil
    else
      {
      val p: List[(K, List[A])] = contiguousRuns(s.tail)(f)

      val a: A = s.head
      val k: K = f(a)

      if (p.isEmpty)
        {
        List((k, List(a)))
        }
      else
        {
        val (key, values) = p.head

        if (k == key)
          {
          (k, a :: values) :: p.tail
          }
        else
          {
          (k, List(a)) :: p
          }
        }
      }
    }
  def collapseShortRuns[K, A](runs: List[(K, List[A])], minLength: Int): immutable.List[(K, List[A])] =
    {
    if (runs.isEmpty) Nil
    else if (runs.length == 1) runs
    else
      {
      val (key, values) = runs.head
      val next = collapseShortRuns(runs.tail, minLength)

      val (nextkey, nextvalues) = next.head
      if (values.length < minLength || key == nextkey)
        {
        ((nextkey, values ::: nextvalues)) :: next.tail
        }
      else
        {runs.head :: next}
      }
    }
  }
