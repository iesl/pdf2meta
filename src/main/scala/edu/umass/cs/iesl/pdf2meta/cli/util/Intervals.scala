package edu.umass.cs.iesl.pdf2meta.cli.util

import collection.JavaConversions
import com.davidsoergel.dsutils.range.{Interval, BasicInterval, MultiIntervalUnion}

object Intervals
  {

  class DoubleInterval(val min: Double, val max: Double) extends Tuple2[Double, Double](min, max)
    {
    def width = max - min
    }

  implicit def tupleToDoubleInterval(t: (Double, Double)): DoubleInterval =
    {
    new DoubleInterval(t._1, t._2)
    }


  def invert[T](list: List[(T, T)], min : T, max : T): List[(T, T)] = invertIgnoreEdges(((min,min) :: list) :+ (max,max))

  // is there some better foldy way?
  // note we ignore the edges and just return the holes.  Not really correct but it's what we need right now.
  def invertIgnoreEdges[T](list: List[(T, T)]): List[(T, T)] =
    {
    // require list is sorted and nonoverlapping
    list match
    {
      case Nil => Nil
      case a :: t =>
        t match
        {
          case Nil => Nil
          case _ =>
            (a._2, t.head._1) :: invertIgnoreEdges(list.tail);
        }
      // assert(b._1 > a._2)
    }
    }


  def largestHole(list: List[(Double, Double)], minimum: Int): Option[(Double, Double)] =
    {
    val holes: List[(Double, Double)] = holesBySize(list)
    val result = holes match
    {
      case a :: b if (a._2-a._1 >= minimum) => Some(a)
      case _ => None;
    }
    result
    }

  def holesBySize(list: List[(Double, Double)]): List[(Double, Double)] =
    {
    invertIgnoreEdges(list).sortBy[Double]((x: (Double, Double)) => x._1 - x._2)  // note reverse sort
    }

  implicit def tupleToInterval(t: (Double, Double)): Interval[java.lang.Double] =
    {
    new BasicInterval[java.lang.Double](t._1, t._2, true, true)
    }
  implicit def intervalToTuple(i: Interval[java.lang.Double]): (Double, Double) = (i.getMin, i.getMax)

  def union(intervals: Seq[(Double, Double)]): List[(Double, Double)] =
    {
    val i: Seq[Interval[java.lang.Double]] = intervals.map(tupleToInterval)
    val u: MultiIntervalUnion[java.lang.Double] = new MultiIntervalUnion(JavaConversions.setAsJavaSet(i.toSet))
    val r = JavaConversions.asScalaIterator[Interval[java.lang.Double]](u.iterator()).toList
    r.map(intervalToTuple)
    }

  }
