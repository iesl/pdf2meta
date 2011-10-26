package edu.umass.cs.iesl.pdf2meta.cli.util

import collection.mutable.HashMap
import collection.Seq


object WeightedSet
  {
  def apply[T](x: Seq[Tuple2[T, Double]]): WeightedSet[T] =
    {
    new WeightedSet[T]
      {val asMap = Map() ++ x.toMap}
    }
  }

class Tuple2DoubleValueOrdering[T] extends Ordering[Tuple2[T, Double]]
  {
  def compare(x: (T, Double), y: (T, Double)) = (y._2.compare(x._2))
  }

trait WeightedSet[T]
  {
  def asMap: collection.Map[T, Double]
  def apply(v: T) = asMap(v)

  // don't make this a lazy val because the impl could be mutable
  def byWeight = asMap.toSeq.sorted(new Tuple2DoubleValueOrdering[T])

  def best: Option[T] =
    {
    if (asMap.isEmpty)
      None
    else
      Some(byWeight.head._1)
    }

  def unambiguousBest(secondBestRatioThreshold: Double): Option[T] =
    {
    if (asMap.isEmpty)
      None
    else if (asMap.size == 1)
           {
           val p: (T, Double) = asMap.head
           if (p._2 <= 0) None
           else Some(p._1)
           }
    else
      {
      val w = byWeight;
      val w1 = w.head._2
      val w2 = w.tail.head._2
      if (w2 / w1 <= secondBestRatioThreshold)
        {Some(byWeight.head._1)}
      else None
      }
    }

  def mkString(sep: String) = asSeq.mkString(sep)

  def normalized: WeightedSet[T] =
    {
    val c = asMap.values.sum
    def normalize(x: (T, Double)) = (x._1, x._2 / c)

    val result = WeightedSet[T](asMap.map(normalize).toSeq)
    result
    }

  def asSeq: Seq[(T, Double)] = asMap.toSeq.sortBy(_._2).reverse
  override def toString = asSeq.toString()
  }

object MutableWeightedSet
  {
  def apply[T](): MutableWeightedSet[T] =
    {
    new MutableWeightedSet[T]
      {
      val asMap = new HashMap[T, Double]()
        {
        override def default(key: T) = 0
        }
      }
    }
  }

trait MutableWeightedSet[T] extends WeightedSet[T]
  {
  //def incrementBy(v: T, d: Double)
  override def asMap: collection.mutable.Map[T, Double]
  def incrementBy(v: T, d: Double)
    {
    val cur: Double = asMap.getOrElse(v, 0) // the else case should never fire because of the default
    asMap.update(v, cur + d)
    }
  }
