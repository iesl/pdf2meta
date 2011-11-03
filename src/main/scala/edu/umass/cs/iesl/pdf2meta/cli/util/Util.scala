package edu.umass.cs.iesl.pdf2meta.cli.util

import collection.{immutable, Seq}
import immutable.Map
import java.io._
import java.net.URL

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

  // stolen from scalate IOUtil

  def loadText(in: InputStream, encoding: String = "UTF-8"): String = new String(loadBytes(in), encoding)

    def loadTextFile(path: File, encoding: String = "UTF-8") = new String(loadBinaryFile(path), encoding)

    def loadBinaryFile(path: File): Array[Byte] = {
      val baos = new ByteArrayOutputStream
      val in = new FileInputStream(path)
      try {
        copy(in, baos)
      } finally {
        in.close
      }

      baos.toByteArray
    }

    def loadBytes(in: InputStream): Array[Byte] = {
      val baos = new ByteArrayOutputStream
      try {
        copy(in, baos)
      } finally {
        in.close
      }
      baos.toByteArray
    }

  def copy(in: File, out: File): Long = {
    out.getParentFile.mkdirs
    copy(new FileInputStream(in), new FileOutputStream(out))
  }

  def copy(file: File, out: OutputStream): Long = copy(new BufferedInputStream(new FileInputStream(file)), out)

  def copy(in: InputStream, file: File): Long = {
    val out = new FileOutputStream(file)
    try {
      copy(in, out)
    } finally {
      out.close
    }
  }

  def copy(url: URL, file: File): Long = {
    val in = url.openStream
    try {
      copy(in, file)
    } finally {
      in.close
    }
  }

  def copy(in: InputStream, out: OutputStream): Long = {
    var bytesCopied: Long = 0
    val buffer = new Array[Byte](8192)

    var bytes = in.read(buffer)
    while (bytes >= 0) {
      out.write(buffer, 0, bytes)
      bytesCopied += bytes
      bytes = in.read(buffer)
    }

    bytesCopied
  }


  def copy(in: Reader, out: Writer): Long = {
    var charsCopied: Long = 0
    val buffer = new Array[Char](8192)

    var chars = in.read(buffer)
    while (chars >= 0) {
      out.write(buffer, 0, chars)
      charsCopied += chars
      chars = in.read(buffer)
    }

    charsCopied
  }


  }


//http://michid.wordpress.com/2009/02/23/function_mem/
class Memoize1[-T, +R](f: T => R) extends (T => R)
  {

  import scala.collection.mutable

  private[this] val vals = mutable.Map.empty[T, R]

  def apply(x: T): R =
    {
    if (vals.contains(x))
      {
      vals(x)
      }
    else
      {
      val y = f(x)
      vals += ((x, y))
      y
      }
    }
  }

object Memoize1
  {
  def apply[T, R](f: T => R) = new Memoize1(f)
  def Y[T, R](f: (T, T => R) => R) =
    {
    var yf: T => R = null
    yf = Memoize1(f(_, yf(_)))
    yf
    }
  }

/*
case class Memoize1[-T, +R](f: T => R) extends Function1[T, R]
  {

  import scala.collection.mutable

  private[this] val vals = mutable.Map.empty[T, R]

  def apply(x: T): R = vals.getOrElseUpdate(x, f(x))
  }

object RecursiveMemoizedFunction
  {
  def apply[T, R](fRec: (T, T => R) => R): (T => R) =
    {
    def f(n: T): R = fRec(n, n => f(n))
    Memoize1(f)
    }
  }
*/

