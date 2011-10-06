package edu.umass.cs.iesl.pdf2meta.cli.eval

import collection.mutable.HashMap
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.pdf2meta.cli.layout.{LayoutRectangle, Page, TextBox}
object CoarseSegmenterTypes
  {
  type ClassifiedRectangle = (LayoutRectangle, String)
  }

import CoarseSegmenterTypes._

class ClassifiedRectangles(val raw: Seq[ClassifiedRectangle])
  {

  def legit = raw.filter(_._2 != "discard")
  def discarded = raw.filter(_._2 == "discard")

  //def removeRedundant(a:ClassifiedRectangle, b:ClassifiedRectangle) : ClassifiedRectangle = {}
  def legitNonRedundant(current: String, l: List[ClassifiedRectangle]): List[ClassifiedRectangle] =
    {
    l match
    {
      case (x, y) :: t if (y == current) => (x, "") :: legitNonRedundant(current, t)
      case (x, y) :: t => (x, y) :: legitNonRedundant(y, t)
      case Nil => Nil
    }
    }

  def legitNonRedundant : List[ClassifiedRectangle]= legitNonRedundant("Bogus", legit.toList)
  }

class CoarseSegmenter(pages: Seq[Page], scoringFunctions: Seq[ScoringFunction]) extends Logging
  {

  // allow classifying all the boxes, not just the textboxes, so that e.g. an HR can be a "DocBreak"
  lazy val classifiedBoxes: Map[Int, ClassifiedRectangles] =
    {
    val featureMap = new HashMap[LayoutRectangle, MutableWeightedSet[Feature]]
      {
      override def apply(key: LayoutRectangle) = getOrElseUpdate(key, default(key))
      override def default(key: LayoutRectangle) = MutableWeightedSet[Feature]()
      }

    val scores = new HashMap[LayoutRectangle, MutableWeightedSet[String]]
      {
      override def apply(key: LayoutRectangle) = getOrElseUpdate(key, default(key))
      override def default(key: LayoutRectangle) = MutableWeightedSet[String]()
      }

    val features = scoringFunctions.map(_.requiresFeatures).flatten.toSet

    // assign node-local features.  Note the text boxes are in a hierarchy, but we don't take that into account yet.

    for (page <- pages; box <- page.allNodes; feat <- features)
      {
      logger.debug("featureMap(" + box + ").incrementBy(" + feat + ", " + feat(page, box) + ")")
      val featuresPerBox: MutableWeightedSet[Feature] = featureMap(box)
      featuresPerBox.incrementBy(feat, feat(page, box))
      }

    // could propagate up the hierarchy?

    // could propagate neighbor effects here?
    // how to do positional / ordering effects?
    // this is why we have to classify all boxes at once instead of per page

    for (page <- pages; box <- page.children; sc <- scoringFunctions)   // classify page.allNodes??
      {
      val featuresPerBox: MutableWeightedSet[Feature] = featureMap(box)
      logger.debug("scores(" + box.toString + ").incrementBy(" + sc.name + ", " + sc(featuresPerBox) + ")")
      scores(box).incrementBy(sc.name, sc(featuresPerBox))
      }

    val boxesPerPage: Seq[(Int, ClassifiedRectangles)] = for (page <- pages) yield
      {
      def scoreBox(b: LayoutRectangle): Option[ClassifiedRectangle] =
        {
        logger.debug(b.toString + ": " + scores(b))
        val best: Option[String] = scores(b).best
        best match
        {
          case None => Some((b, "Unclassified"))
          case Some(x: String) => Some((b, x))
        }
        }

      ((page.pagenumber -> new ClassifiedRectangles(page.allNodes.map(scoreBox).flatten)))
      }

    val m: Map[Int, ClassifiedRectangles] = boxesPerPage.toMap
    m
    }

  def onPage(i: Int) = classifiedBoxes.get(i);
  }

object ScoringFunction
  {
  def apply(name: String, features: Tuple2[Feature, Double]*) =
    {
    new ScoringFunction(name)
      {
      def featureCoefficients = WeightedSet[Feature](features.toList)
      }
    }
  }

abstract class ScoringFunction(val name: String) extends Logging
  {
  // a linear combination of features
  def featureCoefficients: WeightedSet[Feature]
  def requiresFeatures = featureCoefficients.asMap.keys
  def apply(weights: WeightedSet[Feature]): Double =
    {
    //val dotProduct: (Feature, Double) => Double = (f,d) => (weights(f) * d)
    //logger.debug("Computing dot product: " + weights + " vs. " + featureCoefficients)
    val dotProduct: ((Feature, Double)) => Double = (t: ((Feature, Double))) => (weights(t._1) * t._2)
    (featureCoefficients.asMap.map(dotProduct)).sum
    }
  }

object Feature extends Logging
  {
  def apply(name: String, f: (Page, LayoutRectangle) => Double) =
    {
    new Feature(name)
      {
      def apply(page: Page, box: LayoutRectangle): Double =
        {
        //logger.debug("Applying " + f + " to " + box)
        f(page, box)
        }
      }
    }
  }


object TextFeature extends Logging
  {
  def apply(name: String, f: (Page, TextBox) => Double) =
    {
    new Feature(name)
      {
      def apply(page: Page, box: LayoutRectangle): Double =
        {
        box match
        {
          case b: TextBox =>
            {
            // logger.debug("Applying " + f + " to " + box)
            f(page, b)
            }
          case _ => 0
        }
        }
      }
    }
  }


abstract class Feature(val name: String)
  {
  def apply(page: Page, box: LayoutRectangle): Double
  override def toString = name
  }

object WeightedSet
  {
  def apply[T](x: Seq[Tuple2[T, Double]]): WeightedSet[T] =
    {
    new WeightedSet[T]
      {val asMap = Map() ++ x.toMap}
    }
  }

trait WeightedSet[T]
  {
  def asMap: collection.Map[T, Double]
  def apply(v: T) = asMap(v)
  def best: Option[T] =
    {
    if (asMap.isEmpty)
      None
    else
      Some(asMap.max(new
                                     {} with Ordering[Tuple2[T, Double]]
                       {
                       def compare(x: (T, Double), y: (T, Double)) = (x._2.compare(y._2))
                       })._1)
    }
  override def toString = asMap.toString()
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

/*
class HashMapMutableWeightedSet[T] extends MutableWeightedSet[T]
  {
  val asMap = new HashMap[T, Double]
    {
    override def default(key: T) = 0
    }
  def incrementBy(v: T, d: Double)
    {
    val cur: Double = asMap.getOrElse(v, 0) // the else case should never fire because of the default
    asMap.update(v, cur + d)
    }
  }*/
