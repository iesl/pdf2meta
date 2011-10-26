package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import collection.mutable.HashMap
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.pdf2meta.cli.util.{WeightedSet, MutableWeightedSet}


import CoarseSegmenterTypes._
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode

class WiredPerceptronComponent
        extends CoarseSegmenter with PerceptronCoarseSegmenterComponent with ScoringModelComponent
  {

  val scoringFunctions: List[ScoringFunction] = ScoringModel.scoringFunctions

  val perceptronCoarseSegmenter = new PerceptronCoarseSegmenter

  def apply(pages: DocNode) = perceptronCoarseSegmenter.apply(pages)
  }

trait PerceptronCoarseSegmenterComponent
  {
  this: ScoringModelComponent =>

  val perceptronCoarseSegmenter: PerceptronCoarseSegmenter

  class PerceptronCoarseSegmenter extends Logging
    {
    def apply(doc: DocNode): ClassifiedRectangles =
      {
      // allow classifying all the boxes, not just the textboxes, so that e.g. an HR can be a "DocBreak"
      lazy val classifiedBoxes: ClassifiedRectangles =
        {
        val featureMap = new HashMap[DocNode, MutableWeightedSet[Feature]]
          {
          override def apply(key: DocNode) = getOrElseUpdate(key, default(key))
          override def default(key: DocNode) = MutableWeightedSet[Feature]()
          }

        val scores = new HashMap[DocNode, MutableWeightedSet[String]]
          {
          override def apply(key: DocNode) = getOrElseUpdate(key, default(key))
          override def default(key: DocNode) = MutableWeightedSet[String]()
          }

        val features = scoringFunctions.map(_.requiresFeatures).flatten.toSet

        // assign node-local features.  Note the text boxes are in a hierarchy, but we don't take that into account
        // yet.

        for (page <- doc.children; box <- page.allAtoms; feat <- features)
          {
          box.text match
          {
            case "" =>
            case x =>

              val featuresPerBox: MutableWeightedSet[Feature] = featureMap(box)
              feat match
              {
                case f: ContextFeature =>
                  {
                  //logger.debug("featureMap(" + box + ").incrementBy(" + feat + ", " + f(doc, box) + ")")
                  featuresPerBox.incrementBy(feat, f(doc, box))
                  }
                case f: LocalFeature =>
                  {
                  //logger.debug("featureMap(" + box + ").incrementBy(" + feat + ", " + f(box) + ")")
                  featuresPerBox.incrementBy(feat, f(box))
                  }
              }
          }
          }

        // could propagate up the hierarchy?
        // could propagate neighbor effects here?
        // how to do positional / ordering effects?
        // this is why we have to classify all boxes at once instead of per page

        for (page <- doc.children; box <- page.allAtoms; sc <- scoringFunctions) // classify page.allNodes??
          {
          val featuresPerBox: MutableWeightedSet[Feature] = featureMap(box)
          //logger.debug("scores(" + box.toString + ").incrementBy(" + sc.name + ", " + sc(featuresPerBox) + ")")
          if (featuresPerBox.asMap.isEmpty)
            {
            scores(box).incrementBy("discard", 10)
            }
          else
            scores(box).incrementBy(sc.name, sc(featuresPerBox))
          }

        /*val boxes: ClassifiedRectangles =
          {
          def scoreBox(b: DocNode): Seq[ClassifiedRectangle] =
            {
            logger.debug(b.toString + ": " + scores(b))
            val best: Option[String] = scores(b).unambiguousBest(.9)
            best match
            {
              case None => (for (c <- b.children) yield (scoreBox(c))).flatten
              case Some(x: String) => List((b, x, featureMap(b), scores(b)))
            }
            }

          new ClassifiedRectangles(doc.children.map(scoreBox).flatten)
          }*/
        val boxes: ClassifiedRectangles =
          {
          def scoreBox(b: DocNode): ClassifiedRectangle =
            {
            //logger.debug(b.toString + ": " + scores(b))
            val best: Option[String] = scores(b).unambiguousBest(.9)
            best match
            {
              case None => (b, "[ambiguous]", featureMap(b), scores(b))
              case Some(x: String) => (b, x, featureMap(b), scores(b))
            }
            }

          new ClassifiedRectangles(doc.allAtoms.map(scoreBox))
          }


        boxes
        }

      //def onPage(i: Int) = classifiedBoxes.get(i);

      classifiedBoxes //.values.toMetadataModel
      }
    }

  }


object ScoringFunction
  {
  def apply(name: String, features: Tuple2[Feature, Double]*) =
    {
    new ScoringFunction(name)
      {
      def featureCoefficients = WeightedSet[Feature](features.toList) //.normalized
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

object LocalFeature extends Logging
  {
  def apply(name: String, f: (DocNode) => Double) =
    {
    new LocalFeature(name)
      {
      def apply(box: DocNode): Double =
        {
        //logger.debug("Applying " + f + " to " + box)
        f(box)
        }
      }
    }
  }

/*
object TextLocalFeature extends Logging
  {
  def apply(name: String, f: (TextBox) => Double) =
    {
    new LocalFeature(name)
      {
      def apply(box: DocNode): Double =
        {
        box match
        {
          case b: TextBox =>
            {
            // logger.debug("Applying " + f + " to " + box)
            f(b)
            }
          case _ => 0
        }
        }
      }
    }
  }
*/
object ContextFeature extends Logging
  {
  def apply(name: String, f: (DocNode, DocNode) => Double) =
    {
    new ContextFeature(name)
      {
      def apply(doc: DocNode, box: DocNode): Double =
        {
        //logger.debug("Applying " + f + " to " + box)
        f(doc, box)
        }
      }
    }
  }

/*

object TextContextFeature extends Logging
  {
  def apply(name: String, f: (DocNode, DocNode) => Double) =
    {
    new ContextFeature(name)
      {
      def apply(doc: DocNode, box: DocNode): Double =
        {
        box match
        {
          case b: TextBox =>
            {
            // logger.debug("Applying " + f + " to " + box)
            f(doc, b)
            }
          case _ => 0
        }
        }
      }
    }
  }
*/
abstract class Feature(val name: String)
  {
  override def toString = name

  override def equals(p1: Any) =
    {
    p1 match
    {
      case x: Feature => name == x.name
      case _ => false
    }
    }
  override def hashCode() = name.hashCode
  }

abstract class LocalFeature(override val name: String) extends Feature(name)
  {
  def apply(box: DocNode): Double
  }


abstract class ContextFeature(override val name: String) extends Feature(name)
  {
  def apply(doc: DocNode, box: DocNode): Double
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
