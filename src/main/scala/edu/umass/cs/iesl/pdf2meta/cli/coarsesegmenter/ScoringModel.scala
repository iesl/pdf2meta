package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import edu.umass.cs.iesl.scalacommons.collections.WeightedSet

trait ScoringModel
  {
  def scoringFunctions: List[ScoringFunction]
  }


object ScoringFunction
  {
  def apply(name: String, features: Tuple2[Feature, Double]*) =
    {
    new ScoringFunction(name) { def featureCoefficients = WeightedSet[Feature](features) }
    }
  }

abstract class ScoringFunction(val name: String) extends Logging
  {
  override def toString() = name
  // a linear combination of features
  def featureCoefficients: WeightedSet[Feature]
  def requiresFeatures = featureCoefficients.asMap.keys
  def apply(weights: WeightedSet[Feature]): Double =
    {
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
        f(box)
        }
      }
    }
  }

object ContextFeature extends Logging
  {
  def apply(name: String, f: (DocNode, DocNode) => Double) =
    {
    new ContextFeature(name)
      {
      def apply(context: DocNode) =
        {
        new LocalFeature(name)
          {
          def apply(box: DocNode) = f(context, box)
          }
        }
      }
    }
  }

abstract class Feature(val name: String)
  {
  override def toString() = name

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

abstract class LocalFeature(override val name: String) extends Feature(name) with Function1[DocNode, Double]
  {
  outer =>

  override def toString() = name
  def apply(box: DocNode): Double

  def multiply(multName: String, that: LocalFeature) =
    {
    new LocalFeature(multName)
      {
      def apply(box: DocNode) = outer(box) * that(box)
      }
    }
  }


abstract class ContextFeature(override val name: String) extends Feature(name) with Function1[DocNode, LocalFeature]
  {
  override def toString() = name
  def apply(context: DocNode): LocalFeature
  }

class CompoundFeature(override val name: String, val features: Map[Feature, Double]) extends ContextFeature(name) // this must be a ContextFeature since it may contain ContextFeatures
  {
  def leafFeatures: Map[Feature, Double] =
    {
    features.flatMap({
                     case (x: CompoundFeature, d: Double) => x.leafFeatures.map({case (x2: Feature, d2: Double) => (x2, d * d2)})
                     case (x: Feature, d: Double) => List((x, d))
                     })
    }

  def apply(context: DocNode) =
    {
    new LocalFeature(name)
      {
      def apply(box: DocNode) =
        {
        leafFeatures.map({
                         case ((f: ContextFeature, d: Double)) => (f(context)(box) * d)
                         case ((f: LocalFeature, d: Double)) => (f(box) * d)
                         }).sum
        }
      }
    }
  }

class FunctionFeature(override val name: String, val baseFeature: Feature, val f: (Double => Double)) extends ContextFeature(name)
// this must be a ContextFeature since it may contain ContextFeatures
  {
  def apply(context: DocNode) =
    {
    new LocalFeature(name)
      {
      def apply(box: DocNode): Double =
        {
        baseFeature match
        {
          case base: ContextFeature => f(base(context)(box))
          case base: LocalFeature => f(base(box))
        }
        }
      }
    }
  }
