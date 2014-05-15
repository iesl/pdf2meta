package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import edu.umass.cs.iesl.scalacommons.collections.WeightedSet
import scala.Tuple2
import scala.Predef._

trait ScoringModel
  {
  def scoringFunctions: List[ScoringFunction]
  }


object ScoringFunction
  {
  def apply(name: String, features: Tuple2[Feature, Float]*) =
    {
    new ScoringFunction(name) { def featureCoefficients = WeightedSet.fromFloat(features) }
    }
  }

abstract class ScoringFunction(val name: String) extends Logging
  {
  override def toString() = name
  // a linear combination of features
  def featureCoefficients: WeightedSet[Feature]
  def requiresFeatures = featureCoefficients.asMap.keys
  def apply(weights: WeightedSet[Feature]): Float =
    {
    val dotProduct: ((Feature, Double)) => Double = (t: ((Feature, Double))) => (weights(t._1) * t._2)
    (featureCoefficients.asMap.map(dotProduct)).sum.toFloat
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

object LocalFeature extends Logging
	{
	def apply[T <: DocNode](name: String, f: (T) => Float) =
		{
		new LocalFeature[T](name)
			{
			def apply(box: T): Float =
				{
				f(box)
				}
			}
		}
	}

/**
 * A feature of a node that can be computed with no external context
 * @param name
 */
abstract class LocalFeature[T <: DocNode](override val name: String) extends Feature(name) with (T => Float)
  {
  outer =>

  override def toString() = name
  def apply(box: T): Float

  def multiply(multName: String, that: LocalFeature[T]) =
    {
    new LocalFeature[T](multName)
      {
      def apply(box: T) = outer(box) * that(box)
      }
    }
  }


object ContextFeature extends Logging
	{
	def apply[T <: DocNode](name: String, f: (DocNode, T) => Float) =
		{
		new ContextFeature[T](name)
			{
			def apply(context: DocNode) =
				{
				new LocalFeature[T](name)
					{
					def apply(box: T) = f(context, box)
					}
				}
			}
		}
	}

/**
 * A feature of a node that depends on some context, represented by a containing node
 */
abstract class ContextFeature[T <: DocNode](override val name: String) extends Feature(name) with (DocNode => LocalFeature[T])
  {
  override def toString() = name
  def apply(context: DocNode): LocalFeature[T]
  }

class CompoundFeature[T <: DocNode](override val name: String, val features: Map[Feature, Float]) extends ContextFeature[T](name) // this must be a
// ContextFeature since it may contain ContextFeatures
  {
  def leafFeatures: Map[Feature, Float] =
    {
    features.flatMap({
                     case (x: CompoundFeature[T], d: Float) => x.leafFeatures.map({case (x2: Feature, d2: Float) => (x2, d * d2)})
                     case (x: Feature, d: Float) => List((x, d))
                     })
    }

  def apply(context: DocNode) =
    {
    new LocalFeature[T](name)
      {
      def apply(box: T) =
        {
        leafFeatures.map({
                         case ((f: ContextFeature[T], d: Float)) => (f(context)(box) * d)
                         case ((f: LocalFeature[T], d: Float)) => (f(box) * d)
                         }).sum
        }
      }
    }
  }

class FunctionFeature[T <: DocNode](override val name: String, val baseFeature: Feature, val f: (Float => Float)) extends ContextFeature[T](name)
// this must be a ContextFeature since it may contain ContextFeatures
  {
  def apply(context: DocNode) =
    {
    new LocalFeature[T](name)
      {
      def apply(box: T): Float =
        {
        baseFeature match
        {
          case base: ContextFeature[T] => f(base(context)(box))
          case base: LocalFeature[T] => f(base(box))
        }
        }
      }
    }
  }
