package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.pdf2meta.cli.util.{WeightedSet, MutableWeightedSet}

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode
import collection.mutable.HashMap

trait PerceptronCoarseSegmenterComponent extends CoarseSegmenter with Logging
  {
  val scoringModel: ScoringModel
  val scoringFunctions = scoringModel.scoringFunctions
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

      val mutableScores = new HashMap[DocNode, MutableWeightedSet[String]]
        {
        override def apply(key: DocNode) = getOrElseUpdate(key, default(key))
        override def default(key: DocNode) = MutableWeightedSet[String]()
        }

      val features = scoringFunctions.map(_.requiresFeatures).flatten.toSet

      // assign node-local features.  Note the text boxes may be in a hierarchy, but we don't take that into account; we just classify the "atomic" ones

      for (box <- doc.allLeaves; feat <- features)
        {

        val featuresPerBox: MutableWeightedSet[Feature] = featureMap(box)
        feat match
        {
          case f: ContextFeature =>
            {
            featuresPerBox.incrementBy(feat, f(doc)(box))
            }
          case f: LocalFeature =>
            {
           featuresPerBox.incrementBy(feat, f(box))
            }
        }
        }

      // could propagate up the hierarchy?
      // could propagate neighbor effects here?
      // how to do positional / ordering effects?
      // this is why we have to classify all boxes at once instead of per page
      // => nope, postpone all that to alignment

      for (box <- doc.allLeaves) // classify page.allNodes??
        {
        val featuresPerBox: MutableWeightedSet[Feature] = featureMap(box)
        if (featuresPerBox.asMap.isEmpty)
          {
          mutableScores(box).incrementBy("discard", 10)
          }
        else
          {
          for (sc <- scoringFunctions)
            {
            mutableScores(box).incrementBy(sc.name, sc(featuresPerBox))
            }
          }
        }

      val scores = mutableScores.mapValues[WeightedSet[String]](_.normalized)
      val boxes: ClassifiedRectangles =
        {
        def scoreBox(b: DocNode): ClassifiedRectangle = ClassifiedRectangle(b, featureMap(b), scores(b), None)
        new ClassifiedRectangles(doc.allLeaves.map(scoreBox))
        }


      boxes
      }
    classifiedBoxes
    }
  }

