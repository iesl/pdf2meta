package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.pdf2meta.cli.util.{WeightedSet, MutableWeightedSet}

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode
import collection.mutable.HashMap

trait PerceptronCoarseSegmenterComponent extends CoarseSegmenter with Logging
  {
  //this: ScoringModelComponent =>
  //val scoringFunctions: List[ScoringFunction]
  val scoringModel: ScoringModel
  val scoringFunctions = scoringModel.scoringFunctions
  //val perceptronCoarseSegmenter: PerceptronCoarseSegmenter
  //class PerceptronCoarseSegmenter extends Logging
  //  {
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

      for (box <- doc.allAtoms; feat <- features)
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
                featuresPerBox.incrementBy(feat, f(doc)(box))
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
      for (box <- doc.allAtoms; sc <- scoringFunctions) // classify page.allNodes??
        {
        val featuresPerBox: MutableWeightedSet[Feature] = featureMap(box)
        //logger.debug("scores(" + box.toString + ").incrementBy(" + sc.name + ", " + sc(featuresPerBox) + ")")
        if (featuresPerBox.asMap.isEmpty)
          {
          mutableScores(box).incrementBy("discardY", 10)
          }
        else
          mutableScores(box).incrementBy(sc.name, sc(featuresPerBox))
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
      val scores = mutableScores.mapValues[WeightedSet[String]](_.normalized)
      val boxes: ClassifiedRectangles =
        {
        def scoreBox(b: DocNode): ClassifiedRectangle = ClassifiedRectangle(b, featureMap(b), scores(b), None)
        /*  {
                    //logger.debug(b.toString + ": " + scores(b))
                    best match
                    {
                      case None => ClassifiedRectangle(b, "[ambiguous]", featureMap(b), scores(b))
                      case Some(x: String) => ClassifiedRectangle(b, x, featureMap(b), scores(b))
                    }
                    }*/

        new ClassifiedRectangles(doc.allAtoms.map(scoreBox))
        }


      boxes
      }

    //def onPage(i: Int) = classifiedBoxes.get(i);

    classifiedBoxes //.values.toMetadataModel
    }
  //  }
  }

