package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

class LabelTransformer(labelMap: Map[String, String]) extends Function1[Seq[ClassifiedRectangle], ClassifiedRectangles]
  {
  def apply(v1: Seq[ClassifiedRectangle]) =
    {
    val newRaw = for (c <- v1) yield
      {
      val newWeights = c.labelWeights.mapLabels(labelMap)
      ClassifiedRectangle(c.node, c.featureWeights, newWeights, Some(c),List())
      }
    new ClassifiedRectangles(newRaw)
    }
  }


