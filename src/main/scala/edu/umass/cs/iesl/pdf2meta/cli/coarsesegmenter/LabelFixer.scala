package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

class LabelFixer(labels: Seq[String]) extends Function1[Seq[ClassifiedRectangle], (Map[ClassifiedRectangle, String], Seq[ClassifiedRectangle])]
  {
  def apply(v1: Seq[ClassifiedRectangle]): (Map[ClassifiedRectangle, String], Seq[ClassifiedRectangle]) =
    {
    val fixed = new scala.collection.mutable.HashMap[ClassifiedRectangle, String]
    val remainder = new scala.collection.mutable.ArrayBuffer[ClassifiedRectangle]
    for (c : ClassifiedRectangle <- v1)
      {
      val label: Option[String] = c.labelWeights.unambiguousBest(0.9)
      // todo miserable hack because I don't get Scala imperative control flow
      label.map(x =>
                  {
                  if (labels.contains(x))
                    {
                    fixed += (c -> x)
                    1
                    }
                  else
                    {remainder += c; 1}
                  }).getOrElse({remainder += c; 1})
      }
    (fixed.toMap, remainder.toSeq)
    }
  }
