package edu.umass.cs.iesl.pdf2meta.cli

import coarsesegmenter.{CoarseSegmenter, ClassifiedRectangles}
import extract.XmlExtractor
import layoutmodel.DocNode
import metadatamodel.MetadataModel
import pagetransform.DocTransformer
import java.util.Date
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.scalacommons.Workspace

trait PipelineComponent extends ((Workspace) => MetadataModel)
  {
  // when dependencies have no further dependencies, we can just ask for them directly instead of making an extra "component" layer
  val xmlExtractor: XmlExtractor
  val docTransformer: DocTransformer
  val coarseSegmenter: CoarseSegmenter

  // this component provides a dependency that may be used by others
  val pipeline: Pipeline

  // one way of satisfying that dependency (in fact, the only way since "Pipeline" is not an independent interface)
  // is to instantiate this inner class in the mixed class
  // the trick is this can refer to members of the other components directly (e.g., docTransformer) because they're in the same namespace after mixing.
  class Pipeline extends ((Workspace) => MetadataModel)
    {
    def apply(w: Workspace): MetadataModel =
      {
      val doc = xmlExtractor(w)
      val regrouped = docTransformer(doc)
      val segments: ClassifiedRectangles = coarseSegmenter(regrouped)
      segments
      }
    }

  def apply(w: Workspace): MetadataModel = pipeline.apply(w)
  }

trait ExtractOnlyPipelineComponent extends ((Workspace) => DocNode)
  {
  val xmlExtractor: XmlExtractor
  val docTransformer: DocTransformer

  val pipeline: Pipeline

  class Pipeline extends ((Workspace) => DocNode)
    {
    def apply(w: Workspace): DocNode =
      {
      val doc = xmlExtractor(w)
      val regrouped = docTransformer(doc)
      regrouped
      }
    }

  def apply(w: Workspace): DocNode = pipeline.apply(w)
  }

trait WebPipelineComponent extends ((Workspace) => (DocNode, ClassifiedRectangles)) with Logging
  {
  val xmlExtractor: XmlExtractor
  val docTransformer: DocTransformer
  val coarseSegmenter: CoarseSegmenter

  val pipeline: Pipeline

  class Pipeline extends Function1[Workspace, (DocNode, ClassifiedRectangles)]
    {
    def apply(w: Workspace): (DocNode, ClassifiedRectangles) =
      {
      logger.debug("Starting XML extraction...")
      val startTime = new Date
      logger.debug("Running XML extraction...")

      val doc = xmlExtractor(w)

      logger.debug("XML extraction done ")
      val extractTime = new Date()

      logger.debug("XML extraction took " + ((extractTime.getTime - startTime.getTime)) + " milliseconds")
      val regrouped = docTransformer(doc)

      logger.debug("XML extraction done ")
      val regroupTime = new Date()

      logger.debug("Regrouping took " + ((regroupTime.getTime - extractTime.getTime)) + " milliseconds")

      val segments: ClassifiedRectangles = coarseSegmenter(regrouped)


      logger.debug("Regrouping done ")
      val labellingTime = new Date()

      logger.debug("Labelling took " + ((labellingTime.getTime - regroupTime.getTime)) + " milliseconds")

      (regrouped, segments)
      }
    }

  def apply(w: Workspace): (DocNode, ClassifiedRectangles) = pipeline.apply(w)
  }
