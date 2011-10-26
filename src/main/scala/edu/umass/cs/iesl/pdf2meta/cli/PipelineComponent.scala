package edu.umass.cs.iesl.pdf2meta.cli

import coarsesegmenter.{ClassifiedRectangles, CoarseSegmenterComponent}
import extract.XmlExtractorComponent
import layoutmodel.DocNode
import metadatamodel.MetadataModel
import pagetransform.{DocTransformerComponent, DocTransformer}
import util.Workspace
import java.util.Date
import com.weiglewilczek.slf4s.Logging

trait PipelineComponent
  {
  this: XmlExtractorComponent with
          CoarseSegmenterComponent with DocTransformerComponent =>

  val pipeline: Pipeline

  class Pipeline extends Function1[Workspace, MetadataModel]
    {
    def apply(w: Workspace): MetadataModel =
      {
      val doc = xmlExtractor(w)
      val regrouped = docTransformer(doc)
      val segments: ClassifiedRectangles = coarseSegmenter(regrouped)
      segments.toMetadataModel
      }
    }

  }

trait WebPipelineComponent extends Logging
  {
  this: XmlExtractorComponent with
          CoarseSegmenterComponent =>

  val pipeline: Pipeline
  val docTransformer: DocTransformer

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

  }
