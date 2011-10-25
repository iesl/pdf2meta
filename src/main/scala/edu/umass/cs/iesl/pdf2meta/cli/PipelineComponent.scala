package edu.umass.cs.iesl.pdf2meta.cli

import coarsesegmenter.{ClassifiedRectangles, CoarseSegmenterComponent}
import extract.XmlExtractorComponent
import layoutmodel.DocNode
import metadatamodel.MetadataModel
import pagetransform.{DocTransformerComponent, DocTransformer}
import util.Workspace

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

trait WebPipelineComponent
  {
  this: XmlExtractorComponent with
          CoarseSegmenterComponent =>

  val pipeline: Pipeline
  val docTransformer: DocTransformer

  class Pipeline extends Function1[Workspace, (DocNode, ClassifiedRectangles)]
    {
    def apply(w: Workspace): (DocNode,  ClassifiedRectangles) =
      {
      val doc = xmlExtractor(w)
      val regrouped = docTransformer(doc)

      val segments: ClassifiedRectangles = coarseSegmenter(regrouped)

      (regrouped, segments)
      }
    }

  }
