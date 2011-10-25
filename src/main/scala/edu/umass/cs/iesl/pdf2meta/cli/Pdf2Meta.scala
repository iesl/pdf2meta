package edu.umass.cs.iesl.pdf2meta.cli

import coarsesegmenter.{CoarseSegmenterComponent, WiredPerceptronComponent}
import extract.{XmlExtractorComponent, PdfMiner}
import pagetransform._
import readingorder.RectangularReadingOrder
import util.Workspace
import tools.nsc.io.File
import tools.nsc.io.JFile

object Pdf2Meta
  {

  def main(args: Array[String])
    {

    val infilename = args(0);

    val w = new Workspace(infilename, new File(new JFile(infilename)).inputStream())

    val m = WiredPipeline.pipeline(w)
    }
  }

object WiredPipeline extends PipelineComponent with XmlExtractorComponent with
                             CoarseSegmenterComponent with  DocTransformerComponent
  {
  val xmlExtractor = new PdfMiner

  val docTransformer = new DocTransformerComponent
    {
    val docTransformer = new DocTransformerPipelineComponent
      {
       val transformers = List(new PageStarDocPartitioner,
                              new SlicingDocPartitioner,
                              new DocDeepSorter(RectangularReadingOrder)
                              ,new StarDocPartitioner
                              ,new LineMerger
                              ,new ParagraphMerger
                             )
      val docTransformer = new DocTransformerPipeline
      }.docTransformer
    }.docTransformer

  val coarseSegmenter = new WiredPerceptronComponent
  val pipeline = new Pipeline;
  }
