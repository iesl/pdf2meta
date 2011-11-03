package edu.umass.cs.iesl.pdf2meta.cli.config

import edu.umass.cs.iesl.pdf2meta.cli.extract.PdfMiner
import edu.umass.cs.iesl.pdf2meta.cli.readingorder.RectangularReadingOrder
import edu.umass.cs.iesl.pdf2meta.cli.pagetransform._
import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter.{PerceptronCoarseSegmenterComponent, AlignedPerceptronCoarseSegmenterComponent}
import edu.umass.cs.iesl.pdf2meta.cli.{ExtractOnlyPipelineComponent, PipelineComponent}
import edu.umass.cs.iesl.pdf2meta.cli.segmentsmoother.BestCoarseLabelModelAligner

object WiredPipeline extends PipelineComponent //with CoarseSegmenterComponent//with XmlExtractorComponent with
//CoarseSegmenterComponent //with  DocTransformerComponent
  {
  val xmlExtractor = new PdfMiner

  //val docTransformer = new DocTransformerComponent
  //{
  val docTransformer = new DocTransformerPipelineComponent
    {
    lazy val transformers = List(new PageStarDocPartitioner, new SlicingDocPartitioner, new DocDeepSorter(RectangularReadingOrder), new StarDocPartitioner, new LineMerger, new ParagraphMerger,
                                 new EmptyEndNodeAdder)
    //val docTransformer = new DocTransformerPipeline
    } //.docTransformer
  //}.docTransformer
  val coarseSegmenter = new AlignedPerceptronCoarseSegmenterComponent
    {
    lazy val perceptronPhase = new PerceptronCoarseSegmenterComponent
      {
      lazy val scoringModel = StandardScoringModel
      }
    lazy val segmentSmoother = new BestCoarseLabelModelAligner
      {
      val coarseLabelModels = List(new StandardCoarseLabelModel) //, new LetterCoarseLabelModel)
      }
    }
  val pipeline = new Pipeline;
  }

object WiredExtractOnlyPipeline extends ExtractOnlyPipelineComponent //with XmlExtractorComponent //with DocTransformerComponent
  {
  val xmlExtractor = new PdfMiner

  //val docTransformer = new DocTransformerComponent
  //  {
  val docTransformer = new DocTransformerPipelineComponent
    {
    val transformers = List(new PageStarDocPartitioner, new SlicingDocPartitioner, new DocDeepSorter(RectangularReadingOrder), new StarDocPartitioner, new LineMerger, new ParagraphMerger)
    //val docTransformer = new DocTransformerPipeline
    } //.docTransformer
  //   }.docTransformer
  val pipeline = new Pipeline;
  }













