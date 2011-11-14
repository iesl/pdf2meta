package edu.umass.cs.iesl.pdf2meta.cli.config

import edu.umass.cs.iesl.pdf2meta.cli.extract.PdfMiner
import edu.umass.cs.iesl.pdf2meta.cli.readingorder.RectangularReadingOrder
import edu.umass.cs.iesl.pdf2meta.cli.pagetransform._
import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter.{PerceptronCoarseSegmenterComponent, AlignedPerceptronCoarseSegmenterComponent}
import edu.umass.cs.iesl.pdf2meta.cli.{ExtractOnlyPipelineComponent, PipelineComponent}
import edu.umass.cs.iesl.pdf2meta.cli.segmentsmoother.BestCoarseLabelModelAligner

object WiredPipeline extends PipelineComponent
  {
  val xmlExtractor = new PdfMiner

  val docTransformer = new DocTransformerPipelineComponent
    {
    val transformers = List(new PageHonoringDocFlattener
                            // top-down phase
                            , new SlicingDocPartitioner, new WeakPartitionRemover, new DocDeepSorter(RectangularReadingOrder)

                            // bottom-up phase
                            , new LineMerger, new SidewaysLineMerger, new IndentedParagraphsMerger, new EmptyEndNodeAdder

                            // finally ditch any intermediate hierarchy levels
                            , new DocFlattener)
    }

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

object WiredExtractOnlyPipeline extends ExtractOnlyPipelineComponent
  {
  val xmlExtractor = new PdfMiner
  val docTransformer = new DocTransformerPipelineComponent
    {
    val transformers = List(new PageHonoringDocFlattener
                            // top-down phase
                            , new SlicingDocPartitioner, new WeakPartitionRemover, new DocDeepSorter(RectangularReadingOrder)

                            // bottom-up phase
                            , new LineMerger, new SidewaysLineMerger, new IndentedParagraphsMerger, new EmptyEndNodeAdder

                            // finally ditch any intermediate hierarchy levels
                            , new DocFlattener)
    }
  val pipeline = new Pipeline;
  }













