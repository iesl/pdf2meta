package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode


trait DocTransformerPipelineComponent  extends DocTransformer //extends DocTransformerComponent
  {
  val transformers : List[DocTransformer]

  //val docTransformerPipeline : DocTransformerPipeline

  //class DocTransformerPipeline extends DocTransformer
    //{
    def apply(rect: DocNode): DocNode =
      {
      val result = transformers.foldLeft(rect)((r: DocNode, f: DocTransformer) => f(r))
      result
      }
   // }
  }


