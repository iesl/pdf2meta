package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode


trait DocTransformerPipelineComponent extends DocTransformerComponent
  {

  val transformers : List[DocTransformer]

  class DocTransformerPipeline extends DocTransformer
    {
    def apply(rect: DocNode): DocNode =
      {
      val result = transformers.foldLeft(rect)((r: DocNode, f: DocTransformer) => f(r))
      result
      }
    }
  }


