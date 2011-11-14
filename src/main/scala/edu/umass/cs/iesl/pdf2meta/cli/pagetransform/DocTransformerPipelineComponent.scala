package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode
import com.weiglewilczek.slf4s.Logging


trait DocTransformerPipelineComponent extends DocTransformer with Logging //extends DocTransformerComponent
  {
  val transformers: List[DocTransformer]

  def apply(rect: DocNode): DocNode =
    {
    val result = transformers.foldLeft(rect)((r: DocNode, f: DocTransformer) =>
                                               {
                                               logger.debug("applying " + f.getClass + " to " + r)
                                               val x = f(r)
                                               logger.debug(f.getClass + " produced: \n " + x.printTree(""))
                                               x
                                               })
    result
    }
  }


