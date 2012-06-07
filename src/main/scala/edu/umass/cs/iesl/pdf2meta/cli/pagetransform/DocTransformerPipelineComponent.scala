package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode
import com.weiglewilczek.slf4s.Logging
import java.util.Date

trait DocTransformerPipelineComponent extends DocTransformer with Logging //extends DocTransformerComponent
  {
  val transformers: List[DocTransformer]

  def apply(rect: DocNode): DocNode =
    {
    val result = transformers.foldLeft(rect)((r: DocNode, f: DocTransformer) =>
                                               {
                                               val startTime = new Date
                                               logger.debug("applying " + f.getClass + " to " + r)
                                               val x = f(r)

                                               val doneTime = new Date()

                                               logger.debug(f.getClass + " took " + ((doneTime.getTime - startTime.getTime)) + " milliseconds")

                                               logger.debug(f.getClass + " produced: \n " + x.printTree(""))
                                               x
                                               })
    result
    }
  }


