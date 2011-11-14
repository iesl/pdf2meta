package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode

/**
 * This transforms an entire document.  We describe this separately from preOrderApply and postOrderApply, because given DocTransformer may need to choose one or the other,
 * or something else entirely.
 */
trait DocTransformer extends ((DocNode) => DocNode)

trait PreOrderDocTransformer extends DocTransformer
  {

   def applyLocalOnly(node : DocNode) : Option[DocNode]

// generally, don't override apply.
   def apply(v1: DocNode) = v1.preOrderApply(applyLocalOnly).get
  }


trait PostOrderDocTransformer extends DocTransformer
  {
   def applyLocalOnly(node : DocNode) : Option[DocNode]


// generally, don't override apply.
   def apply(v1: DocNode) = v1.postOrderApply(applyLocalOnly).get
  }

object PruneException extends Exception

//Function1[Page, Page]
/*trait DocTransformerComponent
  {
  // because this component has no dependencies, we don't need a self type here, and the service trait needn't be
  // inner.
  val docTransformer: DocTransformer
  }*/
