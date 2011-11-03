package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode

trait DocTransformer extends ((DocNode) => DocNode)
//Function1[Page, Page]

/*trait DocTransformerComponent
  {
  // because this component has no dependencies, we don't need a self type here, and the service trait needn't be
  // inner.
  val docTransformer: DocTransformer
  }*/
