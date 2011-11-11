package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode

class EmptyEndNodeAdder extends DocTransformer
  {
def apply(v1: DocNode) =
  {
  if (v1.spanningAtoms.last.text.isEmpty) v1
  else
    {
    v1 :+ new DocNode("End", Nil, None, None, true,false)
    }
  }
  }
