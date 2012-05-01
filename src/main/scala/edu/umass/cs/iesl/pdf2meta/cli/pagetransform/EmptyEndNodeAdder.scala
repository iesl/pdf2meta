package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{InternalDocNode, DocNode, LeafNode}

class EmptyEndNodeAdder extends DocTransformer
	{
	def apply(v1: DocNode) =
		{
		if (v1.leaves.last.text.isEmpty) v1
		else
			{
			InternalDocNode(v1.id, v1.children :+ new LeafNode("End", None, None, None), v1.info, v1.errors)
			//v1 :+ new LeafNode("End", None, None, None)
			}
		}
	}
