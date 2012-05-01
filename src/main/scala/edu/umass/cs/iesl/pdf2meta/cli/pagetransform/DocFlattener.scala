package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{PageNode, InternalDocNode, DocNode}

class DocFlattener extends DocTransformer
	{
	def apply(rect: DocNode): DocNode =
		{
		// just ignore any existing groupings, but maintain order
		// also remove empty nodes
		// rect.leaves.filter(p => !p.text.isEmpty)
		// nooo, we need the delimiter boxes for debugging
		val result = InternalDocNode(rect.id, rect.leaves, rect.localInfo, rect.localErrors)
		result
		}
	}

/**
 * Ignore any existing groupings below the first level
 */
class PageHonoringDocFlattener extends DocTransformer
	{

	def apply(doc: DocNode): DocNode =
		{
		val regroupedPages: Seq[DocNode] = doc.children.map(t => t match
		{
			case page: PageNode => new PageNode(page.id, page.leaves.filter(p => !p.text.isEmpty), page.localInfo, page.localErrors,page.page)
			case page: InternalDocNode => InternalDocNode(page.id, page.leaves.filter(p => !p.text.isEmpty), page.localInfo, page.localErrors)
			case other: DocNode        => other
		})
		val result = doc.create(regroupedPages)
		result
		}
	}
