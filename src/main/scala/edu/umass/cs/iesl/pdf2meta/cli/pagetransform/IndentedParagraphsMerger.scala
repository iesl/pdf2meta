package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import runtime.FractionalProxy
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import scala.Predef._

object IndentedParagraphsMerger
	{
	val slop: Float = 5

	class ApproximateFloat(val self: Float) extends FractionalProxy[Float]
		{
		protected val integralNum = Numeric.FloatAsIfIntegral

		def =~(x: Float): Boolean = floatWrapper(x - self).abs <= slop
		}

	implicit def toApproximateFloat(d: Float): ApproximateFloat =
		{
		new ApproximateFloat(d)
		}
	}

/**
 * Find a hanging-indent situation, as well as combining normal lines into paragraphs and columns.
 *
 * The strategy is to make a list of nodes that could possibly be the start of a references section, because the following node starts either at the same X
 * coordinate or indented from there. Each such node gets stored with the indentation level. Then we find strings of consecutive nodes with the same indent
 * distance (to cover the case that references continue in the next column or on the next page).
 *
 * Note this expects to operate on individual lines, not paragraph groups However the lines may still contain multiple nodes based on LineMerger.
 *
 * We'd like to end up with each reference as an individual node (perhaps with a "hangingindent" annotation), but with these also in larger groups,
 * e.g. columns, for better classification.  OK, so we merge as much as possible and declare the result "leaves"; but these have secret children with
 * annotations that we can pull out later
 *
 * Actually we can't easily tell the difference between hanging indents and normal indents.  Normal paragraphs could be interpreted in the inverse sense as a
 * lot of start lines with only a few continuation lines; and conversely a reference section could be interpreted as a lot of short paragraphs.  OK,
 * so we'll just annotate any indented line for now as "indented"; then later we can interpret these as continuation lines in reference sections.
 */
class IndentedParagraphsMerger extends PostOrderDocTransformer
	{
	val MaxIndent = 20

	import IndentedParagraphsMerger.toApproximateFloat

	// ** bad terminology assumes the hanging-indent case
	case class IndentationModel(firstLineX: Float, continuationLineX: Option[Float])
		{
		def indentDistance: Option[Float] = continuationLineX.map(_ - firstLineX)
		}

	/**
	 * Find runs of blocks with alternating left margins: e.g., columns of lines grouped into paragraphs,
	 * where most lines share the far left margin but the first line of each paragraph is
	 * indented; and similarly for hanging indents often found in reference sections.
	 *
	 * Node blocks are processed from bottom to top.
	 *
	 * @see Util.contiguousRuns
	 */
	private def markIndented(node: DocNode): AnnotationNode = new AnnotationNode(node, Seq("indented"))

	private def markIndented(nodes: List[DocNode]): List[AnnotationNode] = nodes.map(markIndented)

	def contiguousIndentRuns(nodes: List[DocNode]): List[(IndentationModel, List[DocNode])] =
		{
		if (nodes.isEmpty) Nil
		else
			{
			val suffix: List[(IndentationModel, List[DocNode])] = contiguousIndentRuns(nodes.tail)

			val currentNode: DocNode = nodes.head
			val currentLeft = currentNode.rectangle.get.left

			if (suffix.isEmpty)
				{
				// Initialize lisxt with a new active block with just the current block in it
				List((new IndentationModel(currentLeft, None), List(currentNode)))
				}
			else
				{
				val (currentIndent: IndentationModel, activeBlockNodes) = suffix.head
				val nextNode = activeBlockNodes.head // shouldn't be empty
				val currentRect: RectangleOnPage = currentNode.rectangle.get
				val nextRect: RectangleOnPage = nextNode.rectangle.get

				// font information is a mess in references sections -- e.g. italic glyphs can be much taller than
				// roman, etc.
				/*
								val sameFont = (a.dominantFont, nextNode.dominantFont) match
										{
										  case (Some(af), Some(bf)) => af.sizeEqualsWithin(1.0)(bf) // use only
										  font size, not font ID, because references have a lot of italic/bold
										  variation
										  case _                    => false
										}
								*/
				// how about: blocks are mergeable if there is some non-zero font size in common
				val minimalFontAgreement: Boolean =
					{
					val aFontSizes = currentNode.allFonts.map(_._1.height)
					val bFontSizes = nextNode.allFonts.map(_._1.height)

					// note font sizes are already quantized
					!aFontSizes.intersect(bFontSizes).filterNot(_ == 0.0).isEmpty
					}

				if (!minimalFontAgreement || currentRect.page != nextRect.page || !nextRect.isBelow(currentRect.top)) // || !sameFont
					{
					// start a new active block with just the current block in it (no idea whether it's indented or not)
					(IndentationModel(currentLeft, None), List(currentNode)) :: suffix
					}
				else
					{
					// we're potentially in the same column, and there's not a major font mismatch.
					// now base merging on the indentation situation

					currentIndent match
					{
						case IndentationModel(activeLeft: Float, None) => // there is only one left position known so far
							{
							val indentDistance = activeLeft - currentLeft

							indentDistance match
							{
								case d if d > MaxIndent || d < -MaxIndent => // not same column; start a new block
									{
									(IndentationModel(currentLeft, None), List(currentNode)) :: suffix
									}
								case d if d =~ 0                          => // add this line to the current indent block
									{
									(currentIndent, currentNode :: activeBlockNodes) :: suffix.tail
									}
								case d if d > 0                           => // the head is a hanging indent of the tail,
									// or a normal line followed by a set of indented lines that weren't yet identified
									{
									// either way, the current line is unannotated, but the next line should be "indented"
									// note so far all activeBlockNodes had the same left margin
									(IndentationModel(currentLeft, Some(activeLeft)), currentNode :: markIndented(activeBlockNodes)) :: suffix.tail
									}
								case d if d < 0                           => // the head is a normal indent of the tail;
									// or it could be a continuation line where the tail has only start lines
									{
									(IndentationModel(activeLeft, Some(currentLeft)), markIndented(currentNode) :: activeBlockNodes) ::
									suffix.tail
									}
							}
							}
						case IndentationModel(left, Some(right))        => // two left positions are already known
							{
							// if the current node matches either of those,
							// add this line to the current indent block
							if (currentLeft =~ left)
								{
								// don't modify the active nodes; they already know whether they're indented or not
								(currentIndent, currentNode :: activeBlockNodes) :: suffix.tail
								}
							else if (currentLeft =~ right)
								{
								(currentIndent, markIndented(currentNode) :: activeBlockNodes) :: suffix.tail
								}
							else
								{
								// this line is not part of the current block; start a new one
								(IndentationModel(currentLeft, None), List(currentNode)) :: suffix
								}
							}
					}
					}
				}
			}
		}

	def applyLocalOnly(node: DocNode) =
		{
		// the children have already been processed
		val runs = contiguousIndentRuns(node.children.toList)
		val newChildren = for (r <- runs) yield
			{
			r match
			{
				case (im, mergeNodes) if mergeNodes.length == 1 => mergeNodes.head
				case (im, mergeNodes)                           =>
					{
					// declare the merged node a leaf iff all of the children were unannotated leaves
					//val allMergeesAreLeaves: Boolean = !mergeNodes.exists(!_.isClassificationLeaf)

					InternalDocNode((mergeNodes map (_.id)).mkString("+"), mergeNodes, None, None).bounceRetainingChildren()

					}
			}
			}
		Some(node.create(newChildren))
		}
	}

