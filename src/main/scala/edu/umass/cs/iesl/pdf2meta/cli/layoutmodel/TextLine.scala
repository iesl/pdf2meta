package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

import collection.Seq

/**
 * A line of text after joining consecutive glyphs, inferring spaces, etc.
 * @param id
 * @param theText
 */
abstract class TextLine(override val id: String, val theText: String, override val rectangle : Option[RectangleOnPage]) extends LeafNode(id, None, None,rectangle)
	{
	override def mkString(d: String) = theText

	override def toString = text

	//override val secretChildren;
	}

/**
 * Typically a single glyph
 *
 * @param id
 * @param theText
 */
abstract class TextAtom(override val id: String, val theText: String, override val rectangle : Option[RectangleOnPage]) extends LeafNode(id, None, None,rectangle)
	{
	override def mkString(d: String) = theText

	override def toString = text

	val charWidths: Array[Float]
	}







