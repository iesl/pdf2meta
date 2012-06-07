package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{BeforeAndAfter, Spec}

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
class RectangleTest extends Spec with ShouldMatchers with BeforeAndAfter
	{

	val x = Rectangle(1, 1, 2, 2)
	val y = Rectangle(2, 1, 3, 2)
	val z = Rectangle(3, 3, 4, 4)

	val q = Rectangle(1, 1, 4, 4)

	describe("A set of nonoverlapping rectangles")
	{
	val s = Set(x, y, z).flatten

	it("is not affected by decomposition")
	{
	Rectangle.decomposeToNonOverlapping(s) should be === s
	}
	}

	describe("A set of overlapping rectangles")
	{
	val s = Set(x, y, z, q).flatten

	val t = Rectangle.decomposeToNonOverlapping(s)
	it("decomposes to a different set")
	{
	(t != s) should be === true // hack
	}

	it("decomposes to a nonoverlapping set")
	{
	// t is now nonoverlapping
	Rectangle.decomposeToNonOverlapping(t) should be === t
	}
	}
	}

