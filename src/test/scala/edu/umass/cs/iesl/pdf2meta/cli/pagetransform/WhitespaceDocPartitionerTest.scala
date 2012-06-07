package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{BeforeAndAfter, Spec}
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.Rectangle
import util.Random
import collection.mutable

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
class WhitespaceDocPartitionerTest extends Spec with ShouldMatchers with BeforeAndAfter
	{

	val p                                                      = new WhitespaceDocPartitioner
	val qualityFunction: (Rectangle, Set[Rectangle]) => Double = (x: Rectangle, y: Set[Rectangle]) => x.area - y.map(_.area).sum
	val pivotPicker    : (Set[Rectangle]) => Rectangle         = (s: Set[Rectangle]) => s.toSeq(Random.nextInt(s.size))

	//perf?
	def configuredFindWhitespaces(bound: Rectangle, allObstacles: Set[Rectangle]) = p.find_whitespaces(qualityFunction, pivotPicker)(bound, allObstacles)

	val bound     = Rectangle(0, 0, 100, 100).get
	val obstacles = mutable.Set() ++ Set(Rectangle(10, 10, 20, 20), Rectangle(40, 40, 50, 80)).flatten

	describe("A fully-above fully-left-of rectangle")
	{
	it("works at all")
	{
	val w = configuredFindWhitespaces(bound, obstacles.toSet)
	w(0).area should be === 5000
	}

	it("works repeatedly")
	{
	val w = configuredFindWhitespaces(bound, obstacles.toSet)
	w.map((r) => info(r.toString))
	w.map(_.area) should be === Seq(5000, 3200, 600, 200, 200, 200)
	/*
	w.area should be === 5000
	obstacles += w

	val w2 = configuredFindWhitespaces(bound, obstacles.toSet).get
	w2.area should be === 3200
	obstacles += w2


	val w3 = configuredFindWhitespaces(bound, obstacles.toSet).get
	w3.area should be === 600
	obstacles += w3

	val w4 = configuredFindWhitespaces(bound, obstacles.toSet).get
	w4.area should be === 200
	obstacles += w4

	val w5 = configuredFindWhitespaces(bound, obstacles.toSet).get
	w5.area should be === 200
	obstacles += w5

	val w6 = configuredFindWhitespaces(bound, obstacles.toSet).get
	w6.area should be === 200
	obstacles += w6


	val w7 = configuredFindWhitespaces(bound, obstacles.toSet)
	w7 should be === None
	*/
	}
	}
	}
