package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import collection.Seq
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.scalacommons.FloatIntervals
import scala.None

object SlicingDocPartitioner
	{
	val MIN_WHITESPACE_GAP  = 5
	val MIN_WHITESPACE_AREA = 200
	}

class SlicingDocPartitioner extends PreOrderDocTransformer with Logging
	{

	import SlicingDocPartitioner._

	def applyLocalOnly(node: DocNode) =
		{
		node match
		{
			case n: LeafNode => Some(n)
			case n           =>
				{
				Some(partitionByDelimiters(node).getOrElse(partitionByLayout(n).getOrElse(node)))
				}
		}
		}

	/**
	 * Starting from a DocNode, partition its children if possible.  Group the resulting intermediate nodes in
	 * a new Rectangle which substitutes for the argument.
	 * attempt to partition, in order:
	 * left vs right, with line
	 * top vs bottom, with line
	 * largest of:
           - left vs right, without line
           - top vs bottom, without line
	 * maybe: fonts or other
         if any partition is found, recurse the whole thing
	 */
	/*  def apply2(node: DocNode): DocNode =
		  {
		  node match
		  {
			case n if n.isAtomic => n
			case n =>
			  {
			  val byDelimiters: Option[PartitionedDocNode]
			  = partitionByDelimiters(node)
			  val newChildren: Seq[DocNode] = byDelimiters.map(_.children).getOrElse({
																					 val byLayout: Option[PartitionedDocNode] = partitionByLayout(n)
																					 byLayout.map(_.children).getOrElse(node.children)
																					 })

			  // note if no partition is found, recurse anyway into the existing children because there may be fixed groups etc.

			  DocNode(node.id, newChildren.map(apply(_)), node.localInfo, node.localErrors, false) //, node.isMergeable)
			  }
		  }
		  }
	  */
	def partitionByDelimiters(node: DocNode): Option[LinePartitionedDocNode] =
		{
		// if there is a RectBox or CurveBox that separates the space, use it
		val delimiters: Seq[DocNode] = node.children.collect({
		                                                     case x: DelimitingBox => x
		                                                     })
		val nonDelimiters: Seq[DocNode] = node.children.collect({
		                                                        case x: DelimitingBox => None
		                                                        case x                => Some(x)
		                                                        }).flatten

		val horizontalDelimiter: Option[DocNode] =
			{
			// sort by line thickness so that we later prefer the thickest delimiter
			val horizontalLines = delimiters.filter(_.rectangle.get.isLandscape).sortBy(x => x.rectangle.get.height)

        val vint = nonDelimiters.map(_.rectangle.get.verticalInterval)
        val uvint = FloatIntervals.union(vint)

			val horizontalHoles = FloatIntervals.holesBySize(uvint)

			def horizontalLineInHole(line: DocNode) =
				{
				horizontalHoles.exists({
				                       case ((bottom, top)) =>
					                       {
					                       (line.rectangle.get.isAbove(bottom) &&
					                        line.rectangle.get.isBelow(top))
					                       }
				                       })
				}


			val lines = horizontalLines.filter(horizontalLineInHole)
			lines match
			{
				case Nil    => None;
				case a :: b => Some(a)
			}
			}

		val verticalDelimiter: Option[DocNode] =
			{
			// sort by line thickness so that we later prefer the thickest delimiter
			val verticalLines = delimiters.filter(_.rectangle.get.isPortrait).sortBy(x => x.rectangle.get.width)
			val verticalHoles = FloatIntervals.holesBySize(FloatIntervals.union(nonDelimiters.map(_.rectangle.get.horizontalInterval)))
			def verticalLineInHole(line: DocNode) =
				{
				verticalHoles.exists({
				                     case ((left, right)) =>
					                     {
					                     (line.rectangle.get.isRightOf(left) &&
					                      line.rectangle.get.isLeftOf(right))
					                     }
				                     })
				}


			val lines = verticalLines.filter(verticalLineInHole)
			lines match
			{
				case Nil    => None;
				case a :: b => Some(a)
			}
			}

		val verticalSplit =
			verticalDelimiter.map(d =>
				                      {
				                      val newChildren = List(InternalDocNode(node.id + ".l",
				                                                             node.children.filter(_.rectangle.get.isLeftOf(d.rectangle.get.left)), None,
				                                                             None),
				                                             d, InternalDocNode(node.id + ".r",
				                                                                node.children.filter(_.rectangle.get.isRightOf(d.rectangle.get.right)), None,
				                                                                None))
				                      new LinePartitionedDocNode(node.id, newChildren, node.localInfo, node.localErrors, 1000) //d.rectangle.get.width)
				                      })

		verticalSplit match
		{
			case Some(x) => verticalSplit
			case None    =>
				{
				val horizontalSplit =
					horizontalDelimiter.map(d =>
						                        {
						                        val newChildren = List(InternalDocNode(node.id + ".t",
						                                                               node.children.filter(_.rectangle.get.isAbove(d.rectangle.get.top)),
						                                                               None,
						                                                               None), d, InternalDocNode(node.id + ".b", node.children
						                                                                                                         .filter(_.rectangle.get
						                                                                                                                 .isBelow(d.rectangle
						                                                                                                                          .get
						                                                                                                                          .bottom)),
						                                                                                         None, None))
						                        new LinePartitionedDocNode(node.id, newChildren, node.localInfo, node.localErrors,
						                                                   1000) //d.rectangle.get.height)
						                        })


				horizontalSplit
				}
		}
		}

	def partitionByLayout(node: DocNode): Option[WhitespacePartitionedDocNode] =
		{
		//******** Vertical Partition
		// for vertical partitions it's good enough to pick the largest, do a binary partition, and recurse
		val verticalPartition: Option[(Float, Float)] = FloatIntervals.largestHole(FloatIntervals.union(node.children.map(_.rectangle.get
		                                                                                                                  .horizontalInterval)),
		                                                                           MIN_WHITESPACE_GAP)


		def leftRightSplit: Option[(WhitespacePartitionedDocNode, Float)] =
			{
			//List[DocNode ] {
			verticalPartition match
			{
				case Some(d) =>
					{
					lazy val vrect = new RectangleOnPage
						{
						val page   = node.rectangle.get.page
						val bottom = node.rectangle.get.bottom
						val left   = d._1
						val right  = d._2
						val top    = node.rectangle.get.top
						}

					if (vrect.area < MIN_WHITESPACE_AREA) None
					else
						{
						lazy val verticalPartitionBox = new WhitespaceBox(node.id + ".vert", vrect)

						val left: InternalDocNode = new
										InternalDocNode(node.id + ".l", node.children.filter(_.rectangle.get.isLeftOf(d._1)), None, None)
						val right: InternalDocNode = new
										InternalDocNode(node.id + ".r", node.children.filter(_.rectangle.get.isRightOf(d._2)), None, None)


						val newChildren = List(left, verticalPartitionBox, right)

						val p = new WhitespacePartitionedDocNode(node.id, newChildren, node.localInfo, node.localErrors,
						                                         1000) // , vrect.width)  // always honor vertical partitions


						Some(p -> vrect.area)
						}
					}
				case None    => None
			}
			}

		//******** Horizontal Partition
		// for horizontal partitions it's good enough to pick the largest, do a binary partition, and recurse
		val childrenArray = node.children.toArray // purely for debugging in IDEA
		val horizontalPartition: Option[(Float, Float)] = FloatIntervals.largestHole(FloatIntervals.union(childrenArray.map(_.rectangle.get
		                                                                                                                    .verticalInterval)),
		                                                                             MIN_WHITESPACE_GAP)

		def topBottomSplit: Option[(WhitespacePartitionedDocNode, Float)] =
			{
			horizontalPartition.map(d =>
				                        {
				                        lazy val hrect = new RectangleOnPage
					                        {
					                        val page   = node.rectangle.get.page
					                        val bottom = d._1
					                        val left   = node.rectangle.get.left
					                        val right  = node.rectangle.get.right
					                        val top    = d._2
					                        }

				                        lazy val horizontalPartitionBox = new WhitespaceBox(node.id + ".horiz", hrect)

				                        val newChildren = List(new InternalDocNode(node.id + ".t", node.children.filter(_.rectangle.get.isAbove(d._1)), None,
				                                                                   None), horizontalPartitionBox, new
								                        InternalDocNode(node.id + ".b", node.children.filter(_.rectangle.get.isBelow(d._2)), None, None))

				                        val p = new WhitespacePartitionedDocNode(node.id, newChildren, node.localInfo, node.localErrors, hrect.height)
				                        (p, hrect.area)
				                        })
			}

		(topBottomSplit, leftRightSplit) match
		{
			case (Some(ho), Some(vo)) =>
				{
				// in general, prefer the horizontal split over the vertical one...
				// unless the horizontal split height is so small that it my not be legit?
				// no, don't worry, we already imposed a 5-point minimum above
				val (h, hs) = ho
				val (v, vs) = vo
				//Some(h)
				// let's try requiring that the horizontal split is at least half as large as the vertical one
				//ach, just take the larger.  It's up to the split creation routine to set the right strengths.

				if (hs >= (vs)) // / 2.))
					Some(h)
				else
					Some(v)


				// if the horizontal split is large, prefer it.  Otherwise take a vertical split.
				/*
					if (hs > 36)
						Some(h)
					else
						Some(v)
	*/
				}
			case (Some(ho), None)     => Some(ho._1)
			case (None, Some(vo))     => Some(vo._1)
			case (None, None)         => None
		}
		}
	}

/*
    //******** Horizontal Partition
    // horizontal partitions are trickier due to the need to detect double-spacing. Because this is context-dependent, we can't just make a binary tree.
    // instead, take the set of all holes, and select the largest size threshold such that there are not four holes within 90%.
    // tht is just a heuristic for an unambiguous threshold.
    val vIntervals: Seq[(Double, Double)] = node.children.map(_.rectangle.get.verticalInterval)
    val vIntervalUnion: List[(Double, Double)] = Intervals.union(vIntervals)
    val vHoles: List[(Double, Double)] = Intervals.holesBySize(vIntervalUnion)

    def acceptHorizontalSplit(holes: List[(Double, Double)]): Boolean =
      {
      if (holes.isEmpty) false
      else
        {
        val h = holes.head
        val hSplitSize = (h._2 - h._1).abs

        val t = holes.tail
        val nextFour = t.slice(0, t.length.min(4))

        if (hSplitSize < vSplitSize || hSplitSize < 5) false
        else if (nextFour.length < 4) true
        else
          {
          val l = nextFour.last
          val gradientDetectorSplitSize: Double = (l._2 - l._1).abs
          (gradientDetectorSplitSize < (hSplitSize * .9), gradientDetectorSplitSize)
          }
        }
      }

    def chooseAcceptedSplits(holes: List[(Double, Double)]): (List[(Double, Double)], Double) =
      {
      // newMaxSize differs from node.minHorizontalGapSize only when the split is not accepted
      val (isAccepted: Boolean, newMaxSize: Double) = acceptHorizontalSplit(holes)
      if (isAccepted)
        {
        ((holes.head :: chooseAcceptedSplits(holes.tail)._1), newMaxSize)
        }
      else
        {
        (List(), newMaxSize)
        }
      }

    val (acceptedSplits, newMaxSize: Double) = chooseAcceptedSplits(vHoles)
    val acceptedSplitsSorted = acceptedSplits.sortBy(_._1)

    val contentBlocks: List[(Double, Double)] = Intervals.invert(acceptedSplitsSorted, Double.MinValue, Double.MaxValue)

    val contentNodes = contentBlocks.zipWithIndex.map({
                                                      case ((bottom, top), index) =>
                                                        {
                                                        val newChildren: Seq[DocNode] = node.children.filter(_.rectangle.get.isBelowAndAbove(top, bottom))
                                                        if (newChildren.isEmpty) None
                                                        else
                                                          Some(new DocNode(node.id + ".v" + index, newChildren, None, None, false,
                                                          false) with HorizontalSplitLimit
                                                            {
                                                            val minHorizontalGapSize = newMaxSize
                                                            })
                                                        }
                                                      }).toList.flatten


    val holeNodes = acceptedSplitsSorted.zipWithIndex.map({
                                                          case ((b, t), index) =>
                                                            {
                                                            val hrect = new RectangleOnPage
                                                              {
                                                              val page = node.rectangle.get.page
                                                              val bottom = b
                                                              val left = node.rectangle.get.left
                                                              val right = node.rectangle.get.right
                                                              val top = t
                                                              }

                                                            new RectBox(node.id + ".vh" + index, hrect) with HorizontalSplitLimit
                                                              {
                                                              val minHorizontalGapSize = newMaxSize // irrelevant
                                                              }
                                                            }
                                                          }).toList

    val horizontalSplit = if (holeNodes.isEmpty) None
                          else
                            {Some(holeNodes ::: contentNodes)}

    // the horizontalSplit is empty if there is a larger vertical hole
    if (!horizontalSplit.isEmpty)
      {
      horizontalSplit
      }
    else
      {
      verticalSplit
      }

    //horizontalSplit.map((_, newMaxSize)).getOrElse(verticalSplit.map((_, newMaxSize)))
    }
  }


*/



