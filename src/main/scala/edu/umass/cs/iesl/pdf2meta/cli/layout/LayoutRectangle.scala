package edu.umass.cs.iesl.pdf2meta.cli.layout

import collection.Seq
import edu.umass.cs.iesl.pdf2meta.cli.util.Intervals

object LayoutRectangle
  {
  def apply(id: String, children: Seq[LayoutRectangle]): LayoutRectangle =
    {
    if (children.exists({case x: TextBox => true; case _ => false}))
      {new TextBox(id, children)}
    else new LayoutRectangle(id, children)
    }
  }

class LayoutRectangle(val id: String, override val children: Seq[LayoutRectangle])
        extends OrderedTreeNode[LayoutRectangle] with Rectangular
  {

  /**
   * recursively collect all nodes in breadth-first order
   */
  def allNodes: List[LayoutRectangle] =
    {
    val childNodeLists: List[List[LayoutRectangle]] = children.map(_.allNodes).toList
    val f: List[LayoutRectangle] = childNodeLists.flatten
    children.toList ::: f
    }

  def allLeaves: List[LayoutRectangle] = allNodes.filter(_.children.isEmpty)


  def partition(boxorder: Ordering[Rectangular]): LayoutRectangle =
    {
    val newChildren = partitionByDelimiters.map(_.partitionByLayout(boxorder))
                      .flatten //.map(_.partitionBySpecial(boxorder))
    LayoutRectangle(id, newChildren)
    }

  def partitionBySpecial(boxorder: Ordering[Rectangular]): Seq[LayoutRectangle] = List(this)

  def rectangle: Rectangle =
    {
    Rectangle.encompassing(children.map(x => x.rectangle), 5).getOrElse(throw new Error("TextBox without rectangle"))
    }
  /*
  def partitionByDelimiters: Seq[LayoutRectangle] =
      {
      var rects : Seq[LayoutRectangle] = List(this)
      var lastRects = rects
      do {
      rects = rects.partitionByDelimitersOnce
      while (rects != lastRects)
      }
      }*/
  def partitionByDelimiters: Seq[LayoutRectangle] =
    {
    // if there is a RectBox or CurveBox that separates the space, use it
    val delimiters: Seq[LayoutRectangle] = children.collect({case x: RectBox => x; case x: CurveBox => x})

    val horizontalDelimiter: Option[LayoutRectangle] =
      {
      // sort by line thickness so that we later prefer the thickest delimiter
      val horizontalLines = delimiters.filter(_.rectangle.isLandscape).sortBy(x => x.rectangle.height)

      val horizontalHoles = Intervals.holesBySize(Intervals.union(children.map(_.rectangle.verticalInterval)))
      /*  def verticalHoleContainsDelimiter(hole: (Double, Double)) =
      {
      !(horizontalLines.filter(d =>
                            {
                            d.rectangle.isAbove(hole._1) &&
                            d.rectangle.isBelow(hole._2)
                            })).isEmpty
      }*/
      def horizontalLineInHole(line: LayoutRectangle) =
        {
        horizontalHoles.exists({
                               case ((bottom, top)) => (line.rectangle.isAbove(bottom) &&
                                                        line.rectangle.isBelow(top))
                               })
        }


      val lines = horizontalLines.filter(horizontalLineInHole)
      lines match
      {
        case Nil => None;
        case a :: b => Some(a)
      }
      }

    val verticalDelimiter: Option[LayoutRectangle] =
      {
      // sort by line thickness so that we later prefer the thickest delimiter
      val verticalLines = delimiters.filter(_.rectangle.isPortrait).sortBy(x => x.rectangle.width)
      val verticalHoles = Intervals.holesBySize(Intervals.union(children.map(_.rectangle.horizontalInterval)))
      def verticalLineInHole(line: LayoutRectangle) =
        {
        verticalHoles.exists({
                             case ((left, right)) => (line.rectangle.isRightOf(left) &&
                                                      line.rectangle.isLeftOf(right))
                             })
        }


      val lines = verticalLines.filter(verticalLineInHole)
      lines match
      {
        case Nil => None;
        case a :: b => Some(a)
      }
      }

    val verticalSplit = verticalDelimiter.map(d =>
                                                {
                                                List(LayoutRectangle(id + ".l", children.filter(_.rectangle
                                                                                                .isLeftOf(d.rectangle
                                                                                                          .left))),
                                                     LayoutRectangle(id + ".r", children.filter(_.rectangle
                                                                                                .isRightOf(d.rectangle
                                                                                                           .right))))
                                                })

    if (!verticalSplit.isEmpty)
      {
      verticalSplit.get.map(_.partitionByDelimiters).flatten
      }
    else
      {
      val horizontalSplit = horizontalDelimiter.map(d =>
                                                      {
                                                      List(LayoutRectangle(id + ".t", children.filter(_.rectangle
                                                                                                      .isAbove(d






                                                                                                               .rectangle
                                                                                                               .top))),
                                                           LayoutRectangle(id + ".b", children.filter(_.rectangle
                                                                                                      .isBelow(d
                                                                                                               .rectangle
                                                                                                               .bottom))))
                                                      })

      if (!horizontalSplit.isEmpty)
        {
        horizontalSplit.get.map(_.partitionByDelimiters).flatten
        }
      else List(this)
      }
    }

  def partitionByLayout(boxorder: Ordering[Rectangular]): Seq[LayoutRectangle] =
    {

    val verticalPartition: Option[(Double, Double)] = Intervals.largestHole(Intervals.union(children.map(_.rectangle



                                                                                                         .horizontalInterval)),
                                                                            3)
    val horizontalPartition: Option[(Double, Double)] = Intervals.largestHole(Intervals.union(children.map(_.rectangle




                                                                                                           .verticalInterval)),
                                                                              3)

    val verticalSplit = verticalPartition.map(d =>
                                                {
                                                List(LayoutRectangle(id + ".l",
                                                                     children.filter(_.rectangle.isLeftOf(d._1))),
                                                     LayoutRectangle(id + ".r",
                                                                     children.filter(_.rectangle.isRightOf(d._2))))
                                                })

    if (!verticalSplit.isEmpty)
      {
      verticalSplit.get.map(_.partitionByLayout(boxorder)).flatten
      }
    else
      {
      val horizontalSplit = horizontalPartition.map(d =>
                                                      {
                                                      List(LayoutRectangle(id + ".t",
                                                                           children.filter(_.rectangle.isAbove(d._1))),
                                                           LayoutRectangle(id + ".b",
                                                                           children.filter(_.rectangle.isBelow(d._2))))
                                                      })

      if (!horizontalSplit.isEmpty)
        {
        horizontalSplit.get.map(_.partitionByLayout(boxorder)).flatten
        }
      else List(this)
      }
    }

  def create(idA: String, childrenA: Seq[LayoutRectangle]) =
    {
    LayoutRectangle(idA, childrenA)
    }
  }

















