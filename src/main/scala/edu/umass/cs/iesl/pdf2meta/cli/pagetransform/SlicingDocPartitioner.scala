package edu.umass.cs.iesl.pdf2meta.cli.pagetransform

import collection.Seq
import edu.umass.cs.iesl.pdf2meta.cli.util.Intervals
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import com.weiglewilczek.slf4s.Logging

/*
object SlicingLayoutPartitioner extends LayoutPartitioner
  {
  def apply(id: String, children: Seq[DocNode]): DocNode with SlicingLayoutPartitioner =
    {
    if (children.exists({case x: TextBox => true; case _ => false}))
      {new TextBox(id, children) with SlicingLayoutPartitioner}
    else new DocNode(id, children) with SlicingLayoutPartitioner
    }
  }
*/
class SlicingDocPartitioner extends DocTransformer with Logging
  {


  def apply(rect: DocNode): DocNode =
    {
    // first ignore any existing groupings
    //    val star = DocNode(rect.id,rect.allLeaves,rect.localInfo,rect.localErrors)
    // then do top-down slice partitioning
    val result = partition(rect)
    result
    }

  /**
   * Starting from a DocNode, partition its children if possible.  Group the resulting intermediate nodes in
   * a new Rectangle which substitutes for the argument.
   */
  def partition(node: DocNode): DocNode =
    {
    node match
    {
      // if this node has already been partitioned, just honor that
      case n: PartitionedDocNode => PartitionedDocNode(node.id, node.children.map(apply(_)), node.localInfo, node.localErrors)
      case n =>
        {
        val debugDelimiters = node.delimitingBoxes
        if (debugDelimiters.length > 0)
          {
          logger.info("Got delimiters")
          }
        // attempt to partition, in order:
        // * left vs right, with line
        // * top vs bottom, with line
        // * largest of:
        //   - left vs right, without line
        //   - top vs bottom, without line
        // * maybe: fonts or other
        // if any partition is found, recurse the whole thing
        val byDelimiters: Option[Seq[DocNode]] = partitionByDelimiters(node)
        val x: Seq[DocNode] = byDelimiters.getOrElse({
                                                     val byLayout: Option[Seq[DocNode]] = partitionByLayout(node)
                                                     byLayout.getOrElse(Nil)
                                                     })
        x match
        {
          case Nil => node //DocNode(node.id, node.children.map(apply(_)), node.localInfo, node.localErrors)
          case newChildren => DocNode(node.id, newChildren.map(apply(_)), node.localInfo, node.localErrors, false)
        }
        }
    }
    }


  //def partitionBySpecial(rect: DocNode): Seq[DocNode] = List(rect)
  def partitionByDelimiters(rect: DocNode): Option[Seq[DocNode]] =
    {
    // if there is a RectBox or CurveBox that separates the space, use it
    val delimiters: Seq[DocNode] = rect.children.collect({case x: DelimitingBox => x})
    val nonDelimiters: Seq[DocNode] = rect.children.collect({
                                                            case x: DelimitingBox => None
                                                            case x => Some(x)
                                                            }).flatten

    val horizontalDelimiter: Option[DocNode] =
      {
      // sort by line thickness so that we later prefer the thickest delimiter
      val horizontalLines = delimiters.filter(_.rectangle.get.isLandscape).sortBy(x => x.rectangle.get.height)

      val horizontalHoles = Intervals.holesBySize(Intervals.union(nonDelimiters.map(_.rectangle.get.verticalInterval)))
      /*  def verticalHoleContainsDelimiter(hole: (Double, Double)) =
      {
      !(horizontalLines.filter(d =>
                            {
                            d.rectangle.isAbove(hole._1) &&
                            d.rectangle.isBelow(hole._2)
                            })).isEmpty
      }*/
      def horizontalLineInHole(line: DocNode) =
        {
        horizontalHoles.exists({
                               case ((bottom, top)) => (line.rectangle.get.isAbove(bottom) &&
                                                        line.rectangle.get.isBelow(top))
                               })
        }


      val lines = horizontalLines.filter(horizontalLineInHole)
      lines match
      {
        case Nil => None;
        case a :: b => Some(a)
      }
      }

    val verticalDelimiter: Option[DocNode] =
      {
      // sort by line thickness so that we later prefer the thickest delimiter
      val verticalLines = delimiters.filter(_.rectangle.get.isPortrait).sortBy(x => x.rectangle.get.width)
      val verticalHoles = Intervals.holesBySize(Intervals.union(nonDelimiters.map(_.rectangle.get.horizontalInterval)))
      def verticalLineInHole(line: DocNode) =
        {
        verticalHoles.exists({
                             case ((left, right)) => (line.rectangle.get.isRightOf(left) &&
                                                      line.rectangle.get.isLeftOf(right))
                             })
        }


      val lines = verticalLines.filter(verticalLineInHole)
      lines match
      {
        case Nil => None;
        case a :: b => Some(a)
      }
      }

    val verticalSplit =
      verticalDelimiter.map(d =>
                              {
                              List(DocNode(rect.id + ".l", rect.children.filter(_.rectangle.get.isLeftOf(d.rectangle.get.left)), None, None, false), d,
                                   DocNode(rect.id + ".r", rect.children.filter(_.rectangle.get.isRightOf(d.rectangle.get.right)), None, None, false))
                              })

    verticalSplit match
    {
      case Some(x) => verticalSplit
      case None =>
        {
        val horizontalSplit =
          horizontalDelimiter.map(d =>
                                    {
                                    List(DocNode(rect.id + ".t", rect.children.filter(_.rectangle.get.isAbove(d.rectangle.get.top)), None, None, false), d,
                                         DocNode(rect.id + ".b", rect.children.filter(_.rectangle.get.isBelow(d.rectangle.get.bottom)), None, None, false))
                                    })

        horizontalSplit
        }
    }
    }

  def partitionByLayout(rect: DocNode): Option[Seq[DocNode]] =
    {

    val verticalPartition: Option[(Double, Double)] = Intervals.largestHole(Intervals.union(rect.children.map(_.rectangle.get.horizontalInterval)), 1)
    val horizontalPartition: Option[(Double, Double)] = Intervals.largestHole(Intervals.union(rect.children.map(_.rectangle.get.verticalInterval)), 1)


    def verticalSplit =
      {
      verticalPartition.map(d =>
                              {
                              lazy val vrect = new RectangleOnPage
                                {
                                val page = rect.rectangle.get.page
                                val bottom = page.rectangle.bottom
                                val left = d._1
                                val right = d._2
                                val top = page.rectangle.top
                                }

                              lazy val verticalPartitionBox = new RectBox(rect.id + ".vert", vrect)

                              List(DocNode(rect.id + ".l", rect.children.filter(_.rectangle.get.isLeftOf(d._1)), None, None, false), // verticalPartitionBox,
                                   DocNode(rect.id + ".r", rect.children.filter(_.rectangle.get.isRightOf(d._2)), None, None, false))
                              })
      }

    def horizontalSplit =
      {
      horizontalPartition.map(d =>
                                {
                                lazy val hrect = new RectangleOnPage
                                  {
                                  val page = rect.rectangle.get.page
                                  val bottom = d._1
                                  val left = page.rectangle.left
                                  val right = page.rectangle.right
                                  val top = d._2
                                  }

                                lazy val horizontalPartitionBox = new RectBox(rect.id + ".vert", hrect)

                                List(DocNode(rect.id + ".t", rect.children.filter(_.rectangle.get.isAbove(d._1)), None, None, false), // horizontalPartitionBox,
                                     DocNode(rect.id + ".b", rect.children.filter(_.rectangle.get.isBelow(d._2)), None, None, false))
                                })
      }
    (verticalPartition, horizontalPartition) match
    {
      case (Some(v), Some(h)) => if ((v._2 - v._1).abs > (h._2 - h._1).abs) verticalSplit else horizontalSplit
      case (Some(v), None) => verticalSplit
      case (None, Some(h)) => horizontalSplit
      case (None, None) => None
    }
    }
  }





















