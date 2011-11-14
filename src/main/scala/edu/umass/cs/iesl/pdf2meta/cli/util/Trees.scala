package edu.umass.cs.iesl.pdf2meta.cli.util

trait TreeNode[T <: TreeNode[T]] //extends Set[T]  //todo: inheritance vs composition of Set
  {
  def children: Seq[T]
  }

trait OrderedTreeNode[T <: OrderedTreeNode[T]] extends TreeNode[T] //with SortedSet[T]
  {
  self: T =>

  def id: String
  def create(children: Seq[T]): T

  /**
   * provided a function that transforms a single node: apply it recursively, processing children first.
   *
   * The provided function f transforms one node, without transforming its children (since that has already happened).
   * There are three possible outcomes:
   *    1) a transformed node is returned;
   *    2) nothing changed, so the node itself is returned,
   *    3) None is returned (i.e., the node should be eliminated)
   *
   * Cases 1 and 2 are covered by returning Some[DocNode].
   */
  def postOrderApply(f: T => Option[T]): Option[T] =
    {
    // create an intermediate node where the children have been mapped
    val intermediate =
        {
        val newChildren = children.map(_.postOrderApply(f))
        create(newChildren.flatten)
        }
    // then apply the function here
    f(intermediate)
    }

  /**
   * provided a function that transforms a single node: apply it recursively, processing children last.
   *
   * The provided function f transforms one node, without transforming its children (since that will happen next).
   * There are four possible outcomes:
   *    1) a transformed node is returned;
   *    2) nothing changed, so the node itself is returned,
   *    3) the node or its transformation is returned, but there should be no recursion, and
   *    4) None is returned (i.e., the node should be eliminated), in which case obviously there is no recursion
   *
   * Cases 1 and 2 are covered by returning Some[DocNode].  Case 3 should be covered by returning a node with no children.
   */
  def preOrderApply(f: T => Option[T]): Option[T] =
    {
    // first apply the function here
    val intermediate = f(this)

    intermediate.map(n =>
                       {
                       // then apply it to the children
                       val newChildren = n.children.map(_.preOrderApply(f)).flatten
                       n.create(newChildren)
                       })
    }
  }


