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
  def deepSorted(ordering: Ordering[T]): T =
    {
    children.length match
    {
      case 0 => this
      case _ =>
        {
        val sortedChildren = for (node <- this.children) yield node.deepSorted(ordering);

        val result = create(sortedChildren.toSeq.sorted[T](ordering))
        result
        }
    }
    }
  }

