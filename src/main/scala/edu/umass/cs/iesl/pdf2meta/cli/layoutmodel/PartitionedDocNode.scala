package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

/**
 * A node representing an established partition
 */
abstract class PartitionedDocNode(override val id: String, override val children: Seq[DocNode], override val localInfo: Option[Iterator[String]],
                                  override val localErrors: Option[Iterator[String]], val strength: Float)
		extends InternalDocNode(id, children, localInfo, localErrors)
	{
	// if a weak partition contains a strong partition, then the weak one must be legitimate; so just give it the higher strength value
	// this is done the brute-force way because the contained partitions are not necessarily immediate children
	lazy val maxContainedStrength = strength.max(allPartitions.map(_.strength).max)

	/*
	  def applyThreshold(threshold: Float): DocNode =
		{
		if (strength > threshold)
		  {
		  this
		  }
		else
		  {
		  // if this partition doesn't pass the threshold, then none of the children do either
		  DocNode(id, allLeaves, localInfo, localErrors)
		  }
		}*/

	}

object WhitespacePartitionedDocNode
	{
	// "strength" is how certain we are of this partition.  We just use it for the width of the whitespace (and/or a bonus for containing a line, etc)
	def apply(id: String, children: Seq[DocNode], localInfo: Option[Iterator[String]], localErrors: Option[Iterator[String]], strength: Float): DocNode =
		{
		new WhitespacePartitionedDocNode(id, children, localInfo, localErrors, strength)
		}
	}

object LinePartitionedDocNode
	{
	// "strength" is how certain we are of this partition.  We just use it for the width of the whitespace (and/or a bonus for containing a line, etc)
	def apply(id: String, children: Seq[DocNode], localInfo: Option[Iterator[String]], localErrors: Option[Iterator[String]], strength: Float): DocNode =
		{
		new LinePartitionedDocNode(id, children, localInfo, localErrors, strength)
		}
	}

/**
 * A DocNode containing three children: a top/left child, a separating rectangle of whitespace, and a bottom/right child
 */
class WhitespacePartitionedDocNode(override val id: String, override val children: Seq[DocNode], override val localInfo: Option[Iterator[String]],
                                   override val localErrors: Option[Iterator[String]], override val strength: Float)
		extends PartitionedDocNode(id, children, localInfo, localErrors, strength)
	{
	// even if this node is not a partition, it may contain nested partitions due to horizontal/vertical alternation
	// so do no flattening at all; just remove the "partition" designation.
	//we can always flatten as a separate step if desired.
	def makeNonPartition: DocNode = new InternalDocNode(id, children.filter(!_.isInstanceOf[WhitespaceBox]), localInfo, localErrors)

	// { override val secretChildren = WhitespacePartitionedDocNode.this.secretChildren }
	override def create(childrenA: Seq[DocNode]) =
		{
		if (childrenA.length == 1) childrenA(0)
		else if (childrenA == children) this
		else
			WhitespacePartitionedDocNode(id, childrenA, localInfo, localErrors, strength)
		}

	override def printTreeNode = (if (strength >= 1000) "STRONG " else "") + "WHITESPACE PARTITION (" + strength + ") "
	}

/**
 * A DocNode containing three children: a top/left child, a separating line, and a bottom/right child
 */
class LinePartitionedDocNode(override val id: String, override val children: Seq[DocNode], override val localInfo: Option[Iterator[String]],
                             override val localErrors: Option[Iterator[String]], override val strength: Float)
		extends PartitionedDocNode(id, children, localInfo, localErrors, strength)
	{
	override def create(childrenA: Seq[DocNode]) =
		{
		if (childrenA.length == 1) childrenA(0)
		else if (childrenA == children) this
		else
			LinePartitionedDocNode(id, childrenA, localInfo, localErrors, strength)
		}

	override def printTreeNode = "LINE PARTITION (" + strength + ") "
	}
