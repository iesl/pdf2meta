package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

import scala._

object InternalDocNode
	{
	def apply(id: String, children: Seq[DocNode], localInfo: Option[Iterator[String]],
	          localErrors: Option[Iterator[String]]): InternalDocNode = //, isMergeable: Boolean
		{
		new InternalDocNode(id, children, localInfo, localErrors)
		}
	}

class InternalDocNode(override val id: String, override val children: Seq[DocNode], override val localInfo: Option[Iterator[String]],
                      override val localErrors: Option[Iterator[String]]) extends DocNode(id, localInfo, localErrors)
// , val isClassificationLeaf: Boolean = false) //, val isMergeable: Boolean)
	{

	//val isClassificationLeaf = false  // when classifying, don't recurse pst nodes where this is true
	//def isLeaf = children.length == 0
	//def isSecretLeaf = secretChildren.length == 0
	// allow computing internal things based on a set of children that are not the same as the public ones
	// by default just use the same ones
	//def allChildren = children
	/**
	 * List the leaves, together with their extents as a proportion of the whole document.  Used to determine whether a given box is near the beginning or
	 * the
	 * end, etc.
	 */
	// a rectangle may be None if the node has children on multiple pages.
	// todo this may not be the best way to handle this situation, as we frequently do foobar.rectangle.get.height or whatever,
	// which is error-prone, and handling Options in every case is a hassle.
	lazy val rectangle: Option[RectangleOnPage] = computeRectangle

	def computeRectangle: Option[RectangleOnPage] =
		{
		val childRects: Seq[Option[RectangleOnPage]] = children.map((x: DocNode) => x.rectangle)
		if (childRects.exists(x => (x == None)))
			{
			val noRectChildren = children.filter((q: DocNode) => (q.rectangle == None))

			None
			}
		else
			{
			RectangleOnPage.encompassing(childRects.flatten, 0)
			}
		}

	override lazy val errors: Option[Iterator[String]] =
		{
		val r: Iterator[String] = children.map(_.errors).flatten.foldLeft[Iterator[String]](localErrors.getOrElse(Iterator.empty))((a: Iterator[String],
		                                                                                                                            b: Iterator[String]) =>
			                                                                                                                           {
			                                                                                                                           a ++
			                                                                                                                           b
			                                                                                                                           })
		if (r.isEmpty) None else Some(r)
		}

	override lazy val info: Option[Iterator[String]] =
		{
		val r: Iterator[String] = children.map(_.info).flatten.foldLeft[Iterator[String]](localInfo.getOrElse(Iterator.empty))((a: Iterator[String],
		                                                                                                                        b: Iterator[String]) =>
			                                                                                                                       {
			                                                                                                                       a ++
			                                                                                                                       b
			                                                                                                                       })
		if (r.isEmpty) None else Some(r)
		}

	/**
	 * recursively collect all nodes in breadth-first order
	 * excludes the root, but we don't care
	 */
	override def allNodesBreadthFirst: List[DocNode] =
		{
		val childNodeLists: List[List[DocNode]] = children.map(_.allNodesBreadthFirst).toList
		val f: List[DocNode] = childNodeLists.flatten
		children.toList ::: f
		}

	/**
	 * recursively collect all nodes in preorder depth-first order
	 */
	override def allNodesDepthFirst: List[DocNode] =
		{
		val childNodeLists: List[List[DocNode]] = children.map(_.allNodesDepthFirst).toList
		val f: List[DocNode] = childNodeLists.flatten
		this :: f
		}

	// { if(this.isInstanceOf[WhitespaceBox]) Nil else List(this) }
	//def allClassificationLeaves: Seq[DocNode] = if (isClassificationLeaf) List(this) else children.flatMap(_.allClassificationLeaves)
	/*def leaves: Seq[LeafNode] = if (children.length == 0) List(this)
	else children.flatMap({
	                      case l: InternalDocNode => l.leaves
	                      case l                  => List(l)
	                      })*/
	def leaves: Seq[LeafNode] = children.flatMap(_.leaves)

	def create(childrenA: Seq[DocNode]) =
		{
		if (childrenA.length == 1) childrenA(0)
		else if (childrenA == children) this
		else
			InternalDocNode(id, childrenA, localInfo, localErrors)
		}

	def printTreeNode: String = "GROUP "

	def printTree(prefix: String): String =
		{
		val buf = new StringBuilder(prefix)

		buf.append(printTreeNode)
		buf.append(id + " : " + children.length + " children, " + allNodesDepthFirst.length + " nodes, "
		           + "" + leaves.length + " leaves, "
		           + "" + delimiters +
		           " delimiters\n")

		for (c <- children)
			{buf.append(c.printTree(prefix + "   |"))}

		buf.toString()
		}

	/**
	 * Filter the children of this node, recursively; preorder means any immediate children failing the filter don't get recursed.
	 * Note this node itself doesn't get filtered
	 */
	def filterDeep(filt: (DocNode) => Boolean): DocNode = preOrderApply(n => Some(create(children.filter(filt)))).get

	/**drop all children, secret or otherwise, and just create a leaf node
	 *
	 * @return
	 */
	/*	def bounce: Option[LeafNode] =
	   {

	   val n = this
	   if (children.length > 0)
		   {
		   //val rect = Rectangle.encompassing(n..flatMap(_.rectangle), 0)
		   //val leafFonts = children.map(c => (new FontWithHeight(c.font, c.fontHeight), c.text.length))
		   //note n.id is likely a long mess, so just use the head instead
		   val result = new LeafNode(children.head.id, localInfo, localErrors, rectangle)
			   {
			   override lazy val allFonts: Seq[(FontWithHeight, Int)] = n.allFonts
			   /*	{
															 StatsUtils.histogramAccumulate(leafFonts).toSeq
															 }*/
			   override def create(childrenA: Seq[DocNode]) =
				   {
				   assert(childrenA.isEmpty)
				   this
				   }
			   }
		   Some(result)
		   }
	   else if (!text.isEmpty)
		   {
		   val result = new LeafNode(id, localInfo, localErrors, rectangle)
			   {
			   override      val text                                 = n.text
			   override lazy val allFonts: Seq[(FontWithHeight, Int)] = n.allFonts
			   /*	{
															 StatsUtils.histogramAccumulate(leafFonts).toSeq
															 }*/
			   override def create(childrenA: Seq[DocNode]) =
				   {
				   assert(childrenA.isEmpty)
				   this
				   }
			   }
		   Some(result)
		   }
	   else None
	   }*/
	// aggregate nodes into a group
	def :+(r: DocNode): DocNode =
		{
		InternalDocNode(id + "+" + r.id, children :+ r, localInfo, localErrors)

		/*
	//require(!(this.isInstanceOf[AnnotationNode] || r.isInstanceOf[AnnotationNode]))
	if (children.isEmpty)
		{
		// because "this" is a leaf node, include it in the container
		InternalDocNode(id + "+" + r.id, List(this, r), None, None)
		}
	else
		{
		// drop "this" node, since its children are all we care about anyway
		InternalDocNode(id + "+" + r.id, children :+ r, localInfo, localErrors)
		}
		*/
		}
	}

class PageNode(override val id: String, override val children: Seq[DocNode], override val localInfo: Option[Iterator[String]],
               override val localErrors: Option[Iterator[String]], val page: Page) extends InternalDocNode(id, children, localInfo, localErrors)
	{

	override def create(childrenA: Seq[DocNode]): DocNode =
		{
		if (childrenA.length == 1) childrenA(0)
		else if (childrenA == children) this
		else
			new PageNode(id, childrenA, localInfo, localErrors, page)
		}

	// aggregate nodes into a group
	override def :+(r: DocNode): DocNode =
		{
		new PageNode(id + "+" + r.id, children :+ r, localInfo, localErrors, page)
		}
	}
