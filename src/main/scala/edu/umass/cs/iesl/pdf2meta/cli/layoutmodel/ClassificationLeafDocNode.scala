package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

/*
object ClassificationLeafDocNode
	{
	def apply(id: String, children: Seq[DocNode], localInfo: Option[Iterator[String]],
	          localErrors: Option[Iterator[String]]): DocNode = //, isMergeable: Boolean {
		new ClassificationLeafDocNode(id, children, localInfo, localErrors)
	}
*/
/**
 * store the underlying "secret" children, but be a leaf on the main tree by reporting Nil children.
 *
 * This defines the granularity at which classification will be done.
 * Larger blocks are preferred for accurate classification, if one can tell in advance that the whole block should be classified the same.
 */
/*
class ClassificationLeafDocNode(override val id: String, override val children: Seq[DocNode], override val localInfo: Option[Iterator[String]],
                  override val localErrors: Option[Iterator[String]])
		extends DocNode(id, children, localInfo, localErrors)
	{
// def allChildren = secretChildren  // children ++    There aren't both secret and normal children simultaneously
override val isClassificationLeaf = true
}*/

/**A node carrying annotations, considered to apply to the single child and all of its descendants */
/*
object AnnotatedDocNode
  {
  def apply(id: String, children: Seq[DocNode], localInfo: Option[Iterator[String]], localErrors: Option[Iterator[String]], annotations: Seq[String]): DocNode =
    {
    new AnnotatedDocNode(id, children, localInfo, localErrors, annotations)
    }
  }

/**
 * A leaf node carrying some textual tags.  Currently used to mark "sideways" text; probably also good for "hangingindent" paragraphs
 * @param id
 * @param secretChildren
 * @param localInfo
 * @param localErrors
 * @param annotations
 */
class AnnotatedDocNode(override val id: String, override val secretChildren: Seq[DocNode], override val localInfo: Option[Iterator[String]],
override val localErrors: Option[Iterator[String]],
                       val annotations: Seq[String])
        extends LeafDocNode(id, secretChildren, localInfo, localErrors)
  {

  override def create(childrenA: Seq[DocNode]) =
    {
    if (childrenA.length == 1) childrenA(0)
    else if (childrenA == children) this
    else
      AnnotatedDocNode(id, childrenA, localInfo, localErrors, annotations)
    }
  }
*/
