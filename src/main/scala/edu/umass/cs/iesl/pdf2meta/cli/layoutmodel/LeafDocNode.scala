package edu.umass.cs.iesl.pdf2meta.cli.layoutmodel

object LeafDocNode {
  def apply(id: String, children: Seq[DocNode], localInfo: Option[Iterator[String]], localErrors: Option[Iterator[String]]): DocNode = //, isMergeable: Boolean {
    new LeafDocNode(id, children, localInfo, localErrors)
}


/**
 * store the underlying "secret" children, but be a leaf on the main tree by reporting Nil children.
 */
class LeafDocNode(override val id: String, override val secretChildren: Seq[DocNode], override val localInfo: Option[Iterator[String]], override val localErrors: Option[Iterator[String]])
  extends DocNode(id, Seq.empty, localInfo, localErrors)





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
class AnnotatedDocNode(override val id: String, override val secretChildren: Seq[DocNode], override val localInfo: Option[Iterator[String]], override val localErrors: Option[Iterator[String]],
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
