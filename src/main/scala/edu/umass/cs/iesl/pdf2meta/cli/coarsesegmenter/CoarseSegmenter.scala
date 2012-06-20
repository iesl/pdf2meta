package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import edu.umass.cs.iesl.scalacommons.collections.WeightedSet
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import collection.Seq
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import edu.umass.cs.iesl.scalacommons.{NonemptyString, ListUtils, StringUtils}

trait CoarseSegmenter extends (DocNode => ClassifiedRectangles)

case class ClassifiedRectangle(node: DocNode, featureWeights: WeightedSet[Feature], labelWeights: WeightedSet[String], basedOn: Option[ClassifiedRectangle])
	{
	def callLabel(os: Option[String]): ClassifiedRectangle =
		{

		os.map(s =>
			       {
			       val newWeights = new WeightedSet[String]()
				       {
				       def asMap = Map((s, 1.0))
				       }

			       new ClassifiedRectangle(node, featureWeights, newWeights.normalized, Some(this))
			       }).getOrElse(this)
		}

	lazy val label: Option[String] = labelWeights.unambiguousBest(0.9) match
	{
		case Some(x) => Some(x)
		case None    => basedOn.flatMap(_.label)
	}

	def discarded = label.map(_.equalsIgnoreCase("discard")).getOrElse(false)
	}

// extends CitationMention
class ClassifiedRectangles(val raw: Seq[ClassifiedRectangle])
	{

	//  val nonwhite  = raw.map(_.node).filter({case x: WhitespaceBox => false; case _ => true})
	lazy val legit = raw.filter(_.label.map(s => !s.equalsIgnoreCase("discard")).getOrElse(true))

	// for debugging
	val legitSections = legit.size
	val rawSections   = raw.size

	def discarded = raw.filter(_.discarded)

	val delimiters = raw.map(_.node).filter({
	                                        case x: DelimitingBox => true;
	                                        case _                => false
	                                        })

	def onPage(page: Page) =
		{
		new ClassifiedRectangles(raw.filter(_.node.rectangle match
		                                    {
			                                    case Some(x) => x.page == page;
			                                    case None    => false;
		                                    }))
		}

	def docid = "bogus"

	def sourcefile = "bogus"

	def body =
		{
		val bodyNodes: Seq[ClassifiedRectangle] = raw.filter(_.label.map(_.equals("body")).getOrElse(false))
		val bodyNodeTexts: Seq[String] = bodyNodes.map(_.node.text)
		bodyNodeTexts.mkString(" ")
		}

	def title =
		{
		val titleNodes: Seq[ClassifiedRectangle] = raw.filter(_.label.map(_.equals("title")).getOrElse(false))
		val titleNodeTexts: Seq[String] = titleNodes.map(_.node.text)
		titleNodeTexts.mkString(" ")
		}

	def paperAbstract = getSections("abstract").map(_.node.text).mkString(" ")

	def authors = getSections("authors").map(_.node.text).mkString(" ")

/*	def mergeRefs(node: DocNode, prefix: List[DocNode]): List[DocNode] =
		{
		if (prefix.isEmpty) List(node)
		else
			{
			val lastNode = prefix.head // reversed!
			// if this node is indented, combine
			if (node.isInstanceOf[AnnotationNode] && node.asInstanceOf[AnnotationNode].annotations.contains("indented"))
				{
				val combinedNode: DocNode = lastNode :+ node //DocNode(node.id + "+" + lastNode.id, lastNode + node, None, None)
				//new AnnotationNode(combinedNode,Seq("indented"))
				combinedNode :: prefix.tail
				}
			else
				{
				node :: prefix
				}
			}
		}
*/
	def similar(a:DocNode, b:DocNode) : Boolean = b.isInstanceOf[AnnotationNode] && b.asInstanceOf[AnnotationNode].annotations.contains("indented")

	def referenceStrings: Seq[String] =
		{
		val refSections = getSections("references");
		// ** need to split
		val refLines = refSections.flatMap(_.node.children) // these could not previously be distinguished by layout
		// group reflines so that each non-indented line starts a new group
		//val individualReferences = refLines.reverse.foldRight(List[DocNode]())((a: DocNode, b: List[DocNode]) => mergeRefs(a, b)).reverse

		val refGroups = ListUtils.groupContiguousSimilar(similar)(refLines.toList)

		import StringUtils.enrichString
		// and/or, look for "hanging-indent" annotation
		//val refStrings = individualReferences.map(_.text.removeNewlines)

		val refStrings = refGroups.map(group => group.map(_.text).mkString(" ").removeNewlines)
		refStrings
		}

	val referenceIds = Nil
	val venue        = None
	val year         = None

	private def getSections(l: String): Seq[ClassifiedRectangle] =
		{
		legit.filter(_.label.map(_.equals(l)).getOrElse(false))
		}
	}

object ClassifiedRectangles
	{
	import StringUtils.emptyStringToNone
	//** this shouldn't really be implicit; too much magic?  Or, it's OK because the magic is above?
	// could be just another Transformer
	implicit def classifiedRectanglesToStructuredCitation: (ClassifiedRectangles) => StructuredCitation = cr =>
		{
		new StructuredCitation
			{
			override val title       : Option[NonemptyString]             = StringUtils.emptyStringToNone(cr.title)
			override val doctype     : Option[DocType]            = JournalArticle
			override val abstractText: Iterable[TextWithLanguage] = cr.paperAbstract match
			{
				case "" => None
				case a  => Some(new TextWithLanguage(None, a))
			}
			override val bodyText    : Seq[BodyTextSection]       = Seq(new UndifferentiatedBodyTextSection(cr.body))
			override val authors                                  =
				{
				val a = cr.authors.split("and|,").map(fullname => new AuthorInRole(Person(fullname), Nil)).toList
				a
				}
			override val references  : Seq[StructuredCitation]    = cr.referenceStrings.map(s => new StructuredCitation()
				{
				override val unstructuredString : Option[NonemptyString] = s
				})
			}
		}
	}
