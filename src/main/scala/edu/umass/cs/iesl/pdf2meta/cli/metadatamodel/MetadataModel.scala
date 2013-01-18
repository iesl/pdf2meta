package edu.umass.cs.iesl.pdf2meta.cli.metadatamodel

import edu.umass.cs.iesl.bibmogrify.model._
import java.net.URL
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.scalacommons.StringUtils._

/*
@deprecated
trait MetadataModel
  {
  val sourcefile: String
  val docid: String //pubmedid?
  val year: Option[Int]
  val title: String
  val authors: Seq[Author]
  val paperAbstract: String
  val body: String
  val referenceStrings: List[String]
  val referenceIds: List[String]
  val venue: Option[Venue]

  def totalTextSize = cleanAbstract.size + cleanBody.size

  def toOneLine: String = sourcefile + "\t" + docid + "\t" + year.getOrElse("") + "\t" + authors.mkString(", ") + "\t" + title + "\t" + cleanAbstract + "\t" + referenceIds.mkString(", ")

  def toMalletAbstract: String = docid + "\t" + year.getOrElse("") + "\t" + cleanAbstract

  def toMalletFull: String = docid + "\t" + year.getOrElse("") + "\t" + cleanTotal

  //val cleanAbstract = paperAbstract.toLowerCase.replaceAll("\\s", " ").replaceAll("[^\\w ]", " ").split(" +").mkString(" ")
  //val cleanBody = body.toLowerCase.replaceAll("\\s", " ").replaceAll("[^\\w ]", " ").split(" +").mkString(" ")
  def cleanAbstract = paperAbstract.replaceAll("\\s", " ")
  def cleanBody = body.replaceAll("\\s", " ")
  def cleanTotal = cleanAbstract + cleanBody
  }

object SimpleMetadataModel
  {
  val oneLineHeaders = "sourcefile\tdocid\tyear\tauthors\ttitle\tabstract\treferenceids"
  }
*/
// case classes?
class SimpleMetadataModel(sourcefile: URL, idauth: IdentifierAuthority, docid: NonemptyString, //pubmedid?
                          pubyear: Option[Int], override val title: Option[NonemptyString], override val authors: Seq[AuthorInRole], paperAbstract: String,
                          body: String, referenceStrings: List[String], referenceIds: List[String],
                          override val containedIn: Option[ContainmentInfo]) extends StructuredCitation {
  override val doctype: Option[DocType] = None //JournalArticle
  // val docSubtype: Option[String] = None // for journal articles: Letter; Application Note; Research Article, etc.  For grants: R01, K99, etc.
  //val title: String = title
  //val authors: Seq[AuthorInRole] = Nil
  // val otherContributors: Seq[OtherContributorInRole] = Nil
  //  val language: Option[Language] = None
  override val identifiers: Seq[Identifier] = Seq(new Identifier {
    override val authority = Some(idauth)
    override val value = docid
  })
  override val locations: Seq[Location] = Seq(BasicUrlLocation(sourcefile, Nil))
  
  // val supplementaryLocations: Seq[Location] = Nil // where to find supplementary material, databases, etc.
  //val containedIn: Option[ContainmentInfo] = None // journal, book, proceedings, etc.
  // val publisher: Option[Institution] = None // likely blank when there is containedIn, i.e. paper -> journal -> publisher
  override val dates: Seq[CitationEvent] = Seq(new CitationEvent {
    val date = Some(new PartialDate {
      val month = None
      val year = pubyear
      val day = None
    })
    val eventType = Published
  })
  // val grants: Seq[GrantInfo] = Nil

  // val references: Seq[CitationMention] = Nil // could include context here
  // val keywords: Seq[Keyword] = Nil

  override val abstractText: Iterable[TextWithLanguage] = paperAbstract match {
    case "" => None
    case a => TextWithLanguage(None, a)
  }
    // val introText: Option[String] = None
    // val bodyText: Option[String] = None

    // val notes: Seq[String] = Nil

    //  val refMarker: Option[String] = None // within-document ID
  }

/*
case class Venue(journalTitle: String, journalNlmTaID: String, journalPPubISSN: String, journalEPubISSN: String, publisher: Publisher)

case class Publisher(name: String, location: String)

@deprecated
case class Author(firstName: Option[String], initials: Option[String], lastName: String, affiliation: Option[String], email: Option[String])
  {
  override def toString = firstName.map(_ + " ").getOrElse("") + lastName
  }
*/

/*
 docid (sufficient to find the original source)
 year
 authors (comma separated)
 title
 abstract
 references (as comma separated list of docids) [do this later?]
 */
