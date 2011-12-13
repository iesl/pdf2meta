package edu.umass.cs.iesl.pdf2meta.cli.metadatamodel


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

// case classes?
case class SimpleMetadataModel(sourcefile: String, docid: String, //pubmedid?
                               year: Option[Int], title: String, authors: Seq[Author], paperAbstract: String, body: String, referenceStrings: List[String], referenceIds: List[String],
                               venue: Option[Venue]) extends MetadataModel


case class Venue(journalTitle: String, journalNlmTaID: String, journalPPubISSN: String, journalEPubISSN: String, publisher: Publisher)

case class Publisher(name: String, location: String)

case class Author(firstName: Option[String], initials: Option[String], lastName: String, affiliation: Option[String], email: Option[String])
  {
  override def toString = firstName.map(_ + " ").getOrElse("") + lastName
  }

/*
 docid (sufficient to find the original source)
 year
 authors (comma separated)
 title
 abstract
 references (as comma separated list of docids) [do this later?]
 */
