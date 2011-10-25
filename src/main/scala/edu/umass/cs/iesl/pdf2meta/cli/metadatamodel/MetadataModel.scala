package edu.umass.cs.iesl.pdf2meta.cli.metadatamodel

// case classes?

trait MetadataModel {
val title : String
val authors : List[Author]
val paperabstract : String
val body : String
val referenceStrings : List[String]
}

trait Author
  {
  val firstName : Option[String]
  val initials : String
  val lastName : String
  val affiliation : String
  val email : String
  }
