package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import edu.umass.cs.iesl.scalacommons.Lexicon

object Lexicon {
  val cities = new Lexicon(getClass.getResourceAsStream("/lexicons/cities"))
  val companies = new Lexicon(getClass.getResourceAsStream("/lexicons/companies"))
  val companysuffix = new Lexicon(getClass.getResourceAsStream("/lexicons/companysuffix"))
  val countries = new Lexicon(getClass.getResourceAsStream("/lexicons/countries"))
  val days = new Lexicon(getClass.getResourceAsStream("/lexicons/days"))
  val firstnameHigh = new Lexicon(getClass.getResourceAsStream("/lexicons/firstnameHigh"))
  val firstnameHighest = new Lexicon(getClass.getResourceAsStream("/lexicons/firstnameHighest"))
  val firstnameMed = new Lexicon(getClass.getResourceAsStream("/lexicons/firstnameMed"))
  val jobtitle = new Lexicon(getClass.getResourceAsStream("/lexicons/jobtitle"))
  val lastnameHigh = new Lexicon(getClass.getResourceAsStream("/lexicons/lastnameHigh"))
  val lastnameHighest = new Lexicon(getClass.getResourceAsStream("/lexicons/lastnameHighest"))
  val lastnameMed = new Lexicon(getClass.getResourceAsStream("/lexicons/lastnameMed"))
  val months = new Lexicon(getClass.getResourceAsStream("/lexicons/months"))
  val states = new Lexicon(getClass.getResourceAsStream("/lexicons/states"))
}
