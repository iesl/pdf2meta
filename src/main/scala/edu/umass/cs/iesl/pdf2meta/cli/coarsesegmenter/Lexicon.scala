package edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter

import edu.umass.cs.iesl.pdf2meta.cli.util.Util
import java.io.InputStream

object Lexicon
  {
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


class Lexicon(s: InputStream)
  {
  val (lexTokens, lexTokensLC) =
    {
    val text: String = Util.loadText(s)
    val lexTokensLC: Map[String, Boolean] = text.toLowerCase.split("\n").map(x => (x -> true)).toMap
    val lexTokens: Map[String, Boolean] = text.split("\n").map(x => (x -> true)).toMap
    (lexTokens, lexTokensLC)
    }
  def countMatches(tokens: Seq[String]): Int =
    {
    tokens.map(lexTokens.get(_)).flatten.length
    }

  def countMatchesLC(tokens: Seq[String]): Int =
    {
    tokens.map((x: String) => lexTokensLC.get(x.toLowerCase)).flatten.length
    }

  def matches(tokens: Seq[String]): Seq[String] =
    {
    tokens.filter(t =>
                    {
                    val r: Option[Boolean] = lexTokens.get(t)
                    r.getOrElse(false)
                    })
    }

  def matchesLC(tokens: Seq[String]): Seq[String] =
    {
    tokens.filter(t =>
                    {
                    val r: Option[Boolean] = lexTokensLC.get(t.toLowerCase)
                    r.getOrElse(false)
                    })
    }
  }
