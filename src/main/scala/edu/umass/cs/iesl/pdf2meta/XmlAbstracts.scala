package edu.umass.cs.iesl.pdf2meta

import cli.metadatamodel.{Venue, Author, MetadataModel, SimpleMetadataModel}
import scala.xml.factory.XMLLoader
import javax.xml.parsers.SAXParser
import com.weiglewilczek.slf4s.Logging
import tools.nsc.io.File
import xml.{Text, NodeSeq, Elem}
import scala.actors.Actor._
import actors.{IScheduler, Actor}
import actors.scheduler.DaemonScheduler

object XmlAbstracts
  {

  def main(args: Array[String])
    {
    new XmlAbstracts().run(args)
    }

  def usage()
    {
    println("Usage: find *.xml | xmlabstracts")
    }
  }

class XmlAbstractException(s: String) extends Exception(s)


class XmlAbstracts extends Logging
  {

  def daemonactor(body: => Unit): Actor =
    {
    val a = new Actor
      {
      def act() = body
      override final val scheduler: IScheduler = DaemonScheduler
      }
    a.start()
    a
    }

  // just make sure that lines are printed atomically, and don't block the computation waiting for writes
  val syncPrinter = daemonactor
                    {
                    loop
                    {
                    react
                    {
                    case x => println(x)
                    }
                    }
                    }


  sealed trait Command extends ((String) => String)

  case class toMalletAbstract() extends Command
    {
    def apply(filename: String) : String =
      {
      val m = nxmlToMetadataModel(filename)
      if (m.totalTextSize < 150) throw new XmlAbstractException("Abstract + Body too short")
      m.toMalletAbstract
      }
    }

  case class toMalletFull() extends Command
    {
    def apply(filename: String) : String =
      {
      val m = nxmlToMetadataModel(filename)
      if (m.totalTextSize < 150) throw new XmlAbstractException("Abstract + Body too short")
      m.toMalletFull
      }
    }


  case class toOneLine() extends Command
    {
    def apply(filename: String) : String =
      {
      val m = nxmlToMetadataModel(filename)
      if (m.totalTextSize < 150) throw new XmlAbstractException("Abstract + Body too short")
      m.toOneLine
      }
    }



  def run(args: Array[String])
    {

    val converter : ((String) => String) = args(0) match
    {
    case "toOneLine" => { println(SimpleMetadataModel.oneLineHeaders); toOneLine() }
    case "toMalletFull" => toMalletFull()
    case "toMalletAbstract" => toMalletAbstract()
    }


    val lines = io.Source.stdin.getLines.toList.par
    lines foreach
    {ln =>
      {
      try
      {
      syncPrinter ! converter(ln)
      }
      catch
      {
      case e: XmlAbstractException =>
        {logger.info(e.getMessage + " in " + ln); None}
      }
      }
    }
    }

  //var nonPmidIncrementor = 0
  def convert(filename: String): String =
    {
    val m = nxmlToMetadataModel(filename)
    if (m.totalTextSize < 150) throw new XmlAbstractException("Abstract + Body too short")
    m.toOneLine
    }


  def nxmlToMetadataModel(filename: String): MetadataModel =
    {
    lazy val xml = XMLIgnoreDTD.loadFile(filename)
    val meta = xml \ "front" \ "article-meta"
    // <article-id pub-id-type="pmid">11812844</article-id>
    val docid = ((meta \ "article-id") find
                 {x => (x \ "@pub-id-type").text.trim == "pmid"
                 }).map(_.text.trim).getOrElse(File(filename).name)



    val title = (meta \ "title-group" \ "article-title").text.trim

    //{nonPmidIncrementor += 1; "x-" + nonPmidIncrementor}) //throw new XmlAbstractException("No PMID"))
    //<pub-date pub-type="ppub"><day>15</day><month>12</month><year>2001</year></pub-date>
    object date
      {
      val dateNodes: NodeSeq = meta \ "pub-date"
      val dateNode = (dateNodes find
                      {x => (x \ "@pub-type").text.trim == "ppub"}).getOrElse((dateNodes find
                                                                               {x => (x \ "@pub-type").text.trim == "epub"}).getOrElse(throw new XmlAbstractException("No Date")))

      val year = (dateNode \ "year").text.trim.toInt
      val month = (dateNode \ "month").text.trim
      val day = (dateNode \ "day").text.trim
      }

    val authorNodes: NodeSeq = (meta \ "contrib-group" \ "contrib") filter
                               {x => (x \ "@contrib-type").text.trim == "author"
                               }

    val authors = authorNodes.map(n =>
                                    {
                                    val name: NodeSeq = n \ "name"
                                    val lastName = (name \ "surname").text.trim
                                    val initials = None
                                    val firstName = (name \ "given-names").text.trim
                                    val affiliation = None
                                    val email = None

                                    Author(Some(firstName), initials, lastName, affiliation, email)
                                    })
    // <abstract>...</abstract>
    /*  def allTextNodes(n: NodeSeq): Seq[String] =
              {
              n match
              {
                case Text(t) => List(t)
                case _ => n.child.flatMap(allTextNodes)
              }
              }*/
    val paperAbstract = (meta \ "abstract").flatMap(_.descendant.flatMap({
                                                                         case Text(t) => Some(t)
                                                                         case _ => None
                                                                         })).mkString(" ")




    // val cleanAbstract = paperAbstract.toLowerCase.replaceAll("\\s", " ").replaceAll("[^\\w ]", " ").split(" +").mkString(" ")
    //  if (cleanAbstract.size < 150) throw new XmlAbstractException("Abstract too short")
    val body = (xml \ "body").flatMap(_.descendant.flatMap({
                                                           case Text(t) => Some(t)
                                                           case _ => None
                                                           })).mkString(" ")


    //  val cleanBody = body.toLowerCase.replaceAll("\\s", " ").replaceAll("[^\\w ]", " ").split(" +").mkString(" ")
    // allow the body to be too short or missing
    // if (cleanAbstract.size < 150) throw new XmlAbstractException("Abstract too short")
    val referenceStrings: scala.List[String] = Nil

    val referenceIds: scala.List[String] = Nil
    val venue: Option[Venue] = None

    SimpleMetadataModel(filename, docid, Some(date.year), title, authors, paperAbstract, body, referenceStrings, referenceIds, venue)
    }
  }

object XMLIgnoreDTD extends XMLLoader[Elem]
  {
  override def parser: SAXParser =
    {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    f.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
    f.newSAXParser()
    }
  }
