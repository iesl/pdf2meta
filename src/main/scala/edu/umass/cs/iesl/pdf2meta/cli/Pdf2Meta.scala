package edu.umass.cs.iesl.pdf2meta.cli

import config.{WiredExtractOnlyPipeline, WiredPipeline}
import layoutmodel.DocNode
import tools.nsc.io.JFile
import util.FileWorkspace


object Pdf2Meta
  {

  def main(args: Array[String])
    {
    new Pdf2Meta().run(args)
    }


  // Some ANSI helpers...
  def ANSI(value: Any) = "\u001B[" + value + "m"
  val BOLD = ANSI(1)
  val RESET = ANSI(0)

  def usage()
    {
    println("Usage: pdf2meta extractonly filename")
    }
  }

class Pdf2Meta
  {
  def run(args: Array[String])
    {
    val command = args(0)
    val infilename = args(1);

    // really quick & dirty command line parse
    val w = new FileWorkspace(new JFile(infilename))

    command match
    {
      case "textonly" =>
        {
        val w = new FileWorkspace(new JFile(infilename))
        val doc: DocNode = WiredExtractOnlyPipeline(w)
        val result: String = doc.text
        println(result)
        }
      case _ => Pdf2Meta.usage()
    }

    val m = WiredPipeline.pipeline(w)
    }
  }


/*
@command(scope = "pdf2meta", name = "pdf2meta", description = "Convert PDFs and metadata among various formats")
class Pdf2Meta extends Main with Action
  {

  import Pdf2Meta._

  //setUser("me")
  setApplication("pdf2meta")

  var debug = false

  override def getDiscoveryResource = "META-INF/services/edu.umass.cs.iesl.pdf2meta/commands.index"

  override def isMultiScopeMode() = false


/*  override def createConsole(commandProcessor: CommandProcessorImpl, in: InputStream, out: PrintStream, err: PrintStream, terminal: Terminal) =
    {
    new Console(commandProcessor, in, out, err, terminal, null)
      {
      protected override def getPrompt = BOLD + "pdf2meta> " + RESET
      protected override def welcome =
        {
        session.getConsole().println("hello world") //Util.loadText(getClass().getResourceAsStream("banner.txt")))
        }
      protected override def setSessionProperties =
        {}
      }
    }*/

  @argument(name = "args", description = "sub command arguments", multiValued = true)
  var args = Array[String]()

  def execute(session: CommandSession): AnyRef =
    {
    run(session, args)
    null
    }
  }



*/

