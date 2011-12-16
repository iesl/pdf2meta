package edu.umass.cs.iesl.pdf2meta.cli

import config.WiredExtractOnlyPipeline
import layoutmodel.DocNode
import tools.nsc.io.JFile
import edu.umass.cs.iesl.scalacommons.FileWorkspace

object Pdf2Meta
  {

  def main(args: Array[String])
    {
    new Pdf2Meta().run(args)
    }

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
    //val w = new FileWorkspace(new JFile(infilename))

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

    //val m = WiredPipeline.pipeline(w)
    }
  }
