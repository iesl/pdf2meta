package edu.umass.cs.iesl.pdf2meta.cli

import config.{WiredExtractOnlyPipeline, WiredPipeline}
import layoutmodel.DocNode
import tools.nsc.io.JFile
<<<<<<< HEAD
//import com.typesafe.scalalogging.slf4j.Logging
=======
>>>>>>> e76b1fb7f4eb4f393d65aef7df75867a65148eb5
import com.typesafe.scalalogging.slf4j.Logging
import java.net.URL
import edu.umass.cs.iesl.scalacommons.{StreamWorkspace, FileWorkspace}
import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.NamedPlugin

/*
object Pdf2Meta {

  def main(args: Array[String]) {
    new Pdf2Meta().run(args)
  }

  def usage() {
    println("Usage: pdf2meta extractonly filename")
  }
}

class Pdf2Meta {
  def run(args: Array[String]) {
    val command = args(0)
    val infilename = args(1);

    // really quick & dirty command line parse
    //val w = new FileWorkspace(new JFile(infilename))

    command match {
      case "textonly" => {
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
*/


object PdfReader extends Transformer[URL, StructuredCitation] with NamedPlugin with Logging {
  override val fromType = "java.net.URL"
  override val toType = "StructuredCitation"

  val name = "pdf"

  def apply(v1: URL) = {
    logger.debug("Processing PDF: " + v1 + " with protocol " + v1.getProtocol)
    val w = if (v1.getProtocol.equals("file")) {
      new FileWorkspace(new JFile(v1.getPath))
    } else {
      new StreamWorkspace(v1.getFile, v1.openStream())
    }

    val cm = WiredPipeline(w)
    Seq(cm)
  }
}
