package edu.umass.cs.iesl.pdf2meta.cli.util

import scalax.io._
import tools.nsc.io._
import java.io.InputStream

class Workspace(val filename: String, instream: InputStream)
  {
  lazy val dir =
    {
    val dir = Directory.makeTemp()
    val file = File(dir + File.separator + filename)
    val outstream = file bufferedOutput (false)
    Resource.fromInputStream(instream) copyDataTo Resource.fromOutputStream(outstream)
    instream.close()
    outstream.close()

    dir
    }

  lazy val file = File(dir + File.separator + filename)

  def clean = dir deleteRecursively()

  }
