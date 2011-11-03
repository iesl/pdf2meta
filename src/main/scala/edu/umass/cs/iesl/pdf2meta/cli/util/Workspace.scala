package edu.umass.cs.iesl.pdf2meta.cli.util

import scalax.io._
import tools.nsc.io._
import java.io.InputStream

/**
 * a File to operate on together with a temp directory.
 */
trait Workspace
  {
  def filename: String
  def file: File
  def dir: Directory
  def clean()
  }

class StreamWorkspace(val filename: String, instream: InputStream) extends Workspace
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

  def clean()
    {dir deleteRecursively()}
  }

class FileWorkspace(val jfile: java.io.File) extends Workspace
  {
  val file = File(jfile)
  lazy val dir = Directory.makeTemp()

  lazy val filename = file.name
  //lazy val file = File(dir + File.separator + filename)
  def clean()
    {dir deleteRecursively()}
  }
