package edu.umass.cs.iesl.pdf2meta.cli.commands
/*
import java.{util => ju, lang => jl}
import java.io.File
import org.apache.felix.gogo.commands.{Action, Option => option, Argument => argument, Command => command}
import org.apache.felix.service.command.CommandSession
import edu.umass.cs.iesl.pdf2meta.cli.WiredExtractOnlyPipeline
import edu.umass.cs.iesl.pdf2meta.cli.util.FileWorkspace

@command(scope = "pdf2meta", name = "toRawText", description = "Extract a PDF to text")
class ToRawText extends Action
  {


  @argument(required = false, name = "regex", description = "Regex for files to parse.  Defaults to '^.*\\.pdf$'")
  var fileRegex: String = _

  @option(name = "--indir", description = "Sets the directory where input files are found.  Defaults to the current directory.")
  var indir: File = _

  @option(name = "--outdir", description = "Sets the directory where output files are written.  Defaults to the input directory, so files are converted in place.")
  var outdir: File = _

  @option(name = "--delim", description = "Delimit paragraphs/text chunks with this string.  Default \" \", so the whole document ends up on one line.")
  var delim : String = _

  val pipeline = WiredExtractOnlyPipeline.pipeline

  def execute(session: CommandSession) : AnyRef =
    {
    try
    {
    forEachRecursiveListFiles(indir, fileRegex.r, process)
    "Done"
    }
    catch
    {
    case e: Exception =>
      "Error: " + e
    }
    }

  val process: (File => Unit) =
    {(f: File) =>
      {
      val w = new FileWorkspace(f)
      val r = pipeline.apply(w)
      val text = r.mkString(delim)
      w.clean
      }
    }

  // http://stackoverflow.com/questions/2637643/how-do-i-list-all-files-in-a-subdirectory-in-scala

  import scala.util.matching.Regex

  def forEachRecursiveListFiles(f: File, r: Regex, process: (File => Unit))
    {
    if (f.isDirectory)
      {
      val these = f.listFiles
      if (these != null)
        {
        val good = these.filter(f => r.findFirstIn(f.getName).isDefined)
        good.map(process)
        these.filter(_.isDirectory).map(forEachRecursiveListFiles(_, r, process))
        }
      }
    }
  }
*/
