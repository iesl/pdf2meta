package edu.umass.cs.iesl.pdf2meta.cli.extract

import edu.umass.cs.iesl.pdf2meta.cli.util.Workspace
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode


/*object XmlExtractor
  {

  type extractor = (Workspace) => List[Page]
  }
*/
trait XmlExtractor extends Function1[Workspace, DocNode]
  {
  //def workspace: Workspace
  //def pages: List[Page]
  //def extract: extractor
  def apply(w: Workspace): DocNode
  }

//case class Status(command: String, output: String)

trait XmlExtractorComponent
  {
  // because this component has no dependencies, we don't need a self type here, and the service class needn't be inner.

  val xmlExtractor: XmlExtractor
  }

/*
class test extends XmlExtractor
  {
  def apply(w: Workspace) = List.empty[Page]
  }

object t2
  {
  val q = new Workspace("", new FileInputStream(""))
  val t = new test
  val r = t(q)
  }
*/
