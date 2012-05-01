package edu.umass.cs.iesl.pdf2meta.cli.extract

import edu.umass.cs.iesl.scalacommons.Workspace
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{DocNode, InternalDocNode}

trait PdfExtractor extends Function1[Workspace, DocNode]

class PdfExtractorException(message: String) extends Exception(message)


