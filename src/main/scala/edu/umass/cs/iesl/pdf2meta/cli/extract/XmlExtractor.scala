package edu.umass.cs.iesl.pdf2meta.cli.extract

import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode
import edu.umass.cs.iesl.scalacommons.Workspace


trait XmlExtractor extends Function1[Workspace, DocNode]
