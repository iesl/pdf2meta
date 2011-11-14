package edu.umass.cs.iesl.pdf2meta.cli.extract

import edu.umass.cs.iesl.pdf2meta.cli.util.Workspace
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.DocNode


trait XmlExtractor extends Function1[Workspace, DocNode]
