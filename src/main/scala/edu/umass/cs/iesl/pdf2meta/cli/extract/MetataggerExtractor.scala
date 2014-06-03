package edu.umass.cs.iesl.pdf2meta.cli.extract
import edu.umass.cs.iesl.scalacommons.Workspace
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.{DocNode, InternalDocNode}
import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter.ClassifiedRectangles

/**
 * Created by klimzaporojets on 5/31/14.
 */
trait MetataggerExtractor extends Function1[Workspace, (DocNode, ClassifiedRectangles)]

class MetataggerExtractorException(message: String) extends Exception(message)
