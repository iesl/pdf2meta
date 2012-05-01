package edu.umass.cs.iesl.pdf2meta.cli.extract.pdfbox

import edu.umass.cs.iesl.scalacommons.Workspace
import com.weiglewilczek.slf4s.Logging
import org.apache.pdfbox.exceptions.InvalidPasswordException
import org.apache.pdfbox.pdmodel.{PDPage, PDDocument}
import collection.mutable.Buffer
import edu.umass.cs.iesl.pdf2meta.cli.extract.{PdfExtractorException, PdfExtractor}
import java.awt.Dimension
import org.apache.pdfbox.pdmodel.common.{PDRectangle, PDStream}
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
class PdfBoxExtractor extends PdfExtractor with Logging with Function1[Workspace, DocNode]
	{
	def apply(v1: Workspace) =
		{
		val document: PDDocument = PDDocument.load(v1.file.bufferedInput())
		try
		{
		if (document.isEncrypted())
			{
			try
			{
			document.decrypt("")
			}
			catch
			{
			case e: InvalidPasswordException => throw new PdfExtractorException("Error: Document is encrypted with a password.")
			}
			}

		import collection.JavaConversions._
		val allPagesB: Buffer[_] = document.getDocumentCatalog.getAllPages;
		val allPages: Seq[PDPage] = allPagesB.toSeq.asInstanceOf[Seq[PDPage]]
		//asInstanceOf[Seq[PDPage]]


		val allPageNodes = for ((thepage: PDPage, pageNum : Int) <- allPages.zipWithIndex) yield
			{
			//logger.info("Processing page: " + page.);
			val contents: PDStream = thepage.getContents
			val mbox: PDRectangle = thepage.getMediaBox

			// note that zipWithIndex naturally started at 0, but page numbering should be 1-based.
			val pPage = new Page(pageNum+1, new RealRectangle(mbox.getLowerLeftX,mbox.getLowerLeftY,mbox.getUpperRightX,mbox.getUpperRightY))

			if (contents != null)
				{
				val printer: LayoutItemsToDocNodes = new
					{
					val pdpage= thepage
					private val box: PDRectangle = pdpage.findMediaBox()
					val pageSize =  new Dimension( box.getWidth.asInstanceOf[Int], box.getHeight.asInstanceOf[Int]);
					val docPage = pPage
					} with LayoutItemsToDocNodes

				printer.processStream(thepage, thepage.findResources(), thepage.getContents().getStream());
				Some(printer.getRootDocNode());
				}
			else None
			}

		new InternalDocNode("root", allPageNodes.flatten, None, None)
		}
		finally
			{
			document.close();
			}
		}
	}


class PdfBoxTextAtom(override val id: String, override val theText: String,  font: String,  fontHeight: Float, val rect: RectangleOnPage,
                     val charWidths : Array[Float])
		extends TextAtom(id, theText,Some(rect))
	{
	override lazy val dominantFont : Option[FontWithHeight]             = Some(new FontWithHeight(font, fontHeight))
	override lazy val allFonts     : Seq[(FontWithHeight, Int)] = Seq((dominantFont.get, theText.length))

	override def partitionByFont(boxorder: Ordering[RectangularOnPage]) = Seq(this)

	//def fontSize = fontHeight

	override def create(childrenA: Seq[DocNode]) =
		{
		assert(childrenA.isEmpty)
		this
		}
	}
