package edu.umass.cs.iesl.pdf2meta.cli.extract.pdfbox

import org.apache.pdfbox.util.operator.OperatorProcessor

import com.weiglewilczek.slf4s.Logging
import org.apache.pdfbox.cos.COSBase
import org.apache.pdfbox.util._
import org.apache.pdfbox.util.Matrix
import java.awt.BasicStroke
import java.util.List

class StrokePath extends OperatorProcessor with Logging
	{
	/**
	 * S stroke the path.
	 * @param operator The operator that is being executed.
	 * @param arguments List
	 *
	 * @throws IOException If an error occurs while processing the font.
	 */
	def process(operator: PDFOperator, arguments: List[COSBase])
		{

		try
		{
		var drawer: LayoutItemsToDocNodes = context.asInstanceOf[LayoutItemsToDocNodes]
		var lineWidth: Float = context.getGraphicsState.getLineWidth.asInstanceOf[Float]
		var ctm: Matrix = context.getGraphicsState.getCurrentTransformationMatrix
		if (ctm != null && ctm.getXScale > 0)
			{
			lineWidth = lineWidth * ctm.getXScale
			}
		var stroke: BasicStroke = drawer.stroke.asInstanceOf[BasicStroke]
		if (stroke == null)
			{
			drawer.stroke = (new BasicStroke(lineWidth))
			}
		else
			{
			drawer.stroke = (new BasicStroke(lineWidth, stroke.getEndCap, stroke.getLineJoin, stroke.getMiterLimit, stroke.getDashArray, stroke.getDashPhase))
			}

		// don't need to actually draw anything.
		//drawer.strokePath
		System.out.println("*******************************************************************")

		System.out.println("Found path: " + drawer.linePath.getBounds)

		drawer.linePath.reset
		}
		catch
		{
		case exception: Exception =>
			{
			logger.warn("Error", exception)
			}
		}
		}

}
