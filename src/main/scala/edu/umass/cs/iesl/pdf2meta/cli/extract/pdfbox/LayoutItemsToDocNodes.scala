package edu.umass.cs.iesl.pdf2meta.cli.extract.pdfbox

import org.apache.pdfbox.pdmodel.graphics.PDGraphicsState
import org.apache.pdfbox.pdmodel.{PDResources, PDPage}
import org.apache.pdfbox.pdmodel.graphics.xobject.{PDXObject, PDXObjectImage, PDXObjectForm}
import org.apache.pdfbox.cos.{COSBase, COSName, COSStream}
import org.apache.pdfbox.util._
import org.apache.pdfbox.util.Matrix
import edu.umass.cs.iesl.pdf2meta.cli.extract.pdfbox.pagedrawer.GraphicsAwarePDFStreamEngine
import java.awt.image.BufferedImage
import java.awt.geom.{Point2D, Area, GeneralPath, AffineTransform}
import java.awt.{Dimension, Stroke}
import collection.mutable
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel._
import edu.umass.cs.iesl.pdf2meta.cli.pagetransform.SimpleLineMerger
import org.apache.pdfbox._
import edu.umass.cs.iesl.pdf2meta.cli.layoutmodel.Page

/**
 * Combine needed elements from PDFTextStripper, PrintImageLocations, and PageDrawer
 */
// following adapted from
// package org.apache.pdfbox.util.operator.pagedrawer
/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
/**
 * Implementation of content stream operator for page drawer.
 *
 * @author <a href="mailto:ben@benlitchfield.com">Ben Litchfield</a>
 * @version $Revision: 1.3 $
 */
abstract class LayoutItemsToDocNodes
		extends PDFStreamEngine(ResourceLoader.loadProperties("edu/umass/cs/iesl/pdf2meta/cli/extract/CustomPageDrawer.properties", true)) with
		        GraphicsAwarePDFStreamEngineImpl
		//extends PDFTextStripper(ResourceLoader.loadProperties("org/apache/pdfbox/resources/PageDrawer.properties", true)) with Logging
	{
	val nodes       = new mutable.ListBuffer[DocNode]
	var lineCounter = 0

	/**
	 * Because of the way PDFBox works, we have to accumulate DocNodes here and then dump them all out at the end
	 * @return
	 */
	def getRootDocNode(): DocNode =
		{
		// ** grouping characters into words, and sorting by X and Y, is done here, not part of the main pipeline
		// ** and estimating spaces ??
		val chars = new InternalDocNode("" + docPage.pagenum, nodes, None, None)

		//val lineMerger = new ContinuousLineMerger
		//val lineMerger = new SimpleSortingLineMerger
		val lineMerger = new SimpleLineMerger
		val lines = lineMerger.applyLocalOnly(chars).get
		//chars
		val pageNode = new PageNode("" + docPage.pagenum, lines, None, None,docPage)
		pageNode
		}

	//super(ResourceLoader.loadProperties("org/apache/pdfbox/resources/PDFTextStripper.properties", true))
	/**
	 * A method provided as an event interface to allow a subclass to perform
	 * some specific functionality when text needs to be processed.
	 *
	 * @param text The text to be processed
	 */
	override def processTextPosition(text: TextPosition)
		{
		val rectOnPage = new RectangleOnPage()
			{
			val page   = docPage
			val bottom = page.rectangle.height - text.getY
			// pdfbox reports origin in upper left
			val left  = text.getX
			val right = left + text.getWidth
			require(text.getHeight >= 0)
			val top = bottom + text.getHeight
			}

		if (rectOnPage.isReal)
			{
			nodes += new PdfBoxTextAtom("" + docPage.pagenum + "." + lineCounter, text.getCharacter, text.getFont.getBaseFont, text.getFontSizeInPt,
			                            rectOnPage,
			                            text.getIndividualWidths)
			lineCounter += 1;

			/*System.out.println("String[" + text.getXDirAdj() + "," +
											   text.getYDirAdj() + " fs=" + text.getFontSize() + " xscale=" +
											   text.getXScale() + " height=" + text.getHeightDir() + " space=" +
											   text.getWidthOfSpace() + " width=" +
											   text.getWidthDirAdj() + "]" + text.getCharacter());*/
			}
		}

	private val INVOKE_OPERATOR = "Do";

	// copied from PrintImageLocations
	/**
	 * This is used to handle an operation.
	 *
	 * @param operator The operation to perform.
	 * @param arguments The list of arguments.
	 *
	 * @throws IOException If there is an error processing the operation.
	 */
	override def processOperator(operator: PDFOperator, //operation: java.lang.String,
	                             arguments: java.util.List[COSBase])
		{
		val operation: String = operator.getOperation
		//logger.warn(operation)
		if (INVOKE_OPERATOR == operation)
			{
			val objectName: COSName = arguments.get(0).asInstanceOf[COSName]
			val xobjects: java.util.Map[String, PDXObject] = getResources.getXObjects.asInstanceOf[java.util.Map[String, PDXObject]]
			val xobject: PDXObject = xobjects.get(objectName.getName).asInstanceOf[PDXObject]
			if (xobject.isInstanceOf[PDXObjectImage])
				{
				val image: PDXObjectImage = xobject.asInstanceOf[PDXObjectImage]
				val page: PDPage = getCurrentPage
				val imageWidth: Int = image.getWidth
				val imageHeight: Int = image.getHeight
				val pageHeight: Float = page.getMediaBox.getHeight
				System.out.println("*******************************************************************")
				System.out.println("Found image [" + objectName.getName + "]")
				val ctmNew: Matrix = getGraphicsState.getCurrentTransformationMatrix
				val yScaling: Float = ctmNew.getYScale
				var angle: Float = Math.acos(ctmNew.getValue(0, 0) / ctmNew.getXScale).asInstanceOf[Float]
				if (ctmNew.getValue(0, 1) < 0 && ctmNew.getValue(1, 0) > 0)
					{
					angle = (-1) * angle
					}
				ctmNew.setValue(2, 1, (pageHeight - ctmNew.getYPosition - Math.cos(angle) * yScaling).asInstanceOf[Float])
				ctmNew.setValue(2, 0, (ctmNew.getXPosition - Math.sin(angle) * yScaling).asInstanceOf[Float])
				ctmNew.setValue(0, 1, (-1) * ctmNew.getValue(0, 1))
				ctmNew.setValue(1, 0, (-1) * ctmNew.getValue(1, 0))
				val ctmAT: AffineTransform = ctmNew.createAffineTransform
				ctmAT.scale(1f / imageWidth, 1f / imageHeight)
				var imageXScale: Float = ctmNew.getXScale
				var imageYScale: Float = ctmNew.getYScale
				System.out.println("position = " + ctmNew.getXPosition + ", " + ctmNew.getYPosition)
				System.out.println("size = " + imageWidth + "px, " + imageHeight + "px")
				System.out.println("size = " + imageXScale + ", " + imageYScale)
				imageXScale /= 72f
				imageYScale /= 72f
				System.out.println("size = " + imageXScale + "in, " + imageYScale + "in")
				imageXScale *= 25.4f
				imageYScale *= 25.4f
				System.out.println("size = " + imageXScale + "mm, " + imageYScale + "mm")
				System.out.println
				}
			else if (xobject.isInstanceOf[PDXObjectForm])
				{
				getGraphicsStack.push(getGraphicsState.clone.asInstanceOf[PDGraphicsState])
				val page: PDPage = getCurrentPage
				val form: PDXObjectForm = xobject.asInstanceOf[PDXObjectForm]
				val invoke: COSStream = form.getCOSObject.asInstanceOf[COSStream]
				var pdResources: PDResources = form.getResources
				if (pdResources == null)
					{
					pdResources = page.findResources
					}
				val matrix: Matrix = form.getMatrix
				if (matrix != null)
					{
					val xobjectCTM: Matrix = matrix.multiply(getGraphicsState.getCurrentTransformationMatrix)
					getGraphicsState.setCurrentTransformationMatrix(xobjectCTM)
					}
				processSubStream(page, pdResources, invoke)
				setGraphicsState(getGraphicsStack.pop.asInstanceOf[PDGraphicsState])
				}
			}
		else
			{
			val bogus = super.processOperator(operator, arguments)
			}
		Unit
		}

	def strokePath
		{
		System.out.println("*******************************************************************")

		System.out.println("Found path: " + linePath.getBounds + " " + linePath.toString)
		}

	def fillPath(windNonZero: Int)
		{strokePath}

	def SHFill(ShadingName: COSName)
		{strokePath}
	}

trait GraphicsAwarePDFStreamEngineImpl extends GraphicsAwarePDFStreamEngine
	{
	val pageSize: Dimension
	val pdpage  : PDPage
	val docPage : Page

	var linePath: GeneralPath = new GeneralPath()

	var stroke: Stroke = null

	def getStroke: Stroke = stroke

	def setStroke(s: Stroke) =
		{stroke = s}

	/**
	 * Set the line path to draw.
	 *
	 * @param newLinePath Set the line path to draw.
	 */
	def setLinePath(newLinePath: GeneralPath): Unit =
		{
		if (linePath == null || linePath.getCurrentPoint == null)
			{
			linePath = newLinePath
			}
		else
			{
			linePath.append(newLinePath, false)
			}
		}

	def getLinePath = linePath

	def drawImage(image: BufferedImage, transform: AffineTransform)
		{}

	/**
	 * Get the page that is currently being drawn.
	 *
	 * @return The page that is being drawn.
	 */
	def getPage: PDPage = pdpage

	/**
	 * Get the size of the page that is currently being drawn.
	 *
	 * @return The size of the page that is being drawn.
	 */
	def getPageSize: Dimension = pageSize

	def getGraphicsState(): PDGraphicsState

	def setClippingPath(windingRule: Int)
		{
		var graphicsState: PDGraphicsState = getGraphicsState();
		var clippingPath: GeneralPath = getLinePath.clone().asInstanceOf[GeneralPath];
		clippingPath.setWindingRule(windingRule);
		// If there is already set a clipping path, we have to intersect the new with the existing one
		if (graphicsState.getCurrentClippingPath() != null)
			{
			val currentArea: Area = new Area(getGraphicsState().getCurrentClippingPath());
			val newArea: Area = new Area(clippingPath);
			currentArea.intersect(newArea);
			graphicsState.setCurrentClippingPath(currentArea);
			}
		else
			{
			graphicsState.setCurrentClippingPath(clippingPath);
			}
		getLinePath.reset();
		}

	def transformedPoint(x: Float, y: Float): java.awt.geom.Point2D.Float =
		{
		var position: Array[Float] = Array(x, y);
		getGraphicsState().getCurrentTransformationMatrix().createAffineTransform().transform(position, 0, position, 0, 1);
		position(1) = fixY(position(1));
		return new Point2D.Float(position(0), position(1));
		}

	def fixY(y: Float) = pageSize.getHeight.toFloat - y;
	}

