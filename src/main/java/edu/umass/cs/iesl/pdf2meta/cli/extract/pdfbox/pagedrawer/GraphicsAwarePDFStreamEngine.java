package edu.umass.cs.iesl.pdf2meta.cli.extract.pdfbox.pagedrawer;

import org.apache.pdfbox.cos.COSName;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDResources;
import org.apache.pdfbox.pdmodel.graphics.PDGraphicsState;
import sun.java2d.pipe.Region;

import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;

/**
 * Poor design of PDFBox forces us to cut-and-paste all the operators to avoid the cast to PageDrawer in each of them
 *
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
public interface GraphicsAwarePDFStreamEngine
	{
	Point2D transformedPoint( float x1, float y1 );
	GeneralPath getLinePath();

	PDPage getPage();
	Dimension getPageSize();
	PDGraphicsState getGraphicsState();
	void drawImage( BufferedImage awtImage, AffineTransform at );
	void setClippingPath( int windNonZero );
	Stroke getStroke();
	void setStroke( Stroke basicStroke );
	void strokePath();
	void SHFill( COSName cosName );
	PDResources getResources();
	void fillPath( int windNonZero );
	void setLinePath( GeneralPath currentPath );
	}
