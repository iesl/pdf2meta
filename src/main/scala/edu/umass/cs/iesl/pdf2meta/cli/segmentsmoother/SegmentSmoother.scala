package edu.umass.cs.iesl.pdf2meta.cli.segmentsmoother

import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter.ClassifiedRectangles

trait SegmentSmoother extends Function1[ClassifiedRectangles, ClassifiedRectangles]
