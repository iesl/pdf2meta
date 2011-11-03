package edu.umass.cs.iesl.pdf2meta.cli.segmentsmoother

import edu.umass.cs.iesl.pdf2meta.cli.coarsesegmenter.ClassifiedRectangles

trait SegmentSmoother extends Function1[ClassifiedRectangles, ClassifiedRectangles]
/*
class GlobalRulesSegmentSmoother extends SegmentSmoother {
def apply(v1: ClassifiedRectangles) : ClassifiedRectangles = {

}
}
*/
