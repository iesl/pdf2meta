#!/usr/bin/env python

import sys, os
from os import path
import codecs, locale

#print sys.path

# from collectionutil import unique, asbag, classname

from pdfminer.cmapdb import CMapDB
from pdfminer.converter import PDFConverter
from pdfminer.layout import LAParams
from pdfminer.layout import LTContainer, LTPage, LTText, LTLine, LTRect
from pdfminer.layout import LTFigure, LTImage, LTChar, LTTextLine
from pdfminer.layout import LTTextBox, LTTextBoxVertical, LTTextGroup, LTAnon
from pdfminer.pdfcolor import LITERAL_DEVICE_GRAY, LITERAL_DEVICE_RGB
from pdfminer.pdfdevice import PDFDevice, PDFTextDevice
from pdfminer.pdfdevice import PDFDevice, TagExtractor
from pdfminer.pdffont import PDFUnicodeNotDefined
from pdfminer.pdfinterp import PDFResourceManager, PDFPageInterpreter, process_pdf
from pdfminer.pdfparser import PDFDocument, PDFParser
from pdfminer.pdftypes import LITERALS_DCT_DECODE
from pdfminer.utils import apply_matrix_pt, mult_matrix
from pdfminer.utils import enc, bbox2str
#, create_bmp
# from rexapdfminer import RexaXMLConverter

# order preserving uniq-ify
def unique(seq, ident=None):
    if ident is None: ident = lambda x: x
    seen, result = set(), []
    for item in seq:
        marker = ident(item)
        if marker not in seen:
            seen.add(marker)
            result.append(item)
    return result

class RexaXMLConverter(PDFConverter):

    def __init__(self, rsrcmgr, outfp, codec='utf-8', pageno=1, laparams=None):
        PDFConverter.__init__(self, rsrcmgr, codecs.getwriter(locale.getpreferredencoding())(outfp), codec=codec, pageno=pageno, laparams=laparams)
        self.document_root = {'tag': 'pages'}
        self.stack = []
        self.taglists = dict([(t, []) for t in "pages page textbox textline textbox page rect polygon line figure curve textgrouplrtb textgrouptbrl".split()])
        self.hasrun = False
        self.interesting_attributes = 'tag index id bbox rotate pageid text size sizes orientation fontname fontnames fontstyles fonts bboxes length height width'.split(' ')
        self.open()

    def bbox2str(self, (x0,y0,x1,y1)): return '%.2f,%.2f,%.2f,%.2f' % (x0, y0, x1, y1)

    def print_stack(self): print 'stack', self.stack[-1] if self.stack else '[]'
    def push(self, item): self.stack.append(item)
    def pop(self): return self.stack.pop()
    def top(self): return self.stack[-1]

    def write(self, s): self.outfp.write(s)
    def writeln(self, s): self.outfp.write(s + "\n")


    def ltitem2dict(self, item):
        ## print("item", dir(item))
        tagname = self.tagname(item)
        orientation = None
        if tagname.endswith('vertical'):
            orientation = 'vertical'
            tagname = tagname[:-len('vertical')]
        elif tagname.endswith('horizontal'):
            tagname = tagname[:-len('horizontal')]

        d = {'tag': tagname}

        # use this to put include pdfminer's 'anon' tags, which include extra spaces and newlines
        #   to mimic wide word spacing and newlines at textline endings
        if tagname == 'anon': d = {'tag': 'char', 'bbox': (0,0,0,0), 'fontname': '?'}
        if tagname == 'undefcodepoint': d = {'tag': 'char'}

        if orientation:
            d.update(orientation=orientation)

        try:
            d.update(text=item.get_text())
        except:
            pass

        d.update([(k,v) for k,v in item.__dict__.items() if k in self.interesting_attributes])

        return d

    def tagname(self, obj):
        """ transform class name e.g.,  LTTextLine -> textline """
        return type(obj).__name__[2:].lower()

    def link_to_parent(self, node):
        """ attach node to tree w/parent at stack.top()"""
        parent = self.top()
        node['parent'] = parent
        parent.setdefault('children', []).append(node)

    def link_to_taglist(self, node):
        """ store all nodes of a given tagname into a list """
        self.taglists.setdefault(node['tag'], []).append(node)

    def visit(self, item):
        """
        Depth 1st traversal turning pdfminer tree of items into tree of python dicts
        Also appends nodes to lists according to node['tag'] value
        """
        node = self.ltitem2dict(item)
        self.link_to_parent(node)
        self.link_to_taglist(node)
        self.push(node)
        try:
            for child in item:
                self.visit(child)
        except TypeError:
            pass # ok, not all items have children
        self.pop()

    ### Various dict formatting functions for dev/debugging output
    def of_interest(self, x):
        return x not in ['parent', 'children', 'tag'] and x in self.interesting_attributes

    def format_dict_abbrev(self, node):
        return node['tag'] +': '+ ' '.join(["%s: %s" % (k,'..') for (k,v) in node.items() if self.of_interest(k)]), None, None

    def format_dict_full(self, node):
        return node['tag'] +': '+ ' '.join(["%s: %s" % (k,v) for (k,v) in node.items() if self.of_interest(k)]), None, None

    def format_dict_fuller(self, node):
        return node['tag'] +': '+ ' '.join(["%s: %s" % (k,v) for (k,v) in node.items()]), None, None


    def format_dict_xml(self, node):
        """ return triple of formatted xml sections: begin-tag, text-content, end-tag """
        tagformats = {
            'char'     : (None, None, None),
            'anon'     : (None, None, None),
            'pages'    : (u"<{tag} fontnames='{fontnames}' fontstyles='{fontstyles}'>", None, u"</{tag}>"),
            'page'     : (u"<{tag} pageid='{pageid}' bbox='{bbox}' rotate='{rotate}'>", None, u"</{tag}>"),
            'textline' : (u"<{tag} length='{length}' fonts='{fonts}' bbox='{bbox}' bboxes='{bboxes}'>{text}</{tag}>", None, None),
            'textbox'  : (u"<{tag} index='{index}' bbox='{bbox}'>", None, u"</{tag}>"),
            'line'     : (u"<{tag} bbox='{bbox}'/>", None, None),
            'polygon'  : (u"<{tag} bbox='{bbox}'/>", None, None),
            'rect'     : (u"<{tag} bbox='{bbox}'/>", None, None),
            'curve'    : (u"<{tag} curve='{bbox}'/>", None, None),
            'image'    : (u"<{tag} width='{width}' height='{height}'/>", None, None),
            'figure'   : (u"<{tag} bbox='{bbox}'>", None, u"</{tag}>"),
            'undefcodepoint'  : (None, None, None),
            }
        preformat, midformat, postformat = tagformats[node['tag']]

        pre  = preformat.format(**node) if preformat else None
        mid  = midformat.format(**node) if midformat else None
        post = postformat.format(**node) if postformat else None
        return pre, mid, post

    def print_tree(self, node, level=0, formatter=format_dict_abbrev):
        """ write out tree using provided formatter (xml or dict..)"""
        indent = (u'  ' * level)
        pre, mid, post = formatter(node)
        if pre: self.writeln("%s%s" % (indent, pre))
        if mid: self.writeln("%s  %s" % (indent, mid))
        if 'children' in node:
            for c in node['children']:
                self.print_tree(c, level+1, formatter)
        if post: self.writeln("%s%s" % (indent, post))


    def ancestor(self, node, tagname):
        """ walk tree up until node with tagname is found """
        if not node or node['tag'] == tagname:
            return node
        if 'parent' in node:
            return self.ancestor(node['parent'], tagname)
        return None

    def bubbleup_chars(self):
        for char in self.taglists['char']:
            # print("char", char)
            textline = self.ancestor(char, 'textline')
            if textline:
                pages = self.ancestor(char, 'pages')
                textline.setdefault('bboxes', []).append(char['bbox'])
                textline.setdefault('chars', []).append(char["text"])
                textline.setdefault('fontnames', []).append(char['fontname'])
                pages.setdefault('fontnames', []).append(char['fontname'])

    def reformat_bboxes(self):
        for node in self.taglists['textline']:
            bboxes = ' '.join([self.bbox2str(bbox) for bbox in node['bboxes']])
            node['bboxes'] = bboxes

        def reformat_bbox(tag):
            for node in self.taglists[tag]:
                node['bbox'] = self.bbox2str(node['bbox'])

        for tag in "textline textbox page rect polygon line figure curve".split(' '):
            reformat_bbox(tag)


    def normalize_text_usecharnodes(self):
        """
        Pdfminer inserts extra spaces into textlines to simulate
        the extra intra-word spacing caused by, e.g., column justification.
        This method will format the text without those extra spaces.
        See normalize_text_usetextline() to format the text with extra spaces.
        """
        for textline in self.taglists['textline']:
            text = ''.join(textline['chars'])
            bboxes = textline['bboxes']
            fonts = textline['fontnames']
            while text and text[-1] in " ":
                text = text[:-1]
                bboxes = bboxes[:-1]
                fonts = fonts[:-1]

            textline["text"] = text
            textline['bboxes'] = bboxes
            textline['fontnames'] = fonts

    def normalize_text_usetextline(self):
        for textline in self.taglists['textline']:
            text = textline["text"]
            bboxes = textline['bboxes']
            fonts = textline['fontnames']
            deleted = 0
            while text and text[-1] == u"\n":
                deleted += 1
                text = text[:-1]
                bboxes = bboxes[:-1]
                fonts = fonts[:-1]
            while text and text[-1] in " ":
                deleted += 1
                text = text[:-1]
                bboxes = bboxes[:-1]
                fonts = fonts[:-1]

            textline["text"] = text
            textline['bboxes'] = bboxes
            textline['fontnames'] = fonts

    def encode_text(self):
        for textline in self.taglists['textline']:
            textline["text"] = enc(textline["text"])

    def include_textline_stats(self):
        for textline in self.taglists['textline']:
            ccs, bbs, ffs = len(textline["text"]), len(textline['bboxes']), len(textline['fontnames'])
            textline['length'] = ccs
            if not (ccs == bbs == ffs):
                textline['length'] = "%s,%s,%s" % (ccs, bbs, ffs)

    def reformat_fontnames(self):
        """
        replace font name/styles with numerical representation
        """
        pages = self.document_root
        uniqfonts = unique(pages['fontnames'])
        ffonts = [font.split(',') for font in uniqfonts]
        fnames = unique([f[0] for f in ffonts])
        fstyles = unique([f[1] for f in ffonts if len(f)==2])
        pages['fontnames'] = ' '.join(fnames)
        pages['fontstyles'] = ' '.join(fstyles)
        for textline in self.taglists['textline']:
            textline.setdefault('fontnames', [])
            fontspecs = []
            for font in textline['fontnames']:
                if ',' in font:
                    font, style = font.strip().split(',')
                    ifont = fnames.index(font)
                    istyle = fstyles.index(style)
                    fontspec = "%d,%d" % (ifont, istyle)
                else:
                    ifont = fnames.index(font)
                    fontspec = "%d" % (ifont)
                fontspecs.append(fontspec)

            if len(unique(fontspecs))==1:
                textline['fonts'] = fontspecs[0]
            else:
                textline['fonts'] = ' '.join(fontspecs)

    ### open, receive, close document formatting/writing
    def open(self):
        self.push(self.document_root)

    def receive_layout(self, ltpage):
        self.visit(ltpage)

    def close(self):
        self.bubbleup_chars()
        self.normalize_text_usetextline()
        self.include_textline_stats()
        self.reformat_bboxes()
        self.reformat_fontnames()
        self.encode_text()
        self.writeln(u'<?xml version="1.0" encoding="%s" ?>' % self.codec)
        self.print_tree(self.document_root, formatter=self.format_dict_xml)


def parse_args():
    from optparse import OptionParser
    parser = OptionParser("usage: %prog [options]")
    for s in [["--file",      "input file",     "filename",   "store",       None],
		["--outdir",      "output directory",     "outdir",   "store",       "/tmp/pdfminer"]]:
        parser.add_option(s[0], help=s[1], dest=s[2], action=s[3], default=s[4])

    try:
        return parser.parse_args()
    except:
        print parser.format_help()
        exit(1)

def pdfminer2txt(pdf):
    pdfpath, pdffile = path.split(pdf)
    pdfdir = pdf + ".d"
    pdfminerdir = path.join(pdfdir, "pdfminer")
    _mkdirs(pdfminerdir)
    outputfile = path.join(pdfminerdir, _fmt(locals(), "{pdffile}.pdfminer"))
    print "extracting", pdf, 'to', outputfile
    extracttext(pdf, outputfile)

def create_output_path(pdfpath, outdir):
    pdfroot, pdffile = path.split(pdfpath)
    #pdfdir = pdfpath + ".d"
    #pdfminerdir = path.join(pdfdir, "pdfminer")
    #pdfminerdir = path.join(outdir, pdfminerdir)
    #try:
    #    os.makedirs(pdfminerdir)
    #except OSError:
    #    print "error creating output dirs"
    #    exit(1)
    pdfminerdir = os.getcwd()
    result = path.join(pdfminerdir, pdffile + ".pdfminer.xml")
    print "writing PDFMiner output to ", result
    return result

def main():
    opts, args = parse_args()
    pdf = opts.filename
    output_path = create_output_path(pdf, opts.outdir)
    print "extracting", pdf, 'to', output_path
    extracttext(pdf, output_path)

def extracttext(pdffile, outfile):
    debug = 0
    password = ''
    pagenos = set()
    maxpages = 0
    layoutmode = 'normal'
    codec = 'utf-8'
    pageno = 1
    scale = 1
    showpageno = True
    laparams = LAParams()
    #
    CMapDB.debug = debug
    PDFResourceManager.debug = debug
    PDFDocument.debug = debug
    PDFParser.debug = debug
    PDFPageInterpreter.debug = debug
    PDFDevice.debug = debug
    #

    if outfile:
        outfp = file(outfile, mode='w')
    else:
        outfp = sys.stdout
    rsrcmgr = PDFResourceManager()
    device = RexaXMLConverter(rsrcmgr, outfp, codec=codec, laparams=laparams)

    with file(pdffile, 'rb') as fp:
        process_pdf(rsrcmgr, device, fp, pagenos, maxpages=maxpages, password=password, check_extractable=True)

    device.close()
    if outfile:
        outfp.close()

if __name__ == '__main__':
    main()

