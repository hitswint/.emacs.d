# This shows how to use xelatex (http://en.wikipedia.org/wiki/XeTeX)
# with latexmk.  Xelatex uses Unicode and "supporting modern font
# technologies such as OpenType or Apple Advanced Typography.
#
#   WARNING: latexmk.pl is changing, and plans to have more internal
#     support for xelatex.
#
# Since xelatex only produces pdf files, it is a replacement for
# pdflatex.  To make it your default typesetting engine within latexmk
# you will not only need to set the $pdflatex variable to require the
# use of xelatex, but also to turn on production of pdf files and to
# turn off the production of dvi and ps files, as in the following
# code:


# $pdflatex = 'xelatex -interaction=nonstopmode %O %S';
# $pdflatex = 'pdflatex -interaction=nonstopmode %O %S';
# $postscript_mode = $dvi_mode = 0;
# $preview_continuous_mode = 1;
$pdf_mode = 1;
$pdf_update_method = 2;
$pdf_previewer = "start llpp %O %S";
