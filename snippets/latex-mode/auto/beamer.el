(TeX-add-style-hook "beamer"
 (lambda ()
    (LaTeX-add-labels
     "eq:1")
    (TeX-add-symbols
     '("icon" 1))
    (TeX-run-style-hooks
     "verbatim"
     "pgfshade"
     "pgfheaps"
     "pgfautomata"
     "pgfnodes"
     "pgfarrows"
     "pgf"
     "inputenc"
     "utf8"
     "ucs"
     "wasysym"
     "thumbpdf"
     "dcolumn"
     "colortbl"
     "xmpmulti"
     "amssymb"
     "amsmath"
     "ctex"
     "url"
     "subfigure"
     "graphicx"
     "10pt"
     "latex2e"
     "beamer10"
     "")))

