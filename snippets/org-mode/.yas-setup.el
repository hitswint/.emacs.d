(require 'yasnippet)

(defvar org-beamer-available-themes (mapcar #'(lambda (x) (substring-no-properties (file-name-sans-extension x) 11 nil))
                                            (directory-files "/usr/share/texlive/texmf-dist/tex/latex/beamer" nil "^beamertheme.+\\.sty")))
(defvar org-beamer-available-colorthemes (mapcar #'(lambda (x) (substring-no-properties (file-name-sans-extension x) 16 nil))
                                                 (directory-files "/usr/share/texlive/texmf-dist/tex/latex/beamer" nil "^beamercolortheme.+\\.sty")))

(defun tikz-second-arg (type)
  (cond
   ((member type '("--" "edge[->,bend left]" "grid" "rectangle" "parabola" "sin" "cos"))
    (swint-cursor-localtion))
   ((equal type "arc")
    "0:180:10mm")
   ((equal type "circle")
    "10mm")
   ((equal type "ellipse")
    "10mm and 20mm")))
