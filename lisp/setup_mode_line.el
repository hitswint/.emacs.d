;; ================smart-mode-line===================
;; 设定theme为dark/light/respectful/automatic
(sml/setup)
(sml/apply-theme nil)
(setq column-number-mode t)
(cond
 (is-lin
  (add-to-list 'sml/replacer-regexp-list '("^~/Documents/MATLAB/" ":M:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/.emacs.d/" ":E:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/linux/" ":L:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/book/" ":B:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/papers/" ":P:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/org/" ":O:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/tex/" ":T:")))
 (is-win
  (add-to-list 'sml/replacer-regexp-list '("^c:/Users/swint/Documents/MATLAB/" ":M:"))
  (add-to-list 'sml/replacer-regexp-list '("^c:/Users/swint/.emacs.d/" ":E:"))
  (add-to-list 'sml/replacer-regexp-list '("^c:/Users/swint/linux/" ":L:"))
  (add-to-list 'sml/replacer-regexp-list '("^c:/Users/swint/book/" ":B:"))
  (add-to-list 'sml/replacer-regexp-list '("^c:/Users/swint/papers/" ":P:"))
  (add-to-list 'sml/replacer-regexp-list '("^c:/Users/swint/org/" ":O:"))
  (add-to-list 'sml/replacer-regexp-list '("^c:/Users/swint/tex/" ":T:"))
  (add-to-list 'sml/replacer-regexp-list '("^c:/Users/swint/" "~/"))))
;; only hide minor mode
(add-to-list 'sml/hidden-modes " yas")
(add-to-list 'sml/hidden-modes " iImg")
(add-to-list 'sml/hidden-modes " Fill")
(add-to-list 'sml/hidden-modes " Abbrev")
(add-to-list 'sml/hidden-modes " AC")
(add-to-list 'sml/hidden-modes " Outl")
(add-to-list 'sml/hidden-modes " zx")
(add-to-list 'sml/hidden-modes " Ref")
(add-to-list 'sml/hidden-modes " Undo-Tree")
(add-to-list 'sml/hidden-modes " Image")
(add-to-list 'sml/hidden-modes " Paredit")
(add-to-list 'sml/hidden-modes " Isearch")
(add-to-list 'sml/hidden-modes " OCDL")
(add-to-list 'sml/hidden-modes " God")
(add-to-list 'sml/hidden-modes " Par-")
(add-to-list 'sml/hidden-modes " SliNav")
(add-to-list 'sml/hidden-modes " Anzu")
(add-to-list 'sml/hidden-modes " drag")
(add-to-list 'sml/hidden-modes " Helm")
(add-to-list 'sml/hidden-modes " $")
(add-to-list 'sml/hidden-modes " MRev")
(add-to-list 'sml/hidden-modes " OrgTbl")
(add-to-list 'sml/hidden-modes " Isearch[拼音]")
(add-to-list 'sml/hidden-modes " =>")
(add-to-list 'sml/hidden-modes " Guide")
(add-to-list 'sml/hidden-modes " wr")
(add-to-list 'sml/hidden-modes " HelmGtags")
(add-to-list 'sml/hidden-modes " FA")
(add-to-list 'sml/hidden-modes " hs")
(add-to-list 'sml/hidden-modes " WK")
(add-to-list 'sml/hidden-modes " ARev")
(add-to-list 'sml/hidden-modes " company")
(add-to-list 'sml/hidden-modes " Spell")
(add-to-list 'sml/hidden-modes " mLaTeX")
;; ================smart-mode-line===================
;; ====================abbrev-for-mode-line========================
(defvar mode-line-cleaner-alist
  '((dired-mode . "Dr")
    (octave-mode . "Oc")
    (gnuplot-mode . "Gp")
    (LaTeX-mode . "Tx")                ;找不到LaTeX/FMP代表的major mode
    (emacs-lisp-mode . "El")
    (inferior-octave-mode . "Ic")
    (lisp-interaction-mode . "Li"))
  "Alist for `clean-mode-line'.
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")
(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))
(add-hook 'after-change-major-mode-hook 'clean-mode-line)
;; ====================abbrev-for-mode-line========================
;; =================Lisp Interaction mode===================
;; Deal with the problems caused by using eval-print-last-sexp(C-j) in scratch buffer
(define-key lisp-interaction-mode-map (kbd "C-j") nil)
;; =================Lisp Interaction mode===================
;; ================nyan-mode=================
(nyan-mode t)
(setq nyan-bar-length 20)
;; ================nyan-mode=================
(provide 'setup_mode_line)
