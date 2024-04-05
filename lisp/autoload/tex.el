;;; orgtbl-to-latex-booktabs
;; =========orgtbl-to-latex-booktabs=========
;;;###autoload
(defun orgtbl-to-latex-booktabs (table params)
  (let* ((alignment (mapconcat (lambda (x) (if x "r" "l"))
                               org-table-last-alignment ""))
         (params2
          (list
           :tstart (concat "\\begin{tabular}{" alignment "}\n\\toprule")
           :tend "\\bottomrule\n\\end{tabular}"
           :lstart "" :lend " \\\\" :sep " & "
           :efmt "%s\\,(%s)" :hline "\\midrule")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))
;; =========orgtbl-to-latex-booktabs=========
;;; eval-math-with-calc
;; ============eval-math-with-calc============
;;;###autoload
(defun eval-math-with-calc (&optional arg)
  "Calculate the region and display the result in the echo area.
With prefix ARG non-nil, insert the result at the end of region.
To solve equation, solve([x+y=3, x-y=1], [x,y])."
  (interactive "P")
  (let* ((beg (if (region-active-p)
                  (region-beginning)
                (line-beginning-position)))
         (end (if (region-active-p)
                  (region-end)
                (line-end-position)))
         (expr (buffer-substring-no-properties beg end))
         (result (if (eq major-mode 'LaTeX-mode)
                     (calc-eval `(,expr
                                  calc-language latex
                                  calc-prefer-frac t
                                  calc-angle-mode rad))
                   (calc-eval expr))))
    (if (null arg)
        (message "%s" result)
      (kill-region beg end)
      (insert result))))
;; ============eval-math-with-calc============
