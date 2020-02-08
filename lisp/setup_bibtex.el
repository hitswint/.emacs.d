;;; bibtex
;; ====================bibtex======================
(def-package! bibtex
  :after (:any ebib helm-bibtex org-ref)
  :config
  (defun bibtex-autokey-name-convert (str)
    (if (pyim-string-match-p "\\cc" str)
        (let ((str-list (pyim-hanzi2pinyin str nil nil t)))
          (if (= (length str-list) 1)
              (car str-list)
            (completing-read "Choose: " str-list)))
      (funcall 'downcase str)))
  (defun bibtex-autokey-titleword-convert (str)
    (if (pyim-string-match-p "\\cc" str)
        (let ((str-list (pyim-hanzi2pinyin-capitalize str nil nil t)))
          (if (= (length str-list) 1)
              (car str-list)
            (completing-read "Choose: " str-list)))
      (funcall 'upcase-initials str)))
  (defun bibtex-autokey-get-title/around (orig-fun &rest args)
    (let ((case-fold-search t)
          (titlestring
           (bibtex-autokey-get-field "title"
                                     bibtex-autokey-titleword-change-strings)))
      (if (string-match-p "\\cc" titlestring)
          (setq bibtex-autokey-titleword-ignore nil)
        (setq bibtex-autokey-titleword-ignore
              '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das"
                "[^[:upper:]].*" ".*[^[:upper:][:lower:]0-9].*")))
      (apply orig-fun args)))
  (defun bibtex-autokey-add_pages (key)
    (concat key "_" (bibtex-autokey-get-field "pages")))
  (advice-add 'bibtex-autokey-get-title :around #'bibtex-autokey-get-title/around)
  (setq bibtex-autokey-titleword-length nil)
  (setq bibtex-autokey-titlewords-stretch 0)
  (setq bibtex-autokey-titleword-separator "")
  (setq bibtex-autokey-name-case-convert-function 'bibtex-autokey-name-convert)
  (setq bibtex-autokey-titleword-case-convert-function 'bibtex-autokey-titleword-convert)
  (setq bibtex-autokey-before-presentation-function 'bibtex-autokey-add_pages)
  (setq bibtex-autokey-name-year-separator "_")
  (setq bibtex-autokey-year-title-separator "_")
  (setq bibtex-autokey-year-length 4))
;; ====================bibtex======================
;;; helm-bibtex
;; ==================helm-bibtex===================
(def-package! helm-bibtex
  :commands (helm-bibtex-with-local-bibliography
             bibtex-completion-find-pdf
             bibtex-completion-get-entry-for-pdf)
  :bind (("C-x b" . swint-helm-bibtex)
         ("C-x B" . helm-bibtex))
  :init
  (add-hook 'LaTeX-mode-hook '(lambda ()
                                (bind-key "C-c b" 'helm-bibtex-with-local-bibliography LaTeX-mode-map)))
  :config
  (define-key helm-map (kbd "C-c j") '(lambda () (interactive)
                                        (with-helm-alive-p
                                          (helm-run-after-exit 'helm-bibtex-open-pdf-externally (helm-marked-candidates)))))
  (define-key helm-map (kbd "C-c o") '(lambda () (interactive)
                                        (with-helm-alive-p
                                          (helm-run-after-exit 'helm-bibtex-open-pdf (helm-marked-candidates)))))
  (define-key helm-map (kbd "C-c l") '(lambda () (interactive)
                                        (with-helm-alive-p
                                          (helm-run-after-exit 'helm-bibtex-edit-notes (helm-marked-candidates)))))
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-field "file"
        bibtex-completion-bibliography (delete (expand-file-name "~/.bib/Zotero.bib")
                                               (directory-files "~/.bib" t "\\.bib$"))
        bibtex-completion-notes-path "~/Zotero/storage/TKM9D893/notes.org")
  (defvar bibtex-completion-bibliography/curr nil)
  (defun swint-helm-bibtex (&optional arg)
    "With a prefix ARG，choose bib file and execute bibtex-completion-clear-cache."
    (interactive "P")
    (when (or arg (not bibtex-completion-bibliography/curr))
      (setq bibtex-completion-bibliography/curr
            (helm-comp-read "Bibtex completion bibliography: "
                            (directory-files (expand-file-name "~/.bib/") t "\\.bib$")
                            :marked-candidates t
                            :buffer "*helm bibtex-swint*")))
    (let ((bibtex-completion-bibliography bibtex-completion-bibliography/curr))
      (if (and (buffer-live-p (get-buffer "*helm bibtex*")) (not arg))
          (helm-resume "*helm bibtex*")
        (helm-bibtex arg bibtex-completion-bibliography/curr))))
  (defun bibtex-completion-get-entry-for-pdf (pdf-file)
    "Find entry for pdf-file in .bib file."
    (with-temp-buffer
      (mapc #'insert-file-contents
            (-flatten (list bibtex-completion-bibliography)))
      (goto-char (point-min))
      (when (re-search-forward pdf-file nil t)
        (re-search-backward (concat "^@\\(" parsebib--bibtex-identifier
                                    "\\)[[:space:]]*[\(\{][[:space:]]*"
                                    parsebib--key-regexp "[[:space:]]*,"))
        (let ((entry-type (match-string 1)))
          (reverse (bibtex-completion-prepare-entry (parsebib-read-entry entry-type) nil nil))))))
  (defcustom helm-bibtex-pdf-open-externally-function '(lambda (fpath)
                                                         (dired-async-shell-command fpath))
    "The function used for opening PDF files externally."
    :group 'bibtex-completion
    :type 'function)
  (defun bibtex-completion-open-pdf-externally (candidates)
    "Open the PDFs associated with the marked entries externally."
    (--if-let
        (-flatten
         (-map 'bibtex-completion-find-pdf
               (if (listp candidates) candidates (list candidates))))
        (-each it helm-bibtex-pdf-open-externally-function)
      (message "No PDF(s) found.")))
  (helm-bibtex-helmify-action bibtex-completion-open-pdf-externally helm-bibtex-open-pdf-externally))
;; ==================helm-bibtex===================
;;; ebib
;; =====================ebib=======================
(def-package! ebib
  :bind ("C-x M-b" . ebib)
  :config
  (define-key ebib-index-mode-map (kbd ",") 'ebib-prev-database)
  (define-key ebib-index-mode-map (kbd ".") 'ebib-next-database)
  (define-key ebib-index-mode-map (kbd "C-q") 'ebib-quit)
  (define-key ebib-index-mode-map (kbd "C-j") 'ebib-view-file)
  (define-key ebib-index-mode-map (kbd "<RET>") 'ebib-view-file-in-emacs)
  (define-key ebib-index-mode-map (kbd "C-c d") 'ebib-delete-entry-from-zotero)
  (define-key ebib-index-mode-map (kbd "C-c j") 'ebib-join-bib)
  (define-key ebib-index-mode-map (kbd "C-x b") nil)
  (smartrep-define-key ebib-index-mode-map "C-c"
    '(("n" . ebib-next-collection)
      ("p" . ebib-prev-collection)))
  (define-key ebib-entry-mode-map (kbd "C-x b") nil)
  (define-key ebib-strings-mode-map (kbd "C-x b") nil)
  (define-key ebib-entry-mode-map (kbd "C-p") nil)
  (define-key ebib-entry-mode-map (kbd "C-n") nil)
  (add-hook 'ebib-entry-mode-hook '(lambda ()
                                     ;; (setq word-wrap t) ;中文支持不好
                                     (setq truncate-lines nil)))
  (setq ebib-index-columns '(("Note" 1 t)
                             ("collection" 20 t)
                             ("Author/Editor" 20 t)
                             ("Year" 4 t)
                             ("Title" 50 t))
        ebib-hide-cursor nil
        ebib-file-associations '(("pdf" . "llpp_qpdfview.sh") ("ps" . "gv"))
        ebib-truncate-file-names nil
        ebib-preload-bib-files (delete "Zotero.bib" (directory-files "~/.bib" nil "\\.bib$"))
        ebib-bib-search-dirs '("~/.bib")
        ebib-notes-file (expand-file-name "~/Zotero/storage/TKM9D893/notes.org")
        ebib-notes-template "* %T\n  :PROPERTIES:\n  %K\n  :END:\n>|<\n"
        ebib-reading-list-file "~/.bib/reading-list.org"
        ebib-use-timestamp t
        ebib-timestamp-format "%Y-%m-%dT%TZ" ;same as zotero export
        ebib-index-default-sort '("timestamp" . descend)
        ebib-index-window-size 20)
  (defun ebib-create-org-identifier/override (key _)
    (format ":Custom_ID: %s" key))
  (advice-add 'ebib-create-org-identifier :override #'ebib-create-org-identifier/override)
  (defun ebib-view-file-in-emacs (arg)
    (interactive "P")
    (ebib--execute-when
      (entries
       (let ((file (ebib-get-field-value ebib-file-field (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref))
             (num (if (numberp arg) arg nil)))
         (let ((file-full-path (ebib--expand-file-name (ebib--select-file file num (ebib--get-key-at-point)))))
           (when (file-exists-p file-full-path)
             (message "Opening `%s'" file-full-path)
             (ebib-lower)
             (find-file file-full-path)))))
      (default
        (beep))))
  (defun ebib-join-bib ()
    "Join to Zotero.bib."
    (interactive)
    (when (y-or-n-p "Join Zotero.bib?")
      (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "py3")
        (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "py3")))
      (let* ((ebib-join-command (concat "python " (expand-file-name "~/Documents/Python/ebib_bibtexparser.py")))
             (process (start-process-shell-command "ebib-join" "*ebib-join*" ebib-join-command)))
        (message "Ebib joining.")
        (set-process-sentinel
         process
         (lambda (process signal)
           (when (memq (process-status process) '(exit signal))
             (message "Ebib join done.")))))))
  (defun ebib-delete-entry-from-zotero ()
    "Delete item from zotero."
    (interactive)
    (let ((item-title (if (region-active-p)
                          (buffer-substring (region-beginning) (region-end))
                        (ebib--get-field-value-for-display
                         "Title" (ebib--get-key-at-point) ebib--cur-db))))
      (when (y-or-n-p (format "Delete [%s] from zotero?"
                              (truncate-string-to-width item-title 100 nil nil t)))
        (unless (equal (bound-and-true-p pyvenv-virtual-env-name) "py3")
          (pyvenv-activate (format "%s/%s" (pyvenv-workon-home) "py3")))
        (let* ((ebib-delete-command (concat "python " (expand-file-name "~/Documents/Python/ebib_pyzotero.py")
                                            " -t " "\"" item-title "\""))
               (process (start-process-shell-command "ebib-delete" "*ebib-delete*" ebib-delete-command)))
          (lexical-let ((mode-string (truncate-string-to-width item-title 100 nil nil t)))
            (message "Ebib deleting %s." mode-string)
            (set-process-sentinel
             process
             (lambda (process signal)
               (cond ((equal (process-status process) 'exit)
                      (if (zerop (process-exit-status process))
                          (message "Ebib delete %s done." mode-string)
                        (message "Ebib delete %s failed." mode-string)))
                     ((memq (process-status process) '(stop signal))
                      (message "Ebib delete %s failed." mode-string))))))))))
  (defun ebib-next-collection ()
    (interactive)
    (let ((current_collection (ebib--get-field-value-for-display
                               "collection" (ebib--get-key-at-point) ebib--cur-db)))
      (while (equal current_collection (ebib--get-field-value-for-display
                                        "collection" (ebib--get-key-at-point) ebib--cur-db))
        (ebib-next-entry))))
  (defun ebib-prev-collection ()
    (interactive)
    (let ((current_collection (ebib--get-field-value-for-display
                               "collection" (ebib--get-key-at-point) ebib--cur-db)))
      (while (equal current_collection (ebib--get-field-value-for-display
                                        "collection" (ebib--get-key-at-point) ebib--cur-db))
        (ebib-prev-entry)))))
;; =====================ebib=======================
(provide 'setup_bibtex)
