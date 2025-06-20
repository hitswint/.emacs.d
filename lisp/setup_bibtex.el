;;; bibtex
;; ====================bibtex======================
(use-package bibtex
  :after (:any ebib helm-bibtex oc)
  :config
  (setq bibtex-autokey-titleword-length nil
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-titleword-separator ""
        bibtex-autokey-name-case-convert-function 'bibtex-autokey-name-convert
        bibtex-autokey-titleword-case-convert-function 'bibtex-autokey-titleword-convert
        bibtex-autokey-before-presentation-function 'bibtex-autokey-add_pages
        bibtex-autokey-name-year-separator "_"
        bibtex-autokey-year-title-separator "_"
        bibtex-autokey-year-length 4)
  (defun bibtex-autokey-name-convert (str)
    (if (string-match-p "\\cC" str)
        (let ((str-list (pyim-hanzi2pinyin str nil nil t)))
          (if (= (length str-list) 1)
              (car str-list)
            (completing-read "Choose: " str-list)))
      (funcall 'downcase str)))
  (defun bibtex-autokey-titleword-convert (str)
    (if (string-match-p "\\cC" str)
        (let ((str-list (pyim-hanzi2pinyin-capitalize str nil nil t)))
          (if (= (length str-list) 1)
              (car str-list)
            (completing-read "Choose: " str-list)))
      (funcall 'upcase-initials str)))
  (defun bibtex-autokey-get-title/around (orig-fun &rest args)
    (let* ((case-fold-search t)
           (titlestring (bibtex-autokey-get-field "title"
                                                  bibtex-autokey-titleword-change-strings))
           (bibtex-autokey-titleword-ignore (unless (string-match-p "\\cC" titlestring)
                                              bibtex-autokey-titleword-ignore)))
      (apply orig-fun args)))
  (defun bibtex-autokey-add_pages (key)
    (concat key "_" (bibtex-autokey-get-field "pages")))
  (advice-add 'bibtex-autokey-get-title :around #'bibtex-autokey-get-title/around))
;; ====================bibtex======================
;;; bibtex-completion
;; ==============bibtex-completion=================
(use-package bibtex-completion
  :commands (bibtex-completion-find-pdf
             bibtex-completion-get-value
             bibtex-completion-edit-notes
             bibtex-completion-get-entry-for-pdf
             bibtex-completion-pdf-viewer
             bibtex-completion-open-pdf-externally)
  :config
  (bind-key "C-c o" #'(lambda () (interactive) (bibtex-completion-open-pdf
                                                (list (bibtex-completion-get-key-bibtex))))
            bibtex-mode-map)
  (bind-key "C-c j" #'(lambda () (interactive) (bibtex-completion-open-pdf-externally
                                                (list (bibtex-completion-get-key-bibtex))))
            bibtex-mode-map)
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-additional-search-fields '(keywords timestamp)
        bibtex-completion-pdf-field "file"
        bibtex-completion-bibliography (delete (expand-file-name "~/.bib/Zotero.bib")
                                               (directory-files "~/.bib" t "\\.bib$"))
        bibtex-completion-notes-path "~/Zotero/storage/TKM9D893/notes.org"
        bibtex-completion-string-hash-table (make-hash-table :test #'equal))
  (defun bibtex-completion-get-entry-for-pdf (pdf-file)
    "Find entry for pdf-file in .bib file."
    (with-temp-buffer
      (mapc #'insert-file-contents
            (-flatten (list bibtex-completion-bibliography)))
      (goto-char (point-min))
      (when (re-search-forward pdf-file nil t)
        (re-search-backward (concat "^@\\(" parsebib--bibtex-identifier
                                    "\\)[[:space:]]*[\(\{][[:space:]]*"
                                    parsebib--bibtex-key-regexp "[[:space:]]*,"))
        (let ((entry-type (match-string 1)))
          (reverse (bibtex-completion-prepare-entry (parsebib-read-entry) nil nil))))))
  (advice-add 'bibtex-completion-candidates :filter-return
              #'(lambda (candidates)
                  (if (assoc "timestamp" (car candidates))
                      (sort candidates
                            (lambda (a b)
                              (when-let ((a-timestamp (assoc "timestamp" a))
                                         (b-timestamp (assoc "timestamp" b)))
                                (> (float-time (parse-iso8601-time-string (cdr a-timestamp)))
                                   (float-time (parse-iso8601-time-string (cdr b-timestamp)))))))
                    candidates)))
  (defun bibtex-completion-pdf-viewer (fpath)
    (let ((parent-dir (file-name-nondirectory (directory-file-name (file-name-directory fpath)))))
      (start-process "Shell" nil shell-file-name shell-command-switch
                     (format "[[ $(pgrep zotero) && ! $(pgrep qpdfview) ]] && (xdg-open zotero://open-pdf/library/items/%s; run-or-raise.sh Navigator) || pdfviewer.sh \"%s\""
                             parent-dir fpath))))
  (defun bibtex-completion-open-pdf-externally (candidates)
    "Open the PDFs associated with the marked entries externally."
    (--if-let
        (-flatten
         (-map 'bibtex-completion-find-pdf
               (if (listp candidates) candidates (list candidates))))
        (-each it 'bibtex-completion-pdf-viewer)
      (message "No PDF(s) found."))))
;; ==============bibtex-completion=================
;;; helm-bibtex
;; ==================helm-bibtex===================
(use-package helm-bibtex
  :commands (helm-bibtex-with-local-bibliography helm-org-ref-link)
  :bind (("C-x b" . swint-helm-bibtex)
         ("C-x B" . helm-bibtex))
  :init
  ;; 根据org中#+BIBLIOGRAPHY:和tex中\bibliography{}定义，查找本地bib文件
  (dolist (hook '(LaTeX-mode-hook org-mode-hook))
    (add-hook hook (lambda ()
                     (local-set-key (kbd "C-c b") #'(lambda (&optional arg) (interactive "P")
                                                      (let ((org-cite-global-bibliography nil))
                                                        (call-interactively 'helm-bibtex-with-local-bibliography)))))))
  :config
  (defvar helm-bibtex-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map helm-map)
      (define-key map (kbd "C-j") #'(lambda () (interactive) (with-helm-alive-p
                                                               (helm-run-after-exit 'helm-bibtex-open-pdf-externally (helm-marked-candidates)))))
      (define-key map (kbd "C-o") #'(lambda () (interactive) (with-helm-alive-p
                                                               (helm-run-after-exit 'helm-bibtex-open-pdf (helm-marked-candidates)))))
      (define-key map (kbd "C-M-'") #'(lambda () (interactive) (with-helm-alive-p
                                                                 (helm-run-after-exit 'helm-bibtex-edit-notes (helm-marked-candidates)))))
      (define-key map (kbd "RET") #'(lambda () (interactive) (with-helm-alive-p
                                                               (helm-run-after-exit 'helm-bibtex-insert-citation (helm-marked-candidates)))))
      map)
    "Keymap for `helm-bibtex'.")
  (helm-set-attr 'keymap helm-bibtex-map helm-source-bibtex)
  (helm-set-attr 'persistent-action 'helm-bibtex-insert-citation helm-source-bibtex)
  (helm-bibtex-helmify-action bibtex-completion-open-pdf-externally helm-bibtex-open-pdf-externally)
  ;; 改变helm-bibtex中Insert citation格式
  (setf (cdr (assoc 'org-mode bibtex-completion-format-citation-functions))
        'org-cite-format-citation)
  (defun org-cite-format-citation (keys)
    ;; 使用BIBLIOGRAPHY关键词判断引用格式：(org-cite-list-bibliography-files)，但其也用于判断是否存在local bib文件
    ;; 改用CITE_EXPORT关键词，org-cite与pandoc支持相同格式，CITE_EXPORT也用于判断后续输入引用和文献的格式
    (if (org-collect-keywords '("CITE_EXPORT"))
        (bibtex-completion-format-citation-org-cite keys)  ;使用org-cite格式
      (concat "cite:" (s-join ","       ;使用org-ref格式
                              (--map (format "%s" it) keys))
              " ")))
  (defvar helm-source-bibtex-swint
    (helm-build-sync-source "BibTeX entries swint"
      :header-name (lambda (name)
                     (format "%s%s: " name (if helm-bibtex-local-bib " (local)" "")))
      :candidates 'helm-bibtex-candidates
      :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
      :action (helm-get-attr 'action helm-source-bibtex)
      :keymap helm-bibtex-map
      :persistent-action 'helm-bibtex-insert-citation)
    "Source for searching in BibTeX files.")
  (defvar helm-source-bibtex-local
    (helm-build-sync-source "BibTeX entries local"
      :header-name (lambda (name)
                     (format "%s%s: " name (if helm-bibtex-local-bib " (local)" "")))
      :candidates 'helm-bibtex-candidates
      :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
      :action (helm-get-attr 'action helm-source-bibtex)
      :keymap helm-bibtex-map
      :persistent-action 'helm-bibtex-insert-citation)
    "Source for searching in BibTeX files.")
  (defun helm-bibtex (&optional arg local-bib input)
    (interactive "P")
    (let ((helm-split-window-default-side 'below)
          (helm-always-two-windows t)
          (helm-bibtex-full-frame nil)
          helm-bibtex-buffer source-bibtex)
      (cond ((null local-bib)
             (setq helm-bibtex-buffer "*helm bibtex*"
                   source-bibtex helm-source-bibtex))
            ((file-in-directory-p (car local-bib) "~/.bib")
             (setq helm-bibtex-buffer "*helm bibtex-swint*"
                   source-bibtex helm-source-bibtex-swint))
            (t
             (setq helm-bibtex-buffer "*helm bibtex-local*"
                   source-bibtex helm-source-bibtex-local)))
      (if (and (buffer-live-p (get-buffer helm-bibtex-buffer)) (not arg))
          (let ((helm-current-buffer (current-buffer)))
            (helm-resume helm-bibtex-buffer))
        (when arg
          (bibtex-completion-clear-cache))
        (bibtex-completion-init)
        (let* ((candidates (bibtex-completion-candidates))
               (key (bibtex-completion-key-at-point))
               (preselect (and key
                               (cl-position-if (lambda (cand)
                                                 (member (cons "=key=" key)
                                                         (cdr cand)))
                                               candidates))))
          (helm :sources (list source-bibtex helm-source-fallback-options)
                :full-frame helm-bibtex-full-frame
                :buffer helm-bibtex-buffer
                :input input
                :preselect (lambda ()
                             (and preselect
                                  (> preselect 0)
                                  (helm-next-line preselect)))
                :candidate-number-limit (max 500 (1+ (or preselect 0)))
                :bibtex-candidates candidates
                :bibtex-local-bib local-bib)))))
  (defvar bibtex-completion-bibliography/curr nil)
  (defun swint-helm-bibtex (&optional arg)
    "With a prefix ARG，choose bib file and execute bibtex-completion-clear-cache."
    (interactive "P")
    (when (or arg (not bibtex-completion-bibliography/curr))
      (setq bibtex-completion-bibliography/curr
            (helm-comp-read "Bibtex completion bibliography: "
                            (append (directory-files (expand-file-name "~/.bib/") t "\\.bib$")
                                    (directory-files (helm-current-directory) t "\\.bib$"))
                            :marked-candidates t
                            :buffer "*helm bibtex select*")))
    (let ((bibtex-completion-bibliography bibtex-completion-bibliography/curr))
      (helm-bibtex arg bibtex-completion-bibliography/curr)))
  (defvar helm-org-ref-link-buffer "*helm org ref link-swint*")
  (defvar helm-org-ref-link-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map helm-map)
      (define-key map (kbd "C-j") #'(lambda () (interactive)
                                      (helm-run-after-exit #'(lambda (_candidates)
                                                               (goto-char (point-min))
                                                               (search-forward (format "#+NAME: %s" (car _candidates)) nil t))
                                                           (helm-marked-candidates))))
      map)
    "Keymap for `helm-org-ref-link'.")
  ;; RET 插入并退出 / C-l 连续插入 / C-j 跳转
  (defun helm-org-ref-link ()
    (interactive)
    (helm--push-and-remove-dups helm-org-ref-link-buffer 'helm-buffers)
    (setq helm-last-buffer helm-org-ref-link-buffer)
    (let* ((helm-split-window-default-side 'below)
           (helm-always-two-windows t)
           (export-to-oc (org-collect-keywords '("CITE_EXPORT")))
           (label-names (helm-comp-read "Label: " (org-ref-label-list)
                                        :marked-candidates t
                                        :buffer helm-org-ref-link-buffer
                                        :keymap helm-org-ref-link-map
                                        :persistent-action (lambda (candidate)
                                                             (with-helm-current-buffer
                                                               (if export-to-oc
                                                                   (insert (format "[cite:@%s] " candidate))
                                                                 (insert (format "[[%s]] " candidate))))))))
      ;; 将插入引用定义为keymap中的命令的话，存在类似helm-insert-or-copy的Quit警告问题
      (when (sequencep label-names)
        ;; 根据#+NAME/#+LABEL的前缀是latex/office选择不同的引用方式
        (if export-to-oc
            (insert (bibtex-completion-format-citation-org-cite label-names))
          ;; 若引用时无章节编号的需要，仍可采用latex格式；若采取office格式，则需外部调用pandoc
          (insert (s-join " " (--map (format "[[%s]]" it) label-names))))))))
;; ==================helm-bibtex===================
;;; ebib
;; =====================ebib=======================
(use-package ebib
  :bind ("C-x C-b" . ebib)
  :config
  (define-key ebib-index-mode-map (kbd ",") 'ebib-prev-database)
  (define-key ebib-index-mode-map (kbd ".") 'ebib-next-database)
  (define-key ebib-index-mode-map (kbd "C-q") 'ebib-quit)
  (define-key ebib-index-mode-map (kbd "C-j") 'ebib-view-file)
  (define-key ebib-index-mode-map (kbd "C-o") 'ebib-view-file-in-emacs)
  (define-key ebib-index-mode-map (kbd "<RET>") 'ebib-select-and-popup-entry)
  (define-key ebib-index-mode-map (kbd "C-c d") 'ebib-delete-entry-from-zotero)
  (define-key ebib-index-mode-map (kbd "C-c j") 'ebib-join-bib)
  (define-key ebib-index-mode-map (kbd "C-M-p") 'ebib-prev-collection)
  (define-key ebib-index-mode-map (kbd "C-M-n") 'ebib-next-collection)
  (define-key ebib-index-mode-map (kbd "C-x b") nil)
  (define-key ebib-index-mode-map (kbd "C-p") nil)
  (define-key ebib-index-mode-map (kbd "C-n") nil)
  (define-key ebib-filters-map "F" 'ebib-filter-collection)
  (define-key ebib-strings-mode-map (kbd "C-x b") nil)
  (define-key ebib-entry-mode-map (kbd "C-x b") nil)
  (define-key ebib-entry-mode-map (kbd "C-p") nil)
  (define-key ebib-entry-mode-map (kbd "C-n") nil)
  (define-key ebib-entry-mode-map (kbd "z") 'ebib-leave-ebib-windows)
  (add-hook 'ebib-entry-mode-hook #'(lambda ()
                                      ;; (setq word-wrap t) ;中文支持不好
                                      (setq truncate-lines nil)))
  (defcustom ebib-file-symbol "F"
    "Symbol used to indicate the presence of a file for the current entry."
    :group 'ebib
    :type '(string :tag "File symbol"))
  (defun ebib-display-file-symbol (field key db)
    (if (ebib-get-field-value field key db 'noerror 'unbraced 'xref)
        (propertize ebib-file-symbol
                    'face '(:inherit ebib-link-face :height 0.8))
      (propertize (make-string (string-width ebib-file-symbol) ?\s)
                  'face '(:height 0.8))))
  (add-to-list 'ebib-field-transformation-functions '("file" . ebib-display-file-symbol))
  (setq ebib-index-columns '(("file" 1 t)
                             ("Note" 1 t)
                             ("collection" 15 t)
                             ("Author/Editor" 25 t)
                             ("Year" 4 t)
                             ("Title" 50 t))
        ebib-layout 'index-only
        ebib-hide-cursor nil
        ebib-file-associations '(("pdf" . bibtex-completion-pdf-viewer) ("ps" . "gv"))
        ebib-truncate-file-names nil
        ebib-preload-bib-files (delete "Zotero.bib" (directory-files "~/.bib" nil "\\.bib$"))
        ebib-bib-search-dirs '("~/.bib")
        ebib-notes-default-file (expand-file-name "~/Zotero/storage/TKM9D893/notes.org")
        ebib-notes-template "* %T\n  :PROPERTIES:\n  %K\n  :END:\n>|<\n"
        ebib-reading-list-file nil
        ebib-use-timestamp t
        ebib-timestamp-format "%Y-%m-%dT%TZ" ;same as zotero export
        ebib-index-default-sort '("timestamp" . descend)
        ebib-index-window-size 30
        ebib-expand-strings t
        ebib-filename-separator ";")
  (defun ebib-create-org-identifier/override (key _)
    (format ":Custom_ID: %s" key))
  (advice-add 'ebib-create-org-identifier :override #'ebib-create-org-identifier/override)
  (defun ebib-view-file-in-emacs (arg)
    (interactive "P")
    (ebib--execute-when (entries
                         (let* ((key (ebib--get-key-at-point))
                                (file (ebib-get-field-value "file" key ebib--cur-db 'noerror 'unbraced 'xref))
                                (num (if (numberp arg) arg nil))
                                (file-full-path (ebib--expand-file-name (ebib--select-file file num key))))
                           (if (not (file-exists-p file-full-path))
                               (error "[Ebib] File not found: `%s'" (funcall ebib-file-name-mod-function file-full-path nil))
                             ;; (ebib-lower)
                             (other-window 1)
                             (find-file file-full-path)
                             (message "Opening `%s'" file-full-path))))
      (default (beep))))
  (defun ebib-join-bib ()
    "Join to Zotero.bib."
    (interactive)
    (when (y-or-n-p "Join Zotero.bib?")
      (pyvenv-activate-py3)
      (let* ((ebib-join-command (concat "python " (expand-file-name "~/Documents/Python/bibtex/ebib_bibtexparser.py")))
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
        (pyvenv-activate-py3)
        (let* ((ebib-delete-command (concat "python " (expand-file-name "~/Documents/Python/bibtex/ebib_pyzotero.py")
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
    (let ((current-collection (ebib--get-field-value-for-display
                               "collection" (ebib--get-key-at-point) ebib--cur-db))
          current-position)
      (while (and (equal current-collection (ebib--get-field-value-for-display
                                             "collection" (ebib--get-key-at-point) ebib--cur-db))
                  (not (equal current-position (point))))
        (setq current-position (point))
        (ebib-next-entry))))
  (defun ebib-prev-collection ()
    (interactive)
    (let ((current-collection (ebib--get-field-value-for-display
                               "collection" (ebib--get-key-at-point) ebib--cur-db))
          current-position)
      (while (and (equal current-collection (ebib--get-field-value-for-display
                                             "collection" (ebib--get-key-at-point) ebib--cur-db))
                  (not (equal current-position (point))))
        (setq current-position (point))
        (ebib-prev-entry))))
  (defun ebib-filter-collection ()
    (interactive)
    (let* ((collection-key "collection")
           (collection-list (cl-remove-duplicates (cl-loop for entry in (hash-table-values (ebib-db-val 'entries ebib--cur-db))
                                                           collect (substring (assoc-default collection-key entry) 1 -1))
                                                  :test 'string=))
           (regexp (helm-comp-read "Collection: " collection-list
                                   :buffer "*helm ebib collection-swint*")))
      (ebib--execute-when
        (entries
         (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
         (ebib-db-set-filter `(contains collection-key regexp) ebib--cur-db)
         (ebib--update-buffers))
        (default
         (beep))))))
;; =====================ebib=======================
(provide 'setup_bibtex)
