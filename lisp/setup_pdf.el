;;; pdf-tools
;; ====================pdf-tools===================
(def-package! pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :init
  ;; Pdf-tools默认设置x-gtk-use-system-tooltips为nil。
  (setq pdf-annot-tweak-tooltips nil)
  :config
  (pdf-tools-install)
  (setq pdf-outline-imenu-use-flat-menus t)
  ;; pdf-view-auto-slice-minor-mode 翻页自动切边。
  ;; (add-hook 'pdf-view-mode-hook 'pdf-view-auto-slice-minor-mode)
  ;; 打开pdf时手动切边一次。手动切边(s b)，重设(s r)。
  ;; (add-hook 'pdf-view-mode-hook 'pdf-view-set-slice-from-bounding-box)
  (define-key pdf-view-mode-map (kbd "M-w") 'pdf-view-kill-ring-save)
  (define-key pdf-view-mode-map (kbd "M-v") 'pdf-view-scroll-down-or-previous-page)
  (define-key pdf-view-mode-map (kbd "C-v") 'pdf-view-scroll-up-or-next-page)
  (define-key pdf-view-mode-map (kbd "C-p") #'(lambda () (interactive) (pdf-view-previous-line-or-previous-page 3)))
  (define-key pdf-view-mode-map (kbd "C-n") #'(lambda () (interactive) (pdf-view-next-line-or-next-page 3)))
  (define-key pdf-view-mode-map (kbd "M-;") 'swint-open-notes-file-for-pdf))
;; ====================pdf-tools===================
;;; doc-view-mode
;; ==================doc-view-mode=================
;; 使用soffice/unoconv转换。
;; 默认缓存文件保存在/tmp和~/AppData/Local/Temp中，使用doc-view-clear-cache清理。
(def-package! doc-view
  :defer t
  :config
  (setq doc-view-continuous t)
  (define-key doc-view-mode-map (kbd "M-v") 'doc-view-scroll-down-or-previous-page)
  (define-key doc-view-mode-map (kbd "C-v") 'doc-view-scroll-up-or-next-page)
  (define-key doc-view-mode-map (kbd "C-p") #'(lambda () (interactive) (doc-view-previous-line-or-previous-page 3)))
  (define-key doc-view-mode-map (kbd "C-n") #'(lambda () (interactive) (doc-view-next-line-or-next-page 3)))
  (define-key doc-view-mode-map (kbd "M-;") 'swint-open-notes-file-for-pdf))
;; ==================doc-view-mode=================
;;; pdfgrep
;; =================pdfgrep========================
(def-package! pdfgrep
  :bind (("M-s v v" . pdfgrep-opened)
         ("M-s v d" . pdfgrep-dired)
         ("M-s V" . pdfgrep-zotero))
  :config
  ;; 三种方式pdfgrep/rg/helm-ag，速度从慢到快，文件数量小于5时用pdfgrep，小于100时用rg，其他用helm-ag
  ;; pdfgrep-opened/pdfgrep-dired/pdfgrep-zotero分别针对已打开文件/当前文件夹/Zotero仓库
  (require 'helm-ag)
  (pdfgrep-mode)
  (define-key grep-mode-map (kbd "C-j") 'compile-goto-error-externally)
  (add-hook 'grep-mode-hook #'(lambda () (setq truncate-lines t)))
  (defun compile-goto-error-externally ()
    (interactive)
    (let* ((loc (compilation--message->loc (get-text-property (line-beginning-position) 'compilation-message)))
           (file-name (caar (compilation--loc->file-struct loc)))
           (meta (pdfgrep-current-page-and-match (current-buffer))))
      (start-process "Shell" nil shell-file-name shell-command-switch
                     (concat "qpdfview --unique --search \"" (cdr meta) "\" \""
                             file-name "#" (number-to-string (car meta))
                             "\""))))
  (defun pdfgrep-dired (&optional arg)
    (interactive "P")
    (let ((string-to-grep (read-string "Dired pdfgrep: "))
          (string-to-escape "\\( \\|(\\|)\\|\\[\\|\\]\\|{\\|}\\|'\\|&\\)")
          (pdf-files (if arg
                         (directory-files-recursively default-directory "\\.pdf$")
                       (directory-files default-directory t "\\.pdf$"))))
      (cl-flet ((escape-local (x)
                              (replace-regexp-in-string string-to-escape
                                                        "\\\\\\1" x)))
        (if (or arg (>= (length pdf-files) 5))
            (rg-run string-to-grep "pdf" default-directory t nil (mapcar #'escape-local pdf-files))
          (pdfgrep (concat (pdfgrep-default-command) (escape-local string-to-grep) " *.pdf"))))))
  (defun pdfgrep-opened ()
    (interactive)
    (let* ((string-to-escape "\\( \\|(\\|)\\|\\[\\|\\]\\|{\\|}\\|'\\|&\\)")
           (zotero-storage "~/Zotero/storage")
           (qpdfview-database (expand-file-name "~/.local/share/qpdfview/qpdfview/database"))
           (qpdfview-file-list (unless (string= (shell-command-to-string "pgrep -x qpdfview") "")
                                 (split-string (shell-command-to-string
                                                (format "sqlite3 %s \"select filePath from tabs_v5\"" qpdfview-database))
                                               "\n" t)))
           (string-to-grep (read-string "Opened pdfgrep: ")))
      (cl-flet ((escape-local (x)
                              (replace-regexp-in-string string-to-escape
                                                        "\\\\\\1" x)))
        (if (<= (length qpdfview-file-list) 5)
            (pdfgrep (concat (pdfgrep-default-command) (escape-local string-to-grep)
                             (cl-loop for x in qpdfview-file-list
                                      concat (concat " " (escape-local x)))
                             (cl-loop for x in (split-string (shell-command-to-string "pgrep -x llpp") "\n" t)
                                      concat (concat " " (escape-local (shell-command-to-string
                                                                        (format "cat /proc/%s/cmdline | sed -e 's/.*\\x00\\([^\\x00].*\\)\\x00$/\\1/'" x)))))
                             (cl-loop for x in (cl-remove-if-not (lambda (x) (equal (buffer-mode x) 'pdf-view-mode)) (buffer-list))
                                      concat (concat " " (escape-local (buffer-file-name x))))))
          (rg-run string-to-grep "pdf" (expand-file-name zotero-storage) t nil (mapcar #'escape-local qpdfview-file-list))))))
  (defun pdfgrep-zotero ()
    (interactive)
    (cl-flet ((escape-local (x)
                            (replace-regexp-in-string string-to-escape
                                                      "\\\\\\1" x)))
      (let* ((string-to-escape "\\( \\|(\\|)\\|\\[\\|\\]\\|{\\|}\\|'\\|&\\)")
             (zotero-database (expand-file-name "~/Zotero/zotero.sqlite"))
             (zotero-storage "~/Zotero/storage")
             (string-to-grep (read-string "Zotero pdfgrep: "))
             (str-list (split-string string-to-grep (if (pyim-string-match-p "\\cC" string-to-grep) "" " ")
                                     t "[ \t\n]+"))
             (zotero-file-list-orig (split-string
                                     (replace-regexp-in-string "|storage:" "/"
                                                               (shell-command-to-string
                                                                (concat "sqlite3 " zotero-database " \"
SELECT items.key, itemAttachments.path
FROM items, itemAttachments
WHERE items.itemID = itemAttachments.itemID AND items.itemID in" "("
(string-join (cl-loop for x in str-list
                      collect (format "SELECT itemID FROM fulltextItemWords where wordID in (SELECT wordID FROM fulltextWords WHERE word LIKE '%s')" x))
             " INTERSECT ")
") ORDER BY items.itemID
\""))) "\n" t))
             (zotero-file-list (mapcar #'(lambda (x) (escape-local (expand-file-name x zotero-storage))) zotero-file-list-orig)))
        (if (and (> (length zotero-file-list) 1) (<= (length zotero-file-list) 5))
            (pdfgrep (concat (pdfgrep-default-command) (escape-local string-to-grep)
                             (string-join zotero-file-list " ")))
          (let* ((match-cache-list (split-string (shell-command-to-string
                                                  (format "%s -l -G .zotero-ft-cache \"%s\" %s" helm-ag-base-command string-to-grep zotero-storage))
                                                 "\n" t))
                 (zotero-dir-list (cl-loop for x in match-cache-list
                                           collect (substring x 27 35))))
            (if (<= (length match-cache-list) 100)
                (rg-run string-to-grep "pdf" (expand-file-name zotero-storage) t nil zotero-dir-list)
              (let ((helm-ag-command-option "-G .zotero-ft-cache"))
                (cl-letf (((symbol-function 'helm-ag--marked-input)
                           (lambda (escape)
                             (replace-regexp-in-string " " "\\\\ " string-to-grep))))
                  (helm-do-ag (expand-file-name zotero-storage) zotero-dir-list)))))))))
  (defun helm-ag-zotero-open-pdf (candidates &optional arg)
    (--if-let
        (-flatten
         (-map #'(lambda (x)
                   (car (directory-files (concat "~/Zotero/storage/" (car (split-string x "/.zotero-ft-cache"))) t ".+\\.pdf")))
               (if (listp candidates) candidates (list candidates))))
        (-each it (if arg #'(lambda (pdffile)
                              (start-process "Shell" nil shell-file-name shell-command-switch
                                             (concat "qpdfview --unique --search " helm-ag--last-query
                                                     " \"" pdffile "\"")))
                    'find-file))
      (message "No PDF(s) found.")))
  (define-key helm-do-ag-map (kbd "C-c j") #'(lambda () (interactive) (with-helm-alive-p
                                                                        (helm-run-after-exit 'helm-ag-zotero-open-pdf (helm-marked-candidates) t))))
  (define-key helm-do-ag-map (kbd "C-c o") #'(lambda () (interactive) (with-helm-alive-p
                                                                        (helm-run-after-exit 'helm-ag-zotero-open-pdf (helm-marked-candidates))))))
;; =================pdfgrep========================
(provide 'setup_pdf)
