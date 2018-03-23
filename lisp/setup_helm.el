;;; helm
;; ====================helm=====================
(def-package! helm
  :diminish helm-mode
  :config
  (def-package! helm-config)
  (def-package! helm-for-files)
  (helm-mode 1)
  (setq helm-completing-read-handlers-alist '((describe-function . helm-completing-read-symbols)
                                              (describe-variable . helm-completing-read-symbols)
                                              (debug-on-entry . helm-completing-read-symbols)
                                              (find-function . helm-completing-read-symbols)
                                              (find-tag . helm-completing-read-with-cands-in-buffer)
                                              (ffap-alternate-file)
                                              (tmm-menubar)
                                              (find-file)
                                              (org-annotate-file)
                                              (swint-org-annotate-file)
                                              (dired-do-copy)
                                              (dired-create-directory)))
  (setq helm-projectile-sources-list '(helm-source-projectile-projects
                                       helm-source-projectile-files-list
                                       helm-source-projectile-buffers-list))
  (setq helm-buffer-details-flag nil)
  (setq helm-ff-newfile-prompt-p nil)
  (setq helm-split-window-default-side 'same)
  (setq helm-kill-ring-threshold 1)
  (setq helm-pdfgrep-default-read-command "llpp -page %p \"%f\"")
  (setq helm-boring-buffer-regexp-list (append helm-boring-buffer-regexp-list '("\\`Enjoy\\ Music\\'" "\\`\\*Inferior\\ Octave\\*\\'" "\\`\\*Ibuffer\\*\\'" "\\`\\*MATLAB\\*\\'" "\\`\\*shell\\*\\'" "\\`\\*calculator\\*\\'" "\\`\\*Calendar\\*\\'" "\\`\\*Process\\ List\\*\\'" "\\`\\*toc\\*\\'" "\\`\\*buffer-selection\\*\\'" "\\`\\*Disabled\\ Command\\*\\'" "\\`\\*Mingus\\*\\'" "\\`\\*Ido\\ Completions\\*\\'" "\\`.english-words\\'" "\\`\\*Help\\*\\'")))
  (custom-set-faces '(helm-buffer-directory ((t (:foreground "yellow" :weight bold))))
                    '(helm-buffer-file ((t (:inherit font-lock-type-face))))
                    '(helm-ff-directory ((t (:foreground "yellow" :weight bold))))
                    '(helm-ff-dotted-directory ((t (:foreground "yellow" :weight bold))))
                    '(helm-ff-file ((t (:foreground "white"))))
                    '(helm-grep-file ((t (:foreground "cyan"))))
                    '(helm-selection ((t (:background "black" :underline t))))
                    '(helm-visible-mark ((t (:foreground "DeepSkyBlue1")))))
;;;; keybindings
  ;; ===============keybindings=================
  (global-set-key (kbd "C-M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-'") 'helm-bookmarks)
  (global-set-key (kbd "C-,") 'swint-helm-file-buffers-list)
  (global-set-key (kbd "C-.") 'swint-helm-dired-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x f") 'helm-find)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x c d") 'helm-apt)
  (global-set-key (kbd "C-x y") 'helm-resume)
  (define-key helm-map (kbd "C-;") 'helm-toggle-visible-mark)
  (define-key helm-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "M-U") 'helm-unmark-all)
  (define-key helm-map (kbd "M-T") 'helm-toggle-all-marks)
  (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "M-U") 'helm-unmark-all)
  (define-key helm-find-files-map (kbd "M-T") 'helm-toggle-all-marks)
  (define-key helm-find-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
  (define-key helm-buffer-map (kbd "C-M-j") 'swint-helm-buffer-persp-add-buffers)
  (define-key helm-buffer-map (kbd "C-M-k") 'swint-helm-buffer-persp-remove-buffers)
  (define-key helm-buffer-map (kbd "C-o") 'swint-helm-buffer-switch-persp/other-window)
  (define-key helm-read-file-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key helm-grep-map (kbd "C-o") 'helm-grep-run-other-window-action)
  (define-key helm-generic-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
  (define-key helm-map (kbd "C-,") 'swint-helm-file-buffers-after-quit)
  (define-key helm-map (kbd "C-.") 'swint-helm-dired-buffers-after-quit)
  (define-key helm-map (kbd "C-'") 'swint-helm-bookmarks-after-quit)
  (define-key helm-map (kbd "M-'") 'swint-helm-projectile-after-quit)
  (define-key helm-map (kbd "M-RET") 'helm-quit-and-find-file)
  (define-key helm-find-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-lister)
  (define-key helm-find-files-map (kbd "C-j") 'helm-ff-run-open-file-externally)
  (define-key helm-generic-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-lister)
  (define-key helm-generic-files-map (kbd "C-j") 'helm-ff-run-open-file-externally)
  (global-set-key (kbd "C-x F") 'swint-helm-locate)
  ;; ===============keybindings=================
;;;; helm-pinyin
  ;; ================helm-pinyin================
  (require 'iswitchb-pinyin)
  ;; 支持中文拼音首字母匹配，会使helm-find-files匹配过多。
  (cl-defun helm-mm-3-match-py (orig-fn str &rest args)
    (apply orig-fn (concat str "|" (str-unicode-to-pinyin-initial str)) args))
  (advice-add 'helm-mm-3-match :around #'helm-mm-3-match-py)
  ;; 默认在输入前面加空格解决匹配问题。
  (defun helm-find-files-1-py (orig-fn fname &rest args)
    (apply orig-fn (concat fname " ") args))
  (advice-add 'helm-find-files-1 :around #'helm-find-files-1-py)
  ;; ================helm-pinyin================
  )
;; ====================helm=====================
;;; helm_lacarte
;; ================helm_lacarte=================
(def-package! lacarte
  :commands helm-math-symbols
  :bind (("<escape> M-x" . lacarte-execute-command)
         ("C-x `" . lacarte-execute-menu-command))
  :config
  ;; 使用helm-insert-latex-math代替。
  (defvar helm-source-lacarte-math
    '((name . "Math Symbols")
      (init . (lambda()
                (setq helm-lacarte-major-mode major-mode)))
      (candidates
       . (lambda () (if (eq helm-lacarte-major-mode 'latex-mode)
                        (delete '(nil) (lacarte-get-a-menu-item-alist LaTeX-math-mode-map)))))
      (action . (("Open" . (lambda (candidate)
                             (call-interactively candidate)))))))
  (defun helm-math-symbols ()
    "Helm for searching math menus."
    (interactive)
    (helm '(helm-source-lacarte-math)
          (thing-at-point 'symbol) "Symbol: "
          nil nil "*helm math symbols*")))
;; ================helm_lacarte=================
;;; helm-bibtex
;; ================helm-bibtex==================
(def-package! helm-bibtex
  :commands (helm-bibtex-with-local-bibliography
             bibtex-completion-find-pdf
             bibtex-completion-get-entry-for-pdf)
  :bind (("C-x b" . helm-bibtex)
         ("C-x B" . swint-helm-bibtex))
  :init
  (add-hook 'LaTeX-mode-hook '(lambda ()
                                (bind-key "C-c b" 'helm-bibtex-with-local-bibliography LaTeX-mode-map)))
  :config
  (define-key helm-map (kbd "C-c j") '(lambda () (interactive)
                                        (with-helm-alive-p
                                          (helm-exit-and-execute-action 'helm-bibtex-open-pdf-externally))))
  (define-key helm-map (kbd "C-c o") '(lambda () (interactive)
                                        (with-helm-alive-p
                                          (helm-exit-and-execute-action 'helm-bibtex-open-pdf))))
  (define-key helm-map (kbd "C-c l") '(lambda () (interactive)
                                        (with-helm-alive-p
                                          (helm-exit-and-execute-action 'helm-bibtex-edit-notes))))
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-field "file"
        bibtex-completion-bibliography '("~/.bib/ALL.bib") ;zotero-better-bibtex自动更新。
        bibtex-completion-notes-path "~/Zotero/storage/TKM9D893/notes.org")
  (defun swint-helm-bibtex ()
    (interactive)
    (let ((bibtex-completion-bibliography
           (read-file-name "File: " (expand-file-name "~/.bib/"))))
      (call-interactively 'helm-bibtex)))
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
;; ================helm-bibtex==================
;;; helm-swoop
;; ================helm-swoop===================
(def-package! helm-swoop
  :bind (("M-s M-s" . helm-swoop)
         ("M-s M-S" . helm-multi-swoop-all))
  :config
  ;; helm-swoop 中使用C-c C-e编辑，C-x C-s保存。
  (define-key isearch-mode-map (kbd "M-s M-s") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-S") 'helm-multi-swoop-all-from-helm-swoop))
;; ================helm-swoop===================
;;; helm-unicode
;; ===============helm-unicode==================
(def-package! helm-unicode
  :bind ("C-x c u" . helm-unicode))
;; ===============helm-unicode==================
;;; helm-ag
;; =================helm-ag=====================
(def-package! helm-ag
  ;; helm-do-ag 互动式搜索，但只能搜索一个词。
  ;; helm-ag 先输入词，可以在结果中搜索第二个词。
  :bind (("C-x g" . helm-do-ag)
         ("C-x G" . helm-do-ag-buffers))
  :config
  ;; C-c C-e 进入编辑模式，C-x C-s 保存helm-ag结果。
  (define-key helm-ag-map (kbd "C-h") 'helm-ag--up-one-level)
  (define-key helm-ag-map (kbd "C-o") 'helm-ag--run-other-window-action))
;; =================helm-ag=====================
;;; helm-descbinds
;; ==============helm-descbinds=================
(def-package! helm-descbinds
  :commands helm-descbinds
  :config
  (helm-descbinds-mode))
;; ==============helm-descbinds=================
;;; helm-imenu
;; ================helm-imenu===================
(def-package! imenu
  :commands imenu-choose-buffer-index)
(def-package! imenu-anywhere
  :bind ("M-s I" . helm-imenu-anywhere)
  :config
  (setq imenu-anywhere-delimiter " | "))
(def-package! helm-imenu
  :bind (("M-s i" . helm-semantic-or-imenu)
         ("M-s M-i" . helm-imenu-outshine))
  :config
  (setq helm-imenu-delimiter " | ")
  ;; helm-imenu-outshine.
  (defvar helm-source-imenu-outshine nil)
  (defvar helm-cached-imenu-outshine-tick nil)
  (defvar helm-cached-imenu-outshine-candidates nil)
  (defvar-local imenu-outshine--index-alist nil)
  (defun imenu-outshine--make-index-alist ()
    "Create an index alist for the outshine headings."
    (setq imenu-outshine--index-alist
          (save-excursion
            (save-restriction
              (widen)
              (imenu--generic-function `((nil ,(concat (outshine-calc-outline-regexp) "\\(.*$\\)") 1))))))
    (imenu--truncate-items imenu-outshine--index-alist))
  (defun helm-imenu-outshine-candidates (&optional buffer)
    (with-current-buffer (or buffer helm-current-buffer)
      (let ((tick (buffer-modified-tick)))
        (if (eq helm-cached-imenu-outshine-tick tick)
            helm-cached-imenu-outshine-candidates
          (setq imenu-outshine--index-alist nil)
          (prog1 (setq helm-cached-imenu-outshine-candidates
                       (let ((index (imenu-outshine--make-index-alist)))
                         (helm-imenu--candidates-1
                          (delete (assoc "*Rescan*" index) index))))
            (setq helm-cached-imenu-outshine-tick tick))))))
  (defclass helm-imenu-outshine-source (helm-source-sync)
    ((candidates :initform 'helm-imenu-outshine-candidates)
     (candidate-transformer :initform 'helm-imenu-transformer)
     (persistent-action :initform 'helm-imenu-persistent-action)
     (persistent-help :initform "Show this entry")
     (keymap :initform helm-imenu-map)
     (help-message :initform 'helm-imenu-help-message)
     (action :initform 'helm-imenu-action)))
  (defun helm-imenu-outshine ()
    "Preconfigured `helm' for `imenu'."
    (interactive)
    (unless helm-source-imenu-outshine
      (setq helm-source-imenu-outshine
            (helm-make-source "Imenu outshine" 'helm-imenu-outshine-source
              :fuzzy-match helm-imenu-fuzzy-match)))
    (let ((imenu-auto-rescan t)
          (str (thing-at-point 'symbol))
          (helm-execute-action-at-once-if-one
           helm-imenu-execute-action-at-once-if-one))
      (helm :sources 'helm-source-imenu-outshine
            :default (list (concat "\\_<" str "\\_>") str)
            :preselect str
            :buffer "*helm imenu outshine*"))))
;; ================helm-imenu===================
(provide 'setup_helm)
