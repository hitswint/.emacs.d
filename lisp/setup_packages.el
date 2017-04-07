;;; server
;; =====================server=====================
(use-package server
  ;; Enabled at idle.
  :defer 2
  :config
  (unless (server-running-p)
    (server-start)))
;; =====================server=====================
;;; 文件加密
;; ====================文件加密====================
(use-package epa-file
  ;; Enabled at idle.
  :defer 2
  :config
  ;;(epa-file-enable)
  (setenv "GPG_AGENT_INFO" nil)
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  (setq epa-file-inhibit-auto-save nil))
;; ====================文件加密====================
;;; multiple-cursors
;; ================multiple-cursors================
(use-package multiple-cursors
  ;; Enabled at commands.
  ;; mc/xxx函数都不在mc包中，但能激活mc包。
  :defer t
  :after multiple-cursors-core
  :init
  (bind-key "C-M-," 'mc/mark-previous-like-this)
  (bind-key "C-M-." 'mc/mark-next-like-this)
  (bind-key "<C-M-mouse-1>" 'mc/add-cursor-on-click)
  (smartrep-define-key global-map "C-x"
    '(("C-M-," . mc/unmark-previous-like-this)
      ("C-M-." . mc/unmark-next-like-this)
      ("M-<" . mc/skip-to-previous-like-this)
      ("M->" . mc/skip-to-next-like-this)
      ("C-M-;" . mc/mark-all-like-this)
      ("C-M-/" . mc/edit-lines)
      ("C-M-'" . mc/mark-more-like-this-extended)
      ("M-:" . mc/insert-numbers)
      ("M-\"" . mc/insert-letters)
      ("M-?" . mc/sort-regions)
      ("M-m" . mc/mark-pop)))
  :config
  (define-key mc/keymap (kbd "C-`") 'mc-hide-unmatched-lines-mode))
;; ================multiple-cursors================
;;; expand-region
;; =================expand-region==================
(use-package expand-region
  ;; Enabled at commands.
  :defer t
  :bind ("C-M-;" . er/expand-region)
  :config
  (setq expand-region-contract-fast-key ":")
  (setq expand-region-reset-fast-key "C-;"))
;; 在octave中使用会导致emacs假死，原因是octave的function中必须带有end。
;; =================expand-region==================
;;; 回收站
;; ===================回收站=======================
(use-package trashcan
  ;; Enabled automatically.
  :load-path "site-lisp/trashcan/")
;; ===================回收站=======================
;;; undo-tree
;; ==================undo-tree=====================
(use-package undo-tree
  ;; Enabled at commands.
  :defer t
  :bind (("C-/" . undo-tree-undo)
         ("C-M-/" . undo-tree-redo))
  :config
  (global-undo-tree-mode)
  (define-key undo-tree-map (kbd "M-_") nil))
;; ==================undo-tree=====================
;;; breadcrumb
;; ==================breadcrumb====================
(use-package breadcrumb
  ;; Enabled at commands.
  :load-path "site-lisp/breadcrumb/"
  :defer t
  :commands bc-set
  :config
  (bind-key "C-M-q" 'bc-previous)
  (define-key emacs-lisp-mode-map "\e\C-q" nil)
  (define-key lisp-interaction-mode-map "\e\C-q" nil)
  (define-key prog-mode-map "\e\C-q" nil))
;; 删除breadcrumb.el源文件中(message "breadcrumb bookmark is set for the current position.")，使bc-set不出现提示。
;; (global-set-key (kbd "C-x C-/") 'bc-list) ;; C-x M-j for the bookmark menu list
;; (global-set-key (kbd "C-x C-/") 'bc-local-previous) ;; M-up-arrow for local previous
;; (global-set-key (kbd "C-x C-/") 'bc-local-next)     ;; M-down-arrow for local next
;; (global-set-key (kbd "C-x C-/") 'bc-goto-current) ;; C-c j for jump to current bookmark
;; ==================breadcrumb====================
;;; auto-mark
;; ==================auto-mark=====================
(use-package auto-mark
  ;; Enabled at commands.
  :load-path "site-lisp/auto-mark/"
  :defer 2
  :config
  (setq auto-mark-command-class-alist
        '((goto-line . jump)
          (indent-for-tab-command . ignore)
          (undo . ignore)
          (goto-last-change . jump)
          (goto-last-change-reverse . jump)))
  ;; 会导致(void-variable last-command-char)错误。
  ;; (setq auto-mark-command-classifiers
  ;;       (list (lambda (command)
  ;;               (if (and (eq command 'self-insert-command)
  ;;                        (eq last-command-char ? ))
  ;;                   'ignore))))
  (global-auto-mark-mode 1))
;; ==================auto-mark=====================
;;; visible-mark
;; ================visible-mark====================
(use-package visible-mark
  ;; Enabled automatically.
  :config
  (global-visible-mark-mode 1)
  (setq visible-mark-max 2)
  (defface swint-visible-mark-face-1
    '((t (:background "#666666" :foreground "white")))
    "Face for the mark."
    :group 'visible-mark)
  (setq visible-mark-faces '(visible-mark-active swint-visible-mark-face-1))
  (setq visible-mark-forward-max 2)
  (defface swint-visible-mark-forward-face-1
    '((t (:background "dark red" :foreground "white")))
    "Face for the mark."
    :group 'visible-mark)
  (defface swint-visible-mark-forward-face-2
    '((t (:background "dark green" :foreground "white")))
    "Face for the mark."
    :group 'visible-mark)
  (setq visible-mark-forward-faces '(swint-visible-mark-forward-face-1 swint-visible-mark-forward-face-2)))
;; ================visible-mark====================
;;; unicad
;; =====================unicad=====================
;; lin中不会出现乱码，不需要，这个包会拖慢启动速度。
(use-package unicad
  ;; Enabled automatically.
  :load-path "site-lisp/unicad/"
  :if is-win
  :config
  ;; 解决关emacs时保存.session的编码问题
  (setq session-save-file-coding-system 'utf-8)
  ;; Set default encoding to utf-8.
  (setq-default buffer-file-coding-system 'utf-8)
  ;; Set writing buffer default to utf-8, or emacs always show encoding problem when saving files.
  (setq save-buffer-coding-system 'utf-8)
  (setq coding-system-for-write 'utf-8))
;; =====================unicad=====================
;;; everything
;; ===================everything===================
(use-package everything
  ;; Enabled at commands.
  :load-path "site-lisp/everything/"
  :if is-win
  :defer t
  :bind ("C-s-s" . everything-default-directory)
  :config
  (defun everything-default-directory ()
    (interactive)
    (w32-shell-execute
     "open" "c:/Program Files/Everything/Everything.exe"
     (concat "-p " (expand-file-name default-directory))))
  (setq everything-ffap-integration nil)
  (setq everything-matchpath t)
  (setq everything-cmd "c:/Program Files/Everything/es.exe"))
;; ===================everything===================
;;; popwin
;; =====================popwin=====================
(use-package popwin
  ;; Enabled at idle.
  :defer 2
  :config
  (popwin-mode 1))
;; =====================popwin=====================
;;; anchored-transpose
;; ================anchored-transpose==============
(use-package anchored-transpose
  ;; Enabled at commands.
  :defer t
  :bind ("C-x t" . anchored-transpose)
  :config
  (autoload 'anchored-transpose "anchored-transpose" nil t))
;; First select the entire phrase and type C-x t. Then select the anchor phrase and type C-x t again. You’re done!
;; 首先选择整个区域，然后选择锚点，锚点两侧的内容交换。
;; You can select the anchor phrase first followed by the phrase to be transposed if more convenient. Or select the 2 phrases independently. If you select 2 overlapping sections it ignores the overlap and swaps the non-overlapping sections. It even supports SecondarySelection. Somehow it can always tell what you want ;-)
;; 另外，可以分别选择两部分交换的内容。
;; ================anchored-transpose==============
;;; God-mode
;; ====================God-mode====================
(use-package god-mode
  ;; Enabled at commands.
  :defer t
  :bind ("<S-escape>" . god-local-mode)
  :config
  ;; (global-set-key (kbd "<escape>") 'god-mode-all)
  (defun my-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'bar
                        'box)))
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor)
  (add-to-list 'god-exempt-major-modes 'dired-mode))
;; ====================God-mode====================
;;; elisp-slime-nav
;; =================elisp-slime-nav================
(use-package help
  ;; Enabled at commands.
  ;; Enabled automatically actually.
  :defer t
  :commands help-command
  :config
  (define-key 'help-command (kbd "C-l") 'find-library)
  (define-key 'help-command (kbd "C-f") 'find-function)
  (define-key 'help-command (kbd "C-k") 'find-function-on-key)
  (define-key 'help-command (kbd "C-v") 'find-variable)
  (define-key 'help-command (char-to-string help-char) nil))
(use-package help-mode
  ;; Enabled with defer.
  :defer t
  :config
  (define-key help-mode-map (kbd "q") 'kill-buffer-and-window))
(use-package elisp-slime-nav
  ;; Enabled in modes.
  :defer t
  :commands elisp-slime-nav-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode))
  :config
  (define-key elisp-slime-nav-mode-map (kbd "C-c C-,") 'elisp-slime-nav-find-elisp-thing-at-point)
  (define-key elisp-slime-nav-mode-map (kbd "C-c C-.") 'pop-tag-mark)
  (define-key elisp-slime-nav-mode-map (kbd "C-c C-/") 'elisp-slime-nav-describe-elisp-thing-at-point)
  (define-key elisp-slime-nav-mode-map (kbd "M-.") nil)
  (define-key elisp-slime-nav-mode-map (kbd "M-,") nil)
  (define-key elisp-slime-nav-mode-map (kbd "C-c C-d d") nil)
  (define-key elisp-slime-nav-mode-map (kbd "C-c C-d C-d") nil))
;; =================elisp-slime-nav================
;;; drag stuff
;; ===================drag stuff===================
(use-package drag-stuff
  ;; Enabled in modes.
  :defer t
  :bind (("M-P" . drag-stuff-up)
         ("M-N" . drag-stuff-down)
         ("M-B" . drag-stuff-left)
         ("M-F" . drag-stuff-right))
  :config
  (drag-stuff-global-mode t)
  ;; 重新定义drag-stuff-define-keys函数，取消 M+方向键 的快捷键。
  (defun drag-stuff-define-keys-cancel-keys ()
    "Defines keys for `drag-stuff-mode'."
    (define-key drag-stuff-mode-map (drag-stuff--kbd 'up) nil)
    (define-key drag-stuff-mode-map (drag-stuff--kbd 'down) nil)
    (define-key drag-stuff-mode-map (drag-stuff--kbd 'left) nil)
    (define-key drag-stuff-mode-map (drag-stuff--kbd 'right) nil))
  (advice-add 'drag-stuff-define-keys :override #'drag-stuff-define-keys-cancel-keys))
;; ===================drag stuff===================
;;; popup-kill-ring
;; ================popup-kill-ring=================
(use-package popup-kill-ring
  ;; Enabled at commands.
  :defer t
  :bind ("M-Y" . popup-kill-ring)
  :config
  (setq popup-kill-ring-interactive-insert nil)
  (setq popup-kill-ring-popup-width 30)
  (setq popup-kill-ring-popup-margin-left 0)
  (setq popup-kill-ring-popup-margin-right 0)
  (setq popup-kill-ring-timeout 0.5))
;; ================popup-kill-ring=================
;;; popup
;; ====================popup=======================
(use-package popup
  ;; Enabled at commands.
  :defer 2
  :commands (popup-tip popup-create popup-make-item)
  :config
  (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)
  (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
  (define-key popup-menu-keymap (kbd "TAB") 'popup-next))
;; ====================popup=======================
;;; pos-tip
;; ===================pos-tip======================
(use-package pos-tip
  ;; Enabled at commands.
  :defer 2
  :commands (pos-tip-show pos-tip-show-no-propertize))
;; ===================pos-tip======================
;;; elmacro
;; ===================elmacro======================
;; 需要先打开elmacro-mode，然后F3/F4录制宏。
;; 然后使用elmacro-show-last-macro来将操作转换为elisp。
;; ===================elmacro======================
;;; hungry-delete
;; ===================hungry-delete================
(use-package hungry-delete
  ;; Enabled at idle.
  :defer 2
  :config
  (add-to-list 'hungry-delete-except-modes 'dired-mode)
  (add-to-list 'hungry-delete-except-modes 'ein:notebook-mode)
  (global-hungry-delete-mode))
;; ===================hungry-delete================
;;; fcitx
;; ======================fcitx=====================
(use-package fcitx
  ;; Enabled automatically.
  :if is-lin
  :config
  (fcitx-prefix-keys-add "M-s" "M-g")
  (fcitx-aggressive-setup)
  (fcitx-isearch-turn-on))
;; ======================fcitx=====================
;;; aggressive-indent
;; ================aggressive-indent===============
(use-package aggressive-indent
  ;; Enabled at idle.
  :defer 2
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'asm-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (global-aggressive-indent-mode 1))
;; ================aggressive-indent===============
;;; clean-aindent-mode
;; ===============clean-aindent-mode===============
(use-package clean-aindent-mode
  ;; Enabled in modes.
  :defer t
  :commands clean-aindent-mode
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode)
  :config
  (clean-aindent-mode t)
  (setq clean-aindent-is-simple-indent t)
  (define-key global-map (kbd "RET") 'newline-and-indent)
  (define-key clean-aindent-mode--keymap (kbd "M-DEL") '(lambda ()
                                                          (interactive)
                                                          (if (or paredit-mode
                                                                  paredit-everywhere-mode)
                                                              (call-interactively 'swint-backward-kill-word)
                                                            (call-interactively 'clean-aindent--bsunindent)))))
;; RET：自动清除white space，光标停留在前一行indentation处。
;; M-DEL：unindent，回到前一行indentation处。
;; ===============clean-aindent-mode===============
;;; multifiles
;; ===================multifiles===================
(use-package multifiles
  ;; Enabled at commands.
  :defer t
  :bind ("M-s t" . mf/mirror-region-in-multifile))
;; ===================multifiles===================
;;; ztree
;; =====================ztree======================
(use-package ztree-diff
  ;; Enabled at commands.
  :defer t
  :bind ("M-s z" . ztree-diff))
;; =====================ztree======================
;;; which-key
;; ====================which-key===================
;; 原用guide-key，改用which-key。
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence
;;       '("C-c" (org-mode "C-c C-x")))
(use-package which-key
  ;; Enabled at idle.
  :defer 2
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-description-order)
  ;; 使用C-h或?切换页面。
  (setq which-key-use-C-h-for-paging t
        which-key-prevent-C-h-from-cycling nil)
  ;; 默认的C-h启用describe-prefix-bindings不带prefix。
  (defun which-key-show-standard-help-with-helm-descbinds ()
    (interactive)
    (let ((which-key-inhibit t))
      (which-key--hide-popup-ignore-command)
      (helm-descbinds which-key--current-prefix)))
  (advice-add 'which-key-show-standard-help :override
              #'which-key-show-standard-help-with-helm-descbinds))
;; ====================which-key===================
;;; pdf-tools
;; ====================pdf-tools===================
(use-package pdf-tools
  ;; Enabled in modes.
  :if is-lin
  :defer t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :init
  ;; Pdf-tools会默认设置x-gtk-use-system-tooltips为nil，导致chinese-pyim选词框失效。
  (setq pdf-annot-tweak-tooltips nil)
  :config
  (pdf-tools-install)
  (setq pdf-outline-imenu-use-flat-menus t)
  ;; pdf-view-auto-slice-minor-mode 翻页自动切边。
  ;; (add-hook 'pdf-view-mode-hook 'pdf-view-auto-slice-minor-mode)
  ;; 打开pdf时手动切边一次。手动切边(s b)，重设(s r)。
  ;; (add-hook 'pdf-view-mode-hook 'pdf-view-set-slice-from-bounding-box)
  (define-key pdf-view-mode-map (kbd "i") 'imenu)
  (define-key pdf-view-mode-map (kbd "M-w") 'pdf-view-kill-ring-save)
  (define-key pdf-view-mode-map (kbd "M-v") 'pdf-view-scroll-down-or-previous-page)
  (define-key pdf-view-mode-map (kbd "C-v") 'pdf-view-scroll-up-or-next-page)
  (define-key pdf-view-mode-map (kbd "C-p") '(lambda () (interactive) (pdf-view-previous-line-or-previous-page 3)))
  (define-key pdf-view-mode-map (kbd "C-n") '(lambda () (interactive) (pdf-view-next-line-or-next-page 3)))
  (define-key pdf-view-mode-map (kbd "C-c l") 'swint-interleave-open-notes-file-for-pdf))
;; Failed to fix the bug of pdf-view-mode.
;; (defun swint-pdf-history-goto-beginning ()
;;   "Fix the bug of reverting to beginning of pdf after persp-switch."
;;   (interactive)
;;   (let ((pdf-buffers (remove-if-not
;;                       (lambda (x) (eq (buffer-mode x) 'pdf-view-mode))
;;                       (persp-buffers persp-curr))))
;;     (loop for pdf-buffer in pdf-buffers
;;           do (with-current-buffer pdf-buffer
;;                (pdf-history-goto 0)))))
;; (add-hook 'persp-activated-hook 'swint-pdf-history-goto-beginning)
;; ====================pdf-tools===================
;;; doc-view-mode
;; ==================doc-view-mode=================
;; Lin上使用soffice转换；win上使用unoconv转换。
;; Pdf文件使用gs转换成png。
;; Win下使用doc-view查看office和pdf文件时，文件名都不可以包含中文字符。
;; 默认的缓存文件夹分别为/tmp和c:/Users/swint/AppData/Local/Temp，使用doc-view-clear-cache清理。
(use-package doc-view
  ;; Enabled in modes.
  :defer t
  :config
  (setq doc-view-continuous t)
  (define-key doc-view-mode-map (kbd "i") 'imenu)
  (define-key doc-view-mode-map (kbd "M-v") 'doc-view-scroll-down-or-previous-page)
  (define-key doc-view-mode-map (kbd "C-v") 'doc-view-scroll-up-or-next-page)
  (define-key doc-view-mode-map (kbd "C-p") '(lambda () (interactive) (doc-view-previous-line-or-previous-page 3)))
  (define-key doc-view-mode-map (kbd "C-n") '(lambda () (interactive) (doc-view-next-line-or-next-page 3)))
  (define-key doc-view-mode-map (kbd "C-c l") 'swint-interleave-open-notes-file-for-pdf)
  (when is-win
    ;; 使用libreoffice自带python.exe运行unoconv脚本。
    (setq doc-view-odf->pdf-converter-program "c:/Program Files (x86)/LibreOffice 5/program/python.exe")
    ;; 指定使用unoconv方法转换。
    (setq doc-view-odf->pdf-converter-function 'doc-view-odf->pdf-converter-unoconv)
    (defun doc-view-odf->pdf-converter-unoconv (odf callback)
      "Convert ODF to PDF asynchronously and call CALLBACK when finished.
The converted PDF is put into the current cache directory, and it
is named like ODF with the extension turned to pdf."
      (doc-view-start-process "odf->pdf" doc-view-odf->pdf-converter-program
                              (list "c:/Program Files (x86)/unoconv/unoconv" "-f" "pdf" "-o"
                                    ;; 修改下句，原函数会生成无base name文件。
                                    (concat (doc-view--current-cache-dir) (file-name-base odf) ".pdf") odf)
                              callback))))
;; ==================doc-view-mode=================
;;; backup
;; ======================backup====================
(use-package git-timemachine
  ;; Enabled at commands.
  :defer t
  :bind ("M-s M-b" . git-timemachine))
(use-package backup-walker
  ;; Enabled at commands.
  :defer t
  :bind ("M-s M-B" . backup-walker-start))
;; ======================backup====================
;;; visual-regexp
;; ===================visual-regexp================
(use-package visual-regexp
  ;; Enabled at commands.
  :defer t
  :bind (("M-s r" . vr/query-replace)
         ("M-s R" . vr/replace)
         ("M-s ;" . vr/mc-mark)))
;; ===================visual-regexp================
;;; vlf
;; =======================vlf======================
;; vlf把大文件分成多个batch，以改善性能。
(use-package vlf
  ;; Enabled at idle.
  :defer t
  :bind (:map dired-mode-map
              ("M-s v" . dired-vlf))
  :init
  ;; Enable vlf when opening files bigger than 100MB.
  (setq large-file-warning-threshold 100000000)
  :config
  (use-package vlf-setup)
  (define-key vlf-prefix-map "\C-c\C-v" nil)
  (define-key vlf-prefix-map "\M-sv" vlf-mode-map)
  (smartrep-define-key vlf-mode-map ""
    '(("n" . vlf-next-batch)
      ("p" . vlf-prev-batch)
      ("<" . vlf-beginning-of-file)
      (">" . vlf-end-of-file)))
  (custom-set-variables '(vlf-application 'dont-ask))
  (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode))
;; C-c C-v n and C-c C-v p move batch by batch.
;; C-c C-v SPC displays batch starting from current point.
;; C-c C-v [ and C-c C-v ] take you to the beginning and end of file respectively.
;; C-c C-v j jumps to particular batch number.
;; C-c C-v + and C-c C-v - control current batch size by factors of 2.
;; C-c C-v s and C-c C-v r search forward and backward respectively over the whole file, batch by batch.
;; C-c C-v % does search and query replace saving intermediate changes.
;; C-c C-v l jumps to given line in file.
;; M-x vlf-ediff-files and M-x vlf-ediff-buffers to compare files/buffers batch by batch.
;; C-c C-v o builds index over whole file for given regular expression just like M-x occur.
;; =======================vlf======================
;;; easy-kill
;; =====================easy-kill==================
(use-package easy-kill
  ;; Enabled at commands.
  :defer t
  :bind ("M-w" . kill-ring-save)
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark)
  :config
  (define-key easy-kill-base-map (kbd "C-w") 'easy-kill-region)
  (define-key easy-kill-base-map (kbd ",") 'easy-kill-shrink)
  (define-key easy-kill-base-map (kbd ".") 'easy-kill-expand))
;; M-w ?: help 查看M-w prefix快捷键。
;; =====================easy-kill==================
;;; smex
;; ======================smex======================
(use-package smex
  ;; Enabled at commands.
  :defer t
  :bind (("C-x M-x" . smex)
         ("C-c M-x" . smex-major-mode-commands)))
;; ======================smex======================
;;; bm
;; =======================bm=======================
(use-package bm
  ;; Enabled at commands.
  :defer t
  :commands (bm-toggle bm-previous bm-next)
  :init
  (smartrep-define-key global-map "C-x"
    '(("C-'" . bm-toggle)
      ("'" . bm-previous)
      ("\"" . bm-next)))
  (setq bm-restore-repository-on-load t)
  :config
  (setq-default bm-buffer-persistence t)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda ()
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (setq bm-cycle-all-buffers nil)
  (setq bm-highlight-style 'bm-highlight-only-fringe)
  (bm-buffer-restore-all))
;; =======================bm=======================
;;; helm-bm
;; ====================helm-bm=====================
(use-package helm-bm
  ;; Enabled after features.
  :defer t
  :bind ("C-M-'" . helm-bm)
  :config
  (defun helm-bm-action-switch-to-persp/buffer (candidate)
    "Switch to buffer of CANDIDATE."
    (helm-bm-with-candidate candidates
                            (helm-switch-persp/buffer bufname)
                            (goto-char (point-min))
                            (forward-line (1- lineno))))
  (advice-add 'helm-bm-action-switch-to-buffer :override
              #'helm-bm-action-switch-to-persp/buffer))
;; ====================helm-bm=====================
;;; operate-on-number
;; ================operate-on-number===============
;; 两种操作方式：C-= 计算符号，支持C-u前缀数字；C-= = 依次确定计算符号和数字。
(use-package operate-on-number
  ;; Enabled at commands.
  :defer t
  :commands (apply-operation-to-number-at-point operate-on-number-at-point)
  :init
  (smartrep-define-key global-map "C-="
    '(("+" . apply-operation-to-number-at-point)
      ("-" . apply-operation-to-number-at-point)
      ("*" . apply-operation-to-number-at-point)
      ("/" . apply-operation-to-number-at-point)
      ("\\" . apply-operation-to-number-at-point)
      ("^" . apply-operation-to-number-at-point)
      ("<" . apply-operation-to-number-at-point)
      (">" . apply-operation-to-number-at-point)
      ("#" . apply-operation-to-number-at-point)
      ("%" . apply-operation-to-number-at-point)
      ("=" . operate-on-number-at-point))))
;; ================operate-on-number===============
;;; goto-last-change
;; =================goto-last-change===============
(use-package goto-chg
  ;; Enabled at commands.
  :defer t
  :bind ("M-s M-m" . swint-goto-last-change-with-prefix)
  :config
  (defun swint-goto-last-change-with-prefix (&optional arg)
    (interactive)
    (goto-last-change arg)
    (setq last-command 'goto-last-change)
    ;; (invoke-prefix-command)
    ;; (initialize-event-loop)
    ;; The form (condition-case ERR FORM (quit QUIT-HANDLER))
    ;; is there to catch C-g key presses and make sure that
    ;; finalization code is run.
    (condition-case e
        (smartrep-read-event-loop
         '(("M-m" . goto-last-change)
           ("M-M" . goto-last-change-reverse)))
      (quit nil))
    ;; (finalize-event-loop)
    ))
;; =================goto-last-change===============
;;; Proced
;; =====================Proced=====================
(use-package proced
  ;; Enabled at commands.
  :defer t
  :commands proced-process-attributes
  :bind ("C-M-4" . proced)
  :config
  (define-key proced-mode-map (kbd "q") 'kill-buffer-and-window)
  ;; Proced自动更新，10秒。
  (defun proced-settings ()
    (proced-toggle-auto-update 1))
  (add-hook 'proced-mode-hook 'proced-settings))
;; =====================Proced=====================
;;; bbyac
;; =====================bbyac======================
(use-package bbyac
  ;; Enabled at commands.
  :defer t
  :bind (("M-s M-u" . bbyac-expand-substring)
         ("M-s M-U" . bbyac-expand-symbols))
  :config
  (bbyac-global-mode 1)
  (setq browse-kill-ring-display-style 'one-line)
  (define-key bbyac-mode-map (kbd "M-s M-u") 'bbyac-expand-substring)
  (define-key bbyac-mode-map (kbd "M-s M-U") 'bbyac-expand-symbols)
  (define-key bbyac-mode-map (kbd "M-g <return>") nil)
  (define-key bbyac-mode-map (kbd "M-g RET") nil)
  (define-key bbyac-mode-map (kbd "M-s <return>") nil)
  (define-key bbyac-mode-map (kbd "M-s <RET>") nil)
  (define-key bbyac-mode-map (kbd "M-g x") nil)
  (define-key bbyac-mode-map (kbd "M-s l") nil)
  (define-key bbyac-mode-map (kbd "M-s s") nil)
  (define-key bbyac-mode-map (kbd "M-s p") nil))
;; =====================bbyac======================
;;; avy-menu
;; ===================avy-menu=====================
(use-package avy-menu
  ;; Enabled at idle.
  :defer 2)
(use-package ace-popup-menu
  ;; Enabled after features.
  :defer t
  :after avy-menu
  :config
  (ace-popup-menu-mode 1)
  (setq ace-popup-menu-show-pane-header t))
;; ===================avy-menu=====================
;;; char-menu
;; ===================char-menu====================
(use-package char-menu
  ;; Enabled at commands.
  :defer t
  :bind ("M-s M" . char-menu)
  :config
  (setq char-menu '(;; "—" "‘’" "“”" "…" "«»" "–"
                    ("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
                    ("Math" "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√")
                    ("Arrows" "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
                    ("Greek small"
                     "α" "β" "Y" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω")
                    ("Greek capital"
                     "Α" "Β" "Γ" "Δ" "Ε" "Ζ" "Η" "Θ" "Ι" "Κ" "Λ" "Μ" "Ν" "Ξ" "Ο" "Π" "Ρ" "Σ" "Τ" "Υ" "Φ" "Χ" "Ψ" "Ω"))))
;; ===================char-menu====================
;;; vimish-fold
;; ==================vimish-fold===================
(use-package vimish-fold
  ;; Enabled at commands.
  :defer t
  :bind (("C-x C-`" . vimish-fold)
         ("C-x C-~" . vimish-fold-delete)))
;; ==================vimish-fold===================
;;; clipmon
;; ====================clipmon=====================
(use-package clipmon
  ;; Enabled after features.
  :defer t
  :after easy-kill
  :config
  (clipmon-mode-start)
  (clipmon-persist)
  (defun clipmon--get-selection ()
    "Get the clipboard contents"
    ;; Note: When the OS is first started these functions will throw
    ;; (error "No selection is available"), so need to ignore errors.
    (cond ((fboundp 'gui-get-selection) ; emacs25
                                        ; better to (setq selection-coding-system 'utf-8) to handle chinese,
                                        ; which is the default value for gui-get-selection etc
                                        ; because windows needs STRING. same below.
                                        ; (ignore-errors (gui-get-selection 'CLIPBOARD 'UTF8_STRING)))
           (ignore-errors (gui-get-selection 'CLIPBOARD))) ; for windows needs STRING
          ((eq window-system 'w32) ; windows/emacs24
           ;; Note: (x-get-selection 'CLIPBOARD) doesn't work on Windows.
           (ignore-errors (x-get-selection-value))) ; can be nil
          (t ; linux+osx/emacs24
                                        ; (ignore-errors (x-get-selection 'CLIPBOARD 'UTF8_STRING)))))
           (ignore-errors (x-get-selection 'CLIPBOARD 'UTF8_STRING))))))
;; ====================clipmon=====================
;;; volatile-highlights
;; ==============volatile-highlights===============
(use-package volatile-highlights
  ;; Enabled at idle.
  :defer 2
  :config
  (volatile-highlights-mode t))
;; ==============volatile-highlights===============
;;; quickrun
;; ===================quickrun=====================
(use-package quickrun
  :defer t
  :bind (("M-s q" . swint-quickrun)
         ("M-s Q" . quickrun-shell))
  :config
  (defun swint-quickrun ()
    (interactive)
    (if mark-active
        (call-interactively 'quickrun-region)
      (call-interactively 'quickrun))))
;; ===================quickrun=====================
;;; highlight-symbol
;; ================highlight-symbol================
(use-package highlight-symbol
  ;; Enabled at commands.
  :defer t
  :commands (highlight-symbol-prev highlight-symbol-next highlight-symbol-at-point highlight-symbol-get-symbol)
  :init
  (smartrep-define-key global-map "C-x"
    '(("," . highlight-symbol-prev)
      ("." . highlight-symbol-next)
      (";" . highlight-symbol-at-point)))
  :config
  (setq highlight-symbol-foreground-color "gray30"))
;; ================highlight-symbol================
;;; auto-highlight-symbol
;; =============auto-highlight-symbol==============
(use-package auto-highlight-symbol
  ;; Enabled at idle.
  :defer 2
  :init
  (setq ahs-default-range 'ahs-range-whole-buffer)
  :config
  (global-auto-highlight-symbol-mode t)
  (set-face-attribute 'ahs-face nil
                      :foreground "white"
                      :background "DarkMagenta")
  (set-face-attribute 'ahs-plugin-whole-buffer-face nil
                      :foreground "white"
                      :background "DarkBlue")
  (add-to-list 'ahs-modes 'octave-mode)
  (defun swint-ahs-backward ()
    (interactive)
    (let ((symbol (highlight-symbol-get-symbol)))
      (unless ahs-highlighted
        (ahs-highlight-now))
      (call-interactively 'ahs-backward)
      (highlight-symbol-count symbol t)))
  (defun swint-ahs-forward ()
    (interactive)
    (let ((symbol (highlight-symbol-get-symbol)))
      (unless ahs-highlighted
        (ahs-highlight-now))
      (call-interactively 'ahs-forward)
      (highlight-symbol-count symbol t)))
  (add-to-list 'ahs-unhighlight-allowed-commands 'swint-ahs-forward)
  (add-to-list 'ahs-unhighlight-allowed-commands 'swint-ahs-backward)
  (smartrep-define-key auto-highlight-symbol-mode-map "C-x"
    '(("," . swint-ahs-backward)
      ("." . swint-ahs-forward)
      ("/" . ahs-edit-mode)))
  (define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M--") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "C-x C-'") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "C-x C-a") nil))
;; =============auto-highlight-symbol==============
;;; dumb-jump
;; ==================dumb-jump=====================
(use-package dumb-jump
  ;; Enabled at idle.
  :defer 2
  :config
  (dumb-jump-mode)
  (define-key dumb-jump-mode-map (kbd "C-M-g") nil)
  (define-key dumb-jump-mode-map (kbd "C-M-p") nil)
  (define-key dumb-jump-mode-map (kbd "C-M-q") nil)
  (define-key dumb-jump-mode-map (kbd "C-x C-,") 'dumb-jump-go)
  (define-key dumb-jump-mode-map (kbd "C-x C-.") 'dumb-jump-back)
  (define-key dumb-jump-mode-map (kbd "C-x C-/") 'dumb-jump-quick-look))
;; ==================dumb-jump=====================
;;; diff-hl
;; ===================diff-hl======================
(use-package diff-hl
  ;; Enabled at idle.
  :defer 2
  :config
  ;; 默认快捷键以C-x v为前缀。
  (smartrep-define-key global-map "C-x v"
    '((";" . diff-hl-mark-hunk)
      ("," . diff-hl-previous-hunk)
      ("." . diff-hl-next-hunk)
      ("/" . diff-hl-revert-hunk)))
  (global-diff-hl-mode)
  (add-hook 'prog-mode-hook 'diff-hl-flydiff-mode)
  (add-hook 'text-mode-hook 'diff-hl-flydiff-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (dolist (buf (remove-if-not (lambda (x)
                                (equal (buffer-mode x) 'dired-mode))
                              (helm-buffer-list)))
    (with-current-buffer buf
      (diff-hl-dired-mode)
      (diff-hl-dired-update)))
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (set-face-attribute 'diff-hl-insert nil
                      :foreground "gray"
                      :background "dark green")
  (set-face-attribute 'diff-hl-delete nil
                      :foreground "gray"
                      :background "dark red")
  (set-face-attribute 'diff-hl-change nil
                      :foreground "gray"
                      :background "dark blue"))
;; ===================diff-hl======================
;;; evil-nerd-commenter
;; =============evil-nerd-commenter================
(use-package evil-nerd-commenter
  ;; Enabled at commands.
  :defer t
  :bind ("M-:" . evilnc-comment-or-uncomment-lines))
;; =============evil-nerd-commenter================
;;; ivy/swiper/counsel/hydra
;; ===========ivy/swiper/counsel/hydra=============
(use-package ivy
  ;; Enabled after features.
  :defer t
  :after (swiper counsel)
  :config
  (bind-key "M-s y" 'ivy-resume)
  (bind-key "C-h" 'ivy-avy ivy-minibuffer-map)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "%d/%d "))
(use-package swiper
  ;; Enabled at commands.
  :defer t
  :bind (("M-s s" . swint-swiper)
         ("M-s S" . swiper-all)
         :map isearch-mode-map
         ("M-s s" . swiper-from-isearch))
  :config
  (defun swint-swiper ()
    (interactive)
    (let ((swint-swiper-current-thing
           (if (region-active-p)
               (buffer-substring (region-beginning) (region-end))
             (symbol-name-at-point))))
      (deactivate-mark)
      (swiper swint-swiper-current-thing))))
(use-package counsel
  ;; Enabled at commands.
  :defer t
  ;; 按键逻辑：helm(C-x c x)/counsel(M-s c x)。
  :bind (("M-X" . counsel-M-x)
         ("C-x C-r" . swint-counsel-history)
         ("M-s `" . counsel-tmm)
         ("M-s m" . counsel-unicode-char)
         ("M-s c l" . counsel-locate)
         ("M-s c o" . counsel-outline)
         ("M-s c d" . counsel-dpkg)
         ("M-s c g" . counsel-ag)
         ("M-s c p" . counsel-list-processes)
         ("M-s c M-y" . counsel-yank-pop))
  :config
  (defun swint-counsel-history ()
    "List command history based on major-mode."
    (interactive)
    (cond
     ((memq major-mode '(shell-mode inferior-python-mode inferior-octave-mode))
      (call-interactively 'counsel-shell-history))
     ((eq major-mode 'eshell-mode)
      (call-interactively 'counsel-esh-history))
     (t (call-interactively 'swint-counsel-bash-history))))
  (defun swint-counsel-bash-history ()
    "Insert the bash history."
    (interactive)
    (let (hist-cmd collection val)
      (shell-command "history -r") ; reload history
      (setq collection
            (nreverse
             (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                             (buffer-string))
                           "\n" t)))
      (when (and collection (> (length collection) 0)
                 (setq val (if (= 1 (length collection)) (car collection)
                             (ivy-read (format "Bash history:") collection))))
        (insert val)))))
(use-package ivy-hydra
  ;; Enabled after features.
  :defer t
  :after ivy)
(use-package hydra
  ;; Enabled after features.
  :defer t
  :after ivy-hydra)
;; ===========ivy/swiper/counsel/hydra=============
;; =================markdown-mode==================
(use-package markdown-mode
  ;; Enabled in modes.
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))
;; =================markdown-mode==================
(provide 'setup_packages)
