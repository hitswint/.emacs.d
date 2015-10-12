;; =======================SERVER========================
(require 'server)
(unless (server-running-p)
  (server-start))
;; =======================SERVER========================
;; ======================文件加密===================================
(require 'epa-file)
;;(epa-file-enable)
(setenv "GPG_AGENT_INFO" nil)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setq epa-file-inhibit-auto-save nil)
;; ======================文件加密===================================
;; ====================multiple-cursors============================
(require 'multiple-cursors)
;; (global-set-key (kbd "C-M-;") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-M-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-0") 'mc/edit-lines)
(global-set-key (kbd "C-c C-9") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-8") 'mc/insert-numbers)
;; mc/i:mc/insert-numbers: Insert increasing numbers for each cursor, top to bottom.
;; mc/sort-regions: Sort the marked regions alphabetically.
;; mc/reverse-regions: Reverse the order of the marked regions.
;; messages中会出现Error during redisplay: (error Lisp nesting exceeds `max-lisp-eval-depth')的错误。
;; ====================multiple-cursors============================
;; ====================expand-region=========================
(require 'expand-region)
(global-set-key (kbd "C-M-;") 'er/expand-region)
;; 在octave中使用会导致emacs假死，原因是octave的function中必须带有end。
;; emacs不是真正的假死，C-g会恢复。
;; ====================expand-region=========================
;; ==================回收站=======================
(require 'trashcan)
;; ==================回收站=======================
;; ==================undo-tree===================
(require 'undo-tree)
(global-undo-tree-mode)
(add-hook 'undo-tree-mode-hook
          '(lambda ()
             (define-key undo-tree-map (kbd "M-_") nil)))
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-M-/") 'undo-tree-redo)
;; ==================undo-tree===================
;; ==================breadcrumb==================
(require 'breadcrumb)
(global-set-key (kbd "C-c C-/") 'bc-set)
(global-set-key (kbd "C-c C-,") 'bc-previous)
(global-set-key (kbd "C-c C-.") 'bc-next)
;; 删除breadcrumb.el源文件中(message "breadcrumb bookmark is set for the current position.")，使bc-set不出现提示。
;; (global-set-key (kbd "C-c /") 'bc-list) ;; C-x M-j for the bookmark menu list
;; (global-set-key (kbd "C-c /") 'bc-local-previous) ;; M-up-arrow for local previous
;; (global-set-key (kbd "C-c /") 'bc-local-next)     ;; M-down-arrow for local next
;; (global-set-key (kbd "C-c /") 'bc-goto-current) ;; C-c j for jump to current bookmark
;; ==================breadcrumb==================
;; ==================auto-mark===================
(require 'auto-mark)
(global-auto-mark-mode 1)
;; 会导致(void-variable last-command-char)错误
;; (when (require 'auto-mark nil t)
;;   (setq auto-mark-command-class-alist
;;         '((anything . anything)
;;           (goto-line . jump)
;;           (indent-for-tab-command . ignore)
;;           (undo . ignore)))
;;   (setq auto-mark-command-classifiers
;;         (list (lambda (command)
;;                 (if (and (eq command 'self-insert-command)
;;                          (eq last-command-char ? ))
;;                     'ignore))))
;;   (global-auto-mark-mode 1))
(defun jump-to-mark ()
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-m") 'jump-to-mark)
;; ==================auto-mark===================
;; =================visible-mark=================
(require 'visible-mark)
(global-visible-mark-mode 1)
;; 下面的代码增加一个桔黄色的mark，显示灰色和桔黄色两个mark
(setq visible-mark-max 2)
(defface my-visible-mark-face-2
  '((t (:background "#666666" ;; :foreground "white"
                    )))
  "Face for the mark."
  :group 'visible-mark)
(setq visible-mark-faces '(visible-mark-active my-visible-mark-face-2))
;; =================visible-mark=================
;; =================smooth-scrolling=================
(require 'smooth-scrolling)
;; =================smooth-scrolling=================
;; ====================ace-jump-buffer========================
;; 放弃ace-jump-buffer
(require 'ace-jump-buffer)
(global-set-key (kbd "C-c ,") 'ace-jump-buffer)
;; ====================ace-jump-buffer========================
;; =====================unicad=====================
;; lin中不会出现乱码，不需要，这个包会拖慢启动速度
(when is-win
  (require 'unicad)
  ;; 解决关emacs时保存.session的编码问题
  (setq session-save-file-coding-system 'utf-8)
  ;; set default encoding to utf-8
  (setq-default buffer-file-coding-system 'utf-8)
  ;; set writing buffer default to utf-8, or emacs always show encoding problem when saving files.
  (setq save-buffer-coding-system 'utf-8)
  (setq coding-system-for-write 'utf-8))
;; =====================unicad=====================
;; =====================everything======================
(when is-win
  (setq everything-ffap-integration nil) ;; to disable ffap integration
  (setq everything-matchpath t)
  (setq everything-cmd "c:/Program Files/Everything/es.exe") ;; to let everything.el know where to find es.exe
  (require 'everything)
  (global-set-key (kbd "C-c C-S-f") 'everything)
  (global-set-key (kbd "C-c C-M-f") '(lambda () (interactive)
                                       (w32-shell-execute
                                        "open" "c:/Program Files/Everything/Everything.exe")))
  (global-set-key (kbd "C-c M-f") '(lambda () (interactive)
                                     (w32-shell-execute
                                      "open" "c:/Program Files/Everything/Everything.exe"
                                      (concat "-p " (expand-file-name default-directory))))))
;; =====================everything======================
;; =====================popwin======================
(require 'popwin)
(popwin-mode 1)
;; =====================popwin======================
;; =====================anchored-transpose======================
(require 'anchored-transpose)
(global-set-key [?\C-x ?t] 'anchored-transpose)
(autoload 'anchored-transpose "anchored-transpose" nil t)
;; First select the entire phrase and type C-x t. Then select the anchor phrase and type C-x t again. You’re done!
;; 首先选择整个区域，然后选择锚点，锚点两侧的内容交换
;; You can select the anchor phrase first followed by the phrase to be transposed if more convenient. Or select the 2 phrases independently. If you select 2 overlapping sections it ignores the overlap and swaps the non-overlapping sections. It even supports SecondarySelection. Somehow it can always tell what you want ;-)
;; 另外，可以分别选择两部分交换的内容
;; =====================anchored-transpose======================
;; =====================God-mode======================
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)
;; (global-set-key (kbd "<escape>") 'god-mode-all)
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'bar
                      'box)))
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
(add-to-list 'god-exempt-major-modes 'dired-mode)
;; =====================God-mode======================
;; =====================elisp-slime-nav======================
(global-set-key (kbd "C-M-'") 'help-for-help)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(require 'cl-lib)
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))
(define-key elisp-slime-nav-mode-map (kbd "C-x C-,") 'elisp-slime-nav-find-elisp-thing-at-point)
(define-key elisp-slime-nav-mode-map (kbd "C-x C-.") 'pop-tag-mark)
(define-key elisp-slime-nav-mode-map (kbd "C-x C-/") 'elisp-slime-nav-describe-elisp-thing-at-point)
(define-key elisp-slime-nav-mode-map (kbd "M-.") nil)
(define-key elisp-slime-nav-mode-map (kbd "M-,") nil)
(define-key elisp-slime-nav-mode-map (kbd "C-c C-d d") nil)
(define-key elisp-slime-nav-mode-map (kbd "C-c C-d C-d") nil)
;; =====================elisp-slime-nav======================
;; ===================diredful======================
(require 'diredful)
;; ===================diredful======================
;; ===================drag stuff====================
(require 'drag-stuff)
(add-hook 'dired-mode-hook 'drag-stuff-mode)
(add-hook 'octave-mode-hook 'drag-stuff-mode)
(add-hook 'org-mode-hook 'drag-stuff-mode)
(add-hook 'gnuplot-mode-hook 'drag-stuff-mode)
(add-hook 'emacs-lisp-mode-hook 'drag-stuff-mode)
(add-hook 'c-mode-hook 'drag-stuff-mode)
(add-hook 'graphviz-dot-mode-hook 'drag-stuff-mode)
(add-hook 'LaTeX-mode-hook 'drag-stuff-mode)
;; 重新定义drag-stuff.el文件中的 drag-stuff-define-keys 函数，取消关于 M+方向键 的快捷键定义。
(defun drag-stuff-define-keys ()
  "Defines keys for `drag-stuff-mode'."
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'up) nil)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'down) nil)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'right) nil)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'left) nil))
(define-key drag-stuff-mode-map (kbd "M-P") 'drag-stuff-up)
(define-key drag-stuff-mode-map (kbd "M-N") 'drag-stuff-down)
(define-key drag-stuff-mode-map (kbd "M-B") 'drag-stuff-left)
(define-key drag-stuff-mode-map (kbd "M-F") 'drag-stuff-right)
;; ===================drag stuff====================
;; ===================popup-kill-ring====================
(require 'popup)
(require 'pos-tip)
;; (pos-tip-w32-max-width-height)   ; Maximize frame temporarily
;; (pos-tip-w32-max-width-height t) ; Keep frame maximized
(require 'popup-kill-ring)
(global-set-key "\C-\M-y" 'popup-kill-ring) ; For example.
(setq popup-kill-ring-interactive-insert t)
;; ===================popup-kill-ring====================
;; ===================highlight-symbol====================
(require 'highlight-symbol)
(global-set-key (kbd "M-g h") 'highlight-symbol-at-point)
(global-set-key (kbd "M-g n") 'highlight-symbol-next)
(global-set-key (kbd "M-g p") 'highlight-symbol-prev)
(global-set-key (kbd "M-g r") 'highlight-symbol-query-replace)
;; ===================highlight-symbol====================
;; ===================elmacro====================
;; 需要先打开elmacro-mode，然后F3/F4录制宏
;; 然后使用elmacro-show-last-macro来将操作转换为elisp
;; ===================elmacro====================
;; ===================hungry-delete====================
(require 'hungry-delete)
(global-hungry-delete-mode)
(add-hook 'wdired-mode-hook
          (lambda ()
            (setq hungry-delete-mode nil)))
;; ===================hungry-delete====================
;; ===================imenu-anywhere====================
;; elpa安装imenu-anywhere
;; imenu-anywhere与imenu额区别在于，前者包括所有打开的相同mode的buffer，而后者只限于当前buffer。
;; 但是imenu-anywhere在初次使用时经常失效，没有结果。
(global-set-key (kbd "M-s i") 'helm-imenu)
(global-set-key (kbd "M-s C-i") 'helm-imenu-anywhere)
;; ===================imenu-anywhere====================
;; ================fcitx.el=================
;; https://github.com/cute-jumper/fcitx.el
(when is-lin
  (require 'fcitx)
  (fcitx-prefix-keys-add "M-s")
  (fcitx-default-setup)
  (fcitx-M-x-turn-on)
  (fcitx-shell-command-turn-on)
  (fcitx-eval-expression-turn-on)
  ;; (fcitx-aggressive-minibuffer-turn-on)
  ;; 会导致tramp问题，使需要在minibuffer输入密码时hang
  )
;; ================fcitx.el=================
;; ============aggressive-indent=============
(global-aggressive-indent-mode 1)
;; ============aggressive-indent=============
;; ============clean-aindent-mode=============
(clean-aindent-mode t)
(setq clean-aindent-is-simple-indent t)
(add-hook 'prog-mode-hook 'clean-aindent-mode)
(define-key global-map (kbd "RET") 'newline-and-indent)
;; RET：自动清除white space，光标停留在前一行indentation处。
;; M-DEL：unindent，回到前一行indentation处。
;; ============clean-aindent-mode=============
;; ============multifiles=============
(require 'multifiles)
(global-set-key (kbd "C-c t") 'mf/mirror-region-in-multifile)
;; ============multifiles=============
;; ============ztree=============
(require 'ztree-diff)
(global-set-key (kbd "C-c z") 'ztree-diff)
;; ============ztree=============
;; ============guide-key=============
(require 'guide-key)
(setq guide-key/guide-key-sequence
      '("M-s" "C-x 8"
        (org-mode "C-c C-x")
        ))
(guide-key-mode 1)
(setq guide-key/popup-window-position 'bottom)
;; ============guide-key=============
;; ===================pdf-tools=================
;; 添加的注释跟okular类似，将注释的内容另存为一份文件，只有在emacs中才能看到
;; 上述注释为旧版本，新版本的pdf-tools已经可以将注释放在pdf文件本身之中了。
(when is-lin
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook 'pdf-annot-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-outline-minor-mode)
  ;; (add-hook 'pdf-view-mode-hook 'pdf-view-auto-slice-minor-mode)
  ;; pdf-view-auto-slice-minor-mode 翻页自动切边。
  (add-hook 'pdf-view-mode-hook 'pdf-view-set-slice-from-bounding-box)
  ;; 打开pdf时手动切边一次。手动切边(s b)，重设(s r)。
  (define-key pdf-view-mode-map (kbd "C-c C-i") nil)
  (define-key pdf-view-mode-map (kbd "M-s i") 'helm-imenu)
  (define-key pdf-view-mode-map (kbd "M-v") 'pdf-view-scroll-down-or-previous-page)
  (define-key pdf-view-mode-map (kbd "C-v") 'pdf-view-scroll-up-or-next-page)
  (define-key pdf-view-mode-map (kbd "C-p") '(lambda () (interactive) (pdf-view-previous-line-or-previous-page 3)))
  (define-key pdf-view-mode-map (kbd "C-n") '(lambda () (interactive) (pdf-view-next-line-or-next-page 3)))
  (define-key pdf-view-mode-map (kbd "M-w") 'pdf-view-kill-ring-save)
  (define-key pdf-view-mode-map (kbd "C-x C-l") '(lambda () (interactive)
                                                   (dired-jump-other-window)
                                                   (org-annotate-file (abbreviate-file-name (dired-get-filename)))))
  (define-key pdf-view-mode-map (kbd "C-c C-l") '(lambda () (interactive)
                                                   (dired-jump-other-window)
                                                   (swint-org-annotate-file (abbreviate-file-name (dired-get-filename)))))
  ;; Failed to fix the bug of pdf-view-mode.
  ;; (defun swint-pdf-history-goto-beginning ()
  ;;   "Fix the bug of reverting to beginning of pdf after persp-switch."
  ;;   (interactive)
  ;;   (let ((pdf-buffers (remove-if-not (lambda (x) (eq (buffer-mode x) 'pdf-view-mode)) (persp-buffers persp-curr))))
  ;;     (loop for pdf-buffer in pdf-buffers
  ;;           do (with-current-buffer pdf-buffer
  ;;                (pdf-history-goto 0)))))
  ;; (add-hook 'persp-activated-hook 'swint-pdf-history-goto-beginning)
  )
;; ===================pdf-tools=================
;; ==================doc-view-mode================
;; lin上使用soffice转换；win上使用unoconv转换。
;; pdf文件使用gs转换成png。
;; win下使用doc-view查看office和pdf文件时，文件名都不可以包含中文字符。
;; 默认的缓存文件夹分别为/tmp和c:/Users/swint/AppData/Local/Temp，使用doc-view-clear-cache清理。
(setq doc-view-continuous t)
(define-key doc-view-mode-map (kbd "M-v") 'doc-view-scroll-down-or-previous-page)
(define-key doc-view-mode-map (kbd "C-v") 'doc-view-scroll-up-or-next-page)
(define-key doc-view-mode-map (kbd "C-p") '(lambda () (interactive) (doc-view-previous-line-or-previous-page 3)))
(define-key doc-view-mode-map (kbd "C-n") '(lambda () (interactive) (doc-view-next-line-or-next-page 3)))
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
                            callback)))
;; ==================doc-view-mode================
;; =================total commander===============
(when is-win
  ;;===========使用tc打开当前文件夹===========
  (global-set-key (kbd "C-x j") '(lambda ()
                                   (interactive)
                                   (w32-shell-execute
                                    "open" "c:/totalcmd/TOTALCMD.EXE" (concat "/O /T \" " (expand-file-name default-directory)))))
  ;;========使用lister直接浏览文件========
  (define-key dired-mode-map (kbd "C-M-j") '(lambda ()
                                              (interactive)
                                              (w32-shell-execute
                                               "open" "c:/totalcmd/TOTALCMD.EXE" (concat "/O /T /S=L \" " (dired-get-filename)))))
  (defun helm-open-file-with-lister (_candidate)
    "Opens a file with lister of total commander."
    (w32-shell-execute
     "open" "c:/totalcmd/TOTALCMD.EXE" (concat "/O /T /S=L \" " (expand-file-name _candidate))))
  (defun helm-ff-run-open-file-with-lister ()
    "Run Rename file action from `helm-source-find-files'."
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-open-file-with-lister)))
  (define-key helm-find-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-lister)
  (define-key helm-generic-files-map (kbd "C-M-j") 'helm-ff-run-open-file-with-lister))
;; =================total commander===============
(provide 'setup_packages)
