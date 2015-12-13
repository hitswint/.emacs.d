;; ================wicd-mode=================
(use-package wicd-mode
  :disabled
  :load-path "site-lisp/wicd-mode/"
  :if is-lin
  :defer t
  :bind ("C-M-$" . wicd)
  :config
  (defvar dbus-object-end-scan nil)
  (defvar dbus-object-start-scan nil)
  (defvar dbus-object-connect nil)
  ;; 解决wicd关闭后仍然刷新wicd的问题。
  ;; 源程序在加载文件时创建signal，其后并不关闭。
  ;; 现在更改为启动wicd-mode时创建signal，退出时注销signal。
  ;; 同时注释掉源文件中dbus-register-signal相关项。
  ;; 可以使用dbus-registered-objects-table查看当前signals。
  (defun dbus-register-objects-for-wicd-mode ()
    "Redisplay wireless network list."
    (interactive)
    (setq dbus-object-end-scan (dbus-register-signal
                                :system
                                wicd-dbus-name
                                (wicd-dbus-path :wireless)
                                (wicd-dbus-name :wireless)
                                "SendEndScanSignal"
                                (lambda ()
                                  (setq wicd-wireless-scanning nil)
                                  (run-hooks 'wicd-wireless-scan-hook))))
    (setq dbus-object-start-scan (dbus-register-signal
                                  :system
                                  wicd-dbus-name
                                  (wicd-dbus-path :wireless)
                                  (wicd-dbus-name :wireless)
                                  "SendStartScanSignal"
                                  (lambda ()
                                    (setq wicd-wireless-scanning t)
                                    (with-current-buffer (wicd-buffer)
                                      (let (buffer-read-only)
                                        (erase-buffer)
                                        (insert "Scanning…\n"))))))
    (setq dbus-object-connect (dbus-register-signal
                               :system
                               wicd-dbus-name
                               (wicd-dbus-path :daemon)
                               (wicd-dbus-name :daemon)
                               "ConnectResultsSent"
                               (lambda (s)
                                 (message "Connexion " s)
                                 (when (string= s "success")
                                   (wicd-wireless-emphase-network))))))
  (defun quit-wicd-mode ()
    "Redisplay wireless network list."
    (interactive)
    (kill-buffer-and-window)
    (dbus-unregister-object dbus-object-end-scan)
    (dbus-unregister-object dbus-object-start-scan)
    (dbus-unregister-object dbus-object-connect))
  (add-hook 'wicd-mode-hook 'dbus-register-objects-for-wicd-mode)
  (define-key wicd-mode-map (kbd "q") 'quit-wicd-mode))
;; ================wicd-mode=================
;; ====================ace-jump=========================
;; 使用avy替代ace-jump。
(use-package ace-jump-mode
  :disabled
  :defer t
  :bind (("C-h" . swint-ace-jump-char-mode)
         ("C-c C-h" . ace-jump-mode)
         ("C-M-h" . ace-jump-line-mode))
  :config
  (use-package pinyin-search)
  ;; (setq ace-jump-mode-gray-background nil)
  ;; (setq ace-jump-mode-move-keys
  ;;       (nconc (loop for i from ?a to ?z collect i)
  ;;              (loop for i from ?0 to ?9 collect i)
  ;;              (loop for i from ?A to ?Z collect i)))
  ;; If you use viper mode:
  ;; (define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
  (defun swint-ace-jump-search-candidate (re-query-string visual-area-list)
    "Search the RE-QUERY-STRING in current view, and return the candidate position list.
RE-QUERY-STRING should be an valid regex used for `search-forward-regexp'.
You can control whether use the case sensitive or not by `ace-jump-mode-case-fold'.
Every possible `match-beginning' will be collected.
The returned value is a list of `aj-position' record."
    (loop for va in visual-area-list
          append (let* ((current-window (aj-visual-area-window va))
                        (start-point (window-start current-window))
                        (end-point   (window-end   current-window t)))
                   (with-selected-window current-window
                     (save-excursion
                       (goto-char start-point)
                       (let ((case-fold-search ace-jump-mode-case-fold)
                             re-query-string-all) ;定义包括中英文字符串
                         (if (string-empty-p (pinyin-search--pinyin-to-regexp re-query-string))
                             (setq re-query-string-all (concat "[" re-query-string "]")) ;无法跳转.*+?等正则表达式使用的符号
                           (setq re-query-string-all (concat "[" (substring (pinyin-search--pinyin-to-regexp re-query-string) 1 -1) re-query-string "]")))
                         (loop while (re-search-forward re-query-string-all nil t) ;使用pinyin-search实现中文ace-jump
                               until (or
                                      (> (point) end-point)
                                      (eobp))
                               if (or ace-jump-allow-invisible (not (invisible-p (match-beginning 0))))
                               collect (make-aj-position :offset (match-beginning 0)
                                                         :visual-area va)
                               ;; when we use "^" to search line mode,
                               ;; re-search-backward will not move one
                               ;; char after search success, as line
                               ;; begin is not a valid visible char.
                               ;; We need to help it to move forward.
                               do (if (string-equal re-query-string "^")
                                      (goto-char (1+ (match-beginning 0)))))))))))
  (defun swint-ace-jump-do (re-query-string)
    "The main function to start the AceJump mode.
QUERY-STRING should be a valid regexp string, which finally pass to `search-forward-regexp'.
You can constrol whether use the case sensitive via `ace-jump-mode-case-fold'."
    ;; we check the move key to make it valid, cause it can be customized by user
    (if (or (null ace-jump-mode-move-keys)
            (< (length ace-jump-mode-move-keys) 2)
            (not (every #'characterp ace-jump-mode-move-keys)))
        (error "[AceJump] Invalid move keys: check ace-jump-mode-move-keys"))
    ;; search candidate position
    (let* ((visual-area-list (ace-jump-list-visual-area/exclude-pdf-view)) ;exclude pdf-view buffer
           (candidate-list (swint-ace-jump-search-candidate re-query-string visual-area-list)))
      (cond
       ;; cannot find any one
       ((null candidate-list)
        (setq ace-jump-current-mode nil)
        (error "[AceJump] No one found"))
       ;; we only find one, so move to it directly
       ((eq (cdr candidate-list) nil)
        (ace-jump-push-mark)
        (run-hooks 'ace-jump-mode-before-jump-hook)
        (ace-jump-jump-to (car candidate-list))
        (message "[AceJump] One candidate, move to it directly")
        (run-hooks 'ace-jump-mode-end-hook))
       ;; more than one, we need to enter AceJump mode
       (t
        ;; make indirect buffer for those windows that show the same buffer
        ;; (setq ace-jump-recover-visual-area-list
        ;;       (ace-jump-mode-make-indirect-buffer visual-area-list))
        ;; create background for each visual area
        (if ace-jump-mode-gray-background
            (setq ace-jump-background-overlay-list
                  (loop for va in visual-area-list
                        collect (let* ((w (aj-visual-area-window va))
                                       (b (aj-visual-area-buffer va))
                                       (ol (make-overlay (window-start w)
                                                         (window-end w)
                                                         b)))
                                  (overlay-put ol 'face 'ace-jump-face-background)
                                  ol))))
        ;; construct search tree and populate overlay into tree
        (setq ace-jump-search-tree
              (ace-jump-tree-breadth-first-construct (length candidate-list)
                                                     (length ace-jump-mode-move-keys)))
        (ace-jump-populate-overlay-to-search-tree ace-jump-search-tree
                                                  candidate-list)
        (swint-ace-jump-update-overlay-in-search-tree ace-jump-search-tree
                                                      ace-jump-mode-move-keys)
        ;; do minor mode configuration
        (cond
         ((eq ace-jump-current-mode 'ace-jump-char-mode)
          (setq ace-jump-mode " AceJump - Char"))
         ((eq ace-jump-current-mode 'ace-jump-word-mode)
          (setq ace-jump-mode " AceJump - Word"))
         ((eq ace-jump-current-mode 'ace-jump-line-mode)
          (setq ace-jump-mode " AceJump - Line"))
         (t
          (setq ace-jump-mode " AceJump")))
        (force-mode-line-update)
        ;; override the local key map
        (setq overriding-local-map
              (let ( (map (make-keymap)) )
                (dolist (key-code ace-jump-mode-move-keys)
                  (define-key map (make-string 1 key-code) 'swint-ace-jump-move))
                (define-key map (kbd "C-c C-c") 'ace-jump-quick-exchange)
                (define-key map [t] 'ace-jump-done)
                map))
        (add-hook 'mouse-leave-buffer-hook 'ace-jump-done)
        (add-hook 'kbd-macro-termination-hook 'ace-jump-done)))))
  (defun swint-ace-jump-char-mode (query-char)
    "AceJump char mode"
    (interactive (list (read-char "Query Char:")))
    ;; We should prevent recursion call this function.  This can happen
    ;; when you trigger the key for ace jump again when already in ace
    ;; jump mode.  So we stop the previous one first.
    (if ace-jump-current-mode (ace-jump-done))
    (if (eq (ace-jump-char-category query-char) 'other)
        (error "[AceJump] Non-printable character"))
    ;; others : digit , alpha, punc
    (setq ace-jump-query-char query-char)
    (setq ace-jump-current-mode 'ace-jump-char-mode)
    (swint-ace-jump-do (regexp-quote (make-string 1 query-char))))
  (defun swint-ace-jump-update-overlay-in-search-tree (tree keys)
    "Update overlay 'display property using each name in keys"
    (lexical-let* (;; create dynamic variable for following function
                   (key ?\0)
                   ;; populdate each leaf node to be the specific key,
                   ;; this only update 'display' property of overlay,
                   ;; so that user can see the key from screen and select
                   (func-update-overlay
                    (lambda (node)
                      (let ((ol (cdr node)))
                        (overlay-put
                         ol
                         'display
                         (concat (make-string 1 key)
                                 (let* ((pos (overlay-get ol 'aj-data))
                                        (subs (ace-jump-buffer-substring pos)))
                                   (cond
                                    ;; when tab, we use more space to prevent screen
                                    ;; from messing up
                                    ((string-equal subs "\t")
                                     (make-string (1- tab-width) ? ))
                                    ;; when enter, we need to add one more enter
                                    ;; to make the screen not change
                                    ((string-equal subs "\n")
                                     "\n")
                                    ((string-match "\\cc" subs) ;判断是否为汉字
                                     (make-string (- tab-width 7) ? )) ;对于中文ace-jump，在英文后增加一个空格，一个tab-width是8
                                    (t
                                     "")))))))))
      (loop for k in keys
            for n in (cdr tree)
            do (progn
                 ;; update "key" variable so that the function can use
                 ;; the correct context
                 (setq key k)
                 (if (eq (car n) 'branch)
                     (ace-jump-tree-preorder-traverse n
                                                      func-update-overlay)
                   (funcall func-update-overlay n))))))
  (defun swint-ace-jump-move ()
    "move cursor based on user input"
    (interactive)
    (let* ((index (let ((ret (position (aref (this-command-keys) 0)
                                       ace-jump-mode-move-keys)))
                    (if ret ret (length ace-jump-mode-move-keys))))
           (node (nth index (cdr ace-jump-search-tree))))
      (cond
       ;; we do not find key in search tree. This can happen, for
       ;; example, when there is only three selections in screen
       ;; (totally five move-keys), but user press the forth move key
       ((null node)
        (message "No such position candidate.")
        (ace-jump-done))
       ;; this is a branch node, which means there need further
       ;; selection
       ((eq (car node) 'branch)
        (let ((old-tree ace-jump-search-tree))
          ;; we use sub tree in next move, create a new root node
          ;; whose child is the sub tree nodes
          (setq ace-jump-search-tree (cons 'branch (cdr node)))
          (swint-ace-jump-update-overlay-in-search-tree ace-jump-search-tree
                                                        ace-jump-mode-move-keys)
          ;; this is important, we need remove the subtree first before
          ;; do delete, we set the child nodes to nil
          (setf (cdr node) nil)
          (ace-jump-delete-overlay-in-search-tree old-tree)))
       ;; if the node is leaf node, this is the final one
       ((eq (car node) 'leaf)
        ;; need to save aj data, as `ace-jump-done' will clean it
        (let ((aj-data (overlay-get (cdr node) 'aj-data)))
          (ace-jump-done)
          (ace-jump-push-mark)
          (run-hooks 'ace-jump-mode-before-jump-hook)
          (ace-jump-jump-to aj-data))
        (run-hooks 'ace-jump-mode-end-hook))
       (t
        (ace-jump-done)
        (error "[AceJump] Internal error: tree node type is invalid")))))
  ;; ace-jump-list-visual-area/exclude-pdf-view
  ;; win中pdf-tools暂时不可用。
  (defun ace-jump-list-visual-area/exclude-pdf-view ()
    "Exclude pdf view area to improve performance."
    (loop for f in (frame-list)
          append (loop for w in (remove-if (lambda (x) (eq (buffer-mode (window-buffer x)) 'pdf-view-mode)) (window-list f))
                       collect (make-aj-visual-area :buffer (window-buffer w)
                                                    :window w
                                                    :frame f)))))
;; ====================ace-jump=========================
;; ===================MATLAB==================
(use-package matlab-mode
  :disabled
  :mode ("\\.[mM]\\'" . matlab-mode)
  :config
  (setenv "PATH" (concat (getenv "PATH") "/usr/local/MATLAB/R2011b/bin/"))
  (setq exec-path (append exec-path '("/usr/local/MATLAB/R2011b/bin/")))
  (server-start)
  (add-to-list 'load-path
               "~/.emacs.d/matlab-emacs/matlab-emacs")
  (require 'matlab-load)
  (autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
  (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
  (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
  (setq matlab-indent-function-body t)    ; if you want function bodies indented
  (setq matlab-verify-on-save-flag nil)   ; turn off auto-verify on save
  (defun my-matlab-mode-hook ()
    (setq fill-column 76)
    (imenu-add-to-menubar "Find"))        ; where auto-fill should wrap
  (add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
  (defun my-matlab-shell-mode-hook ()
    '())
  (add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)
  (setq matlab-shell-command-switches '("-nodesktop -nosplash"))
  (add-hook 'matlab-mode-hook
            '(lambda ()
               (define-key matlab-mode-map [(control \h)] nil)
               (define-key matlab-mode-map [(meta \q)] nil)
               (define-key matlab-mode-map [(control meta \e)] nil)
               )))
;; ===================MATLAB==================
;; =========================tabbar================================
(use-package tabbar
  :disabled
  :config
  (add-to-list 'load-path "~/.emacs.d/tabbar")
  (require 'tabbar)
  (tabbar-mode 1)
  (global-set-key (kbd "M-2") 'tabbar-backward)
  (global-set-key (kbd "M-3") 'tabbar-forward)
  ;; (global-set-key (kbd "C-M-2") 'tabbar-backward-group)
  ;; (global-set-key (kbd "C-M-3") 'tabbar-forward-group)
  ;;设置tabbar外观
  ;;设置默认主题: 字体, 背景和前景颜色，大小
  (set-face-attribute 'tabbar-default nil
                      :family "DejaVu Sans Mono"
                      :background "gray80"
                      :foreground "gray30"
                      :height 0.9
                      )
  ;;设置左边按钮外观：外框框边大小和颜色
  (set-face-attribute 'tabbar-button nil
                      :inherit 'tabbar-default
                      :box '(:line-width 1 :color "yellow")
                      )
  ;;设置当前tab外观：颜色，字体，外框大小和颜色
  (set-face-attribute 'tabbar-selected nil
                      :inherit 'tabbar-default
                      :foreground "DarkGreen"
                      :background "LightGoldenrod"
                      :box '(:line-width 1 :color "DarkGoldenrod")
                      :overline "black"
                      :underline "black"
                      :weight 'bold
                      )
  ;;设置非当前tab外观：外框大小和颜色
  (set-face-attribute 'tabbar-unselected nil
                      :inherit 'tabbar-default
                      :box '(:line-width 1 :color "#00B2BF")
                      )
  ;; don't show help information,don't show tabbar button
  (setq
   tabbar-scroll-left-help-function nil
   tabbar-scroll-right-help-function nil
   tabbar-help-on-tab-function nil
   tabbar-home-help-function nil
   tabbar-buffer-home-button (quote (("") ""))
   tabbar-scroll-left-button (quote (("") ""))
   tabbar-scroll-right-button (quote (("") "")))
  ;; user and emacs group
  (defun my-tabbar-buffer-groups ()
    ;; customize to show all normal files in one group
    ;;   "Returns the name of the tab group names the current buffer belongs to.
    ;; There are two groups: Emacs buffers (those whose name starts with “*”, plus
    ;; dired buffers), and the rest.  This works at least with Emacs v24.2 using
    ;; tabbar.el v1.7.
    (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
                ;; ((eq major-mode 'dired-mode) "emacs")
                (t "user"))))
  (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
  ;; 根据major-mode选择是否打开tabbar
  (when (require 'tabbar nil t)
    ;; Enable tabbars globally:
    (tabbar-mode 1)
    ;; I use this minor-mode mainly as a global mode (see below):
    (define-minor-mode tabbar-on-dired-only-mode
      "Display tabbar on terminals and buffers in fundamental mode only."
      :init-value t
      :lighter nil
      :keymap nil
      (if tabbar-on-dired-only-mode
          ;; filter is enabled
          (if (eq major-mode 'dired-mode); <- this can be easily customizable...
              (tabbar-local-mode -1)
            (tabbar-local-mode 1))
        ;; always activate tabbar locally when we disable the minor mode:
        (tabbar-local-mode -1)))
    (defun tabbar-on-dired-only-mode-on ()
      "Turn on tabbar if current buffer is a terminal."
      (unless (minibufferp) (tabbar-on-dired-only-mode 1)))
    ;; Define a global switch for the mode. Note that this is not set for buffers
    ;; in fundamental mode.
    ;; I use it 'cause some major modes do not run the
    ;; `after-change-major-mode-hook'...
    (define-globalized-minor-mode global-tabbar-on-dired-only-mode
      tabbar-on-dired-only-mode tabbar-on-dired-only-mode-on)
    ;; Eventually, switch on this global filter for tabbars:
    (global-tabbar-on-dired-only-mode 1)))
;; =========================tabbar================================
;; ==================color-theme===================
;; 用于emacs23以下。
(use-package color-theme
  :disabled
  :config
  ;; (add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
  (setq color-theme-directory (concat user-emacs-directory "themes/")
        color-theme-load-all-themes nil)
  (color-theme-initialize)
  ;; (add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/themes/")
  ;; (color-theme-tango-light)
  (color-theme-tangotango))
;; ==================color-theme===================
;; ============原win语言环境字符集设置==============
;; (defun qiang-font-existsp (font)
;;   (if (null (x-list-fonts font))
;;       nil t))
;; (defvar font-list '("宋体" "新宋体" "Microsoft Yahei" "文泉驿等宽微米黑" "黑体" ))
;; (require 'cl) ;; find-if is in common list package
;; (find-if #'qiang-font-existsp font-list)
;; (defun qiang-make-font-string (font-name font-size)
;;   (if (and (stringp font-size)
;;            (equal ":" (string (elt font-size 0))))
;;       (format "%s%s" font-name font-size)
;;     (format "%s %s" font-name font-size)))
;; (defun qiang-set-font (english-fonts
;;                        english-font-size
;;                        chinese-fonts
;;                        &optional chinese-font-size)
;;   "english-font-size could be set to \":pixelsize=18\" or a integer.
;; If set/leave chinese-font-size to nil, it will follow english-font-size"
;;   (require 'cl)                         ; for find if
;;   (let ((en-font (qiang-make-font-string
;;                   (find-if #'qiang-font-existsp english-fonts)
;;                   english-font-size))
;;         (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
;;                             :size chinese-font-size)))
;;     ;; Set the default English font
;;     ;; The following 2 method cannot make the font settig work in new frames.
;;     ;; (set-default-font "Consolas:pixelsize=18")
;;     ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
;;     ;; We have to use set-face-attribute
;;     (message "Set English Font to %s" en-font)
;;     (set-face-attribute
;;      'default nil :font en-font)
;;     ;; Set Chinese font
;;     ;; Do not use 'unicode charset, it will cause the english font setting invalid
;;     (message "Set Chinese Font to %s" zh-font)
;;     (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;       (set-fontset-font (frame-parameter nil 'font)
;;                         charset
;;                         zh-font))))
;; (qiang-set-font
;;  '("Consolas" "Monaco" "DejaVu Sans Mono" "Monospace" "Courier New") ":pixelsize=15"
;;  '("宋体" "新宋体" "Microsoft Yahei" "文泉驿等宽微米黑" "黑体" ))
;; ;;Setting English Font
;; ;;(set-face-attribute  'default nil :font "Consolas 12")
;; ;; Chinese Font
;; ;;(dolist (charset '(kana han symbol cjk-misc bopomofo))  (set-fontset-font (frame-parameter nil 'font)         charset                   (font-spec :family "Microsoft Yahei" :size 12)))
;; ;;处理shell-mode乱码,好像没作用
;;============原win语言环境字符集设置==============
(provide 'setup_abandoned)
