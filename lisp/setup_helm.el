;; =========================helm_lacarte=============================
;; (add-to-list 'load-path "~/.emacs.d/lacarte")
(require 'lacarte)
(setq LaTeX-math-menu-unicode t)
(global-set-key (kbd "C-c m") 'helm-insert-latex-math)
(global-set-key (kbd "C-x m") 'helm-browse-menubar)
;; 使用helm自带的程序而不使用下列自定义的命令
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
  "helm for searching math menus"
  (interactive)
  (helm '(helm-source-lacarte-math)
        (thing-at-point 'symbol) "Symbol: "
        nil nil "*helm math symbols*"))
;; =========================helm_lacarte=============================
;; ================helm================
(require 'helm)
(require 'helm-config)
(helm-mode 1)
;; (global-set-key (kbd "C-x l") 'locate)
;; win上locate似乎搜索中文有问题，改用es。但是es似乎仍然搜索不了中文，而且会导致emacs死掉，放弃。
;; win上面的linux工具包括grep/locate/find都不能够搜索中文。
;; (setq helm-locate-command "es %s %s")
;; 使用 updatedb -l 0 -o ~/.swint-locate.db -U ~/ 建立用户数据库。
(setq helm-locate-create-db-command "updatedb -l 0 -o ~/.swint-locate.db -U ~/")
(setq helm-locate-command "locate -b -i %s -r %s -d ~/.swint-locate.db")
;; ==================helm-dired-buffer====================
(defun swint-helm-dired-buffer-list ()
  "Return the current list of dired buffers.
Currently visible buffers are put at the end of the list."
  (iswitchb-make-buflist nil))
(defclass swint-helm-dired-source-buffers (helm-source-sync helm-type-buffer)
  ((init :initform (lambda ()
                     ;; Issue #51 Create the list before `helm-buffer' creation.
                     (setq helm-buffers-list-cache (swint-helm-dired-buffer-list))
                     (let ((result (cl-loop for b in helm-buffers-list-cache
                                            maximize (length b) into len-buf
                                            maximize (length (with-current-buffer b
                                                               (symbol-name major-mode)))
                                            into len-mode
                                            finally return (cons len-buf len-mode))))
                       (unless helm-buffer-max-length
                         (setq helm-buffer-max-length (car result)))
                       (unless helm-buffer-max-len-mode
                         ;; If a new buffer is longer that this value
                         ;; this value will be updated
                         (setq helm-buffer-max-len-mode (cdr result))))))
   (candidates :initform helm-buffers-list-cache)
   (matchplugin :initform nil)
   (match :initform 'helm-buffers-list--match-fn)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (volatile :initform t)
   (mode-line :initform helm-buffer-mode-line-string)
   (persistent-help
    :initform
    "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))
(defvar swint-helm-dired-source-buffers-list (helm-make-source "Dired Buffers" 'swint-helm-dired-source-buffers))
(defun swint-helm-dired-buffers ()
  "Preconfigured `helm-dired-buffers' to list dired buffers."
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm :sources '(swint-helm-dired-source-buffers-list
                     swint-helm-source-recentf-directory
                     helm-source-buffer-not-found)
          :buffer "*helm dired buffers-swint*"
          :keymap helm-buffer-map
          :truncate-lines t)))
(global-set-key (kbd "C-.") 'swint-helm-dired-buffers)
;; ==================helm-dired-buffer====================
;; ==================helm-bookmarks====================
(defcustom swint-helm-bookmarks-list
  '(helm-source-bookmarks)
  "Default sources list used in `swint-helm-bookmarks'."
  :type '(repeat (choice symbol))
  :group 'helm-files)
(defun swint-helm-bookmarks ()
  "Preconfigured `helm-bookmarks' for opening files."
  (interactive)
  (let ((helm-ff-transformer-show-only-basename t))
    (helm-other-buffer swint-helm-bookmarks-list "*helm bookmarks-swint*")))
(global-set-key (kbd "C-'") 'swint-helm-bookmarks)
;; ==================helm-bookmarks====================
;; ==================helm-mini====================
(defcustom swint-helm-mini-default-sources '(helm-source-buffers-list
                                             swint-helm-source-recentf-file
                                             helm-source-buffer-not-found)
  "Default sources list used in `swint-helm-mini'."
  :group 'helm-misc
  :type '(repeat (choice symbol)))
(defun swint-helm-mini ()
  "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)
  (require 'helm-files)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer swint-helm-mini-default-sources "*helm mini-swint*")))
(global-set-key (kbd "C-,") 'swint-helm-mini)
;; ==================helm-mini====================
;; ==================helm-find-file====================
(defcustom swint-helm-ff-list
  '(helm-source-files-in-current-dir
    ;; helm-source-locate
    )
  "Default sources list used in `swint-helm-ff'."
  :type '(repeat (choice symbol))
  :group 'helm-files)
(defun swint-helm-ff ()
  "Preconfigured `swint-helm-ff' for opening files."
  (interactive)
  (let ((helm-ff-transformer-show-only-basename t))
    (helm-other-buffer swint-helm-ff-list "*helm find file-swint*")))
(global-set-key (kbd "C-x C-f") 'swint-helm-ff)
;; ==================helm-find-file====================
;; ================在别的helm-buffer中运行helm命令==============
(defun swint-helm-dired-buffers-after-quit ()
  "List swint-helm-dired-buffers."
  (interactive)
  (helm-run-after-quit #'(lambda () (swint-helm-dired-buffers))))
(defun swint-helm-mini-after-quit ()
  "List swint-helm-mini."
  (interactive)
  (helm-run-after-quit #'(lambda () (swint-helm-mini))))
(defun swint-helm-bookmarks-after-quit ()
  "List swint-helm-bookmarks."
  (interactive)
  (helm-run-after-quit #'(lambda () (swint-helm-bookmarks))))
(defun swint-helm-ff-after-quit ()
  "List swint-helm-ff."
  (interactive)
  (helm-run-after-quit #'(lambda () (swint-helm-ff))))
(define-key helm-find-files-map (kbd "C-.") 'swint-helm-dired-buffers-after-quit)
(define-key helm-map (kbd "C-.") 'swint-helm-dired-buffers-after-quit)
(define-key helm-map (kbd "C-'") 'swint-helm-bookmarks-after-quit)
(define-key helm-map (kbd "C-,") 'swint-helm-mini-after-quit)
(define-key helm-find-files-map (kbd "C-x C-f") 'swint-helm-ff-after-quit)
(define-key helm-map (kbd "C-x C-f") 'swint-helm-ff-after-quit)
(define-key helm-map (kbd "C-/") 'helm-quit-and-find-file)
;; ================在别的helm-buffer中运行helm命令==============
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x g") 'helm-do-grep) ;不是递归grep，加C-u为递归
(global-set-key (kbd "C-c C-f") 'helm-locate)
;; (global-set-key (kbd "C-c C-f") 'helm-recentf)
;; (global-set-key (kbd "C-j") 'helm-select-action)
(define-key helm-map (kbd "C-;") 'helm-toggle-visible-mark)
(define-key helm-map (kbd "C-l") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-M-p") 'helm-previous-source)
(define-key helm-map (kbd "C-M-n") 'helm-next-source)
(define-key helm-buffer-map (kbd "C-M-k") 'helm-buffer-run-kill-persistent)
(define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-l") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-read-file-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "M-U") 'helm-unmark-all)
(define-key helm-find-files-map (kbd "M-t") 'helm-toggle-all-marks)
(define-key helm-find-files-map (kbd "C-j") 'helm-ff-run-open-file-externally)
(define-key helm-generic-files-map (kbd "C-j") 'helm-ff-run-open-file-externally)
(define-key helm-buffer-map (kbd "C-o") 'helm-buffer-switch-other-window)
(define-key helm-find-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
(define-key helm-generic-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
(define-key helm-grep-map (kbd "C-o") 'helm-grep-run-other-window-action)
(define-key helm-bookmark-map (kbd "C-o") 'helm-bookmark-run-jump-other-window)
;; C-x c c helm-colors
;; helm-mini下：C-c C-d 从列表中删除，但实际不kill buffer；C-c d kill buffer同时不关闭helm buffer；M-D kill buffer同时关闭helm
;; helm-find-files-map下：
;; C-l 一次expand candidate，二次打开文件
;; TAB 打开选项
;; C-j 使用外部命令打开文件，加C-u选择命令
;; M-g s grep文件
;; M-e 打开eshell
;; M-C M-D M-R 复制 删除 重命名
;; C-= ediff
;; M-p 历史
;; 再重复一次C-x C-f locate查找文件，可以用sudo updatedb升级数据库
;; C-t 转换列表显示方式
;; C-] toggle basename/fullpath
;; C-backspace 开启关闭自动补全
;; C-{ C-} 放大缩小helm窗口
;; find file as root
;; ================helm================
;; ================helm-swoop================
;; (add-to-list 'load-path "~/.emacs.d/helm-swoop")
(require 'helm-swoop)
(global-set-key (kbd "M-s M-s") 'helm-swoop)
(global-set-key (kbd "M-s M-S") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "M-s C-s") 'helm-multi-swoop)
(global-set-key (kbd "M-s C-S-s") 'helm-multi-swoop-all)
;; helm-swoop 中使用C-c C-e编辑，C-x C-s保存
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-s M-s") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-s M-s") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)
;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)
;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)
;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)
;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)
;; ================helm-swoop================
;; =======================helm-pinyin==============================
;; 让helm支持拼音头字母搜索
;; (add-to-list 'load-path "~/.emacs.d/helm-pinyin")
(load "iswitchb-pinyin")   ; 给iswitchb-mode添加按拼音首字母匹配的能力
;; 这个东西本身是给iswitchb增加拼音头字母搜索的，使用其中的pinyin-initials-string-match函数。
;; buffer 支持中文拼音首字母
(defun helm-buffer--match-pattern (pattern candidate)
  (let ((fun (if (and helm-buffers-fuzzy-matching
                      (not (pinyin-initials-string-match "\\`\\^" pattern)))
                 #'helm--mapconcat-candidate
               #'identity)))
    (if (pinyin-initials-string-match "\\`!" pattern)
        (not (pinyin-initials-string-match (funcall fun (substring pattern 1))
                                           candidate))
      (pinyin-initials-string-match (funcall fun pattern) candidate))))
;; recentf 支持中文拼音首字母
(defun helm-files-match-only-basename (candidate)
  "Allow matching only basename of file when \" -b\" is added at end of pattern.
If pattern contain one or more spaces, fallback to match-plugin
even is \" -b\" is specified."
  (let ((source (helm-get-current-source)))
    (if (pinyin-initials-string-match "\\([^ ]*\\) -b\\'" helm-pattern)
        (progn
          (helm-attrset 'no-matchplugin nil source)
          (pinyin-initials-string-match (match-string 1 helm-pattern)
                                        (helm-basename candidate)))
      ;; Disable no-matchplugin by side effect.
      (helm-aif (assq 'no-matchplugin source)
          (setq source (delete it source)))
      (pinyin-initials-string-match
       (replace-regexp-in-string " -b\\'" "" helm-pattern)
       candidate))))
;; helm-find-files 支持中文拼音首字母，但是会破坏搜索方式，使搜索结果过多。
;; 直接输入xx，会产生过多的匹配结果；xx后面加空格，以xx为起始字母；xx前面加空格，搜索结果正常。
;; 搜索结果匹配不好，另建swint-helm-ff。
(defun helm-mp-3-match (str &optional pattern)
  "Check if PATTERN match STR.
When PATTERN contain a space, it is splitted and matching is done
with the several resulting regexps against STR.
e.g \"bar foo\" will match \"foobar\" and \"barfoo\".
Argument PATTERN, a string, is transformed in a list of
cons cell with `helm-mp-3-get-patterns' if it contain a space.
e.g \"foo bar\"=>((identity . \"foo\") (identity . \"bar\")).
Then each predicate of cons cell(s) is called with regexp of same
cons cell against STR (a candidate).
i.e (identity (string-match \"foo\" \"foo bar\")) => t."
  (let ((pat (helm-mp-3-get-patterns (or pattern helm-pattern))))
    (cl-loop for (predicate . regexp) in pat
             always (funcall predicate (pinyin-initials-string-match regexp str)))))
;; =======================helm-pinyin==============================
(provide 'setup_helm)
