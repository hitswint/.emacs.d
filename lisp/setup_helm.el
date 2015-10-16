;; =========================helm_lacarte=============================
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
(cond
 (is-lin
  ;; 使用 updatedb -l 0 -o ~/.helm-locate.db -U ~/ 建立用户数据库。
  (setq helm-locate-create-db-command "updatedb -l 0 -o ~/.helm-locate.db -U ~/")
  (setq helm-locate-command "locate -b -i %s -r %s -d ~/.helm-locate.db"))
 (is-win
  ;; 下列的命令建立locate数据库的时候，会导致数据库中记录的文件路径名为/cygdrive/c/，这种路径名emacs无法识别。
  ;; (setq helm-locate-create-db-command "updatedb --output=/cygdrive/c/Users/swint/.helm-locate.db --localpaths='/cygdrive/c/Users/swint/'")
  ;; (setq helm-locate-command "locate -b -i %s -r %s -d /cygdrive/c/Users/swint/.helm-locate.db")
  ;; 下面的命令建立locate数据库的时候，会导致cygwin警告ms dos path sytle，无妨，时间稍长；locate命令无法识别这种ms dos path sytle。
  (setq helm-locate-create-db-command "updatedb --output=c:/Users/swint/.helm-locate.db --localpaths='c:/Users/swint/'")
  (setq helm-locate-command "locate -b -i %s -r %s -d /cygdrive/c/Users/swint/.helm-locate.db")))
;; ==================helm-dired-buffer====================
(defun swint-helm-dired-buffers-list--init ()
  ;; Issue #51 Create the list before `helm-buffer' creation.
  (setq swint-helm-dired-buffers-list-cache (iswitchb-make-buflist nil))
  (let ((result (cl-loop for b in swint-helm-dired-buffers-list-cache
                         maximize (length b) into len-buf
                         maximize (length (with-current-buffer b
                                            (symbol-name major-mode)))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))
(defun swint-helm-dired-buffers-list--init/other-persps ()
  ;; Issue #51 Create the list before `helm-buffer' creation.
  (setq swint-helm-dired-buffers-list-cache/other-persps iswitchb-temp-buflist/other-persps)
  (let ((result (cl-loop for b in swint-helm-dired-buffers-list-cache/other-persps
                         maximize (length b) into len-buf
                         maximize (length (with-current-buffer b
                                            (symbol-name major-mode)))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))
(defclass swint-helm-dired-source-buffers (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-buffer-list
    :custom function
    :documentation
    "  A function with no arguments to create buffer list.")
   (init :initform 'swint-helm-dired-buffers-list--init)
   (candidates :initform swint-helm-dired-buffers-list-cache)
   (matchplugin :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (resume :initform (lambda ()
                       (run-with-idle-timer
                        0.1 nil (lambda ()
                                  (with-helm-buffer
                                    (helm-force-update))))))
   (keymap :initform helm-buffer-map)
   (volatile :initform t)
   (help-message :initform 'helm-buffer-help-message)
   (persistent-help
    :initform
    "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))
(defclass swint-helm-dired-source-buffers/other-persps (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-buffer-list
    :custom function
    :documentation
    "  A function with no arguments to create buffer list.")
   (init :initform 'swint-helm-dired-buffers-list--init/other-persps)
   (candidates :initform swint-helm-dired-buffers-list-cache/other-persps)
   (matchplugin :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (resume :initform (lambda ()
                       (run-with-idle-timer
                        0.1 nil (lambda ()
                                  (with-helm-buffer
                                    (helm-force-update))))))
   (keymap :initform helm-buffer-map)
   (volatile :initform t)
   (help-message :initform 'helm-buffer-help-message)
   (persistent-help
    :initform
    "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))
(defvar swint-helm-dired-source-buffers-list nil)
(defvar swint-helm-dired-source-buffers-list/other-persps nil)
(defun helm-switch-persp/buffer (buffer)
  "helm-switch to persp/buffer simultaneously"
  (cond
   ((memq buffer (persp-buffers persp-curr))
    (switch-to-buffer buffer))
   ((memq buffer (persp-buffers (gethash "8" perspectives-hash)))
    (swint-persp-switch "8")
    (if (window-live-p (get-buffer-window buffer))
        (select-window (get-buffer-window buffer))
      (switch-to-buffer buffer)))
   ((memq buffer (persp-buffers (gethash "9" perspectives-hash)))
    (swint-persp-switch "9")
    (if (window-live-p (get-buffer-window buffer))
        (select-window (get-buffer-window buffer))
      (switch-to-buffer buffer)))
   ((memq buffer (persp-buffers (gethash "0" perspectives-hash)))
    (swint-persp-switch "0")
    (if (window-live-p (get-buffer-window buffer))
        (select-window (get-buffer-window buffer))
      (switch-to-buffer buffer)))
   (t
    (swint-persp-switch "i")
    (if (window-live-p (get-buffer-window buffer))
        (select-window (get-buffer-window buffer))
      (switch-to-buffer buffer)))))
(defun swint-helm-dired-buffers-list ()
  "Preconfigured `helm' to list buffers."
  (interactive)
  (unless swint-helm-dired-source-buffers-list
    (setq swint-helm-dired-source-buffers-list
          (helm-make-source "Dired Buffers" 'swint-helm-dired-source-buffers)))
  (unless swint-helm-dired-source-buffers-list/other-persps
    (progn (setq swint-helm-dired-source-buffers-list/other-persps
                 (helm-make-source "Dired Buffers in other persps" 'swint-helm-dired-source-buffers/other-persps))
           (helm-add-action-to-source "Switch to persp/buffer" 'helm-switch-persp/buffer swint-helm-dired-source-buffers-list/other-persps 0)))
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm :sources '(swint-helm-dired-source-buffers-list
                     swint-helm-dired-source-buffers-list/other-persps
                     swint-helm-source-recentf-directory
                     helm-source-buffer-not-found)
          :buffer "*helm dired buffers-swint*"
          :keymap helm-buffer-map
          :truncate-lines t)))
(global-set-key (kbd "C-.") 'swint-helm-dired-buffers-list)
;; ==================helm-dired-buffer====================
;; ==================helm-mini====================
(defun helm-buffer-list ()
  "Return the current list of buffers.
Currently visible buffers are put at the end of the list.
See `ido-make-buffer-list' for more infos."
  (require 'ido)
  (let ((ido-process-ignore-lists t)
        ido-ignored-list
        ;; 在原函数中注销下句，因为会导致ido忽略buffer的定义失效。
        ;; ido-ignore-buffers
        ido-use-virtual-buffers)
    (ido-make-buffer-list nil)))
(defun helm-buffers-list--init/other-persps ()
  ;; Issue #51 Create the list before `helm-buffer' creation.
  (setq helm-buffers-list-cache/other-persps ido-temp-list/other-persps)
  (let ((result (cl-loop for b in helm-buffers-list-cache/other-persps
                         maximize (length b) into len-buf
                         maximize (length (with-current-buffer b
                                            (symbol-name major-mode)))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))
(defclass helm-source-buffers/other-persps (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-buffer-list
    :custom function
    :documentation
    "  A function with no arguments to create buffer list.")
   (init :initform 'helm-buffers-list--init/other-persps)
   (candidates :initform helm-buffers-list-cache/other-persps)
   (matchplugin :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (resume :initform (lambda ()
                       (run-with-idle-timer
                        0.1 nil (lambda ()
                                  (with-helm-buffer
                                    (helm-force-update))))))
   (keymap :initform helm-buffer-map)
   (volatile :initform t)
   (help-message :initform 'helm-buffer-help-message)
   (persistent-help
    :initform
    "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))
(defvar helm-source-buffers-list/other-persps nil)
(defcustom swint-helm-mini-default-sources '(helm-source-buffers-list
                                             helm-source-buffers-list/other-persps
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
          (helm-make-source "File Buffers" 'helm-source-buffers)))
  (unless helm-source-buffers-list/other-persps
    (progn (setq helm-source-buffers-list/other-persps
                 (helm-make-source "File Buffers in other persps" 'helm-source-buffers/other-persps))
           (helm-add-action-to-source "Switch to persp/buffer" 'helm-switch-persp/buffer helm-source-buffers-list/other-persps 0)))
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer swint-helm-mini-default-sources "*helm mini-swint*")))
(global-set-key (kbd "C-,") 'swint-helm-mini)
;; ==================helm-mini====================
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
;; ==================helm-find-file====================
(defun swint-helm-find-files ()
  "Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-preferred-list'."
  (interactive)
  (helm--maybe-build-source 'helm-source-files-in-current-dir
    'helm-build-files-in-current-dir-source)
  (helm :sources '(helm-source-files-in-current-dir)
        :ff-transformer-show-only-basename t
        :buffer "*helm find files-swint*"))
(global-set-key (kbd "C-x C-f") 'swint-helm-find-files)
;; ==================helm-find-file====================
;; ================在别的helm-buffer中运行helm命令==============
(defun swint-helm-dired-buffers-after-quit ()
  "List swint-helm-dired-buffers."
  (interactive)
  (helm-run-after-quit #'(lambda () (swint-helm-dired-buffers-list))))
(defun swint-helm-mini-after-quit ()
  "List swint-helm-mini."
  (interactive)
  (helm-run-after-quit #'(lambda () (swint-helm-mini))))
(defun swint-helm-bookmarks-after-quit ()
  "List swint-helm-bookmarks."
  (interactive)
  (helm-run-after-quit #'(lambda () (swint-helm-bookmarks))))
(defun swint-helm-find-files-after-quit ()
  "List swint-helm-find-files."
  (interactive)
  (helm-run-after-quit #'(lambda () (swint-helm-find-files))))
;; Fix bugs of helm-quit-and-find-file on dired buffer
(defun swint-helm-quit-and-find-file ()
  "Drop into `helm-find-files' from `helm'.
If current selection is a buffer or a file, `helm-find-files'
from its directory."
  (interactive)
  (require 'helm-grep)
  (helm-run-after-quit
   (lambda (f)
     (if (file-exists-p f)
         (helm-find-files-1 (file-name-directory f)
                            (concat
                             "^"
                             (regexp-quote
                              (if helm-ff-transformer-show-only-basename
                                  (helm-basename f) f))))
       (helm-find-files-1 f)))
   (let* ((sel       (helm-get-selection))
          (grep-line (and (stringp sel)
                          (helm-grep-split-line sel)))
          (bmk-name  (and (stringp sel)
                          (not grep-line)
                          (replace-regexp-in-string "\\`\\*" "" sel)))
          (bmk       (and bmk-name (assoc bmk-name bookmark-alist)))
          (buf       (helm-aif (get-buffer sel) (buffer-name it)))
          (default-preselection (or (buffer-file-name helm-current-buffer)
                                    default-directory)))
     (cond
      ;; Buffer.
      (buf (or (buffer-file-name sel)
               (car (rassoc (get-buffer buf) dired-buffers))
               (and (with-current-buffer buf
                      (eq major-mode 'org-agenda-mode))
                    org-directory
                    (expand-file-name org-directory))
               (with-current-buffer buf default-directory)))
      ;; Bookmark.
      (bmk (helm-aif (bookmark-get-filename bmk)
               (if (and ffap-url-regexp
                        (string-match ffap-url-regexp it))
                   it (expand-file-name it))
             default-directory))
      ((or (file-remote-p sel)
           (file-exists-p sel))
       (expand-file-name sel))
      ;; Grep.
      ((and grep-line (file-exists-p (car grep-line)))
       (expand-file-name (car grep-line)))
      ;; Occur.
      (grep-line
       (with-current-buffer (get-buffer (car grep-line))
         (or (buffer-file-name) default-directory)))
      ;; Url.
      ((and ffap-url-regexp (string-match ffap-url-regexp sel)) sel)
      ;; Default.
      (t default-preselection)))))
(define-key helm-find-files-map (kbd "C-.") 'swint-helm-dired-buffers-after-quit)
(define-key helm-map (kbd "C-.") 'swint-helm-dired-buffers-after-quit)
(define-key helm-map (kbd "C-'") 'swint-helm-bookmarks-after-quit)
(define-key helm-map (kbd "C-,") 'swint-helm-mini-after-quit)
(define-key helm-find-files-map (kbd "C-x C-f") 'swint-helm-find-files-after-quit)
(define-key helm-map (kbd "C-x C-f") 'swint-helm-find-files-after-quit)
(define-key helm-map (kbd "C-/") 'swint-helm-quit-and-find-file)
;; ================在别的helm-buffer中运行helm命令==============
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x g") 'helm-do-grep) ;不是递归grep，加C-u为递归
(global-set-key (kbd "C-c C-f") 'helm-locate)
;; (global-set-key (kbd "C-c C-f") 'helm-recentf)
;; (define-key helm-map (kbd "C-j") 'helm-select-action)
(define-key helm-map (kbd "C-;") 'helm-toggle-visible-mark)
(define-key helm-map (kbd "C-l") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-M-p") 'helm-previous-source)
(define-key helm-map (kbd "C-M-n") 'helm-next-source)
(define-key helm-buffer-map (kbd "C-M-k") 'helm-buffer-run-kill-buffers)
(define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-l") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-read-file-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "M-U") 'helm-unmark-all)
(define-key helm-find-files-map (kbd "M-t") 'helm-toggle-all-marks)

(define-key helm-buffer-map (kbd "C-o") 'helm-buffer-switch-other-window)
(define-key helm-find-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
(define-key helm-generic-files-map (kbd "C-o") 'helm-ff-run-switch-other-window)
(define-key helm-grep-map (kbd "C-o") 'helm-grep-run-other-window-action)
(define-key helm-bookmark-map (kbd "C-o") 'helm-bookmark-run-jump-other-window)
(when is-lin
  (define-key helm-find-files-map (kbd "C-j") 'helm-ff-run-open-file-externally)
  (define-key helm-generic-files-map (kbd "C-j") 'helm-ff-run-open-file-externally))
(when is-win
  (define-key helm-find-files-map (kbd "C-j") 'helm-ff-run-open-file-with-default-tool)
  (define-key helm-generic-files-map (kbd "C-j") 'helm-ff-run-open-file-with-default-tool))
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
(load "iswitchb-pinyin")   ; 给iswitchb-mode添加按拼音首字母匹配的能力
;; 这个东西本身是给iswitchb增加拼音头字母搜索的，使用其中的pinyin-initials-string-match函数。
;; 使buffer支持中文拼音首字母
(defun helm-buffer--match-pattern (pattern candidate)
  (let ((fun (if (and helm-buffers-fuzzy-matching
                      (not (pinyin-initials-string-match "\\`\\^" pattern)))
                 #'helm--mapconcat-candidate
               #'identity)))
    (if (pinyin-initials-string-match "\\`!" pattern)
        (not (pinyin-initials-string-match (funcall fun (substring pattern 1))
                                           candidate))
      (pinyin-initials-string-match (funcall fun pattern) candidate))))
;; 使helm-find-files/recentf支持中文拼音首字母。
;; 会破坏helm-find-files的搜索方式，匹配过多，另建swint-helm-find-files。
;; 直接输入xx，会产生过多的匹配结果；xx后面加空格，以xx为起始字母；xx前面加空格，搜索结果正常。
(cl-defun helm-mm-3-match (str &optional (pattern helm-pattern))
  "Check if PATTERN match STR.
When PATTERN contain a space, it is splitted and matching is done
with the several resulting regexps against STR.
e.g \"bar foo\" will match \"foobar\" and \"barfoo\".
Argument PATTERN, a string, is transformed in a list of
cons cell with `helm-mm-3-get-patterns' if it contain a space.
e.g \"foo bar\"=>((identity . \"foo\") (identity . \"bar\")).
Then each predicate of cons cell(s) is called with regexp of same
cons cell against STR (a candidate).
i.e (identity (string-match \"foo\" \"foo bar\")) => t."
  (let ((pat (helm-mm-3-get-patterns pattern)))
    (cl-loop for (predicate . regexp) in pat
             always (funcall predicate
                             (condition-case _err
                                 ;; FIXME: Probably do nothing when
                                 ;; using fuzzy leaving the job
                                 ;; to the fuzzy fn.
                                 (pinyin-initials-string-match regexp str)
                               (invalid-regexp nil))))))
;; =======================helm-pinyin==============================
;; =======================helm-bibtex==============================
(setq helm-bibtex-bibliography "./literature.bib")
;; (zotelo-translator-charsets (quote ((BibTeX . "Unicode") (Default . "Unicode"))))
;; 设置.bib文件的编码格式，否则出现乱码
(defvar helm-source-bibtex
  '((name                                      . "BibTeX entries")
    (init                                      . helm-bibtex-init)
    (candidates                                . helm-bibtex-candidates)
    (filtered-candidate-transformer            . helm-bibtex-candidates-formatter)
    (action . (("Insert citation"              . helm-bibtex-insert-citation)
               ("Open PDF file (if present)"   . helm-bibtex-open-pdf)
               ("Open URL or DOI in browser"   . helm-bibtex-open-url-or-doi)
               ("Insert reference"             . helm-bibtex-insert-reference)
               ("Insert BibTeX key"            . helm-bibtex-insert-key)
               ("Insert BibTeX entry"          . helm-bibtex-insert-bibtex)
               ("Attach PDF to email"          . helm-bibtex-add-PDF-attachment)
               ("Edit notes"                   . helm-bibtex-edit-notes)
               ("Show entry"                   . helm-bibtex-show-entry))))
  "Source for searching in BibTeX files.")
;; 因为没有使用xpdf索引pdf文件，无法直接打开pdf文件。设置insert-citation为默认选项
(defun swint-helm-bibtex-format-citation-cite (keys)
  "Formatter for LaTeX citation macro."
  (format "\\citep{%s}" (s-join ", " keys)))
;; 重新定义插入citation命令为\citep{}，快捷键定义在setup_latex.el中。
;; =======================helm-bibtex==============================
;; =======================helm-unicode==============================
(global-set-key (kbd "M-s m") 'helm-unicode)
;; =======================helm-unicode==============================
(provide 'setup_helm)
