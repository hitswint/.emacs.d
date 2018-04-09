;;; 切换cap和大小写
;; ===============切换cap和大小写==================
;;;###autoload
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds) p2 (cdr bds))))
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps"))
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
         (t (put this-command 'state "all lower")))))
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")))))
;; ===============切换cap和大小写==================
;;; compact-uncompact-block
;; ===========compact-uncompact-block==============
;;;###autoload
(defun compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)
  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (currentStateIsCompact (bigFillColumnVal 90002000) (deactivate-mark nil))
    ;; 90002000 is just random. you can use `most-positive-fixnum'
    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil)))
      (if (region-active-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))))
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil))))
      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)))))
;; ===========compact-uncompact-block==============
;;; 复制和粘贴行
;; =================复制和粘贴行===================
;;;###autoload
(defun cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))
;; =================复制和粘贴行===================
;;; 光标不动，窗口上下移动两三行
;; ========光标不动，窗口上下移动两三行============
;;;###autoload
(defun window-move-up (&optional arg)
  "Current window move-up 2 lines."
  (interactive "P")
  (if arg
      (scroll-up arg)
    (scroll-up 2)))
;;;###autoload
(defun window-move-down (&optional arg)
  "Current window move-down 3 lines."
  (interactive "P")
  (if arg
      (scroll-down arg)
    (scroll-down 3)))
;; ========光标不动，窗口上下移动两三行============
;;; 移除行尾的空格并indent
;; ============移除行尾的空格并indent==============
(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))
;; Various superfluous white-space. Just say no.
;; (add-hook 'before-save-hook 'cleanup-buffer-safe) ;会导致mew发送附件时，保存失败，进而发送失败。
;;;###autoload
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))
;; ============移除行尾的空格并indent==============
;;; 跳转到某行时行号暂时可见
;; ============跳转到某行时行号暂时可见============
;;;###autoload
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-char (point-min))
        (forward-line (1- (read-number "Goto line: "))))
    (linum-mode -1)))
;; ============跳转到某行时行号暂时可见============
;;; 注释/反注释-行或区域
;; ==============注释/反注释-行或区域==============
;;;###autoload
(defun comment-or-uncomment-region-or-line ()
  "Like comment-or-uncomment-region, but if there's no mark \(that means no
region\) apply comment-or-uncomment to the current line"
  (interactive)
  (if (not mark-active)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (if (< (point) (mark))
        (comment-or-uncomment-region (point) (mark))
      (comment-or-uncomment-region (mark) (point)))))
;; ==============注释/反注释-行或区域==============
;;; smart-beginning-of-line
;; =============smart-beginning-of-line============
;;;###autoload
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^") ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (if (memq major-mode '(dired-mode wdired-mode))
        (dired-move-to-filename)
      (back-to-indentation))
    (and (= oldpos (point))
         (beginning-of-line))))
;; =============smart-beginning-of-line============
;;; 添加连接线和下划线
;; ===============添加连接线和下划线===============
;;;###autoload
(defun jcs-dashify ()
  "Place hyphens between words in region."
  (interactive)
  (when (use-region-p)
    (goto-char (region-beginning))
    (while (re-search-forward "[ \n]+" (region-end) t)
      (replace-match "-"))))
;;;###autoload
(defun jcs-dashify-underline ()
  "Place hyphens between words in region."
  (interactive)
  (when (use-region-p)
    (goto-char (region-beginning))
    (while (re-search-forward "[ \n]+" (region-end) t)
      (replace-match "_"))))
;; ===============添加连接线和下划线===============
;;; 合并C-j和C-o
;; ==================合并C-j和C-o==================
;;;###autoload
(defun open-line-or-new-line-dep-pos ()
  "if point is in head of line then open-line
if point is at end of line , new-line-and-indent"
  (interactive)
  (cond
   ((and (= (point) (point-at-bol))
         (not (looking-at "^[ \t]*$")))
    (progn
      (open-line 1)
      (indent-for-tab-command)))
   ((= (- (point) (point-at-bol)) (current-indentation))
    (progn
      (split-line)
      (indent-for-tab-command)))
   (t
    (progn (delete-horizontal-space t)  ;clean-aindent-mode重新定义了newline-and-indent
           (newline nil t)
           (indent-according-to-mode)))))
;; ==================合并C-j和C-o==================
;;; jump to mark
;; ==================jump to mark==================
;;;###autoload
(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (let ((pos (marker-position (car (last mark-ring)))))
      (if (not (= (point) pos))
          (goto-char pos)
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) pos)
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))))
;;;###autoload
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
;;;###autoload
(defadvice unpop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
;; ==================jump to mark==================
;;; diff two regions
;; ================diff two regions================
;; Step 1: Select a region and `M-x diff-region-tag-selected-as-a'
;; Step 2: Select another region and `M-x diff-region-compare-with-b'
(defvar swint-diff-region-tag nil
  "Stores the current list of buffers that will be searched through.")
(defun diff-region-format-region-boundary (b e)
  "Make sure lines are selected and B is less than E"
  (let (tmp rlt)
    ;; swap b e, make sure b < e
    (when (> b e)
      (setq tmp b)
      (setq b e)
      (set e tmp))
    ;; select lines
    (save-excursion
      ;; Another workaround for evil-visual-line bug:
      ;; In evil-mode, if we use hotkey V or `M-x evil-visual-line` to select line,
      ;; the (line-beginning-position) of the line which is after the last selected
      ;; line is always (region-end)! Don't know why.
      (if (and (> e b)
               (save-excursion (goto-char e) (= e (line-beginning-position)))
               (boundp 'evil-state) (eq evil-state 'visual))
          (setq e (1- e)))
      (goto-char b)
      (setq b (line-beginning-position))
      (goto-char e)
      (setq e (line-end-position)))
    (setq rlt (list b e))
    rlt))
;;;###autoload
(defun diff-region-tag-selected-as-a ()
  "Select a region to compare"
  (interactive)
  (setq swint-diff-region-tag t)
  (when (region-active-p)
    (let (tmp buf)
      ;; select lines
      (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (car tmp) (cadr tmp))))
  (message "Now select other region to compare and run `diff-region-compare-with-b`"))
;;;###autoload
(defun diff-region-compare-with-b ()
  "Compare current region with region selected by `diff-region-tag-selected-as-a' "
  (interactive)
  (setq swint-diff-region-tag nil)
  (if (region-active-p)
      (let (rlt-buf
            diff-output
            (fa (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory))))
            (fb (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory)))))
        (when fb
          (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
          (write-region (car tmp) (cadr tmp) fb))
        (setq rlt-buf (get-buffer-create "*Diff-region-output*"))
        (when (and fa (file-exists-p fa) fb (file-exists-p fb))
          (save-current-buffer
            (set-buffer (get-buffer-create "*Diff-regionA*"))
            (write-region (point-min) (point-max) fa))
          (setq diff-output (shell-command-to-string (format "diff -Nabur %s %s" fa fb)))
          ;; show the diff output
          (if (string= diff-output "")
              (message "Two regions are SAME!")
            (save-current-buffer
              (switch-to-buffer-other-window rlt-buf)
              (set-buffer rlt-buf)
              (erase-buffer)
              (insert diff-output)
              (diff-mode))))
        (if (and fa (file-exists-p fa))
            (delete-file fa))
        (if (and fb (file-exists-p fb))
            (delete-file fb)))
    (message "Please select region at first!")))
;; ================diff two regions================
;;; parenthesis
;; =================parenthesis=================
;; (setq show-paren-style 'parenthesis)    ;Highlight just brackets.
;; (setq show-paren-style 'expression)     ;Highlight entire bracket expression.
;; (setq skeleton-pair t)                  ;自动插入匹配的括号。
;; (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "<") 'skeleton-pair-insert-maybe)
(defun insert-bracket-pair (leftBracket rightBracket)
  "Insert bracket pair automatically."
  (if (region-active-p)
      (let ((p1 (region-beginning))
            (p2 (region-end)))
        (goto-char p2)
        (insert rightBracket)
        (goto-char p1)
        (insert leftBracket)
        (goto-char (+ p2 2)))
    (progn
      (insert leftBracket rightBracket)
      (backward-char 1))))
(defun insert-bracket-pair-with-space (leftBracket rightBracket)
  "Insert bracket pair with space around automatically."
  (interactive)
  (if (or (char-equal (char-before) 32)
          (char-equal (char-before) 10))
      (insert leftBracket)
    (insert (concat " " leftBracket)))
  (if (or (char-equal (char-after) 32)
          (char-equal (char-after) 10))
      (progn (insert rightBracket)
             (backward-char 1))
    (progn (insert (concat rightBracket " "))
           (backward-char 2))))
;;;###autoload
(defun insert-pair-paren () (interactive) (insert-bracket-pair "(" ")"))
;;;###autoload
(defun insert-pair-bracket () (interactive) (insert-bracket-pair "[" "]"))
;;;###autoload
(defun insert-pair-brace () (interactive) (insert-bracket-pair "{" "}"))
;;;###autoload
(defun insert-pair-angle-bracket () (interactive) (insert-bracket-pair "<" ">"))
;;;###autoload
(defun insert-pair-double-angle-bracket () (interactive) (insert-bracket-pair "《" "》"))
;;;###autoload
(defun insert-pair-double-straight-quote () (interactive) (insert-bracket-pair "\"" "\""))
;;;###autoload
(defun insert-pair-single-straight-quote () (interactive) (insert-bracket-pair "'" "'"))
;;;###autoload
(defun insert-pair-double-curly-quote () (interactive) (insert-bracket-pair "“" "”"))
;;;###autoload
(defun insert-pair-single-curly-quote () (interactive) (insert-bracket-pair "‘" "’"))
;;;###autoload
(defun insert-pair-single-angle-quote‹› () (interactive) (insert-bracket-pair "‹" "›"))
;;;###autoload
(defun insert-pair-double-angle-quote«» () (interactive) (insert-bracket-pair "«" "»"))
;;;###autoload
(defun insert-pair-corner-bracket「」 () (interactive) (insert-bracket-pair "「" "」"))
;;;###autoload
(defun insert-pair-white-corner-bracket『』 () (interactive) (insert-bracket-pair "『" "』"))
;;;###autoload
(defun insert-pair-white-lenticular-bracket〖〗 () (interactive) (insert-bracket-pair "〖" "〗"))
;;;###autoload
(defun insert-pair-black-lenticular-bracket【】 () (interactive) (insert-bracket-pair "【" "】"))
;;;###autoload
(defun insert-pair-tortoise-shell-bracket〔〕 () (interactive) (insert-bracket-pair "〔" "〕"))
;;;###autoload
(defun insert-pair-math-bracket () (interactive) (insert-bracket-pair-with-space "$" "$"))
;; =================parenthesis=================
