;; ===============切换cap和大小写==================
(global-set-key (kbd "M-c") 'toggle-letter-case)
(global-set-key (kbd "M-C") 'capitalize-word)
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
      (downcase-region p1 p2) (put this-command 'state "all lower")))
    ))
;; ===============切换cap和大小写==================
;; ===========compact-uncompact-block==============
(global-set-key (kbd "C-M-q") 'compact-uncompact-block)
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
;; =================复制和粘贴行===================
;; (defun copy-line-or-region ()
;;   "Copy current line, or current text selection."
;;   (interactive)
;;   (if (region-active-p)
;;       (kill-ring-save (region-beginning) (region-end))
;;     (kill-ring-save (line-beginning-position) (line-beginning-position 2))))
(defun cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))
;; (global-set-key (kbd "M-w") 'copy-line-or-region)
(global-set-key (kbd "C-w") 'cut-line-or-region)
;; =================复制和粘贴行===================
;; ========光标不动，窗口上下移动两三行============
(defun window-move-up (&optional arg)
  "Current window move-up 2 lines."
  (interactive "P")
  (if arg
      (scroll-up arg)
    (scroll-up 2)))
(defun window-move-down (&optional arg)
  "Current window move-down 3 lines."
  (interactive "P")
  (if arg
      (scroll-down arg)
    (scroll-down 3)))
(global-set-key [(meta n)] 'window-move-up)
(global-set-key [(meta p)] 'window-move-down)
;; ========光标不动，窗口上下移动两三行============
;; ===================合并一行=====================
(global-set-key (kbd "M-Q")
                (lambda()
                  (interactive)
                  (join-line -1)))
;; ===================合并一行=====================
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
;; (add-hook 'before-save-hook 'cleanup-buffer-safe) ;会导致mew发送附件时，保存失败，进而发送失败
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-x C-;") 'cleanup-buffer)
;; ============移除行尾的空格并indent==============
;; ==================临时标记======================
(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.
Use ska-jump-to-register to jump back to the stored
position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 1))
(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
    (jump-to-register 1)
    (set-register 8 tmp)))
;; ==================临时标记======================
;; ============跳转到某行时行号暂时可见============
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
;; ============跳转到某行时行号暂时可见============
;; ==============注释/反注释-行或区域==============
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
(global-set-key (kbd "M-:") 'comment-or-uncomment-region-or-line)
;; ==============注释/反注释-行或区域==============
;; =============smart-beginning-of-line============
;; "smart" home, i.e., home toggles b/w 1st non-blank character and 1st column
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^") ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "C-a") 'smart-beginning-of-line)
;; =============smart-beginning-of-line============
;; ==================滚动更流畅====================
;; (setq redisplay-dont-pause t
;;       scroll-margin 1
;;       scroll-step 1
;;       scroll-conservatively 10000
;;       scroll-preserve-screen-position 1)
;; (setq mouse-wheel-follow-mouse 't)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; ==================滚动更流畅====================
;; ==================上下移动行====================
;; ;; 使用 drag-stuff-mode 替代
;; (defun move-line-down ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (forward-line)
;;       (transpose-lines 1))
;;     (forward-line)
;;     (move-to-column col)))
;; (defun move-line-up ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (forward-line)
;;       (transpose-lines -1))
;;     (move-to-column col)))
;; (global-set-key (kbd "M-N") 'move-line-down)
;; (global-set-key (kbd "M-P") 'move-line-up)
;; ==================上下移动行====================
;; ===============添加连接线和下划线===============
(defun jcs-dashify ()
  "Place hyphens between words in region."
  (interactive)
  (unless (use-region-p)
    (error "No region specified"))
  (replace-regexp "[ \n]+" "-" nil (region-beginning) (region-end)))
(defun jcs-dashify-underline ()
  "Place hyphens between words in region."
  (interactive)
  (unless (use-region-p)
    (error "No region specified"))
  (replace-regexp "[ \n]+" "_" nil (region-beginning) (region-end)))
(global-set-key (kbd "C-x -") 'jcs-dashify)
(global-set-key (kbd "C-x _") 'jcs-dashify-underline)
;; ===============添加连接线和下划线===============
;; ==================复制矩形区域==================
(defun copy-rectangle-as-kill ()
  (interactive)
  (save-excursion
    (kill-rectangle (mark) (point))
    (exchange-point-and-mark)
    (yank-rectangle)))
(global-set-key (kbd "C-x r w") 'copy-rectangle-as-kill)
;; 复制矩形区域三种方法：
;; 1. 使用C-x rrr复制，使用C-x rir粘贴。
;; 2. 使用multiple-cursors复制，使用C-x ry粘贴。
;; 3. 使用C-x SPC直接矩形复制，使用C-y粘贴。
;; ==================复制矩形区域==================
;; ==================合并C-j和C-o==================
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
(global-set-key (kbd "C-j") 'open-line-or-new-line-dep-pos)
;; ==================合并C-j和C-o==================
;; ==================jump to mark==================
(global-set-key (kbd "M-m") 'pop-to-mark-command)
(global-set-key (kbd "M-M") 'unpop-to-mark-command)
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
;; When popping the mark, continue popping until the cursor actually moves.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
(defadvice unpop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
;; ==================jump to mark==================
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
(global-set-key (kbd "M-s =") '(lambda ()
                                 (interactive)
                                 (if swint-diff-region-tag
                                     (diff-region-compare-with-b)
                                   (diff-region-tag-selected-as-a))))
;; ================diff two regions================
