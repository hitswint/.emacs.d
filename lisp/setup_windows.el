;;; winner-mode
;; =================winner-mode=================
(def-package! winner
  :bind (("M-/" . winner-undo)
         ("M-s M-/" . winner-redo))
  :config
  (winner-mode 1))
;; =================winner-mode=================
;;; window-numbering
;; ==============window-numbering===============
(def-package! window-numbering
  :bind (("C-<tab>" . select-previously-selected-window)
         ("<C-S-iso-lefttab>" . transpose-with-previously-selected-window))
  :config
  (window-numbering-mode 1)
  (set-face-attribute 'window-numbering-face nil :background "dark red" :foreground "white")
  ;; 当按键大于现有窗口数目时，选中最后一个窗口。
  (defvar previously-selected-window nil
    "Previously-selected-window.")
  (defun select-window-by-number (i &optional arg)
    "Select window given number I by `window-numbering-mode'.
If prefix ARG is given, delete the window instead of selecting it."
    (interactive "P")
    (let ((windows (car (gethash (selected-frame) window-numbering-table)))
          window)
      (if (and (>= i 0) (< i 10)
               (setq window (aref windows i)))
          ()
        (setq window (aref windows (- (car window-numbering-left) 1))))
      (setq previously-selected-window (selected-window))
      (if arg
          (delete-window window)
        (select-window window))))
  (define-key window-numbering-keymap (kbd "C-1") 'select-window-1)
  (define-key window-numbering-keymap (kbd "C-2") 'select-window-2)
  (define-key window-numbering-keymap (kbd "C-3") 'select-window-3)
  (define-key window-numbering-keymap (kbd "C-4") 'select-window-4)
  (define-key window-numbering-keymap (kbd "C-5") 'select-window-5)
  (define-key window-numbering-keymap (kbd "C-6") 'select-window-0)
  (defun transpose-window-by-number (i &optional arg)
    "Transpose the buffers shown in two windows."
    (interactive "p")
    (let ((windows (car (gethash (selected-frame) window-numbering-table)))
          (this-win (window-buffer))
          window)
      (if (and (>= i 0) (< i 10)
               (setq window (aref windows i)))
          ()
        (setq window (aref windows (- (car window-numbering-left) 1))))
      (set-window-buffer (selected-window) (window-buffer window))
      (set-window-buffer window this-win)
      (select-window window)))
  (dotimes (i 10)
    (eval `(defun ,(intern (format "transpose-window-%s" i)) (&optional arg)
             ,(format "Transpose the window with number %i." i)
             (interactive "P")
             (transpose-window-by-number ,i arg))))
  (define-key window-numbering-keymap (kbd "C-!") 'transpose-window-1)
  (define-key window-numbering-keymap (kbd "C-@") 'transpose-window-2)
  (define-key window-numbering-keymap (kbd "C-#") 'transpose-window-3)
  (define-key window-numbering-keymap (kbd "C-$") 'transpose-window-4)
  (define-key window-numbering-keymap (kbd "C-%") 'transpose-window-5)
  (define-key window-numbering-keymap (kbd "C-^") 'transpose-window-0)
  (defun select-previously-selected-window ()
    "Select previously selected window."
    (interactive)
    (let ((current-selected-window (selected-window))
          (prev-buf (car (or (swint-filter-buffer-list (cl-loop for x in (window-prev-buffers)
                                                                collect (car x)))
                             (swint-filter-buffer-list (buffer-list (selected-frame)) t)))))
      (if (one-window-p)
          (switch-to-buffer prev-buf)
        (if (and (memq previously-selected-window (window-list))
                 (not (equal previously-selected-window (selected-window))))
            (select-window previously-selected-window)
          (other-window 1)))
      (setq previously-selected-window current-selected-window)))
  (defun transpose-with-previously-selected-window ()
    "Select previously selected window."
    (interactive)
    (let ((current-selected-window (selected-window))
          (this-win (window-buffer)))
      (if (and
           (memq previously-selected-window (window-list) ;; (append (car (gethash (selected-frame) window-numbering-table)) nil)
                 ) ;之前选择window在当前window列表中
           (not (equal previously-selected-window (selected-window)))) ;之前选择window与当前window不同
          (progn (set-window-buffer (selected-window) (window-buffer previously-selected-window))
                 (set-window-buffer previously-selected-window this-win)
                 (select-window previously-selected-window))
        (progn (set-window-buffer (selected-window) (window-buffer (next-window)))
               (set-window-buffer (next-window) this-win)
               (other-window 1)))
      (setq previously-selected-window current-selected-window))))
;; ==============window-numbering===============
;;; windmove
;; ================windmove=====================
(def-package! windmove
  :commands (swint-windmove-left
             swint-windmove-right
             swint-windmove-up
             swint-windmove-down
             move-border-left
             move-border-right
             move-border-up
             move-border-down)
  :init
  (smartrep-define-key global-map "M-s"
    '(("H" . swint-windmove-left)
      ("L" . swint-windmove-right)
      ("K" . swint-windmove-up)
      ("J" . swint-windmove-down)
      ("h" . move-border-left)
      ("l" . move-border-right)
      ("k" . move-border-up)
      ("j" . move-border-down)))
  :config
  (defun swint-windmove-left ()
    "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
    (interactive)
    (setq previously-selected-window (selected-window))
    (funcall (ignore-error-wrapper 'windmove-left)))
  (defun swint-windmove-right ()
    "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
    (interactive)
    (setq previously-selected-window (selected-window))
    (funcall (ignore-error-wrapper 'windmove-right)))
  (defun swint-windmove-up ()
    "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
    (interactive)
    (setq previously-selected-window (selected-window))
    (funcall (ignore-error-wrapper 'windmove-up)))
  (defun swint-windmove-down ()
    "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
    (interactive)
    (setq previously-selected-window (selected-window))
    (funcall (ignore-error-wrapper 'windmove-down)))
  (defun xor (b1 b2)
    (or (and b1 b2)
        (and (not b1) (not b2))))
  (defun move-border-left-or-right (arg dir)
    "General function covering move-border-left and move-border-right. If DIR is
     t, then move left, otherwise move right."
    (interactive)
    (if (null arg) (setq arg 10))
    (let ((left-edge (nth 0 (window-edges))))
      (if (xor (= left-edge 0) dir)
          (shrink-window arg t)
        (enlarge-window arg t))))
  (defun move-border-up-or-down (arg dir)
    "General function covering move-border-up and move-border-down. If DIR is
     t, then move up, otherwise move down."
    (interactive)
    (if (null arg) (setq arg 10))
    (let ((top-edge (nth 1 (window-edges))))
      (if (xor (= top-edge 0) dir)
          (shrink-window arg nil)
        (enlarge-window arg nil))))
  (defun move-border-left (arg)
    (interactive "P")
    (move-border-left-or-right arg t))
  (defun move-border-right (arg)
    (interactive "P")
    (move-border-left-or-right arg nil))
  (defun move-border-up (arg)
    (interactive "P")
    (move-border-up-or-down arg t))
  (defun move-border-down (arg)
    (interactive "P")
    (move-border-up-or-down arg nil)))
;; ================windmove=====================
(provide 'setup_windows)
