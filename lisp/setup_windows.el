;;; winner-mode
;; =================winner-mode=================
(use-package winner
  ;; Enabled at commands.
  :defer t
  :bind (("M-/" . winner-undo)
         ("M-s M-/" . winner-redo))
  :config
  (winner-mode 1))
;; =================winner-mode=================
;;; window-numbering
;; ==============window-numbering===============
(use-package window-numbering
  ;; Enabled automatically.
  :defer t
  :bind ("C-<tab>" . select-previously-selected-window)
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
          (prev-buf (car (or (swint-filter-buffer-list (mapcar #'(lambda (x) (car x)) (window-prev-buffers)))
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
      (setq previously-selected-window current-selected-window)))
  (cond
   (is-lin (global-set-key (kbd "<C-S-iso-lefttab>") 'transpose-with-previously-selected-window))
   (is-win (global-set-key (kbd "C-S-<tab>") 'transpose-with-previously-selected-window))))
;; ==============window-numbering===============
;;; windmove
;; ================windmove=====================
(use-package windmove
  ;; Enabled at commands.
  :defer t
  :commands (swint-windmove-left swint-windmove-right swint-windmove-up swint-windmove-down)
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
    (funcall (ignore-error-wrapper 'windmove-down))))
;; ================windmove=====================
;;; intuitive window resizing
;; ===========intuitive window resizing============
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
  (move-border-up-or-down arg nil))
;; ===========intuitive window resizing============
;;; 三窗口设置
;; ================三窗口设置===================
(defun split-window-3-horizontally (&optional arg)
  "Split window into 3 while largest one is in horizon."
  ;; (interactive "P")
  (delete-other-windows)
  (split-window-horizontally)
  (if arg (other-window 1))
  (split-window-vertically))
(defun split-window-3-vertically (&optional arg)
  "Split window into 3 while largest one is in vertical."
  ;; (interactive "P")
  (delete-other-windows)
  (split-window-vertically)
  (if arg (other-window 1))
  (split-window-horizontally))
(defun change-split-type (split-fn &optional arg)
  "Change 3 window style from horizontal to vertical and vice-versa."
  (let ((bufList (mapcar 'window-buffer (window-list))))
    (select-window (get-largest-window))
    (funcall split-fn arg)
    (mapcar* 'set-window-buffer (window-list) bufList)))
(defun change-split-type-3-v (&optional arg)
  "change 3 window style from horizon to vertical"
  (interactive "P")
  (change-split-type 'split-window-3-horizontally arg))
(defun change-split-type-3-h (&optional arg)
  "change 3 window style from vertical to horizon"
  (interactive "P")
  (change-split-type 'split-window-3-vertically arg))
;; ================三窗口设置===================
;;; 切换窗口分割模式
;; ===============切换窗口分割模式=================
(global-set-key (kbd "C-\\") 'toggle-window-split)
;; (defun window-toggle-split-direction ()
;;   "Switch window split from horizontally to vertically, or vice versa.
;; i.e. change right window to bottom, or change bottom window to right."
;;   (interactive)
;;   (require 'windmove)
;;   (let ((done))
;;     (dolist (dirs '((right . down) (down . right)))
;;       (unless done
;;         (let* ((win (selected-window))
;;                (nextdir (car dirs))
;;                (neighbour-dir (cdr dirs))
;;                (next-win (windmove-find-other-window nextdir win))
;;                (neighbour1 (windmove-find-other-window neighbour-dir win))
;;                (neighbour2 (if next-win (with-selected-window next-win
;;                                           (windmove-find-other-window neighbour-dir next-win)))))
;;           ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
;;           (setq done (and (eq neighbour1 neighbour2)
;;                           (not (eq (minibuffer-window) next-win))))
;;           (if done
;;               (let* ((other-buf (window-buffer next-win)))
;;                 (delete-window next-win)
;;                 (if (eq nextdir 'right)
;;                     (split-window-vertically)
;;                   (split-window-horizontally))
;;                 (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))
;; 上面的函数经常会失效，用下面的替代。
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
;; ===============切换窗口分割模式=================
;;; 循环窗口
;; ===================循环窗口=====================
(global-set-key (kbd "C-x O") 'rotate-windows)
(defun rotate-windows ()
  "Rotate your windows."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1 b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))
;; ===================循环窗口=====================
(provide 'setup_windows)
