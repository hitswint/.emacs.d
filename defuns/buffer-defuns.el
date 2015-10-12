;; ===============退出shell时关闭buffer===============
(defun kill-shell-buffer(process event)
  "The one actually kill shell buffer when exit. "
  (kill-buffer (process-buffer process))
  )
(defun kill-shell-buffer-after-exit()
  "kill shell buffer when exit."
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'kill-shell-buffer)
  )
(add-hook 'shell-mode-hook 'kill-shell-buffer-after-exit t)
;; ===============退出shell时关闭buffer===============
;; ===============上一个下一个buffer=================
(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))
(defun previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))
(defun next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (next-buffer) )))
(defun previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))
;; sample easy keys
(global-set-key (kbd "<C-prior>") 'previous-user-buffer) ; Ctrl+PageUp
(global-set-key (kbd "<C-next>") 'next-user-buffer) ; Ctrl+PageDown
;; (global-set-key (kbd "<C-S-prior>") 'previous-emacs-buffer) ; Ctrl+Shift+PageUp
;; (global-set-key (kbd "<C-S-next>") 'next-emacs-buffer) ; Ctrl+Shift+PageDown
;; ===============上一个下一个buffer=================
;; ======================intuitive window resizing=======================
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
;; keybindings for window resizing
(global-set-key (kbd "C-s-h") 'move-border-left)
(global-set-key (kbd "C-s-l") 'move-border-right)
(global-set-key (kbd "C-s-k") 'move-border-up)
(global-set-key (kbd "C-s-j") 'move-border-down)
;; ======================intuitive window resizing=======================
;; ======================切换窗口分割模式========================
(global-set-key (kbd "C-c C-i") 'toggle-window-split)
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
;; ======================切换窗口分割模式========================
;; ======================循环窗口============================
(global-set-key (kbd "C-c C-o") 'rotate-windows)
(defun rotate-windows ()
  "Rotate your windows"
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
;; ======================循环窗口============================
;; ======================关闭buffer并删除文件================
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)
;; ======================关闭buffer并删除文件================
;; ====================重命名当前buffer======================
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
;; ====================重命名当前buffer======================
;; ====================获得当前buffer的major-mode======================
(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
                      (if buffer-or-name
			  (get-buffer buffer-or-name)
			(current-buffer))))
;; ====================获得当前buffer的major-mode======================
