;;; dos2unix
;; =====================dos2unix===================
;;;###autoload
(defun save-buffer-with-dos2unix ()
  (interactive)
  (let ((file-to-convert (file-name-nondirectory (buffer-file-name (current-buffer)))))
    (if (buffer-modified-p)
        (progn (save-buffer)
               (shell-command (concat "dos2unix " file-to-convert))
               (revert-buffer nil t))
      (save-buffer))))
;; =====================dos2unix===================
;;; swint-open-output-file
;; =============swint-open-output-file=============
;;;###autoload
(defun swint-open-output-file ()
  "Swint open gnuplot output file."
  (interactive)
  (let* ((output-pdf-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))
         (output-png-file (concat (file-name-sans-extension (buffer-file-name)) ".png"))
         (output-eps-file (concat (file-name-sans-extension (buffer-file-name)) ".eps"))
         (output-file (cond ((file-exists-p output-pdf-file) output-pdf-file)
                            ((file-exists-p output-png-file) output-png-file)
                            ((file-exists-p output-eps-file) output-eps-file))))
    (if output-file
        (dired-async-shell-command output-file)
      (message "Warning: No export file."))))
;; =============swint-open-output-file=============
;;; get-auth
;; ====================get-auth====================
;;;###autoload
(defun get-auth-user (host)
  (require 'netrc)
  (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
         (hostentry (netrc-machine netrc host)))
    (when hostentry (netrc-get hostentry "login"))))
;;;###autoload
(defun get-auth-pass (host)
  (require 'netrc)
  (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
         (hostentry (netrc-machine netrc host)))
    (when hostentry (netrc-get hostentry "password"))))
;; ====================get-auth====================
;;; 截图
;; ===================截图====================
;; Screenshot-local截图到./pic文件夹中，screenshot截图到~/org/pic文件夹中。
;;;###autoload
(defun swint-screenshot (&optional arg)
  "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
  (interactive "P")
  ;; 将截图名字定义为buffer名字加日期。
  (let ((screen-file-path (if arg
                              (concat (getenv "HOME") "/org/pic/")
                            (progn (unless (file-exists-p "./pic")
                                     ;; 建立pic文件夹。
                                     (dired-create-directory "./pic"))
                                   "./pic/")))
        screen-file)
    (setq screen-file (concat (concat screen-file-path (file-name-base (or (buffer-file-name) (buffer-name)))
                                      "_" (format-time-string "%Y%m%d_%H%M%S")) ".png"))
    ;; (suspend-frame)
    (call-process-shell-command
     ;; (concat "scrot" " -s " "\"" screen-file "\"" )
     (concat "import " "\"" screen-file "\"" ))
    screen-file))
;; ===================截图====================
;;; 插入截图
;; =================插入截图==================
;;;###autoload
(defun swint-insert-screenshot (&optional arg)
  "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
  (interactive "P")
  (let ((screen-file-name (swint-screenshot arg)))
    (if (eq major-mode 'org-mode)
        (progn (insert (concat "[[" (abbreviate-file-name screen-file-name) "]]"))
               (org-redisplay-inline-images))
      (insert (abbreviate-file-name screen-file-name)))))
;; =================插入截图==================
