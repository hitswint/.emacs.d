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
