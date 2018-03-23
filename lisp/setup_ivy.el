;;; ivy
;; =====================ivy========================
(def-package! ivy
  :after (:any swiper counsel)
  :config
  (bind-key "M-s y" 'ivy-resume)
  (bind-key "C-h" 'ivy-avy ivy-minibuffer-map)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "%d/%d ")
  ;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
  (defun swint-pinyinlib-build-regexp-string (str)
    (cond ((equal str ".*") ".*")
          (t (pinyinlib-build-regexp-string str t))))
  (defun swint-pinyin-regexp-helper (str)
    (cond ((equal str " ") ".*")
          ((equal str "") nil)
          (t str)))
  (defun pinyin-to-utf8 (str)
    (cond ((equal 0 (length str)) nil)
          (t (mapconcat 'swint-pinyinlib-build-regexp-string
                        (remove nil (mapcar 'swint-pinyin-regexp-helper
                                            (split-string str ""))) ""))))
  (defun re-builder-pinyin (str)
    (or (pinyin-to-utf8 str)
        (ivy--regex-plus str)
        (ivy--regex-ignore-order str)))
  (setq ivy-re-builders-alist '((t . re-builder-pinyin)))
  (def-package! ivy-hydra
    :after ivy)
  (def-package! hydra
    :after ivy-hydra))
;; =====================ivy========================
;;; swiper
;; ===================swiper=======================
(def-package! swiper
  :commands swiper-from-isearch
  :bind (("M-s s" . swint-swiper)
         ("M-s S" . swiper-all))
  :init
  (add-hook 'isearch-mode-hook (lambda ()
                                 (bind-key "M-s s" 'swiper-from-isearch isearch-mode-map)))
  :config
  (defun swint-swiper ()
    (interactive)
    (let ((swint-swiper-current-thing
           (if (region-active-p)
               (buffer-substring (region-beginning) (region-end))
             (symbol-name-at-point))))
      (deactivate-mark)
      (swiper swint-swiper-current-thing))))
;; ===================swiper=======================
;;; counsel
;; ===================counsel======================
(def-package! counsel
  ;; 按键逻辑：helm(C-x c x)/counsel(M-s c x)。
  :bind (("M-X" . counsel-M-x)
         ("C-x C-r" . swint-counsel-history)
         ("M-s c u" . counsel-unicode-char)
         ("M-s c l" . counsel-locate)
         ("M-s c i" . counsel-imenu)
         ("M-s c C-x C-f" . counsel-find-file)
         ("M-s c o" . counsel-outline)
         ("M-s c d" . counsel-dpkg)
         ("M-s c g" . counsel-ag)
         ("M-s c p" . counsel-list-processes)
         ("M-s c M-y" . counsel-yank-pop))
  :config
  (defun swint-counsel-history ()
    "List command history based on major-mode."
    (interactive)
    (cond
     ((memq major-mode '(shell-mode inferior-python-mode inferior-octave-mode))
      (call-interactively 'counsel-shell-history))
     ((eq major-mode 'eshell-mode)
      (call-interactively 'counsel-esh-history))
     (t (call-interactively 'swint-counsel-sh-history))))
  (defun swint-counsel-sh-history ()
    "Insert the bash history."
    (interactive)
    (let (hist-cmd collection val)
      (setq collection
            (nreverse
             (split-string (with-temp-buffer (insert-file-contents
                                              (if (file-exists-p "~/.zsh_history")
                                                  (file-truename "~/.zsh_history")
                                                (file-truename "~/.bash_history")))
                                             (while (re-search-forward "^: [0-9]+:[0-9];\\(.+\\)\n" nil t)
                                               (replace-match "\\1\n"))
                                             (buffer-string))
                           "\n" t)))
      (when (and collection (> (length collection) 0)
                 (setq val (if (= 1 (length collection)) (car collection)
                             (ivy-read (format "Bash/Zsh history:") collection))))
        (insert val)))))
;; ===================counsel======================
(provide 'setup_ivy)
