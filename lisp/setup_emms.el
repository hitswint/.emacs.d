;;; emms
;; ================emms==================
(use-package emms
  :bind (("M-o M-e" . helm-emms-list)
         ("M-o e" . emms-playlist-mode-go)
         ("<s-down>" . emms-pause)
         ("<S-s-down>" . emms-stop)
         ("<s-left>" . emms-seek-backward)
         ("<s-right>" . emms-seek-forward)
         ("<S-s-left>" . emms-previous)
         ("<S-s-right>" . emms-next)
         ("<C-s-down>" . emms-volume-lower)
         ("<C-s-up>" . emms-volume-raise))
  :config
  (use-package emms-setup
    :config
    (emms-all)
    (emms-default-players))
  (add-hook 'emms-player-started-hook 'emms-show)
  (setq emms-source-file-default-directory "~/Music/"
        emms-playlist-buffer-name "Enjoy Music"
        emms-show-format "%s"
        emms-mode-line-mode-line-function nil
        emms-mode-line-format ""
        emms-lyrics-display-format ""
        emms-lyrics-display-on-modeline nil
        emms-playing-time-display-format ""
        emms-info-report-each-num-tracks 0)
;;;; emms-mpd
  ;; ==============emms-mpd================
  (use-package emms-player-mpd
    :config
    (define-key emms-playlist-mode-map (kbd "C-c c") 'emms-player-mpd-connect)
    (advice-add 'emms-playlist-mode-go :before #'emms-player-mpd-connect)
    (advice-add 'emms-play-file :before #'(lambda (file) (emms-player-mpd-update-all)))
    (setq emms-player-mpd-server-name "localhost"
          emms-player-mpd-server-port "6600"
          emms-player-mpd-music-directory "~/Music")
    (add-to-list 'emms-info-functions 'emms-info-mpd)
    ;; 播放时遍历emms-player-list，使用第1个可播放的player
    (add-to-list 'emms-player-list 'emms-player-mpd)
    ;; 解决emms无法播放链接文件的问题
    (advice-add 'emms-player-mpd-playable-p :around #'(lambda (fn &rest args)
                                                        (cl-letf (((symbol-function 'file-in-directory-p) 'f-descendant-of-p))
                                                          (apply fn args)))))
  (defun helm-emms-list (&optional arg)
    (interactive "P")
    (if arg
        (call-interactively 'emms-play-file)
      (when (or (not emms-playlist-buffer)
                (not (buffer-live-p emms-playlist-buffer)))
        (emms-playlist-mode-go)
        (sit-for 1))
      (let ((selected (helm-comp-read "Select track: " (cl-loop for v being the hash-values in emms-cache-db
                                                                for name = (assoc-default 'name v)
                                                                unless (string-match "^\\(http\\|mms\\):" name)
                                                                collect (cons (file-name-base name) v))
                                      :preselect (when-let ((current-track (emms-playlist-current-selected-track)))
                                                   (file-name-base (assoc-default 'name current-track)))
                                      :candidate-number-limit 9999
                                      :buffer "*helm emms-swint*")))
        (with-current-emms-playlist
          (when-let ((pos (text-property-any (point-min) (point-max) 'emms-track selected)))
            (when emms-player-playing-p
              (emms-stop))
            (emms-playlist-select pos)
            (emms-start))))))
  ;; ==============emms-mpd================
  )
;; ================emms==================
(provide 'setup_emms)
