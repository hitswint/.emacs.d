;;; emms
;; ================emms==================
(def-package! emms
  :bind (("M-s e l" . emms-playlist-mode-go)
         ("M-s e o" . emms-play-file)
         ("<s-down>" . emms-pause)
         ("<S-s-down>" . emms-stop)
         ("<s-left>" . emms-seek-backward)
         ("<s-right>" . emms-seek-forward)
         ("<S-s-left>" . emms-previous)
         ("<S-s-right>" . emms-next)
         ("<C-s-down>" . emms-volume-lower)
         ("<C-s-up>" . emms-volume-raise))
  :config
  (def-package! emms-setup
    :config
    (emms-all)
    (emms-default-players))
  (add-hook 'emms-player-started-hook 'emms-show)
  (setq emms-source-file-default-directory "~/Music/")
  (setq emms-playlist-buffer-name "Enjoy Music")
  (setq emms-show-format "%s")
  (setq emms-mode-line-mode-line-function nil)
  (setq emms-mode-line-format ""
        emms-lyrics-display-format ""
        emms-lyrics-display-on-modeline nil
        emms-playing-time-display-format "")
;;;; emms-mpd
  ;; ==============emms-mpd================
  (def-package! emms-player-mpd
    :config
    (define-key emms-playlist-mode-map (kbd "C-c c") 'emms-player-mpd-connect)
    (advice-add 'emms-playlist-mode-go :before #'emms-player-mpd-connect)
    (advice-add 'emms-play-file :before #'(lambda (file) (emms-player-mpd-update-all)))
    (setq emms-player-mpd-server-name "localhost")
    (setq emms-player-mpd-server-port "6600")
    (setq emms-player-mpd-music-directory "~/Music")
    (add-to-list 'emms-info-functions 'emms-info-mpd)
    ;; 播放时遍历emms-player-list，使用可播放的最后一个player
    (add-to-list 'emms-player-list 'emms-player-mpd t))
  ;; ==============emms-mpd================
  )
;; ================emms==================
(provide 'setup_emms)
