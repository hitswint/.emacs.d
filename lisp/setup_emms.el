;;; emms
;; ================emms==================
(use-package emms
  :bind (("M-o M-e" . emms-playlist-mode-go)
         ("M-o e" . emms-play-file)
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
        emms-playing-time-display-format "")
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
    (add-to-list 'emms-player-list 'emms-player-mpd))
  ;; ==============emms-mpd================
  )
;; ================emms==================
(provide 'setup_emms)
