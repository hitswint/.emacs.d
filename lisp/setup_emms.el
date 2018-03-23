;;; emms
;; ================emms==================
(def-package! emms
  :bind (("M-s e l" . emms-playlist-mode-go)
         ("M-s e o" . emms-play-file))
  :config
  (advice-add 'emms-playlist-mode-go :before #'emms-player-mpd-connect)
  (advice-add 'emms-play-file :before #'(lambda (file) (emms-player-mpd-update-all)))
  (def-package! emms-setup)
  (emms-all)
  (setq emms-player-mpg321-command-name "mpg321"
        emms-player-mplayer-command-name "mplayer"
        emms-player-list '(emms-player-mplayer
                           emms-player-mplayer-playlist
                           emms-player-ogg123
                           emms-player-mpg321))
  (setq emms-source-file-default-directory "~/Music/")
  (setq emms-playlist-buffer-name "Enjoy Music")
  (add-hook 'emms-player-started-hook 'emms-show)
  (setq emms-show-format "%s")
  (setq emms-mode-line-mode-line-function nil)
  (setq emms-mode-line-format ""
        emms-lyrics-display-format ""
        emms-lyrics-display-on-modeline nil
        emms-playing-time-display-format "")
;;;; emms-mpd
  ;; ==============emms-mpd================
  (def-package! emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory "~/Music")
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  ;; ==============emms-mpd================
  (global-set-key (kbd "M-s e c") 'emms-player-mpd-connect)
  (global-set-key (kbd "s-/") 'emms-pause)
  (global-set-key (kbd "s-?") 'emms-stop)
  (global-set-key (kbd "s-,") 'emms-seek-backward)
  (global-set-key (kbd "s-.") 'emms-seek-forward)
  (global-set-key (kbd "s-<") 'emms-previous)
  (global-set-key (kbd "s->") 'emms-next)
  (global-set-key (kbd "C-s-,") 'emms-volume-lower)
  (global-set-key (kbd "C-s-.") 'emms-volume-raise))
;; ================emms==================
(provide 'setup_emms)
