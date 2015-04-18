;; ==============emms==================
(require 'emms-setup)
(emms-devel)
(setq emms-player-mpg321-command-name "mpg321"
      emms-player-mplayer-command-name "mplayer"
      emms-player-list '(emms-player-mplayer
                         emms-player-mplayer-playlist
                         emms-player-ogg123
                         emms-player-mpg321))
(setq emms-source-file-default-directory "~/Music/")
(setq emms-playlist-buffer-name "Enjoy Music")
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "播放: %s")
(setq emms-mode-line-mode-line-function nil)
(setq emms-mode-line-format ""
      emms-lyrics-display-format ""
      emms-playing-time-display-format "") ; 关闭emms在mode-line上的显示
;; ==============emms-mpd================
(require 'emms-player-mpd)
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")
(setq emms-player-mpd-music-directory "~/Music")
(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
;; ==============emms-mpd================
(defun swint-playlist ()
  "open file with external app"
  (interactive)
  (emms-player-mpd-connect)
  (emms-playlist-mode-go))
;; emms-mpd快捷键设置
(global-set-key (kbd "C-c e c") 'emms-player-mpd-connect)
(global-set-key (kbd "C-c e l") 'swint-playlist)
(global-set-key (kbd "C-c e o") 'emms-play-file)
(global-set-key (kbd "C-M-SPC") 'emms-pause)
(global-set-key (kbd "C-M-<up>") 'emms-volume-raise)
(global-set-key (kbd "C-M-<down>") 'emms-volume-lower)
(global-set-key (kbd "C-M-<left>") 'emms-seek-backward)
(global-set-key (kbd "C-M-<right>") 'emms-seek-forward)
(global-set-key (kbd "C-s-<left>") 'emms-previous)
(global-set-key (kbd "C-s-<right>") 'emms-next)
(global-set-key (kbd "C-s-SPC") 'emms-stop)
;; emms使用mpg321或者mplayer的快捷键设置
;; (global-set-key (kbd "C-c e l") 'swint-playlist)
;; (global-set-key (kbd "C-c e s") 'emms-start)
;; (global-set-key (kbd "C-c e t") 'emms-stop)
;; (global-set-key (kbd "C-c e n") 'emms-next)
;; (global-set-key (kbd "C-c e p") 'emms-previous)
;; (global-set-key (kbd "C-c e e") 'emms-pause)
;; (global-set-key (kbd "C-c e f") 'emms-play-playlist)
;; (global-set-key (kbd "C-c e o") 'emms-play-file)
;; (global-set-key (kbd "C-c e d") 'emms-play-directory-tree)
;; (global-set-key (kbd "C-c e a") 'emms-add-directory-tree)
;; (global-set-key (kbd "C-c C--") 'emms-seek-backward)
;; (global-set-key (kbd "C-c C-=") 'emms-seek-forward)
;; (global-set-key (kbd "C-c e c") 'emms-player-mpd-connect)
;; ==============emms==================
;; ;; ==============mingus==================
;; (add-to-list 'load-path "~/.emacs.d/mingus/")
;; (autoload 'mingus "mingus-stays-home" nil t)
;; (global-set-key (kbd "C-c e l") 'mingus)
;; (global-set-key (kbd "C-c e o") '(lambda () (interactive)
;;                                    (mingus)
;;                                    (mingus-browse)
;;                                    ))
;; (global-set-key (kbd "C-M-SPC") 'mingus-toggle)
;; (global-set-key (kbd "C-M-<up>") 'mingus-vol-up)
;; (global-set-key (kbd "C-M-<down>") 'mingus-vol-down)
;; (global-set-key (kbd "C-M-<left>") 'mingus-seek-backward)
;; (global-set-key (kbd "C-M-<right>") 'mingus-seek)
;; (global-set-key (kbd "C-s-<left>") 'mingus-prev)
;; (global-set-key (kbd "C-s-<right>") 'mingus-next)
;; (global-set-key (kbd "C-s-SPC") 'mingus-stop)
;; (add-hook 'mingus-browse-hook '(lambda ()
;;                                  (define-key mingus-browse-map (kbd "C-i") '(lambda () (interactive)
;;                                                                               (mingus-clear t)
;;                                                                               (mingus-insert-and-play)
;;                                                                               (kill-this-buffer)))
;;                                  ))
;; ;; ==============mingus==================
(provide 'setup_emms_mingus)
