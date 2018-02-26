;;; emms
;; ================emms==================
(use-package emms
  :if is-lin
  :bind (("M-s e l" . emms-playlist-mode-go)
         ("M-s e o" . emms-play-file))
  :config
  (advice-add 'emms-playlist-mode-go :before #'emms-player-mpd-connect)
  (advice-add 'emms-play-file :before #'(lambda (file) (emms-player-mpd-update-all)))
  (use-package emms-setup)
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
        emms-lyrics-display-on-modeline nil
        emms-playing-time-display-format "")
;;;; emms-mpd
  ;; ==============emms-mpd================
  (use-package emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory "~/Music")
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  ;; ==============emms-mpd================
  ;; Emms-mpd快捷键设置。
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
;;; mingus
;; ===============mingus=================
(use-package mingus
  :if is-win
  :bind (("M-s e l" . mingus)
         ("M-s e o" . mingus-browse))
  :config
  (autoload 'mingus "mingus-stays-home" nil t)
  (setq mingus-mode-always-modeline nil
        mingus-mode-line-show-consume-and-single-status nil
        mingus-mode-line-show-elapsed-percentage nil
        mingus-mode-line-show-elapsed-time nil
        mingus-mode-line-show-random-and-repeat-status nil
        mingus-mode-line-show-status nil
        mingus-mode-line-show-volume nil)
  (global-set-key (kbd "s-/") 'mingus-toggle)
  (global-set-key (kbd "s-?") 'mingus-stop)
  (global-set-key (kbd "s-,") 'mingus-seek-backward)
  (global-set-key (kbd "s-.") 'mingus-seek)
  (global-set-key (kbd "s-<") 'mingus-prev)
  (global-set-key (kbd "s->") 'mingus-next)
  (global-set-key (kbd "C-s-,") 'mingus-vol-down)
  (global-set-key (kbd "C-s-.") 'mingus-vol-up)
  (add-hook 'mingus-browse-hook '(lambda ()
                                   (define-key mingus-browse-map (kbd "C-j") '(lambda () (interactive)
                                                                                (mingus-clear t)
                                                                                (mingus-insert-and-play)
                                                                                (swint-kill-this-buffer))))))
;; 在win上使用emms提示找不到service:6600，似乎找不到mpd后台。这个问题似乎是因为emms-player-mpd-ensure-process需要接受一个整数作为port number，但是接受到了一个字符串"6600"，按下述网址https://lists.gnu.org/archive/html/emms-help/2013-08/msg00002.html修改emms-player-mpd.el能够解决这个问题。但播放音乐时出现musicpd error {add}  not found的错误，似乎是mpd数据库问题。
;; ===============mingus=================
(provide 'setup_emms_mingus)
