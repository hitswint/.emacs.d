;;; emms
;; ================emms==================
(use-package emms
  ;; Enabled at commands.
  :if is-lin
  :defer t
  :bind (("M-s e l" . swint-emms-playlist-mode-go)
         ("M-s e o" . emms-play-file))
  :config
  (defun swint-emms-playlist-mode-go ()
    "swint-playlist."
    (interactive)
    (emms-player-mpd-connect)
    (emms-playlist-mode-go))
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
        emms-playing-time-display-format "") ;关闭emms在mode-line上的显示
;;;; emms-mpd
  ;; ==============emms-mpd================
  (use-package emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory "~/Music")
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  ;; ==============emms-mpd================
  ;; emms-mpd快捷键设置
  (global-set-key (kbd "M-s e c") 'emms-player-mpd-connect)
  (global-set-key (kbd "C-M-SPC") 'emms-pause)
  (global-set-key (kbd "C-M-<up>") 'emms-volume-raise)
  (global-set-key (kbd "C-M-<down>") 'emms-volume-lower)
  (global-set-key (kbd "C-M-<left>") 'emms-seek-backward)
  (global-set-key (kbd "C-M-<right>") 'emms-seek-forward)
  (global-set-key (kbd "C-s-<left>") 'emms-previous)
  (global-set-key (kbd "C-s-<right>") 'emms-next)
  (global-set-key (kbd "C-s-SPC") 'emms-stop))
;; ================emms==================
;;; mingus
;; ===============mingus=================
(use-package mingus
  ;; Enabled at commands.
  :if is-win
  :defer t
  :bind (("M-s e l" . mingus)
         ("M-s e o" . swint-mingus-browse))
  :config
  (defun swint-mingus-browse ()
    "swint-mingus-browse."
    (interactive)
    (mingus)
    (mingus-browse))
  (autoload 'mingus "mingus-stays-home" nil t)
  (global-set-key (kbd "C-M-SPC") 'mingus-toggle)
  (global-set-key (kbd "C-M-<up>") 'mingus-vol-up)
  (global-set-key (kbd "C-M-<down>") 'mingus-vol-down)
  (global-set-key (kbd "C-M-<left>") 'mingus-seek-backward)
  (global-set-key (kbd "C-M-<right>") 'mingus-seek)
  (global-set-key (kbd "C-s-<left>") 'mingus-prev)
  (global-set-key (kbd "C-s-<right>") 'mingus-next)
  (global-set-key (kbd "C-s-SPC") 'mingus-stop)
  (add-hook 'mingus-browse-hook '(lambda ()
                                   (define-key mingus-browse-map (kbd "C-j") '(lambda () (interactive)
                                                                                (mingus-clear t)
                                                                                (mingus-insert-and-play)
                                                                                (dirtree-kill-this-buffer))))))
;; 在win上使用emms提示找不到service:6600，似乎找不到mpd后台。这个问题似乎是因为emms-player-mpd-ensure-process需要接受一个整数作为port number，但是接受到了一个字符串"6600"，按下述网址https://lists.gnu.org/archive/html/emms-help/2013-08/msg00002.html修改emms-player-mpd.el能够解决这个问题。但播放音乐时出现musicpd error {add}  not found的错误，似乎是mpd数据库问题。
;; ===============mingus=================
(provide 'setup_emms_mingus)
