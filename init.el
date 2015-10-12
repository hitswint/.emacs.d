;; ===================OS====================
(setq is-mac (equal system-type 'darwin))
(setq is-win (equal system-type 'windows-nt))
(setq is-lin (equal system-type 'gnu/linux))
;; ===================PATH==================
(when is-win
  (setenv "HOME" "c:/Users/swint")
  ;; (setenv "PATH" "c:/Users/swint/")
  ;; 必须注销这行，因为会让latex失效。
  ;; set the default file path
  (setq default-directory "~/")
  ;; 使emacs可以使用win键
  (setq w32-lwindow-modifier 'super)
  (run-with-idle-timer 0.0 nil 'w32-send-sys-command 61488))
;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(setq lisp-dir
      (expand-file-name "lisp" user-emacs-directory))
(setq defuns-dir
      (expand-file-name "defuns" user-emacs-directory))
(setq user-settings-dir                 ;Settings for currently logged in user
      (concat user-emacs-directory "users/" user-login-name))
;; Set up load path
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path defuns-dir)
(add-to-list 'load-path user-settings-dir)
;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))
(add-to-list 'load-path (concat site-lisp-dir "/org/lisp")) ;org-mode的路径设置
;; Functions (load all files in defuns-dir)
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))
;; =================DEFAULT===================
(setq default-major-mode 'text-mode)    ;一打开就起用 text 模式。
(global-font-lock-mode t)               ;语法高亮
(auto-image-file-mode t)                ;打开图片显示功能
(fset 'yes-or-no-p 'y-or-n-p)           ;以 y/n代表 yes/no
(global-linum-mode 0)
(show-paren-mode t)                     ;显示括号匹配
(tool-bar-mode 0)                       ;去掉那个大大的工具栏
(menu-bar-mode 0)                       ;去掉菜单栏
(scroll-bar-mode 0)                     ;去掉滚动条
(mouse-avoidance-mode 'animate)         ;光标靠近鼠标指针时，让鼠标指针自动让开
(require 'hl-line)                      ;光标所在行高亮
(global-hl-line-mode t)
(transient-mark-mode t)                 ;高亮选中得区域
(setq x-select-enable-clipboard t)      ;支持emacs和外部程序的粘贴
(setq frame-title-format "Emacs@ %b")   ;在标题栏提示你目前在什么位置。
(setq default-fill-column 80)           ;默认显示 80列就换行
(setq inhibit-startup-message t)        ;禁用启动信息
(setq visible-bell t)                   ;关闭烦人的出错时的提示声。
(setq mouse-yank-at-point t)            ;支持中键粘贴
(setq kill-ring-max 200)                ;用一个很大的 kill ring
(delete-selection-mode t)
(setq diary-file "~/org/journal.org.gpg") ;设置日记文件为加密文件
;; =================SAVE===================
;; (when is-win
;;   (require 'setup_language_env))
;; (eval-after-load 'ido '(require 'setup-ido))
(require 'setup_desktop_session)
;; =================CUSTOM=================
(cond
 (is-lin (setq custom-file (expand-file-name "custom-lin.el" user-emacs-directory)))
 (is-win (setq custom-file (expand-file-name "custom-win.el" user-emacs-directory))))
(load custom-file)
;; =================SETUP==================
(require 'setup_elpa)
(require 'setup_backup_autosave)
(require 'setup_keybindings)
(require 'setup_abbrev)
(require 'setup_ace_jump)
(require 'setup_appearance)
(require 'setup_ccmode)
(require 'setup_chinese_fonts_setup)
(require 'setup_dired)
(require 'setup_dirtree)
(require 'setup_emms_mingus)
(require 'setup_flycheck)
(require 'setup_flyspell)
(require 'setup_gnuplot)
(require 'setup_graphviz)
(require 'setup_hippie_expand)
(require 'setup_ibuffer)
(require 'setup_ido)
(require 'setup_isearch)
(require 'setup_latex)
(require 'setup_magit)
(require 'setup_matlab_octave)
(require 'setup_mew)
(require 'setup_minibuffer)
(require 'setup_mode_line)
(require 'setup_org_mode)
(require 'setup_helm)
(require 'setup_projectile)
(require 'setup_paredit)
(require 'setup_parenthesis)
(require 'setup_perspective)
(require 'setup_recentf)
(require 'setup_smex)
(require 'setup_stardict)
(require 'setup_w3m)
(require 'setup_windows)
(require 'setup_rainbow_delimiters)
(require 'setup_auto_complete)
(require 'setup_eshell)
(require 'setup_yasnippet)
(require 'setup_packages)
(when is-lin
  (require 'setup_wicd))
;; =================END====================
