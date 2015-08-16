;;============语言环境字符集设置结束=============================
(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font))
      nil t))
(defvar font-list '("宋体" "新宋体" "Microsoft Yahei" "文泉驿等宽微米黑" "黑体" ))
(require 'cl) ;; find-if is in common list package
(find-if #'qiang-font-existsp font-list)
(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))
(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-size)
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl)                         ; for find if
  (let ((en-font (qiang-make-font-string
                  (find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
                            :size chinese-font-size)))
    ;; Set the default English font
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (message "Set English Font to %s" en-font)
    (set-face-attribute
     'default nil :font en-font)
    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the english font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        zh-font))))
(qiang-set-font
 '("Consolas" "Monaco" "DejaVu Sans Mono" "Monospace" "Courier New") ":pixelsize=15"
 '("宋体" "新宋体" "Microsoft Yahei" "文泉驿等宽微米黑" "黑体" ))
;;Setting English Font
;;(set-face-attribute  'default nil :font "Consolas 12")
;; Chinese Font
;;(dolist (charset '(kana han symbol cjk-misc bopomofo))  (set-fontset-font (frame-parameter nil 'font)         charset                   (font-spec :family "Microsoft Yahei" :size 12)))
;;处理shell-mode乱码,好像没作用
;;============语言环境字符集设置结束=============================
(provide 'setup_language_env)
