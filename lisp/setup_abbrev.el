;;; abbrev
;; ====================abbrev====================
(use-package abbrev
  ;; Enabled at idle.
  :defer 2
  :config
  ;; Turn on abbrev mode globally.
  (setq-default abbrev-mode t)
  ;; Stop asking whether to save newly added abbrev when quitting emacs.
  (setq save-abbrevs t)
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  ;; Sample use of emacs abbreviation feature.
  (define-abbrev-table 'global-abbrev-table
    '(("mqq" "278064399@qq.com")
      ("mgg" "guiqiangw2013@gmail.com")
      ("mhot" "wguiqiang@hotmail.com")
      ("m126" "wgq_hit@126.com")
      ("m163" "wgq_713@163.com")
      ("wgq" "Guiqiang Wang"))))
;; 编辑abbrev-table：C-x a g 为当前位置之前词语，全局加入abbrev。
;; C-x a + 为当前位置之前词语，在当前mode下加入abbrev。
;; 上述命令前加前缀C-u 3表示当前位置之前三个词。
;; 另define-global-abbrev define-mode-abbrev 可以自定义要abbrev的词。
;; 退出时会要求保存abbrev_defs文件。
;; ====================abbrev====================
(provide 'setup_abbrev)
