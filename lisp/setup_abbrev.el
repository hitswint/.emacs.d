;; ====================abbrev====================
(use-package abbrev
  ;; Enabled at idle.
  :defer 2
  :config
  ;; turn on abbrev mode globally
  (setq-default abbrev-mode t)
  ;; stop asking whether to save newly added abbrev when quitting emacs
  (setq save-abbrevs t)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  (setq abbrev-file-name
        "~/.emacs.d/abbrev_defs")
  ;; sample use of emacs abbreviation feature
  (define-abbrev-table 'global-abbrev-table
    '(("emqq" "278064399@qq.com")
      ("emhml" "wguiqiang@hotmail.com")
      ("emgml" "guiqiangw2013@gmail.com")
      ("em126" "wgq_hit@126.com")
      ("em163" "wgq_713@163.com")
      ("wgq" "Guiqiang Wang"))))
;; 编辑abbrev-table：C-x a g 为当前位置之前词语，全局加入abbrev。
;; C-x a + 为当前位置之前词语，在当前mode下加入abbrev。
;; 上述命令前加前缀C-u 3表示当前位置之前三个词。
;; 另define-global-abbrev define-mode-abbrev 可以自定义要abbrev的词。
;; 退出时会要求保存abbrev_defs文件。
;; ====================abbrev====================
(provide 'setup_abbrev)
