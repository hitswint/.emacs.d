#!/bin/bash

xwin_id=`xdpyinfo | sed -ne 's/^focus:.*\(0x[^,]\+\).*/\1/p'`
if xwininfo -id $xwin_id | grep "(has no name)"
then
    xwin_id=`printf "0x%x\n" $(( $xwin_id - 1 ))` #Decrement by one.
fi
xwin_title=`xwininfo -id $xwin_id | sed -ne 's/xwininfo: .*"\([^"]\+\)"/\1/p'`

Emacs_style='^.*(emacs@|Microsoft Word).*$'

# 使用当前frame。
# run-or-raise.sh emacs
# trap exit_hook EXIT
# exit_hook()
# {
#     wmctrl -ia $xwin_id
#     if [[ $xwin_title =~ $Emacs_style  ]]; then
#         xdotool keyup ctrl+y
#         xdotool key --clearmodifiers ctrl+y
#     else
#         xdotool keyup ctrl+v
#         xdotool key --clearmodifiers ctrl+v
#     fi
# }
# emacsclient -e "(let ((helm-full-frame t)) (save-window-excursion (delete-other-windows) (helm-show-kill-ring)))"

# 使用新建frame。
# emacsclient -a '' -c -F "((name . \"ec_float\")(top . -1))" -e "(let (helm-quit-hook helm-after-action-hook) (dolist (hook '(helm-quit-hook helm-after-action-hook)) (add-hook hook (lambda () (write-region (current-kill 0) nil \"/tmp/eaclipboard\") (shell-command \"xclip -selection clipboard /tmp/eaclipboard &> /dev/null\") (delete-frame)))) (helm-show-kill-ring))"  # 使用(current-kill 0)
# emacsclient -a '' -c -F "((name . \"ec_float\")(top . -1))" -e "(let (helm-quit-hook helm-after-action-hook) (dolist (hook '(helm-quit-hook helm-after-action-hook)) (add-hook hook 'delete-frame)) (helm-show-kill-ring))"  # 在helm中C-c C-k复制进剪贴板并后续粘贴至当前窗口，直接使用RET会粘贴到Emacs中
# emacsclient -a '' -c -F "((name . \"ec_float\")(top . -1))" -e "(let (helm-quit-hook helm-after-action-hook) (dolist (hook '(helm-quit-hook helm-after-action-hook)) (add-hook hook (lambda () (write-region helm-kill-ring-current nil \"/tmp/eaclipboard\") (shell-command \"xclip -selection clipboard /tmp/eaclipboard &> /dev/null\") (delete-frame)))) (helm-show-kill-ring))"  # 使用C-j将helm-kill-ring-current保存至/tmp/eaclipboard
emacsclient -a '' -c -F "((name . \"ec_float\")(top . -1))" -e "(let (helm-quit-hook helm-after-action-hook) (dolist (hook '(helm-quit-hook helm-after-action-hook)) (add-hook hook (lambda () (write-region helm-kill-ring-current nil \"/tmp/eaclipboard\") (shell-command \"xclip -selection clipboard /tmp/eaclipboard &> /dev/null\") (delete-frame)))) (helm-show-copyq))"  # 使用C-j将helm-kill-ring-current保存至/tmp/eaclipboard

wmctrl -ia $xwin_id

if [[ $xwin_title =~ $Emacs_style  ]]; then
    xdotool keyup ctrl+y
    xdotool key --clearmodifiers ctrl+y
else
    xdotool keyup ctrl+v
    xdotool key --clearmodifiers ctrl+v
fi
