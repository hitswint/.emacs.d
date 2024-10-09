#!/bin/bash

xwin_id=`xdpyinfo | sed -ne 's/^focus:.*\(0x[^,]\+\).*/\1/p'`
if xwininfo -id $xwin_id | grep "(has no name)"
then
    xwin_id=`printf "0x%x\n" $(( $xwin_id - 1 ))` #Decrement by one.
fi

Wind_id=`xdotool getactivewindow`;
sleep 0.5

if [ $(xdotool search --onlyvisible --class "URxvt" | grep -i $Wind_id) ]; then
    word=$(xclip -selection primary -o | sed 's/[\"]/\\&/g')
else
    xdotool keyup ctrl+c
    xdotool key --clearmodifiers ctrl+c
    word=$(xclip -selection clipboard -o | sed 's/[\"]/\\&/g')
fi

# run-or-raise.sh emacs

transResult_sdcv=$(sdcv -n -e --utf8-input --utf8-output "$word")

if [[ $transResult_sdcv == *'Nothing similar to'* ]]; then
    emacsclient -a '' -c -F "((name . \"ec_float\")(top . -1))" -e "(progn (defun swint-copy-to-clipboard (frame) (remove-hook 'delete-frame-functions 'swint-copy-to-clipboard) (write-region (current-kill 0) nil \"/tmp/eaclipboard\") (shell-command \"xclip -selection clipboard /tmp/eaclipboard &> /dev/null\") (kill-this-buffer)) (add-hook 'delete-frame-functions 'swint-copy-to-clipboard) (swint-online-to-buffer (substring-no-properties \"$word\")) (use-local-map (copy-keymap org-mode-map)) (local-set-key (kbd \"q\") 'delete-frame))"
else
    emacsclient -a '' -c -F "((name . \"ec_float\")(top . -1))" -e "(progn (defun swint-copy-to-clipboard (frame) (remove-hook 'delete-frame-functions 'swint-copy-to-clipboard) (write-region (current-kill 0) nil \"/tmp/eaclipboard\") (shell-command \"xclip -selection clipboard /tmp/eaclipboard &> /dev/null\") (kill-this-buffer)) (add-hook 'delete-frame-functions 'swint-copy-to-clipboard) (swint-sdcv-to-buffer (substring-no-properties \"$word\")) (use-local-map (copy-keymap org-mode-map)) (local-set-key (kbd \"q\") 'delete-frame))"
fi

# wmctrl -ia $xwin_id
