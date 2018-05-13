#!/bin/bash

xwin_id=`xdpyinfo | sed -ne 's/^focus:.*\(0x[^,]\+\).*/\1/p'`
if xwininfo -id $xwin_id | grep "(has no name)"
then
    xwin_id=`printf "0x%x\n" $(( $xwin_id - 1 ))` #Decrement by one.
fi

Wind_id=`xdotool getactivewindow`;
sleep 0.5

if [ $(xdotool search --onlyvisible --class "llpp" | grep -i $Wind_id) ]; then
    word=$(xclip -selection primary -o | sed 's/[\"]/\\&/g')
else
    xdotool key --clearmodifiers ctrl+c
    word=$(xclip -selection clipboard -o | sed 's/[\"]/\\&/g')
fi

# run-or-raise.sh emacs

emacsclient -a '' -c -F "((name . \"ec_float\")(top . -1))" -e "(progn (defun swint-copy-to-clipboard (frame) (remove-hook 'delete-frame-functions 'swint-copy-to-clipboard) (write-region (current-kill 0) nil \"/tmp/eaclipboard\") (shell-command \"xclip -selection clipboard /tmp/eaclipboard &> /dev/null\") (kill-this-buffer)) (add-hook 'delete-frame-functions 'swint-copy-to-clipboard) (swint-online-to-buffer (substring-no-properties \"$word\")) (local-set-key (kbd \"q\") 'delete-frame))"

wmctrl -ia $xwin_id
