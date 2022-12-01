#!/bin/bash

Wind_id=`xdotool getactivewindow`;
sleep 0.5

if [ $(xdotool search --onlyvisible --class "URxvt" | grep -i $Wind_id) ]; then
    selectText=$(xclip -selection primary -o | sed 's/[\"]/\\&/g')
elif [ $(xdotool search --onlyvisible --class "Emacs" | grep -i $Wind_id) ]; then
    xdotool keyup alt+w
    xdotool key --clearmodifiers alt+w
    selectText=$(xclip -selection clipboard -o | sed 's/[\"]/\\&/g')
else
    xdotool keyup ctrl+c
    xdotool key --clearmodifiers ctrl+c
    selectText=$(xclip -selection clipboard -o | sed 's/[\"]/\\&/g')
fi

# * dict.sh
# https://gist.github.com/Amooti73/9dac66ffee26f93baf211ab8c05949cd
# LAUNCHER="rofi.sh -width 80 -l 32 -dmenu -i -p Word:"

# pag() {
#     # sed -e 1d -e 's; _\([A-Z]\); \1;p' -e '/^$/d' -e '/^-->/d' | eval "$LAUNCHER" -l 20 -p 'Done'
#     eval "$LAUNCHER" -p 'Done'
# }

# {
#     # sdcv
#     # translation=$(sdcv -n --utf8-input --utf8-output "$word")
#     # translation=$(emacsclient --eval "(pos-tip-fill-string (sdcv-search-with-dictionary \"$word\" sdcv-dictionary-list) 160)")
#     # translation=$(emacsclient --eval "(progn (swint-sdcv-to-buffer \"$word\") (pos-tip-fill-string (buffer-substring-no-properties (point-min) (point-max)) 160))")

#     # bing/youdao/baidu
#     translation=$(ydcv "$word")
#     # translation=$(emacsclient --eval "(let ((bing-result (ignore-errors (bing-dict-brief \"$word\" t))) (youdao-result (ignore-errors (youdao-dictionary--format-result (youdao-dictionary--request \"$word\")))) (baidu-result (ignore-errors (baidu-translate-at-point \"$word\") (buffer-substring-no-properties (point-min) (point-max))))) (pos-tip-fill-string (substring-no-properties (concat \"------------------------------Bing Dict------------------------------\n\" (or bing-result \"Nothing\") \"\n------------------------------Youdao Dictionary------------------------------\n\" (or youdao-result \"Nothing\") \"\n------------------------------Baidu Translate------------------------------\n\" (or baidu-result \"Nothing\"))) 160))")
#     # translation=$(emacsclient --eval "(progn (swint-online-to-buffer \"$word\") (pos-tip-fill-string (buffer-substring-no-properties (point-min) (point-max)) 160))")

#     printf "$translation"
# } | pag

# * rofi-translate
export transHistory_sdcv=$HOME/.rofi_trans_sdcv
export transHistory_ydcv=$HOME/.rofi_trans_ydcv

if [[ $1 == "sdcv" ]]; then
    transResult=$(sdcv -n --utf8-input --utf8-output "$selectText")
    cat /dev/null > $transHistory_sdcv
    echo "$transResult" > $transHistory_sdcv
elif [[ $1 == "ydcv" ]]; then
    transResult=$(ydcv "$selectText")
    cat /dev/null > $transHistory_ydcv
    echo "$transResult" > $transHistory_ydcv
fi

# https://github.com/garyparrot/rofi-translate
rofi.sh -width 80 -l 32 -sidebar-mode -modi "sdcv:rofi_trans_sdcv,ydcv:rofi_trans_ydcv" -show $1

# ** 直接复制选择项
# *** 使用kb-secondary-copy
# -kb-secondary-copy Control-c wiki上有直接复制所选项的命令，但实际没有
# https://github.com/davatorium/rofi/blob/next/doc/rofi-keys.5.markdown
# *** 使用kb-custom-1
# Each of the custom key commands in rofi related to corresponding exit code.
# (kb-custom-1 to kb-custom-19) -> (10 to 28)
# If you issue 'kb-custom-1' command, rofi will return the output of the selected row(s) and the exit code 10 which corresponds to custom command 1.
# 似乎跟dwm冲突，kb-custom-1定义无效
# content=$(rofi.sh -width 80 -l 32 -sidebar-mode -kb-custom-1 Control-c -modi "sdcv:rofi_trans_sdcv $2,ydcv:rofi_trans_ydcv $2" -show "$default")
# exit_code=$?
# case $exit_code in
#     10) echo $content | xclip -selection clipboard ;;
#     # 11) xdotool key "shift+Insert" ;;
#     *) exit $exit_code ;;
# esac
