#!/bin/zsh

Wind_id=`xdotool getactivewindow`;
sleep 0.5

if [ $(xdotool search --onlyvisible --class "URxvt" | grep -i $Wind_id) ]; then
    selectText=$(xclip -selection primary -o)
else
    if [ $(xdotool search --onlyvisible --class "Emacs" | grep -i $Wind_id) ]; then
        xdotool keyup alt+w
        xdotool key --clearmodifiers alt+w
        sleep 1
    elif [ $(xdotool search --onlyvisible --class "qpdfview" | grep -i $Wind_id) ]; then
        echo None
    else
        xdotool keyup ctrl+c
        xdotool key --clearmodifiers ctrl+c
    fi
    selectText=$(xclip -selection clipboard -o)
fi

export transHistory=$HOME/.zenity_trans

selectText=$(echo $selectText | sed 's/[\"]/\\&/g' | tr -d '\n')
# 对待翻译内容进一步编辑
# echo "$selectText" > $transHistory
# selectText=$(zenity --width=800 --height=500 --text-info --title "Enter text:" --editable --filename=$transHistory)
# 使用--entry时，selectText会分成多行
# selectText=$(echo $selectText | sed 's/[\"]/\\&/g' | tr -d '\n' | xargs zenity --entry --text "Enter text:" --entry-text)

if [ $? -eq 0 ]; then
    transmode=sdcv
    transResult=$(sdcv -n -e --utf8-input --utf8-output "$selectText")
    if [[ $transResult == *'Nothing similar to'* ]]; then
        transmode=ydcv
        transResult=$(ydcv "$selectText")
    fi

    if [[ $transResult == *'Translation'* ]]; then
        transResult=$(echo $transResult | sed -n '/Translation:/ { :a; n; p; ba; }' | awk '{$1=$1;print}')
    fi

    echo "$transResult" > $transHistory

    zenity --width=800 --height=500 --text-info --title=$transmode --filename=$transHistory

    # 使用C-RET退出时保存翻译结果到剪贴板
    if [[ $? -eq 0 ]]; then
        echo $transResult | xclip -selection clipboard
    fi
fi
