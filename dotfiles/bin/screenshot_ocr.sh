#!/bin/bash

DEPENDENCIES=(
    scrot
    mogrify
    tesseract
)

for DEPENDENCY in "${DEPENDENCIES[@]}"
do
    if ! [ -x "$(command -v $DEPENDENCY)" ]; then
        echo "This script depends on ${bold}$DEPENDENCY${normal}. It is not available."
        exit 1
    fi
done

tmp_file_base=/tmp/ScreenShot_ocr_$(date '+%Y%m%d_%H%M%S')

# sleep 0.2 ; scrot -s $tmp_file_base.png 2> /dev/null
sleep 0.2 ; import $tmp_file_base.png 2> /dev/null
mogrify -resize 300% $tmp_file_base.png
mogrify -colorspace Gray $tmp_file_base.png

if [ $1 == "chi_sim" ]; then
    # tesseract识别中文效果不好
    # tesseract $tmp_file_base.png $tmp_file_base -l $1 >> /dev/null
    # cat $tmp_file_base.txt | sed 's/ //g' | xclip -selection clipboard -i # 删除中文字间空格

    # 百度OCR已失效
    # python3 ~/Documents/Python/ocr/ocr_baidu.py -i $tmp_file_base.png | xclip -selection clipboard -i

    # paddleocr
    zsh -is eval "source ~/.virtualenvs/ocr/bin/activate; python3 ~/Documents/Python/ocr/ocr_paddle.py -i $tmp_file_base.png && notify-send \"OCR finished!\""
else
    # -l eng+chi_sim
    tesseract $tmp_file_base.png $tmp_file_base -l $1 >> /dev/null
    cat $tmp_file_base.txt | xclip -selection clipboard -i
fi
