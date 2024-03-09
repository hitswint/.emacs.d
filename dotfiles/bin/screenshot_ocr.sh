#!/bin/bash

if [ x$1 == x ]; then
    echo "Choose:"
    echo "t) tesseract"
    echo "p) paddle"
    echo "l) latex"
    read input1
else
    input1=$1
fi

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

# tmp_file_base=/tmp/ScreenShot_ocr_$(date '+%Y%m%d_%H%M%S')
tmp_file_base=/tmp/ScreenShot_ocr

# sleep 0.2 ; scrot -s $tmp_file_base.png 2> /dev/null
sleep 0.2 ; import $tmp_file_base.png 2> /dev/null
mogrify -resize 300% $tmp_file_base.png
mogrify -colorspace Gray $tmp_file_base.png

case $input1 in
    t)
        # -l eng+chi_sim
        tesseract $tmp_file_base.png $tmp_file_base -l eng+chi_sim >> /dev/null
        cat $tmp_file_base.txt | xclip -selection clipboard -i
        notify-send "Tesseract-OCR finished!"
        ;;
    p)
        # tesseract识别中文效果不好
        # tesseract $tmp_file_base.png $tmp_file_base -l $1 >> /dev/null
        # cat $tmp_file_base.txt | sed 's/ //g' | xclip -selection clipboard -i # 删除中文字间空格

        # 百度OCR已失效
        # python3 ~/Documents/Python/ocr/ocr_baidu.py -i $tmp_file_base.png | xclip -selection clipboard -i

        # paddleocr
        # 不能以-i形式向ocr_paddle传递参数，因为paddleocr内部也会解析命令行参数，进而弹出错误error: unrecognized arguments: -i
        # zsh -c "source ~/.virtualenvs/ocr/bin/activate; python3 ~/Documents/Python/ocr/ocr_paddle.py -i $tmp_file_base.png && notify-send \"OCR finished!\""
        zsh -c "source ~/.virtualenvs/ocr/bin/activate; python3 ~/Documents/Python/ocr/ocr_paddle.py"
        notify-send "Paddle-OCR finished!"
        ;;
    l)
        # $tmp_file_base中有反斜线，导致语法错误
        # zsh -c "source ~/.virtualenvs/ocr/bin/activate; pix2tex $tmp_file_base.png" | sed -e "s/$tmp_file_base.png: //g" | xclip -selection clipboard -i
        # 定义s后的字符为新的分隔符，在这里“#”代替默认的“/”分隔符
        zsh -c "source ~/.virtualenvs/ocr/bin/activate; pix2tex $tmp_file_base.png" | sed -e "s#$tmp_file_base.png: ##g" | xclip -selection clipboard -i
        notify-send "LaTeX-OCR finished!"
        ;;
    q)
        break
        ;;
esac
