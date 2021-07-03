#!/bin/bash

# unar解压缩docx文件，图片文件位于word/media/文件夹下
shopt -s nullglob

for image in *.{emf,wmf}; do
    file_name=$(basename "$image" | sed 's/\(.*\)\..*/\1/')
    unoconv -f pdf -o "$file_name".pdf "$image"
    convert -density 300 -trim -bordercolor white -border 5 "$file_name".pdf "$file_name".png
    rm "$file_name".pdf
done
