#!/bin/bash

# feh.sh [-t] xxx.png $@

shopt -s nullglob
shopt -s nocaseglob

if [[ ! -f $1 ]]; then
    # echo "$0: first argument is not a file" >&2
    # exit 1
    ls_arg=$1

    # 使命令行参数编号左移，使得$2变为$1，$0仍为命令本身
    shift
fi

file=$(basename -- "$1")
dir=$(dirname -- "$1")
arr=()

shift

cd -- "$dir"

if [ x$ls_arg == x ]; then
    for i in *.{png,jpg,jpeg,bmp,svg}; do
        # file_ext=$(echo $file |awk -F . '{if (NF>1) {print $NF}}')
        [[ -f $i ]] || continue
        arr+=("$i")
        [[ $i == $file ]] && c=$((${#arr[@]} - 1))
    done
else
    # file_list=$(ls $ls_arg *.{png,jpg,jpeg,bmp,svg})
    while read line; do
        [[ -f "$line" ]] || continue
        arr+=("$line")
        [[ $line == $file ]] && c=$((${#arr[@]} - 1))
        # done <<<$(printf '%s\n' "$file_list")
    done <<<$(ls $ls_arg *.{png,jpg,jpeg,bmp,svg})
fi

# 图片打开很快，如果在xdotool执行前关闭feh的话，--sync选项会导致xdotool一直等待
exec feh -Z -. -q "$@" -- "${arr[@]:c}" "${arr[@]:0:c}" & PID=$! ; win_id=$(timeout 10 xdotool search --sync --onlyvisible --pid $PID) && xseticon -id $win_id /usr/share/icons/Adwaita/48x48/mimetypes/image-x-generic.png
# /usr/share/icons/hicolor/scalable/apps/feh.svg

# 上述命令执行完后会直接退出，需根据feh进程判断退出时机
while ps -p $PID > /dev/null; do sleep 0.5; done
