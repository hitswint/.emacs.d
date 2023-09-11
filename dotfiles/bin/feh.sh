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
    for i in *.{png,jpg,jpeg,bmp}; do
        # file_ext=$(echo $file |awk -F . '{if (NF>1) {print $NF}}')
        [[ -f $i ]] || continue
        arr+=("$i")
        [[ $i == $file ]] && c=$((${#arr[@]} - 1))
    done
else
    # file_list=$(ls $ls_arg *.{png,jpg,jpeg,bmp})
    while read line; do
        [[ -f "$line" ]] || continue
        arr+=("$line")
        [[ $line == $file ]] && c=$((${#arr[@]} - 1))
        # done <<<$(printf '%s\n' "$file_list")
    done <<<$(ls $ls_arg *.{png,jpg,jpeg,bmp})
fi

exec feh -Z -. -q "$@" -- "${arr[@]:c}" "${arr[@]:0:c}"
