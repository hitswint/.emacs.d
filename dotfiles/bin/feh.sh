#!/bin/bash

shopt -s nullglob
shopt -s nocaseglob

if [[ ! -f $1 ]]; then
    echo "$0: first argument is not a file" >&2
    exit 1
fi

file=$(basename -- "$1")
dir=$(dirname -- "$1")
arr=()
shift

cd -- "$dir"

for i in *.{png,jpg,jpeg,bmp}; do
    # file_ext=$(echo $file |awk -F . '{if (NF>1) {print $NF}}')
    [[ -f $i ]] || continue
    arr+=("$i")
    [[ $i == $file ]] && c=$((${#arr[@]} - 1))
done

exec feh -Z -. -q "$@" -- "${arr[@]:c}" "${arr[@]:0:c}"
