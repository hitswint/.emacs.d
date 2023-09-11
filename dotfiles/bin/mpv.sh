#!/bin/bash

# mpv.sh [-t] xxx.mp4 $@

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

file_list=$(ls $ls_arg *.{rmvb,rm,mp4,avi,flv,f4v,mpg,mkv,3gp,wmv,mov,dat,asf,mpeg,wma,webm})
index=-1

# 若使用管道向while传递变量：printf '%s\n' "$file_list" | while read line; do
# 则相当于开启子进程，其内部对index变量的修改不会影响全局index
while read line; do
    [[ -f "$line" ]] || continue
    index=$((index+1))
    if [[ "$line" == "$file" ]]; then
        break
    fi
done <<<$(printf '%s\n' "$file_list")

# file_list变量中含有\newline，但使用echo输出后会删除
# echo $file_list | mpv --playlist=- --playlist-start=$index
printf '%s\n' "$file_list" | mpv "$@" --playlist=- --playlist-start=$index
# ls *.{rmvb,rm,mp4,avi,flv,f4v,mpg,mkv,3gp,wmv,mov,dat,asf,mpeg,wma,webm} | mpv --playlist=- --playlist-start=$index
