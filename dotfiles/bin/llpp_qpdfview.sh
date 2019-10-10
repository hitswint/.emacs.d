#! /bin/bash

Windx_llpp=`xdotool search --onlyvisible --classname llpp | head -1`
Windx_qpdfview=`xdotool search --onlyvisible --classname qpdfview | head -1`
# llpp_pid_open=`pgrep llpp_qpdfview | head -1` # 最新实例
llpp_pid_list=`pgrep llpp_qpdfview` # llpp_qpdfview实例pid列表

# 统计llpp进程数量，当多于2个时，使用qpdfview。
llpp_num=`pgrep -x llpp | wc -l`

declare -a llpp_file_list

if [ $Windx_qpdfview ]; then
    qpdfview --unique "$1";
elif [[ $Windx_llpp && ($llpp_num -ge 2) ]]; then # 当llpp实例数量>=2时
    for i in $llpp_pid_list; do
        if [[ $i != $$ ]]; then # 当非当前进程时
            llpp_curr=$(cat /proc/$i/cmdline | sed -e 's/.*\x00\([^\x00].*\)$/\1/')
            pkill -P $i         # kill process and all subprocess
            llpp_file_list+=( "${llpp_curr}" )
        fi
    done
    qpdfview --unique "${llpp_file_list[@]}" "$1"
else
    llpp "$1";
fi
