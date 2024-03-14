#!/bin/zsh

Wind_id=`xdotool getactivewindow`;
sleep 0.2

if [ $(xdotool search --onlyvisible --class "Emacs" | grep -i $Wind_id) ]; then
    curr_dir=$(emacsclient --eval "(openincd-directory)")
    eval $1 $curr_dir
elif [ $(xdotool search --onlyvisible --class "qpdfview" | grep -i $Wind_id) ]; then
    config_file=$HOME/.config/qpdfview/qpdfview.conf
    data_file=$HOME/.local/share/qpdfview/qpdfview/database
    curr_tab=$(cat $config_file | grep currentTabIndex | sed 's/currentTabIndex=//g')
    curr_file=$(sqlite3 $data_file "select filePath from tabs_v5" | awk 'NR==curr_tab' curr_tab=`expr $curr_tab + 1`)
    curr_dir=$(dirname -- "$curr_file")
    eval $1 $curr_dir
else
    eval $1 "`xcwd`"
fi
