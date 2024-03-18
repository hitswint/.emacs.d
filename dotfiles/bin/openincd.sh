#!/bin/zsh

Wind_id=`xdotool getactivewindow`;
sleep 0.2

curr_dir="`xcwd`"

if [[ $(xdotool search --onlyvisible --class "Emacs" | grep -i $Wind_id) ]]; then
    curr_dir=$(emacsclient --eval "(openincd-directory)")
elif [[ $(xdotool search --onlyvisible --class "qpdfview" | grep -i $Wind_id) ]]; then
    config_file=$HOME/.config/qpdfview/qpdfview.conf
    data_file=$HOME/.local/share/qpdfview/qpdfview/database
    curr_tab=$(cat $config_file | grep currentTabIndex | sed 's/currentTabIndex=//g')
    curr_file=$(sqlite3 $data_file "select filePath from tabs_v5" | awk 'NR==curr_tab' curr_tab=`expr $curr_tab + 1`)
    curr_dir=$(dirname -- "$curr_file")
elif [[ $(xdotool search --onlyvisible --class "Wps|Et|Wpp|Wpspdf|Wpsoffice" | grep -i $Wind_id) || $curr_dir == $HOME ]]; then
    PID=$(xdotool getactivewindow getwindowpid)
    curr_file=$(ls /proc/$PID/fd/* | xargs -L 1 readlink | grep -E "$HOME|/mnt" | grep -vE "$HOME/(.local|.config)" | grep -v "$HOME/\.\+.*" | rofi -no-custom -auto-select -dmenu -i -p "Open:")
    if [[ $? -eq 1 ]]; then     # C-g取消时返回1，否则返回0
        exit
    fi
    if [[ $curr_file ]]; then
        curr_dir=$(dirname -- "$curr_file")
    fi
fi

# eval $1 "`xcwd`"
eval $1 $curr_dir
