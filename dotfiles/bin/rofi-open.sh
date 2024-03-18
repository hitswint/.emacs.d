#!/bin/zsh

win_id=`xdotool getactivewindow`;
sleep 0.2

if [[ $(xdotool search --onlyvisible --class "Emacs" | grep -i $win_id) ]]; then
    curr_file=$(emacsclient --eval "(openincd-file)" | tr -d '"')
elif [[ $(xdotool search --onlyvisible --class "qpdfview" | grep -i $win_id) ]]; then
    config_file=$HOME/.config/qpdfview/qpdfview.conf
    data_file=$HOME/.local/share/qpdfview/qpdfview/database
    curr_tab=$(cat $config_file | grep currentTabIndex | sed 's/currentTabIndex=//g')
    curr_file=$(sqlite3 $data_file "select filePath from tabs_v5" | awk 'NR==curr_tab' curr_tab=`expr $curr_tab + 1`)
else
    # PID=$(xprop -id `xdotool getwindowfocus` | grep '_NET_WM_PID' | grep -oE '[[:digit:]]*$')
    PID=$(xdotool getactivewindow getwindowpid)
    # curr_file=$(lsof -u $USER -a -b -p $PID | grep REG | grep -E "$HOME|/mnt" | grep -vE "$HOME/(.local|.config)")
    curr_file=$(ls /proc/$PID/fd/* | xargs -L 1 readlink | grep -E "$HOME|/mnt" | grep -vE "$HOME/(.local|.config)" | grep -v "$HOME/\.\+.*" | rofi -no-custom -auto-select -dmenu -i -p "Open:")
    if [[ $? -eq 1 ]]; then     # C-g取消时返回1，否则返回0
        exit
    fi
fi

if [[ $1 == "other" ]]; then
    [[ $curr_file ]] && curr_dir=$(dirname -- "$curr_file") || curr_dir=$(xcwd)
    curr_file=$(rofi -modi filebrowser -show filebrowser -filebrowser-directory "$curr_dir" -run-command "echo {cmd}" | sed 's/xdg-open //g')
fi

if [[ $curr_file ]]; then
    select_command=$(rofi -show run -run-command "echo {cmd}")
    if [[ $select_command ]]; then
        eval $select_command \"$curr_file\"
    fi
fi
