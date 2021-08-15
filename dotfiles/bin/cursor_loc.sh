#!/bin/zsh


# cnee --record --mouse --events-to-record 1 | awk  '/7,4,0,0,1/ { system("xdotool getmouselocation") }'
cnee --record --mouse | # --mouse后加&>/dev/null可屏蔽cnee的输出，但导致命令无法立即返回，需再移动一下鼠标
    while read line; do
        # 7,4,0,0,1为鼠标左键按下，7,5,0,0,1为鼠标左键抬起
        # 7,4,0,0,3为鼠标右键按下，7,5,0,0,3为鼠标右键抬起
        if [ ! -z "$(echo "$line" | awk  '/7,4,0,0,1/')"  ]; then
            win_id=$(xdotool getactivewindow)
            break
        elif [ ! -z "$(echo "$line" | awk  '/7,4,0,0,3/')" ]; then
            break
        fi
    done

if [[ ! -z $win_id ]]; then
    win_pos_x=$(xwininfo -id $win_id | grep "Relative upper-left X" | awk '{print $NF}')
    win_pos_y=$(xwininfo -id $win_id | grep "Relative upper-left Y" | awk '{print $NF}')
    win_width=$(xwininfo -id $win_id | grep "Width" | awk '{print $NF}')
    win_height=$(xwininfo -id $win_id | grep "Height" | awk '{print $NF}')

    # 使用-geometry选项获得widthxheight+X+Y，但宽高数据不对
    # win_info_geo=$(xwininfo -id $(xdotool getactivewindow) | grep "\-geometry" | awk '{print $NF}')
    # 以+分割为数组(widthxheight, X, Y)
    # win_info_arr=(${(s:+:)win_info_geo})
    # win_pos_x=${win_info_arr[2]}
    # win_pos_y=${win_info_arr[3]}
    # 以x分割为数组(width, height)
    # win_size_arr=(${(s:x:)win_info_arr[1]})
    # win_width=${win_size_arr[1]}
    # win_height=${win_size_arr[2]}

    mouse_pos=$(xdotool getmouselocation)
    mouse_pos_y=$(echo $mouse_pos | awk '{$2=substr($2,3,length($2)); print $2}')
    mouse_pos_x=$(echo $mouse_pos | awk '{$1=substr($1,3,length($1)); print $1}')

    percent_x=$(printf %.2f $((($mouse_pos_x - $win_pos_x + 0.0) / $win_width)))
    percent_y=$(printf %.2f $((1 - ($mouse_pos_y - $win_pos_y + 0.0) / $win_height)))

    echo $percent_x, $percent_y
else
    echo 0.0, 0.0
fi
