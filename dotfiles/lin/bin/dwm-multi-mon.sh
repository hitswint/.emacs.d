#! /bin/bash

dmode="$(cat /sys/class/drm/card0-VGA-1/status)"
export DISPLAY=:0
export XAUTHORITY=$HOME/.Xauthority
function list_devices(){
    xrandr | grep connected | cut -d' ' -f 1
}
function get_device(){
    index=$1
    list_devices | head -n $index | tail -n 1
}
function print_usage {
    printf "Usage: `basename $0` <mode>\nwhere:\n\tmode = (single|dual|left|right)\n";
    exit 0
}
if [ "x$1" == "x-h" ]; then
    printf "Finds the first two displays from xrandr output and sets them according to the MODE parameter.\n\n"
    print_usage
fi
MODE=$1
[ -z $MODE ] && print_usage
PRIMARY=$(get_device 1)
SECONDARY=$(get_device 2)
echo "Primary display device: $PRIMARY"
echo "Secondary display device: $SECONDARY"

if [ "$dmode" == "disconnected" ]; then
    xrandr --auto
elif [ "$dmode" == "connected" ];then
    if [ "$MODE" == "single" ];then
        xrandr --output $SECONDARY --auto --left-of $PRIMARY
        xrandr --output $PRIMARY --off
        # xrandr  --output $PRIMARY --auto
    elif [ "$MODE" == "left" ]; then
        xrandr  --output $SECONDARY --auto --output $PRIMARY --auto  --right-of $SECONDARY;
    elif [ "$MODE" == "right" ]; then
        xrandr  --output $SECONDARY --auto --output $PRIMARY --auto  --left-of $SECONDARY;
    elif [ "$MODE" == "dual" ]; then
        xrandr  --output $SECONDARY --auto --output $PRIMARY --auto  --same-as $SECONDARY;
    else
        print_usage;
    fi
else xrandr --auto
fi

# Display two screens side by side.
# xrandr --output VGA1 --auto --left-of LVDS1
# xrandr --output LVDS1 --auto --right-of VGA1

# Only display one screens.
# xrandr --output VGA1 --auto --left-of LVDS1
# xrandr --output LVDS1 --off

# Display duplicate contents on two screens.
# xrandr --output VGA1 --auto --same-as LVDS1 --mode 1024x768
