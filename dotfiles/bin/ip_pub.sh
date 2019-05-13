#!/bin/bash

Wan_IP=`wget http://ipecho.net/plain -O - -q; echo`
# Wan_IP=`curl ipecho.net/plain; echo`
# Wan_IP=`curl icanhazip.com`
# Wan_IP=`curl ifconfig.me`

Last_IP=`ssh ali "cat ~/.ip.pub"`

if [[ (x$Last_IP == x) || ($Last_IP != $Wan_IP) ]]; then
    # echo "IP changed."
    echo $Wan_IP | ssh ali "cat > ~/.ip.pub"
fi
