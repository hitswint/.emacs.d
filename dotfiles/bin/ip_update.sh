#!/bin/bash

server="raspberrypi"
Last_IP=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server" | sed 's/^.*@//g')
Curr_IP=`ssh AliyunECS "cat ~/.ip.pub"`

echo "Last IP: $Last_IP"
echo "Curr IP: $Curr_IP"

if [[ (x$Curr_IP != x) && ($Last_IP != $Curr_IP) ]]; then
    emacsclient -e "(with-current-buffer (find-file \"~/.authinfo.gpg\") (replace-string \"$Last_IP\" \"$Curr_IP\" nil (point-min) (point-max)) (save-buffer))"
    # sed -e "s/$Last_IP/$Curr_IP/g" -i ./.authinfo.gpg;
    sed -e "s/$Last_IP/$Curr_IP/g" -i ~/.ssh/config;
    echo "IP changed.";
fi
