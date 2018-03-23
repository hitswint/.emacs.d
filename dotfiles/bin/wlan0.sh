#! /bin/bash

pass_sudo=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2=="localhost" {print $NF}')
echo $pass_sudo|sudo -S ifconfig wlan0 down
sudo ifconfig wlan0 up
sudo iwconfig wlan0 essid LIANGWENFENG
sudo dhclient wlan0
