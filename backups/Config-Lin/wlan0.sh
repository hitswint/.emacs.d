#! /bin/bash
echo "hit"|sudo -S ifconfig wlan0 down
sudo ifconfig wlan0 up
sudo iwconfig wlan0 essid LIANGWENFENG
sudo dhclient wlan0
