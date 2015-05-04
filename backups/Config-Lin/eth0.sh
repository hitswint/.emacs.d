#! /bin/bash
echo "hit"|sudo -S ifconfig eth0 down
sudo ifconfig eth0 up
sudo dhclient eth0
