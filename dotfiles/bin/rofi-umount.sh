#! /bin/bash

export SUDO_ASKPASS=$HOME/bin/askpass.sh

LAUNCHER="rofi.sh -width 30 -dmenu -i -p Umount:"
mount_entry=$(df -h | grep '/mnt\|/media/iso' | $LAUNCHER | tr -d '\r\n')
network_style='^.*/mnt/(sshfs|share)/.*$'
device_style='^/dev/.*$'

if [[ $mount_entry != "" ]]; then
    if [[ $mount_entry =~ $network_style ]]; then
        mount_point=$(echo $mount_entry | awk '{print $6}')
        sudo -A umount $mount_point
        notify-send "Umounted:" $mount_point
    elif [[ $mount_entry =~ $device_style ]]; then
        dev_point=$(echo $mount_entry | awk '{print $1}')
        sudo -A udisksctl unmount -b $dev_point
        sudo -A udisksctl power-off -b ${dev_point%?}
        notify-send "Umounted:" $dev_point
    else
        notify-send "Wrong mount entry"
    fi
fi
