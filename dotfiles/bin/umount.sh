#! /bin/bash

pass_sudo=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2=="localhost" {print $NF}')
mount_entry=$(df -h | grep '/mnt\|/media/iso' | percol)
network_style='^.*/mnt/(sshfs|share)/.*$'
device_style='^/dev/.*$'

if [[ $mount_entry != "" ]]; then
    if [[ $mount_entry =~ $network_style ]]; then
        mount_point=$(echo $mount_entry | awk '{print $6}')
        echo $pass_sudo | sudo -S umount $mount_point
    elif [[ $mount_entry =~ $device_style ]]; then
        dev_point=$(echo $mount_entry | awk '{print $1}')
        echo $pass_sudo | sudo -S udisksctl unmount -b $dev_point
        echo $pass_sudo | sudo -S udisksctl power-off -b ${dev_point%?}
    else
        echo "Wrong mount entry"
    fi
fi
