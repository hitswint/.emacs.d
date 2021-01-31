#! /bin/bash

pass_sudo=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2=="localhost" {print $NF}')
mount_point=$(df -h | awk '{print $6}' | grep '/mnt/sshfs\|/mnt/share\|/media/iso' | percol)

if [[ $mount_point != "" ]]; then
    echo $pass_sudo | sudo -S umount $mount_point
fi
