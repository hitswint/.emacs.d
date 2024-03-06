#!/bin/bash

export SUDO_ASKPASS=$HOME/bin/askpass.sh

LAUNCHER="rofi.sh -width 30 -dmenu -i -p Mount:"
REMOTES=$(cat ~/.ssh/config | grep "^Host " | awk '{print $2}' | sed "1 i local" | xargs | sed -e 's/ /\\n/g')
remote=`echo -e $REMOTES | $LAUNCHER | tr -d '\r\n'`

if [[ $remote == "local" ]]; then
    # percol选择
    mydevice=$(lsblk -nrp | awk '$6==type {print $1,$7}' type="part" | $LAUNCHER | tr -d '\r\n')

    # 直接输入
    # lsblk
    # read mydevice

    if [[ $mydevice != "" ]]; then
        sudo -A mount -o uid=1000,gid=1000 $mydevice /media/iso
        thunar /media/iso
    fi
elif [[ $remote != "" ]]; then
    target=/mnt/sshfs/$remote
    mkdir -p $target
    # mount默认: rw/suid/dev/exec/auto/nouser/async/relatime
    # sshfs -o rw,dirsync,nosuid,nodev,noexec,follow_symlinks $remote:/ $target # EasySSHFS默认选项，但电脑端速度慢
    sshfs -o follow_symlinks $remote:/ $target
    thunar $target$HOME
fi
