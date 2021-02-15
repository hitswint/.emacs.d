#!/bin/bash

remote=$(cat ~/.ssh/config | grep "^Host " | awk '{print $2}' | sed "$ a local" | percol)

if [[ $remote == "local" ]]; then
    lsblk
    read mydevice
    pass_sudo=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2=="localhost" {print $NF}')
    echo $pass_sudo | sudo -S mount -o uid=1000,gid=1000 /dev/$mydevice /media/iso
    cd /media/iso
    $SHELL
elif [[ $remote != "" ]];then
    target=/mnt/sshfs/$remote
    mkdir -p $target
    sshfs $remote:/ $target

    cd $target$HOME # 该脚本运行在子进程下，直接改变目录会在执行结束之后返回父进程，导致切换失败
    $SHELL     # source x.sh或重开一个子进程
fi
