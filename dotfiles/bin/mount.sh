#!/bin/bash

# remote=$(cat ~/.ssh/config | grep "^Host " | awk '{print $2}' | sed "$ a local" | percol) # 最后一行后加local
remote=$(cat ~/.ssh/config | grep "^Host " | awk '{print $2}' | sed "1 i local" | percol) # 第一行前加local

if [[ $remote == "local" ]]; then
    # percol选择
    mydevice=$(lsblk -nrp | awk '$6==type {print $1,$7}' type="part" | percol)

    # 直接输入
    # lsblk
    # read mydevice

    if [[ $mydevice != "" ]]; then
        pass_sudo=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2=="localhost" {print $NF}')
        echo $pass_sudo | sudo -S mount -o uid=1000,gid=1000 $mydevice /media/iso
        cd /media/iso
        $SHELL
    fi
elif [[ $remote != "" ]]; then
    target=/mnt/sshfs/$remote
    mkdir -p $target
    # mount默认: rw/suid/dev/exec/auto/nouser/async/relatime
    # sshfs -o rw,dirsync,nosuid,nodev,noexec,follow_symlinks $remote:/ $target # EasySSHFS默认选项，但电脑端速度慢
    sshfs -o follow_symlinks $remote:/ $target

    cd $target$HOME # 该脚本运行在子进程下，直接改变目录会在执行结束之后返回父进程，导致切换失败
    $SHELL     # source x.sh或重开一个子进程
fi
