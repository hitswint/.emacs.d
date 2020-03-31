#!/bin/bash

server=Nas
login=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")

ping -c 1 192.168.1.105 &>/dev/null

if [ $? -eq 0 ]; then
    state=$(ssh Nas "sudo -S hdparm -C /dev/sdc" | awk 'END{print $NF}')
    if [ "$state" == "active/idle" ];then
        echo "$state"
        # $HOME/git-repo/rsync-time-backup/rsync_tmbackup.sh --rsync-get-flags # 默认包含--compress选项。
        # rsync_flags="-D --numeric-ids --links --hard-links --one-file-system --itemize-changes --times --recursive --perms --owner --group --stats --human-readable" # --progress
        # $HOME/git-repo/rsync-time-backup/rsync_tmbackup.sh --rsync-set-flags "$rsync_flags" -p $port $HOME $login:/mnt/TOSHIBA-3000/share/${HOSTNAME}_backup $HOME/bin/.rsync_tmbackup_exclude.txt;
        $HOME/git-repo/rsync-time-backup/rsync_tmbackup.sh -p $port $HOME $login:/mnt/TOSHIBA-3000/share/${HOSTNAME}_backup $HOME/bin/.rsync_tmbackup_exclude.txt;
    elif [ "$state" == "standby" ]; then
        echo "$state"
    else
        break
    fi
fi
