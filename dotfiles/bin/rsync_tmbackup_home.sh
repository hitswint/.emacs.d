#!/bin/bash

server=N5095
login=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")
orig=$HOME
dest=/mnt/TOSHIBA/share/${HOSTNAME}_backup

ping -c 1 192.168.1.101 &>/dev/null

bin_path=$HOME/bin

if [ $? -eq 0 ]; then
    state=$(ssh $server "sudo -S hdparm -C /dev/sda" | awk 'END{print $NF}')
    if [ "$state" == "active/idle" ]; then
        echo "$state"
        # $bin_path/repos/rsync-time-backup/rsync_tmbackup.sh --rsync-get-flags # 默认包含--compress选项。
        # rsync_flags="-D --numeric-ids --links --hard-links --one-file-system --itemize-changes --times --recursive --perms --owner --group --stats --human-readable" # --progress
        # $bin_path/repos/rsync-time-backup/rsync_tmbackup.sh --rsync-set-flags "$rsync_flags" -p $port $orig $login:$dest $bin_path/.rsync_tmbackup_exclude.txt;
        $bin_path/repos/rsync-time-backup/rsync_tmbackup.sh -p $port $orig $login:$dest $bin_path/$HOSTNAME/.rsync_tmbackup_exclude;
    elif [ "$state" == "standby" ]; then
        echo "$state"
    else
        break
    fi
fi
