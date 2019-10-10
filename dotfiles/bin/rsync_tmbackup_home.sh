#!/bin/bash

ping -c 1 192.168.1.105 &>/dev/null

if [ $? -eq 0 ]; then
    state=$(ssh Nas "sudo -S hdparm -C /dev/sdc" | awk 'END{print $NF}')
    if [ "$state" == "active/idle" ];then
        echo "$state"
        # /home/swint/git-repo/rsync-time-backup/rsync_tmbackup.sh --rsync-get-flags # 默认包含--compress选项。
        rsync_flags="-D --numeric-ids --links --hard-links --one-file-system --itemize-changes --times --recursive --perms --owner --group --stats --human-readable" # --progress
        # /home/swint/git-repo/rsync-time-backup/rsync_tmbackup.sh --rsync-set-flags "$rsync_flags" /home/swint swint@192.168.1.105:/mnt/TOSHIBA-3000/share/T510_backup /home/swint/bin/rsync_tmbackup_home_exclude.txt;
        /home/swint/git-repo/rsync-time-backup/rsync_tmbackup.sh --rsync-set-flags "$rsync_flags" /home/swint swint@192.168.1.105:/mnt/TOSHIBA-3000/share/T510_backup;
    elif [ "$state" == "standby" ]; then
        echo "$state"
    else
        break
    fi
fi
