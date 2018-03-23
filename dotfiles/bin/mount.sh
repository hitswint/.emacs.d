#! /bin/bash

pass_sudo=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2=="localhost" {print $NF}')
echo $pass_sudo|sudo -S fdisk -l
read mydevice
echo $pass_sudo|sudo -S mount /dev/$mydevice /media/iso
