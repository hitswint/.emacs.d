#! /bin/bash
echo "hit"|sudo -S fdisk -l
read mydevice
echo "hit"|sudo -S mount /dev/$mydevice /media/iso
