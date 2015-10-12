#! /bin/bash
read mydevice
echo "hit"|sudo -S mount /dev/$mydevice /media/iso
