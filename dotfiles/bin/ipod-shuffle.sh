#! /bin/bash

pass_sudo=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2=="localhost" {print $NF}')
echo $pass_sudo|sudo -S python $HOME/bin/repos/IPod-Shuffle-4g/ipod-shuffle-4g.py -t -p -d -1 /media/iso
