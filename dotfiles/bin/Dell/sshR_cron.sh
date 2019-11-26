#!/bin/bash

Curr=`su - swint -c "ssh Dell-ali hostname"`

ping -c 1 swint.mynetgear.com &>/dev/null || ping -c 1 www.baidu.com &>/dev/null

if [[ ($? -eq 0) && (x$Curr == x) ]]; then
    systemctl restart sshR_ali.service
fi
