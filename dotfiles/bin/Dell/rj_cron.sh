#!/bin/bash

ping -c 1 swint.mynetgear.com &>/dev/null || ping -c 1 www.baidu.com &>/dev/null

if [ $? -ne 0 ]; then
    systemctl restart rj_sys.service
fi
