#! /bin/sh

sleep 90
/usr/bin/autossh -M 0 -o "ServerAliveInterval 60" -o "ServerAliveCountMax 60" -N -R 2222:127.0.0.1:22 -p 10100 swint@swint.mynetgear.com
