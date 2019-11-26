#! /bin/sh

sleep 90
/usr/bin/autossh -M 0 -o "ServerAliveInterval 60" -o "ServerAliveCountMax 60" -N -R 19998:127.0.0.1:19999 -p 22 swint@47.94.151.140
