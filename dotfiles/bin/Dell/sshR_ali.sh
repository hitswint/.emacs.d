#! /bin/sh

sleep 90
/usr/bin/autossh -M 0 -o "ServerAliveInterval 60" -o "ServerAliveCountMax 60" -N -R 12345:127.0.0.1:22 -p 22 swint@47.94.151.140
