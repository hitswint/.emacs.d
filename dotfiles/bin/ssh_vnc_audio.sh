#! /bin/bash

server=$1
login=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")

/usr/bin/autossh -M 0 -o "ServerAliveInterval 60" -o "ServerAliveCountMax 60" -N -R 4000:127.0.0.1:4000 -p $port $login & socat TCP-LISTEN:4000,fork UNIX-CONNECT:/tmp/pulse-server
