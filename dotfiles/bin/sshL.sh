#!/bin/sh

server=$1
login=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")

# ssh -N -L 22:127.0.0.1:8022 -p $port $login
# autossh -f -N -L 22:127.0.0.1:8022 -p $port $login
autossh -M 0 -o "ServerAliveInterval 30" -o "ServerAliveCountMax 3" -N -L 22:127.0.0.1:8022 -p $port $login
