#!/bin/sh

server=$1
login=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")

# ssh -N -R 8022:127.0.0.1:22 -p $port $login
# autossh -f -N -R 8022:127.0.0.1:22 -p $port $login
autossh -M 0 -o "ServerAliveInterval 30" -o "ServerAliveCountMax 3" -N -R 8022:127.0.0.1:22 -p $port $login
