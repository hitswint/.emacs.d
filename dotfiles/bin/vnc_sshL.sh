#!/bin/sh

server=$1
login=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")

# autossh -N -L 12345:127.0.0.1:12345 -p $port $login
ssh -f -N -L 5901:127.0.0.1:5901 -p $port $login
