#!/bin/sh

server=$1
login=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")

# ssh -N -L 12345:127.0.0.1:12345 -p $port $login
autossh -f -N -L 12345:127.0.0.1:12345 -p $port $login
