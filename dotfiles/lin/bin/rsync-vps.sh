#! /bin/bash

server="vps"
login=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")

if getopts "du" arg; then
    if [ $arg == "d" ]; then
        rsync -avzP $login:$2 $3 -e "ssh -p $port";
    elif [ $arg == "u" ]; then
        rsync -avzP $2 $login:$3 -e "ssh -p $port";
    else
        echo "unknown argument";
    fi
else
    rsync -avzP $1 $login:$2 -e "ssh -p $port";
fi
