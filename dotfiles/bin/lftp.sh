#!/bin/bash

# 加-dm忽略shell环境，用于开机启动，加-S用于命名。
if [ -z "$STY" ]; then exec screen /bin/zsh $0 $1; fi

server=$1
host=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server" | sed 's/^.*@//g')
user=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server" | sed 's/@.*$//g')
pass=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $NF}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")

lftp -u $user,$pass $host -p $port # -e 'cd shares'
