#!/bin/sh

server=$1"_ss"
login=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")
pass=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $NF}' server="$server")

/bin/bash -c "source ~/.virtualenvs/shadowsocks/bin/activate; sslocal -s $login -p $port -l 1080 -k $pass -m aes-256-cfb; exec /bin/bash -i"
# /bin/bash -c "source ~/.virtualenvs/shadowsocks/bin/activate; ssserver -s 0.0.0.0 -p 443 -k r408hxkjopzZA -m aes-256-cfb -t 600; exec /bin/bash -i"
# sslocal -c ~/bin/shadowsocks.json # -d start
# ssserver -c ~/bin/shadowsocks.json # -d start
