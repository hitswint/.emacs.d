#!/bin/sh

# $1开启ssserver，可选$2做内网穿透。
server=$1
login=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server" | sed 's/^.*@//g')
port="4433"
pass="r408hxkjopzZA"

server_sshL=$2
login_sshL=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server_sshL {print $6}' server_sshL="$server_sshL")
port_sshL=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server_sshL {print $4}' server_sshL="$server_sshL")

if [ x$2 != x ]; then
    autossh -f -N -L $port:127.0.0.1:$port -p $port_sshL $login_sshL
fi

/bin/bash -c "source ~/.virtualenvs/shadowsocks/bin/activate; sslocal -s \"$login\" -p \"$port\" -l 1080 -k \"$pass\" -m aes-256-cfb; exec /bin/bash -i"
# sslocal -c ~/bin/shadowsocks.json # -d start
# ssserver -c ~/bin/shadowsocks.json # -d start
