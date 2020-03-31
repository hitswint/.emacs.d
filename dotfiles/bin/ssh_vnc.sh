#! /bin/bash

server=$1
login=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")
port_client=`expr $port + 59`

if [ x$2 == x ]; then
    echo "Choose:"
    echo "c) client"
    echo "s) server"
    read input1
else
    input1=$2
fi

case $input1 in
    c)
        # * 客户端实现转发
        /usr/bin/autossh -M 0 -o "ServerAliveInterval 60" -o "ServerAliveCountMax 60" -N -L $port_client:127.0.0.1:5901 -p $port $login
        ;;
    s)
        # * 服务端实现转发
        systemctl --user start vncserver@:1.service
        /usr/bin/autossh -M 0 -o "ServerAliveInterval 60" -o "ServerAliveCountMax 60" -N -R 5901:127.0.0.1:5901 -p $port $login
        systemctl --user stop vncserver@:1.service
        ;;
    q)
        break
        ;;
esac
