#! /bin/sh

server=$1
login=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")

# * 客户端实现转发
# ssh -f -N -L 5901:127.0.0.1:5901 -p $port $login
# * 服务端实现转发
/usr/bin/autossh -M 0 -o "ServerAliveInterval 60" -o "ServerAliveCountMax 60" -N -R 5901:127.0.0.1:5901 -p $port $login
