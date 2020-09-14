#! /bin/bash

server=$1
login=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")

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
        if [ $port == 22 ]; then  # 处理局域网地址和DDNS地址差异
            login_ip=${login##*.} # 提取ip地址最后一组
            port_client=`expr $login_ip \* 100 + 59`
        else
            port_client=`expr $port + 59`
        fi

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
