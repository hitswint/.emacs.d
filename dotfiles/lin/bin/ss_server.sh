#!/bin/sh

# screen加-dm忽略shell环境，可开机启动；加-S xxx命名。
if [ -z "$STY" ]; then exec screen /bin/zsh $0 $1; fi

port="4433"
pass="r408hxkjopzZA"

# 可选$1做内网穿透。
server_sshR=$1
login_sshR=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server_sshR {print $6}' server_sshR="$server_sshR")
port_sshR=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server_sshR {print $4}' server_sshR="$server_sshR")

if [ x$1 != x ]; then
    autossh -f -N -R $port:127.0.0.1:$port -p $port_sshR $login_sshR
fi

# /bin/bash -c "source /home/swint/.virtualenvs/shadowsocks/bin/activate; ssserver -s 0.0.0.0 -p \"$port\" -k \"$pass\" -m aes-256-cfb -t 600; exec /bin/bash -i"
zsh -is eval "source /home/swint/.virtualenvs/shadowsocks/bin/activate; ssserver -s 0.0.0.0 -p \"$port\" -k \"$pass\" -m aes-256-cfb -t 600;"
