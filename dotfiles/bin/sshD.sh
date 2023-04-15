#! /bin/bash

server=$1

if [ x$2 == x ]; then
    echo "Choose:"
    echo "i) localhost"
    echo "o) ip address"
    read input1
else
    input1=$2
fi

case $input1 in
    i)
        # * 从本机访问
        autossh -N -D 127.0.0.1:1080 $server
        ;;
    o)
        # * 从外部访问
        ip_addr=$(hostname -I | awk '{print $1}')
        autossh -N -D $ip_addr:1080 $server
        ;;
    q)
        break
        ;;
esac
