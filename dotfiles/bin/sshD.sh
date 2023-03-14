#!/bin/sh

server=$1

# 从本机访问
# autossh -N -D 127.0.0.1:1080 $server
# 从外部访问
ip_addr=$(hostname -I | awk '{print $1}')
autossh -N -D $ip_addr:1080 $server
