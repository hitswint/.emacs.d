#!/bin/sh

/bin/bash -c "source /home/swint/.virtualenvs/shadowsocks/bin/activate; ssserver -s 0.0.0.0 -p 443 -k r408hxkjopzZA -m aes-256-cfb -t 600; exec /bin/bash -i"
