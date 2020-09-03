#!/bin/bash

USERID=`id -u`

if [ x"$XDG_RUNTIME_DIR" = x ]; then
    server_socket=/tmp/emacs$USERID/server
else
    server_socket=$XDG_RUNTIME_DIR/emacs/server
fi

if [ ! -e $server_socket ]
then
    echo "Starting server."
    systemctl --user start emacs.service
    while [ ! -e $server_socket ] ; do sleep 1 ; done
fi

echo $server_socket
emacsclient -a '' -c # -e "(swint-load-perspectives)"
