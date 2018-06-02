#!/bin/bash

USERID=`id -u`

if [ ! -e /tmp/emacs$USERID/server ]
then
    echo "Starting server."
    systemctl --user start emacs.service
    while [ ! -e "/tmp/emacs$USERID/server" ] ; do sleep 1 ; done
fi

emacsclient -a '' -c # -e "(swint-load-perspectives)"
