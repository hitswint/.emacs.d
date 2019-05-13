#!/bin/sh

server=$1

autossh -N -D 127.0.0.1:1080 $server
