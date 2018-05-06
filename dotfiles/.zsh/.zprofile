#!/bin/zsh

# Load the config shared between bash and zsh.
if [[ -z "${LOADED_SH_PROFILE}" ]]; then
    source $HOME/.profile
fi

# 自动启动startx
if [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty1 ]]; then
    startx
    logout
fi
