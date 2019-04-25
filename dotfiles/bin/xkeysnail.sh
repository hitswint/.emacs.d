#! /bin/sh

if [ -z "$STY" ]; then exec screen -dm -S xkeysnail /bin/zsh $0 $1; fi
zsh -is eval "source ~/.virtualenvs/xkeysnail/bin/activate; sudo -S ~/.virtualenvs/xkeysnail/bin/xkeysnail --watch ~/.xkeysnail.config.py"
