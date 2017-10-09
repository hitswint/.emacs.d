#! /bin/bash
# echo "1:eth0 0:wlan0"
# read auth

if [[ (x$1 != x) && ( $1 == "w" ) ]]; then
    auth="0";
    nic="wlan0";
    server=$2;

else
    auth="1";
    nic="eth0";
    server=$1;
fi

if [ x$1 == x ]; then
    server="swint";
fi

user=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server")
pass=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $NF}' server="$server")
pass_sudo=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2=="localhost" {print $NF}')
echo $pass_sudo|sudo -S echo
sudo ~/.rjsupplicant/rjsupplicant.sh -a $auth -d 1 -n $nic -s internet -u $user -p $pass
