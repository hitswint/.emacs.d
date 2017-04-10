#! /bin/bash
# echo "1:eth0 0:wlan0"
# read auth

if [[ (x$1 != x) && ( $1 = "w" ) ]]; then
    auth="0";
    nic="wlan0";
else
    auth="1";
    nic="eth0";
fi
echo "hit"|sudo -S echo
sudo ~/.rjsupplicant/rjsupplicant.sh -a $auth -d 1 -n $nic -s internet -u 02510 -p hitswint
# sudo ~/.rjsupplicant/rjsupplicant.sh -a $auth -d 1 -n $nic -s internet -u 02496 -p 252917
# sudo ~/.rjsupplicant/rjsupplicant.sh -a $auth -d 1 -n $nic -s internet -u 02505 -p sia306
