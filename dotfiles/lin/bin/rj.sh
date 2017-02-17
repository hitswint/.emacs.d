#! /bin/bash
echo "1:eth0 0:wlan0"
read auth

if [ $auth = 0 ]; then
    nic="wlan0";
else
    nic="eth0";
fi

# echo "hit"|sudo -S ~/.rjsupplicant/rjsupplicant.sh -a $auth -d 1 -n $nic -u 02510 -p hitswint
# sudo ~/.rjsupplicant/rjsupplicant.sh -a $auth -d 1 -n $nic -s internet -u 02510 -p hitswint
# sudo ~/.rjsupplicant/rjsupplicant.sh -a $auth -d 1 -n $nic -s internet -u 02505 -p sia306
sudo ~/.rjsupplicant/rjsupplicant.sh -a $auth -d 1 -n $nic -s internet -u 02496 -p 252917
