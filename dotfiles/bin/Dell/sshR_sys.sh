#! /bin/sh

sleep 60

# echo
# echo test network connection ...
# echo

PINGRET=$( ping 8.8.8.8 -c 4 | grep "ttl=" )

[ -z "$PINGRET" ] &&
    {
        PINGRET=$( ping 8.8.4.4 -c 4 | grep "ttl=" )
        [ -z "$PINGRET"  ] &&
            {
                # echo no network connection ...
                exit 1
            }
    }||
        {
            # echo test network successfully ...
            autossh -f -N -R 12345:127.0.0.1:22 -p 22 swint@47.94.151.140
            # exit 0
        }

# if [ "$?" == "0" ]; then
#     echo "O.K"
# fi
