#!/bin/bash

# 四种方法判断是否运行于interactive终端下：
# 变量$PS1是否存在：[ -z "$PS1" ] && echo "Noop" || echo "Yes"
# 变量$-是否包含i：if [[ $- == *i* ]]; then
# tty命令：tty -s && echo "This shell is interactive" || echo "This shell is not interactive" ;;
# test命令：if [ -t 0 ]; then

if [[ -t 0 ]]; then
    if [[ x$1 == x ]]; then
        echo "Choose:"
        echo "p) Poweroff"
        echo "r) Reboot"
        echo "s) Suspend"
        echo "h) Hibernate"
        read input1
    else
        input1=$1
    fi

    if [[ ${#input1} -gt 0 ]]; then
        remote=$(cat ~/.ssh/config | grep "^Host " | awk '{print $2}' | percol)
        if [[ ! -z $remote ]]; then
            case $input1 in
                p)
                    ssh $remote "systemctl poweroff"
                    ;;
                r)
                    ssh $remote "systemctl reboot"
                    ;;
                s)
                    ssh $remote "systemctl suspend"
                    ;;
                h)
                    ssh $remote "systemctl hibernate"
                    ;;
                q)
                    break
                    ;;
            esac
        fi
    else
        ssh rpi "~/bin/wol.sh"
    fi
else
    COMMANDS="Poweroff system\nReboot system\nSuspend system\nHibernate system"
    LAUNCHER="rofi.sh -width 30 -dmenu -i -p Power:"
    power_command=`echo -e $COMMANDS | $LAUNCHER | awk '{print $1}' | tr -d '\r\n'`

    REMOTES=$(cat ~/.ssh/config | grep "^Host " | awk '{print $2}' | xargs | sed -e 's/ /\\n/g') # xargs合并多行
    remote=`echo -e $REMOTES | $LAUNCHER | tr -d '\r\n'`

    if [[ (${#power_command} -gt 0) && (${#remote} -gt 0) ]]; then
        case $power_command in
            Poweroff)
                ssh $remote "systemctl poweroff"
                ;;
            Reboot)
                ssh $remote "systemctl reboot"
                ;;
            Suspend)
                ssh $remote "systemctl suspend"
                ;;
            Hibernate)
                ssh $remote "systemctl hibernate"
                ;;
            WakeOnLan)
                ssh rpi "~/bin/wol.sh $remote"
                ;;
            *)
                ;;
        esac
    elif [[ (${#power_command} -eq 0) && (${#remote} -gt 0) ]]; then
        ssh rpi "~/bin/wol.sh $remote"
    fi
fi
