#!/bin/bash

ALTERNATIVE=""

function check_root {
    ID=`id -u`
    if [ ${ID} -ne 0 ]; then
        echo "You have to be root to run this script"
        exit 0
    fi
}

function syntax {
    echo "Syntax: $0 [nvidia|nouveau]"
    exit 0
}

function set_nouveau {
    # No need anymore. update-alternatives会自动处理blacklists
    # sed -i "s/nouveau/nvidia/" /etc/modprobe.d/nvidia-blacklists-nouveau.conf

    # This doesn't really seem to be needed any more
    #    echo 'Section "Device"
    #    Identifier     "Device0"
    #    Driver         "nouveau"
    #EndSection
    #' > /usr/share/X11/xorg.conf.d/20-nouveau.conf

    mv /etc/X11/xorg.conf /etc/X11/xorg.conf.disabled
    ALTERNATIVE="/usr/lib/mesa-diverted"
}

function set_nvidia {
    # No need anymore. update-alternatives会自动处理blacklists
    # sed -i "s/nvidia/nouveau/" /etc/modprobe.d/nvidia-blacklists-nouveau.conf

    # This doesn't really seem to be needed any more
    # rm -f /usr/share/X11/xorg.conf.d/20-nouveau.conf

    mv /etc/X11/xorg.conf.disabled /etc/X11/xorg.conf
    ALTERNATIVE="/usr/lib/nvidia"
}

check_root

if [ x$1 == x ]; then
    echo "Choose:"
    echo "n) Nvidia"
    echo "m) Nouveau"
    echo "h) Help"
    read input1
else
    input1=$1
fi

case $input1 in
    n)
        set_nvidia
        ;;
    m)
        set_nouveau
        ;;
    h)
        syntax
        ;;
    q)
        break
        ;;
esac

update-alternatives --set glx "${ALTERNATIVE}"
update-initramfs -u

echo ""
echo "${1} successfully set. Reboot your system to apply the changes ..."
