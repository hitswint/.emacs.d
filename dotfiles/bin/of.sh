#!/bin/bash

if [ x$1 == x ]; then
    echo "Choose:"
    echo "com) openfoam.com"
    echo "org) openfoam.org"
    read flavour
else
    flavour=$1
fi

if [ x$2 == x ]; then
    if [ $flavour = "com" ]; then
        version=$(docker images | awk '($1~/^opencfd/) {print $2}' | awk '!a[$1]++{print}' | percol)
    elif [ $flavour = "org" ]; then
        version=$(docker images | awk -F"-" '($1~/^openfoam\/openfoam/) {print substr($1,18,length($1))}' | awk '!a[$1]++{print}' | percol)
    fi
else
    version=$2
fi

if [[ ( x$flavour != x ) && ( x$version != x ) ]]; then
    case $flavour in
        com)
            # https://hub.docker.com/r/opencfd/openfoam-default
            of_cmd="openfoam-docker -$version -default -dir=$HOME/OpenFOAM/${USER}-com/"
            ;;
        org)
            # https://hub.docker.com/u/openfoam
            of_cmd="xhost +; openfoam$version-linux -d $HOME/OpenFOAM/${USER}-$version -x"  # -u
            ;;
        q)
            break
            ;;
    esac
fi

screen -R $flavour-$version /bin/sh -c "$of_cmd"
