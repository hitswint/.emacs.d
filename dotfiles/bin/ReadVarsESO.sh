#! /bin/zsh

esoname=$(basename -- "$1")
esoname_noext="${esoname%.*}"
extname="${esoname##*.}"

if [[ $extname == "eso" ]]; then
    ext=rvi
elif [[ $extname == "mtr" ]]; then
    ext=mvi
fi

echo "$esoname\n$esoname.csv\n0" > $esoname.$ext

# Timestep/Hourly/Daily/Monthly/Annual/RunPeriod
ReadVarsESO $esoname.$ext unlimited
