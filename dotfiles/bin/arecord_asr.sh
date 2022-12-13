#! /bin/bash

if [ -f ~/.recordpid ]; then
    echo "already running" >&2
    exit 1
fi

tmp_asr_file=/tmp/arecord_asr.wav

nohup arecord -r 16000 -t wav $tmp_asr_file &> /dev/null &

echo "$!" > ~/.recordpid

zenity --question --width 300 --height 100

y_or_n=$?

if [ -f ~/.recordpid ]; then
    pid=$(cat ~/.recordpid)
    # kill -l SIGTERM -- $pid &>/dev/null
    kill $pid
    rm -f ~/.recordpid
fi

if [ $y_or_n -eq 0 ]; then
    zsh -c "source ~/.virtualenvs/asr/bin/activate; paddlespeech asr -y --lang zh --input $tmp_asr_file | xargs paddlespeech text --task punc --lang zh --input | xclip -selection cilpboard -i"
    notify-send "ASR finished!"
fi

rm -f $tmp_asr_file
