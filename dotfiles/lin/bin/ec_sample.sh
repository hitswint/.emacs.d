#!/bin/sh

xte_copy()
{
    case $wine_title in
        *Mozilla\ Firefox* | *Microsoft\ Word* )
            xte <<EOF
sleep 1
keydown Alt_L
key w
keyup Alt_L
sleep 1
EOF
            ;;
        *)
            xte <<EOF
sleep 1
keydown Control_L
key c
keyup Control_L
sleep 1
EOF
            ;;
    esac
}

Wind_wine=`xdotool getwindowfocus -f`

if [ $(xdotool search --onlyvisible --class "wine" | grep -i $Wind_wine) ]; then
    wine_id=`xdpyinfo | sed -ne 's/^focus:.*\(0x[^,]\+\).*/\1/p'`
    if xwininfo -id $wine_id | grep "(has no name)"
    then
        wine_id=`printf "0x%x\n" $(( $wine_id - 1 ))` #Decrement by one.
    fi
    wine_title=`xwininfo -id $wine_id | sed -ne 's/xwininfo: .*"\([^"]\+\)"/\1/p'`
    xte_copy

    word=$(xclip -selection clipboard -o | sed 's/[\"]/\\&/g')
else
    word=$(xclip -selection primary -o | sed 's/[\"]/\\&/g')
fi

run-or-raise.sh emacs
emacsclient -e "(w3m-youdao-sample-sentences (substring-no-properties \"$word\"))"
