#!/bin/sh

xvkbd -xsendevent -text "$(xclip -selection clipboard -o)"
# xclip -selection clipboard -o | xvkbd -xsendevent -file - 2>/dev/null
