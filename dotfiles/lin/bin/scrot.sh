#!/bin/sh

sleep 0.2 ; scrot $1 'ScreenShot_%Y-%m-%d_at_%I:%M:%S-%p.png' -e 'mv $f ~/Downloads/'
