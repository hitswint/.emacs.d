#!/bin/bash

find ~/.wine/drive_c/users/swint/Application\ Data/Microsoft/Word/STARTUP -name "~*" -exec rm {} \;
wine /home/swint/.wine/drive_c/Program\ Files/Microsoft\ Office/Office12/WINWORD.EXE $1
