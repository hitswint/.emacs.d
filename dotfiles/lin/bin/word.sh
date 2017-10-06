#!/bin/bash

find ~/.wine/drive_c/users/$USER/Application\ Data/Microsoft/Word/STARTUP -name "~*" -exec rm {} \;
wine ~/.wine/drive_c/Program\ Files/Microsoft\ Office/Office12/WINWORD.EXE $1
