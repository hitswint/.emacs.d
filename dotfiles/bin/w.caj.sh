#!/bin/bash

Filename="${1//\//\\}"
# env LANG=zh_CN.UTF-8 wine ~/.wine/drive_c/Program\ Files/CAJViewer/CAJViewer.exe "$Filename"
env LANG=zh_CN.UTF-8 wine ~/.wine/drive_c/Program\ Files/TTKN/CAJViewer8.0/CAJVieweru.exe "$Filename"
