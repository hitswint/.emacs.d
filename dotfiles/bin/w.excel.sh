#!/bin/bash

Filename="${1//\//\\}"
env LANG=zh_CN.UTF-8 wine ~/.wine/drive_c/Program\ Files/Microsoft\ Office/Office12/EXCEL.EXE "$Filename"
