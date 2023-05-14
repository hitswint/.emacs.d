#!/bin/bash

Filename="${1//\//\\}"
wine ~/.wine/drive_c/Program\ Files/xEsoView/xEsoView.exe "$Filename"
