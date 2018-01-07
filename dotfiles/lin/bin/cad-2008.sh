#!/bin/bash

Filename="${1//\//\\}"
wine ~/.wine/drive_c/Program\ Files/AutoCAD\ 2008/acad.exe "$Filename"
