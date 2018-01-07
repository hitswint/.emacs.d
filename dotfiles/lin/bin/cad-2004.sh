#!/bin/bash

Filename="${1//\//\\}"
wine ~/.wine/drive_c/Program\ Files/AutoCAD\ 2004/acad.exe "$Filename"
