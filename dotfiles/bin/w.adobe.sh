#!/bin/bash

Filename="${1//\//\\}"
wine ~/.wine/drive_c/Program\ Files/Adobe\ Acrobat\ Pro/Acrobat/Acrobat.exe "$Filename"
