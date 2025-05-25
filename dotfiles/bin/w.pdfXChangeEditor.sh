#!/bin/bash

Filename="${1//\//\\}"
env LANG=zh_CN.UTF-8 wine ~/.wine/drive_c/Program\ Files/Tracker\ Software/PDF\ Editor/PDFXEdit.exe "$Filename"
