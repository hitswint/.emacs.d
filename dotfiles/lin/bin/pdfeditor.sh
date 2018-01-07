#!/bin/bash

Filename="${1//\//\\}"
wine ~/.wine/drive_c/Program\ Files/Foxit\ Software/PDF\ Editor/PDFEdit.exe "$Filename"
