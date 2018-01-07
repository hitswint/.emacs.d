#!/bin/bash

Filename="${1//\//\\}"
wine ~/.wine/drive_c/Program\ Files/Acme\ CAD\ Converter/AcmeCADConverter.exe "$Filename"
