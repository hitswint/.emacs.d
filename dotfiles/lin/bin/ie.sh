#!/bin/bash

Filename="${1//\//\\}"
wine ~/.wine/drive_c/Program\ Files/Internet\ Explorer/iexplore.exe "$Filename"
