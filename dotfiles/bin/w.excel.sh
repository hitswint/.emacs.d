#!/bin/bash

Filename="${1//\//\\}"
wine ~/.wine/drive_c/Program\ Files/Microsoft\ Office/Office12/EXCEL.EXE "$Filename"
