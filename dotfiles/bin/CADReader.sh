#!/bin/bash

Filename="${1//\//\\}"
wine ~/.wine/drive_c/Program\ Files/CADReaderInternational/CADReader.exe # "$Filename"
