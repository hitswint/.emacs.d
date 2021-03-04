#!/bin/bash

file_name=$(basename "$1")
dir_name=$(dirname "$1")            # 若单独文件名，为.
dir_true=$(readlink -f "$dir_name") # 转换路径，若为.时转为当前路径
filepath=$dir_true/$file_name
filepath_win="${filepath//\//\\}"

wine ~/.wine/drive_c/Program\ Files/CADReaderInternational/CADReader.exe "Z:$filepath_win"
