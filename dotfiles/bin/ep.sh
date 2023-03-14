#! /bin/bash

version=$(docker images | awk '($1~/^nrel\/energyplus/) {print $2}' | awk '!a[$1]++{print}' | percol)

if [ x$1 == x ]; then
    input_file=$(ls *.idf | percol --match-method pinyin)
else
    input_file=$(basename -- "$1")
fi

if [ x$2 == x ]; then
    weather_file=$(ls *.epw | percol --match-method pinyin)
else
    weather_file=$(basename -- "$2")
fi

output_prefix=$(basename -s .idf "$input_file")
output_dir=$(dirname -- "$input_file")/$output_prefix

# 由EP自动生成的文件夹存在权限问题，无法删除
mkdir -p $output_dir

# 22.1之后修改了EP安装位置
# docker run -it --rm -v $(pwd):/var/simdata/energyplus nrel/energyplus:latest /bin/bash -c "cp /usr/local/bin/Energy+.idd /var/simdata/energyplus; cd /var/simdata/energyplus && EnergyPlus -r -w \"$weather_file\" -d \"$output_dir\" -p \"$output_prefix\" \"$input_file\"; exec /bin/bash -i"

if [[ $input_file && $weather_file ]]; then
    docker run -it --rm -v $(pwd):/var/simdata/energyplus nrel/energyplus:latest /bin/bash -c "cd /var/simdata/energyplus && EnergyPlus -r -w \"$weather_file\" -d \"$output_dir\" -p \"$output_prefix\" \"$input_file\"; exec /bin/bash -i"
else
    docker run -it --rm -v $(pwd):/var/simdata/energyplus nrel/energyplus:latest /bin/bash -c "cd /var/simdata/energyplus && exec /bin/bash -i"
fi
