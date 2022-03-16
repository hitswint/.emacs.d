#! /bin/zsh

weather_file=$(ls *.epw | percol --match-method pinyin)
input_file=$(ls *.idf | percol --match-method pinyin)
output_prefix=$(basename -s .idf "$input_file")
output_dir=$(dirname -- "$input_file")

docker run -it --rm -v $(pwd):/var/simdata/energyplus nrel/energyplus:latest /bin/bash -c "cp /usr/local/bin/Energy+.idd /var/simdata/energyplus; cd /var/simdata/energyplus&& EnergyPlus -r -w \"$weather_file\" -d \"$output_dir\" -p \"$output_prefix\" \"$input_file\"; exec /bin/bash -i"
