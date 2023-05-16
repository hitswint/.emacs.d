#! /bin/bash

# screen加-dm忽略shell环境，可开机启动；加-S xxx命名。
if [ -z "$STY" ]; then exec screen /bin/zsh $0 $1; fi

proj_path=$1
proj_name=$(basename $1)
# nohup gunicorn --chdir \"$proj_path\" --bind unix:/tmp/xxx.socket \"$proj_name\".wsgi:application --reload&
# /bin/bash -c "source ~/.virtualenvs/py3/bin/activate; gunicorn --chdir \"$proj_path\" --bind 127.0.0.1:8000 \"$proj_name\".wsgi:application --reload; exec /bin/bash -i"
zsh -is eval "source ~/.virtualenvs/py3/bin/activate; gunicorn --chdir \"$proj_path\" --bind 127.0.0.1:8000 \"$proj_name\".wsgi:application --reload;"
