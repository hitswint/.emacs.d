#! /bin/bash

proj_path=$1
proj_name=$(basename $1)
# nohup gunicorn --chdir /home/swint/git-repo/vmaig_blog/ --bind unix:/tmp/vmaig_blog.socket vmaig_blog.wsgi:application --reload&
/bin/bash -c "source /home/swint/.virtualenvs/py3/bin/activate; gunicorn --chdir \"$proj_path\" --bind 127.0.0.1:8000 \"$proj_name\".wsgi:application --reload; exec /bin/bash -i"
