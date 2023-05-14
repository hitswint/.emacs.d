#! /bin/bash

server="Nextcloud"
host=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server" | sed 's/^.*@//g')
user=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $6}' server="$server" | sed 's/@.*$//g')
pass=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $NF}' server="$server")
port=$(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '$2==server {print $4}' server="$server")

java -Dderby.system.home=$HOME/.webdav_sync/ -Dbe.re.http.no-compress -jar $HOME/.webdav_sync/webdav_sync1_1_6.jar -r -bi -u https://$user:$pass@$host:$port/nextcloud/remote.php/dav/files/swint/Downloads/ -d $HOME/Downloads/
