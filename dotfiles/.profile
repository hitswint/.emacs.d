# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# Flags
export LOADED_SH_PROFILE="yes"

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export GRML_OSTYPE="$(uname -s)"

# wine
export WINEPREFIX=$HOME/.wine
export WINEARCH=win32

if [[ -z $HOSTNAME ]]; then
    # For zsh compatibility with bash.
    export HOSTNAME=$HOST
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# User configuration
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:$PATH"
export MANPATH="/usr/local/man:$MANPATH"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

PATH+=":/usr/local/texlive/2012/bin/i386-linux"
MANPATH+=":/usr/local/texlive/2012/texmf/doc/man"
INFOPATH+=":/usr/local/texlive/2012/texmf/doc/info"
PATH+=":/usr/local/MATLAB/R2011b/bin"
PATH+=":/opt/emacs25/bin"
