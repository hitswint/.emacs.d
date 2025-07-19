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

# fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"

# matlab
# export MATLAB_JAVA=/usr/lib/jvm/java-8-openjdk-amd64/jre
export _JAVA_AWT_WM_NONREPARENTING=1

# mu
export XAPIAN_CJK_NGRAM=1

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# User configuration
export PATH="/opt/emacs30/bin:$HOME/bin:$HOME/.local/bin:$PATH"
export MANPATH="/usr/local/man:$MANPATH"
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi

export PULSE_SERVER="unix:/tmp/pulse-server"

# JAVA
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64
export JRE_HOME=${JAVA_HOME}/jre
export CLASSPATH=.:${JAVA_HOME}/lib:${JRE_HOME}/lib
export GRADLE_HOME=/opt/gradle/gradle-6.7.1
export PATH=${GRADLE_HOME}/bin:${PATH}

# snap
export PATH=/snap/bin:${PATH}

export EDITOR="emacs -q -nw --eval=\"(setq make-backup-files nil)\""
