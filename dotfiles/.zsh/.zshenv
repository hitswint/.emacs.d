#!/bin/zsh

export ZDOTDIR="${HOME}/.zsh"

if [[ -z "${LOADED_SH_PROFILE}" ]]; then
    source $HOME/.profile
fi

function autoload-executables-in-dir() {
    local autoload_dir="$1"
    fpath+="${autoload_dir}"

    # Autoload all shell functions from in a given directory that have
    # the executable bit set.  The executable bit is not necessary, but
    # gives you an easy way to stop the autoloading of a particular
    # shell function.
    for func in ${autoload_dir}/*(N-.x:t); do
        autoload -Uz $func;
    done
}

function source-if-exists() {
    [[ -e "$1" ]] && source "$1"
}

autoload-executables-in-dir "${ZDOTDIR}/functions"
