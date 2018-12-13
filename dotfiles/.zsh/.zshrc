#!/bin/zsh

# ZSH loads the following files in order. $ZDOTDIR is used instead of $HOME if set.
# 1. /etc/zshenv (always)
# If NO_RCS is set, none of the following are run.
# 2. ~/.zshenv (Usually run for all shells)
# 3. /etc/zprofile (login)
# 4. ~/.zprofile (login)
# 5. /etc/zshrc (interactive)
# 6 ~/.zshrc (interactive)
# 7. /etc/zlogin (login)
# 8. ~/.zlogin (login)
# If a login shell, the following are run on logout or exit.
# ~/.zlogout
# /etc/zlogout

# Solution for tramp hangs.
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# 使M-backspace回删一个词。
autoload -U select-word-style
select-word-style bash

# history
HISTFILE=$HOME/.zsh_history
export SAVEHIST=1000
export HISTSIZE=1000
setopt sharehistory
setopt hist_ignore_all_dups
setopt hist_ignore_space

# chpwd_functions
export CD_HISTORY_FILE=$HOME/.cd_history_file
typeset -ga chpwd_functions
function chpwd_record_history() {
    echo $PWD >> ${CD_HISTORY_FILE}
}
chpwd_functions=($chpwd_functions chpwd_record_history)

# virtualenv
export WORKON_HOME=$HOME/.virtualenvs
source-if-exists "/usr/local/bin/virtualenvwrapper_lazy.sh"

# functions
autoload-executables-in-dir "${ZDOTDIR}/widgets"
source-if-exists "${ZDOTDIR}/antigen.zsh"
source-if-exists "${ZDOTDIR}/completions.zsh"
source-if-exists "${ZDOTDIR}/alias.zsh"
source-if-exists "${ZDOTDIR}/keys.zsh"

# zsh -is eval "commands"
if [[ $1 == eval ]]
then
    "$@"
    set --
fi

if [ ! -f $HOME/git-repo/pinyin-completion/shell/pinyin-comp.zsh ];then
    git clone https://github.com/hitswint/pinyin-completion.git $HOME/git-repo/pinyin-completion
else
    source-if-exists "${HOME}/git-repo/pinyin-completion/shell/pinyin-comp.zsh"
fi
