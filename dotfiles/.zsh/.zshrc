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

if [ ! -f ${ZDOTDIR}/src/zsh-interactive-cd/zsh-interactive-cd.plugin.zsh ];then
    git clone https://github.com/hitswint/zsh-interactive-cd ${ZDOTDIR}/src/zsh-interactive-cd
else
    source-if-exists "${ZDOTDIR}/src/zsh-interactive-cd/zsh-interactive-cd.plugin.zsh"
fi

PROMPT_ORIG=$PROMPT
if [[ -n $SSH_CONNECTION ]]; then
    PROMPT="%{$fg_bold[yellow]%}%m%{$reset_color%} ${PROMPT}"
fi

# OpenFOAM
if [[ -n $WM_PROJECT ]]; then
    gdbe () { emacs --eval "(gdb \"gdb -i=mi $*\")";}
    # 添加$PROMPT中OpenFOAM相关信息
    # export PROMPT_ORIG=$PROMPT
    # precmd () {
    #     PROMPT="%{$fg_bold[yellow]%}$HOSTNAME%{$fg_bold[magenta]%} $WM_COMPILER/$WM_COMPILE_OPTION%{$reset_color%} ${PROMPT_ORIG}"
    # }

    function of_prompt_info () {
        if [[ $WM_PROJECT_DIR =~ $HOME ]]; then
            echo Custom
        else
            echo Native
        fi
    }

    setopt prompt_subst
    PROMPT='%{$fg_bold[yellow]%}$HOSTNAME%{$fg_bold[magenta]%} $WM_COMPILER/$WM_COMPILE_OPTION%{$fg_bold[green]%} $(of_prompt_info)'"%{$reset_color%} ${PROMPT_ORIG}"

    # 添加OpenFOAM提供的补全
    export BASH=/bin/bash
    autoload bashcompinit
    bashcompinit

    # 使用add-zsh-hook机制，在precmd之前
    function _openfoam_compinit () {
        # 直接source无效，需打开zsh后手动执行
        # 原因：~/.antigen/init.zsh中将_antigen_compinit加入add-zsh-hook/precmd，使bash_completion被覆盖
        # 使用zsh -x 2>&1 | grep bash_completion查看zsh启动进程
        if [[ $WM_PROJECT_VERSION =~ '^v[0-9]{4}$' ]]; then
            # 自带/usr/lib/openfoam/openfoam2306/etc/config.sh/bash_completion无效
            # 借鉴org版本foamGenerateBashCompletion工具生成bash_completion
            source $FOAM_USER_APPBIN/bash_completion
            configFile=$HOME/git-repo/OpenFOAM/OpenFOAM-v2306/etc/bashrc
            alias switchenv="echo \"Gcc Opt\nGcc Debug\nClang Opt\" | percol | awk '{print \$1,\$2}' | while read a b; do sed -i \"s/^export WM_COMPILER=.*$/export WM_COMPILER=\$a/g\" $configFile; sed -i \"s/^export WM_COMPILE_OPTION=.*$/export WM_COMPILE_OPTION=\$b/g\" $configFile; source $configFile; done"
        else
            source $WM_PROJECT_DIR/etc/config.sh/bash_completion
            alias switchenv="echo \"Gcc Opt\nGcc Debug\nClang Opt\" | percol | awk '{print \$1,\$2}' | while read a b; do source $HOME/OpenFOAM/OpenFOAM-10/etc/bashrc WM_PROJECT_USER_DIR=$HOME/$WM_PROJECT/$user-$WM_PROJECT_VERSION WM_COMPILER=\$a WM_COMPILE_OPTION=\$b; done"
        fi
        # source只需运行一次，不需要重复运行
        # add-zsh-hook -D precmd _openfoam_compinit
    }
    autoload -U add-zsh-hook
    add-zsh-hook precmd _openfoam_compinit

    if [[ -z $TMUX ]] # && [[ -n $SSH_TTY ]]
       ; then
        exec tmux new-session -A -s of
    fi
fi
