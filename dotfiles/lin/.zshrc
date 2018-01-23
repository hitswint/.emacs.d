# Solution for tramp hangs.
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# antigen && oh-my-zsh
source $HOME/git-repo/antigen/antigen.zsh
antigen use oh-my-zsh
antigen theme robbyrussell
antigen bundle git
antigen bundle extract
antigen bundle pip
antigen bundle command-not-find
antigen bundle sudo
antigen bundle web-search
antigen bundle z
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-completions

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
PATH=/usr/local/texlive/2012/bin/i386‐linux:$PATH; export PATH
MANPATH=/usr/local/texlive/2012/texmf/doc/man:$MANPATH; export MANPATH
INFOPATH=/usr/local/texlive/2012/texmf/doc/info:$INFOPATH; export INFOPATH
export PATH=/usr/local/MATLAB/R2011b/bin:$PATH
export PATH=/opt/emacs25/bin:$PATH

# Use modern completion system
autoload -Uz compinit
compinit
# 使M-backspace回删一个词。
autoload -U select-word-style
select-word-style bash

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e
bindkey " " magic-space

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=$HOME/.zsh_history
setopt histignorealldups sharehistory

# 载入相关函数。
[ -f "$HOME/.zsh/percol.sh" ] && source "$HOME/.zsh/percol.sh"
[ -f "$HOME/.zsh/alias.sh" ] && source "$HOME/.zsh/alias.sh"
[ -f "$HOME/.zsh/xsel.sh" ] && source "$HOME/.zsh/xsel.sh"

# virtualenv和virtualenvwrapper设置。
export WORKON_HOME=$HOME/.virtualenvs
[ -f "/usr/local/bin/virtualenvwrapper.sh" ] && source "/usr/local/bin/virtualenvwrapper.sh"

# wine
export WINEPREFIX=$HOME/.wine
export WINEARCH=win32

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search
antigen apply

# 自动启动startx
if [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty1 ]]; then
    startx
    logout
fi

# zsh -is eval "commands"
if [[ $1 == eval ]]
then
    "$@"
    set --
fi
