#!/bin/zsh

if [ ! -f ${ZDOTDIR}/src/antigen/antigen.zsh ];then
    git clone https://github.com/zsh-users/antigen.git ${ZDOTDIR}/src/antigen
fi

if [ -f ${ZDOTDIR}/src/antigen/antigen.zsh ];then
    # Antigen
    # Bundles from oh-my-zsh
    source ${ZDOTDIR}/src/antigen/antigen.zsh
    antigen use oh-my-zsh
    antigen theme robbyrussell
    antigen bundle git
    antigen bundle extract
    antigen bundle pip
    antigen bundle sudo
    antigen bundle web-search

    # https://github.com/rupa/z
    # 无法排除特定路径，导致/etc/fstab中automount的网络存储自动加载
    # 需修改源代码(https://github.com/rupa/z/issues/189)
    # antigen bundle z
    # (( ${+_Z_EXCLUDE_DIRS} )) || typeset -ga _Z_EXCLUDE_DIRS
    # _Z_EXCLUDE_DIRS+="/mnt/share"
    # 或 _Z_EXCLUDE_DIRS=("/mnt/share")

    # https://github.com/agkozak/zsh-z
    antigen bundle agkozak/zsh-z
    (( ${+ZSHZ_EXCLUDE_DIRS} )) || typeset -ga ZSHZ_EXCLUDE_DIRS
    ZSHZ_EXCLUDE_DIRS+="/mnt"
    export ZSHZ_NO_RESOLVE_SYMLINKS=1

    # https://github.com/Aloxaf/fzf-tab/
    antigen bundle Aloxaf/fzf-tab
    # zstyle ':fzf-tab:*' fzf-command percol
    zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup
    zstyle ':fzf-tab:*' popup-pad 0 0
    zstyle ':fzf-tab:*' fzf-bindings 'alt-m:toggle' 'alt-t:toggle-all'
    zstyle ':fzf-tab:*' switch-group 'alt-p' 'alt-n'
    zstyle ':fzf-tab:*' fzf-flags --color=bg+:8
    # export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'--bind ctrl-v:page-down,alt-v:page-up'

    antigen bundle zsh-users/zsh-autosuggestions
    antigen bundle zsh-users/zsh-completions
    antigen bundle zsh-users/zsh-syntax-highlighting
    antigen bundle zsh-users/zsh-history-substring-search

    antigen apply
fi
