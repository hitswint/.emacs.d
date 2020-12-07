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
    antigen bundle z

    antigen bundle zsh-users/zsh-autosuggestions
    antigen bundle zsh-users/zsh-completions
    antigen bundle zsh-users/zsh-syntax-highlighting
    antigen bundle zsh-users/zsh-history-substring-search

    antigen apply
fi
