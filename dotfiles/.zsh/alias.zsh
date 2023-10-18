#!/bin/zsh

# 将zsh的alias导入eshell。
if [ -f $HOME/.emacs.d/eshell/alias ];then
    alias | sed -E "s/^(.*)='?(.*)/alias \1 \2/" | sed -E "s/'$//" > $HOME/.emacs.d/eshell/alias
    # Compatible with bash.
    # alias | sed -E "s/^alias ([^=]+)='(.*)'$/alias \1 \2 \$*/g; s/'\\\''/'/g;" > $HOME/.emacs.d/eshell/alias
fi

alias cd=' cd'
alias ..=' cd ..; ls'
alias ...=' cd ..; cd ..; ls'
alias ....=' cd ..; cd ..; cd ..; ls'
alias cd..='..'
alias cd...='...'
alias cd....='....'
alias ls=' ls --color=auto'
alias ll=' ls -aCHlF --color=always'
alias la=' ls -A'
alias l=' ls -CF'
alias pp='percol --match-method pinyin'
alias -s {htm,html}=firefox
# alias -s {el,py,js,c,h,java,m,dot,gp,org,tex,txt}=emacsclient
alias -s {rmvb,rm,mp4,avi,flv,f4v,mpg,mkv,3gp,wmv,mov,dat,asf,mpeg,wma}=mpv
alias -s {jpg,jpeg,png,bmp}=feh
alias -s {eps,ps}=gv
alias -s {doc,docx,xls,xlsx,ppt,pptx}=libreoffice
alias -s pdf=llpp
alias ec='emacsclient -n'
alias ecc='emacsclient -c -n'
alias ect='emacsclient -t'
alias dia="env GTK_IM_MODULE=xim dia"
alias drawio="env GTK_IM_MODULE=xim drawio"
alias qpdfview="qpdfview --unique"
alias emacs="env LC_CTYPE=zh_CN.UTF-8 XMODIFIERS=@im=fcitx emacs"
alias e="emacs -q --eval=\"(setq make-backup-files nil)\""
alias sudo='sudo '

# OpenFOAM
if [[ -n $WM_PROJECT ]]; then
    gdbe () { emacs --eval "(gdb \"gdb -i=mi $*\")";}
    alias switchenv="echo \"Gcc Opt\nGcc Debug\nClang Opt\" | percol | awk '{print \$1,\$2}' | while read a b; do source $HOME/OpenFOAM/OpenFOAM-10/etc/bashrc WM_PROJECT_USER_DIR=$HOME/$WM_PROJECT/$user-$WM_PROJECT_VERSION WM_COMPILER=\$a WM_COMPILE_OPTION=\$b; done"
fi
