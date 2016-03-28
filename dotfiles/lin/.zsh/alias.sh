# Some aliases.
# 不将以空格开始的命令记入历史。
setopt hist_ignore_space
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
alias ag='ag --path-to-agignore=~/.agignore'
alias -s {htm,html}=firefox
alias -s {el,py,js,c,h,java,m,dot,gp,org,tex,txt}=emacsclient
alias -s {rmvb,rm,mp4,avi,flv,f4v,mpg,mkv,3gp,wmv,mov,dat,asf,mpeg,wma}=mplayer
alias -s {jpg,jpeg,png,bmp}="~/feh.sh"
alias -s {eps,ps}=gv
alias -s {doc,docx}=wps
alias -s {xls,xlsx}=et
alias -s {ppt,pptx}=wpp
alias -s pdf=llpp
