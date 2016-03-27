# 通过xsel与其他程序进行复制粘贴。
x-copy-region-as-kill () {
    zle copy-region-as-kill
    print -rn $CUTBUFFER | xsel -i -p
}
zle -N x-copy-region-as-kill
x-kill-region () {
    zle kill-region
    print -rn $CUTBUFFER | xsel -i -p
}
zle -N x-kill-region
x-yank () {
    CUTBUFFER=$(xsel -o -p </dev/null)
    zle yank
}
zle -N x-yank
bindkey -e '\ew' x-copy-region-as-kill
bindkey -e '^w' x-kill-region
bindkey -e '^x^y' x-yank
bindkey -e '\em' set-mark-command
