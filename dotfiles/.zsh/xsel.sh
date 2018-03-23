# 通过xsel与其他程序进行复制粘贴。
x-copy-region-as-kill () {
    zle copy-region-as-kill
    print -rn $CUTBUFFER | xsel -i -b
    zle deactivate-region
}
zle -N x-copy-region-as-kill
x-kill-region () {
    zle kill-region
    print -rn $CUTBUFFER | xsel -i -b
}
zle -N x-kill-region
x-yank () {
    CUTBUFFER=$(xsel -o -b </dev/null)
    zle yank
}
zle -N x-yank
bindkey -e '^x\ew' x-copy-region-as-kill
bindkey -e '^x^w' x-kill-region
bindkey -e '^x^y' x-yank
