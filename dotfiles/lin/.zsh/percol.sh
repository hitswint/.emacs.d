# Percol
# Find file.
function ff()
{
    local fullpath=$*
    local filename=${fullpath##*/} # remove "/" from the beginning
    filename=${filename##*./} # remove  ".../" from the beginning
    echo file=$filename
    # only the filename without path is needed
    # filename should be reasonable
    local cli=`find $PWD -not -iwholename '*/target/*' -not -iwholename '*.svn*' -not -iwholename '*.git*' -not -iwholename '*.sass-cache*' -not -iwholename '*.hg*' -type f -iwholename '*'${filename}'*' -print 2>/dev/null | percol`
    if [ $? -eq 0 ]; then
        local new_left="${LBUFFER} ${cli} "
        BUFFER=${new_left}${RBUFFER}
        CURSOR=${#new_left}
    fi
    echo ${cli}
}
function swint-find-file()
{
    local fullpath=$*
    local filename=${fullpath##*/} # remove "/" from the beginning
    filename=${filename##*./} # remove  ".../" from the beginning
    # only the filename without path is needed
    # filename should be reasonable
    local cli=`find $PWD -not -iwholename '*/target/*' -not -iwholename '*.svn*' -not -iwholename '*.git*' -not -iwholename '*.sass-cache*' -not -iwholename '*.hg*' -type f -iwholename '*'${filename}'*' -print 2>/dev/null | percol`
    if [ $? -eq 0 ]; then
        local new_left="${LBUFFER} ${cli} "
        BUFFER=${new_left}${RBUFFER}
        CURSOR=${#new_left}
    fi
    zle reset-prompt
}
zle -N swint-find-file
bindkey '^xf' swint-find-file
function swint-locate-file()
{
    local selected=$(locate ~/ | percol)
    if [ $? -eq 0 ]; then
        local new_left="${LBUFFER} ${selected} "
        BUFFER=${new_left}${RBUFFER}
        CURSOR=${#new_left}
    fi
    zle reset-prompt
}
zle -N swint-locate-file
bindkey '^xF' swint-locate-file

function swint-find-file-current-dir(){
    LBUFFER=$LBUFFER$(ls -Ap | grep -v / | percol --match-method pinyin | tr '\n' ' ' | \
                             sed 's/[[:space:]]*$//') # delete trailing space
    zle -R -c
}
zle -N swint-find-file-current-dir
bindkey '^x^f' swint-find-file-current-dir

# Search the history.
function exists { which $1 &> /dev/null }
if exists percol; then
    function percol_select_history() {
        local tac
        exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
        BUFFER=$(fc -l -n 1 | eval $tac | percol --query "$LBUFFER")
        CURSOR=$#BUFFER         # move cursor
        zle -R -c               # refresh
    }
    zle -N percol_select_history
    bindkey '^x^r' percol_select_history
fi

# Grep or kill process.
function ppgrep() {
    if [[ $1 == "" ]]; then
        PERCOL=percol
    else
        PERCOL="percol --query $1"
    fi
    ps aux | eval $PERCOL | awk '{ print $2 }'
}
function ppkill() {
    if [[ $1 =~ "^-" ]]; then
        QUERY=""            # options only
    else
        QUERY=$1            # with a query
        [[ $# > 0 ]] && shift
    fi
    ppgrep $QUERY | xargs kill $*
}

# cd历史记录。
typeset -U chpwd_functions
CD_HISTORY_FILE=${HOME}/.cd_history_file
function chpwd_record_history() {
    echo $PWD >> ${CD_HISTORY_FILE}
}
chpwd_functions=($chpwd_functions chpwd_record_history)
function percol_get_destination_from_history() {
    sort ${CD_HISTORY_FILE} | uniq -c | sort -r | \
        sed -e 's/^[ ]*[0-9]*[ ]*//' | \
        sed -e s"/^${HOME//\//\\/}/~/" | \
        percol --match-method pinyin| xargs echo
}
function percol_cd_history() {
    local destination=$(percol_get_destination_from_history)
    [ -n $destination ] && cd ${destination/#\~/${HOME}}
    zle reset-prompt
}
zle -N percol_cd_history
function percol_insert_history() {
    local destination=$(percol_get_destination_from_history)
    if [ $? -eq 0 ]; then
        local new_left="${LBUFFER} ${destination} "
        BUFFER=${new_left}${RBUFFER}
        CURSOR=${#new_left}
    fi
    zle reset-prompt
}
zle -N percol_insert_history
bindkey '^x^i' percol_cd_history
bindkey '^xi' percol_insert_history

# Cd using percol.
function swint-cd() {
    cd ./$(ls -F ./ | grep / | percol --match-method pinyin)
    zle reset-prompt
}
zle -N swint-cd
bindkey '^x^m' swint-cd

# percol_cd_upper_dirs
PERCOL_ENABLED=true
function exists() {
    which $1 &> /dev/null
}
exists gtac && _PERCOL_TAC="gtac" || \
        { exists tac && _PERCOL_TAC="tac" || \
                    { _PERCOL_TAC="tail -r" } }
function _percol_tac() {
    eval $_PERCOL_TAC
}
PERCOL_TMP_DIR=/tmp/percol.zsh.tmp
if ! [ -d ${PERCOL_TMP_DIR} ]; then
    mkdir ${PERCOL_TMP_DIR}
fi
local PERCOL_IN=${PERCOL_TMP_DIR}/percol-channel-in-$$
local PERCOL_OUT=${PERCOL_TMP_DIR}/percol-channel-out-$$
function _percol_create_fifo() {
    [ -p $PERCOL_IN ] || { command rm -f $PERCOL_IN; mkfifo -m 600 $PERCOL_IN }
    [ -p $PERCOL_OUT ] || { command rm -f $PERCOL_OUT; mkfifo -m 600 $PERCOL_OUT }
}
function _percol_clean_fifos() {
    command rm -f $PERCOL_IN
    command rm -f $PERCOL_OUT
}
trap _percol_clean_fifos EXIT SIGINT SIGTERM
function _percol_popup() {
    INPUT=$1
    PERCOL_OPTION=$2
    if [[ -n $TMUX ]]; then
        eval "_percol_popup_tmux ${PERCOL_OPTION}; ${INPUT} > ${PERCOL_IN} &; cat ${PERCOL_OUT}"
    else
        eval "${INPUT} | percol ${(Q)PERCOL_OPTION}"
    fi
}
function _percol_clean_prompt() {
    if [[ -n $TMUX ]]; then
        zle reset-prompt
    else
        zle -R -c
    fi
}
function _percol_list_upper_directories() {
    local dir_cursor
    dir_cursor=$(dirname ${PWD})
    local dir_prefix="../../"
    while [[ ${dir_cursor} != "/" ]]; do
        echo ${dir_prefix}$(basename ${dir_cursor})
        dir_cursor=$(dirname ${dir_cursor})
        dir_prefix="../"${dir_prefix}
    done
    echo "/"
}
function percol_cd_upper_dirs() {
    destination=$(_percol_popup "_percol_list_upper_directories")
    if [[ $destination != "" ]]; then
        cd $destination
    fi
    _percol_clean_prompt
    zle reset-prompt
}
zle -N percol_cd_upper_dirs
bindkey '^x^j' percol_cd_upper_dirs

# Ag.
# 显示ag所有结果，然后使用percol过滤。缺点是如果文件名中含有关键词，无法剔除。
function swint-ag() {
    LBUFFER=$LBUFFER$(ag -C 1000 ./ | percol | tr '\n' ' ' | \
                             sed 's/[[:space:]]*$//' | cut -f1 -d ":")
    zle -R -c
}
zle -N swint-ag
bindkey '^xg' swint-ag
# 在命令行中输入关键词，然后显示该关键词对应的搜索结果，不包含文件名含有关键词的结果。
function swint-do-ag() {
    LBUFFER=$(ag $LBUFFER ./ | percol | tr '\n' ' ' | \
                     sed 's/[[:space:]]*$//' | cut -f1 -d ":")
    zle -R -c
}
zle -N swint-do-ag
bindkey '^xG' swint-do-ag

# Switch git branch.
function switch-git-branch() {
    # commiterdate:relativeを commiterdate:localに変更すると普通の時刻表示
    local selected_line="$(git for-each-ref --format='%(refname:short) | %(committerdate:relative) | %(committername) | %(subject)' --sort=-committerdate refs/heads refs/remotes \
            | column -t -s '|' \
            | percol \
            | head -n 1 \
            | awk '{print $1}')"
    if [ -n "$selected_line" ]; then
        BUFFER="git checkout ${selected_line}"
        CURSOR=$#BUFFER
        # ↓そのまま実行の場合
        zle accept-line
    fi
    zle clear-screen
}
zle -N switch-git-branch
bindkey '^xb' switch-git-branch
