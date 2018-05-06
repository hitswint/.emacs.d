#!/bin/zsh

# bindkey -v # Link viins to main.
bindkey -e # Link emacs to main.

# Hundreths of a second (1=10ms).  Default is 40 (400ms).
# KEYTIMEOUT=10

function widget-autoload-register() {
    emulate -L zsh
    local widget=$1
    autoload $widget && zle -N $widget
}

function autoload-widgets-in-dir() {
    local autoload_dir="$1"
    fpath+="${autoload_dir}"

    # Autoload all shell functions from in a given directory that have
    # the executable bit set.  The executable bit is not necessary, but
    # gives you an easy way to stop the autoloading of a particular
    # shell function.
    for widget in ${autoload_dir}/*(N-.x:t); do
        widget-autoload-register "${widget}"
    done
}

autoload-widgets-in-dir "${ZDOTDIR}/widgets"

function exists { which $1 &> /dev/null }

function keymap-exists () {
    [[ -n ${(M)keymaps:#$1} ]]
}

# Binds a key-binding in provided maps.
# Uses all maps until '--' followed by a key and function.
# bind-maps emacs viins -- '^x.' widget-insert-abbreviation
function bind-maps() {
    local i sequence widget
    local -a maps

    while [[ "$1" != "--" ]]; do
        maps+=( "$1" )
        shift
    done

    # Remove '--'.
    shift
    sequence="$1"
    widget="$2"
    [[ -z "$sequence" ]] && return 1
    for map in "${maps[@]}"; do
        bindkey -M "$map" "$sequence" "$widget"
    done
}

function bind-maps-by-key-name() {
    local i sequence widget
    local -a maps
    while [[ "$1" != "--" ]]; do
        maps+=( "$1" )
        shift
    done

    # Remove '--'.
    shift
    sequence="${key[$1]}"
    widget="$2"
    [[ -z "$sequence" ]] && return 1
    for map in "${maps[@]}"; do
        bindkey -M "$map" "$sequence" "$widget"
    done
}

# menuselect
if keymap-exists menuselect; then
    # Shift-tab Perform backwards menu completion
    bind-maps-by-key-name menuselect -- BackTab reverse-menu-complete
    # menu selection: pick item but stay in the menu
    bind-maps menuselect -- '\e^M' accept-and-menu-complete
    # also use + and INSERT since it's easier to press repeatedly
    bind-maps menuselect -- '+' accept-and-menu-complete
    bind-maps-by-key-name menuselect -- Insert accept-and-menu-complete
    # accept a completion and try to complete again by using menu
    # completion; very useful with completing directories
    # by using 'undo' one's got a simple file browser
    bind-maps menuselect -- '^o' accept-and-infer-next-history
    # ^@ is \C-<space>
    bind-maps menuselect -- '^@' accept-and-menu-complete
fi

# URxvt sequences:
typeset -a keys_all_modes

bind-maps emacs -- ' ' magic-space
bind-maps emacs -- '^xo' menu-complete
bind-maps emacs -- '^xM' widget-inplace-mkdirs
bind-maps emacs -- "\em" widget-insert-last-typed-word
bind-maps emacs -- '^z' widget-jobs-fg
bind-maps emacs -- '^[X' widget-select-command
bind-maps emacs -- '^[^_Lb^_' undo
bind-maps emacs -- '^[^_Lf^_' redo
bind-maps emacs -- '^[^_D"^_' set-mark-command
bind-maps emacs -- '^[^_3B^_' widget-slash-backward-kill-word
# Percol.
bind-maps emacs -- '^xf' widget-swint-find-file
bind-maps emacs -- '^xF' widget-swint-locate-file
bind-maps emacs -- '^x^f' widget-swint-find-file-current-dir
bind-maps emacs -- '^x^r' widget-percol-select-history
bind-maps emacs -- '^x^i' widget-percol-cd-history
bind-maps emacs -- '^xi' widget-percol-insert-history
bind-maps emacs -- '^x^m' widget-swint-cd
bind-maps emacs -- '^xv' widget-swint-virtualenvs
bind-maps emacs -- '^x^j' widget-percol-cd-upper-dirs
bind-maps emacs -- '^xg' widget-swint-ag
bind-maps emacs -- '^xG' widget-swint-do-ag
bind-maps emacs -- '^xb' widget-switch-git-branch
bind-maps emacs -- '^x\ew' widget-x-copy-region-as-kill
bind-maps emacs -- '^x^w' widget-x-kill-region
bind-maps emacs -- '^x^y' widget-x-yank
bind-maps emacs -- '^X^[^_Db^_' widget-shell-bookmark-add-cwd
bind-maps emacs -- '^[^_Db^_' widget-shell-bookmark-cd-to
