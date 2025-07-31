is_os() { [ "$(uname -s | tr '[:upper:]' '[:lower:]')" = "$1" ] ; }
is_macos() { is_os "darwin" ; }
is_linux() { is_os "linux" ; }

if is_macos; then
    export BASH_SILENCE_DEPRECATION_WARNING=1
fi

shopt -s histappend
export HISTSIZE=10000
alias l="ls -lA --color=always"
alias r='source $HOME/.bashrc'
export COLORTERM="truecolor"

# Package Management -----------------------------------------------------------

init_homebrew() {
    if is_macos; then
	    brew_prefix="/opt/homebrew"
    elif is_linux; then
	    brew_prefix="/home/linuxbrew/.linuxbrew"
    else
	    return 1
    fi

    if [ -x "$brew_prefix/bin/brew" ]; then
	    eval "$("$brew_prefix"/bin/brew shellenv)"
    fi
}
init_homebrew

# Editor -----------------------------------------------------------------------

if command -v emacs &>/dev/null; then
    export EDITOR="emacs" && export VISUAL="emacs"
elif command -v vim &>/dev/null; then
    export EDITOR="vim" && export VISUAL="vim"
fi
alias e='$EDITOR'
alias ec="emacsclient -nw" # TODO: Update EDITOR if I like this

# Tmux -------------------------------------------------------------------------

alias tma="tmux attach -t"
alias tmk="tmux kill-session -t"
alias tml="tmux list-sessions"
alias tmn="tmux new-session -As"

# Transient Environment --------------------------------------------------------
# Support for persistent, on-the-fly modifications to the shell environment.

TRANSIENT_ENVIRONMENT="${TRANSIENT_ENVIRONMENT:=$HOME/.bash_transient}"
if [ ! -f "$TRANSIENT_ENVIRONMENT" ]; then
    touch "$TRANSIENT_ENVIRONMENT"
fi
. "$TRANSIENT_ENVIRONMENT"

# Create an alias in the transient environment.
# (transient-environment-create)
#
# $1: Name of the alias
# $2: Value of the alias
tec() {
    if [ -z "$1" ] || [ -z "$2" ]; then
	echo "usage: tec <alias name> <alias value>"
	return 1
    fi

    echo "alias $1=\"$2\"" >> "$TRANSIENT_ENVIRONMENT"
    . "$TRANSIENT_ENVIRONMENT"
}

# Create a transient alias to `cd` into $pwd
# (transient-environment-directory)
#
# $1: Name of the alias
ted() {
    if [ -z "$1" ]; then
	echo "usage: ted <alias name>"
	return 1
    fi

    tec "$1" "cd $(pwd)"
    . "$TRANSIENT_ENVIRONMENT"
}

# Edit transient environment configuration
tee() {
    e "$TRANSIENT_ENVIRONMENT"
}

# List transient environment configuration
tel() {
    cat "$TRANSIENT_ENVIRONMENT"
}

# Prompt -----------------------------------------------------------------------

# Expects $1 to be a color var & $2 to be a font-style var (see below)
print_style() { printf "\[\e[%s;3%sm\]" "${2:-0}" "$1" ; }
reset_style() { printf "\[\e[0;0;0m\]" ; }
is_nerd_font() { fc-list : family | grep -iE --quiet ".*nerd-font.*|.*nerd_font.*|.*nerd font.*" ; }

BLACK=0
RED=1
GREEN=2
YELLOW=3
BLUE=4

BOLD=1

FILE_ICON="!"
GIT_ICON="@"
TIME_ICON="ts"
if is_nerd_font; then
    FILE_ICON=""
    GIT_ICON=""
    TIME_ICON="󰔛"
fi

_prompt_previous_exit_section() {
    prevexit=$?
    if [ $prevexit -eq 0 ]; then
	return
    fi

    echo "$(print_style $RED $BOLD) [$prevexit]$(reset_style) "
}

_prompt_git_section() {

    if ! git branch &>/dev/null; then
	return
    fi
    display_color=$GREEN
    if ! git diff --exit-code &>/dev/null; then
	display_color=$YELLOW
    fi
    branch=$(git branch 2>/dev/null | sed '/^[^*]/d' | sed -r 's/[* ]+//g')
    echo "$(print_style $display_color)$GIT_ICON $branch$(reset_style) "
}

_prompt() {
    PS1=$(_prompt_previous_exit_section)
    PS1+=$(_prompt_git_section)
    PS1+="$(print_style $BLACK) \w$(print_style $GREEN) $ $(reset_style)"
}
PROMPT_COMMAND=_prompt

# Misc -------------------------------------------------------------------------

# Rust
CARGO_DIR="$HOME/.cargo"
CARGO_BIN="$CARGO_DIR/bin"
if [ -d "$CARGO_BIN" ]; then
    PATH=$PATH:"$CARGO_BIN"
fi

# AWS CLI completions
if which aws_completer &>/dev/null; then
    complete -C "$(which aws_completer)" aws
fi

# Local Configuration ----------------------------------------------------------
# Consider everything in $HOME/.config/bash as additional shell configuration.
# Source these last so that local configuration takes precedence.

export PATH="$HOME/.config/bin:$PATH"
for local_config in "$HOME/.config/bash/"*; do
    if [ -f "$local_config" ]; then
	. "$local_config"
    fi
done
