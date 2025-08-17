is_os() { [ "$(uname -s | tr '[:upper:]' '[:lower:]')" = "$1" ] ; }
is_macos() { is_os "darwin" ; }
is_linux() { is_os "linux" ; }

if is_macos; then
    export BASH_SILENCE_DEPRECATION_WARNING=1
    # Replace mac's bsd utils with gnu
    brew_path="$(brew --prefix)" # TODO: This probably doesn't handle no brew well
    gnu_binpath_suffix="libexec/gnubin"
    gnutils=("coreutils" "grep" "findutils")
    for gnutil in "${gnutils[@]}"; do
        full_gnu_binpath="${brew_path}/opt/${gnutil}/${gnu_binpath_suffix}"
        if [ -d "$full_gnu_binpath" ]; then
            export PATH="$full_gnu_binpath":$PATH
        fi
    done
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

# => fzf

configure_fzf() {

    ! type fzf &>/dev/null && echo "[dotfiles] 'fzf' not found." && return

    # TODO: Make the theme if it ends up mattering
    export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'
  --style="full"
  --color=border:#272b35,bg+:#3B4252,bg:#2E3440,spinner:#81A1C1,hl:#616E88
  --color=fg:#D8DEE9,header:#616E88,info:#81A1C1,pointer:#81A1C1,marker:#81A1C1
  --color=fg+:#D8DEE9,prompt:#81A1C1,hl+:#81A1C1
  --border="rounded"
  --scrollbar="│"'
    export FZF_DEFAULT_COMMAND="find . -type f ! -path '*/.*' ! -name '.*'"
    
    fe() {
        fzf --multi --bind 'enter:become(emacs {+})'
    }

    fg() {
        local grep_cmd='grep --exclude ".*" --exclude-dir ".*" -inrsI'
        local initial_query="${*:-}"
        fzf --disabled \
            --query "$initial_query" \
            --bind "start:reload:$grep_cmd {q}" \
            --bind "change:reload:sleep 0.1; $grep_cmd {q} || true" \
            --bind "alt-enter:unbind(change,alt-enter)+change-prompt(2. fzf> )+enable-search+clear-query" \
            --prompt "1. grep> " \
            --delimiter : \
            --preview 'tail -n +{2} {1}' \
            --preview-window 'right' \
            --bind 'enter:become(emacs +{2} {1})'
    }
}

configure_fzf

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
