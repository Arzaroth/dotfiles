#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

ANTIDOTE_HOME="${ZDOTDIR:-$HOME}/.antidote"

# ---- Install plugin managers on first login ----
_clone_into() {
    local url=$1 dest=$2 tmp="${2}.$$.tmp"
    print -P "%F{33}▓▒░ %F{160}Installing (%F{33}${${url#https://github.com/}%.git}%F{160})…%f"
    command mkdir -p "${dest:h}"
    if command git clone -q --depth=1 "$url" "$tmp" && command mv -T "$tmp" "$dest"; then
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b"
    else
        command rm -rf "$tmp"
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
        return 1
    fi
}

[[ -r "${ANTIDOTE_HOME}/antidote.zsh" ]] || \
    _clone_into https://github.com/mattmc3/antidote.git "$ANTIDOTE_HOME"

if [[ ! -d "${HOME}/.tmux/plugins/tpm" ]]; then
    command mkdir -p "${HOME}/.tmux/plugins"
    command chmod g-rwX "${HOME}/.tmux/plugins"
    _clone_into https://github.com/tmux-plugins/tpm "${HOME}/.tmux/plugins/tpm"
fi

unfunction _clone_into

[[ -r "${ANTIDOTE_HOME}/antidote.zsh" ]] || return 0

# ---- Load Antidote ----
source "${ANTIDOTE_HOME}/antidote.zsh"

zsh_plugins="${ZDOTDIR:-$HOME}/.zsh_plugins.zsh"
zsh_plugins_txt="${ZDOTDIR:-$HOME}/.zsh_plugins.txt"

if [[ ! "$zsh_plugins.zwc" -nt "$zsh_plugins_txt" ]] || \
   [[ ! "$zsh_plugins"     -nt "$zsh_plugins_txt" ]]; then
    zsh_plugins_tmp="${zsh_plugins}.$$.tmp"
    if antidote bundle <"$zsh_plugins_txt" >| "$zsh_plugins_tmp"; then
        command mv -f "$zsh_plugins_tmp" "$zsh_plugins"
        zcompile "$zsh_plugins"
    else
        command rm -f "$zsh_plugins_tmp"
        print -ru2 -- "zsh: antidote could not regenerate ${zsh_plugins}"
    fi
    unset zsh_plugins_tmp
fi

# ---- Load plugins ----
[[ -r "$zsh_plugins" ]] && source "$zsh_plugins"

unset zsh_plugins zsh_plugins_txt
