#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

ANTIDOTE_HOME="${ZDOTDIR:-$HOME}/.antidote"

if [[ ! -r "${ANTIDOTE_HOME}/antidote.zsh" ]]; then
    print -ru2 -- "zsh: antidote is not installed, run 'make bootstrap' in the dotfiles checkout"
    return 0
fi

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
