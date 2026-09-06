#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

ANTIDOTE_HOME="${ZDOTDIR:-$HOME}/.antidote"

# ---- Auto-install Antidote if missing ----
if [[ ! -r "${ANTIDOTE_HOME}/antidote.zsh" ]]; then
    print -P "%F{33}▓▒░ %F{160}Installing (%F{33}mattmc3/antidote%F{160})…%f"
    command git clone -q --depth=1 https://github.com/mattmc3/antidote.git "${ANTIDOTE_HOME}" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

# ---- Load Antidote ----
zsh_plugins="${ZDOTDIR:-$HOME}/.zsh_plugins.zsh"
zsh_plugins_txt="${ZDOTDIR:-$HOME}/.zsh_plugins.txt"

# Regenerate the bundle whenever the plugin list is newer than the generated
# file or its compiled form. Generation goes to a temporary file and is
# committed with mv, so a failed run leaves the previous working bundle in
# place rather than an empty one that would still look up to date.
if [[ ! "$zsh_plugins.zwc" -nt "$zsh_plugins_txt" ]] || \
   [[ ! "$zsh_plugins"     -nt "$zsh_plugins_txt" ]]; then
    zsh_plugins_tmp="${zsh_plugins}.$$.tmp"
    if source "${ANTIDOTE_HOME}/antidote.zsh" && \
       antidote bundle <"$zsh_plugins_txt" >| "$zsh_plugins_tmp"; then
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
