#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

# ---- History ----
unsetopt share_history
setopt inc_append_history_time

# ---- fzf ----
if (( $+commands[fzf] )); then
    eval "$(fzf --zsh)"

    export FZF_DEFAULT_OPTS="
        --no-mouse --height 50% -1 --reverse --multi --inline-info --border
        --bind='?:toggle-preview'
        --bind='ctrl-a:select-all+accept'
        --bind='ctrl-u:preview-page-up'
        --bind='ctrl-d:preview-page-down'
        --preview-window 'right:hidden:wrap'
        --preview '([[ -d {} ]] && tree {}) || ([[ -f {} ]] && ([[ \$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always --line-range :300 {} || (cat {} | head -300)) 2>/dev/null)) || echo {}'"
fi

# ---- mise ----
if (( $+commands[mise] )); then
    eval "$(mise activate zsh)"
fi

# ---- oh-my-posh ----
if (( $+commands[oh-my-posh] )); then
    eval "$(oh-my-posh init zsh --config "${HOME}/.config/oh-my-posh/config.toml")"
fi

# ---- atuin ----
if (( $+commands[atuin] )); then
    eval "$(atuin init zsh --disable-up-arrow)"
fi

# ---- worktrunk ----
if (( $+commands[wt] )); then
    eval "$(wt config shell init zsh)"
fi

(( $+functions[_zsh_autosuggest_start] )) && _zsh_autosuggest_start

# ---- User aliases ----
[[ -r "${HOME}/.shell_aliases" ]] && source "${HOME}/.shell_aliases"
