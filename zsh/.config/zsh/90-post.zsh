#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

# ---- tmux TPM bootstrap ----
if [[ ! -d "${HOME}/.tmux/plugins/tpm" ]]; then
  print -P "%F{33}▓▒░ %F{160}Installing (%F{33}tpm%F{160})…%f"
  command mkdir -p "${HOME}/.tmux/plugins"
  command chmod g-rwX "${HOME}/.tmux/plugins"
  command git clone -q https://github.com/tmux-plugins/tpm \
    "${HOME}/.tmux/plugins/tpm"
fi

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
  eval "$(oh-my-posh init zsh --config "${XDG_CONFIG_HOME:-$HOME/.config}/oh-my-posh/config.toml")"
fi

(( $+functions[_zsh_autosuggest_start] )) && _zsh_autosuggest_start

# ---- User aliases ----
[[ -r "${HOME}/.shell_aliases" ]] && source "${HOME}/.shell_aliases"
