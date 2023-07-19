#!/usr/bin/env zsh
#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# ssh-agent from wsl
[ -n "${WSL_AUTH_SOCK}" ] && export SSH_AUTH_SOCK="${WSL_AUTH_SOCK}"
# zstyle :omz:plugins:ssh-agent identities id_rsa
zstyle :omz:plugins:ssh-agent agent-forwarding on
zstyle :omz:plugins:ssh-agent quiet yes

# custom completion path
fpath=(~/.zsh/completion $fpath)

# fzf
export FZF_DEFAULT_OPTS="
  --no-mouse --height 50% -1 --reverse --multi --inline-info --border
  --bind='?:toggle-preview'
  --bind='ctrl-a:select-all+accept'
  --bind='ctrl-u:preview-page-up'
  --bind='ctrl-d:preview-page-down'
  --preview-window 'right:hidden:wrap'
  --preview '([[ -d {} ]] && exa --tree {}) || ([[ -f {} ]] && ([[ \$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always --line-range :300 {} || (cat {} | head -300)) 2>/dev/null)) || echo {}'"

# oh-my-zsh stuff. Keep it.
export ZSH="${HOME}/.oh-my-zsh"
ZSH_CUSTOM="${HOME}"/.zsh/
ZSH_THEME="powerlevel10k/powerlevel10k"
# CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
zstyle ':omz:update' mode reminder  # just remind me to update when it's time
# zstyle ':omz:update' frequency 13
# DISABLE_MAGIC_FUNCTIONS="true"
# DISABLE_LS_COLORS="true"
# DISABLE_AUTO_TITLE="true"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
# DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="yyyy-mm-dd"
plugins=(
  git python themes
  command-not-found history-substring-search battery
  colored-man-pages sudo pip wd tmux fd emacs
  virtualenv docker docker-compose nmap yarn httpie
  ssh-agent fzf ssh delta
  zsh-autosuggestions zsh-syntax-highlighting
)
[[ ! -f "${ZSH}"/oh-my-zsh.sh ]] || source "${ZSH}"/oh-my-zsh.sh
[[ ! -f /etc/environment ]] || source /etc/environment
[[ ! -f ~/.shell_aliases ]] || source ~/.shell_aliases

export ALTERNATE_EDITOR=""
export TERM="xterm-256color"
export ARCHFLAGS="-arch x86_64"

export FIGNORE=".o:.pyc"

export HISTFILE="${HOME}"/.zsh_history

setopt nohashdirs
setopt interactivecomments
unsetopt share_history
unsetopt beep
bindkey \^U backward-kill-line

precmd () { echo -n "\x1b]1337;CurrentDir=$(pwd)\x07" }

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
