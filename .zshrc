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
zstyle :omz:plugins:ssh-agent agent-forwarding on
zstyle :omz:plugins:ssh-agent quiet yes

# custom completion path
fpath=(~/.zsh/completion $fpath)

autoload -Uz compinit && compinit

typeset -Ag ZI
typeset -gx ZI[HOME_DIR]="${HOME}/.zi"
typeset -gz ZI[BIN_DIR]="${ZI[HOME_DIR]}/bin"

if [ ! -d "${ZI[BIN_DIR]}" ]; then
  print -P "%F{33}▓▒░ %F{160}Installing (%F{33}z-shell/zi%F{160})…%f"
  command mkdir -p "${ZI[HOME_DIR]}" && command chmod g-rwX "${ZI[HOME_DIR]}"
  command git clone -q --depth=1 --branch "main" https://github.com/z-shell/zi "${ZI[BIN_DIR]}" && \
    print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
    print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi
source "${ZI[BIN_DIR]}/zi.zsh"
autoload -Uz _zi
(( ${+_comps} )) && _comps[zi]=_zi
zicompinit

zi light-mode for \
  z-shell/z-a-meta-plugins \
  @annexes \
  @zsh-users+fast

zi ice atinit'typeset -gx HYPHEN_INSENSITIVE=true ENABLE_CORRECTION=true COMPLETION_WAITING_DOTS=true HIST_STAMPS=yyyy-mm-dd HISTFILE=${HOME}/.zsh_history'
zi light ohmyzsh/ohmyzsh
zi ice depth=1
zi light romkatv/powerlevel10k
zi wait lucid for \
    atinit"ZI[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
  z-shell/F-Sy-H \
    atload"unalias grv g" \
  OMZP::{git,themes,battery,sudo,encode64,extract,colored-man-pages,wd,tmux,nmap,command-not-found} \
    has'emacs' \
  OMZP::emacs \
    has'python' \
  OMZP::{python,pip,virtualenv} \
    has'docker' \
  OMZP::{docker,docker-compose} \
    if'[[ -f /etc/os-release ]] && source /etc/os-release && [[ "$ID" = arch ]]' \
  OMZP::archlinux \
    if'[[ -f /etc/os-release ]] && source /etc/os-release && [[ "$ID" = debian ]]' \
  OMZP::debian \
    has'yarn' \
  OMZP::yarn \
    if'[[ -d ~/.ssh ]]' \
  OMZP::ssh-agent \
    as"completion" \
  OMZP::fd/_fd \
    as"completion" \
  OMZP::httpie/_httpie \
    as"completion" \
  OMZP::ripgrep/_ripgrep

zi pack"bgn-binary+keys" for fzf

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

[[ ! -f /etc/environment ]] || source /etc/environment
[[ ! -f ~/.profile ]] || source ~/.profile
[[ ! -f ~/.shell_aliases ]] || source ~/.shell_aliases
if [ ! -d "~/.tmux/plugins/tpm" ]; then
  print -P "%F{33}▓▒░ %F{160}Installing (%F{33}tpm%F{160})…%f"
  command mkdir -p "~/.tmux/plugins/tpm" && command chmod g-rwX "~/.tmux/plugins/tpm"
  command git clone -q https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm && \
    print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
    print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

setopt auto_cd
setopt nohashdirs
setopt interactivecomments
unsetopt share_history
unsetopt beep
bindkey \^U backward-kill-line

precmd () { echo -n "\x1b]1337;CurrentDir=$(pwd)\x07" }

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
