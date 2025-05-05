#!/usr/bin/env zsh
#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

# ssh-agent from wsl
[ -n "${WSL_AUTH_SOCK}" ] && export SSH_AUTH_SOCK="${WSL_AUTH_SOCK}"
zstyle :omz:plugins:ssh-agent agent-forwarding on
zstyle :omz:plugins:ssh-agent quiet yes

setopt re_match_pcre
setopt extended_glob

# Workaround for https://github.com/zdharma-continuum/zinit/issues/504
# Remove subversion dependency. Function clones all files in plugin
# directory (on github) that might be useful to zinit snippet directory.
# Should only be invoked via `zi ice atpull"%atclone" atclone"_fix-omz-plugin"`
_fix-omz-plugin() {
  [[ -f ./._zinit/teleid ]] || return 1
  local teleid="$(<./._zinit/teleid)"
  local pluginid
  for pluginid (${teleid#OMZ::plugins/} ${teleid#OMZP::}) {
    [[ $pluginid != $teleid ]] && break
  }
  (($?)) && return 1
  print "Fixing $teleid..."
  git clone --quiet --no-checkout --depth=1 --filter=tree:0 https://github.com/ohmyzsh/ohmyzsh
  cd ./ohmyzsh
  git sparse-checkout set --no-cone /plugins/$pluginid
  git checkout --quiet
  cd ..
  local file
  for file (./ohmyzsh/plugins/$pluginid/*~(.gitignore|*.plugin.zsh)(D)) {
    print "Copying ${file:t}..."
    command cp -R $file ./${file:t}
  }
  rm -rf ./ohmyzsh
}

typeset -Ag ZINIT
typeset -gx ZINIT[HOME_DIR]="${HOME}/.zinit"
typeset -gz ZINIT[BIN_DIR]="${ZINIT[HOME_DIR]}/zinit.git"

if [ ! -d "${ZINIT[BIN_DIR]}" ]; then
  print -P "%F{33}▓▒░ %F{160}Installing (%F{33}zdharma-continuum/zinit%F{160})…%f"
  command mkdir -p "${ZINIT[HOME_DIR]}" && command chmod g-rwX "${ZINIT[HOME_DIR]}"
  command git clone -q --depth=1 --branch "main" https://github.com/zdharma-continuum/zinit.git "${ZINIT[BIN_DIR]}" && \
    print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
    print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi
source "${ZINIT[BIN_DIR]}/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
zicompinit

zi light-mode for \
  zdharma-continuum/zinit-annex-bin-gem-node \
  zdharma-continuum/zinit-annex-patch-dl \
  wfxr/forgit

zi wait lucid for \
    atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
  zdharma-continuum/fast-syntax-highlighting \
    blockf \
  zsh-users/zsh-completions \
    atload"!_zsh_autosuggest_start" \
  zsh-users/zsh-autosuggestions

# Make sure $ZSH_CACHE_DIR is writable, otherwise use a directory in $HOME
if [[ ! -w "$ZSH_CACHE_DIR" ]]; then
  ZSH_CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/oh-my-zsh"
fi
# Create cache and completions dir and add to $fpath
mkdir -p "$ZSH_CACHE_DIR/completions"
(( ${fpath[(Ie)"$ZSH_CACHE_DIR/completions"]} )) || fpath=("$ZSH_CACHE_DIR/completions" $fpath)

zi light-mode for \
  OMZL::{compfix,directories,functions,git,grep,key-bindings,misc,prompt_info_functions,spectrum,termsupport,theme-and-appearance,vcs_info}.zsh \
    atinit'typeset -gx HYPHEN_INSENSITIVE=true COMPLETION_WAITING_DOTS=true' \
  OMZL::completion.zsh \
    atinit'typeset -gx ENABLE_CORRECTION=true' \
  OMZL::correction.zsh \
    atinit'typeset -gx HIST_STAMPS=yyyy-mm-dd HISTSIZE=290000 SAVEHIST=290000 HISTFILE=${HOME}/.zsh_history' \
  OMZL::history.zsh

# set completion colors to be the same as `ls`, after theme has been loaded
[[ -z "$LS_COLORS" ]] || zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

zi wait lucid for \
  OMZP::{themes,battery,sudo,encode64,extract,nmap,command-not-found} \
    if'[[ -f /etc/os-release ]] && source /etc/os-release && [[ "$ID" = arch ]]' \
  OMZP::archlinux \
    if'[[ -f /etc/os-release ]] && source /etc/os-release && [[ "$ID" = debian ]]' \
  OMZP::debian \
    has'yarn' \
  OMZP::yarn \
    if'[[ -d "${HOME}/.ssh" ]]' \
  OMZP::ssh-agent

zi is-snippet wait lucid for \
   atpull"%atclone" atclone"_fix-omz-plugin" \
 OMZP::aliases \
   atpull"%atclone" atclone"_fix-omz-plugin" \
 OMZP::colored-man-pages

zi is-snippet for \
   has'emacs' nocompile atpull"%atclone" atclone"_fix-omz-plugin" \
 OMZP::emacs

zi has'docker' is-snippet wait lucid for \
   atpull"%atclone" atclone"_fix-omz-plugin" \
 OMZP::{docker,docker-compose}

zi has'python' is-snippet wait lucid for \
  OMZP::{python,pip,virtualenv}

zi as'completion' is-snippet wait lucid for \
  OMZP::httpie/_httpie

zi pack"bgn-binary+keys" multisrc"key-bindings.zsh _fzf_completion" for fzf
zi as'program' from'gh-r' id-as'JanDeDobbeleer/oh-my-posh' lucid nocompile mv'posh-* -> oh-my-posh' for @JanDeDobbeleer/oh-my-posh

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

[[ ! -f /etc/environment ]] || source /etc/environment
[[ ! -f "${HOME}/.profile" ]] || source "${HOME}/.profile"
[[ ! -f "${HOME}/.shell_aliases" ]] || source "${HOME}/.shell_aliases"
if [ ! -d "${HOME}/.tmux/plugins/tpm" ]; then
  print -P "%F{33}▓▒░ %F{160}Installing (%F{33}tpm%F{160})…%f"
  command mkdir -p "${HOME}/.tmux/plugins" && command chmod g-rwX "${HOME}/.tmux/plugins"
  command git clone -q https://github.com/tmux-plugins/tpm "${HOME}/.tmux/plugins/tpm" && \
    print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
    print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

setopt auto_cd
setopt nohashdirs
setopt interactivecomments
unsetopt beep
bindkey \^U backward-kill-line

precmd () { echo -n "\x1b]1337;CurrentDir=$(pwd)\x07" }

eval "$(oh-my-posh init zsh --config "${HOME}/.promptitude.omp.toml")"
function set_poshcontext() {
  export BG_JOBS="$(jobs | wc -l | xargs)"
}

zstyle ':completion:*:(ssh|scp|ftp|sftp):*' hosts $hosts
zstyle ':completion:*:(ssh|scp|ftp|sftp):*' users $users
zstyle ':completion:*:(scp|rsync):*' tag-order ' hosts:-ipaddr:ip\ address hosts:-host:host files'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'
zstyle ':completion:*' hosts off
zstyle -s ':completion:*:hosts' hosts _ssh_config
[[ -r ~/.ssh/config ]] && _ssh_config+=($(cat ~/.ssh/config | sed -ne 's/Host[=\t ]//p'))
zstyle ':completion:*:hosts' hosts $_ssh_config
