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

setopt RE_MATCH_PCRE   # _fix-omz-plugin function uses this regex style

# Workaround for zinit issue#504: remove subversion dependency. Function clones all files in plugin
# directory (on github) that might be useful to zinit snippet directory. Should only be invoked
# via zinit atclone"_fix-omz-plugin"
_fix-omz-plugin() {
  if [[ ! -f ._zinit/teleid ]] then return 0; fi
  if [[ ! $(cat ._zinit/teleid) =~ "^OMZP::.*" ]] then return 0; fi
  local OMZP_NAME=$(cat ._zinit/teleid | sed -n 's/OMZP:://p')
  git clone --quiet --no-checkout --depth=1 --filter=tree:0 https://github.com/ohmyzsh/ohmyzsh
  cd ohmyzsh
  git sparse-checkout set --no-cone plugins/$OMZP_NAME
  git checkout --quiet
  cd ..
  local OMZP_PATH="ohmyzsh/plugins/$OMZP_NAME"
  local file
  for file in $OMZP_PATH/*~(.gitignore|README.md|*.plugin.zsh)(D); do
    local filename="${file:t}"
    echo "Copying $file to $(pwd)/$filename..."
    cp $file $filename
  done
  rm -rf ohmyzsh
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
  z-shell/zsh-fancy-completions \
  wfxr/forgit

zinit wait lucid for \
    atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
  zdharma-continuum/fast-syntax-highlighting \
    blockf \
  zsh-users/zsh-completions \
    atload"!_zsh_autosuggest_start" \
  zsh-users/zsh-autosuggestions

zi ice depth=1
zi light romkatv/powerlevel10k

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
    if'[[ -d ~/.ssh ]]' \
  OMZP::ssh-agent

zi is-snippet wait lucid for \
   atpull"%atclone" atclone"_fix-omz-plugin" \
 OMZP::aliases \
   atpull"%atclone" atclone"_fix-omz-plugin" \
 OMZP::colored-man-pages

# zi is-snippet wait lucid for \
#    has'emacs' nocompile atpull"%atclone" atclone"_fix-omz-plugin" \
#  OMZP::emacs

zi has'docker' is-snippet wait lucid for \
   atpull"%atclone" atclone"_fix-omz-plugin" \
 OMZP::{docker,docker-compose}

zi has'python' is-snippet wait lucid for \
  OMZP::{python,pip,virtualenv}

zi as'completion' is-snippet wait lucid for \
  OMZP::{fd/_fd,httpie/_httpie,ripgrep/_ripgrep}

zi pack"bgn-binary+keys" multisrc"key-bindings.zsh _fzf_completion" for fzf

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

[[ ! -f /etc/environment ]] || source /etc/environment
[[ ! -f ~/.profile ]] || source ~/.profile
[[ ! -f ~/.shell_aliases ]] || source ~/.shell_aliases
if [ ! -d ~/.tmux/plugins/tpm ]; then
  print -P "%F{33}▓▒░ %F{160}Installing (%F{33}tpm%F{160})…%f"
  command mkdir -p ~/.tmux/plugins && command chmod g-rwX ~/.tmux/plugins
  command git clone -q https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm && \
    print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
    print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

setopt auto_cd
setopt nohashdirs
setopt interactivecomments
unsetopt beep
bindkey \^U backward-kill-line

precmd () { echo -n "\x1b]1337;CurrentDir=$(pwd)\x07" }

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
