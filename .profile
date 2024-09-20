#!/bin/sh
#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
         "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# pyenv
if [ -d "$HOME/.pyenv" ] ; then
    export PYENV_ROOT="$HOME/.pyenv"
    [[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi

# fzf
export FZF_DEFAULT_OPTS="
  --no-mouse --height 50% -1 --reverse --multi --inline-info --border
  --bind='?:toggle-preview'
  --bind='ctrl-a:select-all+accept'
  --bind='ctrl-u:preview-page-up'
  --bind='ctrl-d:preview-page-down'
  --preview-window 'right:hidden:wrap'
  --preview '([[ -d {} ]] && tree {}) || ([[ -f {} ]] && ([[ \$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always --line-range :300 {} || (cat {} | head -300)) 2>/dev/null)) || echo {}'"

export ALTERNATE_EDITOR=""
export TERM="xterm-256color"
export ARCHFLAGS="-arch x86_64"

export FIGNORE=".o:.pyc"
