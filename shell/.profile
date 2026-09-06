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
        . "$HOME/.bashrc"
    fi
fi

# Prepend a directory to PATH, if it exists and is not already listed.
# This file is sourced by every interactive Zsh (see .config/zsh/00-pre.zsh),
# so the additions below have to be idempotent.
prepend_path() {
    [ -d "$1" ] || return 0
    case ":$PATH:" in
        *":$1:"*) ;;
        *) PATH="$1:$PATH" ;;
    esac
}

# user's private bins
prepend_path "$HOME/bin"
prepend_path "$HOME/.local/bin"

# pyenv
if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    prepend_path "$PYENV_ROOT/bin"
    if command -v pyenv >/dev/null 2>&1; then
        eval "$(pyenv init -)"
    fi
fi

unset -f prepend_path
export PATH

export ALTERNATE_EDITOR=""
export ARCHFLAGS="-arch x86_64"

export FIGNORE=".o:.pyc"
