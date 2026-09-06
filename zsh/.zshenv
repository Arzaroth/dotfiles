#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

# Locale. LANG is the default for every LC_* category that is not set
# explicitly; exporting LC_ALL as well would override them all and make
# per-machine settings such as LC_TIME impossible.
export LANG=en_US.UTF-8

# Multiplexer TERM fix.
if [[ -n $ZELLIJ || -n $TMUX ]] && [[ $TERM == xterm-ghostty ]]; then
    export TERM=xterm-256color
fi

# WSL ssh-agent forwarding
[[ -n "${WSL_AUTH_SOCK}" ]] && export SSH_AUTH_SOCK="${WSL_AUTH_SOCK}"
