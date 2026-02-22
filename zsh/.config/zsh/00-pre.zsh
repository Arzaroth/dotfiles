#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

# ---- Shell options ----
setopt re_match_pcre
setopt extended_glob
setopt auto_cd
setopt nohashdirs
setopt interactivecomments
unsetopt beep

bindkey \^U backward-kill-line

# ---- OMZ knobs ----
typeset -gx HYPHEN_INSENSITIVE=true
typeset -gx COMPLETION_WAITING_DOTS=true
typeset -gx ENABLE_CORRECTION=true

typeset -gx HIST_STAMPS="yyyy-mm-dd"
typeset -gx HISTSIZE=50000000
typeset -gx SAVEHIST=10000000
typeset -gx HISTFILE="${HOME}/.zsh_history"

# ---- Source environment files ----
[[ -r /etc/environment ]] && source /etc/environment
[[ -r "${HOME}/.profile" ]] && source "${HOME}/.profile"
[[ -r "${HOME}/.shell_aliases" ]] && source "${HOME}/.shell_aliases"

# ---- Current directory reporting ----
precmd() {
  echo -n "\x1b]1337;CurrentDir=$(pwd)\x07"
}
