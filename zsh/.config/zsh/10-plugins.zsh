#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

ANTIDOTE_HOME="${ZDOTDIR:-$HOME}/.antidote"

# ---- Auto-install Antidote if missing ----
if [[ ! -r "${ANTIDOTE_HOME}/antidote.zsh" ]]; then
  print -P "%F{33}▓▒░ %F{160}Installing (%F{33}mattmc3/antidote%F{160})…%f"
  command git clone -q --depth=1 https://github.com/mattmc3/antidote.git "${ANTIDOTE_HOME}" && \
    print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
    print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

# ---- Load Antidote ----
zsh_plugins="${ZDOTDIR:-$HOME}/.zsh_plugins.zsh"
zsh_plugins_txt="${ZDOTDIR:-$HOME}/.zsh_plugins.txt"

if [[ ! "$zsh_plugins.zwc" -nt "$zsh_plugins_txt" ]] || \
   [[ ! "$zsh_plugins"     -nt "$zsh_plugins_txt" ]]; then
  source "${ANTIDOTE_HOME}/antidote.zsh"
  antidote bundle <"$zsh_plugins_txt" >"$zsh_plugins"
  zcompile "$zsh_plugins"
fi

# ---- Load plugins ----
source "$zsh_plugins"
