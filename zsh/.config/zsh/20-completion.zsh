#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

# ---- Cache directory ----
if [[ -z "$ZSH_CACHE_DIR" || ! -w "$ZSH_CACHE_DIR" ]]; then
  ZSH_CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/oh-my-zsh"
fi

mkdir -p "$ZSH_CACHE_DIR/completions"
(( ${fpath[(Ie)"$ZSH_CACHE_DIR/completions"]} )) || \
  fpath=("$ZSH_CACHE_DIR/completions" $fpath)

# ---- zellij completion ----
if (( $+commands[zellij] )); then
  mkdir -p "$ZSH_CACHE_DIR/completions"
  (( ${fpath[(Ie)"$ZSH_CACHE_DIR/completions"]} )) || fpath=("$ZSH_CACHE_DIR/completions" $fpath)

  local _zj_ver _zj_cache _zj_stamp
  _zj_ver="$(zellij --version 2>/dev/null | awk '{print $NF}')"
  _zj_cache="$ZSH_CACHE_DIR/completions/_zellij"
  _zj_stamp="$ZSH_CACHE_DIR/completions/.zellij-version"

  if [[ ! -s "$_zj_cache" || ! -r "$_zj_stamp" || "$(<"$_zj_stamp")" != "$_zj_ver" ]]; then
    if [[ ! -s "$_zj_cache" ]]; then
      autoload -Uz _zellij
      typeset -g -A _comps
      _comps[zellij]=_zellij
    fi

    (
      umask 022
      local tmp="${_zj_cache}.$$.tmp"
      if zellij setup --generate-completion zsh >| "$tmp" 2>/dev/null; then
        mv -f "$tmp" "$_zj_cache"
        print -r -- "$_zj_ver" >| "$_zj_stamp"
      else
        rm -f "$tmp"
      fi
    ) &|
  fi

  unset _zj_ver _zj_cache _zj_stamp
fi

autoload -Uz compinit
compinit
