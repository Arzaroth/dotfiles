#!/usr/bin/env zsh
#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

[[ -o interactive ]] || return

ZSH_CFG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"

for f in \
  "$ZSH_CFG_DIR/00-pre.zsh" \
  "$ZSH_CFG_DIR/05-styles.zsh" \
  "$ZSH_CFG_DIR/10-plugins.zsh" \
  "$ZSH_CFG_DIR/20-completion.zsh" \
  "$ZSH_CFG_DIR/90-post.zsh"
do
  [[ -r "$f" ]] && source "$f"
done

unset ZSH_CFG_DIR
