#  ______                            _               _
# (_____ \                      _   (_)_            | |
#  _____) )___ ___  ____  ____ | |_  _| |_ _   _  _ | | ____
# |  ____/ ___) _ \|    \|  _ \|  _)| |  _) | | |/ || |/ _  )
# | |   | |  | |_| | | | | | | | |__| | |_| |_| ( (_| ( (/ /
# |_|   |_|   \___/|_|_|_| ||_/ \___)_|\___)____|\____|\____)
#                        |_|

# ---- Completion UI ----
[[ -z "$LS_COLORS" ]] || \
  zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# ---- SSH completion tweaks ----
zstyle ':completion:*:(ssh|scp|ftp|sftp):*' hosts $hosts
zstyle ':completion:*:(ssh|scp|ftp|sftp):*' users $users
zstyle ':completion:*:(scp|rsync):*' tag-order \
  ' hosts:-ipaddr:ip\ address hosts:-host:host files'

zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' \
  ignored-patterns '*(.|:)*' loopback ip6-loopback localhost \
  ip6-localhost broadcasthost

zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' \
  ignored-patterns \
  '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' \
  '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'

zstyle ':completion:*' hosts off
zstyle -s ':completion:*:hosts' hosts _ssh_config
[[ -r ~/.ssh/config ]] && \
  _ssh_config+=($(sed -ne 's/Host[=\t ]//p' ~/.ssh/config))

zstyle ':completion:*:hosts' hosts $_ssh_config

# ---- OMZ disable update ----
zstyle ':omz:update' mode disabled

# ---- OMZ ssh-agent plugin styles ----
zstyle :omz:plugins:ssh-agent agent-forwarding on
zstyle :omz:plugins:ssh-agent quiet yes

# ---- OMZ disable yarn global bin call ----
zstyle :omz:plugins:yarn global-path no

# ---- antidote clean names ----
zstyle ':antidote:bundle' use-friendly-names 'yes'
