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

zmodload zsh/complist
bindkey -M menuselect '^[[Z' reverse-menu-complete

# ---- SSH completion tweaks ----
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

_ssh_config_concrete_hosts() {
    local -a hosts
    hosts=(${=${(f)"$(sed -nE 's/^[[:space:]]*[Hh]ost[[:space:]=]+(.*)$/\1/p' ~/.ssh/config)"}})
    reply=(${hosts:#*[*?]*})
}

if [[ -r ~/.ssh/config ]]; then
    _ssh_config_concrete_hosts
    zstyle ':completion:*:hosts' hosts $reply
fi
unfunction _ssh_config_concrete_hosts

# ---- OMZ disable update ----
zstyle ':omz:update' mode disabled

# ---- OMZ ssh-agent plugin styles ----
zstyle :omz:plugins:ssh-agent agent-forwarding on
zstyle :omz:plugins:ssh-agent quiet yes

# ---- OMZ disable yarn global bin call ----
zstyle :omz:plugins:yarn global-path no

# ---- antidote clean names ----
zstyle ':antidote:bundle' use-friendly-names 'yes'
