pathcolor() {
    if [[ -w "${PWD}" ]]; then
        echo -n "${fg_bold[yellow]}"
    else
        echo -n "${fg_bold[red]}"
    fi
}

local user='%{$fg_bold[green]%}%n%{$reset_color%}@%{$fg_bold[magenta]%}%m%{$reset_color%}'
local njobs='%{$fg_bold[cyan]%}%(1j.%j .)%{$reset_color%}'
local pwd='%{$(pathcolor)%}${PWD/$HOME/~}%{$reset_color%}'
local return_code='%(?.. %{$fg_bold[red]%}%? ↵%{$reset_color%})'
local git_branch='%{$reset_color%}$(git_prompt_info)%{$reset_color%}'
local prompt_symbol='%(!.%{$fg_bold[red]%}.%{$fg_bold[blue]%})>%{$reset_color%}'
local venv_status='$(virtualenv_prompt_info)'

ZSH_THEME_VIRTUALENV_PREFIX="%{$fg_bold[white]%}("
ZSH_THEME_VIRTUALENV_SUFFIX=")%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_PREFIX=" [%{$fg_bold[green]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}]"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}✖%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

setopt promptsubst
PROMPT="${user} ${njobs}${pwd} ${prompt_symbol} "
RPROMPT="${venv_status}${return_code} [%*]${git_branch}"
