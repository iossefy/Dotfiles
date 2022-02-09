PROMPT=$'%{$fg[white]%}[%{$fg[cyan]%}%~%{$reset_color%}%{$fg[white]%}]-> %{$(git_prompt_info)%}%(?,,%{$fg[white]%}[%{$fg_bold[white]%}%?%{$reset_color%}%{$fg[white]%}])
%{$fg_bold[blue]%};%{$reset_color%} '
PS2=$' %{$fg[green]%}\\>%{$reset_color%} '

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[white]%}<%{$fg_bold[white]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}%{$fg[white]%}> "
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[white]%}⚡%{$reset_color%}"
