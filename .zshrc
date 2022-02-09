LANG=en_US.UTF-8
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

autoload -U colors && colors
autoload -Uz compinit


autoload -U compinit promptinit
compinit
promptinit; prompt gentoo

bindkey -v
bindkey '^R' history-incremental-search-backward


zstyle ':completion:*' menu select
typeset -g -A key

setopt hist_ignore_space      # ignore commands that start with space
setopt extended_history       # record timestamp of command in HISTFILE
setopt -o sharehistory
setopt INC_APPEND_HISTORY
#setopt interactivecomments

zstyle ':completion::complete:*' use-cache 1

# TMUX shit here
#if command -v tmux &> /dev/null && [ -n "$PROMPT" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
#    exec tmux
#fi

if [[ "${TERM}" != "" && "${TERM}" == "alacritty" ]]
then
    precmd()
    {
        # output on which level (%L) this shell is running on.
        # append the current directory (%~), substitute home directories with a tilde.
        # "\a" bell (man 1 echo)
        # "print" must be used here; echo cannot handle prompt expansions (%L)
        print -Pn "\e]0;$(id --user --name)@$(hostname): zsh[%L] %~\a"
    }

    preexec()
    {
        # output current executed command with parameters
        echo -en "\e]0;$(id --user --name)@$(hostname): ${1}\a"
    }
fi

# aliases
source $HOME/.scripts/zsh/aliases.sh

# ===== Auto correction
setopt always_to_end # When completing from the middle of a word, move the cursor to the end of the word
setopt auto_menu # show completion menu on successive tab press. needs unsetop menu_complete to work
#setopt auto_name_dirs # any parameter that is set to the absolute name of a directory immediately becomes a name for that directory
setopt complete_in_word # Allow completion from within a word/phrase

unsetopt menu_complete # do not autoselect the first completion entry

# ===== Correction
#unsetopt correct_all # spelling correction for arguments
#setopt correct # spelling correction for commands

# exports
source $HOME/.scripts/zsh/exports.sh
