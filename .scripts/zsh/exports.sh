# Some PATH Variables and exports
PATH="$HOME/.cargo/bin:$PATH"
# PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
PATH="$HOME/.node_modules/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/.scripts/bin/:$PATH"

PS1="%{$fg[white]%}[%{$fg[cyan]%}%~%{$fg[white]%}]-> %(?,,%{$fg[white]%}[%{$fg_bold[white]%}%?%{$reset_color%}%{$fg[white]%}])
%{$fg_bold[blue]%};%{$reset_color%} "

PS2="%{$fg[green]%}\\>%{$reset_color%} "

## History file configuration
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh_history"
[ "$HISTSIZE" -lt 50000 ] && HISTSIZE=50000
[ "$SAVEHIST" -lt 10000 ] && SAVEHIST=10000

#export ZSH_DISABLE_COMPINIT
# XDG stuff
export XDG_DESKTOP_DIR="$HOME/Desktop"
export XDG_DOCUMENTS_DIR="$HOME/Documents"
export XDG_DOWNLOAD_DIR="$HOME/Downloads"
export XDG_MUSIC_DIR="$HOME/Music"
export XDG_PICTURES_DIR="$HOME/Pictures"
export XDG_PUBLICSHARE_DIR="$HOME/Public"
export XDG_VIDEOS_DIR="$HOME/Videos"

export TERM=xterm-256color
export TERMINAL=alacritty
export EDITOR="vim"
export GOPATH=$HOME/go/
export BROWSER="firefox-bin"
export CC=gcc
export AS=as
export AR=ar
export CXX=g++
export LD=ld
export TAR=tar
export LIBGL_ALWAYS_SOFTWARE=on
export XDG_CURRENT_DESKTOP='X-Generic'
export QT_X11_NO_MITSHM=1 # fixes the issue when opening qt app with sudo
export PATH
export npm_config_prefix=~/.node_modules
export GEM_HOME=$HOME/.gem
export PROMPT
