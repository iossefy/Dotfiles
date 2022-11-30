alias help="echo help yourself"

alias dir='dir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias ls="ls --color=auto"
alias rm="rm -i"

alias glog="git log --color --all --date-order --decorate --dirstat=lines,cumulative --stat|less -R"

# alias mp3-dl="youtube-dl --extract-audio --audio-format mp3"

function stopwatch(){
  date1=`date +%s`;
   while true; do
    echo -ne "$(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)\r";
    sleep 0.1
   done
}


function open() {
    [[ $# -lt 1 ]] && xdg-open --help \
        && return 1 \
        && disown \
        || xdg-open $@ \
        &  disown;
}

# creates ignore files for git
function gi() {
    curl -L -s https://www.gitignore.io/api/${@}
}

