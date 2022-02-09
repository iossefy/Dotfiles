alias help="echo help yourself"

alias dir='dir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias ls="ls --color=auto"
alias rm="rm -i"
alias vi="vim"
alias vim="nvim"

alias nse="ls /usr/share/nmap/scripts/|grep "

alias glog="git log --color --all --date-order --decorate --dirstat=lines,cumulative --stat|less -R"

alias mp3-dl="youtube-dl --extract-audio --audio-format mp3"

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

# Vim every where
function :q() {
    exit 0
}

function :q!() {
    exit 1
}

# creates ignore files for git
function gi() {
    curl -L -s https://www.gitignore.io/api/${@}
}

# FFmpeg Stuff
function scrncast(){
    # record screen with audio
    # arguments:
    #   - output.format
    ffmpeg -f x11grab -video_size ${RESOLUTION} -framerate 30 -i :0.0 \
    -f pulse -i default -preset ultrafast -crf 18 -pix_fmt yuv420p ${1}
}

function scrnshot(){
    # take a screen shot
    # arguments
    #   - output.format
    ffmpeg -f x11grab -video_size ${RESOLUTION} -i ${DISPLAY} -vframes 1 ${1}
}

function webcam(){
    # record webcam with audio
    # arguments
    #   - output.format
    WEBCAM_RESOLUTION=640x480
    ffmpeg -f v4l2 -video_size ${WEBCAM_RESOLUTION} -i /dev/video0 -f pulse \
    -i default -c:v libx264 -preset ultrafast -c:a aac ${1}
}

function audiorec(){
    # record audio (microphone)
    # arguments
    #   - output.format
    ffmpeg -f pulse -i default ${1}
}


