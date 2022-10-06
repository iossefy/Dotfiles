#!/usr/bin/env bash

# opens a dmenu and asks for input
# shutdown, reboot, suspend, lockscreen



declare -a options=(
    "Shutdown"
    "Reboot"
    # "Suspend"
    "Lock screen"
    # "Lock & Suspend"
)

CMD=$(printf '%s\n' "${options[@]}" | dmenu -i -p "Power manager:")

case ${CMD} in
    Shutdown) loginctl poweroff;;
    Reboot) loginctl reboot;;
    # Suspend) sudo s2ram;;
    "Lock screen") i3lock -p default \
        -u -e -i ~/Pictures/wallpapers/lockscreen.png;;
    # "Lock & Suspend") betterlockscreen --lock && sudo s2ram;;
esac
