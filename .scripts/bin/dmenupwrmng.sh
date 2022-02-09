#!/usr/bin/env bash

# opens a dmenu and asks for input
# shutdown, reboot, suspend, lockscreen



declare -a options=(
    "Shutdown"
    "Reboot"
    "Suspend"
    "Lock screen"
)

CMD=$(printf '%s\n' "${options[@]}" | dmenu -i -p "Power manager:")

case ${CMD} in
    Shutdown) loginctl poweroff;;
    Reboot) loginctl reboot;;
    Suspend) loginctl suspend;;
    "Lock screen") betterlockscreen --lock blur;;
esac
