#!/bin/bash

query=$(printf "%s" | dmenu -p "Man page ")

TERMINAL=alacritty
TERM_EXEC=-e
MAN=man

if [ ! -z "$query" ]
then
    exec ${TERMINAL} ${TERM_EXEC} ${MAN} ${query}
fi

exit 0
