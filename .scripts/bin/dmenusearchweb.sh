#!/usr/bin/env bash
# Search the web
# $BROWSER env variable must be set
# or the script will not wor
BROWSER=qutebrowser
engine="https://duckduckgo.com/?q="
query=$(printf "%s" |dmenu -p "Search DuckDuckGo:")
if [ ! -z "$query" ]
then
    $BROWSER "${engine}${query}"
fi
