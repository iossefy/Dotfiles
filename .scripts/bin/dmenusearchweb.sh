#!/usr/bin/env bash
# Search the web
# $BROWSER env variable must be set
# or the script will not wor
BROWSER=firefox-bin
engine="https://duckduckgo.com/?q="
query=$(printf "%s" |dmenu -p "Search DuckDuckGo:")

# Go to the website directly if the scheme
# is provided
case $query in
	*"http://"*)
		$BROWSER "$query"
		exit;;
	*"https://"*)
		$BROWSER "$query"
		exit;;
esac

# Search using duckduckgo
if [ ! -z "$query" ]
then
    $BROWSER "${engine}${query}"
fi
