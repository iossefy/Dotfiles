#!/bin/sh
# Search the web

engine="https://duckduckgo.com/?q="

if [ -z $BROWSER ];
then
    BROWSER=firefox-bin
fi

query=$(printf "%s" | dmenu -p "Search DuckDuckGo ")

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
    case $BROWSER in
	# Special case for chrome
	"chromium-bin")
	    $BROWSER --new-tab "${engine}${query}" &disown #
	    exit 0
            ;;
	*)
	    $BROWSER "${engine}${query}"
	    ;;

    esac

fi

exit 0
