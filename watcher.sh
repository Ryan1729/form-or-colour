#!/bin/bash

#runs elm-live and updates terminal title
# according to the sate of the compile

main="ShapeOrColour"

#installed version
# elmLive=`which elm-live`
# my local fork
elmLive="/opt/lampp/htdocs/elm-live/bin/elm-live.js"

#record profile log while running elm live
#watcher="unbuffer node --prof -- $elmLive ${main}.elm --output=elm.js"
#inspect in dev tools while running elm live
#watcher="unbuffer node --inspect -- $elmLive ${main}.elm --output=elm.js"

#just run elm-live
watcher="unbuffer $elmLive ${main}.elm --output=elm.js "

# Upon exit, set the terminal title
trap "echo -e \"\e]0;closed\a\"" EXIT

# $@ represents the arguments to the script,
# placing variables net to each other is string concatention
$watcher"$@" |
  while read -r line
  do
    echo "$line"

    # setting the terminal title based on status of compile
    if [[ $line =~ "failed" ]]
    then
      echo -e "\e]0;☒ failed\a"
    elif [[ $line =~ "Successfully" ]]
    then
      echo -e "\e]0;☑ succeeded\a"
    elif [[ $line =~ "Rebuilding" ]]
    then
      echo -e "\e]0;☐ rebuilding\a"
    fi


  done
