#!/bin/bash
echo "Provide a number for the day to set up"
read DAYNUMBER

PROJECT=$HOME/Documents/git/aoc22
DIR=$HOME/Documents/git/aoc22/src/Day$DAYNUMBER
FILE=$HOME/Documents/git/aoc22/src/Day$DAYNUMBER/Day$DAYNUMBER.hs
TEMPLATE=$HOME/Documents/git/aoc22/src/Template.hs

if [  -d "$DIR" ]; then
    echo "The dir: $DIR already exist buckaroo!"
else
    if [ -f "FILE" ]; then
        echo "The file: $FILE already exist buckaroo!"
    else
        mkdir $DIR
        touch $FILE
        curl --cookie ~/Downloads/aocookies.txt https://adventofcode.com/2022/day/$DAYNUMBER/input >> $HOME/Documents/git/aoc22/src/Day$DAYNUMBER/input.txt
        cp $TEMPLATE $DIR/Day$DAYNUMBER.hs
        sed -i "s/dayNumber = 0/dayNumber = $DAYNUMBER/g" $FILE
        sed -i "s/module Template/module Day$DAYNUMBER.Day$DAYNUMBER/g" $FILE
        sed -i "s/Day\d*.Day\d*/Day$DAYNUMBER.Day$DAYNUMBER/g" $PROJECT/app/Main.hs
        sed -i "s/import Day.*\.Day.* qualified as D/import Day$DAYNUMBER.Day$DAYNUMBER qualified as D/g" $PROJECT/app/Main.hs
        echo "\n Files generated!"
    fi
fi
