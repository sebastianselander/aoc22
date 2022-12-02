#!/bin/bash
echo "Day number:"
read DAYNUMBER
echo "Curl? [y/n]"
read WITHCURL

PROJECT=$HOME/Documents/git/aoc22
DIR=$PROJECT/src/Day$DAYNUMBER
FILE=$PROJECT/src/Day$DAYNUMBER/Day$DAYNUMBER.hs
TEMPLATE=$PROJECT/src/Template.hs

if [  -d "$DIR" ]; then
    echo "The dir: $DIR already exist buckaroo!"
else
    if [ -f "FILE" ]; then
        echo "The file: $FILE already exist buckaroo!"
    else
        mkdir $DIR
        touch $FILE

        if [ "$WITHCURL" = 'y' ]; then
                curl --cookie aocookies.txt https://adventofcode.com/2022/day/$DAYNUMBER/input > $HOME/Documents/git/aoc22/src/Day$DAYNUMBER/input.txt
        fi

        cp $TEMPLATE $DIR/Day$DAYNUMBER.hs
        sed -i "s/module Template/module Day$DAYNUMBER.Day$DAYNUMBER/g" $FILE

        sed -i "s/import Day.*\.Day.* qualified as D/import Day$DAYNUMBER.Day$DAYNUMBER qualified as D/g" $PROJECT/app/Main.hs
        sed -i "s/\"Day.*\" </\"Day$DAYNUMBER\" </g" $PROJECT/app/Main.hs

        sed -i "s/-- Day$DAYNUMBER.Day$DAYNUMBER/   Day$DAYNUMBER.Day$DAYNUMBER/g" $PROJECT/aoc22.cabal
        echo ""
        echo "Files generated!"
    fi
fi
