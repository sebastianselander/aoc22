#!/bin/bash
echo "Provide a number for the day to set up"
read DAY

DIR=$HOME/Documents/git/aoc22/src/Day$DAY
FILE=$HOME/Documents/git/aoc22/src/Day$DAY/Day$DAY.hs
TEMPLATE=$HOME/Documents/git/aoc22/src/Template.hs
if [  -d "$DIR" ]; then
    echo "The dir: $DIR already exist buckaroo!"
else
    if [ -f "FILE" ]; then
        echo "The file: $FILE already exist buckaroo!"
    else
        mkdir $DIR
        touch $FILE
        # curl --cookie ~/Downloads/aocookies.txt https://adventofcode.com/2022/day/$DAY/input >> $HOME/Documents/git/aoc22/src/Day$DAY/input.txt
        cp $TEMPLATE $DIR/Day$DAY.hs
        sed -i "s/dayNumber = 0/dayNumber = $DAY/g"
    fi
fi

