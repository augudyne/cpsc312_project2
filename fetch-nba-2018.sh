#!/bin/bash 

FOLDER=data/retrieved_`date +%s`
echo "Fetching play-by-play files into $FOLDER"
mkdir -p $FOLDER/

# fetch 2017-18 regular season (up to 12-25-2017)
mkdir $FOLDER/nba-2018-2019-season

for ((i=401070213;i<=401070966;i++))
do
   curl http://www.espn.com/nba/playbyplay?gameId=$i > $FOLDER/nba-2018-2019-season/$i.html
   curl http://www.espn.com/nba/game?gameId=$i > $FOLDER/nba-2018-2019-season/$i-gameinfo.html
done
exit
