rm /ctools/sfi/*

Submit MainCont YearEnd.Sub09

Submit MainCont YearEnd2.Sub09

cd /ctools/ps09
cat /ctools/ps09/* >> /ctools/sfi/slp22
rm /ctools/ps09/*
cd /ctools/bin

cp /ctools/spl/DrStateCo09 /ctools/sfi/drstateco09
cp /ctools/spl/ctoolsDrLaserCo09 /ctools/sfi/drlaserco09
#cp /ctools/spl/drnomalco09 /ctools/sfi/drnomalco09
#cp /ctools/spl/dremailco09 /ctools/sfi/dremailco09
#cp /ctools/sfi/sl11 /ctools/spl/sfidrage
cp /ctools/spl/DBPrintCo09 /ctools/sfi/dbprintco09

#mkdir /media/ctools/USB/data09

#echo The After YearEnd Backup is Being Run.....
#cp /ctools/data01/* /media/ctools/USB/data01
#cp /ctools/data02/* /media/ctools/USB/data02
#cp /ctools/data03/* /media/ctools/USB/data03
#cp /ctools/data05/* /media/ctools/USB/data05
#cp /ctools/data06/* /media/ctools/USB/data06
#cp /ctools/data07/* /media/ctools/USB/data07
#cp /ctools/data08/* /media/ctools/USB/data08
#cp -a /ctools/data09/* /media/ctools/USB/data09
#cp /ctools/data10/* /media/ctools/USB/data10
#cp /ctools/data11/* /media/ctools/USB/data11
#cp /ctools/data12/* /media/ctools/USB/data12


echo SFI Year-END DONE........
