rm /ctools/krs/*

Submit MainCont YearEnd.Sub12

Submit MainCont YearEnd2.Sub12

cd /ctools/ps12
cat /ctools/ps12/* >> /ctools/krs/slp22 
rm /ctools/ps12/*
cd /ctools/bin

cp /ctools/spl/DrStateCo12 /ctools/krs/drstateco12
cp /ctools/spl/ctoolsDrLaserCo12 /ctools/krs/drlaserco12
#cp /ctools/spl/drnomalco12 /ctools/krs/drnomalco12
#cp /ctools/spl/dremailco12 /ctools/krs/dremailco12
#cp /ctools/krs/sl11 /ctools/spl/krsdrage
cp /ctools/spl/DBPrintCo12 /ctools/krs/dbprintco12

#mkdir /media/ctools/USB/data12

#echo The After YearEnd Backup is Being Run.....
#cp /ctools/data01/* /media/ctools/USB/data01
#cp /ctools/data02/* /media/ctools/USB/data02
#cp /ctools/data03/* /media/ctools/USB/data03
#cp /ctools/data05/* /media/ctools/USB/data05
#cp /ctools/data06/* /media/ctools/USB/data06
#cp /ctools/data07/* /media/ctools/USB/data07
#cp /ctools/data08/* /media/ctools/USB/data08
#cp /ctools/data09/* /media/ctools/USB/data09
#cp /ctools/data10/* /media/ctools/USB/data10
#cp /ctools/data11/* /media/ctools/USB/data11
#cp -a /ctools/data12/* /media/ctools/USB/data12

echo KRS Year-END DONE........
