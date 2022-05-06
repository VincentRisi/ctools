rm /ctools/orx/*
rm /ctools/pdf05/*

Submit MainCont YearEnd.Sub05

Submit MainCont YearEnd2.Sub05

cd /ctools/ps05
cat /ctools/ps05/* >> /ctools/orx/slp22
rm /ctools/ps05/*

cp /ctools/spl/DrStateCo05 /ctools/orx/drstateco05
cp /ctools/spl/ctoolsDrLaserCo05 /ctools/orx/drlaserco05
#cp /ctools/spl/drnomalco05 /ctools/orx/drnomalco05
#cp /ctools/spl/dremailco05 /ctools/orx/dremailco05
cp /ctools/orx/sl11 /ctools/spl/orxdrage
cp /ctools/spl/DBPrintCo05 /ctools/orx/dbprintco05

#mkdir /media/ctools/USB/data05

#echo The After YearEnd Backup is Being Run.....
#cp /ctools/data01/* /media/ctools/USB/data01
#cp /ctools/data02/* /media/ctools/USB/data02
#cp /ctools/data03/* /media/ctools/USB/data03
#cp -a /ctools/data05/* /media/ctools/USB/data05
#cp /ctools/data06/* /media/ctools/USB/data06
#cp /ctools/data07/* /media/ctools/USB/data07
#cp /ctools/data08/* /media/ctools/USB/data08
#cp /ctools/data09/* /media/ctools/USB/data09
#cp /ctools/data10/* /media/ctools/USB/data10
#cp /ctools/data11/* /media/ctools/USB/data11
#cp /ctools/data12/* /media/ctools/USB/data12

echo ORX Year-END DONE........
