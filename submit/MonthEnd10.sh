rm /ctools/qtm/*

Submit MainCont MonthEnd.Sub10

Submit MainCont MonthEnd2.Sub10

cd /ctools/ps10
cat /ctools/ps10/* >> /ctools/qtm/slp22
rm /ctools/ps10/*
cd /ctools/bin

cp /ctools/spl/DrStateCo10 /ctools/qtm/drstateco10
cp /ctools/spl/ctoolsDrLaserCo10 /ctools/qtm/drlaserco10
#cp /ctools/spl/drnomalco10 /ctools/qtm/drnomalco10
#cp /ctools/spl/dremailco10 /ctools/qtm/dremailco10
#cp /ctools/qtm/sl11 /ctools/spl/qtmdrage
cp /ctools/spl/DBPrintCo10 /ctools/qtm/dbprintco10

#mkdir /media/ctools/USB/data10

#echo The After MonthEnd Backup is Being Run.....
#cp /ctools/data01/* /media/ctools/USB/data01
#cp /ctools/data02/* /media/ctools/USB/data02
#cp /ctools/data03/* /media/ctools/USB/data03
#cp /ctools/data05/* /media/ctools/USB/data05
#cp /ctools/data06/* /media/ctools/USB/data06
#cp /ctools/data07/* /media/ctools/USB/data07
#cp /ctools/data08/* /media/ctools/USB/data08
#cp /ctools/data09/* /media/ctools/USB/data09
#cp -a /ctools/data10/* /media/ctools/USB/data10
#cp /ctools/data11/* /media/ctools/USB/data11
#cp /ctools/data12/* /media/ctools/USB/data12


echo QTM MONTH-END DONE........
