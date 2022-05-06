rm /ctools/csc/*
rm /ctools/pdf08/*

echo Removing Lock files.....
rm /ctools/lock/*db.00*

Submit MainCont MonthEnd.Sub08

echo Removing Lock files.....
rm /ctools/lock/*db.00*

Submit MainCont MonthEnd2.Sub08

cd /ctools/ps08
cat /ctools/ps08/* >> /ctools/csc/slp22
rm /ctools/ps08/*

cp /ctools/spl/DrStateCo08 /ctools/csc/drstateco08
cp /ctools/spl/DrLaserCo08 /ctools/csc/drlaserco08
cp /ctools/spl/ctoolsDrNoMalCo08 /ctools/csc/drnomalco08
cp /ctools/spl/DrEMailCo08 /ctools/csc/dremailco08
cp /ctools/csc/sl11 /ctools/spl/cscdrage
cp /ctools/spl/DBPrintCo08 /ctools/csc/dbprintco08

#mkdir /media/ctools/USB/data08
#
#echo The After MonthEnd Backup is Being Run.....
#cp /ctools/data01/* /media/ctools/USB/data01
#cp /ctools/data02/* /media/ctools/USB/data02
#cp /ctools/data03/* /media/ctools/USB/data03
#cp -a /ctools/data05/* /media/ctools/USB/data05
#cp /ctools/data06/* /media/ctools/USB/data06
#cp /ctools/data07/* /media/ctools/USB/data07
#cp -a /ctools/data08/* /media/ctools/USB/data08
#cp /ctools/data09/* /media/ctools/USB/data09
#cp /ctools/data10/* /media/ctools/USB/data10
#cp /ctools/data11/* /media/ctools/USB/data11
#cp /ctools/data12/* /media/ctools/USB/data12


echo CSC MONTH-END DONE........
