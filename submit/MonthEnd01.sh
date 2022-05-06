rm /ctools/ctj/*
rm /ctools/pdf01/*

echo Removing Lock files.....
rm /ctools/lock/*db.00*

Submit MainCont MonthEnd.Sub01

echo Removing Lock files.....
rm /ctools/lock/*db.00*

Submit MainCont MonthEnd2.Sub01

cd /ctools/ps01
cat /ctools/ps01/* >> /ctools/ctj/slp22
rm /ctools/ps01/*

cp /ctools/spl/DrStateCo01 /ctools/ctj/drstateco01
cp /ctools/spl/DrLaserCo01 /ctools/ctj/drlaserco01
cp /ctools/spl/ctoolsDrNoMalCo01 /ctools/ctj/drnomalco01
cp /ctools/spl/DrEMailCo01 /ctools/ctj/dremailco01
cp /ctools/ctj/sl11 /ctools/spl/ctjdrage
cp /ctools/spl/DBPrintCo01 /ctools/ctj/dbprintco01

mkdir /media/ctools/USB/data01
mkdir /media/ctools/USB/data02
mkdir /media/ctools/USB/data03
mkdir /media/ctools/USB/data04
mkdir /media/ctools/USB/data05
mkdir /media/ctools/USB/data06
mkdir /media/ctools/USB/data07
mkdir /media/ctools/USB/data08
mkdir /media/ctools/USB/data09
mkdir /media/ctools/USB/data10
mkdir /media/ctools/USB/data11
mkdir /media/ctools/USB/data12

echo Removing Lock files.....
rm /ctools/lock/*db.00*

echo The After MonthEnd Backup is Being Run.....
echo Copying Data01.....
cp -a /ctools/data01/* /media/ctools/USB/data01
echo Copying Data02.....
cp -a /ctools/data02/* /media/ctools/USB/data02
echo Copying Data03.....
cp -a /ctools/data03/* /media/ctools/USB/data03
echo Copying Data04.....
cp -a /ctools/data04/* /media/ctools/USB/data04
echo Copying Data05.....
cp -a /ctools/data05/* /media/ctools/USB/data05
echo Copying Data06.....
cp -a /ctools/data06/* /media/ctools/USB/data06
echo Copying Data07.....
cp -a /ctools/data07/* /media/ctools/USB/data07
echo Copying Data08.....
cp -a /ctools/data08/* /media/ctools/USB/data08
echo Copying Data09.....
cp -a /ctools/data09/* /media/ctools/USB/data09
echo Copying Data10.....
cp -a /ctools/data10/* /media/ctools/USB/data10
echo Copying Data11.....
cp -a /ctools/data11/* /media/ctools/USB/data11
echo Copying Data12.....
cp -a /ctools/data12/* /media/ctools/USB/data12

echo CTJ MONTH-END DONE........
