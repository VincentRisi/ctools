rm /ctools/sfj/*
rm /ctools/pdf07/*

echo Removing Lock files.....
rm /ctools/lock/*db.00*

Submit MainCont YearEnd.Sub07

echo Removing Lock files.....
rm /ctools/lock/*db.00*

Submit MainCont YearEnd2.Sub07

cd /ctools/ps07
cat /ctools/ps07/* >> /ctools/sfj/slp22
rm /ctools/ps07/*

cp /ctools/spl/DrStateCo07 /ctools/sfj/drstateco07
cp /ctools/spl/DrLaserCo07 /ctools/sfj/drlaserco07
cp /ctools/spl/ctoolsDrNoMalCo07 /ctools/sfj/drnomalco07
cp /ctools/spl/DrEMailCo07 /ctools/sfj/dremailco07
cp /ctools/sfj/sl11 /ctools/spl/sfjdrage
cp /ctools/spl/DBPrintCo07 /ctools/sfj/dbprintco07

#mkdir /media/ctools/BACKUPUSB/data07

#echo The After YearEnd Backup is Being Run.....
#cp /ctools/data01/* /media/ctools/USB/data01
#cp /ctools/data02/* /media/ctools/USB/data02
#cp /ctools/data03/* /media/ctools/USB/data03
#cp -a /ctools/data05/* /media/ctools/USB/data05
#cp /ctools/data06/* /media/ctools/USB/data06
#cp -a /ctools/data07/* /media/ctools/USB/data07
#cp /ctools/data08/* /media/ctools/USB/data08
#cp /ctools/data09/* /media/ctools/USB/data09
#cp /ctools/data10/* /media/ctools/USB/data10
#cp /ctools/data11/* /media/ctools/USB/data11
#cp /ctools/data12/* /media/ctools/USB/data12


echo SFJ Year-END DONE........
