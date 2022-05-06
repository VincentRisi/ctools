rm /ctools/hky/*

Submit MainCont YearEnd.Sub11

Submit MainCont YearEnd2.Sub11

cd /ctools/ps11
cat /ctools/ps11/* >> /ctools/hky/slp22
rm /ctools/ps11/*
cd /ctools/bin

cp /ctools/spl/DrStateCo11 /ctools/hky/drstateco11
cp /ctools/spl/ctoolsDrLaserCo11 /ctools/hky/drlaserco11
#cp /ctools/spl/drnomalco11 /ctools/hky/drnomalco11
#cp /ctools/spl/dremailco11 /ctools/hky/dremailco11
#cp /ctools/hky/sl11 /ctools/spl/hkydrage
cp /ctools/spl/DBPrintCo11 /ctools/hky/dbprintco11

#mkdir /media/ctools/USB/data11

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
#cp -a /ctools/data11/* /media/ctools/USB/data11
#cp /ctools/data12/* /media/ctools/USB/data12


echo HKY Year-END DONE........
