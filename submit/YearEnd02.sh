rm /ctools/grc/*
rm /ctools/pdf02/*

Submit MainCont YearEnd.Sub02

Submit MainCont YearEnd2.Sub02

cd /ctools/ps02
cat /ctools/ps02/* >> /ctools/grc/slp22
rm /ctools/ps02/*

cp /ctools/spl/DrStateCo02 /ctools/grc/drstateco02
cp /ctools/spl/DrLaserCo02 /ctools/grc/drlaserco02
cp /ctools/spl/ctoolsDrNoMalCo02 /ctools/grc/drnomalco02
cp /ctools/spl/DrEMailCo02 /ctools/grc/dremailco02
cp /ctools/grc/sl11 /ctools/spl/grcdrage
cp /ctools/spl/DBPrintCo02 /ctools/grc/dbprintco02

#mkdir /media/ctools/USB/data02
#
#echo The After YearEnd Backup is Being Run.....
#cp /ctools/data01/* /media/ctools/USB/data01
#cp -a /ctools/data02/* /media/ctools/USB/data02
#cp /ctools/data03/* /media/ctools/USB/data03
#cp /ctools/data05/* /media/ctools/USB/data05
#cp /ctools/data06/* /media/ctools/USB/data06
#cp /ctools/data07/* /media/ctools/USB/data07
#cp /ctools/data08/* /media/ctools/USB/data08
#cp /ctools/data09/* /media/ctools/USB/data09
#cp /ctools/data10/* /media/ctools/USB/data10
#cp /ctools/data11/* /media/ctools/USB/data11
#cp /ctools/data12/* /media/ctools/USB/data12

lp -d CTJ-SplStr3 /ctools/grc/st28a
lp -d CTJ-SplStr3 /ctools/grc/st28b
lp -d CTJ-SplStr3 /ctools/grc/st28c

echo GRC Year-END DONE........
`
