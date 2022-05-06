# THE $1 PARAM IS LAST MONTH AND YEAR e.g. 1506 for 2015 June - USED FOR COMPANY FILES
# THE $2 PARAM IS THE CURRENT MONTH AND YEAR e.g. 1506 for 2015 June - USED FOR THE DRZIP FILES
# 
#
echo Your Before Month / Year End Backup is in Process,  Please be patient... 

echo Removing USB Data Files ...... 
rm /media/ctools/USB/data01/*
rm /media/ctools/USB/data02/*
rm /media/ctools/USB/data03/*
rm /media/ctools/USB/data04/*
rm /media/ctools/USB/data05/*
rm /media/ctools/USB/data06/*
rm /media/ctools/USB/data07/*
rm /media/ctools/USB/data08/*
rm /media/ctools/USB/data09/*
rm /media/ctools/USB/data10/*
rm /media/ctools/USB/data11/*
rm /media/ctools/USB/data12/*

#echo Removing BACKUPHDD Data Files ...... 
#rm /media/ctools/BACKUPHDD/ctools/data01/*
#rm /media/ctools/BACKUPHDD/ctools/data02/*
#rm /media/ctools/BACKUPHDD/ctools/data03/*
#rm /media/ctools/BACKUPHDD/ctools/data04/*
#rm /media/ctools/BACKUPHDD/ctools/data05/*
#rm /media/ctools/BACKUPHDD/ctools/data06/*
#rm /media/ctools/BACKUPHDD/ctools/data07/*
#rm /media/ctools/BACKUPHDD/ctools/data08/*
#rm /media/ctools/BACKUPHDD/ctools/data09/*
#rm /media/ctools/BACKUPHDD/ctools/data10/*
#rm /media/ctools/BACKUPHDD/ctools/data11/*
#rm /media/ctools/BACKUPHDD/ctools/data12/*

echo Removing BACKUPHDD Data Files ...... 
rm /BACKUPHDD/ctools/data01/*
rm /BACKUPHDD/ctools/data02/*
rm /BACKUPHDD/ctools/data03/*
rm /BACKUPHDD/ctools/data04/*
rm /BACKUPHDD/ctools/data05/*
rm /BACKUPHDD/ctools/data06/*
rm /BACKUPHDD/ctools/data07/*
rm /BACKUPHDD/ctools/data08/*
rm /BACKUPHDD/ctools/data09/*
rm /BACKUPHDD/ctools/data10/*
rm /BACKUPHDD/ctools/data11/*
rm /BACKUPHDD/ctools/data12/*

echo Removing Lock files.....
rm /ctools/lock/*db.00*

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

rm /ctools/lock/_db.*

echo COPYING DATA TO BACKUP DISK ON SERVER
cp -a /ctools/data* /BACKUPHDD/ctools/

echo COPYING ALL FOLDERS TO BACKUP DISK ON SERVER
cp -a /ctools/* /BACKUPHDD/ctools

Submit MainCont TapeBackupMonth.Sub01

cd /ctools/ctj
zip /ctools/company/$1MonthEnd01.Zip *

cd /ctools/grc
zip /ctools/company/$1MonthEnd02.Zip *

cd /ctools/ctn
zip /ctools/company/$1MonthEnd03.Zip *

cd /ctools/orx
zip /ctools/company/$1MonthEnd05.Zip *

cd /ctools/sfj
zip /ctools/company/$1MonthEnd07.Zip *

cd /ctools/csc
zip /ctools/company/$1MonthEnd08.Zip *

cd /ctools/sfi
zip /ctools/company/$1MonthEnd09.Zip *

cd /ctools/qtm
zip /ctools/company/$1MonthEnd10.Zip *

cd /ctools/hky
zip /ctools/company/$1MonthEnd11.Zip *

cd /ctools/krs
zip /ctools/company/$1MonthEnd12.Zip *

cd /ctools/dr
zip /ctools/drzip/$2DrPay.Zip *

rm *

cd /ctools/bin

echo Moving Faxes to FaxSent folder......
cp /ctools/fax/*.pdf /ctools/faxsent/
rm /ctools/fax/*
