echo Your Friday Backup is in Process,  Please be patient... 

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

echo Removing Spl and Lock files.....
rm /ctools/lock/*db.00*
rm /ctools/spl/*

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

#cd /ctools/ps01
#echo TapebackupvolMessage2 
#Zip Weekend1.Zip *-*
#rm *-*
#rm /ctools/spl/*
#cd /ctools/bin

Submit MainCont TapeBackupVol.Sub01

echo Running TapeBackupVol.Sub01 Complete.......


lp -d ctj-splXtra -o cpi=17 /ctools/spl/ctj-aa1
lp -d ctj-splXtra -o cpi=17 /ctools/spl/ctj-aa2
lp -d ctj-splXtra -o cpi=17 /ctools/spl/ctj-aa3

echo Moving Faxes to FaxSent folder......
cp /ctools/fax/*.pdf /ctools/faxsent/
rm /ctools/fax/*

echo COPYING COMPANY DATA TO BACKUP DISK ON SERVER
#ll /media/ctools/BACKUPHDD/ctools/*
cp -a /ctools/data* /BACKUPHDD/ctools

echo COPYING ALL FOLDERS TO BACKUP DISK ON SERVER
cp -a /ctools/* /BACKUPHDD/ctools
