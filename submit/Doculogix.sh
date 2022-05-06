# $1 is the Year & Month numbers e.g 1510 = Oct 2015
# e.g. 
#     Doculogix.sh 1510
echo Doculogix Submit Routine

cd /ctools/spl

echo Removing eState and Image files .....
#rm /ctools/estate/*
rm /ctools/image/*

echo Copying ctj files to image folder....
cd /ctools/ctj
#cp dbprintco01 /ctools/image/InCo01
#cp drlaserco01 /ctools/image/StCo01
#cp drlaserco01 /ctools/image/CtjStat
#cp dremailco01 /ctools/image/$1Co01

echo Copying grc files to image folder....
cd /ctools/grc
#cp dbprintco02 /ctools/image/InCo02
#cp drlaserco02 /ctools/image/StCo02
#cp drlaserco02 /ctools/image/GrcStat
#cp dremailco02 /ctools/image/$1Co02

echo Copying ctn files to image folder....
cd /ctools/ctn
#cp dbprintco03 /ctools/image/InCo03
#cp drlaserco03 /ctools/image/StCo03
#cp drlaserco03 /ctools/image/CtnStat
#cp dremailco03 /ctools/image/$1Co03

echo Copying orx files to image folder....
cd /ctools/orx
#cp dbprintco05 /ctools/image/InCo05
#cp drlaserco05 /ctools/image/StCo05

echo Copying sfj files to image folder....
cd /ctools/sfj
#cp dbprintco07 /ctools/image/InCo07
#cp drlaserco07 /ctools/image/StCo07
#cp drlaserco07 /ctools/image/SfjStat
#cp dremailco07 /ctools/image/$1Co07

echo Copying csc files to image folder....
cd /ctools/csc
#cp dbprintco08 /ctools/image/InCo08
#cp drlaserco08 /ctools/image/StCo08
#cp drlaserco08 /ctools/image/CscStat
#cp dremailco08 /ctools/image/$1Co08


echo Removing PDFs from the Dropbox folders .....
rm /home/ctools/Dropbox/ChristensenPDF/'Christensen Tools'/*
rm /home/ctools/Dropbox/ChristensenPDF/'Garrarc Manufacturing Company'/*
rm /home/ctools/Dropbox/ChristensenPDF/'Christensen Tools (Natal)'/*
rm /home/ctools/Dropbox/ChristensenPDF/'Oryx Soldering Products'/*
rm /home/ctools/Dropbox/ChristensenPDF/'Saftec (JHB)'/*
rm /home/ctools/Dropbox/ChristensenPDF/'Christensen Saftec (Cape)'/*

echo Copying PDFs from Linux Server to Dropbox folders .....
cp /ctools/pdf01/* /home/ctools/Dropbox/ChristensenPDF/'Christensen Tools'/
#cp /ctools/pdf02/* /home/ctools/Dropbox/ChristensenPDF/'Garrarc Manufacturing Company'/
cp /ctools/pdf03/* /home/ctools/Dropbox/ChristensenPDF/'Christensen Tools (Natal)'/
#cp /ctools/pdf05/* /home/ctools/Dropbox/ChristensenPDF/'Oryx Soldering Products'/
cp /ctools/pdf07/* /home/ctools/Dropbox/ChristensenPDF/'Saftec (JHB)'/
cp /ctools/pdf08/* /home/ctools/Dropbox/ChristensenPDF/'Christensen Saftec (Cape)'/

echo PDF update to Dropbox is finished ........
cd /ctools/bin

#cp /ctools/image/$1Co0* /ctools/estate
echo Statement files copied to /ctools/estate ........
