echo CREDITOR MONTH-END MAINTENANCE ROUTINE FOR THE GROUP.....

cd /ctools/cr
zip /ctools/crzip/$1Cr.Zip *

rm /ctools/cr/*
rm /media/ctools/BACKUPHDD/ctools/cr/*


echo CREDITOR MONTH-END ZIPPING ROUTINE FOR GROUP FINISHED......
