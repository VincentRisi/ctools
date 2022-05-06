# THE $1 PARAM IS LAST MONTH AND YEAR e.g. 1506 for 2015 June - USED FOR COMPANY FILES
# THE $2 PARAM IS THE CURRENT MONTH AND YEAR e.g. 1506 for 2015 June - USED FOR TGHE DRZIP FILES
# 
#
cd /ctools/ctj
zip /ctools/company/$1MonthEnd01.Zip *

cd /ctools/grc
zip /ctools/company/$1MonthEnd02.Zip *

cd /ctools/ctn
zip /ctools/company/$1MonthEnd03.Zip *

cd /ctools/dr
zip /ctools/drzip/$2DrPay.Zip *

rm *

cd /ctools/bin

echo COPYING DATA TO [D1]
