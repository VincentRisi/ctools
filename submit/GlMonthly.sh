# Enter ALL to get All as $1 is all the companies renamed to the Month below.
# Enter a Month Name like Jun, Aug etc. as $2 input.
#for example    Submit MainCont GlMonthly.Sub ALL Jun
# example.  From the command line run the following:
# GlMonthly.sh ALL Jun
Submit MainCont GlMonthly.Sub

python renamegl.py $1 $2

echo   CT Group GL Monthly Reports Run Successfully 

