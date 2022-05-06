# Enter ALL to get All companies changed as $1 is the companies renamed to the Month below.
# Enter a Month Name like Jun, Aug etc. as $2 input.
#example  Submit MainCont GlGroupReports.Sub ALL Jun
# example from the command line, run the following:
#  GlGroup.sh ALL Jun
Submit MainCont GlGroupReports.Sub

python renamegp.py $1 $2

echo   CT Group GL Monthly Reports Run Successfully 
