# **COPY THE FILES FROM USB TO /ctools/temp/ BEFORE RUNNING THIS SCRIPT**

# Enter ALL to get All as $1 is all the companies renamed to the Month below.
# Enter a Month Name like Jun16, Aug16 etc. as $2 input.
# example.  From the command line run the following:
# Pay2Rename.sh ALL Jun16

python renamefnl.py $1 $2
echo   The rename of all Fnl files Run Successfully 

python renamemain.py $1 $2
echo   The rename of all Main files Run Successfully 

python renamepen.py $1 $2
echo   The rename of all Pen files Run Successfully 
