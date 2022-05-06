# **COPY THE FILES FROM USB TO /ctools/temp/ BEFORE RUNNING THIS SCRIPT**

# This script used to rename all CSC.Exp, CTJ.Exp etc files to a month & Year name
# Enter ALL to get All as $1 is all the companies renamed to the Month/Year below.
# Enter a Month Name like Jun16, Aug16 etc. as $2 input.
# example.  From the command line run the following:
# FnbRename.sh ALL Jun16

python renamefnbcams.py $1 $2
echo   The rename of all .Exp files Run Successfully 

