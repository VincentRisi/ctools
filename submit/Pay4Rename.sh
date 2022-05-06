# **COPY THE FILES FROM USB TO /ctools/temp/ BEFORE RUNNING THIS SCRIPT**

# Enter ALL to get All as $1 is all the companies renamed to the Month below.
# Enter a Month Name like Jun16, Aug16 etc. as $2 input.
# example.  From the command line run the following:
# Pay4Rename.sh ALL Feb16

python renamewmans.py $1 $2
echo   The rename of all COID WMans files Run Successfully 

python renameye.py $1 $2
echo   The rename of all Year-End Final Reports Run Successfully 
