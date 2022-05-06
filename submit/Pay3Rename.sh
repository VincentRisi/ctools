# **COPY THE FILES FROM USB TO /ctools/temp/ BEFORE RUNNING THIS SCRIPT**

# Enter ALL to get All as $1 is all the companies renamed to the Month below.
# Enter a Month Name like Jun16, Aug16 etc. as $2 input.
# example.  From the command line run the following:
# Pay3Rename.sh ALL Jun16

python renamecc.py $1 $2
echo   The rename of all CC files Run Successfully 

python renameend.py $1 $2
echo   The rename of all END files Run Successfully 

cd /ctools/temp
zip /ctools/uif/$2Uif.Zip SLUIF*.PRN 
rm SLUIF*
echo   The zipping and subsequent deletion of all UIF.PRN files Run Successfully 
