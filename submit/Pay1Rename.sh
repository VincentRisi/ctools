# **COPY THE FILES FROM USB TO /ctools/temp/ BEFORE RUNNING THIS SCRIPT**

# Enter ALL to get All as $1 is all the companies renamed to the Month below.
# Enter a Month Name like Jun16, Aug16 etc. as $2 input.
# example.  From the command line run the following:
# Pay1Rename.sh ALL Jun16

python renamebank.py $1 $2
echo   The rename of all BANK files Run Successfully 

python renamecams.py $1 $2
echo   The rename of all CAMS files Run Successfully 

python renamepay.py $1 $2
echo   The rename of all SLIP files Run Successfully 

python renametax.py $1 $2
echo   The rename of all TAX files Run Successfully 

