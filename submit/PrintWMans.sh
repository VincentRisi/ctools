# **COPY THE FILES FROM USB TO /ctools/temp/ BEFORE RUNNING THIS SCRIPT**

# Enter $1 for the month / year of the print file.
# example.  From the command line run the following:
# PrintWMans.sh Feb18

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1WMansCTJ

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1WMansGRC

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1WMansCTN

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1WMansORX

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1WMansSFJ

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1WMansCSC

echo The printing of ALL WMans COID files is done ..........
