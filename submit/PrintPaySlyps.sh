# **COPY THE FILES FROM USB TO /ctools/temp/ BEFORE RUNNING THIS SCRIPT**

# Enter $1 for the month / year of the print file.
# example.  From the command line run the following:
# PrintPaySlyps.sh Jun16

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1PayCTJ

#lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1PayGRC

#lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1PayCTN

#lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1PayORX

#lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1PaySFJ

#lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1PayCSC

echo The printing of ALL PaySlyp files is done ..........
