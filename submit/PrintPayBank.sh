# **COPY THE FILES FROM USB TO /ctools/temp/ BEFORE RUNNING THIS SCRIPT**

# Enter $1 for the month / year of the print file.
# example.  From the command line run the following:
# PrintPayBank.sh Jun16

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1Totals

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1BankCTJ

#lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1BankGRC

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1BankCTN

#lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1BankORX

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1BankSFJ

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1BankCSC

echo The printing of ALL Bank files is done ..........
