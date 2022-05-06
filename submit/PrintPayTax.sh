# **COPY THE FILES FROM USB TO /ctools/temp/ BEFORE RUNNING THIS SCRIPT**

# Enter $1 for the month / year of the print file.
# example.  From the command line run the following:
# PrintTax.sh Jun16

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1TaxCTJ

#lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1TaxGRC

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1TaxCTN

#lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1TaxORX

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1TaxSFJ

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1TaxCSC

echo The printing of ALL Tax files is done ..........
