# **COPY THE FILES FROM USB TO /ctools/temp/ BEFORE RUNNING THIS SCRIPT**

# Enter $1 for the month / year of the print file.
# example.  From the command line run the following:
# PrintCams.sh Jun16

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1CamsCTJ

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1CamsGRC

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1CamsCTN

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1CamsORX

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1CamsSFJ

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1CamsCSC

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1CamsSFI

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1CamsQTM

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1CamsHKY

lp -d CTJ-Spl -o cpi=17 /ctools/temp/$1CamsKRS

echo The printing of ALL Cams files is done ..........
