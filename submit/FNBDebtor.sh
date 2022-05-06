echo Copying Company files to the spl folder for printing

cp /ctools/ctj/dr8 /ctools/spl/ctjdr8.txt
cp /ctools/ctn/dr8 /ctools/spl/ctndr8.txt
cp /ctools/grc/dr8 /ctools/spl/grcdr8.txt
cp /ctools/sfj/dr8 /ctools/spl/sfjdr8.txt
cp /ctools/csc/dr8 /ctools/spl/cscdr8.txt

cp /ctools/ctj/sl11 /ctools/spl/ctjsl11.txt
cp /ctools/ctn/sl11 /ctools/spl/ctnsl11.txt
cp /ctools/grc/sl11 /ctools/spl/grcsl11.txt
cp /ctools/sfj/sl11 /ctools/spl/sfjsl11.txt
cp /ctools/csc/sl11 /ctools/spl/cscsl11.txt

cp /ctools/ctj/sl21 /ctools/spl/ctjsl21.txt
cp /ctools/ctn/sl21 /ctools/spl/ctnsl21.txt
cp /ctools/grc/sl21 /ctools/spl/grcsl21.txt
cp /ctools/orx/sl21 /ctools/spl/orxsl21.txt
cp /ctools/sfj/sl21 /ctools/spl/sfjsl21.txt
cp /ctools/csc/sl21 /ctools/spl/cscsl21.txt
cp /ctools/sfi/sl21 /ctools/spl/sfisl21.txt
cp /ctools/qtm/sl21 /ctools/spl/qtmsl21.txt
cp /ctools/hky/sl21 /ctools/spl/hkysl21.txt
cp /ctools/krs/sl21 /ctools/spl/krssl21.txt

cp /ctools/ctj/sl31 /ctools/spl/ctjsl31.txt
cp /ctools/ctn/sl31 /ctools/spl/ctnsl31.txt
cp /ctools/sfj/sl31 /ctools/spl/sfjsl31.txt
cp /ctools/csc/sl31 /ctools/spl/cscsl31.txt

cp /ctools/ctj/sl32 /ctools/spl/ctjsl32.txt
cp /ctools/ctn/sl32 /ctools/spl/ctnsl32.txt
cp /ctools/sfj/sl32 /ctools/spl/sfjsl32.txt
cp /ctools/csc/sl32 /ctools/spl/cscsl32.txt

cd /ctools/spl

cp /ctools/spl/*dr8.txt /media/ctools/USB

echo  Sending a Copy of DR8 to CTJ-SplCheque for Yvonne

lp -d CTJ-SplCheque /ctools/dev/source/print/Compressed.Key
lp -d CTJ-SplCheque /ctools/spl/*dr8.txt
lp -d CTJ-SplCheque /ctools/spl/*sl11.txt
lp -d CTJ-SplCheque /ctools/spl/*sl21.txt
lp -d CTJ-SplCheque /ctools/spl/*sl31.txt
lp -d CTJ-SplCheque /ctools/spl/*sl32.txt

cd /ctools/bin

echo FNB Debtor List Submit Routine Finished. 
