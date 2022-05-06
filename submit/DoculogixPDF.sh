
echo Removing PDFs from the Dropbox folders .....
rm /home/ctools/Dropbox/ChristensenPDF/'Christensen Tools'/*
rm /home/ctools/Dropbox/ChristensenPDF/'Garrarc Manufacturing Company'/*
rm /home/ctools/Dropbox/ChristensenPDF/'Christensen Tools (Natal)'/*
rm /home/ctools/Dropbox/ChristensenPDF/'Oryx Soldering Products'/*
rm /home/ctools/Dropbox/ChristensenPDF/'Saftec (JHB)'/*
rm /home/ctools/Dropbox/ChristensenPDF/'Christensen Saftec (Cape)'/*


echo Copying PDFs from Linux Server to Dropbox folders .....
cp /ctools/pdf01/* /home/ctools/Dropbox/ChristensenPDF/'Christensen Tools'/
cp /ctools/pdf02/* /home/ctools/Dropbox/ChristensenPDF/'Garrarc Manufacturing Company'/
cp /ctools/pdf03/* /home/ctools/Dropbox/ChristensenPDF/'Christensen Tools (Natal)'/
cp /ctools/pdf05/* /home/ctools/Dropbox/ChristensenPDF/'Oryx Soldering Products'/
cp /ctools/pdf07/* /home/ctools/Dropbox/ChristensenPDF/'Saftec (JHB)'/
cp /ctools/pdf08/* /home/ctools/Dropbox/ChristensenPDF/'Christensen Saftec (Cape)'/

echo PDF update to Dropbox is finished ........
