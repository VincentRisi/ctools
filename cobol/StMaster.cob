        IDENTIFICATION DIVISION.
        PROGRAM-ID. StMaster.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStChanges".
          Copy "SelectSlParameter".
          Copy "SelectBmMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStockChanges.
           COPY ChlfdParam.
           COPY ChlfdToolkit.

       WORKING-STORAGE SECTION.
       77  WS-CATEGORY        PIC X(3) VALUE " ".
       77  NEW-STOCKNO        PIC X VALUE " ".      
       77  WS-STOCK-CHANGE    PIC X VALUE " ".      
       77  WS-ALL-ENTERED     PIC X VALUE " ".
       77  WS-END             PIC X VALUE " ".      
       77  WS-STOCKNUMBER     PIC X(15) VALUE " ".
       77  WS-VATPRICE        PIC 9(6)V99 VALUE 0.
       77  WS-INQUIRY-PROGRAM PIC X(8) VALUE "StDescIq".
       77  WS-OLDPRICE        PIC 9(6)V99.
       77  WS-QUES-MU-GP-PERC PIC X VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1        PIC 99.
       01  WS-STCHANGE-STATUS.
           03  WS-STCHANGE-ST1     PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1  PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-TOOLKIT-ST1      PIC 99.
       Copy "WsDateInfo".
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM FILL-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO STOCK-RECORD.
            MOVE "N" TO NEW-STOCKNO
                        WS-ALL-ENTERED
                        WS-END.
       GET-001.              
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ST-STOCKNUMBER.
            IF F-EXIT-CH = X"0C"
                MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER
                PERFORM START-STOCK
                PERFORM READ-STOCK-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLSC-010
               GO TO GET-000.
            IF F-EXIT-CH = X"05"
                PERFORM READ-STOCK-PREVIOUS
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLSC-010
               GO TO GET-000.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF ST-STOCKNUMBER = 0 OR = "   "
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-STOCK.
            IF NEW-STOCKNO = "Y"
                PERFORM CLSC-010
                PERFORM MOVE-IN-DISCOUNTS
                MOVE "CREATE-DATE" TO F-FIELDNAME
                MOVE 11            TO F-CBFIELDNAME
                MOVE WS-DATE       TO ST-DATE-CREATED SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE  TO F-NAMEFIELD
                MOVE 10            TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM GET-060
                PERFORM GET-100
                
                MOVE ST-STOCKNUMBER TO WS-CATEGORY
                MOVE WS-CATEGORY    TO ST-CATEGORY
                MOVE "CATEGORY"     TO F-FIELDNAME
                MOVE 8              TO F-CBFIELDNAME
                MOVE ST-CATEGORY    TO F-NAMEFIELD
                MOVE 3              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                
                MOVE "ANALYSIS"  TO F-FIELDNAME
                MOVE 8           TO F-CBFIELDNAME
                MOVE "S"         TO F-NAMEFIELD
                MOVE 1           TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA

                GO TO GET-999.
            GO TO GET-005.
       GET-003.
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE ST-STOCKNUMBER TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-005.
            MOVE "DESCRIPTION1" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-DESCRIPTION1 TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESCRIPTION2" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-DESCRIPTION2 TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CATEGORY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE ST-CATEGORY TO F-NAMEFIELD.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE ST-SUPPLIER TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FOREIGNCOST" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE ST-FOREIGNCOST TO F-EDNAMEFIELDFOREIGN.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-FOREIGN.

            MOVE "SUPP-DISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-SUPPLIERDISC TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "CURRENCY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE ST-CURRENCY TO F-NAMEFIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CURR-RATE"      TO F-FIELDNAME.
            MOVE 9                TO F-CBFIELDNAME.
            MOVE ST-CURRENCY-RATE TO F-EDNAMEFIELDVALUE.
            MOVE 9                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-VALUE.

            MOVE "PRICE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE ST-PRICE TO F-EDNAMEFIELDAMOUNT.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "OLDPRICE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE ST-OLDPRICE TO F-EDNAMEFIELDAMOUNT.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.

            COMPUTE WS-VATPRICE ROUNDED =
                 ST-PRICE + ((ST-PRICE * PA-GST-PERCENT) / 100).
            MOVE "VATPRICE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-VATPRICE TO F-EDNAMEFIELDAMOUNT.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "MIN-MU"        TO F-FIELDNAME.
            MOVE 6               TO F-CBFIELDNAME.
             IF WS-QUES-MU-GP-PERC = "N"
               MOVE "MIN M/U %:" TO F-NAMEFIELD
            ELSE
               MOVE "MIN G/P %:" TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PERC-MIN"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE ST-MIN-PERC TO F-EDNAMEFIELDPERC.
            MOVE 6           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-PERC.
 
            MOVE "UNITOFMEASURE" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE ST-UNITOFMEASURE TO F-NAMEFIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-060.
            MOVE "DISCOUNT1" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-DISCOUNT1 TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
 
            MOVE "DISCOUNT2" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-DISCOUNT2 TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
 
            MOVE "DISCOUNT3" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-DISCOUNT3 TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
 
            MOVE "DISCOUNT4" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-DISCOUNT4 TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
 
            MOVE "DISCOUNT5" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-DISCOUNT5 TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
 
            MOVE "DISCOUNT6" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-DISCOUNT6 TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
 
            MOVE "DISCOUNT7" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-DISCOUNT7 TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
 
            MOVE "DISCOUNT8" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-DISCOUNT8 TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
 
            MOVE "DISCOUNT9" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-DISCOUNT9 TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
       GET-070.
            MOVE "AVERAGECOST" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE ST-AVERAGECOST TO F-EDNAMEFIELDAMOUNT.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
 
            MOVE "LASTCOST" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE ST-LASTCOST TO F-EDNAMEFIELDAMOUNT.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
 
            MOVE "BINLOCATION" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE ST-BINLOCATION TO F-NAMEFIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
 
            MOVE "MINBUYQTY" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-MINBUYQTY TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.

            MOVE "DEL-DELAY" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-DEL-DELAY TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.
 
            MOVE "MAXIMUMLEVEL" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-MAXIMUMLEVEL TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.
 
            MOVE "MINIMUMLEVEL" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-MINIMUMLEVEL TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.
 
            MOVE "QTYONHAND" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-QTYONHAND TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.
 
            MOVE "QTYONRESERVE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-QTYONRESERVE TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.
 
            MOVE "QTYONORDER" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE ST-QTYONORDER TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.
 
            MOVE "QTYONB/ORDER" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-QTYONBORDER TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.
 
            MOVE "QTYSTOCKTAKE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-QTY-ST-TAKE TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.
 
            MOVE "CREATE-DATE"   TO F-FIELDNAME
            MOVE 11              TO F-CBFIELDNAME
            MOVE ST-DATE-CREATED TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE    TO F-NAMEFIELD
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
 
            MOVE "LASTPRICECHG"     TO F-FIELDNAME.
            MOVE 12                 TO F-CBFIELDNAME.
            MOVE ST-LASTPRICECHANGE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE       TO F-NAMEFIELD.
            MOVE 10                 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
 
            MOVE "LASTSALEDATE"  TO F-FIELDNAME.
            MOVE 12              TO F-CBFIELDNAME.
            MOVE ST-LASTSALEDATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
 
            MOVE "LASTRECEIPTDATE"  TO F-FIELDNAME.
            MOVE 15                 TO F-CBFIELDNAME.
            MOVE ST-LASTRECEIPTDATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE       TO F-NAMEFIELD.
            MOVE 10                 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
   
            MOVE "LASTORDERDATE"  TO F-FIELDNAME.
            MOVE 13               TO F-CBFIELDNAME.
            MOVE ST-LASTORDERDATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE     TO F-NAMEFIELD.
            MOVE 10               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QTYADJMTD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-QTYADJMTD TO F-EDNAMEFIELDNUMBER.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMBER.

            MOVE "QTYADJYTD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-QTYADJYTD TO F-EDNAMEFIELDNUMBER.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMBER.

            MOVE "QTYADJLAST" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE ST-QTYADJLAST TO F-EDNAMEFIELDNUMBER.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMBER.

            MOVE "QTYRECMTD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-QTYRECMTD TO F-EDNAMEFIELDNUMBER.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMBER.

            MOVE "QTYRECYTD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-QTYRECYTD TO F-EDNAMEFIELDNUMBER.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMBER.

            MOVE "QTYRECLAST" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE ST-QTYRECLAST TO F-EDNAMEFIELDNUMBER.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMBER.
 
            MOVE "SALESUNITMTD" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-SALESUNITMTD TO F-EDNAMEFIELDNUMBER.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMBER.
 
            MOVE "SALESUNITSYTD" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE ST-SALESUNITSYTD TO F-EDNAMEFIELDNUMBER.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMBER.
 
            MOVE "LASTYEARUNIT" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-SALESUNITSLAST TO F-EDNAMEFIELDNUMBER.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMBER.
 
            MOVE "SALESRANDSMTD" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE ST-SALESRANDSMTD TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
 
            MOVE "SALESRANDSYTD" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE ST-SALESRANDSYTD TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
 
            MOVE "LASTYEARSALE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-SALESRANDSLAST TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
 
            MOVE "SALESCOSTMTD" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-SALESCOSTMTD TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
 
            MOVE "SALESCOSTYTD" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-SALESCOSTYTD TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
 
            MOVE "LASTYEARCOST" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-SALESCOSTLAST TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.

            MOVE "ANALYSIS" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE ST-ANALYSIS TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
 
            MOVE "DUTYPERCENT" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE ST-DUTYPERCENT TO F-EDNAMEFIELDDUTY.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-DUTY.
 
            MOVE "SURCHARGE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-SURCHARGE TO F-EDNAMEFIELDDUTY.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-DUTY.
 
            MOVE "DUTYTARIFF" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE ST-DUTYTARIFF TO F-EDNAMEFIELDTARIFF.
            MOVE 8 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-TARIFF.
       GET-100.
            MOVE "PERMIT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE ST-PERMIT TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "DESCRIPTION1" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ST-DESCRIPTION1.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF ST-DESCRIPTION1 = " "
               MOVE "This Field May Not Be Blank, Enter A Character"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-195.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.
        FILL-010.
            MOVE "DESCRIPTION2" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ST-DESCRIPTION2.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-010.
        FILL-015.
            MOVE "CATEGORY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ST-CATEGORY.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF ST-CATEGORY = " "
               MOVE "This Field May Not Be Blank, Enter A Character"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-015.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-015.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-015.
        FILL-020.
            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ST-SUPPLIER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF ST-SUPPLIER = " "
               MOVE "This Field May Not Be Blank, Enter A Character"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
        FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "FOREIGNCOST" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDFOREIGN
                                 ST-FOREIGNCOST.
            PERFORM WRITE-FIELD-FOREIGN.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-025.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-025.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-025.
        FILL-027.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SUPP-DISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 ST-SUPPLIERDISC.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-027.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-027.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-027.
        FILL-028.
            MOVE "CURRENCY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ST-CURRENCY.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-028.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-027.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-028.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-028.
        FILL-029.
            MOVE "CURR-RATE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO ST-CURRENCY-RATE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-029.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-028.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-029.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-029.
        FILL-030.
            MOVE ST-PRICE TO WS-OLDPRICE.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PRICE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNT
                                 ST-PRICE.
            PERFORM WRITE-FIELD-AMOUNT.
            IF WS-OLDPRICE NOT = ST-PRICE
               MOVE WS-OLDPRICE TO ST-OLDPRICE
               MOVE WS-DATE     TO ST-LASTPRICECHANGE.

            COMPUTE WS-VATPRICE ROUNDED =
                 ST-PRICE + ((ST-PRICE * PA-GST-PERCENT) / 100).
            MOVE "VATPRICE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-VATPRICE TO F-EDNAMEFIELDAMOUNT.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF ST-PRICE = 0
               GO TO FILL-030.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-029.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.
        FILL-032.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "OLDPRICE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNT
                                 ST-OLDPRICE.
            PERFORM WRITE-FIELD-AMOUNT.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF ST-PRICE = 0
               GO TO FILL-032.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-032.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-032.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-032.
        FILL-033.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PERC-MIN"   TO F-FIELDNAME.
            MOVE 8            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDPERC
                                ST-MIN-PERC.
            MOVE 6            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-PERC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF ST-MIN-PERC = 0
               GO TO FILL-033.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-033.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-032.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-033.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-033.
        FILL-035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "UNITOFMEASURE" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ST-UNITOFMEASURE.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF ST-UNITOFMEASURE = " "
               GO TO FILL-035.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-035.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-032.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-035.
        FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISCOUNT1" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 ST-DISCOUNT1.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-040.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-035.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-040.
        FILL-045.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISCOUNT2" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 ST-DISCOUNT2.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-045.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-040.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-045.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-045.
        FILL-050.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISCOUNT3" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 ST-DISCOUNT3.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-050.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-045.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-050.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-050.
        FILL-055.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISCOUNT4" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 ST-DISCOUNT4.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-055.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-050.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-055.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-055.
        FILL-060.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISCOUNT5" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 ST-DISCOUNT5.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-060.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-055.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-060.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-060.
        FILL-065.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISCOUNT6" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 ST-DISCOUNT6.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-065.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-060.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-065.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-065.
        FILL-070.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISCOUNT7" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 ST-DISCOUNT7.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-070.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-065.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-070.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-070.
        FILL-075.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISCOUNT8" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 ST-DISCOUNT8.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-075.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-070.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-075.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-075.
        FILL-080.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISCOUNT9" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 ST-DISCOUNT9.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-080.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-075.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-080.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-080.
        FILL-085.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "AVERAGECOST" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNT
                                 ST-AVERAGECOST.
            PERFORM WRITE-FIELD-AMOUNT.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-085.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-080.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-085.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-085.
        FILL-090.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LASTCOST" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNT
                                 ST-LASTCOST.
            PERFORM WRITE-FIELD-AMOUNT.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-090.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-085.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-090.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-090.
        FILL-105.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BINLOCATION" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ST-BINLOCATION.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF ST-BINLOCATION = " "
             MOVE "THIS FIELD MAY NOT BE BLANK, RE-ENTER" TO WS-MESSAGE
             PERFORM ERROR-000
               GO TO FILL-105.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-105.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-090.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-105.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-105.
        FILL-106.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DEL-DELAY" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO ST-DEL-DELAY
                                 F-EDNAMEFIELDANAL.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF ST-DEL-DELAY NOT > 0
               MOVE "NUMBER OF WEEKS MUST BE > ZERO, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-106.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-105.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-106.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-106.
        FILL-108.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "MINBUYQTY" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDINV
                                 ST-MINBUYQTY.
            PERFORM WRITE-FIELD-INV.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF ST-MINBUYQTY = 0
               MOVE "THIS FIELD MUST BE > ZERO, RE-ENTER" TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-108.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-108.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-106.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-108.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-108.
        FILL-110.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "MAXIMUMLEVEL" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDINV
                                 ST-MAXIMUMLEVEL.
            PERFORM WRITE-FIELD-INV.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-110.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-108.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-110.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-110.
        FILL-115.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "MINIMUMLEVEL" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDINV
                                 ST-MINIMUMLEVEL.
            PERFORM WRITE-FIELD-INV.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-115.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-110.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-115.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-115.
        FILL-120.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QTYONHAND" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDINV
                                 ST-QTYONHAND.
            PERFORM WRITE-FIELD-INV.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-120.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-115.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-120.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-120.
        FILL-122.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QTYONRESERVE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDINV
                                 ST-QTYONRESERVE.
            PERFORM WRITE-FIELD-INV.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-122.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-120.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-122.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-122.
        FILL-125.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QTYONORDER" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDINV
                                 ST-QTYONORDER.
            PERFORM WRITE-FIELD-INV.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-125.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-122.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-125.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-125.
        FILL-130.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QTYONB/ORDER" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDINV
                                 ST-QTYONBORDER.
            PERFORM WRITE-FIELD-INV.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-130.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-125.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-130.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-130.
        FILL-132.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QTYSTOCKTAKE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDINV
                                 ST-QTY-ST-TAKE.
            PERFORM WRITE-FIELD-INV.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-132.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-130.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-132.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-132.
        FILL-133.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CREATE-DATE" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-133.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO ST-DATE-CREATED.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-133.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-133.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-132.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-133.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-133.
        FILL-134.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LASTPRICECHG" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-134.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO ST-LASTPRICECHANGE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-134.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-134.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-133.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-134.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-134.
        FILL-135.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LASTSALEDATE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-135.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO ST-LASTSALEDATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-135.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-135.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-134.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-135.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-135.
        FILL-140.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LASTRECEIPTDATE" TO F-FIELDNAME.
            MOVE 15 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-140.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO ST-LASTRECEIPTDATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-140.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-140.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-135.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-140.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-140.
        FILL-142.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LASTORDERDATE" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-142.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO ST-LASTORDERDATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-142.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-142.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-140.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-142.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-142.
        FILL-1144.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QTYADJMTD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUMBER
                                 ST-QTYADJMTD.
            PERFORM WRITE-FIELD-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1144.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-142.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1144.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1144.
        FILL-1146.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QTYADJYTD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUMBER
                                 ST-QTYADJYTD.
            PERFORM WRITE-FIELD-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1146.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1144.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1146.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1146.
        FILL-1148.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QTYADJLAST" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUMBER
                                 ST-QTYADJLAST.
            PERFORM WRITE-FIELD-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1148.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1146.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1148.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1148.
        FILL-144.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QTYRECMTD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUMBER
                                 ST-QTYRECMTD.
            PERFORM WRITE-FIELD-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-144.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1148.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-144.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-144.
        FILL-146.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QTYRECYTD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUMBER
                                 ST-QTYRECYTD.
            PERFORM WRITE-FIELD-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-146.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-144.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-146.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-146.
        FILL-148.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QTYRECLAST" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUMBER
                                 ST-QTYRECLAST.
            PERFORM WRITE-FIELD-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-148.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-146.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-148.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-148.
        FILL-150.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESUNITMTD" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUMBER
                                 ST-SALESUNITMTD.
            PERFORM WRITE-FIELD-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-150.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-148.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-150.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-150.
        FILL-155.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESUNITSYTD" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUMBER
                                 ST-SALESUNITSYTD.
            PERFORM WRITE-FIELD-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-155.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-150.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-155.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-155.
        FILL-160.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LASTYEARUNIT" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUMBER
                                 ST-SALESUNITSLAST.
            PERFORM WRITE-FIELD-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-160.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-155.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-160.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-160.
        FILL-165.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESRANDSMTD" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDSALE
                                 ST-SALESRANDSMTD.
            PERFORM WRITE-FIELD-SALE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-165.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-160.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-165.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-165.
        FILL-170.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESRANDSYTD" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDSALE
                                 ST-SALESRANDSYTD.
            PERFORM WRITE-FIELD-SALE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-170.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-165.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-170.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-170.
        FILL-175.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LASTYEARSALE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDSALE
                                 ST-SALESRANDSLAST.
            PERFORM WRITE-FIELD-SALE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-175.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-170.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-175.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-175.
        FILL-180.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESCOSTMTD" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDSALE
                                 ST-SALESCOSTMTD.
            PERFORM WRITE-FIELD-SALE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-180.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-175.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-180.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-180.
        FILL-185.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESCOSTYTD" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDSALE
                                 ST-SALESCOSTYTD.
            PERFORM WRITE-FIELD-SALE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-185.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-180.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-185.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-185.
        FILL-187.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LASTYEARCOST" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDSALE
                                 ST-SALESCOSTLAST.
            PERFORM WRITE-FIELD-SALE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-187.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-185.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-187.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-187.
        FILL-190.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ANALYSIS" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ST-ANALYSIS.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-190.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-187.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-190.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-190.
        FILL-191.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DUTYPERCENT" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDDUTY
                                 ST-DUTYPERCENT.
            PERFORM WRITE-FIELD-DUTY.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-191.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-190.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-191.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-191.
        FILL-192.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SURCHARGE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDDUTY
                                 ST-SURCHARGE.
            PERFORM WRITE-FIELD-DUTY.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-192.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-191.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-192.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-192.
        FILL-194.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DUTYTARIFF" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 8 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDTARIFF
                                 ST-DUTYTARIFF.
            PERFORM WRITE-FIELD-TARIFF.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-194.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-192.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-194.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-194.
        FILL-195.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PERMIT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ST-PERMIT.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF ST-PERMIT NOT = "Y" AND NOT = "N"
              GO TO FILL-195.
              
            MOVE "Y" TO WS-ALL-ENTERED.
              
            IF NEW-STOCKNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-195.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-NEXT
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM READ-STOCK-PREVIOUS
               PERFORM GET-003 THRU GET-100
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-194.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STOCK-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-195.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-195.
 
            GO TO FILL-001.
       FILL-999.
             EXIT.
      *
       DELETE-STOCK-RECORD SECTION.
       DSR-000.
            IF NEW-STOCKNO = "Y"
               PERFORM RELEASE-STOCK-RECORD
               GO TO DSR-999.
       DSR-005.
            IF ST-QTYONHAND NOT = 0
             OR ST-QTYONRESERVE NOT = 0
              OR ST-QTYONBORDER NOT = 0
               OR ST-QTYONORDER NOT = 0
               MOVE
           "QTY VALUES NOT = ZERO, CAN'T DELETE, 'ESC' TO CLEAR SCREEN."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM RELEASE-STOCK-RECORD
               GO TO DSR-999.
           IF ST-SALESUNITSYTD NOT = 0
            OR ST-SALESUNITSLAST NOT = 0
             OR ST-QTYRECYTD NOT = 0
              OR ST-QTYRECLAST NOT = 0
               OR ST-QTYADJYTD NOT = 0
                OR ST-QTYADJLAST NOT = 0
                 OR ST-SALESCOSTYTD NOT = 0
                  OR ST-SALESCOSTLAST NOT = 0
               MOVE
           "HISTORY NOT = ZERO, CAN'T DELETE, 'ESC' TO CLEAR SCREEN."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM RELEASE-STOCK-RECORD
               GO TO DSR-999.
       DSR-010.
           DELETE STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO DSR-010. 
           IF WS-STOCK-CHANGE NOT = "Y"
             GO TO DSR-900.
           PERFORM WRITE-STOCK-CHANGES.
       DSR-900.
           PERFORM DELETE-OLD-KIT.
       DSR-999.
           EXIT. 
      *
       RELEASE-STOCK-RECORD SECTION.
       REL-000.
           UNLOCK STOCK-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-STOCK-RECORD SECTION.
       RSR-010.
          IF NEW-STOCKNO = "Y"
              GO TO RSR-020.
          REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STOCK-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STOCK-ST1
              GO TO RSR-010.
          GO TO RSR-900.
       RSR-020.
          WRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK RECORD BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
              GO TO RSR-020.
       RSR-900.
          IF WS-STOCK-CHANGE NOT = "Y"
             GO TO RSR-999.
          PERFORM WRITE-STOCK-CHANGES.
       RSR-999.
          EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
             MOVE ST-STOCKNUMBER TO WS-STOCKNUMBER.
             START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-010.
             READ STOCK-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-SCREEN-FORM
                MOVE "Y" TO NEW-STOCKNO
                MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER
                GO TO R-ST-999.
             IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK-ST1
                GO TO R-ST-010.
       R-ST-999.
             EXIT.
      *
       START-STOCK SECTION.
       ST-ST-000.
              MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
              START STOCK-MASTER KEY NOT LESS ST-STOCKNUMBER.
       ST-ST-999.
             EXIT.
      *
       READ-STOCK-NEXT SECTION.
       RSN-005. 
           READ STOCK-MASTER NEXT WITH LOCK
             AT END 
               MOVE 0 TO ST-STOCKNUMBER
                           WS-STOCKNUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "STOCK BUSY ON READ-NEXT-23, PRESS 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RSN-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON READ-NEXT, PRESS 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               PERFORM START-STOCK
               GO TO RSN-005.
           MOVE ST-STOCKNUMBER TO WS-STOCKNUMBER.
           MOVE "N"            TO NEW-STOCKNO.
       RSN-999.
             EXIT.
      *
       READ-STOCK-PREVIOUS SECTION.
       RSPREV-005. 
           READ STOCK-MASTER PREVIOUS WITH LOCK
             AT END 
               MOVE " " TO ST-STOCKNUMBER
                           WS-STOCKNUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSPREV-999.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "STOCK BUSY ON READ-PREV-23, PRESS 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RSPREV-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON READ-PREV, PRESS 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               PERFORM START-STOCK
               GO TO RSPREV-005.
           MOVE ST-STOCKNUMBER TO WS-STOCKNUMBER.
           MOVE "N"            TO NEW-STOCKNO.
       RSPREV-999.
             EXIT.
      *
       DELETE-OLD-KIT SECTION.
       DT-010.
           MOVE 2910 TO POS
           DISPLAY "DELETING TOOLKIT ENTRIES...." AT POS.
           
           MOVE WS-STOCKNUMBER TO TO-TOOLKIT-NUMBER.
           MOVE " "            TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKITS BUSY ON START-DELETE, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO DT-999.
       DT-020.
           READ TOOLKITS NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               GO TO DT-900.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKITS BUSY ON READ-DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO DT-020.
           IF TO-TOOLKIT-NUMBER = WS-STOCKNUMBER
               GO TO DT-050.
           GO TO DT-900.
       DT-050.
           DELETE TOOLKITS
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               GO TO DT-020.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKIT RECORD BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO DT-050.
           GO TO DT-020.
       DT-900.
           PERFORM ERROR1-020.
       DT-999.
           EXIT.
      *
       READ-INVQUES-FILE SECTION.
       RINVQUES-000.
            MOVE 1 TO PA-RECORD.
            MOVE 6 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RINVQUES-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "N" TO WS-STOCK-CHANGE
               GO TO RINVQUES-999.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "READ INV-QUES BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SLPARAMETER-ST1
              GO TO RINVQUES-010.
            MOVE INVQUES-STOCK-CHANGE TO WS-STOCK-CHANGE.
       RINVQUES-999.
            EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           READ PARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO PARAMETER RECORD FOUND, 'ESC' TO END PROG."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SLPARAMETER-ST1
              PERFORM ERROR-MESSAGE
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-005.
            OPEN I-O STOCKCHANGE-MASTER.
            IF WS-STCHANGE-ST1 = 35 OR 49
                OPEN OUTPUT STOCKCHANGE-MASTER.
           
            IF WS-STCHANGE-ST1 NOT = 0
               MOVE 0 TO WS-STCHANGE-ST1
               MOVE "STCHANGE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-006.
            OPEN I-O TOOLKITS.
            IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLFILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.
       OPEN-014.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
           PERFORM READ-INVQUES-FILE.
           PERFORM READ-PARAMETER.
           CLOSE PARAMETER-FILE.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StMastIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER
                  STOCKCHANGE-MASTER
                  TOOLKITS.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "WriteStockChanges".
       Copy "MoveInDiscounts".
       Copy "ClearFormStock".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAnalysis".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldPrice".
       Copy "WriteFieldPerc".
       Copy "WriteFieldInv".
       Copy "WriteFieldDate".
       Copy "WriteFieldDuty".
       Copy "WriteFieldForeign".
       Copy "WriteFieldTariff".
       Copy "WriteFieldAmount".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldSale".
       Copy "WriteFieldNumber".
       Copy "WriteFieldValue".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB
