        IDENTIFICATION DIVISION.
        PROGRAM-ID. StNoCopy.
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
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStockChanges.
           COPY ChlfdParam.

       WORKING-STORAGE SECTION.
       77  NEW-STOCKNO        PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-VALID           PIC X VALUE " ".      
       77  WS-INQUIRY-PROGRAM PIC X(8) VALUE "StDescIq".
       77  WS-STOCKNUMBER     PIC X(15) VALUE " ".
       77  WS-NEWSTOCKNUMBER  PIC X(15) VALUE " ".
       77  WS-VATPRICE        PIC 9(6)V99 VALUE 0.
       77  WS-OLDSTOCKNUMBER  PIC X(15) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-STCHANGE-STATUS.
           03  WS-STCHANGE-ST1    PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       Copy "WsDateInfo".
      *
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
           PERFORM ERROR1-020
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO STOCK-RECORD.
            MOVE "N" TO NEW-STOCKNO
                        WS-VALID
                        WS-END.
       GET-001.              
            MOVE "OLDSTOCK" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ST-STOCKNUMBER.
            IF F-EXIT-CH = X"0C"
      *          MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER
      *          PERFORM START-STOCK
                PERFORM READ-STOCK-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-SCREEN-FORM
               GO TO GET-000.
            IF F-EXIT-CH = X"05"
                PERFORM READ-STOCK-PREVIOUS
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-SCREEN-FORM
               GO TO GET-000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-SCREEN-FORM
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
               MOVE "YOU CAN ONLY COPY AN EXISTING STOCK NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-000.
            GO TO GET-005.
       GET-003.
            MOVE "OLDSTOCK" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
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
 
            MOVE "UNITOFMEASURE" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE ST-UNITOFMEASURE TO F-NAMEFIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
 
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

            MOVE "DEL-DELAY" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-DEL-DELAY TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.
 
            MOVE "MINBUYQTY" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-MINBUYQTY TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.
 
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
            MOVE ST-QTYRECMTD TO F-EDNAMEFIELDQTY.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-QTY.

            MOVE "QTYRECYTD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-QTYRECYTD TO F-EDNAMEFIELDQTY.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-QTY.

            MOVE "QTYRECLAST" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE ST-QTYRECLAST TO F-EDNAMEFIELDQTY.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-QTY.

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

            MOVE "PERMIT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE ST-PERMIT TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-500.
            MOVE ST-STOCKNUMBER TO WS-OLDSTOCKNUMBER.
            MOVE "NEWSTOCK" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"01" OR = X"07"
               PERFORM CLEAR-SCREEN-FORM
               PERFORM DISPLAY-FORM
               GO TO GET-000.
            IF F-NAMEFIELD = "    "
               MOVE "YOU MUST ENTER A NEW STOCK NUMBER" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-500.
            MOVE F-NAMEFIELD TO ST-STOCKNUMBER WS-NEWSTOCKNUMBER.
            MOVE "Y" TO NEW-STOCKNO.
            PERFORM WRITE-STOCK-RECORD.
            IF WS-VALID = "Y"
                MOVE "THIS TRY HAS BEEN ABORTED, TRY AGAIN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM CLEAR-SCREEN-FORM
                PERFORM DISPLAY-FORM
                GO TO GET-999.
            MOVE WS-OLDSTOCKNUMBER TO ST-STOCKNUMBER.
            PERFORM READ-STOCK.
       GET-999.
            EXIT.
      *
       WRITE-STOCK-RECORD SECTION.
       RSR-005.
          PERFORM CLSC-010.
          PERFORM CLEAR-FIELDS.
       RSR-020.
          WRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STOCK-ST1 NOT = 0
              MOVE "YOUR NEW STOCKNUMBER IS ALREADY A VALID NUMBER."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               MOVE "Y" TO WS-VALID
               GO TO RSR-999.
       RSR-900.
           IF INVQUES-STOCK-CHANGE NOT = "Y"
             GO TO RSR-999.
           PERFORM WRITE-STOCK-CHANGES.
       RSR-999.
          EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 0   TO ST-MAXIMUMLEVEL
                         ST-MINIMUMLEVEL
                         ST-QTYONHAND
                         ST-QTYONRESERVE
                         ST-QTYONORDER
                         ST-QTYONBORDER
                         ST-QTY-ST-TAKE
                         ST-LASTPRICECHANGE
                         ST-LASTSALEDATE
                         ST-LASTRECEIPTDATE
                         ST-LASTORDERDATE
                         ST-QTYADJMTD
                         ST-QTYADJYTD
                         ST-QTYADJLAST
                         ST-QTYRECMTD
                         ST-QTYRECYTD
                         ST-QTYRECLAST
                         ST-SALESUNITMTD
                         ST-SALESUNITSYTD
                         ST-SALESUNITSLAST
                         ST-SALESRANDSMTD
                         ST-SALESRANDSYTD
                         ST-SALESRANDSLAST
                         ST-SALESCOSTMTD
                         ST-SALESCOSTYTD
                         ST-SALESCOSTLAST.
             MOVE WS-DATE TO ST-DATE-CREATED.
       CF-999.
           EXIT.
      *
       WRITE-STOCK-CHANGES SECTION.
       WSTCH-000.
             MOVE ST-STOCKNUMBER TO STCH-STOCKNUMBER.
             START STOCKCHANGE-MASTER KEY NOT < STCH-KEY
               INVALID KEY NEXT SENTENCE.
       WSTCH-005.
             READ STOCKCHANGE-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STCHANGE-ST1 = 23 OR 35 OR 49
                GO TO WSTCH-006.
             IF WS-STCHANGE-ST1 NOT = 0
                MOVE "STOCKCHANGE BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STCHANGE-ST1
                GO TO WSTCH-005.
       WSTCH-006.
          MOVE ST-DESCRIPTION1     TO STCH-DESCRIPTION1
          MOVE ST-DESCRIPTION2     TO STCH-DESCRIPTION2
          MOVE ST-CATEGORY         TO STCH-CATEGORY
          MOVE ST-SUPPLIER         TO STCH-SUPPLIER
          MOVE ST-FOREIGNCOST      TO STCH-FOREIGNCOST
          MOVE ST-PRICE            TO STCH-PRICE
          MOVE ST-UNITOFMEASURE    TO STCH-UNITOFMEASURE
          MOVE ST-DISCOUNT1        TO STCH-DISCOUNT1
          MOVE ST-DISCOUNT2        TO STCH-DISCOUNT2
          MOVE ST-DISCOUNT3        TO STCH-DISCOUNT3
          MOVE ST-DISCOUNT4        TO STCH-DISCOUNT4
          MOVE ST-DISCOUNT5        TO STCH-DISCOUNT5
          MOVE ST-DISCOUNT6        TO STCH-DISCOUNT6
          MOVE ST-DISCOUNT7        TO STCH-DISCOUNT7
          MOVE ST-DISCOUNT8        TO STCH-DISCOUNT8
          MOVE ST-DISCOUNT9        TO STCH-DISCOUNT9
          MOVE ST-MINBUYQTY        TO STCH-MINBUYQTY
          MOVE ST-ANALYSIS         TO STCH-ANALYSIS
          MOVE ST-DUTYPERCENT      TO STCH-DUTYPERCENT
          MOVE ST-DUTYTARIFF       TO STCH-DUTYTARIFF
          MOVE ST-SURCHARGE        TO STCH-SURCHARGE
          MOVE ST-PERMIT           TO STCH-PERMIT.
          MOVE "N"                 TO STCH-TYPE-OF-CHANGE.
       WSTCH-010.
           IF WS-STCHANGE-ST1 = 23 OR 35 OR 49
              GO TO WSTCH-020.
           REWRITE STOCKCHANGE-RECORD
              INVALID KEY NEXT SENTENCE.
           IF WS-STCHANGE-ST1 NOT = 0
              MOVE "STOCKCHANGE BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STCHANGE-ST1
                GO TO WSTCH-010.
          GO TO WSTCH-999.
       WSTCH-020.
          WRITE STOCKCHANGE-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STCHANGE-ST1 NOT = 0
              MOVE "STOCKCHANGE BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STCHANGE-ST1
                GO TO WSTCH-020.
       WSTCH-999.
           EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
             MOVE ST-STOCKNUMBER TO WS-STOCKNUMBER.
             START STOCK-MASTER KEY NOT < ST-KEY
                 INVALID KEY NEXT SENTENCE.
       R-ST-010.
             READ STOCK-MASTER
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
             MOVE "N" TO NEW-STOCKNO.
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
           READ STOCK-MASTER NEXT
             AT END 
               MOVE " " TO ST-STOCKNUMBER
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
            MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
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
       READ-INVQUES-FILE SECTION.
       RINVQUES-000.
            MOVE 1 TO PA-RECORD.
            MOVE 6 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RINVQUES-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "N" TO INVQUES-STOCK-CHANGE
               GO TO RINVQUES-999.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SLPARAMETER-ST1
              GO TO RINVQUES-010.
       RINVQUES-999.
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
            IF WS-STCHANGE-ST1 NOT = 0
               MOVE 0 TO WS-STCHANGE-ST1
               MOVE "STCHANGE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-014.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY." 
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
           PERFORM READ-INVQUES-FILE.
           CLOSE PARAMETER-FILE.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StNoCopy"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER
                  STOCKCHANGE-MASTER.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ClearFormStock".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumber".
       Copy "WriteFieldForeign".
       Copy "WriteFieldInv".
       Copy "WriteFieldTariff".
       Copy "WriteFieldNumNeg".
       Copy "WriteFieldQty".
       Copy "WriteFieldDate".
       Copy "WriteFieldDuty".
       Copy "WriteFieldAmount".
       Copy "WriteFieldAmount1".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldSale".
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
      * END-OF-JOB
