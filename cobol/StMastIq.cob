        IDENTIFICATION DIVISION.
        PROGRAM-ID. StMastIq.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        REPOSITORY. 
           FUNCTION ALL INTRINSIC.
        SPECIAL-NAMES.
           CRT STATUS IS W-CRTSTATUS.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStCatalogue".
          Copy "SelectStAlternative".
          Copy "SelectStSpecPr".
          Copy "SelectSlParameter".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStCatalogue.
           COPY ChlfdStAlternative.
           COPY ChlfdStPrice.
           COPY ChlfdParam.

       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM     PIC X(8) VALUE "StDescIq".
       77  WS-CATALOGUE-PROGRAM   PIC X(8) VALUE "StCatPIq".
       77  WS-ALTERNATIVE-PROGRAM PIC X(8) VALUE "StAlteIq".
       77  WS-NEW-PROG            PIC X(8) VALUE "StBrchIq".
       77  WS-ACCEPT              PIC X(20) VALUE " ".
       77  WS-NO-OF-READS         PIC 9(2) VALUE 0.
       77  WS-VATPRICE            PIC 9(6)V99 VALUE 0.
       77  WS-PERCENT             PIC S9(3)V99 VALUE 0.
       77  WS-END                 PIC X VALUE " ".
       77  WS-QUES-MU-GP-PERC     PIC X VALUE " ".
       77  WS-STOCKNUMBER         PIC X(15) VALUE " ".
       77  WS-DIS-COSTS           PIC X VALUE "N".
       77  WS-PasswordSaved       PIC X(10).
       77  PSW-SUB1               PIC S9(5) VALUE 0.
       77  PSW-SUB2               PIC S9(5) VALUE 0.
       01  W-READ-KEY             PIC X(11).
       01  W-CRTSTATUS            PIC 9(4) VALUE 0.
       01  WS-PASSWORD-KEY.
           03  WS-PA-KEY          PIC X OCCURS 11.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-STPR-STATUS.
           03  WS-STPR-ST1        PIC 99.
       01  WS-STCAT-STATUS.
           03  WS-STCAT-ST1       PIC 99.
       01  WS-STALT-STATUS.
           03  WS-STALT-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
           
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
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-001.
            MOVE 2710 TO POS
            DISPLAY 
            "ST-CAT=Catalogue, ST-ALT=Alternatives, Blank=Desc Inq."
               AT POS.
            MOVE 2810 TO POS
            DISPLAY
             "PRESS <ALT-F12> TO SWITCH BETWEEN SHOWING COSTS OR NOT."
               AT POS.
            MOVE " " TO WS-END.
            MOVE 0   TO WS-NO-OF-READS.
            MOVE WS-LASTPASSWORD TO WS-PASSWORDSAVED.
            
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD   TO ST-STOCKNUMBER WS-STOCKNUMBER.

      * DISLAY-COSTS Y / N
      *   IF F-EXIT-CH = X"91" = <CODE-SCROLL-UP> CTOS
      *   IF F-EXIT-CH = X"91" = <Alt-F12> linux
           IF F-EXIT-CH = X"91"
              MOVE WS-LASTPASSWORD TO WS-PASSWORDSAVED
              PERFORM CHECK-PASSWORD.
           IF F-EXIT-CH = X"91"
            IF WS-PASSWORD-VALID = "Y"
             IF WS-DIS-COSTS = "N"
                 MOVE "Y" TO WS-DIS-COSTS
             ELSE
                 MOVE "N" TO WS-DIS-COSTS.
           IF F-EXIT-CH = X"91"
            IF WS-PASSWORD-VALID = "N"
             IF WS-DIS-COSTS = "Y"
                 MOVE "N" TO WS-DIS-COSTS.
            IF WS-LASTPASSWORD = " "  
               MOVE WS-PASSWORDSAVED TO WS-LASTPASSWORD.
      * READ-NEXT
            IF F-EXIT-CH = X"0C"
                 PERFORM READ-STOCK-NEXT
                 GO TO GET-003.
      * DISPLAY-STOCK-PHOTO
            IF F-EXIT-CH = X"D6"
                 PERFORM DISPLAY-STOCK-PHOTO
                 GO TO GET-001.
      * READ-PREVIOUS
            IF F-EXIT-CH = X"05"
                 PERFORM READ-STOCK-PREVIOUS
                 GO TO GET-003.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                  PERFORM CLEAR-SCREEN-FORM
                  PERFORM DISPLAY-FORM
                  GO TO GET-001.
            IF ST-STOCKNUMBER = 0 OR = "   "
             IF F-EXIT-CH NOT = X"19"
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            IF ST-STOCKNUMBER = "ST-BRANCH"
                CLOSE STOCK-MASTER
                CALL WS-NEW-PROG USING WS-LINKAGE
                CANCEL WS-NEW-PROG
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            IF ST-STOCKNUMBER = "ST-CAT"
                CLOSE STOCK-MASTER
                CALL WS-CATALOGUE-PROGRAM USING WS-LINKAGE
                CANCEL WS-CATALOGUE-PROGRAM
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            IF ST-STOCKNUMBER = "ST-ALT"
                CLOSE STOCK-MASTER
                CALL WS-ALTERNATIVE-PROGRAM USING WS-LINKAGE
                CANCEL WS-ALTERNATIVE-PROGRAM
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"91"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-001.
            PERFORM READ-STOCK.
            GO TO GET-005.
       GET-003.
            MOVE ST-STOCKNUMBER TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
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

            MOVE "SUPP-DISC"     TO F-FIELDNAME.
            MOVE 9               TO F-CBFIELDNAME.
            MOVE ST-SUPPLIERDISC TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5               TO F-CBFIELDLENGTH.
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

            MOVE "SPRICE" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE STPR-PRICE TO F-EDNAMEFIELDAMOUNT.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.

            COMPUTE WS-VATPRICE ROUNDED =
                 ST-PRICE + ((ST-PRICE * PA-GST-PERCENT) / 100).
            MOVE "VATPRICE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-VATPRICE TO F-EDNAMEFIELDAMOUNT.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.

            IF WS-DIS-COSTS = "N"
               GO TO GET-006.
            MOVE "PU-DESC"       TO F-FIELDNAME.
            MOVE 7               TO F-CBFIELDNAME.
             IF WS-QUES-MU-GP-PERC = "N"
               MOVE "M/U% S/P :" TO F-NAMEFIELD
            ELSE
               MOVE "G/P% S/P :" TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            
            IF WS-QUES-MU-GP-PERC = "N"
               COMPUTE WS-PERCENT ROUNDED =
                  ((ST-PRICE - ST-AVERAGECOST) / ST-AVERAGECOST) * 100
            ELSE
               COMPUTE WS-PERCENT ROUNDED =
                  ((ST-PRICE - ST-AVERAGECOST) / ST-PRICE) * 100.
            MOVE "PERC"     TO F-FIELDNAME.
            MOVE 4          TO F-CBFIELDNAME.
            MOVE WS-PERCENT TO F-EDNAMEFIELDPERCNEG.
            MOVE 7          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-PERC-NEG.

            MOVE "MIN-DESC"      TO F-FIELDNAME.
            MOVE 8               TO F-CBFIELDNAME.
             IF WS-QUES-MU-GP-PERC = "N"
               MOVE "M/U% - D9:" TO F-NAMEFIELD
            ELSE
               MOVE "G/P% - D9:" TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            
            IF WS-QUES-MU-GP-PERC = "N"
               COMPUTE WS-PERCENT ROUNDED = (((ST-PRICE - (ST-PRICE *
                       ST-DISCOUNT9 / 100)) - ST-AVERAGECOST)
                        / ST-AVERAGECOST) * 100
            ELSE
               COMPUTE WS-PERCENT ROUNDED = (((ST-PRICE - (ST-PRICE *
                       ST-DISCOUNT9 / 100)) - ST-AVERAGECOST) 
                       / ST-PRICE) * 100.
            MOVE "PERC-DS"  TO F-FIELDNAME.
            MOVE 7          TO F-CBFIELDNAME.
            MOVE WS-PERCENT TO F-EDNAMEFIELDPERCNEG.
            MOVE 7          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-PERC-NEG.

            MOVE "MIN-MU"        TO F-FIELDNAME.
            MOVE 6               TO F-CBFIELDNAME.
             IF WS-QUES-MU-GP-PERC = "N"
               MOVE "M/U% MIN :" TO F-NAMEFIELD
            ELSE
               MOVE "G/P% MIN :" TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PERC-MIN"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE ST-MIN-PERC TO F-EDNAMEFIELDPERC.
            MOVE 6           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-PERC.
            
            GO TO GET-010.
       GET-006.
            MOVE "PU-DESC"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE "     "    TO F-NAMEFIELD
            MOVE 10         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA
            
            MOVE "PERC"     TO F-FIELDNAME
            MOVE 4          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 7          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "MIN-DESC" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE "     "    TO F-NAMEFIELD
            MOVE 10         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA
            
            MOVE "PERC-DS"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 7          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-010.
            MOVE "UNITOFMEASURE" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE ST-UNITOFMEASURE TO F-NAMEFIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM READ-CATALOGUE-REF.
            MOVE "CAT-PAGE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            IF STCAT-PAGE-NUM = " "
               MOVE "NONE" TO F-NAMEFIELD
            ELSE
               MOVE STCAT-PAGE-NUM TO F-NAMEFIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
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
            IF WS-DIS-COSTS = "Y"
                MOVE ST-AVERAGECOST TO F-EDNAMEFIELDAMOUNT
                MOVE 9              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNT
            ELSE
                MOVE " " TO F-NAMEFIELD
                MOVE 9   TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.

            MOVE "LASTCOST" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            IF WS-DIS-COSTS = "Y"
                MOVE ST-LASTCOST TO F-EDNAMEFIELDAMOUNT
                MOVE 9           TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNT
            ELSE
                MOVE " " TO F-NAMEFIELD
                MOVE 9   TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.

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
            MOVE ST-QTYRECYTD TO F-EDNAMEFIELDNUMBER
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMBER.

            MOVE "QTYRECLAST" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE ST-QTYRECLAST TO F-EDNAMEFIELDNUMBER
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
            MOVE ST-SALESRANDSMTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "SALESRANDSYTD" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE ST-SALESRANDSYTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "LASTYEARSALE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE ST-SALESRANDSLAST TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "SALESCOSTMTD" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            IF WS-DIS-COSTS = "Y"
                MOVE ST-SALESCOSTMTD TO F-EDNAMEFIELDNUM6
                MOVE 11              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-NUMERIC6
            ELSE
                MOVE " " TO F-NAMEFIELD
                MOVE 11  TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESCOSTYTD" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            IF WS-DIS-COSTS = "Y"
                MOVE ST-SALESCOSTYTD TO F-EDNAMEFIELDNUM6
                MOVE 11              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-NUMERIC6
            ELSE
                MOVE " " TO F-NAMEFIELD
                MOVE 11  TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.

            MOVE "LASTYEARCOST" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            IF WS-DIS-COSTS = "Y"
                MOVE ST-SALESCOSTLAST TO F-EDNAMEFIELDNUM6
                MOVE 11               TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-NUMERIC6
            ELSE
                MOVE " " TO F-NAMEFIELD
                MOVE 11  TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.

            MOVE "ANALYSIS"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE ST-ANALYSIS TO F-NAMEFIELD.
            MOVE 1           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DUTYPERCENT" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE ST-DUTYPERCENT TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "SURCHARGE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-SURCHARGE TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "DUTYTARIFF" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE ST-DUTYTARIFF TO F-NAMEFIELD.
            MOVE 8 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PERMIT"  TO F-FIELDNAME.
            MOVE 6         TO F-CBFIELDNAME.
            MOVE ST-PERMIT TO F-NAMEFIELD.
            MOVE 1         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            GO TO GET-001.
       GET-999.
            EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
            MOVE ST-STOCKNUMBER TO WS-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-010.
            READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-SCREEN-FORM          
                MOVE WS-STOCKNUMBER         TO ST-STOCKNUMBER
                MOVE "Enter An Existing St" TO ST-DESCRIPTION1
                MOVE "ock Item, Try Again!" TO ST-DESCRIPTION2
                GO TO R-ST-900.
             IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-000.
       R-ST-900.
             PERFORM READ-SPECIAL-PRICES.
             PERFORM ERROR-020.
       R-ST-999.
             EXIT.
      *
       START-STOCK SECTION.
       ST-ST-000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT LESS ST-STOCKNUMBER
                INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
                PERFORM CLEAR-SCREEN-FORM
                MOVE "Y" TO WS-END.
       ST-ST-999.
             EXIT.
      *
       READ-STOCK-NEXT SECTION.
       R-ST-NX-005. 
           IF WS-END = "Y"
               MOVE "End of NEXT-PAGE seq" TO ST-DESCRIPTION1
               MOVE "uence, RE-ENTER.    " TO ST-DESCRIPTION2
               GO TO R-ST-NX-999.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 0
               GO TO R-ST-NX-900
           ELSE
             MOVE "STOCK BUSY READ-NEXT-START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               PERFORM START-STOCK
               GO TO R-ST-NX-005.
       R-ST-NX-900.
             PERFORM READ-SPECIAL-PRICES.
             PERFORM ERROR-020.
       R-ST-NX-999.
             EXIT.
      *
       READ-STOCK-PREVIOUS SECTION.
       R-ST-PR-005. 
           IF WS-END = "Y"
               MOVE "End of PREVIOUS sequ" TO ST-DESCRIPTION1
               MOVE "ence, RE-ENTER.     " TO ST-DESCRIPTION2
               GO TO R-ST-PR-999.
           READ STOCK-MASTER PREVIOUS
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 0
               GO TO R-ST-PR-900
           ELSE
             MOVE "STOCK BUSY READ-PREV-START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               PERFORM START-STOCK
               GO TO R-ST-PR-005.
       R-ST-PR-900.
             PERFORM READ-SPECIAL-PRICES.
             PERFORM ERROR-020.
       R-ST-PR-999.
             EXIT.
      *
       READ-SPECIAL-PRICES SECTION.
       SPR-000.
           MOVE ST-STOCKNUMBER TO STPR-STOCKNUMBER.
           START STPR-MASTER KEY NOT < STPR-KEY
               INVALID KEY NEXT SENTENCE.
       SPR-005.
           READ STPR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO STPR-PRICE
               GO TO SPR-999.
           IF WS-STPR-ST1 NOT = 0
              Move "SPECIAL PRICES BUSY ON READ, 'ESC' to RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STPR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STPR-ST1
              GO TO SPR-005.
       SPR-999.
           EXIT.
      *
       READ-CATALOGUE-REF SECTION.
       RCREF-000.
           MOVE ST-STOCKNUMBER TO STCAT-STOCKNUMBER.
           START STCAT-MASTER KEY NOT < STCAT-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-STCAT-ST1 NOT = 0
               MOVE 0 TO STCAT-PAGE-NUM
               GO TO RCREF-999.
       RCREF-005.
           READ STCAT-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STCAT-ST1 = 23 OR 35 OR 49
               MOVE 0 TO STCAT-PAGE-NUM
               GO TO RCREF-999.
           IF WS-STCAT-ST1 NOT = 0
              Move "ST-CATALOGUE PAGE BUSY ON READ, 'ESC' to RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STCAT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STCAT-ST1
              GO TO RCREF-005.
       RCREF-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           READ PARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO PARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
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
       READ-INVQUES-FILE SECTION.
       RINVQUES-000.
            MOVE 1 TO PA-RECORD.
            MOVE 6 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RINVQUES-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "N" TO WS-QUES-MU-GP-PERC
               GO TO RINVQUES-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY RINVQUES, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SLPARAMETER-ST1
              GO TO RINVQUES-010.
       RINVQUES-900.
            MOVE INVQUES-MU-GP-PERC TO WS-QUES-MU-GP-PERC.
       RINVQUES-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STOCK-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STOCK-ST1
              GO TO OPEN-000.
       OPEN-005.
            OPEN I-O STPR-MASTER.
            IF WS-STPR-ST1 NOT = 0
              MOVE "STOCK PRICE FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STPR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STPR-ST1
              GO TO OPEN-005.
       OPEN-006.
            OPEN I-O STCAT-MASTER.
            IF WS-STCAT-ST1 NOT = 0
              MOVE "ST-CATALOGUE FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STCAT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STCAT-ST1
              GO TO OPEN-006.
       OPEN-007.
            OPEN I-O STALT-MASTER.
            IF WS-STALT-ST1 NOT = 0
              MOVE "ST-ALTERNATIVES BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STALT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STALT-ST1
              GO TO OPEN-007.
       OPEN-008.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SLPARAMETER-ST1
              GO TO OPEN-008.
           PERFORM READ-INVQUES-FILE.
           PERFORM READ-PARAMETER.
           CLOSE PARAMETER-FILE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StMastIq"      TO F-FORMNAME.
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER
                 STPR-MASTER
                 STCAT-MASTER
                 STALT-MASTER.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ClearFormStock".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumeric6".
       Copy "WriteFieldPercNeg".
       Copy "WriteFieldPerc".
       Copy "WriteFieldInv".
       Copy "WriteFieldNumDec".
       Copy "WriteFieldPrice".
       Copy "WriteFieldQty".
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
      * Copy "ReadKBD".
       Copy "StockSpecPassword".
       Copy "DisplayStockPhoto".
       Copy "CTOSCobolAccept".
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
