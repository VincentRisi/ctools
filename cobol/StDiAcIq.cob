        IDENTIFICATION DIVISION.
        PROGRAM-ID. StDiAcIq.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES.
          CRT STATUS IS W-CRTSTATUS.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectDrMaster".
          Copy "SelectStDiscAcc".
          Copy "SelectStMaster".
          Copy "SelectStCatalogue".
          Copy "SelectStSpecPr".
          Copy "SelectSlParameter".
          Copy "SelectStAlternative".
          Copy "SelectSlSbRep".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStDiscAcc.
           COPY ChlfdStock.
           COPY ChlfdDebtor.
           COPY ChlfdSbRep.
           COPY ChlfdStCatalogue.
           COPY ChlfdStAlternative.
           COPY ChlfdStPrice.
           COPY ChlfdParam.

       WORKING-STORAGE SECTION.
       77  NEW-STPRICENO          PIC X VALUE " ".      
       77  WS-INQUIRY-ACC         PIC X(8) VALUE "DrNameIq".
       77  WS-INQUIRY-PROGRAM     PIC X(8) VALUE "StDescIq".
       77  WS-CATALOGUE-PROGRAM   PIC X(8) VALUE "StCatPIq".
       77  WS-ALTERNATIVE-PROGRAM PIC X(8) VALUE "StAlteIq".
       77  WS-PERCENT             PIC S9(3)V99 VALUE 0.
       77  WS-QUES-MU-GP-PERC     PIC X VALUE " ".
       77  WS-END                 PIC X VALUE " ".      
       77  WS-SALESMAN            PIC X(15) VALUE " ".
       77  WS-NO-OF-READS         PIC 9(2) VALUE 0.
       77  WS-VATPRICE            PIC 9(6)V99 VALUE 0.
       77  WS-STOCKNUMBER         PIC X(15) VALUE " ".
       77  WS-DIS-COSTS           PIC X VALUE "N".
       77  WS-STDISC-NUMBER       PIC X(15) VALUE " ".
       77  WS-STDISC-ACCOUNT      PIC 9(7) VALUE 0.
       77  WS-STDISC-PERCENT      PIC 99V99 VALUE 0.
       77  WS-PASSWORDSAVED       PIC X(10).
       77  PSW-SUB1               PIC S9(5) VALUE 0.
       77  PSW-SUB2               PIC S9(5) VALUE 0.
       01  W-READ-KEY             PIC X.
       01  W-CRTSTATUS            PIC 9(4) value 0.
       01  WS-PASSWORD-KEY.
           03  WS-PA-KEY          PIC X OCCURS 11.
       01  WS-STDESC.
           03  WS-DESC1           PIC X(20) VALUE " ".
           03  WS-DESC2           PIC X(20) VALUE " ".
       01  WS-STDISC-STATUS.
           03  WS-STDISC-ST1      PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1      PIC 99.
       01  WS-SBREP-STATUS.
           03  WS-SBREP-ST1       PIC 99.
       01  WS-STPR-STATUS.
           03  WS-STPR-ST1        PIC 99.
       01  WS-STCAT-STATUS.
           03  WS-STCAT-ST1       PIC 99.
       01  WS-STALT-STATUS.
           03  WS-STALT-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1   PIC 99.
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
       GET-000.
            MOVE 2810 TO POS
            DISPLAY
             "PRESS <ALT-F12> TO SWITCH BETWEEN SHOWING COSTS OR NOT."
               AT POS.
            MOVE 2910 TO POS
            DISPLAY 
            "ST-CAT=Catalogue, ST-ALT=Alternatives, Blank=Desc Inq."
               AT POS.
            MOVE "N" TO NEW-STPRICENO
                        WS-END.
            MOVE WS-LASTPASSWORD TO WS-PASSWORDSAVED.
            IF STDISC-ACCOUNT > 0
                PERFORM READ-DEBTOR
                PERFORM GET-004
                GO TO GET-002.    
       GET-001.          
            MOVE SPACES    TO F-NAMEFIELD.
            MOVE "ACCOUNT" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                  PERFORM CLEAR-SCREEN-FORM
                  PERFORM DISPLAY-FORM
                  GO TO GET-001.
            IF F-NAMEFIELD = " "
             IF F-EXIT-CH NOT = X"0C"
                 CLOSE DEBTOR-MASTER
                 CALL WS-INQUIRY-ACC USING WS-LINKAGE
                 CANCEL WS-INQUIRY-ACC
                 PERFORM CLEAR-SCREEN
                 PERFORM OPEN-006
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-NAMEFIELD > " "
                MOVE SPACES       TO ALPHA-RATE
                MOVE F-NAMEFIELD  TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO STDISC-ACCOUNT.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-DEBTOR.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                GO TO GET-001.
            PERFORM READ-SBREP.
            PERFORM GET-004.
       GET-002.              
            MOVE SPACES        TO F-NAMEFIELD.
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD   TO ST-STOCKNUMBER WS-STOCKNUMBER.
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

            IF F-EXIT-CH = X"0C"
                 PERFORM READ-STOCK-NEXT
                 GO TO GET-003.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-STOCK-PREV
                 GO TO GET-003.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                  PERFORM CLEAR-SCREEN-FORM
                  PERFORM DISPLAY-FORM
                  GO TO GET-000.
            IF F-EXIT-CH = X"01"
                GO TO GET-001.
            IF ST-STOCKNUMBER = 0 OR = "   "
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-005
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            IF ST-STOCKNUMBER = "ST-CAT"
                CLOSE STOCK-MASTER
                CALL WS-CATALOGUE-PROGRAM USING WS-LINKAGE
                CANCEL WS-CATALOGUE-PROGRAM
                PERFORM OPEN-005
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            IF ST-STOCKNUMBER = "ST-ALT"
                CLOSE STOCK-MASTER
                CALL WS-ALTERNATIVE-PROGRAM USING WS-LINKAGE
                CANCEL WS-ALTERNATIVE-PROGRAM
                PERFORM OPEN-005
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B" AND NOT = X"91"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-002.

            PERFORM READ-STOCK.
            GO TO GET-005.
       GET-003.
            PERFORM READ-DEBTOR
            PERFORM READ-SBREP
            PERFORM READ-STOCK.
            
            MOVE "STOCKNUMBER"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE ST-STOCKNUMBER TO F-NAMEFIELD
            MOVE 25             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-004.
            MOVE "ACCOUNT"         TO F-FIELDNAME
            MOVE 7                 TO F-CBFIELDNAME
            MOVE DR-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 7                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "NAME1" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE DR-NAME TO F-NAMEFIELD
            MOVE 40      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SMAN"         TO F-FIELDNAME
            MOVE 4              TO F-CBFIELDNAME
            MOVE WS-SALESMAN    TO F-NAMEFIELD
            MOVE 15             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STATUS"       TO F-FIELDNAME
            MOVE 6              TO F-CBFIELDNAME.
            IF DR-SUPPLY-Y-N = "Y"
               MOVE " "         TO F-NAMEFIELD.
            IF DR-SUPPLY-Y-N = "N"
               MOVE "** ACCOUNT ON HOLD **" TO F-NAMEFIELD.
            IF DR-SUPPLY-Y-N = "S"
               MOVE "* ACCOUNT SUSPENDED *" TO F-NAMEFIELD.
            MOVE 21             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-005.
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "DESC"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-006.
            MOVE "PERCENT"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME.
            IF STDISC-PERCENT > 0
                MOVE STDISC-PERCENT TO F-EDNAMEFIELDAMOUNTDIS
                MOVE 5              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS
            ELSE
                MOVE WS-STDISC-PERCENT TO STDISC-PERCENT
                                          F-EDNAMEFIELDAMOUNTDIS
                MOVE 5                 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS.
 
            MOVE "DATE"       TO F-FIELDNAME
            MOVE 4            TO F-CBFIELDNAME
            IF STDISC-DATE > 0
                MOVE STDISC-DATE  TO SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE TO F-NAMEFIELD
                MOVE 10           TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
            ELSE
                MOVE " "          TO F-NAMEFIELD
                MOVE 10           TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.

            IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "THIS IS NOT A VALID STOCK-NUMBER" TO WS-MESSAGE
               PERFORM ERROR-000.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "THIS IS NOT A VALID ACCOUNT-NUMBER" TO WS-MESSAGE
               PERFORM ERROR1-000.

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
               GO TO GET-007.
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
            
            GO TO GET-010.
       GET-007.
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
                MOVE 9 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNT
            ELSE
                MOVE " " TO F-NAMEFIELD
                MOVE 9 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.

            MOVE "LASTCOST" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            IF WS-DIS-COSTS = "Y"
                MOVE ST-LASTCOST TO F-EDNAMEFIELDAMOUNT
                MOVE 9 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNT
            ELSE
                MOVE " " TO F-NAMEFIELD
                MOVE 9 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.

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
 
            MOVE "CREATE-DATE"    TO F-FIELDNAME
            MOVE 11               TO F-CBFIELDNAME
            MOVE ST-DATE-CREATED  TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE     TO F-NAMEFIELD
            MOVE 10               TO F-CBFIELDLENGTH
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

            MOVE "ANALYSIS"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE ST-ANALYSIS TO F-NAMEFIELD.
            MOVE 1           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ANAL-DESC" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME.
            IF ST-ANALYSIS = "D"
                MOVE "FLAGGED FOR DELETION" TO F-NAMEFIELD.
            IF ST-ANALYSIS = "S"
                MOVE "SPECIAL ORDER ITEM  " TO F-NAMEFIELD.
            IF ST-ANALYSIS NOT = "S" AND NOT = "D"
                MOVE "                    " TO F-NAMEFIELD.
            MOVE 20          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-200.
            IF DR-DISCOUNT-CODE = " " OR = "0"
                PERFORM CLEAR-DISCOUNT-TAG
                GO TO GET-205.
                
            MOVE DR-DISCOUNT-CODE TO SUB-1
            MOVE SUB-1            TO F-INDEX.

            MOVE "DISCNUM"   TO F-FIELDNAME.
            MOVE 7           TO F-CBFIELDNAME.
            MOVE "*"         TO F-NAMEFIELD.
            MOVE 1           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-205.
            PERFORM CHECK-DATE-LAST-SOLD.
 
            GO TO GET-002.
       GET-999.
            EXIT.
      *
       CLEAR-DISCOUNT-TAG SECTION.
       CDT-005.
            MOVE 0 TO SUB-1 F-INDEX.
       CDT-010.
            IF SUB-1 < 9
               ADD 1 TO SUB-1 F-INDEX
            ELSE
               GO TO CDT-999.
               
            MOVE "DISCNUM"   TO F-FIELDNAME.
            MOVE 7           TO F-CBFIELDNAME.
            MOVE " "         TO F-NAMEFIELD.
            MOVE 1           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            
            GO TO CDT-010.
       CDT-999.
            EXIT.
      *
       READ-STDISC SECTION.
       R-STDISC-000.
             MOVE WS-STOCKNUMBER    TO STDISC-STOCKNUMBER.
             MOVE DR-ACCOUNT-NUMBER TO STDISC-ACCOUNT.
             START STDISC-MASTER KEY NOT < STDISC-KEY
                  INVALID KEY NEXT SENTENCE.
       R-STDISC-010.
             READ STDISC-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STDISC-ST1 = 23 OR 35 OR 49
                MOVE 0 TO STDISC-PERCENT
                          STDISC-DATE
                GO TO R-STDISC-999.
             IF WS-STDISC-ST1 NOT = 0
                MOVE "STDISC BUSY ON READ, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-STDISC-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-STDISC-ST1
                 GO TO R-STDISC-010.
       R-STDISC-999.
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
                MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER
                MOVE "Enter An Existing St" TO ST-DESCRIPTION1
                MOVE "ock Item, Try Again!" TO ST-DESCRIPTION2
                GO TO R-ST-999.
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
             PERFORM READ-STDISC.
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
               MOVE "STOCK FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               PERFORM START-STOCK
               GO TO R-ST-NX-005.
       R-ST-NX-900.
             PERFORM READ-SPECIAL-PRICES.
             PERFORM READ-STDISC.
             PERFORM ERROR-020.
       R-ST-NX-999.
             EXIT.
      *
       READ-STOCK-PREV SECTION.
       R-ST-PREV-005. 
           IF WS-END = "Y"
               MOVE "End of NEXT-PAGE seq" TO ST-DESCRIPTION1
               MOVE "uence, RE-ENTER.    " TO ST-DESCRIPTION2
               GO TO R-ST-PREV-999.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 0
               GO TO R-ST-PREV-900
           ELSE
               MOVE "STOCK FILE BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               PERFORM START-STOCK
               GO TO R-ST-PREV-005.
       R-ST-PREV-900.
             PERFORM READ-SPECIAL-PRICES.
             PERFORM READ-STDISC.
             PERFORM ERROR-020.
       R-ST-PREV-999.
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
              Move "SPECIAL PRICES BUSY ON READ, 'ESC' to RETRY."
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
               MOVE " " TO STCAT-PAGE-NUM
               GO TO RCREF-999.
       RCREF-005.
           READ STCAT-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STCAT-ST1 = 23 OR 35 OR 49
               MOVE " " TO STCAT-PAGE-NUM
               GO TO RCREF-999.
           IF WS-STCAT-ST1 NOT = 0
              Move "ST-CATALOGUE PAGE BUSY ON READ, 'ESC' to RETRY."
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
       CHECK-DATE-LAST-SOLD SECTION.
       CDLS-005.
           IF ST-QTYONHAND > 0
           OR ST-QTYONRESERVE > 0
               GO TO CDLS-999.
                      
           MOVE WS-DATE TO    WS-AGE-DATE.
           IF WS-AGE-MM > 3
              SUBTRACT 3 FROM WS-AGE-MM
           ELSE
              ADD 12       TO WS-AGE-MM
              SUBTRACT 1 FROM WS-AGE-YY.
           IF ST-LASTPRICECHANGE NOT > WS-AGE-DATE
              GO TO CDLS-010
           ELSE
              GO TO CDLS-999.
       CDLS-010.
           MOVE WS-DATE TO    WS-AGE-DATE.
           IF WS-AGE-MM > 3
              SUBTRACT 3 FROM WS-AGE-MM
           ELSE
              ADD 12       TO WS-AGE-MM
              SUBTRACT 1 FROM WS-AGE-YY.

           IF ST-LASTSALEDATE NOT > WS-AGE-DATE
             MOVE "THIS ITEM WAS PREV SOLD OVER 3 MONTHS AGO & PRICES" &
             "/COSTS MAY HAVE CHANGED." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE 
             "PLEASE CHECK PRICES & COSTS BEFORE QUOTING NEW CUSTOMER."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
       CDLS-999.
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
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       READ-DEBTOR SECTION.
       RD-000.
             MOVE STDISC-ACCOUNT TO DR-ACCOUNT-NUMBER.
             START DEBTOR-MASTER KEY NOT < DR-KEY
                INVALID KEY NEXT SENTENCE.
       RD-010.
             READ DEBTOR-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE "Enter An Existing Debtor Account Number."
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-999.
             IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTORS BUSY ON READ, 'ESC' TO RETRY."
                  TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-010.
       RD-999.
             EXIT.
      *
       READ-SBREP SECTION.
       RSB-005.
           IF DR-SALESMAN = " " OR = "0"
              MOVE " " TO WS-SALESMAN
              GO TO RSB-999.
       RSB-020.
           MOVE DR-SALESMAN TO SBREP-REP.
           START SBREP-MASTER KEY NOT < SBREP-KEY
              INVALID KEY NEXT SENTENCE.
       RSB-030.
           READ SBREP-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-SBREP-ST1 = 23 OR 35 OR 49
              MOVE "  " TO WS-SALESMAN
              GO TO RSB-999.
           IF WS-SBREP-ST1 NOT = 0
               MOVE "SBREP FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SBREP-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SBREP-ST1
               GO TO RSB-030.
               
           MOVE SBREP-REPNAME TO WS-SALESMAN.
       RSB-999.
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
               MOVE "PARAMETER BUSY RINVQUES ON READ, 'ESC' TO RETRY."
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
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO STDISC-STOCKNUMBER.
             MOVE 0   TO STDISC-PERCENT
      *                   STDISC-ACCOUNT
                         STDISC-DATE.
       CLSC-500.
             IF WS-STDISC-ST1 = 51
                  UNLOCK STDISC-MASTER.
       CLSC-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STDISC-MASTER.
            IF WS-STDISC-ST1 NOT = 0
               MOVE 0 TO WS-STDISC-ST1
               MOVE "STOCK SPEC-DISC BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-001.
            OPEN I-O STPR-MASTER.
            IF WS-STPR-ST1 NOT = 0
               MOVE 0 TO WS-STPR-ST1
               MOVE "STOCK PRICE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-001.
       OPEN-002.
            OPEN I-O STCAT-MASTER.
            IF WS-STCAT-ST1 NOT = 0
               MOVE 0 TO WS-STCAT-ST1
               MOVE "ST-CATALOGUE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
       OPEN-003.
            OPEN I-O STALT-MASTER.
            IF WS-STALT-ST1 NOT = 0
               MOVE 0 TO WS-STALT-ST1
               MOVE "ST-ALTERNATIVES BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-003.
       OPEN-005.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.
       OPEN-006.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.
       OPEN-007.
           OPEN I-O SBREP-MASTER.
           IF WS-SBREP-ST1 NOT = 0 
              MOVE 0 TO WS-SBREP-ST1
              MOVE "SBREP FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-007.
       OPEN-008.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-008.
           PERFORM READ-INVQUES-FILE.
           PERFORM READ-PARAMETER.
           CLOSE PARAMETER-FILE.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StDiAcIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STDISC-MASTER
                  STOCK-MASTER
                  DEBTOR-MASTER
                  SBREP-MASTER
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
       Copy "GetSystemY2KDate".
       Copy "ReadKBD".
       Copy "StockSpecPassword".
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
