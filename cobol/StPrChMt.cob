        IDENTIFICATION DIVISION.
        PROGRAM-ID. StPrChMt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStChanges".
          Copy "SelectStNewPrices".
          Copy "SelectSlParameter".
          Copy "SelectCrCurrency".
          Copy "SelectSlDaily".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStockChanges.
           COPY ChlfdStNewPrice.
           COPY ChlfdParam.
           COPY ChlfdCrCurr.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-STOCK-CHANGED     PIC X VALUE " ".
       77  WS-SELECTED          PIC 99 VALUE 0.
       77  WS-REDUCE            PIC X VALUE " ".
       77  WS-HARDWARE          PIC X VALUE " ".
       77  WS-MIN-MU            PIC X(7) VALUE " ".
       77  WS-WRITE-PRICELIST   PIC X VALUE " ".
       77  WS-FACTOR            PIC 9(3)V99999 VALUE 0.
       77  WS-CURRENCY          PIC 9(3)V99999 VALUE 0.
       77  WS-INDFACTOR         PIC 9(3)V99999 VALUE 0.
       77  WS-MINMARGIN         PIC 9(3)V999 VALUE 0.
       77  WS-MARGIN            PIC 9(3)V999 VALUE 0.
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-ONHANDZERO        PIC X VALUE " ".
       77  WS-PRICE-SAVE        PIC 9(6)V99 VALUE 0.
       77  WS-AVERAGECOST       PIC 9(6)V99 VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5)V99 VALUE 0.
       77  WS-MONTH             PIC 9 VALUE 0.
       77  WS-DATE-ACCEPT       PIC X(10) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1        PIC 99.
       01  WS-STNWPR-STATUS.
           03  WS-STNWPR-ST1       PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-STCHANGE-STATUS.
           03  WS-STCHANGE-ST1     PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1  PIC 99.
       01  WS-CURRENCY-STATUS.
           03  WS-CURRENCY-ST1     PIC 99.
       01  WS-DATE-ENTER.
           03  WS-YYE           PIC 9999.
           03  WS-MME           PIC 99.
           03  WS-DDE           PIC 99.
       01  WS-CALC-DATE.
           03  WS-YYC           PIC 9999.
           03  WS-MMC           PIC 99.
           03  WS-DDC           PIC 99.
       01  WS-CURRENCY-NAMES.
         02  WS-CURRENCY-FILE OCCURS 20.
           03  WS-CURRENCY-TYPE    PIC X(5).
           03  WS-CURRENCY-VALUE   PIC 9(3)V9(5).
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
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
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "                             " TO F-NAMEFIELD.
            MOVE "ANSWER1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 GO TO GET-999.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = " "
                GO TO GET-000.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-SELECTED.
            IF WS-SELECTED = 1 OR = 2 OR = 3 OR = 4 OR = 5
                        OR = 6 OR = 7 OR = 8 OR = 9 OR = 10
                        OR = 11
                GO TO GET-010.
            MOVE "WHAT WILL IT BE; ANSWER BETWEEN 1 & 11."
            TO WS-MESSAGE
            PERFORM ERROR-MESSAGE.
            GO TO GET-000.
       GET-010.
            IF WS-SELECTED = 4 OR = 8 OR = 10
               MOVE 2610 TO POS
               DISPLAY
            "NB! THE DUTY% & SURCHARGE% WILL BE ADDED TO THE FACTOR."
               AT POS.
            IF WS-SELECTED = 5
               MOVE 2610 TO POS
               DISPLAY
            "NB! NO DUTIES WILL BE ADDED TO THE FACTOR."  AT POS.
            MOVE "                             " TO F-NAMEFIELD.
            MOVE "RANGE1" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-000.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-RANGE1.
            IF WS-RANGE1 = " "
                GO TO GET-010.
       GET-020.
            MOVE "                             " TO F-NAMEFIELD.
            MOVE "RANGE2" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-010.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-RANGE2.
            IF WS-RANGE2 = " "
                GO TO GET-020.
       GET-025.
            IF WS-SELECTED = 8
               MOVE 2915 TO POS
               DISPLAY
            "The factor is the ON-COST % PLUS M/UP % in FACTOR form."
                AT POS.
            IF WS-SELECTED = 10
               MOVE 2915 TO POS
               DISPLAY
           "The factor is the ON-COST % to Get the Cost in FACTOR form."
                AT POS.
               
            MOVE "                             " TO F-NAMEFIELD
            MOVE "FACTOR" TO F-FIELDNAME
            MOVE 6 TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-020.
            MOVE 9 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-FACTOR
                                 F-EDNAMEFIELDVALUE.
            IF WS-FACTOR = 0
              MOVE "DON'T BOTHER ME WITH A ZERO CHANGE!!!"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE 
              GO TO GET-030.
            MOVE 9 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-VALUE.
       GET-026.
           PERFORM ERROR1-020
           PERFORM CLEAR-010.
      *     PERFORM GET-032.
           IF WS-SELECTED = 9
              GO TO GET-027.
           IF WS-SELECTED = 10
              GO TO GET-030.
           IF WS-SELECTED = 1 OR = 3 OR = 6 OR = 11
              GO TO GET-029.
           MOVE " " TO WS-REDUCE
           MOVE 2510 TO POS
           DISPLAY
           "If the CALCULATED price is LOWER than the OLD PRICE" AT POS
           MOVE 2615 TO POS
           DISPLAY " should the PRICE be REDUCED, Yes Or No [ ]" AT POS
           MOVE 2656 TO POS
           ACCEPT WS-REDUCE AT POS.
           IF W-ESCAPE-KEY = 4
                GO TO GET-025.
           IF WS-REDUCE NOT = "Y" AND NOT = "N"
               MOVE "THE ANSWER MUST BE YES OR NO" TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-026.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-027
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-026.
       GET-027.
           IF WS-SELECTED = 10
               PERFORM GET-031 THRU GET-032.
           PERFORM CLEAR-010.
           MOVE " " TO WS-MIN-MU
           MOVE 2510 TO POS
           DISPLAY
           "Enter the MINIMUM Percentage MARKUP below which the MARGIN"
           AT POS
           MOVE 2741 TO POS
           DISPLAY "FORMAT = 999.999" AT POS
           MOVE 2615 TO POS
           DISPLAY " should not GO, Blank for unused: [       ]"
           AT POS
           ADD 35 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 23        TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-MIN-MU.

           MOVE WS-MIN-MU TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-MINMARGIN.
           IF W-ESCAPE-KEY = 4
                GO TO GET-025.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-029
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-027.
       GET-029.
           IF WS-SELECTED = 10
               PERFORM GET-031 THRU GET-032.
           PERFORM CLEAR-010.
           IF WS-SELECTED = 6 OR = 11
              MOVE "U" TO WS-WRITE-PRICELIST
              GO TO GET-030.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE " " TO WS-WRITE-PRICELIST
           MOVE 2510 TO POS
           DISPLAY "W=Write Pricelist, U=Update StockMaster File : [ ]"
           AT POS
           ADD 48 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 22        TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-WRITE-PRICELIST.

           IF W-ESCAPE-KEY = 4
            IF WS-SELECTED = 1 OR = 3 OR = 6
                GO TO GET-025
            ELSE
                GO TO GET-027.
           IF WS-WRITE-PRICELIST NOT = "U" AND NOT = "W"
               MOVE "THE ANSWER MUST BE 'U' OR 'W'" TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-029.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-029.
       GET-030.
           IF WS-SELECTED NOT = 10 AND NOT = 11
              GO TO GET-033.
       GET-031.
           MOVE " " TO WS-MESSAGE
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS
           DISPLAY "UPDATE ONLY WHERE ON HAND > 0; NO OR YES : [ ]"
             AT POS
           ADD 44 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 22        TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ONHANDZERO.

           IF W-ESCAPE-KEY = 4
               GO TO GET-026.
           IF WS-ONHANDZERO NOT = "N" AND NOT = "Y"
              GO TO GET-031.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-032
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-031.
       GET-032.
           MOVE 0 TO WS-DATE-ENTER.
           MOVE 2510 TO POS.
           DISPLAY "ENTER THE DATE STOCK WAS SOLD SINCE: [          ]"
               AT POS.
           MOVE 2610 TO POS.
           DISPLAY "                    Enter the DATE as DD/MM/YYYY"
             AT POS.
           MOVE 2710 TO POS.
           DISPLAY "LEAVE BLANK TO UPDATE ALL ITEMS." AT POS.
           MOVE 2548 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 22        TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATE-ACCEPT.
           
           IF WS-DATE-ACCEPT = " "
               GO TO GET-033.

           IF W-ESCAPE-KEY = 4
               GO TO GET-031.
           MOVE WS-DATE-ACCEPT TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO GET-032.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE 2548 TO POS.
           DISPLAY DISPLAY-DATE AT POS.
      *     MOVE DISPLAY-DATE TO H-DATE.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-DATE-ENTER.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO GET-032.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-033
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-032.
       GET-033.
            MOVE " " TO WS-MESSAGE
            MOVE 2510 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 2610 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 2710 TO POS
            DISPLAY WS-MESSAGE AT POS.
       GET-035.
            MOVE 2510 TO POS.
            MOVE " " TO WS-ANSWER
            DISPLAY "H=HARDWARE ONLY, N=NO HARDWARE, A=ALL    : [ ]"
                AT POS
            ADD 44 TO POS
           
           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 22        TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-HARDWARE.
           
            IF W-ESCAPE-KEY = 4
                GO TO GET-026.
            IF WS-HARDWARE NOT = "A" AND NOT = "H" AND NOT = "N"
               GO TO GET-035.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-036
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-035.
       GET-036.
            MOVE 2510 TO POS.
            MOVE " " TO WS-ANSWER
            DISPLAY "IS EVERYTHING CORRECT ? ENTER Y=Yes, N=No. [ ]"
                AT POS
            ADD 44 TO POS
           
           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 22        TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

            IF W-ESCAPE-KEY = 4
                GO TO GET-030.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-037
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-036.
       GET-037.
            MOVE " " TO WS-MESSAGE
            MOVE 2510 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 2725 TO POS
            DISPLAY WS-MESSAGE AT POS.
            IF WS-ANSWER = "Y"
               GO TO GET-040.
            IF WS-ANSWER = "N"
               GO TO GET-050.
            MOVE "TRY AND GIVE ME AN UNDERSTANDABLE ANSWER!!!"
               TO WS-MESSAGE
            PERFORM ERROR-MESSAGE
            GO TO GET-036.
       GET-040.
            MOVE 2725 TO POS.
            DISPLAY "Update in progress.........." AT POS.
            IF WS-SELECTED = 8 OR = 10
               MOVE 2910 TO POS
               DISPLAY "Reading Currencies......" AT POS
               MOVE 1 TO SUB-1
               PERFORM READ-CURRENCIES.
               
            PERFORM ERROR1-020.
               
            PERFORM UPDATE-STOCK.
            MOVE " " TO WS-ANSWER
                        WS-RANGE1
                        WS-RANGE2
            MOVE 0   TO WS-FACTOR
                        WS-SELECTED.
       GET-050.
            MOVE " " TO WS-MESSAGE
            MOVE 2510 TO POS
            DISPLAY WS-MESSAGE AT POS.
       GET-999.
            EXIT.
      *
       UPDATE-STOCK SECTION.
       UPST-000.
            MOVE WS-RANGE1 TO ST-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 NOT = 0
               GO TO UPST-999.
       UPST-010.
            READ STOCK-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-STOCK-ST1 = 10
               GO TO UPST-999.
            IF WS-STOCK-ST1 NOT = 0
             MOVE "STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO UPST-010.
            MOVE " " TO WS-MESSAGE.
            PERFORM ERROR-020.
            
           MOVE 2620 TO POS
           DISPLAY "StockNumber Being Read:" AT POS
           ADD 24 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
           
           IF ST-STOCKNUMBER < WS-RANGE1
              GO TO UPST-010.
           IF ST-STOCKNUMBER > WS-RANGE2
              GO TO UPST-999.
              
           MOVE "Y" TO WS-STOCK-CHANGED.
           
           IF WS-HARDWARE = "A"
              GO TO UPST-015.
           IF WS-HARDWARE = "H"
            IF ST-ANALYSIS NOT = "H"
              GO TO UPST-010.
           IF WS-HARDWARE = "N"
            IF ST-ANALYSIS = "H"
              GO TO UPST-010.
       UPST-015.
           MOVE ST-PRICE TO WS-PRICE-SAVE.
       UPST-020.
           IF WS-SELECTED = 1
               COMPUTE ST-PRICE ROUNDED = ST-PRICE * WS-FACTOR
               GO TO UPST-900.
           IF WS-SELECTED = 2
               COMPUTE ST-PRICE ROUNDED = ST-AVERAGECOST * WS-FACTOR
               GO TO UPST-900.
           IF WS-SELECTED = 3
               COMPUTE ST-PRICE ROUNDED = ST-PRICE * WS-FACTOR
               COMPUTE ST-LASTCOST ROUNDED = ST-LASTCOST * WS-FACTOR
               GO TO UPST-900.
           IF WS-SELECTED = 4
               COMPUTE WS-INDFACTOR ROUNDED = WS-FACTOR *
             ((1 + (ST-DUTYPERCENT / 100)) * (1 + (ST-SURCHARGE / 100)))
               COMPUTE ST-PRICE ROUNDED = ST-FOREIGNCOST * WS-INDFACTOR
               GO TO UPST-900.
           IF WS-SELECTED = 5
               COMPUTE ST-PRICE ROUNDED = ST-FOREIGNCOST * WS-FACTOR
               GO TO UPST-900.
           IF WS-SELECTED = 6
               COMPUTE ST-FOREIGNCOST ROUNDED =
                         ST-FOREIGNCOST * WS-FACTOR
               GO TO UPST-920.
           IF WS-SELECTED = 7
               COMPUTE ST-PRICE ROUNDED = ST-LASTCOST * WS-FACTOR
               GO TO UPST-900.
               
           MOVE ST-LASTSALEDATE TO WS-CALC-DATE.
           IF WS-SELECTED = 11
            IF WS-ONHANDZERO = "Y"
             IF ST-QTYONHAND > 0
              IF WS-DATE-ACCEPT > " "
               IF WS-CALC-DATE > WS-DATE-ENTER
               COMPUTE ST-AVERAGECOST ROUNDED =
                ST-AVERAGECOST * WS-FACTOR
                GO TO UPST-940.
           IF WS-SELECTED = 11
            IF WS-ONHANDZERO = "Y"
             IF ST-QTYONHAND > 0
              IF WS-DATE-ACCEPT = " "
               COMPUTE ST-AVERAGECOST ROUNDED =
                ST-AVERAGECOST * WS-FACTOR
                GO TO UPST-940.
           IF WS-SELECTED = 11
            IF WS-ONHANDZERO = "N"
             IF WS-DATE-ACCEPT > " "
              IF WS-CALC-DATE > WS-DATE-ENTER
               COMPUTE ST-AVERAGECOST ROUNDED =
                ST-AVERAGECOST * WS-FACTOR
                GO TO UPST-940.
           IF WS-SELECTED = 11
            IF WS-ONHANDZERO = "N"
             IF WS-DATE-ACCEPT = " "
               COMPUTE ST-AVERAGECOST ROUNDED =
                ST-AVERAGECOST * WS-FACTOR
                GO TO UPST-940.
           IF WS-SELECTED = 11
               GO TO UPST-010.

           IF WS-SELECTED = 9
            IF ST-AVERAGECOST = 0
            AND ST-LASTCOST = 0
               COMPUTE ST-AVERAGECOST ROUNDED = ST-PRICE * WS-FACTOR
               MOVE ST-AVERAGECOST TO ST-LASTCOST
               GO TO UPST-940
            ELSE
               GO TO UPST-010.
      ********************************************
      *WS-FACTOR HERE = ON-COST PERC + M/U PERC. *
      ********************************************
            IF WS-SELECTED = 8 OR = 10
               PERFORM CHECK-CURRENCY.
            IF WS-SELECTED = 8 OR = 10
             IF WS-CURRENCY = 0
               MOVE "PRICE NOT CHANGED:  " TO WS-DAILY-1ST
               MOVE ST-STOCKNUMBER         TO WS-DAILY-2ND
               MOVE "CURRENCY INVALID    " TO WS-DAILY-3RD
               MOVE ST-CURRENCY            TO WS-DAILY-4TH
               PERFORM WRITE-DAILY
               GO TO UPST-010.
               
      ************************
      *THIS SECTION IS = 8   *
      ************************
           IF WS-SELECTED = 10
              GO TO UPST-500.
            COMPUTE WS-INDFACTOR ROUNDED = WS-FACTOR *
             ((1 + (ST-DUTYPERCENT / 100)) * (1 + (ST-SURCHARGE / 100))).
            COMPUTE ST-PRICE ROUNDED =
            ((ST-FOREIGNCOST - (ST-FOREIGNCOST * ST-SUPPLIERDISC / 100))
                 / WS-CURRENCY) * WS-INDFACTOR.
            GO TO UPST-900.
       UPST-500.
      ************************
      *THIS SECTION IS = 10  *
      ************************
            IF WS-ONHANDZERO = "Y"
             IF ST-QTYONHAND NOT = 0
                GO TO UPST-010.
            IF WS-ONHANDZERO = "Y"
             IF ST-QTYONRESERVE NOT = 0
                GO TO UPST-010.
                
            COMPUTE WS-INDFACTOR ROUNDED = WS-FACTOR *
             ((1 + (ST-DUTYPERCENT / 100)) * (1 + (ST-SURCHARGE / 100))).
            COMPUTE WS-AVERAGECOST ROUNDED =
            ((ST-FOREIGNCOST - (ST-FOREIGNCOST * ST-SUPPLIERDISC / 100))
                 / WS-CURRENCY) * WS-INDFACTOR.

            MOVE WS-AVERAGECOST TO ST-AVERAGECOST.
            GO TO UPST-940.
       UPST-900.
            IF ST-PRICE = 0
               MOVE "PRICE NOT CHANGED:  " TO WS-DAILY-1ST
               MOVE ST-STOCKNUMBER         TO WS-DAILY-2ND
               MOVE "ST-PRICE = ZERO     " TO WS-DAILY-3RD
               MOVE "                    " TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            IF WS-REDUCE = "N"
             IF WS-PRICE-SAVE > ST-PRICE
               MOVE WS-PRICE-SAVE TO ST-PRICE
               MOVE "N" TO WS-STOCK-CHANGED
               GO TO UPST-910.
            MOVE WS-PRICE-SAVE TO ST-OLDPRICE
            MOVE WS-DATE       TO ST-LASTPRICECHANGE.
       UPST-910.
      *IF WS-MINMARGIN = 0 THIS MEANS THAT WE DON'T WANT TO 
      *FORCE A MINIMUM MARGIN PER ITEM.  MOST TIMES IT WILL BE
      * 85% AT LEAST
            IF WS-MINMARGIN = 0
               GO TO UPST-920.
            COMPUTE WS-MARGIN = 
               ((ST-PRICE - ST-AVERAGECOST) / ST-AVERAGECOST) * 100.
            IF WS-MARGIN < WS-MINMARGIN
               COMPUTE ST-PRICE ROUNDED =
                    ST-AVERAGECOST * (1 + (WS-MINMARGIN / 100))
               MOVE "MARGIN LOWER < MIN. " TO WS-DAILY-1ST
               MOVE "MARGIN ENTERED.     " TO WS-DAILY-2ND
               MOVE "     ON STOCKNUMBER " TO WS-DAILY-3RD
               MOVE ST-STOCKNUMBER         TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
       UPST-920.
            IF ST-PRICE = 0
               MOVE "OLD PRICE FOR ITEM #" TO WS-DAILY-1ST
               MOVE ST-STOCKNUMBER         TO WS-DAILY-2ND
               MOVE "FOREIGN PRICE = ZERO" TO WS-DAILY-3RD
               MOVE "                    " TO WS-DAILY-4TH
               PERFORM WRITE-DAILY
               GO TO UPST-010.
               
            IF WS-WRITE-PRICELIST = "W"
             IF WS-STOCK-CHANGED = "Y"
               PERFORM WRITE-ROUTINE
               GO TO UPST-010
             ELSE
               GO TO UPST-010.
       UPST-940.
            IF WS-STOCK-CHANGED = "N"
               GO TO UPST-010.
            REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "PRICE NOT CHANGD" TO WS-DAILY-1ST
               MOVE ST-STOCKNUMBER     TO WS-DAILY-2ND
               MOVE "WS-STOCK-ST1 =23" TO WS-DAILY-3RD
                                          WS-DAILY-4TH
               PERFORM WRITE-DAILY
               GO TO UPST-950.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
              GO TO UPST-940.
       UPST-950.
           IF WS-SELECTED = 10 OR = 11
              GO TO UPST-010.
           IF INVQUES-STOCK-CHANGE = "Y"
               PERFORM WRITE-STOCK-CHANGES.
           GO TO UPST-010.
       UPST-999.
           EXIT.
      *
       CHECK-CURRENCY SECTION.
       CCS-005.
            MOVE 1 TO SUB-1.
            IF ST-CURRENCY = " "
                MOVE 0 TO WS-CURRENCY
                GO TO CCS-999.
       CCS-010.
            IF ST-CURRENCY NOT = WS-CURRENCY-TYPE (SUB-1)
             IF SUB-1 < 20
                ADD 1 TO SUB-1
                GO TO CCS-010
             ELSE
                MOVE 0 TO WS-CURRENCY
                GO TO CCS-999.
            MOVE WS-CURRENCY-VALUE (SUB-1) TO WS-CURRENCY.
       CCS-999.
            EXIT.
      *
       READ-CURRENCIES SECTION.
       RCUR-000.
           MOVE " " TO CU-CURRENCY-TYPE.
             START CURRENCY-MASTER KEY NOT < CU-KEY
               INVALID KEY NEXT SENTENCE.
       RCUR-005.
           READ CURRENCY-MASTER NEXT
             AT END
               GO TO RCUR-999.
           IF WS-CURRENCY-ST1 = 23 OR 35 OR 49
              MOVE "CURRENCY FILE BUSY ON READ-NEXT-23, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CURRENCY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CURRENCY-ST1
               GO TO RCUR-999.
           IF WS-CURRENCY-ST1 NOT = 0
              MOVE "CURRENCY FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CURRENCY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CURRENCY-ST1
               GO TO RCUR-005.
           MOVE CU-CURRENCY-TYPE TO WS-CURRENCY-TYPE (SUB-1)
           MOVE CU-VALUE         TO WS-CURRENCY-VALUE (SUB-1).
           IF SUB-1 < 20
               ADD 1 TO SUB-1
               GO TO RCUR-005.
       RCUR-999.
           EXIT.
      *
       WRITE-STOCK-CHANGES SECTION.
       WSTCH-000.
             MOVE ST-STOCKNUMBER TO STCH-STOCKNUMBER.
             START STOCKCHANGE-MASTER KEY NOT < STCH-KEY
               INVALID KEY NEXT SENTENCE.
       WSTCH-005.
             MOVE ST-STOCKNUMBER TO STCH-STOCKNUMBER.
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
          MOVE ST-PERMIT           TO STCH-PERMIT
          MOVE "C"                 TO STCH-TYPE-OF-CHANGE.
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
       WRITE-ROUTINE SECTION.
       WRITE-010.
           MOVE ST-STOCKNUMBER   TO STNWPR-STOCKNUMBER
           MOVE ST-PRICE         TO STNWPR-PRICE
           MOVE WS-DATE          TO STNWPR-DATE.
       WRITE-020.
           WRITE STNWPR-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-STNWPR-ST1 = 23 OR 35 OR 49
               GO TO WRITE-030.
           IF WS-STNWPR-ST1 NOT = 0
               MOVE "WRITING OF NEW PRICE LIST IN ERROR"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               GO TO WRITE-020.
           GO TO WRITE-999.
       WRITE-030.
           REWRITE STNWPR-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-STNWPR-ST1 = 23 OR 35 OR 49
               GO TO WRITE-020.
           IF WS-STNWPR-ST1 NOT = 0
               MOVE "REWRITING OF NEW PRICE LIST IN ERROR" TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               GO TO WRITE-030.
       WRITE-999.
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
               MOVE "PARAMETER BUSY RINVQUES, 'ESC' TO RETRY."
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
           PERFORM GET-SYSTEM-Y2K-DATE.
      *      ACCEPT WS-DATE FROM DATE.
       OPEN-005.
            OPEN I-O STOCKCHANGE-MASTER.
            IF WS-STCHANGE-ST1 NOT = 0
               MOVE 0 TO WS-STCHANGE-ST1
               MOVE "STCHANGE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-010.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
                MOVE 0 TO WS-STOCK-ST1
                MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO OPEN-010.
       OPEN-012.
            OPEN I-O STNWPR-MASTER.
            IF WS-STNWPR-ST1 NOT = 0
               MOVE 0 TO WS-STNWPR-ST1
               MOVE "PRICELIST FILE BUSY ON OPEN I-O, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-013.
            GO TO OPEN-014.
       OPEN-013.
            OPEN OUTPUT STNWPR-MASTER.
            IF WS-STNWPR-ST1 NOT = 0
               MOVE 0 TO WS-STNWPR-ST1
              MOVE "PRICELIST FILE BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-013.
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
       OPEN-015.
            OPEN I-O CURRENCY-MASTER.
            IF WS-CURRENCY-ST1 NOT = 0
               MOVE 0 TO WS-CURRENCY-ST1
               MOVE "CURRENCY FILE BUSY ON OPEN, 'CANCEL TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-015.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StPrChMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER
                  STOCKCHANGE-MASTER
                  CURRENCY-MASTER.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *      
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldFactor".
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
       Copy "CTOSCobolAccept".
       Copy "WriteDailyExcep1".
      * END-OF-JOB
      
