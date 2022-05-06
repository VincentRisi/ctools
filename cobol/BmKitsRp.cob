        IDENTIFICATION DIVISION.
        PROGRAM-ID. BmKitsRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
       Copy "SelectStMaster".
       Copy "SelectBmMaster".
       Copy "SelectSlParameter".
       Copy "SelectSlDaily".
       Copy "SelectStNewPrices".
       Copy "SelectStChanges".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdToolkit.
           COPY ChlfdParam.
           COPY ChlfdStockChanges.
           COPY ChlfdStNewPrice.
           COPY ChlfdDaily.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-ANSWER1           PIC X VALUE " ".
       77  WS-ANSWER2           PIC X VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
       77  WS-ANSWER4           PIC X VALUE " ".
       77  WS-ANSWER5           PIC X VALUE " ".
       77  WS-ANSWER6           PIC X(10) VALUE " ".
       77  WS-ANSWER7           PIC X VALUE " ".
       77  WS-FACTOR-FIELD      PIC X(6) VALUE " ".
       77  WS-FACTOR            PIC 9(3)V99 VALUE 0.
       77  WS-FACTOR-COST       PIC 9(3)V99 VALUE 0.
       77  WS-FACTOR-DIS        PIC Z(2)9.99.
       77  WS-TOTAL             PIC X VALUE " ".
       77  WS-STORE             PIC X(15) VALUE " ".
       77  WS-NEW-KIT           PIC X VALUE " ".
       77  WS-AVE               PIC 9(6)V99.
       77  WS-REP               PIC 9(6)V99.
       77  WS-PRICE             PIC 9(6)V99.
       77  WS-PRICE-SAVE        PIC 9(6)V99.
       77  WS-COST              PIC 9(6)V99.
       77  WS-AVECOSTTOTAL      PIC 9(6)V99.
       77  WS-REPCOSTTOTAL      PIC 9(6)V99.
       77  WS-CALC-MARGIN       PIC S9(6)V99.
       77  WS-CALC-PERC         PIC S9(4)V99.
       77  WS-CALC-PRICE        PIC 9(6)V99.
       77  WS-CALC-PRICE-RND-T  PIC 9(6)V9.
       77  WS-CALC-PRICE-RND-R  PIC 9(6).
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY1       PIC X(20).
           03  WS-DAILY2       PIC X(20).
           03  WS-DAILY3       PIC X(20).
           03  WS-DAILY4       PIC X(20).
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1       PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-ST-ST1          PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-TOOLKIT-ST1     PIC 99.
       01  WS-STNWPR-STATUS.
           03  WS-STNWPR-ST1      PIC 99.
       01  WS-STCHANGE-STATUS.
           03  WS-STCHANGE-ST1    PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  ACCEPT-DATE.
           03  ACCEPT-YY       PIC 9999.
           03  ACCEPT-MM       PIC 99.
           03  ACCEPT-DD       PIC 99.
       01  WS-KIT-DATE.
           03  KIT-YY       PIC 9999.
           03  KIT-MM       PIC 99.
           03  KIT-DD       PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(35) VALUE
           "T O O L K I T   R E P O R T".
           03  H1-COMMENT     PIC X(41).
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(45) VALUE " ".
           03  FILLER         PIC X(27) VALUE ALL "*".
           03  FILLER         PIC X(60) VALUE " ".
       01  HEAD3-1.
           03  FILLER         PIC X(18) VALUE "TOOLKIT NUMBER  :".
           03  H3-KIT         PIC X(20) VALUE " ".
           03  FILLER         PIC X(94) VALUE " ".
       01  HEAD3-2.
           03  FILLER         PIC X(18) VALUE "KIT DESCRIPTION :".
           03  H3-DESC1       PIC X(20) VALUE " ".
           03  H3-DESC2       PIC X(25) VALUE " ".
           03  FILLER         PIC X(69) VALUE " ".
       01  HEAD3-3.
           03  FILLER         PIC X(20) VALUE "SELLING PRICE   : R".
           03  H3-PRICE       PIC Z(4)9.99. 
           03  FILLER         PIC X(104) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(17) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(45) VALUE "DESCRIPTION".
           03  FILLER         PIC X(10) VALUE "QTY.".
           03  FILLER         PIC X(11) VALUE "ITEM PRICE".
           03  FILLER         PIC X(11) VALUE "    AVE".
           03  FILLER         PIC X(7) VALUE "% M/U".
           03  FILLER         PIC X(8) VALUE "    REP".
           03  FILLER         PIC X(12) VALUE "  TOT AVE".
           03  FILLER         PIC X(12) VALUE "  TOT REP".
       01  HEAD4-1.
           03  FILLER         PIC X(70) VALUE " ".
           03  FILLER         PIC X(10) VALUE "ITEM PRICE".
           03  FILLER         PIC X(11) VALUE "     AVE".
           03  FILLER         PIC X(7) VALUE "% M/U".
           03  FILLER         PIC X(9) VALUE "     REP".
           03  FILLER         PIC X(12) VALUE "  TOT AVE".
           03  FILLER         PIC X(12) VALUE "  TOT REP".
       01  HEAD5.
           03  FILLER         PIC X(17) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(46) VALUE "DESCRIPTION".
           03  FILLER         PIC X(10) VALUE "QTY.".
           03  FILLER         PIC X(59) VALUE "ITEM PRICE".
       01  HEAD5-1.
           03  FILLER         PIC X(17) VALUE " ".
           03  FILLER         PIC X(46) VALUE " ".
           03  FILLER         PIC X(10) VALUE "QTY".
           03  FILLER         PIC X(59) VALUE "ITEM PRICE".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(17).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(24).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-QTY          PIC Z(2)9.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-PRICE        PIC Z(5)9.99.
           03  D-AVE          PIC Z(5)9.99.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-PERC         PIC Z(3)9.99-.
           03  D-REP          PIC Z(5)9.99.
           03  D-AVECOST      PIC Z(5)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-REPCOST      PIC Z(5)9.99.
           03  FILLER         PIC X(4) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(72) VALUE "STOCKITEM TOTALS:".
           03  T-PRICE        PIC Z(5)9.99.
           03  T-AVE          PIC Z(5)9.99.
           03  FILLER         PIC X(2) VALUE " ".
           03  T-PERC         PIC Z(3)9.99-.
           03  T-REP          PIC Z(5)9.99.
           03  T-AVECOST      PIC Z(5)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  T-REPCOST      PIC Z(5)9.99.
           03  FILLER         PIC X(1) VALUE " ".
       01  PERC-LINE.
           03  FILLER         PIC X(21) VALUE "% M/U ON SELL PRICE:".
           03  P-CUR-PERC     PIC Z(3)9.99-.
           03  FILLER         PIC X(37) VALUE " ".
           03  FILLER         PIC X(15) VALUE "PRICE ON ITEMS:".
           03  P-ITEM-PRICE   PIC Z(5)9.99.
           03  FILLER         PIC X(4) VALUE " ".
           03  FILLER         PIC X(15) VALUE "PRICE ON COSTS:".
           03  P-COST-PRICE   PIC Z(5)9.99.
           03  FILLER         PIC X(6) VALUE "  FCTR".
           03  P-MU-PERC      PIC Z(2)9.99.
           03  FILLER         PIC X(3) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 325 TO POS
           DISPLAY "** TOOLKIT ITEM LISTING BY NUMBER REPORT **" AT POS
           MOVE 425 TO POS
           DISPLAY "*******************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           IF WS-TOOLKIT-ST1 NOT = 0 
              GO TO CONTROL-020.
           PERFORM TOTALS.
        CONTROL-020.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE " " TO WS-RANGE1 WS-RANGE2
           MOVE 933 TO POS
           DISPLAY "From TOOLKIT NUMBER  : [               ]" AT POS
           MOVE 957 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 6         TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
           ELSE
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
           MOVE 1033 TO POS
           DISPLAY "To TOOLKIT NUMBER    : [               ]" AT POS
           MOVE 1057 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           IF W-ESCAPE-KEY = 4
               GO TO GET-000.
           IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-020.
            MOVE 1225 TO POS
            DISPLAY "Print COSTS for the Toolkit  : [ ]" AT POS
            MOVE 1257 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

            IF W-ESCAPE-KEY = 4
               GO TO GET-010.
            IF WS-ANSWER1 NOT = "Y" AND NOT = "N"
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-025
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-025.
            MOVE 1325 TO POS
            DISPLAY "Print TOTALS only YES Or NO  : [ ]" AT POS
            MOVE 1357 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TOTAL.

            IF W-ESCAPE-KEY = 4
               GO TO GET-020.
            IF WS-TOTAL NOT = "Y" AND NOT = "N"
               GO TO GET-025.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-025.
       GET-030.
            MOVE 1425 TO POS
            DISPLAY "Print % MARGINS,  YES Or NO  : [ ]" AT POS
            MOVE 1457 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

            IF W-ESCAPE-KEY = 4
               GO TO GET-025.
            IF WS-ANSWER2 NOT = "Y" AND NOT = "N"
               GO TO GET-030.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-035
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-035.
           IF WS-ANSWER2 = "N"
              MOVE "N" TO WS-ANSWER3
              GO TO GET-130.
           MOVE 1510 TO POS
           DISPLAY
           "REWRITE Stock-file with NEW Kit Price, Y/N  : [ ]" AT POS
           MOVE 1557 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER3.

           IF W-ESCAPE-KEY = 4
              GO TO GET-030.
           IF WS-ANSWER3 NOT = "Y" AND NOT = "N"
              GO TO GET-035.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-036
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-035.
       GET-036.
           IF WS-ANSWER3 = "N"
              MOVE " " TO WS-ANSWER7
              GO TO GET-150.
           MOVE 1610 TO POS
           DISPLAY
           "W=WRITE New PRICELIST, U=UPDATE Stockfile   : [ ]" AT POS
           MOVE 1657 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER7.

           IF W-ESCAPE-KEY = 4
              GO TO GET-035.
           IF WS-ANSWER7 NOT = "U" AND NOT = "W"
              GO TO GET-036.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-037
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-036.
       GET-037.
           IF WS-ANSWER3 = "N"
              GO TO GET-040.
           MOVE 1710 TO POS
           DISPLAY
           "If NEW PRICE < OLD PRICE Lower Price, Y/N   : [ ]" AT POS
           MOVE 1757 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER4.

           IF W-ESCAPE-KEY = 4
              GO TO GET-035.
           IF WS-ANSWER4 NOT = "Y" AND NOT = "N"
              GO TO GET-037.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-038
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-037.
       GET-038.
           IF WS-ANSWER3 = "N"
              GO TO GET-040.
           MOVE 1810 TO POS
           DISPLAY
           "Do You Wish To Round Off The Prices,        : [ ]" AT POS
           MOVE 1910 TO POS
           DISPLAY
           " N=NO, R=Rounded To RAND, T=Rounded To TEN CENTS." AT POS
           MOVE 1857 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER5.

           IF W-ESCAPE-KEY = 4
              GO TO GET-037.
           IF WS-ANSWER5 NOT = "N" AND NOT = "R" AND NOT = "T"
              GO TO GET-038.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-040
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-038.
       GET-040.
           MOVE "                   " TO WS-FACTOR-FIELD.
           MOVE 2110 TO POS.
           DISPLAY
           "Enter FACTOR to Discount the SELLING PRICE  : [      ]"
                AT POS
           MOVE 2231 TO POS
           DISPLAY "The MAX. format should be 999.99" AT POS
           MOVE 2157 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 6         TO CDA-DATALEN.
           MOVE 18        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FACTOR-FIELD.

           IF W-ESCAPE-KEY = 4
              GO TO GET-035.
           IF WS-FACTOR-FIELD = "   "
              GO TO GET-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-130
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-040.
       GET-130.
            MOVE WS-FACTOR-FIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-FACTOR
                                 WS-FACTOR-DIS.
            IF NUMERIC-RATE = 0
               MOVE 3010 TO POS
               DISPLAY "THE FACTOR MUST BE > 0" AT POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
            IF SIGN-FOUND = 1
               MOVE 3010 TO POS
               DISPLAY "THE FACTOR MUST BE POSITIVE" AT POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
            MOVE 2157 TO POS.
            DISPLAY WS-FACTOR-DIS AT POS.
       GET-150.
           MOVE "                   " TO WS-FACTOR-FIELD
           MOVE 2310 TO POS
           DISPLAY
           "Enter a MARKUP to ReCalc PRICE on AVE COST  : [      ]"
                 AT POS
           MOVE 2431 TO POS
           DISPLAY "The MAX. format should be 999.99" AT POS.
           MOVE 2357 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 6        TO CDA-DATALEN.
           MOVE 20        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FACTOR-FIELD.

           IF W-ESCAPE-KEY = 4
            IF WS-ANSWER3 = "N"
              GO TO GET-035
            ELSE
              GO TO GET-040.
           IF WS-FACTOR-FIELD = "   "
              GO TO GET-150.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-160
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-150.
       GET-160.
           MOVE WS-FACTOR-FIELD TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WS-FACTOR-COST
                                WS-FACTOR-DIS.
           IF NUMERIC-RATE = 0
              MOVE 3010 TO POS
              DISPLAY "THE FACTOR MUST BE > 0" AT POS
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-150.
           IF SIGN-FOUND = 1
              MOVE 3010 TO POS
              DISPLAY "THE FACTOR MUST BE POSITIVE" AT POS
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-150.
           MOVE 2357 TO POS.
           DISPLAY WS-FACTOR-DIS AT POS.
       GET-200.
           MOVE " " TO WS-ANSWER6
           MOVE 2510 TO POS
           DISPLAY
           "Enter a DATE after which B/M were Made      : [          ]"
                  AT POS
           MOVE 2620 TO POS
           DISPLAY "leave BLANK to print ALL B/M's  " AT POS
           MOVE 2557 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 22        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER6.

           IF W-ESCAPE-KEY = 4
              GO TO GET-150.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-550
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-200.
       GET-550.
           IF WS-ANSWER6 = " "
                GO TO GET-600.
           MOVE WS-ANSWER6 TO ALPHA-RATE
           PERFORM DATE-CHECKING
           IF SIGN-FOUND = 9
              GO TO GET-200.
              
           MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO ACCEPT-DATE

           IF WS-ANSWER6 NOT = " "
               MOVE 2557 TO POS
               DISPLAY DISPLAY-DATE AT POS.
      *     PERFORM CONVERT-DATE-FORMAT
      *     MOVE WS-CONVERT-DATE TO ACCEPT-DATE

           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO GET-200.
       GET-600.
           MOVE 2910 TO POS
           DISPLAY "The report is being compiled........." AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
       PRR-001.
            MOVE WS-RANGE1 TO TO-TOOLKIT-NUMBER.
            START TOOLKITS KEY NOT < TO-KEY
                INVALID KEY NEXT SENTENCE.
            IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "NO KIT WITHIN RANGE ENTERED, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PRR-999.
       PRR-002.
            READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
            IF WS-TOOLKIT-ST1 = 10
               MOVE 0 TO WS-TOOLKIT-ST1
               PERFORM TOTALS
               GO TO PRR-999.
            IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKIT FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO PRR-002.
            IF TO-TOOLKIT-NUMBER < WS-RANGE1
               GO TO PRR-002.
            IF TO-TOOLKIT-NUMBER > WS-RANGE2
               GO TO PRR-999.

            IF WS-STORE = " "
               MOVE TO-TOOLKIT-NUMBER TO WS-STORE
                                         ST-STOCKNUMBER
               MOVE 3020 TO POS
               DISPLAY "TOOLKIT BEING READ:" AT POS
               ADD 20 TO POS
               DISPLAY TO-TOOLKIT-NUMBER AT POS
               PERFORM RS-010
               MOVE ST-LASTRECEIPTDATE TO WS-KIT-DATE
               MOVE TO-TOOLKIT-NUMBER  TO H3-KIT
               MOVE ST-DESCRIPTION1    TO H3-DESC1
               MOVE ST-DESCRIPTION2    TO H3-DESC2
               MOVE ST-PRICE           TO H3-PRICE
                                          WS-PRICE-SAVE
               GO TO PRR-002.

            IF TO-TOOLKIT-NUMBER NOT = WS-STORE
               MOVE "Y" TO WS-NEW-KIT
               PERFORM TOTALS
               MOVE TO-TOOLKIT-NUMBER TO H3-KIT
                                         WS-STORE
                                         TO-COMPONENT-NUMBER
               MOVE 3020 TO POS
               DISPLAY "TOOLKIT BEING READ:" AT POS
               ADD 20 TO POS
               DISPLAY TO-TOOLKIT-NUMBER AT POS
               PERFORM READ-STOCK
               MOVE ST-LASTRECEIPTDATE TO WS-KIT-DATE
               MOVE ST-DESCRIPTION1    TO H3-DESC1
               MOVE ST-DESCRIPTION2    TO H3-DESC2
               MOVE ST-PRICE           TO H3-PRICE
                                          WS-PRICE-SAVE.
             IF WS-NEW-KIT = "Y"
              IF WS-TOTAL = "Y"
               IF WS-KIT-DATE > ACCEPT-DATE
                  PERFORM PRR-025
                  MOVE "N" TO WS-NEW-KIT
                  GO TO PRR-002.
             IF WS-NEW-KIT = "Y"
              IF WS-TOTAL NOT = "Y"
               IF WS-KIT-DATE > ACCEPT-DATE
                  PERFORM PRR-015 THRU PRR-018
                  MOVE "N" TO WS-NEW-KIT
                  GO TO PRR-002.

            IF WS-ANSWER6 NOT = " "
             IF WS-KIT-DATE < ACCEPT-DATE
               GO TO PRR-002.

            PERFORM READ-STOCK.
       PRR-010.
            IF WS-TOTAL = "Y"
             IF LINE-CNT > 60
               PERFORM PRR-015
               PERFORM PRR-017
              IF PAGE-CNT = 1
               PERFORM PRR-018
               PERFORM PRR-016
              ELSE
               PERFORM PRR-018.
            IF WS-TOTAL = "Y"
               GO TO PRR-022.
            IF LINE-CNT < 61
               GO TO PRR-020.
       PRR-015.
            ADD 1         TO PAGE-CNT.
            MOVE PAGE-CNT TO H1-PAGE.
            MOVE " "      TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC.
            MOVE " " TO PRINT-REC.
            IF WS-ANSWER3 = "Y"
                MOVE "*PRICE UPDATED USING FACTOR ON COST*"
                TO H1-COMMENT
            ELSE
                MOVE "*KIT PRICES NOT UPDATED THIS PRINT *"
                TO H1-COMMENT.
            WRITE PRINT-REC FROM HEAD1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD2.
            MOVE " " TO PRINT-REC.
       PRR-016.
            WRITE PRINT-REC 
            WRITE PRINT-REC FROM HEAD3-1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3-2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3-3
            MOVE " " TO PRINT-REC
            ADD 4 TO LINE-CNT.
            IF WS-TOTAL = "N"
              WRITE PRINT-REC.
       PRR-017.
            IF WS-ANSWER1 = "Y"
             IF WS-TOTAL = "N"
                WRITE PRINT-REC FROM HEAD4
            ELSE
                WRITE PRINT-REC FROM HEAD4-1.
            IF WS-ANSWER1 = "N"
             IF WS-TOTAL = "N"
                WRITE PRINT-REC FROM HEAD5
            ELSE
                WRITE PRINT-REC FROM HEAD5-1.
            MOVE " " TO PRINT-REC.
       PRR-018.
            MOVE 6 TO LINE-CNT.
       PRR-020.
           MOVE ST-STOCKNUMBER     TO D-STOCK
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE TO-QUANTITY        TO D-QTY
           MOVE ST-PRICE           TO D-PRICE
           COMPUTE WS-PRICE = WS-PRICE + (ST-PRICE * TO-QUANTITY).
           IF WS-ANSWER1 = "Y"
              MOVE ST-AVERAGECOST  TO D-AVE
              COMPUTE WS-COST = ST-AVERAGECOST * TO-QUANTITY
              COMPUTE WS-CALC-MARGIN = ST-PRICE - ST-AVERAGECOST
              COMPUTE WS-CALC-PERC =
                    (WS-CALC-MARGIN / ST-AVERAGECOST) * 100
              MOVE WS-CALC-PERC    TO D-PERC
              MOVE WS-COST         TO D-AVECOST
              ADD ST-AVERAGECOST   TO WS-AVE
              ADD WS-COST          TO WS-AVECOSTTOTAL

              MOVE ST-LASTCOST     TO D-REP
              COMPUTE WS-COST = ST-LASTCOST * TO-QUANTITY
              MOVE WS-COST         TO D-REPCOST
              ADD ST-LASTCOST      TO WS-REP
              ADD WS-COST          TO WS-REPCOSTTOTAL.

           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRR-002.
       PRR-022.
           COMPUTE WS-PRICE = WS-PRICE + (ST-PRICE * TO-QUANTITY).
           IF WS-ANSWER1 = "Y"
              COMPUTE WS-COST = ST-AVERAGECOST * TO-QUANTITY
              ADD ST-AVERAGECOST   TO WS-AVE
              ADD WS-COST          TO WS-AVECOSTTOTAL
              MOVE 0               TO WS-COST
              COMPUTE WS-COST = ST-LASTCOST * TO-QUANTITY
              ADD ST-LASTCOST      TO WS-REP
              ADD WS-COST          TO WS-REPCOSTTOTAL.
           GO TO PRR-002.
       PRR-025.
            IF LINE-CNT > 60
               PERFORM PRR-015
               PERFORM PRR-017
             IF PAGE-CNT = 1
               PERFORM PRR-018
               PERFORM PRR-016
             ELSE
               PERFORM PRR-018.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3-1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3-2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3-3
            MOVE " " TO PRINT-REC
            ADD 4 TO LINE-CNT.
       PRR-999.
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
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER BUSY ON READ RINVQUES, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RINVQUES-010.
       RINVQUES-999.
            EXIT.
      *
       READ-STOCK SECTION.
       RS-005.
           MOVE TO-COMPONENT-NUMBER TO ST-STOCKNUMBER.
       RS-010.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-ST-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-ST-ST1
               MOVE "** INVALID STOCK ITE" TO ST-DESCRIPTION1
               MOVE "M IN TOOLKIT FILE **" TO ST-DESCRIPTION2
               MOVE 0 TO ST-PRICE
                         ST-AVERAGECOST
                         ST-LASTCOST
               GO TO RS-999.
          IF WS-ST-ST1 NOT = 0
               MOVE "STOCK BUSY ON READ, RS-010, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-ST-ST1
               GO TO RS-010.
       RS-999.
           EXIT.
      *
       TOTALS SECTION.
       TS-010.
            IF WS-ANSWER6 NOT = " "
             IF WS-KIT-DATE < ACCEPT-DATE
               PERFORM TS-090
               GO TO TS-999.
           MOVE WS-PRICE TO T-PRICE.
           IF WS-ANSWER1 = "Y"
               MOVE WS-AVE          TO T-AVE
               MOVE WS-REP          TO T-REP
               MOVE WS-AVECOSTTOTAL TO T-AVECOST
               MOVE WS-REPCOSTTOTAL TO T-REPCOST
               COMPUTE WS-CALC-MARGIN = WS-PRICE - WS-AVECOSTTOTAL
               COMPUTE WS-CALC-PERC =
                 (WS-CALC-MARGIN / WS-AVECOSTTOTAL) * 100
               MOVE WS-CALC-PERC    TO T-PERC
               MOVE 0               TO WS-CALC-MARGIN
                                       WS-CALC-PERC.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC.
           IF WS-ANSWER2 = "N"
               WRITE PRINT-REC
               GO TO TS-090.
       TS-020.
           COMPUTE WS-CALC-MARGIN = WS-PRICE-SAVE - WS-AVECOSTTOTAL.
           COMPUTE WS-CALC-PERC =
               (WS-CALC-MARGIN / WS-AVECOSTTOTAL) * 100.
           MOVE WS-CALC-PERC TO P-CUR-PERC.
           MOVE 0 TO WS-CALC-MARGIN
                     WS-CALC-PERC.

           COMPUTE WS-CALC-PRICE ROUNDED = WS-PRICE * WS-FACTOR.
           MOVE WS-CALC-PRICE TO P-ITEM-PRICE.
           COMPUTE WS-CALC-PRICE ROUNDED =
                    WS-AVECOSTTOTAL * WS-FACTOR-COST.
           MOVE WS-CALC-PRICE TO P-COST-PRICE.
   
           MOVE WS-FACTOR-COST TO P-MU-PERC.

           WRITE PRINT-REC FROM PERC-LINE.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
           IF WS-ANSWER3 = "Y"
              PERFORM UPDATE-STOCK-PRICE.
       TS-090.
           MOVE 0 TO WS-AVE
                     WS-REP
                     WS-PRICE
                     WS-COST
                     WS-AVECOSTTOTAL
                     WS-REPCOSTTOTAL
                     H3-PRICE.
           MOVE " " TO H3-KIT
                       H3-DESC1
                       H3-DESC2
                       WS-STORE.
       TS-095.
           IF WS-ANSWER2 = "Y"
             ADD 3 TO LINE-CNT
           ELSE
             ADD 2 TO LINE-CNT.
        TS-999.
          EXIT.
      *
       UPDATE-STOCK-PRICE SECTION.
       USP-005.
           MOVE WS-STORE TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
       USP-010.
           READ STOCK-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-ST-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-ST-ST1
               MOVE ST-STOCKNUMBER        TO WS-DAILY1
               MOVE "NOT UPDATED FOR KIT" TO WS-DAILY2
               MOVE "PRICE & COSTS.     " TO WS-DAILY3
               MOVE "                   " TO WS-DAILY4
               PERFORM WRITE-DAILY
               GO TO USP-999.
           IF WS-ST-ST1 NOT = 0
               MOVE "STOCK-LOCK BUSY ON READ, UPS-010, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-ST-ST1
               GO TO USP-010.
           IF WS-ANSWER5 = "R"
               COMPUTE WS-CALC-PRICE-RND-R ROUNDED = WS-CALC-PRICE * 1
               MOVE WS-CALC-PRICE-RND-R TO WS-CALC-PRICE.
           IF WS-ANSWER5 = "T"
               COMPUTE WS-CALC-PRICE-RND-T ROUNDED = WS-CALC-PRICE * 1
               MOVE WS-CALC-PRICE-RND-T TO WS-CALC-PRICE.
       USP-020.
           IF WS-CALC-PRICE < ST-PRICE
            IF WS-ANSWER4 = "Y"
               MOVE ST-STOCKNUMBER        TO WS-DAILY1
               MOVE "OLD & NEW PRICES:  " TO WS-DAILY2
               MOVE H3-PRICE              TO WS-DAILY3
               MOVE P-ITEM-PRICE          TO WS-DAILY4
               PERFORM WRITE-DAILY
               MOVE WS-CALC-PRICE         TO ST-PRICE
               MOVE WS-AVECOSTTOTAL       TO ST-AVERAGECOST
               GO TO USP-030 
           ELSE
               GO TO USP-999. 
           IF WS-CALC-PRICE > ST-PRICE
               MOVE ST-STOCKNUMBER        TO WS-DAILY1
               MOVE "OLD & NEW PRICES:  " TO WS-DAILY2
               MOVE H3-PRICE              TO WS-DAILY3
               MOVE P-ITEM-PRICE          TO WS-DAILY4
               PERFORM WRITE-DAILY
               MOVE WS-CALC-PRICE         TO ST-PRICE
               MOVE WS-AVECOSTTOTAL       TO ST-AVERAGECOST.
           IF WS-ANSWER7 = "W"
               MOVE WS-CALC-PRICE         TO ST-PRICE
               PERFORM WRITE-ROUTINE
               GO TO USP-999.
        USP-030.
           MOVE WS-PRICE-SAVE TO ST-OLDPRICE
           MOVE WS-DATE       TO ST-LASTPRICECHANGE.
        USP-040.
           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-ST-ST1 = 23 OR 35 OR 49
               GO TO USP-999.
           IF WS-ST-ST1 NOT = 0
               MOVE 0 TO WS-ST-ST1
               MOVE "STOCK RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO USP-040.
       USP-950.
           IF INVQUES-STOCK-CHANGE = "Y"
               PERFORM WRITE-STOCK-CHANGES.
       USP-999.
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
                MOVE 0 TO WS-STCHANGE-ST1
                MOVE "STOCKCHANGE BUSY ON READ-LOCK, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
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
              MOVE 0 TO WS-STCHANGE-ST1
              MOVE "STOCKCHANGE BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WSTCH-010.
          GO TO WSTCH-999.
       WSTCH-020.
          WRITE STOCKCHANGE-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STCHANGE-ST1 NOT = 0
              MOVE 0 TO WS-STCHANGE-ST1
              MOVE "STOCKCHANGE BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
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
           IF WS-StNwPr-ST1 = 23 OR 35 OR 49
               GO TO WRITE-030.
           IF WS-StNwPr-ST1 NOT = 0
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
               MOVE
                "REWRITING OF NEW PRICE LIST IN ERROR, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               GO TO WRITE-030.
       WRITE-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O STOCK-MASTER.
           IF WS-ST-ST1 NOT = 0 
              MOVE 0 TO WS-ST-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-005.
           OPEN I-O TOOLKITS.
           IF WS-TOOLKIT-ST1 NOT = 0 
              MOVE 0 TO WS-TOOLKIT-ST1
              MOVE "KIT FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.
       OPEN-010.
            OPEN I-O STOCKCHANGE-MASTER.
            IF WS-STCHANGE-ST1 NOT = 0
               MOVE 0 TO WS-STCHANGE-ST1
               MOVE "STCHANGE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-010.
       OPEN-012.
            OPEN I-O STNWPR-MASTER.
            IF WS-STNWPR-ST1 NOT = 0
               MOVE 0 TO WS-STNWPR-ST1
               MOVE "PRICELIST FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-012.
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
        OPEN-050.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER
                  STOCKCHANGE-MASTER
                  STNWPR-MASTER
                  PARAMETER-FILE
                  TOOLKITS.
            IF WS-ANSWER4 = "Y"
               MOVE " " TO PRINT-REC
               MOVE "** SELLING PRICE UPDATED WITH CALCULATED PRICE **"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 3.
           IF WS-ANSWER6 NOT = " "
               MOVE " " TO PRINT-REC
               MOVE
            "** ONLY BILLS RECEIVED AFTER THE FOLLOWING DATE PRINTED **"
               TO PRINT-REC
               WRITE PRINT-REC
               MOVE ACCEPT-DATE TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE DISPLAY-DATE TO PRINT-REC
               WRITE PRINT-REC.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      *
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "WriteDailyExcep1".
       Copy "CTOSCobolAccept".
      * END-OF-JOB.
