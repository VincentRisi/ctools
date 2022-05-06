        IDENTIFICATION DIVISION.
        PROGRAM-ID. StReduRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(250).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(15) VALUE " ".
       77  WS-CAT-ONLY          PIC X VALUE " ".
       77  WS-COST-SELL         PIC X VALUE " ".
       77  WS-REDUN-TYPE        PIC X VALUE " ".
       77  WS-ANALYSIS          PIC X VALUE " ".
       77  WS-QTY               PIC S9(6) VALUE 0.
       77  WS-STQTYONHAND       PIC S9(6) VALUE 0.
       77  WS-AVECOST           PIC S9(7)V99 VALUE 0.
       77  WS-REPCOST           PIC S9(7)V99 VALUE 0.
       77  WS-AVECOSTTOT        PIC S9(7)V99 VALUE 0.
       77  WS-REPCOSTTOT        PIC S9(7)V99 VALUE 0.
       77  WS-AVETOTAL          PIC S9(7)V99 VALUE 0.
       77  WS-REPTOTAL          PIC S9(7)V99 VALUE 0.
       77  WS-AVEVALUE          PIC Z(7)9.99-.
       77  WS-REPVALUE          PIC Z(7)9.99-.
       77  WS-DATE-ZERO         PIC X VALUE " ".
       77  WS-DATE-ACCEPT       PIC X(10) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-DATE-ENTER.
           03  WS-YYE           PIC 9999.
           03  WS-MME           PIC 99.
           03  WS-DDE           PIC 99.
       01  WS-CALC-DATE.
           03  WS-YYC           PIC 9999.
           03  WS-MMC           PIC 99.
           03  WS-DDC           PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(17) VALUE "R E D U N D A N T".
           03  FILLER         PIC X(15) VALUE "   S T O C K".
           03  FILLER         PIC X(18) VALUE "N O T   S O L D".
           03  FILLER         PIC X(11) VALUE "S I N C E".
           03  H-DATE         PIC X(10).
           03  FILLER         PIC X(20) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(4) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(71) VALUE ALL "*".
           03  FILLER         PIC X(34) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(60) VALUE " ".
           03  FILLER         PIC X(34) VALUE " ON".
           03  FILLER         PIC X(11) VALUE "VALUE AT".
           03  H3-1           PIC X(11) VALUE " ".
           03  FILLER         PIC X(8) VALUE "LAST".
       01  HEAD4.
           03  FILLER         PIC X(16) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(43) VALUE "DESCRIPTION".
           03  FILLER         PIC X(9) VALUE "HAND".
           03  FILLER         PIC X(6) VALUE "PRICE".
           03  H4-1           PIC X(20) VALUE " ".
           03  H4-2           PIC X(21) VALUE " ".
           03  FILLER         PIC X(12) VALUE "SALE DATE".
           03  FILLER         PIC X(8) VALUE "CREATED".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(16) VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(21) VALUE " ".
           03  D-ONHAND       PIC Z(5)9.
           03  D-OVERMAX      PIC X(1) VALUE " ".
           03  D-PRICE        PIC Z(5)9.99.
           03  D-AVECOST      PIC Z(5)9.99.
           03  D-REPCOST      PIC Z(5)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-AVECOSTTOT   PIC Z(6)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-REPCOSTTOT   PIC Z(6)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-LASTSOLD     PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-CREATED      PIC X(10).
       01  TOTAL-LINE.
           03  FILLER         PIC X(77) VALUE " ".
           03  T-CATEGORY     PIC X(15) VALUE " ".
           03  T-AVECOST      PIC Z(6)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  T-REPCOST      PIC Z(6)9.99.
           03  FILLER         PIC X(12) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 321 TO POS
           DISPLAY "** REDUNDANT STOCK VALUATION REPORT **" AT POS
           MOVE 421 TO POS
           DISPLAY "**************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 710 TO POS.
           DISPLAY "FROM STOCK NUMBER : [               ]" AT POS.
           MOVE 731 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 4         TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-012
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-012.
           MOVE 810 TO POS.
           DISPLAY "  TO STOCK NUMBER : [               ]" AT POS.
           MOVE 831 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           IF WS-ANSWER2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
       CONTROL-015.
           MOVE 1010 TO POS.
           DISPLAY "  CATEGORIES ONLY : [ ]" AT POS.
           MOVE 1031 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CAT-ONLY.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-012.
           IF WS-CAT-ONLY NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
           MOVE 0 TO WS-DATE-ENTER.
           MOVE 1210 TO POS.
           DISPLAY "ENTER THE LAST DATE STOCK WAS SOLD : [          ]"
               AT POS.
           MOVE 1310 TO POS.
           DISPLAY "                    Enter the DATE as DD/MM/YYYY"
             AT POS.
           MOVE 1248 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATE-ACCEPT.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           MOVE WS-DATE-ACCEPT TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO CONTROL-020.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE 1248 TO POS.
           DISPLAY DISPLAY-DATE AT POS.
           MOVE DISPLAY-DATE TO H-DATE.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-DATE-ENTER.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO CONTROL-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-030.
           MOVE " " TO WS-DATE-ZERO.
           MOVE 1510 TO POS.
           DISPLAY "PRINT DATES THAT = 0, Y OR N       : [ ]" AT POS.
           MOVE 1548 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATE-ZERO.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-020.
           IF WS-DATE-ZERO NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-030.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO CONTROL-035
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-030.
       CONTROL-035.
           MOVE " " TO WS-COST-SELL.
           MOVE 1710 TO POS.
           DISPLAY "PRINT VALUE ON COSTS OR S/PRICES ? : [ ]" AT POS.
           MOVE 1810 TO POS.
           DISPLAY "Y=COSTS , N=SELLING PRICES.             " AT POS.
           MOVE 1748 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COST-SELL.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-030.
           IF WS-COST-SELL NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-035.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO CONTROL-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-035.
       CONTROL-040.
           MOVE " " TO WS-REDUN-TYPE.
           MOVE 2010 TO POS.
           DISPLAY "PRINT STOCK IF ON-HAND + ON-RESERVE = 0  : [ ]"
             AT POS.
           MOVE 2120 TO POS
           DISPLAY "Y=PRINT ALL REDUNDANT ITEMS." AT POS
           MOVE 2220 TO POS
           DISPLAY "N=NO PRINT IF ON-HAND + ON-RESERVE = 0" AT POS
           MOVE 2320 TO POS
           DISPLAY "Z=PRINT ONLY IF ON-HAND + ON-RESERVE = 0"
            AT POS.
           MOVE 2420 TO POS
           DISPLAY
          "X=PRINT ONLY IF ON-HAND + ON-RES > 0, OR OVER MAX-LEVEL"
            AT POS.
           MOVE 2054 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-REDUN-TYPE.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-035.
           IF WS-REDUN-TYPE NOT = "Y" AND NOT = "N"
                     AND NOT = "Z" AND NOT = "X"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO CONTROL-045
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
       CONTROL-045.
           MOVE "N" TO WS-ANALYSIS.
           MOVE 2610 TO POS
           DISPLAY 
           "THIS FUNCTION PLACES REDUNDANT ITEMS ON 'S' ONLY, IT WON'T"
           AT POS
           MOVE 2711 TO POS
           DISPLAY 
           "TAKE ITEMS OFF 'S'; OR FLAG AS 'D' TO DELETE LATER,"
            AT POS
           MOVE 2811 TO POS
           DISPLAY 
        "OR 'X' TO FLAG AS 'D' TO DELETE LATER AND MOVE 1c TO AVE COST."
            AT POS
           MOVE 2510 TO POS.
           DISPLAY "RE-WRITE ANALYSIS FIELD WITH S, D N OR X : [ ]"
             AT POS.
           MOVE 2554 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 22        TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANALYSIS.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-040.
           IF WS-ANALYSIS NOT = "D" AND NOT = "N" AND NOT = "S"
                      AND NOT = "X"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-045.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO CONTROL-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-045.
       CONTROL-050.
           PERFORM ERROR1-020
           PERFORM ERROR-020.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE 3010 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
           AT POS.
           PERFORM OPEN-FILES
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           IF WS-COST-SELL = "Y"
               MOVE "AVE COST REP COST"     TO H4-1
               MOVE "AVE COST   REP COST"   TO H4-2
               MOVE "VALUE AT"              TO H3-1
           ELSE
               MOVE "                    "  TO H4-1
               MOVE "S. PRICE             " TO H4-2
               MOVE "        "              TO H3-1.
           MOVE WS-ANSWER1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       PRR-005.
           READ STOCK-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               PERFORM PRR-025
               GO TO PRR-999.
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
               GO TO PRR-005.
           IF ST-STOCKNUMBER < WS-ANSWER1
              GO TO PRR-005.
           IF ST-STOCKNUMBER > WS-ANSWER2
              PERFORM PRR-025
              GO TO PRR-999.
              
           MOVE 0            TO WS-QTY.
           MOVE ST-QTYONHAND TO WS-STQTYONHAND.
              
           IF WS-STORE = "   "
              MOVE ST-CATEGORY TO WS-STORE.
           
           MOVE 2910 TO POS
           DISPLAY "STOCKNUMBER BEING READ:" AT POS
           ADD 24 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
              
      *******************************************
      * WS-CALC-DATE = WS-YYC, WS-MMC, & WS-DDC *
      *******************************************
           MOVE ST-LASTSALEDATE TO WS-CALC-DATE.
           IF WS-YYC = 0
            IF WS-MMC = 0
             IF WS-DDC = 0
              IF WS-DATE-ZERO = "N"
               IF ST-DATE-CREATED > WS-DATE-ENTER
               GO TO PRR-005.
               
      ******************* CHANGED 24/8/2011
      * IF NO SALES BUT ITEM CREATED AFTER DATE ENTERED, DON'T
      * SHOW AS REDUNDANT.
      *******************
           IF ST-LASTSALEDATE = 0 
            IF ST-DATE-CREATED  > WS-DATE-ENTER
               GO TO PRR-005.
           
      ******************* CHANGED 24/8/2011
      * IF ANY OF THE STOCK QTY FIELDS HAVE A VALUE > 0, DON'T 
      * SHOW AS REDUNDANT.
      *******************
           IF ST-LASTSALEDATE = 0 
            IF WS-DATE-ZERO = "Y"
             IF ST-QTYONHAND = 0
              IF ST-QTYONRESERVE = 0
               IF ST-QTYONORDER = 0 
                IF ST-QTYONBORDER = 0
                   GO TO PRR-006
                ELSE
                   GO TO PRR-005.
       PRR-006.
      ********************************************
      * WS-DATE-ENTER = WS-YYE, WS-MME, & WS-DDE *
      ********************************************
           IF WS-YYC < WS-YYE
               GO TO PRR-010.
           IF WS-YYC = WS-YYE
            IF WS-MMC < WS-MME
               GO TO PRR-010.
           IF WS-YYC = WS-YYE
            IF WS-MMC = WS-MME
             IF WS-DDC < WS-DDE
               GO TO PRR-010.
               
           IF WS-REDUN-TYPE = "X"
            IF ST-QTYONHAND > ST-MAXIMUMLEVEL
              MOVE "*" TO D-OVERMAX
              COMPUTE WS-QTY =
                ((ST-QTYONHAND + ST-QTYONRESERVE) - ST-QTYONBORDER)
                    - ST-MAXIMUMLEVEL
             IF WS-QTY NOT > 0
                 GO TO PRR-005
             ELSE
                MOVE WS-QTY TO ST-QTYONHAND
                 GO TO PRR-011.
           GO TO PRR-005.
       PRR-010.
           COMPUTE WS-QTY =
                ((ST-QTYONHAND + ST-QTYONRESERVE) - ST-QTYONBORDER)
                    - ST-MAXIMUMLEVEL.
           IF WS-REDUN-TYPE = "N" OR = "X" OR = "Y"
            IF WS-QTY NOT > 0
               GO TO PRR-005.
           IF WS-REDUN-TYPE = "Z"
            IF WS-QTY > 0
               GO TO PRR-005.

            MOVE WS-QTY TO ST-QTYONHAND.
       PRR-011.
           IF LINE-CNT < 58
               GO TO PRR-020.
       PRR-015.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.
           MOVE " " TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD3
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD4
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE 8 TO LINE-CNT.
       PRR-020.
           IF ST-CATEGORY NOT = WS-STORE
               PERFORM PRR-025.
           IF WS-CAT-ONLY = "N"
              MOVE ST-STOCKNUMBER   TO D-STOCK
              MOVE ST-DESCRIPTION1  TO D-DESC1
              MOVE ST-DESCRIPTION2  TO D-DESC2
              MOVE ST-QTYONHAND     TO D-ONHAND
              MOVE ST-PRICE         TO D-PRICE.
       PRR-021.
           IF WS-COST-SELL = "Y"
              MOVE ST-AVERAGECOST   TO D-AVECOST
              MOVE ST-LASTCOST      TO D-REPCOST
              COMPUTE WS-AVECOST = (ST-QTYONHAND * ST-AVERAGECOST)
              COMPUTE WS-REPCOST = (ST-QTYONHAND * ST-LASTCOST)
              MOVE WS-AVECOST       TO D-AVECOSTTOT
              MOVE WS-REPCOST       TO D-REPCOSTTOT.
           IF WS-COST-SELL = "N"
              COMPUTE WS-AVECOST = (ST-QTYONHAND * ST-PRICE)
              MOVE WS-AVECOST       TO D-AVECOSTTOT.
              
           MOVE ST-LASTSALEDATE     TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE        TO D-LASTSOLD.
           
           MOVE ST-DATE-CREATED     TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE        TO D-CREATED.
           ADD WS-AVECOST TO WS-AVECOSTTOT
                             WS-AVETOTAL.
           ADD WS-REPCOST TO WS-REPCOSTTOT
                             WS-REPTOTAL.
           
           IF WS-ANALYSIS = "S" OR = "D" OR "X"
              MOVE WS-STQTYONHAND TO ST-QTYONHAND
              PERFORM REWRITE-STOCK.
           
           IF WS-CAT-ONLY = "Y"
                GO TO PRR-005.
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           ADD 1 TO LINE-CNT.
           GO TO PRR-005.
       PRR-023.
           COMPUTE WS-AVECOST = (ST-QTYONHAND * ST-AVERAGECOST).
           COMPUTE WS-REPCOST = (ST-QTYONHAND * ST-LASTCOST).
           ADD WS-AVECOST TO WS-AVECOSTTOT
                             WS-AVETOTAL.
           ADD WS-REPCOST TO WS-REPCOSTTOT
                             WS-REPTOTAL.
           GO TO PRR-005.
       PRR-025.
           MOVE WS-STORE      TO T-CATEGORY.
           MOVE WS-AVECOSTTOT TO T-AVECOST.
           MOVE WS-REPCOSTTOT TO T-REPCOST.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           WRITE PRINT-REC.
           ADD 2 TO LINE-CNT.
           MOVE 1855 TO POS
           DISPLAY "Category: " AT POS
           MOVE 1865 TO POS.
           DISPLAY WS-STORE AT POS.
           MOVE 1955 TO POS.
           DISPLAY "Ave Cost: R" AT POS.
           MOVE 1966 TO POS.
           MOVE WS-AVECOSTTOT TO WS-AVEVALUE.
           DISPLAY WS-AVEVALUE AT POS.
           MOVE 2055 TO POS.
           DISPLAY "Rep Cost: R" AT POS.
           MOVE 2066 TO POS.
           MOVE WS-REPCOSTTOT TO WS-REPVALUE.
           DISPLAY WS-REPVALUE AT POS.
           MOVE 0 TO WS-AVECOST
                     WS-AVECOSTTOT
                     WS-REPCOST
                     WS-REPCOSTTOT.
           MOVE ST-CATEGORY TO WS-STORE.
       PRR-999.
           EXIT.
      *
       REWRITE-STOCK SECTION.
       RWS-005.
      *     IF ST-ANALYSIS = "D"
      *        GO TO RWS-999.
           IF WS-ANALYSIS = "S"
             MOVE "S" TO ST-ANALYSIS.
           IF WS-ANALYSIS = "D"
             MOVE "D" TO ST-ANALYSIS.
           IF WS-ANALYSIS = "X"
             MOVE 0.01 TO ST-AVERAGECOST
             MOVE "D"  TO ST-ANALYSIS.
       RWS-010.
           REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK FILE BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RWS-010.
       RWS-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-045.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-045.
       OPEN-150.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
           EXIT.
      *   
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 58
                PERFORM PRR-015.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE "   **TOTALS**  " TO T-CATEGORY
           MOVE WS-AVETOTAL       TO T-AVECOST
           MOVE WS-REPTOTAL       TO T-REPCOST
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           IF WS-REDUN-TYPE = "Y"
             MOVE "Y= *** ALL REDUNDANT ITEMS PRINTED ****" TO 
                 PRINT-REC.
           IF WS-REDUN-TYPE = "N"
             MOVE
           "N= *** ONLY REDUNDANT ITEMS WHERE QTY > 0 PRINTED ****"
              TO PRINT-REC.
           IF WS-REDUN-TYPE = "X"
             MOVE
           "X= *** ONLY REDUNDANT ITEMS WHERE QTY > 0, '*'=QTY ON" &
           " HAND IS OVER MAXIMUM WERE PRINTED ****"
              TO PRINT-REC.
           IF WS-REDUN-TYPE = "Z"
             MOVE 
           "Z= *** ONLY REDUNDANT ITEMS WHERE QTY = 0 PRINTED ****"
              TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           MOVE " " TO PRINT-REC.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE STOCK-MASTER.
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
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
