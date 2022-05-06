        IDENTIFICATION DIVISION.
        PROGRAM-ID. StValuRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
         Copy "SelectStTrans".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-CAT               PIC X(3) VALUE " ".
       77  WS-BEGSTOCK          PIC X(15) VALUE " ".
       77  WS-ENDSTOCK          PIC X(15) VALUE " ".
       77  WS-CAT-ONLY          PIC X VALUE " ".
       77  WS-NON-STOCK         PIC X VALUE " ".
       77  WS-AVECOST           PIC S9(8)V99 VALUE 0.
       77  WS-REPCOST           PIC S9(8)V99 VALUE 0.
       77  WS-AVECOSTTOT        PIC S9(8)V99 VALUE 0.
       77  WS-REPCOSTTOT        PIC S9(8)V99 VALUE 0.
       77  WS-BORDER            PIC S9(8)V99 VALUE 0.
       77  WS-BORDERTOTAL       PIC S9(8)V99 VALUE 0.
       77  WS-BORDERMAXTOT      PIC S9(8)V99 VALUE 0.
       77  WS-AVETOTAL          PIC S9(8)V99 VALUE 0.
       77  WS-REPTOTAL          PIC S9(8)V99 VALUE 0.
       77  WS-MAXAVECOST        PIC S9(8)V99 VALUE 0.
       77  WS-MAXREPCOST        PIC S9(8)V99 VALUE 0.
       77  WS-MAXAVECOSTTOT     PIC S9(8)V99 VALUE 0.
       77  WS-MAXREPCOSTTOT     PIC S9(8)V99 VALUE 0.
       77  WS-MAXAVETOTAL       PIC S9(8)V99 VALUE 0.
       77  WS-MAXREPTOTAL       PIC S9(8)V99 VALUE 0.
       77  WS-MAXMARGINAVE      PIC S9(8)V99 VALUE 0.
       77  WS-MAXMARGINREP      PIC S9(8)V99 VALUE 0.
       77  WS-AVEVALUE          PIC Z(7)9.99-.
       77  WS-REPVALUE          PIC Z(7)9.99-.
       77  WS-DATE-ACCEPT       PIC X(10) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1   PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X VALUE " ".
           03  SP-REST          PIC X(14) VALUE " ".
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
           03  FILLER         PIC X(22) VALUE " ".
           03  FILLER         PIC X(15) VALUE "S T O C K   V A".
           03  FILLER         PIC X(17) VALUE " L U A T I O N   ".
           03  FILLER         PIC X(20) VALUE "R E P O R T".
           03  H-SINCE        PIC X(11) VALUE "S I N C E: ".
           03  H-DATE         PIC X(10).
           03  FILLER         PIC X(7) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(39) VALUE " ".
           03  FILLER         PIC X(43) VALUE ALL "*".
           03  FILLER         PIC X(50) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(68) VALUE " ".
           03  FILLER         PIC X(42) VALUE "STOCK ON:".
           03  FILLER         PIC X(22) VALUE "VALUE AT     VALUE AT".
       01  HEAD4.
           03  FILLER         PIC X(18) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(49) VALUE "DESCRIPTION".
           03  FILLER         PIC X(19) VALUE "HAND RESERVE".
           03  FILLER         PIC X(24) VALUE "AVE COST     REP COST".
           03  FILLER         PIC X(22) VALUE "AVE COST     REP COST".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(18) VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(26) VALUE " ".
           03  D-ONHAND       PIC Z(5)9.
           03  D-ONRES        PIC Z(5)9.
           03  FILLER         PIC X(6) VALUE " ".
           03  D-AVECOST      PIC Z(7)9.99.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-REPCOST      PIC Z(7)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-AVECOSTTOT   PIC Z(7)9.99.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-REPCOSTTOT   PIC Z(7)9.99.
           03  FILLER         PIC X(1) VALUE " ".
       01  TOT-HEAD.
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(19) VALUE "CATEGORY".
           03  FILLER         PIC X(49) VALUE
             "MAX AVE     AVE COST       EXCESS     RESERVE".
           03  FILLER         PIC X(59) VALUE
             "MAX REP     REP COST       EXCESS".
       01  TOTAL-LINE.
           03  FILLER         PIC X(5) VALUE " ".
           03  T-CATEGORY     PIC X(15) VALUE " ".
           03  T-MAXAVECOST   PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  T-AVECOST      PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  T-MARGINAVE    PIC Z(7)9.99-.
           03  T-RESERVE      PIC Z(7)9.99-.
           03  T-MAXREPCOST   PIC Z(7)9.99-.
      *     03  FILLER         PIC X(2) VALUE " ".
           03  T-REPCOST      PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  T-MARGINREP    PIC Z(7)9.99-.
           03  FILLER         PIC X(25) VALUE " ".
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
           DISPLAY "** STOCK VALUATION REPORT **" AT POS
           MOVE 421 TO POS
           DISPLAY "****************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1210 TO POS.
           DISPLAY "FROM STOCK NUMBER : [               ]" AT POS.
           MOVE 1231 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9        TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-BEGSTOCK.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-012
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-012.
           MOVE 1310 TO POS.
           DISPLAY "  TO STOCK NUMBER : [               ]" AT POS.
           MOVE 1331 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ENDSTOCK.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           IF WS-ENDSTOCK = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
       CONTROL-015.
           MOVE 1510 TO POS.
           DISPLAY "  CATEGORIES ONLY : [ ]" AT POS.
           MOVE 1531 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
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
           MOVE "Y" TO WS-NON-STOCK
           MOVE 1710 TO POS
           DISPLAY "INCLUDE NON STOCK : [ ]" AT POS
           MOVE 1731 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-NON-STOCK.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF WS-NON-STOCK NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-022
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-022.
           MOVE 0 TO WS-DATE-ENTER.
           MOVE 1910 TO POS.
           DISPLAY "ENTER THE LAST DATE STOCK WAS SOLD : [          ]"
               AT POS.
           MOVE 2010 TO POS.
           DISPLAY "                    Enter the DATE as DD/MM/YYYY"
             AT POS.
           MOVE 2111 TO POS.
           DISPLAY "LEAVE BLANK FOR ALL ITEMS." AT POS.
           MOVE 1948 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATE-ACCEPT.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-020.
               
           IF WS-DATE-ACCEPT = " "
              MOVE " " TO H-DATE H-SINCE
              GO TO CONTROL-025.
               
           MOVE WS-DATE-ACCEPT TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO CONTROL-022.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE 1948 TO POS.
           DISPLAY DISPLAY-DATE AT POS.
           MOVE DISPLAY-DATE TO H-DATE.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-DATE-ENTER.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO CONTROL-022.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-022.
       CONTROL-025.
           MOVE 2810 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
             AT POS.
           PERFORM OPEN-FILES.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           IF WS-NON-STOCK = "Y"
              PERFORM PRINT-NON-STOCK-ITEMS.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           IF WS-NON-STOCK = "Y"
              MOVE "   " TO WS-CAT. 
           PERFORM ERROR1-020
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS
           DISPLAY "READING STOCK ITEMS ...                " AT POS.
           MOVE WS-BEGSTOCK TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       PRR-005.
           READ STOCK-MASTER NEXT
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
           IF ST-STOCKNUMBER < WS-BEGSTOCK
              GO TO PRR-005.
           IF ST-STOCKNUMBER > WS-ENDSTOCK
              PERFORM PRR-025
              GO TO PRR-999.

           MOVE 2510 TO POS
           DISPLAY "STOCKNUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

           IF WS-CAT = "   "
              MOVE ST-CATEGORY TO WS-CAT.
              
           MOVE ST-LASTSALEDATE TO WS-CALC-DATE.
           IF WS-DATE-ACCEPT = " "
               GO TO PRR-010.
           IF WS-DATE-ACCEPT > " "
            IF WS-CALC-DATE > WS-DATE-ENTER
               GO TO PRR-010
            ELSE            
               GO TO PRR-005.
       PRR-010.
            IF LINE-CNT < 60
              GO TO PRR-020.
            ADD 1         TO PAGE-CNT.
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
            WRITE PRINT-REC.
            IF WS-CAT-ONLY = "N"
              WRITE PRINT-REC FROM HEAD3
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC FROM HEAD4
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC
              MOVE 8 TO LINE-CNT
            ELSE
              WRITE PRINT-REC FROM TOT-HEAD
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC
              MOVE 7 TO LINE-CNT.
       PRR-020.
           IF ST-CATEGORY NOT = WS-CAT
               PERFORM PRR-025.
           IF WS-CAT-ONLY = "Y"
               GO TO PRR-023.
           MOVE ST-STOCKNUMBER   TO D-STOCK
           MOVE ST-DESCRIPTION1  TO D-DESC1
           MOVE ST-DESCRIPTION2  TO D-DESC2
           MOVE ST-QTYONHAND     TO D-ONHAND
           MOVE ST-QTYONRESERVE  TO D-ONRES
           MOVE ST-AVERAGECOST   TO D-AVECOST
           MOVE ST-LASTCOST      TO D-REPCOST.
           
           COMPUTE WS-AVECOST =
                (ST-QTYONHAND + ST-QTYONRESERVE) * ST-AVERAGECOST.
           COMPUTE WS-REPCOST =
                (ST-QTYONHAND + ST-QTYONRESERVE) * ST-LASTCOST.
           COMPUTE WS-BORDER = ST-QTYONRESERVE * ST-AVERAGECOST.
           
           MOVE WS-AVECOST       TO D-AVECOSTTOT.
           MOVE WS-REPCOST       TO D-REPCOSTTOT.
           ADD WS-AVECOST TO WS-AVECOSTTOT
                             WS-AVETOTAL.
           ADD WS-REPCOST TO WS-REPCOSTTOT
                             WS-REPTOTAL.
           ADD WS-BORDER  TO WS-BORDERTOTAL
                             WS-BORDERMAXTOT.
           COMPUTE WS-MAXAVECOST = ST-MAXIMUMLEVEL * ST-AVERAGECOST.
           COMPUTE WS-MAXREPCOST = ST-MAXIMUMLEVEL * ST-LASTCOST.
           ADD WS-MAXAVECOST TO WS-MAXAVECOSTTOT
                                WS-MAXAVETOTAL.
           ADD WS-MAXREPCOST TO WS-MAXREPCOSTTOT
                                WS-MAXREPTOTAL.
           WRITE PRINT-REC FROM DETAIL-LINE.
           ADD 1 TO LINE-CNT.
           GO TO PRR-005.
       PRR-023.
           COMPUTE WS-AVECOST =
                (ST-QTYONHAND + ST-QTYONRESERVE) * ST-AVERAGECOST.
           COMPUTE WS-REPCOST =
                (ST-QTYONHAND + ST-QTYONRESERVE) * ST-LASTCOST.
           COMPUTE WS-BORDER = ST-QTYONRESERVE * ST-AVERAGECOST.
           
           COMPUTE WS-MAXAVECOST = ST-MAXIMUMLEVEL * ST-AVERAGECOST.
           COMPUTE WS-MAXREPCOST = ST-MAXIMUMLEVEL * ST-LASTCOST.
           ADD WS-AVECOST TO WS-AVECOSTTOT
                             WS-AVETOTAL.
           ADD WS-REPCOST TO WS-REPCOSTTOT
                             WS-REPTOTAL.
           ADD WS-MAXAVECOST TO WS-MAXAVECOSTTOT
                                WS-MAXAVETOTAL.
           ADD WS-MAXREPCOST TO WS-MAXREPCOSTTOT
                                WS-MAXREPTOTAL.
           ADD WS-BORDER  TO WS-BORDERTOTAL
                             WS-BORDERMAXTOT.
           GO TO PRR-005.
       PRR-025.
           MOVE WS-CAT           TO T-CATEGORY
           MOVE WS-AVECOSTTOT    TO T-AVECOST
           MOVE WS-REPCOSTTOT    TO T-REPCOST
           MOVE WS-MAXAVECOSTTOT TO T-MAXAVECOST
           MOVE WS-MAXREPCOSTTOT TO T-MAXREPCOST
      *     COMPUTE WS-MAXMARGINAVE = WS-AVECOSTTOT - WS-MAXAVECOSTTOT
      *     COMPUTE WS-MAXMARGINREP = WS-REPCOSTTOT - WS-MAXREPCOSTTOT
           COMPUTE WS-MAXMARGINAVE =
             WS-AVECOSTTOT - WS-BORDERTOTAL - WS-MAXAVECOSTTOT.
           COMPUTE WS-MAXMARGINREP =
             WS-REPCOSTTOT - WS-BORDERTOTAL - WS-MAXREPCOSTTOT.
           MOVE WS-MAXMARGINAVE  TO T-MARGINAVE
           MOVE WS-MAXMARGINREP  TO T-MARGINREP
           MOVE WS-BORDERTOTAL   TO T-RESERVE.
           IF WS-CAT-ONLY NOT = "Y"
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC FROM TOT-HEAD.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           WRITE PRINT-REC
           IF WS-CAT-ONLY NOT = "Y"
              ADD 3 TO LINE-CNT
           ELSE
              ADD 2 TO LINE-CNT.
           MOVE 1810 TO POS
           DISPLAY "Category: " AT POS
           MOVE 1820 TO POS
           DISPLAY WS-CAT AT POS
           MOVE 1910 TO POS
           DISPLAY "Valuation At Ave Cost : R" AT POS
           MOVE 1935 TO POS
           MOVE WS-AVECOSTTOT TO WS-AVEVALUE
           DISPLAY WS-AVEVALUE AT POS
           MOVE 2010 TO POS
           DISPLAY "Valuation At Rep Cost : R" AT POS
           MOVE 2035 TO POS
           MOVE WS-REPCOSTTOT TO WS-REPVALUE
           DISPLAY WS-REPVALUE AT POS.
           MOVE 0 TO WS-AVECOST
                     WS-AVECOSTTOT
                     WS-BORDER
                     WS-BORDERTOTAL
                     WS-REPCOST
                     WS-REPCOSTTOT
                     WS-MAXAVECOST
                     WS-MAXAVECOSTTOT
                     WS-MAXREPCOST
                     WS-MAXREPCOSTTOT
                     WS-MAXMARGINAVE
                     WS-MAXMARGINREP.
           IF ST-CATEGORY = "ZZZ"
            IF WS-CAT-ONLY = "Y"
              PERFORM END-000.
           MOVE ST-CATEGORY TO WS-CAT.
       PRR-999.
           EXIT.
      *
       PRINT-NON-STOCK-ITEMS SECTION.
       PNSI-000.
            MOVE 2610 TO POS
            DISPLAY "READING NON STOCK ITEMS ON P/SLIPS...." AT POS.
            IF WS-BEGSTOCK = "    "
               MOVE "/"      TO WS-BEGSTOCK.
            MOVE "N"         TO STTR-ST-COMPLETE
            MOVE WS-BEGSTOCK TO STTR-STOCK-NUMBER.
            START STOCK-TRANS-FILE KEY NOT < STTR-ST-KEY
                  INVALID KEY NEXT SENTENCE.
       PNSI-002.
            READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-STTRANS-ST1 = 10
               PERFORM PNSI-025
               GO TO PNSI-999.
            IF WS-STTRANS-ST1 NOT = 0
              MOVE "STTRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PNSI-002.
               
            IF STTR-ST-COMPLETE NOT = "N" AND NOT = "B" AND NOT = "C"
               GO TO PNSI-999.
            IF STTR-STOCK-NUMBER < "/"
               GO TO PNSI-002.
            IF STTR-STOCK-NUMBER > "A"
               PERFORM PNSI-025
               GO TO PNSI-999.
            IF STTR-TYPE NOT = 4 AND NOT = 7
               GO TO PNSI-002.

            MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
               GO TO PNSI-002.

            MOVE 2648 TO POS
            DISPLAY STTR-STOCK-NUMBER AT POS.

            IF STTR-SHIPQTY > 0
               GO TO PNSI-005.

            GO TO PNSI-002.
       PNSI-005.
            IF SP-1STCHAR = "/"
               MOVE "/  " TO WS-CAT. 
       PNSI-010.
            IF LINE-CNT < 60
               GO TO PNSI-020.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE
            MOVE " " TO PRINT-REC.
            IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            MOVE " "      TO PRINT-REC.
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
            WRITE PRINT-REC.
            IF WS-CAT-ONLY = "N"
              WRITE PRINT-REC FROM HEAD3
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC FROM HEAD4
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC
              MOVE 7 TO LINE-CNT
            ELSE
              WRITE PRINT-REC FROM TOT-HEAD
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC
            MOVE 6 TO LINE-CNT.
       PNSI-020.
           IF SP-1STCHAR = "/"
               MOVE "/  " TO WS-CAT. 
           IF WS-CAT-ONLY = "Y"
               GO TO PNSI-023.
           MOVE STTR-STOCK-NUMBER       TO D-STOCK
           MOVE STTR-DESC1              TO D-DESC1
           MOVE STTR-DESC2              TO D-DESC2
           MOVE STTR-SHIPQTY            TO D-ONHAND
           MOVE 0                       TO D-ONRES
           MOVE STTR-COST-VALUE         TO D-AVECOST
           MOVE 0                       TO D-REPCOST.
           
           COMPUTE WS-AVECOST = STTR-SHIPQTY * STTR-COST-VALUE.
           MOVE 0 TO WS-REPCOST WS-BORDER.
           
           MOVE WS-AVECOST       TO D-AVECOSTTOT.
           MOVE WS-REPCOST       TO D-REPCOSTTOT.
           ADD WS-AVECOST TO WS-AVECOSTTOT
                             WS-AVETOTAL.
           ADD WS-REPCOST TO WS-REPCOSTTOT
                             WS-REPTOTAL.
           ADD WS-BORDER  TO WS-BORDERTOTAL
                             WS-BORDERMAXTOT.
           MOVE 0 TO WS-MAXAVECOST
                     WS-MAXREPCOST.

           WRITE PRINT-REC FROM DETAIL-LINE.
           ADD 1 TO LINE-CNT.
           GO TO PNSI-002.
       PNSI-023.
           COMPUTE WS-AVECOST = STTR-SHIPQTY * STTR-COST-VALUE.
           MOVE 0 TO WS-REPCOST WS-BORDER.
           
           ADD WS-AVECOST TO WS-AVECOSTTOT
                             WS-AVETOTAL.
           ADD WS-REPCOST TO WS-REPCOSTTOT
                             WS-REPTOTAL.
           GO TO PNSI-002.
       PNSI-025.
           IF LINE-CNT > 60 
              PERFORM PNSI-010.
           MOVE WS-CAT           TO T-CATEGORY
           MOVE WS-AVECOSTTOT    TO T-AVECOST
           MOVE WS-REPCOSTTOT    TO T-REPCOST
           MOVE WS-MAXAVECOSTTOT TO T-MAXAVECOST
           MOVE WS-MAXREPCOSTTOT TO T-MAXREPCOST
           MOVE WS-MAXMARGINAVE  TO T-MARGINAVE
           MOVE WS-MAXMARGINREP  TO T-MARGINREP
           MOVE WS-BORDERTOTAL   TO T-RESERVE.
           IF WS-CAT-ONLY NOT = "Y"
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC FROM TOT-HEAD.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           WRITE PRINT-REC
           IF WS-CAT-ONLY NOT = "Y"
              ADD 3 TO LINE-CNT
           ELSE
              ADD 2 TO LINE-CNT.
           MOVE 1810 TO POS
           DISPLAY "Category: " AT POS
           MOVE 1820 TO POS
           DISPLAY WS-CAT AT POS
           MOVE 1910 TO POS
           DISPLAY "Valuation At Ave Cost : R" AT POS
           MOVE 1935 TO POS
           MOVE WS-AVECOSTTOT TO WS-AVEVALUE
           DISPLAY WS-AVEVALUE AT POS
           MOVE 2010 TO POS
           DISPLAY "Valuation At Rep Cost : R" AT POS
           MOVE 2035 TO POS
           MOVE WS-REPCOSTTOT TO WS-REPVALUE
           DISPLAY WS-REPVALUE AT POS.
           MOVE 0 TO WS-AVECOST
                     WS-AVECOSTTOT
                     WS-BORDER
                     WS-BORDERTOTAL
                     WS-REPCOST
                     WS-REPCOSTTOT
                     WS-MAXAVECOST
                     WS-MAXAVECOSTTOT
                     WS-MAXREPCOST
                     WS-MAXREPCOSTTOT
                     WS-MAXMARGINAVE
                     WS-MAXMARGINREP.
       PNSI-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-003.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-003.
       OPEN-055.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-055.
       OPEN-060.
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
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE "TOTALS***"    TO T-CATEGORY
           MOVE WS-AVETOTAL    TO T-AVECOST
           MOVE WS-REPTOTAL    TO T-REPCOST
           MOVE WS-MAXAVETOTAL TO T-MAXAVECOST
           MOVE WS-MAXREPTOTAL TO T-MAXREPCOST
           COMPUTE WS-MAXMARGINAVE =
            WS-AVETOTAL - WS-MAXAVETOTAL - WS-BORDERMAXTOT
           COMPUTE WS-MAXMARGINREP =
            WS-REPTOTAL - WS-MAXREPTOTAL - WS-BORDERMAXTOT
           MOVE WS-MAXMARGINAVE TO T-MARGINAVE
           MOVE WS-MAXMARGINREP TO T-MARGINREP
           MOVE WS-BORDERMAXTOT TO T-RESERVE
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           WRITE PRINT-REC AFTER 2.
           ADD 4 TO LINE-CNT.
       END-450.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE STOCK-MASTER
                  STOCK-TRANS-FILE.
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
