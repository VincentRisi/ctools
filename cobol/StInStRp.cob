        IDENTIFICATION DIVISION.
        PROGRAM-ID. StInStRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectStMaster".
         Copy "SelectStTrans".
         Copy "SelectSlRegister".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
              ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdStock.
           COPY ChlfdStTrans.
           COPY ChlfdRegister.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-QTY               PIC S9(6) VALUE 0.
       77  WS-CREDQTY           PIC S9(6) VALUE 0.
       77  WS-TOTALQTY          PIC S9(6) VALUE 0.
       77  WS-QTY-DIS           PIC Z(5)9.
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-RANGE3            PIC X(7) VALUE " ".
       77  WS-RANGE4            PIC X(7) VALUE " ".
       77  WS-DET-SUM           PIC X VALUE " ".
       77  WS-CAT               PIC X(3) VALUE " ".
       77  WS-VALUE             PIC S9(6)V99 VALUE 0.
       77  WS-TOTAL-VALUE       PIC S9(7)V99 VALUE 0.
       77  WS-RUNNING-TOTAL     PIC S9(7)V99 VALUE 0.
       01  WS-CALC-DATE.
           03  WS-YYC           PIC 9999.
           03  WS-MMC           PIC 99.
           03  WS-DDC           PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1   PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X VALUE " ".
           03  SP-REST          PIC X(14) VALUE " ".
       01  END-COMMENT.
           03  END-1STCHARS     PIC X(27) VALUE " ".
           03  END-BEG-DATE     PIC X(10) VALUE " ".
           03  END-2NDCHARS     PIC X(14) VALUE " ".
           03  END-END-DATE     PIC X(10) VALUE " ".
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(70) VALUE
           "STOCK SALES ANALYSIS BY ACCOUNT NUMBER REPORT".
           03  H1-TYPE        PIC X(23) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
       01  HEAD2.
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(45) VALUE ALL "*".
           03  FILLER         PIC X(57) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(15) VALUE "ACCOUNT NUMBER:".
           03  H4-ACC         PIC X(10).
           03  FILLER         PIC X(6) VALUE "NAME:".
           03  H4-NAME        PIC X(101).
       01  HEAD5.
           03  FILLER         PIC X(16) VALUE "NUMBER".
           03  FILLER         PIC X(43) VALUE "DESCRIPTION".
           03  FILLER         PIC X(9) VALUE "INVOICE".
           03  FILLER         PIC X(12) VALUE " INV DATE".
           03  FILLER         PIC X(17) VALUE "ORDER  SHIPD".
           03  FILLER         PIC X(32) VALUE
            "PRICE    VALUE  DISC  ACCOUNT".
       01  DETAIL-LINE.
           03  D-STOCKNO      PIC X(16).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(24).
           03  D-INVOICE      PIC Z(5)9.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-DATE         PIC X(11).
           03  D-ORDER        PIC Z(5)9-.
           03  D-SHIP         PIC Z(5)9-.
           03  D-PRICE        PIC Z(5)9.99.
           03  D-VALUE        PIC Z(5)9.99-.
           03  D-DISC         PIC Z9.99.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-ACCOUNT      PIC X(7).
       01  TOTAL-LINE.
           03  FILLER         PIC X(48) VALUE " ".
           03  FILLER         PIC X(13) VALUE "TOTALS FOR :".
           03  TOT-STOCK      PIC X(20).
           03  FILLER         PIC X(5) VALUE "QTY:".
           03  TOT-QTY        PIC Z(5)9.
           03  FILLER         PIC X(9) VALUE " ".
           03  TOT-VALUE      PIC Z(6)9.99.
           03  FILLER         PIC X(12) VALUE " ".
       01  RUNNING-LINE.
           03  FILLER         PIC X(48) VALUE " ".
           03  RUN-DESC       PIC X(37) VALUE
            "TOTAL No OF ITEMS & VALUE:".
           03  RUN-QTY        PIC Z(6)9.
           03  FILLER         PIC X(9) VALUE " ".
           03  RUN-VALUE      PIC Z(6)9.99.
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
           MOVE 315 TO POS
           DISPLAY "** STOCK SALES ANALYSIS BY ACCOUNT NUMBER REPORT **"
            AT POS
           MOVE 415 TO POS
           DISPLAY "***************************************************"
            AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM OPEN-FILES
           PERFORM GET-DATA.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM READ-STOCK.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO WS-RANGE1 WS-RANGE2.
            MOVE 1020 TO POS.
            DISPLAY "   FROM STOCK NUMBER   : [               ]" AT POS.
            MOVE 1046 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 45        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

            IF W-ESCAPE-KEY = 4
               PERFORM END-500
               GO TO CONTROL-005.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
            MOVE 1220 TO POS.
            DISPLAY "        TO STOCK NUMBER: [               ]" AT POS.
            MOVE 1246 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 45        TO CDA-COL.
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
           MOVE 1510 TO POS
           DISPLAY "   OR ENTER 'ALL' TO PRINT FOR ALL ACC'S" AT POS
           MOVE 1410 TO POS
           DISPLAY "BEGINNING ACCOUNT NUMBER TO PRINT: [       ]" AT POS
           ADD 36 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 45        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE3.

           IF W-ESCAPE-KEY = 4
              GO TO GET-010.
           
           IF WS-RANGE3 = " "
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-023
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-020.
       GET-023.
            IF WS-RANGE3 NOT = "ALL"
               GO TO GET-025.
            IF WS-RANGE3 = "ALL"
                MOVE " "            TO H4-ACC
                MOVE "ALL ACCOUNTS" TO H4-NAME DR-NAME
                GO TO GET-025.
       GET-0231.
            MOVE WS-RANGE3 TO DR-ACCOUNT-NUMBER.
            START DEBTOR-MASTER KEY NOT < DR-KEY
              INVALID KEY NEXT SENTENCE.
            READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "INVALID NUMBER ENTERED, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-020.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO GET-0231.
       GET-024.
            MOVE 1620 TO POS
            DISPLAY "Account Name:" AT POS
            ADD 14 TO POS
            DISPLAY DR-NAME AT POS.
       GET-025.
           MOVE 1610 TO POS
           DISPLAY
           "   ENDING ACCOUNT NUMBER TO PRINT: [       ]" AT POS
           ADD 36 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 45        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE4.

           IF W-ESCAPE-KEY = 4
              GO TO GET-020.
           
           IF WS-RANGE4 = " "
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-025.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-060
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-025.
       GET-060.
            IF WS-RANGE4 > WS-RANGE3
                MOVE " "              TO H4-ACC
                MOVE "RANGE OF ACC'S" TO H4-NAME DR-NAME
            ELSE
                PERFORM GET-0231
                MOVE DR-ACCOUNT-NUMBER TO H4-ACC
                MOVE DR-NAME           TO H4-NAME.
            MOVE 1710 TO POS
            DISPLAY " ENTER D=DETAIL, S=SUMMARY       : [ ]" AT POS
            ADD 36 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 45        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DET-SUM.

            IF W-ESCAPE-KEY = 4
               GO TO GET-020.
            IF WS-DET-SUM NOT = "D" AND NOT = "S"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-060.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-065
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-060.
       GET-065.
           PERFORM OPEN-010.
           MOVE 1       TO WS-DD
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO F-FIELDNAME.
           MOVE 2008 TO POS.
           DISPLAY "ENTER THE START DATE STOCK WAS SOLD: [          ]"
               AT POS.
           MOVE 2110 TO POS.
           DISPLAY "Enter the DATE as DD/MM/YYYY"  AT POS.
           MOVE 2046 TO POS.

           MOVE F-FIELDNAME TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 45        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO F-FIELDNAME.

           IF W-ESCAPE-KEY = 4
               GO TO GET-060.
           MOVE F-FIELDNAME TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO GET-065.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE 2008 TO POS.
           DISPLAY "ENTER THE START DATE STOCK WAS SOLD: [          ]"
               AT POS.
           MOVE 2046 TO POS.
           DISPLAY DISPLAY-DATE AT POS.
           MOVE DISPLAY-DATE TO H1-DATE END-BEG-DATE.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE   TO WS-BEG-DATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO GET-065.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO GET-070
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-065.
       GET-070.
           MOVE 2308 TO POS.
           DISPLAY "ENTER THE END DATE STOCK WAS SOLD  : [          ]"
               AT POS.
           MOVE 2410 TO POS.
           DISPLAY "Enter the DATE as DD/MM/YYYY"  AT POS.
           MOVE 2346 TO POS.

           MOVE F-FIELDNAME TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 20        TO CDA-ROW.
           MOVE 45        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO F-FIELDNAME.

           IF W-ESCAPE-KEY = 4
               GO TO GET-065.
           MOVE F-FIELDNAME TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO GET-070.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE 2308 TO POS.
           DISPLAY "ENTER THE END DATE STOCK WAS SOLD  : [          ]"
               AT POS.
           MOVE 2346 TO POS.
           DISPLAY DISPLAY-DATE AT POS
           MOVE DISPLAY-DATE TO H1-DATE END-END-DATE
           PERFORM CONVERT-SPLIT-FORMAT
           MOVE SPLIT-DATE   TO WS-END-DATE
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO GET-070.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO GET-900
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-070.
       GET-900.
            MOVE 2820 TO POS.
            DISPLAY "The Report Is Being Run........" AT POS.
       GET-999.
            EXIT.
      *
       READ-STOCK SECTION.
       RS-001.
            MOVE "Y"               TO STTR-ST-COMPLETE
            MOVE WS-RANGE1         TO STTR-STOCK-NUMBER.
            START STOCK-TRANS-FILE KEY NOT < STTR-ST-KEY
                  INVALID KEY NEXT SENTENCE.
            MOVE WS-RANGE1         TO ST-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY
                  INVALID KEY NEXT SENTENCE.
       RS-006.
            READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE. 
            IF WS-STOCK-ST1 = 10
               GO TO RS-999.
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "STOCK INVALID KEY, 'ESC' TO EXIT."
                 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RS-999.
             MOVE "STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RS-006.
            IF ST-STOCKNUMBER < WS-RANGE1
               GO TO RS-006.
            IF ST-STOCKNUMBER > WS-RANGE2
               GO TO RS-999.
       RS-010.
            MOVE "Y"            TO STTR-ST-COMPLETE
            MOVE ST-STOCKNUMBER TO STTR-STOCK-NUMBER.
            START STOCK-TRANS-FILE KEY NOT < STTR-ST-KEY
                  INVALID KEY NEXT SENTENCE.
            PERFORM PRINT-ROUTINE.
            GO TO RS-006.
       RS-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-002.
            READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-STTRANS-ST1 = 10
               PERFORM SUB-TOTAL-LINE
               GO TO PRR-999.
            IF WS-STTRANS-ST1 NOT = 0
             MOVE "STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRR-002.
               
            IF STTR-STOCK-NUMBER NOT = ST-STOCKNUMBER
               PERFORM SUB-TOTAL-LINE
               GO TO PRR-999.
               
            IF STTR-DATE < WS-BEG-DATE
               GO TO PRR-002.
            IF STTR-DATE > WS-END-DATE
               GO TO PRR-002.
               
            IF STTR-TYPE NOT = 1 AND NOT = 6
               GO TO PRR-002.
               
            IF STTR-SHIPQTY = 0
               GO TO PRR-002.
               
      *      IF DR-NAME NOT = "ALL ACCOUNTS"
      *       IF STTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER   
      *         GO TO PRR-002.
            IF DR-NAME NOT = "ALL ACCOUNTS"
             IF STTR-ACCOUNT-NUMBER < WS-RANGE3
               GO TO PRR-002.
            IF DR-NAME NOT = "ALL ACCOUNTS"
             IF STTR-ACCOUNT-NUMBER > WS-RANGE4
               GO TO PRR-002.
            
            MOVE 2610 TO POS
            DISPLAY "STOCK NUMBER BEING READ:                  QTY:"
              AT POS
            ADD 25 TO POS
            DISPLAY STTR-STOCK-NUMBER AT POS.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE
            MOVE " "      TO PRINT-REC
            WRITE PRINT-REC.
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
            WRITE PRINT-REC FROM HEAD4
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD5
            MOVE " " TO PRINT-REC
            MOVE 7 TO LINE-CNT.
       PRR-020.
           IF WS-DET-SUM = "S"
              PERFORM PRR-030
              GO TO PRR-050.
           MOVE STTR-STOCK-NUMBER       TO D-STOCKNO
           MOVE STTR-DESC1              TO D-DESC1
           MOVE STTR-DESC2              TO D-DESC2
           MOVE STTR-REFERENCE1         TO D-INVOICE
           MOVE STTR-DATE               TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-DATE.
           IF STTR-TYPE = 6
               MOVE STTR-ORDERQTY       TO WS-CREDQTY
               COMPUTE WS-CREDQTY = WS-CREDQTY * -1
               MOVE WS-CREDQTY          TO D-ORDER
           
               MOVE STTR-SHIPQTY        TO WS-CREDQTY
               COMPUTE WS-CREDQTY = WS-CREDQTY * -1
               MOVE WS-CREDQTY          TO D-SHIP
           ELSE
              MOVE STTR-ORDERQTY        TO D-ORDER
              MOVE STTR-SHIPQTY         TO D-SHIP.

           MOVE STTR-PRICE              TO D-PRICE
           MOVE STTR-ITEMDISC           TO D-DISC
           MOVE STTR-ACCOUNT-NUMBER     TO D-ACCOUNT.
       PRR-030.
           MOVE STTR-SALES-VALUE TO WS-VALUE
           MOVE WS-VALUE         TO D-VALUE
           ADD WS-VALUE          TO WS-TOTAL-VALUE.
       PRR-040.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT.
       PRR-050.
           IF STTR-TYPE = 1
              ADD STTR-SHIPQTY TO WS-QTY
           ELSE
              SUBTRACT STTR-SHIPQTY FROM WS-QTY.
           MOVE WS-QTY TO WS-QTY-DIS
           MOVE 2657   TO POS
           DISPLAY WS-QTY-DIS AT POS
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       SUB-TOTAL-LINE SECTION.
       STL-005.
            IF LINE-CNT > 60
               MOVE 66 TO LINE-CNT
               PERFORM PRR-010.
           IF WS-QTY = 0
               GO TO STL-999.
           MOVE WS-TOTAL-VALUE   TO TOT-VALUE
           MOVE ST-STOCKNUMBER   TO TOT-STOCK
           MOVE WS-QTY           TO TOT-QTY
           IF WS-DET-SUM = "D"
              WRITE PRINT-REC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           
           ADD WS-QTY            TO WS-TOTALQTY
           ADD WS-TOTAL-VALUE    TO WS-RUNNING-TOTAL
           MOVE 0                TO WS-TOTAL-VALUE
                                    WS-QTY.
           MOVE " " TO PRINT-REC
           IF WS-DET-SUM = "D"
              WRITE PRINT-REC
              ADD 3 TO LINE-CNT
           ELSE
              ADD 2 TO LINE-CNT.
       STL-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O DEBTOR-MASTER
           IF WS-DEBTOR-ST1 NOT = 0 
              MOVE 0 TO WS-DEBTOR-ST1
              MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-002.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0 
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-002.
       OPEN-003.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-003.
       OPEN-004.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0 
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-004.
        OPEN-010.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
        OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 60
              MOVE 66 TO LINE-CNT
              PERFORM PRR-010.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE WS-TOTALQTY      TO RUN-QTY
           MOVE WS-RUNNING-TOTAL TO RUN-VALUE
           WRITE PRINT-REC FROM RUNNING-LINE AFTER 1.
           
           MOVE "SALES PRINTED, STARTING AT" TO END-1STCHARS
           MOVE " FINISHING AT"              TO END-2NDCHARS
           MOVE END-COMMENT TO PRINT-REC
           WRITE PRINT-REC AFTER 2.
       
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-500.
           CLOSE DEBTOR-MASTER
                 STOCK-MASTER
                 STOCK-TRANS-FILE
                 INCR-REGISTER.
       END-900.
           EXIT PROGRAM.
      *      STOP RUN.
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
