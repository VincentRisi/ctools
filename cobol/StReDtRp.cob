        IDENTIFICATION DIVISION.
        PROGRAM-ID. StReDtRp.
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
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(15) VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
       77  WS-ANSWER4           PIC X VALUE " ".
       77  WS-QTY               PIC S9(6) VALUE 0.
       77  WS-AVECOST           PIC 9(7)V99 VALUE 0.
       77  WS-REPCOST           PIC 9(7)V99 VALUE 0.
       77  WS-AVECOSTTOT        PIC 9(7)V99 VALUE 0.
       77  WS-REPCOSTTOT        PIC 9(7)V99 VALUE 0.
       77  WS-AVETOTAL          PIC 9(7)V99 VALUE 0.
       77  WS-REPTOTAL          PIC 9(7)V99 VALUE 0.
       77  WS-AVEVALUE          PIC Z(7)9.99.
       77  WS-REPVALUE          PIC Z(7)9.99.
       77  WS-DATE-ZERO         PIC X VALUE " ".
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
           03  FILLER         PIC X(23) VALUE " ".
           03  FILLER         PIC X(42) VALUE
           "S T O C K   R E C E I V E D   S I N C E :".
           03  H-DATE         PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(4) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(40) VALUE " ".
           03  FILLER         PIC X(52) VALUE ALL "*".
           03  FILLER         PIC X(42) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(62) VALUE " ".
           03  FILLER         PIC X(37) VALUE "ON     QTY RECEIVED".
           03  H3-1           PIC X(20) VALUE "VALUE AT".
           03  FILLER         PIC X(8) VALUE "LAST".
       01  HEAD4.
           03  FILLER         PIC X(16) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(45) VALUE "DESCRIPTION".
           03  FILLER         PIC X(7) VALUE "HAND".
           03  FILLER         PIC X(19) VALUE "MTD   YTD   L/Y".
           03  H4-1           PIC X(12) VALUE " ".
           03  H4-2           PIC X(12) VALUE " ".
           03  FILLER         PIC X(6) VALUE " ".
           03  FILLER         PIC X(20) VALUE "RECEIVED".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(16) VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(21) VALUE " ".
           03  FILLER         PIC X(1) VALUE " ".
           03  D-ONHAND       PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-REC-MTD      PIC Z(5)9.
           03  D-REC-YTD      PIC Z(5)9.
           03  D-REC-L-Y      PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-AVECOST      PIC Z(7)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-AVECOSTTOT   PIC Z(7)9.99.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-REC-DATE     PIC X(10).
      *     03  FILLER         PIC X(2) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(81) VALUE " ".
           03  T-CATEGORY     PIC X(15) VALUE " ".
           03  T-AVECOST      PIC Z(7)9.99.
           03  FILLER         PIC X(2) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 321 TO POS
           DISPLAY "** STOCK RECEIVED BY DATE REPORT **" AT POS
           MOVE 421 TO POS
           DISPLAY "***********************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1010 TO POS.
           DISPLAY "FROM STOCK NUMBER : [               ]" AT POS.
           MOVE 1031 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7        TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

      *     ACCEPT WS-ANSWER1 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-012
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-012.
           MOVE 1110 TO POS.
           DISPLAY "  TO STOCK NUMBER : [               ]" AT POS.
           MOVE 1131 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 8         TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

      *     ACCEPT WS-ANSWER2 AT POS.
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
           MOVE 1310 TO POS.
           DISPLAY "  CATEGORIES ONLY : [ ]" AT POS.
           MOVE 1331 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER3.

      *     ACCEPT WS-ANSWER3 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-012.
           IF WS-ANSWER3 NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
           PERFORM OPEN-150.
           MOVE 1       TO WS-DD
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO F-FIELDNAME.
           MOVE 1510 TO POS.
           DISPLAY 
           "ENTER THE START DATE STOCK WAS RECEIVED: [          ]"
               AT POS.
           MOVE 1610 TO POS.
           DISPLAY "Enter the DATE as DD/MM/YYYY"  AT POS.
           MOVE 1552 TO POS.

           MOVE F-FIELDNAME TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO F-FIELDNAME.

      *     ACCEPT F-FIELDNAME AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           MOVE F-FIELDNAME TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO CONTROL-020.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE 1510 TO POS.
           DISPLAY 
           "ENTER THE START DATE STOCK WAS RECEIVED: [          ]"
               AT POS.
           MOVE 1552 TO POS.
           DISPLAY DISPLAY-DATE AT POS.
           MOVE DISPLAY-DATE TO H-DATE.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-DATE-ENTER.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO CONTROL-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-035
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-035.
           MOVE " " TO WS-ANSWER4.
           MOVE 1810 TO POS.
           DISPLAY "PRINT VALUE ON COSTS OR SELLING PRICES ?" AT POS.
           MOVE 1911 TO POS.
           DISPLAY "Y=COSTS , N=SELLING PRICES.   [ ]" AT POS.
           MOVE 1942 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER4.

      *     ACCEPT WS-ANSWER4 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-020.
           IF WS-ANSWER4 NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-035.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO CONTROL-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-035.
       CONTROL-040.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE 2510 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
           AT POS.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           IF WS-ANSWER4 = "Y"
               MOVE "AVE COST   " TO H4-1
               MOVE "AVE COST   " TO H4-2
           ELSE
               MOVE "S. PRICE   " TO H4-1
               MOVE "S. PRICE   " TO H4-2.
           MOVE WS-ANSWER1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STARTING STOCKNUMBER TOO HIGH, 'ESC' TO EXIT."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
              GO TO PRR-999.
       PRR-005.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10 OR = 23
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
           IF WS-STORE = "   "
              MOVE ST-CATEGORY TO WS-STORE.
           MOVE ST-LASTRECEIPTDATE TO WS-CALC-DATE.
           IF WS-YYC = 0
            IF WS-MMC = 0
             IF WS-DDC = 0
              IF WS-DATE-ZERO = "N"
               GO TO PRR-005.
           IF WS-YYC > WS-YYE
               GO TO PRR-010.
           IF WS-YYC = WS-YYE
            IF WS-MMC > WS-MME
               GO TO PRR-010.
           IF WS-YYC = WS-YYE
            IF WS-MMC = WS-MME
             IF WS-DDC NOT < WS-DDE
               GO TO PRR-010.
           GO TO PRR-005.
       PRR-010.
           MOVE 2210 TO POS
           DISPLAY "Stock-Number Being Read:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
           COMPUTE WS-QTY = (ST-QTYONHAND + ST-QTYONRESERVE).
           MOVE WS-QTY TO ST-QTYONHAND.
           IF LINE-CNT < 58
               GO TO PRR-020.
       PRR-015.
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
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD2.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD3.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD4.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE 8 TO LINE-CNT.
       PRR-020.
           IF ST-CATEGORY NOT = WS-STORE
               PERFORM PRR-025.
           IF WS-ANSWER3 = "N"
              MOVE ST-STOCKNUMBER   TO D-STOCK
              MOVE ST-DESCRIPTION1  TO D-DESC1
              MOVE ST-DESCRIPTION2  TO D-DESC2
              MOVE ST-QTYONHAND     TO D-ONHAND
              MOVE ST-QTYRECMTD     TO D-REC-MTD
              MOVE ST-QTYRECYTD     TO D-REC-YTD
              MOVE ST-QTYRECLAST    TO D-REC-L-Y.
       PRR-021.
           IF WS-ANSWER4 = "Y"
              MOVE ST-AVERAGECOST   TO D-AVECOST
              COMPUTE WS-AVECOST = (ST-QTYONHAND * ST-AVERAGECOST)
              MOVE WS-AVECOST       TO D-AVECOSTTOT.
           IF WS-ANSWER4 = "N"
              MOVE ST-PRICE         TO D-AVECOST
              COMPUTE WS-AVECOST = (ST-QTYONHAND * ST-PRICE)
              MOVE WS-AVECOST       TO D-AVECOSTTOT.
           MOVE ST-LASTRECEIPTDATE  TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE        TO D-REC-DATE.
           ADD WS-AVECOST           TO WS-AVECOSTTOT
                                       WS-AVETOTAL.
           IF WS-ANSWER3 = "Y"
                GO TO PRR-005.
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           ADD 1 TO LINE-CNT.
           GO TO PRR-005.
       PRR-023.
           COMPUTE WS-AVECOST = (ST-QTYONHAND * ST-AVERAGECOST).
           ADD WS-AVECOST TO WS-AVECOSTTOT
                             WS-AVETOTAL.
           GO TO PRR-005.
       PRR-025.
           MOVE WS-STORE      TO T-CATEGORY.
           MOVE WS-AVECOSTTOT TO T-AVECOST.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           WRITE PRINT-REC.
           ADD 3 TO LINE-CNT.
           MOVE 0 TO WS-AVECOST
                     WS-AVECOSTTOT.
           MOVE ST-CATEGORY TO WS-STORE.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-045.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-045.
       OPEN-150.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
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
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE "TOTALS***" TO T-CATEGORY.
           MOVE WS-AVETOTAL TO T-AVECOST.
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE STOCK-MASTER.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
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
