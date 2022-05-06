        IDENTIFICATION DIVISION.
        PROGRAM-ID. StAlteRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
         Copy "SelectStAlternative".
         Copy "SelectStCatalogue".
         Copy "SelectStSpecPr".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStAlternative.
           COPY ChlfdStCatalogue.
           COPY ChlfdStPrice.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-STOCKNUMBER       PIC X(15) VALUE " ".
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-CAT               PIC X(3) VALUE " ".
       77  WS-TOTANSWER         PIC X VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-STALT-STATUS.
           03  WS-STALT-ST1     PIC 99.
       01  WS-STPR-STATUS.
           03  WS-STPR-ST1      PIC 99.
       01  WS-STCAT-STATUS.
           03  WS-STCAT-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(55) VALUE
           "S T O C K   A L T E R N A T I V E S   R E P O R T".
           03  FILLER         PIC X(24) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(49) VALUE ALL "*".
           03  FILLER         PIC X(7) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(17) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(55) VALUE "DESCRIPTION".
           03  FILLER         PIC X(9) VALUE "CAT PAGE".
           03  FILLER         PIC X(14) VALUE "SPECIAL".
           03  FILLER         PIC X(8) VALUE "PRICE".
           03  FILLER         PIC X(10) VALUE "ONHAND".
       01  HEAD4-1.
           03  FILLER         PIC X(17) VALUE "ALTERNATIVE".
           03  FILLER         PIC X(100) VALUE " ".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(17).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(37).
           03  D-CAT          PIC X(5) VALUE " ".
           03  D-SPECIAL      PIC Z(5)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-PRICE        PIC Z(5)9.99.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-QTY          PIC Z(5)9.
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
           DISPLAY "** STOCK ALTERNATIVES REPORT" AT POS
           MOVE 421 TO POS
           DISPLAY "****************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA
           PERFORM OPEN-FILES
           PERFORM PRINT-ROUTINE
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO WS-RANGE1 WS-RANGE2
            MOVE 1010 TO POS
            DISPLAY "            FROM STOCK NUMBER: [               ]"
                      AT POS
            MOVE 1042 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

      *      ACCEPT WS-RANGE1 AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
            MOVE 1210 TO POS
            DISPLAY "              TO STOCK NUMBER: [               ]"
                      AT POS
            MOVE 1242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

      *      ACCEPT WS-RANGE2 AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO GET-000.
            IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
            MOVE 3010 TO POS
            DISPLAY "The Report Is Being Compiled, Please Be Patient."
               AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE WS-RANGE1 TO ST-STOCKNUMBER
            START STOCK-MASTER KEY NOT < ST-STOCKNUMBER.
       PRR-002.
           READ STOCK-MASTER NEXT
               AT END
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
               GO TO PRR-002.
               
           MOVE ST-STOCKNUMBER     TO WS-STOCKNUMBER.
           IF WS-STOCKNUMBER < WS-RANGE1
               GO TO PRR-002.
           IF WS-STOCKNUMBER > WS-RANGE2
               GO TO PRR-999.

           MOVE 2610 TO POS
           DISPLAY "STOCKNUMBER BEING READ:" AT POS
           ADD 24 TO POS
           DISPLAY WS-STOCKNUMBER AT POS.
       PRR-010.
           IF LINE-CNT < 56
               GO TO PRR-011.
            ADD 1            TO PAGE-CNT
            MOVE PAGE-CNT    TO H1-PAGE
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
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC AFTER 2
            MOVE 5 TO LINE-CNT.
       PRR-011.
           WRITE PRINT-REC FROM HEAD4 AFTER 2
           MOVE " " TO PRINT-REC.

           MOVE ST-STOCKNUMBER     TO WS-STOCKNUMBER.
           MOVE ST-STOCKNUMBER     TO D-STOCK
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE ST-PRICE           TO D-PRICE
           MOVE ST-QTYONHAND       TO D-QTY.

           MOVE 1 TO SUB-1.
           MOVE ST-STOCKNUMBER TO STALT-NUMBER (SUB-1)
           PERFORM READ-CATALOGUE-REF.
           MOVE STCAT-PAGE-NUM     TO D-CAT.
           PERFORM READ-SPECIAL-PRICES.
           MOVE STPR-PRICE         TO D-SPECIAL.
           
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT.
       PRR-015.
           PERFORM READ-ALTERNATIVE.
           MOVE 0 TO SUB-1.
           IF WS-STALT-ST1 = 23 OR 35 OR 49
               GO TO PRR-002.
           
           WRITE PRINT-REC FROM HEAD4-1 AFTER 1
           MOVE " " TO PRINT-REC.
       PRR-020.
           IF SUB-1 < 10
              ADD 1 TO SUB-1.
           IF STALT-NUMBER (SUB-1) = " "
               GO TO PRR-050.
               
           PERFORM READ-ALT-STOCK
           MOVE ST-STOCKNUMBER     TO D-STOCK
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE ST-PRICE           TO D-PRICE
           MOVE ST-QTYONHAND       TO D-QTY.

           PERFORM READ-CATALOGUE-REF.
           MOVE STCAT-PAGE-NUM     TO D-CAT.
           
           PERFORM READ-SPECIAL-PRICES.
           MOVE STPR-PRICE         TO D-SPECIAL.
       PRR-040.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT.
           
           GO TO PRR-020.
       PRR-050.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER
           START STOCK-MASTER KEY > ST-STOCKNUMBER.
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       READ-ALT-STOCK SECTION.
       RAS-001.
           MOVE STALT-NUMBER (SUB-1) TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
               MOVE " " TO ST-STOCKNUMBER
               GO TO RAS-999.
       RAS-005.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE " " TO ST-STOCKNUMBER
               GO TO RAS-999.
           IF WS-STOCK-ST1 NOT = 0
              Move "STOCK FILE BUSY ON READ, 'ESC' to RETRY"
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RAS-005.
       RAS-999.
           EXIT.
      *
       READ-SPECIAL-PRICES SECTION.
       SPR-000.
           MOVE STALT-NUMBER (SUB-1) TO STPR-STOCKNUMBER.
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
           MOVE STALT-NUMBER (SUB-1) TO STCAT-STOCKNUMBER.
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
              MOVE "ST-CATALOGUE PAGE BUSY ON READ, 'ESC' TO RETRY"
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
       READ-ALTERNATIVE SECTION.
       R-CAT-000.
             MOVE WS-STOCKNUMBER TO STALT-STOCKNUMBER.
             START STALT-MASTER KEY NOT < STALT-KEY
                INVALID KEY NEXT SENTENCE.
       R-CAT-010.
             READ STALT-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STALT-ST1 = 23 OR 35 OR 49
                GO TO R-CAT-999.
             IF WS-STALT-ST1 NOT = 0
                MOVE "ST-ALT RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STALT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STALT-ST1
               GO TO R-CAT-010.
       R-CAT-999.
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
       OPEN-002.
            OPEN I-O STALT-MASTER.
            IF WS-STALT-ST1 NOT = 0
               MOVE 0 TO WS-STALT-ST1
               MOVE "ST-ALTERNATIVE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
       OPEN-005.
            OPEN I-O STPR-MASTER.
            IF WS-STPR-ST1 NOT = 0
               MOVE 0 TO WS-STPR-ST1
               MOVE "STOCK PRICE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-006.
            OPEN I-O STCAT-MASTER.
            IF WS-STCAT-ST1 NOT = 0
               MOVE 0 TO WS-STCAT-ST1
               MOVE "ST-CATALOGUE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.
        OPEN-010.
           MOVE Ws-Co-Name TO CO-NAME
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 56
              PERFORM PRR-010.
           MOVE " " TO PRINT-REC.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-040.
           CLOSE STOCK-MASTER
                 STPR-MASTER
                 STCAT-MASTER
                 STALT-MASTER.
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
