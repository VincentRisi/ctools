        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrPrAcRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B26.
        OBJECT-COMPUTER. B26.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectDrMaster".
           Copy "SelectStMaster".
           Copy "SelectStDiscAcc".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStDiscAcc.
           COPY ChlfdStock.
           COPY ChlfdDebtor.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-MARGIN            PIC S9(4)V99.
       77  WS-TOTAL-AVE         PIC S9(7)V99.
       77  WS-TOTAL-PRICE       PIC S9(7)V99.
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-ACCOUNT           PIC X(7) VALUE " ".
       77  WS-BEG-STOCK         PIC X(15) VALUE " ".
       77  WS-END-STOCK         PIC X(15) VALUE " ".
       77  WS-DR-DISC           PIC 99V99 VALUE 0.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-STDISC-STATUS.
           03  WS-STDISC-ST1    PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  SPLIT-STOCK.
           03  SP-1ST3          PIC X(3).
           03  SP-REST          PIC X(12).
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(14) VALUE " ".
           03  FILLER         PIC X(65) VALUE
           "STOCK PRICELIST BY ACCOUNT/STOCK NUMBER".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(39) VALUE ALL "*".
           03  FILLER         PIC X(62) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(59) VALUE 
           "STOCK NUMBER    DESCRIPTION".
           03  FILLER         PIC X(63) VALUE
           "  PRICE    DISCOUNT  DISCOUNT".
       01  HEAD3-1.
           03  FILLER         PIC X(59) VALUE " ".
           03  FILLER         PIC X(64) VALUE
           "SELLING      NORMAL   SPECIAL".
       01  HEAD4.
           03  FILLER         PIC X(16) VALUE "ACCOUNT NUMBER:".
           03  H4-ACC         PIC X(8).
           03  H4-NAME        PIC X(45).
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(16) VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(21) VALUE " ".
           03  D-PRICE        PIC Z(5)9.99.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-DISC-NORM    PIC Z9.99.
           03  FILLER         PIC X(5) VALUE " ".
           03  D-DISC-SPEC    PIC Z9.99 BLANK WHEN ZERO.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 310 TO POS
           DISPLAY "** STOCK PRICELIST BY ACCOUNT/STOCK REPORT**"
            AT POS
           MOVE 410 TO POS
           DISPLAY "********************************************"
            AT POS.
       CONTROL-003.
           Copy "PrinterAcceptDr".
       CONTROL-010.
           MOVE 1230 TO POS.
           DISPLAY "ACCOUNT TO PRINT  : [       ]" AT POS.
           MOVE 1251 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCOUNT.

      *     ACCEPT WS-ACCOUNT AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-050.
           MOVE 1530 TO POS.
           DISPLAY "FROM STOCKNUMBER  : [               ]" AT POS.
           MOVE 1551 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-BEG-STOCK.

      *     ACCEPT WS-BEG-STOCK AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-060
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-050.
       CONTROL-060.
           MOVE 1730 TO POS.
           DISPLAY "  TO STOCKNUMBER  : [               ]" AT POS.
           MOVE 1751 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-END-STOCK.

      *     ACCEPT WS-END-STOCK AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-050.
           IF WS-END-STOCK < WS-BEG-STOCK
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-060.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-070
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-060.
       CONTROL-070.
           PERFORM OPEN-FILES
       
           PERFORM READ-DEBTOR.
           MOVE 1339 TO POS
           DISPLAY DR-NAME AT POS.
       
           MOVE Ws-Co-Name TO CO-NAME.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE 2910 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
                AT POS.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE WS-BEG-STOCK TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       PRR-005.
           READ STOCK-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              GO TO PRR-999.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK BUSY ON READ-NEXT, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STOCK-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE ST-STOCKNUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR-020
              MOVE 0 TO WS-STOCK-ST1
              GO TO PRR-005.
           IF ST-STOCKNUMBER < WS-BEG-STOCK
              GO TO PRR-005.
           IF ST-STOCKNUMBER > WS-END-STOCK
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              GO TO PRR-999.

           MOVE 2510 TO POS
           DISPLAY "Stock Number Being Read:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

           IF WS-STORE = " "
              MOVE ST-CATEGORY TO WS-STORE
              GO TO PRR-010.
       PRR-006.
            IF ST-CATEGORY = WS-STORE
               GO TO PRR-010.
            MOVE ST-CATEGORY TO WS-STORE
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC AFTER 1
            ADD 1 TO LINE-CNT.
       PRR-010.
           IF LINE-CNT < 60
               GO TO PRR-020.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE
           MOVE " " TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD3-1 AFTER 2
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE 7 TO LINE-CNT.
       PRR-020.
           PERFORM READ-STDISC.
           
           PERFORM CHECK-DISCOUNT.
           
           MOVE ST-STOCKNUMBER    TO D-STOCK
           MOVE ST-DESCRIPTION1   TO D-DESC1
           MOVE ST-DESCRIPTION2   TO D-DESC2
           MOVE ST-PRICE          TO D-PRICE
           MOVE WS-DR-DISC        TO D-DISC-NORM
           MOVE STDISC-PERCENT    TO D-DISC-SPEC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           
           ADD 1 TO LINE-CNT
           MOVE " " TO PRINT-REC DETAIL-LINE.
           GO TO PRR-005.
       PRR-999.
           EXIT.
      *
       READ-DEBTOR SECTION.
       RD-007.
            MOVE WS-ACCOUNT TO DR-ACCOUNT-NUMBER.
            START DEBTOR-MASTER KEY NOT < DR-KEY.
            READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "UNKNOWN DEBTOR" TO DR-NAME
               MOVE 0                TO DR-DISCOUNT-CODE
               GO TO RD-999.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-007.
           MOVE DR-ACCOUNT-NUMBER TO H4-ACC
           MOVE DR-NAME           TO H4-NAME.
       RD-999.
           EXIT.
      *
       CHECK-DISCOUNT SECTION.
       CDS-005.
           IF DR-DISCOUNT-CODE = " " OR = "0"
              MOVE 0            TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "1"
              MOVE ST-DISCOUNT1 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "2"
              MOVE ST-DISCOUNT2 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "3"
              MOVE ST-DISCOUNT3 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "4"
              MOVE ST-DISCOUNT4 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "5"
              MOVE ST-DISCOUNT5 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "6"
              MOVE ST-DISCOUNT6 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "7"
              MOVE ST-DISCOUNT7 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "8"
              MOVE ST-DISCOUNT8 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "9"
              MOVE ST-DISCOUNT9 TO WS-DR-DISC.
       CDS-999.
           EXIT.
      *
       READ-STDISC SECTION.
       RS-001.
           MOVE ST-STOCKNUMBER TO STDISC-STOCKNUMBER.
           MOVE WS-ACCOUNT     TO STDISC-ACCOUNT
           START STDISC-MASTER KEY NOT < STDISC-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STDISC-ST1 NOT = 0
               MOVE 0 TO STDISC-PERCENT
               GO TO RS-999.
       RS-005.
           READ STDISC-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STDISC-ST1 = 23 OR 35 OR 49
               MOVE 0 TO STDISC-PERCENT
               GO TO RS-999.
           IF WS-STDISC-ST1 NOT = 0
               MOVE "STOCK-DISC BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STDISC-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STDISC-ST1
               GO TO RS-005.
       RS-999.
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
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE STDISC-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
           EXIT.
      *   
       END-OFF SECTION.
       END-200.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           
           CLOSE STOCK-MASTER
                 STDISC-MASTER
                 DEBTOR-MASTER.
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
