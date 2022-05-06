        IDENTIFICATION DIVISION.
        PROGRAM-ID. StBoLeRp.
        AUTHOR.  CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectSlRegister".
         Copy "SelectStTrans".
         Copy "SelectStOrders".
         Copy "SelectSlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdRegister.
           COPY ChlfdStTrans.
           COPY ChlfdOutOrd.
           COPY ChlfdParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-NEW-ACC           PIC X VALUE " ".
       77  WS-ACC-PRINTED       PIC X VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(7) VALUE " ".
       77  WS-RANGE2            PIC X(7) VALUE " ".
       77  WS-STORE             PIC 9(7) VALUE 0.
       77  WS-SALESMAN          PIC X VALUE " ".
       77  WS-QUANTITY          PIC 9(5) VALUE 0.
       77  WS-VALUE             PIC 9(6)V99 VALUE 0.
       77  WS-TOTAL-VALUE       PIC 9(7)V99 VALUE 0.
       77  WS-COMMENT           PIC X(75) VALUE " ".
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1          PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1          PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1      PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1        PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR     PIC X.
           03  SP-REST        PIC X(14).
       01  WS-DUE-DATE.
           03  WS-DUE-YY      PIC 9999.
           03  WS-DUE-MM      PIC 99.
           03  WS-DUE-DD      PIC 99.
       01  NAME1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(20) VALUE " ".
           03  P-DIG1         PIC X.
           03  H1-NAME        PIC X(40).
           03  P-DIG2         PIC X.
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC 9.
           03  FILLER         PIC X(4) VALUE " ".
       01  NAME2.
           03  FILLER         PIC X(40) VALUE " ".
           03  CO-ADD1        PIC X(30).
           03  CO-DEL1        PIC X(62).
       01  NAME5.
           03  FILLER         PIC X(40) VALUE " ".
           03  FILLER         PIC X(7) VALUE "PHONE: ".
           03  N-PHONE        PIC X(23).
           03  FILLER         PIC X(5) VALUE "FAX: ".
           03  N-FAX          PIC X(42).
       01  ACC1.
           03  FILLER         PIC X(7) VALUE "NAME :".
           03  H3-NAME        PIC X(40).
           03  H3-DESC1       PIC X(58) VALUE " ".
           03  FILLER         PIC X(16) VALUE "ACCOUNT NUMBER:".
           03  H3-ACCOUNT     PIC X(11).
       01  ACC.
           03  FILLER         PIC X(7) VALUE " ".
           03  H3-ADD         PIC X(30).
           03  H3-DESC        PIC X(95) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(61) VALUE "STOCK".
           03  FILLER         PIC X(31) VALUE "P/ORDER".
           03  FILLER         PIC X(17) VALUE "INTERNAL ORDER".
           03  FILLER         PIC X(22) VALUE "B/O UNIT   APPROX".
       01  HEAD5.
           03  FILLER         PIC X(18) VALUE "NUMBER".
           03  FILLER         PIC X(43) VALUE "DESCRIPTION".
           03  FILLER         PIC X(23) VALUE "NUMBER".
           03  FILLER         PIC X(16) VALUE "QTY.     No:".
           03  FILLER         PIC X(20) VALUE " DATE       PRICE".
           03  FILLER         PIC X(12) VALUE " DUE DATE".
       01  DETAIL-LINE.
           03  D-STOCKNO      PIC X(18).
           03  D-DESC         PIC X(20).
           03  D-DESC2        PIC X(23).
           03  D-PONO         PIC X(21).
           03  D-QTY          PIC Z(4)9.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-INVNO        PIC Z(6)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-DATE         PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-VALUE        PIC Z(5)9.99.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-APPROXDATE   PIC X(10).
           03  FILLER         PIC X(2) VALUE " ".
       01  CONTINUED-LINE.
           03  FILLER         PIC X(50) VALUE " ".
           03  FILLER         PIC X(18) VALUE "Continued to page".
           03  CONTROL-PAGE   PIC Z9 VALUE " ".
           03  FILLER         PIC X(62) VALUE " ".
       01  COMMENT-LINE.
           03  FILLER         PIC X(20) VALUE " ".
           03  COMMENT        PIC X(112) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(80) VALUE " ".
           03  FILLER         PIC X(26) VALUE
           "TOTAL VALUE OF B/ORDERS: R".
           03  TOT-VALUE      PIC Z(6)9.99.
           03  FILLER         PIC X(16) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 315 TO POS.
           DISPLAY "** BACK-ORDER LETTER BY ACCOUNT NUMBER **" AT POS.
           MOVE 415 TO POS.
           DISPLAY "*****************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM OPEN-FILES.
           PERFORM GET-DATA.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO WS-RANGE1.
            MOVE 1033 TO POS.
            DISPLAY "FROM ACCOUNT NUMBER: [       ]" AT POS.
            MOVE 1055 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
            MOVE " " TO WS-RANGE2.
            MOVE 1233 TO POS.
            DISPLAY "  TO ACCOUNT NUMBER: [       ]" AT POS.
            MOVE 1255 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

            IF W-ESCAPE-KEY = 4
               GO TO GET-000.
            IF WS-RANGE2 < WS-RANGE1
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-020
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-020.
            MOVE 1510 TO POS.
            DISPLAY "ENTER A COMMENT TO BE PRINTED ON ALL LETTERS."
                AT POS.
            MOVE 1701 TO POS.
            DISPLAY "[" AT POS.
            MOVE 1777 TO POS.
            DISPLAY "]" AT POS.
            MOVE 1702 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 76        TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 1         TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMMENT.

            IF W-ESCAPE-KEY = 4
               GO TO GET-010.
            IF WS-COMMENT = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-030.
            MOVE 1910 TO POS.
            DISPLAY "ENTER A SALESMAN No TO BE PRINTED," AT POS.
            MOVE 2012 TO POS.
            DISPLAY "LEAVE BLANK TO PRINT ALL ACCOUNTS." AT POS.
            MOVE 2054 TO POS.
            DISPLAY "[ ]" AT POS.
            MOVE 2055 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SALESMAN.

            IF W-ESCAPE-KEY = 4
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-040
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-040.
            MOVE 2510 TO POS.
            DISPLAY "The report is being compiled, please be patient."
                AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE "Y"       TO WS-NEW-ACC
           MOVE "N"       TO WS-ACC-PRINTED
                             STTR-AC-COMPLETE
           MOVE WS-RANGE1 TO STTR-ACCOUNT-NUMBER
           MOVE 0         TO STTR-AC-DATE.
           START STOCK-TRANS-FILE KEY NOT < STTR-AC-KEY
                INVALID KEY NEXT SENTENCE.
       PRR-002.
           READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PRR-999.
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
               GO TO PRR-002.
           IF STTR-AC-COMPLETE NOT = "N"
               GO TO PRR-999.
           IF STTR-ACCOUNT-NUMBER < WS-RANGE1
               GO TO PRR-002.
           IF STTR-ACCOUNT-NUMBER > WS-RANGE2
               GO TO PRR-999.

           IF STTR-TYPE NOT = 4
               GO TO PRR-002.
           MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               GO TO PRR-002.
           COMPUTE WS-QUANTITY = STTR-ORDERQTY -
                    (STTR-SHIPQTY + STTR-SHIPPEDQTY).
           IF WS-QUANTITY = 0
               GO TO PRR-002.
           IF STTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
              GO TO PRR-006.
       PRR-005.
           MOVE STTR-ACCOUNT-NUMBER TO DR-KEY.
           READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "UNKNOWN DEBTOR" TO DR-NAME
               GO TO PRR-010.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PRR-005.
       PRR-006.
           IF WS-SALESMAN = " "
              GO TO PRR-010.
           IF DR-SALESMAN = WS-SALESMAN
              GO TO PRR-010
           ELSE
              GO TO PRR-002.
       PRR-010.
           IF WS-STORE NOT = 0
             IF STTR-ACCOUNT-NUMBER NOT = WS-STORE
               MOVE WS-TOTAL-VALUE TO TOT-VALUE
               WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
               MOVE " " TO PRINT-REC
               MOVE 0 TO WS-TOTAL-VALUE
               MOVE WS-COMMENT TO COMMENT
               WRITE PRINT-REC FROM COMMENT-LINE AFTER 3
               MOVE " " TO PRINT-REC
               MOVE 66 TO LINE-CNT
               MOVE "Y" TO WS-NEW-ACC
               MOVE 2310 TO POS
               DISPLAY "Account Number Being Processed:" AT POS
               MOVE 2342 TO POS
               DISPLAY WS-STORE AT POS.
       PRR-015.
           IF LINE-CNT < 61
               GO TO PRR-020.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF WS-NEW-ACC = "Y"
               MOVE 1 TO PAGE-CNT
           ELSE
               ADD 1 TO PAGE-CNT
               MOVE PAGE-CNT TO CONTROL-PAGE
               WRITE PRINT-REC FROM CONTINUED-LINE AFTER 3
               MOVE " " TO PRINT-REC.
           MOVE PAGE-CNT        TO H1-PAGE
           MOVE WS-PRINT-BOLD   TO P-DIG1
           MOVE WS-PRINT-UNBOLD TO P-DIG2
           MOVE PA-NAME         TO H1-NAME.
           IF LINE-CNT = 66
               WRITE PRINT-REC FROM NAME1
           ELSE
               WRITE PRINT-REC FROM NAME1 AFTER PAGE.
           MOVE " " TO PRINT-REC
           MOVE PA-ADD1 TO CO-ADD1
           MOVE PA-DEL1 TO CO-DEL1
           WRITE PRINT-REC FROM NAME2 AFTER 2
           MOVE " " TO PRINT-REC
           MOVE PA-ADD2 TO CO-ADD1
           MOVE PA-DEL2 TO CO-DEL1
           WRITE PRINT-REC FROM NAME2 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE PA-ADD3 TO CO-ADD1
           MOVE " "     TO CO-DEL1
           WRITE PRINT-REC FROM NAME2 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE PA-PHONE TO N-PHONE
           MOVE PA-FAX   TO N-FAX
           WRITE PRINT-REC FROM NAME5 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE 7 TO LINE-CNT.

           MOVE STTR-ACCOUNT-NUMBER TO WS-STORE
           MOVE WS-STORE TO H3-ACCOUNT
           MOVE DR-NAME  TO H3-NAME
           MOVE "           *** BACK-ORDER STATUS REPORT ***"
               TO H3-DESC1
           WRITE PRINT-REC FROM ACC1 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE DR-ADDRESS1 TO H3-ADD
           WRITE PRINT-REC FROM ACC AFTER 1
           MOVE " " TO PRINT-REC ACC
           MOVE DR-ADDRESS2 TO H3-ADD
           MOVE
           "WE CONFIRM THAT THE FOLLOWING ITEMS ARE ON BACK-ORDER"
               TO H3-DESC
           WRITE PRINT-REC FROM ACC AFTER 1
           MOVE " " TO PRINT-REC ACC.
           IF DR-ADDRESS3 = " "
               MOVE DR-POST-CODE TO H3-ADD
               MOVE
           "AND WILL BE SHIPPED TO YOU ON RECEIPT OF OUR NEW STOCK."
               TO H3-DESC
               WRITE PRINT-REC FROM ACC AFTER 1
               MOVE " " TO PRINT-REC ACC
               ADD 1 TO LINE-CNT
          ELSE
               MOVE DR-ADDRESS3  TO H3-ADD
               MOVE
           "AND WILL BE SHIPPED TO YOU ON RECEIPT OF OUR NEW STOCK."
               TO H3-DESC
               WRITE PRINT-REC FROM ACC AFTER 1
               MOVE " "          TO PRINT-REC ACC
               MOVE DR-POST-CODE TO H3-ADD
               WRITE PRINT-REC FROM ACC AFTER 1
               MOVE " "          TO PRINT-REC
               ADD 2 TO LINE-CNT.
           WRITE PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD5 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           ADD 8 TO LINE-CNT
           MOVE "N" TO WS-NEW-ACC.
           MOVE "Y" TO WS-ACC-PRINTED.
       PRR-020.
           IF STTR-REFERENCE1 NOT = INCR-INVOICE
               PERFORM READ-ORDER-REGISTER.
           MOVE STTR-STOCK-NUMBER TO D-STOCKNO
           MOVE STTR-DESC1        TO D-DESC
           MOVE STTR-DESC2        TO D-DESC2
           MOVE INCR-PORDER       TO D-PONO
           MOVE WS-QUANTITY       TO D-QTY
           MOVE STTR-REFERENCE1   TO D-INVNO
           MOVE STTR-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-DATE
           MOVE STTR-PRICE        TO D-VALUE
           COMPUTE WS-TOTAL-VALUE = (WS-TOTAL-VALUE +
                (WS-QUANTITY * STTR-PRICE)).
           MOVE 0 TO WS-DUE-DATE.
           IF SP-1STCHAR = "/"
              GO TO PRR-030.
           PERFORM READ-SORDERS.
           IF D-APPROXDATE = "< 2 MNTH"
              GO TO PRR-030.
           MOVE WS-DUE-DATE        TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE      TO D-APPROXDATE.
       PRR-030.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           ADD 1 TO LINE-CNT.
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       READ-ORDER-REGISTER SECTION.
       ROR-010.
           MOVE STTR-REFERENCE1 TO INCR-INVOICE.
           MOVE STTR-TYPE       TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
                INVALID KEY NEXT SENTENCE.
       ROR-020.
           READ INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-INCR-ST1
              MOVE "*ORDER NOT FOUND*" TO INCR-PORDER
              GO TO ROR-999.
           IF WS-INCR-ST1 NOT = 0
              MOVE
              "REGISTER LOCKED AT ANOTHER WORKSTATION, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-INCR-ST1
              GO TO ROR-020.
       ROR-999.
           EXIT.
      *
       READ-SORDERS SECTION.
       RS-010.
           MOVE 0                 TO WS-DUE-DATE.
           MOVE STTR-STOCK-NUMBER TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-STOCK-NUMBER
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
               GO TO RS-050.
       RS-020.
           READ OUTSTANDING-ORDERS NEXT
              AT END NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 10
              GO TO RS-050.
           IF WS-OUTORD-ST1 NOT = 0
              MOVE "ST-ORDERS BUSY ON READ, 'ESC' TO RE-TRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-OUTORD-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-OUTORD-ST1
              GO TO RS-020.
           IF OO-STOCK-NUMBER < STTR-STOCK-NUMBER
              GO TO RS-020.
           IF OO-STOCK-NUMBER > STTR-STOCK-NUMBER
              GO TO RS-050.
       RS-030.
           IF OO-QUANTITY NOT > 0
              GO TO RS-020.
           MOVE OO-DUEDATE TO WS-DUE-DATE.
           IF WS-DUE-DATE > WS-DATE
              GO TO RS-999
           ELSE
              GO TO RS-060.
       RS-050.
           MOVE "< 2 MNTH" TO D-APPROXDATE
           GO TO RS-999.
       RS-060.
           MOVE WS-DATE TO WS-DUE-DATE.
           ADD 15 TO WS-DUE-DD.
           ADD 1  TO WS-DUE-MM.
           IF WS-DUE-DD > 30
             ADD 1 TO WS-DUE-MM
             SUBTRACT 30 FROM WS-DUE-DD.
           IF WS-DUE-MM > 12
             ADD 1 TO WS-DUE-YY
             MOVE 1 TO WS-DUE-MM.
       RS-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           START PARAMETER-FILE KEY NOT < PA-KEY.
       RP-010.
           READ PARAMETER-FILE
               INVALID KEY
               NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO SLPARAMETER RECORD, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "SLPARAMETER BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RP-010.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0 
              MOVE 0 TO WS-DEBTOR-ST1
              MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-010.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-010.
       OPEN-014.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
           PERFORM READ-PARAMETER.
           CLOSE PARAMETER-FILE.
       OPEN-015.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0 
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-015.
       OPEN-020.
           OPEN I-O OUTSTANDING-ORDERS.
           IF WS-OUTORD-ST1 NOT = 0 
              MOVE 0 TO WS-OUTORD-ST1
              MOVE "S-ORDERS FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-015.
       OPEN-500.
           Move Ws-CO-NAME to H1-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-010.
            IF WS-ACC-PRINTED = "N"
                GO TO END-850.
            IF LINE-CNT > 60
                PERFORM PRR-015.
            MOVE WS-TOTAL-VALUE TO TOT-VALUE.
            WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.
            MOVE " " TO PRINT-REC.
            MOVE WS-COMMENT TO COMMENT.
            WRITE PRINT-REC FROM COMMENT-LINE AFTER 3.
            MOVE " " TO PRINT-REC.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-850.
            CLOSE DEBTOR-MASTER
                  STOCK-TRANS-FILE
                  OUTSTANDING-ORDERS
                  PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
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
