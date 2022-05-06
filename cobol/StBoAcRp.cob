        IDENTIFICATION DIVISION.
        PROGRAM-ID. StBoAcRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectStTrans".
         Copy "SelectSlRegister".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdRegister.
           COPY ChlfdStTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-REP-ONLY          PIC X VALUE " ".
       77  WS-SKIP-REC          PIC X VALUE " ".
       77  WS-INITIAL           PIC X(2) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC 9(7) VALUE 0.
       77  WS-RANGE2            PIC 9(7) VALUE 0.
       77  WS-RANGE3            PIC X VALUE " ".
       77  WS-RANGE4            PIC X VALUE " ".
       77  WS-STORE             PIC 9(7) VALUE 0.
       77  WS-VALUE             PIC 9(6)V99 VALUE 0.
       77  WS-TOTAL-VALUE       PIC 9(7)V99 VALUE 0.
       77  WS-RUNNING-TOTAL     PIC 9(7)V99 VALUE 0.
       77  WS-QUANTITY          PIC 9(5) VALUE 0.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1        PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1        PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(15) VALUE "B A C K - O R D".
           03  FILLER         PIC X(9) VALUE " E R   R ".
           03  FILLER         PIC X(7) VALUE "E P O R".
           03  FILLER         PIC X(5) VALUE " T   ".
           03  FILLER         PIC X(25) VALUE "B Y   A C C O U N T".
           03  H1-TYPE        PIC X(21) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(12) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(55) VALUE ALL "*".
           03  FILLER         PIC X(47) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(10) VALUE "ACCOUNT :".
           03  H3-ACCOUNT     PIC X(11).
           03  FILLER         PIC X(7) VALUE "NAME :".
           03  H3-NAME        PIC X(104).
       01  HEAD4.
           03  FILLER         PIC X(62) VALUE "STOCK".
           03  FILLER         PIC X(31) VALUE "INTERNAL ORDER".
           03  FILLER         PIC X(39) VALUE " ".
       01  HEAD5.
           03  FILLER         PIC X(18) VALUE "NUMBER".
           03  FILLER         PIC X(43) VALUE "DESCRIPTION".
           03  FILLER         PIC X(24) VALUE "   No:    DATE".
           03  FILLER         PIC X(22) VALUE "ORDER READY SHIPD".
           03  FILLER         PIC X(25) VALUE "PRICE    VALUE  TRANS".
       01  DETAIL-LINE.
           03  D-STOCKNO      PIC X(18).
           03  D-DESC         PIC X(20).
           03  D-DESC2        PIC X(23).
           03  D-PONO         PIC Z(5)9.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-DATE         PIC X(10).
           03  FILLER         PIC X(6) VALUE " ".
           03  D-ORDERQTY     PIC Z(4)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SHIPQTY      PIC Z(4)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SHIPPEDQTY   PIC Z(4)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PRICE        PIC Z(5)9.99.
           03  D-VALUE        PIC Z(5)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-TRANS        PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(95) VALUE " ".
           03  FILLER         PIC X(7) VALUE "TOTAL:".
           03  TOT-VALUE      PIC Z(6)9.99.
           03  FILLER         PIC X(20) VALUE " ".
       01  RUNNING-LINE.
           03  FILLER         PIC X(87) VALUE " ".
           03  FILLER         PIC X(15) VALUE "RUNNING TOTAL:".
           03  RUN-VALUE      PIC Z(6)9.99.
           03  FILLER         PIC X(20) VALUE " ".
       01  INITIAL-LINE.
           03  FILLER         PIC X(10) VALUE " ".
           03  INIT-DESC1     PIC X(20) VALUE "B/O'S PRINTED FOR:".
           03  INIT-NAME      PIC XX.
           03  FILLER         PIC X(20) VALUE " ".
       01  SMAN-LINE.
           03  FILLER          PIC X(10) VALUE " ".
           03  SMAN-DESC1.
               05  SMAN-DESC2     PIC X(24) VALUE
                  "B/O'S PRINTED FOR REP #:".
               05  SMAN-NAME      PIC X.
               05  FILLER         PIC X(20) VALUE " ".
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
           DISPLAY "** BACK-ORDER REPORT BY ACCOUNT NUMBER **" AT POS
           MOVE 415 TO POS
           DISPLAY "*****************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 0 TO WS-RANGE1.
            MOVE 833 TO POS.
            DISPLAY "FROM ACCOUNT NUMBER  : [       ]" AT POS.
            MOVE 857 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
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
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
            MOVE 0 TO WS-RANGE2.
            MOVE 1033 TO POS.
            DISPLAY "  TO ACCOUNT NUMBER  : [       ]" AT POS.
            MOVE 1057 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
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
               GO TO GET-015
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
           MOVE 1210 TO POS.
           DISPLAY "ENTER A REPS INITIALS, BLANK FOR ALL.       : [  ]"
           AT POS.
           ADD 47 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-INITIAL.

           IF W-ESCAPE-KEY = 4
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO GET-018
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-018.
           MOVE 1310 TO POS
           DISPLAY "ENTER A REP # FROM DEBTOR FILE, BLANK FOR ALL,"
           AT POS
           MOVE 1410 TO POS
           DISPLAY "OR ENTER 'Z' FOR ACCOUNTS WITH NO REP#      : [ ]"
           AT POS
           ADD 47 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-REP-ONLY.

           IF W-ESCAPE-KEY = 4
               GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO GET-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-018.
       GET-020.
           MOVE " " TO WS-RANGE3
           MOVE 1533 TO POS
           DISPLAY "TYPE OF B-ORDERS     : [ ]" AT POS
           MOVE 1622 TO POS
           DISPLAY
           "ENTER A=ALL ITEMS (B/O'S AND PENDING), B=B/ORDERS ONLY."
           AT POS
           MOVE 1557 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE3.

            IF W-ESCAPE-KEY = 4
               GO TO GET-015.
            IF WS-RANGE3 NOT = "A" AND NOT = "B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-030.
            MOVE " " TO WS-RANGE4.
            MOVE 1825 TO POS.
            DISPLAY "  PRINT ACC'S ON A NEW PAGE  : [ ]" AT POS.
            MOVE 1857 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE4.

            IF W-ESCAPE-KEY = 4
               GO TO GET-020.
            IF WS-RANGE4 NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-050
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-050.
            MOVE 2515 TO POS
            DISPLAY "The report is being compiled ......." AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE "N"       TO WS-SKIP-REC.
            MOVE "N"       TO STTR-AC-COMPLETE
            MOVE 0         TO STTR-AC-DATE
            MOVE WS-RANGE1 TO STTR-ACCOUNT-NUMBER.
            START STOCK-TRANS-FILE KEY NOT < STTR-AC-KEY
                  INVALID KEY NEXT SENTENCE.
       PRR-002.
            READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-STTRANS-ST1 = 10
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PRR-999.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE "STTRANS BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE "STTRANS ACC KEY SHOWN NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE STTR-AC-KEY TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE "STTRANS MAIN KEY SHOWN NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE STTR-KEY TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PRR-002.

            IF STTR-AC-COMPLETE NOT = "N"
               GO TO PRR-999.
            IF STTR-ACCOUNT-NUMBER < WS-RANGE1
               GO TO PRR-002.
            IF STTR-ACCOUNT-NUMBER > WS-RANGE2
               GO TO PRR-999.
            IF STTR-TYPE NOT = 4 AND NOT = 7
               GO TO PRR-002.
            MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
               GO TO PRR-002.
            IF WS-RANGE3 = "A" OR = "B"
             IF STTR-AC-COMPLETE NOT = "N"
                GO TO PRR-002.

               MOVE 2310 TO POS
               DISPLAY "Account Number Being Read :     " AT POS
               MOVE 2342 TO POS
               DISPLAY STTR-ACCOUNT-NUMBER AT POS.
                
           PERFORM READ-REGISTER.
              
           IF WS-INITIAL = "  "
              GO TO PRR-003.
           IF WS-INITIAL NOT = "  "
            IF INCR-SB-TYPE = WS-INITIAL
              GO TO PRR-003.
              
           MOVE STTR-ACCOUNT-NUMBER TO WS-STORE
           MOVE "Y" TO WS-SKIP-REC
           GO TO PRR-002.
       PRR-003.
      ****************************************
      * A= ALL PENDING B/O'S  B= B/O'S ONLY  *
      ****************************************
            COMPUTE WS-QUANTITY = 
                  STTR-ORDERQTY - (STTR-SHIPPEDQTY + STTR-SHIPQTY).
            IF WS-RANGE3 = "B"
             IF WS-QUANTITY = 0
               GO TO PRR-002.
            IF WS-RANGE3 = "A"
             IF STTR-SHIPQTY = 0
              IF WS-QUANTITY = 0
               GO TO PRR-002.
               
            IF WS-STORE NOT = 0
             IF STTR-ACCOUNT-NUMBER NOT = WS-STORE
              IF WS-SKIP-REC = "Y"
      *         MOVE STTR-ACCOUNT-NUMBER TO WS-STORE
               MOVE "N" TO WS-SKIP-REC
              ELSE
               MOVE WS-TOTAL-VALUE TO TOT-VALUE
               WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
               MOVE " "              TO PRINT-REC
               MOVE 0                TO WS-TOTAL-VALUE
               MOVE WS-RUNNING-TOTAL TO RUN-VALUE
               WRITE PRINT-REC FROM RUNNING-LINE AFTER 1
               MOVE " " TO PRINT-REC
               ADD 3 TO LINE-CNT
               MOVE 2310 TO POS
               DISPLAY "Account Number Being Processed:" AT POS
               MOVE 2342 TO POS
               DISPLAY WS-STORE AT POS
             IF WS-RANGE4 = "Y"
               MOVE 66 TO LINE-CNT.
       PRR-005.
            IF STTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
               GO TO PRR-008.
            MOVE STTR-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
            START DEBTOR-MASTER KEY NOT < DR-KEY.
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
       PRR-008.
           IF WS-REP-ONLY = "Z"
            IF DR-SALESMAN NOT = " "
               MOVE "Y" TO WS-SKIP-REC
               GO TO PRR-002.
           IF WS-REP-ONLY NOT = " " AND NOT = "Z"
            IF WS-REP-ONLY NOT = DR-SALESMAN
               MOVE "Y" TO WS-SKIP-REC
               GO TO PRR-002.
       PRR-010.
           IF LINE-CNT < 60
               GO TO PRR-015.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE " " TO PRINT-REC.
           IF WS-RANGE3 = "A" 
                MOVE "PENDING B/O" TO H1-TYPE
           ELSE
                MOVE "ONLY B/O'S" TO H1-TYPE.
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
           MOVE 6 TO LINE-CNT.
        PRR-015.
           IF STTR-ACCOUNT-NUMBER NOT = WS-STORE
               MOVE STTR-ACCOUNT-NUMBER TO WS-STORE
               MOVE WS-STORE            TO H3-ACCOUNT
               MOVE DR-NAME             TO H3-NAME
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD3 AFTER 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD4 AFTER 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD5 AFTER 1
               MOVE " " TO PRINT-REC
               ADD 4 TO LINE-CNT.
       PRR-020.
           MOVE STTR-STOCK-NUMBER TO D-STOCKNO
           MOVE STTR-DESC1        TO D-DESC
           MOVE STTR-DESC2        TO D-DESC2
           MOVE STTR-REFERENCE1   TO D-PONO
           MOVE STTR-ORDERQTY     TO D-ORDERQTY
           MOVE STTR-SHIPQTY      TO D-SHIPQTY
           MOVE STTR-SHIPPEDQTY   TO D-SHIPPEDQTY
           MOVE STTR-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-DATE
           MOVE STTR-PRICE        TO D-PRICE
           COMPUTE WS-QUANTITY = STTR-ORDERQTY -
                  (STTR-SHIPQTY + STTR-SHIPPEDQTY)
           COMPUTE WS-VALUE ROUNDED = WS-QUANTITY * STTR-PRICE
           MOVE WS-VALUE                TO D-VALUE
           MOVE STTR-TRANSACTION-NUMBER TO D-TRANS
           ADD WS-VALUE TO WS-TOTAL-VALUE
                           WS-RUNNING-TOTAL
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           MOVE "N" TO WS-SKIP-REC
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       READ-REGISTER SECTION.
       RR-000.
           MOVE STTR-TYPE       TO INCR-TRANS
           MOVE STTR-REFERENCE1 TO INCR-INVOICE
               START INCR-REGISTER KEY NOT < INCR-KEY
                   INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
              MOVE 88 TO WS-INCR-ST1
              GO TO RR-999.
       RR-005.
           READ INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
              MOVE 88 TO WS-INCR-ST1
              GO TO RR-999.
           IF WS-INCR-ST1 NOT = 0
              MOVE
              "REGISTER LOCKED AT ANOTHER WORKSTATION, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RR-005.
       RR-999.
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
       OPEN-010.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-010.
       OPEN-020.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-020.
       OPEN-025.
           Move Ws-Co-Name To Co-Name.
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
           IF LINE-CNT > 60
               PERFORM PRR-010.
           IF WS-SKIP-REC = "Y"
               GO TO END-100
           ELSE
               MOVE WS-TOTAL-VALUE TO TOT-VALUE
               WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
               MOVE WS-RUNNING-TOTAL TO RUN-VALUE
               WRITE PRINT-REC FROM RUNNING-LINE AFTER 1.
       END-100.
           IF WS-INITIAL > " "
               MOVE "B/O'S PRINTED FOR:" TO INIT-DESC1
               MOVE WS-INITIAL           TO INIT-NAME
           ELSE
               MOVE "ALL INITIALS PRINTED" TO INIT-DESC1
               MOVE " "                    TO INIT-NAME.
           WRITE PRINT-REC FROM INITIAL-LINE AFTER 1.
           
           IF WS-REP-ONLY = "Z"
               MOVE "B/O'S PRINTED FOR ACC'S WITH NO REP:" TO SMAN-DESC1
               MOVE " "                                    TO SMAN-NAME
               WRITE PRINT-REC FROM SMAN-LINE AFTER 1
               GO TO END-700.
                   
           IF WS-REP-ONLY > " "
               MOVE "B/O'S PRINTED FOR REP #:" TO SMAN-DESC2
               MOVE WS-REP-ONLY                TO SMAN-NAME
           ELSE
               MOVE "ALL REP B/O' PRINTED." TO SMAN-DESC2
               MOVE " "                     TO SMAN-NAME.
           WRITE PRINT-REC FROM SMAN-LINE AFTER 1.
       END-700.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-800.
           CLOSE DEBTOR-MASTER
                 STOCK-TRANS-FILE
                 INCR-REGISTER
                 PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
      *      STOP RUN.
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
