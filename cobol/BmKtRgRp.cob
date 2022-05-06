        IDENTIFICATION DIVISION.
        PROGRAM-ID. BmKtRgRp.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
       Copy "SelectSlRegister".
       Copy "SelectStTrans".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdRegister.
           COPY ChlfdStTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).

      *
       WORKING-STORAGE SECTION.
       77  WS-ANSWER1           PIC X VALUE " ".
       77  WS-QUANTITY          PIC S9(5) VALUE 0.
       77  WS-QTY               PIC 9(3) VALUE 0.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-TRANSNO           PIC 9(3) VALUE 0.
       77  WS-TOTI-INVOICE      PIC S9(6)V99 VALUE 0.
       77  WS-INVNO             PIC 9(5) VALUE 0.
       77  WS-ACCEPT            PIC XX VALUE " ".
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-BO-ST1        PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  HEAD1.
           03  FILLER           PIC X(5) VALUE "DATE".
           03  H1-DATE          PIC X(10).
           03  FILLER           PIC X(28) VALUE " ".
           03  FILLER           PIC X(50) VALUE 
           "BILL OF MATERIALS - WORKS ORDERS OUTSTANDING".
           03  H1-COMM          PIC X(26).
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(43) VALUE " ".
           03  FILLER           PIC X(44) VALUE ALL "*".
           03  FILLER           PIC X(45) VALUE " ".
       01  HEAD3.
           03  FILLER           PIC X(9) VALUE "SLIP #".
           03  FILLER           PIC X(10) VALUE "DATE".
           03  FILLER           PIC X(20) VALUE "KIT NAME".
           03  FILLER           PIC X(7) VALUE "PRICE".
           03  FILLER           PIC X(41) VALUE
           "DESCRIPTION / SLIP COMMENT".
           03  FILLER           PIC X(45) VALUE "ORD. QTY    MFG QTY".
       01  DETAIL-LINE.
           03  D-INVNO          PIC Z(5)9.
           03  FILLER           PIC X(1) VALUE " ".
           03  D-DATE           PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
           03  D-NAME           PIC X(16) VALUE " ".
           03  D-PRICE          PIC Z(5)9.99.
           03  FILLER           PIC X(2) VALUE " ".
           03  D-DESC1          PIC X(20) VALUE " ".
           03  D-DESC2          PIC X(25) VALUE " ".
           03  D-ORDQTY         PIC Z(2)9.
           03  FILLER           PIC X(7) VALUE " ".
           03  D-MFGQTY         PIC Z(2)9.
           03  D-PULLED         PIC X(2) VALUE " ".
           03  D-PULL-BY        PIC X(2) VALUE " ".
           03  FILLER           PIC X(24) VALUE " ".
       01  TRANS-LINE.
           03  FILLER           PIC X(28) VALUE " ".
           03  FILLER           PIC X(13) VALUE "ITEMS ON B/O:".
           03  D-LINE           PIC Z(2)9.
           03  FILLER           PIC X(2) VALUE " ".
           03  D-COMMENT        PIC X(40) VALUE " ".
           03  FILLER           PIC X(46) VALUE " ".
       01  TOTAL-LINE.
           03  TOT-DESC         PIC X(17) VALUE " ".
           03  TOT-NO           PIC Z(4)9.
           03  FILLER           PIC X(110) VALUE " ".
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
           DISPLAY
           "** Bill Of Materials Outstanding Works Orders Report **"
             AT POS
           MOVE 415 TO POS
           DISPLAY
           "*******************************************************"
             AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-015.
           MOVE 1310 TO POS.
           DISPLAY "R=RESERVED ONLY, I=INCOMPLETE ORDERS," AT POS.
           MOVE 1410 TO POS.
           DISPLAY "A=ALL ORDERS NOT FULLY COMPLETE, C=COMPLETE: [ ]"
            AT POS.
           ADD 46 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF WS-ANSWER1 NOT = "R" AND NOT = "I"
                     AND NOT = "A" AND NOT = "C"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
           IF WS-ANSWER1 = "R" 
                MOVE "RESERVED BILLS ONLY       " TO H1-COMM.
           IF WS-ANSWER1 = "I" 
                MOVE "PARTIAL MNFG. OF BILLS    " TO H1-COMM.
           IF WS-ANSWER1 = "A" 
                MOVE "ALL BILLS NOT COMPLETE    " TO H1-COMM.
           IF WS-ANSWER1 = "C" 
                MOVE "ALL COMPLETED BILLS       " TO H1-COMM.
           MOVE 2510 TO POS
           DISPLAY "The report is being compiled ......." AT POS.
           PERFORM OPEN-FILES.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM PRINT-ROUTINE.
           PERFORM PRINT-TOTALS.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RT-000.
            MOVE 0            TO WS-TRANSNO.
            MOVE INCR-INVOICE TO STTR-REFERENCE1.
            MOVE 7            TO STTR-TYPE.
            MOVE 0            TO STTR-TRANSACTION-NUMBER.
            START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                 INVALID KEY NEXT SENTENCE.
       RT-002.
            READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-BO-ST1 = 10
               GO TO RT-999.
            IF WS-BO-ST1 NOT = 0
               MOVE "ST-TRANS BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-BO-ST1
               GO TO RT-002.
            IF STTR-REFERENCE1 NOT = INCR-INVOICE
               GO TO RT-999.
            IF STTR-TYPE NOT = 7
               GO TO RT-999.
            IF STTR-COMPLETE = "Y" OR = "L"
             IF WS-ANSWER1 NOT = "C"
               GO TO RT-002.
            MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
               GO TO RT-002.
       RT-005.
            IF STTR-ORDERQTY NOT = STTR-SHIPQTY + STTR-SHIPPEDQTY
               ADD 1 TO WS-TRANSNO.
            GO TO RT-002.
       RT-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           MOVE 7 TO INCR-TRANS
           MOVE 0 TO INCR-INVOICE.
           START INCR-REGISTER KEY NOT < INCR-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
              GO TO PR-999.
       PR-005.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
              GO TO PR-999.
           IF INCR-TRANS NOT = 7
              GO TO PR-999.
           IF INCR-PRINTED = "Y" OR = "L"
            IF WS-ANSWER1 NOT = "C"
              GO TO PR-005.
              
           IF WS-ANSWER1 = "R"
            IF INCR-PRINTED = "N"
              GO TO PR-010.
           IF WS-ANSWER1 = "I"
            IF INCR-PRINTED = "P"
              GO TO PR-010.
           IF WS-ANSWER1 = "A"
            IF INCR-PRINTED = "P" OR = "N"
              GO TO PR-010.
           IF WS-ANSWER1 = "C"
            IF INCR-PRINTED = "Y" OR = "L"
              GO TO PR-010.
           GO TO PR-005.
       PR-010.
           IF WS-LINE > 58
               PERFORM PRINT-HEADINGS.
           MOVE INCR-INVOICE      TO D-INVNO
           MOVE INCR-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-DATE.
           MOVE INCR-KITNAME      TO D-NAME
           MOVE INCR-KITPRICE     TO D-PRICE
           MOVE INCR-KITDESC1     TO D-DESC1
           MOVE INCR-KITDESC2     TO D-DESC2
           MOVE INCR-KITQTY       TO D-ORDQTY
           MOVE INCR-KITSHPDQTY   TO D-MFGQTY
           MOVE INCR-KITCOMMENT   TO D-COMMENT.
           IF INCR-PULL-DATE > 0
               MOVE "P"           TO D-PULLED
               MOVE INCR-PULLBY   TO D-PULL-BY
           ELSE
               MOVE " "           TO D-PULL-BY
               MOVE " "           TO D-PULLED.
           PERFORM READ-TRANSACTIONS.
           MOVE WS-TRANSNO        TO D-LINE.
       PR-020.
           ADD WS-AMOUNT TO WS-TOTI-INVOICE.
           ADD 1         TO WS-INVNO.
       PR-900.
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           WRITE PRINT-REC FROM TRANS-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           ADD 3 TO WS-LINE
           GO TO PR-005.
       PR-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           ADD 1        TO WS-PAGE
           MOVE WS-PAGE TO H-PAGE
           MOVE 7       TO WS-LINE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF WS-PAGE = 1
              WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
              WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           WRITE PRINT-REC FROM HEAD3 AFTER 2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
       PH-999.
           EXIT.
      *
       PRINT-TOTALS SECTION.
       PT-000.
           IF WS-LINE > 56
              PERFORM PRINT-HEADINGS.
           IF WS-ANSWER1 NOT = "C"
              MOVE "No. OF O/S BILLS:" TO TOT-DESC
           ELSE
              MOVE "Completed BILLS :" TO TOT-DESC.
           MOVE WS-INVNO            TO TOT-NO.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.
           MOVE " " TO PRINT-REC.
       PT-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-021.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-021.
       OPEN-025.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-BO-ST1 NOT = 0
              MOVE 0 TO WS-BO-ST1
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-025.
       OPEN-050.
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
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE
                 INCR-REGISTER
                 STOCK-TRANS-FILE.
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
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
