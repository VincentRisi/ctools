        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlQuRgRp.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectSlRegister".
        Copy "SelectDrMaster".
         Copy "SelectSlQuoteBatch".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdRegister.
           COPY ChlfdDebtor.
           COPY ChlfdSlQuoteBatch.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(255).

      *
       WORKING-STORAGE SECTION.
       77  WS-INITIAL           PIC X(2) VALUE " ".
       77  WS-DATE-ACCEPT       PIC X(10) VALUE " ".
       77  WS-REP-ONLY          PIC X VALUE " ".
       77  WS-TODAY-ONLY        PIC X VALUE " ".
       77  WS-QUANTITY          PIC S9(5) VALUE 0.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-TRANSNO           PIC 9(3) VALUE 0.
       77  WS-TOTI-INVOICE      PIC S9(8)V99 VALUE 0.
       77  WS-INVNO             PIC 9(5) VALUE 0.
       77  WS-INVDISPLAY        PIC Z(5)9.
       77  WS-ACCEPT            PIC XX VALUE " ".
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-QUBATCH-STATUS.
           03  WS-QUBATCH-ST1         PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  HEAD1.
           03  FILLER           PIC X(5) VALUE "DATE".
           03  H1-DATE          PIC X(10).
           03  FILLER           PIC X(6) VALUE " ".
           03  FILLER           PIC X(11) VALUE "INTERNAL :".
           03  H-CODE           PIC X(17) VALUE " ".
           03  FILLER           PIC X(40) VALUE 
           "Q U O T A T I O N   R E G I S T E R".
           03  H-TYPE           PIC X(30) VALUE " ".
           03  FILLER           PIC X(6) VALUE "PAGE: ".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(4) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(21) VALUE " ".
           03  FILLER           PIC X(11) VALUE "SALESMAN#:".
           03  H2-SALESMAN      PIC X(17) VALUE " ".
           03  FILLER           PIC X(103) VALUE 
           "***********************************".
       01  HEAD3.
           03  FILLER           PIC X(7) VALUE "  SLIP".
           03  FILLER           PIC X(36) VALUE "  A C C O U N T".
           03  FILLER           PIC X(20) VALUE "QUOTE       REMAIN".
           03  FILLER           PIC X(64) VALUE
           "CUSTOMER               LI  CONTACT               PHONE".
           03  FILLER           PIC X(7) VALUE "  SOLD".
       01  HEAD4.
           03  FILLER           PIC X(7) VALUE "   No:".
           03  FILLER           PIC X(35) VALUE " NUMBER NAME".
           03  FILLER           PIC X(21) VALUE "  DATE       AMOUNT".
           03  FILLER           PIC X(65) VALUE
           "ORDER No:              NE  NAME                  No:".
           03  FILLER           PIC X(7) VALUE "  BY".
       01  DETAIL-LINE.
           03  D-INVNO          PIC Z(5)9.
           03  D-QB-FOUND       PIC X(1) VALUE " ".
           03  D-CUSTNO         PIC X(8) VALUE " ".
           03  D-NAME           PIC X(25) VALUE " ".
           03  FILLER           PIC X VALUE " ".
           03  D-DATE           PIC X(10).
           03  FILLER           PIC X VALUE " ".
           03  D-INVAMT         PIC Z(5)9.99.
           03  FILLER           PIC X(2) VALUE " ".
           03  D-PO             PIC X(22) VALUE " ".
           03  D-LINE           PIC Z(2)9.
           03  FILLER           PIC X(2) VALUE " ".
           03  D-CONTACT        PIC X(22) VALUE " ".
           03  D-PHONE          PIC X(17) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  D-SOLDBY         PIC X(6) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER           PIC X(5) VALUE " ".
           03  TOT-DESC         PIC X(22) VALUE " ".
           03  TOT-NO           PIC Z(4)9.
           03  FILLER           PIC X(7) VALUE " ".
           03  FILLER           PIC X(11) VALUE "  TOTALS: R".
           03  TOT-INVAMT       PIC Z(7)9.99.
           03  FILLER           PIC X(73) VALUE " ".
       01  TOTAL-LINE2.
         02  TOT2-LINE.
           03  TOT2-DESC        PIC X(27) VALUE " ".
           03  TOT2-DATE        PIC X(10).
           03  FILLER           PIC X(23) VALUE " ".
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
           DISPLAY "** Quotation Register Report **" AT POS
           MOVE 415 TO POS
           DISPLAY "*******************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-015.
           MOVE 1310 TO POS.
           DISPLAY "ENTER AN INTERNALS INITIALS, BLANK FOR ALL.   [  ]"
           AT POS.
           ADD 47 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-INITIAL.

      *     ACCEPT WS-INITIAL AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-018
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-018.
           MOVE 1410 TO POS
           DISPLAY "ENTER A REP # FROM DEBTOR FILE, BLANK FOR ALL,"
           AT POS
           MOVE 1512 TO POS
           DISPLAY "OR ENTER 'Z' FOR ACCOUNTS WITH NO REP#      [ ]"
           AT POS
           ADD 47 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-REP-ONLY.

      *     ACCEPT WS-REP-ONLY AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-019
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-018.
       CONTROL-019.
           MOVE "Y" TO WS-TODAY-ONLY.
           MOVE 1810 TO POS
           DISPLAY
           "ENTER: O= ONE WEEK AGO, T= TWO WEEKS AGO, M= 1 MONTH AGO."
            AT POS
           MOVE 1710 TO POS.
           DISPLAY "PRINT ONLY QUOTES FOR TODAY, ENTER Y OR N :   [ ]"
           AT POS.
           ADD 47 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TODAY-ONLY.

      *     ACCEPT WS-TODAY-ONLY AT POS.
           IF WS-TODAY-ONLY NOT = "Y" AND NOT = "N" AND NOT = "M"
                        AND NOT = "T" AND NOT = "O"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-019.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-018.
           IF WS-TODAY-ONLY = "Y" OR = "T" OR = "M" OR = "O"
               GO TO CONTROL-025.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-019.
       CONTROL-020.
           MOVE 1910 TO POS.
           DISPLAY
           "ENTER A DATE TO PRINT FROM, BLANK FOR ALL. :  [          ]"
               AT POS.
           ADD 47 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATE-ACCEPT.

      *     ACCEPT WS-DATE-ACCEPT AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-019.
               
           IF WS-DATE-ACCEPT  = " "
               GO TO CONTROL-025.
           MOVE WS-DATE-ACCEPT TO ALPHA-RATE
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
               GO TO CONTROL-020.
           MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE
           DISPLAY DISPLAY-DATE AT POS
           MOVE DISPLAY-DATE    TO TOT2-DATE
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE      TO INCR-DATE WS-AGE-DATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
               GO TO CONTROL-020.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-025.
           MOVE 2510 TO POS.
           DISPLAY "The report is being compiled ......." AT POS.
           PERFORM OPEN-FILES.
           IF WS-TODAY-ONLY = "T" OR = "M" OR = "O"
              PERFORM CALCULATE-DATE-TO-PRINT-FOR.
           IF WS-TODAY-ONLY = "Y"
               MOVE WS-DATE TO INCR-DATE WS-DATE-ACCEPT.
           PERFORM PRINT-ROUTINE.
           PERFORM PRINT-TOTALS.
           PERFORM END-OFF.
      *
       CALCULATE-DATE-TO-PRINT-FOR SECTION.
       CDTPF-005.
           IF WS-TODAY-ONLY = "M"
              GO TO CDTPF-500.
           MOVE WS-DD TO WS-QUANTITY.
           IF WS-TODAY-ONLY = "T"
               SUBTRACT 14 FROM WS-QUANTITY
           ELSE
               SUBTRACT 7  FROM WS-QUANTITY.
           IF WS-QUANTITY < 1
              ADD 31 TO WS-QUANTITY
              SUBTRACT 1 FROM WS-MM.
           IF WS-MM = 0 
              MOVE 12 TO WS-MM
              SUBTRACT 1 FROM WS-YY.
           MOVE WS-QUANTITY TO WS-DD
           GO TO CDTPF-600.
       CDTPF-500.
           SUBTRACT 1 FROM WS-MM.
           IF WS-MM = 0
              MOVE 12 TO WS-MM.
       CDTPF-600.
           MOVE WS-DATE TO INCR-DATE WS-DATE-ACCEPT.
       CDTPF-999.
           EXIT.
      *
       READ-DEBTOR SECTION.
       RD-005.
           MOVE INCR-ACCOUNT TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "** UNKNOWN DEBTOR ON START **" TO DR-NAME
               GO TO RD-999.
       RD-010.
           READ DEBTOR-MASTER 
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "** UNKNOWN DEBTOR ON READ **" TO DR-NAME
               GO TO RD-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-010.
       RD-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           IF WS-DATE-ACCEPT = " "
               MOVE 8 TO INCR-TRANS
               MOVE 1 TO INCR-INVOICE
               START INCR-REGISTER KEY NOT < INCR-KEY
                   INVALID KEY NEXT SENTENCE
           ELSE
               START INCR-REGISTER KEY NOT < INCR-DATE
                   INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
              GO TO PR-999.
       PR-005.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10 OR 23
              GO TO PR-999.
           IF WS-INCR-ST1 NOT = 0
             MOVE "REGISTER BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-INCR-ST1
              GO TO PR-005.
           IF INCR-TRANS NOT = 8
            IF WS-DATE-ACCEPT NOT = " "
              GO TO PR-005
            ELSE
              GO TO PR-999.
              
           MOVE 2410 TO POS
           DISPLAY "Quote Number Read:" AT POS
           ADD 20 TO POS
           MOVE INCR-INVOICE TO WS-INVDISPLAY
           DISPLAY WS-INVDISPLAY AT POS.
           
           IF WS-TODAY-ONLY = "Y" OR = "T" OR = "M" OR = "O"
            IF INCR-DATE NOT = WS-DATE
              GO TO PR-999.
      *        GO TO PR-005.
              
           IF WS-INITIAL = "  "
              GO TO PR-008.
           IF WS-INITIAL NOT = "  "
            IF INCR-SB-TYPE = WS-INITIAL
              GO TO PR-008.
           GO TO PR-005.
       PR-008.
           IF WS-REP-ONLY = "Z"
                PERFORM READ-DEBTOR
            IF DR-SALESMAN NOT = " "
                GO TO PR-005.
           IF WS-REP-ONLY NOT = " " AND NOT = "Z"
                PERFORM READ-DEBTOR
            IF WS-REP-ONLY NOT = DR-SALESMAN
                GO TO PR-005.
           IF WS-DATE-ACCEPT NOT = " "
            IF INCR-DATE NOT < WS-AGE-DATE
              GO TO PR-010
            ELSE
              GO TO PR-005.
       PR-010.
           IF WS-LINE > 59
               PERFORM PRINT-HEADINGS.
               
           PERFORM READ-QUOTE-BATCH.
               
           MOVE INCR-INVOICE      TO D-INVNO
           MOVE INCR-ACCOUNT      TO D-CUSTNO
           MOVE INCR-NAME         TO D-NAME
           MOVE INCR-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-DATE
           MOVE INCR-PORDER       TO D-PO
           MOVE INCR-INVCRED-AMT  TO D-INVAMT
           MOVE INCR-LINENO       TO D-LINE
           MOVE INCR-CONTACT      TO D-CONTACT
           MOVE INCR-PHONE        TO D-PHONE
           MOVE INCR-SB-TYPE      TO D-SOLDBY.
       PR-020.
           ADD INCR-INVCRED-AMT TO WS-TOTI-INVOICE.
           ADD 1                TO WS-INVNO.
           MOVE 2610 TO POS
           DISPLAY "Number Of Quotes Printed:" AT POS
           ADD 25 TO POS
           MOVE WS-INVNO TO WS-INVDISPLAY
           DISPLAY WS-INVDISPLAY AT POS.
       PR-900.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 2.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           ADD 2 TO WS-LINE.
           GO TO PR-005.
       PR-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           IF WS-INITIAL = "  "
               MOVE "ALL REPS" TO H-CODE
           ELSE
               MOVE WS-INITIAL TO H-CODE.
           IF WS-REP-ONLY = "Z"
               MOVE "NO SALESMEN" TO H2-SALESMAN.
           IF WS-REP-ONLY NOT = " " AND NOT = "Z"
               MOVE WS-REP-ONLY    TO H2-SALESMAN
           ELSE
               MOVE "ALL SALESMEN" TO H2-SALESMAN.
           ADD 1        TO WS-PAGE
           MOVE WS-PAGE TO H-PAGE
           MOVE 8       TO WS-LINE.

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
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
       PH-999.
           EXIT.
      *
       PRINT-TOTALS SECTION.
       PT-000.
           IF WS-LINE > 55
              PERFORM PRINT-HEADINGS.
           MOVE WS-TOTI-INVOICE         TO TOT-INVAMT.
           MOVE "    NUMBER OF QUOTES:" TO TOT-DESC.
           MOVE WS-INVNO                TO TOT-NO.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.
           MOVE " " TO PRINT-REC.
           
           IF WS-TODAY-ONLY = "Y"
              MOVE "*** ONLY TODAY'S OUTSTANDING QUOTES PRINTED ***"
              TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           IF WS-TODAY-ONLY = "O"
              MOVE
           "*** ONLY OUTSTANDING QUOTES FROM ONE WEEK AGO PRINTED ***"
              TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           IF WS-TODAY-ONLY = "T"
              MOVE
           "*** ONLY OUTSTANDING QUOTES FROM TWO WEEKS AGO PRINTED ***"
              TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           IF WS-TODAY-ONLY = "M"
              MOVE 
           "*** ONLY OUTSTANDING QUOTES FROM A MONTH AGO PRINTED ***"
              TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
           IF WS-DATE-ACCEPT NOT = " "
               MOVE "ONLY QUOTES PRINTED FROM :" TO TOT2-DESC
               WRITE PRINT-REC FROM TOTAL-LINE2.
           MOVE " " TO PRINT-REC TOTAL-LINE2.
              
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       PT-999.
           EXIT.
      *
       READ-QUOTE-BATCH SECTION.
       RQB-010.
            MOVE INCR-INVOICE TO QB-QUOTENUMBER.
            START QUOTEBATCH KEY NOT < QB-KEY
                   INVALID KEY NEXT SENTENCE.
           IF WS-QUBATCH-ST1 NOT = 0
                MOVE " " TO D-QB-FOUND
              GO TO RQB-999.
       RQB-020.
            READ QUOTEBATCH
                INVALID KEY NEXT SENTENCE.
            IF WS-QUBATCH-ST1 NOT = 0
      *          MOVE "QUOTE BATCH FILE BUSY ON READ, 'ESC' TO RETRY"
      *          TO WS-MESSAGE
      *          PERFORM ERROR1-000
      *          MOVE WS-QUBATCH-ST1 TO WS-MESSAGE
      *          PERFORM ERROR-MESSAGE
      *          PERFORM ERROR1-020
      *          MOVE 0 TO WS-QUBATCH-ST1
                MOVE " " TO D-QB-FOUND
                GO TO RQB-999.
            MOVE "*" TO D-QB-FOUND.
       RQB-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-020.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-020.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-021.
          OPEN I-O QUOTEBATCH.
          IF WS-QUBATCH-ST1 NOT = 0
              MOVE "QUBATCH FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-QUBATCH-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE WS-SLQUOTEBATCH TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-QUBATCH-ST1
              GO TO OPEN-021.
       OPEN-030.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE 0 TO WS-DEBTOR-ST1
              MOVE "DR MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-030.
       OPEN-050.
           MOVE "ALL PENDING QUOTES" TO H-TYPE.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE INCR-REGISTER
                 DEBTOR-MASTER
                 QUOTEBATCH.
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
      * END-OF-JOB
