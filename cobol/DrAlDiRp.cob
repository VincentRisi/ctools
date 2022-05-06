        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrAlDiRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(250).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-SUB1-DIS          PIC ZZ9.
       77  WS-RANGE             PIC X VALUE " ".
       77  W-ACC-DISPLAY        PIC Z(6)9.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-DISCOUNT-FIELD-NAMES.
         02  WS-DISCOUNT-FIELDS OCCURS 10.
           03  WS-NO-ACCS     PIC 9(7).
           03  WS-SALES-PTD   PIC S9(7)V99.
           03  WS-SALES-YTD   PIC S9(7)V99.
           03  WS-SALES-LY    PIC S9(7)V99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10) VALUE " ".
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(51) VALUE
           "DEBTOR  DISCOUNTS  BY  CODE".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(4) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(20) VALUE " ".
           03  FILLER         PIC X(27) VALUE ALL "*".
           03  FILLER         PIC X(33) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(55) VALUE
           "NUMBER   NAME                                     DISC".
           03  H3-FILLER      PIC X(32) VALUE
           "SALES PTD  SALES YTD  SALES L/Y".
       01  DETAIL-LINE.
           03  D-ACCOUNT      PIC X(7).
           03  FILLER         PIC X(2) VALUE " ".
           03  D-NAME         PIC X(42) VALUE " ".
           03  D-DISCOUNT     PIC X(3).
           03  D-SALESPTD     PIC Z(6)9.99-.
           03  D-SALESYTD     PIC Z(6)9.99-.
           03  D-SALES-LY     PIC Z(6)9.99-.
           03  FILLER         PIC X(4) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 315 TO POS
           DISPLAY "** ONLY DEBTORS WITH DISCOUNT REPORT **" AT POS
           MOVE 415 TO POS
           DISPLAY "***************************************" AT POS.
           PERFORM OPEN-FILES.
       CONTROL-003.
           Copy "PrinterAcceptDr".
       CONTROL-010.
            MOVE 1210 TO POS.
            DISPLAY "Print SALES Amounts, Enter Y Or N." AT POS.
            MOVE 1254 TO POS.
            DISPLAY "[ ]" AT POS.  
            MOVE 1255 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE.

      *      ACCEPT WS-RANGE AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
            IF WS-RANGE NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-015
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-015.
            MOVE 2910 TO POS.
            DISPLAY "The Report Is Being Compiled, Please Be Patient."
                 AT POS.
       CONTROL-020.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            PERFORM PRINT-ROUTINE.
            PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           IF WS-RANGE = "N"
               MOVE " " TO H3-FILLER.
           START DEBTOR-MASTER KEY NOT < DR-KEY
               INVALID KEY NEXT SENTENCE.
       PRR-005.
           READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
               GO TO PRR-999.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49 OR 91
               MOVE "DEBTOR FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
              MOVE 0 TO WS-DEBTOR-ST1
              GO TO PRR-005.
           IF WS-MESSAGE NOT = " "
              MOVE 3010 TO POS
              MOVE " " TO WS-MESSAGE
              DISPLAY WS-MESSAGE AT POS.
           IF DR-DISCOUNT-CODE > "0"
              GO TO PRR-010.
           GO TO PRR-005.
       PRR-010.
            MOVE 2510 TO POS
            DISPLAY "Debtor Being Printed:" AT POS
            ADD 22 TO POS
            DISPLAY DR-ACCOUNT-NUMBER AT POS.
            IF LINE-CNT < 60
               GO TO PRR-020.
       PRR-015.
           ADD 1      TO PAGE-CNT
           MOVE DISPLAY-DATE TO H1-DATE
           MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD1
            WRITE PRINT-REC FROM HEAD2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE 7 TO LINE-CNT.
       PRR-020.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           IF WS-RANGE = "N"
              GO TO PRR-025.
           MOVE DR-DISCOUNT-CODE TO SUB-1
           MOVE DR-SALES-PTD     TO D-SALESPTD
           MOVE DR-SALES-YTD     TO D-SALESYTD
           MOVE DR-SALES-LAST    TO D-SALES-LY.
           ADD 1                 TO WS-NO-ACCS (SUB-1)
           ADD  DR-SALES-PTD     TO WS-SALES-PTD (SUB-1)
           ADD  DR-SALES-YTD     TO WS-SALES-YTD (SUB-1)
           ADD  DR-SALES-LAST    TO WS-SALES-LY (SUB-1).
       PRR-025.
           MOVE DR-ACCOUNT-NUMBER TO D-ACCOUNT
           MOVE DR-NAME           TO D-NAME
           MOVE DR-DISCOUNT-CODE  TO D-DISCOUNT
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC.
           ADD 1 TO LINE-CNT.
           GO TO PRR-005.
       PRR-999.
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
           MOVE Ws-Co-Name TO CO-Name.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
      *     ACCEPT WS-DATE FROM DATE.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF WS-RANGE = "N"
               GO TO END-500.
           MOVE 1 TO SUB-1.
       END-010.
           IF LINE-CNT > 50
              PERFORM PRR-015.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           WRITE PRINT-REC.
           WRITE PRINT-REC.
           MOVE "      ***TOTALS FOR CODE:" TO D-NAME.
       END-020.
           MOVE WS-NO-ACCS (SUB-1)          TO W-ACC-DISPLAY
           MOVE W-ACC-DISPLAY               TO D-ACCOUNT
           MOVE SUB-1                       TO WS-SUB1-DIS
           MOVE WS-SUB1-DIS                 TO D-DISCOUNT
           MOVE WS-SALES-PTD (SUB-1)        TO D-SALESPTD
           MOVE WS-SALES-YTD (SUB-1)        TO D-SALESYTD
           MOVE WS-SALES-LY (SUB-1)         TO D-SALES-LY.
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO DETAIL-LINE.
           IF SUB-1 = 9
              GO TO END-500.
           ADD 1 TO LINE-CNT SUB-1.
           GO TO END-020.
       END-500.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE
                DEBTOR-MASTER.
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
