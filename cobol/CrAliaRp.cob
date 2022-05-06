        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrAliaRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrMaster".
          Copy "SelectCrAlias".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrAlias.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(7) VALUE " ".
       77  WS-RANGE2            PIC X(7) VALUE " ".
       77  WS-NIL-BAL           PIC X VALUE " ".
       01  WS-CREDITOR-STATUS.
           03  WS-CR-ST1        PIC 99.
       01  WS-ALIAS-STATUS.
           03  WS-ALIAS-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(8) VALUE " ".
           03  FILLER         PIC X(45) VALUE
           "C R E D I T O R   A L I A S   L I S T".
           03  FILLER         PIC X(1) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
       01  HEAD2.
           03  FILLER         PIC X(25) VALUE " ".
           03  FILLER         PIC X(37) VALUE ALL "*".
           03  FILLER         PIC X(49) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(10) VALUE "ALIAS".
           03  FILLER         PIC X(10) VALUE "ACCOUNT".
           03  FILLER         PIC X(42) VALUE "NAME".
           03  FILLER         PIC X(10) VALUE "CURRENCY".
           03  FILLER         PIC X(7) VALUE "TERMS".
       01  DETAIL-LINE.
           03  D-ALIAS        PIC X(10).
           03  D-ACCOUNT      PIC X(10).
           03  D-NAME         PIC X(42).
           03  D-CURRENCY     PIC X(12).
           03  D-TERMS        PIC X(7).
      *
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
           DISPLAY "** CREDITOR ALIAS LIST **" AT POS
           MOVE 421 TO POS
           DISPLAY "*************************" AT POS.
       CONTROL-003.
           Copy "PrinterAcceptCr".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
       GET-DATA SECTION.
       GET-000.
           MOVE " " TO WS-RANGE1 WS-RANGE2
           MOVE 1010 TO POS
           DISPLAY "         FROM ALIAS NAME     : [       ]"
                      AT POS
           MOVE 1042 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

      *     ACCEPT WS-RANGE1 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
           MOVE 1210 TO POS
           DISPLAY "           TO ALIAS NAME     : [       ]"
                     AT POS
           MOVE 1242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

      *     ACCEPT WS-RANGE2 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-000.
           IF WS-RANGE2 = " "
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-060
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-060.
           MOVE 2510 TO POS
           DISPLAY "The Report is being compiled.........." AT POS
           PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            START CREDITOR-MASTER KEY NOT < CR-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-CR-ST1 NOT = 0
               MOVE "NO CREDITORS TO PRINT, 'ESC' TO EXIT"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PRR-999.
            
            MOVE WS-RANGE1 TO CRAL-ALIAS.
            START CRALIAS-MASTER KEY NOT < CRAL-ALIAS
               INVALID KEY NEXT SENTENCE.
            IF WS-ALIAS-ST1 NOT = 0
               MOVE "NO ALIAS'S TO PRINT ON START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PRR-999.
       PRR-002.
            READ CRALIAS-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-ALIAS-ST1 = 10
               MOVE 0 TO WS-ALIAS-ST1
               GO TO PRR-999.
            IF WS-ALIAS-ST1 NOT = 0
               MOVE "ALIAS BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-ALIAS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-ALIAS-ST1
               GO TO PRR-002.
            IF CRAL-ALIAS < WS-RANGE1
               GO TO PRR-002.
            IF CRAL-ALIAS > WS-RANGE2
               GO TO PRR-999.
       PRR-008.
            MOVE CRAL-ACCOUNT-NUMBER TO CR-ACCOUNT-NUMBER.
            
            MOVE 2610 TO POS
            DISPLAY "Account Number Being Read:" AT POS
            ADD 27 TO POS
            DISPLAY CR-ACCOUNT-NUMBER AT POS.
       PRR-010.
            READ CREDITOR-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-CR-ST1 = 23 OR 35 OR 49
               MOVE " " TO CR-NAME.
            IF LINE-CNT < 60
               GO TO PRR-020.
       PRR-015.
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
               WRITE PRINT-REC BEFORE PAGE
               WRITE PRINT-REC FROM COMPANY-LINE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD3
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC
           MOVE 6   TO LINE-CNT.
       PRR-020.
           MOVE CRAL-ALIAS           TO D-ALIAS
           MOVE CR-ACCOUNT-NUMBER    TO D-ACCOUNT
           MOVE CR-NAME              TO D-NAME.
           IF CR-CURRENCY = " "
               MOVE "SA RANDS"       TO D-CURRENCY
           ELSE
               MOVE CR-CURRENCY      TO D-CURRENCY.
           MOVE CR-TERMS             TO D-TERMS.

           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC.
           ADD 1 TO LINE-CNT.
           
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O CREDITOR-MASTER.
           IF WS-CR-ST1 NOT = 0
             MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO OPEN-000.
       OPEN-003.
            OPEN I-O CRALIAS-MASTER.
            IF WS-ALIAS-ST1 NOT = 0
               MOVE 0 TO WS-ALIAS-ST1
               MOVE "ALIAS FILE BUSY ON OPEN,  'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-003.
       OPEN-020.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
          EXIT.
      *
       END-OFF SECTION.
       END-000.
            IF LINE-CNT > 60
               PERFORM PRR-015.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-500.
           CLOSE CREDITOR-MASTER
                 CRALIAS-MASTER.
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
      *
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
      * END-OF-JOB.
