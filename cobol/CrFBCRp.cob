        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrFBCRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrFBCHeader".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCrFBCHeader.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-RANGE1            PIC X(20) VALUE " ".
       77  WS-RANGE2            PIC X(20) VALUE " ".
       77  WS-CURRENCY          PIC X(5) VALUE " ".
       77  WS-DATEANSWER        PIC X(10) VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-LOCAL-UNAPP       PIC S9(7)V99.
       77  WS-LOC-TOTAL         PIC S9(7)V99.
       77  WS-LOC-UNAPPLIED     PIC S9(7)V99.
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       01  WS-HEADER-STATUS.
           03  WS-HEADER-ST1    PIC 99.
       01  WS-CHECKDATE.
           03  WS-CHECKYY       PIC 9999.
           03  WS-CHECKMM       PIC 99.
           03  WS-CHECKDD       PIC 99.
       01  CURRENCY-TOTALS-NAME.
         02  CURRENCY-TOTALS OCCURS 20.
           03  WS-CURRENCY-TYPE PIC X(5).
           03  WS-FOR-AMOUNT    PIC S9(7)V99.
           03  WS-FOR-REMAIN    PIC S9(7)V99.
           03  WS-LOC-AMOUNT    PIC S9(7)V99.
           03  WS-LOC-REMAIN    PIC S9(7)V99.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE " DATE:".
           03  H-DATE           PIC X(10).
           03  FILLER           PIC X(22) VALUE " ".
           03  FILLER           PIC X(50) VALUE 
           "FOREX FBC LISTING FOR ALL CURRENCIES".
           03  FILLER           PIC X(1) VALUE " ".
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(32) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(39) VALUE " ".
           03  FILLER           PIC X(93) VALUE 
           "************************************".
       01  HEAD3.
           03  FILLER           PIC X(46) VALUE " ".
           03  FILLER           PIC X(10) VALUE " INITIAL".
           03  FILLER           PIC X(76) VALUE
           "REMAINING     INITIAL   REMAINING".
       01  HEAD3-1.
           03  FILLER           PIC X(56) VALUE
           "FBC CONTRACT #       CURRENCY    EXCHANGE     FOREIGN".
           03  FILLER           PIC X(76) VALUE
           "  FOREIGN       LOCAL       LOCAL   CONTRACT   MATURITY".
       01  HEAD4.
           03  FILLER           PIC X(56) VALUE
           "                       TYPE          RATE      AMOUNT".
           03  FILLER           PIC X(76) VALUE
           "   AMOUNT      AMOUNT      AMOUNT     DATE       DATE".
       01  CRFOREX-LINE1.
           03  FOREX-FBC-NUM        PIC X(20).
           03  FILLER               PIC X(3) VALUE " ".
           03  FOREX-CURRENCY       PIC X(7) VALUE " ".
           03  FILLER               PIC X(2) VALUE " ".
           03  FOREX-EXCHANGE-RATE  PIC Z(2)9.9(5).
           03  FILLER               PIC X(1) VALUE " ".
           03  FOREX-FOREIGN        PIC Z(7)9.99 VALUE " ".
           03  FILLER               PIC X(1) VALUE " ".
           03  FOREX-UNAPPLIED      PIC Z(7)9.99.
           03  FILLER               PIC X(1) VALUE " ".
           03  FOREX-LOCAL          PIC Z(7)9.99.
           03  FILLER               PIC X(1) VALUE " ".
           03  FOREX-LOCAL-UNAPP    PIC Z(7)9.99.
           03  FILLER               PIC X(3) VALUE " ".
           03  FOREX-MATURITY-DATE  PIC X(10).
           03  FILLER               PIC X(1) VALUE " ".
           03  FOREX-DUE-DATE       PIC X(10).
       01  TOTAL-LINE.
           03  TOTAL-DESC           PIC X(23) VALUE " ".
           03  TOTAL-CURRENCY       PIC X(7) VALUE " ".
           03  FILLER               PIC X(12) VALUE " ".
           03  TOTAL-FOREIGN        PIC Z(7)9.99 VALUE " ".
           03  FILLER               PIC X(1) VALUE " ".
           03  TOTAL-UNAPPLIED      PIC Z(7)9.99.
           03  FILLER               PIC X(1) VALUE " ".
           03  TOTAL-LOCAL          PIC Z(7)9.99.
           03  FILLER               PIC X(1) VALUE " ".
           03  TOTAL-LOCAL-UNAPP    PIC Z(7)9.99.
           03  FILLER               PIC X(21) VALUE " ".
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
           DISPLAY "**FOREX Header Details Report**" AT POS
           MOVE 415 TO POS
           DISPLAY "*******************************" AT POS.
           PERFORM ERROR-020.
       CONTROL-003.
           Copy "PrinterAcceptCr".
       CONTROL-010.
           MOVE " " TO WS-RANGE1.
           MOVE 1010 TO POS.
           DISPLAY "FROM FBC NUMBER    : [                    ]"
            AT POS.
           MOVE 1032 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 31        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

      *     ACCEPT WS-RANGE1 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-015.
           MOVE " " TO WS-RANGE2.
           MOVE 1210 TO POS.
           DISPLAY "  TO FBC NUMBER    : [                    ]"
            AT POS.
           MOVE 1232 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 31        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

      *     ACCEPT WS-RANGE2 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
           MOVE 1410 TO POS.
           DISPLAY "ENTER CURRENCY TO PRINT, BLANK FOR ALL: [     ]"
            AT POS.
           MOVE 1451 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 5        TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CURRENCY.

      *     ACCEPT WS-CURRENCY AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-040.
           MOVE 1710 TO POS
           DISPLAY "ONLY FBC's that mature AFTER this date will print."
            AT POS
           MOVE 1610 TO POS
           DISPLAY "ENTER A MATURITY DATE TO PRINT FROM : [          ]"
              AT POS
           ADD 39 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 48        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATEANSWER.

      *     ACCEPT WS-DATEANSWER AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF WS-DATEANSWER = " "
               GO TO CONTROL-040.
           MOVE WS-DATEANSWER   TO ALPHA-RATE
           PERFORM DATE-CHECKING
            IF SIGN-FOUND = 9
              GO TO CONTROL-040.
           MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE
           MOVE 1648 TO POS
           DISPLAY DISPLAY-DATE AT POS.
           PERFORM CONVERT-SPLIT-FORMAT
           MOVE SPLIT-DATE TO WS-CHECKDATE.
           PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
              GO TO CONTROL-040.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
       CONTROL-050.
           MOVE 2610 TO POS.
           DISPLAY "The Report Is Being compiled, Please Be Patient"
               AT POS.
       CONTROL-060.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           MOVE WS-RANGE1 TO CRFOREX-FBC-NUMBER.
           START CRFOREX-FILE KEY NOT < CRFOREX-FBC-NUMBER
               INVALID KEY NEXT SENTENCE.
           IF WS-HEADER-ST1 NOT = 0
               MOVE 88 TO WS-HEADER-ST1
               MOVE "CRFOREX-FILE ERROR IN START, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PR-999.
       PR-002.
           READ CRFOREX-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-HEADER-ST1 = 10
               GO TO PR-999.
           IF WS-HEADER-ST1 NOT = 0
               MOVE "CRFOREX-JRN FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-HEADER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-HEADER-ST1
               GO TO PR-002.
           IF CRFOREX-FBC-NUMBER < WS-RANGE1
               GO TO PR-002.
           IF CRFOREX-FBC-NUMBER > WS-RANGE2
               GO TO PR-999.
           IF CRFOREX-MATURITY-DATE < WS-CHECKDATE
               GO TO PR-002.
           IF WS-MESSAGE NOT = " "
               MOVE " " TO WS-MESSAGE
               PERFORM ERROR-020.
       PR-010.
           IF WS-LINE > 56
               PERFORM PRINT-HEADINGS.
           MOVE CRFOREX-FBC-NUMBER          TO FOREX-FBC-NUM
           MOVE CRFOREX-CURRENCY-TYPE       TO FOREX-CURRENCY
           MOVE CRFOREX-INITIAL-FOREIGN-AMT TO FOREX-FOREIGN
           MOVE CRFOREX-UNAPPLIED-AMT       TO FOREX-UNAPPLIED
           MOVE CRFOREX-EXCHANGE-RATE       TO FOREX-EXCHANGE-RATE
           MOVE CRFOREX-LOCAL-AMT           TO FOREX-LOCAL
           COMPUTE WS-LOCAL-UNAPP =
                  CRFOREX-UNAPPLIED-AMT / CRFOREX-EXCHANGE-RATE
           MOVE WS-LOCAL-UNAPP              TO FOREX-LOCAL-UNAPP
           MOVE CRFOREX-CONTRACT-DATE       TO WS-AGE-DATE SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE                TO FOREX-MATURITY-DATE
           MOVE CRFOREX-MATURITY-DATE       TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE                TO FOREX-DUE-DATE.

       PR-030.
           IF WS-LINE > 60
               PERFORM PRINT-HEADINGS.
           WRITE PRINT-REC FROM CRFOREX-LINE1 AFTER 1.
           MOVE " " TO CRFOREX-LINE1.
           ADD 1 TO WS-LINE.
           
           PERFORM ADD-TO-TOTALS.
           GO TO PR-002.
       PR-999.
           EXIT.
      *
       ADD-TO-TOTALS SECTION.
       ATT-005.
           MOVE 1 TO SUB-1.
       ATT-010.
           IF CRFOREX-CURRENCY-TYPE NOT = WS-CURRENCY-TYPE (SUB-1)
            IF WS-CURRENCY-TYPE (SUB-1) NOT = " "
             IF SUB-1 < 20
               ADD 1 TO SUB-1
               GO TO ATT-010.
           IF WS-CURRENCY-TYPE (SUB-1) = " "
             MOVE CRFOREX-CURRENCY-TYPE      TO WS-CURRENCY-TYPE (SUB-1)
             ADD CRFOREX-INITIAL-FOREIGN-AMT TO WS-FOR-AMOUNT (SUB-1)
             ADD CRFOREX-UNAPPLIED-AMT       TO WS-FOR-REMAIN (SUB-1)
             ADD CRFOREX-LOCAL-AMT           TO WS-LOC-AMOUNT (SUB-1)
             ADD WS-LOCAL-UNAPP              TO WS-LOC-REMAIN (SUB-1)
             GO TO ATT-999.
           IF CRFOREX-CURRENCY-TYPE = WS-CURRENCY-TYPE (SUB-1)
             ADD CRFOREX-INITIAL-FOREIGN-AMT TO WS-FOR-AMOUNT (SUB-1)
             ADD CRFOREX-UNAPPLIED-AMT       TO WS-FOR-REMAIN (SUB-1)
             ADD CRFOREX-LOCAL-AMT           TO WS-LOC-AMOUNT (SUB-1)
             ADD WS-LOCAL-UNAPP              TO WS-LOC-REMAIN (SUB-1).
       ATT-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           ADD 1        TO WS-PAGE
           MOVE WS-PAGE TO H-PAGE.
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
           WRITE PRINT-REC FROM HEAD3-1 AFTER 1
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE 6 TO WS-LINE.
       PH-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE  FROM DATE.
           MOVE WS-DATE      TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H-DATE.
           
           MOVE WS-CO-NAME TO CO-NAME.
       OPEN-010.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       OPEN-020.
           OPEN I-O CRFOREX-FILE.
           IF WS-HEADER-ST1 NOT = 0
               MOVE 0 TO WS-HEADER-ST1
               MOVE "CRfbc TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-020.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF WS-HEADER-ST1 = 88
            IF WS-LINE > 56
               PERFORM PRINT-HEADINGS
               MOVE "** NOTHING TO PRINT IN THAT RANGE **" TO PRINT-REC
               WRITE PRINT-REC AFTER 2
               GO TO END-500.
           MOVE 1 TO SUB-1.
       END-010.
           IF WS-CURRENCY-TYPE (SUB-1) = " "
              GO TO END-300.
           MOVE " " TO PRINT-REC
           IF SUB-1 = 1
              WRITE PRINT-REC AFTER 2
              MOVE "** CURRENCY TOTALS **" TO TOTAL-DESC
           ELSE
              MOVE "                     " TO TOTAL-DESC.
           MOVE WS-CURRENCY-TYPE (SUB-1)   TO TOTAL-CURRENCY
           MOVE WS-FOR-AMOUNT (SUB-1)      TO TOTAL-FOREIGN
           MOVE WS-FOR-REMAIN (SUB-1)      TO TOTAL-UNAPPLIED
           MOVE WS-LOC-AMOUNT (SUB-1)      TO TOTAL-LOCAL
           MOVE WS-FOR-REMAIN (SUB-1)      TO TOTAL-LOCAL-UNAPP
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           
           ADD WS-LOC-AMOUNT (SUB-1) TO WS-LOC-TOTAL
           ADD WS-LOC-REMAIN (SUB-1) TO WS-LOC-UNAPPLIED.
           IF SUB-1 < 20
              ADD 1 TO SUB-1
              GO TO END-010.
       END-300.
           MOVE " "                       TO PRINT-REC TOTAL-LINE
           MOVE "** TOTAL RAND AMOUNT **" TO TOTAL-DESC
           MOVE WS-LOC-TOTAL              TO TOTAL-LOCAL
           MOVE WS-LOC-UNAPPLIED          TO TOTAL-LOCAL-UNAPP
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
       END-450.
           IF WS-LINE > 56
               PERFORM PRINT-HEADINGS.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
           CLOSE PRINT-FILE
                 CRFOREX-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
      *     STOP RUN.
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
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
