        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrAgeTRp.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrTrans".
          Copy "SelectGlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCrTrans.
           COPY ChlfdGlParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-AMT-OF-INVOICE    PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-BALANCE     PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-CURRENT     PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-30DAY       PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-60DAY       PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-90DAY       PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-120DAY      PIC S9(8)V99 VALUE 0.
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       01  WS-CRTRANS-STATUS.
           03  WS-CRTRANS-ST1     PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE "  DATE".
           03  H-DATE           PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
           03  FILLER           PIC X(44) VALUE 
           "C R E D I T O R    A G E   A N A L Y S I S ".
           03  FILLER           PIC X(40) VALUE 
           " G R O S S   T O T A L S   O N L Y".
           03  FILLER           PIC X(8) VALUE "PERIOD:".
           03  H-PERIOD         PIC X(5).
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(7) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(19) VALUE " ".
           03  FILLER           PIC X(47) VALUE 
           "***********************************************".
           03  FILLER           PIC X(64) VALUE 
           "*******************************".
       01  HEAD4.
           03  FILLER           PIC X(38) VALUE " ".
           03  FILLER           PIC X(20) VALUE "TOTAL CREDITORS".
           03  FILLER           PIC X(15) VALUE "CURRENT".
           03  FILLER           PIC X(15) VALUE "31-60 DAY".
           03  FILLER           PIC X(15) VALUE "61-90 DAY".
           03  FILLER           PIC X(15) VALUE "91-120 DAY".
           03  FILLER           PIC X(12) VALUE "121+ DAY".
       01  GRAND-TOTAL-LINE.
           03  FILLER           PIC X(23) VALUE " ".
           03  FILLER           PIC X(17) VALUE "GROSS TOTALS : R".
           03  GR-TOT-BALANCE   PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  GR-TOT-CURRENT   PIC Z(7)9.99-.
           03  FILLER           PIC X(5) VALUE " ".
           03  GR-TOT-30DAY     PIC Z(7)9.99-.
           03  FILLER           PIC X(3) VALUE " ".
           03  GR-TOT-60DAY     PIC Z(7)9.99-.
           03  FILLER           PIC X(4) VALUE " ".
           03  GR-TOT-90DAY     PIC Z(7)9.99-.
           03  FILLER           PIC X(1) VALUE " ".
           03  GR-TOT-120DAY    PIC Z(7)9.99-.
           03  FILLER           PIC X(5) VALUE " ".
       01  WS-TEMP-LINE.
           03  FILLER           PIC X(10) VALUE " ".
           03  T-TRANS-DATE     PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  T-TRANS-DUEDATE  PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 320 TO POS
           DISPLAY "Creditors Age Analysis : Gross Totals !" AT POS
           MOVE 420 TO POS
           DISPLAY "***************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAcceptCr".
        CONTROL-009.
           MOVE 1010 TO POS.
           DISPLAY "This Report Will Print Only The Gross Totals"
               AT POS.
           MOVE 1110 TO POS.
           DISPLAY "Of All The Accounts On File."
               AT POS.
           MOVE 1210 TO POS.
           DISPLAY "Be Patient While The Report Is Being Compiled."
               AT POS.
           PERFORM ERROR-020.
       CONTROL-010.
           OPEN I-O CRTR-FILE.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CR-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CONTROL-010.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-012.

           PERFORM READ-GLPARAMETER.
           MOVE GLPA-NAME TO CO-NAME.
           PERFORM ENTER-PERIOD-DATES.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       CONTROL-030.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H-DATE.

           MOVE " " TO CRTR-KEY.
           START CRTR-FILE KEY NOT < CRTR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CR-TRANS FILE BUSY ON START, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CONTROL-010.
           PERFORM PRINT-ROUTINE.
           PERFORM GRAND-TOTALS.
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           PERFORM PRINT-HEADINGS.
           READ CRTR-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 10
               GO TO PR-999.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CR-TRANS BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PR-000.
           IF WS-MESSAGE NOT = " "
               MOVE " " TO WS-MESSAGE
               PERFORM ERROR-020.
       PR-020.
           MOVE CRTR-DATE            TO WS-AGE-DATE.
           MOVE CRTR-UNAPPLIED-AMT TO WS-AMT-OF-INVOICE.
           IF WS-AMT-OF-INVOICE = 0
               GO TO PR-040.
           PERFORM COMPUTE-DATE-PERIOD.
           IF WS-CALC-PERIOD = 0
               ADD WS-AMT-OF-INVOICE TO WS-GRTOT-CURRENT
                                        WS-GRTOT-BALANCE
               GO TO PR-040.
           IF WS-CALC-PERIOD = 1
               ADD WS-AMT-OF-INVOICE TO WS-GRTOT-30DAY
                                        WS-GRTOT-BALANCE
               GO TO PR-040.
           IF WS-CALC-PERIOD = 2
               ADD WS-AMT-OF-INVOICE TO WS-GRTOT-60DAY
                                        WS-GRTOT-BALANCE
               GO TO PR-040.
           IF WS-CALC-PERIOD = 3
               ADD WS-AMT-OF-INVOICE TO WS-GRTOT-90DAY
                                        WS-GRTOT-BALANCE
               GO TO PR-040.
           IF WS-CALC-PERIOD > 3
               ADD WS-AMT-OF-INVOICE TO WS-GRTOT-120DAY
                                        WS-GRTOT-BALANCE.
       PR-040.
           READ CRTR-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 10
               GO TO PR-999.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE 
          "CR-TRANS BUSY ON READ-NEXT, IN 1 SECOND GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               GO TO PR-040.
           IF WS-MESSAGE NOT = " "
               MOVE " " TO WS-MESSAGE
               PERFORM ERROR-020.
           GO TO PR-020.
       PR-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           ADD 1        TO WS-PAGE
           MOVE WS-PAGE TO H-PAGE
           MOVE 6       TO WS-LINE
           MOVE GLPA-CURRENT-CRPER TO H-PERIOD
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
           WRITE PRINT-REC FROM HEAD4 AFTER 2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
       PH-999.
           EXIT.
      *
       GRAND-TOTALS SECTION.
       GT-000.
           MOVE WS-GRTOT-BALANCE TO GR-TOT-BALANCE
           MOVE WS-GRTOT-CURRENT TO GR-TOT-CURRENT
           MOVE WS-GRTOT-30DAY   TO GR-TOT-30DAY
           MOVE WS-GRTOT-60DAY   TO GR-TOT-60DAY
           MOVE WS-GRTOT-90DAY   TO GR-TOT-90DAY
           MOVE WS-GRTOT-120DAY  TO GR-TOT-120DAY
           WRITE PRINT-REC FROM GRAND-TOTAL-LINE AFTER 1.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       GT-999.
           EXIT.
      *
       READ-GLPARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RP-010.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER BUSY ON READ, RP-010, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RP-010.
       RP-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CRTR-FILE
                 GLPARAMETER-FILE.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           EXIT PROGRAM.
        END-999.
           EXIT.
      *
       Copy "ComputeCRDatePeriod".
       Copy "EnterPeriodDates".
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
      * END-OF-JOB
