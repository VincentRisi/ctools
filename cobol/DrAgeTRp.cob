        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrAgeTRp.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrTrans".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       COPY ChlfdDebtor.
       COPY ChlfdDrTrans.
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
       77  WS-BRANCH-BALANCE    PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-CURRENT    PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-30DAY      PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-60DAY      PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-90DAY      PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-120DAY     PIC S9(8)V99 VALUE 0.
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1   PIC 99.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE "  DATE".
           03  H-DATE           PIC X(10) VALUE " ".
           03  FILLER           PIC X(2) VALUE " ".
           03  FILLER           PIC X(45) VALUE 
           "O P E N   I T E M   A G E   A N A L Y S I S ".
           03  FILLER           PIC X(53) VALUE 
           "  G R O S S   T O T A L S   O N L Y".
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(7) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(19) VALUE " ".
           03  FILLER           PIC X(47) VALUE 
           "***********************************************".
           03  FILLER           PIC X(66) VALUE 
           "*********************************".
       01  HEAD4.
          03  FILLER           PIC X(38) VALUE " ".
          03  FILLER           PIC X(20) VALUE "TOTAL DEBTORS".
          03  FILLER           PIC X(15) VALUE "CURRENT".
          03  FILLER           PIC X(15) VALUE "31-60 DAY".
          03  FILLER           PIC X(15) VALUE "61-90 DAY".
          03  FILLER           PIC X(15) VALUE "91-120 DAY".
          03  FILLER           PIC X(12) VALUE "121+ DAY".
       01  GRAND-TOTAL-LINE.
           03  FILLER           PIC X(23) VALUE " ".
           03  GR-TOT-NAME      PIC X(17) VALUE " ".
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
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONT-PARAGRAPH SECTION.
       CONT-000.
           PERFORM CLEAR-SCREEN
           MOVE 320 TO POS
           DISPLAY "Open Item Age Analysis : Gross Totals !" AT POS
           MOVE 420 TO POS
           DISPLAY "***************************************" AT POS.
       CONT-003.
           Copy "PrinterAcceptDr".
       CONT-009.
           MOVE 1010 TO POS.
           DISPLAY "This Report Will Print Only The Gross Totals"
               AT POS.
           MOVE 1110 TO POS.
           DISPLAY "Of All The Accounts On File."
               AT POS.
           MOVE 1210 TO POS.
           DISPLAY "Be Patient While The Report Is Being Compiled.."
               AT POS.
           PERFORM ERROR-020.
       CONT-010.
           PERFORM OPEN-FILES
           MOVE Ws-Co-Name TO CO-NAME.
       CONT-030.
           PERFORM PRINT-ROUTINE
           PERFORM GRAND-TOTALS
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           MOVE " " TO DRTR-ACC-KEY
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE 88 TO WS-DRTRANS-ST1.

           PERFORM PRINT-HEADINGS.
       PR-010.
           READ DEBTOR-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10 OR = 88
               GO TO PR-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DRTRANS BUSY ON READ-PRN, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO PR-010.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       PR-020.
           MOVE 2510 TO POS
           DISPLAY "Account Number Being Read:" AT POS
           ADD 27 TO POS
           DISPLAY DRTR-ACCOUNT-NUMBER AT POS.

           MOVE DRTR-DATE  TO WS-AGE-DATE.
           MOVE SPLIT-DATE TO WS-DATE.
           IF WS-AGE-YY < WS-YY
               COMPUTE WS-MM = (((WS-YY - WS-AGE-YY) * 12)
                                   + WS-MM).
           SUBTRACT WS-AGE-MM FROM WS-MM.
           MOVE DRTR-AMT-OUTSTANDING TO WS-AMT-OF-INVOICE.
           IF WS-AMT-OF-INVOICE = 0
               GO TO PR-040.
           IF DRTR-TYPE = 2 OR = 5 OR = 6 OR = 8 OR = 9 OR = 11
               COMPUTE WS-AMT-OF-INVOICE = 
                (WS-AMT-OF-INVOICE - (WS-AMT-OF-INVOICE * 2)).
           IF WS-MM = 0
               ADD WS-AMT-OF-INVOICE TO WS-GRTOT-CURRENT
                                        WS-GRTOT-BALANCE
               GO TO PR-040.
           IF WS-MM = 1
               ADD WS-AMT-OF-INVOICE TO WS-GRTOT-30DAY
                                        WS-GRTOT-BALANCE
               GO TO PR-040.
           IF WS-MM = 2
               ADD WS-AMT-OF-INVOICE TO WS-GRTOT-60DAY
                                        WS-GRTOT-BALANCE
               GO TO PR-040.
           IF WS-MM = 3
               ADD WS-AMT-OF-INVOICE TO WS-GRTOT-90DAY
                                        WS-GRTOT-BALANCE
               GO TO PR-040.
           IF WS-MM > 3
               ADD WS-AMT-OF-INVOICE TO WS-GRTOT-120DAY
                                        WS-GRTOT-BALANCE.
       PR-040.
           IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               PERFORM READ-DEBTOR.
           IF DR-SALES-ANALYSIS NOT = 53 AND NOT = 57
               GO TO PR-050.
           IF WS-MM = 0
               ADD WS-AMT-OF-INVOICE TO WS-BRANCH-CURRENT
                                        WS-BRANCH-BALANCE
               GO TO PR-050.
           IF WS-MM = 1
               ADD WS-AMT-OF-INVOICE TO WS-BRANCH-30DAY
                                        WS-BRANCH-BALANCE
               GO TO PR-050.
           IF WS-MM = 2
               ADD WS-AMT-OF-INVOICE TO WS-BRANCH-60DAY
                                        WS-BRANCH-BALANCE
               GO TO PR-050.
           IF WS-MM = 3
               ADD WS-AMT-OF-INVOICE TO WS-BRANCH-90DAY
                                        WS-BRANCH-BALANCE
               GO TO PR-050.
           IF WS-MM > 3
               ADD WS-AMT-OF-INVOICE TO WS-BRANCH-120DAY
                                        WS-BRANCH-BALANCE.
       PR-050.
           GO TO PR-010.
       PR-999.
           EXIT.
      *
       READ-DEBTOR SECTION.
       RD-005.
           MOVE DRTR-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY
              INVALID KEY NEXT SENTENCE.
       RD-010.
           READ DEBTOR-MASTER 
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
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
       PRINT-HEADINGS SECTION.
       PH-000.
           ADD 1        TO WS-PAGE
           MOVE WS-PAGE TO H-PAGE
           MOVE 6       TO WS-LINE.
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
           MOVE "GROSS TOTALS  : R" TO GR-TOT-NAME
           MOVE WS-GRTOT-BALANCE    TO GR-TOT-BALANCE
           MOVE WS-GRTOT-CURRENT    TO GR-TOT-CURRENT
           MOVE WS-GRTOT-30DAY      TO GR-TOT-30DAY
           MOVE WS-GRTOT-60DAY      TO GR-TOT-60DAY
           MOVE WS-GRTOT-90DAY      TO GR-TOT-90DAY
           MOVE WS-GRTOT-120DAY     TO GR-TOT-120DAY
           WRITE PRINT-REC FROM GRAND-TOTAL-LINE AFTER 1.
           
           MOVE "BRANCH TOTALS : R" TO GR-TOT-NAME
           MOVE WS-BRANCH-BALANCE   TO GR-TOT-BALANCE
           MOVE WS-BRANCH-CURRENT   TO GR-TOT-CURRENT
           MOVE WS-BRANCH-30DAY     TO GR-TOT-30DAY
           MOVE WS-BRANCH-60DAY     TO GR-TOT-60DAY
           MOVE WS-BRANCH-90DAY     TO GR-TOT-90DAY
           MOVE WS-BRANCH-120DAY    TO GR-TOT-120DAY
           WRITE PRINT-REC FROM GRAND-TOTAL-LINE AFTER 1.
           
           MOVE "GROSS - BRANCH: R" TO GR-TOT-NAME
           COMPUTE WS-GRTOT-BALANCE =
               WS-GRTOT-BALANCE - WS-BRANCH-BALANCE
           MOVE WS-GRTOT-BALANCE    TO GR-TOT-BALANCE
           COMPUTE WS-GRTOT-CURRENT =
               WS-GRTOT-CURRENT - WS-BRANCH-CURRENT
           MOVE WS-GRTOT-CURRENT    TO GR-TOT-CURRENT
           COMPUTE WS-GRTOT-30DAY =
               WS-GRTOT-30DAY - WS-BRANCH-30DAY
           MOVE WS-GRTOT-30DAY      TO GR-TOT-30DAY
           COMPUTE WS-GRTOT-60DAY =
               WS-GRTOT-60DAY - WS-BRANCH-60DAY
           MOVE WS-GRTOT-60DAY      TO GR-TOT-60DAY
           COMPUTE WS-GRTOT-90DAY =
               WS-GRTOT-90DAY - WS-BRANCH-90DAY
           MOVE WS-GRTOT-90DAY      TO GR-TOT-90DAY
           COMPUTE WS-GRTOT-120DAY =
               WS-GRTOT-120DAY - WS-BRANCH-120DAY
           MOVE WS-GRTOT-120DAY     TO GR-TOT-120DAY
           WRITE PRINT-REC FROM GRAND-TOTAL-LINE AFTER 2.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       GT-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H-DATE.
       OPEN-010.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-020.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE 0 TO WS-DEBTOR-ST1
              MOVE "DR MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-020.
           PERFORM ERROR-020.
       OPEN-030.
           OPEN I-O DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-DRTRANS-ST1
               MOVE "DR-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-030.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE DEBTOR-TRANS-FILE
                 DEBTOR-MASTER.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           EXIT PROGRAM.
       END-999.
            EXIT.
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
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
