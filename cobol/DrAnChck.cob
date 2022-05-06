        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrAnChck.
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
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-MARGIN            PIC S9(7)V99 VALUE 0.
       77  WS-PERC              PIC S9(4)V99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(14) VALUE " ".
           03  FILLER         PIC X(89) VALUE
           "DEBTORS SALES ANALYSIS CHECKING WITH VAT-NUMBER".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(6) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(47) VALUE ALL "*".
           03  FILLER         PIC X(56) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(9) VALUE "ACCOUNT".
           03  FILLER         PIC X(30) VALUE "NAME".
           03  FILLER         PIC X(7) VALUE "CODE".
           03  FILLER         PIC X(86) VALUE "VAT-CODE".
       01  DETAIL-LINE.
           03  D-ACCOUNT  PIC X(9) VALUE " ".
           03  D-NAME     PIC X(30) VALUE " ".
           03  D-CODE     PIC Z9.
           03  FILLER     PIC X(5) VALUE " ".
           03  D-VATNO    PIC X(86) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONT-000.
           PERFORM CLEAR-SCREEN.
       CONT-003.
           Copy "PrinterAcceptDr".
       CONT-010.
           MOVE 310 TO POS
           DISPLAY
           "* DEBTORS SALES ANALYSIS CHECKING WITH VAT-NUMBER REPORT *"
              AT POS
           MOVE 410 TO POS
           DISPLAY
           "**********************************************************"
              AT POS.
           MOVE 2510 TO POS
           DISPLAY "PRESS ANY KEY TO CONTINUE OR 'END' TO EXIT" AT POS
           MOVE " "  TO CDA-DATA
           MOVE 23   TO CDA-ROW
           MOVE 55   TO CDA-COL
           MOVE 1    TO CDA-DATALEN
           MOVE "A"  TO CDA-ATTR
           MOVE 3    TO CDA-COLOR
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.
            IF W-ESCAPE-KEY = 3
              PERFORM END-900.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
              GO TO CONT-015
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO CONT-010.
        CONT-015.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE 2810 TO POS.
           DISPLAY "Report Is Being Compiled, Please Be Patient."
              AT POS.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
       CONT-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE SPACES TO WS-MESSAGE
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS.
           
           MOVE " " TO DR-KEY
           START DEBTOR-MASTER KEY NOT < DR-KEY
                 INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE 0 TO WS-DEBTOR-ST1
              MOVE "DEBTOR RECORD BUSY ON START, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PRR-000.
       PRR-005.
           READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
      *        MOVE "DEBTOR FILE AT END, 'ESC' TO EXIT."
      *        TO WS-MESSAGE
      *        PERFORM ERROR-MESSAGE
              GO TO PRR-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               CLOSE DEBTOR-MASTER
               OPEN I-O DEBTOR-MASTER
               GO TO PRR-005.

           MOVE 2510 TO POS
           DISPLAY "Debtor Account Being Read:" AT POS
           ADD 27 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.

           IF DR-SALES-ANALYSIS = 6 OR = 52
            IF DR-GSTNO NOT = "EXPORT       "
              GO TO PRR-010.
           IF DR-GSTNO = "EXPORT       "
            IF DR-SALES-ANALYSIS NOT = 6 AND NOT = 52
              GO TO PRR-010.
           GO TO PRR-005.
       PRR-010.
           IF LINE-CNT < 60
               GO TO PRR-020.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.
           IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD3 AFTER 2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE 6 TO LINE-CNT.
       PRR-020.
           MOVE DR-ACCOUNT-NUMBER TO D-ACCOUNT
           MOVE DR-NAME           TO D-NAME
           MOVE DR-SALES-ANALYSIS TO D-CODE
           MOVE DR-GSTNO          TO D-VATNO
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
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
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-020.
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
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           WRITE PRINT-REC
           MOVE "*** END OF PRINT FILE ****" TO PRINT-REC
           WRITE PRINT-REC.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE DEBTOR-MASTER.
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
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
