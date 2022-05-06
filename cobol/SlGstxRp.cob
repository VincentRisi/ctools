        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlGstxRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectSlDistributions".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDisTot.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER          PIC X(10) VALUE " ".
       77  LINE-CNT                PIC 9(3) VALUE 66.
       77  PAGE-CNT                PIC 9(3) VALUE 0.
       77  WS-TOTALS               PIC S9(9)V99 VALUE 0.
       01  WS-DISTRIBUTION-STATUS.
           03  WS-DISTRIBUTION-ST1 PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(10) VALUE " ".
           03  FILLER         PIC X(12) VALUE "V A L U E".
           03  FILLER         PIC X(15) VALUE "A D D E D   T A".
           03  FILLER         PIC X(17) VALUE " X   A N A L Y S ".
           03  FILLER         PIC X(17) VALUE "I S   R E P O R T".
           03  FILLER         PIC X(21) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC 9.
           03  FILLER         PIC X(16) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(27) VALUE " ".
           03  FILLER         PIC X(61) VALUE ALL "*".
           03  FILLER         PIC X(44) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(47) VALUE " ".
           03  FILLER         PIC X(14) VALUE "WEEK TO".
           03  FILLER         PIC X(16) VALUE "MONTH TO".
           03  FILLER         PIC X(55) VALUE "YEAR TO ".
       01  HEAD4.
           03  FILLER         PIC X(50) VALUE " ".
           03  FILLER         PIC X(15) VALUE "DATE".
           03  FILLER         PIC X(67) VALUE "DATE           DATE".
       01  DETAIL-LINE.
           03  FILLER         PIC X(20) VALUE " ".
           03  D-NAME         PIC X(22) VALUE " ".
           03  D-AMTWEEK      PIC Z(8)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-AMTPTD       PIC Z(8)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-AMTYTD       PIC Z(8)9.99-.
           03  FILLER         PIC X(48) VALUE " ".
       01  UNDER-LINE.
           03  FILLER         PIC X(42) VALUE " ".
           03  FILLER         PIC X(90) VALUE
               "------------   ------------   ------------".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 322 TO POS
           DISPLAY "** VALUE ADDED TAX ANALYSIS REPORT **" AT POS
           MOVE 422 TO POS
           DISPLAY "*************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-020.
           MOVE 2510 TO POS
           DISPLAY "Report Is Being Compiled, Please Be Patient....."
               AT POS.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE
           PERFORM OPEN-FILES
           PERFORM PRINT-ROUTINE
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE "1" TO DIST-KEY.
           START DISTRIBUTIONS KEY NOT < DIST-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-DISTRIBUTION-ST1 NOT = 0
              MOVE "DIST-VAT RECORD BUSY ON START, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DISTRIBUTION-ST1
              GO TO PRR-000.
       PRR-005.
           READ DISTRIBUTIONS
               INVALID KEY NEXT SENTENCE.
           IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
              MOVE "DISTRIBUTIONS RECORD NOT FOUND, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DISTRIBUTION-ST1
              EXIT PROGRAM.
           IF WS-DISTRIBUTION-ST1 NOT = 0
              MOVE "DIST-VAT RECORD BUSY ON READ, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DISTRIBUTION-ST1
              GO TO PRR-005.
       PRR-010.
           IF LINE-CNT < 60
              GO TO PRR-020.
           ADD 1 TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC.

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
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE 7 TO LINE-CNT.
       PRR-020.
           MOVE "VAT COLLECTED      :" TO D-NAME
           MOVE GST-AMT-TAXED-WEEK     TO D-AMTWEEK
           MOVE GST-AMT-TAXED-PTD      TO D-AMTPTD
           MOVE GST-AMT-TAXED-YTD      TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC
           WRITE PRINT-REC.

           MOVE "TAXABLE SALES      :" TO D-NAME
           MOVE GST-AMT-TAXABLE-WEEK   TO D-AMTWEEK
           MOVE GST-AMT-TAXABLE-PTD    TO D-AMTPTD
           MOVE GST-AMT-TAXABLE-YTD    TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
                       DETAIL-LINE

           MOVE "NON TAXABLE SALES  :"  TO D-NAME
           MOVE GST-AMT-NONTAXABLE-WEEK TO D-AMTWEEK
           MOVE GST-AMT-NONTAXABLE-PTD  TO D-AMTPTD
           MOVE GST-AMT-NONTAXABLE-YTD  TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.

           MOVE "EXPORT SALES       :" TO D-NAME
           MOVE GST-AMT-EXPORT-WEEK    TO D-AMTWEEK
           MOVE GST-AMT-EXPORT-PTD     TO D-AMTPTD
           MOVE GST-AMT-EXPORT-YTD     TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.

           WRITE PRINT-REC FROM UNDER-LINE
           MOVE " " TO PRINT-REC
           
           MOVE "TOTAL SALES        :" TO D-NAME
           COMPUTE WS-TOTALS = (GST-AMT-TAXABLE-WEEK +
                 GST-AMT-NONTAXABLE-WEEK + GST-AMT-EXPORT-WEEK)
           MOVE WS-TOTALS           TO D-AMTWEEK

           COMPUTE WS-TOTALS = (GST-AMT-TAXABLE-PTD +
                 GST-AMT-NONTAXABLE-PTD + GST-AMT-EXPORT-PTD)
           MOVE WS-TOTALS           TO D-AMTPTD

           COMPUTE WS-TOTALS = (GST-AMT-TAXABLE-YTD +
                 GST-AMT-NONTAXABLE-YTD + GST-AMT-EXPORT-YTD)
           MOVE WS-TOTALS           TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
                       DETAIL-LINE

           WRITE PRINT-REC FROM UNDER-LINE
           MOVE " " TO PRINT-REC.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-010.
           OPEN I-O DISTRIBUTIONS.
           IF WS-DISTRIBUTION-ST1 NOT = 0
               MOVE 0 TO WS-DISTRIBUTION-ST1
               MOVE "DISTRIBUTION FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-010.
       OPEN-015.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE
                 DISTRIBUTIONS.
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
