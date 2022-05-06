        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlDistRp.
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
           
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       01  WS-DISTRIBUTION-STATUS.
           03  WS-DISTRIBUTION-ST1 PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(6) VALUE " ".
           03  FILLER         PIC X(26) VALUE "S A L E S   L E D G E R".
           03  FILLER         PIC X(15) VALUE "D I S T R I B U".
           03  FILLER         PIC X(17) VALUE " T I O N   T O T ".
           03  FILLER         PIC X(19) VALUE "A L S   R E P O R T".
           03  FILLER         PIC X(9) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC 9.
           03  FILLER         PIC X(16) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(23) VALUE " ".
           03  FILLER         PIC X(77) VALUE ALL "*".
           03  FILLER         PIC X(32) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(47) VALUE " ".
           03  FILLER         PIC X(13) VALUE "WEEK TO".
           03  FILLER         PIC X(15) VALUE "MONTH TO".
           03  FILLER         PIC X(57) VALUE "YEAR TO ".
       01  HEAD4.
           03  FILLER         PIC X(50) VALUE " ".
           03  FILLER         PIC X(14) VALUE "DATE".
           03  FILLER         PIC X(65) VALUE "DATE          DATE".
       01  DETAIL-LINE.
           03  FILLER         PIC X(20) VALUE " ".
           03  D-NAME         PIC X(23) VALUE " ".
           03  D-AMTWEEK      PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-AMTPTD       PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-AMTYTD       PIC Z(7)9.99-.
           03  FILLER         PIC X(49) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 322 TO POS
           DISPLAY "** SALES LEDGER DISTRIBUTION TOTALS REPORT **"
           AT POS
           MOVE 422 TO POS
           DISPLAY "*********************************************"
           AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-015.
           MOVE 2510 TO POS
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
           AT POS
           PERFORM OPEN-FILES.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE
           PERFORM PRINT-ROUTINE
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PRR-006.
           MOVE "1" TO DIST-KEY.
           START DISTRIBUTIONS KEY NOT < DIST-KEY
              INVALID KEY NEXT SENTENCE.
       PRR-008.
           READ DISTRIBUTIONS
               INVALID KEY NEXT SENTENCE.
           IF WS-DISTRIBUTION-ST1 NOT = 0
                MOVE "DISTRIBUTIONS BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DISTRIBUTION-ST1
                GO TO PRR-999.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-015.
            ADD 1 TO PAGE-CNT

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC.

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
            WRITE PRINT-REC FROM HEAD4 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC AFTER 1
            MOVE 8 TO LINE-CNT.
       PRR-015.
           MOVE "INVOICES           :" TO D-NAME
           MOVE DIST-INVOICEWEEK       TO D-AMTWEEK
           MOVE DIST-INVOICEPTD        TO D-AMTPTD
           MOVE DIST-INVOICEYTD        TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.

           MOVE "PAYMENTS           :" TO D-NAME
           MOVE DIST-PAYMENTWEEK       TO D-AMTWEEK
           MOVE DIST-PAYMENTPTD        TO D-AMTPTD
           MOVE DIST-PAYMENTYTD        TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.

           MOVE "R/D CHEQUES        :" TO D-NAME
           MOVE DIST-RDCHEQUEWEEK      TO D-AMTWEEK
           MOVE DIST-RDCHEQUEPTD       TO D-AMTPTD
           MOVE DIST-RDCHEQUEYTD       TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.

           MOVE "JOURNAL DEBIT      :" TO D-NAME
           MOVE DIST-JOURNALDRWEEK     TO D-AMTWEEK
           MOVE DIST-JOURNALDRPTD      TO D-AMTPTD
           MOVE DIST-JOURNALDRYTD      TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.

           MOVE "JOURNAL CREDIT     :" TO D-NAME
           MOVE DIST-JOURNALCRWEEK     TO D-AMTWEEK
           MOVE DIST-JOURNALCRPTD      TO D-AMTPTD
           MOVE DIST-JOURNALCRYTD      TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.

           MOVE "CREDIT NOTES       :" TO D-NAME
           MOVE DIST-CNOTEWEEK         TO D-AMTWEEK
           MOVE DIST-CNOTEPTD          TO D-AMTPTD
           MOVE DIST-CNOTEYTD          TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.

           MOVE "INTEREST           :" TO D-NAME
           MOVE DIST-INTERESTWEEK      TO D-AMTWEEK
           MOVE DIST-INTERESTPTD       TO D-AMTPTD
           MOVE DIST-INTERESTYTD       TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.

           MOVE "DISCOUNT           :" TO D-NAME
           MOVE DIST-DISCOUNTWEEK      TO D-AMTWEEK
           MOVE DIST-DISCOUNTPTD       TO D-AMTPTD
           MOVE DIST-DISCOUNTYTD       TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.

           MOVE "ADD-ONS            :" TO D-NAME
           MOVE DIST-ADDONWEEK         TO D-AMTWEEK
           MOVE DIST-ADDONPTD          TO D-AMTPTD
           MOVE DIST-ADDONYTD          TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.

           MOVE "BAD-DEBTS          :" TO D-NAME
           MOVE DIST-BDEBTWEEK         TO D-AMTWEEK
           MOVE DIST-BDEBTPTD          TO D-AMTPTD
           MOVE DIST-BDEBTYTD          TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.

           MOVE "ACCOUNTS RECEIVED  :" TO D-NAME
           MOVE DIST-ACCRECWEEK        TO D-AMTWEEK
           MOVE DIST-ACCRECPTD         TO D-AMTPTD
           MOVE DIST-ACCRECYTD         TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.
       PRR-020.
           MOVE "VAT COLLECTED      :" TO D-NAME
           MOVE GST-AMT-TAXED-WEEK     TO D-AMTWEEK
           MOVE GST-AMT-TAXED-PTD      TO D-AMTPTD
           MOVE GST-AMT-TAXED-YTD      TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.

           MOVE "TAXABLE SALES      :" TO D-NAME
           MOVE GST-AMT-TAXABLE-WEEK   TO D-AMTWEEK
           MOVE GST-AMT-TAXABLE-PTD    TO D-AMTPTD
           MOVE GST-AMT-TAXABLE-YTD    TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.

           MOVE "NON TAXABLE SALES  :"  TO D-NAME
           MOVE GST-AMT-NONTAXABLE-WEEK TO D-AMTWEEK
           MOVE GST-AMT-NONTAXABLE-PTD  TO D-AMTPTD
           MOVE GST-AMT-NONTAXABLE-YTD  TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.

           MOVE "EXPORT SALES       :" TO D-NAME
           MOVE GST-AMT-EXPORT-WEEK    TO D-AMTWEEK
           MOVE GST-AMT-EXPORT-PTD     TO D-AMTPTD
           MOVE GST-AMT-EXPORT-YTD     TO D-AMTYTD
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-010.
           OPEN I-O DISTRIBUTIONS.
           IF WS-DISTRIBUTION-ST1 NOT = 0
               MOVE "DISTRIBUTION FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DISTRIBUTION-ST1
               GO TO OPEN-010.
           MOVE Ws-Co-Name To CO-NAME.

           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
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
