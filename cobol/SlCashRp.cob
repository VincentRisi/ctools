        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlCashRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectCoCashSales".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCashSale.
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-PRINTED           PIC X VALUE " ".
       77  WS-RANGE             PIC X VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-999               PIC S9(8)V99 VALUE 0.
       77  WS-03150             PIC S9(8)V99 VALUE 0.
       77  WS-03200             PIC S9(8)V99 VALUE 0.
       77  WS-OTHER             PIC S9(8)V99 VALUE 0.
       77  WS-SALE-AMT          PIC S9(8)V99 VALUE 0.
       01  WS-CASHSALE-STATUS.
           03  WS-CASHSALE-ST1  PIC 99.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE "  DATE".
           03  H1-DATE          PIC X(10).
           03  FILLER           PIC X(3) VALUE " ".
           03  FILLER           PIC X(45) VALUE 
           "C A S H S A L E S    B Y    I N V O I C E".
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC Z9.
           03  FILLER           PIC X(14) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(20) VALUE " ".
           03  FILLER           PIC X(93) VALUE 
           "*****************************************".
       01  HEAD3.
           03  FILLER           PIC X(9) VALUE "Invoice".
           03  FILLER           PIC X(21) VALUE "Acc No      Soldby".
           03  FILLER           PIC X(13) VALUE  "Inv Date".
           03  FILLER           PIC X(14) VALUE "Paid Amt".
       01  DETAIL-LINE.
           03  P-INVOICE        PIC Z(6).
           03  FILLER           PIC X(2) VALUE " ".
           03  P-ACCOUNT        PIC X(15) VALUE " ".
           03  P-SB             PIC X(6).
           03  P-DATE           PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  P-SALE           PIC Z(7)9.99-.
       01  P-UNDERLINE.
           03  FILLER           PIC X(40) VALUE " ".
           03  FILLER           PIC X(11) VALUE "-----------".
       01  P-DOUBLELINE.
           03  FILLER           PIC X(40) VALUE " ".
           03  FILLER           PIC X(11) VALUE "===========".
       01  TOT-TOTAL-LINE.
           03  FILLER           PIC X(13) VALUE " ".
           03  TOT-DESC         PIC X(18) VALUE "Gross Cash".
           03  FILLER           PIC X(9) VALUE "Totals :R".
           03  P-TOT-SALE       PIC Z(7)9.99-.
       Copy "WsDateInfo".
      *
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 320 TO POS
           DISPLAY "***** CashSale By Invoice Report *****" AT POS
           MOVE 420 TO POS
           DISPLAY "**************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1510 TO POS.
           DISPLAY "Do You wish to DELETE the trans after printing: [ ]"
           AT POS.
           MOVE 1559 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 58        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

      *     ACCEPT WS-ANSWER AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF WS-ANSWER NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-050.
           MOVE 2510 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
           AT POS.
       CONTROL-070.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           MOVE "N" TO WS-PRINTED.
           MOVE 0 TO CS-INVOICE.
           START CASH-SALE KEY NOT < CS-KEY
             INVALID KEY NEXT SENTENCE.
           IF WS-CASHSALE-ST1 NOT = 0
               MOVE "NO CASHSALE ON START TO PRINT, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 88 TO WS-CASHSALE-ST1
               GO TO PR-999.
       PR-001.
           READ CASH-SALE NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-CASHSALE-ST1 = 10
               PERFORM PRINT-TOTALS
               GO TO PR-999.
           IF WS-CASHSALE-ST1 NOT = 0
               MOVE "CASH SALES BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CASHSALE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CASHSALE-ST1
               GO TO PR-001.
       PR-010.
           IF LINE-CNT > 60
               PERFORM PRINT-HEADINGS.
       PR-030.
           MOVE CS-INVOICE         TO P-INVOICE
           MOVE CS-INITIAL         TO P-SB
           MOVE CS-ACCOUNT         TO P-ACCOUNT
           MOVE CS-INV-DATE        TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE       TO P-DATE
           MOVE CS-SALE-AMOUNT     TO P-SALE
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.

           IF WS-PRINTED NOT = "Y"
              MOVE "Y" TO WS-PRINTED.
           
           ADD 1 TO LINE-CNT
           ADD CS-SALE-AMOUNT TO WS-SALE-AMT.
           IF CS-ACCOUNT = 9999999
              ADD CS-SALE-AMOUNT TO WS-999.
           IF CS-ACCOUNT = 0300150
              ADD CS-SALE-AMOUNT TO WS-03150.
           IF CS-ACCOUNT = 0300200
              ADD CS-SALE-AMOUNT TO WS-03200.
           
           IF WS-ANSWER = "Y"
              GO TO PR-900.
           GO TO PR-001.
       PR-900.
           DELETE CASH-SALE
               INVALID KEY NEXT SENTENCE.
           IF WS-CASHSALE-ST1 = 23 OR 35 OR 49
               MOVE "CASHSALE RECORD NOT DELETED, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PR-001.
           IF WS-CASHSALE-ST1 NOT = 0
               MOVE "SOLD BY FILE BUSY ON READ-DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CASHSALE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CASHSALE-ST1
               GO TO PR-900.
           GO TO PR-001.
       PR-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H-PAGE
           MOVE " "      TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC AFTER 1.
           IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE 4 TO LINE-CNT.       
        PH-999.
           EXIT.
      *
       PRINT-TOTALS SECTION.
       PT-010.
           WRITE PRINT-REC FROM P-UNDERLINE AFTER 1
           ADD 1                 TO LINE-CNT
           MOVE WS-SALE-AMT      TO P-TOT-SALE
           WRITE PRINT-REC FROM   TOT-TOTAL-LINE AFTER 1
           WRITE PRINT-REC FROM     P-DOUBLELINE AFTER 1
           ADD 1                 TO LINE-CNT
           
           MOVE "99/99 ACCOUNT " TO TOT-DESC
           MOVE WS-999           TO P-TOT-SALE
           WRITE PRINT-REC FROM TOT-TOTAL-LINE AFTER 2
           MOVE "03/150 ACCOUNT" TO TOT-DESC
           MOVE WS-03150         TO P-TOT-SALE
           WRITE PRINT-REC FROM TOT-TOTAL-LINE AFTER 1
           MOVE "03/200 ACCOUNT" TO TOT-DESC
           MOVE WS-03200         TO P-TOT-SALE
           WRITE PRINT-REC FROM TOT-TOTAL-LINE AFTER 1

           COMPUTE WS-OTHER = WS-SALE-AMT - WS-999 - WS-03150 - WS-03200
           MOVE "OTHER ACCOUNTS" TO TOT-DESC
           MOVE WS-OTHER         TO P-TOT-SALE
           WRITE PRINT-REC FROM TOT-TOTAL-LINE AFTER 1.
       PT-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           MOVE 2910 TO POS
           DISPLAY "Opening Files......" AT POS.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-060.
           OPEN I-O CASH-SALE.
           IF WS-CASHSALE-ST1 = 23 OR 35 OR 49
               MOVE "NO CASH SALE FILE TO OPEN, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-065.
           IF WS-CASHSALE-ST1 NOT = 0
               MOVE "CASH SALE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CASHSALE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-COCASHSALE TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO OPEN-060.
            GO TO OPEN-070.
        OPEN-065.
           OPEN OUTPUT CASH-SALE.
           CLOSE CASH-SALE.
           EXIT PROGRAM.
        OPEN-070.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM ERROR-020.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF WS-PRINTED NOT = "Y"
               PERFORM PRINT-HEADINGS
               PERFORM PRINT-TOTALS
               GO TO END-500.
           IF LINE-CNT > 62
               PERFORM PRINT-HEADINGS.
       END-500.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
      *     IF WS-CASHSALE-ST1 = 88
      *        GO TO END-900.
           CLOSE CASH-SALE.
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
      * END-OF-JOB
