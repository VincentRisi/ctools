        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlAnCoRp.
        AUTHOR.     STEVE CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectSlMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdSales.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(135).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE             PIC X(2) VALUE " ".
       77  WS-RANGEDIS          PIC Z9.
       77  WS-RANGE1            PIC 99.
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-SALESAMT          PIC S9(8)V99 VALUE 0.
       77  WS-COST              PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(8)V99 VALUE 0.
       77  WS-PERC              PIC S999V99.
       77  WS-SALESAMT-YTD      PIC S9(8)V99 VALUE 0.
       77  WS-COST-YTD          PIC S9(8)V99 VALUE 0.
       77  WS-SALESAMT-LY       PIC S9(8)V99 VALUE 0.
       77  WS-COST-LY           PIC S9(8)V99 VALUE 0.
       77  TOT-SALESAMT         PIC S9(8)V99 VALUE 0.
       77  TOT-COST             PIC S9(8)V99 VALUE 0.
       77  TOT-MARGIN           PIC S9(8)V99 VALUE 0.
       77  TOT-PERC             PIC S999V99.
       77  TOT-SALESAMT-YTD     PIC S9(8)V99 VALUE 0.
       77  TOT-COST-YTD         PIC S9(8)V99 VALUE 0.
       77  TOT-SALESAMT-LY      PIC S9(8)V99 VALUE 0.
       77  TOT-COST-LY          PIC S9(8)V99 VALUE 0.
       01  WS-SALES-STATUS.
           03  WS-SALES-ST1     PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(14) VALUE "S A L E S   ".
           03  FILLER         PIC X(18) VALUE "A N A L Y S I S   ".
           03  FILLER         PIC X(11) VALUE "R E P O R T".
           03  FILLER         PIC X(16) VALUE " ".
           03  FILLER         PIC X(15) VALUE "ANALYSIS CODE :".
           03  H1-ANAL-CODE   PIC Z9.
           03  FILLER         PIC X(15) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(43) VALUE ALL "*".
           03  FILLER         PIC X(16) VALUE " ".
           03  H2-ANALYSIS    PIC X(25).
           03  FILLER         PIC X(19) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(78) VALUE " ".
           03  FILLER         PIC X(48) VALUE
            "PERIOD / YEAR / LAST YEAR TO DATE.".
       01  HEAD4.
           03  FILLER         PIC X(54) VALUE "ACC.NO.  NAME ".
           03  FILLER         PIC X(16) VALUE "BALANCE".
           03  FILLER         PIC X(18) VALUE "SALES AMT".
           03  FILLER         PIC X(10) VALUE " COST".
           03  FILLER         PIC X(14) VALUE " MARGIN".
           03  FILLER         PIC X(8) VALUE "%".
       01  DETAIL-LINE.
           03  D-CATEGORY.
               05  D-ACCOUNT  PIC X(9) VALUE " ".
               05  D-NAME     PIC X(43) VALUE " ".
               05  D-BALANCE  PIC Z(5)9.99-.
               05  FILLER     PIC X(3) VALUE " ".
           03  FILLER         PIC X(4) VALUE " ".
           03  D-SALESAMT     PIC Z(6)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-COST         PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-MARGIN       PIC Z(6)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-PERC         PIC Z99.99-.
           03  FILLER         PIC X(5) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN
           MOVE 320 TO POS.
           DISPLAY "** SALES ANALYSIS BY ANALYSIS CODE REPORT **"
              AT POS.
           MOVE 420 TO POS.
           DISPLAY "********************************************"
              AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1210 TO POS.
           DISPLAY "Enter 'A' To Print ALL Debtors" AT POS.
           MOVE 1010 TO POS.
           DISPLAY "Enter The ANALYSIS CODE:[  ]" AT POS.
           MOVE 1035 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 34        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF WS-RANGE = "A"
               GO TO CONTROL-015.
           MOVE WS-RANGE TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WS-RANGEDIS
                                WS-RANGE1.
           MOVE 1035 TO POS.
           DISPLAY WS-RANGEDIS AT POS.
           IF NUMERIC-RATE > 0
                GO TO CONTROL-015
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO CONTROL-010.
       CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-030.
           MOVE 2510 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
               AT POS.
           PERFORM READ-ANALYSIS.
           IF WS-SALES-ST1 = 88
               MOVE "NO SUCH ANALYSIS NUMBER, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO CONTROL-010.
           PERFORM ERROR-020.
           MOVE 1134 TO POS
           DISPLAY SA-NAME AT POS.
           MOVE 2510 TO POS.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       READ-ANALYSIS SECTION.
       RA-001.
           IF WS-RANGE = "A"
              MOVE "ALL ACCOUNTS" TO SA-NAME
              GO TO RA-999.
           MOVE WS-RANGE1 TO SA-KEY.
           START SALES-ANALYSIS KEY NOT < SA-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-SALES-ST1 NOT = 0
              MOVE 88 TO WS-SALES-ST1
              GO TO RA-999.
       RA-005.
           READ SALES-ANALYSIS
               INVALID KEY NEXT SENTENCE.
           IF WS-SALES-ST1 = 23 OR 35 OR 49
              MOVE 88 TO WS-SALES-ST1
              GO TO RA-999.
           IF WS-SALES-ST1 NOT = 0
               MOVE "SALES ANALYSIS BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
              GO TO RA-005.
           MOVE SA-NAME TO H2-ANALYSIS.
       RA-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           START DEBTOR-MASTER KEY NOT < DR-KEY
             INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
              GO TO PRR-999.
       PRR-005.
           READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
               GO TO PRR-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PRR-005.
           IF WS-MESSAGE NOT = "   "
              PERFORM ERROR-020.
           IF DR-SALES-ANALYSIS = 0
              GO TO PRR-005.
              
           MOVE 2410 TO POS
           DISPLAY "Debtor Account Being Read:" AT POS
           ADD 27 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
           
           IF WS-RANGE = "A"
              GO TO PRR-010.
           IF DR-SALES-ANALYSIS = WS-RANGE1
              GO TO PRR-010.
           GO TO PRR-005.
       PRR-010.
            IF LINE-CNT < 59
               GO TO PRR-020.
            ADD 1          TO PAGE-CNT
            MOVE WS-RANGE1 TO H1-ANAL-CODE
            MOVE PAGE-CNT  TO H1-PAGE.

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
            MOVE 8 TO LINE-CNT.
       PRR-020.
           MOVE DR-ACCOUNT-NUMBER TO D-ACCOUNT
           MOVE DR-NAME           TO D-NAME
           MOVE DR-BALANCE        TO D-BALANCE
           MOVE DR-SALES-PTD      TO D-SALESAMT
           MOVE DR-COST-PTD       TO D-COST
           ADD DR-SALES-PTD  TO WS-SALESAMT
                               TOT-SALESAMT
           ADD DR-SALES-YTD  TO WS-SALESAMT-YTD
                               TOT-SALESAMT-YTD
           ADD DR-SALES-LAST TO WS-SALESAMT-LY
                               TOT-SALESAMT-LY
           ADD DR-COST-PTD   TO WS-COST
                               TOT-COST
           ADD DR-COST-YTD   TO WS-COST-YTD
                               TOT-COST-YTD
           ADD DR-COST-LAST  TO WS-COST-LY
                               TOT-COST-LY.

           COMPUTE WS-MARGIN = WS-SALESAMT - WS-COST
           MOVE WS-MARGIN   TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / WS-COST * 100
           MOVE WS-PERC     TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC DETAIL-LINE.

           MOVE WS-SALESAMT-YTD TO D-SALESAMT
           MOVE WS-COST-YTD     TO D-COST
           COMPUTE WS-MARGIN = WS-SALESAMT-YTD - WS-COST-YTD
           MOVE WS-MARGIN       TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / 
                   WS-COST-YTD * 100
           MOVE WS-PERC         TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC DETAIL-LINE

           MOVE WS-SALESAMT-LY  TO D-SALESAMT
           MOVE WS-COST-LY      TO D-COST
           COMPUTE WS-MARGIN = WS-SALESAMT-LY - WS-COST-LY
           MOVE WS-MARGIN       TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / 
                   WS-COST-LY * 100
           MOVE WS-PERC         TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC DETAIL-LINE
           WRITE PRINT-REC AFTER 1
           ADD 4  TO LINE-CNT
           MOVE 0 TO WS-SALESAMT
                     WS-COST
                     WS-MARGIN
                     WS-PERC
                     WS-SALESAMT-YTD
                     WS-COST-YTD
                     WS-SALESAMT-LY
                     WS-COST-LY.
           GO TO PRR-005.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-020.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-020.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-030.
            OPEN I-O SALES-ANALYSIS.
            IF WS-SALES-ST1 NOT = 0
               MOVE 0 TO WS-SALES-ST1
               MOVE "SALES ANALYSIS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-030.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 60
              PERFORM PRR-010.
              
           MOVE "TOTALS***"  TO D-CATEGORY
           MOVE TOT-SALESAMT TO D-SALESAMT
           MOVE TOT-COST     TO D-COST
           COMPUTE TOT-MARGIN = TOT-SALESAMT - TOT-COST
           MOVE TOT-MARGIN   TO D-MARGIN
           COMPUTE TOT-PERC ROUNDED = TOT-MARGIN / TOT-COST * 100
           MOVE TOT-PERC     TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC DETAIL-LINE.

           MOVE TOT-SALESAMT-YTD TO D-SALESAMT
           MOVE TOT-COST-YTD     TO D-COST
           COMPUTE TOT-MARGIN = TOT-SALESAMT-YTD - TOT-COST-YTD
           MOVE TOT-MARGIN   TO D-MARGIN
           COMPUTE TOT-PERC ROUNDED = TOT-MARGIN / 
                   TOT-COST-YTD * 100
           MOVE TOT-PERC     TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC DETAIL-LINE.

           MOVE TOT-SALESAMT-LY TO D-SALESAMT
           MOVE TOT-COST-LY     TO D-COST
           COMPUTE TOT-MARGIN = TOT-SALESAMT-LY - TOT-COST-LY
           MOVE TOT-MARGIN   TO D-MARGIN
           COMPUTE TOT-PERC ROUNDED = TOT-MARGIN / 
                   TOT-COST-LY * 100
           MOVE TOT-PERC     TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           
           IF WS-RANGE = "A"
             MOVE "** ALL SALES ANALYSIS CODES PRINTED. **" TO PRINT-REC
             WRITE PRINT-REC AFTER 1.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-800.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE SALES-ANALYSIS
                 DEBTOR-MASTER.
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
