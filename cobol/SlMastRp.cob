        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlMastRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectSlMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdSales.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-SALESAMT          PIC S9(8)V99 VALUE 0.
       77  WS-COST              PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(8)V99 VALUE 0.
       77  WS-PERC              PIC S9(4)V99.
       77  WS-RETAIL-SALES-WEEK PIC S9(8)V99 VALUE 0.
       77  WS-RETAIL-SALES-PTD  PIC S9(8)V99 VALUE 0.
       77  WS-RETAIL-SALES-YTD  PIC S9(8)V99 VALUE 0.
       77  WS-RETAIL-COST-WEEK  PIC S9(8)V99 VALUE 0.
       77  WS-RETAIL-COST-PTD   PIC S9(8)V99 VALUE 0.
       77  WS-RETAIL-COST-YTD   PIC S9(8)V99 VALUE 0.
       77  TOT-SALESWEEK        PIC S9(8)V99 VALUE 0.
       77  TOT-SALESPTD         PIC S9(8)V99 VALUE 0.
       77  TOT-SALESYTD         PIC S9(8)V99 VALUE 0.
       77  TOT-COSTWEEK         PIC S9(8)V99 VALUE 0.
       77  TOT-COSTPTD          PIC S9(8)V99 VALUE 0.
       77  TOT-COSTYTD          PIC S9(8)V99 VALUE 0.
       77  TOT-MARGIN           PIC S9(8)V99 VALUE 0.
       77  TOT-PERC             PIC S9(4)V99.
       01  WS-SALES-STATUS.
           03  WS-SALES-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(22) VALUE " ".
           03  FILLER         PIC X(15) VALUE "S A L E S   A N".
           03  FILLER         PIC X(17) VALUE " A L Y S I S   R ".
           03  FILLER         PIC X(14) VALUE "E P O R T".
           03  FILLER         PIC X(32) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(8) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(39) VALUE " ".
           03  FILLER         PIC X(41) VALUE ALL "*".
           03  FILLER         PIC X(52) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(7) VALUE " ".
           03  FILLER         PIC X(16) VALUE "ANALYSIS".
           03  FILLER         PIC X(48) VALUE "NAME".
           03  FILLER         PIC X(61) VALUE
           "WEEK TO DATE / MONTH TO DATE / YEAR TO DATE".
       01  HEAD4.
           03  FILLER         PIC X(10) VALUE " ".
           03  FILLER         PIC X(54) VALUE "CODE".
           03  FILLER         PIC X(19) VALUE "SALES".
           03  FILLER         PIC X(13) VALUE "COSTS".
           03  FILLER         PIC X(20) VALUE "RAND MARGIN".
           03  FILLER         PIC X(16) VALUE "MARGIN %".
       01  DETAIL-LINE.
           03  FILLER         PIC X(11) VALUE " ".
           03  D-CODE         PIC Z9.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-NAME         PIC X(35) VALUE " ".
           03  D-SALES        PIC Z(7)9.99-.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-COST         PIC Z(7)9.99-.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-MARGIN       PIC Z(7)9.99-.
           03  FILLER         PIC X(9) VALUE " ".
           03  D-PERC         PIC Z(3)9.99-.
           03  FILLER         PIC X(6) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(23) VALUE " ".
           03  D-DIVTOT       PIC X(20) VALUE " ".
           03  D-NAMETOT      PIC X(15) VALUE " ".
           03  D-SALESTOT     PIC Z(7)9.99-.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-COSTTOT      PIC Z(7)9.99-.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-MARGINTOT    PIC Z(7)9.99-.
           03  FILLER         PIC X(9) VALUE " ".
           03  D-PERCTOT      PIC Z(3)9.99-.
           03  FILLER         PIC X(6) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 322 TO POS
           DISPLAY "** SALES ANALYSIS MASTER REPORT BY CODE **" AT POS
           MOVE 422 TO POS
           DISPLAY "******************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM OPEN-FILES.
           MOVE 2510 TO POS
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
              AT POS
           PERFORM PRINT-ROUTINE
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE 0 TO SA-KEY.
           START SALES-ANALYSIS KEY NOT < SA-KEY
              INVALID KEY NEXT SENTENCE.
       PRR-005.
           READ SALES-ANALYSIS NEXT
               AT END NEXT SENTENCE.
           IF WS-SALES-ST1 = 10
               PERFORM END-500 THRU END-900.
           IF WS-SALES-ST1 NOT = 0
             MOVE "SALES BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO PRR-005.
            IF LINE-CNT < 60
               GO TO PRR-020.
       PRR-010.
           ADD 1         TO PAGE-CNT
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
           WRITE PRINT-REC AFTER 2
           MOVE 7 TO LINE-CNT.
       PRR-020.
           IF SA-ANALYSIS-CODE = 1
               MOVE "         ** RETAIL SALES DIVISION **" TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 3 TO LINE-CNT.
           IF SA-ANALYSIS-CODE = 50
               PERFORM END-000
               MOVE "         ** WHOLESALE SALES DIVISION **"
               TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 3 TO LINE-CNT.

           MOVE SA-ANALYSIS-CODE TO D-CODE
           MOVE SA-NAME          TO D-NAME
           MOVE SA-COST-WEEK     TO D-COST
                                   WS-COST
           ADD WS-COST           TO TOT-COSTWEEK
           MOVE SA-SALES-WEEK    TO D-SALES
                                    WS-SALESAMT
           ADD WS-SALESAMT          TO TOT-SALESWEEK
           COMPUTE WS-MARGIN    = WS-SALESAMT - WS-COST
           MOVE WS-MARGIN        TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = 
                      (WS-MARGIN / WS-COST) * 100
           MOVE WS-PERC          TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-SALESAMT
                     WS-COST
                     WS-MARGIN
                     WS-PERC.

           MOVE SA-COST-PTD      TO D-COST
                                   WS-COST.
           ADD WS-COST           TO TOT-COSTPTD.
           MOVE SA-SALES-PTD     TO D-SALES
                                    WS-SALESAMT.
           ADD WS-SALESAMT          TO TOT-SALESPTD.
           COMPUTE WS-MARGIN    = WS-SALESAMT - WS-COST.
           MOVE WS-MARGIN        TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED = 
                      (WS-MARGIN / WS-COST) * 100.
           MOVE WS-PERC          TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-SALESAMT
                     WS-COST
                     WS-MARGIN
                     WS-PERC.

           MOVE SA-COST-YTD  TO D-COST
                                WS-COST.
           ADD WS-COST       TO TOT-COSTYTD.
           MOVE SA-SALES-YTD TO D-SALES
                                WS-SALESAMT.
           ADD WS-SALESAMT      TO TOT-SALESYTD.
           COMPUTE WS-MARGIN = WS-SALESAMT - WS-COST.
           MOVE WS-MARGIN    TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED = 
                      (WS-MARGIN / WS-COST) * 100.
           MOVE WS-PERC      TO D-PERC.

           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-SALESAMT
                     WS-COST
                     WS-MARGIN
                     WS-PERC.

           WRITE PRINT-REC.
           ADD 4 TO LINE-CNT.
           GO TO PRR-005.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-015.
           OPEN I-O SALES-ANALYSIS.
           IF WS-SALES-ST1 NOT = 0
               MOVE 0 TO WS-SALES-ST1
               MOVE "SALES ANALYSIS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-015.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            IF LINE-CNT > 58
               PERFORM PRR-010.
           MOVE "RETAIL DIVISION   " TO D-DIVTOT
           MOVE TOT-SALESWEEK        TO WS-RETAIL-SALES-WEEK
           MOVE TOT-SALESPTD         TO WS-RETAIL-SALES-PTD
           MOVE TOT-SALESYTD         TO WS-RETAIL-SALES-YTD
           MOVE TOT-COSTWEEK         TO WS-RETAIL-COST-WEEK
           MOVE TOT-COSTPTD          TO WS-RETAIL-COST-PTD
           MOVE TOT-COSTYTD          TO WS-RETAIL-COST-YTD
           MOVE "WEEK TOTALS:"       TO D-NAMETOT
           MOVE TOT-SALESWEEK        TO D-SALESTOT
           MOVE TOT-COSTWEEK         TO D-COSTTOT
           COMPUTE TOT-MARGIN = TOT-SALESWEEK - TOT-COSTWEEK
           MOVE TOT-MARGIN           TO D-MARGINTOT
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTWEEK) * 100
           MOVE TOT-PERC             TO D-PERCTOT
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE
           MOVE 0 TO TOT-PERC
                     TOT-MARGIN

           MOVE "MTD TOTALS :"       TO D-NAMETOT
           MOVE TOT-SALESPTD         TO D-SALESTOT
           MOVE TOT-COSTPTD          TO D-COSTTOT
           COMPUTE TOT-MARGIN = TOT-SALESPTD - TOT-COSTPTD
           MOVE TOT-MARGIN           TO D-MARGINTOT
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTPTD) * 100
           MOVE TOT-PERC             TO D-PERCTOT
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE
           MOVE 0 TO TOT-PERC
                     TOT-MARGIN.

           MOVE "YTD TOTALS :" TO D-NAMETOT
           MOVE TOT-SALESYTD   TO D-SALESTOT
           MOVE TOT-COSTYTD    TO D-COSTTOT
           COMPUTE TOT-MARGIN = TOT-SALESYTD - TOT-COSTYTD
           MOVE TOT-MARGIN     TO D-MARGINTOT
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTYTD) * 100
           MOVE TOT-PERC       TO D-PERCTOT
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE " " TO PRINT-REC
                       TOTAL-LINE
           WRITE PRINT-REC
           ADD 4 TO LINE-CNT
           MOVE 0 TO TOT-SALESWEEK
                     TOT-SALESPTD
                     TOT-SALESYTD
                     TOT-COSTWEEK
                     TOT-COSTPTD
                     TOT-COSTYTD
                     TOT-PERC
                     TOT-MARGIN.
       END-500.
            IF LINE-CNT > 56
               PERFORM PRR-010.
           MOVE "WHOLESALE DIVISION" TO D-DIVTOT
           MOVE "WEEK TOTALS:"  TO D-NAMETOT
           MOVE TOT-SALESWEEK   TO D-SALESTOT
           MOVE TOT-COSTWEEK    TO D-COSTTOT
           COMPUTE TOT-MARGIN = TOT-SALESWEEK - TOT-COSTWEEK
           MOVE TOT-MARGIN     TO D-MARGINTOT
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTWEEK) * 100
           MOVE TOT-PERC       TO D-PERCTOT
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE " " TO PRINT-REC
                       TOTAL-LINE
           MOVE 0 TO TOT-PERC
                     TOT-MARGIN

           MOVE "MTD TOTALS :" TO D-NAMETOT
           MOVE TOT-SALESPTD   TO D-SALESTOT
           MOVE TOT-COSTPTD    TO D-COSTTOT
           COMPUTE TOT-MARGIN = TOT-SALESPTD - TOT-COSTPTD
           MOVE TOT-MARGIN     TO D-MARGINTOT
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTPTD) * 100
           MOVE TOT-PERC       TO D-PERCTOT
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE
           MOVE 0 TO TOT-PERC
                     TOT-MARGIN

           MOVE "YTD TOTALS :" TO D-NAMETOT
           MOVE TOT-SALESYTD   TO D-SALESTOT
           MOVE TOT-COSTYTD    TO D-COSTTOT
           COMPUTE TOT-MARGIN = TOT-SALESYTD - TOT-COSTYTD
           MOVE TOT-MARGIN     TO D-MARGINTOT
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTYTD) * 100
           MOVE TOT-PERC       TO D-PERCTOT
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE " " TO PRINT-REC
                       TOTAL-LINE
           WRITE PRINT-REC
           ADD 4 TO LINE-CNT
           MOVE 0 TO TOT-PERC
                     TOT-MARGIN.
       END-700.
            IF LINE-CNT > 58
               PERFORM PRR-010.
           MOVE "WHOLESALE + RETAIL" TO D-DIVTOT
           ADD WS-RETAIL-SALES-WEEK  TO TOT-SALESWEEK
           ADD WS-RETAIL-SALES-PTD   TO TOT-SALESPTD
           ADD WS-RETAIL-SALES-YTD   TO TOT-SALESYTD
           ADD WS-RETAIL-COST-WEEK   TO TOT-COSTWEEK
           ADD WS-RETAIL-COST-PTD    TO TOT-COSTPTD
           ADD WS-RETAIL-COST-YTD    TO TOT-COSTYTD

           MOVE "WEEK TOTALS:"  TO D-NAMETOT
           MOVE TOT-SALESWEEK   TO D-SALESTOT
           MOVE TOT-COSTWEEK    TO D-COSTTOT
           COMPUTE TOT-MARGIN = TOT-SALESWEEK - TOT-COSTWEEK
           MOVE TOT-MARGIN     TO D-MARGINTOT
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTWEEK) * 100
           MOVE TOT-PERC       TO D-PERCTOT
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE
           MOVE 0 TO TOT-PERC
                     TOT-MARGIN

           MOVE "MTD TOTALS :" TO D-NAMETOT
           MOVE TOT-SALESPTD   TO D-SALESTOT
           MOVE TOT-COSTPTD    TO D-COSTTOT
           COMPUTE TOT-MARGIN = TOT-SALESPTD - TOT-COSTPTD
           MOVE TOT-MARGIN     TO D-MARGINTOT
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTPTD) * 100
           MOVE TOT-PERC       TO D-PERCTOT
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE
           MOVE 0 TO TOT-PERC
                     TOT-MARGIN

           MOVE "YTD TOTALS :" TO D-NAMETOT
           MOVE TOT-SALESYTD   TO D-SALESTOT
           MOVE TOT-COSTYTD    TO D-COSTTOT
           COMPUTE TOT-MARGIN = TOT-SALESYTD - TOT-COSTYTD
           MOVE TOT-MARGIN     TO D-MARGINTOT
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTYTD) * 100
           MOVE TOT-PERC       TO D-PERCTOT
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           ADD 4 TO LINE-CNT.
       END-800.
           CLOSE PRINT-FILE
                 SALES-ANALYSIS.
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
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
