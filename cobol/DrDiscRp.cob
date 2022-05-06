        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrDiscRp.
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
       77  WS-RANGE             PIC X VALUE " ".
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-MARGIN            PIC S9(7)V99 VALUE 0.
       77  WS-PERC              PIC S9(4)V99.
       77  TOT-SALESAMT-PTD     PIC S9(7)V99 VALUE 0.
       77  TOT-SALESAMT-YTD     PIC S9(7)V99 VALUE 0.
       77  TOT-SALESAMT-LAST    PIC S9(7)V99 VALUE 0.
       77  TOT-COST-PTD         PIC S9(7)V99 VALUE 0.
       77  TOT-COST-YTD         PIC S9(7)V99 VALUE 0.
       77  TOT-COST-LAST        PIC S9(7)V99 VALUE 0.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(89) VALUE
           "D E B T O R S   A N A L Y S I S   B Y   D I S C O U N T".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(55) VALUE ALL "*".
           03  FILLER         PIC X(9) VALUE " ".
           03  FILLER         PIC X(15) VALUE "DISCOUNT CODE: ".
           03  H1-DISC-CODE   PIC X.
           03  FILLER         PIC X(23) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(82) VALUE " ".
           03  FILLER         PIC X(14) VALUE "PERIOD / YEAR ".
           03  FILLER         PIC X(11) VALUE "/ LAST YEAR".
           03  FILLER         PIC X(42) VALUE " TO DATE".
       01  HEAD4.
           03  FILLER         PIC X(52) VALUE "NUMBER   NAME ".
           03  FILLER         PIC X(24) VALUE " BALANCE".
           03  FILLER         PIC X(18) VALUE "SALES AMT".
           03  FILLER         PIC X(10) VALUE " COST".
           03  FILLER         PIC X(15) VALUE " MARGIN".
           03  FILLER         PIC X(17) VALUE "%".
       01  DETAIL-LINE.
           03  D-CATEGORY.
               05  D-ACCOUNT  PIC X(9) VALUE " ".
               05  D-NAME     PIC X(42) VALUE " ".
               05  D-BALANCE  PIC Z(5)9.99-.
               05  FILLER     PIC X(10) VALUE " ".
           03  FILLER         PIC X(4) VALUE " ".
           03  D-SALESAMT     PIC Z(6)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-COST         PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-MARGIN       PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-PERC         PIC Z(3)9.99-.
           03  FILLER         PIC X(66) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 320 TO POS
           DISPLAY "** DEBTORS DISCOUNT ANALYSIS REPORT **" AT POS
           MOVE 420 TO POS
           DISPLAY "**************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAcceptDr".
        CONTROL-010.
            MOVE 1510 TO POS
            DISPLAY "ENTER A DISCOUNT CODE BETWEEN 0 & 9" AT POS
            MOVE 1610 TO POS
            DISPLAY "OR ENTER 'A' FOR ALL DEBTORS WITH DISCOUNT" AT POS
            MOVE 1710 TO POS
            DISPLAY "OR ENTER 'D' FOR ALL DEBTORS WITH NO DISCOUNT."
              AT POS
            MOVE 1010 TO POS
            DISPLAY "ENTER THE DISCOUNT CODE:" AT POS
            MOVE 1054 TO POS
            DISPLAY "[ ]" AT POS
            MOVE 1055 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE.

      *      ACCEPT WS-RANGE AT POS.
            IF W-ESCAPE-KEY = 4
                GO TO CONTROL-005.
            IF WS-RANGE = "0" OR = "1" OR = "2" OR = "3" OR = "4"
                     OR = "5" OR = "6" OR = "7" OR = "8" OR = "9"
                     OR = "A" OR = "D"
                GO TO CONTROL-015
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO CONTROL-010.
       CONTROL-015.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-020
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-020.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE
           MOVE 2510 TO POS
           DISPLAY "Report Is Being Compiled, Please Be Patient."
              AT POS
           PERFORM OPEN-FILES
           PERFORM PRINT-ROUTINE
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           IF WS-RANGE = "A" OR = "D"
                MOVE " "      TO DR-DISCOUNT-CODE
           ELSE
                MOVE WS-RANGE TO DR-DISCOUNT-CODE.
           START DEBTOR-MASTER KEY NOT < DR-DISCOUNT-CODE
                 INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE "DEBTOR RECORD BUSY ON START, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 88 TO WS-DEBTOR-ST1
              GO TO PRR-999.
       PRR-005.
           READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
              GO TO PRR-999.
           IF WS-DEBTOR-ST1 = 91
              MOVE 0 TO WS-DEBTOR-ST1
              MOVE "DEBTOR RECORD ST1 = 91, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              CLOSE DEBTOR-MASTER
              OPEN I-O DEBTOR-MASTER
              GO TO PRR-005.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PRR-005.
              
           MOVE 2310 TO POS
           DISPLAY "ACCOUNT BEING READ:" AT POS
           ADD 21 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
              
           IF WS-RANGE = "A"
            IF DR-DISCOUNT-CODE NOT = "0" AND NOT = " "
              GO TO PRR-010.
           IF WS-RANGE = "D"
            IF DR-DISCOUNT-CODE = "0" OR = " "
              GO TO PRR-010.
           IF DR-DISCOUNT-CODE NOT = WS-RANGE
              GO TO PRR-999.
       PRR-010.
           IF LINE-CNT < 60
               GO TO PRR-020.
           ADD 1         TO PAGE-CNT
           MOVE WS-RANGE TO H1-DISC-CODE
           MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
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
           ADD DR-SALES-PTD  TO TOT-SALESAMT-PTD
           ADD DR-SALES-YTD  TO TOT-SALESAMT-YTD
           ADD DR-SALES-LAST TO TOT-SALESAMT-LAST
           ADD DR-COST-PTD   TO TOT-COST-PTD
           ADD DR-COST-YTD   TO TOT-COST-YTD
           ADD DR-COST-LAST  TO TOT-COST-LAST.
       PRR-025.
           MOVE DR-ACCOUNT-NUMBER TO D-ACCOUNT
           MOVE DR-NAME           TO D-NAME
           MOVE DR-BALANCE        TO D-BALANCE
           MOVE DR-SALES-PTD      TO D-SALESAMT
           MOVE DR-COST-PTD       TO D-COST
           COMPUTE WS-MARGIN = DR-SALES-PTD - DR-COST-PTD
           MOVE WS-MARGIN         TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / DR-COST-PTD * 100
           MOVE WS-PERC           TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0   TO WS-MARGIN
                       WS-PERC
           MOVE DR-SALES-YTD      TO D-SALESAMT
           MOVE DR-COST-YTD       TO D-COST
           COMPUTE WS-MARGIN = DR-SALES-YTD - DR-COST-YTD
           MOVE WS-MARGIN         TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / DR-COST-YTD * 100
           MOVE WS-PERC           TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0   TO WS-MARGIN
                       WS-PERC
           MOVE DR-SALES-LAST      TO D-SALESAMT
           MOVE DR-COST-LAST       TO D-COST
           COMPUTE WS-MARGIN = DR-SALES-LAST - DR-COST-LAST
           MOVE WS-MARGIN         TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / DR-COST-LAST * 100
           MOVE WS-PERC           TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0   TO WS-MARGIN
                       WS-PERC
           WRITE PRINT-REC AFTER 1
           ADD 4 TO LINE-CNT
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
           IF LINE-CNT > 60
               PERFORM PRR-010.
               
           IF WS-DEBTOR-ST1 = 88
               MOVE "NO DEBTORS WITH THAT DISCOUNT CODE TO PRINT."
               TO PRINT-REC
               WRITE PRINT-REC AFTER 2.    
               
           MOVE "TOTALS***"      TO D-CATEGORY
           MOVE TOT-SALESAMT-PTD TO D-SALESAMT
           MOVE TOT-COST-PTD     TO D-COST
           COMPUTE WS-MARGIN = TOT-SALESAMT-PTD - TOT-COST-PTD
           MOVE WS-MARGIN        TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / TOT-COST-PTD) * 100
           MOVE WS-PERC          TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0   TO WS-MARGIN
                       WS-PERC.
           MOVE TOT-SALESAMT-YTD TO D-SALESAMT
           MOVE TOT-COST-YTD     TO D-COST
           COMPUTE WS-MARGIN = TOT-SALESAMT-YTD - TOT-COST-YTD
           MOVE WS-MARGIN        TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / TOT-COST-YTD) * 100
           MOVE WS-PERC          TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0   TO WS-MARGIN
                       WS-PERC
           MOVE TOT-SALESAMT-LAST TO D-SALESAMT
           MOVE TOT-COST-LAST     TO D-COST
           COMPUTE WS-MARGIN = TOT-SALESAMT-LAST - TOT-COST-LAST
           MOVE WS-MARGIN         TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / TOT-COST-LAST) * 100
           MOVE WS-PERC           TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC.
           
           IF WS-RANGE = "D"
           MOVE "** ALL ACCOUNTS WITH NO DISCOUNT HAVE BEEN PRINTED **"
             TO PRINT-REC
             WRITE PRINT-REC AFTER 1
             GO TO END-500.
           IF WS-RANGE = "A"
           MOVE "* ALL ACCOUNTS WITH DISCOUNT HAVE BEEN PRINTED *"
             TO PRINT-REC
             WRITE PRINT-REC AFTER 1
             GO TO END-500.
           IF WS-RANGE NOT = "D" AND NOT = "A"
           MOVE 
          "* ALL ACCOUNTS WITH A SPECIFIC DISCOUNT HAVE BEEN PRINTED *"
             TO PRINT-REC
             WRITE PRINT-REC AFTER 1.
       END-500.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE DEBTOR-MASTER.
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
