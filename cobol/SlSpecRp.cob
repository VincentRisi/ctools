        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlSpecRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectSlSpecials".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdSpecialSales.
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       77  WS-LINE-ANSWER       PIC X VALUE " ".
       77  WS-LINECNT           PIC S9(3) VALUE 0.
       77  WS-LINE-DIS          PIC S9(4) VALUE 0.
       77  LINE-DISPLAY         PIC Z(3)9.
       77  WS-LINE-TOT          PIC 9(4) VALUE 0.
       77  WS-PRINT-THAT-SB     PIC XX VALUE " ".
       77  WS-PRINT-ONLY-SB     PIC XX VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-TOT-ONLY          PIC X VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-ANSWER2           PIC X VALUE " ".
       77  WS-SALE              PIC S9(7)V99 VALUE 0.
       77  WS-COST              PIC S9(7)V99 VALUE 0.
       77  WS-SALE-AMT          PIC S9(7)V99 VALUE 0.
       77  WS-COST-AMT          PIC S9(7)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(7)V99 VALUE 0.
       77  WS-PERC              PIC S9(3)V999 VALUE 0.
       77  WS-SALE-TOT          PIC S9(7)V99 VALUE 0.
       77  WS-COST-TOT          PIC S9(7)V99 VALUE 0.
       01  WS-SPECIALS-STATUS.
           03  WS-SPECIALS-ST1  PIC 99.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE "  DATE".
           03  H1-DATE          PIC X(10).
           03  FILLER           PIC X(22) VALUE " ".
           03  FILLER           PIC X(81) VALUE 
           "S P E C I A L S    B Y   I N V O I C E   R E P O R T".
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(1) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(39) VALUE " ".
           03  FILLER           PIC X(93) VALUE 
           "****************************************************".
       01  HEAD3.
           03  FILLER           PIC X(9) VALUE " ".
           03  FILLER           PIC X(10) VALUE "Initials: ".
           03  P-Initials       PIC XX.
           03  FILLER           PIC X(111) VALUE " ".
       01  HEAD4.
           03  FILLER           PIC X(9) VALUE " ". 
           03  FILLER           PIC X(123) VALUE "**********".
       01  HEAD5.
           03  FILLER           PIC X(9) VALUE  " ".
           03  H5-LINE.
             05  FILLER         PIC X(7) VALUE "  Inv.".
             05  FILLER         PIC X(16) VALUE "Stock Number".
             05  FILLER         PIC X(8) VALUE "Acc No".
             05  FILLER         PIC X(29) VALUE "Acc Name".
             05  FILLER         PIC X(13) VALUE  "Inv Date".
           03  FILLER           PIC X(14) VALUE "Sale Amt".
           03  FILLER           PIC X(16) VALUE "Cost Amt".
           03  FILLER           PIC X(18) VALUE "Margin      Perc".
       01  DETAIL-LINE.
           03  FILLER           PIC X(9) VALUE " ".
           03  P-INVOICE        PIC Z(6).
           03  FILLER           PIC X(1) VALUE " ".
           03  P-STOCK          PIC X(15) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  P-ACCOUNT        PIC X(8) VALUE " ".
           03  P-NAME           PIC X(29) VALUE " ".
           03  P-DATE           PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  P-SALE           PIC Z(6)9.99-.
           03  FILLER           PIC X(3) VALUE " ".
           03  P-COST           PIC Z(6)9.99.
           03  FILLER           PIC X(4) VALUE " ".
           03  P-MARGIN         PIC Z(6)9.99-.
           03  FILLER           PIC X(3) VALUE " ".
           03  P-PERC           PIC Z(2)9.99-.
       01  SUB-TOTAL-LINE.
           03  FILLER           PIC X(21) VALUE " ".
           03  FILLER           PIC X(16) VALUE "No. Of Orders:".
           03  P-SUB-CNT        PIC Z(2)9-.
           03  FILLER           PIC X(29) VALUE " ".
           03  FILLER           PIC X(10) VALUE "Totals :R".
           03  P-SUB-SALE       PIC Z(6)9.99-.
           03  FILLER           PIC X(3) VALUE " ".
           03  P-SUB-COST       PIC Z(6)9.99.
           03  FILLER           PIC X(4) VALUE " ".
           03  P-SUB-MARGIN     PIC Z(6)9.99-.
           03  FILLER           PIC X(3) VALUE " ".
           03  P-SUB-PERC       PIC Z(2)9.99-.
       01  TOT-TOTAL-LINE.
           03  FILLER           PIC X(21) VALUE "Gross Totals".
           03  FILLER           PIC X(15) VALUE "No. Of lines :".
           03  P-TOT-CNT        PIC Z(4).
           03  FILLER           PIC X(30) VALUE " ".
           03  FILLER           PIC X(10) VALUE "Totals :R".
           03  P-TOT-SALE       PIC Z(6)9.99-.
           03  FILLER           PIC X(3) VALUE " ".
           03  P-TOT-COST       PIC Z(6)9.99.
           03  FILLER           PIC X(4) VALUE " ".
           03  P-TOT-MARGIN     PIC Z(6)9.99-.
           03  FILLER           PIC X(3) VALUE " ".
           03  P-TOT-PERC       PIC Z(2)9.99-.
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
           DISPLAY "***** Specials By Invoice Report *****" AT POS
           MOVE 420 TO POS
           DISPLAY "**************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-030.
           MOVE 1010 TO POS.    
           DISPLAY "Enter The Initials To Be Printed" AT POS.
           MOVE 1054 TO POS.
           DISPLAY "[  ]" AT POS.
           MOVE 1210 TO POS.
           DISPLAY "Leave Blank To Print All Initials" AT POS.
           MOVE 1055 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PRINT-THAT-SB.

      *     ACCEPT WS-PRINT-THAT-SB AT POS.
           MOVE WS-PRINT-THAT-SB TO WS-PRINT-ONLY-SB
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-030.
       CONTROL-040.
           MOVE 1510 TO POS.
           DISPLAY "Print Only Totals Of Each Salesman" AT POS.
           MOVE 1554 TO POS.
           DISPLAY "[ ]" AT POS.
           MOVE 1555 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

      *     ACCEPT WS-ANSWER AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-030.
           IF WS-ANSWER = "Y" 
              MOVE "Y" TO WS-TOT-ONLY.
           IF WS-ANSWER NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-045
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
       CONTROL-045.
           MOVE 1710 TO POS.
           DISPLAY "Do you wish to DELETE the file after Printing: [ ]"
           AT POS.
           MOVE 1758 TO POS.
           
           MOVE 'N'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-040.
           IF WS-ANSWER2 NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-045.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-045.
       CONTROL-050.
           MOVE 2510 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
           AT POS.
       CONTROL-070.
           PERFORM OPEN-FILES.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           IF WS-TOT-ONLY NOT = "Y"
               MOVE WS-PRINT-THAT-SB TO SP-INITIALS
           ELSE
               MOVE 0                TO SP-INITIALS.
           MOVE 0                    TO SP-INVOICE-NUMBER
           MOVE " "                  TO SP-STOCK.
           
           START SPECIALS-FILE KEY NOT < SP-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-SPECIALS-ST1 NOT = 0
               PERFORM SUB-LINE
               PERFORM PRINT-TOTALS
               GO TO PR-999.
       PR-001.
           READ SPECIALS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-SPECIALS-ST1 = 10
               PERFORM SUB-LINE
               PERFORM PRINT-TOTALS
               GO TO PR-999.
           IF WS-SPECIALS-ST1 NOT = 0
               MOVE "SPECIALS-FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SPECIALS-ST1
               GO TO PR-001.
       PR-005.
           IF WS-PRINT-THAT-SB = " "
             IF WS-LINE = 66
              MOVE SP-INITIALS TO WS-PRINT-THAT-SB P-INITIALS
               GO TO PR-010.
           IF WS-PRINT-ONLY-SB NOT = " "
            IF SP-INITIALS NOT = WS-PRINT-ONLY-SB
             IF WS-LINE = 66
               GO TO PR-001
             ELSE
               PERFORM SUB-LINE
               GO TO PR-999.
           IF WS-TOT-ONLY NOT = "Y"
            IF SP-INITIALS NOT = WS-PRINT-THAT-SB 
             IF WS-PRINT-ONLY-SB = " "
               PERFORM SUB-LINE
               GO TO PR-010
             ELSE
               PERFORM SUB-LINE
               GO TO PR-999.
           IF WS-TOT-ONLY NOT = "Y"
               GO TO PR-010.

           IF WS-PRINT-THAT-SB NOT = " "
              GO TO PR-010.
       PR-010.
           IF WS-LINE > 60
               MOVE "N" TO WS-LINE-ANSWER
               PERFORM PRINT-HEADINGS.
           IF SP-INITIALS NOT = WS-PRINT-THAT-SB
             IF WS-PRINT-THAT-SB NOT = " "
               PERFORM SUB-LINE.
      *     IF SP-INITIALS NOT = WS-PRINT-THAT-SB
           IF SP-INITIALS NOT = P-INITIALS
             IF WS-PRINT-THAT-SB = " "
               PERFORM SUB-LINE.
      *         MOVE SP-INITIALS TO WS-PRINT-THAT-SB.
       PR-030.
           IF WS-ANSWER = "Y"
               GO TO PR-033.
           MOVE SP-INVOICE-NUMBER  TO P-INVOICE
           MOVE SP-STOCK           TO P-STOCK
           MOVE SP-ACCOUNT-NUMBER  TO P-ACCOUNT
           MOVE SP-ACCOUNT-NAME    TO P-NAME
           MOVE SP-DATE-OF-INVOICE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO P-DATE.
           IF SP-TRANS = "6"
              COMPUTE WS-SALE = SP-SALE-AMOUNT * -1
              MOVE WS-SALE         TO P-SALE
              COMPUTE WS-COST = SP-COST-AMOUNT * -1
              MOVE WS-COST         TO P-COST
           ELSE
              MOVE SP-SALE-AMOUNT  TO P-SALE
              MOVE SP-COST-AMOUNT  TO P-COST.
           COMPUTE WS-MARGIN = SP-SALE-AMOUNT - SP-COST-AMOUNT.
           IF SP-TRANS = "6"
              COMPUTE WS-MARGIN = WS-MARGIN * -1.
           MOVE WS-MARGIN          TO P-MARGIN
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / SP-COST-AMOUNT) * 100
           MOVE WS-PERC            TO P-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           ADD 1 TO WS-LINE
           MOVE 0 TO WS-MARGIN WS-PERC P-COST P-SALE.
       PR-033.
           IF SP-TRANS = "6"
               SUBTRACT SP-SALE-AMOUNT FROM WS-SALE-AMT
               SUBTRACT SP-COST-AMOUNT FROM WS-COST-AMT
           ELSE
               ADD SP-SALE-AMOUNT TO WS-SALE-AMT
               ADD SP-COST-AMOUNT TO WS-COST-AMT.
       PR-035.
           IF SP-TRANS = "1"
              ADD 1 TO WS-LINECNT.
           IF SP-TRANS = "6"
              SUBTRACT 2 FROM WS-LINECNT.
           IF WS-PRINT-THAT-SB = " " 
               MOVE SP-INITIALS TO WS-PRINT-THAT-SB.
               
           ADD 1 TO WS-LINE-DIS
           MOVE WS-LINE-DIS TO LINE-DISPLAY
           MOVE 2610 TO POS
           DISPLAY "Number of Records Read:" AT POS
           ADD 23 TO POS
           DISPLAY LINE-DISPLAY AT POS.
         
           IF WS-ANSWER2 = "Y"
                PERFORM PR-050.
               
           GO TO PR-001.
       PR-050.
      * TEMP FIX TO REMOVE PREVIOUS DATA NOT DELETED BEFORE.  DISCOVERED
      * IN SEP 2016 AFTER RUNNING FOR A YEAR, THAT THE OLD CTOS WAY
      * OF DELETING THE FILE USING ISAM COMMANDS WAS LEFT BEHIND WHEN
      * WE MOVED OVER TO LINUX.
      *     IF SP-DATE-OF-INVOICE < 20160900
           DELETE SPECIALS-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-SPECIALS-ST1 NOT = 0
               MOVE "SPECIALS FILE BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SPECIALS-ST1
               GO TO PR-050.
       PR-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           IF WS-LINE-ANSWER = "Y"
               MOVE SP-INITIALS TO P-INITIALS.
           IF WS-PAGE = 0
               MOVE SP-INITIALS TO P-INITIALS.
           ADD 1        TO WS-PAGE
           MOVE WS-PAGE TO H-PAGE
           MOVE " "     TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF WS-PAGE = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC.
           IF WS-TOT-ONLY = "Y"
             MOVE " " TO H5-LINE
             WRITE PRINT-REC FROM HEAD5 AFTER 1
             MOVE " " TO PRINT-REC.
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC.
           IF WS-TOT-ONLY NOT = "Y"
             WRITE PRINT-REC FROM HEAD5 AFTER 1
             MOVE " " TO PRINT-REC
             WRITE PRINT-REC.
           MOVE 7 TO WS-LINE.       
        PH-999.
           EXIT.
      *
       SUB-LINE SECTION.
       SL-000.
           IF WS-LINE > 60
               MOVE "N" TO WS-LINE-ANSWER
               PERFORM PRINT-HEADINGS.
           MOVE WS-SALE-AMT TO P-SUB-SALE
           MOVE WS-COST-AMT TO P-SUB-COST
           COMPUTE WS-MARGIN = WS-SALE-AMT - WS-COST-AMT
           MOVE WS-MARGIN   TO P-SUB-MARGIN
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / WS-COST-AMT) * 100
           MOVE WS-PERC     TO P-SUB-PERC
           MOVE WS-LINECNT  TO P-SUB-CNT
           WRITE PRINT-REC FROM SUB-TOTAL-LINE AFTER 1
           ADD 1 TO WS-LINE.
       SL-010.
           MOVE SP-INITIALS    TO P-INITIALS WS-PRINT-THAT-SB
           ADD WS-SALE-AMT     TO WS-SALE-TOT
           ADD WS-COST-AMT     TO WS-COST-TOT
           ADD WS-LINECNT      TO WS-LINE-TOT
           MOVE 0 TO WS-SALE-AMT
                     WS-COST-AMT
                     WS-LINECNT
                     WS-MARGIN
                     WS-PERC.
           IF WS-LINE > 56
               PERFORM PRINT-HEADINGS
               GO TO SL-999.
      *     IF WS-TOT-ONLY NOT = "Y"
      *         GO TO SL-999.
           IF WS-SPECIALS-ST1 NOT = 10
            IF WS-PRINT-ONLY-SB = " "
              WRITE PRINT-REC FROM HEAD3 AFTER 2
              WRITE PRINT-REC FROM HEAD4 AFTER 1
              MOVE " " TO PRINT-REC
              ADD 3 TO WS-LINE.
           MOVE "Y" TO  WS-LINE-ANSWER.
       SL-999.
           EXIT.
      *
       PRINT-TOTALS SECTION.
       PT-010.
           MOVE WS-SALE-TOT TO P-TOT-SALE
           MOVE WS-COST-TOT TO P-TOT-COST
           COMPUTE WS-MARGIN = WS-SALE-TOT - WS-COST-TOT
           MOVE WS-MARGIN   TO P-TOT-MARGIN
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / WS-COST-TOT) * 100
           MOVE WS-PERC     TO P-TOT-PERC
           MOVE WS-LINE-TOT TO P-TOT-CNT
           WRITE PRINT-REC FROM TOT-TOTAL-LINE AFTER 2
           ADD 2 TO WS-LINE.
       PT-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-060.
           OPEN I-O SPECIALS-FILE.
           IF WS-SPECIALS-ST1 = 23 OR 35 OR 49
               MOVE "NO SPECIALS-FILE TO OPEN I-O, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SPECIALS-ST1
               OPEN OUTPUT SPECIALS-FILE
           IF WS-SPECIALS-ST1 NOT = 0
               MOVE "SPECIALS-FILE BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SPECIALS-ST1
               GO TO OPEN-060.
           IF WS-SPECIALS-ST1 NOT = 0
               MOVE "SPECIALS-FILE BUSY ON OPEN I-O, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SPECIALS-ST1
               GO TO OPEN-060.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE SPECIALS-FILE.
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
