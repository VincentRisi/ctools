        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlSoByRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        REPOSITORY. 
           FUNCTION ALL INTRINSIC.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectSlSoldBy".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdSoldBy.

       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-MESSAGE-PART      PIC X(79) VALUE " ".
       77  WS-MESS              PIC X VALUE " ".
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  LINE-CNT-ANSWER      PIC X VALUE " ".
       77  WS-LINECNT           PIC S9(3) VALUE 0.
       77  WS-LINE-TOT          PIC 9(4) VALUE 0.
       77  PRINT-SB             PIC XX VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-ACCEPT            PIC X(10) VALUE " ".
       77  WS-PRINT-ALL         PIC X VALUE " ".
       77  WS-DETAIL            PIC X VALUE " ".
       77  WS-PRINT-TOT         PIC X VALUE " ".
       77  WS-SALE              PIC S9(7)V99 VALUE 0.
       77  WS-COST              PIC S9(7)V99 VALUE 0.
       77  WS-SALE-AMT          PIC S9(7)V99 VALUE 0.
       77  WS-COST-AMT          PIC S9(7)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(7)V99 VALUE 0.
       77  WS-PERC              PIC S9(3)V999 VALUE 0.
       77  WS-SALE-TOT          PIC S9(7)V99 VALUE 0.
       77  WS-COST-TOT          PIC S9(7)V99 VALUE 0.
       01  WS-SOLDBY-STATUS.
           03  WS-SOLDBY-ST1    PIC 99.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE "  DATE".
           03  H1-DATE          PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
           03  FILLER           PIC X(50) VALUE 
           "S O L D   B Y   I N V O I C E   R E P O R T".
           03  FILLER           PIC X(6) VALUE "PAGE: ".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(14) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(19) VALUE " ".
           03  FILLER           PIC X(93) VALUE 
           "*******************************************".
       01  HEAD3.
           03  FILLER           PIC X(8) VALUE "SoldBy: ".
           03  P-SOLDBY         PIC XX.
           03  FILLER           PIC X(70) VALUE " ".
       01  HEAD4.
           03  FILLER           PIC X(80) VALUE "**********".
       01  HEAD5.
           03  H5-LINE.
              05  FILLER        PIC X(8) VALUE "  Inv".
              05  FILLER        PIC X(7) VALUE "Acc No".
              05  FILLER        PIC X(16) VALUE "Acc Name".
              05  FILLER        PIC X(11) VALUE "Inv Date".
           03  FILLER           PIC X(11) VALUE "Sale Amt".
           03  FILLER           PIC X(12) VALUE "Cost Amt".
           03  FILLER           PIC X(31) VALUE "Margin   Perc".
       01  DETAIL-LINE.
           03  P-INVOICE        PIC Z(6).
           03  FILLER           PIC X(1) VALUE " ".
           03  P-ACCOUNT        PIC X(8) VALUE " ".
           03  P-NAME           PIC X(14) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  P-DATE           PIC X(10).
           03  P-SALE           PIC Z(6)9.99-.
           03  P-COST           PIC Z(6)9.99.
           03  P-MARGIN         PIC Z(6)9.99-.
           03  P-PERC           PIC Z(2)9.99-.
       01  SUB-TOTAL-LINE.
           03  FILLER           PIC X(16) VALUE "No. Of Orders:".
           03  P-SUB-CNT        PIC Z(3)9-.
           03  FILLER           PIC X(9) VALUE " ".
           03  FILLER           PIC X(10) VALUE "Totals :R".
           03  P-SUB-SALE       PIC Z(6)9.99-.
           03  P-SUB-COST       PIC Z(6)9.99.
           03  P-SUB-MARGIN     PIC Z(6)9.99-.
           03  P-SUB-PERC       PIC Z(2)9.99-.
           03  FILLER           PIC X(1) VALUE "%".
       01  TOT-TOTAL-LINE.
           03  FILLER           PIC X(15) VALUE 
           " Total Orders:".
           03  P-TOT-CNT        PIC Z(3)9-.
           03  FILLER           PIC X(10) VALUE " ".
           03  FILLER           PIC X(10) VALUE "Totals :R".
           03  P-TOT-SALE       PIC Z(6)9.99-.
           03  P-TOT-COST       PIC Z(6)9.99.
           03  P-TOT-MARGIN     PIC Z(6)9.99-.
           03  P-TOT-PERC       PIC Z(2)9.99-.
           03  FILLER           PIC X(1) VALUE "%".
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
           DISPLAY "***** Sold By Invoice Report *****" AT POS
           MOVE 420 TO POS
           DISPLAY "**********************************" AT POS.
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
           MOVE CDA-DATA TO PRINT-SB.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF PRINT-SB = " " 
              MOVE "Y" TO WS-PRINT-ALL.
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
           MOVE CDA-DATA TO WS-PRINT-TOT.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-030.
           IF WS-PRINT-TOT NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
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
       PRR-000.
           IF WS-PRINT-ALL NOT = "Y"
               MOVE PRINT-SB TO SB-TYPE
               MOVE 0        TO SB-INVOICE-NUMBER
           ELSE
               MOVE " " TO SB-TYPE
               MOVE 0   TO SB-INVOICE-NUMBER.
               
           START SOLD-BY KEY NOT < SB-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-SOLDBY-ST1 NOT = 0
               MOVE 88 TO WS-SOLDBY-ST1
               PERFORM PRINT-HEADINGS
               PERFORM SUB-LINE
               PERFORM PRINT-TOTALS
               GO TO PRR-999.
       PRR-001.
           READ SOLD-BY NEXT
               AT END NEXT SENTENCE.
           IF WS-SOLDBY-ST1 = 10
               PERFORM SUB-LINE
               PERFORM PRINT-TOTALS
               GO TO PRR-999.
           IF WS-SOLDBY-ST1 NOT = 0
               MOVE "SOLD BY FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SOLDBY-ST1
               GO TO PRR-001.
       PRR-005.
           IF PRINT-SB = " "
               GO TO PRR-010.
           IF WS-PRINT-ALL NOT = "Y"
            IF SB-TYPE NOT = PRINT-SB 
               PERFORM SUB-LINE
               GO TO PRR-999.
       PRR-010.
           IF LINE-CNT > 60
               MOVE "N" TO LINE-CNT-ANSWER
               PERFORM PRINT-HEADINGS.
       PRR-020.
           IF SB-TYPE NOT = PRINT-SB
             IF PRINT-SB NOT = " "
               PERFORM SUB-LINE.
       PRR-030.
           IF WS-PRINT-TOT = "Y"
               GO TO PRR-033.
           MOVE SB-INVOICE-NUMBER  TO P-INVOICE
           MOVE SB-ACCOUNT-NUMBER  TO P-ACCOUNT
           MOVE SB-ACCOUNT-NAME    TO P-NAME
           MOVE SB-DATE-OF-INVOICE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO P-DATE.
           IF SB-TRANS = "6"
              COMPUTE WS-SALE = SB-SALE-AMOUNT * -1
              MOVE WS-SALE         TO P-SALE
              COMPUTE WS-COST = SB-COST-AMOUNT * -1
              MOVE WS-COST         TO P-COST
           ELSE
              MOVE SB-SALE-AMOUNT  TO P-SALE
              MOVE SB-COST-AMOUNT  TO P-COST.
           COMPUTE WS-MARGIN = SB-SALE-AMOUNT - SB-COST-AMOUNT.
           IF SB-TRANS = "6"
              COMPUTE WS-MARGIN = WS-MARGIN * -1.
           MOVE WS-MARGIN          TO P-MARGIN
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / SB-COST-AMOUNT) * 100
           MOVE WS-PERC            TO P-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           ADD 1 TO LINE-CNT
           MOVE 0 TO WS-MARGIN WS-PERC P-COST P-SALE.
       PRR-033.
           IF SB-TRANS = "6"
               SUBTRACT SB-SALE-AMOUNT FROM WS-SALE-AMT
               SUBTRACT SB-COST-AMOUNT FROM WS-COST-AMT
           ELSE
               ADD SB-SALE-AMOUNT TO WS-SALE-AMT
               ADD SB-COST-AMOUNT TO WS-COST-AMT.
       PRR-035.
           IF SB-TRANS = "1"
              ADD 1 TO WS-LINECNT.
           IF SB-TRANS = "6"
              SUBTRACT 2 FROM WS-LINECNT.
           IF PRINT-SB = " " 
               MOVE SB-TYPE TO PRINT-SB.
              
           GO TO PRR-001.
       PRR-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           IF LINE-CNT-ANSWER = "Y"
               MOVE SB-TYPE TO P-SOLDBY.
           IF WS-PAGE = 0
               MOVE SB-TYPE TO P-SOLDBY.
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
           WRITE PRINT-REC FROM HEAD1 AFTER 1.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC.
           
           IF WS-PRINT-TOT = "Y"
              MOVE " " TO H5-LINE
              WRITE PRINT-REC FROM HEAD5 AFTER 1
              MOVE " " TO PRINT-REC
           ELSE
              WRITE PRINT-REC FROM HEAD5 AFTER 1
              MOVE " " TO PRINT-REC.
              
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD4 AFTER 1.
           MOVE " " TO PRINT-REC.

           MOVE 7 TO LINE-CNT.
        PH-999.
           EXIT.
      *
       SUB-LINE SECTION.
       SL-000.
           MOVE WS-SALE-AMT TO P-SUB-SALE
           MOVE WS-COST-AMT TO P-SUB-COST
           COMPUTE WS-MARGIN = WS-SALE-AMT - WS-COST-AMT
           MOVE WS-MARGIN   TO P-SUB-MARGIN
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / WS-COST-AMT) * 100
           MOVE WS-PERC     TO P-SUB-PERC
           MOVE WS-LINECNT  TO P-SUB-CNT
           WRITE PRINT-REC FROM SUB-TOTAL-LINE AFTER 1
           ADD 2 TO LINE-CNT.
       SL-010.
           MOVE SB-TYPE    TO P-SOLDBY PRINT-SB
           ADD WS-SALE-AMT TO WS-SALE-TOT
           ADD WS-COST-AMT TO WS-COST-TOT
           ADD WS-LINECNT  TO WS-LINE-TOT
           MOVE 0 TO WS-SALE-AMT
                     WS-COST-AMT
                     WS-LINECNT
                     WS-MARGIN
                     WS-PERC.
       SL-020.
           IF LINE-CNT > 56
               PERFORM PRINT-HEADINGS
               GO TO SL-999.
           IF WS-PRINT-ALL NOT = "Y"
               GO TO SL-999.
           IF WS-SOLDBY-ST1 NOT = 10
              WRITE PRINT-REC FROM HEAD3 AFTER 1
              WRITE PRINT-REC FROM HEAD4 AFTER 1
              MOVE " " TO PRINT-REC
              ADD 2 TO LINE-CNT.
           MOVE "Y" TO  LINE-CNT-ANSWER.
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
           ADD 2 TO LINE-CNT.
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
           OPEN I-O SOLD-BY.
           IF WS-SOLDBY-ST1 NOT = 0
               MOVE "SOLD BY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SOLDBY-ST1
               GO TO OPEN-060.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-010.
           IF LINE-CNT > 56
             PERFORM PRINT-HEADINGS.
       END-020.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           
      *     MOVE WS-PRINTERNUMBER (21) TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
           IF WS-PRINTERNUMBER (21) NOT = 20 AND NOT = 0
                PERFORM SEND-REPORT-TO-PRINTER
                GO TO END-900.
           IF WS-PRINTERNUMBER (21) = 0
                GO TO END-900.
           
            MOVE "When Finished Viewing The Report, Press Q to Quit."
              TO WS-MESSAGE
            PERFORM ERROR-MESSAGE. 
              
            MOVE 
            CONCATENATE('less ', ' ', TRIM(WS-PRINTER))
                TO WS-COMMAND-LINE.
      
            CALL "SYSTEM" USING WS-COMMAND-LINE.
      *      RETURNING W-STATUS.
       END-900.
           CLOSE SOLD-BY.
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
