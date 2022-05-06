        IDENTIFICATION DIVISION.
        PROGRAM-ID. StReLoRp.
        AUTHOR.   CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStReceipt".
         Copy "SelectStReceiptLy".
         Copy "SelectStMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStkReceipts.
           COPY ChlfdStkReceiptsLy.
           COPY ChlfdStock.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  PAGE-CNT             PIC 9(2) VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-YEAR              PIC X VALUE "T".
       77  WS-MONTHEND          PIC X VALUE "N".
       77  WS-CODE              PIC X VALUE " ".
       77  WS-ACCEPT-DATE       PIC X(10) VALUE " ".
       77  WS-VALIDDATE         PIC 9(8) VALUE 0.
       77  WS-TOTAL-PRICE       PIC S9(8)V99 VALUE 0.
       77  WS-RECEIPT-TOTAL     PIC S9(8)V99 VALUE 0.
       77  WS-ADJ-TOTAL         PIC S9(8)V99 VALUE 0.
       77  WS-GRV-TOTAL         PIC S9(8)V99 VALUE 0.
       77  WS-SALES-TOTAL       PIC S9(8)V99 VALUE 0.
       77  WS-ORDERS-TOTAL      PIC S9(8)V99 VALUE 0.
       77  WS-KITADJ-TOTAL      PIC S9(8)V99 VALUE 0.
       77  WS-MV-TOTAL          PIC S9(8)V99 VALUE 0.
       01  WS-STKRECEIPT-STATUS.
           03  WS-STKRECEIPT-ST1    PIC 99.
       01  WS-STKRECEIPTSLY-STATUS.
           03  WS-STKRECEIPTSLY-ST1 PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1         PIC 99.
       01  HEAD1.
           03  FILLER          PIC X(7) VALUE "  DATE".
           03  H1-DATE         PIC X(10).
           03  FILLER          PIC X(12) VALUE " ".
           03  FILLER          PIC X(80) VALUE
           "S T O C K   R E C E I P T S   A U D I T   T R A I L".
           03  FILLER          PIC X(5) VALUE "PAGE:".
           03  H1-PAGE         PIC ZZ9.
           03  FILLER          PIC X(8) VALUE " ".
       01  HEAD2.
           03  FILLER          PIC X(29) VALUE " ".
           03  FILLER          PIC X(51) VALUE ALL "*".
           03  FILLER          PIC X(52) VALUE " ".
       01  HEAD3.
           03  FILLER          PIC X(15) VALUE "TRANSACTION".
           03  FILLER          PIC X(20) VALUE "STOCK".
           03  FILLER          PIC X(14) VALUE "QTY".
           03  FILLER          PIC X(83) VALUE "P R I C E S".
       01  HEAD4.
           03  FILLER          PIC X(15) VALUE "NUMBER CODE".
           03  FILLER          PIC X(20) VALUE "NUMBER".
           03  FILLER          PIC X(11) VALUE " ".
           03  FILLER          PIC X(11) VALUE "UNIT".
           03  FILLER          PIC X(9) VALUE "TOTAL".
           03  FILLER          PIC X(27) VALUE "REFERENCE NUMBER".
           03  FILLER          PIC X(10) VALUE " DATE".
           03  FILLER          PIC X(32) VALUE "ORDER NUMBER".
       01  DETAIL-LINE.
           03  D-TRANSNO       PIC Z(5)9.
           03  FILLER          PIC X(3) VALUE " ".
           03  D-TRANSCODE     PIC 9.
           03  FILLER          PIC X(5) VALUE " ".
           03  D-STOCKNO       PIC X(17).
           03  D-QTY           PIC Z(4)9-.
           03  FILLER          PIC X(1) VALUE " ".
           03  D-UNIT          PIC Z(7)9.99 BLANK WHEN ZERO.
           03  FILLER          PIC X(1) VALUE " ".
           03  D-TOTAL         PIC Z(7)9.99 BLANK WHEN ZERO.
           03  FILLER          PIC X(4) VALUE " ".
           03  D-REFNO         PIC X(25).
           03  D-REFDATE       PIC X(10).
           03  FILLER          PIC X(2) VALUE " ".
           03  D-ORDER-NO      PIC X(32).
       01  TOTAL-LINE.
           03  FILLER          PIC X(31) VALUE " ".
           03  T-DESC          PIC X(20) VALUE " ".
           03  T-TOTAL         PIC Z(7)9.99-.
      *
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "** STOCK RECEIPTS PRINT REPORT **" AT POS
           MOVE 415 TO POS
           DISPLAY "*********************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-009.
           MOVE 810 TO POS.
           DISPLAY "IS THIS A MONTH/YEAR END RUN, Y OR N:[ ]" AT POS
           ADD 38 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-MONTHEND.

           IF WS-MONTHEND NOT = "Y" AND NOT = "N"
                GO TO CONTROL-009.
           IF W-ESCAPE-KEY = 4
                GO TO CONTROL-005.
           IF WS-MONTHEND = "Y"
              PERFORM GET-SYSTEM-Y2K-DATE
              MOVE 01 TO WS-DD
              MOVE WS-DD TO CONVERT-DD
              MOVE WS-MM TO CONVERT-MM
              MOVE WS-YY TO CONVERT-YY
              MOVE CONVERT-DATE TO WS-ACCEPT-DATE.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO CONTROL-010
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO CONTROL-009.
       CONTROL-010.
           MOVE 1010 TO POS.
           DISPLAY "ENTER A STARTING DATE TO PRINT FROM :[          ]"
               AT POS.  
           ADD 38 TO POS.

           MOVE WS-ACCEPT-DATE TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT-DATE.

           IF W-ESCAPE-KEY = 4
                GO TO CONTROL-009.
           MOVE WS-ACCEPT-DATE TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
               GO TO CONTROL-010.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           DISPLAY DISPLAY-DATE AT POS.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-VALIDDATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO CONTROL-010.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO CONTROL-015
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO CONTROL-010.
       CONTROL-015.
           MOVE 1410 TO POS.
           DISPLAY "ENTER A=ADJUSTMENTS, R=RECEIPTS, O=ORDERS, G=GRV."
              AT POS.
           MOVE 1210 TO POS.
           DISPLAY "ENTER A CODE TO PRINT, BLANK FOR ALL:[ ]" AT POS.  
           ADD 38 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CODE.

           IF W-ESCAPE-KEY = 4
                GO TO CONTROL-010.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO CONTROL-017
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO CONTROL-015.
       CONTROL-017.
           MOVE 1610 TO POS.
           DISPLAY "SHOULD THIS BE T=THIS YEAR, L=LAST YEAR: [ ]"
              AT POS.
           ADD 42 TO POS.

           MOVE 'T'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-YEAR.

           IF W-ESCAPE-KEY = 4
                GO TO CONTROL-010.
           IF WS-YEAR NOT = "T" AND NOT = "L"
                DISPLAY " " AT 3079 WITH BELL
                GO TO CONTROL-017.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO CONTROL-025
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO CONTROL-017.
       CONTROL-025.
           MOVE 2815 TO POS.
           DISPLAY "The report is being compiled........" AT POS.
           PERFORM OPEN-FILES.
           IF WS-YEAR = "T"
              PERFORM PRINT-ROUTINE
           ELSE
              PERFORM PRINT-LY-ROUTINE.
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE WS-VALIDDATE TO STRE-REFERENCE-DATE
                                WS-AGE-DATE.
           START STKRECEIPTS-FILE KEY NOT < STRE-REFERENCE-DATE
               INVALID KEY NEXT SENTENCE.
           IF WS-STKRECEIPT-ST1 NOT = 0
               PERFORM PRR-006
               MOVE "** STARTING DATE TOO HIGH, NOTHING TO PRINT. **"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 2
               GO TO PRR-999.
       PRR-002.
           READ STKRECEIPTS-FILE NEXT
              AT END
               GO TO PRR-999.
           IF WS-CODE = " "
               GO TO PRR-003.
           IF WS-CODE = "A"
            IF STRE-TRANSACTION-CODE NOT = 5 AND NOT = 8 AND NOT = 9
               GO TO PRR-002.
           IF WS-CODE = "O"
            IF STRE-TRANSACTION-CODE NOT = 3
               GO TO PRR-002.
           IF WS-CODE = "R"
            IF STRE-TRANSACTION-CODE NOT = 1 AND NOT = 2
               GO TO PRR-002.
           IF WS-CODE = "G"
            IF STRE-TRANSACTION-CODE NOT = 4
               GO TO PRR-002.
       PRR-003.
      * THIS NEW SECTION ADDED TO THE PRINT SO THAT ONLY THAT MONTH
      * WILL PRINT IF SELECTING A PRINT AT ANOTHER TIME OTHER THAN
      * MONTH/YEAR-END.  ADDED 1/5/2002.
           MOVE STRE-REFERENCE-DATE TO SPLIT-DATE.
           IF SPLIT-MM NOT = WS-AGE-MM
               GO TO PRR-999.
           IF SPLIT-YY NOT = WS-AGE-YY
               GO TO PRR-999.
       PRR-005.
           IF LINE-CNT < 60
               GO TO PRR-010.
       PRR-006.
           ADD 1 TO PAGE-CNT
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
           MOVE 7 TO LINE-CNT.
       PRR-010.
           IF STRE-TRANSACTION-CODE = 5 OR = 9
               MOVE STRE-STOCK-NUMBER TO ST-STOCKNUMBER
               PERFORM READ-STOCK
               MOVE ST-AVERAGECOST TO STRE-UNIT-PRICE
               COMPUTE STRE-TOTAL-PRICE = 
                 STRE-UNIT-PRICE * STRE-QUANTITY.

           MOVE STRE-TRANSACTION-NUMBER TO D-TRANSNO
           MOVE STRE-TRANSACTION-CODE   TO D-TRANSCODE
           MOVE STRE-STOCK-NUMBER       TO D-STOCKNO
           MOVE STRE-QUANTITY           TO D-QTY
           MOVE STRE-UNIT-PRICE         TO D-UNIT
           MOVE STRE-TOTAL-PRICE        TO D-TOTAL
           MOVE STRE-REFERENCE-NO       TO D-REFNO
           MOVE STRE-REFERENCE-DATE     TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-REFDATE
           MOVE STRE-ORDER-NUMBER       TO D-ORDER-NO
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           
           IF STRE-TRANSACTION-CODE = 1 OR = 2
              ADD STRE-TOTAL-PRICE TO WS-RECEIPT-TOTAL.
           IF STRE-TRANSACTION-CODE = 3
              ADD STRE-TOTAL-PRICE TO WS-ORDERS-TOTAL.
           IF STRE-TRANSACTION-CODE = 4
              ADD STRE-TOTAL-PRICE TO WS-GRV-TOTAL.
           IF STRE-TRANSACTION-CODE = 5
            IF STRE-QUANTITY > 0
              ADD STRE-TOTAL-PRICE TO WS-ADJ-TOTAL
            ELSE
              SUBTRACT STRE-TOTAL-PRICE FROM WS-ADJ-TOTAL.
           IF STRE-TRANSACTION-CODE = 6
              ADD STRE-TOTAL-PRICE TO WS-SALES-TOTAL.
           IF STRE-TRANSACTION-CODE = 9
            IF STRE-QUANTITY > 0
              ADD STRE-TOTAL-PRICE TO WS-KITADJ-TOTAL
            ELSE
              SUBTRACT STRE-TOTAL-PRICE FROM WS-KITADJ-TOTAL.

           MOVE 2510 TO POS
           DISPLAY "CURRENT DATE BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY DISPLAY-DATE AT POS.
           
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       PRINT-LY-ROUTINE SECTION.
       PRLY-000.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE WS-VALIDDATE TO STRELY-REFERENCE-DATE.
           START STKRECEIPTSLY-FILE KEY NOT < STRELY-REFERENCE-DATE
               INVALID KEY NEXT SENTENCE.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
               PERFORM PRLY-006
               MOVE "** STARTING DATE TOO HIGH, NOTHING TO PRINT. **"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 2
               GO TO PRLY-999.
       PRLY-002.
           READ STKRECEIPTSLY-FILE NEXT
              AT END
               GO TO PRLY-999.
           IF WS-CODE = " "
               GO TO PRLY-005.
           IF WS-CODE = "A"
            IF STRELY-TRANSACTION-CODE NOT = 5 AND NOT = 8 AND NOT = 9
               GO TO PRLY-002.
           IF WS-CODE = "O"
            IF STRELY-TRANSACTION-CODE NOT = 3
               GO TO PRLY-002.
           IF WS-CODE = "R"
            IF STRELY-TRANSACTION-CODE NOT = 1 AND NOT = 2
               GO TO PRLY-002.
           IF WS-CODE = "G"
            IF STRELY-TRANSACTION-CODE NOT = 4
               GO TO PRLY-002.
       PRLY-005.
           IF LINE-CNT < 60
               GO TO PRLY-010.
       PRLY-006.
           ADD 1 TO PAGE-CNT
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
           MOVE 7 TO LINE-CNT.
       PRLY-010.
           IF STRELY-TRANSACTION-CODE = 5 OR = 9
               MOVE STRELY-STOCK-NUMBER TO ST-STOCKNUMBER
               PERFORM READ-STOCK
               MOVE ST-AVERAGECOST TO STRELY-UNIT-PRICE
               COMPUTE STRELY-TOTAL-PRICE =
                     STRELY-UNIT-PRICE * STRELY-QUANTITY.
           MOVE STRELY-TRANSACTION-NUMBER TO D-TRANSNO
           MOVE STRELY-TRANSACTION-CODE   TO D-TRANSCODE
           MOVE STRELY-STOCK-NUMBER       TO D-STOCKNO
           MOVE STRELY-QUANTITY           TO D-QTY
           MOVE STRELY-UNIT-PRICE         TO D-UNIT
           MOVE STRELY-TOTAL-PRICE        TO D-TOTAL
           MOVE STRELY-REFERENCE-NO       TO D-REFNO
           MOVE STRELY-REFERENCE-DATE     TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE              TO D-REFDATE
           MOVE STRELY-ORDER-NUMBER       TO D-ORDER-NO
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           
           
           IF STRELY-TRANSACTION-CODE = 1 OR = 2
              ADD STRELY-TOTAL-PRICE TO WS-RECEIPT-TOTAL.
           IF STRELY-TRANSACTION-CODE = 3
              ADD STRELY-TOTAL-PRICE TO WS-ORDERS-TOTAL.
           IF STRELY-TRANSACTION-CODE = 4
              ADD STRELY-TOTAL-PRICE TO WS-GRV-TOTAL.
           IF STRELY-TRANSACTION-CODE = 5
              ADD STRELY-TOTAL-PRICE TO WS-ADJ-TOTAL.
           IF STRELY-TRANSACTION-CODE = 6
              ADD STRELY-TOTAL-PRICE TO WS-SALES-TOTAL.
           IF STRELY-TRANSACTION-CODE = 9
              ADD STRELY-TOTAL-PRICE TO WS-KITADJ-TOTAL.
           
           MOVE 2510 TO POS
           DISPLAY "CURRENT DATE BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY DISPLAY-DATE AT POS.
           
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRLY-002.
       PRLY-999.
           EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
             START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-010.
             READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                 MOVE " " TO ST-DESCRIPTION2
                 MOVE "** UNKNOWN NUMBER **" TO ST-DESCRIPTION1
                 MOVE 0                      TO ST-AVERAGECOST
                 GO TO R-ST-999.
             IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK-ST1
                MOVE STRE-STOCK-NUMBER TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-ST-010.
       R-ST-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-030.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
           IF WS-YEAR = "L"
              GO TO OPEN-035.
           OPEN I-O STKRECEIPTS-FILE.
           IF WS-STKRECEIPT-ST1 NOT = 0 
              MOVE 0 TO WS-STKRECEIPT-ST1
              MOVE "RECEIPT FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-030.
           GO TO OPEN-040.
       OPEN-035.
           OPEN I-O STKRECEIPTSLY-FILE.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
              MOVE "ST-RECEIPTSLY FILE BUSY ON OPEN, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-STKRECEIPTSLY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-035.
       OPEN-040.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 60
               PERFORM PRR-006.
           
           MOVE WS-RECEIPT-TOTAL     TO T-TOTAL
           MOVE "VALUE OF RECEIPTS +:" TO T-DESC
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.

           MOVE WS-ORDERS-TOTAL      TO T-TOTAL
           MOVE "VALUE OF ORDERS    :" TO T-DESC
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.

           MOVE WS-GRV-TOTAL         TO T-TOTAL
           MOVE "VALUE OF GRV'S    -:" TO T-DESC
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.

           MOVE WS-ADJ-TOTAL         TO T-TOTAL
           MOVE "VALUE OF ADJ.   +/-:" TO T-DESC
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.

           MOVE WS-SALES-TOTAL       TO T-TOTAL
           MOVE "VALUE OF SALES    -:" TO T-DESC
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.

           MOVE WS-KITADJ-TOTAL      TO T-TOTAL
           MOVE "VALUE OF KITADJ.+/-:" TO T-DESC
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.

           COMPUTE WS-MV-TOTAL = WS-RECEIPT-TOTAL - WS-GRV-TOTAL
                 + WS-ADJ-TOTAL + WS-KITADJ-TOTAL - WS-SALES-TOTAL.
           MOVE WS-MV-TOTAL          TO T-TOTAL
           MOVE "MOVEMENT IN STOCK:" TO T-DESC
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           
           
           IF WS-CODE = " "
               MOVE "ALL CODES PRINTED FOR THIS RUN" TO PRINT-REC
               WRITE PRINT-REC AFTER 2.
           IF WS-CODE = "A"
               MOVE "ONLY 'ADJUSTMENT' CODES PRINTED FOR THIS RUN"
                TO PRINT-REC
               WRITE PRINT-REC AFTER 2.
           IF WS-CODE = "G"
               MOVE "ONLY 'GRV' CODES PRINTED FOR THIS RUN"
                TO PRINT-REC
               WRITE PRINT-REC AFTER 2.
           IF WS-CODE = "R"
               MOVE "ONLY 'RECEIPTS' CODES PRINTED FOR THIS RUN"
                TO PRINT-REC
               WRITE PRINT-REC AFTER 2.
           IF WS-CODE = "O"
               MOVE "ONLY 'ORDERS PLACED' CODES PRINTED FOR THIS RUN"
                TO PRINT-REC
               WRITE PRINT-REC AFTER 2.
           IF WS-YEAR = "T"
               MOVE "THIS YEAR INFO PRINTED FOR THIS RUN" TO PRINT-REC
               WRITE PRINT-REC AFTER 2
           ELSE
               MOVE "LAST YEAR INFO PRINTED FOR THIS RUN" TO PRINT-REC
               WRITE PRINT-REC AFTER 2.
               
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           IF WS-YEAR = "T"
              CLOSE STKRECEIPTS-FILE
           ELSE
              CLOSE STKRECEIPTSLY-FILE.
           CLOSE STOCK-MASTER.
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
      * END-OF-JOB
