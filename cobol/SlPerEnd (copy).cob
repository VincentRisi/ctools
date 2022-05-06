        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlPerEnd.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectDrMaster".
          Copy "SelectStMaster".
          Copy "SelectDrTrans".
          Copy "SelectStTrans".
          Copy "SelectStTransLy".
          Copy "SelectStOrders".
          Copy "SelectSlMaster".
          Copy "SelectSlParameter".
          Copy "SelectSlDistributions".
          Copy "SelectStImports".
          Copy "SelectSlSoldBy".
          Copy "SelectSlRegister".
          Copy "SelectSlRegLy".
          Copy "SelectStReceipt".
          Copy "SelectStReceiptLy".
          Copy "SelectGlMaster".
          Copy "SelectGlTrans".
          Copy "SelectGlParameter".
          Copy "SelectSlDaily".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdStock.
           COPY ChlfdDrTrans.
           COPY ChlfdStTrans.
           COPY ChlfdStTransLy.
           COPY ChlfdOutOrd.
           COPY ChlfdSales.
           COPY ChlfdParam.
           COPY ChlfdDisTot.
           COPY ChlfdImpReceipts.
           COPY ChlfdSoldBy.
           COPY ChlfdRegister.
           COPY ChlfdRegisterLy.
           COPY ChlfdStkReceipts.
           COPY ChlfdStkReceiptsLy.
           COPY ChlfdGlMast.
           COPY ChlfdGlTrans.
           COPY ChlfdGlParam.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  WS-STOP-UPDATE       PIC X VALUE " ".
       77  WS-PERIOD            PIC 99 VALUE 0.
       77  WS-ORDERS            PIC 9(5).
       77  WS-INTERNAL-ORDERS   PIC 9(5).
       77  WS-NETT-ORDERS       PIC 9(5).
       77  WS-NAME-CHECK        PIC X(16) VALUE " ".
       77  WS-ANSWER1           PIC X VALUE " ".
       77  WS-ANSWER2           PIC X VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
       77  WS-ANSWER4           PIC X VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-CALC-WW           PIC 9(2) VALUE 0.
       77  WS-CALC-MM           PIC 9(2) VALUE 0.
       77  WS-WORK-MM           PIC 9(4) VALUE 0.
       77  WS-WORK-PO-MM        PIC 9(4) VALUE 0.
       77  PSW-SUB1             PIC S9(5) VALUE 0.
       77  PSW-SUB2             PIC S9(5) VALUE 0.
       01  SPLIT-STOCK.
           03  SP-1STCHAR      PIC X.
           03  SP-REST         PIC X(14).
       01  W-READ-KEY           PIC X(20).
       01  SPLIT-TIME.
           03  SPLIT-HR         PIC 99.
           03  SPLIT-FIL1       PIC X.
           03  SPLIT-MN         PIC 99.
           03  SPLIT-FIL2       PIC X.
           03  SPLIT-SC         PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST     PIC X(20) VALUE " ".
           03  WS-DAILY-2ND     PIC X(20) VALUE " ".
           03  WS-DAILY-3RD     PIC X(20) VALUE " ".
           03  WS-DAILY-4TH     PIC X(20) VALUE " ".
       01  WS-PORDER-DATE.
           03  WS-PO-YY         PIC 9999.
           03  WS-PO-MM         PIC 99.
           03  WS-PO-DD         PIC 99.
       01  WS-ALT-KEY.
           03  WS-SUPPLIER      PIC X(5) VALUE " ".
           03  WS-INVOICENUM    PIC X(10) VALUE " ".
      * 01  WS-PASSWORD-KEY.
      *     03  WS-PA-KEY         PIC X OCCURS 11.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-RESTOFACCOUNT    PIC X(6).
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1         PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1          PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1        PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1        PIC 99.
       01  WS-STTRANSLY-STATUS.
           03  WS-STTRANSLY-ST1      PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1         PIC 99.
       01  WS-SALES-STATUS.
           03  WS-SALES-ST1          PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1    PIC 99.
       01  WS-DISTRIBUTION-STATUS.
           03  WS-DISTRIBUTION-ST1   PIC 99.
       01  WS-IMPRECEIPT-STATUS.
           03  WS-IMPRECEIPT-ST1     PIC 99.
       01  WS-SOLDBY-STATUS.
           03  WS-SOLDBY-ST1         PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1           PIC 99.
       01  WS-INCR-LY-STATUS.
           03  WS-INCR-LY-ST1        PIC 99.
       01  WS-STKRECEIPT-STATUS.
           03  WS-STK-ST1            PIC 99.
       01  WS-STKRECEIPTSLY-STATUS.
           03  WS-STKLY-ST1          PIC 99.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1         PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1        PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1    PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1          PIC 99.
       01  WS-MONTH-DESCRIPTIONS.
           03  FILLER          PIC X(4) VALUE "MAR ".
           03  FILLER          PIC X(4) VALUE "APR ".
           03  FILLER          PIC X(4) VALUE "MAY ".
           03  FILLER          PIC X(4) VALUE "JUN ".
           03  FILLER          PIC X(4) VALUE "JUL ".
           03  FILLER          PIC X(4) VALUE "AUG ".
           03  FILLER          PIC X(4) VALUE "SEP ".
           03  FILLER          PIC X(4) VALUE "OCT ".
           03  FILLER          PIC X(4) VALUE "NOV ".
           03  FILLER          PIC X(4) VALUE "DEC ".
           03  FILLER          PIC X(4) VALUE "JAN ".
           03  FILLER          PIC X(4) VALUE "FEB ".
       01  WS-MONTH-DESCRED REDEFINES WS-MONTH-DESCRIPTIONS.
           03  WS-MONTH-DESC   PIC X(4) OCCURS 12.
       01  WS-DR-MONTH-DESCRIPTIONS.
           03  FILLER          PIC X(10) VALUE "  JANUARY ".
           03  FILLER          PIC X(10) VALUE " FEBRUARY ".
           03  FILLER          PIC X(10) VALUE "    MARCH ".
           03  FILLER          PIC X(10) VALUE "    APRIL ".
           03  FILLER          PIC X(10) VALUE "      MAY ".
           03  FILLER          PIC X(10) VALUE "     JUNE ".
           03  FILLER          PIC X(10) VALUE "     JULY ".
           03  FILLER          PIC X(10) VALUE "   AUGUST ".
           03  FILLER          PIC X(10) VALUE "SEPTEMBER ".
           03  FILLER          PIC X(10) VALUE "  OCTOBER ".
           03  FILLER          PIC X(10) VALUE " NOVEMBER ".
           03  FILLER          PIC X(10) VALUE " DECEMBER ".
       01  WS-DR-MONTH-DESCRED REDEFINES WS-DR-MONTH-DESCRIPTIONS.
           03  WS-DR-MONTH-DESC   PIC X(10) OCCURS 12.
       01  JOURNAL-DATA.
           03  WS-JRN.
               05  WS-JRN-1STCHAR   PIC X(2) VALUE "DR".
               05  WS-JRN-MONTH     PIC X(4).
               05  WS-JRN-WEEK      PIC X(2) VALUE "WK".
               05  WS-JRN-WW        PIC X(2).
       01  WS-DISTRIBUTIONS.
           03  WS-BATCH-TOTAL           PIC S9(8)V99.
           03  WS-DIST-TOTALS OCCURS 12.
               05  WS-DIST-DESC         PIC X(10).
               05  WS-DIST-ACCNO        PIC X(12).
               05  WS-DIST-AMOUNTS      PIC S9(8)V99.
       01  LINE-DESCRIPTION.
           03  LINE-DESC            PIC X(10).
           03  LINE-MONTH           PIC X(10).
       01  WEEKEND-DESCRIPTION.
           03  WS-WEEK-INFO         PIC X(18).
           03  WS-WEEK-NUM          PIC X(2).
       01  DR-MONTH-CHANGES.
           03  WS-MONTH            PIC X(10).
           03  WS-DRYY             PIC X(15).
       01  DEBTOR-RATE.
           03  DR-RATE             PIC X OCCURS 15.
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
           PERFORM CLEAR-SCREEN.
           MOVE 0318 TO POS.
           DISPLAY "** WEEK, MONTH, & YEAR END PROCESSING **"
               AT POS.
           MOVE 0418 TO POS.
           DISPLAY "****************************************"
                AT POS.
       CONTROL-005.
           MOVE " " TO WS-MESSAGE.
           MOVE 510 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 710 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 510 TO POS
           DISPLAY "Is This A WEEK, MONTH Or YEAR End ?????" AT POS.
           MOVE 710 TO POS.
           DISPLAY "Enter (W = Week, M = Month, Y = Year.)     :[ ]"
                 AT POS.
           MOVE 755 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 4         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

           IF W-ESCAPE-KEY = 3
               PERFORM CONTROL-900.
           IF WS-ANSWER1 NOT = "W" AND NOT = "M" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-005.
       CONTROL-010.
           IF WS-ANSWER1 = "W"
              MOVE " " TO WS-MESSAGE
              MOVE 510 TO POS
              DISPLAY WS-MESSAGE AT POS
              MOVE 710 TO POS
              DISPLAY WS-MESSAGE AT POS.
           MOVE 510 TO POS.
           IF WS-ANSWER1 = "W"
           DISPLAY "You Have Selected To Do WEEK END Processing."
               AT POS
               GO TO CONTROL-020.
           MOVE 810 TO POS.
           IF WS-ANSWER1 = "M" OR = "Y"
           DISPLAY "Have You Run Your Month / Year End Reports?:[ ]"
               AT POS
               MOVE 855 TO POS
               MOVE ' '       TO CDA-DATA
               MOVE 1         TO CDA-DATALEN
               MOVE 5         TO CDA-ROW
               MOVE 54        TO CDA-COL
               MOVE CDA-WHITE TO CDA-COLOR
               MOVE 'F'       TO CDA-ATTR
               PERFORM CTOS-ACCEPT
               MOVE CDA-DATA TO WS-ANSWER2.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF WS-ANSWER2 = "Y"
               GO TO CONTROL-012
           ELSE
               GO TO CONTROL-900.
       CONTROL-012.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-015
           ELSE
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-015.
           MOVE " " TO WS-MESSAGE
           MOVE 510 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 710 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 810 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 510 TO POS.
           IF WS-ANSWER1 = "M"
           DISPLAY "You Have Selected To Do MONTH END Processing."
               AT POS.
           IF WS-ANSWER1 = "Y"
           DISPLAY "You Have Selected To Do YEAR END Processing." 
               AT POS.
           GO TO CONTROL-040.
       CONTROL-020. 
           MOVE 710 TO POS.
           DISPLAY "Have You Printed Your Invoice Register ??? :[ ]"
               AT POS.
           MOVE 755 TO POS.

           MOVE ' '       TO CDA-DATA
           MOVE 1         TO CDA-DATALEN
           MOVE 4         TO CDA-ROW
           MOVE 54        TO CDA-COL
           MOVE CDA-WHITE TO CDA-COLOR
           MOVE 'F'       TO CDA-ATTR
           PERFORM CTOS-ACCEPT
           MOVE CDA-DATA TO WS-ANSWER3.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF WS-ANSWER3 = "N"
               MOVE " " TO WS-ANSWER3
               GO TO CONTROL-700.
           IF WS-ANSWER3 = "Y"
               MOVE " " TO WS-ANSWER3
               GO TO CONTROL-025.
           MOVE 1055 TO POS.
           DISPLAY " " AT 3079 WITH BELL.
           GO TO CONTROL-020.
       CONTROL-025.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-030
           ELSE
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-030.
           MOVE 810 TO POS.
           DISPLAY "Are You Sure About The Invoice Register ?  :[ ]"
               AT POS.
           MOVE 855 TO POS.

           MOVE ' '       TO CDA-DATA
           MOVE 1         TO CDA-DATALEN
           MOVE 5         TO CDA-ROW
           MOVE 54        TO CDA-COL
           MOVE CDA-GREEN TO CDA-COLOR
           MOVE 'F'       TO CDA-ATTR
           PERFORM CTOS-ACCEPT
           MOVE CDA-DATA TO WS-ANSWER3.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-020.
           IF WS-ANSWER3 = "N"
               GO TO CONTROL-700.
           IF WS-ANSWER3 = "Y"
               MOVE " " TO WS-ANSWER3
               GO TO CONTROL-035.
           MOVE 1155 TO POS.
           DISPLAY " " AT 3079 WITH BELL.
           GO TO CONTROL-030.
       CONTROL-035.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-030.
       CONTROL-040.
           IF WS-ANSWER1 = "M" OR = "Y"
               PERFORM CHECK-PASSWORD.
           IF WS-PASSWORD-VALID = "N"
              MOVE 
           "YOU DON'T HAVE A PASSWORD TO RUN THIS ROUTINE, PLEASE EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              EXIT PROGRAM.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT.
       CONTROL-050.
           IF WS-ANSWER1 = "M" OR = "Y"
               GO TO CONTROL-055.
           PERFORM CHECK-REGISTER.
           IF WS-STOP-UPDATE = "Y"
             MOVE 2810 TO POS
             DISPLAY "Press 'NEXT' or 'GO' To End The Program. "
               AT POS
             MOVE 2910 TO POS
             DISPLAY
           "Once Invoice/Credits are Printed RE-RUN this program."
             AT POS
             MOVE 2975 TO POS

             MOVE ' '       TO CDA-DATA
             MOVE 1         TO CDA-DATALEN
             MOVE 26        TO CDA-ROW
             MOVE 74        TO CDA-COL
             MOVE CDA-GREEN TO CDA-COLOR
             MOVE 'F'       TO CDA-ATTR
             PERFORM CTOS-ACCEPT
             MOVE CDA-DATA TO WS-ANSWER3
             GO TO CONTROL-900.

            PERFORM READ-SL-PARAMETER.
            PERFORM WRITE-DIST-TO-GL.
            PERFORM CLEAR-WEEK-DIST-AMTS.
            PERFORM CLEAR-WEEK-SALES-AMTS.
      ****************************************************************
      *New section added to see how many times and when the week-end *
      *processes are done.                                           *
      ****************************************************************
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            MOVE "S/Ledger Week-End:"   TO WS-WEEK-INFO
            MOVE WS-JRN-WW              TO WS-WEEK-NUM
            MOVE WEEKEND-DESCRIPTION    TO WS-DAILY-1ST
            MOVE "Process completed on" TO WS-DAILY-2ND
            MOVE DISPLAY-DATE           TO WS-DAILY-3RD
            MOVE SPLIT-TIME             TO WS-DAILY-4TH
            PERFORM WRITE-DAILY.
           MOVE 2810 TO POS
           DISPLAY "Week End Process Run Successfully for Week   ," 
           AT POS
           ADD 43 TO POS
           DISPLAY WS-JRN-WW AT POS
           MOVE 2910 TO POS
           DISPLAY "Press 'NEXT' or 'GO' To End The Program. " AT POS.
       CONTROL-051.
             MOVE 3010 TO POS.

             MOVE ' '       TO CDA-DATA
             MOVE 1         TO CDA-DATALEN
             MOVE 26        TO CDA-ROW
             MOVE 74        TO CDA-COL
             MOVE CDA-GREEN TO CDA-COLOR
             MOVE 'F'       TO CDA-ATTR
             PERFORM CTOS-ACCEPT
             MOVE CDA-DATA TO WS-ANSWER3.

           IF W-ESCAPE-KEY = 1 OR 2
               GO TO CONTROL-900
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-051.
       CONTROL-055.
           MOVE 710 TO POS.
           IF WS-ANSWER1 = "M"
           DISPLAY "There Are 14 Data Files To Process." AT POS
              ELSE
           DISPLAY "There Are 16 Data Files To Process." AT POS.

      *     PERFORM CHECK-WEEK-END-DONE.
      *     PERFORM CGP-005 THRU CGP-010.
      *     PERFORM CGP-900.

      *     CALL "C$SLEEP" USING 1.
      *     PERFORM PARAMETERS.
      *     CALL "C$SLEEP" USING 1.
      *     PERFORM DEBTORS.
      *     CALL "C$SLEEP" USING 1.
      *     PERFORM STOCK.
      *     CALL "C$SLEEP" USING 1.
      *     PERFORM DEBTOR-TRANS.
      *     CALL "C$SLEEP" USING 1.
      *     PERFORM OUT-ORDERS.
      *     CALL "C$SLEEP" USING 1.
      *     PERFORM S-ANALYSIS.
      *     CALL "C$SLEEP" USING 1.
      *     PERFORM IMPORTS.
      *     CALL "C$SLEEP" USING 1.
      *     PERFORM UP-DIST.
      *     CALL "C$SLEEP" USING 1.
      *     PERFORM DELETE-SOLDBY.
      *     CALL "C$SLEEP" USING 1.
      *     PERFORM DELETE-REGISTER.
       CONTROL-056.
      *     CALL "C$SLEEP" USING 1.
      *     PERFORM DELETE-STOCK-TRANS.
      *     CALL "C$SLEEP" USING 1.
      *     PERFORM DELETE-STOCK-RECEIPTS.
      *     CALL "C$SLEEP" USING 1.
      *     PERFORM CHANGE-GL-PARAMETER.
       CONTROL-057.
      *     PERFORM DELETE-QUOTE-REGISTER.
      *     IF WS-ANSWER1 = "Y"
      *         PERFORM DELETE-REPAIR-REGISTER.
           IF WS-ANSWER1 = "Y"
               PERFORM DELETE-PSLIP-REGISTER.
       CONTROL-600.
           MOVE 2810 TO POS.
           DISPLAY "We Are Finished, Run The Maintainance Submit File,"
              AT POS.
           MOVE 2910 TO POS.
           DISPLAY "Then Do Your Final Backup to The USB Drive." AT POS.
       CONTROL-700.
           MOVE 3010 TO POS.
           DISPLAY "Press 'NEXT' Or 'GO' To End The Program. " AT POS.
           MOVE 2865 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 70        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER3.

           IF W-ESCAPE-KEY = 1 OR 2
               GO TO CONTROL-900
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-600.
       CONTROL-900.
           EXIT PROGRAM.
       CONTROL-999.
           EXIT.
      *
       CHECK-REGISTER SECTION.
       CRS-000.
            OPEN I-O INCR-REGISTER.
            MOVE "N" TO WS-STOP-UPDATE.
            MOVE 0 TO INCR-KEY WS-ORDERS WS-INTERNAL-ORDERS.
            START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
       CRS-010.
            READ INCR-REGISTER NEXT
               AT END
               MOVE 1610 TO POS
               DISPLAY "Checking Credit Notes are Printed ... Done"
               AT POS
               GO TO CRS-900.
            IF INCR-TRANS = 1
               MOVE 1410 TO POS
               DISPLAY "Checking Invoices are Printed ....." AT POS.
            IF INCR-TRANS = 4
               MOVE 1410 TO POS
               DISPLAY "Checking Invoices are Printed ... Done" AT POS
               MOVE 1510 TO POS
               DISPLAY "Checking Number Of Picking Slips ....." AT POS.
            IF INCR-TRANS = 6
               MOVE 1510 TO POS
               DISPLAY "Checking Number Of Picking Slips ... Done"
               AT POS
               MOVE 1610 TO POS
               DISPLAY "Checking Credit Notes are Printed ....."
               AT POS.
            IF INCR-TRANS > 6
               MOVE 1610 TO POS
               DISPLAY "Checking Credit Notes are Printed ... Done"
               AT POS
               GO TO CRS-900.
            IF INCR-TRANS = 1
             IF INCR-PRINTED NOT = "Y" AND NOT = "P"
               MOVE "YOU MUST GO AND PRINT THE INVOICES !!"
               TO WS-MESSAGE
               MOVE "Y" TO WS-STOP-UPDATE
               MOVE 1920 TO POS
               DISPLAY WS-MESSAGE AT POS
               GO TO CRS-010.
            IF INCR-TRANS = 6
             IF INCR-PRINTED NOT = "Y" AND NOT = "P"
                MOVE "YOU MUST GO AND PRINT THE CREDIT NOTES !!"
               TO WS-MESSAGE
               MOVE "Y" TO WS-STOP-UPDATE
               MOVE 2020 TO POS
               DISPLAY WS-MESSAGE AT POS
               GO TO CRS-010.
            MOVE INCR-NAME TO WS-NAME-CHECK.
            IF INCR-TRANS = 4
             IF INCR-PRINTED NOT = "Y" AND NOT = "L"
              IF WS-NAME-CHECK NOT = "INTERNAL ACCOUNT"
               ADD 1 TO WS-ORDERS
               MOVE 2410 TO POS
               DISPLAY "Total No of Outstanding Customer Orders ="
                  AT POS
               ADD 42 TO POS
               DISPLAY WS-ORDERS AT POS
               GO TO CRS-010.
            IF INCR-TRANS = 4
             IF INCR-PRINTED NOT = "Y" AND NOT = "L"
              IF WS-NAME-CHECK = "INTERNAL ACCOUNT"
               ADD 1 TO WS-INTERNAL-ORDERS
               MOVE 2310 TO POS
               DISPLAY "Total No of Outstanding Internal Orders ="
                  AT POS
               ADD 42 TO POS
               DISPLAY WS-INTERNAL-ORDERS AT POS
               GO TO CRS-010.
            GO TO CRS-010.
       CRS-900.
            CLOSE INCR-REGISTER.
       CRS-999.
            EXIT.
      *
       CHECK-WEEK-END-DONE SECTION.
       CWED-005.
            PERFORM OPEN-000.
            MOVE "1" TO DIST-KEY.
       CWED-010.
            READ DISTRIBUTIONS
                INVALID KEY NEXT SENTENCE.
            IF DIST-INVOICEWEEK          NOT = 0
              OR DIST-PAYMENTWEEK        NOT = 0
              OR DIST-RDCHEQUEWEEK       NOT = 0
              OR DIST-JOURNALDRWEEK      NOT = 0
              OR DIST-JOURNALCRWEEK      NOT = 0
              OR DIST-CNOTEWEEK          NOT = 0
              OR DIST-INTERESTWEEK       NOT = 0
              OR DIST-DISCOUNTWEEK       NOT = 0
              OR DIST-ADDONWEEK          NOT = 0
              OR DIST-BDEBTWEEK          NOT = 0
              OR DIST-ACCRECWEEK         NOT = 0
              OR GST-AMT-TAXED-WEEK      NOT = 0
              OR GST-AMT-TAXABLE-WEEK    NOT = 0
              OR GST-AMT-NONTAXABLE-WEEK NOT = 0
              OR GST-AMT-EXPORT-WEEK     NOT = 0
                CLOSE DISTRIBUTIONS
                PERFORM READ-SL-PARAMETER
                PERFORM WRITE-DIST-TO-GL
                PERFORM CLEAR-WEEK-DIST-AMTS
                PERFORM CLEAR-WEEK-SALES-AMTS
      *New section added to see how many times and when the week-end
      *processes are done.
                PERFORM GET-SYSTEM-Y2K-DATE
                MOVE WS-DATE TO SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                ACCEPT WS-TIME FROM TIME
                MOVE WS-HR TO SPLIT-HR
                MOVE ":"   TO SPLIT-FIL1
                MOVE WS-MIN TO SPLIT-MN
                MOVE ":"   TO SPLIT-FIL2
                MOVE WS-SEC TO SPLIT-SC
                MOVE "S/Ledger Week-End:"   TO WS-WEEK-INFO
                MOVE WS-JRN-WW              TO WS-WEEK-NUM
                MOVE WEEKEND-DESCRIPTION    TO WS-DAILY-1ST
                MOVE "Run in M/Y End on  :" TO WS-DAILY-2ND
                MOVE DISPLAY-DATE           TO WS-DAILY-3RD
                MOVE SPLIT-TIME             TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                GO TO CWED-999.
            CLOSE DISTRIBUTIONS.
       CWED-999.
           EXIT.
      *
       CLEAR-WEEK-DIST-AMTS SECTION.
       CWDA-005.
            PERFORM OPEN-000.
            MOVE "1" TO DIST-KEY.
       CWDA-010.
            READ DISTRIBUTIONS WITH LOCK
                INVALID KEY
                MOVE "DISTRIBUTION TOTALS RECORD NOT FOUND!!!"
                     TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                EXIT PROGRAM
                GO TO CWDA-900.
            MOVE 2910 TO POS.
            DISPLAY "Distribution file in Process...." AT POS.
            MOVE 0 TO DIST-INVOICEWEEK
                      DIST-PAYMENTWEEK
                      DIST-RDCHEQUEWEEK
                      DIST-JOURNALDRWEEK
                      DIST-JOURNALCRWEEK
                      DIST-CNOTEWEEK
                      DIST-INTERESTWEEK
                      DIST-DISCOUNTWEEK
                      DIST-ADDONWEEK
                      DIST-BDEBTWEEK
                      DIST-ACCRECWEEK
                      GST-AMT-TAXED-WEEK
                      GST-AMT-TAXABLE-WEEK
                      GST-AMT-NONTAXABLE-WEEK
                      GST-AMT-EXPORT-WEEK.
            ADD 1 TO PA-CURRENT-PER-WW.
            REWRITE DIST-REC
               INVALID KEY
               MOVE "DISTRIBUTION TOTALS RECORD NOT UPDATED!!!!"
                  TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               CLOSE DISTRIBUTIONS
               EXIT PROGRAM.
       CWDA-900.
            CLOSE DISTRIBUTIONS.
       CWDA-999.
            EXIT.
      *
       CLEAR-WEEK-SALES-AMTS SECTION.
       CWSA-005.
            PERFORM OPEN-005.
       CWSA-010.
            READ SALES-ANALYSIS NEXT WITH LOCK
                AT END
                CLOSE SALES-ANALYSIS
                GO TO CWSA-900.
            MOVE 2910 TO POS.
            DISPLAY "Sales-Anal Being Processed :          " AT POS.
            ADD 29 TO POS.
            DISPLAY SA-NAME AT POS.
            MOVE 0 TO SA-COST-WEEK
                      SA-SALES-WEEK.
            REWRITE SALES-ANALYSIS-REC
               INVALID KEY
                   MOVE "SALES RECORD:"  TO WS-DAILY-1ST
                   MOVE SA-KEY           TO WS-DAILY-2ND
                   MOVE "NOT UPDATED"    TO WS-DAILY-3RD
                   MOVE "THIS WEEK END"  TO WS-DAILY-4TH
                   PERFORM WRITE-DAILY.
            GO TO CWSA-010.
       CWSA-900.
            MOVE 2910 TO POS.
            MOVE " " TO WS-MESSAGE.
            DISPLAY WS-MESSAGE AT POS.
       CWSA-999.
            EXIT.
      *
       PARAMETERS SECTION.
       PAR-000.
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            MOVE "S/Ledger Period End:" TO WS-DAILY-1ST
            MOVE GLPA-CURRENT-SLPER     TO WS-DAILY-2ND
            MOVE "Start of process    " TO WS-DAILY-3RD
            MOVE SPLIT-TIME             TO WS-DAILY-4TH
            PERFORM WRITE-DAILY
            MOVE " 1. Parameter file  " TO WS-DAILY-1ST
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       PAR-005.
            MOVE 910 TO POS.
            DISPLAY " 1. Parameter File Being Processed........" AT POS.
            PERFORM OPEN-010.
            MOVE 0 TO PA-TYPE.
            MOVE 1 TO PA-RECORD.
            READ PARAMETER-FILE WITH LOCK
                INVALID KEY
                MOVE "PARAMETER RECORD NOT FOUND!!!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                EXIT PROGRAM
                GO TO PAR-999.
       PAR-010.
            IF PA-CURRENT-PER-MM = 2
              IF WS-ANSWER1 = "M"
                MOVE 1510 TO POS
                DISPLAY " " AT 3079 WITH BELL
                DISPLAY "You should be running the YEAR END" AT POS
                MOVE 1610 TO POS
                DISPLAY "Press 'NEXT' Or 'GO' To End The Program."
                AT POS
                MOVE 3010 TO POS                

                MOVE ' '       TO CDA-DATA
                MOVE 1         TO CDA-DATALEN
                MOVE 13        TO CDA-ROW
                MOVE 70        TO CDA-COL
                MOVE CDA-GREEN TO CDA-COLOR
                MOVE 'F'       TO CDA-ATTR
                PERFORM CTOS-ACCEPT
                MOVE CDA-DATA TO WS-ANSWER3
           IF W-ESCAPE-KEY = 1 OR 2
                CLOSE PARAMETER-FILE
                EXIT PROGRAM
           ELSE
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO PAR-010.
               
            IF PA-CURRENT-PER-MM NOT = 2
             IF WS-ANSWER1 = "Y"
                MOVE 1510 TO POS
                DISPLAY " " AT 3079 WITH BELL
                DISPLAY "You should be running the MONTH END" AT POS
                MOVE 1610 TO POS
                DISPLAY "Press 'NEXT' Or 'GO' To End The Program."
                AT POS
                MOVE 3010 TO POS

                MOVE ' '       TO CDA-DATA
                MOVE 1         TO CDA-DATALEN
                MOVE 13        TO CDA-ROW
                MOVE 70        TO CDA-COL
                MOVE CDA-GREEN TO CDA-COLOR
                MOVE 'F'       TO CDA-ATTR
                PERFORM CTOS-ACCEPT
                MOVE CDA-DATA TO WS-ANSWER3
           IF W-ESCAPE-KEY = 1 OR 2
                CLOSE PARAMETER-FILE
                EXIT PROGRAM
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO PAR-010.
            ADD 1  TO PA-CURRENT-PER-MM.
            MOVE 1 TO PA-CURRENT-PER-WW.
            COMPUTE WS-CALC-MM = PA-CURRENT-PER-MM - WS-MM.
       PAR-020.
            IF WS-CALC-MM > 1
                MOVE 1510 TO POS
                DISPLAY " " AT 3079 WITH BELL
                DISPLAY "How Many Times A Month Do You Want To Run" &
                " This Program  ??" AT POS
                MOVE 1710 TO POS
                DISPLAY "It Has Been Run Once This Month Already!!!!!"
                    AT POS
                MOVE 1910 TO POS
                DISPLAY "Press 'NEXT' or 'GO' To Finish The Program."
                    AT POS
                MOVE 3010 TO POS
                MOVE ' '       TO CDA-DATA
                MOVE 1         TO CDA-DATALEN
                MOVE 27        TO CDA-ROW
                MOVE 70        TO CDA-COL
                MOVE CDA-WHITE TO CDA-COLOR
                MOVE 'F'       TO CDA-ATTR
                PERFORM CTOS-ACCEPT
                MOVE CDA-DATA TO WS-ANSWER3
           IF W-ESCAPE-KEY = 1 OR 2
                CLOSE PARAMETER-FILE
                EXIT PROGRAM
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO PAR-020.
            IF PA-CURRENT-PER-MM > 12
                ADD 1 TO PA-CURRENT-PER-YY
                MOVE 1 TO PA-CURRENT-PER-MM.
            REWRITE PARAMETER-REC
                INVALID KEY
                MOVE "PARAMETER RECORD" TO WS-DAILY-1ST
                MOVE "NOT UPDATED!!!"   TO WS-DAILY-2ND
                MOVE " "                TO WS-DAILY-3RD
                MOVE " "                TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            CLOSE PARAMETER-FILE.
        PAR-900.
            MOVE 910 TO POS.
            DISPLAY " 1. Parameter File Processed.             " AT POS.
            MOVE " " TO WS-MESSAGE
            MOVE 2910 TO POS
            DISPLAY WS-MESSAGE AT POS.
        PAR-999.
            EXIT.
      *
       DEBTORS SECTION.
       DR-000.
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-HR TO SPLIT-HR.
            MOVE ":"   TO SPLIT-FIL1.
            MOVE WS-MIN TO SPLIT-MN.
            MOVE ":"   TO SPLIT-FIL2.
            MOVE WS-SEC TO SPLIT-SC.
            MOVE " 2. Debtor-Master   " TO WS-DAILY-1ST.
            MOVE "Start of process    " TO WS-DAILY-2ND.
            MOVE SPLIT-TIME             TO WS-DAILY-3RD.
            PERFORM WRITE-DAILY.
       DR-005.
            PERFORM OPEN-015.
            MOVE 0 TO DR-KEY.
            START DEBTOR-MASTER KEY NOT < DR-KEY
                INVALID KEY NEXT SENTENCE.
            MOVE 1010 TO POS.
            DISPLAY " 2. Debtor Master Files Being Processed..." AT POS.
       DR-010.
            READ DEBTOR-MASTER NEXT WITH LOCK
                AT END
                CLOSE DEBTOR-MASTER 
                GO TO DR-900.
            MOVE 2910 TO POS.
            DISPLAY "Account Being Processed" AT POS.
            ADD 25 TO POS.
            DISPLAY DR-ACCOUNT-NUMBER AT POS.
            IF DR-ACCOUNT-NUMBER = 0300087 OR 0300090 OR 0300100
                     OR 0300150 OR 0300200 OR 9999999
                GO TO DR-015.

            IF WS-ANSWER1 = "M"
             IF DR-BALANCE = 0  
              IF DR-BAL-LAST-STATE = 0
               IF DR-SALES-PTD = 0
                IF DR-COST-PTD = 0
                 GO TO DR-010.
            IF WS-ANSWER1 = "Y"
             IF DR-BALANCE = 0  
              IF DR-BAL-LAST-STATE = 0
               IF DR-SALES-YTD = 0
                IF DR-SALES-LAST = 0
                 IF DR-COST-YTD = 0
                  GO TO DR-010.
       DR-015.
            MOVE DR-BALANCE TO DR-BAL-LAST-STATE.
            ADD DR-90DAY    TO DR-120DAY.
            MOVE DR-60DAY   TO DR-90DAY.
            MOVE DR-30DAY   TO DR-60DAY.
            MOVE DR-CURRENT TO DR-30DAY.
            MOVE 0          TO DR-CURRENT
                               DR-SALES-PTD
                               DR-COST-PTD.
       DR-020.
            IF WS-ANSWER1 = "Y"
                MOVE DR-SALES-YTD TO DR-SALES-LAST
                MOVE DR-COST-YTD  TO DR-COST-LAST
                MOVE 0            TO DR-SALES-YTD
                                     DR-COST-YTD.
            IF DR-ACCOUNT-NUMBER = 0300087 OR 0300090 OR 0300100
                     OR 0300150 OR 0300200 OR 9999999 OR 0300500
                     OR 0300501 OR 0300502 OR 0300503 OR 0300504
                     OR 0300505 OR 0300506 OR 0300507 OR 0300508
                     OR 0300509 OR 0300510
                GO TO DR-025
            ELSE
                GO TO DR-030.
       DR-025.
           COMPUTE WS-MM = WS-MM + 1.
           IF WS-MM > 12
              MOVE 1 TO WS-MM
              ADD 1  TO WS-YY.
           MOVE WS-DR-MONTH-DESC (WS-MM) TO WS-MONTH.
           MOVE WS-YY                    TO WS-DRYY.
           MOVE DR-MONTH-CHANGES         TO ALPHA-RATE.
           MOVE 1                        TO SUB-1.
       DR-026.
           IF AL-RATE (SUB-1) = " "
               ADD 1 TO SUB-1
               GO TO DR-026.
           MOVE SUB-1 TO SUB-2.
           MOVE     1 TO SUB-1.
       DR-027.
           MOVE AL-RATE (SUB-2) TO DR-RATE (SUB-1).
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-2 < 16
           GO TO DR-027.
           MOVE 1 TO SUB-1 SUB-2.
           MOVE DEBTOR-RATE TO DR-ADDRESS3.
           SUBTRACT 1 FROM WS-MM.
           IF WS-MM = 0
               MOVE 12 TO WS-MM
               SUBTRACT 1 FROM WS-YY.
       DR-030.
            REWRITE DEBTOR-RECORD
                INVALID KEY
                MOVE "DEBTOR RECORD:"  TO WS-DAILY-1ST
                MOVE DR-ACCOUNT-NUMBER TO WS-DAILY-2ND
                MOVE "NOT ROLLED!!"    TO WS-DAILY-3RD
                MOVE "THIS MONTH END"  TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            GO TO DR-010.
       DR-900.
            MOVE 1010 TO POS.
            DISPLAY " 2. Debtor Master Files Processed.        " AT POS.
            MOVE " " TO WS-MESSAGE
            MOVE 2910 TO POS
            DISPLAY WS-MESSAGE AT POS.
       DR-999.
            EXIT.
      *
       STOCK SECTION.
       ST-000.
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-HR TO SPLIT-HR.
            MOVE ":"   TO SPLIT-FIL1.
            MOVE WS-MIN TO SPLIT-MN.
            MOVE ":"   TO SPLIT-FIL2.
            MOVE WS-SEC TO SPLIT-SC.
            MOVE " 3. Stock-Master    " TO WS-DAILY-1ST.
            MOVE "Start of process    " TO WS-DAILY-2ND.
            MOVE SPLIT-TIME             TO WS-DAILY-3RD.
            PERFORM WRITE-DAILY.
       ST-005.
            PERFORM OPEN-020.
            MOVE " " TO ST-KEY.
            START STOCK-MASTER KEY NOT < ST-KEY
                 INVALID KEY NEXT SENTENCE.
            MOVE 1110 TO POS.
            DISPLAY " 3. Stock Master Files Being Processed...." AT POS.
       ST-010.
            READ STOCK-MASTER NEXT WITH LOCK
                 AT END
                 CLOSE STOCK-MASTER
                 GO TO ST-900.
            MOVE 2910 TO POS.
            DISPLAY "Stock No Being Processed" AT POS.
            ADD 25 TO POS.
            DISPLAY ST-STOCKNUMBER AT POS.
            IF WS-ANSWER1 = "M"
             IF ST-SALESUNITMTD = 0
              IF ST-SALESRANDSMTD = 0
               IF ST-SALESCOSTMTD = 0
                IF ST-QTYRECMTD = 0
                 IF ST-QTYADJMTD = 0
                GO TO ST-010.
            MOVE 0 TO ST-SALESUNITMTD
                      ST-SALESRANDSMTD
                      ST-SALESCOSTMTD
                      ST-QTYADJMTD
                      ST-QTYRECMTD.
            IF WS-ANSWER1 = "Y"
                 MOVE ST-QTYADJYTD     TO ST-QTYADJLAST
                 MOVE ST-QTYRECYTD     TO ST-QTYRECLAST
                 MOVE ST-SALESUNITSYTD TO ST-SALESUNITSLAST
                 MOVE ST-SALESRANDSYTD TO ST-SALESRANDSLAST
                 MOVE ST-SALESCOSTYTD  TO ST-SALESCOSTLAST
                 MOVE 0                TO ST-SALESUNITSYTD
                                          ST-SALESRANDSYTD
                                          ST-SALESCOSTYTD
                                          ST-QTYADJYTD
                                          ST-QTYRECYTD.
       ST-020.
            REWRITE STOCK-RECORD
                INVALID KEY
                MOVE "STOCK RECORD:"  TO WS-DAILY-1ST
                MOVE ST-STOCKNUMBER   TO WS-DAILY-2ND
                MOVE "NOT ROLLED!!"   TO WS-DAILY-3RD
                MOVE "THIS MONTH END" TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            GO TO ST-010.
       ST-900.
            MOVE 1110 TO POS.
            DISPLAY " 3. Stock Master Files Processed.         " AT POS.
            MOVE " " TO WS-MESSAGE
            MOVE 2910 TO POS
            DISPLAY WS-MESSAGE AT POS.
       ST-999.
             EXIT.
      *
       DEBTOR-TRANS SECTION.
       DRTR-000.
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-HR TO SPLIT-HR.
            MOVE ":"   TO SPLIT-FIL1.
            MOVE WS-MIN TO SPLIT-MN.
            MOVE ":"   TO SPLIT-FIL2.
            MOVE WS-SEC TO SPLIT-SC.
            MOVE " 4. Dr Trans-file   " TO WS-DAILY-1ST.
            MOVE "Start of process    " TO WS-DAILY-2ND.
            MOVE SPLIT-TIME             TO WS-DAILY-3RD.
            PERFORM WRITE-DAILY.
       DRTR-005.
            PERFORM OPEN-025.
            MOVE 0 TO DRTR-KEY.
            START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY
                INVALID KEY NEXT SENTENCE.
            MOVE 1210 TO POS.
            DISPLAY " 4. Debtor Transactions Being Processed.." AT POS.
       DRTR-010.
            READ DEBTOR-TRANS-FILE NEXT WITH LOCK
                 AT END
                 CLOSE DEBTOR-TRANS-FILE
                 GO TO DRTR-900.
            MOVE 2910 TO POS.
            DISPLAY "Dr Trans Being Processed" AT POS.
            ADD 25 TO POS.
            DISPLAY DRTR-TRANSACTION-NUMBER AT POS.
             IF DRTR-AMT-OUTSTANDING < .01
                 GO TO DRTR-020.
             GO TO DRTR-010.
       DRTR-020.
             DELETE DEBTOR-TRANS-FILE
                 INVALID KEY
                 MOVE "DEBTOR TRANS:"         TO WS-DAILY-1ST
                 MOVE DRTR-TRANSACTION-NUMBER TO WS-DAILY-2ND
                 MOVE DRTR-TYPE               TO WS-DAILY-3RD
                 MOVE "NOT DELETED!!"         TO WS-DAILY-4TH
                 PERFORM WRITE-DAILY.
             GO TO DRTR-010.
       DRTR-900.
            MOVE 1210 TO POS.
            DISPLAY " 4. Debtor Transactions Processed.        " AT POS.
            MOVE " " TO WS-MESSAGE
            MOVE 2910 TO POS
            DISPLAY WS-MESSAGE AT POS.
       DRTR-999.
            EXIT.
      *
       OUT-ORDERS SECTION.
       OO-000.
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-HR TO SPLIT-HR.
            MOVE ":"   TO SPLIT-FIL1.
            MOVE WS-MIN TO SPLIT-MN.
            MOVE ":"   TO SPLIT-FIL2.
            MOVE WS-SEC TO SPLIT-SC.
            MOVE " 5. Suppliers-Orders" TO WS-DAILY-1ST.
            MOVE "Start of process    " TO WS-DAILY-2ND.
            MOVE SPLIT-TIME             TO WS-DAILY-3RD.
            PERFORM WRITE-DAILY.
       OO-005.
           PERFORM OPEN-030.
           MOVE " " TO OO-KEY.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
                INVALID KEY NEXT SENTENCE.
           MOVE 1310 TO POS
           DISPLAY " 5. Suppliers Order File Being Processed..." AT POS.
       OO-010.
             READ OUTSTANDING-ORDERS NEXT WITH LOCK
                 AT END
                 CLOSE OUTSTANDING-ORDERS
                 GO TO OO-900.
            MOVE 2910 TO POS.
            DISPLAY "Sup-Order Being Processed" AT POS.
            ADD 26 TO POS.
            DISPLAY OO-ORDER-NUMBER AT POS.
           IF OO-QUANTITY NOT = 0
              GO TO OO-010.
           DELETE OUTSTANDING-ORDERS
               INVALID KEY
                   MOVE "O/ORDER NOT DELETED" TO WS-DAILY-1ST
                   MOVE OO-ORDER-NUMBER       TO WS-DAILY-2ND
                   MOVE OO-STOCK-NUMBER       TO WS-DAILY-3RD
                   MOVE "THIS MONTH END"      TO WS-DAILY-4TH
                   PERFORM WRITE-DAILY.
           GO TO OO-010.
       OO-900.
           MOVE 1310 TO POS.
           DISPLAY " 5. Suppliers Order File Processed.        " AT POS.
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS.
       OO-999.
           EXIT.
      *
       S-ANALYSIS SECTION.
       SA-000.
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-HR TO SPLIT-HR.
            MOVE ":"   TO SPLIT-FIL1.
            MOVE WS-MIN TO SPLIT-MN.
            MOVE ":"   TO SPLIT-FIL2.
            MOVE WS-SEC TO SPLIT-SC.
            MOVE " 6. Sales-Analysis  " TO WS-DAILY-1ST.
            MOVE "Start of process    " TO WS-DAILY-2ND.
            MOVE SPLIT-TIME             TO WS-DAILY-3RD.
            PERFORM WRITE-DAILY.
       SA-005.
            PERFORM OPEN-035.
            MOVE 0 TO SA-KEY.
            START SALES-ANALYSIS KEY NOT < SA-KEY
                INVALID KEY NEXT SENTENCE.
            MOVE 1410 TO POS.
            DISPLAY " 6. Sales Analysis File Being Processed..." AT POS.
       SA-010.
            READ SALES-ANALYSIS NEXT WITH LOCK
                AT END
                CLOSE SALES-ANALYSIS
                GO TO SA-900.
            MOVE 2910 TO POS.
            DISPLAY "Sales-Anal Being Processed :" AT POS.
            ADD 29 TO POS.
            DISPLAY SA-NAME AT POS.
            MOVE 0 TO SA-COST-PTD
                      SA-SALES-PTD.
            IF WS-ANSWER1 = "Y"
               MOVE 0 TO SA-COST-YTD
                         SA-SALES-YTD.
            REWRITE SALES-ANALYSIS-REC
               INVALID KEY
                   MOVE "SALES RECORD:"  TO WS-DAILY-1ST
                   MOVE SA-KEY           TO WS-DAILY-2ND
                   MOVE "NOT UPDATED"    TO WS-DAILY-3RD
                   MOVE "THIS MONTH END" TO WS-DAILY-4TH
                   PERFORM WRITE-DAILY.
            GO TO SA-010.
       SA-900.
           MOVE 1410 TO POS.
           DISPLAY " 6. Sales Analysis File Processed.         " AT POS.
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS.
       SA-999.
            EXIT.
      *
       IMPORTS SECTION.
       IMP-000.
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-HR TO SPLIT-HR.
            MOVE ":"   TO SPLIT-FIL1.
            MOVE WS-MIN TO SPLIT-MN.
            MOVE ":"   TO SPLIT-FIL2.
            MOVE WS-SEC TO SPLIT-SC.
            MOVE " 7. Imports-File    " TO WS-DAILY-1ST.
            MOVE "Start of process    " TO WS-DAILY-2ND.
            MOVE SPLIT-TIME             TO WS-DAILY-3RD.
            PERFORM WRITE-DAILY.
       IMP-005.
            PERFORM OPEN-040.
            MOVE 0 TO IMRE-KEY.
            START IMPRECEIPTS-FILE KEY NOT < IMRE-KEY
                 INVALID KEY NEXT SENTENCE.
            MOVE 1510 TO POS.
            DISPLAY " 7. Import Receipts File Being Processed.." AT POS.
       IMP-010.
            READ IMPRECEIPTS-FILE NEXT WITH LOCK
               AT END
               GO TO IMP-900.
            MOVE 2910 TO POS.
            DISPLAY "Import File Being Processed" AT POS.
            ADD 28 TO POS.
            DISPLAY IMRE-ALT-KEY AT POS.
       IMP-015.
            IF IMRE-UPDATED-YN NOT = "Y"
               GO TO IMP-010.
       IMP-020.
            DELETE IMPRECEIPTS-FILE
                INVALID KEY
                MOVE "IMPORT RECORD"  TO WS-DAILY-1ST
                MOVE IMRE-ALT-KEY     TO WS-DAILY-2ND
                MOVE " NOT DELETED!!" TO WS-DAILY-3RD
                MOVE "THIS MONTH END" TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            GO TO IMP-010.
       IMP-900.
            CLOSE IMPRECEIPTS-FILE.
            MOVE 1510 TO POS
            DISPLAY " 7. Import Receipts File Processed.       " AT POS.
            MOVE " " TO WS-MESSAGE
            MOVE 2910 TO POS
            DISPLAY WS-MESSAGE AT POS.
       IMP-999.
            EXIT.
      *
       UP-DIST SECTION.
       UPDIS-000.
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-HR TO SPLIT-HR.
            MOVE ":"   TO SPLIT-FIL1.
            MOVE WS-MIN TO SPLIT-MN.
            MOVE ":"   TO SPLIT-FIL2.
            MOVE WS-SEC TO SPLIT-SC.
            MOVE " 8. Distributions   " TO WS-DAILY-1ST.
            MOVE "Start of process    " TO WS-DAILY-2ND.
            MOVE SPLIT-TIME             TO WS-DAILY-3RD.
            PERFORM WRITE-DAILY.
       UPDIS-005.
            PERFORM OPEN-000.
            MOVE "1" TO DIST-KEY.
            MOVE 1610 TO POS.
            DISPLAY " 8. Distributions File Being Processed...." AT POS.
       UPDIS-010.
            READ DISTRIBUTIONS WITH LOCK
                INVALID KEY
                MOVE "DISTRIBUTION TOTALS RECORD NOT FOUND!!!"
                     TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                EXIT PROGRAM
                GO TO UPDIS-900.
            MOVE 0 TO DIST-INVOICEPTD
                      DIST-PAYMENTPTD
                      DIST-RDCHEQUEPTD
                      DIST-JOURNALDRPTD
                      DIST-JOURNALCRPTD
                      DIST-CNOTEPTD
                      DIST-INTERESTPTD
                      DIST-DISCOUNTPTD
                      DIST-ADDONPTD
                      DIST-BDEBTPTD
                      DIST-ACCRECPTD
                      GST-AMT-TAXED-PTD
                      GST-AMT-TAXABLE-PTD
                      GST-AMT-NONTAXABLE-PTD
                      GST-AMT-EXPORT-PTD.
            IF WS-ANSWER1 = "Y"
                MOVE 0 TO DIST-INVOICEYTD
                          DIST-PAYMENTYTD
                          DIST-RDCHEQUEYTD
                          DIST-JOURNALDRYTD
                          DIST-JOURNALCRYTD
                          DIST-CNOTEYTD
                          DIST-INTERESTYTD
                          DIST-DISCOUNTYTD
                          DIST-ADDONYTD
                          DIST-BDEBTYTD
                          GST-AMT-TAXED-YTD
                          GST-AMT-TAXABLE-YTD
                          GST-AMT-NONTAXABLE-YTD
                          GST-AMT-EXPORT-YTD.
            REWRITE DIST-REC
               INVALID KEY
               MOVE "DISTRIBUTION TOTALS RECORD NOT UPDATED!!!!"
                  TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               CLOSE DISTRIBUTIONS
               EXIT PROGRAM.
       UPDIS-900.
           CLOSE DISTRIBUTIONS.
           MOVE 1610 TO POS
           DISPLAY " 8. Distributions File Processed.          " AT POS.
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS.
       UPDIS-999.
            EXIT.
      *
       DELETE-SOLDBY SECTION.
       SOLD-BY-005.
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-HR TO SPLIT-HR.
            MOVE ":"   TO SPLIT-FIL1.
            MOVE WS-MIN TO SPLIT-MN.
            MOVE ":"   TO SPLIT-FIL2.
            MOVE WS-SEC TO SPLIT-SC.
            MOVE " 9. Soldby-File     " TO WS-DAILY-1ST.
            MOVE "Start of process    " TO WS-DAILY-2ND.
            MOVE SPLIT-TIME             TO WS-DAILY-3RD.
            PERFORM WRITE-DAILY.
       SOLD-BY-006.
           MOVE 1710 TO POS.
           DISPLAY " 9. Sold By File Being Processed..........." AT POS.
       SOLD-BY-007.
           PERFORM OPEN-045.
           START SOLD-BY KEY NOT < SB-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-SOLDBY-ST1 NOT = 0
               GO TO SOLD-BY-900.
       SOLD-BY-010.
           READ SOLD-BY NEXT WITH LOCK
               AT END
               GO TO SOLD-BY-900.
       SOLD-BY-800.
           DELETE SOLD-BY
               INVALID KEY
                MOVE "SOLD-BY RECORD NOT  " TO WS-DAILY-1ST
                MOVE "DELETED             " TO WS-DAILY-2ND
                MOVE SB-KEY                 TO WS-DAILY-3RD
                MOVE " "                    TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO SOLD-BY-010.
       SOLD-BY-900.
           CLOSE SOLD-BY.

           MOVE 1710 TO POS
           DISPLAY " 9. Sold By File Processed.                " AT POS.
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS.
       SOLD-BY-999.
           EXIT.
      *
       DELETE-REGISTER SECTION.
       DIR-000.
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            MOVE "10. Register-File   " TO WS-DAILY-1ST
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       DIR-005.
           PERFORM OPEN-050.
           MOVE 1810 TO POS
           DISPLAY "10. Invoice Register Being Processed.......     "
           AT POS.
           IF WS-ANSWER1 = "Y"
               PERFORM DELETE-REG-LY-FILE
               PERFORM OPEN-052.
       DIR-006.
           MOVE "Y" TO INCR-PRINTED.
           START INCR-REGISTER KEY NOT < INCR-PRINTED
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               GO TO DIR-900.
               
      *TEMP FIX DUE TO ERC 23 BELOW WHEN REG HAS BEEN FIXED.
      *     GO TO DIR-900.
       DIR-010.
           READ INCR-REGISTER NEXT
               AT END
               GO TO DIR-900.
           IF WS-INCR-ST1 = 11 OR = 23
               GO TO DIR-900.
           MOVE 2910 TO POS.
           DISPLAY "Register Being Processed" AT POS.
           ADD 26 TO POS.
           DISPLAY INCR-INVOICE AT POS.
           ADD 10 TO POS 
           DISPLAY INCR-PRINTED AT POS.
           ADD 10 TO POS 
           DISPLAY INCR-DATE AT POS.

           IF INCR-PRINTED NOT = "Y"
               GO TO DIR-900.
           IF WS-INCR-ST1 = 91
              PERFORM DIR-900
              PERFORM DIR-005
              PERFORM DIR-006
              GO TO DIR-010.
       DIR-020.
      ******************************************************************
      * FEB 2003.                                                      *
      * NEW SECTION SO THAT WE STORE THE WHOLE YEARS INFO EXCEPT FOR   *
      * P/SLIPS (INCR-TRANS=4) WHICH WILL BE DELETED AFTER 6 MONTHS AS *
      * ORIGINALLY.                                                    *
      * AT YEAR END THE INV, C/NOTE, REPAIR & B/M TRANS WILL BE COPIED *
      * INTO A NEW SLREGLY FILE AND DELETED FROM THE CURRENT FILE.     *
      ******************************************************************
           IF WS-ANSWER1 = "M"
            IF INCR-TRANS = 1 OR = 3 OR = 4 OR = 6 OR = 7 OR = 8
               GO TO DIR-010.
      * Quotes & P/SLIP
           IF WS-ANSWER1 = "Y"
            IF INCR-TRANS = 3 OR = 4
               GO TO DIR-010.
      * Inv, C/Note & BM
           IF WS-ANSWER1 = "Y"
            IF INCR-TRANS = 1 OR = 6 OR = 7
               PERFORM WRITE-REG-LY
               GO TO DIR-800.
       DIR-600.
           MOVE INCR-DATE TO WS-PORDER-DATE.
           COMPUTE WS-WORK-PO-MM = WS-PO-MM + 6.
           IF WS-WORK-PO-MM > 12
               COMPUTE WS-WORK-PO-MM = WS-WORK-PO-MM - 12.

           IF WS-PO-YY < WS-YY
            IF WS-WORK-PO-MM = WS-MM
               GO TO DIR-800.
           IF WS-PO-YY = WS-YY
            IF WS-PO-MM + 6 < WS-MM
               GO TO DIR-800.
           IF WS-PO-YY < WS-YY
            IF WS-PO-MM < 6
               GO TO DIR-800
            ELSE
             IF WS-PO-MM - 6 < WS-MM
               GO TO DIR-800.
           GO TO DIR-010.
       DIR-800.
           DELETE INCR-REGISTER
               INVALID KEY
                MOVE "REGISTER RECORD NOT " TO WS-DAILY-1ST
                MOVE "DELETED             " TO WS-DAILY-2ND
                MOVE INCR-INVOICE           TO WS-DAILY-3RD
                MOVE " "                    TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           MOVE 1810 TO POS
           DISPLAY
           "10. Invoice Register Being Processed.......             "
            AT POS.
           GO TO DIR-010.
       DIR-900.
           CLOSE INCR-REGISTER.
           IF WS-ANSWER1 = "Y"
              CLOSE INCR-LY-REGISTER.
           MOVE 1810 TO POS
           DISPLAY "10. Invoice Register Processed.                 " 
           AT POS.
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS.
       DIR-999.
            EXIT.
      *
       WRITE-REG-LY SECTION.
       WRL-010.
           MOVE 1810 TO POS
           DISPLAY
           "10. Invoice Register Being Processed....... - LY WRITE.."
            AT POS.
       
           MOVE INCR-TRANS           TO INCR-LY-TRANS
           MOVE INCR-INVOICE         TO INCR-LY-INVOICE
           MOVE INCR-ACCOUNT         TO INCR-LY-ACCOUNT
           MOVE INCR-PORDER          TO INCR-LY-PORDER
           MOVE INCR-GSTNO           TO INCR-LY-GSTNO
           MOVE INCR-DATE            TO INCR-LY-DATE
           MOVE INCR-SALES           TO INCR-LY-SALES
           MOVE INCR-INVCRED-AMT     TO INCR-LY-INVCRED-AMT
           MOVE INCR-TAX             TO INCR-LY-TAX
           MOVE INCR-ADDONS          TO INCR-LY-ADDONS
           MOVE INCR-DISCOUNT        TO INCR-LY-DISCOUNT
           MOVE INCR-INVCRED-COST    TO INCR-LY-INVCRED-COST
           MOVE INCR-SB-TYPE         TO INCR-LY-SB-TYPE
           MOVE INCR-DRTRANS-NO      TO INCR-LY-DRTRANS-NO
           MOVE INCR-PULLBY          TO INCR-LY-PULLBY
           MOVE INCR-AREA            TO INCR-LY-AREA
           MOVE INCR-PART-ORDERS     TO INCR-LY-PART-ORDERS
           MOVE INCR-PULL-DATE       TO INCR-LY-PULL-DATE
           MOVE INCR-PULL-TIME       TO INCR-LY-PULL-TIME
           MOVE INCR-PRINTED         TO INCR-LY-PRINTED
           MOVE INCR-COPY-NUMBER     TO INCR-LY-COPY-NUMBER.

           IF INCR-TRANS = 7
               GO TO WRL-030.

           MOVE  INCR-NAME           TO  INCR-LY-NAME
           MOVE  INCR-ADD1           TO  INCR-LY-ADD1
           MOVE  INCR-ADD2           TO  INCR-LY-ADD2
           MOVE  INCR-ADD3           TO  INCR-LY-ADD3
           MOVE  INCR-CODE           TO  INCR-LY-CODE
           MOVE  INCR-DEL1           TO  INCR-LY-DEL1
           MOVE  INCR-DEL2           TO  INCR-LY-DEL2
           MOVE  INCR-DEL3           TO  INCR-LY-DEL3
           MOVE  INCR-TERMS          TO  INCR-LY-TERMS
           MOVE  INCR-PHONE          TO  INCR-LY-PHONE
           MOVE  INCR-CONTACT        TO  INCR-LY-CONTACT
           MOVE  INCR-DELIVERY       TO  INCR-LY-DELIVERY
           MOVE  INCR-BIN            TO  INCR-LY-BIN
           MOVE  INCR-COMMENT        TO  INCR-LY-COMMENT
           MOVE  INCR-BO-INV-NO      TO  INCR-LY-BO-INV-NO
           MOVE  INCR-BO-DATE        TO  INCR-LY-BO-DATE
           MOVE  INCR-ADDPOST        TO  INCR-LY-ADDPOST
           MOVE  INCR-ADDFREIGHT     TO  INCR-LY-ADDFREIGHT
           MOVE  INCR-ADDLABOUR      TO  INCR-LY-ADDLABOUR
           MOVE  INCR-ADDMISC        TO  INCR-LY-ADDMISC
           MOVE  INCR-LINENO         TO  INCR-LY-LINENO.
        WRL-030.
           IF INCR-TRANS NOT = 7
              GO TO WRL-500.
           MOVE SPACES               TO  INCR-LY-KIT-FIELDS.
           MOVE  INCR-KITNAME        TO  INCR-LY-KITNAME
           MOVE  INCR-KITQTY         TO  INCR-LY-KITQTY
           MOVE  INCR-KITSHPDQTY     TO  INCR-LY-KITSHPDQTY
           MOVE  INCR-KITDESC1       TO  INCR-LY-KITDESC1
           MOVE  INCR-KITDESC2       TO  INCR-LY-KITDESC2
           MOVE  INCR-KITPRICE       TO  INCR-LY-KITPRICE
           MOVE  INCR-KITCOMMENT     TO  INCR-LY-KITCOMMENT.
        WRL-500.
           WRITE INCR-LY-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-LY-ST1 NOT = 0
           
               MOVE "REG-LY NOT =0" TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020

              MOVE "SL-REG MOVE TO L/Y  " TO WS-DAILY-1ST
              MOVE INCR-LY-INVOICE        TO WS-DAILY-2ND
              MOVE "NOT WRITTEN IN PER- " TO WS-DAILY-3RD
              MOVE "END ROUTINE.        " TO WS-DAILY-4TH
              PERFORM WRITE-DAILY.
       WRL-999.
            EXIT.
      *
       DELETE-STOCK-TRANS SECTION.
       CST-000.
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            MOVE "11. St Trans-File   " TO WS-DAILY-1ST
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       CST-005.
           PERFORM OPEN-055.
           MOVE 1910 TO POS
           DISPLAY "11. Stock Transactions being processed....." AT POS.
           IF WS-ANSWER1 = "Y"
               PERFORM DELETE-STRANSLY-FILE
               PERFORM OPEN-056.
      *     MOVE "Y" TO STTR-COMPLETE.
      *     START STOCK-TRANS-FILE KEY NOT < STTR-COMPLETE
           MOVE 0  TO STTR-KEY.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
               GO TO CST-900.
       CST-010.
            READ STOCK-TRANS-FILE NEXT WITH LOCK
               AT END
               GO TO CST-900.
           IF WS-STTRANS-ST1 = 10 OR = 23
               GO TO CST-900.
               
            MOVE 2910 TO POS.
            DISPLAY "St-Trans Being Processed" AT POS.
            ADD 26 TO POS.
            DISPLAY STTR-REFERENCE1 AT POS.
            ADD 10 TO POS
            DISPLAY STTR-TYPE AT POS
            ADD 10 TO POS
            DISPLAY STTR-COMPLETE AT POS
            ADD 10 TO POS
            DISPLAY STTR-DATE AT POS.
            
      *      IF STTR-COMPLETE NOT = "Y"
      *         GO TO CST-900.
            IF WS-STTRANS-ST1 = 91
              MOVE 3010 TO POS
              DISPLAY "WS-STTRANS-ST1 = 9" AT POS
              PERFORM CST-900
              PERFORM CST-005
              GO TO CST-010.
       CST-020.
      ******************************************************************
      * FEB 2003.                                                      *
      * NEW SECTION SO THAT WE STORE THE WHOLE YEARS INFO EXCEPT FOR   *
      * P/SLIPS (INCR-TRANS=4) WHICH WILL BE DELETED AFTER 6 MONTHS AS *
      * ORIGINALLY.                                                    *
      * AT YEAR END THE INV, C/NOTE, REPAIR & B/M TRANS WILL BE COPIED *
      * INTO A NEW SLREGLY FILE AND DELETED FROM THE CURRENT FILE.     *
      ******************************************************************
      
           IF WS-ANSWER1 = "M"
            IF STTR-TYPE = 1 OR = 3 OR = 4 OR = 6 OR = 7 OR = 8
               GO TO CST-010.
           IF WS-ANSWER1 = "Y"
            IF STTR-TYPE = 3 OR = 4
               GO TO CST-010.
      *     IF WS-ANSWER1 = "Y"
      *      IF STTR-TYPE = 4
      *       IF STTR-COMPLETE = "Y"
      *         GO TO CST-600.

           IF WS-ANSWER1 = "Y"
            IF STTR-TYPE = 1 OR = 6 OR = 7
             IF STTR-COMPLETE = "Y"
               PERFORM WRITE-STTRANS-LY
               GO TO CST-800
           ELSE 
               GO TO CST-010.

           GO TO CST-010.
       CST-600.
           MOVE STTR-DATE TO WS-PORDER-DATE.   
           COMPUTE WS-WORK-PO-MM = WS-PO-MM + 6.
           IF WS-WORK-PO-MM > 12
               COMPUTE WS-WORK-PO-MM = WS-WORK-PO-MM - 12.
           IF WS-WORK-PO-MM = WS-MM
               GO TO CST-800.
           IF WS-PO-YY = WS-YY
            IF WS-PO-MM + 6 < WS-MM
               GO TO CST-800.
           IF WS-PO-YY < WS-YY
            IF WS-PO-MM < 6
               GO TO CST-800
            ELSE
             IF WS-PO-MM - 6 < WS-MM
               GO TO CST-800.
           GO TO CST-010.
       CST-800.
            DELETE STOCK-TRANS-FILE
               INVALID KEY
                MOVE "STOCK TRANS RECORD N"  TO WS-DAILY-1ST
                MOVE "OT DELETED, NUMBER :"  TO WS-DAILY-2ND
                MOVE STTR-TRANSACTION-NUMBER TO WS-DAILY-3RD
                MOVE " "                     TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           MOVE 1910 TO POS
           DISPLAY
           "11. Stock Transactions being processed.....             "
            AT POS.
            GO TO CST-010.
       CST-900.
           CLOSE STOCK-TRANS-FILE.
           IF WS-ANSWER1 = "Y"
              CLOSE STOCK-TRANSLY-FILE.
           MOVE 1910 TO POS
           DISPLAY 
           "11. Stock Transactions processed.                      "
           AT POS.
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 3010 TO POS
           DISPLAY WS-MESSAGE AT POS.
       CST-999.
           EXIT.
      *
       WRITE-STTRANS-LY SECTION.
       WR-ST-LY-010.
           MOVE 1910 TO POS
           DISPLAY
           "11. Stock Transactions being processed..... - LY WRITE.."
            AT POS.
       WR-ST-LY-015.
           MOVE STTR-TYPE               TO STTR-LY-TYPE
           MOVE STTR-REFERENCE1         TO STTR-LY-REFERENCE1
           MOVE STTR-TRANSACTION-NUMBER TO STTR-LY-TRANSACTION-NUMBER
           MOVE STTR-ST-COMPLETE        TO STTR-LY-ST-COMPLETE
           MOVE STTR-STOCK-NUMBER       TO STTR-LY-STOCK-NUMBER
           MOVE STTR-AC-COMPLETE        TO STTR-LY-AC-COMPLETE
           MOVE STTR-ACCOUNT-NUMBER     TO STTR-LY-ACCOUNT-NUMBER
           MOVE STTR-INV-NO             TO STTR-LY-INV-NO
           MOVE STTR-DATE               TO STTR-LY-DATE
           MOVE STTR-COMPLETE           TO STTR-LY-COMPLETE.

           MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               GO TO WR-ST-LY-020.

           MOVE STTR-ORDERQTY    TO STTR-LY-ORDERQTY
           MOVE STTR-SHIPQTY     TO STTR-LY-SHIPQTY
           MOVE STTR-SHIPPEDQTY  TO STTR-LY-SHIPPEDQTY
           MOVE STTR-SALES-VALUE TO STTR-LY-SALES-VALUE
           MOVE STTR-COST-VALUE  TO STTR-LY-COST-VALUE
           MOVE STTR-PRICE       TO STTR-LY-PRICE
           MOVE STTR-DESC1       TO STTR-LY-DESC1
           MOVE STTR-DESC2       TO STTR-LY-DESC2
           MOVE STTR-ITEMDISC    TO STTR-LY-ITEMDISC
           MOVE STTR-TAX         TO STTR-LY-TAX
           MOVE STTR-UNIT        TO STTR-LY-UNIT.
           
           GO TO WR-ST-LY-030.
        WR-ST-LY-020.
           MOVE SPACES       TO COMMENT-LY-FIELDS.
           MOVE COM-ORDERQTY TO COM-LY-ORDERQTY
           MOVE COM-SHIPQTY  TO COM-LY-SHIPQTY
           MOVE COM-DESC     TO COM-LY-DESC
           MOVE COM-UNIT     TO COM-LY-UNIT
           MOVE COM-PRICE    TO COM-LY-PRICE
           MOVE COM-COST     TO COM-LY-COST
           MOVE COM-DISC     TO COM-LY-DISC
           MOVE " "          TO COM-LY-FILLER.
        WR-ST-LY-030.
           WRITE STOCK-TRANSLY-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 NOT = 0
              MOVE "ST-TRANS MOVE TO L/Y" TO WS-DAILY-1ST
              MOVE STTR-LY-REFERENCE1     TO WS-DAILY-2ND
              MOVE "NOT WRITTEN IN PER- " TO WS-DAILY-3RD
              MOVE "END ROUTINE.        " TO WS-DAILY-4TH
              PERFORM WRITE-DAILY.
        WR-ST-LY-EXIT.
           EXIT.
      *
       DELETE-STOCK-RECEIPTS SECTION.
       DSR-000.
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            MOVE "12. St-Receipts File" TO WS-DAILY-1ST
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       DSR-005.
           PERFORM OPEN-060.
           IF WS-ANSWER1 = "Y"
              PERFORM OPEN-061.
           MOVE 2010 TO POS
           DISPLAY "12. Stock Receipts being processed........." AT POS.
           MOVE 0 TO STRE-KEY.
           START STKRECEIPTS-FILE KEY NOT < STRE-KEY 
                INVALID KEY NEXT SENTENCE.
            IF WS-STK-ST1 NOT = 0
               GO TO DSR-900.
       DSR-010.
            READ STKRECEIPTS-FILE NEXT WITH LOCK
               AT END
               GO TO DSR-900.
            MOVE 2910 TO POS
            DISPLAY "St-Receipts Being Processed" AT POS
            ADD 28 TO POS
            DISPLAY STRE-TRANSACTION-NUMBER AT POS.
            IF WS-STK-ST1 NOT = 0
               MOVE "STK-RECEIPTS STATUS NOT = 0, GOING TO RE-READ."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO DSR-010.

           IF WS-ANSWER1 = "Y"
               PERFORM WRITE-ST-RECEIPTSLY
               GO TO DSR-800
           ELSE 
               GO TO DSR-010.
            GO TO DSR-010.
       DSR-800.
            DELETE STKRECEIPTS-FILE
               INVALID KEY
                MOVE "STK-RECEIPT RECORD N"  TO WS-DAILY-1ST
                MOVE "OT DELETED, NUMBER :"  TO WS-DAILY-2ND
                MOVE STRE-TRANSACTION-NUMBER TO WS-DAILY-3RD
                MOVE " "                     TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            GO TO DSR-010.
       DSR-900.
           CLOSE STKRECEIPTS-FILE.
           IF WS-ANSWER1 = "Y"
              CLOSE STKRECEIPTSLY-FILE.
           MOVE 2010 TO POS
           DISPLAY 
           "12. Stock Receipts processed.                             " 
           AT POS
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 3010 TO POS
           DISPLAY WS-MESSAGE AT POS.
       DSR-999.
            EXIT.
      *
       WRITE-ST-RECEIPTSLY SECTION.
       WRSTKLY-010.
           MOVE 2010 TO POS
           DISPLAY
           "12. Stock Receipts being processed......... - LY WRITE.."
            AT POS.
       WRSTKLY-015.
           MOVE STRE-TRANSACTION-NUMBER  TO STRELY-TRANSACTION-NUMBER
           MOVE STRE-TRANSACTION-CODE    TO STRELY-TRANSACTION-CODE
           MOVE STRE-STOCK-NUMBER        TO STRELY-STOCK-NUMBER
           MOVE STRE-QUANTITY            TO STRELY-QUANTITY
           MOVE STRE-UNIT-PRICE          TO STRELY-UNIT-PRICE
           MOVE STRE-TOTAL-PRICE         TO STRELY-TOTAL-PRICE
           MOVE STRE-REFERENCE-NO        TO STRELY-REFERENCE-NO
           MOVE STRE-REFERENCE-DATE      TO STRELY-REFERENCE-DATE
           MOVE STRE-ORDER-NUMBER        TO STRELY-ORDER-NUMBER.
        WRSTKLY-030.
           WRITE STOCK-RECEIPTSLY-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-STKLY-ST1 NOT = 0
              MOVE "STK-RECEIPTS TO L/Y " TO WS-DAILY-1ST
              MOVE STTR-LY-REFERENCE1     TO WS-DAILY-2ND
              MOVE "NOT WRITTEN IN PER- " TO WS-DAILY-3RD
              MOVE "END ROUTINE.        " TO WS-DAILY-4TH
              PERFORM WRITE-DAILY.
        WRSTKLY-EXIT.
           EXIT.
      *
       CHANGE-GL-PARAMETER SECTION.
       CGP-000.
           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR TO SPLIT-HR
           MOVE ":"   TO SPLIT-FIL1
           MOVE WS-MIN TO SPLIT-MN
           MOVE ":"   TO SPLIT-FIL2
           MOVE WS-SEC TO SPLIT-SC
           MOVE "13. Gl-ParameterFile" TO WS-DAILY-1ST
           MOVE "Start of process    " TO WS-DAILY-2ND
           MOVE SPLIT-TIME             TO WS-DAILY-3RD
           PERFORM WRITE-DAILY
           MOVE 2110 TO POS
           DISPLAY "13. Gl-Parameter being processed..........." AT POS.
       CGP-005.
           PERFORM OPEN-065.
       CGP-010.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO GLPARAMETER RECORD ON CGP-010 READ"
                   TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               STOP RUN.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO CGP-000.
       CGP-020.
           ADD 1 TO GLPA-CURRENT-SLPER.
           IF GLPA-CURRENT-SLPER > 12
               MOVE 1 TO GLPA-CURRENT-SLPER.
       CGP-030.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "GLPARAMETER RECORD NOT UPDATED!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               STOP RUN.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO CGP-030.
           MOVE 2110 TO POS.
           DISPLAY "13. Gl-Parameter processed.                " AT POS.
           MOVE " " TO WS-MESSAGE.
           MOVE 2910 TO POS.
           DISPLAY WS-MESSAGE AT POS.
       CGP-900.
           CLOSE GLPARAMETER-FILE.
       CGP-999.
           EXIT.
      *
       DELETE-QUOTE-REGISTER SECTION.
       DQR-000.
            ACCEPT WS-TIME FROM TIME
            PERFORM GET-SYSTEM-Y2K-DATE.
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            MOVE "14. Quote-File      " TO WS-DAILY-1ST
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       DQR-004.
           PERFORM OPEN-055.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
       DQR-005.
           PERFORM OPEN-050.
           IF WS-ANSWER1 = "Y"
               PERFORM OPEN-057.
           MOVE 2210 TO POS.
           DISPLAY "14. Quote File Being Processed............." AT POS.
           MOVE 8 TO INCR-TRANS
           MOVE 1 TO INCR-INVOICE.
           START INCR-REGISTER KEY NOT < INCR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               GO TO DQR-900.
       DQR-010.
           READ INCR-REGISTER NEXT WITH LOCK
               AT END
               GO TO DQR-900.
           MOVE 2910 TO POS
           DISPLAY "Quote # Being Processed:" AT POS
           ADD 24 TO POS
           DISPLAY INCR-INVOICE AT POS
           ADD 10 TO POS
           DISPLAY INCR-DATE AT POS.
           IF WS-INCR-ST1 = 91
              PERFORM DQR-900
              PERFORM DQR-004
              PERFORM DQR-005
              GO TO DQR-010.
           IF INCR-TRANS NOT = 8
              GO TO DQR-900.
       DQR-020.
           MOVE INCR-DATE TO SPLIT-DATE.
           ADD 6 TO SPLIT-MM.
           IF SPLIT-MM > 12
               COMPUTE SPLIT-MM = SPLIT-MM - 12
               ADD 1 TO SPLIT-YY.
           IF SPLIT-DATE < WS-DATE
               GO TO DQR-800.

           GO TO DQR-010.
       DQR-800.
           PERFORM DELETE-QUOTE-TRANSACTIONS.
           DELETE INCR-REGISTER
               INVALID KEY
                MOVE "QUOTE-REG RECORD NOT" TO WS-DAILY-1ST
                MOVE "DELETED             " TO WS-DAILY-2ND
                MOVE INCR-INVOICE           TO WS-DAILY-3RD
                MOVE " "                    TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO DQR-010.
       DQR-900.
           CLOSE INCR-REGISTER
           CLOSE STOCK-TRANS-FILE
           IF WS-ANSWER1 = "Y"
              CLOSE STOCK-TRANSLY-FILE
              CLOSE INCR-LY-REGISTER.
           MOVE 2210 TO POS
           DISPLAY "14. Quote File Processed.                  " AT POS
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS.
           
           IF WS-ANSWER1 = "Y"
              GO TO DQR-999.
           
           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR TO SPLIT-HR
           MOVE ":"   TO SPLIT-FIL1
           MOVE WS-MIN TO SPLIT-MN
           MOVE ":"   TO SPLIT-FIL2
           MOVE WS-SEC TO SPLIT-SC
           MOVE " Sales: Period End :" TO WS-DAILY-1ST
           MOVE "All Files Processed." TO WS-DAILY-2ND
           MOVE SPLIT-TIME             TO WS-DAILY-3RD
           PERFORM WRITE-DAILY.
       DQR-999.
            EXIT.
      *
       DELETE-QUOTE-TRANSACTIONS SECTION.
       DQT-005.
           MOVE 8            TO STTR-TYPE.
           MOVE INCR-INVOICE TO STTR-REFERENCE1.
           MOVE 1            TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
       DQT-010.
           READ STOCK-TRANS-FILE NEXT WITH LOCK
               AT END
               GO TO DQT-999.
           IF WS-STTRANS-ST1 NOT = 0
               GO TO DQT-999.
           IF STTR-TYPE NOT = 8
               GO TO DQT-999.
           IF STTR-REFERENCE1 NOT = INCR-INVOICE
               GO TO DQT-999.
       DQT-800.
            DELETE STOCK-TRANS-FILE
               INVALID KEY
                MOVE "STOCK TRANS RECORD N"  TO WS-DAILY-1ST
                MOVE "OT DELETED, NUMBER :"  TO WS-DAILY-2ND
                MOVE STTR-TRANSACTION-NUMBER TO WS-DAILY-3RD
                MOVE " "                     TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            GO TO DQT-010.
       DQT-999.
            EXIT.
      *
       DELETE-REPAIR-REGISTER SECTION.
       DRR-000.
            ACCEPT WS-TIME FROM TIME
            PERFORM GET-SYSTEM-Y2K-DATE.
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            MOVE "15. Repair-File     " TO WS-DAILY-1ST
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
           IF WS-ANSWER1 = "M"
                GO TO DRR-900.
       DRR-004.
           PERFORM OPEN-055.
           IF WS-ANSWER1 = "Y"
              PERFORM OPEN-057.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
       DRR-005.
           PERFORM OPEN-050.
           MOVE 2310 TO POS.
           DISPLAY "15. Repair File Being Processed............" AT POS.
           MOVE 3 TO INCR-TRANS
           MOVE 1 TO INCR-INVOICE.
           START INCR-REGISTER KEY NOT < INCR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               GO TO DRR-900.
       DRR-010.
           READ INCR-REGISTER NEXT WITH LOCK
               AT END
               GO TO DRR-900.
           MOVE 2910 TO POS
           DISPLAY "Repair # Being Processed:" AT POS
           ADD 25 TO POS
           DISPLAY INCR-INVOICE AT POS
           ADD 10 TO POS
           DISPLAY INCR-DATE AT POS.
           IF WS-INCR-ST1 = 91
              PERFORM DRR-900
              PERFORM DRR-004
              PERFORM DRR-005
              GO TO DRR-010.
           IF INCR-TRANS NOT = 3
              GO TO DRR-900.
       DRR-020.
           IF INCR-PRINTED = "Y"
              GO TO DRR-800.

           GO TO DRR-010.
       DRR-800.
           PERFORM DELETE-REPAIR-TRANSACTIONS.
           
           DELETE INCR-REGISTER
               INVALID KEY
                MOVE "REPAIR-RG RECORD NOT" TO WS-DAILY-1ST
                MOVE "DELETED             " TO WS-DAILY-2ND
                MOVE INCR-INVOICE           TO WS-DAILY-3RD
                MOVE " "                    TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO DRR-010.
       DRR-900.
           CLOSE INCR-REGISTER
           CLOSE STOCK-TRANS-FILE.
           IF WS-ANSWER1 = "Y"
               CLOSE STOCK-TRANSLY-FILE.
           MOVE 2310 TO POS
           DISPLAY "15. Repair File Processed.                 " AT POS
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS.

           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR TO SPLIT-HR
           MOVE ":"   TO SPLIT-FIL1
           MOVE WS-MIN TO SPLIT-MN
           MOVE ":"   TO SPLIT-FIL2
           MOVE WS-SEC TO SPLIT-SC
           MOVE " Sales: Period End :" TO WS-DAILY-1ST
           MOVE "All Files Processed." TO WS-DAILY-2ND
           MOVE SPLIT-TIME             TO WS-DAILY-3RD
           PERFORM WRITE-DAILY.
       DRR-999.
            EXIT.
      *      
       DELETE-REPAIR-TRANSACTIONS SECTION.
       DRT-005.
           MOVE 3            TO STTR-TYPE.
           MOVE INCR-INVOICE TO STTR-REFERENCE1.
           MOVE 1            TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
                
           START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-KEY
                INVALID KEY NEXT SENTENCE.
       DRT-010.
           READ STOCK-TRANS-FILE NEXT WITH LOCK
               AT END
               GO TO DRT-999.
           IF WS-STTRANS-ST1 NOT = 0
               GO TO DRT-999.
           IF STTR-TYPE NOT = 3
               GO TO DRT-999.
           IF STTR-REFERENCE1 NOT = INCR-INVOICE
               GO TO DRT-999.
       DRT-800.
            PERFORM WR-ST-LY-015 THRU WR-ST-LY-030.
       
            DELETE STOCK-TRANS-FILE
               INVALID KEY
                MOVE "REPAIR STTRANS REC N"  TO WS-DAILY-1ST
                MOVE "OT DELETED, NUMBER :"  TO WS-DAILY-2ND
                MOVE STTR-TRANSACTION-NUMBER TO WS-DAILY-3RD
                MOVE " "                     TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            GO TO DRT-010.
       DRT-999.
            EXIT.
      *
       DELETE-PSLIP-REGISTER SECTION.
       DPSLP-000.
            ACCEPT WS-TIME FROM TIME
            PERFORM GET-SYSTEM-Y2K-DATE.
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            MOVE "16. P/Slip-File     " TO WS-DAILY-1ST
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
           IF WS-ANSWER1 = "M"
                GO TO DPSLP-900.
       DPSLP-004.
           PERFORM OPEN-055.
           PERFORM OPEN-057.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
       DPSLP-005.
           PERFORM OPEN-050.
           PERFORM OPEN-051.
           MOVE 2410 TO POS.
           DISPLAY "16. P/Slip File Being Processed..........." AT POS.
           MOVE 4 TO INCR-TRANS
           MOVE 1 TO INCR-INVOICE.
           START INCR-REGISTER KEY NOT < INCR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               GO TO DPSLP-900.
       DPSLP-010.
           READ INCR-REGISTER NEXT WITH LOCK
               AT END
               GO TO DPSLP-900.
           MOVE 2910 TO POS
           DISPLAY "P/Slip # Being Processed:" AT POS
           ADD 25 TO POS
           DISPLAY INCR-INVOICE AT POS
           ADD 10 TO POS
           DISPLAY INCR-DATE AT POS.
           
           IF WS-INCR-ST1 = 91
              PERFORM DPSLP-900
              PERFORM DPSLP-004
              PERFORM DPSLP-005
              GO TO DPSLP-010.
           IF INCR-TRANS NOT = 4
              GO TO DPSLP-900.
       DPSLP-020.
           IF INCR-PRINTED = "Y"
            IF INCR-TRANS = 4

      *         MOVE "GOING TO WRITE-REG-LY" TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               
              PERFORM WRITE-REG-LY.
      *        GO TO DPSLP-800.

           GO TO DPSLP-010.
       DPSLP-800.

      *         MOVE "GOING TO DELETE-TRANS" TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               
      *     PERFORM DELETE-PSLIP-TRANSACTIONS.
           
           DELETE INCR-REGISTER
               INVALID KEY
               
               MOVE "REGISTER NOT DELETED" TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               
                MOVE "P/SLIP-RG RECORD NOT" TO WS-DAILY-1ST
                MOVE "DELETED             " TO WS-DAILY-2ND
                MOVE INCR-INVOICE           TO WS-DAILY-3RD
                MOVE " "                    TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO DPSLP-010.
       DPSLP-900.
           CLOSE INCR-REGISTER
                 STOCK-TRANS-FILE.
           IF WS-ANSWER1 = "Y"
                 CLOSE INCR-LY-REGISTER
                       STOCK-TRANSLY-FILE
           MOVE 2410 TO POS
           DISPLAY "16. Repair File Processed.                 " AT POS
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS.

           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR TO SPLIT-HR
           MOVE ":"   TO SPLIT-FIL1
           MOVE WS-MIN TO SPLIT-MN
           MOVE ":"   TO SPLIT-FIL2
           MOVE WS-SEC TO SPLIT-SC
           MOVE " Sales: Period End :" TO WS-DAILY-1ST
           MOVE "All Files Processed." TO WS-DAILY-2ND
           MOVE SPLIT-TIME             TO WS-DAILY-3RD
           PERFORM WRITE-DAILY.
       DPSLP-999.
            EXIT.
      *      
       DELETE-PSLIP-TRANSACTIONS SECTION.
       DPSTR-005.
           MOVE 4            TO STTR-TYPE.
           MOVE INCR-INVOICE TO STTR-REFERENCE1.
           MOVE 1            TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
                
           START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-KEY
                INVALID KEY NEXT SENTENCE.
       DPSTR-010.
           READ STOCK-TRANS-FILE NEXT WITH LOCK
               AT END
               GO TO DPSTR-999.
           IF WS-STTRANS-ST1 NOT = 0
               GO TO DPSTR-999.
           IF STTR-TYPE NOT = 4
               GO TO DPSTR-999.
           IF STTR-REFERENCE1 NOT = INCR-INVOICE
               GO TO DPSTR-999.
       DPSTR-800.
            PERFORM WR-ST-LY-015 THRU WR-ST-LY-030.

            DELETE STOCK-TRANS-FILE
               INVALID KEY
                MOVE "PSLIP STTRANS REC. N"  TO WS-DAILY-1ST
                MOVE "OT DELETED, NUMBER :"  TO WS-DAILY-2ND
                MOVE STTR-TRANSACTION-NUMBER TO WS-DAILY-3RD
                MOVE " "                     TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            GO TO DPSTR-010.
       DPSTR-999.
            EXIT.
      *
       READ-SL-PARAMETER SECTION.
       R-PAR-005.
           PERFORM OPEN-010.
       R-PAR-010.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           READ PARAMETER-FILE WITH LOCK
                INVALID KEY
                MOVE "PARAMETER NOT FOUND ON READ-LOCK, 'ESC' TO EXIT."
                 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                EXIT PROGRAM.
       R-PAR-010.
            MOVE PA-CURRENT-PER-WW TO WS-CALC-WW.
            ADD 1                  TO PA-CURRENT-PER-WW.
            REWRITE PARAMETER-REC
                INVALID KEY
                MOVE "PARAMETER RECORD" TO WS-DAILY-1ST
                MOVE "NOT UPDATED!!!  " TO WS-DAILY-2ND
                MOVE "FOR CURRENT WEEK" TO WS-DAILY-3RD
                MOVE WS-CALC-WW         TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
        R-PAR-020.
            MOVE WS-CALC-WW             TO WS-JRN-WW.
            CLOSE PARAMETER-FILE.
        R-PAR-999.
            EXIT.
      *
       WRITE-DIST-TO-GL SECTION.
       WDTG-001.
           PERFORM OPEN-065.
           PERFORM READ-PARAMETER-LOCK.
           MOVE WS-MONTH-DESC (GLPA-CURRENT-SLPER) TO WS-JRN-MONTH.
       WDTG-002.
           PERFORM OPEN-070.
       WDTG-005.
           PERFORM OPEN-000.
           MOVE "1" TO DIST-KEY.
       WDTG-010.
           READ DISTRIBUTIONS
                INVALID KEY
                MOVE "DISTRIBUTION RECORD NOT FOUND IN GL-SALES-WRITE."
                     TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                EXIT PROGRAM.
            MOVE 2910 TO POS.
            DISPLAY "Writing Sales Figures to General Ledger." AT POS.
           MOVE 1 TO SUB-1.
       WDTG-025.
           IF SUB-1 = 1
      *        MOVE DIST-INVOICEWEEK       TO WS-DIST-AMOUNTS (SUB-1)
      *        MOVE GST-AMT-TAXABLE-WEEK   TO WS-DIST-AMOUNTS (SUB-1)
              COMPUTE WS-DIST-AMOUNTS (SUB-1) = 
                GST-AMT-TAXABLE-WEEK + DIST-CNOTEWEEK
              COMPUTE WS-DIST-AMOUNTS (SUB-1) =
                             WS-DIST-AMOUNTS (SUB-1) * -1
              ADD WS-DIST-AMOUNTS (SUB-1) TO WS-BATCH-TOTAL
              MOVE "INVOICES  "           TO WS-DIST-DESC (SUB-1)
              MOVE GLPA-GLSALES-ACC       TO WS-DIST-ACCNO (SUB-1).
           IF SUB-1 = 2
              MOVE DIST-PAYMENTWEEK       TO WS-DIST-AMOUNTS (SUB-1)
              ADD WS-DIST-AMOUNTS (SUB-1) TO WS-BATCH-TOTAL
              MOVE "RECEIPTS  "           TO WS-DIST-DESC (SUB-1)
              MOVE GLPA-GLDRBANK          TO WS-DIST-ACCNO (SUB-1).
           IF SUB-1 = 3
              MOVE DIST-JOURNALDRWEEK     TO WS-DIST-AMOUNTS (SUB-1)
              COMPUTE WS-DIST-AMOUNTS (SUB-1) =
                             WS-DIST-AMOUNTS (SUB-1) * -1
              ADD WS-DIST-AMOUNTS (SUB-1) TO WS-BATCH-TOTAL
              MOVE "DR-JOURNL "           TO WS-DIST-DESC (SUB-1)
              MOVE GLPA-GLSALES-ADJ       TO WS-DIST-ACCNO (SUB-1).
           IF SUB-1 = 4
              MOVE DIST-JOURNALCRWEEK     TO WS-DIST-AMOUNTS (SUB-1)
              ADD WS-DIST-AMOUNTS (SUB-1) TO WS-BATCH-TOTAL
              MOVE "CR-JOURNL "           TO WS-DIST-DESC (SUB-1)
              MOVE GLPA-GLSALES-ADJ       TO WS-DIST-ACCNO (SUB-1).
           IF SUB-1 = 5
              MOVE DIST-CNOTEWEEK         TO WS-DIST-AMOUNTS (SUB-1)
              ADD WS-DIST-AMOUNTS (SUB-1) TO WS-BATCH-TOTAL
              MOVE "C-NOTES   "           TO WS-DIST-DESC (SUB-1)
              MOVE GLPA-GLSALES-ACC       TO WS-DIST-ACCNO (SUB-1).
           IF SUB-1 = 6
              MOVE DIST-INTERESTWEEK      TO WS-DIST-AMOUNTS (SUB-1)
              COMPUTE WS-DIST-AMOUNTS (SUB-1) =
                             WS-DIST-AMOUNTS (SUB-1) * -1
              ADD WS-DIST-AMOUNTS (SUB-1) TO WS-BATCH-TOTAL
              MOVE "INTEREST  "           TO WS-DIST-DESC (SUB-1)
              MOVE "60-200-15-00"         TO WS-DIST-ACCNO (SUB-1).
      *        MOVE GLPA-GLSALES-ACC       TO WS-DIST-ACCNO (SUB-1).
           IF SUB-1 = 7
              MOVE DIST-DISCOUNTWEEK      TO WS-DIST-AMOUNTS (SUB-1)
              ADD WS-DIST-AMOUNTS (SUB-1) TO WS-BATCH-TOTAL
              MOVE "PAY-DISCNT"           TO WS-DIST-DESC (SUB-1)
              MOVE GLPA-GLSALES-DISC      TO WS-DIST-ACCNO (SUB-1).
           IF SUB-1 = 8
              MOVE DIST-ADDONWEEK         TO WS-DIST-AMOUNTS (SUB-1)
              COMPUTE WS-DIST-AMOUNTS (SUB-1) =
                             WS-DIST-AMOUNTS (SUB-1) * -1
              ADD WS-DIST-AMOUNTS (SUB-1) TO WS-BATCH-TOTAL
              MOVE "ADD-ONS   "           TO WS-DIST-DESC (SUB-1)
              MOVE GLPA-GLSALES-ADDONS    TO WS-DIST-ACCNO (SUB-1).
           IF SUB-1 = 9
              MOVE GST-AMT-TAXED-WEEK     TO WS-DIST-AMOUNTS (SUB-1)
              COMPUTE WS-DIST-AMOUNTS (SUB-1) =
                             WS-DIST-AMOUNTS (SUB-1) * -1
              ADD WS-DIST-AMOUNTS (SUB-1) TO WS-BATCH-TOTAL
              MOVE "VAT-O/PUT "           TO WS-DIST-DESC (SUB-1)
              MOVE GLPA-GLVATOUTPUT-ACC   TO WS-DIST-ACCNO (SUB-1).
           IF SUB-1 = 10
              MOVE GST-AMT-EXPORT-WEEK    TO WS-DIST-AMOUNTS (SUB-1)
              COMPUTE WS-DIST-AMOUNTS (SUB-1) =
                             WS-DIST-AMOUNTS (SUB-1) * -1
              ADD WS-DIST-AMOUNTS (SUB-1) TO WS-BATCH-TOTAL
              MOVE "INVOICES  "           TO WS-DIST-DESC (SUB-1)
              MOVE "60-200-07-00"         TO WS-DIST-ACCNO (SUB-1).
           IF SUB-1 = 11
              MOVE DIST-RDCHEQUEWEEK      TO WS-DIST-AMOUNTS (SUB-1)
              COMPUTE WS-DIST-AMOUNTS (SUB-1) =
                             WS-DIST-AMOUNTS (SUB-1) * -1
              ADD WS-DIST-AMOUNTS (SUB-1) TO WS-BATCH-TOTAL
              MOVE "RD-CHEQUES"           TO WS-DIST-DESC (SUB-1)
              MOVE GLPA-GLDRBANK          TO WS-DIST-ACCNO (SUB-1).
           IF SUB-1 = 12
              MOVE DIST-BDEBTWEEK         TO WS-DIST-AMOUNTS (SUB-1)
              ADD WS-DIST-AMOUNTS (SUB-1) TO WS-BATCH-TOTAL
              MOVE "B-DEBTS   "           TO WS-DIST-DESC (SUB-1)
              MOVE GLPA-GLBDEBT-ACC       TO WS-DIST-ACCNO (SUB-1).
           ADD 1 TO SUB-1.
           IF SUB-1 < 13
               GO TO WDTG-025.
           MOVE 1 TO SUB-1.
           COMPUTE WS-BATCH-TOTAL = WS-BATCH-TOTAL * -1.
           IF WS-BATCH-TOTAL NOT = DIST-ACCRECWEEK
                MOVE "BATCH-TOTALS & DIST-" TO WS-DAILY-1ST
                MOVE "ACC-REC. IN IM-BAL. " TO WS-DAILY-2ND
                MOVE "FOR CURRENT WEEK    " TO WS-DAILY-3RD
                MOVE WS-CALC-WW             TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           PERFORM REWRITE-PARAMETER.
           PERFORM WRTR-000.
       WDTG-030.
           IF WS-DIST-AMOUNTS (SUB-1) = 0
               GO TO WDTG-031.
           PERFORM WRTR-010 THRU WRTR-015.
           PERFORM UPDATE-GLMASTER.
           PERFORM UPDATE-GLHEADER.
           PERFORM UPDATE-GLSUBHEADER.
           PERFORM WRITE-DR-ACC-TRANS.
       WDTG-031.
           ADD 1 TO SUB-1.
           IF SUB-1 < 11
               GO TO WDTG-030.
           MOVE 1 TO SUB-1.
      **************************************************************
      *SUB-1=11 OR 12 TRANS WILL NOT BE WRITTEN AS THEY ARE        *
      *       WRITTEN IMMEDIATELY FROM DrPaymnt.Int. ONLY INCLUDED *
      *       TO CHECK BATCH TOTALS ARE EQUAL.                     *
      **************************************************************
       WDTG-900.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-GLPER
               PERFORM WRTR-900
               GO TO WDTG-950.
           PERFORM UPDATE-GL-DEBTOR-ACC.
           MOVE GL-NUMBER TO WS-GLNUMBER.
           MOVE " " TO GL-NUMBER.
           MOVE WS-GLHEADER TO GL-NUMBER.
           PERFORM UGLCA-010 THRU UGLCA-900.
           MOVE " " TO GL-NUMBER.
           MOVE WS-HEAD-SUB TO GL-NUMBER.
           PERFORM UGLCA-010 THRU UGLCA-900.
           PERFORM WRTR-900.
           CLOSE GL-MASTER.
       WDTG-950.
           PERFORM CGP-900.
           MOVE 2910 TO POS.
           DISPLAY "                                         " AT POS.
           CLOSE DISTRIBUTIONS.
       WDTG-999.
           EXIT.
      *
       WRITE-GLTRANS SECTION.
       WRTR-000.
           PERFORM OPEN-075.
       WRTR-010.
           PERFORM READ-PARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO          TO GLTRANS-TRANS.
           ADD 1                        TO GLPA-GLTRANSNO.
           PERFORM REWRITE-PARAMETER.
           MOVE WS-JRN                  TO GLTRANS-REFERENCE.
           MOVE 1                       TO GLTRANS-TYPE.
           IF GLPA-CURRENT-GLPER = GLPA-CURRENT-SLPER
               MOVE " "                 TO GLTRANS-FUTURE
               MOVE GLPA-CURRENT-SLPER  TO GLTRANS-NO
           ELSE
               MOVE "F"                 TO GLTRANS-FUTURE
               MOVE GLPA-CURRENT-SLPER  TO GLTRANS-NO.
           MOVE WS-DATE                 TO GLTRANS-DATE.
           MOVE WS-DIST-AMOUNTS (SUB-1) TO GLTRANS-AMOUNT.
           MOVE WS-DIST-ACCNO (SUB-1)   TO GLTRANS-ACCOUNT-NUMBER.
           MOVE WS-DIST-DESC (SUB-1)    TO LINE-DESC.
           MOVE WS-JRN                  TO LINE-MONTH.
           MOVE LINE-DESCRIPTION        TO GLTRANS-LINE-DESC.
       WRTR-015.
           WRITE GLTRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS - NO FILE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRTR-015.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS FILE BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRTR-015.
       WRTR-900.
           CLOSE GLTRANS-FILE.
       WRTR-999.
           EXIT.
      *
       UPDATE-GLMASTER SECTION.
       UPGL-000.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-GLPER
               GO TO UPGL-999.
           MOVE GLPA-CURRENT-GLPER TO WS-PERIOD.
       UPGL-005.
           MOVE WS-DIST-ACCNO (SUB-1) TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGL-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLMASTER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLMASTER RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGL-010.
           ADD WS-DIST-AMOUNTS (SUB-1) TO GL-BALANCE.
           ADD WS-DIST-AMOUNTS (SUB-1) TO GL-PER (WS-PERIOD).
       UPGL-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLMASTER RECORD BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGL-900.
       UPGL-999.
           EXIT.
     *
       UPDATE-GLHEADER SECTION.
       UPGLH-005.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-GLPER
               GO TO UPGLH-999.
           MOVE GLPA-CURRENT-GLPER TO WS-PERIOD.
           MOVE WS-DIST-ACCNO (SUB-1) TO WS-GLNUMBER.
           MOVE WS-GLHEADER TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLHEADER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLHEADER FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLH-010.
           ADD WS-DIST-AMOUNTS (SUB-1) TO GL-BALANCE.
           ADD WS-DIST-AMOUNTS (SUB-1) TO GL-PER (WS-PERIOD).
       UPGLH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLHEADER RECORD BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLH-900.
       UPGLH-999.
           EXIT.
     *
       UPDATE-GLSUBHEADER SECTION.
       UPGLSH-005.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-GLPER
               GO TO UPGLSH-999.
           MOVE GLPA-CURRENT-GLPER TO WS-PERIOD.
           MOVE WS-DIST-ACCNO (SUB-1) TO WS-GLNUMBER.
           MOVE WS-HEAD-SUB TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLSH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLSUBHEAD FILE DOES'NT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLSUBHEADER FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLSH-010.
           ADD WS-DIST-AMOUNTS (SUB-1) TO GL-BALANCE.
           ADD WS-DIST-AMOUNTS (SUB-1) TO GL-PER (WS-PERIOD).
       UPGLSH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLSUBHEAD FILE BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLSH-900.
       UPGLSH-999.
           EXIT.
      *
       WRITE-DR-ACC-TRANS SECTION.
       WDATR-010.
           PERFORM READ-PARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO          TO GLTRANS-TRANS.
           ADD 1                        TO GLPA-GLTRANSNO.
           PERFORM REWRITE-PARAMETER.
           MOVE WS-JRN                  TO GLTRANS-REFERENCE.
           MOVE 1                       TO GLTRANS-TYPE.
           IF GLPA-CURRENT-GLPER = GLPA-CURRENT-SLPER
               MOVE " "                 TO GLTRANS-FUTURE
               MOVE GLPA-CURRENT-SLPER  TO GLTRANS-NO
           ELSE
               MOVE "F"                 TO GLTRANS-FUTURE
               MOVE GLPA-CURRENT-SLPER  TO GLTRANS-NO.
           MOVE WS-DATE                 TO GLTRANS-DATE.
           MOVE WS-DIST-AMOUNTS (SUB-1) TO GLTRANS-AMOUNT.
           COMPUTE GLTRANS-AMOUNT = GLTRANS-AMOUNT * -1.
           MOVE GLPA-GLDEBT-NO          TO GLTRANS-ACCOUNT-NUMBER.
           MOVE WS-DIST-DESC (SUB-1)    TO LINE-DESC.
           MOVE WS-JRN                  TO LINE-MONTH.
           MOVE LINE-DESCRIPTION        TO GLTRANS-LINE-DESC.
       WDATR-015.
           WRITE GLTRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS-DR, NO FILE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WDATR-015.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS-DR FILE BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WDATR-015.
       WDATR-999.
           EXIT.
      *
       UPDATE-GL-DEBTOR-ACC SECTION.
       UGLCA-000.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-GLPER
               GO TO UGLCA-999.
           MOVE GLPA-CURRENT-GLPER TO WS-PERIOD.
       UGLCA-005.
           PERFORM READ-PARAMETER-LOCK.
           MOVE GLPA-GLDEBT-NO TO GL-NUMBER.
           PERFORM REWRITE-PARAMETER.
           START GL-MASTER KEY NOT < GL-KEY.
       UGLCA-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLDEBT-ACC DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UGLCA-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLDEBT-ACC BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UGLCA-010.
           ADD WS-BATCH-TOTAL TO GL-BALANCE.
           ADD WS-BATCH-TOTAL TO GL-PER (WS-PERIOD).
       UGLCA-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UGLCA-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLDEBT-ACC BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UGLCA-900.
       UGLCA-999.
           EXIT.
      *
       READ-PARAMETER-LOCK SECTION.
       RPL-010.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO GLPARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RPL-010.
       RPL-999.
           EXIT.
      *
       REWRITE-PARAMETER SECTION.
       REWP-000.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "GLPARAMETER RECORD NOT UPDATED!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO REWP-000.
       REWP-999.
           EXIT.
      *
       WRITE-DAILY SECTION.
       WRDA-000.
            OPEN EXTEND DAILY-EXCEPTIONS.
            MOVE WS-DAILY-MESSAGE TO DAILY-EX-REC.
            WRITE DAILY-EX-REC.
            CLOSE DAILY-EXCEPTIONS.
            MOVE " " TO WS-DAILY-MESSAGE.
       WRDA-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DISTRIBUTIONS.
            IF WS-DISTRIBUTION-ST1 NOT = 0
              MOVE "ERROR IN OPENING DIST FILE, GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO OPEN-000.
       OPEN-005.
            OPEN I-O SALES-ANALYSIS.
            IF WS-SALES-ST1 NOT = 0
              MOVE "ERROR IN OPENING SALES FILE, GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO OPEN-005.
       OPEN-010.
            OPEN I-O PARAMETER-FILE.
            IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "ERROR IN OPENING PARAM FILE, GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO OPEN-010.
       OPEN-015.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
              MOVE "ERROR IN OPENING DEBTOR FILE, GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO OPEN-015.
       OPEN-020.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
              MOVE "ERROR IN OPENING STOCK FILE, GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO OPEN-020.
       OPEN-025.
            OPEN I-O DEBTOR-TRANS-FILE.
            IF WS-DRTRANS-ST1 NOT = 0
              MOVE "ERROR IN OPENING DRTRANS FILE, GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO OPEN-025.
       OPEN-030.
            OPEN I-O OUTSTANDING-ORDERS.
            IF WS-OUTORD-ST1 NOT = 0
              MOVE "ERROR IN OPENING ORDERS FILE, GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-000
      *        move WS-OUTORD-ST1 to ws-message
      *        perform error-message
      *        move WS-OUTORD-ST2 to ws-message
      *        perform error-message
              GO TO OPEN-030.
       OPEN-035.
            OPEN I-O SALES-ANALYSIS.
            IF WS-SALES-ST1 NOT = 0
              MOVE "ERROR IN OPENING SALES FILE, GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO OPEN-035.
       OPEN-040.
            OPEN I-O IMPRECEIPTS-FILE.
            IF WS-IMPRECEIPT-ST1 NOT = 0
              MOVE "ERROR IN OPENING IMPORTS FILE, GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO OPEN-040.
       OPEN-045.
            OPEN I-O SOLD-BY.
            IF WS-SOLDBY-ST1 NOT = 0
               MOVE "SOLDBY FILE BUSY ON OPEN I-O, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-SOLDBY-ST1
               CLOSE SOLD-BY
              GO TO OPEN-045.
       OPEN-050.
            OPEN I-O INCR-REGISTER.
            IF WS-INCR-ST1 NOT = 0
              MOVE "ERROR IN OPENING REGISTER FILE, GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO OPEN-050.
       OPEN-051.
            OPEN I-O INCR-LY-REGISTER.
            IF WS-INCR-LY-ST1 NOT = 0
              MOVE "ERROR IN OPENING I-O LY-REG, GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-SLREGLY TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-051.
       OPEN-052.
            OPEN OUTPUT INCR-LY-REGISTER.
            IF WS-INCR-LY-ST1 NOT = 0
              MOVE "ERROR IN OPENING OUTPUT LY-REG , GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-SLREGLY TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-052.
       OPEN-055.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-055.
       OPEN-056.
           OPEN OUTPUT STOCK-TRANSLY-FILE.
           IF WS-STTRANSLY-ST1 NOT = 0
              MOVE "ST-TRANS-LY FILE BUSY OPEN OUTPUT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-056.
       OPEN-057.
           OPEN I-O STOCK-TRANSLY-FILE.
           IF WS-STTRANSLY-ST1 NOT = 0
              MOVE "ST-TRANS-LY FILE BUSY ON OPEN I-O, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-057.
       OPEN-060.
            OPEN I-O STKRECEIPTS-FILE.
            IF WS-STK-ST1 NOT = 0
               MOVE 0 TO WS-STK-ST1
               MOVE "ST-RECEIPTS BUSY ON OPEN, PRESS 'ESC' TO RETRY." 
                  TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-060.
       OPEN-061.
            OPEN OUTPUT STKRECEIPTSLY-FILE.
            IF WS-STKLY-ST1 NOT = 0
               MOVE 0 TO WS-STKLY-ST1
               MOVE "ST-RECEIPTSLY BUSY ON OPEN, PRESS 'ESC' TO RETRY." 
                  TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-061.
       OPEN-065.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-065.
       OPEN-070.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0 
              MOVE 0 TO WS-GLMAST-ST1
              MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-070.
       OPEN-075.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GL-TRANS BUSY ON OPEN, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-075.
       OPEN-999.
            EXIT.
      *
       CHECK-DATA-SIZE SECTION.         
       CDS-005.
           Move " " To Alpha-Rate.
           Move 0   To Sub-1.
       CDS-015.
           Add 1 To Sub-1.
           If Al-Rate (Sub-1) Not = " "
            If Sub-1 Not > 60
            Go To CDS-015.
          Subtract 1 from Sub-1.
       CDS-999.
          EXIT.
      *
       DELETE-REG-LY-FILE SECTION.
       DST-010.
      *    FOR LINUX GO THE BELOW.....
           GO TO DST-999.
         
           PERFORM CDS-005.
           Move Ws-SlRegLY To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE Ws-SlRegLy      TO F-FILENAME
           MOVE SUB-1           TO F-CBFILENAME.
           CALL "OPENFILE" USING   F-ERROR1
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE-MM.
           CALL "DELETEFILE" USING F-ERROR1
                                   F-FH.
       DST-999.
           EXIT.
      *
       DELETE-STRANSLY-FILE SECTION.
       DSTLY-010.
      *    FOR LINUX GO THE BELOW.....
           GO TO DSTLY-999.

           PERFORM CDS-005.
           Move Ws-StTransLY To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE Ws-StTransLy    TO F-FILENAME
           MOVE SUB-1           TO F-CBFILENAME.
           CALL "OPENFILE" USING   F-ERROR1
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE-MM.
           CALL "DELETEFILE" USING F-ERROR1
                                   F-FH.
       DSTLY-999.
            EXIT.
      *
       Copy "ComputeDatePeriod".
       Copy "EnterPeriodDates".
       Copy "GetSystemY2KDate".
       Copy "SlPerEndPassword".
      * Copy "ReadKBD".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
