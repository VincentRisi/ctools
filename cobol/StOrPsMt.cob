        IDENTIFICATION DIVISION.
        PROGRAM-ID. StOrPsMt.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectSlParameter".
          Copy "SelectStReceipt".
          Copy "SelectStOrders".
          Copy "SelectSlDaily".
          Copy "SelectStOrderGen".
          Copy "SelectCrMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdDaily.
           COPY ChlfdParam.
           COPY ChlfdOutOrd.
           COPY ChlfdStkReceipts.
           COPY ChlfdOrderGen.
           COPY ChlfdCreditor.
      *
       WORKING-STORAGE SECTION.
       77  WS-TYPE               PIC X VALUE " ".
       77  WS-YN                 PIC X VALUE " ".
       77  WS-OLD-ORDER          PIC X VALUE " ".
       77  WS-ABOVE-BODY         PIC X VALUE " ".
       77  WS-SUPPLIER           PIC X(7) VALUE " ".
       77  WS-CREDITOR           PIC 9(7) VALUE 0.
       77  WS-CREDITOR-ACCEPT    PIC X(7) VALUE " ".
       77  WS-ACCEPT             PIC X VALUE " ".
       77  WS-NEXT               PIC X VALUE " ".
       77  WS-SEA-AIR            PIC X VALUE " ".
       77  WS-STOCKNUMBER        PIC X(34) VALUE " ".
       77  WS-ORDERNUMBER        PIC X(20) VALUE " ".
       77  WS-CHANGE-ORDER       PIC X VALUE " ".
       77  WS-DELIVERY           PIC X VALUE " ".
       77  WS-DELVIA             PIC X(20) VALUE " ".
       77  WS-ACCEPT-DATE        PIC X(10) VALUE " ".
       77  WS-DUEDATE            PIC 9(8) VALUE 0.
       77  WS-ANSWER             PIC X VALUE " ".
       77  WS-ORDERNOTFOUND      PIC X VALUE " ".
       77  WS-END                PIC X VALUE " ".
       77  WS-EMAIL-FAX          PIC X VALUE " ".
       77  WS-PORDER-PRN-PROGRAM PIC X(8) VALUE "StOrPrRp".
       77  WS-READS              PIC 99.
       77  WS-DEL-SUB            PIC 9 VALUE 0.
       01  WS-COMMENT-LINE.
           03  WS-COMMENT        PIC X(60) OCCURS 3.
       01  STORE-DEL.
         02  WS-DEL-OCCUR OCCURS 10.
           03  WS-DEL-TYPE       PIC X.
           03  WS-DEL-CODE       PIC X.
           03  WS-DEL-TERM       PIC X(20).
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-STKRECEIPT-STATUS.
           03  WS-STKRECEIPT-ST1  PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1      PIC 99.
       01  WS-GEN-STATUS.
           03  WS-GEN-ST1         PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1     PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST     PIC X(20) VALUE " ".
           03  WS-DAILY-2ND     PIC X(20) VALUE " ".
           03  WS-DAILY-3RD     PIC X(20) VALUE " ".
           03  WS-DAILY-4TH     PIC X(20) VALUE " ".
       01  WS-ORDER-CHECK.
           03  WS-O-C           PIC X OCCURS 20.
       01  WS-ORDER-REFERENCE.
           03  FILLER                 PIC X(3) VALUE "P/O".
           03  WS-ORDERREF            PIC 9(6).
           03  FILLER                 PIC X VALUE ".".
           03  WS-ORDER-MM            PIC 99.
           03  FILLER                 PIC X VALUE ".".
           03  WS-ORDER-YY            PIC 9999.
       01  SLIP-CPI.
           03  SO-CPI          PIC X(4) VALUE " ".
           03  SO-CPI-1        PIC X VALUE " ".
           03  FILLER          PIC X(127) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 315 TO POS.
           DISPLAY "* STOCK ORDER GENERATION POSTING PROGRAM *" AT POS.
           MOVE 415 TO POS.
           DISPLAY "******************************************" AT POS.
       CONTROL-010.
           PERFORM OPEN-FILES.
       CONTROL-015.
           PERFORM GET-DATA.
           PERFORM CONTROL-000.
           GO TO CONTROL-015.
      *
       GET-DATA SECTION.
       GET-005.
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 3001 TO POS
           DISPLAY WS-MESSAGE AT POS
           PERFORM UPDATE-OUT-ORDERS.
           IF WS-EMAIL-FAX = "Y"
               CLOSE STOCK-MASTER
               CLOSE OUTSTANDING-ORDERS
               PERFORM CLEAR-SCREEN
               MOVE WS-ORDERNUMBER TO WS-LINK-PORDER
               MOVE WS-CREDITOR    TO WS-LINK-ACCOUNT
               CALL WS-PORDER-PRN-PROGRAM USING WS-LINKAGE
               PERFORM CLEAR-SCREEN
               CANCEL WS-PORDER-PRN-PROGRAM
               MOVE " " TO WS-LINK-PORDER
               MOVE 0   TO WS-LINK-ACCOUNT
               PERFORM OPEN-000
               PERFORM OPEN-020
               GO TO GET-999.
       GET-999.
            EXIT.
      *
       READ-PARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           START PARAMETER-FILE KEY NOT < PA-KEY.
       RPL-010.
           READ PARAMETER-FILE LOCK          
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO PARAMETER RECORD ON FILE, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ-LOCK, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RPL-010.
       RPL-999.
           EXIT.
      *
       REWRITE-PARAMETER SECTION.
       REWP-000.
           REWRITE PARAMETER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "PARAMETER RECORD NO UPDATED, WS-ST1=2."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY ON REWRITE, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO REWP-000.
       REWP-999.
           EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
             MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
             START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 NOT = 0
                 MOVE " " TO ST-STOCKNUMBER
                             ST-DESCRIPTION2
                 MOVE "NOT THERE!!!" TO ST-DESCRIPTION1
                 MOVE 0 TO ST-PRICE
                           ST-AVERAGECOST
                           ST-DISCOUNT1
                GO TO R-ST-999.
       R-ST-010.
             READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                 MOVE " " TO ST-STOCKNUMBER
                             ST-DESCRIPTION2
                 MOVE "NOT THERE!!!" TO ST-DESCRIPTION1
                 MOVE 0 TO ST-PRICE
                           ST-AVERAGECOST
                           ST-DISCOUNT1
                 GO TO R-ST-999.
             IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-010.
       R-ST-999.
             EXIT.
      *
       READ-STOCK-LOCK SECTION.
       RSL-000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       RSL-010.
           READ STOCK-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE 3010 TO POS
               DISPLAY "NO STOCK FILE TO BE UPDATED" AT POS
               ADD 30 TO POS
               DISPLAY WS-STOCKNUMBER AT POS
               CALL "LOCKKBD" USING F-FIELDNAME
               GO TO RSL-999.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RSL-010.
       RSL-999.
           EXIT.
      *
       UPDATE-STOCK-RECORD SECTION.
       UPST-010.
            MOVE OG-STOCK-NUMBER TO WS-STOCKNUMBER.
            PERFORM READ-STOCK-LOCK.

            ADD OG-QUANTITY TO ST-QTYONORDER.
            MOVE WS-DATE    TO ST-LASTORDERDATE.
            IF WS-TYPE = "F"
                MOVE ST-FOREIGNCOST TO STRE-UNIT-PRICE
                GO TO UPST-900.

            IF ST-SUPPLIERDISC > 0
                MOVE ST-PRICE       TO STRE-UNIT-PRICE
            ELSE
                MOVE ST-LASTCOST    TO STRE-UNIT-PRICE.
       UPST-900.
            REWRITE STOCK-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE 3010 TO POS
                DISPLAY "STOCK RECORD NOT THERE!!" AT POS
                ADD 25 TO POS
                DISPLAY ST-STOCKNUMBER AT POS
                CALL "LOCKKBD" USING F-FIELDNAME
                GO TO UPST-950.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON REWRITE UPST-900, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO UPST-900.
       UPST-950.
            PERFORM READ-PARAMETER-LOCK.
            MOVE PA-STOCK-RECEIPT-NUMBER TO STRE-TRANSACTION-NUMBER
            ADD 1                        TO PA-STOCK-RECEIPT-NUMBER
            PERFORM REWRITE-PARAMETER
            MOVE 3                       TO STRE-TRANSACTION-CODE
            MOVE OG-STOCK-NUMBER         TO STRE-STOCK-NUMBER
            MOVE OG-QUANTITY             TO STRE-QUANTITY.
            COMPUTE STRE-TOTAL-PRICE =
             (STRE-UNIT-PRICE - ((STRE-UNIT-PRICE * OO-DISC) /100))
                     * OG-QUANTITY. 
            MOVE WS-DATE                 TO STRE-REFERENCE-DATE
            MOVE WS-ORDERNUMBER         TO STRE-ORDER-NUMBER.
       UPST-960.
            WRITE STOCK-RECEIPTS-REC
                 INVALID KEY NEXT SENTENCE.
            IF WS-STKRECEIPT-ST1 = 23 OR 35 OR 49
                 GO TO UPST-999.
            IF WS-STKRECEIPT-ST1 NOT = 0
                MOVE "ST-RECEIPT BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STKRECEIPT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STKRECEIPT-ST1
                GO TO UPST-960.
       UPST-999.
            EXIT.
      *
       GET-ORDER-OFF-SYSTEM SECTION.
       GOOS-000.
           MOVE 0810 TO POS.
           DISPLAY "DO YOU WISH TO ADD TO AN OLD ORDER: [ ]" AT POS.
           MOVE 0847 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 05        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-OLD-ORDER.

           IF W-ESCAPE-KEY = 3
               PERFORM END-OFF.
           IF W-ESCAPE-KEY = 4
               MOVE "2" TO WS-ABOVE-BODY
               GO TO GOOS-999.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GOOS-005
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GOOS-000.
       GOOS-005.
           IF WS-OLD-ORDER NOT = "Y" AND NOT = "N"
                 GO TO GOOS-000.
           IF WS-OLD-ORDER = "Y"
                 MOVE " " TO WS-ORDERNUMBER
                 GO TO GOOS-999.
           PERFORM READ-PARAMETER-LOCK.
           MOVE PA-SUPPLY-ORDER-NUMBER TO WS-ORDERREF.
           ADD 1                       TO PA-SUPPLY-ORDER-NUMBER.
           PERFORM REWRITE-PARAMETER.
           PERFORM CHECK-ORDER-FOR-ZEROS.
           MOVE 0810 TO POS.
           DISPLAY
            "THE ORDER NUMBER FOR THIS ORDER IS :[                    ]"
              AT POS.
           MOVE 0847 TO POS.
           DISPLAY WS-ORDERNUMBER AT POS.
       GOOS-010.
           MOVE 0910 TO POS.
           DISPLAY "PRESS <RETURN> TO ACCEPT THIS ORDER." AT POS.
           ADD 50 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 06        TO CDA-ROW.
           MOVE 61        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-YN.

      *      ACCEPT WS-YN AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GOOS-000.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               PERFORM ERROR-020
               GO TO GOOS-999
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GOOS-010.
       GOOS-999.
           EXIT.
      *
       CHECK-ORDER-FOR-ZEROS SECTION.
       OC-001.
           MOVE 1 TO SUB-1.
       OC-005.
           MOVE " " TO AL-RATE (SUB-1).
           IF SUB-1 < 20
               ADD 1 TO SUB-1
               GO TO OC-005.
       OC-010.
           MOVE WS-ORDER-REFERENCE TO WS-ORDER-CHECK.
           MOVE 1 TO SUB-1 SUB-2.
       OC-015.
           IF WS-O-C (SUB-1) = "0"
              ADD 1 TO SUB-1
              GO TO OC-015.
           IF SUB-1 < 4
              MOVE WS-O-C (SUB-1) TO AL-RATE (SUB-2)
              ADD 1 TO SUB-1 SUB-2
              GO TO OC-015.
       OC-020.
           MOVE WS-O-C (SUB-1) TO AL-RATE (SUB-2).
           IF SUB-1 < 20
              ADD 1 TO SUB-1 SUB-2
              GO TO OC-020.
           MOVE ALPHA-RATE TO WS-ORDERNUMBER.
       OC-999.
           EXIT.
      *
       UPDATE-OUT-ORDERS SECTION.
       UPOO-000.
           PERFORM CLEAR-010.
           PERFORM GET-ORDER-OFF-SYSTEM.
           IF WS-ORDERNUMBER NOT = " "
               GO TO UPOO-001.
           MOVE " " TO WS-ORDERNUMBER.
           MOVE 0810 TO POS.
           DISPLAY "ENTER THE ORDER NUMBER: [                    ]"
            AT POS.
           MOVE 0835 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 05        TO CDA-ROW.
           MOVE 34        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ORDERNUMBER.

           IF W-ESCAPE-KEY = 3
               PERFORM END-OFF.
           IF W-ESCAPE-KEY = 4
               MOVE "2" TO WS-ABOVE-BODY
               GO TO UPOO-999.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-001
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-000.
       UPOO-001.
           IF WS-ORDERNUMBER = "           "
               GO TO UPOO-000.
           MOVE WS-ORDERNUMBER TO OO-ORDER-NUMBER.
       UPOO-002.
           MOVE " " TO WS-DELIVERY.
           PERFORM ERROR1-020.
           MOVE 1010 TO POS.
           DISPLAY "ENTER THE DELIVERY METHOD CODE: [ ]" AT POS.
           MOVE 1043 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DELIVERY.

           IF W-ESCAPE-KEY = 4
               GO TO UPOO-000.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-003
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-002.
       UPOO-003.
           IF WS-DELIVERY NOT = "1" AND NOT = "2" AND NOT = "3"
               AND NOT = "4" AND NOT = "5" AND NOT = "6"
               AND NOT = "7" AND NOT = "8" AND NOT = "9" AND NOT = "0"
               GO TO UPOO-002.
           MOVE WS-DELIVERY TO WS-DEL-SUB.
           IF WS-DEL-SUB = 0
               MOVE 1 TO WS-DEL-SUB.
           MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELVIA.
           DISPLAY WS-DELVIA AT POS.
       UPOO-004.
           MOVE " " TO WS-SUPPLIER.
           PERFORM ERROR1-020.
           MOVE 1210 TO POS.
           DISPLAY "ENTER THE SUPPLIER: [       ]" AT POS.
           MOVE 1310 TO POS. 
           DISPLAY "NB! THIS SHOULD BE THE SUPPLIER NAME FOUND ON THE ST"
      -    "OCK-FILE." AT POS.
           MOVE 1231 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SUPPLIER.

           IF W-ESCAPE-KEY = 4
               GO TO UPOO-002.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-006
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-003.
       UPOO-006.
           PERFORM GET-CREDITOR-INFO.
       UPOO-007.
           IF WS-SUPPLIER = " "
               GO TO UPOO-003.
           MOVE 0    TO WS-DUEDATE.
           PERFORM ERROR1-020.
           MOVE " " TO WS-ACCEPT-DATE.
           MOVE 1410 TO POS.
           DISPLAY "ENTER THE DUE DATE: [          ]" AT POS.
           MOVE 1431 TO POS.
           DISPLAY "     " AT POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT-DATE.

           IF W-ESCAPE-KEY = 4
               GO TO UPOO-003.
           IF WS-ACCEPT-DATE = " "
               MOVE 0 TO WS-DUEDATE
               GO TO UPOO-008.
           MOVE WS-ACCEPT-DATE TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO UPOO-007.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           DISPLAY DISPLAY-DATE AT POS.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-DUEDATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO UPOO-007.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-008
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-007.
       UPOO-008.
      ****************************************************************
      *     IF WS-DUEDATE = 0 OR = " "                               *
      *         GO TO UPOO-007.                                      *
      * REMOVED IN THE MAIN PROG CHANGE 23/6/2001 TO ALLOW FOR ZERO  *
      * DUE DATE WHICH = DUE DATE NOT CONFIRMED EVEN IF THE ORDER IS *
      * CONFIRMED AS RECEIVED BY SUPPLIER.  DUE DATE NOW MEANS THIS  *
      * IS A CONFIRMED DUE DATE BY THE SUPPLIER - NOT A GUESSTIMATE  *
      * ANYMORE.                                                     *
      ****************************************************************
           MOVE " " TO WS-MESSAGE WS-TYPE.
           MOVE 2610 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           PERFORM ERROR1-020.
           MOVE 1610 TO POS.
           DISPLAY "IS THIS A LOCAL ORDER OR A FOREIGN ORDER:"
           AT POS.
           MOVE 1710 TO POS.
           DISPLAY "ENTER L=LOCAL ORDER; F=FOREIGN ORDER : [ ]" AT POS.
           MOVE 1750 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TYPE.

           IF W-ESCAPE-KEY = 4
               GO TO UPOO-007.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-008.
       UPOO-030.
           IF WS-TYPE NOT = "L" AND NOT = "F"
               GO TO UPOO-008.
       UPOO-035.
           MOVE " " TO WS-MESSAGE.
           MOVE 2610 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           PERFORM ERROR1-020.
           MOVE 1810 TO POS.
           DISPLAY "WAS THIS SEA OR AIR ON THE ORDER-GEN REPORT: [ ]"
                AT POS.
           ADD 46 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SEA-AIR.

           IF W-ESCAPE-KEY = 4
               GO TO UPOO-008.
           IF WS-SEA-AIR NOT = "S" AND NOT = "A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-035.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-060
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-035.
       UPOO-060.
           MOVE " " TO WS-MESSAGE.
           MOVE 2610 TO POS.
           DISPLAY WS-MESSAGE AT POS.
       UPOO-101.
           MOVE 2910 TO POS.
           MOVE " " TO WS-MESSAGE.
           DISPLAY WS-MESSAGE AT POS.

           DISPLAY "SEND EMAIL OR FAX Y=YES N=NO: [ ]" AT POS.
           MOVE 2941 TO POS.

           MOVE 'N'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 40        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-EMAIL-FAX.
           
           IF W-ESCAPE-KEY = 4
               GO TO UPOO-060.
           IF WS-EMAIL-FAX NOT = "Y" AND NOT = "N"
               GO TO UPOO-101.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-110
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-101.
       UPOO-110.
           PERFORM ERROR1-020.
           MOVE 2910 TO POS.
           DISPLAY "THE ORDER IS BEING PROCESSED, PLEASE WAIT." AT POS.
           PERFORM READ-NEXT-TEMP-FILE.
           IF WS-END = "Y"
               GO TO UPOO-900.
       UPOO-120.
           MOVE OG-STOCK-NUMBER TO OO-STOCK-NUMBER 
                                   WS-STOCKNUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
           READ OUTSTANDING-ORDERS WITH LOCK
               INVALID KEY NEXT SENTENCE.
      *NEXT SENTENCE TO SEE IF AN ORDER ALREADY EXISTS, IF NOT UPOO-125
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-OUTORD-ST1
                GO TO UPOO-125.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "ST-ORDERS BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO UPOO-120.
                
      * ADDED NEXT TWO LINES INCASE QTY=0, DONE WITH LINUX UPGRADE
           IF OG-QUANTITY = 0
                GO TO UPOO-120.
                
           ADD OG-QUANTITY        TO OO-QUANTITY
           MOVE WS-DATE           TO OO-ORDERDATE
           MOVE WS-DUEDATE        TO OO-DUEDATE
           MOVE WS-DELIVERY       TO OO-DELIVERY-METHOD
           MOVE OG-SUPPLIER       TO OO-SUPPLIER-NUMBER
           MOVE ST-SUPPLIERDISC   TO OO-DISC
           MOVE "N"               TO OO-UPDATED.
       UPOO-122.
           REWRITE OUT-ORDER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
               MOVE "ORDER NOT UPDATED UPOO-122, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO UPOO-120.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "SORDERS BUSY ON REWRITE UPOO-122, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO UPOO-122.
           GO TO UPOO-130.
       UPOO-125.
           PERFORM READ-STOCK.
           MOVE WS-ORDERNUMBER    TO OO-ORDER-NUMBER
           MOVE OG-STOCK-NUMBER   TO OO-STOCK-NUMBER
           MOVE OG-QUANTITY       TO OO-QUANTITY
                                     OO-ORIG-QTY
           MOVE WS-DATE           TO OO-ORDERDATE
           MOVE WS-DUEDATE        TO OO-DUEDATE
           MOVE WS-DELIVERY       TO OO-DELIVERY-METHOD
           MOVE OG-SUPPLIER       TO OO-SUPPLIER-NUMBER
           MOVE ST-SUPPLIERDISC   TO OO-DISC
           MOVE "N"               TO OO-UPDATED.
           IF WS-TYPE = "F"
              MOVE ST-FOREIGNCOST TO OO-COST
              MOVE "F"            TO OO-FOR-LOC
           ELSE
              MOVE ST-LASTCOST    TO OO-COST
              MOVE "L"            TO OO-FOR-LOC.
              
           WRITE OUT-ORDER-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
                MOVE "SORDER NOT WRITTEN UPOO-120, 'ESC' TO RETRY."
                TO WS-MESSAGE
                GO TO UPOO-130.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "SORDERS BUSY ON WRITE UPOO-125, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO UPOO-125.
       UPOO-130.
           PERFORM UPDATE-STOCK-RECORD.
           PERFORM DELETE-GEN-RECORD.
           GO TO UPOO-110.
       UPOO-900.
           PERFORM ERROR-020.
           PERFORM ERROR1-020.
           CLOSE ORDER-GEN-FILE.
           PERFORM OPEN-050.
           MOVE " " TO WS-END.
       UPOO-999.
           EXIT.
      *
       GET-CREDITOR-INFO SECTION.
       GET-CRED-004.
           MOVE "N" TO WS-NEXT.
           PERFORM ERROR1-020
           MOVE 2810 TO POS.
           DISPLAY WS-MESSAGE AT POS
           DISPLAY "ENTER THE SUPPLIER NUMBER: [       ]" AT POS.
           MOVE 2910 TO POS.
           DISPLAY "Enter the ACCOUNT NUMBER and <Return>, OR" AT POS.
           MOVE 3015 TO POS.
           DISPLAY
           "Enter a SHORT NAME and <PgDn> to scroll through Acc'S."
             AT POS.
           MOVE 2838 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 25        TO CDA-ROW.
           MOVE 37        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CREDITOR-ACCEPT.

           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-CRED-005.
           IF W-ESCAPE-KEY = 7
                GO TO GET-CRED-006
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-CRED-004.
       GET-CRED-005.
           IF WS-CREDITOR-ACCEPT = 0 OR = " "
               GO TO GET-CRED-004.
           MOVE WS-CREDITOR-ACCEPT TO WS-CREDITOR
           PERFORM READ-CREDITOR.
           IF CR-NAME = "** UNKNOWN **"
             MOVE 
            "RE-ENTER THE NUMBER OF A VALID SUPPLIER, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO GET-CRED-004.
           GO TO GET-CRED-020.
       GET-CRED-006.
           MOVE WS-CREDITOR-ACCEPT TO F-NAMEFIELD.
       GET-CRED-007.
           PERFORM READ-NEXT-CREDITOR.
           IF CR-NAME = "** UNKNOWN **"
           MOVE 
            "RE-ENTER THE NUMBER OF A VALID SUPPLIER, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO GET-CRED-004.
       GET-CRED-020.
           PERFORM ERROR-020
           PERFORM ERROR1-020
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2810 TO POS
           DISPLAY "SUPPLIER: [       ]" AT POS.
           MOVE 2821 TO POS.
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
           MOVE 2832 TO POS.
           DISPLAY CR-NAME AT POS.
           MOVE 2910 TO POS.
           DISPLAY "Press <Return> to ACCEPT This Account,    " AT POS.
           MOVE 3015 TO POS.
           DISPLAY 
           "OR Press <PgDn> To Scroll Through More Accounts       "
            AT POS.
           MOVE 3075 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 25        TO CDA-ROW.
           MOVE 74        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.
 
           IF W-ESCAPE-KEY = 1 OR = 2 OR = 5
               GO TO GET-CRED-900.
           IF W-ESCAPE-KEY = 7
               MOVE "Y" TO WS-NEXT
               MOVE " " TO WS-CREDITOR-ACCEPT
               GO TO GET-CRED-007.
           IF W-ESCAPE-KEY = 6
               MOVE " " TO WS-CREDITOR-ACCEPT
               GO TO GET-CRED-004.
           GO TO GET-CRED-020.
       GET-CRED-900.
            PERFORM ERROR-020
            PERFORM ERROR1-020
            MOVE 2810 TO POS
            DISPLAY WS-MESSAGE AT POS.

           MOVE 2110 TO POS
           DISPLAY "SUPPLIER: [       ]" AT POS.
           MOVE 2121 TO POS.
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
           MOVE 2132 TO POS.
           DISPLAY CR-NAME AT POS.
       GET-CRED-999.
            EXIT.
      *
       READ-CREDITOR SECTION.
       RCR-000.
           MOVE "N"         TO WS-NEXT.
           MOVE WS-CREDITOR TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-KEY
                INVALID KEY NEXT SENTENCE.
       RCR-010.
           READ CREDITOR-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "** UNKNOWN **" TO CR-NAME
                MOVE 88              TO WS-CREDITOR-ST1
                GO TO RCR-999.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITOR BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO RCR-010.
       RCR-999.
             EXIT.
      *
       READ-NEXT-CREDITOR SECTION.
       RNC-000.
           IF WS-NEXT = "Y"
               GO TO RNC-010.
           MOVE F-NAMEFIELD TO CR-NAME
           START CREDITOR-MASTER KEY NOT < CR-ALT-KEY
                  INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE "** UNKNOWN **" TO CR-NAME
                MOVE 88              TO WS-CREDITOR-ST1
                MOVE "N"             TO WS-NEXT
                GO TO RNC-999.
       RNC-010.
           READ CREDITOR-MASTER NEXT
                AT END NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 10
                MOVE "END OF SEARCH, 'ESC' TO RE-ENTER A SHORT NAME."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE "N" TO WS-NEXT
                GO TO RNC-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE "CREDITOR BUSY ON READ-NEXT, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO RNC-010.
           MOVE CR-ACCOUNT-NUMBER TO WS-CREDITOR.
       RNC-999.
           EXIT.
      *
       DELETE-GEN-RECORD SECTION.
       DGR-010.
           DELETE ORDER-GEN-FILE
              INVALID KEY NEXT SENTENCE.
           IF WS-GEN-ST1 NOT = 0
               MOVE "ORDER-GEN BUSY ON DELETE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GEN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GEN-ST1
               GO TO DGR-010.
       DGR-999.
           EXIT.
      *
       START-GEN-RECORD SECTION.
       STR-000.
           MOVE WS-SUPPLIER TO OG-SUPPLIER.
           MOVE WS-SEA-AIR  TO OG-SEA-AIR.
           MOVE " "         TO OG-STOCK-NUMBER.
           START ORDER-GEN-FILE KEY NOT < OG-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-GEN-ST1 NOT = 0
               MOVE "Y" TO WS-END.
       STR-999.
             EXIT.
      *
       READ-NEXT-TEMP-FILE SECTION.
       RNX-001.
           MOVE 0 TO WS-GEN-ST1.
       RNX-005.
           READ ORDER-GEN-FILE NEXT WITH LOCK
             AT END
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-GEN-ST1 = 23 OR 35 OR 49
               MOVE "ORDER-GEN BUSY ON READ-NEXT-23, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GEN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GEN-ST1
               GO TO RNX-005.
           IF WS-GEN-ST1 NOT = 0
               MOVE "ORDER-GEN BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GEN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GEN-ST1
               GO TO RNX-005.
           IF OG-SUPPLIER NOT = WS-SUPPLIER
               GO TO RNX-005.
           IF OG-SEA-AIR NOT = WS-SEA-AIR
               GO TO RNX-005.
           MOVE 2615 TO POS.
           DISPLAY "STOCK-NUMBER READ:" AT POS.
           ADD 20 TO POS.
           DISPLAY ST-STOCKNUMBER AT POS.
       RNX-999.
           EXIT.
      *
       CHECK-TO-CHANGE-ORDER SECTION.
       CTCO-005.
            PERFORM ERROR1-020.
       CTCO-006.
            PERFORM ERROR-020.
       CTCO-010.
            MOVE 3010 TO POS
            DISPLAY "THIS IS THE ORDER-NUMBER:[              ]"
                 AT POS.
            ADD 26 TO POS.
            DISPLAY WS-ORDERNUMBER AT POS.
            MOVE 2910 TO POS.
            DISPLAY "DO YOU WISH TO CHANGE THE ORDER-NUMBER ENTERED.[ ]"
                 AT POS.
            ADD 48 TO POS.

            MOVE ' '       TO CDA-DATA.
            MOVE 1         TO CDA-DATALEN.
            MOVE 26        TO CDA-ROW.
            MOVE 57        TO CDA-COL.
            MOVE CDA-GREEN TO CDA-COLOR.
            MOVE 'F'       TO CDA-ATTR.
            PERFORM CTOS-ACCEPT.
            MOVE CDA-DATA TO WS-CHANGE-ORDER.

            IF WS-CHANGE-ORDER NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CTCO-010.
            IF WS-CHANGE-ORDER = "N"
               GO TO CTCO-999.
       CTCO-015.
            PERFORM CTCO-005.
            MOVE " " TO WS-ORDERNUMBER.
            MOVE 3010 TO POS
            DISPLAY " ENTER NEW ORDER NUMBER :[               ]"
                 AT POS.
            ADD 26 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 35        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SEA-AIR.

            ACCEPT WS-ORDERNUMBER AT POS.
            IF WS-ORDERNUMBER = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CTCO-015.
            PERFORM CTCO-005 THRU CTCO-006.
       CTCO-999.
            EXIT.
      *
       READ-DELIVERY-FILE SECTION.
       RDELIV-000.
            OPEN I-O PARAMETER-FILE.
            MOVE 1 TO SUB-1
                      PA-RECORD.
            MOVE 3 TO PA-TYPE.
       RDELIV-010.
            READ PARAMETER-FILE NEXT
                AT END NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 10
                GO TO RDELIV-999.
            IF PA-TYPE < 3
               GO TO RDELIV-010.
            IF PA-TYPE > 3
                GO TO RDELIV-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY ON DEL-READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RDELIV-010.
            IF PARAMETER-REC = "           "
               GO TO RDELIV-010.           
            MOVE PARAMETER-REC TO WS-DEL-OCCUR (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 = 11
               PERFORM ERROR-020
               MOVE 1 TO SUB-1
               GO TO RDELIV-999.
            GO TO RDELIV-010.
       RDELIV-999.
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
       OPEN-010.
            OPEN I-O PARAMETER-FILE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER BUSY ON OPEN, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-010.
            MOVE ALL "X" TO STORE-DEL.
            PERFORM READ-DELIVERY-FILE.
       OPEN-020.
            OPEN I-O OUTSTANDING-ORDERS.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO WS-OUTORD-ST1
               MOVE "SORDERS BUSY ON OPEN, PRESS 'ESC' TO RETRY."
                   TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-020.
       OPEN-040.
            OPEN I-O STKRECEIPTS-FILE.
            IF WS-STKRECEIPT-ST1 NOT = 0
               MOVE 0 TO WS-STKRECEIPT-ST1
               MOVE "ST-RECEIPTS BUSY ON OPEN, PRESS 'ESC' TO RETRY." 
                  TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-040.
       OPEN-045.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-045.
        OPEN-050.
            OPEN I-O ORDER-GEN-FILE.
            IF WS-GEN-ST1 NOT = 0
               MOVE 0 TO WS-GEN-ST1
               MOVE "ORDER-GEN-FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-050.
        OPEN-060.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-MM TO WS-ORDER-MM
           MOVE WS-YY TO WS-ORDER-YY.
        OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER
                 PARAMETER-FILE
                 OUTSTANDING-ORDERS
                 STKRECEIPTS-FILE
                 CREDITOR-MASTER
                 ORDER-GEN-FILE.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *      
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
       Copy "WriteDailyExcep1".

      * END-OF-JOB
