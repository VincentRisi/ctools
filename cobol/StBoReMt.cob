        IDENTIFICATION DIVISION.
        PROGRAM-ID. StBoReMt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStOrders".
          Copy "SelectStTrans".
          Copy "SelectSlDaily".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdOutOrd.
           COPY ChlfdStTrans.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  WS-STTR-ORDERQTY     PIC S9(6) VALUE 0.
       77  WS-STTR-SHIPQTY      PIC S9(6) VALUE 0.
       77  WS-SUPP-ORDERQTY     PIC S9(6) VALUE 0.
       77  WS-QUANTITY          PIC S9(6) VALUE 0.
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-RANGE3            PIC X VALUE " ".
       77  WS-CAT               PIC X(3) VALUE " ".
       77  WS-VALUE             PIC S9(5)V99 VALUE 0.
       77  WS-TOTAL-VALUE       PIC S9(7)V99 VALUE 0.
       77  WS-RUNNING-TOTAL     PIC S9(7)V99 VALUE 0.
       01  WS-STOCK-STATUS.
           03  WS-ST-ST1        PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1    PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-BO-ST1        PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1     PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST     PIC X(20) VALUE " ".
           03  WS-DAILY-2ND     PIC X(20) VALUE " ".
           03  WS-DAILY-3RD     PIC X(20) VALUE " ".
           03  WS-DAILY-4TH     PIC X(20) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONT-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
           MOVE 315 TO POS.
           DISPLAY
           "** CHECKING OF B/O, RES & SUPPLIER ORDERS ON STOCK FILE **"
               AT POS.
           MOVE 415 TO POS.
           DISPLAY
           "**********************************************************"
               AT POS.
        CONT-010.
           PERFORM GET-DATA.
           PERFORM READ-STOCK-NEXT.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 0 TO WS-RANGE1 WS-RANGE2.
            MOVE 1020 TO POS.
            DISPLAY "   FROM STOCK NUMBER: [               ]" AT POS.
            MOVE 1043 TO POS.
           
           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

            IF W-ESCAPE-KEY = 3
               PERFORM END-OFF.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
            MOVE 1220 TO POS.
            DISPLAY "     TO STOCK NUMBER: [               ]" AT POS.
            MOVE 1243 TO POS.
           
           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

            IF W-ESCAPE-KEY = 4
               GO TO GET-000.
            IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
            MOVE 1420 TO POS.
            DISPLAY "R=RESERVE ADEQUATE STOCK FOR B/O'S," AT POS
            MOVE 1520 TO POS
            DISPLAY
            "W=WRITE ALL RESERVED STOCK NOT ON P/SLIPS TO ON HAND."
               AT POS
            MOVE 1640 TO POS
            DISPLAY "ENTER : [ ]" AT POS
            ADD 9 TO POS
           
           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 48        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE3.

            IF W-ESCAPE-KEY = 4
               GO TO GET-010.
            IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-020
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-020.
            MOVE 1810 TO POS
            DISPLAY "                                         " AT POS
            MOVE 2510 TO POS
            DISPLAY
            "B-Order, Res. & Supp-Order Qty checking in progress....."
              AT POS.
       GET-999.
            EXIT.
      *
       READ-STOCK-NEXT SECTION.
       RSN-005.
            MOVE WS-RANGE1 TO ST-STOCKNUMBER
            START STOCK-MASTER KEY NOT < ST-KEY
                 INVALID KEY NEXT SENTENCE.
            IF WS-ST-ST1 NOT = 0
               MOVE 0 TO WS-ST-ST1
               GO TO RSN-999.
       RSN-010.
            READ STOCK-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE. 
            IF WS-ST-ST1 = 10
               MOVE 0 TO WS-ST-ST1
               GO TO RSN-999.
            IF WS-ST-ST1 NOT = 0
               MOVE 
           "NEXT STOCK FILE BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-ST-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-ST-ST1
               GO TO RSN-010.
            IF ST-STOCKNUMBER < WS-RANGE1
               GO TO RSN-010.
            IF ST-STOCKNUMBER > WS-RANGE2
               GO TO RSN-999.
            MOVE 2210 TO POS
            DISPLAY "STOCKNUMBER BEING PROCESSED IS :" AT POS
            MOVE 2244 TO POS
            DISPLAY ST-STOCKNUMBER AT POS.
       RSN-050.
            MOVE 2610 TO POS
            DISPLAY "READING BACK-ORDER ITEMS...    " AT POS
            PERFORM READ-TRANSACTIONS
            MOVE 2610 TO POS
            DISPLAY "READING SUPPLIER-ORDER ITEMS..." AT POS
            PERFORM READ-SUPPLIER-QTY.
       RSN-055.
            IF WS-SUPP-ORDERQTY NOT = ST-QTYONORDER
               PERFORM WRITE-EXCEPTION-LOG.
            MOVE WS-SUPP-ORDERQTY TO ST-QTYONORDER.

            COMPUTE WS-QUANTITY = ST-QTYONHAND + ST-QTYONRESERVE.
            IF WS-STTR-ORDERQTY = 0
             IF WS-STTR-SHIPQTY = 0
               MOVE 0 TO ST-QTYONBORDER
                         ST-QTYONRESERVE
               MOVE WS-QUANTITY TO ST-QTYONHAND
               GO TO RSN-070.
            MOVE WS-STTR-ORDERQTY TO ST-QTYONBORDER.
            IF WS-RANGE3 = "W"
               GO TO RSN-060.
            IF ST-QTYONBORDER NOT > WS-QUANTITY
               MOVE WS-STTR-ORDERQTY TO ST-QTYONRESERVE
               SUBTRACT WS-STTR-ORDERQTY FROM WS-QUANTITY
               MOVE WS-QUANTITY TO ST-QTYONHAND
               GO TO RSN-070.
            IF ST-QTYONBORDER > WS-QUANTITY
               MOVE WS-QUANTITY TO ST-QTYONRESERVE
               MOVE 0 TO ST-QTYONHAND.
            IF WS-STTR-SHIPQTY > WS-QUANTITY
               PERFORM WRITE-EXCEPTION-LOG.
            IF WS-RANGE3 = "R"
               GO TO RSN-070.
       RSN-060.
            IF WS-STTR-SHIPQTY NOT > WS-QUANTITY
               MOVE WS-STTR-SHIPQTY       TO ST-QTYONRESERVE
               SUBTRACT WS-STTR-SHIPQTY FROM WS-QUANTITY
               MOVE WS-QUANTITY           TO ST-QTYONHAND
            ELSE
               MOVE WS-QUANTITY TO ST-QTYONRESERVE
               MOVE 0           TO ST-QTYONHAND.
       RSN-070.
            REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
            IF WS-ST-ST1 NOT = 0
               MOVE 0 TO WS-ST-ST1
               GO TO RSN-070.
       RSN-090.
            GO TO RSN-010.
       RSN-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       PRR-000.
            MOVE 0 TO WS-STTR-ORDERQTY
                      WS-STTR-SHIPQTY
                      WS-QUANTITY.
            MOVE "N"            TO STTR-ST-COMPLETE.
            MOVE ST-STOCKNUMBER TO STTR-STOCK-NUMBER.
            START STOCK-TRANS-FILE KEY NOT < STTR-ST-KEY
                 INVALID KEY NEXT SENTENCE.
            IF WS-BO-ST1 NOT = 0
               GO TO PRR-999.
       PRR-002.
            READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-BO-ST1 = 10
               GO TO PRR-999.
            IF WS-BO-ST1 NOT = 0
               MOVE 
             "NEXT ST-TRANS BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-BO-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-BO-ST1
               GO TO PRR-002.
            IF STTR-STOCK-NUMBER NOT = ST-STOCKNUMBER
               GO TO PRR-999.
            IF STTR-TYPE NOT = 4 AND NOT = 7
               GO TO PRR-002.
            IF STTR-ST-COMPLETE NOT = "N"
               GO TO PRR-999.
       PRR-005.
            COMPUTE WS-STTR-ORDERQTY = WS-STTR-ORDERQTY +
                (STTR-ORDERQTY - STTR-SHIPPEDQTY).
            ADD STTR-SHIPQTY TO WS-STTR-SHIPQTY.
            GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       READ-SUPPLIER-QTY SECTION.
       RSQ-000.
            MOVE 0 TO WS-SUPP-ORDERQTY.
            MOVE ST-STOCKNUMBER TO OO-STOCK-NUMBER.
            START OUTSTANDING-ORDERS KEY NOT < OO-STOCK-NUMBER
                  INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
               GO TO RSQ-999.
       RSQ-002.
            READ OUTSTANDING-ORDERS NEXT
               AT END NEXT SENTENCE.
            IF WS-OUTORD-ST1 = 10
               GO TO RSQ-999.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE
            "NEXT ST-ORDERS BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-OUTORD-ST1
               GO TO RSQ-002.
            IF OO-STOCK-NUMBER NOT = ST-STOCKNUMBER
               GO TO RSQ-999.
       RSQ-005.
            COMPUTE WS-SUPP-ORDERQTY = WS-SUPP-ORDERQTY + OO-QUANTITY.
            GO TO RSQ-002.
       RSQ-999.
           EXIT.
      *
       WRITE-EXCEPTION-LOG SECTION.
       WRDA-000.
           OPEN EXTEND DAILY-EXCEPTIONS.
           IF WS-DAILY-ST1 = 35 OR = 48
              MOVE "DAILY FILE BUSY ON EXTEND, PRESS 'ESC' TO RETRY"
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DAILY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO WRDA-000.
            GO TO WRDA-008.
       WRDA-005.
           OPEN OUTPUT DAILY-EXCEPTIONS.
           IF WS-DAILY-ST1 NOT = 0 AND NOT = 35
              MOVE WS-DAILY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-DAILY-ST1
              MOVE "DAILY FILE BUSY OPEN OUTPUT, PRESS 'ESC' TO RETRY."
                TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRDA-005.
       WRDA-008.
           MOVE ST-STOCKNUMBER            TO WS-DAILY-1ST.
           MOVE "ERROR IN AMOUNT ON "     TO WS-DAILY-2ND.
           IF WS-STTR-SHIPQTY > WS-QUANTITY
               MOVE "RESERVE, CHECK THE " TO WS-DAILY-3RD
               MOVE "PHYSICAL STOCK QTY " TO WS-DAILY-4TH
           ELSE
               MOVE "ORDER WITH SUPPLIER" TO WS-DAILY-3RD
               MOVE "QTY NOW CHANGED.   " TO WS-DAILY-4TH.
       WRDA-010.
           MOVE WS-DAILY-MESSAGE TO DAILY-EX-REC.
           WRITE DAILY-EX-REC.
           IF WS-DAILY-ST1 NOT = 0
              MOVE "DAILY FILE BUSY ON WRITE, 'ESC' TO RETRY"
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DAILY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DAILY-ST1
               GO TO WRDA-010.
       WRDA-900.
           CLOSE DAILY-EXCEPTIONS.
           IF WS-DAILY-ST1 NOT = 0
              GO TO WRDA-900.
           MOVE 0 TO WS-DAILY-MESSAGE.
       WRDA-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O STOCK-MASTER.
           IF WS-ST-ST1 NOT = 0 
              MOVE 0 TO WS-ST-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'CANCEL' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-005.
           OPEN I-O OUTSTANDING-ORDERS.
           IF WS-OUTORD-ST1 NOT = 0 
              MOVE 0 TO WS-OUTORD-ST1
              MOVE "S-ORDERS FILE BUSY ON OPEN, 'CANCEL' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.
       OPEN-010.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-BO-ST1 NOT = 0 
              MOVE 0 TO WS-BO-ST1
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'CANCEL' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-010.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *      ACCEPT WS-DATE FROM DATE.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER
                  OUTSTANDING-ORDERS
                  STOCK-TRANS-FILE.
            EXIT PROGRAM.
      *      STOP RUN.
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
      * END-OF-JOB.
