        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlOrChMt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
         Copy "SelectStTrans".
         Copy "SelectStTrans2".
         Copy "SelectSlRegister".
         Copy "SelectSlDaily".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStTrans.
           COPY ChlfdStTrans2.
           COPY ChlfdDaily.
           COPY ChlfdRegister.
      *
       WORKING-STORAGE SECTION.
       77  WS-STTR-ORDERQTY     PIC S9(6) VALUE 0.
       77  WS-STTR-SHIPQTY      PIC S9(6) VALUE 0.
       77  WS-ORDER-QTY         PIC S9(5) VALUE 0.
       77  WS-SHIP-QTY          PIC S9(5) VALUE 0.
       77  WS-SHIPPED-QTY       PIC S9(5) VALUE 0.
       77  WS-SHIPQTY           PIC S9(6) VALUE 0.
       77  WS-PREVIOUS-ALLOC    PIC S9(6) VALUE 0.
       77  WS-NEW-SHIPQTY       PIC S9(6) VALUE 0.
       77  WS-QUANTITY          PIC S9(6) VALUE 0.
       77  WS-PENDING           PIC S9(6) VALUE 0.
       77  WS-BM-PENDING        PIC S9(6) VALUE 0.
       77  WS-READY             PIC S9(6) VALUE 0.
       77  WS-NEWPRICE          PIC S9(6)V99 VALUE 0.
       77  WS-PERC              PIC S9(4)V99 VALUE 0.
       77  WS-CLOSE-CNT         PIC 9(6) VALUE 0.
       77  WS-PENDING-DISPLAY   PIC Z(5)9.
       77  WS-READY-DISPLAY     PIC Z(5)9.
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-ALLOC-TO-SUSP     PIC X VALUE " ".
       77  WS-ALLOC-TO-BM       PIC X VALUE " ".
       77  B-SPECIALTO-BM       PIC X VALUE " ".
       77  WS-ALLOCATE          PIC X VALUE " ".
       77  WS-CAT               PIC X(3) VALUE " ".
       77  WS-PRICE             PIC S9(7)V99 VALUE 0.
       77  WS-STOCK-AVE         PIC S9(7)V99 VALUE 0.
       77  WS-ST-AVERAGECOST    PIC S9(7)V99 VALUE 0.
       77  WS-AVE-ALLOC              PIC S9(7)V99 VALUE 0.
       77  WS-SHIPQTY-ALLOC-NO-CHNG  PIC S9(7) VALUE 0.
       77  WS-SHIPQTY-ALLOC          PIC S9(7) VALUE 0.
       77  WS-VALUE             PIC S9(7)V99 VALUE 0.
       77  WS-COST-TOTAL        PIC S9(7)V99 VALUE 0.
       77  WS-SALE-TOTAL        PIC S9(7)V99 VALUE 0.
       77  WS-COST-DISPLAY      PIC Z(6)9.99.
       77  WS-SALE-DISPLAY      PIC Z(6)9.99.
       01  WS-STOCK-STATUS.
           03  WS-ST-ST1        PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-BO-ST1        PIC 99.
       01  WS-STTRANS2-STATUS.
           03  WS-BO2-ST1       PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1     PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  WS-UNALLOCATEDS.
         02  WS-UNALLOCATED-ITEMS OCCURS 200.
           03  WS-TYPE        PIC 99.
           03  WS-REF         PIC 9(6).
           03  WS-TRANS       PIC 9(6).
           03  WS-SUSPEND     PIC X.
           03  WS-BRANCH      PIC X.
           03  B-SPECIAL      PIC X.
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
           DISPLAY
           "** AUTO ALLOCATION OF RESERVED STOCK TO P-SLIPS **" AT POS
           MOVE 415 TO POS
           DISPLAY
           "**************************************************" AT POS.
        CONT-010.
           PERFORM OPEN-FILES
           PERFORM GET-DATA.
           PERFORM READ-STOCK-NEXT.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO WS-RANGE1 WS-RANGE2.
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
               GO TO GET-020
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-020.
            MOVE 1420 TO POS
            DISPLAY "ALLOCATE TO SUSPENDED ORDERS :      [ ]" AT POS
            MOVE 1520 TO POS
            DISPLAY "ENTER: 'A' to Allocate, but NO print," AT POS
            MOVE 1620 TO POS
            DISPLAY "       'P' to Allocate AND print," AT POS
            MOVE 1720 TO POS
            DISPLAY "       'N' NO Allocation of SUSPENDED ORDERS."
            AT POS.
            MOVE 1457 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ALLOC-TO-SUSP.

            IF W-ESCAPE-KEY = 4
               GO TO GET-010.
            IF WS-ALLOC-TO-SUSP NOT = "A" AND NOT = "N" AND NOT = "P"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-030.
            MOVE "N" TO WS-ALLOC-TO-BM.
            MOVE 1920 TO POS
            DISPLAY "ALLOCATE TO BILLS OF MATERIAL:      [ ]" AT POS
            MOVE 1957 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ALLOC-TO-BM.

            IF W-ESCAPE-KEY = 4
               GO TO GET-010.
            IF WS-ALLOC-TO-BM NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-040
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-040.
            MOVE 1810 TO POS
            DISPLAY "                                         " AT POS
            MOVE 2710 TO POS
            DISPLAY
            "Allocation of Stock to Picking-Slips in progress....."
              AT POS.
       GET-999.
            EXIT.
      *
       READ-STOCK-NEXT SECTION.
       RSN-005.
            MOVE 0 TO WS-VALUE
                      WS-COST-TOTAL
                      WS-SALE-TOTAL.
            MOVE WS-RANGE1 TO ST-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY
                 INVALID KEY NEXT SENTENCE.
            IF WS-ST-ST1 NOT = 0
               MOVE "STOCK MASTER BUSY ON START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               GO TO RSN-010.
            IF ST-STOCKNUMBER < WS-RANGE1
            MOVE "STOCK FILE < RANGE1, IN 2 SEC GOING TO READ-NEXT."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               GO TO RSN-010.
            IF ST-STOCKNUMBER > WS-RANGE2
               GO TO RSN-999.
            MOVE 1810 TO POS
            DISPLAY "STOCKNUMBER BEING PROCESSED IS :" AT POS
            MOVE 1844 TO POS
            DISPLAY ST-STOCKNUMBER AT POS.
            IF ST-QTYONBORDER = 0
                 GO TO RSN-010.
       RSN-050.
            MOVE 2610 TO POS
            DISPLAY "READING BACK-ORDER ITEMS...               " AT POS
            COMPUTE WS-QUANTITY = ST-QTYONHAND + ST-QTYONRESERVE
            PERFORM CHECK-PREVIOUS-ALLOC-TOTAL.
            MOVE 2610 TO POS
            DISPLAY "AFTER CHECK-PREVIOUS.........             " AT POS
            
      *NEW SECTION - SEE CPAT-005.
            COMPUTE WS-QUANTITY =
                 WS-QUANTITY - WS-STTR-SHIPQTY
            COMPUTE WS-STOCK-AVE = WS-QUANTITY * ST-AVERAGECOST.
                 
            ADD WS-AVE-ALLOC TO WS-STOCK-AVE 
            COMPUTE WS-QUANTITY = ST-QTYONHAND + ST-QTYONRESERVE
            COMPUTE WS-ST-AVERAGECOST ROUNDED =
               WS-STOCK-AVE / (WS-QUANTITY - WS-SHIPQTY-ALLOC-NO-CHNG).
            IF WS-ST-AVERAGECOST > 0
                MOVE WS-ST-AVERAGECOST TO ST-AVERAGECOST.
            
                 
      * THIS SECTION ADDED TO FIND PREVIOUSLY ALLOCATED ST-TRANS
      * THAT ARE FLAGGED AS 'B' BUT HAVE THE SL-REGISTER RECORD DELETED
      * AND ARE THEREFORE "MISSING" IN THE SYSTEM..
      * SEE ALSO SECTION CPAT-005
      *      MOVE STTR-REFERENCE1 TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE
      *      MOVE WS-STTR-SHIPQTY TO WS-MESSAGE
      *      PERFORM ERROR1-000
      *      MOVE WS-QUANTITY TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE
      *      PERFORM ERROR1-020.
      
            SUBTRACT WS-STTR-SHIPQTY FROM WS-QUANTITY.
      
            IF WS-ALLOCATE = "Y"
               PERFORM READ-TRANSACTIONS
            ELSE
               GO TO RSN-010.
            MOVE 2610 TO POS
            DISPLAY "                                         " AT POS.
       RSN-055.
            COMPUTE WS-QUANTITY = ST-QTYONHAND + ST-QTYONRESERVE.
            IF WS-STTR-ORDERQTY = 0
               MOVE 0 TO ST-QTYONBORDER
                         ST-QTYONRESERVE
               MOVE WS-QUANTITY      TO ST-QTYONHAND
               GO TO RSN-070.
            MOVE WS-STTR-ORDERQTY    TO ST-QTYONBORDER.
            IF ST-QTYONBORDER NOT >     WS-QUANTITY
               MOVE WS-STTR-ORDERQTY TO ST-QTYONRESERVE
               SUBTRACT WS-STTR-ORDERQTY FROM WS-QUANTITY
               MOVE WS-QUANTITY      TO ST-QTYONHAND
               GO TO RSN-070.
            IF ST-QTYONBORDER > WS-QUANTITY
               MOVE WS-QUANTITY      TO ST-QTYONRESERVE
               MOVE 0                TO ST-QTYONHAND.
      
            IF WS-STTR-SHIPQTY > WS-QUANTITY
               PERFORM WRITE-DAILY.
       RSN-070.
            IF ST-AVERAGECOST = 0
            MOVE ST-LASTCOST TO ST-AVERAGECOST.
            
            REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
            IF WS-ST-ST1 NOT = 0
               MOVE
           "NEXT STOCK FILE BUSY ON RE-WRITE, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-ST-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-ST-ST1
               GO TO RSN-070.
       RSN-090.
            GO TO RSN-010.
       RSN-999.
            EXIT.
      *
       CHECK-PREVIOUS-ALLOC-TOTAL SECTION.
       CPAT-000.
            MOVE 2610 TO POS
            DISPLAY "CHECKING PREVIOUS QTY'S ......            " AT POS
      *      PERFORM OPEN-010.
            MOVE 0 TO WS-STTR-ORDERQTY
                      WS-STTR-SHIPQTY
                      WS-NEW-SHIPQTY
                      WS-AVE-ALLOC
                      WS-SHIPQTY-ALLOC
                      WS-SHIPQTY-ALLOC-NO-CHNG.
                      
            PERFORM CLEAR-UNALLOCATED-NUMBERS.
            MOVE 2610 TO POS
            DISPLAY "AFTER CLEARING UN-ALLOC.....              " AT POS
            
            MOVE " "            TO STTR2-ST-COMPLETE
            MOVE ST-STOCKNUMBER TO STTR2-STOCK-NUMBER
            START STOCK2-TRANS-FILE KEY NOT < STTR2-STOCK-NUMBER
                 INVALID KEY NEXT SENTENCE.
            IF WS-BO2-ST1 NOT = 0
               GO TO CPAT-900.
       CPAT-002.
            READ STOCK2-TRANS-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-BO2-ST1 = 10 OR = 23
               MOVE 0 TO WS-BO2-ST1
               GO TO CPAT-900.
               
            IF WS-BO2-ST1 NOT = 0 
            MOVE
            "NEXT ST-TRANS2 BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-BO2-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-BO-ST1
               MOVE 0 TO WS-BO2-ST1
               GO TO CPAT-002.
               
            IF STTR2-STOCK-NUMBER NOT = ST-STOCKNUMBER
               GO TO CPAT-900.
            IF STTR2-TYPE NOT = 4 AND NOT = 7
      *         MOVE "GOING TO CPAT-002, TYPE NOT 4 OR 7" TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               GO TO CPAT-002.
            IF STTR2-ST-COMPLETE = "L" OR = "Y" OR = "R"
      *         MOVE "GOING TO CPAT-002, COMP = L, Y OR R" TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               GO TO CPAT-002.
       CPAT-005.
      ******************************************************************
      *NEW SECTION TO FIND THE VALUE OF THE PREVIOUSLY ALLOCATED STOCK *
      *THIS WILL BE ADDED UP FOR ALL STOCK ON P/SLIPS AND THEN ADDED   *
      * AND RE-AVERAGED ONTO THE ST-AVERAGECOST FIELD.  THEREFORE ALL  *
      *STOCK THAT IS ALLOCATED TO P/SLIPS WILL BE A NEW AVERAGE BASED  *
      *ON STOCK ON HAND PLUS PREVIOUSLY ALLOCATED STOCK ON P/SLIPS     *
      *THIS DONE AS IN SLORDERS.INT TO FIX DIFFERENCES IN THE SL19 &   *
      *GL GROSS PROFIT FIGURES.                                        *
      ******************************************************************
           IF STTR2-ORDERQTY = " "
              MOVE 0 TO STTR2-ORDERQTY.
           IF STTR2-SHIPQTY = " "
              MOVE 0 TO STTR2-SHIPQTY.
           IF STTR2-SHIPPEDQTY = " "
              MOVE 0 TO STTR2-SHIPPEDQTY.
              
           IF STTR2-SHIPQTY NOT = STTR2-ORDERQTY - STTR2-SHIPPEDQTY
             COMPUTE WS-AVE-ALLOC = WS-AVE-ALLOC + 
                 (STTR2-SHIPQTY * STTR2-COST-VALUE)
             ADD STTR2-SHIPQTY TO WS-SHIPQTY-ALLOC
           ELSE
             ADD STTR2-SHIPQTY TO WS-SHIPQTY-ALLOC-NO-CHNG.
                 
           COMPUTE WS-STTR-ORDERQTY =
                WS-STTR-ORDERQTY + (STTR2-ORDERQTY - STTR2-SHIPPEDQTY).
           COMPUTE WS-STTR-SHIPQTY =
                WS-STTR-SHIPQTY + STTR2-SHIPQTY.
                 
      * THIS SECTION ADDED TO FIND PREVIOUSLY ALLOCATED ST-TRANS
      * THAT ARE FLAGGED AS 'B' BUT HAVE THE SL-REGISTER RECORD DELETED
      * AND ARE THEREFORE "MISSING" IN THE SYSTEM..
      * SEE ALSO SECTION RSN-050
      *      MOVE "cpat-005" TO WS-MESSAGE
      *      PERFORM ERROR1-000
      *      MOVE STTR2-REFERENCE1 TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE
      *      MOVE WS-SHIPQTY-ALLOC TO WS-MESSAGE
      *      PERFORM ERROR1-000
      *      MOVE WS-SHIPQTY-ALLOC-NO-CHNG TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE
      *      PERFORM ERROR1-020.
      
           IF STTR2-SHIPQTY NOT = STTR2-ORDERQTY - STTR2-SHIPPEDQTY
              PERFORM ENTER-UNALLOCATED-NUMBERS.

           GO TO CPAT-002.
       CPAT-900.
           ADD 1 TO WS-CLOSE-CNT
           MOVE 2520 TO POS
           DISPLAY "LOCK COUNT:" AT POS
           ADD 12 TO POS
           DISPLAY WS-CLOSE-CNT AT POS.
      *     CALL "C$SLEEP" USING 1
      *     CLOSE STOCK-TRANS-FILE.
       CPAT-999.
           EXIT.
      *
       CLEAR-UNALLOCATED-NUMBERS SECTION.
       CUN-005.
            MOVE 2610 TO POS
            DISPLAY "CLEARING UN-ALLOC NUMBERS.....          " AT POS
           MOVE 0 TO SUB-1.
       CUN-010.
           ADD 1 TO SUB-1.
           IF WS-TYPE (SUB-1) = 0
              GO TO CUN-900.
           MOVE 0   TO WS-TYPE (SUB-1)
                       WS-REF (SUB-1)
                       WS-TRANS (SUB-1)
           MOVE " " TO WS-BRANCH (SUB-1)
                        B-SPECIAL (SUB-1).
           IF SUB-1 < 200
               GO TO CUN-010.
       CUN-900.
           MOVE "N" TO WS-ALLOCATE.
           MOVE 1 TO SUB-1.
       CUN-999.
           EXIT.
      *
       ENTER-UNALLOCATED-NUMBERS SECTION.
       EON-005.
           PERFORM READ-REGISTER.

           MOVE 2610 TO POS
           DISPLAY "ENTER UNALLOC NUMBERS ...........          " AT POS
          
           IF WS-ALLOC-TO-SUSP = "A" OR = "P"
            IF INCR-PRINTED = "S"
              GO TO EON-010.
           IF WS-ALLOC-TO-SUSP = "N"
            IF INCR-PRINTED = "S"
              GO TO EON-999.
       EON-010.
           IF WS-TYPE (SUB-1) NOT = 0
              ADD 1 TO SUB-1
              GO TO EON-010.
           MOVE STTR2-TYPE               TO WS-TYPE (SUB-1)
           MOVE STTR2-REFERENCE1         TO WS-REF (SUB-1)
           MOVE STTR2-TRANSACTION-NUMBER TO WS-TRANS (SUB-1).
           IF INCR-PRINTED = "S"
               MOVE WS-ALLOC-TO-SUSP    TO WS-SUSPEND (SUB-1).
           IF INCR-SALES = 53
              MOVE "B"                  TO WS-BRANCH (SUB-1).
           IF INCR-SALES = 57
              MOVE "A"                  TO WS-BRANCH (SUB-1).
           IF WS-ALLOCATE NOT = "Y"
               MOVE "Y"                 TO WS-ALLOCATE.
           ADD 1 TO SUB-1.
       EON-999.
           EXIT.
      *
       READ-TRANSACTIONS SECTION.
       PRR-000.
      *     PERFORM OPEN-010.
           MOVE 0 TO WS-NEW-SHIPQTY
                     SUB-1.
       PRR-001.
           ADD 1 TO SUB-1.
           IF WS-TYPE (SUB-1) = 0
               GO TO PRR-900.
           IF WS-ALLOC-TO-BM = "N"
            IF WS-TYPE (SUB-1) = 7
               GO TO PRR-001.
           MOVE WS-TYPE (SUB-1)  TO STTR-TYPE
           MOVE WS-REF (SUB-1)   TO STTR-REFERENCE1
           MOVE WS-TRANS (SUB-1) TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-BO-ST1 NOT = 0
              GO TO PRR-900.
       PRR-002.
            READ STOCK-TRANS-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-BO-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-BO-ST1
               GO TO PRR-010.
            IF WS-BO-ST1 NOT = 0
            MOVE
             "NEXT ST-TRANS BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-BO-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-BO-ST1
               GO TO PRR-002.
       PRR-005.
            IF WS-QUANTITY = 0
               GO TO PRR-900.
            MOVE STTR-SHIPQTY TO WS-SHIPQTY.
            ADD WS-SHIPQTY    TO WS-QUANTITY.
            COMPUTE WS-NEW-SHIPQTY =  STTR-ORDERQTY - STTR-SHIPPEDQTY.
            IF WS-NEW-SHIPQTY NOT >     WS-QUANTITY
               MOVE WS-NEW-SHIPQTY TO STTR-SHIPQTY
               SUBTRACT WS-NEW-SHIPQTY FROM WS-QUANTITY
            ELSE
               MOVE WS-QUANTITY    TO   WS-NEW-SHIPQTY
               MOVE WS-NEW-SHIPQTY TO STTR-SHIPQTY
               MOVE 0              TO   WS-QUANTITY.
               
      *OLD VERSION
      *     IF STTR-PRICE NOT = 0
      *      IF STTR-COST-VALUE NOT = 0
      *        MOVE ST-AVERAGECOST TO STTR-COST-VALUE.
      *NEW VERSION
           IF STTR-PRICE NOT = 0
              MOVE ST-AVERAGECOST TO STTR-COST-VALUE
              GO TO PRR-006.
            IF STTR-COST-VALUE NOT = 0
              MOVE ST-AVERAGECOST TO STTR-COST-VALUE.
       PRR-006.
      *A=ASSOCIATE
      *B=BRANCH
           IF WS-BRANCH (SUB-1) = "A"
              PERFORM COMPUTE-ASSOCIATE-PRICES.
           IF WS-BRANCH (SUB-1) = "B"
              PERFORM COMPUTE-SPECIAL-PRICES.

           IF WS-SHIPQTY = STTR-SHIPQTY
              GO TO PRR-010.
           ADD STTR-SHIPQTY TO WS-STTR-SHIPQTY
           COMPUTE WS-VALUE = WS-NEW-SHIPQTY * STTR-COST-VALUE
           ADD WS-VALUE TO WS-COST-TOTAL
           MOVE WS-COST-TOTAL TO WS-COST-DISPLAY.

           COMPUTE WS-PRICE = STTR-PRICE - (STTR-PRICE *
                  (STTR-ITEMDISC / 100))
              COMPUTE WS-VALUE = (WS-NEW-SHIPQTY * WS-PRICE)
              ADD WS-VALUE TO WS-SALE-TOTAL
              MOVE WS-SALE-TOTAL TO WS-SALE-DISPLAY.

           COMPUTE WS-VALUE = WS-SALE-TOTAL - WS-COST-TOTAL.

           MOVE 2010 TO POS
           DISPLAY "Cost Value of Allocated Stock  : R" AT POS
           ADD 34 TO POS
           DISPLAY WS-COST-DISPLAY AT POS.

           MOVE 2110 TO POS
           DISPLAY "Sales Value of Allocated Stock : R" AT POS
           ADD 34 TO POS
           DISPLAY WS-SALE-DISPLAY AT POS.

           MOVE WS-VALUE TO WS-SALE-DISPLAY
           MOVE 2210 TO POS
           DISPLAY "Profit Value of Allocated Stock: R" AT POS
           ADD 34 TO POS
           DISPLAY WS-SALE-DISPLAY AT POS.
       PRR-008.
           IF STTR-TYPE = 4 OR = 7
              PERFORM UPDATE-REGISTER.
           PERFORM PRR-500 THRU PRR-550.
       PRR-010.
           ADD 1 TO SUB-1.
           MOVE 0 TO WS-NEW-SHIPQTY.
           IF WS-QUANTITY = 0
               GO TO PRR-900.
           IF WS-TYPE (SUB-1) = 0
               GO TO PRR-900.
       PRR-015.
           IF WS-ALLOC-TO-BM = "N"
            IF WS-TYPE (SUB-1) = 7
               GO TO PRR-010.
           MOVE WS-TYPE (SUB-1)  TO STTR-TYPE
           MOVE WS-REF (SUB-1)   TO STTR-REFERENCE1
           MOVE WS-TRANS (SUB-1) TO STTR-TRANSACTION-NUMBER.
       PRR-020.
           GO TO PRR-002.
       PRR-500.
           IF WS-SUSPEND (SUB-1) = "A"
                 GO TO PRR-550.
           IF INCR-PRINTED = "C"
                MOVE "C" TO STTR-COMPLETE
                            STTR-AC-COMPLETE
                            STTR-ST-COMPLETE.
           IF INCR-PRINTED = "D"
                MOVE "D" TO STTR-COMPLETE
                            STTR-AC-COMPLETE
                            STTR-ST-COMPLETE.
           IF STTR-TYPE = 7
                MOVE "B" TO STTR-COMPLETE
                            STTR-AC-COMPLETE
                            STTR-ST-COMPLETE.
       PRR-550.
           IF B-SPECIAL (SUB-1) = "Y"
               MOVE 0 TO STTR-ITEMDISC.
               
           COMPUTE STTR-SALES-VALUE = (STTR-PRICE -
               (STTR-PRICE * STTR-ITEMDISC / 100)) * STTR-SHIPQTY.
           REWRITE STOCK-TRANS-REC
                INVALID KEY NEXT SENTENCE.
           IF WS-BO-ST1 NOT = 0
               MOVE
           "ST-TRANS FILE BUSY ON REWRITE, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-BO-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-BO-ST1
               GO TO PRR-500.
       PRR-900.
           ADD 1 TO WS-CLOSE-CNT
           MOVE 2520 TO POS
           DISPLAY "LOCK COUNT:" AT POS
           ADD 12 TO POS
           DISPLAY WS-CLOSE-CNT AT POS.

      *     CALL "C$SLEEP" USING 1
      *     CLOSE STOCK-TRANS-FILE.
       PRR-999.
           EXIT.
      *
       Copy "ComputeSpecialPricesAA".
       Copy "ComputeAssociatePricesAA".
      *
       UPDATE-REGISTER SECTION.
       UIR-000.
           MOVE STTR-REFERENCE1 TO INCR-INVOICE.
           MOVE STTR-TYPE       TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               GO TO UIR-999.
       UIR-005.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               GO TO UIR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE 
             "NEXT REGISTER BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-INCR-ST1
               GO TO UIR-005.
               
           IF INCR-PRINTED = "N" OR = "C"
            IF INCR-TRANS NOT = 7
              MOVE "C"       TO INCR-PRINTED
               ADD 1         TO WS-READY
               MOVE WS-READY TO WS-READY-DISPLAY
               MOVE 2310 TO POS
               DISPLAY "Total No Of Orders Made Ready To Print:" AT POS
               ADD 40 TO POS
               DISPLAY WS-READY-DISPLAY AT POS.
           IF INCR-PRINTED = "S"
            IF WS-ALLOC-TO-SUSP = "P"
             IF INCR-TRANS NOT = 7
               MOVE "C"      TO INCR-PRINTED
               ADD 1         TO WS-READY
               MOVE WS-READY TO WS-READY-DISPLAY
               MOVE 2310 TO POS
               DISPLAY "Total No Of Orders Made Ready To Print:" AT POS
               ADD 40 TO POS
               DISPLAY WS-READY-DISPLAY AT POS.
           IF INCR-PRINTED = "P" OR = "D"
            IF INCR-TRANS NOT = 7
               MOVE "D"        TO INCR-PRINTED 
               ADD 1           TO WS-PENDING
               MOVE WS-PENDING TO WS-PENDING-DISPLAY
               MOVE 2410 TO POS
               DISPLAY "Total No Of Orders Already in Store   :" AT POS
               ADD 40 TO POS
               DISPLAY WS-PENDING-DISPLAY AT POS.
               
           IF INCR-TRANS = 7
               MOVE "B"           TO INCR-PRINTED 
               ADD 1              TO WS-BM-PENDING
               MOVE WS-BM-PENDING TO WS-PENDING-DISPLAY
               MOVE 2510 TO POS
               DISPLAY "Total No Of B/Material slips changed  :" AT POS
               ADD 40 TO POS
               DISPLAY WS-PENDING-DISPLAY AT POS.
       UIR-500.
           REWRITE INCR-REC
                INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE 
             "NEXT REGISTER BUSY ON RE-WRITE, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-INCR-ST1
               GO TO UIR-500.
           MOVE " " TO INCR-REC.
       UIR-999.
           EXIT.
      *
       READ-REGISTER SECTION.
       RIR-000.
           MOVE 2610 TO POS
           DISPLAY "READING REGISTER ................          " AT POS

           MOVE STTR2-REFERENCE1 TO INCR-INVOICE.
           MOVE STTR2-TYPE       TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               GO TO RIR-999.
       RIR-005.
           READ INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE " " TO INCR-PRINTED
               GO TO RIR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE 
             "REGISTER BUSY ON READ (RIR-005), IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-005.
       RIR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O STOCK-MASTER.
           IF WS-ST-ST1 NOT = 0 
               MOVE "STOCK BUSY ON OPEN, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-ST-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-ST-ST1
               GO TO OPEN-000.
      *     GO TO OPEN-016.
       OPEN-010.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-BO-ST1 NOT = 0 
               MOVE "STTRANS BUSY ON OPEN, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-BO-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               GO TO OPEN-010.
       OPEN-012.
           OPEN I-O STOCK2-TRANS-FILE.
           IF WS-BO2-ST1 NOT = 0 
               MOVE "STTRANS2 BUSY ON OPEN, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-BO2-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
              GO TO OPEN-012.
       OPEN-016.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0 
               MOVE 
             "REGISTER BUSY ON OPEN, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-INCR-ST1
              GO TO OPEN-016.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *      ACCEPT WS-DATE FROM DATE.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            PERFORM ERROR-020
            MOVE 2610 TO POS
            DISPLAY WS-MESSAGE AT POS.
            MOVE " " TO WS-RANGE1.
            MOVE 2710 TO POS.
            DISPLAY
            "Run Finished, Press <RETURN> to EXIT The Program.    "
             AT POS.
            ADD 50 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 24        TO CDA-ROW.
           MOVE 60        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO END-500
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO END-000.
       END-500.
            CLOSE STOCK-MASTER
                  INCR-REGISTER
                  STOCK-TRANS-FILE
                  STOCK2-TRANS-FILE.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "Error1Message".
       Copy "ErrorMessage".
       Copy "WriteDailyExcep1".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
