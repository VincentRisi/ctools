        IDENTIFICATION DIVISION.
        PROGRAM-ID. StSplit.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStOrders".
          Copy "SelectStTrans".
          Copy "SelectStTransLy".
          Copy "SelectStImports".
          Copy "SelectStReceipt".
          Copy "SelectStReceiptLy".
          Copy "SelectStChanges".
          Copy "SelectSlParameter".
          Copy "SelectSlDaily".
          Copy "SelectBmMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdOutOrd.
           COPY ChlfdStTrans.
           COPY ChlfdStTransLy.
           COPY ChlfdImpReceipts.
           COPY ChlfdStkReceipts.
           COPY ChlfdStkReceiptsLy.
           COPY ChlfdStockChanges.
           COPY ChlfdParam.
           COPY ChlfdDaily.
           COPY ChlfdToolkit.

       WORKING-STORAGE SECTION.
       77  NEW-STOCKNO        PIC X VALUE " ".      
       77  WS-CATEGORY        PIC X(3) VALUE " ".
       77  WS-END             PIC X VALUE " ".      
       77  WS-CREATE-ONE-STOCK   PIC X VALUE " ".      
       77  WS-VALID           PIC X VALUE " ".      
       77  WS-STOCKNUMBER     PIC X(15) VALUE " ".
       77  WS-VATPRICE        PIC 9(6)V99 VALUE 0.
       77  WS-NEWSTOCKNUMBER  PIC X(15) VALUE " ".
       77  WS-OLDSTOCKNUMBER  PIC X(15) VALUE " ".
       77  WS-BEG-STOCK       PIC X(15) VALUE " ".
       77  WS-END-STOCK       PIC X(15) VALUE " ".
       77  WS-TOOL-VALID      PIC X VALUE " ".
       77  WS-TOOLKIT-INVALID PIC X VALUE " ".
       77  WS-TOOLKITNUMBER   PIC X(15) VALUE " ".
       77  WS-NEW-KIT         PIC X(15) VALUE " ".
       77  WS-QTY             PIC 9(3) VALUE 0.
       77  WS-INQUIRY-PROGRAM PIC X(8) VALUE "StDescIq".
       01  WS-MERGE-STOCK.
           03  WS-QTYONHAND          PIC 9(6).
           03  WS-QTYONRESERVE       PIC 9(6).
           03  WS-QTYONORDER         PIC 9(6).
           03  WS-QTYONBORDER        PIC 9(6).
           03  WS-QTY-ST-TAKE        PIC 9(6).
           03  WS-QTYRECMTD          PIC S9(6).
           03  WS-QTYRECYTD          PIC S9(6).
           03  WS-QTYRECLAST         PIC S9(6).
           03  WS-QTYADJMTD          PIC S9(6).
           03  WS-QTYADJYTD          PIC S9(6).
           03  WS-QTYADJLAST         PIC S9(6).
           03  WS-SALESUNITMTD       PIC S9(6).
           03  WS-SALESUNITSYTD      PIC S9(6).
           03  WS-SALESUNITSLAST     PIC S9(6).
           03  WS-SALESRANDSMTD      PIC S9(7)V99.
           03  WS-SALESRANDSYTD      PIC S9(7)V99.
           03  WS-SALESRANDSLAST     PIC S9(7)V99.
           03  WS-SALESCOSTMTD       PIC S9(7)V99.
           03  WS-SALESCOSTYTD       PIC S9(7)V99.
           03  WS-SALESCOSTLAST      PIC S9(7)V99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1         PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1        PIC 99.
       01  WS-STCHANGE-STATUS.
           03  WS-STCHANGE-ST1      PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1       PIC 99.
       01  WS-STTRANSLY-STATUS.
           03  WS-STTRANSLY-ST1     PIC 99.
       01  WS-IMPRECEIPT-STATUS.
           03  WS-IMPRECEIPT-ST1    PIC 99.
       01  WS-STKRECEIPT-STATUS.
           03  WS-STKRECEIPT-ST1    PIC 99.
       01  WS-STKRECEIPTSLY-STATUS.
           03  WS-STKRECEIPTSLY-ST1 PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1   PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1         PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-TOOLKIT-ST1       PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
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
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           MOVE 0415 TO POS
           DISPLAY "*** STOCK SPLIT PROGRAM ****" AT POS
           MOVE 0515 TO POS
           DISPLAY "****************************" AT POS.
           PERFORM GET-DATA.
       CONTROL-020.
          EXIT PROGRAM.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO STOCK-RECORD.
            MOVE "N" TO NEW-STOCKNO
                        WS-VALID
                        WS-END.
       GET-005.
            MOVE 715 TO POS
            DISPLAY "FROM STOCK NUMBER: [               ]" AT POS
            MOVE 735 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 4         TO CDA-ROW.
           MOVE 34        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-BEG-STOCK.

      *      ACCEPT WS-BEG-STOCK AT POS
            MOVE WS-BEG-STOCK TO WS-STOCKNUMBER
            PERFORM START-STOCK.
            
            IF W-ESCAPE-KEY = 3
               PERFORM END-OFF.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-005.
       GET-010.
            MOVE 915 TO POS
            DISPLAY "  TO STOCK NUMBER: [               ]" AT POS
            MOVE 935 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 6         TO CDA-ROW.
           MOVE 34        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-END-STOCK.

      *      ACCEPT WS-END-STOCK AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO GET-005.
            IF WS-END-STOCK = " "
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-300
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-300.
            MOVE " " TO WS-MESSAGE
            MOVE 1420 TO POS
            DISPLAY WS-MESSAGE AT POS
            PERFORM ERROR-020.
       GET-350.
           PERFORM READ-STOCK-NEXT.

           PERFORM CREATE-SPECIAL-STOCKNUMBER.
           
      *     PERFORM CREATE-NEW-STOCKNUMBER.
      *     PERFORM CREATE-KEN-STOCKNUMBER.

           IF WS-END = "Y"
              GO TO GET-999.
              
           IF WS-STOCK-ST1 = 88
              GO TO GET-350.
               
           MOVE 2210 TO POS
           DISPLAY "OLD PART #:                 NEW PART #:" AT POS
           ADD 11 TO POS
           DISPLAY WS-OLDSTOCKNUMBER AT POS
           MOVE 2250 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
           
           MOVE ST-QTYONHAND      TO WS-QTYONHAND
           MOVE ST-QTYONRESERVE   TO WS-QTYONRESERVE
           MOVE ST-QTYONORDER     TO WS-QTYONORDER
           MOVE ST-QTYONBORDER    TO WS-QTYONBORDER
           MOVE ST-QTY-ST-TAKE    TO WS-QTY-ST-TAKE
           MOVE ST-QTYRECMTD      TO WS-QTYRECMTD
           MOVE ST-QTYRECYTD      TO WS-QTYRECYTD
           MOVE ST-QTYRECLAST     TO WS-QTYRECLAST
           MOVE ST-QTYADJMTD      TO WS-QTYADJMTD
           MOVE ST-QTYADJYTD      TO WS-QTYADJYTD
           MOVE ST-QTYADJLAST     TO WS-QTYADJLAST
           MOVE ST-SALESUNITMTD   TO WS-SALESUNITMTD
           MOVE ST-SALESUNITSYTD  TO WS-SALESUNITSYTD
           MOVE ST-SALESUNITSLAST TO WS-SALESUNITSLAST
           MOVE ST-SALESRANDSMTD  TO WS-SALESRANDSMTD
           MOVE ST-SALESRANDSYTD  TO WS-SALESRANDSYTD
           MOVE ST-SALESRANDSLAST TO WS-SALESRANDSLAST
           MOVE ST-SALESCOSTMTD   TO WS-SALESCOSTMTD
           MOVE ST-SALESCOSTYTD   TO WS-SALESCOSTYTD
           MOVE ST-SALESCOSTLAST  TO WS-SALESCOSTLAST.
       GET-550.
           MOVE "N" TO WS-CREATE-ONE-STOCK.
           MOVE ST-STOCKNUMBER TO WS-NEWSTOCKNUMBER.
           IF WS-CREATE-ONE-STOCK = "N"
               MOVE "Y" TO NEW-STOCKNO.
           PERFORM REWRITE-STOCK-RECORD.
           PERFORM ERROR-020.
           
           IF WS-VALID = "Y"
                MOVE "THIS TRY HAS BEEN ABORTED, TRY AGAIN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM CLEAR-SCREEN-FORM
                PERFORM DISPLAY-FORM
                GO TO GET-900.
            
            MOVE WS-OLDSTOCKNUMBER TO ST-STOCKNUMBER.
            PERFORM READ-STOCK.
            PERFORM DELETE-STOCK-RECORD.

           PERFORM READ-BACK-ORDERS.
           PERFORM READ-ST-TRANSLY.
           PERFORM READ-SUPPLIERS-ORDERS.
           PERFORM READ-STOCK-IMPORTS.
           PERFORM READ-STOCK-RECEIPTS.
           PERFORM READ-STOCK-RECEIPTSLY.
           PERFORM READ-TOOLKIT-HEADER.
           IF WS-TOOLKIT-INVALID = "N"
               PERFORM WRITE-NEW-KIT
               PERFORM DELETE-OLD-KIT.
           PERFORM READ-COMPONENT-OF-KIT.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
       GET-900.
           GO TO GET-350.
       GET-999.
           EXIT.
      *
       CREATE-NEW-STOCKNUMBER SECTION.
       CNST-005.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
           MOVE 1 TO SUB-1 SUB-2.
           MOVE ST-STOCKNUMBER TO ALPHA-RATE.
               
           MOVE ST-STOCKNUMBER TO WS-OLDSTOCKNUMBER.
       CNST-006.
      **************************************************************
      * IF POS 4=BLANK THEN THE PART NUMBER HAS ALREADY GOT A SPACE*
      * THEN WE MUST ABORT CHANGING THIS ITEM AND GO TO THE NEXT.  *
      **************************************************************
           IF AL-RATE (3) = " "
              MOVE 88 TO WS-STOCK-ST1
              GO TO CNST-999.
           IF AL-RATE (4) = " "
              MOVE 88 TO WS-STOCK-ST1
              GO TO CNST-999.
      **************************************************************
      * IF POS 15 BLANK THEN WE CAN EXPAND THE PART NUMBER, IF NOT *
      * THEN WE MUST ABORT CHANGING THIS ITEM AND GO TO THE NEXT.  *
      **************************************************************
           IF AL-RATE (15) = " "
              GO TO CNST-010.
           MOVE ST-STOCKNUMBER         TO WS-DAILY-1ST
           MOVE "NAME LENGTH > 15CHAR" TO WS-DAILY-2ND
           MOVE "CANNOT BE CHANGED IN" TO WS-DAILY-3RD
           MOVE "THIS RUN.           " TO WS-DAILY-4TH
           PERFORM WRITE-DAILY.

           MOVE 88 TO WS-STOCK-ST1
           GO TO CNST-999.
       CNST-010.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2)
           IF SUB-1 < 3
               ADD 1 TO SUB-1 SUB-2
               GO TO CNST-010.
           IF SUB-1 = 3
              ADD 1 TO SUB-1
              ADD 2 TO SUB-2
              GO TO CNST-010.
           IF SUB-1 < 15
              ADD 1 TO SUB-1 SUB-2
              GO TO CNST-010.
       CNST-020.
           MOVE DATA-RATE TO ST-STOCKNUMBER.
       CNST-999.
           EXIT.
      *
       CREATE-KEN-STOCKNUMBER SECTION.
       CNSTK-005.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
           MOVE 1 TO SUB-1 SUB-2.
           MOVE ST-STOCKNUMBER TO ALPHA-RATE.
               
           MOVE ST-STOCKNUMBER TO WS-OLDSTOCKNUMBER.
       CNSTK-006.
      **************************************************************
      * IF POS 15 BLANK THEN WE CAN EXPAND THE PART NUMBER, IF NOT *
      * THEN WE MUST ABORT CHANGING THIS ITEM AND GO TO THE NEXT.  *
      **************************************************************
           IF AL-RATE (15) = " "
              GO TO CNSTK-007.
           MOVE ST-STOCKNUMBER         TO WS-DAILY-1ST
           MOVE "NAME LENGTH > 15CHAR" TO WS-DAILY-2ND
           MOVE "CANNOT BE CHANGED IN" TO WS-DAILY-3RD
           MOVE "THIS RUN.           " TO WS-DAILY-4TH
           PERFORM WRITE-DAILY.
           MOVE 88 TO WS-STOCK-ST1
           GO TO CNSTK-999.
       CNSTK-007.
           IF ST-CATEGORY = "KEN" OR = "SEN" OR = "YRK" OR = "SHR"
            OR = "IND" OR = "SWT" OR = "KBE" OR = "IDX" OR = "PCL"
            OR = "OSA" OR = "OXD" OR = "RTL" OR = "MTL" OR = "ATL"
            OR = "JHL" OR = "YMT" OR = "EDI" OR = "SOL" OR = "OFI"
            OR = "AVN" OR = "COT" OR = "SSF" OR = "TFF"
               GO TO CNSTK-008.
               
           MOVE 88 TO WS-STOCK-ST1.
           GO TO CNSTK-999.
       CNSTK-008.
           IF ST-CATEGORY = "KEN"
              GO TO CNSTK-010.
              
           MOVE "KEN " TO DATA-RATE
           MOVE 5 TO SUB-2.
       CNSTK-010.
           IF AL-RATE (SUB-1) = " " OR = "-"
            IF SUB-1 < 15
              ADD 1 TO SUB-1
              GO TO CNSTK-010
            ELSE
              GO TO CNSTK-020.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           IF SUB-1 < 15
              ADD 1 TO SUB-1 SUB-2
              GO TO CNSTK-010.
       CNSTK-020.
           MOVE DATA-RATE TO ST-STOCKNUMBER.
       CNSTK-999.
           EXIT.
      *
       CREATE-SPECIAL-STOCKNUMBER SECTION.
       CSPNS-005.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
           MOVE 1 TO SUB-1 SUB-2.
           MOVE ST-STOCKNUMBER TO ALPHA-RATE
                                  WS-OLDSTOCKNUMBER.
       CSPNS-007.
           IF ST-CATEGORY = "MAJ"
               GO TO CSPNS-008.
               
           MOVE 88 TO WS-STOCK-ST1
               GO TO CSPNS-999.
       CSPNS-008.
           MOVE "MT " TO DATA-RATE
           MOVE 5 TO SUB-1
           MOVE 4 TO SUB-2.
       CSPNS-010.
           IF SUB-1 = 5
            IF AL-RATE (5) = "M"
             AND AL-RATE (6) = "T"
              ADD 2 TO SUB-1
              GO TO CSPNS-010.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           IF SUB-1 < 15
              ADD 1 TO SUB-1 SUB-2
              GO TO CSPNS-010.
       CSPNS-020.
           MOVE DATA-RATE TO ST-STOCKNUMBER.
       CSPNS-999.
           EXIT.
      *
       READ-BACK-ORDERS SECTION.
       RBO-000.
            PERFORM ERROR1-020.
            MOVE 2910 TO POS.
            DISPLAY "Changing Back-Orders to new NUMBER......" AT POS.
            MOVE WS-OLDSTOCKNUMBER TO STTR-STOCK-NUMBER.
            START STOCK-TRANS-FILE KEY NOT < STTR-STOCK-NUMBER
                 INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 NOT = 0
               GO TO RBO-999.
       RBO-002.
            READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-STTRANS-ST1 = 10
               GO TO RBO-999.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE "BO FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RBO-002.
            IF STTR-STOCK-NUMBER NOT = WS-OLDSTOCKNUMBER
               GO TO RBO-999.
       RBO-005.
            MOVE WS-NEWSTOCKNUMBER TO STTR-STOCK-NUMBER.
            REWRITE STOCK-TRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE STTR-KEY               TO WS-DAILY-1ST
               MOVE STTR-STOCK-NUMBER      TO WS-DAILY-2ND
               MOVE "NO CHANGE TO NEW NO " TO WS-DAILY-3RD
               MOVE WS-NEWSTOCKNUMBER      TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RBO-002.
       RBO-999.
           EXIT.
      *
       READ-ST-TRANSLY SECTION.
       RSTLY-000.
            PERFORM ERROR1-020.
            MOVE 2910 TO POS.
            DISPLAY "Changing ST-TRANSLY to new NUMBER......" AT POS.
            MOVE WS-OLDSTOCKNUMBER TO STTR-LY-STOCK-NUMBER.
            MOVE "Y"               TO STTR-LY-ST-COMPLETE.
            START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-ST-KEY
                 INVALID KEY NEXT SENTENCE.
            IF WS-STTRANSLY-ST1 NOT = 0
               GO TO RSTLY-999.
       RSTLY-002.
            READ STOCK-TRANSLY-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-STTRANSLY-ST1 = 10
               GO TO RSTLY-999.
            IF WS-STTRANSLY-ST1 NOT = 0
               MOVE "STTRANSLY FILE BUSY ON READ, BE PATIENT!"
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO RSTLY-002.
            IF STTR-LY-STOCK-NUMBER NOT = WS-OLDSTOCKNUMBER
               GO TO RSTLY-999.
       RSTLY-005.
            MOVE WS-NEWSTOCKNUMBER TO STTR-LY-STOCK-NUMBER.
            REWRITE STOCK-TRANSLY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-STTRANSLY-ST1 NOT = 0
               MOVE STTR-LY-KEY            TO WS-DAILY-1ST
               MOVE STTR-LY-STOCK-NUMBER   TO WS-DAILY-2ND
               MOVE "NO CHANGE TO NEW NO " TO WS-DAILY-3RD
               MOVE WS-NEWSTOCKNUMBER      TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RSTLY-002.
       RSTLY-999.
           EXIT.
      *
       READ-SUPPLIERS-ORDERS SECTION.
       RSQ-000.
            PERFORM ERROR1-020.
            MOVE 2910 TO POS.
            DISPLAY "Changing Suppliers-Orders to new NUMBER...."
             AT POS.
            MOVE WS-OLDSTOCKNUMBER TO OO-STOCK-NUMBER.
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
               MOVE "ST-ORDERS FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO RSQ-002.
            IF OO-STOCK-NUMBER NOT = WS-OLDSTOCKNUMBER
               GO TO RSQ-999.
       RSQ-005.
            DELETE OUTSTANDING-ORDERS
                INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE OO-KEY                 TO WS-DAILY-1ST
               MOVE OO-STOCK-NUMBER        TO WS-DAILY-2ND
               MOVE "DELETE OF SUPP/ORDER" TO WS-DAILY-3RD
               MOVE WS-NEWSTOCKNUMBER      TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
       RSQ-010.
            MOVE WS-NEWSTOCKNUMBER TO OO-STOCK-NUMBER.
            WRITE OUT-ORDER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE OO-KEY                 TO WS-DAILY-1ST
               MOVE OO-STOCK-NUMBER        TO WS-DAILY-2ND
               MOVE "NO CHANGE TO NEW NO " TO WS-DAILY-3RD
               MOVE WS-NEWSTOCKNUMBER      TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RSQ-002.
       RSQ-999.
           EXIT.
      *
       READ-STOCK-IMPORTS SECTION.
       RSI-000.
            PERFORM ERROR1-020.
            MOVE 2910 TO POS.
            DISPLAY "Changing Stock Imports File........       " AT POS.
            MOVE WS-OLDSTOCKNUMBER TO IMRE-STOCK-NUMBER.
            START IMPRECEIPTS-FILE KEY NOT < IMRE-STOCK-NUMBER
                 INVALID KEY NEXT SENTENCE.
           IF WS-IMPRECEIPT-ST1 NOT = 0
               GO TO RSI-999.
       RSI-030.
           READ IMPRECEIPTS-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-IMPRECEIPT-ST1 = 10
              GO TO RSI-999.
           IF WS-IMPRECEIPT-ST1 NOT = 0
               MOVE "IMPORTS FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-IMPRECEIPT-ST1
              GO TO RSI-030.
           IF IMRE-STOCK-NUMBER NOT = WS-OLDSTOCKNUMBER
              GO TO RSI-999.
       RSI-040.
           MOVE WS-NEWSTOCKNUMBER TO IMRE-STOCK-NUMBER.
       RSI-050.
           REWRITE IMPORT-RECEIPTS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-IMPRECEIPT-ST1 = 23 OR 35 OR 49
              MOVE "IMPORTS STOCK NUMBER" TO WS-DAILY-1ST
              MOVE "NOT CHANGED TO NEW :" TO WS-DAILY-2ND
              MOVE IMRE-STOCK-NUMBER      TO WS-DAILY-3RD
              MOVE " "                    TO WS-DAILY-4TH
              PERFORM WRITE-DAILY
              GO TO RSI-030.
           IF WS-IMPRECEIPT-ST1 NOT = 0
              MOVE "IMPORTS BUSY ON RE-WRITE, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-IMPRECEIPT-ST1
               GO TO RSI-050.
           GO TO RSI-030.
       RSI-999.
           EXIT.
      *
       READ-STOCK-RECEIPTS SECTION.
       RSTK-REC-000.
            PERFORM ERROR1-020.
            MOVE 2910 TO POS.
            DISPLAY "Changing Stock Receipts File........     " AT POS.
            MOVE WS-OLDSTOCKNUMBER TO STRE-STOCK-NUMBER.
            START STKRECEIPTS-FILE KEY NOT < STRE-STOCK-NUMBER
                 INVALID KEY NEXT SENTENCE.
           IF WS-STKRECEIPT-ST1 NOT = 0
               GO TO RSTK-REC-999.
       RSTK-REC-030.
           READ STKRECEIPTS-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-STKRECEIPT-ST1 = 10
              GO TO RSTK-REC-999.
           IF WS-STKRECEIPT-ST1 NOT = 0
              MOVE "RECEIPTS BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STKRECEIPT-ST1
               GO TO RSTK-REC-030.
           IF STRE-STOCK-NUMBER NOT = WS-OLDSTOCKNUMBER
              GO TO RSTK-REC-999.
       RSTK-REC-040.
           MOVE WS-NEWSTOCKNUMBER TO STRE-STOCK-NUMBER.
       RSTK-REC-050.
           REWRITE STOCK-RECEIPTS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-STKRECEIPT-ST1 = 23 OR 35 OR 49
              MOVE "ST-RECP STOCK NUMBER" TO WS-DAILY-1ST
              MOVE "NOT CHANGED TO NEW :" TO WS-DAILY-2ND
              MOVE STRE-STOCK-NUMBER      TO WS-DAILY-3RD
              MOVE " "                    TO WS-DAILY-4TH
              PERFORM WRITE-DAILY
              GO TO RSTK-REC-030.
           IF WS-STKRECEIPT-ST1 NOT = 0
              MOVE "RECEIPTS BUSY ON RE-WRITE, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STKRECEIPT-ST1
               GO TO RSTK-REC-050.
           GO TO RSTK-REC-030.
       RSTK-REC-999.
           EXIT.
      *
       READ-STOCK-RECEIPTSLY SECTION.
       RSTK-RECLY-000.
            PERFORM ERROR1-020.
            MOVE 2910 TO POS.
            DISPLAY "Changing Stock ReceiptsLY File........   " AT POS.
            MOVE WS-OLDSTOCKNUMBER TO STRELY-STOCK-NUMBER.
            START STKRECEIPTSLY-FILE KEY NOT < STRELY-STOCK-NUMBER
                 INVALID KEY NEXT SENTENCE.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
               GO TO RSTK-RECLY-999.
       RSTK-RECLY-030.
           READ STKRECEIPTSLY-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-STKRECEIPTSLY-ST1 = 10
              GO TO RSTK-RECLY-999.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
              MOVE "RECEIPTS BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPTSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STKRECEIPTSLY-ST1
              GO TO RSTK-RECLY-030.
           IF STRELY-STOCK-NUMBER NOT = WS-OLDSTOCKNUMBER
              GO TO RSTK-RECLY-999.
       RSTK-RECLY-040.
           MOVE WS-NEWSTOCKNUMBER TO STRELY-STOCK-NUMBER.
       RSTK-RECLY-050.
           REWRITE STOCK-RECEIPTSLY-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-STKRECEIPTSLY-ST1 = 23 OR 35 OR 49
              MOVE "ST-RECP STOCK NUMBER" TO WS-DAILY-1ST
              MOVE "NOT CHANGED TO NEW :" TO WS-DAILY-2ND
              MOVE STRELY-STOCK-NUMBER    TO WS-DAILY-3RD
              MOVE " "                    TO WS-DAILY-4TH
              PERFORM WRITE-DAILY
              GO TO RSTK-RECLY-030.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
              MOVE "RECEIPTSLY BUSY ON RE-WRITE, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPTSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STKRECEIPTSLY-ST1
               GO TO RSTK-RECLY-050.
           GO TO RSTK-RECLY-030.
       RSTK-RECLY-999.
           EXIT.
      *
       START-KIT SECTION.
       SK-010.
           MOVE WS-OLDSTOCKNUMBER TO TO-TOOLKIT-NUMBER.
           MOVE " "               TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT LESS TO-KEY
                INVALID KEY NEXT SENTENCE.
       SK-999.
           EXIT.
      *
       READ-NEXT-KIT SECTION.
       RNK-000.
           READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               GO TO RNK-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "KIT BUSY ON READ NEXT, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO RNK-000.
       RNK-999.
           EXIT.
      *
       READ-TOOLKIT-HEADER SECTION.
       RTH-000.
           PERFORM ERROR1-020.
           MOVE 2910 TO POS.
           DISPLAY "WRITING NEW BILL-OF-MATERIAL......" AT POS.
           MOVE WS-OLDSTOCKNUMBER TO TO-TOOLKIT-NUMBER.
           MOVE " "               TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-KEY
                INVALID KEY NEXT SENTENCE.
       RTH-010.
           READ TOOLKITS
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               MOVE "Y" TO WS-TOOLKIT-INVALID
               GO TO RTH-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKIT HEADER BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO RTH-000.
           MOVE "N" TO WS-TOOLKIT-INVALID.
       RTH-999.
           EXIT.
      *
       WRITE-NEW-KIT SECTION.
       WNK-005.
           PERFORM ERROR1-020.
           MOVE 2910 TO POS.
           DISPLAY "WRITING NEW BILL-OF-MATERIAL......" AT POS.
           PERFORM START-KIT.
       WNK-010.
           PERFORM READ-NEXT-KIT.
           IF WS-TOOLKIT-ST1 = 10
              GO TO WNK-999.
           IF TO-TOOLKIT-NUMBER NOT = WS-OLDSTOCKNUMBER
              GO TO WNK-999.
           MOVE WS-NEWSTOCKNUMBER TO TO-TOOLKIT-NUMBER.
           PERFORM WRITE-TOOLKIT.
           GO TO WNK-010.
       WNK-999.
           EXIT.
      *
       WRITE-TOOLKIT SECTION.
       WRT-010.
           WRITE TOOL-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              MOVE
              "THE FOLLOWING ITEM ALREADY EXITS, 'ESC' FOR ITEM #."
              TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              MOVE TO-COMPONENT-NUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-TOOLKIT-ST1
              GO TO WRT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "TOOLKITS BUSY ON WRITE, WRT-010, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO WRT-010.
       WRT-999.
           EXIT.
      *
       DELETE-OLD-KIT SECTION.
       DT-010.
           PERFORM ERROR1-020.
           MOVE 2910 TO POS.
           DISPLAY "DELETEING OLD BILL-OF-MATERIAL......" AT POS.
           MOVE WS-OLDSTOCKNUMBER TO TO-TOOLKIT-NUMBER.
           MOVE " "               TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKITS BUSY ON START-DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO DT-010.
       DT-020.
           READ TOOLKITS NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               GO TO DT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKITS BUSY ON READ-DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO DT-020.
           IF TO-TOOLKIT-NUMBER = WS-OLDSTOCKNUMBER
               GO TO DT-050.
           GO TO DT-999.
       DT-050.
           DELETE TOOLKITS
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               GO TO DT-020.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKIT RECORD BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO DT-050.
           GO TO DT-020.
       DT-999.
           EXIT.
      *
       READ-COMPONENT-OF-KIT SECTION.
       RCOK-000.
           PERFORM ERROR1-020.
           CLOSE TOOLKITS.
           PERFORM OPEN-006.
           MOVE 2910 TO POS.
           DISPLAY "CHANGING ITEM IN BILL-OF-MATERIAL......" AT POS.
           MOVE " " TO TO-TOOLKIT-NUMBER
                       TO-COMPONENT-NUMBER.
       RCOK-005.
           MOVE WS-OLDSTOCKNUMBER TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-COMPONENT-NUMBER
               INVALID KEY NEXT SENTENCE.
       RCOK-010.
           READ TOOLKITS NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               GO TO RCOK-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKIT COMP. BUSY ON READ, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE TOOL-REC TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RCOK-999.
           IF TO-COMPONENT-NUMBER NOT = WS-OLDSTOCKNUMBER
               GO TO RCOK-999.
       RCOK-015.
           IF TO-TOOLKIT-NUMBER NOT = WS-TOOLKITNUMBER
               MOVE TO-TOOLKIT-NUMBER TO WS-TOOLKITNUMBER
               MOVE 2610 TO POS
               DISPLAY "TOOLKIT CURRENTLY BEING CHANGED =" AT POS
               MOVE 2645 TO POS
               DISPLAY WS-TOOLKITNUMBER AT POS.
       RCOK-020.
           IF TO-COMPONENT-NUMBER = WS-OLDSTOCKNUMBER
              MOVE TO-TOOLKIT-NUMBER   TO WS-TOOLKITNUMBER
              MOVE TO-QUANTITY         TO WS-QTY
              PERFORM DELETE-OLD-COMPONENT
              PERFORM WRITE-NEW-COMPONENT.
              CLOSE TOOLKITS.
              PERFORM OPEN-006.
           GO TO RCOK-005.
       RCOK-999.
           EXIT.
      *
       WRITE-NEW-COMPONENT SECTION.
       WNC-000.
           MOVE WS-TOOLKITNUMBER  TO TO-TOOLKIT-NUMBER.
           MOVE WS-NEWSTOCKNUMBER TO TO-COMPONENT-NUMBER.
           MOVE WS-QTY            TO TO-QUANTITY.
           START TOOLKITS KEY NOT < TO-KEY
               INVALID KEY NEXT SENTENCE.
       WNC-002.
           READ TOOLKITS WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               GO TO WNC-010.
           ADD WS-QTY TO TO-QUANTITY.
       WNC-005.
           REWRITE TOOL-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              GO TO WNC-010.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "TOOLKIT RECORD NOT REWRITTEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO WNC-005.
           GO TO WNC-999.
       WNC-010.
           WRITE TOOL-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              GO TO WNC-005.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "TOOLKIT RECORD NOT WRITTEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO WNC-010.
       WNC-999.
           EXIT.
      *
       DELETE-OLD-COMPONENT SECTION.
       DOC-050.
           DELETE TOOLKITS
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               GO TO DOC-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKIT BUSY ON COMP. DELETE, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE TOOL-REC TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
      *         GO TO DOC-050.
       DOC-999.
           EXIT.
      *
       DELETE-STOCK-RECORD SECTION.
       DSR-000.
            IF NEW-STOCKNO = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       RELEASE-STOCK-RECORD SECTION.
       REL-000.
           UNLOCK STOCK-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-STOCK-RECORD SECTION.
       RSR-010.
          IF NEW-STOCKNO = "Y"
           IF WS-CREATE-ONE-STOCK = "N"
              MOVE WS-NEWSTOCKNUMBER TO WS-CATEGORY
              MOVE WS-CATEGORY       TO ST-CATEGORY
              GO TO RSR-020.
       RSR-012.
           ADD  WS-QTYONHAND      TO ST-QTYONHAND
           ADD  WS-QTYONRESERVE   TO ST-QTYONRESERVE
           ADD  WS-QTYONORDER     TO ST-QTYONORDER
           ADD  WS-QTYONBORDER    TO ST-QTYONBORDER
           ADD  WS-QTY-ST-TAKE    TO ST-QTY-ST-TAKE
           ADD  WS-QTYRECMTD      TO ST-QTYRECMTD
           ADD  WS-QTYRECYTD      TO ST-QTYRECYTD
           ADD  WS-QTYRECLAST     TO ST-QTYRECLAST
           ADD  WS-QTYADJMTD      TO ST-QTYADJMTD
           ADD  WS-QTYADJYTD      TO ST-QTYADJYTD
           ADD  WS-QTYADJLAST     TO ST-QTYADJLAST
           ADD  WS-SALESUNITMTD   TO ST-SALESUNITMTD
           ADD  WS-SALESUNITSYTD  TO ST-SALESUNITSYTD
           ADD  WS-SALESUNITSLAST TO ST-SALESUNITSLAST
           ADD  WS-SALESRANDSMTD  TO ST-SALESRANDSMTD
           ADD  WS-SALESRANDSYTD  TO ST-SALESRANDSYTD
           ADD  WS-SALESRANDSLAST TO ST-SALESRANDSLAST
           ADD  WS-SALESCOSTMTD   TO ST-SALESCOSTMTD
           ADD  WS-SALESCOSTYTD   TO ST-SALESCOSTYTD
           ADD  WS-SALESCOSTLAST  TO ST-SALESCOSTLAST.
       RSR-015.
          REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STOCK-ST1 = 23 OR 35 OR 49
              MOVE "THE NEW STOCKNUMBER IS NOT A CURRENT VALID NUMBER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
      *        MOVE "Y" TO WS-VALID
              GO TO RSR-999.
          IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCKNUMBER BUSY ON RE-WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RSR-015.
              
          GO TO RSR-999.
      *    GO TO RSR-900.
       RSR-020.
          WRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STOCK-ST1 NOT = 0
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCKNUMBER BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
      *        MOVE "Y" TO WS-VALID
              MOVE WS-NEWSTOCKNUMBER TO WS-CATEGORY
              MOVE WS-CATEGORY       TO ST-CATEGORY
              GO TO RSR-012.              
              
              GO TO RSR-999.
       RSR-900.
          IF INVQUES-STOCK-CHANGE NOT = "Y"
             GO TO RSR-999.
          PERFORM WRITE-STOCK-CHANGES.
       RSR-999.
          EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
             MOVE ST-STOCKNUMBER TO WS-STOCKNUMBER.
             START STOCK-MASTER KEY NOT < ST-KEY
                 INVALID KEY NEXT SENTENCE.
       R-ST-010.
             READ STOCK-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-SCREEN-FORM
                MOVE "Y" TO NEW-STOCKNO
                MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER
                GO TO R-ST-999.
             IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK-ST1
                GO TO R-ST-010.
             MOVE "N" TO NEW-STOCKNO.
       R-ST-999.
             EXIT.
      *
       WRITE-STOCK-CHANGES SECTION.
       WSTCH-000.
             MOVE WS-OLDSTOCKNUMBER TO STCH-STOCKNUMBER.
             START STOCKCHANGE-MASTER KEY NOT < STCH-KEY
               INVALID KEY NEXT SENTENCE.
       WSTCH-005.
             READ STOCKCHANGE-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STCHANGE-ST1 = 23 OR 35 OR 49
                GO TO WSTCH-006.
             IF WS-STCHANGE-ST1 NOT = 0
                MOVE "STOCKCHANGE BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STCHANGE-ST1
                GO TO WSTCH-005.
       WSTCH-006.
          MOVE WS-NEWSTOCKNUMBER   TO STCH-DESCRIPTION1
          MOVE ST-DESCRIPTION2     TO STCH-DESCRIPTION2
          MOVE ST-CATEGORY         TO STCH-CATEGORY
          MOVE ST-SUPPLIER         TO STCH-SUPPLIER
          MOVE ST-FOREIGNCOST      TO STCH-FOREIGNCOST
          MOVE ST-PRICE            TO STCH-PRICE
          MOVE ST-UNITOFMEASURE    TO STCH-UNITOFMEASURE
          MOVE ST-DISCOUNT1        TO STCH-DISCOUNT1
          MOVE ST-DISCOUNT2        TO STCH-DISCOUNT2
          MOVE ST-DISCOUNT3        TO STCH-DISCOUNT3
          MOVE ST-DISCOUNT4        TO STCH-DISCOUNT4
          MOVE ST-DISCOUNT5        TO STCH-DISCOUNT5
          MOVE ST-DISCOUNT6        TO STCH-DISCOUNT6
          MOVE ST-DISCOUNT7        TO STCH-DISCOUNT7
          MOVE ST-DISCOUNT8        TO STCH-DISCOUNT8
          MOVE ST-DISCOUNT9        TO STCH-DISCOUNT9
          MOVE ST-MINBUYQTY        TO STCH-MINBUYQTY
          MOVE ST-ANALYSIS         TO STCH-ANALYSIS
          MOVE ST-DUTYPERCENT      TO STCH-DUTYPERCENT
          MOVE ST-DUTYTARIFF       TO STCH-DUTYTARIFF
          MOVE ST-SURCHARGE        TO STCH-SURCHARGE
          MOVE ST-PERMIT           TO STCH-PERMIT.
      ******************************************************************
      *D=DELETE, C=CHANGE R=RENAMED  N=NEW NUMBER  M=MERGE TWO NUMBERS *
      ******************************************************************
          IF WS-CREATE-ONE-STOCK NOT = "Y"
              MOVE "R" TO STCH-TYPE-OF-CHANGE
          ELSE
              MOVE "M" TO STCH-TYPE-OF-CHANGE.
       WSTCH-010.
           IF WS-STCHANGE-ST1 = 23 OR 35 OR 49
              GO TO WSTCH-020.
           REWRITE STOCKCHANGE-RECORD
              INVALID KEY NEXT SENTENCE.
           IF WS-STCHANGE-ST1 NOT = 0
              MOVE "STOCKCHANGE BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STCHANGE-ST1
                GO TO WSTCH-010.
          GO TO WSTCH-999.
       WSTCH-020.
          WRITE STOCKCHANGE-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STCHANGE-ST1 NOT = 0
              MOVE "STOCKCHANGE BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STCHANGE-ST1
                GO TO WSTCH-020.
       WSTCH-999.
           EXIT.
      *
       START-STOCK SECTION.
       ST-ST-000.
              MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
              START STOCK-MASTER KEY NOT LESS ST-STOCKNUMBER.
       ST-ST-999.
             EXIT.
      *
       READ-STOCK-NEXT SECTION.
       RSN-005. 
           READ STOCK-MASTER NEXT WITH LOCK
             AT END 
               MOVE " " TO ST-STOCKNUMBER
                           WS-STOCKNUMBER
               MOVE "Y" TO WS-END
               GO TO RSN-999.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
            MOVE "STOCK FILE BUSY ON READ-NEXT-LOCK-23, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RSN-005.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ-NEXT-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               PERFORM START-STOCK
               GO TO RSN-005.

           MOVE 2210 TO POS
           DISPLAY "OLD PART #:                            " AT POS
           ADD 11 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
               
           IF ST-STOCKNUMBER < WS-BEG-STOCK
               GO TO RSN-005.
           IF ST-STOCKNUMBER > WS-END-STOCK
               MOVE "Y" TO WS-END
               GO TO RSN-999.
               
           MOVE ST-STOCKNUMBER TO WS-STOCKNUMBER.
           MOVE "N" TO NEW-STOCKNO.
       RSN-999.
             EXIT.
      *
       READ-INVQUES-FILE SECTION.
       RINVQUES-000.
            MOVE 1 TO PA-RECORD.
            MOVE 6 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RINVQUES-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "N" TO INVQUES-STOCK-CHANGE
               GO TO RINVQUES-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY RINVQUES, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RINVQUES-010.
       RINVQUES-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-002.
            OPEN I-O STKRECEIPTS-FILE.
            IF WS-STKRECEIPT-ST1 NOT = 0
               MOVE "STOCK RECEIPTS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
       OPEN-003.
            OPEN I-O IMPRECEIPTS-FILE.
            IF WS-IMPRECEIPT-ST1 NOT = 0
               MOVE 0 TO WS-IMPRECEIPT-ST1
               MOVE "IMPORTS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-003.
       OPEN-004.
            OPEN I-O STKRECEIPTSLY-FILE.
            IF WS-STKRECEIPTSLY-ST1 NOT = 0
               MOVE "STOCK RECEIPTSLY BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-004.
       OPEN-005.
            OPEN I-O STOCK-TRANS-FILE.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE 0 TO WS-STTRANS-ST1
               MOVE "BO FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-006.
            OPEN I-O TOOLKITS.
            IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLFILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.
       OPEN-007.
            OPEN I-O STOCK-TRANSLY-FILE.
            IF WS-STTRANSLY-ST1 NOT = 0
               MOVE "ST-TRANS-LY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO OPEN-007.
       OPEN-008.
            OPEN I-O OUTSTANDING-ORDERS.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO WS-OUTORD-ST1
               MOVE "SORDER-FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-008.
       OPEN-009.
            OPEN I-O STOCKCHANGE-MASTER.
            IF WS-STCHANGE-ST1 NOT = 0
               MOVE 0 TO WS-STCHANGE-ST1
               MOVE "STCHANGE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-009.
       OPEN-014.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
           PERFORM READ-INVQUES-FILE.
           CLOSE PARAMETER-FILE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StNoChMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER
                  OUTSTANDING-ORDERS
                  STOCK-TRANS-FILE
                  STOCK-TRANSLY-FILE
                  IMPRECEIPTS-FILE
                  STKRECEIPTS-FILE
                  STKRECEIPTSLY-FILE
                  TOOLKITS.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "ClearFormStock".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldInv".
       Copy "WriteFieldPrice".
       Copy "WriteFieldQty".
       Copy "WriteFieldDate".
       Copy "WriteFieldDuty".
       Copy "WriteFieldForeign".
       Copy "WriteFieldTariff".
       Copy "WriteFieldAmount".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldSale".
       Copy "WriteFieldNumber".
       Copy "WriteFieldValue".
       Copy "DisplayForm".
       Copy "UserFillField".
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
      *
      * END-OF-JOB
