        IDENTIFICATION DIVISION.
        PROGRAM-ID. StBoStRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectStMaster".
         Copy "SelectStTrans".
         Copy "SelectSlRegister".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
              ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdStock.
           COPY ChlfdStTrans.
           COPY ChlfdRegister.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(255).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-QUANTITY          PIC S9(6) VALUE 0.
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-RANGE3            PIC X VALUE " ".
       77  WS-CAT               PIC X(3) VALUE " ".
       77  WS-VALUE             PIC S9(6)V99 VALUE 0.
       77  WS-TOTAL-VALUE       PIC S9(7)V99 VALUE 0.
       77  WS-RUNNING-TOTAL     PIC S9(7)V99 VALUE 0.
       77  WS-DO-NOT-SUPPLY-TOT PIC S9(7)V99 VALUE 0.
       77  WS-KIT-BO-TOT        PIC S9(7)V99 VALUE 0.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1   PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X VALUE " ".
           03  SP-REST          PIC X(14) VALUE " ".
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(15) VALUE "B A C K - O R D".
           03  FILLER         PIC X(9) VALUE " E R   R ".
           03  FILLER         PIC X(7) VALUE "E P O R".
           03  FILLER         PIC X(5) VALUE " T   ".
           03  FILLER         PIC X(19) VALUE "B Y   S T O C K   N".
           03  FILLER         PIC X(15) VALUE " U M B E R".
           03  H1-TYPE        PIC X(23) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
       01  HEAD2.
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(65) VALUE ALL "*".
           03  FILLER         PIC X(37) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(37) VALUE "STOCK".
           03  FILLER         PIC X(36) VALUE "A C C O U N T".
           03  FILLER         PIC X(25) VALUE "INTERNAL ORDER".
           03  FILLER         PIC X(37) VALUE "QUANTITY".
       01  HEAD5.
           03  FILLER         PIC X(16) VALUE "NUMBER".
           03  FILLER         PIC X(21) VALUE "DESCRIPTION".
           03  FILLER         PIC X(8) VALUE "NUMBER".
           03  FILLER         PIC X(26) VALUE "NAME".
           03  FILLER         PIC X(22) VALUE "   No:      DATE".
           03  FILLER         PIC X(21) VALUE "ORDER READY SHIPD".
           03  FILLER         PIC X(22) VALUE "PRICE    VALUE  SUPPLY".
       01  DETAIL-LINE.
           03  D-STOCKNO      PIC X(16).
           03  D-DESC         PIC X(21).
           03  D-ACCNO        PIC 9(7).
           03  FILLER         PIC X(1).
           03  D-ACCNAME      PIC X(26).
           03  D-PONO         PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-ERROR        PIC X.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-DATE         PIC X(10).
           03  FILLER         PIC X(2) VALUE " ".
           03  D-ORDER        PIC Z(5)9.
           03  D-SHIP         PIC Z(5)9.
           03  D-SHIPPED      PIC Z(5)9.
           03  D-PRICE        PIC Z(5)9.99.
           03  D-VALUE        PIC Z(5)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SUPPLY       PIC X(8).
       01  TOTAL-LINE.
           03  FILLER         PIC X(102) VALUE " ".
           03  FILLER         PIC X(16) VALUE "CATEGORY TOTAL:".
           03  TOT-VALUE      PIC Z(6)9.99.
           03  FILLER         PIC X(12) VALUE " ".
       01  RUNNING-LINE.
           03  FILLER         PIC X(102) VALUE " ".
           03  RUN-DESC       PIC X(16) VALUE "RUNNING TOTAL :".
           03  RUN-VALUE      PIC Z(6)9.99.
           03  FILLER         PIC X(12) VALUE " ".
       01  DO-NOT-LINE.
           03  FILLER         PIC X(102) VALUE " ".
           03  FILLER         PIC X(16) VALUE "DO-NOT-SUPPLY :".
           03  DO-NOT-VALUE   PIC Z(6)9.99.
           03  FILLER         PIC X(12) VALUE " ".
       01  KIT-TOT-LINE.
           03  FILLER         PIC X(102) VALUE " ".
           03  FILLER         PIC X(16) VALUE "KIT-BO-TOTAL  :".
           03  KIT-TOT-VALUE  PIC Z(6)9.99.
           03  FILLER         PIC X(12) VALUE " ".
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
           DISPLAY "** BACK ORDERS BY STOCK NUMBER REPORT **" AT POS
           MOVE 415 TO POS
           DISPLAY "****************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           IF WS-RANGE3 NOT = "R"
              PERFORM PRINT-ROUTINE
           ELSE
              PERFORM PRINT-RESET.
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
           MOVE 7          TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
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
           MOVE 9          TO CDA-ROW.
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
            MOVE 1410 TO POS.
            DISPLAY "PRINT B/O WITH ZERO QUANTITY, Y OR N;" AT POS.
            MOVE 1510 TO POS.
            DISPLAY "OR 'A' FOR ALL ORDERS, (B/O & PENDING)," AT POS.
            MOVE 1610 TO POS.
            DISPLAY "OR 'R' TO RESET ALL QTY FIELDS = ' '     "
                  AT POS.
            MOVE 1710 TO POS.
            DISPLAY "OR 'P' FOR PENDING ORDERS ONLY, QTY > 0 : [ ]"
                  AT POS.
            ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14         TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE3.

            IF W-ESCAPE-KEY = 4
               GO TO GET-010.
            IF WS-RANGE3 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
            IF WS-RANGE3 NOT = "Y" AND NOT = "N"
                     AND NOT = "A" AND NOT = "P" AND NOT = "R"
               MOVE 1810 TO POS
               DISPLAY "ENTER ONLY Y, N, A P, OR R." AT POS
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-030.
            MOVE 1810 TO POS.
            DISPLAY "                               " AT POS.
            MOVE 2820 TO POS.
            DISPLAY "The Report Is Being Run........" AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            IF WS-RANGE3 = "Y"
               MOVE "ALL ORDERS"             TO H1-TYPE.
            IF WS-RANGE3 = "N"
               MOVE "B/ORDERS ONLY"          TO H1-TYPE.
            IF WS-RANGE3 = "A"
               MOVE "ALL B/ORDERS & PENDING" TO H1-TYPE.
            IF WS-RANGE3 = "P"
               MOVE "B/ORDERS READY TO INV"  TO H1-TYPE.
            IF WS-RANGE1 = "    "
               MOVE "/"       TO WS-RANGE1.
            MOVE " "          TO STTR-ST-COMPLETE.
            MOVE WS-RANGE1    TO STTR-STOCK-NUMBER.
            MOVE 0            TO STTR-ST-DATE.
            START STOCK-TRANS-FILE KEY NOT < STTR-ST-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE "STTRANS FILE BUSY ON START, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO PRR-999.
       PRR-002.
            READ STOCK-TRANS-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-STTRANS-ST1 = 10
               GO TO PRR-999.
            IF WS-STTRANS-ST1 = 23
             IF WS-RANGE3 = "R"
           MOVE "STTRANS BUSY ON READ-NEXT-23, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PRR-002.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE "STTRANS FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE "STTRANS STTR-KEY SHOWN NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE STTR-ST-KEY TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO PRR-002.
               
            IF WS-RANGE3 NOT = "R"
             IF STTR-ST-COMPLETE = "L" OR = "Y"
               GO TO PRR-002.
            IF STTR-STOCK-NUMBER < WS-RANGE1
               GO TO PRR-002.
            IF STTR-STOCK-NUMBER > WS-RANGE2
               GO TO PRR-999.

            MOVE 2610 TO POS
            DISPLAY "STOCK NUMBER BEING READ:" AT POS
            ADD 25 TO POS
            DISPLAY STTR-STOCK-NUMBER AT POS.

            IF WS-RANGE3 = "R"
               GO TO PRR-980.
               
            IF STTR-TYPE NOT = 4 AND NOT = 7
               GO TO PRR-002.
            MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
               GO TO PRR-002.

            IF WS-RANGE3 = "Y"
               GO TO PRR-005.
            IF WS-RANGE3 = "N" OR = "A"
             IF STTR-COMPLETE = "N" OR = "B" OR = "C" OR = "D" OR = " "
               GO TO PRR-005.
            IF WS-RANGE3 = "N"
               COMPUTE WS-QUANTITY = STTR-ORDERQTY -
                      (STTR-SHIPQTY + STTR-SHIPPEDQTY)
             IF WS-QUANTITY > 0
               GO TO PRR-005.

            IF WS-RANGE3 = "P"
             IF STTR-SHIPQTY > 0
               GO TO PRR-005.
            GO TO PRR-002.
       PRR-005.
            PERFORM ERROR1-020.
            IF STTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
               GO TO PRR-006.
            MOVE STTR-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
            START DEBTOR-MASTER KEY NOT < DR-KEY.
            READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE 0 TO DR-ACCOUNT-NUMBER
               MOVE "** DEBTOR UNKNOWN **" TO DR-NAME
               GO TO PRR-006.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PRR-005.
       PRR-006.
            IF SP-1STCHAR = "/"
               GO TO PRR-007.
            IF STTR-STOCK-NUMBER = ST-STOCKNUMBER
               GO TO PRR-007.
            MOVE STTR-STOCK-NUMBER TO ST-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY.
            READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE. 
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-STOCK-ST1
               MOVE "INVALID STOCK FOR BO" TO ST-DESCRIPTION1
               GO TO PRR-007.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ PRR-006, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRR-006.
       PRR-007.
            IF STTR-REFERENCE1 = INCR-INVOICE
             IF STTR-TYPE = INCR-TRANS
               GO TO PRR-009.
            MOVE STTR-REFERENCE1 TO INCR-INVOICE
            MOVE STTR-TYPE       TO INCR-TRANS.
            START INCR-REGISTER KEY NOT < INCR-KEY.
            READ INCR-REGISTER
               INVALID KEY NEXT SENTENCE. 
            IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-INCR-ST1
               MOVE "*" TO D-ERROR
               GO TO PRR-009.
            IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ PRR-007, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO PRR-007.
            MOVE " " TO D-ERROR.
       PRR-009.
            IF WS-CAT = "   "
               MOVE ST-CATEGORY TO WS-CAT.
            IF ST-CATEGORY NOT = WS-CAT
               PERFORM SUB-TOTAL-LINE.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-020.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE
            IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            MOVE " "      TO PRINT-REC
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD4
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD5
            MOVE " " TO PRINT-REC
            MOVE 6 TO LINE-CNT.
       PRR-020.
           MOVE STTR-STOCK-NUMBER       TO D-STOCKNO
           MOVE STTR-DESC1              TO D-DESC
           MOVE STTR-ACCOUNT-NUMBER     TO D-ACCNO
           MOVE DR-NAME                 TO D-ACCNAME
           MOVE STTR-REFERENCE1         TO D-PONO.
           IF D-ERROR = "*"
              MOVE STTR-DATE            TO SPLIT-DATE
           ELSE
              MOVE INCR-DATE            TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-DATE
           MOVE STTR-ORDERQTY           TO D-ORDER
           MOVE STTR-SHIPQTY            TO D-SHIP
           MOVE STTR-SHIPPEDQTY         TO D-SHIPPED
           MOVE STTR-PRICE              TO D-PRICE.
           IF DR-SUPPLY-Y-N = "N"
            IF DR-ACCOUNT-NUMBER NOT = 7777777
              MOVE "NO      "           TO D-SUPPLY
            ELSE
              MOVE "YES     "           TO D-SUPPLY.
           IF DR-SUPPLY-Y-N = "Y"
              MOVE "YES     "           TO D-SUPPLY.
           IF DR-SUPPLY-Y-N = "S"
              MOVE "SUSPEND "           TO D-SUPPLY.
           IF WS-RANGE3 NOT = "P"
              COMPUTE WS-QUANTITY = STTR-ORDERQTY -
                        (STTR-SHIPQTY + STTR-SHIPPEDQTY)
           ELSE
              MOVE STTR-SHIPQTY TO WS-QUANTITY.
           COMPUTE WS-VALUE ROUNDED = WS-QUANTITY * STTR-PRICE.
           MOVE WS-VALUE TO D-VALUE.
           ADD WS-VALUE  TO WS-TOTAL-VALUE
                            WS-RUNNING-TOTAL.
           IF DR-SUPPLY-Y-N NOT = "Y"
            IF DR-ACCOUNT-NUMBER NOT = 7777777
               ADD WS-VALUE TO WS-DO-NOT-SUPPLY-TOT.
           IF DR-ACCOUNT-NUMBER = 7777777
               ADD WS-VALUE TO WS-KIT-BO-TOT.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRR-002.
           
       PRR-980.
             IF STTR-ORDERQTY = " "
               MOVE 0 TO STTR-ORDERQTY.
             IF STTR-SHIPQTY = " "
               MOVE 0 TO STTR-SHIPQTY.
             IF STTR-SHIPPEDQTY = " "
               MOVE 0 TO STTR-SHIPPEDQTY.
               
           MOVE STTR-DATE               TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-DATE
           DISPLAY D-DATE AT 2735.
               
           REWRITE STOCK-TRANS-REC
                INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
                MOVE "ST-TRANS BUSY ON REWRITE FOR 'R', 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PRR-980.
           
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       PRINT-RESET SECTION.
       PREST-000.
            MOVE 1  TO STTR-TYPE
                       STTR-REFERENCE1
                       STTR-TRANSACTION-NUMBER.
            START STOCK-TRANS-FILE KEY NOT < STTR-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE "STTRANS RESET FILE BUSY ON START, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PREST-999.
       PREST-002.
            READ STOCK-TRANS-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-STTRANS-ST1 = 10
               GO TO PREST-999.
            IF WS-STTRANS-ST1 = 23
                GO TO PREST-980.
            IF WS-STTRANS-ST1 NOT = 0
              MOVE "STTRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PREST-002.

            MOVE 2610 TO POS
            DISPLAY "STTR NUMBER BEING READ:" AT POS
            ADD 25 TO POS
            DISPLAY STTR-KEY AT POS.
            
      *      GO TO PREST-985.
       PREST-980.
            IF STTR-ORDERQTY = " "
              MOVE 0 TO STTR-ORDERQTY.
            IF STTR-SHIPQTY = " "
              MOVE 0 TO STTR-SHIPQTY.
            IF STTR-SHIPPEDQTY = " "
              MOVE 0 TO STTR-SHIPPEDQTY.

            MOVE STTR-COMPLETE TO STTR-ST-COMPLETE
                                  STTR-AC-COMPLETE.
            MOVE STTR-DATE     TO STTR-ST-DATE
                                  STTR-AC-DATE.
               
            MOVE STTR-DATE               TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE            TO D-DATE
            DISPLAY D-DATE AT 2735.
        PREST-985.
            REWRITE STOCK-TRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 NOT = 0
                MOVE "ST-TRANS BUSY ON REWRITE FOR 'R', 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE WS-STTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO PREST-980.
           
            GO TO PREST-002.
       PREST-999.
           EXIT.
      *
       SUB-TOTAL-LINE SECTION.
       STL-005.
            IF LINE-CNT > 57
               MOVE 66 TO LINE-CNT
               PERFORM PRR-010.
           MOVE WS-TOTAL-VALUE TO TOT-VALUE
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
           MOVE 0 TO WS-TOTAL-VALUE
           MOVE WS-RUNNING-TOTAL TO RUN-VALUE
           WRITE PRINT-REC FROM RUNNING-LINE AFTER 1
           MOVE WS-DO-NOT-SUPPLY-TOT TO DO-NOT-VALUE
           WRITE PRINT-REC FROM DO-NOT-LINE AFTER 1
           MOVE WS-KIT-BO-TOT TO KIT-TOT-VALUE
           WRITE PRINT-REC FROM KIT-TOT-LINE AFTER 1
           MOVE ST-CATEGORY TO WS-CAT
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           ADD 6 TO LINE-CNT.
       STL-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O DEBTOR-MASTER
           IF WS-DEBTOR-ST1 NOT = 0 
              MOVE 0 TO WS-DEBTOR-ST1
              MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-002.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0 
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-002.
       OPEN-003.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-003.
       OPEN-004.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0 
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-004.
        OPEN-010.
           MOVE Ws-Co-Name TO CO-NAME.
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
            IF LINE-CNT > 56
               MOVE 66 TO LINE-CNT
               PERFORM PRR-010.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE WS-TOTAL-VALUE   TO TOT-VALUE
            WRITE PRINT-REC FROM TOTAL-LINE AFTER 1

            MOVE WS-RUNNING-TOTAL TO RUN-VALUE
            WRITE PRINT-REC FROM RUNNING-LINE AFTER 1

            MOVE WS-DO-NOT-SUPPLY-TOT TO DO-NOT-VALUE
            WRITE PRINT-REC FROM DO-NOT-LINE AFTER 1

            MOVE WS-KIT-BO-TOT TO KIT-TOT-VALUE
            WRITE PRINT-REC FROM KIT-TOT-LINE AFTER 1

            COMPUTE WS-RUNNING-TOTAL = WS-RUNNING-TOTAL - 
               (WS-DO-NOT-SUPPLY-TOT + WS-KIT-BO-TOT)
            MOVE WS-RUNNING-TOTAL  TO RUN-VALUE
            MOVE "NETT TOTAL B/O:" TO RUN-DESC
            WRITE PRINT-REC FROM RUNNING-LINE AFTER 1.
       
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
           
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-500.
            CLOSE DEBTOR-MASTER
                  STOCK-MASTER
                  STOCK-TRANS-FILE
                  INCR-REGISTER.
       END-900.
            EXIT PROGRAM.
      *      STOP RUN.
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
