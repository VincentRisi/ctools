        IDENTIFICATION DIVISION.
        PROGRAM-ID. StOrDtMt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStOrders".
          Copy "SelectStMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdOutOrd.
           COPY ChlfdStock.
      *
       WORKING-STORAGE SECTION.
       77  WS-PO-NUM            PIC X(20) VALUE " ".
       77  WS-DATE-ACCEPT       PIC X(10) VALUE " ".
       77  WS-DUE-DATE          PIC 9(8) VALUE 0.
       77  WS-NUM-UPDATED       PIC 9(6) VALUE 0.
       77  WS-CONFIRM           PIC X VALUE " ".
       77  WS-ORDER-UPDATED     PIC X VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1    PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
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
           DISPLAY "** ORDERS ON SUPPLIERS UPDATE OF DUE DATE **"
               AT POS
           MOVE 415 TO POS
           DISPLAY "********************************************"
               AT POS.
       CONTROL-010.
           PERFORM OPEN-FILES.
       CONTROL-015.
           PERFORM GET-DATA.
           PERFORM PRINT-ROUTINE.
           IF WS-ORDER-UPDATED = "N"
              MOVE "NO ITEMS FOUND TO UPDATE ON ABOVE ORDER !!!"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
              
           MOVE 2710 TO POS
           DISPLAY
           "PRESS <RETURN> FOR A NEW P/O TO UPDATE, <END> TO EXIT."
            AT POS
           MOVE 2769 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 24         TO CDA-ROW.
           MOVE 71        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

           IF W-ESCAPE-KEY = 3
              PERFORM END-OFF.
           
           PERFORM CONTROL-000.
           GO TO CONTROL-015.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "                    " TO WS-PO-NUM.
            MOVE 1020 TO POS.
            DISPLAY "ORDER NUMBER TO UPDATE: [                    ]"
                 AT POS.
            MOVE 1045 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 44        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PO-NUM.

            IF W-ESCAPE-KEY = 3
                PERFORM END-OFF.
            IF WS-PO-NUM = "                    "
                GO TO GET-000.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-010
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
       GET-010.
            MOVE " " TO WS-CONFIRM.
            MOVE 1310 TO POS.
            DISPLAY "DO YOU WISH TO CONFIRM RECEIPT OF ORDER :[ ]"
                      AT POS.
            MOVE 1410 TO POS.
            DISPLAY
              "ENTER: N=NO CHANGE, Y=CONFIRM, C=CHANGE TO UN-CONFIRMED"
                      AT POS.
            MOVE 1517 TO POS
            DISPLAY "D=DELETE ENTIRE ORDER" AT POS
            MOVE 1352 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CONFIRM.

            IF W-ESCAPE-KEY = 4
                GO TO GET-000.
            IF WS-CONFIRM NOT = "N" AND NOT = "Y"
                      AND NOT = "C" AND NOT = "D"
                MOVE "PLEASE ENTER C, D, N, OR Y ONLY" TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-020
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
       GET-020.
            MOVE " " TO WS-DATE-ACCEPT.
            MOVE 1710 TO POS
            DISPLAY 
            "LEAVE BLANK IF DATE UN-CHANGED, 'ZERO' IF DATE TO BE ZEROD"
               AT POS
            MOVE 1810 TO POS
            DISPLAY 
            "'DELETE' IF ENTIRE ORDER TO BE DELETED."
               AT POS
            MOVE 1910 TO POS
            DISPLAY "ENTER NEW DUE DATE, DAY/MONTH/YEAR: [          ]"
                      AT POS
            ADD 37 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATE-ACCEPT.

            IF WS-CONFIRM = "D"
             IF WS-DATE-ACCEPT = "DELETE"
              GO TO GET-050
             ELSE
              GO TO GET-020.
            IF WS-DATE-ACCEPT = "ZERO"
               GO TO GET-050.
            IF W-ESCAPE-KEY = 4
                GO TO GET-010.
            IF WS-DATE-ACCEPT = " "
                GO TO GET-050.
            MOVE WS-DATE-ACCEPT TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO GET-020.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            DISPLAY DISPLAY-DATE AT POS.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-DUE-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-050
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
       GET-050.
            MOVE 2510 TO POS.
            DISPLAY "The order is being updated, please be patient."
              AT POS.
       GET-030.
            PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE 0                 TO WS-NUM-UPDATED
            MOVE "N"               TO WS-ORDER-UPDATED
            MOVE WS-PO-NUM         TO OO-ORDER-NUMBER
            MOVE "               " TO OO-STOCK-NUMBER.
            START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
               GO TO PRR-999.
       PRR-002.
            READ OUTSTANDING-ORDERS NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-OUTORD-ST1 = 10
               MOVE 0 TO WS-OUTORD-ST1
               GO TO PRR-999.
            IF WS-OUTORD-ST1 NOT = 0
            MOVE "ST-ORDERS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO PRR-002.
            IF OO-ORDER-NUMBER < WS-PO-NUM
               GO TO PRR-002.
            IF OO-ORDER-NUMBER > WS-PO-NUM
               GO TO PRR-999.
            IF WS-CONFIRM = "D"
             IF WS-DATE-ACCEPT = "DELETE"
               GO TO PRR-015.
            IF OO-QUANTITY = 0
               GO TO PRR-002.
       PRR-010.
           IF WS-DATE-ACCEPT = "ZERO"
               MOVE 0 TO OO-DUEDATE.
           IF WS-DATE-ACCEPT NOT = " " AND NOT = "DELETE"
                         AND NOT = "ZERO"
               MOVE WS-DUE-DATE TO OO-DUEDATE.
           IF WS-CONFIRM = "Y"
               MOVE "Y" TO OO-UPDATED.
           IF WS-CONFIRM = "C"
               MOVE "N" TO OO-UPDATED.
       PRR-011.
           REWRITE OUT-ORDER-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "ST-ORDER FILE BUSY ON RE-WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO PRR-011.
           GO TO PRR-500.
       PRR-015.
           DELETE OUTSTANDING-ORDERS
              INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "ST-ORDER FILE BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO PRR-015.
           PERFORM UPDATE-STOCK.
       PRR-500.
           ADD 1 TO WS-NUM-UPDATED
           MOVE 2610 TO POS
           DISPLAY "Number of Lines Updated :" AT POS
           MOVE WS-NUM-UPDATED TO F-EDNAMEFIELDINV
           ADD 25 TO POS
           DISPLAY F-EDNAMEFIELDINV AT POS.
              
           IF WS-ORDER-UPDATED = "N"
              MOVE "Y" TO WS-ORDER-UPDATED.
           GO TO PRR-002.
       PRR-999.
           UNLOCK OUTSTANDING-ORDERS.
       PRR-9999.
           EXIT.
      *
       UPDATE-STOCK SECTION.
       R-ST-000.
             MOVE OO-STOCK-NUMBER TO ST-STOCKNUMBER.
             START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY NEXT SENTENCE.
       R-ST-010.
             READ STOCK-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE "STOCK ITEM NOT FOUND, 'ESC' TO READ NEXT ITEM."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE OO-STOCK-NUMBER TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO R-ST-999.
             IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE OO-STOCK-NUMBER TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO R-ST-010.
       R-ST-015.
          IF OO-QUANTITY NOT > ST-QTYONORDER
              SUBTRACT OO-QUANTITY FROM ST-QTYONORDER
          ELSE
              MOVE 0                 TO ST-QTYONORDER.
       R-ST-020.
          REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE OO-STOCK-NUMBER TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
              GO TO R-ST-020.
       R-ST-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O OUTSTANDING-ORDERS.
           IF WS-OUTORD-ST1 NOT = 0
              MOVE 0 TO WS-OUTORD-ST1
              MOVE "SORDER FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-005.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE OUTSTANDING-ORDERS
                 STOCK-MASTER.
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
      *
      * END-OF-JOB.
