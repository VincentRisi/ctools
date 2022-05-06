        IDENTIFICATION DIVISION.
        PROGRAM-ID. StStQyMt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
      *
       WORKING-STORAGE SECTION.
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(15) VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
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
           MOVE 321 TO POS
           DISPLAY "** STOCK-TAKE FIELD UPDATE PROGRAM **" AT POS
           MOVE 421 TO POS
           DISPLAY "*************************************" AT POS.
       CONTROL-010.
           MOVE 1210 TO POS
           DISPLAY "FROM STOCK NUMBER : [               ]" AT POS
           MOVE 1231 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1

           IF W-ESCAPE-KEY = 3
               GO TO END-900.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-012
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-012.
           MOVE 1310 TO POS.
           DISPLAY "  TO STOCK NUMBER : [               ]" AT POS.
           MOVE 1331 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           IF WS-ANSWER2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-013
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
       CONTROL-013.
           MOVE 1510 TO POS
           DISPLAY "ADD YTD-SALES FIGURES TO QTY AT ST-TAKE: [ ]" AT POS
           ADD 42 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER3.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-012.
           IF WS-ANSWER3 NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-013.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-013.
       CONTROL-015.
           MOVE 2510 TO POS
           DISPLAY "Run in progress, Please Be Patient....." AT POS.
           PERFORM OPEN-FILES.
           PERFORM UPDATE-ROUTINE.
           PERFORM END-OFF.
      *
       UPDATE-ROUTINE SECTION.
       UDR-000.
           MOVE WS-ANSWER1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       UDR-005.
           READ STOCK-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               GO TO UDR-999.
           IF WS-STOCK-ST1 NOT = 0
             MOVE "STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO UDR-005.
           IF ST-STOCKNUMBER < WS-ANSWER1
              GO TO UDR-005.
           IF ST-STOCKNUMBER > WS-ANSWER2
              GO TO UDR-999.
           MOVE 2610 TO POS.
           DISPLAY "STOCK NUMBER BEING PROCESSED:" AT POS.
           ADD 30 TO POS.
           DISPLAY ST-STOCKNUMBER AT POS.
       UDR-020.
           COMPUTE ST-QTY-ST-TAKE = ST-QTYONHAND + ST-QTYONRESERVE.
           IF WS-ANSWER3 = "Y"
               ADD ST-SALESUNITSYTD TO ST-QTY-ST-TAKE.
       UDR-025.
           REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK-RECORD ERROR ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
                PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO UDR-025.
           GO TO UDR-005.
       UDR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-055.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-055.
       OPEN-999.
           EXIT.
      *   
       END-OFF SECTION.
       END-500.
           CLOSE STOCK-MASTER.
       END-900.
      *     STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *      
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
