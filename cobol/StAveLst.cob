        IDENTIFICATION DIVISION.
        PROGRAM-ID. StAveLst.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
         Copy "SelectSlDaily".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  WS-EOF             PIC X(3) VALUE "   ".
       77  WS-ANSWER          PIC X(5).
       77  WS-TEMP            PIC X.
       77  WS-DEL-DELAY       PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1   PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1   PIC 99.
      *
       Copy "WsDateInfo".
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
           PERFORM CLEAR-SCREEN.
           MOVE 0320 TO POS.
           DISPLAY "** STOCK COSTS REWORK PROGRAM ****" AT POS
           MOVE 0420 TO POS.
           DISPLAY "**********************************" AT POS.

           PERFORM A-INIT.
       CONTROL-020.
           MOVE 1615 TO POS.
           DISPLAY "<RETURN> TO CONTINUE, <END> TO EXIT." AT POS.
           MOVE 1665 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 13         TO CDA-ROW.
           MOVE 66        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 3
               PERFORM C-900.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-030.
           MOVE 1615 TO POS.
           DISPLAY "                                          " AT POS.
           MOVE 2610 TO POS.
           DISPLAY "PROCESS BEING RUN........" AT POS.
           
           PERFORM B-MAIN.
           PERFORM C-END.
       CONTROL-000.
           EXIT.
      *
       A-INIT SECTION.
       A-000.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
             MOVE "STOCK BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO A-000.
           
           MOVE " " TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY
                MOVE "NO RECORDS ON FILE TO START PROGRAM."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                STOP RUN.
       A-EXIT.
           EXIT.
      *
        B-MAIN SECTION.
        B-005.
           READ STOCK-MASTER NEXT WITH LOCK
               AT END 
               MOVE 2010 TO POS
               DISPLAY " FILE AT END, FINISHING" AT POS
               GO TO B-EXIT.

           MOVE 1501 TO POS
           DISPLAY "STOCK NUMBER:" AT POS
           ADD 14 TO POS
           DISPLAY ST-KEY AT POS
           MOVE 1535 TO POS
           DISPLAY "                               " AT POS.
           
           IF ST-AVERAGECOST = 0
            IF ST-LASTCOST = 0
               COMPUTE ST-LASTCOST = ST-PRICE * 0.60
               MOVE ST-LASTCOST TO ST-AVERAGECOST
               MOVE 1535 TO POS
               DISPLAY "BOTH COSTS FIXED" AT POS
               GO TO B-010.

           IF ST-AVERAGECOST = 0
               MOVE ST-LASTCOST TO ST-AVERAGECOST
               MOVE 1535 TO POS
               DISPLAY "AVE COST FIXED" AT POS
               GO TO B-010.

           IF ST-LASTCOST = 0
               MOVE ST-AVERAGECOST TO ST-LASTCOST
               MOVE 1535 TO POS
               DISPLAY "LAST COST FIXED" AT POS.
        B-010.
           REWRITE STOCK-RECORD
                 INVALID KEY
                 MOVE "STOCK BUSY ON REWRITE, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-STOCK-ST1
                 GO TO B-010.
           GO TO B-005.
        B-EXIT.
             EXIT.
      *
        C-END SECTION.
        C-000.
           CLOSE STOCK-MASTER.
        C-900.
           EXIT PROGRAM.
        C-EXIT.
              EXIT.
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
