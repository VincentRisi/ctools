        IDENTIFICATION DIVISION.
        PROGRAM-ID. StTrDuMt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStChanges".
          Copy "SelectSlParameter".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStockChanges.
           COPY ChlfdParam.
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-BEG-STOCK         PIC X(15) VALUE " ".
       77  WS-END-STOCK         PIC X(15) VALUE " ".
       77  WS-TARIFF            PIC 9(8) VALUE 0.
       77  WS-TAR-ACCEPT        PIC X(8) VALUE " ".
       77  WS-TAR-DIS           PIC Z(7)9.
       77  WS-DUTY-ACCEPT       PIC X(4) VALUE " ".
       77  WS-DUTY              PIC 99V9 VALUE 0.
       77  WS-DUTY-DIS          PIC Z9.9.
       77  WS-CHANGE-FILE       PIC X VALUE " ".
       77  NEW-STOCKNO          PIC X VALUE " ".      
       77  WS-MATCH-FIELD       PIC X(20) VALUE " ".
       77  WS-MATCH             PIC 9(8)V999 VALUE 0.
       77  WS-TARIFF-MUST-MATCH   PIC X VALUE " ".
       77  WS-WHAT-TYPE           PIC X VALUE " ".
       77  WS-NUMERIC-DISPLAY     PIC Z(7)9.999.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1          PIC 99.
       01  WS-STCHANGE-STATUS.
           03  WS-STCHANGE-ST1    PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 310 TO POS
           DISPLAY "** STOCK DUTY % CHANGE BASED ON TARIFF PROGRAM **"
            AT POS.
       CONTROL-005.
           MOVE 534 TO POS
           DISPLAY "FROM STOCK NUMBER      : [               ]" AT POS
           MOVE 734 TO POS
           DISPLAY "  TO STOCK NUMBER      : [               ]" AT POS
           MOVE 934 TO POS
           DISPLAY "ENTER TARIFF# TO CHANGE: [        ]" AT POS
           MOVE 1134 TO POS
           DISPLAY "ENTER THE NEW DUTY%    : [    ]" AT POS.

           MOVE " " TO WS-BEG-STOCK WS-END-STOCK WS-DUTY-ACCEPT
           MOVE 0   TO WS-DUTY WS-TARIFF.
       CONTROL-010.
           MOVE 2520 TO POS
           DISPLAY "                                      " AT POS.
           PERFORM OPEN-FILES.
       CONTROL-015.
           PERFORM GET-DATA.
           PERFORM PRINT-ROUTINE.
           MOVE 2519 TO POS
           DISPLAY "                                           " AT POS
           GO TO CONTROL-015.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE 534 TO POS
           DISPLAY "FROM STOCK NUMBER      : [               ]" AT POS
           MOVE 560 TO POS
           MOVE WS-BEG-STOCK TO CDA-DATA.
           
           MOVE 15        TO CDA-DATALEN.
           MOVE 2         TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-BEG-STOCK.

           IF W-ESCAPE-KEY = 3
              PERFORM END-OFF.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-010
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-000.
       GET-010.
           MOVE 734 TO POS
           DISPLAY "  TO STOCK NUMBER      : [               ]" AT POS
           MOVE 760 TO POS
           MOVE WS-END-STOCK TO CDA-DATA.

           MOVE 15        TO CDA-DATALEN.
           MOVE 4         TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-END-STOCK.

           IF W-ESCAPE-KEY = 7
              MOVE WS-BEG-STOCK TO WS-END-STOCK
              DISPLAY WS-END-STOCK AT POS
              GO TO GET-015.
           IF W-ESCAPE-KEY = 4
              GO TO GET-000.
           IF WS-END-STOCK = " "
              GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-015
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-010.
       GET-015.
           MOVE 934 TO POS
           DISPLAY "ENTER TARIFF# TO CHANGE: [        ]" AT POS
           MOVE 960 TO POS
           MOVE WS-TAR-ACCEPT TO CDA-DATA

           MOVE 8         TO CDA-DATALEN.
           MOVE 6         TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TAR-ACCEPT.

           IF WS-TAR-ACCEPT = " "
              GO TO GET-015.
           MOVE WS-TAR-ACCEPT TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-TARIFF
           MOVE WS-TARIFF TO WS-TAR-DIS
           MOVE 960 TO POS
           DISPLAY WS-TAR-DIS AT POS.

           IF W-ESCAPE-KEY = 4
              GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-020
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-015.
       GET-020.
           MOVE 1134 TO POS
           DISPLAY "ENTER THE NEW DUTY%    : [    ]" AT POS.
           MOVE 1160 TO POS


           MOVE ' '       TO CDA-DATA.
           MOVE 4         TO CDA-DATALEN.
           MOVE 8         TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DUTY-ACCEPT.

           MOVE WS-DUTY-ACCEPT TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-DUTY
           MOVE WS-DUTY TO WS-DUTY-DIS
           MOVE 1160 TO POS
           DISPLAY WS-DUTY-DIS AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-500
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-020.
       GET-500.
           MOVE "N" TO WS-CHANGE-FILE
           MOVE 1334 TO POS
           DISPLAY "UPDATE CHANGE FILE     : [ ]" AT POS
           MOVE 1360 TO POS
           MOVE WS-CHANGE-FILE TO CDA-DATA

           MOVE 1         TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CHANGE-FILE.

           IF WS-CHANGE-FILE NOT = "N" AND NOT = "Y"
               MOVE "THIS FIELD MUST BE 'N' OR 'Y' RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-500.
           IF W-ESCAPE-KEY = 4
              GO TO GET-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-600
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-500.
       GET-600.
            MOVE " " TO WS-MESSAGE
            MOVE 1420 TO POS
            DISPLAY WS-MESSAGE AT POS
            PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE WS-BEG-STOCK TO ST-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
       PRR-002.
            READ STOCK-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-STOCK-ST1 = 10
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRR-999.
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
               GO TO PRR-002.
            IF ST-STOCKNUMBER < WS-BEG-STOCK
               GO TO PRR-002.
            IF ST-STOCKNUMBER > WS-END-STOCK
               MOVE ST-STOCKNUMBER TO WS-BEG-STOCK
               GO TO PRR-999.
               
            MOVE 2525 TO POS
            DISPLAY "StockNumber Being Changed:" AT POS
            ADD 27 TO POS
            DISPLAY ST-STOCKNUMBER AT POS.

            IF ST-DUTYTARIFF = WS-TARIFF
                MOVE WS-DUTY TO ST-DUTYPERCENT
            ELSE
                GO TO PRR-002.
            
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       PRR-050.
            REWRITE STOCK-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON RE-WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRR-050.
           PERFORM ERROR-020.
       PRR-900.
           IF INVQUES-STOCK-CHANGE = "Y"
            IF WS-CHANGE-FILE = "Y"
               PERFORM WRITE-STOCK-CHANGES.
           GO TO PRR-002.
       PRR-999.
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
       OPEN-005.
            OPEN I-O STOCKCHANGE-MASTER.
            IF WS-STCHANGE-ST1 NOT = 0
               MOVE 0 TO WS-STCHANGE-ST1
               MOVE "STCHANGE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-014.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
           PERFORM READ-INVQUES-FILE.
           CLOSE PARAMETER-FILE.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER
                  STOCKCHANGE-MASTER.
       END-900.
            EXIT PROGRAM.
      *     STOP RUN.
       END-999.
            EXIT.
      *      
       Copy "WriteStockChanges".
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
