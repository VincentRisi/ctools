        IDENTIFICATION DIVISION.
        PROGRAM-ID. BmKtChMt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
       Copy "SelectStMaster".
       Copy "SelectBmMaster".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdToolkit.
           COPY ChlfdStock.
      *
       WORKING-STORAGE SECTION.
       77  WS-VALID             PIC X VALUE " ".
       77  WS-TOOLKITNUMBER     PIC X(15) VALUE " ".
       77  WS-QTY               PIC 999 VALUE 0.
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(15) VALUE " ".
       77  WS-ANSWER3           PIC X(15) VALUE " ".
       77  WS-ANSWER4           PIC X(15) VALUE " ".
       01  WS-TOOLKIT-STATUS.
           03  WS-TOOLKIT-ST1   PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTR-000.
           PERFORM OPEN-FILES.
       CONTROL-005.
           PERFORM CLEAR-SCREEN
           MOVE 320 TO POS
           DISPLAY "** KIT ITEM REPLACEMENT PROGRAM **" AT POS
           MOVE 420 TO POS
           DISPLAY "**********************************" AT POS.
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM READ-TOOLKIT.
           GO TO CONTROL-005.
      *
       GET-DATA SECTION.
       GET-010.
            MOVE 610 TO POS.
            DISPLAY
            "Enter the OLD STOCKNUMBER to be CHANGED :[               ]"
               AT POS.
            MOVE 652 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 3         TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

            IF W-ESCAPE-KEY = 3
               PERFORM END-OFF.
            IF WS-ANSWER1 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
            MOVE WS-ANSWER1 TO ST-STOCKNUMBER.
            PERFORM READ-STOCK.
            IF WS-VALID = "N"
               MOVE
              "THIS OLD NUMBER ENTERED IS NOT A CURRENT STOCK NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-000.
        GET-020.
            MOVE 810 TO POS.
            DISPLAY
            "Enter the NEW STOCKNUMBER to SUSTITUTE  :[               ]"
               AT POS.
            MOVE 852 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

            IF W-ESCAPE-KEY = 4
               GO TO GET-010.
            IF WS-ANSWER2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-025
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
        GET-025.
            MOVE WS-ANSWER2 TO ST-STOCKNUMBER.
            PERFORM READ-STOCK.
            IF WS-VALID = "N"
               MOVE "ENTER A VALID NEW STOCK NUMBER TO BE CHANGED"
               TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE "               " TO WS-ANSWER2
               GO TO GET-020.
            PERFORM ERROR-020.
        GET-030.
            MOVE 1810 TO POS.
            DISPLAY " " AT POS.
        GET-040.
            MOVE 1210 TO POS
            DISPLAY
            "                FROM BILL OF MATERIAL NO:[               ]"
               AT POS
            MOVE 1252 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER3.

            IF W-ESCAPE-KEY = 4
               GO TO GET-020.
            IF WS-ANSWER3 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-050
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
        GET-050.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
            MOVE 1410 TO POS
            DISPLAY
            "                  TO BILL OF MATERIAL NO:[               ]"
               AT POS
            MOVE 1452 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER4.

            IF W-ESCAPE-KEY = 4
               GO TO GET-040.
            IF WS-ANSWER4 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-999
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
        GET-999.
            EXIT.
      *
       READ-STOCK SECTION.
       RS-000.
           MOVE "N" TO WS-VALID.
       RS-010. 
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-STOCK-ST1
               GO TO RS-999.
           IF WS-STOCK-ST1 = 0
               MOVE "Y" TO WS-VALID.
       RS-999.
           EXIT.
      *
       READ-TOOLKIT SECTION.
       RT-000.
           MOVE " " TO TO-TOOLKIT-NUMBER
                       TO-COMPONENT-NUMBER.
           MOVE WS-ANSWER1 TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-COMPONENT-NUMBER
               INVALID KEY NEXT SENTENCE.
       RT-010.
           READ TOOLKITS NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               MOVE "TOOLKIT FILE AT END." TO WS-MESSAGE
               PERFORM ERROR-010
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO RT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLKIT RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RT-010.
               
           IF TO-TOOLKIT-NUMBER < WS-ANSWER3
               GO TO RT-010.
           IF TO-TOOLKIT-NUMBER > WS-ANSWER4
               GO TO RT-010.
           IF TO-COMPONENT-NUMBER NOT = WS-ANSWER1
               GO TO RT-999.
       RT-015.
           IF TO-TOOLKIT-NUMBER NOT = WS-TOOLKITNUMBER
               MOVE TO-TOOLKIT-NUMBER TO WS-TOOLKITNUMBER
               MOVE 2410 TO POS
               DISPLAY "TOOLKIT CURRENTLY BEING CHANGED =" AT POS
               MOVE 2445 TO POS
               DISPLAY WS-TOOLKITNUMBER AT POS.
       RT-020.
           IF TO-COMPONENT-NUMBER = WS-ANSWER1
              MOVE TO-TOOLKIT-NUMBER   TO WS-TOOLKITNUMBER
              MOVE TO-QUANTITY         TO WS-QTY
              PERFORM DELETE-TOOLKIT
              PERFORM WRITE-TOOLKIT.
           MOVE WS-ANSWER1 TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-COMPONENT-NUMBER
               INVALID KEY NEXT SENTENCE.
           GO TO RT-010.
       RT-999.
           EXIT.
      *
       WRITE-TOOLKIT SECTION.
       WRT-000.
           MOVE WS-TOOLKITNUMBER TO TO-TOOLKIT-NUMBER.
           MOVE WS-ANSWER2       TO TO-COMPONENT-NUMBER.
           MOVE WS-QTY           TO TO-QUANTITY.
           START TOOLKITS KEY NOT < TO-KEY
               INVALID KEY NEXT SENTENCE.
       WRT-002.
           READ TOOLKITS WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               GO TO WRT-010.
           ADD WS-QTY TO TO-QUANTITY.
       WRT-005.
           REWRITE TOOL-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              MOVE "TOOLKIT NOT REWRITTEN, GOING TO RETRY IN 2 SECONDS."
              TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020   
              GO TO WRT-010.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE 0 TO WS-TOOLKIT-ST1
              MOVE "TOOLKIT RECORD NOT REWRITTEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRT-005.
           GO TO WRT-999.
       WRT-010.
           WRITE TOOL-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              MOVE "TOOLKIT NOT WRITTEN, GOING TO RETRY IN 2 SECONDS."
              TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020   
              GO TO WRT-005.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE 0 TO WS-TOOLKIT-ST1
              MOVE "TOOLKIT RECORD NOT WRITTEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRT-010.
       WRT-999.
           EXIT.
      *
       DELETE-TOOLKIT SECTION.
       DT-050.
           DELETE TOOLKITS
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               GO TO DT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLKIT BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DT-050.
       DT-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
            OPEN I-O TOOLKITS.
            IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOL FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-008.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCKMASTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-008.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE TOOLKITS
                  STOCK-MASTER.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
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
