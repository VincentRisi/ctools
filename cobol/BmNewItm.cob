        IDENTIFICATION DIVISION.
        PROGRAM-ID. BmNewItm.
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
       77  WS-QTY               PIC 9(4) VALUE 0.
       77  WS-FACTOR            PIC 9(3)V99 VALUE 0.
       77  WS-FACTOR-DIS        PIC ZZ9.99.
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(5) VALUE " ".
       77  WS-ANSWER3           PIC X(15) VALUE " ".
       77  WS-ANSWER4           PIC X(15) VALUE " ".
       77  WS-ANSWER5           PIC X VALUE " ".
       77  WS-ANSWER6           PIC X(15) VALUE " ".
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
           DISPLAY "*PROGRAM FOR ADDING A NEW ITEM TO A RANGE OF BILLS*"
            AT POS
           MOVE 420 TO POS
           DISPLAY "***************************************************"
            AT POS.
       CONTROL-010.
            PERFORM GET-DATA.
            PERFORM READ-TOOLKIT.
            GO TO CONTROL-005.
      *
       GET-DATA SECTION.
       GET-010.
            MOVE 610 TO POS
            DISPLAY
            "STOCK ITEM TO ADD TO BILL OF MATERIAL   :[               ]"
               AT POS
            MOVE 652 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 3         TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

      *      ACCEPT WS-ANSWER1 AT POS.
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
              "THIS NUMBER ENTERED IS NOT A CURRENT STOCK NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-010.
        GET-020.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
            MOVE 910 TO POS.
            DISPLAY "ENTER AN AMOUNT OF UNITS FOR THIS ITEM TO BE ADDED"
               AT POS
            MOVE 1033 TO POS
            DISPLAY "POSSIBLE FORMAT IS 999" AT POS
            MOVE 810 TO POS.
            DISPLAY "Enter FACTOR by which QTY is to CHANGE  :[      ]"
                AT POS
            MOVE 852 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 6         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

      *      ACCEPT WS-ANSWER2 AT POS.
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
            MOVE WS-ANSWER2 TO ALPHA-RATE
            PERFORM DECIMALISE-RATE.
            IF NUMERIC-RATE NOT > 0
               MOVE "THE FACTOR ENTERED IS NOT > 0, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-020.
            MOVE NUMERIC-RATE TO WS-FACTOR WS-FACTOR-DIS.
            DISPLAY WS-FACTOR-DIS AT POS.
        GET-030.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
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

      *      ACCEPT WS-ANSWER3 AT POS.
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

      *      ACCEPT WS-ANSWER4 AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO GET-040.
            IF WS-ANSWER4 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-060
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
        GET-060.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
            MOVE 1610 TO POS
            DISPLAY "SHOULD THE FACTOR DEPEND ON A STOCK ITEM:[ ]"
                AT POS
            MOVE 1652 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER5.

      *      ACCEPT WS-ANSWER5 AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO GET-050.
            IF WS-ANSWER5 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-060.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-070
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-060.
        GET-070.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
            IF WS-ANSWER5 = "N"
                 GO TO GET-999.
            MOVE 1816 TO POS
            DISPLAY
               "ENTER COMPONENT FACTOR DEPENDS ON :[               ]"
                   AT POS
            MOVE 1852 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER6.

      *      ACCEPT WS-ANSWER6 AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO GET-060.
            IF WS-ANSWER6 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-060.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-080
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-070.
        GET-080.
            MOVE WS-ANSWER6 TO ST-STOCKNUMBER.
            PERFORM READ-STOCK.
            IF WS-VALID = "N"
               MOVE
              "THIS NUMBER ENTERED IS NOT A CURRENT STOCK NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-070.
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
           MOVE WS-ANSWER3 TO TO-TOOLKIT-NUMBER
           MOVE " "        TO TO-COMPONENT-NUMBER.
       RT-005.
           START TOOLKITS KEY NOT < TO-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "BAD START ON READ-NEXT, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RT-999.
       RT-010.
           READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               MOVE "TOOLKIT FILE AT END." TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO RT-999.
           IF WS-TOOLKIT-ST1 = 91
               START TOOLKITS KEY NOT < TO-KEY
                   INVALID KEY NEXT SENTENCE
               GO TO RT-010.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKITS BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO RT-010.
           IF TO-TOOLKIT-NUMBER < WS-ANSWER3
               GO TO RT-010.
           IF TO-TOOLKIT-NUMBER > WS-ANSWER4
               GO TO RT-999.
           IF TO-COMPONENT-NUMBER NOT = SPACES
               GO TO RT-010.
           IF TO-TOOLKIT-NUMBER = WS-TOOLKITNUMBER
               GO TO RT-010.
       RT-015.
           IF TO-TOOLKIT-NUMBER NOT = WS-TOOLKITNUMBER
               MOVE TO-TOOLKIT-NUMBER TO WS-TOOLKITNUMBER
               MOVE 2410 TO POS
               DISPLAY "TOOLKIT CURRENTLY BEING CHANGED =" AT POS
               MOVE 2445 TO POS
               DISPLAY WS-TOOLKITNUMBER AT POS.
       RT-020.
           IF WS-ANSWER5 = "N"
               MOVE WS-QTY TO TO-QUANTITY
               GO TO RT-040
           ELSE
               PERFORM READ-SPECIAL-ITEM.
           IF WS-TOOLKIT-ST1 = 88
               GO TO RT-010.
           MOVE TO-TOOLKIT-NUMBER   TO WS-TOOLKITNUMBER
           MOVE WS-ANSWER1          TO TO-COMPONENT-NUMBER.
           COMPUTE TO-QUANTITY = WS-FACTOR * WS-QTY.
       RT-040.
           PERFORM WRITE-TOOLKIT
           GO TO RT-010.
       RT-999.
           EXIT.
      *
       READ-SPECIAL-ITEM SECTION.
       RSI-000.
           MOVE WS-TOOLKITNUMBER TO TO-TOOLKIT-NUMBER
           MOVE WS-ANSWER6       TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 NOT = 0
               GO TO RSI-999.
       RSI-010.
           READ TOOLKITS
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               MOVE 88 TO WS-TOOLKIT-ST1
               MOVE 1 TO WS-QTY
               GO TO RSI-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLKIT RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSI-010.
           MOVE TO-QUANTITY TO WS-QTY.
       RSI-999.
          EXIT.
      *
       WRITE-TOOLKIT SECTION.
       WRT-010.
           WRITE TOOL-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              MOVE "TOOLKIT RECORD NOT WRITTEN ST23, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRT-020.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE 0 TO WS-TOOLKIT-ST1
              MOVE "TOOLKIT RECORD NOT WRITTEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRT-010.
          GO TO WRT-999.
       WRT-020.
           REWRITE TOOL-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-TOOLKIT-ST1
              MOVE "TOOLKIT RECORD NOT RE-WRITTEN ST23, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRT-010.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE 0 TO WS-TOOLKIT-ST1
              MOVE "TOOLKIT RECORD NOT RE-WRITTEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRT-020.
       WRT-999.
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
      *      STOP RUN.
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
