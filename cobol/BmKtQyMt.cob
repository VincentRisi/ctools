        IDENTIFICATION DIVISION.
        PROGRAM-ID. BmKtQyMt.
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
       77  WS-FACTOR            PIC 9(2)V99 VALUE 0.
       77  WS-FACTOR-DIS        PIC ZZ.99.
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(5) VALUE " ".
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
           PERFORM CLEAR-SCREEN.
           MOVE 320 TO POS.
           DISPLAY "** KIT QTY ADJUSTMENT BY A FACTOR PROGRAM **"
            AT POS.
           MOVE 420 TO POS.
           DISPLAY "********************************************"
            AT POS.
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM READ-TOOLKIT.
           GO TO CONTROL-005.
      *
       GET-DATA SECTION.
       GET-010.
            MOVE 610 TO POS.
            DISPLAY
            "STOCK ITEM on which QTY is to be CHANGED:[               ]"
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
              "THIS NUMBER ENTERED IS NOT A CURRENT STOCK NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-010.
        GET-020.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
            MOVE 910 TO POS.
            DISPLAY "NB! FOR A 20% INCREASE THE FACTOR SHOULD BE 1.2 ,"
               AT POS
            MOVE 1033 TO POS
            DISPLAY "POSSIBLE FORMAT IS 99.99" AT POS
            MOVE 810 TO POS.
            DISPLAY "Enter FACTOR by which QTY is to CHANGE  :[     ]"
                AT POS
            MOVE 852 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 5         TO CDA-DATALEN.
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
            IF NUMERIC-RATE = 0
               MOVE "THE FACTOR ENTERED IS NOT > 0, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-020.
            MOVE NUMERIC-RATE TO WS-FACTOR WS-FACTOR-DIS.
            DISPLAY WS-FACTOR-DIS AT POS.
        GET-030.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
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
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "NO KITS WITH THIS ITEM, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RT-999.
       RT-010.
           READ TOOLKITS NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               MOVE "TOOLKIT FILE AT END." TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO RT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLKIT RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
              PERFORM WRITE-TOOLKIT.
           GO TO RT-010.
       RT-999.
           EXIT.
      *
       WRITE-TOOLKIT SECTION.
       WRT-000.
           MOVE 0 TO WS-QTY.
           COMPUTE WS-QTY ROUNDED = TO-QUANTITY * WS-FACTOR.
           IF WS-QTY > 999
              MOVE 999 TO WS-QTY.
           MOVE WS-QTY TO TO-QUANTITY.
       WRT-010.
           REWRITE TOOL-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              GO TO WRT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE 0 TO WS-TOOLKIT-ST1
              MOVE "TOOLKIT RECORD NOT REWRITTEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRT-010.
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
