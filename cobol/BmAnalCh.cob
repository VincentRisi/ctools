        IDENTIFICATION DIVISION.
        PROGRAM-ID. BmAnalCh.
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
       77  WS-ANSWER1           PIC X VALUE " ".
       77  WS-ANSWER2           PIC X VALUE " ".
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
           DISPLAY "** STOCK ANALYSIS RE-SET TO 'T' PROGRAM **"
            AT POS
           MOVE 420 TO POS
           DISPLAY "******************************************"
            AT POS.
       CONTROL-010.
           PERFORM GET-DATA.
           IF WS-ANSWER1 = "Y"
              PERFORM RESET-STOCK-ANALYSIS.
           PERFORM READ-TOOLKIT.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-010.
            MOVE 610 TO POS.
            DISPLAY
            "Y=REMOVE 'T' WHERE ITEM NOT A KIT ITEM ANY MORE:[ ]"
             AT POS.
            MOVE 715 TO POS.
            DISPLAY "N= ONLY NEW ITEMS WILL BE FLAGGED 'T'" AT POS.
            MOVE 659 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 3         TO CDA-ROW.
           MOVE 58        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

            IF W-ESCAPE-KEY = 3
               PERFORM END-900.
            IF WS-ANSWER1 NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-020
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
        GET-020.
            MOVE 810 TO POS.
            DISPLAY "Press <RETURN> to continue, <END> TO EXIT."
               AT POS.
            MOVE 875 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 75        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

            IF W-ESCAPE-KEY = 3
               PERFORM END-900.
            IF W-ESCAPE-KEY = 4
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-999
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
        GET-999.
            EXIT.
      *
       READ-STOCK-FILE SECTION.
       RS-010. 
           READ STOCK-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               GO TO RS-999.
           IF ST-ANALYSIS = "S" OR = "N"
               GO TO RS-999.
       RS-020.
           MOVE "T" TO ST-ANALYSIS.
           REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK FILE BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RS-020.
       RS-999.
           EXIT.
      *
       RESET-STOCK-ANALYSIS SECTION.
       RSA-000.
           MOVE 2910 TO POS.
           DISPLAY "RESETTING STOCK-ANALYSIS FIELD TO BLANK...." AT POS.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "NO STOCK FILE FOUND, 'ESC' TO EXIT PROGRAM."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              EXIT PROGRAM.
       RSA-010. 
           READ STOCK-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               MOVE 0 TO WS-STOCK-ST1
               GO TO RSA-900.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSA-010.
           IF ST-ANALYSIS NOT = "T"
              GO TO RSA-010.
           MOVE 2410 TO POS
           DISPLAY "STOCKNUMBER CURRENTLY BEING CHANGED =" AT POS
           MOVE 2450 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
       RSA-020. 
           MOVE " " TO ST-ANALYSIS.
           REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK FILE BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSA-020.
           GO TO RSA-010.
       RSA-900.
           PERFORM ERROR-020.
           MOVE 2410 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2910 TO POS.
           DISPLAY WS-MESSAGE AT POS.
       RSA-999.
           EXIT.
      *
       READ-TOOLKIT SECTION.
       RT-000.
           MOVE 2910 TO POS.
           DISPLAY "READING TOOLKIT FILE...." AT POS.
           MOVE " " TO TO-COMPONENT-NUMBER ST-STOCKNUMBER.
           START TOOLKITS KEY NOT < TO-COMPONENT-NUMBER
                INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "NO KIT RECORD FOUND, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RT-999.
       RT-010.
           READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO RT-900.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLKIT RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RT-010.
           IF TO-COMPONENT-NUMBER = " "
               GO TO RT-010.
           IF TO-COMPONENT-NUMBER NOT = ST-STOCKNUMBER
               MOVE TO-COMPONENT-NUMBER TO ST-STOCKNUMBER
               MOVE 2410 TO POS
               DISPLAY "STOCKNUMBER CURRENTLY BEING CHANGED =" AT POS
               MOVE 2450 TO POS
               DISPLAY ST-STOCKNUMBER AT POS
               PERFORM READ-STOCK-FILE
               GO TO RT-010.
           GO TO RT-010.
       RT-900.
           PERFORM ERROR-020.
           MOVE 2410 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2910 TO POS.
           DISPLAY WS-MESSAGE AT POS.
       RT-999.
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
       END-900.
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
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
