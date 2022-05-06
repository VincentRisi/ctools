        IDENTIFICATION DIVISION.
        PROGRAM-ID. StNumber.
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
       77  LINE-CNT             PIC 9(7) VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-PRINTER           PIC X(12) VALUE " ".
       77  WS-RANGE             PIC X(3) VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-STOCK-DISPLAY     PIC Z(6)9.
       01  WS-STOCK-CHECK.
           03  WS-PREFIX        PIC X(3).
           03  WS-REST          PIC X(12).
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 315 TO POS
           DISPLAY "** STOCK NUMBERS CHECKING REPORT **" AT POS
           MOVE 415 TO POS
           DISPLAY "***********************************" AT POS.
           PERFORM OPEN-FILES.
      * CONTROL-003.
      *     Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1210 TO POS
           DISPLAY "ENTER a CATEGORY to Check, Blank for ALL: [   ]"
            AT POS
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 3         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE.

      *     ACCEPT WS-RANGE AT POS.
           IF W-ESCAPE-KEY = 4
              PERFORM END-900.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
              GO TO CONTROL-015
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO CONTROL-010.
       CONTROL-015.
           MOVE 2910 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
                 AT POS.
       CONTROL-020.
            PERFORM PRINT-ROUTINE.
            PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           IF WS-RANGE NOT = "  "
               MOVE WS-RANGE TO ST-KEY ST-CATEGORY.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
       PRR-005.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
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
               GO TO PRR-005.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           IF WS-RANGE NOT = " "
              MOVE ST-STOCKNUMBER TO WS-STOCK-CHECK
            IF WS-PREFIX NOT = WS-RANGE
              GO TO PRR-999.
       PRR-010.
           MOVE 2510 TO POS
           DISPLAY "Stock Number Being Read:" AT POS
           ADD 27 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

           ADD 1 TO LINE-CNT.
           GO TO PRR-005.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Co-Name TO CO-Name.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-010.
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS.
       
           MOVE 2110 TO POS
           DISPLAY "TOTAL NUMBER OF STOCK ITEMS IN THE RANGE:" AT POS
           ADD 42 TO POS
           MOVE LINE-CNT to WS-STOCK-DISPLAY
           DISPLAY WS-STOCK-DISPLAY AT POS.
           
           PERFORM ERROR1-020
           MOVE 2910 TO POS
           DISPLAY "PRESS <ENTER> TO EXIT THE PROGRAM." AT POS
           ADD 40 TO POS
           ACCEPT WS-ACCEPT AT POS.
       END-500.
           CLOSE STOCK-MASTER.
       END-900.
           EXIT PROGRAM.
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
