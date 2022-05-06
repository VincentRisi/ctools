        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrNumber.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(80).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(7) VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE             PIC XX VALUE " ".
       77  WS-RANGE2            PIC X VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-ACC-DISPLAY       PIC Z(6)9.
       77  WS-ACCOUNT-PREVIOUS  PIC 9(7) VALUE 0.
       77  WS-ACCOUNT-NUM       PIC 9(5) VALUE 0.
       77  WS-ACCOUNT-SAVE      PIC 9(5) VALUE 0.
       01  WS-ACCOUNT-CHECK.
           03  WS-PREFIX        PIC X(2).
           03  WS-REST          PIC X(5).
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 315 TO POS
           DISPLAY "** DEBTOR NUMBERS CHECKING REPORT **" AT POS
           MOVE 415 TO POS
           DISPLAY "************************************" AT POS.
           PERFORM OPEN-FILES.
      * CONTROL-003.
      *     Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1410 TO POS
           DISPLAY "ENTER a PREFIX to Check, Blank for ALL: [  ]" AT POS
           ADD 41 TO POS.
           MOVE " " TO CDA-DATA
           MOVE 11 TO CDA-ROW
           MOVE 50 TO CDA-COL
           MOVE 2  TO CDA-DATALEN
           MOVE "A"  TO CDA-ATTR
           MOVE 3  TO CDA-COLOR
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE.
           IF W-ESCAPE-KEY = 4
              PERFORM END-900.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
              GO TO CONTROL-012
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO CONTROL-010.
       CONTROL-012.
           IF WS-RANGE = "  "
               GO TO CONTROL-015.
           MOVE 1610 TO POS
           DISPLAY "DO YOU WISH TO CHECK FOR THE NEXT FREE A/C #: [ ]"
            AT POS
           ADD 47 TO POS.
           MOVE "  " TO CDA-DATA
           MOVE 13   TO CDA-ROW
           MOVE 56   TO CDA-COL
           MOVE 1    TO CDA-DATALEN
           MOVE "A"  TO CDA-ATTR
           MOVE 3    TO CDA-COLOR
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.
           IF WS-RANGE2 NOT = "N" AND NOT = "Y"
              GO TO CONTROL-012.
           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-010.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
              GO TO CONTROL-015
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO CONTROL-012.
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
               MOVE WS-RANGE TO DR-KEY
           ELSE
               MOVE 0 TO DR-KEY.
           START DEBTOR-MASTER KEY NOT < DR-KEY
               INVALID KEY NEXT SENTENCE.
       PRR-005.
           READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
               GO TO PRR-999.
           IF WS-DEBTOR-ST1 = 91
              MOVE "DEBTOR RECORD BUSY ON READ-91, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PRR-005.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE "DEBTOR RECORD BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO PRR-005.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           IF WS-RANGE NOT = " "
              MOVE DR-ACCOUNT-NUMBER TO WS-ACCOUNT-CHECK
            IF WS-PREFIX NOT = WS-RANGE
              ADD 1 TO WS-ACCOUNT-PREVIOUS 
              MOVE 2110 TO POS
              DISPLAY "THE NEXT FREE ACCOUNT IS:" AT POS
              ADD 25 TO POS
              DISPLAY WS-ACCOUNT-PREVIOUS AT POS
              GO TO PRR-999.
       PRR-010.
           MOVE 2710 TO POS
           DISPLAY "Account Number Being Read:" AT POS
           ADD 27 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
           
           IF WS-RANGE2 = "Y"
              PERFORM CHECK-FOR-NEXT-FREE-ACC.
           MOVE DR-ACCOUNT-NUMBER TO WS-ACCOUNT-PREVIOUS.
           ADD 1 TO LINE-CNT.
           GO TO PRR-005.
       PRR-999.
           EXIT.
      *
       CHECK-FOR-NEXT-FREE-ACC SECTION.
       CFNFA-005.
           IF WS-ACCOUNT-NUM = 0
              MOVE WS-REST TO WS-ACCOUNT-NUM
      *                        WS-ACCOUNT-SAVE
            IF WS-ACCOUNT-NUM NOT = 1
              MOVE 1 TO WS-ACCOUNT-NUM WS-ACCOUNT-SAVE
              MOVE WS-ACCOUNT-SAVE TO WS-REST
              MOVE 2710 TO POS
              DISPLAY "<END> TO EXIT OR ANY KEY TO CONTINUE" AT POS
              MOVE 2110 TO POS
              DISPLAY "THE NEXT FREE ACCOUNT IS;" AT POS
              ADD 25 TO POS
              DISPLAY WS-ACCOUNT-CHECK AT POS
              ADD 10 TO POS
              MOVE "  "  TO CDA-DATA
              MOVE 18    TO CDA-ROW
              MOVE 42    TO CDA-COL
              MOVE 1     TO CDA-DATALEN
              MOVE "A"   TO CDA-ATTR              
              MOVE 3     TO CDA-COLOR
              PERFORM CTOS-ACCEPT
              MOVE CDA-DATA TO WS-ACCEPT
           IF W-ESCAPE-KEY = 3
              PERFORM END-900
           ELSE
              GO TO CFNFA-005.
           MOVE WS-REST TO WS-ACCOUNT-NUM.
       CFNFA-100.
           IF WS-ACCOUNT-NUM NOT = WS-ACCOUNT-SAVE + 1
              COMPUTE WS-ACCOUNT-SAVE = WS-ACCOUNT-SAVE + 1
              MOVE WS-ACCOUNT-SAVE TO WS-REST
              MOVE 2710 TO POS
              DISPLAY "<END> TO EXIT OR ANY KEY TO CONTINUE" AT POS
              MOVE 2110 TO POS
              DISPLAY "THE NEXT FREE ACCOUNT IS:" AT POS
              ADD 25 TO POS
              DISPLAY WS-ACCOUNT-CHECK AT POS
      *        ADD 10 TO POS
              MOVE "  " TO CDA-DATA
              MOVE 18   TO CDA-ROW
              MOVE 45   TO CDA-COL
              MOVE 1    TO CDA-DATALEN
              MOVE "A"  TO CDA-ATTR
              MOVE 3    TO CDA-COLOR
              PERFORM CTOS-ACCEPT
              MOVE CDA-DATA TO WS-ACCEPT
           IF W-ESCAPE-KEY = 3
              PERFORM END-900.
           MOVE WS-ACCOUNT-NUM TO WS-ACCOUNT-SAVE.
      *     IF WS-ACCOUNT-NUM NOT = WS-ACCOUNT-SAVE + 1
      *        GO TO CFNFA-100.
       CFNFA-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY." 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Co-Name TO CO-Name.
           ACCEPT WS-DATE FROM DATE.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-010.
           MOVE 2710 TO POS
           DISPLAY WS-MESSAGE AT POS.
       
           MOVE 2310 TO POS
           DISPLAY "TOTAL NUMBER OF ACCOUNTS IN THE RANGE:" AT POS
           ADD 38 TO POS
           MOVE LINE-CNT to WS-ACC-DISPLAY
           DISPLAY WS-ACC-DISPLAY AT POS.
           
           PERFORM ERROR1-020
           MOVE 2910 TO POS
           DISPLAY "PRESS <ENTER> TO EXIT THE PROGRAM." AT POS
           ADD 40 TO POS
           ACCEPT WS-ACCEPT AT POS.
       END-500.
           CLOSE DEBTOR-MASTER.
       END-900.
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
      *
      * END-OF-JOB.
