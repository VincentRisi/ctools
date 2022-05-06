        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrCheqMt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrMaster".
          Copy "SelectCrCheques".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS WS-SPOOLER-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrCheques.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-ACC-TO-PRINT      PIC X(7) VALUE " ".
       77  WS-DATE-ENTERED      PIC X(10) VALUE " ".
       77  WS-ZERO              PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-NAME              PIC X(40) VALUE " ".
       77  WS-FOUND             PIC X VALUE " ".
      * 77  WS-CENTS-ONLY        PIC V99 VALUE 0.
       77  WS-CENTS-ONLY        PIC Z99 VALUE " ".
       77  WS-RAND-AMT          PIC Z(7).99.
       77  WS-AMOUNT-OF-CHEQUE  PIC 9(7)V99.
       77  WS-AMOUNT-ENTRY      PIC X(10) VALUE " ".
       77  WS-AMT-DISPLAY       PIC Z(6)9.99.
       77  WS-CHEQUE-NO         PIC 9(6) VALUE 0.
       77  WS-CHEQUE-ENTRY      PIC X(6) VALUE " ".
       77  WS-CHEQUE-DISPLAY    PIC Z(5)9.
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       77  WS-SEND-CHARS        PIC X VALUE "N".
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1  PIC 99.
       01  WS-CRCHEQUE-STATUS.
           03  WS-CRCHEQUE-ST1  PIC 99.
       01  WS-SPOOLER-STATUS.
           03  WS-SPOOLER-ST1   PIC 99.
       01 WS-CHEQUE-AMT.
           03  WS-CHEQUE-DIG    PIC X OCCURS 8.
       01  HEAD.
           03  H-AMT            PIC X(10) VALUE " " OCCURS 9.
       01  HEAD1.
           03  FILLER           PIC X(60) VALUE " ".
           03  H-DATE           PIC X(10).
       01  HEAD2.
           03  FILLER           PIC X(3) VALUE " ".
           03  P-DIG1           PIC X.
           03  H-ACC-NAME       PIC X(40).
           03  P-DIG2           PIC X.
       01  HEAD3.
           03  FILLER           PIC X(7) VALUE " ".
           03  H-AMT1           PIC X(9) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  H-AMT2           PIC X(7) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  H-AMT3           PIC X(6) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  H-AMT4           PIC X(6) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  H-AMT5           PIC X(6) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  H-AMT6           PIC X(6) VALUE " ".
           03  FILLER           PIC X(2) VALUE " ".
           03  H-CENTS          PIC X(6) VALUE " ".
           03  FILLER           PIC X(2) VALUE " ".
           03  H-RAND-AMT       PIC X(11) VALUE " ".
       01  HEAD4.
           03  FILLER           PIC X(32) VALUE " ".
           03  FILLER           PIC X(5) VALUE "A/C#".
           03  H4-ACC           PIC X(7) VALUE " ".
       01  HEAD5.
           03  FILLER           PIC X(37) VALUE " ".
           03  H5-CHEQUE        PIC X(6) VALUE " ".
      *
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Copy "WStore".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "**Creditors Manual Cheque Print Program **" AT POS
           MOVE 415 TO POS
           DISPLAY "******************************************" AT POS
           PERFORM ERROR-020.
       CONTROL-003.
           Copy "PrinterAcceptCr".
           PERFORM OPEN-FILES.
       CONTROL-011.
           PERFORM ERROR1-020.
           PERFORM ERROR2-020.
       CONTROL-012.
           MOVE 1210 TO POS
           DISPLAY 
           "SEND 4INCH SET UP CHARACTERS TO THE PRINTER, [ ] " AT POS.
           
           MOVE 1410 TO POS
           DISPLAY "THE PAPER WILL FEED UP SOME AND THEN STOP.  PLEASE"
             AT POS
           MOVE 1510 TO POS
           DISPLAY 
           "THEN SET THE PAPER IN THE CORRECT POSITION TO PRINT."
             AT POS.
           MOVE 1710 TO POS
           DISPLAY "IF FOR ANY REASON YOU SWITCH OFF THE PRINTER POWER"
             AT POS.
           MOVE 1810 TO POS
           DISPLAY "YOU MUST RERUN THIS PROCESS TO SET UP PAGE LENGTH."
             AT POS.
           MOVE 1256 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SEND-CHARS.

      *     ACCEPT WS-SEND-CHARS AT POS.
           IF W-ESCAPE-KEY = 3
              PERFORM END-OFF.
           IF WS-SEND-CHARS NOT = "Y" AND NOT = "N"
              GO TO CONTROL-012.
           IF WS-SEND-CHARS = "N"
              PERFORM CLEAR-SCREEN
              PERFORM CONTROL-000
              GO TO CONTROL-013.
      *NEXT LINES USED FOR SETTING PAGE LENGTH TO 4 INCH 
      *
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE WS-PRINT-4 TO PRINT-REC
           WRITE PRINT-REC
           MOVE " "        TO PRINT-REC.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           
           CALL "C$SLEEP" USING 2.
      *     MOVE 10 TO W-DELAY.
      *     CALL "&DELAY" USING W-ERROR
      *                         W-DELAY.
           PERFORM CLEAR-SCREEN.
           PERFORM CONTROL-000.
       CONTROL-013.
           MOVE " " TO WS-ACC-TO-PRINT.
           MOVE 1010 TO POS.
           DISPLAY "                ACCOUNT NUMBER: [       ]" AT POS.
           MOVE 1043 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACC-TO-PRINT.

      *     ACCEPT WS-ACC-TO-PRINT AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF WS-ACC-TO-PRINT = " "
               GO TO CONTROL-014.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-013.
       CONTROL-014.
           MOVE " " TO WS-NAME.
           MOVE 1110 TO POS.
           DISPLAY "  NAME TO BE PRINTED ON CHEQUE: " AT POS.
           MOVE 1210 TO POS
           DISPLAY "[                                        ]" AT POS
           MOVE 1210 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 40        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 10        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-NAME.

      *     ACCEPT WS-NAME AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-013.
           IF WS-NAME = " "
               GO TO CONTROL-014.
           MOVE WS-NAME TO CR-NAME.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-014.
       CONTROL-015.
           MOVE WS-ACC-TO-PRINT TO CR-ACCOUNT-NUMBER.
           READ CREDITOR-MASTER 
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
               MOVE "UNKNOWN CREDITOR" TO CR-NAME
               GO TO CONTROL-013.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE
               "CREDITORS BUSY ON READ, IN 1 SECOND GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               GO TO CONTROL-015.
               
           MOVE 1211 TO POS
           DISPLAY CR-NAME AT POS.
       CONTROL-020.
           MOVE H-DATE TO WS-DATE-ENTERED.
           MOVE 1410 TO POS.
           DISPLAY "       DATE OF CHEQUE TO PRINT: [          ]"
                AT POS.
           MOVE 1443 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATE-ENTERED.

      *     ACCEPT WS-DATE-ENTERED AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-013.
           IF WS-DATE-ENTERED = " "
               GO TO CONTROL-030.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-025.
            MOVE WS-DATE-ENTERED TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               MOVE "DATE ENTRY INCORRECT, RE-ENTER" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CONTROL-020.
           MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE DISPLAY-DATE    TO H-DATE.
           MOVE 1443 TO POS.
           DISPLAY DISPLAY-DATE AT POS.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-DATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
               MOVE "DATE ENTRY MUST BE CORRECTED, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CONTROL-020.
       CONTROL-030.
           PERFORM ERROR2-020.
           PERFORM ERROR1-020.
           MOVE " " TO WS-ACC-TO-PRINT.
           MOVE 1610 TO POS.
           DISPLAY "             AMOUNT OF CHEQUE : [          ]"
             AT POS.
           MOVE 1643 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-AMOUNT-ENTRY.

      *     ACCEPT WS-AMOUNT-ENTRY AT POS.
           MOVE WS-AMOUNT-ENTRY TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-AMOUNT-OF-CHEQUE.
           MOVE WS-AMOUNT-OF-CHEQUE TO WS-AMT-DISPLAY
           MOVE 1643 TO POS.
           DISPLAY WS-AMT-DISPLAY AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-020.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-035
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-030.
       CONTROL-035.
           PERFORM ERROR2-020.
           PERFORM ERROR1-020.
           MOVE " " TO WS-ACC-TO-PRINT.
           MOVE 1810 TO POS.
           DISPLAY "                CHEQUE NUMBER : [      ]"
            AT POS.
           MOVE 1843 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 6         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CHEQUE-ENTRY.

      *     ACCEPT WS-CHEQUE-ENTRY AT POS.
           MOVE WS-CHEQUE-ENTRY TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-CHEQUE-NO.
           MOVE WS-CHEQUE-NO TO WS-CHEQUE-DISPLAY
           MOVE 1843 TO POS.
           DISPLAY WS-CHEQUE-DISPLAY AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-030.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-035.
       CONTROL-040.
           MOVE 2610 TO POS.
           DISPLAY "The Report Is Being compiled, Please Be Patient"
               AT POS.
       CONTROL-045.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           IF WS-PRINTERNUMBER (21) = 1
               MOVE "/ctools/spl/CrChequCo" TO WS-PRINTER
               MOVE WS-PRINTER TO ALPHA-RATE
               MOVE 20 TO SUB-1
               MOVE WS-CO-NUMBER TO AL-RATE (SUB-1)
               MOVE ALPHA-RATE TO WS-PRINTER W-FILENAME.
           MOVE " " TO WS-FOUND.
       PR-001.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
            IF WS-SPOOLER-ST1 NOT = 0
               MOVE
           "Print File is held by another Terminal, 'ESC' to Retry."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PR-001.
           IF WS-PRINTERNUMBER (21) = 1
               MOVE WTELL-PAUSE TO PRINT-REC
               WRITE PRINT-REC.
           MOVE " " TO PRINT-REC.
       PR-010.
           WRITE PRINT-REC FROM HEAD1 AFTER 1.
           
           MOVE WS-PRINT-BOLD     TO P-DIG1
           MOVE CR-NAME           TO H-ACC-NAME
           MOVE WS-PRINT-UNBOLD   TO P-DIG2
           WRITE PRINT-REC FROM HEAD2 AFTER 3.
           
           MOVE WS-AMOUNT-OF-CHEQUE TO WS-CHEQUE-AMT WS-CENTS-ONLY
           MOVE WS-AMOUNT-OF-CHEQUE TO WS-RAND-AMT.
           MOVE 1   TO SUB-1.
           MOVE "N" TO WS-ZERO.
       PR-035.
           IF WS-CHEQUE-DIG (SUB-1) = 0
            IF WS-ZERO = "N"
              MOVE ALL "*" TO H-AMT (SUB-1)
              MOVE "*"     TO WS-CHEQUE-DIG (SUB-1)
              GO TO PR-036
            ELSE
              PERFORM PR-050
              GO TO PR-036.
              
            PERFORM PR-050.            
       PR-036.
           ADD 1 TO SUB-1.
           IF SUB-1 < 8
               GO TO PR-035.
               
           MOVE WS-AMT-DISPLAY TO ALPHA-RATE
           PERFORM MOVE-CENTS 
           MOVE DATA-RATE      TO H-CENTS
           MOVE 0 TO SUB-1.
           PERFORM PR-065.
           MOVE 0 TO SUB-1.
           MOVE WS-RAND-AMT TO ALPHA-RATE
           PERFORM PR-060.
           MOVE ALPHA-RATE   TO H-RAND-AMT
           WRITE PRINT-REC FROM HEAD3 AFTER 3.
      *     WRITE PRINT-REC FROM HEAD3 AFTER 4.
           
           IF CR-ACCOUNT-NUMBER > 0
               MOVE CR-ACCOUNT-NUMBER TO H4-ACC
           ELSE
               MOVE "C.O.D." TO H4-ACC.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD4 AFTER 4.
           
           MOVE WS-CHEQUE-NO TO H5-CHEQUE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD5 AFTER 2.
           
           MOVE " " TO PRINT-REC
      *****************************************************************
      * ENTRY BELOW REMOVED AS THIS WAS USED WHEN THE PAGE LENGTH WAS *
      * SET BY THE NUMBER OF LINES PRINTED WHEN PRINTING MORE THAN    *
      * ONE CHEQUE IN A BATCH.  SEE CrChequRp.Src                     *
      *                                                               *
      *     WRITE PRINT-REC AFTER 8.                                  *
      *****************************************************************
           
           IF WS-FOUND = " "
               MOVE "Y" TO WS-FOUND.
           GO TO PR-999.
       PR-050.
           IF WS-CHEQUE-DIG (SUB-1) = 0
              MOVE "ZERO" TO H-AMT (SUB-1).
           IF WS-CHEQUE-DIG (SUB-1) = 1
              MOVE " ONE" TO H-AMT (SUB-1).
           IF WS-CHEQUE-DIG (SUB-1) = 2
              MOVE " TWO" TO H-AMT (SUB-1).
           IF WS-CHEQUE-DIG (SUB-1) = 3
              MOVE "THREE" TO H-AMT (SUB-1).
           IF WS-CHEQUE-DIG (SUB-1) = 4
              MOVE "FOUR" TO H-AMT (SUB-1).
           IF WS-CHEQUE-DIG (SUB-1) = 5
              MOVE "FIVE" TO H-AMT (SUB-1).
           IF WS-CHEQUE-DIG (SUB-1) = 6
              MOVE " SIX" TO H-AMT (SUB-1).
           IF WS-CHEQUE-DIG (SUB-1) = 7
              MOVE "SEVEN" TO H-AMT (SUB-1).
           IF WS-CHEQUE-DIG (SUB-1) = 8
              MOVE "EIGHT" TO H-AMT (SUB-1).
           IF WS-CHEQUE-DIG (SUB-1) = 9
              MOVE "NINE" TO H-AMT (SUB-1).
              
           MOVE "Y" TO WS-ZERO.
       PR-060.
           ADD 1 TO SUB-1.
           IF AL-RATE (SUB-1) = " "
               MOVE "*" TO AL-RATE (SUB-1).
           IF SUB-1 < 6
              GO TO PR-060.
       PR-065.
           MOVE H-AMT (2) TO H-AMT1
           MOVE H-AMT (3) TO H-AMT2
           MOVE H-AMT (4) TO H-AMT3
           MOVE H-AMT (5) TO H-AMT4
           MOVE H-AMT (6) TO H-AMT5 
           MOVE H-AMT (7) TO H-AMT6.
       PR-999.
           EXIT.
      *
       MOVE-CENTS SECTION.
       MC-001.
           MOVE 0 TO SUB-1 SUB-2.
       MC-005.
           ADD 1 TO SUB-1.
           IF AL-RATE (SUB-1) NOT = "."
              ADD 1 TO SUB-1
            IF SUB-1 < 8
              GO TO MC-005.
           MOVE 1 TO SUB-2.
           ADD  1 TO SUB-1.
       MC-010.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-2 < 3
              GO TO MC-010.
       MC-999.
           EXIT.
      *
       CHECK-SPOOLER SECTION.
       CP-000.               
           MOVE " " TO WS-MESSAGE
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 3010 TO POS
           DISPLAY "Queuing Print File........" AT POS.
           
           PERFORM OPEN-PRINT-FILES
           PERFORM QUEUE-PRINT-FILE
           MOVE SPACE TO W-SPOOLST
           MOVE SPACE TO W-SPOOLST2
           PERFORM CHECK-FOR-PAUSE
           PERFORM ERROR2-020
           PERFORM ERROR-020.

           MOVE
           "Load the Cheque Stationery, Then Press 'ESC' to Start."
               TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.

           PERFORM SEND-CONTROL-CHAR.
           PERFORM ERROR-020.
           MOVE 3010 TO POS.
           DISPLAY "Printing of Cheques in progress ......." AT POS.
      *
      * PRINTING COMPLETE
      *
           PERFORM CHECK-PAUSE-PRINT.
           PERFORM ERROR-020.
       
           MOVE " Load Normal Paper to Continue Other Reports Then"
               TO WS-MESSAGE.
           PERFORM ERROR-000.
           MOVE "  Switch Printer 'OFF And ON' Then Press 'ESC'"
               TO WS-MESSAGE1.
           PERFORM ERROR2-MESSAGE.

           PERFORM SEND-CONTROL-CHAR.
       CP-999.
           EXIT.
      *
       QUEUE-PRINT-FILE SECTION.
           MOVE 20         TO W-CBSPOOLERFILESPEC.
           MOVE Ws-Printer TO W-SPOOLERFILESPEC.
           CALL "ADDQUEUEENTRY" USING
                                W-ERROR
                                W-QUEUENAME
                                W-QUEUENAMELENGTH
                                W-Q-NO-SERVER
                                W-PRIORITY
                                W-QUEUETYPE
                                W-QUEUEENTRYBUFFERSEND
                                W-QUEUEENTRYBUFFERLENGTH
                                W-DATE-TIME
                                W-REPEATTIME.
       QUEUE-PRINT-EXIT.
           EXIT.
      *
       OPEN-PRINT-FILES SECTION.
           MOVE SPACE                  TO W-QUEUEENTRYBUFFERSEND
           MOVE X"00"                  TO W-FDELETEAFTERPROC
           MOVE 0       TO W-CBFORMNAME
           MOVE 0       TO W-CBWHEELNAME
           MOVE 1       TO W-BYTE1
           MOVE 0       TO W-BYTE2
           MOVE X"00" TO W-BPRINTMODE
           MOVE X"00" TO W-FALIGNFORM
           MOVE X"00" TO W-FSECURITYMODE
           MOVE "SPL" TO W-QUEUENAME
           MOVE 3     TO W-QUEUENAMELENGTH
           MOVE 0     TO W-QUEUEENTRYHANDLE
           MOVE 123   TO W-QUEUEENTRYBUFFERLENGTH
           MOVE 11    TO W-STATUSBLOCKLENGTH
           MOVE "PARALLELCONTROL" TO W-PAR-QUEUENAME
           MOVE 15                TO W-PAR-QUEUENAMELENGTH
           MOVE "SPOOLERSTATUS"   TO W-STATUS-QUEUENAME
           MOVE 13                TO W-STATUS-QUEUENAMELEN
           MOVE "PARALLEL"        TO W-PRINTERNAME
           MOVE 8                 TO W-PRINTERNAMELEN
           MOVE X"FF"       TO W-Q-NO-SERVER
           MOVE 5           TO W-PRIORITY
           MOVE 1           TO W-QUEUETYPE
           MOVE X"00000000" TO W-DATE-TIME
           MOVE 0           TO W-REPEATTIME
           MOVE 100         TO W-DELAY
           MOVE 0           TO W-ZERO
           MOVE 255         TO W-SPOOLST-LEN.
       OPEN-PRINT-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           IF WS-DATE > 0
              GO TO OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE  FROM DATE
           MOVE WS-DATE      TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H-DATE.
       OPEN-020.
           OPEN I-O CREDITOR-MASTER.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CR MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-020.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
      * OPEN-030.
      *     OPEN I-O CRCH-FILE.
      *     IF WS-CRCHEQUE-ST1 NOT = 0
      *         MOVE 0 TO WS-CRCHEQUE-ST1
      *         MOVE "CRCHEQUES FILE BUSY ON OPEN, 'ESC' TO RETRY."
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         GO TO OPEN-030.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-010.
           IF WS-PRINTERNUMBER (21) = 1
              WRITE PRINT-REC BEFORE PAGE
              MOVE W-NULL TO PRINT-REC
              WRITE PRINT-REC
              WRITE PRINT-REC
              WRITE PRINT-REC
              WRITE PRINT-REC
              WRITE PRINT-REC
              MOVE WTELL-PAUSE TO PRINT-REC
              WRITE PRINT-REC.
           MOVE " " TO PRINT-REC.
           CLOSE PRINT-FILE
                 CRCH-FILE
                 CREDITOR-MASTER.

           PERFORM SEND-REPORT-TO-PRINTER.

           IF WS-PRINTERNUMBER (21) = 1
            IF WS-FOUND = "Y"
              PERFORM CHECK-SPOOLER.
       END-900.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "GetSystemY2KDate".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      ******************
      *Mandatory Copies*
      ******************
       Copy "CheckForPause".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "Error2Message".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
