        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrRepRst.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectSlSbRep".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdSbRep.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(80).
      *
       WORKING-STORAGE SECTION.
       77  WS-MESSAGE-PART      PIC X(79) VALUE " ".
       77  WS-MESS              PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-ACCEPT            PIC X(10) VALUE " ".
       77  WS-OLD-SALESMAN      PIC X VALUE " ".
       77  WS-NEW-SALESMAN      PIC X VALUE " ".
       77  WS-THIS-YEAR         PIC X VALUE " ".
       77  WS-TOTALS-ONLY       PIC X VALUE " ".
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-SBREP-STATUS.
           03  WS-SBREP-ST1     PIC 99.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 315 TO POS.
           DISPLAY "** SALESMAN NUMBER CHANGE ON DEBTOR FILE **" AT POS
           MOVE 415 TO POS
           DISPLAY "*******************************************" AT POS.
           PERFORM OPEN-FILES.
       CONTROL-010.
           MOVE 1220 TO POS.
           DISPLAY "Enter The OLD Salesman CODE:[ ]" AT POS.
           MOVE 1249 TO POS.
           MOVE " "  TO CDA-DATA
           MOVE 09   TO CDA-ROW
           MOVE 48   TO CDA-COL
           MOVE 1    TO CDA-DATALEN
           MOVE "A"  TO CDA-ATTR
           MOVE 3    TO CDA-COLOR
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-OLD-SALESMAN.

            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
            MOVE WS-OLD-SALESMAN TO SBREP-REP.
            PERFORM READ-SBREP.
            ADD 5 TO POS
            DISPLAY SBREP-REPNAME AT POS.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-012
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-012.
           MOVE 1420 TO POS.
           DISPLAY "Enter The NEW Salesman CODE:[ ]" AT POS.
           MOVE 1449 TO POS.
           MOVE " "  TO CDA-DATA
           MOVE 11   TO CDA-ROW
           MOVE 48   TO CDA-COL
           MOVE 1    TO CDA-DATALEN
           MOVE "A"  TO CDA-ATTR
           MOVE 3    TO CDA-COLOR
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-NEW-SALESMAN.

            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
            MOVE WS-NEW-SALESMAN TO SBREP-REP.
            PERFORM READ-SBREP.
            ADD 5 TO POS
            DISPLAY SBREP-REPNAME AT POS.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-020
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
       CONTROL-020.
           MOVE 3010 TO POS
           DISPLAY "PRESS ANY KEY TO CONTINUE OR <END> TO EXIT."
           AT POS
           ADD 60 TO POS
           MOVE " "  TO CDA-DATA
           MOVE 27   TO CDA-ROW
           MOVE 70   TO CDA-COL
           MOVE 1    TO CDA-DATALEN
           MOVE "A"  TO CDA-ATTR
           MOVE 3    TO CDA-COLOR
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

          IF W-ESCAPE-KEY = 3
              GO TO END-900.
              
          PERFORM ERROR-020.
          MOVE 2510 TO POS
          DISPLAY "The Changes are being processed......" AT POS.
       CONTROL-045.
           PERFORM REP-CHANGE.
           PERFORM END-OFF.
      *
       READ-SBREP SECTION.
       RSB-020.
           START SBREP-MASTER KEY NOT < SBREP-KEY
             INVALID KEY NEXT SENTENCE.
       RSB-030.
           READ SBREP-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-SBREP-ST1 = 23 OR 35 OR 49
              MOVE "*** UNKNOWN REP ***" TO SBREP-REPNAME.
       RSB-999.
            EXIT.
      *
       REP-CHANGE SECTION.
       PRR-000.
           MOVE WS-OLD-SALESMAN TO DR-SALESMAN.
           START DEBTOR-MASTER KEY NOT < DR-SALESMAN
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE 0 TO WS-DEBTOR-ST1
              GO TO PRR-999.
       PRR-005.
           READ DEBTOR-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
               GO TO PRR-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PRR-005.
              
           MOVE 2210 TO POS
           DISPLAY "ACCOUNT NUMBER BEING PROCESSED:" AT POS
           ADD 32 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
              
           IF DR-SALESMAN NOT = WS-OLD-SALESMAN
              GO TO PRR-999.

           MOVE WS-NEW-SALESMAN TO DR-SALESMAN.
       PRR-010.
           REWRITE DEBTOR-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "NO SUCH RECORD TO REWRITE, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PRR-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PRR-010.

           GO TO PRR-005.
       PRR-999.
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
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-008.
           OPEN I-O SBREP-MASTER.
           IF WS-SBREP-ST1 NOT = 0 
              MOVE 0 TO WS-SBREP-ST1
              MOVE "SBREP FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-008.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
       OPEN-999.
            EXIT.
      *
        END-OFF SECTION.
        END-000.
           MOVE 3010 TO POS
           DISPLAY "RUN FINISHED, PRESS ANY KEY TO EXIT THE PROGRAM."
           AT POS
           MOVE 3050 TO POS
           ACCEPT WS-ACCEPT AT POS.
       END-900.
           CLOSE DEBTOR-MASTER
                 SBREP-MASTER.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *      
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
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
