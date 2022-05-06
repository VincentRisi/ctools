        IDENTIFICATION DIVISION.
        PROGRAM-ID. STCHANXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT STOCKCHANGE-MASTER ASSIGN TO "StChanges"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STCH-KEY
               FILE STATUS IS WS-CHANGE-STATUS.
           SELECT STOCKCHANGE-ASCII ASSIGN TO "StChangesASCII"
               FILE STATUS IS WS-CHANGE-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStockChanges.
           COPY ChlfdStockChangesASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(6) VALUE 0.
           77  WS-MESSAGE    PIC X(60) VALUE " ".
           01  WS-CHANGE-STATUS.
               03  WS-STAT1  PIC 99.
      *
        PROCEDURE DIVISION.
        CONTROL-PARAGRAPH SECTION.
           PERFORM A-ACCEPT.
           PERFORM A-INIT.
           IF WS-ACCEPT = "E"
               PERFORM B-EXPORT
           ELSE
               PERFORM B-IMPORT.
          PERFORM C-END.
           STOP RUN.
        CONTROL-000.
           EXIT. 
      *
       A-ACCEPT SECTION.
       A-001.
           MOVE 0810 TO POS.
           DISPLAY "** ST-CHANGES EXPORT / IMPORT OF DATA **" AT POS
           MOVE 1010 TO POS
           DISPLAY "ENTER E=EXPORT TO ASCII, I=IMPORT FROM ASCII: [ ]"
              AT POS
           MOVE 1057 TO POS
           ACCEPT WS-ACCEPT AT POS.
           IF WS-ACCEPT NOT = "E" AND NOT = "I"
              GO TO A-001.
        A-AC-EXIT.
           EXIT.
      *
        A-INIT SECTION.
        A-000.
           OPEN OUTPUT STOCKCHANGE-MASTER.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
              MOVE 0 TO STCH-STOCKNUMBER
              START STOCKCHANGE-MASTER KEY NOT < STCH-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND STOCKCHANGE-ASCII
           ELSE
              OPEN INPUT STOCKCHANGE-ASCII.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
            IF WS-STAT1 NOT = 0
               MOVE "EXCLUDING IMPORT FOR THIS COMPANY" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM C-END
               STOP RUN.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ STOCKCHANGE-MASTER NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY STCH-STOCKNUMBER.
           
           ADD 1 TO WS-COUNT.

           MOVE STOCKCHANGE-RECORD    TO ASCII-RECORD.
        BE-010.
      *     WRITE ASCII-RECORD
      *           INVALID KEY
             DISPLAY "INVALID WRITE FOR ASCII FILE...."
             DISPLAY WS-STAT1
             STOP RUN.

           GO TO BE-005.
        BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           READ STOCKCHANGE-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-MESSAGE AT 1505
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.

           MOVE ASCII-RECORD    TO STOCKCHANGE-RECORD.
        BI-010.
           WRITE STOCKCHANGE-RECORD
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STAT1
             CLOSE STOCKCHANGE-MASTER
                   STOCKCHANGE-ASCII
             CALL "C$SLEEP" USING 3
             STOP RUN.
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE STOCKCHANGE-MASTER
                 STOCKCHANGE-ASCII.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
        COPY "ErrorMessage".
      * END-OF-JOB.
