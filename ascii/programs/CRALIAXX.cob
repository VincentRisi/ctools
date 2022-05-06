        IDENTIFICATION DIVISION.
        PROGRAM-ID. CRALIAXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT CRALIAS-MASTER ASSIGN TO "CrAlias"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CRAL-KEY
               FILE STATUS IS WS-CRALIAS-STATUS.
           SELECT CRALIAS-ASCII ASSIGN TO "CrAliasASCII"
               FILE STATUS IS WS-CRALIAS-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrAlias.
           COPY ChlfdCrAliasASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-MESSAGE    PIC X(60) VALUE " ".
           77  WS-COUNT      PIC 9(4) VALUE 0.
           01  WS-CRALIAS-STATUS.
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
           DISPLAY "** CRALIAS EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT CRALIAS-MASTER.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.

           IF WS-ACCEPT = "E"
              MOVE 0 TO CRAL-ALIAS
              START CRALIAS-MASTER KEY NOT < CRAL-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND CRALIAS-ASCII
           ELSE
              OPEN INPUT CRALIAS-ASCII.

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
           READ CRALIAS-MASTER NEXT
               AT END 
               DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY CRAL-KEY.
           
           ADD 1 TO WS-COUNT.

           MOVE CRALIAS-RECORD    TO ASCII-RECORD.
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
           READ CRALIAS-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-MESSAGE AT 1505
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.

           MOVE ASCII-RECORD    TO CRALIAS-RECORD.
        BI-010.
           WRITE CRALIAS-RECORD
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STAT1
             CLOSE CRALIAS-MASTER
                   CRALIAS-ASCII
             CALL "C$SLEEP" USING 3
             STOP RUN.
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE CRALIAS-MASTER
                 CRALIAS-ASCII.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
           COPY "ErrorMessage".
      * END-OF-JOB.
