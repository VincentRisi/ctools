        IDENTIFICATION DIVISION.
        PROGRAM-ID. CBTRANXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT CRTR-FILE ASSIGN TO "CrTrans"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CRTR-KEY
               ALTERNATE RECORD KEY IS CRTR-PERIOD WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRTR-ACC-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRTR-REFERENCE WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRTR-INV-NO WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRTR-DNOTE-NO WITH DUPLICATES
               FILE STATUS IS WS-CRTR-STATUS.
               
           SELECT CRTR-ASCII ASSIGN TO "CrTransASCII"
               FILE STATUS IS WS-CRTR-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrTrans.
           COPY ChlfdCrTransASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(6) VALUE 0.
           77  WS-MESSAGE    PIC X(60) VALUE " ".
           77  WS-EOF        PIC X(3) VALUE "   ".
           01  F-FIELDNAME   PIC X(20) VALUE " ".
           01  WS-CRTR-STATUS.
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
           DISPLAY "** CRTRANS EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT CRTR-FILE.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
              MOVE " " TO CRTR-KEY
              START CRTR-FILE KEY NOT < CRTR-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND CRTR-ASCII
           ELSE
              OPEN INPUT CRTR-ASCII.
           
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
           READ CRTR-FILE NEXT
               AT END 
               DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY CRTR-KEY.
           
           ADD 1 TO WS-COUNT.

           MOVE CRTR-REC    TO ASCII-REC.
        BE-010.
      *     WRITE ASCII-REC
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
           READ CRTR-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-TRANS AT 1505
           DISPLAY ASCII-UNAPPLIED-AMT AT 1520
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.
      *     CALL "C$SLEEP" USING 1.
            
           MOVE ASCII-TYPE            TO CRTR-TYPE
           MOVE ASCII-TRANS           TO CRTR-TRANS
           MOVE ASCII-FUTURE          TO CRTR-FUTURE
           MOVE ASCII-NO              TO CRTR-NO
           MOVE ASCII-ACC-NUMBER      TO CRTR-ACC-NUMBER
           MOVE ASCII-DATE            TO CRTR-DATE
           MOVE ASCII-REFERENCE       TO CRTR-REFERENCE
           MOVE ASCII-INV-NO          TO CRTR-INV-NO
           MOVE ASCII-DNOTE-NO        TO CRTR-DNOTE-NO
           MOVE ASCII-DUE-DATE        TO CRTR-DUE-DATE
           MOVE ASCII-LOC-AMT         TO CRTR-LOC-AMT
           MOVE ASCII-VAT-AMT         TO CRTR-VAT-AMT
           MOVE ASCII-UNAPPLIED-AMT   TO CRTR-UNAPPLIED-AMT
           MOVE ASCII-FOR-AMT         TO CRTR-FOR-AMT
           MOVE ASCII-EXCHANGE        TO CRTR-EXCHANGE
           MOVE ASCII-SETT-DISC       TO CRTR-SETT-DISC.
        BI-010.
           WRITE CRTR-REC
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             CLOSE CRTR-FILE
                   CRTR-ASCII
             DISPLAY WS-STAT1
             CALL "C$SLEEP" USING 3
             STOP RUN.
             
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE CRTR-FILE
                 CRTR-ASCII.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
        COPY "ErrorMessage".
      * END-OF-JOB.
