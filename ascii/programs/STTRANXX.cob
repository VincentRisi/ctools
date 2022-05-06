        IDENTIFICATION DIVISION.
        PROGRAM-ID. STTRANXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT STOCK-TRANS-FILE ASSIGN TO "StTrans"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STTR-KEY
               ALTERNATE RECORD KEY IS STTR-ST-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS STTR-AC-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS STTR-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS
                               STTR-COMPLETE WITH DUPLICATES
               ALTERNATE RECORD KEY IS
                               STTR-STOCK-NUMBER WITH DUPLICATES
               ALTERNATE RECORD KEY IS 
                               STTR-ACCOUNT-NUMBER WITH DUPLICATES
                     FILE STATUS IS WS-STTR-STATUS.
           SELECT STOCK-TRANS-ASCII ASSIGN TO "StTransASCII"
               FILE STATUS IS WS-STTR-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStTrans.
           COPY ChlfdStTransASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(6) VALUE 0.
           77  WS-MESSAGE    PIC X(60) VALUE " ".
           01  WS-CHECK-ST.
               03  WS-CHECK-1    PIC X.
               03  WS-CHECK-BAL  PIC X(14).
           01  WS-STTR-STATUS.
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
           DISPLAY "** ST-TRANS EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT STOCK-TRANS-FILE.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
               MOVE 0 TO STTR-KEY
              START STOCK-TRANS-FILE KEY NOT < STTR-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND STOCK-TRANS-ASCII
           ELSE
              OPEN INPUT STOCK-TRANS-ASCII.
           
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
           READ STOCK-TRANS-FILE NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY STTR-KEY.
           
           ADD 1 TO WS-COUNT.

           MOVE STOCK-TRANS-REC    TO ASCII-REC.
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
           READ STOCK-TRANS-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-KEY AT 1505
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.
           
           MOVE ASCII-KEY             TO STTR-KEY.
           MOVE ASCII-ST-COMPLETE     TO STTR-ST-COMPLETE
           MOVE ASCII-STOCK-NUMBER    TO STTR-STOCK-NUMBER
                                           WS-CHECK-ST
           MOVE ASCII-DATE            TO STTR-ST-DATE.
           
           MOVE ASCII-AC-COMPLETE     TO STTR-AC-COMPLETE
           MOVE ASCII-ACCOUNT-NUMBER  TO STTR-ACCOUNT-NUMBER
           MOVE ASCII-DATE            TO STTR-AC-DATE.
           
           MOVE ASCII-INV-NO          TO STTR-INV-NO
           MOVE ASCII-DATE            TO STTR-DATE
           MOVE ASCII-COMPLETE        TO STTR-COMPLETE.
           
           IF WS-CHECK-1 = "*"
               MOVE ASCII-BALANCE     TO COMMENT-FIELDS
           ELSE 
               MOVE ASCII-BALANCE     TO DATA-FIELDS.
        BI-010.
           WRITE STOCK-TRANS-REC
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STAT1
             CLOSE STOCK-TRANS-FILE
                   STOCK-TRANS-ASCII
             CALL "C$SLEEP" USING 3
             STOP RUN.
      
            MOVE SPACES TO STOCK-TRANS-REC.
            
            GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE STOCK-TRANS-FILE
                 STOCK-TRANS-ASCII.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
        COPY "ErrorMessage".
      * END-OF-JOB.
