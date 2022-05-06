        IDENTIFICATION DIVISION.
        PROGRAM-ID. DRTRANXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT DEBTOR-TRANS-FILE ASSIGN TO "DrTrans"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DRTR-KEY
               ALTERNATE RECORD KEY IS DRTR-ACC-KEY WITH DUPLICATES
               FILE STATUS IS WS-DEBTOR-TRANS-STATUS.
           SELECT DEBTOR-TRANS-ASCII ASSIGN TO "DrTransASCII"
               FILE STATUS IS WS-DEBTOR-TRANS-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDrTrans.
           COPY ChlfdDrTransASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(6) VALUE 0.
           77  WS-MESSAGE    PIC X(60) VALUE " ".
           01  WS-DEBTOR-TRANS-STATUS.
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
           DISPLAY "** DRTRANS EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT DEBTOR-TRANS-FILE.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
               MOVE 0 TO DRTR-KEY
              START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND DEBTOR-TRANS-ASCII
           ELSE
              OPEN INPUT DEBTOR-TRANS-ASCII.
           
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
           READ DEBTOR-TRANS-FILE NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY DRTR-ACCOUNT-NUMBER.
           
           ADD 1 TO WS-COUNT.

           MOVE DEBTOR-TRANS-REC      TO ASCII-REC.
        BE-010.
      *     WRITE ASCII-REC
      *           INVALID KEY
             DISPLAY "INVALID WRITE FOR ASCII FILE...."
             DISPLAY WS-STAT1
             STOP RUN.
      *     IF WS-COUNT < 500
             GO TO BE-005.
        BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           READ DEBTOR-TRANS-ASCII NEXT
               AT END 
             GO TO BI-EXIT.

           DISPLAY ASCII-DATE AT 1505
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.

           MOVE ASCII-TYPE                TO DRTR-TYPE.
           MOVE ASCII-TRANSACTION-NUMBER  TO DRTR-TRANSACTION-NUMBER.
           MOVE ASCII-ACCOUNT-NUMBER      TO DRTR-ACCOUNT-NUMBER.
           MOVE ASCII-DATE                TO DRTR-DATE.
           MOVE ASCII-REFERENCE1          TO DRTR-REFERENCE1.
           MOVE ASCII-REFERENCE2          TO DRTR-REFERENCE2.
           MOVE ASCII-DEL-DATE            TO DRTR-DEL-DATE. 
           MOVE ASCII-AMT-OF-INVOICE      TO DRTR-AMT-OF-INVOICE.
           MOVE ASCII-AMT-OUTSTANDING     TO DRTR-AMT-OUTSTANDING.
        BI-010.
           WRITE DEBTOR-TRANS-REC
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STAT1
             CLOSE DEBTOR-TRANS-FILE
                   DEBTOR-TRANS-ASCII
             CALL "C$SLEEP" USING 3
             STOP RUN.
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE DEBTOR-TRANS-FILE
                 DEBTOR-TRANS-ASCII.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
        COPY "ErrorMessage".
      * END-OF-JOB.
