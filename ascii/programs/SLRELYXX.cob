        IDENTIFICATION DIVISION.
        PROGRAM-ID. SLRELYXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT INCR-LY-REGISTER ASSIGN TO "SlRegLy"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS INCR-LY-KEY
               ALTERNATE RECORD KEY IS INCR-LY-ALT-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-LY-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-LY-PRINTED WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-LY-PORDER WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-LY-AREA WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-LY-NAME WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-LY-ADD1 WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-LY-DEL1 WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-LY-ACCOUNT WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-LY-PULL-DATE WITH DUPLICATES

               FILE STATUS IS WS-REGLY-STATUS.
           SELECT INCR-LY-ASCII ASSIGN TO "SlRegLyASCII"
               FILE STATUS IS WS-REGLY-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdRegisterLy.
           COPY ChlfdRegisterLyASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(6) VALUE 0.
           77  WS-MESSAGE    PIC X(60) VALUE " ".
           01  WS-REGLY-STATUS.
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
           DISPLAY "** REGLY EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT INCR-LY-REGISTER.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
               MOVE 0 TO INCR-LY-KEY
              START INCR-LY-REGISTER KEY NOT < INCR-LY-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND INCR-LY-ASCII
           ELSE
              OPEN INPUT INCR-LY-ASCII.
           
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
           READ INCR-LY-REGISTER NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY INCR-LY-KEY.
           
           ADD 1 TO WS-COUNT.

           MOVE INCR-LY-REC    TO ASCII-REC.
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
           READ INCR-LY-ASCII NEXT
               AT END 
             GO TO BI-EXIT.

           MOVE ASCII-REC    TO INCR-LY-REC.
           
           DISPLAY INCR-LY-INVOICE AT 1505
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.
        BI-010.
           WRITE INCR-LY-REC
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STAT1
             CLOSE INCR-LY-REGISTER
                   INCR-LY-ASCII
             CALL "C$SLEEP" USING 3
             STOP RUN.
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE INCR-LY-REGISTER
                 INCR-LY-ASCII.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
        COPY "ErrorMessage".
      * END-OF-JOB.
