        IDENTIFICATION DIVISION.
        PROGRAM-ID. COPRIN1X.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT PRINTER-MASTER ASSIGN TO "CoPrintersRemote1"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PRNT-KEY
               FILE STATUS IS WS-PRINTERS-STATUS.
           SELECT PRINTER-ASCII ASSIGN TO "CoPrintersRemote1ASCII"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PRINTERS-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCoPrinters.
           COPY ChlfdCoPrintersASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(4) VALUE 0.
           77  WS-MESSAGE    PIC X(60) VALUE " ".     
           01  WS-PRINTERS-STATUS.
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
           DISPLAY "** PRINTERS CO1 EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT PRINTER-MASTER.

           MOVE WS-PRINTERS-STATUS TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
              OPEN EXTEND PRINTER-ASCII
           ELSE
              OPEN INPUT PRINTER-ASCII.

           MOVE WS-PRINTERS-STATUS TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.

           IF WS-STAT1 NOT = 0
               MOVE "EXCLUDING IMPORT FOR THIS COMPANY" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM C-END
               STOP RUN.
           
      *     MOVE 01 TO ASCII-NUMBER.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ PRINTER-MASTER NEXT
               AT END 
             GO TO BE-EXIT.
               
           DISPLAY PRNT-NUMBER.

           MOVE PRINTER-REC    TO ASCII-RECORD.
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
           READ PRINTER-ASCII NEXT 
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-RECORD AT 1505.
           ADD 1 TO WS-COUNT.
           DISPLAY WS-COUNT AT 2510.

           MOVE ASCII-NUMBER              TO PRNT-NUMBER
           MOVE ASCII-NAME                TO PRNT-NAME
           MOVE ASCII-TYPE                TO PRNT-TYPE
           MOVE ASCII-DESC                TO PRNT-DESC
           MOVE ASCII-PROMPT-PAPER        TO PRNT-PROMPT-PAPER
           MOVE ASCII-COMP                TO PRNT-COMP
           MOVE ASCII-BOLD                TO PRNT-BOLD
           MOVE ASCII-UNBOLD              TO PRNT-UNBOLD
           MOVE ASCII-NORMAL              TO PRNT-NORMAL
           MOVE ASCII-EIGHT               TO PRNT-EIGHT
           MOVE ASCII-ELEVEN              TO PRNT-EIGHT
           MOVE ASCII-FOUR                TO PRNT-FOUR.

           CALL "C$SLEEP" USING 1.

        BI-010.
           WRITE PRINTER-REC
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STAT1
             CALL "C$SLEEP" USING 5
             CLOSE PRINTER-MASTER
                   PRINTER-ASCII
             STOP RUN.
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE PRINTER-MASTER
                 PRINTER-ASCII.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
        COPY "ErrorMessage".
      * END-OF-JOB.
