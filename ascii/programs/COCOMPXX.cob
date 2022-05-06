        IDENTIFICATION DIVISION.
        PROGRAM-ID. CoCompXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT COMPANY-MENU ASSIGN TO "CoCompany"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PTY-KEY
               FILE STATUS IS WS-COMPANY-STATUS.
           SELECT COMPANY-ASCII ASSIGN TO
                      "CoCompanyASCII"
               FILE STATUS IS WS-COMPANY-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY "ChlfdCompany".
           COPY "ChlfdCompanyASCII".
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  WS-COUNT      PIC 9(4) VALUE 0.
           77  POS           PIC 9(4) VALUE 0.
           77  WS-MESSAGE    PIC X(60) VALUE " ".
           01  WS-COMPANY-STATUS.
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
           DISPLAY "** COMPANY EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT COMPANY-MENU.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
      *     MOVE " " TO PTY-KEY.
      *     START COMPANY-MENU KEY NOT < PTY-KEY
      *           INVALID KEY
      *           DISPLAY "NO RECORDS ON FILE "
      *           STOP RUN.

           IF WS-ACCEPT = "E"
              OPEN EXTEND COMPANY-ASCII
           ELSE
              OPEN INPUT COMPANY-ASCII.

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
           MOVE "DOING NOTHING HERE" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE
           GO TO BE-EXIT.
      *    READ COMPANY-MENU NEXT
      *         AT END 
      *       GO TO BE-EXIT.
      *         
      *     DISPLAY PTY-NUMBER.
      *
      *     MOVE COMPANY-RECORD    TO ASCII-RECORD.
      * BE-010.
      *     WRITE ASCII-RECORD
      *           INVALID KEY
      *       DISPLAY "INVALID WRITE FOR ASCII FILE...."
      *       DISPLAY WS-STAT1
      *       STOP RUN.
      *     GO TO BE-005.
       BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           READ COMPANY-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-NUMBER AT 1510
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.

           MOVE ASCII-NUMBER      TO PTY-NUMBER
           MOVE ASCII-NAME        TO PTY-CO-NAME
           MOVE ASCII-VOL         TO PTY-VOL-DIR.
        BI-010.
           WRITE COMPANY-RECORD
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM FILE..." TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             CLOSE COMPANY-MENU
                   COMPANY-ASCII
             STOP RUN.
             
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE COMPANY-MENU
                 COMPANY-ASCII.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
           COPY "ErrorMessage".
      * END-OF-JOB.
