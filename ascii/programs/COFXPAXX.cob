        IDENTIFICATION DIVISION.
        PROGRAM-ID. COFXPAXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT FAX-PARAMETER ASSIGN TO "CoFaxParam"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FAX-PARMKEY
               FILE STATUS IS WS-FAX-STATUS.
           SELECT FAX-ASCII ASSIGN TO "CoFaxParamASCII"
               FILE STATUS IS WS-FAX-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdFaxParam.
           COPY ChlfdFaxParamASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(4) VALUE 0.
           77  WS-MESSAGE    PIC X(60) VALUE " ".     
           01  WS-FAX-STATUS.
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
           DISPLAY "** FAX-PARM EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT FAX-PARAMETER.

           MOVE WS-FAX-STATUS TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
              OPEN EXTEND FAX-ASCII
           ELSE
              OPEN INPUT FAX-ASCII.

           MOVE WS-FAX-STATUS TO WS-MESSAGE
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
           READ FAX-PARAMETER NEXT
               AT END 
             GO TO BE-EXIT.
               
           DISPLAY FAX-PARAMRECORD.


           MOVE FAX-PARMKEY             TO ASCII-PARMKEY
           MOVE FAX-PANUMBER            TO ASCII-PANUMBER
           MOVE FAX-PANAME              TO ASCII-PANAME.
        BE-010.
      *     WRITE ASCII-PARAMRECORD
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
           READ FAX-ASCII NEXT 
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-PARAMRECORD AT 1505.
           ADD 1 TO WS-COUNT.
           DISPLAY WS-COUNT AT 2510.

           MOVE ASCII-PARMKEY             TO FAX-PARMKEY
           MOVE ASCII-PANUMBER            TO FAX-PANUMBER
           MOVE ASCII-PANAME              TO FAX-PANAME

           CALL "C$SLEEP" USING 1.

        BI-010.
           WRITE FAX-PARAMRECORD
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STAT1
             CALL "C$SLEEP" USING 5
             CLOSE FAX-PARAMETER
                   FAX-ASCII
             STOP RUN.
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE FAX-PARAMETER
                 FAX-ASCII.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
        COPY "ErrorMessage".
      * END-OF-JOB.
