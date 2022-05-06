        IDENTIFICATION DIVISION.
        PROGRAM-ID. GLMASTXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT GLPARAMETER-FILE ASSIGN TO "GlParameter"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GLPA-KEY
               FILE STATUS IS WS-GLPA-STATUS.
           SELECT GLPARAMETER-ASCII ASSIGN TO "GlParameterASCII"
               FILE STATUS IS WS-GLPA-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlParam.
           COPY ChlfdGlParamASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  SUB-1         PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(6) VALUE 0.
           77  WS-MESSAGE    PIC X(60) VALUE " ".
           01  WS-GLPA-STATUS.
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
           DISPLAY "** GLPARM EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT GLPARAMETER-FILE.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
               MOVE 0 TO GLPARAMETER-REC
              START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND GLPARAMETER-ASCII
           ELSE
              OPEN INPUT GLPARAMETER-ASCII.
           
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
           READ GLPARAMETER-FILE NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY GLPA-RECORD.
           
           ADD 1 TO WS-COUNT.

           MOVE GLPARAMETER-REC    TO ASCII-REC.
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
           READ GLPARAMETER-ASCII NEXT
               AT END 
             GO TO BI-EXIT.

           DISPLAY ASCII-NAME AT 1505
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.

           MOVE ASCII-RECORD               TO GLPA-RECORD
           MOVE ASCII-NAME                 TO GLPA-NAME
           MOVE ASCII-ADD1                 TO GLPA-ADD1
           MOVE ASCII-ADD2                 TO GLPA-ADD2
           MOVE ASCII-ADD3                 TO GLPA-ADD3
           MOVE ASCII-ADD4                 TO GLPA-ADD4
           MOVE ASCII-ADD5                 TO GLPA-ADD5
           MOVE ASCII-ADD6                 TO GLPA-ADD6
           MOVE ASCII-CURRENT-GLPER        TO GLPA-CURRENT-GLPER
           MOVE ASCII-RECJRN-POST          TO GLPA-RECJRN-POST
           MOVE ASCII-GLTRANSNO            TO GLPA-GLTRANSNO
           MOVE ASCII-GROUP-NUM            TO GLPA-GROUP-NUM.
           
           MOVE ASCII-PERIODS              TO GLPA-PERIODS
           MOVE ASCII-GLVAT-REG-NO         TO GLPA-GLVAT-REG-NO
           MOVE ASCII-CURRENT-CRPER        TO GLPA-CURRENT-CRPER
           MOVE ASCII-CR-REMIT             TO GLPA-CR-REMIT
           MOVE ASCII-CRTRANSNO            TO GLPA-CRTRANSNO
           MOVE ASCII-GLCRED-NO            TO GLPA-GLCRED-NO
           MOVE ASCII-GLBANK               TO GLPA-GLBANK
           MOVE ASCII-GLVAT-ACC            TO GLPA-GLVAT-ACC.
           
           MOVE 1 TO SUB-1.
        BI-006.
           MOVE ASCII-SHORT-CR (SUB-1)     TO GLPA-SHORT-CR (SUB-1).
           ADD 1 TO SUB-1
           IF SUB-1 < 11
              GO TO BI-006.

           MOVE ASCII-CURRENT-SLPER        TO GLPA-CURRENT-SLPER
           MOVE ASCII-GLDEBT-NO            TO GLPA-GLDEBT-NO
           MOVE ASCII-GLDRBANK             TO GLPA-GLDRBANK
           MOVE ASCII-GLVATOUTPUT-ACC      TO GLPA-GLVATOUTPUT-ACC
           MOVE ASCII-GLSALES-ACC          TO GLPA-GLSALES-ACC
           MOVE ASCII-GLSALES-ADDONS       TO GLPA-GLSALES-ADDONS
           MOVE ASCII-GLSALES-DISC         TO GLPA-GLSALES-DISC
           MOVE ASCII-GLSALES-ADJ          TO GLPA-GLSALES-ADJ
           MOVE ASCII-GLBDEBT-ACC          TO GLPA-GLBDEBT-ACC
           MOVE ASCII-CURRENT-CBPER        TO GLPA-CURRENT-CBPER
           MOVE ASCII-CB-POST              TO GLPA-CB-POST
           MOVE ASCII-CBTRANSNO            TO GLPA-CBTRANSNO
           MOVE ASCII-CAMS-BANK-CODE       TO GLPA-CAMS-BANK-CODE
           MOVE ASCII-CAMS-BANK-ACC        TO GLPA-CAMS-BANK-ACC
           MOVE ASCII-CAMS-BANK-NAME       TO GLPA-CAMS-BANK-NAME.
           
        BI-010.
           WRITE GLPARAMETER-REC
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STAT1
             CLOSE GLPARAMETER-FILE
                   GLPARAMETER-ASCII
             CALL "C$SLEEP" USING 3
             STOP RUN.
             
           CALL "C$SLEEP" USING 3
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE GLPARAMETER-FILE
                 GLPARAMETER-ASCII.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
        COPY "ErrorMessage".
      * END-OF-JOB.
