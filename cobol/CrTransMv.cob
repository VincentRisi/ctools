        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrTransMv.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT CRTR-FILE ASSIGN TO Ws-CrTrans
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
                     
           SELECT CRTR1-FILE ASSIGN TO Ws-CrTransTemp
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CRTR1-KEY
               ALTERNATE RECORD KEY IS CRTR1-PERIOD WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRTR1-ACC-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRTR1-REFERENCE WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRTR1-INV-NO WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRTR1-DNOTE-NO WITH DUPLICATES
                     FILE STATUS IS WS-CRTR-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY "ChlfdCrTrans".
           COPY "ChlfdCrTrans1".
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  WS-COUNT      PIC 9(6) VALUE 0.
           01  WS-CHECK-ST.
               03  WS-CHECK-1    PIC X.
               03  WS-CHECK-BAL  PIC X(14).
           01  WS-CRTR-STATUS.
               03  WS-STAT1      PIC 99.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
        PROCEDURE DIVISION Using Ws-Linkage.
        CONTROL-PARAGRAPH SECTION.
           PERFORM CLEAR-SCREEN.
           PERFORM A-ACCEPT.
           PERFORM A-INIT.
           IF WS-ACCEPT = "E"
               PERFORM B-EXPORT
           ELSE
               PERFORM B-IMPORT.
          PERFORM C-END.
          EXIT PROGRAM.
      *     STOP RUN.
        CONTROL-000.
           EXIT. 
      *
       A-ACCEPT SECTION.
       A-AC001.
           MOVE 0810 TO POS.
           DISPLAY "** CR-TRANS EXPORT / IMPORT OF DATA **" AT POS
           MOVE 1010 TO POS
           DISPLAY 
           "ENTER E=EXPORT TO CrTransTemp, I=IMPORT FROM Temp: [ ]"
              AT POS
           MOVE 1062 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 61        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

            IF W-ESCAPE-KEY = 4
               EXIT PROGRAM.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO A-AC010
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO A-AC001.
        A-AC010.
           DISPLAY WS-ACCEPT AT 1075.
           IF WS-ACCEPT = "X"
              EXIT PROGRAM.
           IF WS-ACCEPT NOT = "E" AND NOT = "I"
              GO TO A-AC001.
        A-AC-EXIT.
           EXIT.
      *
        A-INIT SECTION.
        A-000.
           MOVE Ws-CrTrans TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           MOVE Ws-CrTransTemp TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "I"
               OPEN OUTPUT CRTR-FILE
           ELSE
               OPEN I-O CRTR-FILE.
           
      *     MOVE WS-ACCEPT TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
               MOVE 0 TO CRTR-KEY
              START CRTR-FILE KEY NOT < CRTR-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN OUTPUT CRTR1-FILE
           ELSE
              OPEN I-O CRTR1-FILE.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
            IF WS-STAT1 NOT = 0
               MOVE "EXCLUDING TEMP FILE FOR THIS COMPANY" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM C-END
               EXIT PROGRAM.
      *         STOP RUN.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-001.
           MOVE 1 TO CRTR-TYPE
                     CRTR-TRANS.
        BE-005.
           READ CRTR-FILE NEXT WITH LOCK
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY CRTR-KEY   AT 1505
           DISPLAY CRTR-DATE  AT 1520
           DISPLAY "ACC NUM:" AT 1535
           DISPLAY CRTR-ACC-NUMBER AT 1544
           
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.
       
           CALL "C$SLEEP" USING 3.

           IF CRTR-KEY NOT > 0
               MOVE "KEY < 0, GOING TO RE-READ." TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               GO TO BE-005.
           IF CRTR-ACC-NUMBER NOT > 0
               MOVE "ACC NUM < 0, GOING TO RE-READ." TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               GO TO BE-005.

           MOVE CRTR-REC    TO CRTR1-REC.
        BE-010.
           WRITE CRTR1-REC
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM-EXPORT FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE CRTR1-KEY TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             ADD 1 TO CRTR-TRANS
                      START CRTR-FILE KEY > CRTR-KEY.
                      
      *       DISPLAY "INVALID WRITE FOR ASCII FILE...."
      *       DISPLAY WS-STAT1
      *         EXIT PROGRAM.
      *       STOP RUN.

           GO TO BE-005.
        BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           READ CRTR1-FILE NEXT WITH LOCK
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY CRTR1-KEY AT 1505
           DISPLAY CRTR1-DATE AT 1520
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.
           
           MOVE CRTR1-REC    TO CRTR-REC.
        BI-010.
           WRITE CRTR-REC
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM1 FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
      *       DISPLAY "INVALID WRITE FOR ISAM FILE..."
      *       DISPLAY WS-STAT1
      *       CLOSE CRTR-FILE
      *             CRTR1-FILE
      *       CALL "C$SLEEP" USING 3
      *         EXIT PROGRAM.
      *       STOP RUN.
      
      *      MOVE SPACES TO CRTR-REC.
            
            GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE CRTR-FILE
                 CRTR1-FILE.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
      *
       Copy "MenuClearScreen".
       Copy "CTOSCobolAccept".
       COPY "ErrorMessage".
       COPY "Error1Message".
      * END-OF-JOB.
