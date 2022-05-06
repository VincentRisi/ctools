        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlTranMv.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT GLTRANS-FILE ASSIGN TO Ws-GlTrans
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GLTRANS-KEY
               ALTERNATE RECORD KEY IS GLTRANS-PERIOD WITH DUPLICATES
               ALTERNATE RECORD KEY IS GLTRANS-ACC-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS GLTRANS-NO WITH DUPLICATES
                     FILE STATUS IS WS-GLTRANS-STATUS.
                     
           SELECT GLTRANS1-FILE ASSIGN TO WS-GlTransTemp
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GLTRANS1-KEY
               ALTERNATE RECORD KEY IS GLTRANS1-PERIOD WITH DUPLICATES
               ALTERNATE RECORD KEY IS GLTRANS1-ACC-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS GLTRANS1-NO WITH DUPLICATES
                     FILE STATUS IS WS-GLTRANS-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY "ChlfdGlTrans".
           COPY "ChlfdGlTrans1".
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  WS-COUNT      PIC 9(6) VALUE 0.
           01  WS-CHECK-ST.
               03  WS-CHECK-1    PIC X.
               03  WS-CHECK-BAL  PIC X(14).
           01  WS-GLTRANS-STATUS.
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
           MOVE 0820 TO POS.
           DISPLAY "** GL-TRANS EXPORT / IMPORT OF DATA **" AT POS
           MOVE 1010 TO POS
           DISPLAY 
           "ENTER E=EXPORT TO GlTransTemp, I=IMPORT FROM Temp: [ ]"
              AT POS
           MOVE 1110 TO POS
           DISPLAY "OR ENTER X=EXIT THE PROGRAM." AT POS
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
      *     DISPLAY WS-ACCEPT AT 1075.
           IF WS-ACCEPT = "X"
              EXIT PROGRAM.
           IF WS-ACCEPT NOT = "E" AND NOT = "I"
              GO TO A-AC001.
        A-AC-EXIT.
           EXIT.
      *
        A-INIT SECTION.
        A-000.
           MOVE WS-GlTrans TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           MOVE WS-GlTransTemp TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "I"
               OPEN OUTPUT GLTRANS-FILE
           ELSE
               OPEN I-O GLTRANS-FILE.
           
      *     MOVE WS-ACCEPT TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
               MOVE 0 TO GLTRANS-KEY
              START GLTRANS-FILE KEY NOT < GLTRANS-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN OUTPUT GLTRANS1-FILE
           ELSE
              OPEN I-O GLTRANS1-FILE.
           
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
        BE-005.
           READ GLTRANS-FILE NEXT WITH LOCK
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY GLTRANS-KEY  AT 1505
           DISPLAY GLTRANS-DATE AT 1540
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.

           MOVE GLTRANS-REC           TO GLTRANS1-REC.
        BE-010.
           WRITE GLTRANS1-REC
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM-EXPORT FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE GLTRANS1-KEY TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             ADD 1 TO GLTRANS-TRANS
                      START GLTRANS-FILE KEY > GLTRANS-KEY.
                      
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
           READ GLTRANS1-FILE NEXT WITH LOCK
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY GLTRANS1-KEY AT 1505
           DISPLAY GLTRANS1-DATE AT 1540
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.
           
           MOVE GLTRANS1-REC    TO GLTRANS-REC.
        BI-010.
           WRITE GLTRANS-REC
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM1 FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE GLTRANS1-KEY TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
      *       DISPLAY "INVALID WRITE FOR ISAM FILE..."
      *       DISPLAY WS-STAT1
      *       CLOSE GLTRANS-FILEx
      *             GLTRANS1-FILE
      *       CALL "C$SLEEP" USING 3
      *         EXIT PROGRAM.
      *       STOP RUN.
      
      *      MOVE SPACES TO GLTRANS-REC.
            
            GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE GLTRANS-FILE
                 GLTRANS1-FILE.
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
