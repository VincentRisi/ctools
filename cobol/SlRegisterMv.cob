        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlRegisterMv.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT INCR-REGISTER ASSIGN TO Ws-Register 
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS INCR-KEY
               ALTERNATE RECORD KEY IS INCR-ALT-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-PRINTED WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-PORDER WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-AREA WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-NAME WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-ADD1 WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-DEL1 WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-ACCOUNT WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR-PULL-DATE WITH DUPLICATES
               FILE STATUS IS WS-REGISTER-STATUS.
           SELECT INCR1-REGISTER ASSIGN TO Ws-SlRegTemp
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS INCR1-KEY
               ALTERNATE RECORD KEY IS INCR1-ALT-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR1-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR1-PRINTED WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR1-PORDER WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR1-AREA WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR1-NAME WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR1-ADD1 WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR1-DEL1 WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR1-ACCOUNT WITH DUPLICATES
               ALTERNATE RECORD KEY IS INCR1-PULL-DATE WITH DUPLICATES
               FILE STATUS IS WS-REGISTER-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdRegister.
           COPY ChlfdRegister1.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  WS-COUNT      PIC 9(6) VALUE 0.
           01  WS-REGISTER-STATUS.
               03  WS-STAT1  PIC 99.
      *
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
        CONTROL-000.
           EXIT. 
      *
       A-ACCEPT SECTION.
       A-AC001.
           MOVE 0810 TO POS.
           DISPLAY "** REGISTER EXPORT / IMPORT OF DATA **" AT POS
           MOVE 1010 TO POS
           DISPLAY "ENTER E=EXPORT TO ASCII, I=IMPORT FROM ASCII: [ ]"
              AT POS
           MOVE 1110 TO POS
           DISPLAY "OR ENTER X=EXIT THE PROGRAM." AT POS
           MOVE 1057 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 56        TO CDA-COL.
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
           MOVE Ws-Register TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           MOVE Ws-SlRegTemp TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "I"
               OPEN OUTPUT INCR-REGISTER
           ELSE
               OPEN I-O INCR-REGISTER.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
               MOVE 0 TO INCR-KEY
              START INCR-REGISTER KEY NOT < INCR-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN OUTPUT INCR1-REGISTER
           ELSE
              OPEN I-O INCR1-REGISTER.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
            IF WS-STAT1 NOT = 0
               MOVE "EXCLUDING TEMP FILE FOR THIS COMPANY" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM C-END
               EXIT PROGRAM.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ INCR-REGISTER NEXT WITH LOCK
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
              
           DISPLAY INCR-KEY  AT 1505.
           DISPLAY INCR-DATE AT 1520
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.

           MOVE INCR-REC    TO INCR1-REC.
        BE-010.
           WRITE INCR1-REC
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM1 FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
      *       DISPLAY "INVALID WRITE FOR INCR1 FILE...."
      *       DISPLAY WS-STAT1
      *       EXIT PROGRAM.

             GO TO BE-005.
        BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           READ INCR1-REGISTER NEXT WITH LOCK
               AT END 
             GO TO BI-EXIT.

           MOVE INCR1-REC  TO INCR-REC.
           
           DISPLAY INCR1-KEY  AT 1505
           DISPLAY INCR1-DATE AT 1520
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.

        BI-010.
           WRITE INCR-REC
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
      *       CLOSE INCR-REGISTER
      *             INCR1-REGISTER
      *       CALL "C$SLEEP" USING 3
      *       EXIT PROGRAM.
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE INCR-REGISTER
                 INCR1-REGISTER.
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
