        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrMastMv.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT DEBTOR-MASTER ASSIGN TO Ws-Debtor
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DR-KEY
               ALTERNATE RECORD KEY IS DR-ALT-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR-SALES-YTD WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR-SALES-LAST WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR-DISCOUNT-CODE WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR-SUPPLY-Y-N WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR-SALESMAN WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR-CORPORATE-GROUP
                  WITH DUPLICATES
               FILE STATUS IS WS-DEBTOR-STATUS.
                     
           SELECT DEBTOR1-MASTER ASSIGN TO Ws-DebtorTemp
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DR1-KEY
               ALTERNATE RECORD KEY IS DR1-ALT-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR1-SALES-YTD WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR1-SALES-LAST WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR1-DISCOUNT-CODE WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR1-SUPPLY-Y-N WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR1-SALESMAN WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR1-CORPORATE-GROUP
                  WITH DUPLICATES
               FILE STATUS IS WS-DEBTOR-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY "ChlfdDebtor".
           COPY "ChlfdDebtor1".
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  WS-COUNT      PIC 9(6) VALUE 0.
           01  WS-CHECK-ST.
               03  WS-CHECK-1    PIC X.
               03  WS-CHECK-BAL  PIC X(14).
           01  WS-DEBTOR-STATUS.
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
           DISPLAY "** DRMASTER EXPORT / IMPORT OF DATA **" AT POS
           MOVE 1010 TO POS
           DISPLAY 
           "ENTER E=EXPORT TO DrMastTemp, I=IMPORT FROM Temp:  [ ]"
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
           MOVE Ws-Debtor TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           MOVE Ws-DebtorTemp TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "I"
               OPEN OUTPUT DEBTOR-MASTER
           ELSE
               OPEN I-O DEBTOR-MASTER.
           
      *     MOVE WS-ACCEPT TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
               MOVE 0 TO DR-KEY
              START DEBTOR-MASTER KEY NOT < DR-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN OUTPUT DEBTOR1-MASTER
           ELSE
              OPEN I-O DEBTOR1-MASTER.
           
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
           READ DEBTOR-MASTER NEXT WITH LOCK
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY DR-KEY  AT 1505
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.

           MOVE DEBTOR-RECORD    TO DEBTOR1-RECORD.
        BE-010.
           WRITE DEBTOR1-RECORD
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM-EXPORT FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE DR1-KEY TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             ADD 1 TO DR-ACCOUNT-NUMBER
                  START DEBTOR-MASTER KEY > DR-KEY.
                      
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
           READ DEBTOR1-MASTER NEXT WITH LOCK
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY DR1-KEY AT 1505
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.
           
           MOVE DEBTOR1-RECORD    TO DEBTOR-RECORD.
        BI-010.
           WRITE DEBTOR-RECORD
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM1 FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
      *       DISPLAY "INVALID WRITE FOR ISAM FILE..."
      *       DISPLAY WS-STAT1
      *       CLOSE DEBTOR-MASTER
      *             DEBTOR1-MASTER
      *       CALL "C$SLEEP" USING 3
      *         EXIT PROGRAM.
      *       STOP RUN.
      
      *      MOVE SPACES TO DEBTOR-RECORD.
            
            GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE DEBTOR-MASTER
                 DEBTOR1-MASTER.
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
