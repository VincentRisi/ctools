        IDENTIFICATION DIVISION.
        PROGRAM-ID. StOrdrMv.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT OUTSTANDING-ORDERS ASSIGN TO Ws-StOrders
              ORGANIZATION IS INDEXED
              LOCK MANUAL
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS OO-KEY
              ALTERNATE RECORD KEY IS OO-STOCK-NUMBER WITH DUPLICATES
              ALTERNATE RECORD KEY IS
                                   OO-SUPPLIER-NUMBER WITH DUPLICATES
              FILE STATUS IS WS-OO-STATUS.
                     
           SELECT OUTSTANDING-ORDERS1 ASSIGN TO Ws-StOrdersTemp
              ORGANIZATION IS INDEXED
              LOCK MANUAL
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS OO-KEY
              ALTERNATE RECORD KEY IS OO1-STOCK-NUMBER WITH DUPLICATES
              ALTERNATE RECORD KEY IS
                                   OO1-SUPPLIER-NUMBER WITH DUPLICATES
              FILE STATUS IS WS-OO-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY "ChlfdOutOrd".
           COPY "ChlfdOutOrd1".
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  WS-COUNT      PIC 9(6) VALUE 0.
           01  WS-CHECK-ST.
               03  WS-CHECK-1    PIC X.
               03  WS-CHECK-BAL  PIC X(14).
           01  WS-OO-STATUS.
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
           DISPLAY "** ST-ORDERS EXPORT / IMPORT OF DATA **" AT POS
           MOVE 1010 TO POS
           DISPLAY 
           "ENTER E=EXPORT TO StOrdersTemp, I=IMPORT FROM Temp: [ ]"
              AT POS
           MOVE 1110 TO POS
           DISPLAY "OR ENTER X=EXIT THE PROGRAM." AT POS
           MOVE 1062 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 62        TO CDA-COL.
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
           MOVE Ws-StOrders TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           MOVE Ws-StOrdersTemp TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "I"
               OPEN OUTPUT OUTSTANDING-ORDERS
           ELSE
               OPEN I-O OUTSTANDING-ORDERS.
           
      *     MOVE WS-ACCEPT TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
               MOVE " " TO OO-KEY
              START OUTSTANDING-ORDERS KEY NOT < OO-STOCK-NUMBER.
            
           IF WS-ACCEPT = "E"
              OPEN OUTPUT OUTSTANDING-ORDERS1
           ELSE
              OPEN I-O OUTSTANDING-ORDERS1.
           
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
           READ OUTSTANDING-ORDERS NEXT WITH LOCK
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY OO-KEY  AT 1505
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.
           
           IF OO-ORDER-NUMBER NOT > " "
               GO TO BE-005.
           IF OO-STOCK-NUMBER NOT > " "
               GO TO BE-005.
           IF OO-ORDER-NUMBER = "P/O39514.09.2021    "
             MOVE "P/O" TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
               GO TO BE-005.
           IF OO-STOCK-NUMBER = "DPR 031057     "
             MOVE "31057" TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
               GO TO BE-005.
           IF OO-STOCK-NUMBER = "DPR 035385     "
             MOVE "35385" TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
               GO TO BE-005.

           MOVE OUT-ORDER-REC    TO OUT-ORDER-REC1.
      *     MOVE OO-ORDER-NUMBER TO OO1-ORDER-NUMBER.
        BE-010.
           WRITE OUT-ORDER-REC1
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM-EXPORT FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE OO1-KEY TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
                      
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
           READ OUTSTANDING-ORDERS1 NEXT WITH LOCK
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY OO1-KEY AT 1505
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.
           
           MOVE OUT-ORDER-REC1    TO OUT-ORDER-REC.
        BI-010.
           WRITE OUT-ORDER-REC
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM1 FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
      *       DISPLAY "INVALID WRITE FOR ISAM FILE..."
      *       DISPLAY WS-STAT1
      *       CLOSE OUTSTANDING-ORDERS
      *             OUTSTANDING-ORDERS1
      *       CALL "C$SLEEP" USING 3
      *         EXIT PROGRAM.
      *       STOP RUN.
      
      *      MOVE SPACES TO OUT-ORDER-REC.
            
            GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE OUTSTANDING-ORDERS
                 OUTSTANDING-ORDERS1.
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
