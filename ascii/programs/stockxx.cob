        IDENTIFICATION DIVISION.
        PROGRAM-ID. stockxx.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
      *  SOURCE-COMPUTER. B20.
      *  OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT STOCK-MASTER 
               ASSIGN TO "/main/data01/StMaster"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ST-KEY
               ALTERNATE RECORD KEY IS ST-ALT-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS ST-SUPPLIER WITH DUPLICATES
               FILE STATUS IS WS-STOCK-STATUS.
           SELECT STOCK-ASCII ASSIGN TO
                  "/main/asciidata/StMasterASCII"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STOCKA-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStockASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF            PIC X(3) VALUE "   ".
           77  WS-ACCEPT         PIC X VALUE " ".
           77  WS-MESSAGE        PIC X(80) VALUE " ".
           77  POS               PIC 9(4) VALUE 0.
           77  WS-COUNT          PIC 9(6) VALUE 0.
           01  WS-STOCK-STATUS   PIC 9(2).
           01  WS-STOCKA-STATUS  PIC 9(2).
           01  WS-STOCKFNAME     PIC X(100) VALUE 
               "/home/steve/test/StMaster".
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
           DISPLAY "** STOCK EXPORT / IMPORT OF DATA **" AT POS
           MOVE 1010 TO POS
           DISPLAY "ENTER E=EXPORT TO ASCII, I=IMPORT FROM ASCII: [ ]"
              AT POS
           MOVE 1057 TO POS
           ACCEPT WS-ACCEPT AT POS.
           IF WS-ACCEPT NOT = "E" AND NOT = "I"
              GO TO A-001.
           MOVE WS-ACCEPT TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        A-AC-EXIT.
           EXIT.
      *
        A-INIT SECTION.
        A-000.
           OPEN OUTPUT STOCK-MASTER.
           
           MOVE WS-STOCK-STATUS TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
            
      *     MOVE WS-STOCKFNAME TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
            
           MOVE "Openend Stock-Master" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
            
           IF WS-ACCEPT = "E"
               MOVE "CK" TO ST-STOCKNUMBER
               START STOCK-MASTER KEY NOT < ST-KEY
               OPEN EXTEND STOCK-ASCII
           ELSE
               OPEN INPUT STOCK-ASCII.
                         
           MOVE WS-STOCKA-STATUS TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
 
           MOVE "Opened Stock-Masterascii" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
            
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           DISPLAY "DOING NOTHING HERE"
           GO TO BE-EXIT.
      *    READ STOCK-MASTER NEXT
      *        AT END 
      *      GO TO BE-EXIT.
      *        
      *    DISPLAY ST-STOCKNUMBER.
      *    
      *    ADD 1 TO WS-COUNT.
      *
      *    MOVE STOCK-RECORD    TO ASCII-RECORD.
      * BE-010.
      *    WRITE ASCII-RECORD
      *           INVALID KEY
      *       DISPLAY "INVALID WRITE FOR ASCII FILE...."
      *       DISPLAY WS-STAT1
      *       DISPLAY WS-STAT2
      *       STOP RUN.
      *    IF WS-COUNT < 500
      *       GO TO BE-005.
        BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           READ STOCK-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
           
           ADD 1 TO WS-COUNT.    
           DISPLAY ASCII-MESSAGE.

           MOVE ASCII-RECORD    TO STOCK-RECORD.
      *     MOVE " " TO WS-ACCEPT.
        BI-010.
           WRITE STOCK-RECORD
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STOCK-STATUS
             MOVE ST-STOCKNUMBER TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO BI-005.
      *        STOP RUN.
            IF WS-COUNT=2
              MOVE WS-STOCK-STATUS TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
            END-IF.
 
           GO TO BI-005.
        BI-EXIT.
           DISPLAY "Imported ", WS-COUNT, " Lines".
           CALL "C$SLEEP" USING 1
            END-CALL.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE STOCK-MASTER.
           
           MOVE WS-STOCK-STATUS TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
 
           CLOSE STOCK-ASCII.
           
           MOVE WS-STOCKA-STATUS TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
 
        C-EXIT.
           EXIT.
           COPY ErrorMessage.
      * END-OF-JOB.
