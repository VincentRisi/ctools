        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlParmXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT PARAMETER-FILE ASSIGN TO "SlParameter"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PA-KEY
               FILE STATUS IS WS-PARAM-STATUS.
           SELECT PARAMETER-ASCII ASSIGN TO "SlParameterASCII"
               FILE STATUS IS WS-PARAM-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdParam.
           COPY ChlfdParamASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(4) VALUE 0.
           77  WS-MESSAGE    PIC X(60) VALUE " ".
           01  WS-PARAM-STATUS.
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
           DISPLAY "** PARAM EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT PARAMETER-FILE.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
               MOVE 1 TO PA-KEY
              START PARAMETER-FILE KEY NOT < PA-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND PARAMETER-ASCII
           ELSE
              OPEN INPUT PARAMETER-ASCII.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
            IF WS-STAT1 NOT = 0
               MOVE "EXCLUDING IMPORT FOR THIS COMPANY" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM C-END
               STOP RUN.
                        
           IF WS-ACCEPT = "I"
               MOVE SPACES TO PARAMETER-REC
               MOVE 0 TO ASCII-KEY.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ PARAMETER-FILE NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY PA-KEY.
           
           ADD 1 TO WS-COUNT.
               
           DISPLAY PA-KEY.
           
           IF PA-TYPE = 0
            IF PA-RECORD = 1
             MOVE PA-TYPE                 TO ASCII-TYPE
             MOVE PA-RECORD               TO ASCII-RECORD
             MOVE PA-NAME                 TO ASCII-NAME
             MOVE PA-ADD1                 TO ASCII-ADD1
             MOVE PA-ADD2                 TO ASCII-ADD2
             MOVE PA-ADD3                 TO ASCII-ADD3
             MOVE PA-CODE                 TO ASCII-CODE
             MOVE PA-DEL1                 TO ASCII-DEL1
             MOVE PA-DEL2                 TO ASCII-DEL2
             MOVE PA-DEL3                 TO ASCII-DEL3
             MOVE PA-PHONE                TO ASCII-PHONE
             MOVE PA-FAX                  TO ASCII-FAX
             MOVE PA-COMMENT              TO ASCII-COMMENT
             MOVE PA-CO-REG-NO            TO ASCII-CO-REG-NO
             MOVE PA-CO-VAT-NO            TO ASCII-CO-VAT-NO
             MOVE PA-ORDER-NUMBER         TO ASCII-ORDER-NUMBER
             MOVE PA-INVOICE-NUMBER       TO ASCII-INVOICE-NUMBER
             MOVE PA-CREDIT-NUMBER        TO ASCII-CREDIT-NUMBER
             MOVE PA-CASH-RECEIPT-NUMBER  TO ASCII-CASH-RECEIPT-NUMBER
             MOVE PA-STOCK-RECEIPT-NUMBER TO ASCII-STOCK-RECEIPT-NUMBER
             MOVE PA-JOURNAL-NUMBER       TO ASCII-JOURNAL-NUMBER
             MOVE PA-QUOTE-NUMBER         TO ASCII-QUOTE-NUMBER
             MOVE PA-SUPPLY-ORDER-NUMBER  TO ASCII-SUPPLY-ORDER-NUMBER
             MOVE PA-GST-PERCENT          TO ASCII-GST-PERCENT
             MOVE PA-DRTRANS-NUMBER       TO ASCII-DRTRANS-NUMBER
             MOVE PA-STTRANS-NUMBER       TO ASCII-STTRANS-NUMBER
             MOVE PA-CURRENT-PER-WW       TO ASCII-CURRENT-PER-WW
             MOVE PA-CURRENT-PER-MM       TO ASCII-CURRENT-PER-MM
             MOVE PA-CURRENT-PER-YY       TO ASCII-CURRENT-PER-YY
              GO TO BE-010.

           IF PA-TYPE = 2
             MOVE PA-TYPE                 TO ASCII-TYPE
             MOVE PA-RECORD               TO ASCII-RECORD
             MOVE TERM-DESC               TO ASCII-TERM-DESC
              GO TO BE-010.
           IF PA-TYPE = 3
             MOVE PA-TYPE                 TO ASCII-TYPE
             MOVE PA-RECORD               TO ASCII-RECORD
             MOVE DELIV-DESC              TO ASCII-DELIV-DESC
              GO TO BE-010.
           IF PA-TYPE = 4
             MOVE PA-TYPE                 TO ASCII-TYPE
             MOVE PA-RECORD               TO ASCII-RECORD
             MOVE COMM-DESC               TO ASCII-COMM-DESC
              GO TO BE-010.
           IF PA-TYPE = 5
             MOVE PA-TYPE                 TO ASCII-TYPE
             MOVE PA-RECORD               TO ASCII-RECORD
             MOVE QUOTE-DESC              TO ASCII-QUOTE-DESC
              GO TO BE-010.
           IF PA-TYPE = 6
             MOVE PA-TYPE                   TO ASCII-TYPE
             MOVE PA-RECORD                 TO ASCII-RECORD
             MOVE  INVQUES-PRT-PULLERS      TO ASCII-PRT-PULLERS
             MOVE  INVQUES-CASH-SALES       TO ASCII-CASH-SALES
             MOVE  INVQUES-STOCK-CHANGE     TO ASCII-STOCK-CHANGE
             MOVE  INVQUES-PS-NORM-PRINTER  TO ASCII-PS-NORM-PRINTER
             MOVE  INVQUES-PS-REPR-PRINTER  TO ASCII-PS-REPR-PRINTER
             MOVE  INVQUES-PS-RUSH-PRINTER  TO ASCII-PS-RUSH-PRINTER
             MOVE  INVQUES-STOCK-TO-MAX     TO ASCII-STOCK-TO-MAX
             MOVE  INVQUES-ST-LABEL-PRINTER TO ASCII-ST-LABEL-PRINTER
             MOVE  INVQUES-ST-PRINT-LABELS  TO ASCII-ST-PRINT-LABELS
             MOVE  INVQUES-MU-GP-PERC       TO ASCII-MU-GP-PERC
             MOVE  INVQUES-ACC-CONTACT      TO ASCII-ACC-CONTACT
             MOVE  INVQUES-ACC-PHONE        TO ASCII-ACC-PHONE
             MOVE  INVQUES-CHECK-QUOTES     TO ASCII-CHECK-QUOTES
             MOVE  INVQUES-ACC-OVER-LIMIT   TO ASCII-ACC-OVER-LIMIT
             MOVE  INVQUES-PAUSE-ON-PSLIP   TO ASCII-PAUSE-ON-PSLIP
             MOVE  INVQUES-PAUSE-BACKUP     TO ASCII-PAUSE-BACKUP
              GO TO BE-010.

           IF PA-TYPE = 7
             MOVE PA-TYPE                 TO ASCII-TYPE
             MOVE PA-RECORD               TO ASCII-RECORD
             MOVE BRANCH-NUMBER           TO ASCII-BRANCH-NUMBER
             MOVE BRANCH-NAME             TO ASCII-BRANCH-NAME
              GO TO BE-010.

           IF PA-TYPE = 8
             MOVE PA-TYPE                 TO ASCII-TYPE
             MOVE PA-RECORD               TO ASCII-RECORD
             MOVE REPAIR-DESC             TO ASCII-REPAIR-DESC
              GO TO BE-010.

             MOVE "NOTHING SELECTED AT BE-010" TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO BE-005.
        BE-010.
      *     WRITE ASCII-REC
      *           INVALID KEY
             DISPLAY "INVALID WRITE FOR ASCII FILE...."
             DISPLAY WS-STAT1
             CLOSE PARAMETER-FILE
                   PARAMETER-ASCII
             STOP RUN.

             GO TO BE-005.
        BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           READ PARAMETER-ASCII NEXT
               AT END 
             GO TO BI-EXIT.

           DISPLAY ASCII-KEY AT 1505
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.
           
           IF ASCII-TYPE = 0
            IF ASCII-RECORD = 1
             MOVE ASCII-TYPE                 TO PA-TYPE
             MOVE ASCII-RECORD               TO PA-RECORD
             MOVE ASCII-NAME                 TO PA-NAME
             MOVE ASCII-ADD1                 TO PA-ADD1
             MOVE ASCII-ADD2                 TO PA-ADD2
             MOVE ASCII-ADD3                 TO PA-ADD3
             MOVE ASCII-CODE                 TO PA-CODE
             MOVE ASCII-DEL1                 TO PA-DEL1
             MOVE ASCII-DEL2                 TO PA-DEL2
             MOVE ASCII-DEL3                 TO PA-DEL3
             MOVE ASCII-PHONE                TO PA-PHONE
             MOVE ASCII-FAX                  TO PA-FAX
             MOVE ASCII-COMMENT              TO PA-COMMENT
             MOVE ASCII-CO-REG-NO            TO PA-CO-REG-NO
             MOVE ASCII-CO-VAT-NO            TO PA-CO-VAT-NO
             MOVE ASCII-ORDER-NUMBER         TO PA-ORDER-NUMBER
             MOVE ASCII-INVOICE-NUMBER       TO PA-INVOICE-NUMBER
             MOVE ASCII-CREDIT-NUMBER        TO PA-CREDIT-NUMBER
             MOVE ASCII-CASH-RECEIPT-NUMBER  TO PA-CASH-RECEIPT-NUMBER
             MOVE ASCII-STOCK-RECEIPT-NUMBER TO PA-STOCK-RECEIPT-NUMBER
             MOVE ASCII-JOURNAL-NUMBER       TO PA-JOURNAL-NUMBER
             MOVE ASCII-QUOTE-NUMBER         TO PA-QUOTE-NUMBER
             MOVE ASCII-SUPPLY-ORDER-NUMBER  TO PA-SUPPLY-ORDER-NUMBER
             MOVE ASCII-GST-PERCENT          TO PA-GST-PERCENT
             MOVE ASCII-DRTRANS-NUMBER       TO PA-DRTRANS-NUMBER
             MOVE ASCII-STTRANS-NUMBER       TO PA-STTRANS-NUMBER
             MOVE ASCII-CURRENT-PER-WW       TO PA-CURRENT-PER-WW
             MOVE ASCII-CURRENT-PER-MM       TO PA-CURRENT-PER-MM
             MOVE ASCII-CURRENT-PER-YY       TO PA-CURRENT-PER-YY
              GO TO BI-010.

           IF ASCII-TYPE = 2
             MOVE ASCII-TYPE                 TO PA-TYPE
             MOVE ASCII-RECORD               TO PA-RECORD
             MOVE ASCII-TERM-DESC            TO TERM-DESC
              GO TO BI-010.
           IF ASCII-TYPE = 3
             MOVE ASCII-TYPE                 TO PA-TYPE
             MOVE ASCII-RECORD               TO PA-RECORD
             MOVE ASCII-DELIV-DESC           TO DELIV-DESC
              GO TO BI-010.
           IF ASCII-TYPE = 4
             MOVE ASCII-TYPE                 TO PA-TYPE
             MOVE ASCII-RECORD               TO PA-RECORD
             MOVE ASCII-COMM-DESC            TO COMM-DESC
              GO TO BI-010.
           IF ASCII-TYPE = 5
             MOVE ASCII-TYPE                 TO PA-TYPE
             MOVE ASCII-RECORD               TO PA-RECORD
             MOVE ASCII-QUOTE-DESC           TO QUOTE-DESC
              GO TO BI-010.
           IF ASCII-TYPE = 6
             MOVE ASCII-TYPE             TO PA-TYPE
             MOVE ASCII-RECORD           TO PA-RECORD
             MOVE ASCII-PRT-PULLERS      TO INVQUES-PRT-PULLERS
             MOVE ASCII-CASH-SALES       TO INVQUES-CASH-SALES
             MOVE ASCII-STOCK-CHANGE     TO INVQUES-STOCK-CHANGE
             MOVE ASCII-PS-NORM-PRINTER  TO INVQUES-PS-NORM-PRINTER
             MOVE ASCII-PS-REPR-PRINTER  TO INVQUES-PS-REPR-PRINTER
             MOVE ASCII-PS-RUSH-PRINTER  TO INVQUES-PS-RUSH-PRINTER
             MOVE ASCII-STOCK-TO-MAX     TO INVQUES-STOCK-TO-MAX
             MOVE ASCII-ST-LABEL-PRINTER TO INVQUES-ST-LABEL-PRINTER
             MOVE ASCII-ST-PRINT-LABELS  TO INVQUES-ST-PRINT-LABELS
             MOVE ASCII-MU-GP-PERC       TO INVQUES-MU-GP-PERC
             MOVE ASCII-ACC-CONTACT      TO INVQUES-ACC-CONTACT
             MOVE ASCII-ACC-PHONE        TO INVQUES-ACC-PHONE
             MOVE ASCII-CHECK-QUOTES     TO INVQUES-CHECK-QUOTES
             MOVE ASCII-ACC-OVER-LIMIT   TO INVQUES-ACC-OVER-LIMIT
             MOVE ASCII-PAUSE-ON-PSLIP   TO INVQUES-PAUSE-ON-PSLIP
             MOVE ASCII-PAUSE-BACKUP     TO INVQUES-PAUSE-BACKUP
              GO TO BI-010.

           IF ASCII-TYPE = 7
             MOVE ASCII-TYPE                 TO PA-TYPE
             MOVE ASCII-RECORD               TO PA-RECORD
             MOVE ASCII-BRANCH-NUMBER        TO BRANCH-NUMBER
             MOVE ASCII-BRANCH-NAME          TO BRANCH-NAME
              GO TO BI-010.

           IF ASCII-TYPE = 8
             MOVE ASCII-TYPE                 TO PA-TYPE
             MOVE ASCII-RECORD               TO PA-RECORD
             MOVE ASCII-REPAIR-DESC          TO REPAIR-DESC
              GO TO BI-010.

             MOVE "NOTHING SELECTED AT BI-010" TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             CALL "C$SLEEP" USING 3
             GO TO BI-005.
        BI-010.
           WRITE PARAMETER-REC
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STAT1
             CLOSE PARAMETER-FILE
                   PARAMETER-ASCII
             CALL "C$SLEEP" USING 3
             STOP RUN.
             
             MOVE SPACES TO PARAMETER-REC
             MOVE 0 TO ASCII-KEY.

             GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE PARAMETER-FILE
                 PARAMETER-ASCII.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
        COPY "ErrorMessage".
      * END-OF-JOB.
