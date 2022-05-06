        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrAdSlAc.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectSlDaily".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdDaily.

       WORKING-STORAGE SECTION.
       77  NEW-DEBTORNO        PIC X VALUE " ".      
       77  WS-END              PIC X VALUE " ".      
       77  WS-DEBTORNUMBER     PIC 9(7) VALUE 0.
       77  WS-NEWDEBTORNUMBER  PIC 9(7) VALUE 0.
       77  WS-OLDDEBTORNUMBER  PIC 9(7) VALUE 0.
       77  WS-ACCEPT           PIC X VALUE " ".
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1   PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1    PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  DEBTOR-SALES-AMOUNTS.
           03  WS-SALES-PTD          PIC S9(7)V99.
           03  WS-SALES-YTD          PIC S9(7)V99.
           03  WS-SALES-LAST         PIC S9(7)V99.
           03  WS-COST-PTD           PIC S9(7)V99.
           03  WS-COST-YTD           PIC S9(7)V99.
           03  WS-COST-LAST          PIC S9(7)V99.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
       CONT-010.
           PERFORM CLEAR-SCREEN
           PERFORM GET-DATA
           GO TO CONT-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 0510 TO POS
            DISPLAY "**** ADD DEBTOR SALES TO NEW ACCOUNT *****" AT POS
            MOVE 0610 TO POS
            DISPLAY "******************************************" AT POS.
            MOVE " " TO DEBTOR-RECORD.
            MOVE "N" TO NEW-DEBTORNO
                        WS-END.
       GET-001.
           MOVE SPACES TO F-FIELDNAMEACC
           MOVE 0810 TO POS
           DISPLAY
            "ENTER THE OLD ACCOUNT TO TAKE FIGURES FROM : [       ]"
              AT POS
           ADD 46 TO POS
           MOVE " "  TO CDA-DATA
           MOVE 05   TO CDA-ROW
           MOVE 55   TO CDA-COL
           MOVE 7    TO CDA-DATALEN
           MOVE "A"  TO CDA-ATTR
           MOVE 3    TO CDA-COLOR
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO F-FIELDNAMEACC.
      *     ACCEPT F-FIELDNAMEACC AT POS.
           MOVE F-FIELDNAMEACC TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO DR-ACCOUNT-NUMBER.
           IF W-ESCAPE-KEY = 3
                PERFORM END-OFF.
            IF W-ESCAPE-KEY = 7
                MOVE WS-DEBTORNUMBER TO DR-ACCOUNT-NUMBER
                PERFORM START-DEBTOR
                PERFORM READ-DEBTOR-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               GO TO GET-000.
            IF W-ESCAPE-KEY = 6
               PERFORM RELEASE-DEBTOR-RECORD
               GO TO GET-000.
            IF W-ESCAPE-KEY NOT = 0 AND NOT = 1 AND NOT = 2
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-DEBTOR.
            IF NEW-DEBTORNO = "Y"
               MOVE "YOU CAN ONLY CHANGE AN EXISTING DEBTOR NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-000.
        GET-003.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
            MOVE 0856 TO POS
            DISPLAY DR-ACCOUNT-NUMBER AT POS
            MOVE 0940 TO POS
            DISPLAY DR-NAME AT POS.

           MOVE DR-SALES-PTD  TO WS-SALES-PTD
           MOVE DR-SALES-YTD  TO WS-SALES-YTD
           MOVE DR-SALES-LAST TO WS-SALES-LAST
           MOVE DR-COST-PTD   TO WS-COST-PTD
           MOVE DR-COST-YTD   TO WS-COST-YTD
           MOVE DR-COST-LAST  TO WS-COST-LAST.

           MOVE SPACES TO F-FIELDNAMEACC
           MOVE DR-ACCOUNT-NUMBER TO WS-OLDDEBTORNUMBER.
           PERFORM REWRITE-DEBTOR-RECORD.
       GET-500.
           MOVE 1110 TO POS
           DISPLAY
            "ENTER THE NEW ACCOUNT TO ADD FIGURES TO    : [       ]"
              AT POS
           ADD 46 TO POS
           ADD 46 TO POS
           MOVE " "  TO CDA-DATA
           MOVE 08   TO CDA-ROW
           MOVE 55   TO CDA-COL
           MOVE 7    TO CDA-DATALEN
           MOVE "A"  TO CDA-ATTR
           MOVE 3    TO CDA-COLOR
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO F-FIELDNAMEACC.
      *     ACCEPT F-FIELDNAMEACC AT POS.
           MOVE F-FIELDNAMEACC TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
            IF W-ESCAPE-KEY = 4
               GO TO GET-000.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-510
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-500.
       GET-510.
           MOVE 2010 TO POS
           DISPLAY "PRESS <RETURN> TO CONTINUE, <ESC> TO STOP."
              AT POS
           ADD 46 TO POS
           MOVE " "  TO CDA-DATA
           MOVE 17   TO CDA-ROW
           MOVE 55   TO CDA-COL
           MOVE 7    TO CDA-DATALEN
           MOVE "A"  TO CDA-ATTR
           MOVE 3    TO CDA-COLOR
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

      *     ACCEPT WS-ACCEPT AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-500.
           IF W-ESCAPE-KEY = 3
               GO TO END-900.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 6
                GO TO GET-520
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-510.
       GET-520.
           IF W-ESCAPE-KEY = 6
               GO TO GET-999.
       GET-550.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
            MOVE NUMERIC-RATE TO DR-ACCOUNT-NUMBER WS-NEWDEBTORNUMBER.
            PERFORM READ-DEBTOR.
            IF NEW-DEBTORNO = "Y"
               MOVE "YOU CAN ONLY CHANGE AN EXISTING DEBTOR NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-500.
        GET-553.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
            MOVE 1156 TO POS
            DISPLAY DR-ACCOUNT-NUMBER AT POS
            MOVE 1240 TO POS
            DISPLAY DR-NAME AT POS.
       GET-600.
            ADD WS-SALES-PTD  TO DR-SALES-PTD
            ADD WS-SALES-YTD  TO DR-SALES-YTD
            ADD WS-SALES-LAST TO DR-SALES-LAST
            ADD WS-COST-PTD   TO DR-COST-PTD
            ADD WS-COST-YTD   TO DR-COST-YTD
            ADD WS-COST-LAST  TO DR-COST-LAST.

            PERFORM REWRITE-DEBTOR-RECORD.
            MOVE WS-OLDDEBTORNUMBER TO DR-ACCOUNT-NUMBER.
            PERFORM READ-DEBTOR.
            
            MOVE 0 TO DR-SALES-PTD
                      DR-SALES-YTD
                      DR-SALES-LAST
                      DR-COST-PTD
                      DR-COST-YTD
                      DR-COST-LAST.
            PERFORM REWRITE-DEBTOR-RECORD.
       GET-999.
            EXIT.
      *
       RELEASE-DEBTOR-RECORD SECTION.
       REL-000.
           UNLOCK DEBTOR-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-DEBTOR-RECORD SECTION.
       RSR-010.
          REWRITE DEBTOR-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-DEBTOR-ST1 NOT = 0
              MOVE 0 TO WS-DEBTOR-ST1
              MOVE "DEBTOR RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSR-010.
       RSR-999.
          EXIT.
      *
       READ-DEBTOR SECTION.
       R-DR-000.
             MOVE DR-ACCOUNT-NUMBER TO WS-DEBTORNUMBER.
             START DEBTOR-MASTER KEY NOT < DR-KEY
                 INVALID KEY NEXT SENTENCE.
       R-DR-010.
             READ DEBTOR-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE "Y" TO NEW-DEBTORNO
                MOVE WS-DEBTORNUMBER TO DR-ACCOUNT-NUMBER
                GO TO R-DR-999.
             IF WS-DEBTOR-ST1 NOT = 0
                MOVE 0 TO WS-DEBTOR-ST1
                MOVE "DEBTOR RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-DR-010.
             MOVE "N" TO NEW-DEBTORNO.
       R-DR-999.
             EXIT.
      *
       START-DEBTOR SECTION.
       DR-DR-000.
              MOVE WS-DEBTORNUMBER TO DR-ACCOUNT-NUMBER.
              START DEBTOR-MASTER KEY NOT LESS DR-ACCOUNT-NUMBER.
       DR-DR-999.
             EXIT.
      *
       READ-DEBTOR-NEXT SECTION.
       RSN-005. 
           READ DEBTOR-MASTER NEXT WITH LOCK
             AT END 
               MOVE 0 TO DR-ACCOUNT-NUMBER
                         WS-DEBTORNUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "DEBTOR FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RSN-005.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               PERFORM START-DEBTOR
               GO TO RSN-005.
           MOVE DR-ACCOUNT-NUMBER TO WS-DEBTORNUMBER.
           MOVE "N" TO NEW-DEBTORNO.
       RSN-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE DEBTOR-MASTER.
       END-900.
      *      STOP RUN.
            EXIT PROGRAM.
       END-999.
            EXIT.
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
