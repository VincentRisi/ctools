        IDENTIFICATION DIVISION.
        PROGRAM-ID. StDiAcBt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectStDiscAcc".
         Copy "SelectStMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStDiscAcc.
           COPY ChlfdStock.
           COPY ChlfdDebtor.

       WORKING-STORAGE SECTION.
       77  NEW-STPRICENO        PIC X VALUE " ".      
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "DrNameIq".
       77  WS-CARRY-ON          PIC X VALUE " ".      
       77  WS-BEG-STOCK         PIC X(15) VALUE " ".
       77  WS-END-STOCK         PIC X(15) VALUE " ".
       77  WS-ACCOUNT           PIC 9(7) VALUE 0.
       77  WS-DEL-ACCS          PIC X VALUE " ".
       77  WS-ANALYSIS          PIC X VALUE " ".
       77  WS-STDISC-PERCENT    PIC 99V99 VALUE 0.
       01  WS-STDISC-STATUS.
           03  WS-STDISC-ST1    PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       Copy "WsDateInfo".
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           IF WS-CARRY-ON = "Y"
               PERFORM WRITE-RECORDS.
           IF WS-CARRY-ON = "D" OR = "A" OR = "R"
               PERFORM DELETE-RECORDS.
               
           MOVE " " TO WS-BEG-STOCK
                       WS-END-STOCK
           MOVE 0   TO WS-ACCOUNT.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-001.
            MOVE 2710 TO POS
            DISPLAY "ENTER ALLACCS, TO DELETE ALL INVALID STOCK ITEMS"
               AT POS.
            MOVE SPACES TO F-NAMEFIELD.
            MOVE "ACCOUNT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF F-NAMEFIELD = " "
                 CLOSE DEBTOR-MASTER
                 CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                 CANCEL WS-INQUIRY-PROGRAM
                 PERFORM CLEAR-SCREEN
                 PERFORM OPEN-006
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-NAMEFIELD > " "
                MOVE F-NAMEFIELD TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-ACCOUNT.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            IF F-NAMEFIELD NOT = "ALLACCS"
                PERFORM READ-DEBTOR
            ELSE
                MOVE "Y" TO WS-DEL-ACCS
                MOVE 0   TO WS-ACCOUNT
                GO TO GET-002.
            IF WS-DEBTOR-ST1 = 23
                GO TO GET-001.
            PERFORM GET-004.
       GET-002.              
            MOVE SPACES TO F-NAMEFIELD.
            MOVE "BEG-STOCK" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-BEG-STOCK.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-FORM
                GO TO GET-001.
            IF F-EXIT-CH = X"01"
                GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-002.
       GET-0021.
            MOVE SPACES TO F-NAMEFIELD.
            MOVE "END-STOCK" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-END-STOCK.
            IF F-EXIT-CH = X"01"
                GO TO GET-002.
            IF WS-END-STOCK = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-0021.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-002.
            GO TO GET-030.
       GET-004.
            MOVE "ACCOUNT"         TO F-FIELDNAME
            MOVE 7                 TO F-CBFIELDNAME
            MOVE DR-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 7                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "NAME1" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE DR-NAME TO F-NAMEFIELD
            MOVE 40      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-006.
            MOVE "DATE"       TO F-FIELDNAME
            MOVE 4            TO F-CBFIELDNAME
            MOVE STDISC-DATE  TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PERCENT" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 WS-STDISC-PERCENT.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"01"
               GO TO GET-0021.
            IF WS-STDISC-PERCENT = 0
               GO TO GET-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ANALYSIS"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ANALYSIS.
            IF F-EXIT-CH = X"01"
               GO TO GET-030.
            IF WS-STDISC-PERCENT = 0
               GO TO GET-035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-035.
        GET-135.
            MOVE WS-DATE     TO STDISC-DATE
            PERFORM GET-006.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE"      TO F-FIELDNAME.
            MOVE 4           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF F-EXIT-CH = X"01"
               GO TO GET-035.
            
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO GET-135.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO GET-135.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-135.
       GET-500.
            PERFORM CLEAR-010.
            MOVE 2810 TO POS
            DISPLAY
            "N=ABORT RUN, Y=CREATE DISC'S, D=DELETE ONLY = DISC'S,"
               AT POS
            MOVE 2910 TO POS
            DISPLAY
            "A=DELETE ALL ENTRIES, R=REMOVE DELETED ITEMS.    : [ ]"
             AT POS
            ADD 52 TO POS
            ACCEPT WS-CARRY-ON AT POS.
            IF WS-DEL-ACCS = "Y"
             IF WS-CARRY-ON = "R"
                GO TO GET-999.
            IF WS-DEL-ACCS = "Y"
             IF WS-CARRY-ON NOT = "R"
                MOVE "YOU MUST ANSWER 'R' IF ACCOUNT=DELACCS"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-500.
            IF WS-CARRY-ON NOT = "Y" AND NOT = "N" AND NOT = "D" 
                       AND NOT = "A" AND NOT = "R"
               GO TO GET-500.
       GET-999.
            EXIT.
      *
       WRITE-RECORDS SECTION.
       WRS-000.
           MOVE WS-BEG-STOCK TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       WRS-005.
           READ STOCK-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
              GO TO WRS-999.
           IF WS-STOCK-ST1 NOT = 0
             MOVE "STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO WRS-005.
           IF ST-STOCKNUMBER < WS-BEG-STOCK
              GO TO WRS-005.
           IF ST-STOCKNUMBER > WS-END-STOCK
              GO TO WRS-999.
           IF WS-ANALYSIS = " "
              GO TO WRS-010.
           IF WS-ANALYSIS NOT = ST-ANALYSIS
              GO TO WRS-005.
       WRS-010.
           MOVE 2710 TO POS
           DISPLAY "Stock Number being Read :" AT POS
           ADD 26 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
       
           MOVE ST-STOCKNUMBER    TO STDISC-STOCKNUMBER
           MOVE WS-ACCOUNT        TO STDISC-ACCOUNT
           MOVE WS-STDISC-PERCENT TO STDISC-PERCENT
           MOVE WS-DATE           TO STDISC-DATE.
           
           PERFORM WRITE-STDISC-RECORD.
           
           GO TO WRS-005.
       WRS-999.
           EXIT.
      *
       DELETE-RECORDS SECTION.
       DR-001.
          MOVE WS-ACCOUNT   TO STDISC-ACCOUNT
          START STDISC-MASTER KEY NOT < STDISC-ACCOUNT
             INVALID KEY NEXT SENTENCE.
          IF WS-STDISC-ST1 NOT = 0
              MOVE 88 TO WS-STDISC-ST1
              MOVE "NO RECORDS FOR ACCOUNT, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO DR-999.
       DR-005.
           READ STDISC-MASTER NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-STDISC-ST1 = 10
              GO TO DR-999.
           IF WS-STDISC-ST1 NOT = 0
             MOVE "ST-DISC BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STDISC-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STDISC-ST1
               GO TO DR-005.
           IF STDISC-STOCKNUMBER < WS-BEG-STOCK
              GO TO DR-005.
           IF STDISC-STOCKNUMBER > WS-END-STOCK
              GO TO DR-999.
           IF WS-DEL-ACCS NOT = "Y"
            IF STDISC-ACCOUNT NOT = WS-ACCOUNT
              GO TO DR-999.
              
           MOVE 2710 TO POS
           DISPLAY "STOCK NUMBER BEING DELETED:" AT POS
           ADD 28 TO POS
           DISPLAY STDISC-STOCKNUMBER AT POS
           ADD 17 TO POS
           DISPLAY "FOR ACCOUNT:" AT POS
           ADD 13 TO POS
           DISPLAY STDISC-ACCOUNT AT POS.
           
           PERFORM READ-STOCK.
           IF ST-DESCRIPTION1 = "***** INVALID STOCK-"
              GO TO DR-010.
           
           IF WS-CARRY-ON = "R"
               GO TO DR-005.
           IF WS-CARRY-ON = "A"
               GO TO DR-010.
           IF WS-CARRY-ON = "D"
            IF STDISC-PERCENT = WS-STDISC-PERCENT
               GO TO DR-010.
               
           GO TO DR-005.
       DR-010.
          DELETE STDISC-MASTER
              INVALID KEY NEXT SENTENCE.
          IF WS-STDISC-ST1 NOT = 0
              MOVE "STDISC BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STDISC-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STDISC-ST1
               GO TO DR-010.
          GO TO DR-005.
       DR-999.
          EXIT.
      *
       WRITE-STDISC-RECORD SECTION.
       RSR-005.
           GO TO RSR-020.
       RSR-010.
          REWRITE STDISC-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STDISC-ST1 NOT = 0
              MOVE "STPRICE BUSY ON REWRITE, 'ESC' TO WRITE."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STDISC-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STDISC-ST1
               GO TO RSR-020.
          GO TO RSR-999.
       RSR-020.
          WRITE STDISC-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STDISC-ST1 NOT = 0
              MOVE "STPRICE RECORD ALREADY EXISTS, 'ESC' TO REWRITE."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STDISC-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STDISC-ST1
               GO TO RSR-010.
       RSR-999.
          EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
            MOVE STDISC-STOCKNUMBER TO ST-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-010.
            READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE "***** INVALID STOCK-" TO ST-DESCRIPTION1
                MOVE "NUMBER ENTERED *****" TO ST-DESCRIPTION2
                GO TO R-ST-900.
             IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RE-TRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-010.
       R-ST-900.
             PERFORM ERROR-020.
       R-ST-999.
             EXIT.
      *
       READ-DEBTOR SECTION.
       RD-000.
             MOVE WS-ACCOUNT TO DR-ACCOUNT-NUMBER.
             START DEBTOR-MASTER KEY NOT < DR-KEY
                INVALID KEY NEXT SENTENCE.
       RD-010.
             READ DEBTOR-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE "ENTER AN EXISTING DEBTOR ACCOUNT NUMBER."
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-999.
             IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR RECORD BUSY, PRESS 'ESC' TO RETRY"
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RD-010.
       RD-999.
             EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO STDISC-STOCKNUMBER.
             MOVE 0   TO STDISC-PERCENT
      *                   STDISC-ACCOUNT
                         STDISC-DATE.
       CLSC-500.
             UNLOCK STDISC-MASTER.
       CLSC-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STDISC-MASTER.
            IF WS-STDISC-ST1 NOT = 0
               MOVE 0 TO WS-STDISC-ST1
               MOVE "STOCK SPEC-DISC BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-005.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.
       OPEN-006.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StDiAcBt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STDISC-MASTER
                  STOCK-MASTER
                  DEBTOR-MASTER.
            EXIT PROGRAM.
       END-999.
           EXIT.
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldDate".
       Copy "WriteFieldAmountDis".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB
