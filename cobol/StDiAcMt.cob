        IDENTIFICATION DIVISION.
        PROGRAM-ID. StDiAcMt.
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
       77  WS-END               PIC X VALUE " ".      
       77  WS-STDISC-NUMBER     PIC X(15) VALUE " ".
       77  WS-STDISC-ACCOUNT    PIC 9(7) VALUE 0.
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
           PERFORM FILL-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
      *      MOVE " " TO STDISC-RECORD.
            MOVE "N" TO NEW-STPRICENO
                        WS-END.
            IF STDISC-ACCOUNT > 0
                PERFORM GET-004
                PERFORM READ-DEBTOR
                GO TO GET-002.    
       GET-001.
            MOVE SPACES TO F-NAMEFIELD.
            MOVE "ACCOUNT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF F-NAMEFIELD = " "
             IF F-EXIT-CH NOT = X"0C"
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
                MOVE NUMERIC-RATE TO STDISC-ACCOUNT.
            IF F-EXIT-CH = X"0C"
                MOVE WS-STDISC-NUMBER  TO STDISC-STOCKNUMBER
                MOVE WS-STDISC-ACCOUNT TO STDISC-ACCOUNT
                PERFORM START-STDISC
                PERFORM READ-STDISC-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-DEBTOR.
            IF WS-DEBTOR-ST1 = 23
                GO TO GET-001.
            PERFORM GET-004.
       GET-002.              
            MOVE SPACES TO F-NAMEFIELD.
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STDISC-STOCKNUMBER.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-FORM
                GO TO GET-000.
            IF F-EXIT-CH = X"01"
                GO TO GET-001.
            IF F-EXIT-CH = X"0C"
                MOVE WS-STDISC-NUMBER TO STDISC-STOCKNUMBER
                MOVE WS-STDISC-ACCOUNT TO STDISC-ACCOUNT
                PERFORM START-STDISC
                PERFORM READ-STDISC-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               GO TO GET-000.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-002.

            PERFORM READ-STDISC.
            IF NEW-STPRICENO = "Y"
               PERFORM READ-STOCK
             IF WS-STOCK-ST1 NOT = 23
               PERFORM GET-005 THRU GET-006
               GO TO GET-999
             ELSE
               GO TO GET-002.
            GO TO GET-005.
       GET-003.
            PERFORM READ-DEBTOR
            PERFORM READ-STOCK.
            
            MOVE "STOCKNUMBER"      TO F-FIELDNAME
            MOVE 11                 TO F-CBFIELDNAME
            MOVE STDISC-STOCKNUMBER TO F-NAMEFIELD
            MOVE 25                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
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
       GET-005.
            MOVE "DESCRIPTION1"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE ST-DESCRIPTION1 TO F-NAMEFIELD
            MOVE 20              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESCRIPTION2"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE ST-DESCRIPTION2 TO F-NAMEFIELD
            MOVE 20              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-006.
            MOVE "PERCENT"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME.
            IF STDISC-PERCENT > 0
                MOVE STDISC-PERCENT TO F-EDNAMEFIELDAMOUNTDIS
                MOVE 5              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS
            ELSE
                MOVE WS-STDISC-PERCENT TO STDISC-PERCENT
                                          F-EDNAMEFIELDAMOUNTDIS
                MOVE 5                 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS.
 
            MOVE "DATE"       TO F-FIELDNAME
            MOVE 4            TO F-CBFIELDNAME
            IF STDISC-DATE > 0
                MOVE STDISC-DATE  TO SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE TO F-NAMEFIELD
                MOVE 10           TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.

            IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "THIS IS NOT A VALID STOCK-NUMBER" TO WS-MESSAGE
               PERFORM ERROR-000.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "THIS IS NOT A VALID ACCOUNT-NUMBER" TO WS-MESSAGE
               PERFORM ERROR1-000.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-030.
            PERFORM ERROR-020
            PERFORM ERROR1-020.
            IF WS-END = "Y"
                 GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PERCENT" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 STDISC-PERCENT WS-STDISC-PERCENT.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
      *         PERFORM RELEASE-STDISC-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF STDISC-PERCENT = 0
               GO TO FILL-030.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STDISC-RECORD
               PERFORM READ-STDISC-NEXT
            IF WS-END NOT = "Y"
               PERFORM GET-003 THRU GET-006
               GO TO FILL-030
            ELSE
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-135.
            IF F-EXIT-CH = X"1B"
             IF NEW-STPRICENO = "Y"
               MOVE WS-DATE TO STDISC-DATE
               PERFORM REWRITE-STDISC-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM REWRITE-STDISC-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STDISC-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.
        FILL-135.
            IF NEW-STPRICENO = "Y"
               MOVE WS-DATE TO STDISC-DATE
               PERFORM GET-006.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10     TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
      *       PERFORM RELEASE-STDISC-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-135.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO STDISC-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-135.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STDISC-RECORD
               PERFORM READ-STDISC-NEXT
            IF WS-END NOT = "Y"
               PERFORM GET-003 THRU GET-006
               GO TO FILL-030
            ELSE
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STDISC-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STDISC-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-135.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-135.
 
            GO TO FILL-030.
       FILL-999.
             EXIT.
      *
       DELETE-STDISC-RECORD SECTION.
       DSR-000.
            IF NEW-STPRICENO = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE STDISC-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-STDISC-ST1 NOT = 0
              MOVE "STPRICE RECORD BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STDISC-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STDISC-ST1
              GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       RELEASE-STDISC-RECORD SECTION.
       REL-000.
            UNLOCK STDISC-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-STDISC-RECORD SECTION.
       RSR-010.
          IF NEW-STPRICENO = "Y"
              GO TO RSR-020.
          REWRITE STDISC-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STDISC-ST1 NOT = 0
              MOVE "STPRICE RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STDISC-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STDISC-ST1
              GO TO RSR-010.
          GO TO RSR-999.
       RSR-020.
          WRITE STDISC-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STDISC-ST1 NOT = 0
              MOVE "STPRICE RECORD BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STDISC-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STDISC-ST1
              GO TO RSR-020.
       RSR-999.
          EXIT.
      *
       READ-STDISC SECTION.
       R-STDISC-000.
             MOVE STDISC-STOCKNUMBER TO WS-STDISC-NUMBER.
             MOVE STDISC-ACCOUNT     TO WS-STDISC-ACCOUNT.
             START STDISC-MASTER KEY NOT < STDISC-KEY
                  INVALID KEY NEXT SENTENCE.
       R-STDISC-010.
             READ STDISC-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STDISC-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-STPRICENO
                MOVE WS-STDISC-NUMBER  TO STDISC-STOCKNUMBER
                MOVE WS-STDISC-ACCOUNT TO STDISC-ACCOUNT
                GO TO R-STDISC-999.
             IF WS-STDISC-ST1 NOT = 0
                MOVE "STPRICE BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STDISC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STDISC-ST1
                GO TO R-STDISC-010.
       R-STDISC-999.
             EXIT.
      *
       START-STDISC SECTION.
       STDISC-STDISC-000.
              MOVE WS-STDISC-NUMBER  TO STDISC-STOCKNUMBER.
              MOVE WS-STDISC-ACCOUNT TO STDISC-ACCOUNT.
              START STDISC-MASTER KEY NOT < STDISC-KEY
                 INVALID KEY NEXT SENTENCE.
       STDISC-STDISC-999.
             EXIT.
      *
       READ-STDISC-NEXT SECTION.
       RSN-005. 
           READ STDISC-MASTER NEXT WITH LOCK
             AT END 
               MOVE " " TO STDISC-STOCKNUMBER
                           WS-STDISC-NUMBER
               MOVE 0   TO STDISC-ACCOUNT
                           WS-STDISC-ACCOUNT
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-STDISC-ST1 = 23 OR 35 OR 49
               MOVE "STPRICE FILE BUSY, PRESS 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-STDISC-ST1 NOT = 0
               MOVE "STPRICE FILE BUSY, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STDISC-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STDISC-ST1
               PERFORM START-STDISC
               GO TO RSN-005.
           MOVE STDISC-STOCKNUMBER TO WS-STDISC-NUMBER
           MOVE STDISC-ACCOUNT     TO WS-STDISC-ACCOUNT.
           MOVE "N" TO NEW-STPRICENO.
       RSN-999.
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
             MOVE STDISC-ACCOUNT TO DR-ACCOUNT-NUMBER.
             START DEBTOR-MASTER KEY NOT < DR-KEY
                INVALID KEY NEXT SENTENCE.
       RD-010.
             READ DEBTOR-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE 
             "ENTER AN EXISTING DEBTOR ACCOUNT NUMBER, 'ESC' TO EXIT."
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-999.
             IF WS-DEBTOR-ST1 = 10
                MOVE "DEBTOR RECORD BUSY, PRESS 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RD-010.
             MOVE STDISC-ACCOUNT TO WS-STDISC-ACCOUNT.
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
             IF F-EXIT-CH = X"07"
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
           MOVE "StDiAcMt"      TO F-FORMNAME
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
      *
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
