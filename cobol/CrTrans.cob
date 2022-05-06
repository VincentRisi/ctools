        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrTrans.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrMaster".
           Copy "SelectCrTrans".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrTrans.
           COPY ChlfdCreditor.

       WORKING-STORAGE SECTION.
       77  NEW-ORDER          PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-TRANSNO         PIC 9(6) VALUE 0.
       77  WS-TYPE            PIC 99 VALUE 0.
       01  WS-CRTRANS-STATUS.
           03  WS-CRTRANS-ST1  PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1 PIC 99.
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
      *      MOVE " " TO CRTR-REC.
            MOVE "N" TO NEW-ORDER
                        WS-END.
       GET-001.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRTR-TYPE
                                 F-EDNAMEFIELDANAL.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"0C"
      *           MOVE WS-TYPE    TO CRTR-TYPE
      *           MOVE WS-TRANSNO TO CRTR-TRANS
      *           PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
                 PERFORM READ-CREDITOR
              IF WS-END NOT = "Y"
                 PERFORM GET-004
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
                 PERFORM READ-CREDITOR
              IF WS-END NOT = "Y"
                 PERFORM GET-004
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF CRTR-TYPE NOT > 0
                 MOVE "PLEASE ENTER A TYPE IDENTIFIER, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-001.
        GET-002.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TRANSNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRTR-TRANS F-EDNAMEFIELDNUM.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"01"
                 GO TO GET-001.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TYPE    TO CRTR-TYPE
                 MOVE WS-TRANSNO TO CRTR-TRANS
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
                 PERFORM READ-CREDITOR
              IF WS-END NOT = "Y"
                 PERFORM GET-004
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
                 PERFORM READ-CREDITOR
              IF WS-END NOT = "Y"
                 PERFORM GET-004
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF CRTR-TRANS NOT > 0
                 MOVE 
                 "PLEASE ENTER A TRANS No IDENTIFIER, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-002.
            PERFORM READ-TRANS.
            IF NEW-ORDER = "Y"
                GO TO GET-999.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-001.
        GET-004.
            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE CRTR-TYPE TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "TRANSNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CRTR-TRANS TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.
        GET-005.
            MOVE "PERIOD" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CRTR-PERIOD TO F-NAMEFIELD.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CRTR-ACC-NUMBER TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM READ-CREDITOR.
            MOVE "ACCNAME" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CR-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REFR" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE CRTR-REFERENCE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INV" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE CRTR-INV-NO TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DNOTE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CRTR-DNOTE-NO TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE CRTR-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DUEDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CRTR-DUE-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "LOCAL" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CRTR-LOC-AMT TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.

            MOVE "UNAPP" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CRTR-UNAPPLIED-AMT TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.

            MOVE "FORAMT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CRTR-FOR-AMT TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.

            MOVE "EXCH" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE CRTR-EXCHANGE TO F-EDNAMEFIELDVALUE.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-VALUE.

            MOVE "SDISC" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CRTR-SETT-DISC TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-005.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PERIOD" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CRTR-PERIOD.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-065.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-005.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-005.
       FILL-012.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE    TO CRTR-ACC-NUMBER.
            MOVE CRTR-ACC-NUMBER TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-005.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-012.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-012.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-012.

            PERFORM READ-CREDITOR.
            IF CR-NAME = " "
               MOVE "ENTER AN EXISTING ACCOUNT NUMBER" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-012.

            MOVE "ACCNAME" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CR-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "REFR" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CRTR-REFERENCE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"01"
               GO TO FILL-012.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INV" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CRTR-INV-NO.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-025.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-025.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DNOTE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CRTR-DNOTE-NO.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.
       FILL-035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-035.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CRTR-DATE
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-035.

            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-035.
       FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DUEDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-035.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CRTR-DUE-DATE
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-035.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"01"
               GO TO FILL-035.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-040.
       FILL-045.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LOCAL" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDSALE
                                 CRTR-LOC-AMT.
            PERFORM WRITE-FIELD-SALE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-040.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-045.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-045.
       FILL-050.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "UNAPP" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDSALE
                                 CRTR-UNAPPLIED-AMT.
            PERFORM WRITE-FIELD-SALE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-045.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-050.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-050.
       FILL-055.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "FORAMT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDSALE
                                 CRTR-FOR-AMT.
            PERFORM WRITE-FIELD-SALE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-050.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-055.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-055.
       FILL-060.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "EXCH" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDVALUE
                                 CRTR-EXCHANGE.
            PERFORM WRITE-FIELD-VALUE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-055.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-060.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-060.
       FILL-065.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SDISC" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDSALE
                                 CRTR-SETT-DISC.
            PERFORM WRITE-FIELD-SALE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-060.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-065.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-065.

            GO TO FILL-005.
       FILL-999.
            EXIT.
      *
       DELETE-TRANS SECTION.
       DO-000.
            IF NEW-ORDER = "Y"
                GO TO DO-999.
       DO-010.
            DELETE CRTR-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-CRTRANS-ST1 NOT = 0
               MOVE "CRTRANS BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CRTRANS-ST1
               GO TO DO-010.
       DO-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO CRTR-REFERENCE
                         CRTR-FUTURE
                         CRTR-INV-NO
                         CRTR-DNOTE-NO.
             MOVE 0   TO CRTR-TRANS
                         CRTR-TYPE
                         CRTR-NO
                         CRTR-ACC-NUMBER
                         CRTR-DATE
                         CRTR-DUE-DATE
                         CRTR-LOC-AMT
                         CRTR-UNAPPLIED-AMT
                         CRTR-FOR-AMT
                         CRTR-EXCHANGE
                         CRTR-SETT-DISC.
             MOVE WS-TRANSNO   TO CRTR-TRANS.
             MOVE WS-TYPE      TO CRTR-TYPE.
       CLSC-999.
             EXIT.      
      *
       REWRITE-TRANS-RECORD SECTION.
       ROR-010.
            IF NEW-ORDER = "Y"
               GO TO ROR-020.
            REWRITE CRTR-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CRTRANS-ST1 NOT = 0
                MOVE 0 TO WS-CRTRANS-ST1
                MOVE "CRTRANS BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO ROR-010.
            GO TO ROR-999.
       ROR-020.
            WRITE CRTR-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CRTRANS-ST1 NOT = 0
                MOVE 0 TO WS-CRTRANS-ST1
                MOVE "CRTRANS RECORD BUSY ON WRITE,  'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO ROR-020.
       ROR-999.
            EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE CRTR-TRANS     TO WS-TRANSNO.
           MOVE CRTR-TYPE      TO WS-TYPE.
           START CRTR-FILE KEY NOT < CRTR-KEY.
       RO-010.
           READ CRTR-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-CRTRANS-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-CRTRANS-ST1 NOT = 0
                MOVE 0 TO WS-CRTRANS-ST1
                MOVE "CRTRANS BUSY ON READ, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RO-010.
           MOVE "N" TO NEW-ORDER.
           MOVE CRTR-TRANS     TO WS-TRANSNO.
           MOVE CRTR-TYPE      TO WS-TYPE.
       RO-999.
           EXIT.
     *
       READ-CREDITOR SECTION.
       RD-000.
           MOVE CRTR-ACC-NUMBER TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-KEY.
       RD-010.
           READ CREDITOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE " " TO CR-NAME
                GO TO RD-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITORS BUSY ON READ, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.
       RD-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           MOVE WS-TRANSNO   TO CRTR-TRANS.
           MOVE WS-TYPE      TO CRTR-TYPE.
           START CRTR-FILE KEY NOT < CRTR-KEY.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-001.
           MOVE "NUL" TO CRTR-KEY.
           MOVE 0 TO WS-CRTRANS-ST1.
       RONX-005. 
           READ CRTR-FILE NEXT WITH LOCK
            AT END
              MOVE 0   TO CRTR-TRANS
                          CRTR-TYPE
                          WS-TRANSNO
                          WS-TYPE
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE "CR-TRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CRTRANS-ST1
              PERFORM START-TRANS
              GO TO RONX-005.
           MOVE CRTR-TRANS     TO WS-TRANSNO.
           MOVE CRTR-TYPE      TO WS-TYPE.
           MOVE "N"            TO NEW-ORDER.
       RONX-999.
           EXIT.
      *
       READ-TRANS-PREVIOUS SECTION.
       RTPRE-001.
           MOVE 0 TO WS-CRTRANS-ST1.
       RTPRE-005. 
           READ CRTR-FILE PREVIOUS WITH LOCK
            AT END
              MOVE 0   TO CRTR-TRANS
                          CRTR-TYPE
                          WS-TRANSNO
                          WS-TYPE
              MOVE "Y" TO WS-END
            MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RTPRE-999.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
              MOVE "CR-TRANS BUSY ON READ-PREV, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              PERFORM START-TRANS
              GO TO RTPRE-005.
           MOVE CRTR-TRANS     TO WS-TRANSNO.
           MOVE CRTR-TYPE      TO WS-TYPE.
           MOVE "N" TO NEW-ORDER.
       RTPRE-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CRTR-FILE
            IF WS-CRTRANS-ST1 NOT = 0
               MOVE "CR-TRANS FILE BUSY ON OPEN I-O, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CRTRANS-ST1
      *         GO TO OPEN-002.
               GO TO OPEN-000.
            GO TO OPEN-008.
       OPEN-002.
            OPEN OUTPUT CRTR-FILE
            IF WS-CRTRANS-ST1 NOT = 0
               MOVE "CR-TRANS FILE BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CRTRANS-ST1
               GO TO OPEN-000.
       OPEN-008.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CR-MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-008.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrTrans"       TO F-FORMNAME
           MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CRTR-FILE
                 CREDITOR-MASTER.
      *      STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldDate".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldSale".
       Copy "WriteFieldValue".
       Copy "GetSystemY2KDate".
      *
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      * END-OF-JOB
