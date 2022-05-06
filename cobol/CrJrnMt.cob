        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrJrnMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrMaster".
           Copy "SelectCrJrn".
           Copy "SelectGlMaster".
           Copy "SelectGlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrJrn.
           COPY ChlfdCreditor.
           COPY ChlfdGlParam.
           COPY ChlfdGlMast.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).

       WORKING-STORAGE SECTION.
       77  NEW-ORDER          PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-REFERENCE       PIC X(10) VALUE " ".
       77  WS-TRANSNO         PIC 9(6) VALUE 0.
       77  WS-TYPE            PIC 99 VALUE 0.
       77  WS-NEWINVOICE        PIC X VALUE " ".
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-LINE-CHANGED      PIC X VALUE " ".
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-RESTOFACCOUNT    PIC X(6).
       01  JOURNAL-DATA.
           03  WS-JRN.
               05  WS-JRN-1STCHAR   PIC X(2) VALUE "PI".
               05  WS-JRN-REST      PIC X(8).
           03  WS-INV-TOTAL         PIC 9(3).
           03  WS-CONTROL-TOTAL     PIC S9(7)V99.
           03  WS-BATCH-TOTAL       PIC S9(7)V99.
           03  WS-VAT-TOTAL         PIC S9(7)V99.
           03  WS-VAT-AMT           PIC S9(7)V99.
           03  WS-INV-AMT           PIC S9(7)V99.
           03  WS-UNAPPLIED         PIC S9(7)V99.
           03  WS-FORAMT            PIC S9(7)V99.
           03  WS-SETT-DISC         PIC S9(7)V99.
           03  WS-INV-NO            PIC X(10).
           03  WS-DNOTE-NO          PIC X(10).
       01  WS-CRJRN-STATUS.
           03  WS-CRJRN-ST1         PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1      PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1   PIC 99.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1        PIC 99.
       01  WS-TEMP-LINE.
           03  FILLER           PIC X(10) VALUE " ".
           03  T-TRANS-DATE     PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  T-TRANS-DUEDATE  PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
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
            MOVE " " TO CRJRN-REC.
            MOVE "N" TO NEW-ORDER
                        WS-END.
       GET-001.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "REFERENCE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CRJRN-REFERENCE.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-REFERENCE TO CRJRN-REFERENCE
                 MOVE WS-TRANSNO   TO CRJRN-TRANS
                 MOVE WS-TYPE      TO CRJRN-TYPE
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
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF CRJRN-REFERENCE NOT > " "
                 MOVE 
                 "PLEASE ENTER A REFERENCE IDENTIFIER, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                DISPLAY " " AT 3079 WITH BELL
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
            MOVE NUMERIC-RATE TO CRJRN-TRANS F-EDNAMEFIELDNUM.
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
                 MOVE WS-REFERENCE TO CRJRN-REFERENCE
                 MOVE WS-TRANSNO   TO CRJRN-TRANS
                 MOVE WS-TYPE      TO CRJRN-TYPE
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
            IF CRJRN-TRANS NOT > 0
                 MOVE 
                "PLEASE ENTER A TRANS No IDENTIFIER, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-002.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-002.
        GET-003.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRJRN-TYPE
                                 F-EDNAMEFIELDANAL.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-REFERENCE TO CRJRN-REFERENCE
                 MOVE WS-TRANSNO   TO CRJRN-TRANS
                 MOVE WS-TYPE      TO CRJRN-TYPE
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
            IF F-EXIT-CH = X"01"
                 GO TO GET-002.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-003.
            IF CRJRN-TYPE NOT > 0
                 MOVE "PLEASE ENTER A TYPE IDENTIFIER, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-003.
            PERFORM READ-TRANS.
            IF NEW-ORDER = "Y"
                GO TO GET-999.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-003.
        GET-004.
            MOVE "REFERENCE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE CRJRN-REFERENCE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TRANSNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CRJRN-TRANS TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE CRJRN-TYPE TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.
        GET-005.
            MOVE "PERIOD" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CRJRN-PERIOD TO F-NAMEFIELD.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CRJRN-CRACC-NUMBER TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM READ-CREDITOR.
            MOVE "ACCNAME" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CR-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INV" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE CRJRN-INV-NO TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DNOTE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CRJRN-DNOTE-NO TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            MOVE CRJRN-INV-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10     TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DUEDATE"      TO F-FIELDNAME.
            MOVE 7              TO F-CBFIELDNAME.
            MOVE CRJRN-DUE-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE   TO F-NAMEFIELD.
            MOVE 10             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "LOCAL" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CRJRN-LOC-AMT TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.

            MOVE "VAT" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE CRJRN-VAT-AMT TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.

            MOVE "UNAPP" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CRJRN-UNAPPLIED-AMT TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.

            MOVE "FORAMT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CRJRN-FOR-AMT TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.

            MOVE "EXCH" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE CRJRN-EXCHANGE TO F-EDNAMEFIELDVALUE.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-VALUE.

            MOVE "SDISC" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CRJRN-SETT-DISC TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.

            MOVE "COMPLETE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CRJRN-COMPLETE TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-010.
            MOVE 1 TO SUB-1.
            PERFORM SCROLL-NEXT.
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
            MOVE F-NAMEFIELD TO CRJRN-PERIOD.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
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
            MOVE NUMERIC-RATE TO CRJRN-CRACC-NUMBER.
            MOVE CRJRN-CRACC-NUMBER TO F-NAMEFIELD.
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
               PERFORM GET-010
               GO TO FILL-012.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
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
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INV" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CRJRN-INV-NO.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
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
            MOVE F-NAMEFIELD TO CRJRN-DNOTE-NO.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
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
            MOVE SPLIT-DATE TO CRJRN-INV-DATE
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-035.

            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
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
            PERFORM CONVERT-SPLIT-FORMAT
            MOVE SPLIT-DATE TO CRJRN-DUE-DATE
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-035.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
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
                                 CRJRN-LOC-AMT.
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
               PERFORM GET-010
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
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
       FILL-047.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "VAT" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDSALE
                                 CRJRN-VAT-AMT.
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
               PERFORM GET-010
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
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
               GO TO FILL-047.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-047.
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
                                 CRJRN-UNAPPLIED-AMT.
            PERFORM WRITE-FIELD-SALE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-047.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
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
                                 CRJRN-FOR-AMT.
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
               PERFORM GET-010
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
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
                                 CRJRN-EXCHANGE.
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
               PERFORM GET-010
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
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
                                 CRJRN-SETT-DISC.
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
               PERFORM GET-010
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
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
       FILL-070.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COMPLETE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CRJRN-COMPLETE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-005
               PERFORM GET-010
               GO TO FILL-005.
            IF F-EXIT-CH = X"01"
               GO TO FILL-065.
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
               GO TO FILL-070.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-070.

            PERFORM FILL-GL-DATA.
            IF WS-ABOVE-BODY = "1"
               GO TO FILL-070.
       FILL-999.
            EXIT.
      *
       DELETE-TRANS SECTION.
       DO-000.
            IF NEW-ORDER = "Y"
                GO TO DO-999.
       DO-010.
            DELETE CRJRN-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-CRJRN-ST1 NOT = 0
               MOVE "CRJRN BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CRJRN-ST1
               GO TO DO-010.
       DO-999.
           EXIT.
      *
       COMPUTE-UNAPPLIED-AMT SECTION.
       CUA-005.
            MOVE SUB-1 TO SUB-3.
            MOVE CRJRN-LOC-AMT TO WS-UNAPPLIED.
            MOVE 0 TO WS-INV-AMT WS-SETT-DISC WS-VAT-AMT.
            MOVE 1 TO SUB-1.
       CUA-010.
            IF CRJRN-GLACC (SUB-1) = " "
                GO TO CUA-020.
            SUBTRACT CRJRN-GLAMT (SUB-1) FROM WS-UNAPPLIED.
            ADD CRJRN-GLAMT (SUB-1)        TO WS-INV-AMT.
            ADD CRJRN-GLDISC (SUB-1)       TO WS-SETT-DISC.
            IF CRJRN-GLACC (SUB-1) = GLPA-GLVAT-ACC
                ADD CRJRN-GLAMT (SUB-1)    TO WS-VAT-AMT.
            ADD 1 TO SUB-1.
            IF SUB-1 NOT > 10
               GO TO CUA-010.
       CUA-020.
            MOVE SUB-3 TO SUB-1.
            MOVE "UNAPPLIED" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE WS-UNAPPLIED TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
       CUA-999.
           EXIT.
      *
       READ-GLMASTER SECTION.
       RD-000.
           MOVE CRJRN-GLACC (SUB-1) TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       RD-010.
           READ GL-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE " " TO GL-NUMBER
               MOVE "INVALID" TO GL-DESCRIPTION
               GO TO RD-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLMASTER  BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RD-010.
       RD-999.
           EXIT.
      *
       FILL-GL-DATA SECTION.
       FILL-GL-000.
           MOVE " " TO WS-ABOVE-BODY.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
           IF WS-NEWINVOICE = "Y"
               MOVE "N" TO WS-LINE-CHANGED
           ELSE
               MOVE "Y" TO WS-LINE-CHANGED.
       FILL-GL-005.
           PERFORM SCROLL-NEXT.
           PERFORM ERROR-020.
       FILL-GL-010.
           PERFORM COMPUTE-UNAPPLIED-AMT.
           MOVE SUB-1 TO F-INDEX.
           MOVE "                              " TO F-NAMEFIELD.
           MOVE "GLNUMBER" TO F-FIELDNAME.
           MOVE 8 TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.

      *     IF F-EXIT-CH = X"0B" 
      *      IF CRJRN-GLACC (SUB-1) = " "
      *         GO TO FILL-GL-010.

           IF F-EXIT-CH = X"01" AND F-INDEX = 1
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM READ-FIELD-ALPHA
            IF F-NAMEFIELD = CRJRN-GLACC (SUB-1)
               MOVE "1" TO WS-ABOVE-BODY
               GO TO FILL-GL-999
            ELSE
               MOVE CRJRN-GLACC (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               MOVE "1" TO WS-ABOVE-BODY
               GO TO FILL-GL-999.

           MOVE 12 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.

           IF F-EXIT-CH = X"1D"
            IF F-NAMEFIELD = " "
              MOVE SPACES TO CRJRN-GLACC (SUB-1)
               GO TO FILL-GL-020.

           IF F-EXIT-CH = X"01"
            IF CRJRN-GLACC (SUB-1) = "  "
               SUBTRACT 1 FROM F-INDEX SUB-1
               GO TO FILL-GL-010.
           
           IF F-EXIT-CH = X"01" AND F-INDEX > 1
            IF F-NAMEFIELD = CRJRN-GLACC (SUB-1)
               SUBTRACT 1 FROM F-INDEX SUB-1
               MOVE "Y" TO WS-LINE-CHANGED
               GO TO FILL-GL-010
            ELSE
               MOVE CRJRN-GLACC (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               SUBTRACT 1 FROM F-INDEX SUB-1
               MOVE "Y" TO WS-LINE-CHANGED
               GO TO FILL-GL-010.

           IF F-EXIT-CH = X"0B" AND F-INDEX < 10
            IF F-NAMEFIELD = CRJRN-GLACC (SUB-1)
               ADD 1 TO F-INDEX SUB-1
               GO TO FILL-GL-010
            ELSE
               MOVE CRJRN-GLACC (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               ADD 1 TO F-INDEX SUB-1
               GO TO FILL-GL-010.
           IF F-EXIT-CH = X"0B" AND F-INDEX = 10
            IF F-NAMEFIELD = CRJRN-GLACC (SUB-1)
               GO TO FILL-GL-010
            ELSE
               MOVE CRJRN-GLACC (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-GL-010.
      * TAB CHARACTER
           IF F-EXIT-CH = X"09"
            IF F-NAMEFIELD = CRJRN-GLACC (SUB-1)
               MOVE " " TO WS-ABOVE-BODY
               PERFORM ERROR-020
               GO TO FILL-GL-900
            ELSE
               MOVE CRJRN-GLACC (SUB-1) TO F-NAMEFIELD
               MOVE 12                  TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               MOVE " "                 TO WS-ABOVE-BODY
               PERFORM ERROR-020
               GO TO FILL-GL-900.
           IF F-EXIT-CH = X"07"
               AND CRJRN-GLACC (SUB-1) = " "
               GO TO FILL-GL-010.
           IF F-EXIT-CH = X"87"
               MOVE SUB-1 TO SUB-7
               PERFORM CANCEL-TRANSACTION
               MOVE 1 TO SUB-1
                         F-INDEX
               PERFORM SCROLL-NEXT
               GO TO FILL-GL-010.
           IF F-NAMEFIELD = " "
                GO TO FILL-GL-010.
           IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                GO TO FILL-GL-010.

           IF F-NAMEFIELD = "A" OR = "B" OR = "C" OR = "D" OR = "E"
                       OR = "F" OR = "G" OR = "H" OR = "I" OR = "J"
                PERFORM CHECK-GL-SHORT-NO.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           IF ALPHA-RATE > SPACES
                PERFORM NUMBER-CHECK.
           MOVE WS-GLNO-CHECK TO CRJRN-GLACC (SUB-1) WS-GLNUMBER.
           IF WS-GLSUBHEADER = "    "
                MOVE "YOU CAN ONLY POST TO A DETAIL ACCOUNT."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE " " TO CRJRN-GLACC (SUB-1)
                GO TO FILL-GL-010.
           IF WS-RESTOFACCOUNT = "      "
                MOVE "YOU CAN ONLY POST TO A DETAIL ACCOUNT."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE " " TO CRJRN-GLACC (SUB-1)
                GO TO FILL-GL-010.
           IF CRJRN-GLAMT (SUB-1) = 0
               MOVE "N" TO WS-LINE-CHANGED
           ELSE
               MOVE "Y" TO WS-LINE-CHANGED.
           PERFORM READ-GLMASTER.
           IF GL-DESCRIPTION = "INVALID"
                MOVE "INVALID GLMASTER NUMBER !!" TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE " " TO CRJRN-GLACC (SUB-1)
                GO TO FILL-GL-010.

            MOVE "GLNUMBER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-GLNO-CHECK TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM ERROR-020.
       FILL-GL-015.
            MOVE "GLNAME" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE GL-DESCRIPTION TO CRJRN-GLDESC (SUB-1) F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       FILL-GL-020.
            IF WS-LINE-CHANGED = "Y"
             IF GL-NUMBER = GLPA-GLVAT-ACC
                SUBTRACT CRJRN-GLAMT (SUB-1) FROM WS-INV-AMT
                SUBTRACT CRJRN-GLAMT (SUB-1) FROM WS-VAT-AMT
             ELSE
                SUBTRACT CRJRN-GLAMT (SUB-1) FROM WS-INV-AMT.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "GLAMOUNT" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
               GO TO FILL-GL-010.
            MOVE "                       " TO F-NAMEFIELD.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRJRN-GLAMT (SUB-1).

           IF F-EXIT-CH = X"1D"
            IF CRJRN-GLACC (SUB-1) = " "
               GO TO FILL-GL-050.

            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-GL-020.

            IF CRJRN-GLAMT (SUB-1) = 0
                MOVE 2801 TO POS
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-GL-020.
            ADD CRJRN-GLAMT (SUB-1) TO WS-INV-AMT.
            IF CRJRN-GLAMT (SUB-1) > 0
             IF WS-INV-AMT > CRJRN-LOC-AMT
                MOVE "THIS AMOUNT IS MORE THAN THE ORIGINAL INVOICE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                SUBTRACT CRJRN-GLAMT (SUB-1) FROM WS-INV-AMT
                MOVE 0 TO CRJRN-GLAMT (SUB-1)
                GO TO FILL-GL-020.
            IF CRJRN-GLAMT (SUB-1) < 0
             IF WS-INV-AMT < CRJRN-LOC-AMT
                MOVE "THIS AMOUNT IS MORE THAN THE ORIGINAL CREDIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                SUBTRACT CRJRN-GLAMT (SUB-1) FROM WS-INV-AMT
                MOVE 0 TO CRJRN-GLAMT (SUB-1)
                GO TO FILL-GL-020.
           IF GL-NUMBER = GLPA-GLVAT-ACC
                ADD CRJRN-GLAMT (SUB-1) TO WS-VAT-AMT.
            MOVE CRJRN-GLAMT (SUB-1) TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
       FILL-GL-030.
            IF CR-SETT-DISC = 0
                GO TO FILL-GL-050.
            IF GL-NUMBER = GLPA-GLVAT-ACC
               COMPUTE NUMERIC-RATE = CRJRN-SETT-DISC - WS-SETT-DISC
            ELSE
               COMPUTE NUMERIC-RATE =
                  (CRJRN-GLAMT (SUB-1) * CR-SETT-DISC) / 100.

            MOVE "GLSETDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
       FILL-GL-050.
            IF WS-LINE-CHANGED = "Y"
                SUBTRACT CRJRN-GLDISC (SUB-1) FROM WS-SETT-DISC.
            MOVE "                                " TO F-NAMEFIELD.
            MOVE "GLSETDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
                SUBTRACT CRJRN-GLAMT (SUB-1) FROM WS-INV-AMT
                GO TO FILL-GL-020.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-GL-050.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRJRN-GLDISC (SUB-1).
            ADD CRJRN-GLDISC (SUB-1) TO WS-SETT-DISC.
 
           IF F-EXIT-CH = X"1D" 
            IF CRJRN-GLACC (SUB-1) = " "
               GO TO FILL-GL-090.

            IF CRJRN-GLAMT (SUB-1) > 0
             IF WS-SETT-DISC > CRJRN-SETT-DISC
                MOVE "THIS AMOUNT IS MORE THAN THE ORIGINAL DISCOUNT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                SUBTRACT CRJRN-GLDISC (SUB-1) FROM WS-SETT-DISC
                MOVE 0 TO CRJRN-GLDISC (SUB-1)
                GO TO FILL-GL-050.
            IF CRJRN-GLAMT (SUB-1) < 0
             IF WS-SETT-DISC < CRJRN-SETT-DISC
                MOVE "THIS AMOUNT IS MORE THAN THE ORIGINAL DISCOUNT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                SUBTRACT CRJRN-GLDISC (SUB-1) FROM WS-SETT-DISC
                MOVE 0 TO CRJRN-GLDISC (SUB-1)
                GO TO FILL-GL-050.
            MOVE CRJRN-GLDISC (SUB-1) TO F-EDNAMEFIELDSALE.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
       FILL-GL-090.
           IF F-INDEX < 10
               ADD 1 TO SUB-1 F-INDEX.
           MOVE "N" TO WS-LINE-CHANGED.
           GO TO FILL-GL-010.
       FILL-GL-900.
            IF WS-INV-AMT NOT = CRJRN-LOC-AMT
                MOVE
                "THE ALLOCATED AMT'S DO NOT = THE ORIGINAL INVOICE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-GL-010.
            IF WS-SETT-DISC NOT = CRJRN-SETT-DISC
                MOVE
                "THIS ALLOCATED DISC'S DO NOT = THE ORIGINAL DISCOUNT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-GL-010.
            IF WS-VAT-AMT NOT = CRJRN-VAT-AMT
                MOVE
                "THE ALLOCATED VAT AMT IS NOT = THE HEADER VAT AMT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-GL-010.

               PERFORM REWRITE-TRANS-RECORD.
       FILL-GL-999.
           EXIT.
      *
       CHECK-GL-SHORT-NO SECTION.
       C-GL-SH-N-010.
           MOVE 1 TO SUB-2.
       C-GL-SH-N-020.
           IF GLPA-IND (SUB-2) = " "
                GO TO C-GL-SH-N-999.
           IF GLPA-IND (SUB-2) = F-NAMEFIELD
                MOVE GLPA-NO (SUB-2) TO F-NAMEFIELD
                GO TO C-GL-SH-N-999.
           IF SUB-2 NOT > 9
                ADD 1 TO SUB-2
                GO TO C-GL-SH-N-020.
       C-GL-SH-N-999.
           EXIT.
      *
       READ-GLPARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO PARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       CANCEL-TRANSACTION SECTION.
       CAN-005.
           COMPUTE SUB-2 = SUB-1 + 1.
       CAN-010.
           IF SUB-2 > 10
               GO TO CAN-090.
           IF CRJRN-GLACC (SUB-2) = " "
                 MOVE " " TO CRJRN-GLACC (SUB-1)
                             CRJRN-GLDESC (SUB-1)
                 MOVE 0   TO CRJRN-GLAMT (SUB-1)
                             CRJRN-GLDISC (SUB-1)
                 GO TO CAN-090.
             MOVE CRJRN-GL-DATA (SUB-2) TO CRJRN-GL-DATA (SUB-1).
             ADD 1 TO SUB-1 SUB-2.
             GO TO CAN-010.
       CAN-090.
             MOVE " " TO CRJRN-GLACC (SUB-1)
                         CRJRN-GLDESC (SUB-1).
             MOVE 0   TO CRJRN-GLAMT (SUB-1)
                         CRJRN-GLDISC (SUB-1).
       CAN-999.
             EXIT.
      *
       SCROLL-NEXT SECTION.
       NEXT-000.
            MOVE 1 TO SUB-1 F-INDEX. 
            PERFORM CLEAR-BODY.
            MOVE 1 TO F-INDEX. 
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 10
                GO TO NEXT-030.
            IF F-INDEX < 11
                GO TO NEXT-010.
       NEXT-030.
            MOVE 1 TO SUB-1 F-INDEX. 
       NEXT-999.
             EXIT.
      *
       SCROLLING SECTION.
       SCROLL-010.
            MOVE "GLNUMBER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CRJRN-GLACC (SUB-1) TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-015.
            MOVE "GLAMOUNT" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            IF CRJRN-GLACC (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-020.
            MOVE CRJRN-GLAMT (SUB-1) TO F-EDNAMEFIELDSALE.
            PERFORM WRITE-FIELD-SALE.
       SCROLL-016.
            MOVE "GLSETDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            IF CRJRN-GLACC (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-020.
            MOVE CRJRN-GLDISC (SUB-1) TO F-EDNAMEFIELDSALE.
            PERFORM WRITE-FIELD-SALE.
       SCROLL-020.
            MOVE "GLNAME" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CRJRN-GLDESC (SUB-1) TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO CRJRN-REFERENCE
                         CRJRN-FUTURE
                         CRJRN-INV-NO
                         CRJRN-DNOTE-NO
                         CRJRN-COMPLETE.
             MOVE 0   TO CRJRN-TRANS
                         CRJRN-TYPE
                         CRJRN-NO
                         CRJRN-CRACC-NUMBER
                         CRJRN-INV-DATE
                         CRJRN-DUE-DATE
                         CRJRN-LOC-AMT
                         CRJRN-VAT-AMT
                         CRJRN-UNAPPLIED-AMT
                         CRJRN-FOR-AMT
                         CRJRN-EXCHANGE
                         CRJRN-SETT-DISC.
             MOVE WS-REFERENCE TO CRJRN-REFERENCE.
             MOVE WS-TRANSNO   TO CRJRN-TRANS.
             MOVE WS-TYPE      TO CRJRN-TYPE.
       CLSC-999.
             EXIT.      
      *
       REWRITE-TRANS-RECORD SECTION.
       ROR-010.
            IF NEW-ORDER = "Y"
               GO TO ROR-020.
            REWRITE CRJRN-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CRJRN-ST1 NOT = 0
                MOVE 0 TO WS-CRJRN-ST1
                MOVE "CRTRANS BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO ROR-010.
            GO TO ROR-999.
       ROR-020.
            WRITE CRJRN-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CRJRN-ST1 NOT = 0
                MOVE 0 TO WS-CRJRN-ST1
                MOVE "CRTRANS RECORD BUSY ON WRITE,  'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO ROR-020.
       ROR-999.
            EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE CRJRN-REFERENCE TO WS-REFERENCE.
           MOVE CRJRN-TRANS     TO WS-TRANSNO.
           MOVE CRJRN-TYPE      TO WS-TYPE.
           START CRJRN-FILE KEY NOT < CRJRN-KEY.
       RO-010.
           READ CRJRN-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-CRJRN-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-CRJRN-ST1 NOT = 0
                MOVE 0 TO WS-CRJRN-ST1
                MOVE "CRTRANS BUSY ON READ, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RO-010.
           MOVE "N" TO NEW-ORDER.
           MOVE CRJRN-REFERENCE TO WS-REFERENCE.
           MOVE CRJRN-TRANS     TO WS-TRANSNO.
           MOVE CRJRN-TYPE      TO WS-TYPE.
       RO-999.
           EXIT.
     *
       READ-CREDITOR SECTION.
       RD-000.
           MOVE CRJRN-CRACC-NUMBER TO CR-ACCOUNT-NUMBER.
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
           MOVE WS-REFERENCE TO CRJRN-REFERENCE.
           MOVE WS-TRANSNO   TO CRJRN-TRANS.
           MOVE WS-TYPE      TO CRJRN-TYPE.
           START CRJRN-FILE KEY NOT < CRJRN-KEY.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-001.
           MOVE 0 TO WS-CRJRN-ST1.
       RONX-005. 
           READ CRJRN-FILE NEXT WITH LOCK
            AT END
              MOVE 0   TO CRJRN-TRANS
                          CRJRN-TYPE
                          WS-TRANSNO
                          WS-TYPE
              MOVE " " TO WS-REFERENCE
                          CRJRN-REFERENCE
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 
               "CR-JRN BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
               MOVE 0 TO WS-CRJRN-ST1
               PERFORM START-TRANS
               GO TO RONX-005.
           MOVE CRJRN-REFERENCE TO WS-REFERENCE.
           MOVE CRJRN-TRANS     TO WS-TRANSNO.
           MOVE CRJRN-TYPE      TO WS-TYPE.
           MOVE "N"             TO NEW-ORDER.
       RONX-999.
           EXIT.
      *
       READ-TRANS-PREVIOUS SECTION.
       RDPREV-001.
           MOVE 0 TO WS-CRJRN-ST1.
       RDPREV-005. 
           READ CRJRN-FILE PREVIOUS WITH LOCK
            AT END
              MOVE 0   TO CRJRN-TRANS
                          CRJRN-TYPE
                          WS-TRANSNO
                          WS-TYPE
              MOVE " " TO WS-REFERENCE
                          CRJRN-REFERENCE
              MOVE "Y" TO WS-END
            MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDPREV-999.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 
               "CR-JRN BUSY ON READ-PREVIOUS, IN 1 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
               MOVE 0 TO WS-CRJRN-ST1
               PERFORM START-TRANS
               GO TO RDPREV-005.
           MOVE CRJRN-REFERENCE TO WS-REFERENCE.
           MOVE CRJRN-TRANS     TO WS-TRANSNO.
           MOVE CRJRN-TYPE      TO WS-TYPE.
           MOVE "N"             TO NEW-ORDER.
       RDPREV-999.
           EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-000.
            MOVE 0 TO F-INDEX.
       CLEAR-002.
            ADD 1 TO F-INDEX.
            IF F-INDEX > 10
               GO TO CLEAR-BODY-999.

            MOVE "GLNUMBER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLAMOUNT" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLSETDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLNAME" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            GO TO CLEAR-002.
       CLEAR-BODY-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CRJRN-FILE
            IF WS-CRJRN-ST1 NOT = 0
               MOVE 0 TO WS-CRJRN-ST1
               MOVE "CR-JRN FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-008.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "GL-MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-008.
       OPEN-011.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0 
              MOVE 0 TO WS-GLMAST-ST1
              MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-011.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-012.

           PERFORM READ-GLPARAMETER.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrJrnMt"       TO F-FORMNAME
           MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CRJRN-FILE
                 CREDITOR-MASTER
                 GL-MASTER
                 GLPARAMETER-FILE.
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
       Copy "ComputeCRDatePeriod".
       Copy "EnterCRPeriodDates".
       Copy "NumberCheck".
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
