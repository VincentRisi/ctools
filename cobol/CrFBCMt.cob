        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrFBCMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectCrFBCHeader".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrFBCHeader.

       WORKING-STORAGE SECTION.
       77  NEW-ORDER          PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-TYPE            PIC X(20) VALUE " ".
       01  WS-HEADER-STATUS.
           03  WS-HEADER-ST1  PIC 99.
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
            MOVE " " TO CRFOREX-REC.
            MOVE "N" TO NEW-ORDER
                        WS-END.
       GET-001.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "FBC-NUM"              TO F-FIELDNAME.
            MOVE 7                      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20                     TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD            TO CRFOREX-FBC-NUMBER.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TYPE    TO CRFOREX-FBC-NUMBER
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
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
            IF CRFOREX-FBC-NUMBER NOT > " "
                 MOVE "PLEASE ENTER AN FBC NUMBER, 'ESC' TO RE-ENTER"
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-001.
            PERFORM READ-TRANS.
            IF NEW-ORDER = "Y"
               GO TO GET-999.
        GET-004.
            MOVE "FBC-NUM"          TO F-FIELDNAME.
            MOVE 7                  TO F-CBFIELDNAME.
            MOVE CRFOREX-FBC-NUMBER TO F-NAMEFIELD.
            MOVE 20                 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "CURR-TYPE"           TO F-FIELDNAME
            MOVE 9                     TO F-CBFIELDNAME
            MOVE CRFOREX-CURRENCY-TYPE TO F-NAMEFIELD
            MOVE 5                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FORAMT"                    TO F-FIELDNAME
            MOVE 6                           TO F-CBFIELDNAME
            MOVE CRFOREX-INITIAL-FOREIGN-AMT TO F-EDNAMEFIELDFORTOTAL
            MOVE 11                          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FORTOTAL.

            MOVE "UNAPP"               TO F-FIELDNAME
            MOVE 5                     TO F-CBFIELDNAME
            MOVE CRFOREX-UNAPPLIED-AMT TO F-EDNAMEFIELDFORTOTAL
            MOVE 11                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FORTOTAL.
            
            MOVE "EXCH"                TO F-FIELDNAME
            MOVE 4                     TO F-CBFIELDNAME
            MOVE CRFOREX-EXCHANGE-RATE TO F-EDNAMEFIELDVALUE
            MOVE 9                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-VALUE.

            MOVE "LOCAL"           TO F-FIELDNAME
            MOVE 5                 TO F-CBFIELDNAME
            MOVE CRFOREX-LOCAL-AMT TO F-EDNAMEFIELDFORTOTAL
            MOVE 11                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FORTOTAL.

            MOVE "DATE"                TO F-FIELDNAME
            MOVE 4                     TO F-CBFIELDNAME
            MOVE CRFOREX-CONTRACT-DATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE          TO F-NAMEFIELD
            MOVE 10                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "MATDATE"             TO F-FIELDNAME
            MOVE 7                     TO F-CBFIELDNAME
            MOVE CRFOREX-MATURITY-DATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE          TO F-NAMEFIELD
            MOVE 10                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-005.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CURR-TYPE" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CRFOREX-CURRENCY-TYPE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CRFOREX-CURRENCY-TYPE NOT > " "
               MOVE "THIS ENTRY CANNOT BE BLANK, PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-005.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
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
            MOVE "FORAMT" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11       TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRFOREX-INITIAL-FOREIGN-AMT
                                 F-EDNAMEFIELDFORTOTAL
            PERFORM WRITE-FIELD-FORTOTAL.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CRFOREX-INITIAL-FOREIGN-AMT NOT > 0
               MOVE "THIS ENTRY CANNOT BE ZERO, PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-012.
            IF F-EXIT-CH = X"01"
               GO TO FILL-005.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
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
               GO TO FILL-012.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-012.
       FILL-020.
            IF NEW-ORDER = "Y"
               MOVE CRFOREX-INITIAL-FOREIGN-AMT TO CRFOREX-UNAPPLIED-AMT
                                                   F-EDNAMEFIELDFORTOTAL
               MOVE "UNAPP"      TO F-FIELDNAME
               MOVE 5            TO F-CBFIELDNAME
               MOVE 11           TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-FORTOTAL.
               
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "UNAPP"      TO F-FIELDNAME.
            MOVE 5            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRFOREX-UNAPPLIED-AMT
                                 F-EDNAMEFIELDFORTOTAL.
            PERFORM WRITE-FIELD-FORTOTAL.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
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
            MOVE "EXCH" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRFOREX-EXCHANGE-RATE
                                 F-EDNAMEFIELDVALUE.
            PERFORM WRITE-FIELD-VALUE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CRFOREX-EXCHANGE-RATE NOT > 0
               MOVE "THIS ENTRY CANNOT BE ZERO, PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-025.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
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
               
            COMPUTE CRFOREX-LOCAL-AMT = CRFOREX-INITIAL-FOREIGN-AMT / 
                          CRFOREX-EXCHANGE-RATE.
            MOVE "LOCAL"           TO F-FIELDNAME
            MOVE 5                 TO F-CBFIELDNAME
            MOVE CRFOREX-LOCAL-AMT TO F-EDNAMEFIELDFORTOTAL
            MOVE 11                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FORTOTAL.
       FILL-035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10     TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-035.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CRFOREX-CONTRACT-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-035.

            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
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
               GO TO FILL-035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-035.
       FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "MATDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-035.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CRFOREX-MATURITY-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-035.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
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

            GO TO FILL-005.
       FILL-999.
            EXIT.
      *
       DELETE-TRANS SECTION.
       DO-000.
            IF NEW-ORDER = "Y"
                GO TO DO-999.
       DO-010.
            DELETE CRFOREX-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-HEADER-ST1 NOT = 0
                MOVE "FOREX FBC BUSY ON DELETE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-HEADER-ST1
               GO TO DO-010.
       DO-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " "     TO CRFOREX-FBC-NUMBER
                             CRFOREX-CURRENCY-TYPE.
             MOVE 0       TO CRFOREX-INITIAL-FOREIGN-AMT
                             CRFOREX-UNAPPLIED-AMT
                             CRFOREX-EXCHANGE-RATE
                             CRFOREX-LOCAL-AMT
                             CRFOREX-CONTRACT-DATE
                             CRFOREX-MATURITY-DATE.
             MOVE WS-TYPE TO CRFOREX-FBC-NUMBER.
             IF WS-HEADER-ST1 = 51
                 UNLOCK CRFOREX-FILE.
       CLSC-999.
             EXIT.      
      *
       REWRITE-TRANS-RECORD SECTION.
       ROR-010.
            IF NEW-ORDER = "Y"
               GO TO ROR-020.
            REWRITE CRFOREX-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-HEADER-ST1 NOT = 0
                MOVE 0 TO WS-HEADER-ST1
                MOVE "FOREX FBC BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO ROR-010.
            GO TO ROR-999.
       ROR-020.
            WRITE CRFOREX-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-HEADER-ST1 NOT = 0
                MOVE 0 TO WS-HEADER-ST1
                MOVE "FOREX FBC BUSY ON WRITE,  'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO ROR-020.
       ROR-999.
            EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE CRFOREX-FBC-NUMBER TO WS-TYPE.
           START CRFOREX-FILE KEY NOT < CRFOREX-KEY.
       RO-010.
           READ CRFOREX-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-HEADER-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-HEADER-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-HEADER-ST1 NOT = 0
                MOVE 0 TO WS-HEADER-ST1
                MOVE "FOREX FBC BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RO-010.
           MOVE "N"                TO NEW-ORDER.
           MOVE CRFOREX-FBC-NUMBER TO WS-TYPE.
       RO-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           MOVE WS-TYPE TO CRFOREX-FBC-NUMBER.
           START CRFOREX-FILE KEY NOT < CRFOREX-KEY
             INVALID KEY NEXT SENTENCE.
           IF WS-HEADER-ST1 NOT = 0
             MOVE 91 TO WS-HEADER-ST1.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-005. 
           READ CRFOREX-FILE NEXT WITH LOCK
            AT END NEXT SENTENCE.
           IF WS-HEADER-ST1 = 91 OR = 10
              MOVE " "   TO CRFOREX-FBC-NUMBER
                            WS-TYPE
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           IF WS-HEADER-ST1 NOT = 0
               MOVE 0 TO WS-HEADER-ST1
                MOVE "FOREX FBC BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
               PERFORM START-TRANS
               GO TO RONX-005.
           MOVE CRFOREX-FBC-NUMBER TO WS-TYPE.
           MOVE "N" TO NEW-ORDER.
       RONX-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CRFOREX-FILE
            IF WS-HEADER-ST1 NOT = 0
               MOVE 0 TO WS-HEADER-ST1
               MOVE "FOREX FBC FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrFbcMt"       TO F-FORMNAME
           MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CRFOREX-FILE.
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
       Copy "WriteFieldForTotal".
       Copy "WriteFieldValue".
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
