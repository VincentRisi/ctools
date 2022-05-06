        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrFBCTrs.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectCrFBCHeader".
        Copy "SelectCrFBCTrans".
        Copy "SelectCrMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrFBCTrans.
           COPY ChlfdCrFBCHeader.
           COPY ChlfdCreditor.

       WORKING-STORAGE SECTION.
       77  NEW-ORDER             PIC X VALUE " ".      
       77  WS-END                PIC X VALUE " ".      
       77  WS-TYPE               PIC X(20) VALUE " ".
       77  WS-FOREIGNINVAMT      PIC 9(8)V99.
       77  WS-FBC-NUMBER         PIC X(20) VALUE " ".
       77  WS-FBC-COVER-AMT-SAVE PIC 9(8)V99.
       77  WS-FBC-COVER-AMT      PIC 9(8)V99.
       01  WS-FBCTRANS-STATUS.
           03  WS-FBCTRANS-ST1      PIC 99.
           03  WS-FBCTRANS-ST2      PIC X.
       01  WS-HEADER-STATUS.
           03  WS-HEADER-ST1     PIC 99.
           03  WS-HEADER-ST2     PIC X.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1         PIC 99.
           03  WS-CREDITOR-ST2         PIC X.
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
            MOVE " " TO CRFXTRANS-REC.
            MOVE "N" TO NEW-ORDER
                        WS-END.
       GET-001.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PO-NUM"               TO F-FIELDNAME.
            MOVE 6                      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20                     TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD            TO CRFXTRANS-PORDER-NUMBER.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TYPE    TO CRFXTRANS-PORDER-NUMBER
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
            IF CRFXTRANS-PORDER-NUMBER NOT > " "
                 MOVE "PLEASE ENTER A PO NUMBER, 'ESC' TO RE-ENTER"
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-001.
            PERFORM READ-TRANS.
            IF NEW-ORDER = "Y"
                GO TO GET-999.
        GET-004.
            MOVE "PO-NUM"                TO F-FIELDNAME
            MOVE 6                       TO F-CBFIELDNAME
            MOVE CRFXTRANS-PORDER-NUMBER TO F-NAMEFIELD
            MOVE 20                      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "FBC-NUM"            TO F-FIELDNAME
            MOVE 7                    TO F-CBFIELDNAME
            MOVE CRFXTRANS-FBC-NUMBER TO F-NAMEFIELD
            MOVE 20                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACC-NUM"            TO F-FIELDNAME
            MOVE 7                    TO F-CBFIELDNAME
            MOVE CRFXTRANS-ACC-NUMBER TO F-NAMEFIELD
            MOVE 7                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM READ-CREDITOR
            MOVE "ACCNAME" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE CR-NAME   TO F-NAMEFIELD
            MOVE 40        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CURR-TYPE"             TO F-FIELDNAME
            MOVE 9                       TO F-CBFIELDNAME
            MOVE CRFXTRANS-CURRENCY-TYPE TO F-NAMEFIELD
            MOVE 5                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FORAMT"                      TO F-FIELDNAME
            MOVE 6                             TO F-CBFIELDNAME
            MOVE CRFXTRANS-INITIAL-FOREIGN-AMT TO F-EDNAMEFIELDFORTOTAL
            MOVE 11                            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FORTOTAL.

            MOVE "UNAPP"                 TO F-FIELDNAME
            MOVE 5                       TO F-CBFIELDNAME
            MOVE CRFXTRANS-UNAPPLIED-AMT TO F-EDNAMEFIELDFORTOTAL
            MOVE 11                      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FORTOTAL.

            MOVE "FBCCOVER"              TO F-FIELDNAME
            MOVE 8                       TO F-CBFIELDNAME
            MOVE CRFXTRANS-FBC-COVER-AMT TO F-EDNAMEFIELDFORTOTAL
            MOVE 11                      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FORTOTAL.
            
            MOVE "EXCH"                  TO F-FIELDNAME
            MOVE 4                       TO F-CBFIELDNAME
            MOVE CRFXTRANS-EXCHANGE-RATE TO F-EDNAMEFIELDVALUE
            MOVE 9                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-VALUE.

            MOVE "LOCAL"             TO F-FIELDNAME
            MOVE 5                   TO F-CBFIELDNAME
            MOVE CRFXTRANS-LOCAL-AMT TO F-EDNAMEFIELDFORTOTAL
            MOVE 11                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FORTOTAL.

            MOVE "DATE"                TO F-FIELDNAME
            MOVE 4                     TO F-CBFIELDNAME
            MOVE CRFXTRANS-ORDER-DATE  TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE          TO F-NAMEFIELD
            MOVE 10                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "RECDATE"              TO F-FIELDNAME
            MOVE 7                      TO F-CBFIELDNAME
            MOVE CRFXTRANS-RECEIPT-DUE-DATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE           TO F-NAMEFIELD
            MOVE 10                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PAYDATE"              TO F-FIELDNAME
            MOVE 7                      TO F-CBFIELDNAME
            MOVE CRFXTRANS-PAY-DUE-DATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE           TO F-NAMEFIELD
            MOVE 10                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATEPAYED"            TO F-FIELDNAME
            MOVE 9                      TO F-CBFIELDNAME.
            IF CRFXTRANS-PAYMENT-DATE > 0
               MOVE CRFXTRANS-PAYMENT-DATE TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE DISPLAY-DATE           TO F-NAMEFIELD
            ELSE
               MOVE SPACES              TO F-NAMEFIELD.
            MOVE 10                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-500.
            MOVE 0 TO WS-FOREIGNINVAMT.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-600.
           MOVE "INV-NO"                 TO F-FIELDNAME
           MOVE 6                        TO F-CBFIELDNAME
           MOVE CRFXTRANS-INV-NO (SUB-1) TO F-NAMEFIELD
           MOVE 10                       TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "INV-FOR-AMT"                     TO F-FIELDNAME
           MOVE 11                                TO F-CBFIELDNAME.
           IF CRFXTRANS-INV-NO (SUB-1) > " "            
              MOVE CRFXTRANS-INV-FOREIGN-AMT (SUB-1)
                                             TO F-EDNAMEFIELDFORTOTAL
              MOVE 11                        TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-FORTOTAL
           ELSE
              MOVE SPACES                    TO F-NAMEFIELD
              MOVE 11                        TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ALPHA.
           
           ADD CRFXTRANS-INV-FOREIGN-AMT (SUB-1) TO WS-FOREIGNINVAMT
           MOVE "FORINVAMT"                      TO F-FIELDNAME
           MOVE 9                                TO F-CBFIELDNAME
           MOVE WS-FOREIGNINVAMT               TO F-EDNAMEFIELDFORTOTAL
           MOVE 11                               TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-FORTOTAL.

           MOVE "INVDATE"                  TO F-FIELDNAME
           MOVE 7                          TO F-CBFIELDNAME.
           IF CRFXTRANS-INV-NO (SUB-1) > " "            
               MOVE CRFXTRANS-INV-DATE (SUB-1) TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE DISPLAY-DATE               TO F-NAMEFIELD
           ELSE
              MOVE SPACES                      TO F-NAMEFIELD.
           MOVE 10                             TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "INVPAYDATE"               TO F-FIELDNAME
           MOVE 10                         TO F-CBFIELDNAME.
           IF CRFXTRANS-INV-PAY-DATE (SUB-1) > 0
               MOVE CRFXTRANS-INV-PAY-DATE (SUB-1) TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE DISPLAY-DATE               TO F-NAMEFIELD
           ELSE
              MOVE SPACES                      TO F-NAMEFIELD.
           MOVE 10                             TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       GET-700.
            IF SUB-1 < 10
                ADD 1 TO SUB-1 F-INDEX
                GO TO GET-600.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-005.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "FBC-NUM"   TO F-FIELDNAME.
            MOVE 7           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CRFOREX-FBC-NUMBER.
            IF F-NAMEFIELD > " "
               PERFORM READ-FOREX-HEADER
             IF WS-HEADER-ST1 = 23 OR 35 OR 49
               MOVE "THERE IS NO SUCH FOREX FEC CONTRACT, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-005.
               
            IF CRFOREX-FBC-NUMBER NOT = CRFXTRANS-FBC-NUMBER
             IF CRFXTRANS-FBC-NUMBER NOT = " "
               MOVE CRFXTRANS-FBC-NUMBER    TO WS-FBC-NUMBER
               MOVE CRFXTRANS-FBC-COVER-AMT TO WS-FBC-COVER-AMT-SAVE.
               MOVE 0                       TO CRFXTRANS-FBC-COVER-AMT
               MOVE "FBCCOVER"              TO F-FIELDNAME
               MOVE 8                       TO F-CBFIELDNAME
               MOVE CRFXTRANS-FBC-COVER-AMT TO F-EDNAMEFIELDFORTOTAL
               MOVE 11                      TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-FORTOTAL.
            MOVE F-NAMEFIELD TO CRFOREX-FBC-NUMBER
                                CRFXTRANS-FBC-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            PERFORM ERROR-020.
            
            IF F-NAMEFIELD > " "
               MOVE "CURR-TYPE"           TO F-FIELDNAME
               MOVE 9                     TO F-CBFIELDNAME
               MOVE CRFOREX-CURRENCY-TYPE TO F-NAMEFIELD
               MOVE 5                     TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               
               MOVE "EXCH"                TO F-FIELDNAME
               MOVE 4                     TO F-CBFIELDNAME
               MOVE CRFOREX-EXCHANGE-RATE TO F-EDNAMEFIELDVALUE
               MOVE 9                     TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-VALUE.
            
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
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
            MOVE "ACC-NUM" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRFXTRANS-ACC-NUMBER.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CRFXTRANS-ACC-NUMBER NOT > 0
               MOVE "THIS ENTRY CANNOT BE ZERO, PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-012.
            IF F-EXIT-CH = X"01"
               GO TO FILL-005.
            PERFORM READ-CREDITOR
            IF CR-NAME = "UNKNOWN CREDITOR"
               MOVE
            "NUMBER INCORRECT, PLEASE ENTER A VALID CREDITOR NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-012.
            PERFORM ERROR-020.
            MOVE "ACCNAME" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE CR-NAME   TO F-NAMEFIELD
            MOVE 40        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
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
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CURR-TYPE"  TO F-FIELDNAME.
            MOVE 9            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO CRFXTRANS-CURRENCY-TYPE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CRFOREX-FBC-NUMBER > " "
             IF CRFXTRANS-CURRENCY-TYPE NOT > " "
               MOVE
            "PLEASE ENTER A VALID CURRENCY NAME, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF CRFOREX-FBC-NUMBER > " "
             IF CRFXTRANS-CURRENCY-TYPE NOT = CRFOREX-CURRENCY-TYPE
               MOVE
            "NB! THE CURRENCY ENTERED IS NOT = TO THE FBC CURRENCY."
               TO WS-MESSAGE
               PERFORM ERROR-000.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
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
            MOVE "FORAMT" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11       TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRFXTRANS-INITIAL-FOREIGN-AMT
                                 F-EDNAMEFIELDFORTOTAL
            PERFORM WRITE-FIELD-FORTOTAL.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
      *      IF CRFXTRANS-INITIAL-FOREIGN-AMT NOT > 0
      *         MOVE "THIS ENTRY CANNOT BE ZERO, PLEASE RE-ENTER"
      *         TO WS-MESSAGE
      *         PERFORM ERROR-000
      *         GO TO FILL-025.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
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
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-025.
       FILL-027.
            IF NEW-ORDER = "Y"
            MOVE "UNAPP"                 TO F-FIELDNAME
            MOVE 5                       TO F-CBFIELDNAME
            MOVE CRFXTRANS-INITIAL-FOREIGN-AMT TO F-EDNAMEFIELDFORTOTAL
                                               CRFXTRANS-UNAPPLIED-AMT
            MOVE 11                      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FORTOTAL.

            IF NEW-ORDER NOT = "Y"
            COMPUTE CRFXTRANS-LOCAL-AMT =
                CRFXTRANS-INITIAL-FOREIGN-AMT/ CRFXTRANS-EXCHANGE-RATE
               MOVE "LOCAL"             TO F-FIELDNAME
               MOVE 5                   TO F-CBFIELDNAME
               MOVE CRFXTRANS-LOCAL-AMT TO F-EDNAMEFIELDFORTOTAL
               MOVE 11                  TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-FORTOTAL.

            IF F-EXIT-CH NOT = X"1D"
               GO TO FILL-029.
              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "UNAPP" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11       TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRFXTRANS-UNAPPLIED-AMT
                                 F-EDNAMEFIELDFORTOTAL
            PERFORM WRITE-FIELD-FORTOTAL.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
      *      IF CRFXTRANS-INITIAL-FOREIGN-AMT NOT > 0
      *         MOVE "THIS ENTRY CANNOT BE ZERO, PLEASE RE-ENTER"
      *         TO WS-MESSAGE
      *         PERFORM ERROR-000
      *         GO TO FILL-027.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
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
               GO TO FILL-027.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-027.
            IF F-EXIT-CH NOT = X"1D"
               GO TO FILL-029.
       FILL-028.
           IF NEW-ORDER = "Y"
              MOVE "FBCCOVER"              TO F-FIELDNAME
              MOVE 8                       TO F-CBFIELDNAME
             MOVE CRFXTRANS-INITIAL-FOREIGN-AMT TO F-EDNAMEFIELDFORTOTAL
                                                 CRFXTRANS-FBC-COVER-AMT
              MOVE 11                      TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-FORTOTAL
              MOVE                   0 TO WS-FBC-COVER-AMT
           ELSE
              MOVE CRFXTRANS-FBC-COVER-AMT TO WS-FBC-COVER-AMT.
           IF NEW-ORDER = "N"
               ADD CRFXTRANS-FBC-COVER-AMT TO CRFOREX-UNAPPLIED-AMT.

            MOVE "                    " TO F-NAMEFIELD.
            MOVE "FBCCOVER" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11       TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRFXTRANS-FBC-COVER-AMT
                                 F-EDNAMEFIELDFORTOTAL
            PERFORM WRITE-FIELD-FORTOTAL.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CRFXTRANS-FBC-COVER-AMT > CRFXTRANS-INITIAL-FOREIGN-AMT
               MOVE "THIS ENTRY CANNOT BE > ORIGINAL AMOUNT RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-028.
               
            IF CRFOREX-FBC-NUMBER > " "
             IF CRFXTRANS-FBC-COVER-AMT > CRFOREX-UNAPPLIED-AMT
                MOVE
            "THIS AMOUNT EXCEEDS THE FBC CONTRACT BALANCE, RE-ENTER"
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO FILL-028
              
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
               GO TO FILL-005.
            IF F-EXIT-CH = X"01"
               GO TO FILL-027.
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
               GO TO FILL-028.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-028.
       FILL-029.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "EXCH" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRFXTRANS-EXCHANGE-RATE
                                 F-EDNAMEFIELDVALUE
            PERFORM WRITE-FIELD-VALUE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
      *      IF CRFXTRANS-INITIAL-FOREIGN-AMT NOT > 0
      *         MOVE "THIS ENTRY CANNOT BE ZERO, PLEASE RE-ENTER"
      *         TO WS-MESSAGE
      *         PERFORM ERROR-000
      *         GO TO FILL-029.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
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
               GO TO FILL-029.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-029.
       FILL-031.
      *      IF NEW-ORDER = "Y"
               COMPUTE CRFXTRANS-LOCAL-AMT =
                CRFXTRANS-INITIAL-FOREIGN-AMT/ CRFXTRANS-EXCHANGE-RATE
               MOVE "LOCAL"             TO F-FIELDNAME
               MOVE 5                   TO F-CBFIELDNAME
               MOVE CRFXTRANS-LOCAL-AMT TO F-EDNAMEFIELDFORTOTAL
               MOVE 11                  TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-FORTOTAL.
               
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LOCAL" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRFXTRANS-LOCAL-AMT
                                 F-EDNAMEFIELDFORTOTAL
            PERFORM WRITE-FIELD-FORTOTAL.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
      *      IF CRFXTRANS-INITIAL-FOREIGN-AMT NOT > 0
      *         MOVE "THIS ENTRY CANNOT BE ZERO, PLEASE RE-ENTER"
      *         TO WS-MESSAGE
      *         PERFORM ERROR-000
      *         GO TO FILL-031.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
               GO TO FILL-005.
            IF F-EXIT-CH = X"01"
               GO TO FILL-029.
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
               GO TO FILL-031.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-031.
       FILL-035.
            IF NEW-ORDER = "Y"
               MOVE WS-DATE               TO CRFXTRANS-ORDER-DATE
               MOVE "DATE"                TO F-FIELDNAME
               MOVE 4                     TO F-CBFIELDNAME
               MOVE CRFXTRANS-ORDER-DATE  TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE DISPLAY-DATE          TO F-NAMEFIELD
               MOVE 10                    TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.
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
            MOVE SPLIT-DATE TO CRFXTRANS-ORDER-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-035.

            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
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
            MOVE "RECDATE" TO F-FIELDNAME.
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
               GO TO FILL-040.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CRFXTRANS-RECEIPT-DUE-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-040.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
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
            IF NEW-ORDER = "Y"
              PERFORM COMPUTE-PAY-DATE
               MOVE "PAYDATE"              TO F-FIELDNAME
               MOVE 7                      TO F-CBFIELDNAME
               MOVE CRFXTRANS-PAY-DUE-DATE TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE DISPLAY-DATE           TO F-NAMEFIELD
               MOVE 10                     TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.
               
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PAYDATE" TO F-FIELDNAME.
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
               GO TO FILL-045.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CRFXTRANS-PAY-DUE-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-045.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
               GO TO FILL-005.
            IF F-EXIT-CH = X"01"
               GO TO FILL-040.
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
               
            MOVE 1 TO SUB-1 F-INDEX.
       FILL-200.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INV-NO"   TO F-FIELDNAME.
            MOVE 6          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CRFXTRANS-INV-NO (SUB-1).
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
             IF SUB-1 = 1
               GO TO FILL-045
             ELSE
               SUBTRACT 1 FROM SUB-1 F-INDEX
               GO TO FILL-200.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
               GO TO FILL-005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CRFXTRANS-INV-NO (SUB-1) NOT > " "
               MOVE "THIS FIELD MAY NOT BE BLANK, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-200.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-200.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-200.
       FILL-210.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INV-FOR-AMT" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRFXTRANS-INV-FOREIGN-AMT (SUB-1)
                                 F-EDNAMEFIELDFORTOTAL
            PERFORM WRITE-FIELD-FORTOTAL.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           
           PERFORM COMPUTE-FOREIGN-INV-AMT.
           MOVE "FORINVAMT"         TO F-FIELDNAME
           MOVE 9                   TO F-CBFIELDNAME
           MOVE WS-FOREIGNINVAMT    TO F-EDNAMEFIELDFORTOTAL
           MOVE 11                  TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-FORTOTAL.
           IF WS-FOREIGNINVAMT > CRFXTRANS-INITIAL-FOREIGN-AMT
               MOVE
           "INVOICE AMT'S ENTERED EXCEEDS FOREIGN ORDER AMOUNT."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-210.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
               GO TO FILL-005.
            IF F-EXIT-CH = X"01"
               GO TO FILL-200.
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
               GO TO FILL-210.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-210.
       FILL-215.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-210.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-215.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CRFXTRANS-INV-DATE (SUB-1).
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-215.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
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
               GO TO FILL-215.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-215.
            IF F-EXIT-CH NOT = X"1D"
               GO TO FILL-500.
       FILL-220.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVPAYDATE" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-215.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-220.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CRFXTRANS-INV-PAY-DATE (SUB-1).
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-220.
      *******************************************
      * UPDATE AMT NOT PAID AND DATE LAST PAID  *
      *******************************************
            MOVE WS-CONVERT-DATE        TO CRFXTRANS-PAYMENT-DATE
            MOVE "DATEPAYED"            TO F-FIELDNAME
            MOVE 9                      TO F-CBFIELDNAME
            MOVE CRFXTRANS-PAYMENT-DATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE           TO F-NAMEFIELD
            MOVE 10                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            SUBTRACT CRFXTRANS-INV-FOREIGN-AMT (SUB-1) FROM
                     CRFXTRANS-UNAPPLIED-AMT
            MOVE "UNAPP"                 TO F-FIELDNAME
            MOVE 5                       TO F-CBFIELDNAME
            MOVE CRFXTRANS-UNAPPLIED-AMT TO F-EDNAMEFIELDFORTOTAL
            MOVE 11                      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FORTOTAL.
               
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-700
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
               GO TO FILL-220.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-220.
       FILL-500.
            IF SUB-1 < 10
               ADD 1 TO SUB-1 F-INDEX
               GO TO FILL-200.

            GO TO FILL-005.
       FILL-999.
            EXIT.
      *
       DELETE-TRANS SECTION.
       DO-000.
            IF NEW-ORDER = "Y"
                GO TO DO-999.
       DO-010.
            DELETE CRFXTRANS-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-FBCTRANS-ST1 NOT = 0
               MOVE "DELETE OF CRFXTRANS BUSY, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-FBCTRANS-ST1
               GO TO DO-010.
       DO-999.
           EXIT.
      *
       COMPUTE-FOREIGN-INV-AMT SECTION.
       CFIA-005.
            MOVE 1 TO SUB-2.
            MOVE 0 TO WS-FOREIGNINVAMT.
       CFIA-010.
            ADD CRFXTRANS-INV-FOREIGN-AMT (SUB-2) TO WS-FOREIGNINVAMT.
            IF SUB-2 < 10
               ADD 1 TO SUB-2
               GO TO CFIA-010.
       CFIA-999.
            EXIT.
      *
       COMPUTE-PAY-DATE SECTION.
       PD-005.
           MOVE CRFXTRANS-RECEIPT-DUE-DATE TO SPLIT-DATE.
           IF CR-TERMS = "1"
              ADD 1 TO SPLIT-MM.
           IF CR-TERMS = "2"
              ADD 0 TO SPLIT-MM.
           IF CR-TERMS = "3"
              ADD 0 TO SPLIT-MM.
           IF CR-TERMS = "4"
              ADD 2 TO SPLIT-MM.
           IF CR-TERMS = "5"
              ADD 3 TO SPLIT-MM.
           IF CR-TERMS = "6"
              ADD 4 TO SPLIT-MM.
           IF CR-TERMS = "7"
              ADD 3 TO SPLIT-MM.
           IF CR-TERMS = "8"
              ADD 1 TO SPLIT-MM.
           IF CR-TERMS = "9"
              ADD 1 TO SPLIT-MM.
       PD-020.
           IF SPLIT-MM > 12
             COMPUTE SPLIT-MM = SPLIT-MM - 12
             ADD 1 TO SPLIT-YY.
           MOVE SPLIT-DATE TO CRFXTRANS-PAY-DUE-DATE.
       PD-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
           MOVE " "     TO CRFXTRANS-PORDER-NUMBER
                           CRFXTRANS-FBC-NUMBER
                           CRFXTRANS-CURRENCY-TYPE.
           MOVE 0       TO CRFXTRANS-ACC-NUMBER
                           CRFXTRANS-INITIAL-FOREIGN-AMT
                           CRFXTRANS-UNAPPLIED-AMT
                           CRFXTRANS-EXCHANGE-RATE
                           CRFXTRANS-LOCAL-AMT
                           CRFXTRANS-ORDER-DATE
                           CRFXTRANS-RECEIPT-DUE-DATE
                           CRFXTRANS-PAY-DUE-DATE.
           MOVE 1 TO SUB-1.
       CLSC-010.
           MOVE " " TO CRFXTRANS-INV-NO (SUB-1)
           MOVE 0   TO CRFXTRANS-INV-FOREIGN-AMT (SUB-1)
                       CRFXTRANS-INV-DATE (SUB-1)
                       CRFXTRANS-INV-PAY-DATE (SUB-1).
           IF SUB-1 < 10
              ADD 1 TO SUB-1
              GO TO CLSC-010.
           MOVE WS-TYPE TO CRFXTRANS-PORDER-NUMBER.
            IF WS-FBCTRANS-ST1 NOT = 51
               UNLOCK CRFXTRANS-FILE.
           IF WS-HEADER-ST1 NOT = 51
               UNLOCK CRFOREX-FILE.
       CLSC-999.
           EXIT.      
     *
       READ-CREDITOR SECTION.
       RD-000.
           MOVE CRFXTRANS-ACC-NUMBER TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-KEY.
       RD-010.
           READ CREDITOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN CREDITOR" TO CR-NAME
                GO TO RD-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITORS BUSY ON READ, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.
       RD-999.
           EXIT.
      *
       REWRITE-TRANS-RECORD SECTION.
       ROR-010.
            IF NEW-ORDER = "Y"
               GO TO ROR-020.
            REWRITE CRFXTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-FBCTRANS-ST1 NOT = 0
                MOVE 0 TO WS-FBCTRANS-ST1
                MOVE "FBC-TRANS BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO ROR-010.
            GO TO ROR-900.
       ROR-020.
            WRITE CRFXTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-FBCTRANS-ST1 NOT = 0
                MOVE 0 TO WS-FBCTRANS-ST1
                MOVE "FBC-TRANS BUSY ON WRITE,  'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO ROR-020.
       ROR-900.
            IF CRFOREX-FBC-NUMBER > " "
               PERFORM SUBTRACT-FBC-COVER-AMOUNT.
               
            IF CRFOREX-FBC-NUMBER NOT = WS-FBC-NUMBER
             IF WS-FBC-NUMBER NOT = " "
               MOVE WS-FBC-NUMBER           TO CRFOREX-FBC-NUMBER
               PERFORM READ-FOREX-HEADER
               ADD WS-FBC-COVER-AMT-SAVE    TO CRFOREX-UNAPPLIED-AMT
               PERFORM SFCA-010.
       ROR-999.
            EXIT.
      *
       READ-FOREX-HEADER SECTION.
       RFH-000.
           START CRFOREX-FILE KEY NOT < CRFOREX-KEY.
       RFH-010.
           READ CRFOREX-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-HEADER-ST1 = 23 OR 35 OR 49
                MOVE 23 TO WS-HEADER-ST1
                GO TO RFH-999.
           IF WS-HEADER-ST1 NOT = 0
                MOVE "FOREX HEADER BUSY ON READ, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RFH-010.
       RFH-999.
           EXIT.
      *
       SUBTRACT-FBC-COVER-AMOUNT SECTION.
       SFCA-000.
           IF WS-FBC-COVER-AMT > CRFXTRANS-FBC-COVER-AMT
              COMPUTE CRFOREX-UNAPPLIED-AMT = CRFOREX-UNAPPLIED-AMT
                  + (WS-FBC-COVER-AMT - CRFXTRANS-FBC-COVER-AMT).
           IF WS-FBC-COVER-AMT < CRFXTRANS-FBC-COVER-AMT
              COMPUTE CRFOREX-UNAPPLIED-AMT = CRFOREX-UNAPPLIED-AMT
                  - (CRFXTRANS-FBC-COVER-AMT - WS-FBC-COVER-AMT).
       SFCA-010.
           REWRITE CRFOREX-REC
                 INVALID KEY NEXT SENTENCE.
           IF WS-HEADER-ST1 = 23 OR 35 OR 49
                MOVE "NO SUCH FOREX HEADER @ REWRITE, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO SFCA-999.
           IF WS-HEADER-ST1 NOT = 0
                MOVE "FOREX HEADER BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO SFCA-010.
       SFCA-999.
           EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE CRFXTRANS-PORDER-NUMBER TO WS-TYPE.
           START CRFXTRANS-FILE KEY NOT < CRFXTRANS-KEY.
       RO-010.
           READ CRFXTRANS-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-FBCTRANS-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-FBCTRANS-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-FBCTRANS-ST1 NOT = 0
                MOVE 0 TO WS-FBCTRANS-ST1
                MOVE "FBC-TRANS BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RO-010.
           MOVE "N"                TO NEW-ORDER.
           MOVE CRFXTRANS-PORDER-NUMBER TO WS-TYPE.
       RO-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           MOVE WS-TYPE TO CRFXTRANS-PORDER-NUMBER.
           START CRFXTRANS-FILE KEY NOT < CRFXTRANS-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-FBCTRANS-ST1 NOT = 0
              MOVE "CRFXTRANS FILE BUSY ON START, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE " " TO CRFXTRANS-PORDER-NUMBER
              GO TO ST-OO-000.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-001.
           MOVE 0 TO WS-FBCTRANS-ST1.
       RONX-005. 
           READ CRFXTRANS-FILE NEXT WITH LOCK
            AT END
              MOVE " "   TO CRFXTRANS-PORDER-NUMBER
                            WS-TYPE
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           IF WS-FBCTRANS-ST1 NOT = 0
               MOVE 0 TO WS-FBCTRANS-ST1
               MOVE "FBC-TRANS FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM START-TRANS
               GO TO RONX-005.
           MOVE CRFXTRANS-PORDER-NUMBER TO WS-TYPE.
           MOVE "N" TO NEW-ORDER.
       RONX-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CRFXTRANS-FILE
            IF WS-FBCTRANS-ST1 NOT = 0
               MOVE 0 TO WS-FBCTRANS-ST1
               MOVE "FBC-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-005.
            OPEN I-O CRFOREX-FILE
            IF WS-FBCTRANS-ST1 NOT = 0
               MOVE 0 TO WS-FBCTRANS-ST1
               MOVE "FOREX FBC FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-008.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CR-MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-008.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrFBCTrs"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CRFXTRANS-FILE
                 CRFOREX-FILE
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
       Copy "WriteFieldForTotal".
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
