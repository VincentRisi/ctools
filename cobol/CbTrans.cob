        IDENTIFICATION DIVISION.
        PROGRAM-ID. CbTrans.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectCbMaster".
        Copy "SelectCbTrans".
        Copy "SelectGlMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCbMast.
           COPY ChlfdCbTrans.
           COPY ChlfdGlMast.

       WORKING-STORAGE SECTION.
       77  NEW-ORDER          PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-REFERENCE       PIC X(10) VALUE " ".      
       77  WS-TRANSNO         PIC 9(6) VALUE 0.
       77  WS-TYPE            PIC 99 VALUE 0.
       01  WS-CBTRANS-STATUS.
           03  WS-CBTRANS-ST1  PIC 99.
       01  WS-CB-STATUS.
           03  WS-CB-ST1       PIC 99.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1   PIC 99.
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
            MOVE " " TO CBTRANS-REC.
            MOVE "N" TO NEW-ORDER
                        WS-END.
        GET-002.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TRANSNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CBTRANS-TRANS F-EDNAMEFIELDNUM.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-002.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TYPE    TO CBTRANS-TYPE
                 MOVE WS-TRANSNO TO CBTRANS-TRANS
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
                 PERFORM READ-CB
              IF WS-END NOT = "Y"
                 PERFORM GET-004
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF CBTRANS-TRANS NOT > 0
                 MOVE
                  "PLEASE ENTER A TRANS No IDENTIFIER, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
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
            MOVE NUMERIC-RATE TO CBTRANS-TYPE.
            IF F-EXIT-CH = X"01"
                 GO TO GET-002.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TYPE    TO CBTRANS-TYPE
                 MOVE WS-TRANSNO TO CBTRANS-TRANS
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
                 PERFORM READ-CB
              IF WS-END NOT = "Y"
                 PERFORM GET-004
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
                 PERFORM READ-CB
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
                 GO TO GET-003.
            IF CBTRANS-TYPE NOT > 0
                 MOVE "PLEASE ENTER A TYPE IDENTIFIER, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-003.
            PERFORM READ-TRANS.
            IF NEW-ORDER = "Y"
                GO TO GET-999.
            PERFORM READ-CB.
            IF F-EXIT-CH NOT = X"0A"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-003.
        GET-004.
            MOVE "TRANSNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CBTRANS-TRANS TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE CBTRANS-TYPE TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.
        GET-005.
            MOVE "REFR" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE CBTRANS-REFERENCE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FUTURE" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CBTRANS-FUTURE TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PERIOD" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CBTRANS-NO TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "DATE"       TO F-FIELDNAME.
            MOVE 4            TO F-CBFIELDNAME.
            MOVE CBTRANS-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE-OF-POST" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE CBTRANS-TYPE-OF-POST TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CBTRANS-CBMASTER TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNAME" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CB-DESCRIPTION TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ALLOCATED" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE CBTRANS-ALLOCATED TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLACCNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CBTRANS-ACCOUNT-NUMBER TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM READ-GLNUMBER.
            MOVE "GLACCNAME" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE GL-DESCRIPTION TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CBTRANS-AMOUNT TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "DESC" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE CBTRANS-LINE-DESC TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
               GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "REFR" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CBTRANS-REFERENCE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CBTRANS-REFERENCE = " "
               MOVE "REFERENCE CANNOT BE BLANK, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
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
               PERFORM ERROR-000
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-002.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "FUTURE" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CBTRANS-FUTURE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
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
               PERFORM ERROR-000
               GO TO FILL-002.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-002.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-005.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PERIOD" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CBTRANS-NO
                                 F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-002.
            IF CBTRANS-NO NOT > 0
             IF CBTRANS-NO NOT < 13
               MOVE "THE PERIOD MUST BE > 0  AND < 13, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-005.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
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
               PERFORM ERROR-000
               GO TO FILL-005.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-005.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-010.
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
               GO TO FILL-010.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CBTRANS-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
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
               PERFORM ERROR-000
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-010.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-011.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TYPE-OF-POST" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO F-NAMEFIELD
                                CBTRANS-TYPE-OF-POST.
            PERFORM WRITE-FIELD-ALPHA.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF CBTRANS-TYPE-OF-POST NOT = "C" AND NOT = "D"
                                AND NOT = "G" AND NOT = "S"
              MOVE
            "THE FIELD MUST BE EITHER 'C' 'D' 'G' OR 'S', RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-011.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
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
               PERFORM ERROR-000
               GO TO FILL-011.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-011.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-012.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF ALPHA-RATE > SPACES
               PERFORM NUMBER-CHECK
               MOVE WS-GLNO-CHECK TO CBTRANS-CBMASTER
            IF SIGN-FOUND = 9
               MOVE 0 TO SIGN-FOUND
               GO TO FILL-012.
            MOVE CBTRANS-CBMASTER TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-011.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
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
               PERFORM ERROR-000
               GO TO FILL-012.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-012.

            PERFORM READ-CB.
            IF CB-DESCRIPTION = " "
               MOVE "ENTER AN EXISTING ACCOUNT NUMBER" TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-012.

            MOVE "ACCNAME" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CB-DESCRIPTION TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-013.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ALLOCATED" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO F-NAMEFIELD
                                CBTRANS-ALLOCATED.
            PERFORM WRITE-FIELD-ALPHA.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-012.
            IF CBTRANS-ALLOCATED NOT = "N" AND NOT = "Y"
                             AND NOT = "H"
               MOVE
            "THE FIELD MUST BE EITHER 'H' 'N' OR 'Y', RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-013.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
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
               PERFORM ERROR-000
               GO TO FILL-013.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-013.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-014.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "GLACCNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF ALPHA-RATE > SPACES
               PERFORM NUMBER-CHECK
               MOVE WS-GLNO-CHECK TO CBTRANS-ACCOUNT-NUMBER
            IF SIGN-FOUND = 9
               MOVE 0 TO SIGN-FOUND
               GO TO FILL-012.
            MOVE CBTRANS-ACCOUNT-NUMBER TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-013.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
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
               PERFORM ERROR-000
               GO TO FILL-014.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-014.

            PERFORM READ-GLNUMBER.
            IF GL-DESCRIPTION = " "
               MOVE "THIS MUST BE AN EXISTING GL-ACCOUNT NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-014.

            MOVE "GLACCNAME" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE GL-DESCRIPTION TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "AMOUNT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CBTRANS-AMOUNT.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-014.
            IF CBTRANS-AMOUNT = 0
               MOVE "THIS ENTRY CANNOT BE ZERO, RE-ENTER." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-015.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
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
               GO TO FILL-015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-015.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DESC" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CBTRANS-LINE-DESC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CB
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF CBTRANS-LINE-DESC = " "
               MOVE "THIS ENTRY CANNOT BE BLANK, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-015.
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
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       DELETE-TRANS SECTION.
       DO-000.
            IF NEW-ORDER = "Y"
                GO TO DO-999.
       DO-010.
            DELETE CBTRANS-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO DO-010.
       DO-999.
           EXIT.
      *
       REWRITE-TRANS-RECORD SECTION.
       ROR-010.
            IF NEW-ORDER = "Y"
               GO TO ROR-020.
            REWRITE CBTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO ROR-010.
            GO TO ROR-999.
       ROR-020.
            WRITE CBTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO ROR-020.
       ROR-999.
            EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE 0 TO WS-CBTRANS-ST1.
           MOVE CBTRANS-TRANS     TO WS-TRANSNO.
           MOVE CBTRANS-TYPE      TO WS-TYPE.
           START CBTRANS-FILE KEY NOT < CBTRANS-KEY
                 INVALID KEY NEXT SENTENCE.
       RO-010.
           READ CBTRANS-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-CBTRANS-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON READ, PRESS 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO RO-010.
           MOVE "N" TO NEW-ORDER.
           MOVE CBTRANS-TRANS     TO WS-TRANSNO.
           MOVE CBTRANS-TYPE      TO WS-TYPE.
       RO-999.
           EXIT.
     *
       READ-CB SECTION.
       RD-000.
           MOVE CBTRANS-CBMASTER TO CB-NUMBER.
           START CB-MASTER KEY NOT < CB-KEY.
       RD-010.
           READ CB-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-CB-ST1
                MOVE " " TO CB-DESCRIPTION
                GO TO RD-999.
           IF WS-CB-ST1 NOT = 0
              MOVE "CASH BOOK BUSY ON READ, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-ST1
              GO TO RD-010.
       RD-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           MOVE WS-TRANSNO   TO CBTRANS-TRANS.
           MOVE WS-TYPE      TO CBTRANS-TYPE.
           START CBTRANS-FILE KEY NOT < CBTRANS-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE 0   TO CBTRANS-TRANS
                          CBTRANS-TYPE
                          WS-TRANSNO
                          WS-TYPE
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-001.
           MOVE 0 TO WS-CBTRANS-ST1.
       RONX-005.
           IF WS-END = "Y"
              GO TO RONX-999.
           READ CBTRANS-FILE NEXT WITH LOCK
            AT END NEXT SENTENCE.
           IF WS-CBTRANS-ST1  = 10
              MOVE 0   TO CBTRANS-TRANS
                          CBTRANS-TYPE
                          WS-TRANSNO
                          WS-TYPE
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON READ-NEXT, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              PERFORM START-TRANS
              GO TO RONX-005.
           MOVE CBTRANS-TRANS     TO WS-TRANSNO.
           MOVE CBTRANS-TYPE      TO WS-TYPE.
           MOVE "N" TO NEW-ORDER.
       RONX-999.
           EXIT.
      *
       READ-TRANS-PREVIOUS SECTION.
       ROPREV-001.
           MOVE 0 TO WS-CBTRANS-ST1.
       ROPREV-005.
           IF WS-END = "Y"
              GO TO ROPREV-999.
           READ CBTRANS-FILE PREVIOUS WITH LOCK
            AT END NEXT SENTENCE.
           IF WS-CBTRANS-ST1 = 10
              MOVE 0   TO CBTRANS-TRANS
                          CBTRANS-TYPE
                          WS-TRANSNO
                          WS-TYPE
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO ROPREV-999.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON READ-PREV, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              PERFORM START-TRANS
              GO TO ROPREV-005.
           MOVE CBTRANS-TRANS     TO WS-TRANSNO.
           MOVE CBTRANS-TYPE      TO WS-TYPE.
           MOVE "N" TO NEW-ORDER.
       ROPREV-999.
           EXIT.
     *
       READ-GLNUMBER SECTION.
       RD-000.
           MOVE CBTRANS-ACCOUNT-NUMBER TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       RD-010.
           READ GL-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-GLMAST-ST1
                MOVE " " TO GL-DESCRIPTION
                GO TO RD-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER BUSY ON READ, PRESS 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO RD-010.
       RD-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO CBTRANS-REFERENCE
                         CBTRANS-FUTURE
                         CBTRANS-CBMASTER
                         CBTRANS-ACCOUNT-NUMBER
                         CBTRANS-TYPE-OF-POST
                         CBTRANS-ALLOCATED
                         CBTRANS-LINE-DESC.
             MOVE 0   TO CBTRANS-TRANS
                         CBTRANS-TYPE
                         CBTRANS-NO
                         CBTRANS-DATE
                         CBTRANS-AMOUNT.
             MOVE WS-TRANSNO   TO CBTRANS-TRANS.
             MOVE WS-TYPE      TO CBTRANS-TYPE.
             IF F-EXIT-CH = X"07"
                  UNLOCK CBTRANS-FILE.
       CLSC-999.
             EXIT.      
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CBTRANS-FILE
            IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CB-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO OPEN-000.
       OPEN-008.
            OPEN I-O CB-MASTER.
            IF WS-CB-ST1 NOT = 0
              MOVE "CB-MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-ST1
              GO TO OPEN-008.
       OPEN-009.
            OPEN I-O GL-MASTER.
            IF WS-GLMAST-ST1 NOT = 0
              MOVE "GL-MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO OPEN-009.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CbTrans"       TO F-FORMNAME
           MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CBTRANS-FILE
                 CB-MASTER
                 GL-MASTER.
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
       Copy "WriteFieldRec".
       Copy "NumberCheck".
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
