        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrCAMSMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrCAMSTrans".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrCAMSTrans.
           
       WORKING-STORAGE SECTION.
       77  NEW-CAMSNO         PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-CHEQUENUM       PIC 9(6) VALUE 0.
       77  WS-INQUIRY-PROGRAM PIC X(8) VALUE "CrNameIq".
       77  WS-CHEQUE-SAVE     PIC 9(6) VALUE 0.
       77  WS-ALL-ENTERED     PIC X VALUE " ".
       01  WS-CRCAMSTRANS-STATUS.
           03  WS-CRCAMSTRANS-ST1   PIC 99.
       Copy "WsDateInfo".
      **************************************************************
      *                  FORMS WORK FIELDS                         *
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN.
       CONT-010.
           PERFORM DISPLAY-FORM
           PERFORM GET-DATA
           PERFORM FILL-DATA
           GO TO CONT-010.
      *
        GET-DATA SECTION.
        GET-000.
            MOVE 0   TO CR-CAMS-TRANS-REC.
            MOVE "N" TO NEW-CAMSNO
                        WS-ALL-ENTERED
                        WS-END.
        GET-001.              
            MOVE "TRANSNUM"     TO F-FIELDNAME
            MOVE 8              TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 6              TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD    TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE   TO CR-CAMS-TRANS-NUM.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-CHEQUE-SAVE TO CR-CAMS-TRANS-NUM
                 PERFORM START-CAMS
                 PERFORM READ-CAMS-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-CAMS-PREVIOUS
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF CR-CAMS-TRANS-NUM = 0
                 CLOSE CR-CAMS-TRANS-FILE
                 PERFORM CLEAR-SCREEN
                 CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                 CANCEL WS-INQUIRY-PROGRAM
                 PERFORM CLEAR-SCREEN
                 PERFORM OPEN-000
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
            PERFORM READ-CAMS.
            IF NEW-CAMSNO = "Y"
                PERFORM CLSC-010
                GO TO GET-999.
            GO TO GET-005.
        GET-003.
            MOVE "TRANSNUM"        TO F-FIELDNAME
            MOVE 8                 TO F-CBFIELDNAME
            MOVE CR-CAMS-TRANS-NUM TO F-NAMEFIELD
            MOVE 6                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "ACCNUM"                 TO F-FIELDNAME
            MOVE 6                        TO F-CBFIELDNAME
            MOVE CR-CAMS-TRANS-ACC-NUMBER TO F-NAMEFIELD
            MOVE 7                        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "NAME"                 TO F-FIELDNAME
            MOVE 4                      TO F-CBFIELDNAME
            MOVE CR-CAMS-TRANS-ACC-NAME TO F-NAMEFIELD
            MOVE 20                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CREDNUM"              TO F-FIELDNAME
            MOVE 7                      TO F-CBFIELDNAME
            MOVE CR-CAMS-TRANS-CRED-NUM TO F-NAMEFIELD
            MOVE 10                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CHEQUENUM"              TO F-FIELDNAME
            MOVE 9                        TO F-CBFIELDNAME
            MOVE CR-CAMS-TRANS-CHEQUE-NUM TO F-NAMEFIELD
            MOVE 10                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"             TO F-FIELDNAME
            MOVE 4                  TO F-CBFIELDNAME
            MOVE CR-CAMS-TRANS-DATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE       TO F-NAMEFIELD
            MOVE 10                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT"             TO F-FIELDNAME
            MOVE 6                    TO F-CBFIELDNAME
            MOVE CR-CAMS-TRANS-AMOUNT TO F-EDNAMEFIELDNUM6
            MOVE 11                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "BANKNAME"              TO F-FIELDNAME
            MOVE 8                       TO F-CBFIELDNAME
            MOVE CR-CAMS-TRANS-BANK-NAME TO F-NAMEFIELD
            MOVE 20                      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BANKNUM"              TO F-FIELDNAME
            MOVE 7                      TO F-CBFIELDNAME
            MOVE CR-CAMS-TRANS-BANK-NUM TO F-NAMEFIELD
            MOVE 11                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BRANCHNUM"              TO F-FIELDNAME
            MOVE 9                        TO F-CBFIELDNAME
            MOVE CR-CAMS-TRANS-BRANCH-NUM TO F-NAMEFIELD
            MOVE 6                        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACC-TYPE"              TO F-FIELDNAME
            MOVE 8                       TO F-CBFIELDNAME
            MOVE CR-CAMS-TRANS-CRED-TYPE TO F-NAMEFIELD
            MOVE 1                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PAID"             TO F-FIELDNAME
            MOVE 4                  TO F-CBFIELDNAME
            MOVE CR-CAMS-TRANS-PAID TO F-NAMEFIELD
            MOVE 1                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                 GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCNUM"     TO F-FIELDNAME
            MOVE 6            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 7            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO CR-CAMS-TRANS-ACC-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-CAMS-TRANS-ACC-NUMBER = 0
               MOVE "THIS FIELD MAY NOT BE ZERO, ENTER A VALID ACC."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-001.
            IF NEW-CAMSNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-150.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CR-CAMS-TRANS-REC
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
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NAME"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 20          TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO CR-CAMS-TRANS-ACC-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-CAMS-TRANS-ACC-NAME = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A VALID NAME."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-010.
            IF NEW-CAMSNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CR-CAMS-TRANS-REC
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
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CREDNUM"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO CR-CAMS-TRANS-CRED-NUM.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-CAMS-TRANS-CRED-NUM = " "
             MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A VALID CRED-NUM."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-015.
            IF NEW-CAMSNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-015.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-015.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CHEQUENUM"   TO F-FIELDNAME.
            MOVE 9             TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD   TO CR-CAMS-TRANS-CHEQUE-NUM
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-CAMS-TRANS-CHEQUE-NUM = " "
             MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A VALID CHEQUE."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-020.
            IF NEW-CAMSNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
       FILL-102.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE"       TO F-FIELDNAME
            MOVE 4            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-102.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CR-CAMS-TRANS-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-102.

            IF NEW-CAMSNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-102.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-102.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-102.
       FILL-115.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "AMOUNT"   TO F-FIELDNAME.
            MOVE 6          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 CR-CAMS-TRANS-AMOUNT.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF NEW-CAMSNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-115.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-102.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-115.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-115.
       FILL-135.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BANKNAME"     TO F-FIELDNAME.
            MOVE 8              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-CAMS-TRANS-BANK-NAME.
            IF CR-CAMS-TRANS-BANK-NAME = " "
             MOVE "PLEASE ENTER A VALID BANK NAME, PLEASE RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-135.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF NEW-CAMSNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-135.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-115.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-135.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-135.
       FILL-140.
            MOVE 
            "ACC NUMBER MUST BE 11 CHARS, PAD WITH ZERO'S ON THE LEFT."
              TO WS-MESSAGE
              PERFORM ERROR1-000.
       
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BANKNUM"    TO F-FIELDNAME
            MOVE 7            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 11           TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO CR-CAMS-TRANS-BANK-NUM.

            PERFORM ERROR1-020.

            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-NAMEFIELD NOT > " "
               MOVE "THIS FIELD MUST BE A VALID BANK ACCOUNT, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-140.
            IF NEW-CAMSNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-140.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-135.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-140.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-140.
       FILL-145.
            MOVE 
            "BRANCH NUM MUST BE 6 CHARS, PAD WITH ZERO'S ON THE LEFT."
              TO WS-MESSAGE
              PERFORM ERROR1-000.
       
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BRANCHNUM"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 6             TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD   TO CR-CAMS-TRANS-BRANCH-NUM.

            PERFORM ERROR1-020.

            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-NAMEFIELD NOT > " "
               MOVE "THIS FIELD MUST BE A VALID BRANCH NUM, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-145.
            IF NEW-CAMSNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-145.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-140.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-145.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-145.
       FILL-147.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACC-TYPE"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 1           TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDFAX
                                 CR-CAMS-TRANS-CRED-TYPE
            PERFORM WRITE-FIELD-FAX.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-CAMS-TRANS-CRED-TYPE NOT = 1 AND NOT = 2
               MOVE "1=CURRENT ACCOUNT, 2=SAVINGS ACCOUNT, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-147.
            IF NEW-CAMSNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-147.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-145.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-147.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-147.
       FILL-150.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PAID"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 1           TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-CAMS-TRANS-PAID.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-CAMS-TRANS-PAID NOT = "Y" AND NOT = "N" AND NOT = "P"
               MOVE "THIS FIELD MUST BE EITHER 'Y, N OR P', RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-150.
               
            MOVE "Y" TO WS-ALL-ENTERED.
            PERFORM ERROR1-020.

            IF NEW-CAMSNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-150.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM READ-CAMS-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-147.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CR-CAMS-TRANS-REC
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-150.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-150.
               
            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       DELETE-CR-CAMS-TRANS-REC SECTION.
       DCR-CAMS-TRANS-000.
            IF NEW-CAMSNO = "Y"
                GO TO DCR-CAMS-TRANS-999.
       DCR-CAMS-TRANS-010.
            DELETE CR-CAMS-TRANS-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-CRCAMSTRANS-ST1 NOT = 0
               MOVE "CRCAMS BUSY ON DELETE, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CRCAMSTRANS-ST1
               GO TO DCR-CAMS-TRANS-010.
       DCR-CAMS-TRANS-999.
           EXIT.
      *
       RELEASE-CR-CAMS-TRANS-REC SECTION.
       REL-000.
           UNLOCK CR-CAMS-TRANS-FILE.
       REL-999.
           EXIT.
      *
       REWRITE-CR-CAMS-TRANS-REC SECTION.
       RCR-CAMS-TRANS-010.
            IF NEW-CAMSNO = "Y"
               GO TO RCR-CAMS-TRANS-020.
            REWRITE CR-CAMS-TRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CRCAMSTRANS-ST1 NOT = 0
                MOVE "CAMS RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCR-CAMS-TRANS-010.
            GO TO RCR-CAMS-TRANS-999.
       RCR-CAMS-TRANS-020.
            WRITE CR-CAMS-TRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CRCAMSTRANS-ST1 NOT = 0
                MOVE "CAMS RECORD BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCR-CAMS-TRANS-020.
       RCR-CAMS-TRANS-999.
            EXIT.
      *
       READ-CAMS SECTION.
       RD-000.
           MOVE CR-CAMS-TRANS-NUM TO WS-CHEQUENUM.
           START CR-CAMS-TRANS-FILE KEY NOT < CR-CAMS-TRANS-NUM.
       RD-010.
           READ CR-CAMS-TRANS-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-CRCAMSTRANS-ST1 = 23 OR 35 OR 49
                MOVE "Y" TO NEW-CAMSNO
                PERFORM CLEAR-FORM
                MOVE WS-CHEQUENUM TO CR-CAMS-TRANS-NUM
                GO TO RD-999.
           IF WS-CRCAMSTRANS-ST1 NOT = 0
                MOVE 0 TO WS-CRCAMSTRANS-ST1
                MOVE "CRCAMS BUSY ON READ-LOCK, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.
           MOVE "N" TO NEW-CAMSNO.
           MOVE CR-CAMS-TRANS-NUM TO WS-CHEQUE-SAVE.
       RD-999.
             EXIT.
      *
       START-CAMS SECTION.
       ST-CR-CAMS-TRANS-000.
              MOVE WS-CHEQUENUM TO CR-CAMS-TRANS-NUM.
              START CR-CAMS-TRANS-FILE KEY NOT < CR-CAMS-TRANS-NUM.
       ST-CR-CAMS-TRANS-999.
             EXIT.
      *
       READ-CAMS-NEXT SECTION.
       RDNX-001.
           MOVE 0 TO WS-CRCAMSTRANS-ST1.
       RDNX-005. 
           READ CR-CAMS-TRANS-FILE NEXT WITH LOCK
            AT END
              MOVE 0 TO CR-CAMS-TRANS-NUM
                        WS-CHEQUENUM
              MOVE "Y" TO WS-END
              PERFORM CLEAR-FORM
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDNX-999.
           IF WS-CRCAMSTRANS-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-CRCAMSTRANS-ST1
               MOVE "CAMS TRANS BUSY ON READ-NEXT-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDNX-005.
           IF WS-CRCAMSTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CHEQUENUM
               MOVE 0 TO WS-CRCAMSTRANS-ST1
               PERFORM START-CAMS
               GO TO RDNX-005.
           MOVE CR-CAMS-TRANS-NUM TO WS-CHEQUENUM
                                     WS-CHEQUE-SAVE.
           MOVE "N" TO NEW-CAMSNO.
       RDNX-999.
           EXIT.
      *
       READ-CAMS-PREVIOUS SECTION.
       RDPR-001.
           MOVE 0 TO WS-CRCAMSTRANS-ST1.
       RDPR-005. 
           READ CR-CAMS-TRANS-FILE PREVIOUS WITH LOCK
            AT END
              MOVE 0 TO CR-CAMS-TRANS-NUM
                        WS-CHEQUENUM
              MOVE "Y" TO WS-END
              PERFORM CLEAR-FORM
           MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDPR-999.
           IF WS-CRCAMSTRANS-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-CRCAMSTRANS-ST1
           MOVE "CAMS TRANS BUSY ON READ-PREVIOUS-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDPR-005.
           IF WS-CRCAMSTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CHEQUENUM
               MOVE 0 TO WS-CRCAMSTRANS-ST1
               PERFORM START-CAMS
               GO TO RDPR-005.
           MOVE CR-CAMS-TRANS-NUM TO WS-CHEQUENUM
                                     WS-CHEQUE-SAVE.
           MOVE "N" TO NEW-CAMSNO.
       RDPR-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE 0 TO CR-CAMS-TRANS-NUM.
       CLSC-010.
             MOVE " " TO CR-CAMS-TRANS-ACC-NAME
                         CR-CAMS-TRANS-CRED-NUM
                         CR-CAMS-TRANS-CHEQUE-NUM
                         CR-CAMS-TRANS-BANK-NAME
                         CR-CAMS-TRANS-BANK-NUM
                         CR-CAMS-TRANS-BRANCH-NUM
                         CR-CAMS-TRANS-PAID.
             MOVE 0 TO   CR-CAMS-TRANS-DATE
                         CR-CAMS-TRANS-CRED-TYPE
                         CR-CAMS-TRANS-AMOUNT.
       CLSC-500.
             PERFORM RELEASE-CR-CAMS-TRANS-REC.
       CLSC-999.
             EXIT.      
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CR-CAMS-TRANS-FILE.
            IF WS-CRCAMSTRANS-ST1 NOT = 0
               MOVE "CRCAMS TRANS BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRCAMSTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRCAMSTRANS-ST1
               GO TO OPEN-000.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
            MOVE Ws-Forms-Name   TO F-FILENAME
            MOVE Ws-cbForms-name TO F-CBFILENAME.
            MOVE "CrCAMSMt" TO F-FORMNAME
            MOVE 8          TO F-CBFORMNAME.
            Copy "OpenForms".
        OPEN-999.
            EXIT.
      *
        END-OFF SECTION.
        END-000.
            CLOSE CR-CAMS-TRANS-FILE.
            EXIT PROGRAM.
      *      STOP RUN.
        END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAccount".
       Copy "WriteFieldNumeric6".
       Copy "WriteFieldFax".
       Copy "GetSystemY2KDate".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB
