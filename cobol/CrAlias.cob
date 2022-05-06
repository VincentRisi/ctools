        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrAlias.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectCrMaster".
        Copy "SelectCrAlias".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrAlias.

       WORKING-STORAGE SECTION.
       77  WS-INQUIRY         PIC X(8) VALUE "CrNameIq".
       77  NEW-ALIASNO        PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-ACCOUNTNUMBER   PIC X(7) VALUE " ".
       77  WS-ACC-SAVE        PIC X(7) VALUE " ".
       77  WS-CRED-NUMBER     PIC X(7) VALUE " ".
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1 PIC 99.
       01  WS-ALIAS-STATUS.
           03  WS-ALIAS-ST1    PIC 99.
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
            MOVE " " TO CRALIAS-RECORD.
            MOVE "N" TO NEW-ALIASNO
                        WS-END.
       GET-001.              
            MOVE " "     TO F-NAMEFIELD.
            MOVE "ALIAS" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7       TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CRAL-ALIAS.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-ACC-SAVE TO CRAL-ALIAS
                 PERFORM START-CREDITOR-ALIAS
                 PERFORM READ-CREDITOR-ALIAS-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-CREDITOR-ALIAS-PREVIOUS
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
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
            IF CRAL-ALIAS = " "
                CLOSE CREDITOR-MASTER
                CALL WS-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-INQUIRY
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            PERFORM READ-CREDITOR-ALIAS.
            IF NEW-ALIASNO = "Y"
                 PERFORM CLEAR-FORM
                 GO TO GET-999.
            GO TO GET-005.
        GET-003.
            MOVE "ALIAS"    TO F-FIELDNAME
            MOVE 5          TO F-CBFIELDNAME
            MOVE CRAL-ALIAS TO F-NAMEFIELD
            MOVE 7          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            PERFORM READ-CREDITOR.
        GET-005.
            MOVE "ACCNO"             TO F-FIELDNAME
            MOVE 5                   TO F-CBFIELDNAME
            MOVE CRAL-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 7                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "NAME"  TO F-FIELDNAME
            MOVE 4       TO F-CBFIELDNAME
            MOVE CR-NAME TO F-NAMEFIELD
            MOVE 40      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCNO" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 7       TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-CRED-NUMBER.
            MOVE WS-CRED-NUMBER TO CRAL-ACCOUNT-NUMBER.
            
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-ALIAS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-999.
            IF CR-ACCOUNT-NUMBER = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A VALID NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            PERFORM READ-CREDITOR.
            IF CR-NAME = "UNKNOWN"
               MOVE "ENTER A VALID NUMBER. 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
               
            PERFORM GET-005.

            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-ALIAS-RECORD
               PERFORM READ-CREDITOR-ALIAS-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-ALIAS-RECORD
               PERFORM READ-CREDITOR-ALIAS-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-ALIAS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-ALIAS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       DELETE-CREDITOR-ALIAS-RECORD SECTION.
       DCR-000.
           IF NEW-ALIASNO = "Y"
                GO TO DCR-999.
       DCR-010.
      *     MOVE "DELETE NOT ALLOWED IN THIS PROGRAM, 'ESC' TO CLEAR."
      *         TO WS-MESSAGE.
      *     PERFORM ERROR-MESSAGE.
      *     GO TO DCR-999.
        DCR-020.
           DELETE CRALIAS-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-ALIAS-ST1 NOT = 0
                MOVE "ALIAS FILE BUSY ON DELETE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-ALIAS-ST1
                GO TO DCR-020.
       DCR-999.
           EXIT.
      *
       RELEASE-CREDITOR-ALIAS-RECORD SECTION.
       REL-000.
           IF WS-ALIAS-ST1 = 51
               UNLOCK CRALIAS-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-CREDITOR-ALIAS-RECORD SECTION.
       RCR-010.
            IF NEW-ALIASNO = "Y"
               GO TO RCR-020.
            REWRITE CRALIAS-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-ALIAS-ST1 NOT = 0
                MOVE 0 TO WS-ALIAS-ST1
                MOVE "ALIAS FILE BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCR-010.
            GO TO RCR-999.
       RCR-020.
            WRITE CRALIAS-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-ALIAS-ST1 NOT = 0
                MOVE 0 TO WS-ALIAS-ST1
                MOVE "CREDITORS BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCR-020.
       RCR-999.
            EXIT.
      *
       READ-CREDITOR SECTION.
       RD-000.
           MOVE CRAL-ACCOUNT-NUMBER TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-ACCOUNT-NUMBER
              INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE "CREDITORS BUSY ON START 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020.
        RD-010.
           READ CREDITOR-MASTER
                 INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN" TO CR-NAME
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
       READ-CREDITOR-ALIAS SECTION.
       RDAL-000.
           MOVE CRAL-ALIAS TO WS-ACCOUNTNUMBER.
           START CRALIAS-MASTER KEY NOT < CRAL-ALIAS.
       RDAL-010.
           READ CRALIAS-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-ALIAS-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-ALIASNO
                MOVE WS-ACCOUNTNUMBER TO CRAL-ALIAS
                GO TO RDAL-999.
           IF WS-ALIAS-ST1 NOT = 0
                MOVE 0 TO WS-ALIAS-ST1
                MOVE "ALIAS FILE BUSY ON READ-LOCK, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDAL-010.
           MOVE "N" TO NEW-ALIASNO.
           MOVE CRAL-ALIAS TO WS-ACC-SAVE.
           PERFORM READ-CREDITOR.
       RDAL-999.
             EXIT.
      *
       START-CREDITOR-ALIAS SECTION.
       ST-CR-000.
              MOVE WS-ACCOUNTNUMBER TO CRAL-ALIAS.
              START CRALIAS-MASTER KEY NOT < CRAL-ALIAS.
       ST-CR-999.
             EXIT.
      *
       READ-CREDITOR-ALIAS-NEXT SECTION.
       RDNX-005. 
           READ CRALIAS-MASTER NEXT WITH LOCK
            AT END
              MOVE " " TO CRAL-ALIAS
                          WS-ACCOUNTNUMBER
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDNX-999.
           IF WS-ALIAS-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-ALIAS-ST1
               MOVE "ALIAS FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDNX-005.
           IF WS-ALIAS-ST1 NOT = 0
               MOVE 0 TO WS-ALIAS-ST1
               PERFORM START-CREDITOR-ALIAS
               GO TO RDNX-005.
           MOVE CRAL-ALIAS TO WS-ACCOUNTNUMBER
                              WS-ACC-SAVE.
           MOVE "N"        TO NEW-ALIASNO.
       RDNX-999.
           EXIT.
      *
       READ-CREDITOR-ALIAS-PREVIOUS SECTION.
       RDPREV-005. 
           READ CRALIAS-MASTER PREVIOUS WITH LOCK
            AT END
              MOVE " " TO CRAL-ALIAS
                          WS-ACCOUNTNUMBER
              MOVE "Y" TO WS-END
            MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDPREV-999.
           IF WS-ALIAS-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-ALIAS-ST1
               MOVE "ALIAS FILE BUSY ON READ-PREVIOUS, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDPREV-005.
           IF WS-ALIAS-ST1 NOT = 0
               MOVE 0 TO WS-ALIAS-ST1
               PERFORM START-CREDITOR-ALIAS
               GO TO RDPREV-005.
           MOVE CRAL-ALIAS TO WS-ACCOUNTNUMBER
                              WS-ACC-SAVE.
           MOVE "N"        TO NEW-ALIASNO.
       RDPREV-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO CRAL-ALIAS.
             MOVE 0   TO CRAL-ACCOUNT-NUMBER.
             MOVE WS-ACCOUNTNUMBER TO CRAL-ALIAS.
       CLSC-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR FILE BUSY ON OPEN,  'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-003.
            OPEN I-O CRALIAS-MASTER.
            IF WS-ALIAS-ST1 NOT = 0
               MOVE 0 TO WS-ALIAS-ST1
               MOVE "ALIAS FILE BUSY ON OPEN,  'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-003.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME
           MOVE "CrAlias"       TO F-FORMNAME
           MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CREDITOR-MASTER
                 CRALIAS-MASTER.
      *      STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAccount".
       Copy "WriteFieldAlpha".
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
