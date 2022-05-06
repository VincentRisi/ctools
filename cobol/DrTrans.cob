        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrTrans.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrTrans".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDrTrans.
           COPY ChlfdDebtor.
      *
       WORKING-STORAGE SECTION.
       77  NEW-ORDER          PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-TYPE            PIC 99 VALUE 0.
       77  WS-TRANSNO         PIC 9(6) VALUE 0.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1 PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1  PIC 99.
       Copy "WsDateInfo".
      **************************************************************
      * FORMS WORK FIELDS
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
       END-OFF SECTION.
       END-000.
            CLOSE DEBTOR-TRANS-FILE
                  DEBTOR-MASTER.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO DEBTOR-TRANS-REC.
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
            MOVE NUMERIC-RATE TO DRTR-TYPE.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TYPE    TO DRTR-TYPE
                 MOVE WS-TRANSNO TO DRTR-TRANSACTION-NUMBER
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
                 PERFORM READ-DEBTORS
              IF WS-END NOT = "Y"
                 PERFORM GET-004
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
                 PERFORM READ-DEBTORS
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
            IF DRTR-TYPE NOT > 0
                 MOVE "PLEASE ENTER A TYPE IDENTIFIER"
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-001.
        GET-003.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TRANSNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO DRTR-TRANSACTION-NUMBER.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM RELEASE-STOCK-RECORD
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TYPE    TO DRTR-TYPE
                 MOVE WS-TRANSNO TO DRTR-TRANSACTION-NUMBER
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
                 PERFORM READ-DEBTORS
              IF WS-END NOT = "Y"
                 PERFORM GET-004
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
                 PERFORM READ-DEBTORS
              IF WS-END NOT = "Y"
                 PERFORM GET-004
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF DRTR-TRANSACTION-NUMBER NOT > 0
                 MOVE "PLEASE ENTER A TRANS No IDENTIFIER"
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-003.
            PERFORM READ-TRANS.
            IF NEW-ORDER = "Y"
                GO TO GET-999.
            PERFORM READ-DEBTORS.
            IF F-EXIT-CH NOT = X"0A"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
        GET-004.
            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE DRTR-TYPE TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "TRANSNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DRTR-TRANSACTION-NUMBER TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.
        GET-005.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DRTR-ACCOUNT-NUMBER TO F-EDNAMEFIELDACC.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ACCOUNT.

            MOVE "ACCNAME" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DR-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REF1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE DRTR-REFERENCE1 TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REF2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE DRTR-REFERENCE2 TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"       TO F-FIELDNAME.
            MOVE 4            TO F-CBFIELDNAME.
            MOVE DRTR-DATE    TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE-DEL"    TO F-FIELDNAME.
            MOVE 8             TO F-CBFIELDNAME.
            MOVE DRTR-DEL-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE  TO F-NAMEFIELD.
            MOVE 10            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVAMT"            TO F-FIELDNAME
            MOVE 6                   TO F-CBFIELDNAME
            MOVE DRTR-AMT-OF-INVOICE TO F-EDNAMEFIELD9mil
            MOVE 10                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.

            MOVE "AMTOUT"             TO F-FIELDNAME
            MOVE 6                    TO F-CBFIELDNAME
            MOVE DRTR-AMT-OUTSTANDING TO F-EDNAMEFIELD9MIL
            MOVE 10                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                  GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDACC
                                 DRTR-ACCOUNT-NUMBER.
            PERFORM WRITE-FIELD-ACCOUNT.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
             GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-DEBTORS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-DEBTORS
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
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.

            PERFORM READ-DEBTORS.
            IF DR-NAME = " "
               MOVE "ENTER AN EXISTING ACCOUNT NUMBER" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.

            MOVE "ACCNAME" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DR-NAME TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "REF1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DRTR-REFERENCE1.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-DEBTORS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-DEBTORS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
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
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-010.
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "REF2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DRTR-REFERENCE2.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-DEBTORS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-DEBTORS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
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
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10     TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-020.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO DRTR-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-020.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-DEBTORS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-DEBTORS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
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
       FILL-023.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE-DEL" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF F-NAMEFIELD = " "
                MOVE 0 TO DRTR-DEL-DATE
                GO TO FILL-024.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-023.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO DRTR-DEL-DATE.
            IF DRTR-DEL-DATE = 0
                GO TO FILL-024.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-023.
       FILL-024.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-DEBTORS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-DEBTORS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
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
               GO TO FILL-023.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-023.
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVAMT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD9MIL
                                 DRTR-AMT-OF-INVOICE.
            PERFORM WRITE-FIELD-9MIL.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-023.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-DEBTORS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-DEBTORS
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
               GO TO FILL-025.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-025.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "AMTOUT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD9MIL
                                 DRTR-AMT-OUTSTANDING.
            PERFORM WRITE-FIELD-9MIL.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-DEBTORS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-DEBTORS
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
               GO TO FILL-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       DELETE-TRANS SECTION.
       DO-000.
            IF NEW-ORDER = "Y"
                GO TO DO-999.
       DO-010.
            DELETE DEBTOR-TRANS-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DR-TRANS FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO DO-010.
       DO-999.
           EXIT.
      *
       RELEASE-STOCK-RECORD SECTION.
       REL-000.
           UNLOCK DEBTOR-TRANS-FILE.
       REL-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO DRTR-REFERENCE1
                         DRTR-REFERENCE2.
             MOVE 0   TO DRTR-TYPE
                         DRTR-TRANSACTION-NUMBER
                         DRTR-ACCOUNT-NUMBER
                         DRTR-DATE
                         DRTR-DEL-DATE
                         DRTR-AMT-OF-INVOICE
                         DRTR-AMT-OUTSTANDING.
             MOVE WS-TYPE    TO DRTR-TYPE.
             MOVE WS-TRANSNO TO DRTR-TRANSACTION-NUMBER.
       CLSC-999.
             EXIT.      
      *
       REWRITE-TRANS-RECORD SECTION.
       ROR-010.
            IF NEW-ORDER = "Y"
               GO TO ROR-020.
            REWRITE DEBTOR-TRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-DRTRANS-ST1 NOT = 0
                MOVE "DRTRANS RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO ROR-010.
            GO TO ROR-999.
       ROR-020.
            WRITE DEBTOR-TRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DRTRANS RECORD BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO ROR-020.
       ROR-999.
            EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE 0 TO WS-DRTRANS-ST1.
           MOVE DRTR-TYPE               TO WS-TYPE.
           MOVE DRTR-TRANSACTION-NUMBER TO WS-TRANSNO.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY.
       RO-010.
           READ DEBTOR-TRANS-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-DRTRANS-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-DRTRANS-ST1 NOT = 0
                MOVE "DRTRANS BUSY ON READ, PRESS 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO RO-010.
           MOVE "N" TO NEW-ORDER.
           MOVE DRTR-TYPE               TO WS-TYPE.
           MOVE DRTR-TRANSACTION-NUMBER TO WS-TRANSNO.
       RO-999.
           EXIT.
     *
       READ-DEBTORS SECTION.
       RD-000.
           MOVE DRTR-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
       RD-010.
           READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-DEBTOR-ST1
                MOVE " " TO DR-NAME
                GO TO RD-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON READ, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RD-010.
       RD-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           MOVE WS-TYPE    TO DRTR-TYPE.
           MOVE WS-TRANSNO TO DRTR-TRANSACTION-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-001.
           MOVE 0 TO WS-DRTRANS-ST1.
       RONX-005. 
           READ DEBTOR-TRANS-FILE NEXT WITH LOCK
            AT END
              MOVE 0 TO DRTR-TYPE
                        DRTR-TRANSACTION-NUMBER
                        WS-TRANSNO
                        WS-TYPE
              MOVE "Y" TO WS-END
              PERFORM CLEAR-FORM
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DR-TRANS BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO RONX-005.
           MOVE DRTR-TYPE               TO WS-TYPE.
           MOVE DRTR-TRANSACTION-NUMBER TO WS-TRANSNO.
           MOVE "N" TO NEW-ORDER.
       RONX-999.
           EXIT.
      *
       READ-TRANS-PREVIOUS SECTION.
       RTPRE-001.
           MOVE 0 TO WS-DRTRANS-ST1.
       RTPRE-005. 
           READ DEBTOR-TRANS-FILE PREVIOUS WITH LOCK
            AT END
              MOVE 0 TO DRTR-TYPE
                        DRTR-TRANSACTION-NUMBER
                        WS-TRANSNO
                        WS-TYPE
              MOVE "Y" TO WS-END
              PERFORM CLEAR-FORM
           MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RTPRE-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DR-TRANS BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO RTPRE-005.
           MOVE DRTR-TYPE               TO WS-TYPE.
           MOVE DRTR-TRANSACTION-NUMBER TO WS-TRANSNO.
           MOVE "N" TO NEW-ORDER.
       RTPRE-999.
           EXIT.
      *
        OPEN-FILES SECTION.
        OPEN-000.
            OPEN I-O DEBTOR-TRANS-FILE
            IF WS-DRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-DRTRANS-ST1
               MOVE "DRTRANS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
        OPEN-008.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE " " TO WS-DEBTOR-ST1
               MOVE "DR-MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-008.
       OPEN-010.
            MOVE Ws-Forms-Name   TO F-FILENAME
            MOVE Ws-cbForms-name TO F-CBFILENAME.

            MOVE "DrTrans"       TO F-FORMNAME
            MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldAccount".
       Copy "ReadFieldNumeric".
       Copy "WriteField9Mil".
       Copy "WriteFieldAccount".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumeric5".
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
       Copy "CTOSCobolAccept".
      * Copy "WriteDailyExcep1".
      *
      * END-OF-JOB
