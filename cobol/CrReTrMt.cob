        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrReTrMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrRemitTrans".
           Copy "SelectCrMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrRemiTrans.
           COPY ChlfdCreditor.

       WORKING-STORAGE SECTION.
       77  NEW-ORDER          PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-TRYY            PIC 99 VALUE 0.
       77  WS-TRMM            PIC 99 VALUE 0.
       77  WS-TRACC           PIC 9(7) VALUE 0.
       77  WS-TRINV           PIC X(10) VALUE " ".
       01  WS-REMITTRANS-STATUS.
           03  WS-REMITTRANS-ST1 PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1   PIC 99.
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
            MOVE " " TO CRREMTR-RECORD.
            MOVE "N" TO NEW-ORDER
                        WS-END.
       GET-001.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CRYY" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRREMTR-YY
                                 F-EDNAMEFIELDDBL.
            PERFORM WRITE-FIELD-DOUBLE.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TRYY    TO CRREMTR-YY
                 MOVE WS-TRMM    TO CRREMTR-MM
                 MOVE WS-TRACC   TO CRREMTR-ACC-NUMBER
                 MOVE WS-TRINV   TO CRREMTR-INVNO
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
            IF CRREMTR-YY NOT > 0
                 MOVE "PLEASE ENTER A YY IDENTIFIER, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-001.
       GET-0011.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CRMM" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRREMTR-MM
                                 F-EDNAMEFIELDDBL.
            PERFORM WRITE-FIELD-DOUBLE.
            IF F-EXIT-CH = X"01"
                 GO TO GET-001.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TRYY    TO CRREMTR-YY
                 MOVE WS-TRMM    TO CRREMTR-MM
                 MOVE WS-TRACC   TO CRREMTR-ACC-NUMBER
                 MOVE WS-TRINV   TO CRREMTR-INVNO
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
            IF CRREMTR-MM NOT > 0
                 MOVE "PLEASE ENTER A MM IDENTIFIER, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-0011.
        GET-002.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TRACCNO"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRREMTR-ACC-NUMBER
                                 F-EDNAMEFIELDACC.
            PERFORM WRITE-FIELD-ACCOUNT.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"01"
                 GO TO GET-0011.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TRYY    TO CRREMTR-YY
                 MOVE WS-TRMM    TO CRREMTR-MM
                 MOVE WS-TRACC   TO CRREMTR-ACC-NUMBER
                 MOVE WS-TRINV   TO CRREMTR-INVNO
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
                 
            PERFORM READ-CREDITOR.
            
            IF CR-NAME = "** UNKNOWN **"
                MOVE "INVALID ACC NUM ENTERED, 'ESC' TO RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-002.
                
            PERFORM GET-006.
        GET-003.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TRINVNO"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO CRREMTR-INVNO.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"01"
                 GO TO GET-002.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TRYY    TO CRREMTR-YY
                 MOVE WS-TRMM    TO CRREMTR-MM
                 MOVE WS-TRACC   TO CRREMTR-ACC-NUMBER
                 MOVE WS-TRINV   TO CRREMTR-INVNO
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
            IF CRREMTR-INVNO NOT > " "
                 MOVE 
                 "PLEASE ENTER A VALID INVOICE NUMBER, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-003.
                 
            PERFORM READ-TRANS.
            IF NEW-ORDER = "Y"
                MOVE "COMPLETE"       TO F-FIELDNAME
                MOVE 8                TO F-CBFIELDNAME
                MOVE "N"              TO CRREMTR-COMPLETE
                                         F-NAMEFIELD
                MOVE 1                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.
            IF NEW-ORDER = "Y"
                GO TO GET-999.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-001.
        GET-004.
            MOVE "CRYY"     TO F-FIELDNAME.
            MOVE 4          TO F-CBFIELDNAME.
            MOVE CRREMTR-YY TO F-EDNAMEFIELDDBL.
            MOVE 2          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-DOUBLE.

            MOVE "CRMM"     TO F-FIELDNAME.
            MOVE 4          TO F-CBFIELDNAME.
            MOVE CRREMTR-MM TO F-EDNAMEFIELDDBL.
            MOVE 2          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-DOUBLE.

            MOVE "TRACCNO"          TO F-FIELDNAME.
            MOVE 7                  TO F-CBFIELDNAME.
            MOVE CRREMTR-ACC-NUMBER TO F-EDNAMEFIELDACC.
            MOVE 7                  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ACCOUNT.

            MOVE "TRINVNO"          TO F-FIELDNAME.
            MOVE 7                  TO F-CBFIELDNAME.
            MOVE CRREMTR-INVNO      TO F-NAMEFIELD.
            MOVE 10                 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            PERFORM READ-CREDITOR.
        GET-006.
            MOVE "ACCNAME" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE CR-NAME   TO F-NAMEFIELD.
            MOVE 40        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-007.
            MOVE "INVAMT"        TO F-FIELDNAME.
            MOVE 6               TO F-CBFIELDNAME.
            MOVE CRREMTR-INV-AMT TO F-EDNAMEFIELDREC.
            MOVE 12              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "SDISC"          TO F-FIELDNAME.
            MOVE 5                TO F-CBFIELDNAME.
            MOVE CRREMTR-DISC-AMT TO F-EDNAMEFIELDREC.
            MOVE 12               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "COMPLETE"       TO F-FIELDNAME.
            MOVE 8                TO F-CBFIELDNAME.
            MOVE CRREMTR-COMPLETE TO F-NAMEFIELD.
            MOVE 1                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-005.
            IF WS-END = "Y"
                GO TO FILL-999.
       FILL-045.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVAMT" TO F-FIELDNAME.
            MOVE 6  TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CRREMTR-INV-AMT.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-070.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-007
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-007
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
       FILL-065.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SDISC" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CRREMTR-DISC-AMT.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-045.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-007
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-007
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
            IF NEW-ORDER = "Y"
              MOVE "COMPLETE"       TO F-FIELDNAME
              MOVE 8                TO F-CBFIELDNAME
              MOVE CRREMTR-COMPLETE TO F-NAMEFIELD
              MOVE 1                TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ALPHA.

            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COMPLETE"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CRREMTR-COMPLETE.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CRREMTR-COMPLETE NOT = "N" AND NOT = "Y"
                MOVE "THIS FIELD MUST BE EITHER N OR Y, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-070.
            IF F-EXIT-CH = X"01"
               GO TO FILL-065.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-007
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-CREDITOR
               PERFORM GET-004 THRU GET-007
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
               GO TO FILL-070.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-070.

            GO TO FILL-005.
       FILL-999.
            EXIT.
      *
       DELETE-TRANS SECTION.
       DO-000.
            IF NEW-ORDER = "Y"
                GO TO DO-999.
       DO-010.
            DELETE CRREMIT-TRANS-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-REMITTRANS-ST1 NOT = 0
               MOVE "CRREMIT BUSY ON DELETE, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-REMITTRANS-ST1
               GO TO DO-010.
       DO-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
           MOVE " " TO CRREMTR-INVNO
                       CRREMTR-COMPLETE.
           MOVE 0   TO CRREMTR-YY
                       CRREMTR-MM
                       CRREMTR-ACC-NUMBER
                       CRREMTR-INV-AMT
                       CRREMTR-DISC-AMT.
           MOVE WS-TRYY    TO CRREMTR-YY
           MOVE WS-TRMM    TO CRREMTR-MM
           MOVE WS-TRACC   TO CRREMTR-ACC-NUMBER
           MOVE WS-TRINV   TO CRREMTR-INVNO.
       CLSC-999.
             EXIT.      
      *
       REWRITE-TRANS-RECORD SECTION.
       ROR-010.
            IF NEW-ORDER = "Y"
               GO TO ROR-020.
            REWRITE CRREMTR-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-REMITTRANS-ST1 NOT = 0
                MOVE 0 TO WS-REMITTRANS-ST1
                MOVE "CRREMITRANS BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO ROR-010.
            GO TO ROR-999.
       ROR-020.
            WRITE CRREMTR-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-REMITTRANS-ST1 NOT = 0
                MOVE 0 TO WS-REMITTRANS-ST1
                MOVE "CRREMITRANS RECORD BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO ROR-020.
       ROR-999.
            EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE CRREMTR-YY          TO WS-TRYY
           MOVE CRREMTR-MM          TO WS-TRMM
           MOVE CRREMTR-ACC-NUMBER  TO WS-TRACC
           MOVE CRREMTR-INVNO       TO WS-TRINV.
           START CRREMIT-TRANS-FILE KEY NOT < CRREMTR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-REMITTRANS-ST1 NOT = 0
                MOVE 88 TO WS-REMITTRANS-ST1
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
       RO-010.
           READ CRREMIT-TRANS-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-REMITTRANS-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-REMITTRANS-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-REMITTRANS-ST1 NOT = 0
                MOVE 0 TO WS-REMITTRANS-ST1
                MOVE "CRREMI-TRANS BUSY ON READ-LOCK, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RO-010.
                
           MOVE "N"                 TO NEW-ORDER.

           MOVE CRREMTR-YY          TO WS-TRYY
           MOVE CRREMTR-MM          TO WS-TRMM
           MOVE CRREMTR-ACC-NUMBER  TO WS-TRACC
           MOVE CRREMTR-INVNO       TO WS-TRINV.
       RO-999.
           EXIT.
     *
       READ-CREDITOR SECTION.
       RD-000.
           IF WS-END = "Y"
               GO TO RD-999.
           MOVE CRREMTR-ACC-NUMBER TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE " " TO CR-NAME
                GO TO RD-999.
       RD-010.
           READ CREDITOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "** UNKNOWN **" TO CR-NAME
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
           MOVE WS-TRYY    TO CRREMTR-YY
           MOVE WS-TRMM    TO CRREMTR-MM
           MOVE WS-TRACC   TO CRREMTR-ACC-NUMBER
           MOVE WS-TRINV   TO CRREMTR-INVNO.
           START CRREMIT-TRANS-FILE KEY NOT < CRREMTR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-REMITTRANS-ST1 NOT = 0
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-001.
           MOVE 0 TO WS-REMITTRANS-ST1.
           IF WS-END = "Y"
              GO TO RONX-999.
       RONX-005. 
           READ CRREMIT-TRANS-FILE NEXT WITH LOCK
            AT END
              MOVE 0   TO CRREMTR-YY
                          CRREMTR-MM
                          CRREMTR-ACC-NUMBER
                          WS-TRACC
                          WS-TRYY
              MOVE " " TO CRREMTR-INVNO
                          WS-TRINV
              
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           IF WS-REMITTRANS-ST1 NOT = 0
              MOVE "CR-TRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-REMITTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-REMITTRANS-ST1
              PERFORM START-TRANS
              GO TO RONX-005.

           MOVE "N"                 TO NEW-ORDER.
           MOVE CRREMTR-YY          TO WS-TRYY
           MOVE CRREMTR-MM          TO WS-TRMM
           MOVE CRREMTR-ACC-NUMBER  TO WS-TRACC
           MOVE CRREMTR-INVNO       TO WS-TRINV.
       RONX-999.
           EXIT.
      *
       READ-TRANS-PREVIOUS SECTION.
       RDPREV-001.
           MOVE 0 TO WS-REMITTRANS-ST1.
           IF WS-END = "Y"
              GO TO RDPREV-999.
       RDPREV-005. 
           READ CRREMIT-TRANS-FILE PREVIOUS WITH LOCK
            AT END
              MOVE 0   TO CRREMTR-YY
                          CRREMTR-MM
                          CRREMTR-ACC-NUMBER
                          WS-TRACC
                          WS-TRYY
              MOVE " " TO CRREMTR-INVNO
                          WS-TRINV
              
              MOVE "Y" TO WS-END
            MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDPREV-999.
           IF WS-REMITTRANS-ST1 NOT = 0
              MOVE "CR-TRANS BUSY ON READ-PREV, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-REMITTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-REMITTRANS-ST1
              PERFORM START-TRANS
              GO TO RDPREV-005.

           MOVE "N"                 TO NEW-ORDER.
           MOVE CRREMTR-YY          TO WS-TRYY
           MOVE CRREMTR-MM          TO WS-TRMM
           MOVE CRREMTR-ACC-NUMBER  TO WS-TRACC
           MOVE CRREMTR-INVNO       TO WS-TRINV.
       RDPREV-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CRREMIT-TRANS-FILE
            IF WS-REMITTRANS-ST1 NOT = 0
               MOVE 0 TO WS-REMITTRANS-ST1
               MOVE "CR-REMITRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
           MOVE "CrReTrMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CRREMIT-TRANS-FILE
                 CREDITOR-MASTER.
      *      STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldDouble".
       Copy "WriteFieldDate".
       Copy "WriteFieldAccount".
       Copy "WriteFieldRec".
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
