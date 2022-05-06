        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlTranLy.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMasterLy".
        Copy "SelectGlTransLy".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlMastLy.
           COPY ChlfdGlTransLy.

       WORKING-STORAGE SECTION.
       77  NEW-ORDER          PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-REFERENCE       PIC X(10) VALUE " ".      
       77  WS-TRANSNO         PIC 9(6) VALUE 0.
       77  WS-TYPE            PIC 99 VALUE 0.
       01  WS-GLTRANS-LY-STATUS.
           03  WS-GLTRANS-LY-ST1   PIC 99.
       01  WS-GL-LY-STATUS.
           03  WS-GL-LY-ST1        PIC 99.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-RESTOFACCOUNT    PIC X(6).
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
            MOVE " " TO GLTRANS-LY-REC.
            MOVE "N" TO NEW-ORDER
                        WS-END.
        GET-001.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "REFR" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLTRANS-LY-REFERENCE.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-REFERENCE TO GLTRANS-LY-REFERENCE
                 MOVE WS-TRANSNO   TO GLTRANS-LY-TRANS
                 MOVE WS-TYPE      TO GLTRANS-LY-TYPE
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
                 PERFORM READ-GL
              IF WS-END NOT = "Y"
                 PERFORM GET-004
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
                 PERFORM READ-GL
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
            IF GLTRANS-LY-REFERENCE NOT > SPACES
                 MOVE "PLEASE ENTER A REFERENCE NUMBER, 'ESC' TO RETRY."
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
            MOVE NUMERIC-RATE TO GLTRANS-LY-TRANS F-EDNAMEFIELDNUM.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"01"
                 GO TO GET-001.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TYPE    TO GLTRANS-LY-TYPE
                 MOVE WS-TRANSNO TO GLTRANS-LY-TRANS
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
                 PERFORM READ-GL
              IF WS-END NOT = "Y"
                 PERFORM GET-004
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
                 PERFORM READ-GL
              IF WS-END NOT = "Y"
                 PERFORM GET-004
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF GLTRANS-LY-TRANS NOT > 0
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
            MOVE NUMERIC-RATE TO GLTRANS-LY-TYPE.
            IF F-EXIT-CH = X"01"
                 GO TO GET-002.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TYPE    TO GLTRANS-LY-TYPE
                 MOVE WS-TRANSNO TO GLTRANS-LY-TRANS
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
                 PERFORM READ-GL
              IF WS-END NOT = "Y"
                 PERFORM GET-004
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
                 PERFORM READ-GL
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
            IF GLTRANS-LY-TYPE NOT > 0
                 MOVE "PLEASE ENTER A TYPE IDENTIFIER"
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-003.
            PERFORM READ-TRANS.
            IF NEW-ORDER = "Y"
                GO TO GET-999.
            PERFORM READ-GL.
            IF F-EXIT-CH NOT = X"0A"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-003.
        GET-004.
            MOVE "REFR" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE GLTRANS-LY-REFERENCE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TRANSNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE GLTRANS-LY-TRANS TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE GLTRANS-LY-TYPE TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.
        GET-005.
            MOVE "PERIOD"      TO F-FIELDNAME.
            MOVE 6             TO F-CBFIELDNAME.
            MOVE GLTRANS-LY-NO TO F-NAMEFIELD.
            MOVE 2             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"       TO F-FIELDNAME.
            MOVE 4            TO F-CBFIELDNAME.
            MOVE GLTRANS-LY-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE GLTRANS-LY-ACCNO TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNAME" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE GL-LY-DESCRIPTION TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE GLTRANS-LY-AMOUNT TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "DESC" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE GLTRANS-LY-LINE-DESC TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
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
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO GLTRANS-LY-NO.
            IF GLTRANS-LY-NO NOT > 0
               MOVE 
            "THIS FIELD MUST HAVE A VALUE BETWEEN 1 AND 12, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-005.
            IF GLTRANS-LY-NO > 12
               MOVE 
            "THIS FIELD MUST HAVE A VALUE BETWEEN 1 AND 12, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-005.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-GL
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-GL
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
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO GLTRANS-LY-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-GL
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-GL
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
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
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-010.
       FILL-012.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            IF ALPHA-RATE > SPACES
                PERFORM NUMBER-CHECK.
            MOVE WS-GLNO-CHECK TO WS-GLNUMBER
                                  GLTRANS-LY-ACCNO.
            PERFORM WRITE-FIELD-ALPHA.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-GL
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

            PERFORM READ-GL.
            IF GL-LY-DESCRIPTION = " "
               MOVE "ENTER AN EXISTING ACCOUNT NUMBER" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-012.


            MOVE "ACCNO"                TO F-FIELDNAME
            MOVE 5                      TO F-CBFIELDNAME
            MOVE GLTRANS-LY-ACCNO TO F-NAMEFIELD
            MOVE 12                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNAME"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME
            MOVE GL-LY-DESCRIPTION TO F-NAMEFIELD
            MOVE 40             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
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
                                 GLTRANS-LY-AMOUNT.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-012.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-GL
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-GL
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
               GO TO FILL-015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-015.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DESC" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLTRANS-LY-LINE-DESC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM READ-GL
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM READ-GL
               PERFORM GET-004 THRU GET-005
               GO TO FILL-005.
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

            GO TO FILL-005.
       FILL-999.
            EXIT.
      *
       DELETE-TRANS SECTION.
       DO-000.
            IF NEW-ORDER = "Y"
                GO TO DO-999.
       DO-010.
            DELETE GLTRANS-LY-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-GLTRANS-LY-ST1 NOT = 0
              MOVE "GLTRANS BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO DO-010.
       DO-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO GLTRANS-LY-REFERENCE
                         GLTRANS-LY-ACCNO
                         GLTRANS-LY-LINE-DESC.
             MOVE 0   TO GLTRANS-LY-TRANS
                         GLTRANS-LY-TYPE
                         GLTRANS-LY-NO
                         GLTRANS-LY-DATE
                         GLTRANS-LY-AMOUNT.
             MOVE WS-REFERENCE TO GLTRANS-LY-REFERENCE.
             MOVE WS-TRANSNO   TO GLTRANS-LY-TRANS.
             MOVE WS-TYPE      TO GLTRANS-LY-TYPE.
             
             UNLOCK GLTRANS-LY-FILE.
       CLSC-999.
             EXIT.      
      *
       REWRITE-TRANS-RECORD SECTION.
       ROR-010.
            IF NEW-ORDER = "Y"
               GO TO ROR-020.
            REWRITE GLTRANS-LY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-GLTRANS-LY-ST1 NOT = 0
              MOVE "GLTRANSLY BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO ROR-010.
            GO TO ROR-999.
       ROR-020.
            WRITE GLTRANS-LY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-GLTRANS-LY-ST1 NOT = 0
              MOVE "GLTRANSLY BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO ROR-020.
       ROR-999.
            EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE 0 TO WS-GLTRANS-LY-ST1.
           MOVE GLTRANS-LY-REFERENCE TO WS-REFERENCE.
           MOVE GLTRANS-LY-TRANS     TO WS-TRANSNO.
           MOVE GLTRANS-LY-TYPE      TO WS-TYPE.
           START GLTRANS-LY-FILE KEY NOT < GLTRANS-LY-KEY.
       RO-010.
           READ GLTRANS-LY-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-LY-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-GLTRANS-LY-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-GLTRANS-LY-ST1 NOT = 0
              MOVE "GLTRANSLY BUSY ON READ, Press 'ESC' To Retry."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO RO-010.
           MOVE "N" TO NEW-ORDER.
           MOVE GLTRANS-LY-REFERENCE TO WS-REFERENCE.
           MOVE GLTRANS-LY-TRANS     TO WS-TRANSNO.
           MOVE GLTRANS-LY-TYPE      TO WS-TYPE.
       RO-999.
           EXIT.
     *
       READ-GL SECTION.
       RD-000.
           MOVE GLTRANS-LY-ACCNO TO GL-LY-NUMBER.
           START GL-LY-MASTER KEY NOT < GL-LY-KEY.
       RD-010.
           READ GL-LY-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-GL-LY-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-GL-LY-ST1
                MOVE " " TO GL-LY-DESCRIPTION
                GO TO RD-999.
           IF WS-GL-LY-ST1 NOT = 0
              MOVE "GLMASTLY BUSY ON READ, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO RD-010.
       RD-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           MOVE WS-REFERENCE TO GLTRANS-LY-REFERENCE.
           MOVE WS-TRANSNO   TO GLTRANS-LY-TRANS.
           MOVE WS-TYPE      TO GLTRANS-LY-TYPE.
           START GLTRANS-LY-FILE KEY NOT < GLTRANS-LY-KEY.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-001.
           MOVE 0 TO WS-GLTRANS-LY-ST1.
       RONX-005. 
           READ GLTRANS-LY-FILE NEXT WITH LOCK
            AT END
              MOVE " " TO GLTRANS-LY-REFERENCE
                          WS-REFERENCE
              MOVE 0   TO GLTRANS-LY-TRANS
                          GLTRANS-LY-TYPE
                          WS-TRANSNO
                          WS-TYPE
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           IF WS-GLTRANS-LY-ST1 NOT = 0
              MOVE "GLTRANSLY BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
      *        PERFORM START-TRANS
              GO TO RONX-005.
           MOVE GLTRANS-LY-REFERENCE TO WS-REFERENCE.
           MOVE GLTRANS-LY-TRANS     TO WS-TRANSNO.
           MOVE GLTRANS-LY-TYPE      TO WS-TYPE.
           MOVE "N"                  TO NEW-ORDER.
       RONX-999.
           EXIT.
      *
       READ-TRANS-PREVIOUS SECTION.
       RDPREV-001.
           MOVE 0 TO WS-GLTRANS-LY-ST1.
       RDPREV-005. 
           READ GLTRANS-LY-FILE PREVIOUS WITH LOCK
            AT END
              MOVE " " TO GLTRANS-LY-REFERENCE
                          WS-REFERENCE
              MOVE 0   TO GLTRANS-LY-TRANS
                          GLTRANS-LY-TYPE
                          WS-TRANSNO
                          WS-TYPE
              MOVE "Y" TO WS-END
            MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDPREV-999.
           IF WS-GLTRANS-LY-ST1 NOT = 0
              MOVE "GLTRANSLY BUSY ON READ-PREV, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
      *        PERFORM START-TRANS
              GO TO RDPREV-005.
           MOVE GLTRANS-LY-REFERENCE TO WS-REFERENCE.
           MOVE GLTRANS-LY-TRANS     TO WS-TRANSNO.
           MOVE GLTRANS-LY-TYPE      TO WS-TYPE.
           MOVE "N"                  TO NEW-ORDER.
       RDPREV-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O GLTRANS-LY-FILE
            IF WS-GLTRANS-LY-ST1 NOT = 0
              MOVE "GLTRANSLY FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO OPEN-000.
       OPEN-008.
            OPEN I-O GL-LY-MASTER.
            IF WS-GL-LY-ST1 NOT = 0
               MOVE "GLMASTLY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GL-LY-ST1
               GO TO OPEN-008.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "GlTranLy"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE GLTRANS-LY-FILE
                 GL-LY-MASTER.
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
       Copy "GetSystemY2KDate".
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
