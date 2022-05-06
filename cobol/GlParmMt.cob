        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlParmMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlParameter".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlParam.

       WORKING-STORAGE SECTION.
       77  NEW-NO             PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-NUMBER          PIC 9 VALUE 0.
       77  WS-SAVE            PIC 9 VALUE 0.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1   PIC 99.
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
           MOVE "GlParmGl" TO F-FORMNAME.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM FILL-DATA.
           MOVE "GlParmGl" TO F-FORMNAME
           PERFORM OPEN-010 THRU OPEN-020.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "N" TO NEW-NO
                        WS-END.
            MOVE 1           TO GLPA-RECORD.
            MOVE "KEY"       TO F-FIELDNAME.
            MOVE 3           TO F-CBFIELDNAME.
            MOVE GLPA-RECORD TO F-EDNAMEFIELDIND1.
            MOVE 1           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INDEX1.
       GET-001.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "KEY" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDIND1
                                 GLPA-RECORD.
            PERFORM WRITE-FIELD-INDEX1.
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
            PERFORM READ-RECORD.
            IF NEW-NO = "Y"
               GO TO GET-999.

            PERFORM GET-003 THRU GET-010.
            MOVE " " TO F-EXIT-CH.
            GO TO GET-999.
      *
      *DISPLAY OF GL INFORMATION.
      *
       GET-003.
            MOVE "KEY" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE GLPA-RECORD TO F-EDNAMEFIELDIND1.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INDEX1.
       GET-005.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE GLPA-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE GLPA-ADD1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE GLPA-ADD2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE GLPA-ADD3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD4" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE GLPA-ADD4 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD5" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE GLPA-ADD5 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD6" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE GLPA-ADD6 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CURRENTGLPER" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE GLPA-CURRENT-GLPER TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "RECJRNPOST" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE GLPA-RECJRN-POST TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLTRANSNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE GLPA-GLTRANSNO TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "VATREG" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE GLPA-GLVAT-REG-NO TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GROUP"        TO F-FIELDNAME
            MOVE 5              TO F-CBFIELDNAME
            MOVE GLPA-GROUP-NUM TO F-NAMEFIELD
            MOVE 1              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE 1 TO SUB-1 F-INDEX.
       GET-010.
            MOVE "PER"            TO F-FIELDNAME.
            MOVE 3                TO F-CBFIELDNAME.
            MOVE GLPA-PER (SUB-1) TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE     TO F-NAMEFIELD.
            MOVE 10               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 14
               GO TO GET-010.
      *
      *DISPLAY OF CR INFORMATION.
      *
       GET-105.
            MOVE "CURRENTCRPER" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE GLPA-CURRENT-CRPER TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "REMIT" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE GLPA-CR-REMIT TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CRTRANSNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE GLPA-CRTRANSNO TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "CRCONTROL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE GLPA-GLCRED-NO TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CRBANK" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE GLPA-GLBANK TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "VATACC" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE GLPA-GLVAT-ACC TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE 1 TO SUB-1 F-INDEX.
       GET-130.
            MOVE "IND" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE GLPA-IND (SUB-1) TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "NUM" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE GLPA-NO (SUB-1) TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 11
               GO TO GET-130.
            MOVE 1 TO SUB-1.
      *
      *DISPLAY OF SL INFORMATION.
      *
        GET-205.
            MOVE "CURRENTSLPER" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE GLPA-CURRENT-SLPER TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "DRCONTROL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE GLPA-GLDEBT-NO TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DRBANK" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE GLPA-GLDRBANK TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "VATOUTACC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE GLPA-GLVATOUTPUT-ACC TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESACC" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE GLPA-GLSALES-ACC TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESADDONS" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE GLPA-GLSALES-ADDONS TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE GLPA-GLSALES-DISC TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESADJ" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE GLPA-GLSALES-ADJ TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BDEBTACC" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE GLPA-GLBDEBT-ACC TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
      *
      *DISPLAY OF CB INFORMATION.
      *
        GET-305.
            MOVE "CURRENTCBPER" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE GLPA-CURRENT-CBPER TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "CBPOST" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE GLPA-CB-POST TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CBTRANSNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE GLPA-CBTRANSNO TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "CAMS-BANK-NAME"    TO F-FIELDNAME
            MOVE 14                  TO F-CBFIELDNAME
            MOVE GLPA-CAMS-BANK-NAME TO F-NAMEFIELD
            MOVE 20                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CAMS-BANK-CODE"    TO F-FIELDNAME
            MOVE 14                  TO F-CBFIELDNAME
            MOVE GLPA-CAMS-BANK-CODE TO F-NAMEFIELD
            MOVE 6                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CAMS-BANK-ACC"     TO F-FIELDNAME
            MOVE 13                  TO F-CBFIELDNAME
            MOVE GLPA-CAMS-BANK-ACC  TO F-NAMEFIELD
            MOVE 11                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
           IF WS-END = "Y"
               GO TO FILL-999.
           IF F-EXIT-CH = X"17"
            IF F-FORMNAME NOT = "GlParmGl"
               MOVE "GlParmGl" TO F-FORMNAME
               PERFORM OPEN-010 THRU OPEN-020
               PERFORM DISPLAY-FORM
               PERFORM GET-003 THRU GET-010.
           MOVE 2910 TO POS.
           DISPLAY
           "<F5>=CREDITOR-FIELDS,<F7>=SALES-FIELDS,<F9>=C/BOOK-FIELDS  "
               AT POS.
       FILL-002.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLPA-NAME.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-104.
            IF GLPA-NAME = "         "
               MOVE "THIS FIELD MUST BE > SPACES, ENTER A NAME"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-002.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-002.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-002.
       FILL-005.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ADD1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLPA-ADD1.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-002.
            IF GLPA-ADD1 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, ENTER A NAME"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
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
            MOVE "ADD2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLPA-ADD2.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-005.
            IF GLPA-ADD2 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, ENTER A NAME"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
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
            MOVE "ADD3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLPA-ADD3.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF GLPA-ADD3 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, ENTER A NAME"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-015.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
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
            MOVE "ADD4" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLPA-ADD4.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
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
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ADD5" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLPA-ADD5.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
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
            MOVE "ADD6" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLPA-ADD6.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
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
       FILL-090.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CURRENTGLPER" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 GLPA-CURRENT-GLPER.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF GLPA-CURRENT-GLPER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-090.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-090.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-090.
       FILL-100.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "RECJRNPOST" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLPA-RECJRN-POST.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-090.
            IF GLPA-RECJRN-POST NOT = "Y" AND NOT = "N"
               MOVE "THIS FIELD MUST BE Y OR N, RE-ENTER" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-100.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-100.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-100.
       FILL-101.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "GLTRANSNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 GLPA-GLTRANSNO.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-GLTRANSNO = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-101.
            IF F-EXIT-CH = X"01"
               GO TO FILL-100.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-101.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-101.
       FILL-103.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "VATREG" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLPA-GLVAT-REG-NO.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-GLVAT-REG-NO = " "
                MOVE "THIS ACCOUNT CANNOT BE BLANK, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-103.
            IF F-EXIT-CH = X"01"
               GO TO FILL-101.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-103.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-103.
       FILL-104.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "GROUP" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1       TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO GLPA-GROUP-NUM.
            PERFORM WRITE-FIELD-ALPHA.
            IF GLPA-GROUP-NUM = 0
                MOVE "THE GROUP NUMBER MUST BE > 0, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-104.
                
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-103.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-104.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-104.
            MOVE 1 TO SUB-1 F-INDEX.
       FILL-105.
            MOVE "                             " TO F-NAMEFIELD.
            MOVE "PER" TO F-FIELDNAME.
            MOVE 3     TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10    TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-105.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO GLPA-PER (SUB-1).
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-105.
            IF F-EXIT-CH = X"01"
             IF SUB-1 = 1
               GO TO FILL-104
             ELSE
               SUBTRACT 1 FROM SUB-1 F-INDEX
               GO TO FILL-105.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-105.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-105.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 14
               GO TO FILL-105.
            GO TO FILL-001.
      *
      *FILL-IN-CREDITOR-FIELDS.
      *
       FILL-1000.
            MOVE "GlParmCr" TO F-FORMNAME.
            PERFORM OPEN-010 THRU OPEN-020.
            PERFORM DISPLAY-FORM.
            PERFORM GET-105 THRU GET-130.
            MOVE 2910 TO POS.
            DISPLAY
           "<F3>=GENERAL-FIELDS, <F7>=SALES-FIELDS, <F9>=C/BOOK-FIELDS "
              AT POS.
       FILL-1001.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CURRENTCRPER" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 GLPA-CURRENT-CRPER.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-CURRENT-CRPER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1001.
            IF F-EXIT-CH = X"01"
               MOVE 1 TO SUB-1 F-INDEX
               GO TO FILL-1037.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1000.
       FILL-1005.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "REMIT" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLPA-CR-REMIT.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-CR-REMIT NOT = "Y" AND NOT = "N"
               MOVE "THIS FIELD MUST BE Y OR N, RE-ENTER" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1005.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1005.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1005.
       FILL-1015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CRTRANSNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 GLPA-CRTRANSNO.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-CRTRANSNO = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1015.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1015.
       FILL-1030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CRCONTROL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM NUMBER-CHECK.
            IF SIGN-FOUND = 9 OR = 7
               GO TO FILL-1030.
            MOVE WS-GLNO-CHECK TO GLPA-GLCRED-NO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-GLCRED-NO = " "
                MOVE "THIS ACCOUNT CANNOT BE BLANK, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-1030.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1015.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1030.
       FILL-1035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CRBANK" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM NUMBER-CHECK.
            IF SIGN-FOUND = 9 OR = 7
               GO TO FILL-1035.
            MOVE WS-GLNO-CHECK TO GLPA-GLBANK F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-GLBANK = " "
                MOVE "THIS ACCOUNT CANNOT BE BLANK, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-1035.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1030.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1035.
       FILL-1037.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "VATACC" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM NUMBER-CHECK.
            IF SIGN-FOUND = 9 OR = 7
               GO TO FILL-1037.
            MOVE WS-GLNO-CHECK TO GLPA-GLVAT-ACC F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-GLVAT-ACC = " "
                MOVE "THIS ACCOUNT CANNOT BE BLANK, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-1037.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1035.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1037.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1037.

            MOVE 1 TO SUB-1 F-INDEX.
       FILL-1040.
            MOVE "                             " TO F-NAMEFIELD.
            MOVE "IND" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLPA-IND (SUB-1).
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
             IF SUB-1 = 1
               GO TO FILL-1037
             ELSE
               SUBTRACT 1 FROM SUB-1 F-INDEX
               GO TO FILL-1040.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1040.
            IF GLPA-IND (SUB-1) = " " OR = "0"
               MOVE "THIS FIELD MUST BE > SPACES, ENTER A LETTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1040.
       FILL-1045.
            MOVE "                             " TO F-NAMEFIELD.
            MOVE "NUM" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM NUMBER-CHECK.
            IF SIGN-FOUND = 9 OR = 7
               GO TO FILL-1045.
            MOVE WS-GLNO-CHECK TO GLPA-NO (SUB-1) F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-NO (SUB-1) = " " OR = "0"
               MOVE "THIS FIELD MUST BE > SPACES, ENTER A LETTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1045.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1040.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-1040.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 11
               GO TO FILL-1040.
            GO TO FILL-1001.
      *
      *FILL-IN-SALES-FIELDS. 
      *
       FILL-2000.
           MOVE "GlParmSl" TO F-FORMNAME.
           PERFORM OPEN-010 THRU OPEN-020.
           PERFORM DISPLAY-FORM.
           PERFORM GET-205.
           MOVE 2910 TO POS.
           DISPLAY
           "<F3>=GENERAL-FIELDS,<F5>=CREDITOR-FIELDS,<F9>=C/BOOK-FIELDS"
              AT POS.
       FILL-2001.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CURRENTSLPER" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 GLPA-CURRENT-SLPER.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-CURRENT-SLPER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-2001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-2040.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-2001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-2001.
       FILL-2005.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DRCONTROL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM NUMBER-CHECK.
            IF SIGN-FOUND = 9 OR = 7
               GO TO FILL-2005.
            MOVE WS-GLNO-CHECK TO GLPA-GLDEBT-NO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-GLDEBT-NO = " "
                MOVE "THIS ACCOUNT CANNOT BE BLANK, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-2005.
            IF F-EXIT-CH = X"01"
               GO TO FILL-2001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-2005.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-2005.
       FILL-2010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DRBANK" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM NUMBER-CHECK.
            IF SIGN-FOUND = 9 OR = 7
               GO TO FILL-2010.
            MOVE WS-GLNO-CHECK TO GLPA-GLDRBANK F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-GLDRBANK = " "
                MOVE "THIS ACCOUNT CANNOT BE BLANK, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-2010.
            IF F-EXIT-CH = X"01"
               GO TO FILL-2005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-2010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-2010.
       FILL-2015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "VATOUTACC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM NUMBER-CHECK.
            IF SIGN-FOUND = 9 OR = 7
               GO TO FILL-2015.
            MOVE WS-GLNO-CHECK TO GLPA-GLVATOUTPUT-ACC F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-GLVATOUTPUT-ACC = " "
                MOVE "THIS ACCOUNT CANNOT BE BLANK, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-2015.
            IF F-EXIT-CH = X"01"
               GO TO FILL-2010.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-2015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-2015.
       FILL-2020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESACC" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM NUMBER-CHECK.
            IF SIGN-FOUND = 9 OR = 7
               GO TO FILL-2020.
            MOVE WS-GLNO-CHECK TO GLPA-GLSALES-ACC F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-GLSALES-ACC = " "
                MOVE "THIS ACCOUNT CANNOT BE BLANK, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-2020.
            IF F-EXIT-CH = X"01"
               GO TO FILL-2015.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-2020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-2020.
       FILL-2025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESADDONS" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM NUMBER-CHECK.
            IF SIGN-FOUND = 9 OR = 7
               GO TO FILL-2025.
            MOVE WS-GLNO-CHECK TO GLPA-GLSALES-ADDONS F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-GLVATOUTPUT-ACC = " "
                MOVE "THIS ACCOUNT CANNOT BE BLANK, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-2025.
            IF F-EXIT-CH = X"01"
               GO TO FILL-2020.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-2025.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-2025.
       FILL-2030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM NUMBER-CHECK.
            IF SIGN-FOUND = 9 OR = 7
               GO TO FILL-2030.
            MOVE WS-GLNO-CHECK TO GLPA-GLSALES-DISC F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-GLSALES-DISC = " "
                MOVE "THIS ACCOUNT CANNOT BE BLANK, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-2030.
            IF F-EXIT-CH = X"01"
               GO TO FILL-2025.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-2030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-2030.
       FILL-2035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESADJ" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM NUMBER-CHECK.
            IF SIGN-FOUND = 9 OR = 7
               GO TO FILL-2035.
            MOVE WS-GLNO-CHECK TO GLPA-GLSALES-ADJ F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-GLSALES-ADJ = " "
                MOVE "THIS ACCOUNT CANNOT BE BLANK, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-2035.
            IF F-EXIT-CH = X"01"
               GO TO FILL-2030.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-2035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-2035.
       FILL-2040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BDEBTACC" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM NUMBER-CHECK.
            IF SIGN-FOUND = 9 OR = 7
               GO TO FILL-2040.
            MOVE WS-GLNO-CHECK TO GLPA-GLBDEBT-ACC F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1E"
               GO TO FILL-3000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-GLBDEBT-ACC = " "
                MOVE "THIS ACCOUNT CANNOT BE BLANK, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-2040.
            IF F-EXIT-CH = X"01"
               GO TO FILL-2035.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-2040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-2040.
            GO TO FILL-2001.
      *
      *FILL-IN-CASH-BOOK-FIELDS. 
      *
       FILL-3000.
           MOVE "GlParmCb" TO F-FORMNAME.
           PERFORM OPEN-010 THRU OPEN-020.
           PERFORM DISPLAY-FORM.
           PERFORM GET-305.
           MOVE 2910 TO POS.
           DISPLAY
           "<F3>=GENERAL-FIELDS,<F5>=CREDITOR-FIELDS,<F7>=SALES-FIELDS "
              AT POS.
       FILL-3001.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CURRENTCBPER" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 GLPA-CURRENT-CBPER.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-3025.
            IF GLPA-CURRENT-CBPER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-3001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-3001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-3001.
       FILL-3005.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CBPOST" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GLPA-CB-POST.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-3001.
            IF GLPA-CB-POST NOT = "Y" AND NOT = "N"
               MOVE "THIS FIELD MUST BE Y OR N, RE-ENTER" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-3005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-3005.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-3005.
       FILL-3010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CBTRANSNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 GLPA-CBTRANSNO.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GLPA-CBTRANSNO = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-3010.
            IF F-EXIT-CH = X"01"
               GO TO FILL-3005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-3010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-3010.
       FILL-3015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CAMS-BANK-NAME" TO F-FIELDNAME.
            MOVE 14               TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20               TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD      TO GLPA-CAMS-BANK-NAME.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-3010.
            IF GLPA-CAMS-BANK-NAME = " "
               MOVE "THIS FIELD MUST BE A VALID BANK NAME" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-3015.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-3015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-3015.
       FILL-3020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CAMS-BANK-CODE" TO F-FIELDNAME.
            MOVE 14               TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6                TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD      TO GLPA-CAMS-BANK-CODE.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-3015.
            IF GLPA-CAMS-BANK-CODE = " "
               MOVE "THIS FIELD MUST BE A VALID BANK CODE" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-3020.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-3020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-3020.
       FILL-3025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CAMS-BANK-ACC"  TO F-FIELDNAME.
            MOVE 13               TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11               TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD      TO GLPA-CAMS-BANK-ACC.
            IF F-EXIT-CH = X"17"
               GO TO FILL-001.
            IF F-EXIT-CH = X"19"
               GO TO FILL-1000.
            IF F-EXIT-CH = X"1C"
               GO TO FILL-2000.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-3020.
            IF GLPA-CAMS-BANK-ACC = " "
               MOVE "THIS FIELD MUST BE A VALID BANK ACC" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-3025.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-3025.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-3025.

            GO TO FILL-3001.
       FILL-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE 0 TO GLPA-CURRENT-GLPER
                       GLPA-CURRENT-CRPER
                       GLPA-CURRENT-SLPER
                       GLPA-CURRENT-CBPER
                       GLPA-GLTRANSNO
                       GLPA-CRTRANSNO
                       GLPA-CBTRANSNO.
             MOVE 1 TO SUB-1.
       CLSC-005.
             MOVE 0 TO GLPA-PER (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 14
                GO TO CLSC-005.
             MOVE 1 TO SUB-1.
       CLSC-010.
             MOVE " " TO GLPA-SHORT-CR (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 11
                GO TO CLSC-010.
             MOVE 1 TO SUB-1.
             MOVE " " TO GLPA-NAME
                         GLPA-ADD1
                         GLPA-ADD2
                         GLPA-ADD3
                         GLPA-ADD4
                         GLPA-ADD5
                         GLPA-ADD6
                         GLPA-RECJRN-POST
                         GLPA-GLCRED-NO
                         GLPA-GLVAT-ACC
                         GLPA-GLVAT-REG-NO
                         GLPA-GLBANK
                         GLPA-GLDEBT-NO
                         GLPA-GLDRBANK
                         GLPA-GLVATOUTPUT-ACC
                         GLPA-GLSALES-ACC
                         GLPA-GLSALES-ADDONS
                         GLPA-GLSALES-DISC
                         GLPA-GLSALES-ADJ
                         GLPA-GLBDEBT-ACC
                         GLPA-CB-POST.
             MOVE WS-NUMBER TO GLPA-RECORD.

             UNLOCK GLPARAMETER-FILE.
       CLSC-999.
             EXIT.
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF NEW-NO = "Y"
                GO TO DDR-999.
       DDR-010.
           MOVE "DELETE NOT ALLOWED IN THIS PROGRAM, 'ESC' TO CLEAR."
               TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
           GO TO DDR-999.
       DDR-020.
            DELETE GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       REWRITE-RECORD SECTION.
       RDR-005.
            IF NEW-NO = "Y"
               GO TO RDR-020.
       RDR-010.
            REWRITE GLPARAMETER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RDR-020.
            GO TO RDR-999.
       RDR-020.
            WRITE GLPARAMETER-REC
              INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RDR-010.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE GLPA-RECORD TO WS-NUMBER.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
        RD-010.
           READ GLPARAMETER-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-NO
                MOVE WS-NUMBER TO GLPA-RECORD
                GO TO RD-999.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON READ, PRESS 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RD-010.
           MOVE "N" TO NEW-NO.
           MOVE GLPA-RECORD TO WS-SAVE.
       RD-999.
             EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE WS-NUMBER TO GLPA-RECORD.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       STR-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           CALL "OPENFILE" USING  F-ERROR1
                                  F-FH
                                  F-FILENAME
                                  F-CBFILENAME
                                  F-FILENAME
                                  F-INTEGERZERO
                                  F-OPENMODE.
            IF F-ERROR1 NOT = 0
                DISPLAY "OPEN-FILE FORM @ OPEN-010 ERROR"
                DISPLAY F-ERROR1
                STOP RUN.
       OPEN-020.
            MOVE 8 TO F-CBFORMNAME.
            CALL "OPENFORM" USING  F-ERROR2
                                   F-FH
                                   F-FORMNAME
                                   F-CBFORMNAME
                                   F-FORM
                                   F-CBMAX.
            IF F-ERROR2 NOT = 0
                DISPLAY "OPEN-FORM @ OPEN-020 ERROR"
                DISPLAY F-ERROR2
                STOP RUN.
            CALL "CLOSEFILE" USING F-ERROR1
                                       F-FH.
            IF F-ERROR1 NOT = 0
                DISPLAY "CLOSEFILE ERROR"
                DISPLAY F-ERROR1
                STOP RUN.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE GLPARAMETER-FILE.
           EXIT PROGRAM.
      *      STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldIndex1".
       Copy "WriteFieldNumeric".
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
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
