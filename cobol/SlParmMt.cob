        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlParmMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectSlParameter".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdParam.

       WORKING-STORAGE SECTION.
       77  NEW-NO             PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-NUMBER          PIC 9 VALUE 0.
       77  WS-SAVE            PIC 9 VALUE 0.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1   PIC 99.
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
            MOVE 0 TO PA-TYPE.
            MOVE "N" TO NEW-NO
                        WS-END.
            MOVE 1 TO PA-RECORD.
            MOVE "KEY" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE PA-RECORD TO F-EDNAMEFIELDIND1.
            MOVE 1 TO F-CBFIELDLENGTH.
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
                                 PA-RECORD.
            PERFORM WRITE-FIELD-INDEX1.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-SAVE TO PA-RECORD
                 PERFORM START-RECORD
                 PERFORM READ-NEXT
              IF WS-END = "Y"
                 PERFORM CLEAR-FORM
                 PERFORM GET-003 THRU GET-005
                 GO TO GET-999
              ELSE
                 GO TO GET-003.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
            PERFORM READ-RECORD.
            IF NEW-NO = "Y"
               GO TO GET-999.
            GO TO GET-005.
        GET-003.
            MOVE "KEY" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE PA-RECORD TO F-EDNAMEFIELDIND1.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INDEX1.
        GET-005.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE PA-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE PA-ADD1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE PA-ADD2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE PA-ADD3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CODE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE PA-CODE TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE PA-DEL1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE PA-DEL2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE PA-DEL3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PHONE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE PA-PHONE TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FAX" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE PA-FAX TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COMMENT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE PA-COMMENT TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COREGNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE PA-CO-REG-NO TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COVATNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE PA-CO-VAT-NO TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDER" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE PA-ORDER-NUMBER TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE PA-INVOICE-NUMBER TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "CREDIT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE PA-CREDIT-NUMBER TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "CASHREC" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE PA-CASH-RECEIPT-NUMBER TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "STOCKREC" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE PA-STOCK-RECEIPT-NUMBER TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "JOURNAL" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE PA-JOURNAL-NUMBER TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "QUOTE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE PA-QUOTE-NUMBER TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "SUPPORDER" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE PA-SUPPLY-ORDER-NUMBER TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "GST" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE PA-GST-PERCENT TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "DRTRANS" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE PA-DRTRANS-NUMBER TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "STTRANS" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE PA-STTRANS-NUMBER TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "CURRWEEK" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE PA-CURRENT-PER-WW TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "CURRMTH" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE PA-CURRENT-PER-MM TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "CURRYR"          TO F-FIELDNAME.
            MOVE 6                 TO F-CBFIELDNAME.
            MOVE PA-CURRENT-PER-YY TO F-EDNAMEFIELDPOST.
            MOVE 4                 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-POST.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
               GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PA-NAME.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1050.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-NAME = "         "
               MOVE "THIS FIELD MUST BE > SPACES, ENTER A NAME"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
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
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-001.
       FILL-005.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ADD1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PA-ADD1.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF PA-ADD1 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, ENTER A NAME"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-005.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
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
               GO TO FILL-005.
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ADD2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PA-ADD2.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-005.
            IF PA-ADD2 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, ENTER A NAME"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
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
               GO TO FILL-010.
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ADD3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PA-ADD3.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF PA-ADD3 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, ENTER A NAME"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-015.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
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
               GO TO FILL-015.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CODE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PA-CODE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF PA-CODE = "    "
               MOVE "THIS FIELD MUST BE > SPACES, ENTER A CODE"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
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
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-020.
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DEL1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PA-DEL1.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF PA-DEL1 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-025.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
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
               GO TO FILL-025.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-025.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DEL2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PA-DEL2.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF PA-DEL2 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-030.
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
               GO TO FILL-030.
       FILL-035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DEL3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PA-DEL3.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF PA-DEL3 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-035.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-035.
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
               GO TO FILL-035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-035.
       FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PHONE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PA-PHONE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-035.
            IF PA-DEL1 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-040.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-040.
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
               GO TO FILL-040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-040.
       FILL-045.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "FAX" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PA-FAX.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-040.
            IF PA-DEL2 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-045.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-045.
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
               GO TO FILL-045.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-045.
       FILL-050.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COMMENT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PA-COMMENT.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-045.
            IF PA-DEL3 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-050.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-050.
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
               GO TO FILL-050.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-050.
       FILL-055.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COREGNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PA-CO-REG-NO.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-050.
            IF PA-DEL1 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-055.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-055.
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
               GO TO FILL-055.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-055.
       FILL-060.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COVATNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PA-CO-VAT-NO.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-055.
            IF PA-DEL2 = "         "
               MOVE "THIS FIELD MUST BE > SPACES, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-060.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-060.
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
               GO TO FILL-060.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-060.
       FILL-090.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ORDER" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 PA-ORDER-NUMBER.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-ORDER-NUMBER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-090.
            IF F-EXIT-CH = X"01"
               GO TO FILL-060.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
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
               GO TO FILL-090.
       FILL-095.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 PA-INVOICE-NUMBER.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-INVOICE-NUMBER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-095.
            IF F-EXIT-CH = X"01"
               GO TO FILL-090.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
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
               GO TO FILL-095.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-095.
       FILL-1010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CREDIT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 PA-CREDIT-NUMBER.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-CREDIT-NUMBER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1010.
            IF F-EXIT-CH = X"01"
               GO TO FILL-095.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
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
               GO TO FILL-1010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-1010.
       FILL-1015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CASHREC" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 PA-CASH-RECEIPT-NUMBER.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-CASH-RECEIPT-NUMBER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1015.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
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
               GO TO FILL-1015.
       FILL-1020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STOCKREC" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 PA-STOCK-RECEIPT-NUMBER.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-STOCK-RECEIPT-NUMBER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1020.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
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
               GO TO FILL-1020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-1020.
       FILL-1025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "JOURNAL" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 PA-JOURNAL-NUMBER.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-JOURNAL-NUMBER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1025.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1020.
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
               GO TO FILL-1025.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-1025.
       FILL-1027.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QUOTE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 PA-QUOTE-NUMBER.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-QUOTE-NUMBER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1027.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1025.
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
               GO TO FILL-1027.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-1027.
       FILL-1028.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SUPPORDER" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 PA-SUPPLY-ORDER-NUMBER.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-SUPPLY-ORDER-NUMBER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1028.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1027.
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
               GO TO FILL-1028.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-1028.
       FILL-1030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "GST" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 PA-GST-PERCENT.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-GST-PERCENT = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1030.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1028.
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
               GO TO FILL-1030.
       FILL-1035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DRTRANS" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 PA-DRTRANS-NUMBER.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-DRTRANS-NUMBER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1035.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
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
               GO TO FILL-1035.
       FILL-1040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STTRANS" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 PA-STTRANS-NUMBER.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-STTRANS-NUMBER = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1040.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1035.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
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
               GO TO FILL-1040.
       FILL-1042.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CURRWEEK" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 PA-CURRENT-PER-WW.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-CURRENT-PER-WW = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1042.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1040.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
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
               GO TO FILL-1042.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-1042.
       FILL-1045.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CURRMTH" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 PA-CURRENT-PER-MM.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-CURRENT-PER-MM = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1045.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1042.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
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
               GO TO FILL-1045.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-1045.
       FILL-1050.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CURRYR"     TO F-FIELDNAME.
            MOVE 6            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 4            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDPOST
                                 PA-CURRENT-PER-YY.
            PERFORM WRITE-FIELD-POST.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PA-CURRENT-PER-YY = 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-1050.
            IF F-EXIT-CH = X"01"
               GO TO FILL-1045.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
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
               GO TO FILL-1050.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-1050.

            GO TO FILL-001.
       FILL-999.
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
            DELETE PARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER FILE BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO DDR-020.
       DDR-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE 0 TO PA-ORDER-NUMBER
                       PA-INVOICE-NUMBER
                       PA-CREDIT-NUMBER
                       PA-CASH-RECEIPT-NUMBER
                       PA-STOCK-RECEIPT-NUMBER
                       PA-JOURNAL-NUMBER
                       PA-QUOTE-NUMBER
                       PA-SUPPLY-ORDER-NUMBER
                       PA-GST-PERCENT
                       PA-DRTRANS-NUMBER
                       PA-STTRANS-NUMBER
                       PA-CURRENT-PER-WW
                       PA-CURRENT-PER-MM
                       PA-CURRENT-PER-YY.
             MOVE " " TO PA-NAME
                         PA-ADD1
                         PA-ADD2
                         PA-ADD3
                         PA-CODE
                         PA-DEL1
                         PA-DEL2
                         PA-DEL3
                         PA-PHONE
                         PA-FAX
                         PA-COMMENT
                         PA-CO-REG-NO
                         PA-CO-VAT-NO.
             MOVE WS-NUMBER TO PA-RECORD.
             UNLOCK PARAMETER-FILE.
       CLSC-999.
             EXIT.
      *
       REWRITE-RECORD SECTION.
       RDR-010.
            IF NEW-NO = "Y"
               GO TO RDR-020.
            REWRITE PARAMETER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "PARAMETER FILE BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RDR-010.
            GO TO RDR-999.
       RDR-020.
            WRITE PARAMETER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               MOVE "PARAMETER RECORD BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RDR-020.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE 0         TO PA-TYPE.
           MOVE PA-RECORD TO WS-NUMBER.
           START PARAMETER-FILE KEY NOT < PA-KEY.
        RD-010.
           READ PARAMETER-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-SLPARAMETER-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-NO
                MOVE WS-NUMBER TO PA-RECORD
                GO TO RD-999.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RD-010.
           MOVE "N" TO NEW-NO.
           MOVE PA-RECORD TO WS-SAVE.
       RD-999.
             EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE 0         TO PA-TYPE.
           MOVE WS-NUMBER TO PA-RECORD.
           START PARAMETER-FILE KEY NOT < PA-KEY.
       STR-999.
             EXIT.
      *
       READ-NEXT SECTION.
       RNX-001.
           MOVE 0 TO WS-SLPARAMETER-ST1.
       RNX-005.
           READ PARAMETER-FILE NEXT WITH LOCK
             AT END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO PARAMETER-REC
               MOVE 0   TO PA-RECORD
                           WS-NUMBER
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-SLPARAMETER-ST1 =  23 OR 35 OR 49 OR 51
               MOVE "PARAMETER FILE BUSY ON READ,  'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RNX-005.
           IF WS-SLPARAMETER-ST1 NOT = 0
            MOVE "PARAMETER BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               PERFORM START-RECORD
               GO TO RNX-005.
           IF PA-TYPE NOT = 0
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO PARAMETER-REC
               MOVE 0   TO PA-RECORD
                           WS-NUMBER
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF PA-KEY = 0
               PERFORM START-RECORD.
           MOVE PA-RECORD TO WS-NUMBER
                             WS-SAVE.
           MOVE "N" TO NEW-NO.
       RNX-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O PARAMETER-FILE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlParmMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE PARAMETER-FILE.
           EXIT PROGRAM.
      *     STOP RUN.
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
       Copy "WriteFieldPost".
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
      *
      * END-OF-JOB
