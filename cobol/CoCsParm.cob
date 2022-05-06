        IDENTIFICATION DIVISION.
        PROGRAM-ID. CoCsParm.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT PARAMETER-FILE ASSIGN TO Ws-Parameter
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-PARAMETER-STATUS
               RECORD KEY IS PA-KEY.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdParam.
           
       WORKING-STORAGE SECTION.
       77  NEW-NO             PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-NUMBER          PIC 9 VALUE 0.
       77  WS-SAVE            PIC 9 VALUE 0.
       01  WS-PARAMETER-STATUS.
           03  WS-PA-ST1   PIC 99.
      *     03  WS-PA-ST2   PIC X.
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
       GET-DATA SECTION.
       GET-000.
            MOVE "N" TO NEW-NO
                        WS-END.
            MOVE 6 TO PA-TYPE.
            
            MOVE "QUSCODE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 1 TO PA-RECORD F-EDNAMEFIELDFAX.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-FAX.
        GET-001.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "QUSCODE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDFAX
                                 PA-RECORD.
            PERFORM WRITE-FIELD-FAX.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-SAVE TO PA-RECORD
                 PERFORM START-RECORD
                 PERFORM READ-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-003 THRU GET-005
                 GO TO GET-999.
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
            MOVE "QUSCODE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE PA-RECORD TO F-EDNAMEFIELDFAX.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-FAX.
        GET-005.
            MOVE "QUTPULL" TO F-FIELDNAME
            MOVE 7 TO F-CBFIELDNAME
            MOVE INVQUES-PRT-PULLERS TO F-NAMEFIELD
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QUTCASH" TO F-FIELDNAME
            MOVE 7 TO F-CBFIELDNAME
            MOVE INVQUES-CASH-SALES TO F-NAMEFIELD
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CHCKQUT"            TO F-FIELDNAME
            MOVE 7                    TO F-CBFIELDNAME
            MOVE INVQUES-CHECK-QUOTES TO F-NAMEFIELD
            MOVE 1                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STCHNGE"            TO F-FIELDNAME
            MOVE 7                    TO F-CBFIELDNAME
            MOVE INVQUES-STOCK-CHANGE TO F-NAMEFIELD
            MOVE 1                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STTOMAX"            TO F-FIELDNAME
            MOVE 7                    TO F-CBFIELDNAME
            MOVE INVQUES-STOCK-TO-MAX TO F-NAMEFIELD
            MOVE 1                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "MU-GP"                  TO F-FIELDNAME
            MOVE 5                        TO F-CBFIELDNAME
            MOVE INVQUES-MU-GP-PERC       TO F-NAMEFIELD
            MOVE 1                        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE "ACC-CONTACT"       TO F-FIELDNAME
            MOVE 11                  TO F-CBFIELDNAME
            MOVE INVQUES-ACC-CONTACT TO F-NAMEFIELD
            MOVE 15                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE "ACC-PHONE"         TO F-FIELDNAME
            MOVE 9                   TO F-CBFIELDNAME
            MOVE INVQUES-ACC-PHONE   TO F-NAMEFIELD
            MOVE 15                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE "NORMPRT"               TO F-FIELDNAME
            MOVE 7                       TO F-CBFIELDNAME
            MOVE INVQUES-PS-NORM-PRINTER TO F-EDNAMEFIELDANAL
            MOVE 2                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "REPRPRT"               TO F-FIELDNAME
            MOVE 7                       TO F-CBFIELDNAME
            MOVE INVQUES-PS-REPR-PRINTER TO F-EDNAMEFIELDANAL
            MOVE 2                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "RUSHPRT"               TO F-FIELDNAME
            MOVE 7                       TO F-CBFIELDNAME
            MOVE INVQUES-PS-RUSH-PRINTER TO F-EDNAMEFIELDANAL
            MOVE 2                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "LABELPRT"               TO F-FIELDNAME
            MOVE 8                        TO F-CBFIELDNAME
            MOVE INVQUES-ST-LABEL-PRINTER TO F-EDNAMEFIELDANAL
            MOVE 2                        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "STQULABL"               TO F-FIELDNAME
            MOVE 8                        TO F-CBFIELDNAME
            MOVE INVQUES-ST-PRINT-LABELS  TO F-NAMEFIELD
            MOVE 1                        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACC-OVER-LIMIT"         TO F-FIELDNAME
            MOVE 14                       TO F-CBFIELDNAME
            MOVE INVQUES-ACC-OVER-LIMIT   TO F-NAMEFIELD
            MOVE 1                        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PAUSE-PSLIP"            TO F-FIELDNAME
            MOVE 11                       TO F-CBFIELDNAME
            MOVE INVQUES-PAUSE-ON-PSLIP   TO F-NAMEFIELD
            MOVE 1                        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PAUSE-BACKUP"           TO F-FIELDNAME
            MOVE 12                       TO F-CBFIELDNAME
            MOVE INVQUES-PAUSE-BACKUP     TO F-NAMEFIELD
            MOVE 1                        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "ENTRIES ARE: N OR Y" TO WS-MESSAGE
            PERFORM Error4-000.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QUTPULL" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INVQUES-PRT-PULLERS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-PRT-PULLERS NOT = "N" AND NOT = "Y"
               MOVE "THIS FIELD MUST BE Y OR N, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-075.
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
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ENTRIES ARE: N OR Y" TO WS-MESSAGE
            PERFORM Error4-000.
            MOVE "QUTCASH" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INVQUES-CASH-SALES.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-CASH-SALES NOT = "N" AND NOT = "Y"
               MOVE "THIS FIELD MUST BE Y OR N, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
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
       FILL-012.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ENTRIES ARE: A=NO COD ACC'S, N=NO CHECK, Y=ALL A/C'S."
              TO WS-MESSAGE
            PERFORM Error4-000.
            MOVE "CHCKQUT" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INVQUES-CHECK-QUOTES.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-CHECK-QUOTES NOT = "A" AND NOT = "N"
                                AND NOT = "Y"
               MOVE "THIS FIELD MUST BE A, N OR Y, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-012.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-012.
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
               GO TO FILL-012.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ENTRIES ARE: N OR Y" TO WS-MESSAGE
            PERFORM Error4-000.
            MOVE "STCHNGE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INVQUES-STOCK-CHANGE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-STOCK-CHANGE NOT = "N" AND NOT = "Y"
               MOVE "THIS FIELD MUST BE Y OR N, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF F-EXIT-CH = X"01"
               GO TO FILL-012.
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
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ENTRIES ARE: N OR Y" TO WS-MESSAGE
            PERFORM Error4-000.
            MOVE "STTOMAX" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INVQUES-STOCK-TO-MAX.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-STOCK-TO-MAX NOT = "N" AND NOT = "Y"
               MOVE "THIS FIELD MUST BE Y OR N, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-025.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
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
       FILL-028.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ENTRIES ARE: N=M/U, Y=G/P FORMAT." TO WS-MESSAGE
            PERFORM Error4-000.
            MOVE "MU-GP"     TO F-FIELDNAME.
            MOVE 5           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INVQUES-MU-GP-PERC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-MU-GP-PERC NOT = "Y" AND NOT = "N"
              MOVE "THIS FIELD MUST BE Y OR N, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-028.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-028.
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
               GO TO FILL-028.
       FILL-0291.
            MOVE "                    " TO F-NAMEFIELD.
            PERFORM Error4-020.
            MOVE "ACC-CONTACT" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INVQUES-ACC-CONTACT.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-ACC-CONTACT NOT > " "
              MOVE "THIS FIELD MUST BE > SPACES, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-0291.
            IF F-EXIT-CH = X"01"
               GO TO FILL-028.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-0291.
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
               GO TO FILL-0291.
       FILL-0292.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACC-PHONE"   TO F-FIELDNAME.
            MOVE 9             TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INVQUES-ACC-PHONE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-ACC-PHONE NOT > " "
              MOVE "THIS FIELD MUST BE > SPACES, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-0292.
            IF F-EXIT-CH = X"01"
               GO TO FILL-0291.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-0292.
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
               GO TO FILL-0292.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NORMPRT"   TO F-FIELDNAME.
            MOVE 7           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO INVQUES-PS-NORM-PRINTER
                                 F-EDNAMEFIELDANAL.
            MOVE 2                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-PS-NORM-PRINTER NOT > 0
              MOVE "THIS FIELD MUST BE A VALID NUMBER, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE INVQUES-PS-NORM-PRINTER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH = X"01"
               GO TO FILL-0292.
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
       FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "REPRPRT"   TO F-FIELDNAME.
            MOVE 7           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO INVQUES-PS-REPR-PRINTER
                                 F-EDNAMEFIELDANAL.
            MOVE 2                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-PS-REPR-PRINTER NOT > 0
              MOVE "THIS FIELD MUST BE A VALID NUMBER, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-040.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
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
       FILL-050.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "RUSHPRT"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO INVQUES-PS-RUSH-PRINTER
                                 F-EDNAMEFIELDANAL.
            MOVE 2                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-PS-RUSH-PRINTER NOT > 0
              MOVE "THIS FIELD MUST BE A VALID NUMBER, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-050.
            IF F-EXIT-CH = X"01"
               GO TO FILL-040.
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
       FILL-055.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LABELPRT"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO INVQUES-ST-LABEL-PRINTER
                                 F-EDNAMEFIELDANAL.
            MOVE 2                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-ST-LABEL-PRINTER NOT > 0
              MOVE "THIS FIELD MUST BE A VALID NUMBER, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE INVQUES-ST-LABEL-PRINTER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-055.
            IF F-EXIT-CH = X"01"
               GO TO FILL-050.
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
       FILL-060.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ENTRIES ARE: N OR Y" TO WS-MESSAGE
            PERFORM Error4-000.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STQULABL"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INVQUES-ST-PRINT-LABELS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-ST-PRINT-LABELS NOT = "Y" AND NOT = "N"
              MOVE "THIS FIELD MUST BE Y OR N, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-060.
            IF F-EXIT-CH = X"01"
               GO TO FILL-055.
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
       FILL-065.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ENTRIES ARE: N OR Y" TO WS-MESSAGE
            PERFORM Error4-000.
            MOVE "ACC-OVER-LIMIT" TO F-FIELDNAME.
            MOVE 14               TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1                TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD      TO INVQUES-ACC-OVER-LIMIT.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-ACC-OVER-LIMIT NOT = "Y" AND NOT = "N"
              MOVE "THIS FIELD MUST BE Y OR N, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-065.
            IF F-EXIT-CH = X"01"
               PERFORM Error4-020
               GO TO FILL-060.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-065.
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
               GO TO FILL-065.
       FILL-070.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ENTRIES ARE: N OR Y" TO WS-MESSAGE
            PERFORM Error4-000.
            MOVE "PAUSE-PSLIP"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1                TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD      TO INVQUES-PAUSE-ON-PSLIP.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-PAUSE-ON-PSLIP NOT = "Y" AND NOT = "N"
              MOVE "THIS FIELD MUST BE Y OR N, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-070.
            IF F-EXIT-CH = X"01"
               PERFORM Error4-020
               GO TO FILL-065.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-070.
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
               GO TO FILL-070.
       FILL-075.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ENTRIES ARE: N OR Y" TO WS-MESSAGE
            PERFORM Error4-000.
            MOVE "PAUSE-BACKUP"   TO F-FIELDNAME.
            MOVE 12               TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1                TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD      TO INVQUES-PAUSE-BACKUP.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INVQUES-PAUSE-BACKUP NOT = "Y" AND NOT = "N"
              MOVE "THIS FIELD MUST BE Y OR N, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-075.
            IF F-EXIT-CH = X"01"
               PERFORM Error4-020
               GO TO FILL-070.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-075.
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
               GO TO FILL-075.

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO INVQUES-PRT-PULLERS
                         INVQUES-CASH-SALES
                         INVQUES-ST-PRINT-LABELS
                         INVQUES-STOCK-CHANGE
                         INVQUES-STOCK-TO-MAX
                         INVQUES-MU-GP-PERC
                         INVQUES-ACC-CONTACT
                         INVQUES-ACC-PHONE
                         INVQUES-ACC-OVER-LIMIT
                         INVQUES-PAUSE-ON-PSLIP
                         INVQUES-PAUSE-BACKUP.
             MOVE 0   TO INVQUES-PS-NORM-PRINTER
                         INVQUES-PS-REPR-PRINTER
                         INVQUES-PS-RUSH-PRINTER
                         INVQUES-ST-LABEL-PRINTER.
             MOVE WS-NUMBER TO PA-RECORD.
       CLSC-999.
             EXIT.
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF NEW-NO = "Y"
                GO TO DDR-999.
       DDR-010.
            DELETE PARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-PA-ST1 NOT = 0
                MOVE 0 TO WS-PA-ST1
                MOVE "PARAMETER BUSY ON DELETE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       REWRITE-RECORD SECTION.
       RDR-010.
            IF NEW-NO = "Y"
               GO TO RDR-020.
            REWRITE PARAMETER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-PA-ST1 NOT = 0
                MOVE 0 TO WS-PA-ST1
                MOVE "PARAMETER BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-020.
            GO TO RDR-999.
       RDR-020.
            WRITE PARAMETER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-PA-ST1 NOT = 0
                MOVE 0 TO WS-PA-ST1
                MOVE "PARAMETER BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-010.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE 0 TO WS-PA-ST1.
           MOVE 6 TO PA-TYPE.
           MOVE PA-RECORD TO WS-NUMBER.
           START PARAMETER-FILE KEY NOT < PA-KEY.
        RD-010.
           READ PARAMETER-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-PA-ST1 = 35 OR 49 OR 23
                MOVE 0 TO WS-PA-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-NO
                MOVE WS-NUMBER TO PA-RECORD
                GO TO RD-999.
           IF WS-PA-ST1 NOT = 0
                MOVE 0 TO WS-PA-ST1
                MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.
           MOVE "N" TO NEW-NO.
           MOVE PA-RECORD TO WS-SAVE.
       RD-999.
             EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE 6         TO PA-TYPE.
           MOVE WS-NUMBER TO PA-RECORD.
           START PARAMETER-FILE KEY NOT < PA-KEY.
       STR-999.
             EXIT.
      *
       READ-NEXT SECTION.
       RNX-001.
           MOVE 0 TO WS-PA-ST1.
       RNX-005.
           IF PA-RECORD = " "
               PERFORM START-RECORD.
           READ PARAMETER-FILE NEXT WITH LOCK
             AT END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO PA-RECORD
                           WS-NUMBER
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF PA-TYPE NOT = 6
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO PARAMETER-REC
               MOVE 0   TO PA-RECORD
                           WS-NUMBER
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-PA-ST1 = 35
               MOVE 0 TO WS-PA-ST1
               MOVE "PARAMETER BUSY ON READ-NEXT, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNX-005.
           IF WS-PA-ST1 NOT = 0
               MOVE 0 TO WS-PA-ST1
               PERFORM START-RECORD
               GO TO RNX-005.
           MOVE PA-RECORD TO WS-NUMBER
                             WS-SAVE.
           MOVE "N" TO NEW-NO.
       RNX-999.
           EXIT.
      *
        OPEN-FILES SECTION.
        OPEN-000.
            OPEN I-O PARAMETER-FILE.
            IF WS-PA-ST1 NOT = 0
               MOVE 0 TO WS-PA-ST1
               MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CoCsParm"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE PARAMETER-FILE.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldFax".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error4Message".
      *
      * END-OF-JOB
