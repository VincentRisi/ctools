        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlMastMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectSlMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdSales.

       WORKING-STORAGE SECTION.
       77  NEW-NO             PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-NUMBER          PIC 99 VALUE 0.
       77  WS-SAVE            PIC 99 VALUE 0.
       01  WS-SALES-STATUS.
           03  WS-SALES-ST1   PIC 99.
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
       CONTROL-010.
           PERFORM DISPLAY-FORM
           PERFORM GET-DATA
           PERFORM FILL-DATA
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 0 TO SALES-ANALYSIS-REC.
            MOVE "N" TO NEW-NO
                        WS-END.
       GET-001.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "KEY" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 SA-ANALYSIS-CODE.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-SAVE TO SA-ANALYSIS-CODE
                 PERFORM START-RECORD
                 PERFORM READ-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-003 THRU GET-005
                 GO TO GET-001.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-PREVIOUS
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-003 THRU GET-005
                 GO TO GET-001.
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
            GO TO GET-005.
        GET-003.
            MOVE "KEY" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE SA-ANALYSIS-CODE TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.
        GET-005.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE SA-NAME TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COSTWEEK" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE SA-COST-WEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "COSTPTD" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE SA-COST-PTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "COSTYTD" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE SA-COST-YTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "SALESWEEK" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE SA-SALES-WEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "SALESPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE SA-SALES-PTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "SALESYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE SA-SALES-YTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.
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
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO SA-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-RECORD
               PERFORM READ-PREVIOUS
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
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.
       FILL-005.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COSTWEEK" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 SA-COST-WEEK.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-RECORD
               PERFORM READ-PREVIOUS
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
               GO TO FILL-005.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-005.
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COSTPTD" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 SA-COST-PTD.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-005.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-RECORD
               PERFORM READ-PREVIOUS
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
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-010.
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COSTYTD" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 SA-COST-YTD.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-RECORD
               PERFORM READ-PREVIOUS
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
               GO TO FILL-015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-015.
       FILL-017.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESWEEK" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 SA-SALES-WEEK.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-RECORD
               PERFORM READ-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
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
               GO TO FILL-017.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-017.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 SA-SALES-PTD.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-RECORD
               PERFORM READ-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-017.
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
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 SA-SALES-YTD.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-RECORD
               PERFORM READ-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
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

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF SA-SALES-WEEK = 0
             IF SA-SALES-PTD = 0
              IF SA-SALES-YTD = 0
            IF SA-COST-WEEK = 0
             IF SA-COST-PTD = 0
              IF SA-COST-YTD = 0
              GO TO DDR-010.
            MOVE
            "DELETE NOT ALLOWED, AMOUNT MUST = 0, 'ESC' TO CLEAR."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO DDR-999.
            IF NEW-NO = "Y"
                GO TO DDR-999.
       DDR-010.
            DELETE SALES-ANALYSIS
               INVALID KEY NEXT SENTENCE.
            IF WS-SALES-ST1 NOT = 0
            MOVE "SL-MAST BUSY ON DELETE, 'ESC' TO RETRY." TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE 0 TO SA-ANALYSIS-CODE
                       SA-COST-WEEK
                       SA-COST-PTD
                       SA-COST-YTD
                       SA-SALES-WEEK
                       SA-SALES-PTD
                       SA-SALES-YTD.
            MOVE " " TO SA-NAME.
            MOVE WS-NUMBER TO SA-ANALYSIS-CODE.
            UNLOCK SALES-ANALYSIS.
       CLSC-999.
             EXIT.      
      *
       REWRITE-RECORD SECTION.
       RDR-010.
            IF NEW-NO = "Y"
               GO TO RDR-020.
            REWRITE SALES-ANALYSIS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-SALES-ST1 NOT = 0
                MOVE "SALES ANALYSIS BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-SALES-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-SALES-ST1
                GO TO RDR-010.
            GO TO RDR-999.
       RDR-020.
            WRITE SALES-ANALYSIS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-SALES-ST1 NOT = 0
               MOVE "SALES ANALYSIS BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO RDR-020.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE 0 TO WS-SALES-ST1.
           MOVE SA-ANALYSIS-CODE TO WS-NUMBER.
           START SALES-ANALYSIS KEY NOT < SA-KEY.
        RD-010.
           READ SALES-ANALYSIS WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-SALES-ST1 =  23 OR 35 OR 49
                MOVE 0 TO WS-SALES-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-NO
                MOVE WS-NUMBER TO SA-ANALYSIS-CODE
                GO TO RD-999.
           IF WS-SALES-ST1 NOT = 0
               MOVE "SALES-ANALYSIS BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO RD-010.
           MOVE "N" TO NEW-NO.
           MOVE SA-ANALYSIS-CODE TO WS-SAVE.
       RD-999.
             EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE WS-NUMBER TO SA-KEY.
           START SALES-ANALYSIS KEY NOT < SA-KEY.
       STR-999.
             EXIT.
      *
       READ-NEXT SECTION.
       RNX-001.
           MOVE 0 TO WS-SALES-ST1.
       RNX-005.
           READ SALES-ANALYSIS NEXT WITH LOCK
             AT END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO SA-ANALYSIS-CODE
                         WS-NUMBER
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-SALES-ST1 =  23 OR 35 OR 49
               MOVE "SALES ANALYSIS BUSY READ-NEXT-23, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO RNX-005.
           IF WS-SALES-ST1 NOT = 0
               MOVE "SALES ANALYSIS BUSY READ-NEXT, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               PERFORM START-RECORD
               GO TO RNX-005.
           IF SA-ANALYSIS-CODE = 0
               PERFORM START-RECORD.
           MOVE SA-ANALYSIS-CODE TO WS-NUMBER
                                    WS-SAVE.
           MOVE "N" TO NEW-NO.
       RNX-999.
           EXIT.
      *
       READ-PREVIOUS SECTION.
       RPREV-001.
           MOVE 0 TO WS-SALES-ST1.
       RPREV-005.
           READ SALES-ANALYSIS PREVIOUS WITH LOCK
             AT END
              MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO SA-ANALYSIS-CODE
                         WS-NUMBER
               MOVE "Y" TO WS-END
               GO TO RPREV-999.
           IF WS-SALES-ST1 =  23 OR 35 OR 49
               MOVE "SALES ANALYSIS BUSY READ-PREV-23, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO RPREV-005.
           IF WS-SALES-ST1 NOT = 0
               MOVE "SALES ANALYSIS BUSY READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               PERFORM START-RECORD
               GO TO RPREV-005.
           IF SA-ANALYSIS-CODE = 0
               PERFORM START-RECORD.
           MOVE SA-ANALYSIS-CODE TO WS-NUMBER
                                    WS-SAVE.
           MOVE "N" TO NEW-NO.
       RPREV-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O SALES-ANALYSIS.
            IF WS-SALES-ST1 NOT = 0
               MOVE "SALES ANALYSIS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlMastIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE SALES-ANALYSIS.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldRec".
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
