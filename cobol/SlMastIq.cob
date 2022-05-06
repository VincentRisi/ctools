        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlMastIq.
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
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
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
            MOVE 3     TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 SA-ANALYSIS-CODE.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"0C"
                 PERFORM READ-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-999.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-PREVIOUS
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-999.
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
                MOVE "INVALID ANALYSIS NUMBER ENTERED, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-999.
            GO TO GET-005.
        GET-003.
            MOVE "KEY"            TO F-FIELDNAME
            MOVE 3                TO F-CBFIELDNAME
            MOVE SA-ANALYSIS-CODE TO F-EDNAMEFIELDANAL
            MOVE 2                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.
        GET-005.
            MOVE "NAME"  TO F-FIELDNAME
            MOVE 4       TO F-CBFIELDNAME
            MOVE SA-NAME TO F-NAMEFIELD
            MOVE 25      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COSTWEEK"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE SA-COST-WEEK TO F-EDNAMEFIELDREC
            MOVE 12           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC

            MOVE "COSTPTD"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            MOVE SA-COST-PTD TO F-EDNAMEFIELDREC
            MOVE 12          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC

            MOVE "COSTYTD"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            MOVE SA-COST-YTD TO F-EDNAMEFIELDREC
            MOVE 12          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC

            MOVE "SALESWEEK"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            MOVE SA-SALES-WEEK TO F-EDNAMEFIELDREC
            MOVE 12            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC

            MOVE "SALESPTD"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE SA-SALES-PTD TO F-EDNAMEFIELDREC
            MOVE 12           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC

            MOVE "SALESYTD"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE SA-SALES-YTD TO F-EDNAMEFIELDREC
            MOVE 12           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            GO TO GET-001.
       GET-999.
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
            MOVE " " TO SA-NAME
            MOVE WS-NUMBER TO SA-ANALYSIS-CODE.
       CLSC-999.
             EXIT.      
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE 0 TO WS-SALES-ST1.
           MOVE SA-ANALYSIS-CODE TO WS-NUMBER.
           START SALES-ANALYSIS KEY NOT < SA-KEY.
        RD-010.
           READ SALES-ANALYSIS
                 INVALID KEY NEXT SENTENCE.
           IF WS-SALES-ST1 = 23 OR 35 OR 49
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
           READ SALES-ANALYSIS NEXT
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
       RNX-001.
           MOVE 0 TO WS-SALES-ST1.
       RNX-005.
           READ SALES-ANALYSIS PREVIOUS
             AT END
              MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO SA-ANALYSIS-CODE
                         WS-NUMBER
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-SALES-ST1 =  23 OR 35 OR 49
               MOVE "SALES ANALYSIS BUSY READ-PREV-23, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO RNX-005.
           IF WS-SALES-ST1 NOT = 0
               MOVE "SALES ANALYSIS BUSY READ-PREV, 'ESC' TO RETRY"
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
           MOVE "SlMastIq"      TO F-FORMNAME.
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
      *
      * END-OF-JOB
