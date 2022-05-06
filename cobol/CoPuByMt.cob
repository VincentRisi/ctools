        IDENTIFICATION DIVISION.
        PROGRAM-ID. CoPuByMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCoPullBy".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdPullBy.

       WORKING-STORAGE SECTION.
       77  NEW-NO             PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-INITIAL         PIC XX VALUE " ".
       77  WS-SAVE            PIC XX VALUE " ".
       77  WS-INVSAVE         PIC 9(6) VALUE 0.
       01  WS-PULLBY-STATUS.
           03  WS-PB-ST1   PIC 99.
      *     03  WS-PB-ST2   PIC X.
       Copy "WsDateInfo".
      *
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
            MOVE " " TO PULLBY-REC.
            MOVE "N" TO NEW-NO
                        WS-END.
       GET-001.
            MOVE "INIT" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PB-INITIAL.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-SAVE TO PB-INITIAL
                 MOVE WS-INVSAVE TO PB-INVOICE
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
            IF PB-KEY = "  " OR = "00" OR = "0 " OR = " 0"
               MOVE "ENTER A PULLERS INITIAL, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
       GET-002.
            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 PB-INVOICE.
            PERFORM WRITE-FIELD-NUMERIC.
            IF PB-INVOICE = 0
               MOVE "ENTER A NUMBER > 0, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-002.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-SAVE TO PB-INITIAL
                 MOVE WS-INVSAVE TO PB-INVOICE
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
            IF F-EXIT-CH NOT = X"0A"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
            PERFORM READ-RECORD.
            IF NEW-NO = "Y"
               GO TO GET-999.
            GO TO GET-005.
       GET-003.
            MOVE "INIT" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE PB-INITIAL TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE PB-INVOICE TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.
       GET-005.
            MOVE "LINECNT"   TO F-FIELDNAME.
            MOVE 7           TO F-CBFIELDNAME.
            MOVE PB-LINE-CNT TO F-EDNAMEFIELDLINE.
            MOVE 3           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-LINE.

            MOVE "INVDATE"          TO F-FIELDNAME.
            MOVE 7                  TO F-CBFIELDNAME.
            MOVE PB-INV-DATE        TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE       TO F-NAMEFIELD.
            MOVE 10                 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALEAMT"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME
            MOVE PB-SALE-AMOUNT TO F-EDNAMEFIELD9MIL
            MOVE 10             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                  GO TO FILL-999.
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LINECNT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 3  TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDLINE
                                PB-LINE-CNT.
            PERFORM WRITE-FIELD-LINE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PB-LINE-CNT = "     "
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-015.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
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
               GO TO FILL-015.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
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
            MOVE SPLIT-DATE TO PB-INV-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-020.

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
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-020.
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALEAMT"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD9MIL
                                 PB-SALE-AMOUNT.
            PERFORM WRITE-FIELD-9MIL.
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
               GO TO FILL-025.

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF NEW-NO = "Y"
                GO TO DDR-999.
       DDR-010.
            DELETE PULL-BY
               INVALID KEY NEXT SENTENCE.
            IF WS-PB-ST1 NOT = 0
               MOVE 0 TO WS-PB-ST1
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE 0 TO PB-INVOICE
                       PB-INV-DATE
                       PB-LINE-CNT
                       PB-SALE-AMOUNT.
             MOVE " " TO PB-INITIAL.
             
             MOVE WS-INITIAL  TO PB-INITIAL.
             MOVE WS-INVSAVE TO PB-INVOICE.
            IF WS-PB-ST1 = 51
               UNLOCK PULL-BY.
       CLSC-999.
             EXIT.      
      *
       REWRITE-RECORD SECTION.
       RDR-010.
            IF NEW-NO = "Y"
               GO TO RDR-020.
            REWRITE PULLBY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-PB-ST1 NOT = 0
                MOVE 0 TO WS-PB-ST1
                MOVE "PULLBY BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-010.
            GO TO RDR-999.
       RDR-020.
            WRITE PULLBY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-PB-ST1 NOT = 0
                MOVE 0 TO WS-PB-ST1
                MOVE "PULLBY BUSY ON REWRITE,  'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-020.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE 0 TO WS-PB-ST1.
           MOVE PB-INITIAL TO WS-INITIAL.
           MOVE PB-INVOICE TO WS-INVSAVE.
           START PULL-BY KEY NOT < PB-KEY.
        RD-010.
           READ PULL-BY WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-PB-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-PB-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-NO
                MOVE WS-INITIAL TO PB-INITIAL
                MOVE WS-INVSAVE TO PB-INVOICE
                GO TO RD-999.
           IF WS-PB-ST1 NOT = 0
                MOVE 0 TO WS-PB-ST1
                MOVE "PULLBY RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY"
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.
           MOVE "N" TO NEW-NO.
           MOVE PB-INITIAL TO WS-SAVE.
           MOVE PB-INVOICE TO WS-INVSAVE.
       RD-999.
             EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE WS-INITIAL TO PB-INITIAL.
           MOVE WS-INVSAVE TO PB-INVOICE.
           START PULL-BY KEY NOT < PB-KEY.
       STR-999.
             EXIT.
      *
       READ-NEXT SECTION.
       RNX-001.
           MOVE 0 TO WS-PB-ST1.
       RNX-005.
           READ PULL-BY NEXT WITH LOCK
             AT END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "  " TO PB-INITIAL
                            WS-INITIAL
               MOVE 0 TO PB-INVOICE
                         WS-INVSAVE
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-PB-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-PB-ST1
               MOVE "PULLBY BUSY ON READ-NEXT-LOCK,  'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNX-005.
           IF WS-PB-ST1 NOT = 0
               MOVE 0 TO WS-PB-ST1
               PERFORM START-RECORD
               GO TO RNX-005.
           MOVE PB-INITIAL TO WS-INITIAL
                           WS-SAVE.
           MOVE PB-INVOICE TO WS-INVSAVE.
           MOVE "N" TO NEW-NO.
       RNX-999.
           EXIT.
      *
       READ-PREVIOUS SECTION.
       RPREV-001.
           MOVE 0 TO WS-PB-ST1.
       RPREV-005.
           READ PULL-BY PREVIOUS WITH LOCK
             AT END
              MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "  " TO PB-INITIAL
                            WS-INITIAL
               MOVE 0    TO PB-INVOICE
                            WS-INVSAVE
                MOVE "Y" TO WS-END
               GO TO RPREV-999.
           IF WS-PB-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-PB-ST1
               MOVE "PULLBY BUSY ON READ-PREV-LOCK,  'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RPREV-005.
           IF WS-PB-ST1 NOT = 0
               MOVE 0 TO WS-PB-ST1
               PERFORM START-RECORD
               GO TO RPREV-005.
           MOVE PB-INITIAL TO WS-INITIAL
                              WS-SAVE.
           MOVE PB-INVOICE TO WS-INVSAVE.
           MOVE "N" TO NEW-NO.
       RPREV-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O PULL-BY.
            IF WS-PB-ST1 NOT = 0
               MOVE 0 TO WS-PB-ST1
               MOVE "PULLBY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CoPuByMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE PULL-BY.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "WriteFieldLine".
       Copy "WriteFieldAmount".
       Copy "WriteField9Mil".
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldDate".
       Copy "WriteFieldIndex1".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldPrice".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
      *
      * END-OF-JOB
