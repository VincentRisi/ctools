        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrCurrMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectCrCurrency".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrCurr.
      *
       WORKING-STORAGE SECTION.
       77  NEW-NO             PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-NUMBER          PIC X(5) VALUE " ".
       77  WS-CURRENCY-TEMP   PIC S9(3)V99999 VALUE 0.
       77  WS-EXCHANGE-DIS    PIC Z(2)9.99999.
       77  WS-SAVE            PIC X(5) VALUE " ".
       01  WS-CURRENCY-STATUS.
           03  WS-CURRENCY-ST1   PIC 99.
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
            MOVE "N" TO NEW-NO
                        WS-END.
       GET-001.
            PERFORM ERROR1-020.
            MOVE "                " TO F-NAMEFIELD.
            MOVE "CURRENCY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CU-CURRENCY-TYPE.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-SAVE TO CU-KEY
                 PERFORM START-RECORD
                 PERFORM READ-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-003 THRU GET-005
                 GO TO GET-999.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-PREVIOUS
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
            IF CU-CURRENCY-TYPE NOT > " "
                 GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
            PERFORM READ-RECORD.
            IF NEW-NO = "Y"
               GO TO GET-999.
            GO TO GET-005.
        GET-003.
            MOVE "CURRENCY"       TO F-FIELDNAME.
            MOVE 8                TO F-CBFIELDNAME.
            MOVE CU-CURRENCY-TYPE TO F-NAMEFIELD.
            MOVE 5                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "DESC"         TO F-FIELDNAME.
            MOVE 4              TO F-CBFIELDNAME.
            MOVE CU-DESCRIPTION TO F-NAMEFIELD.
            MOVE 20             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "VALUE"  TO F-FIELDNAME.
            MOVE 5        TO F-CBFIELDNAME.
            MOVE CU-VALUE TO F-EDNAMEFIELDVALUE.
            MOVE 9        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-VALUE.
            
            MOVE 2910 TO POS
            DISPLAY "1      = R" AT POS
            COMPUTE WS-CURRENCY-TEMP = 1 / CU-VALUE
            ADD 2 TO POS 
            DISPLAY CU-CURRENCY-TYPE AT POS
            ADD 8 TO POS
            MOVE WS-CURRENCY-TEMP TO WS-EXCHANGE-DIS
            DISPLAY WS-EXCHANGE-DIS AT POS.
            MOVE 3010 TO POS
            DISPLAY
            "ENTER MULTIPLE RAND TO A CURRENCY, PRESS <F8> TO CONVERT."
               AT POS.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DESC" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CU-DESCRIPTION.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CU-DESCRIPTION = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A CHARACTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO FILL-001.
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "VALUE"     TO F-FIELDNAME.
            MOVE 5           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            
            IF F-EXIT-CH = X"1D"
               COMPUTE NUMERIC-RATE = 1 / NUMERIC-RATE.
            
            MOVE NUMERIC-RATE TO CU-VALUE
                                 F-EDNAMEFIELDVALUE.
            PERFORM WRITE-FIELD-VALUE.
            
            MOVE 2910 TO POS
            DISPLAY "1      = R" AT POS
            COMPUTE WS-CURRENCY-TEMP ROUNDED = 1 / CU-VALUE
            ADD 2 TO POS 
            DISPLAY CU-CURRENCY-TYPE AT POS
            ADD 8 TO POS
            MOVE WS-CURRENCY-TEMP TO WS-EXCHANGE-DIS
            DISPLAY WS-EXCHANGE-DIS AT POS.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF SIGN-FOUND = 1
               MOVE "THIS FIELD MAY NOT BE NEGATIVE, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF CU-VALUE = 0
               MOVE "THIS FIELD MAY NOT BE = 0, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO FILL-010.

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " "       TO CU-DESCRIPTION.
             MOVE WS-NUMBER TO CU-CURRENCY-TYPE.
       CLSC-999.
             EXIT.
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF NEW-NO = "Y"
                GO TO DDR-999.
       DDR-010.
            DELETE CURRENCY-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-CURRENCY-ST1 NOT = 0
                MOVE "CURRENCY BUSY ON DELETE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CURRENCY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CURRENCY-ST1
                GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       REWRITE-RECORD SECTION.
       RDR-010.
            IF NEW-NO = "Y"
               GO TO RDR-020.
            REWRITE CURRENCY-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-CURRENCY-ST1 NOT = 0
                MOVE "CURRENCY BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CURRENCY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CURRENCY-ST1
                GO TO RDR-010.
            GO TO RDR-999.
       RDR-020.
            WRITE CURRENCY-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-CURRENCY-ST1 NOT = 0
                MOVE "CURRENCY BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CURRENCY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CURRENCY-ST1
                GO TO RDR-020.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE CU-KEY TO WS-NUMBER.
           START CURRENCY-MASTER KEY NOT < CU-KEY.
        RD-010.
           READ CURRENCY-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-CURRENCY-ST1 = 23 OR 35 OR = 49
                MOVE 0 TO WS-CURRENCY-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-NO
                MOVE WS-NUMBER TO CU-KEY
                GO TO RD-999.
           IF WS-CURRENCY-ST1 NOT = 0
                MOVE "CURRENCY BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CURRENCY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CURRENCY-ST1
                GO TO RD-010.
           MOVE "N" TO NEW-NO.
           MOVE CU-KEY TO WS-SAVE.
       RD-999.
             EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE WS-NUMBER TO CU-KEY.
           START CURRENCY-MASTER KEY NOT < CU-KEY.
       STR-999.
             EXIT.
      *
       READ-NEXT SECTION.
       RNX-001.
           MOVE 0 TO WS-CURRENCY-ST1.
       RNX-005.
           READ CURRENCY-MASTER NEXT WITH LOCK
             AT END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO CU-KEY
                           WS-NUMBER
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-CURRENCY-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-CURRENCY-ST1
               MOVE "CURR FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNX-005.
           IF WS-CURRENCY-ST1 NOT = 0
               MOVE 0 TO WS-CURRENCY-ST1
               PERFORM START-RECORD
               GO TO RNX-005.
           IF CU-KEY = " "
               GO TO RNX-005.
           MOVE CU-KEY TO WS-NUMBER
                          WS-SAVE.
           MOVE "N" TO NEW-NO.
       RNX-999.
           EXIT.
      *
       READ-PREVIOUS SECTION.
       RDPR-001.
           MOVE 0 TO WS-CURRENCY-ST1.
       RDPR-005.
           READ CURRENCY-MASTER PREVIOUS WITH LOCK
             AT END
            MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO CU-KEY
                           WS-NUMBER
               MOVE "Y" TO WS-END
               GO TO RDPR-999.
           IF WS-CURRENCY-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-CURRENCY-ST1
               MOVE "CURR FILE BUSY ON READ-PREVIOUS, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDPR-005.
           IF WS-CURRENCY-ST1 NOT = 0
               MOVE 0 TO WS-CURRENCY-ST1
               PERFORM START-RECORD
               GO TO RDPR-005.
           IF CU-KEY = " "
               GO TO RDPR-005.
           MOVE CU-KEY TO WS-NUMBER
                          WS-SAVE.
           MOVE "N" TO NEW-NO.
       RDPR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CURRENCY-MASTER.
            IF WS-CURRENCY-ST1 = 49
               MOVE 0 TO WS-CURRENCY-ST1
               MOVE "CURRENCY FILE BUSY 49 ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-001.

      *       MOVE "CURRENCY FILE OPENED I-O" TO WS-MESSAGE
      *       PERFORM ERROR-MESSAGE.
            GO TO OPEN-010.
       OPEN-001.
            OPEN OUTPUT CURRENCY-MASTER.
            IF WS-CURRENCY-ST1 NOT = 0
               MOVE 0 TO WS-CURRENCY-ST1
               MOVE "CURRENCY FILE BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-001.

      *      MOVE "CURRENCY FILE OPENED OUTPUT" TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrCurrMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CURRENCY-MASTER.
      *      STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldValue".
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
      *
      * END-OF-JOB
