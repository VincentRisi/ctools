        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlSRepMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectSlSbRep".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdSbRep.

       WORKING-STORAGE SECTION.
       77  NEW-NO             PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-NUMBER          PIC X VALUE " ".
       77  WS-SAVE            PIC X VALUE " ".
       01  WS-SBREP-STATUS.
           03  WS-SBREP-ST1   PIC 99.
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
            MOVE " " TO SBREP-REC.
            MOVE "N" TO NEW-NO
                        WS-END.
       GET-001.
            MOVE "SALESMAN" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO SBREP-REP.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-SAVE TO SBREP-REP
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
      *      IF SBREP-REP = 0
      *           MOVE "REP NUMBER MUST BE > 0" TO WS-MESSAGE
      *           PERFORM ERROR-MESSAGE
      *           GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-001.
            PERFORM READ-RECORD.
            IF NEW-NO = "Y"
               GO TO GET-999.
            GO TO GET-005.
       GET-003.
            MOVE "SALESMAN" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE SBREP-REP TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-005.
            MOVE "REPNAME"     TO F-FIELDNAME.
            MOVE 7             TO F-CBFIELDNAME.
            MOVE SBREP-REPNAME TO F-NAMEFIELD.
            MOVE 15            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESBU"          TO F-FIELDNAME.
            MOVE 7                  TO F-CBFIELDNAME.
            MOVE SBREP-SALES-BUDGET TO F-EDNAMEFIELDFORTOTAL.
            MOVE 11                 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-FORTOTAL.

            MOVE "PROFITBU"          TO F-FIELDNAME.
            MOVE 8                   TO F-CBFIELDNAME.
            MOVE SBREP-PROFIT-BUDGET TO F-EDNAMEFIELDFORTOTAL.
            MOVE 11                  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-FORTOTAL.

            MOVE "INTERNAL" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE SBREP-INT  TO F-NAMEFIELD.
            MOVE 2          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INTERNALNAME" TO F-FIELDNAME.
            MOVE 12             TO F-CBFIELDNAME.
            MOVE SBREP-INTNAME  TO F-NAMEFIELD.
            MOVE 15             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "REPNAME" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO SBREP-REPNAME.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF SBREP-REPNAME = "     "
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A NAME"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
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
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-001.
       FILL-005.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESBU" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO SBREP-SALES-BUDGET
                                 F-EDNAMEFIELDFORTOTAL.
            PERFORM WRITE-FIELD-FORTOTAL.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF SBREP-SALES-BUDGET NOT > 0
               MOVE "THIS FIELD MUST BE > ZERO."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
            IF F-EXIT-CH = X"01"
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
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-005.
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PROFITBU" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO SBREP-PROFIT-BUDGET
                                 F-EDNAMEFIELDFORTOTAL.
            PERFORM WRITE-FIELD-FORTOTAL.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF SBREP-PROFIT-BUDGET NOT > 0
               MOVE "THIS FIELD MUST BE > ZERO."
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
            IF F-EXIT-CH = X"01"
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
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-010.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INTERNAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO SBREP-INT.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF SBREP-INT = "    "
               MOVE "THIS FIELD MUST NOT BE BLANK"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-020.
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INTERNALNAME" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO SBREP-INTNAME.
            IF SBREP-INTNAME = "    "
               MOVE "THIS FIELD MUST NOT BE BLANK"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-025.
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
            DELETE SBREP-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-SBREP-ST1 NOT = 0
               MOVE "SB-REP FILE BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SBREP-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SBREP-ST1
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO SBREP-REP
                         SBREP-REPNAME
                         SBREP-INT
                         SBREP-INTNAME.
             MOVE 0   TO SBREP-SALES-BUDGET
                         SBREP-PROFIT-BUDGET.
             MOVE WS-NUMBER TO SBREP-REP.
             UNLOCK SBREP-MASTER.
       CLSC-999.
             EXIT.      
      *
       REWRITE-RECORD SECTION.
       RDR-010.
            IF NEW-NO = "Y"
               GO TO RDR-020.
            REWRITE SBREP-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-SBREP-ST1 NOT = 0
                MOVE "SBREP BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-SBREP-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-SBREP-ST1
                GO TO RDR-010.
            GO TO RDR-999.
       RDR-020.
            WRITE SBREP-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-SBREP-ST1 NOT = 0
               MOVE "SOLDBY RECORD BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SBREP-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SBREP-ST1               GO TO RDR-020.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE 0 TO WS-SBREP-ST1.
           MOVE SBREP-REP TO WS-NUMBER.
           START SBREP-MASTER KEY NOT < SBREP-KEY.
        RD-010.
           READ SBREP-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-SBREP-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-SBREP-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-NO
                MOVE WS-NUMBER TO SBREP-REP
                GO TO RD-999.
           IF WS-SBREP-ST1 NOT = 0
                MOVE "SOLDBY RECORD BUSY, PRESS 'ESC' TO RETRY"
                  TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SBREP-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SBREP-ST1
               GO TO RD-010.
           MOVE "N" TO NEW-NO.
           MOVE SBREP-REP TO WS-SAVE.
       RD-999.
             EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE WS-NUMBER TO SBREP-REP.
           START SBREP-MASTER KEY NOT < SBREP-KEY.
       STR-999.
             EXIT.
      *
       READ-NEXT SECTION.
       RNX-001.
           MOVE 0 TO WS-SBREP-ST1.
       RNX-005.
           READ SBREP-MASTER NEXT WITH LOCK
             AT END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO SBREP-REP
                           WS-NUMBER
                           WS-SAVE
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-SBREP-ST1 =  23 OR 35 OR 49
               MOVE "SOLDBY BUSY ON READ-NEXT-23, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SBREP-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SBREP-ST1
               GO TO RNX-999.
           IF WS-SBREP-ST1 NOT = 0
               MOVE "SOLDBY BUSY ON READ-NEXT, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SBREP-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SBREP-ST1
               GO TO RNX-005.
           MOVE SBREP-REP TO WS-NUMBER
                             WS-SAVE.
           MOVE "N" TO NEW-NO.
       RNX-999.
           EXIT.
      *
       READ-PREVIOUS SECTION.
       RPREV-001.
           MOVE 0 TO WS-SBREP-ST1.
       RPREV-005.
           READ SBREP-MASTER PREVIOUS WITH LOCK
             AT END
              MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO SBREP-REP
                           WS-NUMBER
                           WS-SAVE
               MOVE "Y" TO WS-END
               GO TO RPREV-999.
           IF WS-SBREP-ST1 =  23 OR 35 OR 49
               MOVE "SOLDBY BUSY ON READ-PREVIOUS, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SBREP-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SBREP-ST1
               GO TO RPREV-999.
           IF WS-SBREP-ST1 NOT = 0
               MOVE "SOLDBY BUSY ON READ-NEXT, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SBREP-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SBREP-ST1
               GO TO RPREV-005.
           MOVE SBREP-REP TO WS-NUMBER
                             WS-SAVE.
           MOVE "N" TO NEW-NO.
       RPREV-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O SBREP-MASTER.
            IF WS-SBREP-ST1 NOT = 0
               MOVE 0 TO WS-SBREP-ST1
               MOVE "SOLDBY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlSbRep"       TO F-FORMNAME
           MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE SBREP-MASTER.
      *      STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldForTotal".
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
