        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlPuByMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectCoPullers".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdPullers.
           
       WORKING-STORAGE SECTION.
       77  NEW-NO             PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-NUMBER          PIC XX VALUE " ".
       77  WS-SAVE            PIC XX VALUE " ".
       01  WS-PULLERS-STATUS.
           03  WS-PULLERS-ST1 PIC 99.
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
            MOVE " " TO PULLER-REC.
            MOVE "N" TO NEW-NO
                        WS-END.
       GET-001.
            MOVE "PULLRMAN" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PU-INITIAL.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-SAVE TO PU-INITIAL
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
            IF PU-INITIAL = "  "
                 MOVE "ENTER AN INITIAL FOR A PULLER, 'ESC' TO RETRY"
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-001.
            PERFORM READ-RECORD.
            IF NEW-NO = "Y"
               GO TO GET-999.
            GO TO GET-005.
        GET-003.
            MOVE "PULLRMAN" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE PU-INITIAL TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-005.
            MOVE "PULNAME" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE PU-NAME TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PULNAME" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PU-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF PU-NAME = "     "
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A NAME"
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

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF NEW-NO = "Y"
                GO TO DDR-999.
       DDR-010.
            DELETE PULLER-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-PULLERS-ST1 NOT = 0
             MOVE "PULLERS BUSY ON DELETE, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-PULLERS-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             MOVE 0 TO WS-PULLERS-ST1
             GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO PU-INITIAL
                         PU-NAME.
             MOVE WS-NUMBER TO PU-INITIAL.
             UNLOCK PULLER-MASTER.
       CLSC-999.
             EXIT.      
      *
       REWRITE-RECORD SECTION.
       RDR-010.
            IF NEW-NO = "Y"
               GO TO RDR-020.
            REWRITE PULLER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-PULLERS-ST1 NOT = 0
                MOVE "PULL BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-PULLERS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-PULLERS-ST1
                GO TO RDR-010.
            GO TO RDR-999.
       RDR-020.
            WRITE PULLER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-PULLERS-ST1 NOT = 0
                MOVE "PULLBY RECORD BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-PULLERS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-PULLERS-ST1
                GO TO RDR-020.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE 0 TO WS-PULLERS-ST1.
           MOVE PU-INITIAL TO WS-NUMBER.
           START PULLER-MASTER KEY NOT < PU-KEY.
        RD-010.
           READ PULLER-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-PULLERS-ST1 = 23 OR = 35 OR 49
                MOVE 0 TO WS-PULLERS-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-NO
                MOVE WS-NUMBER TO PU-INITIAL
                GO TO RD-999.
           IF WS-PULLERS-ST1 NOT = 0
                MOVE "PULLBY BUSY ON READ-LOCK, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-PULLERS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-PULLERS-ST1
                GO TO RD-010.
           MOVE "N" TO NEW-NO.
           MOVE PU-INITIAL TO WS-SAVE.
       RD-999.
             EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE WS-NUMBER TO PU-INITIAL.
           START PULLER-MASTER KEY NOT < PU-KEY.
       STR-999.
             EXIT.
      *
       READ-NEXT SECTION.
       RNX-001.
           MOVE 0 TO WS-PULLERS-ST1.
       RNX-005.
           READ PULLER-MASTER NEXT WITH LOCK
             AT END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO PU-INITIAL
                           WS-NUMBER
                           WS-SAVE
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-PULLERS-ST1 = 23 OR = 35 OR 49 OR 51
               MOVE "PULLBY BUSY ON READ-NEXT, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-PULLERS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-PULLERS-ST1
               GO TO RNX-005.
           IF WS-PULLERS-ST1 NOT = 0
                MOVE "PULLBY BUSY ON READ-NEXT, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-PULLERS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-PULLERS-ST1
                PERFORM START-RECORD
                GO TO RNX-005.
           MOVE PU-INITIAL TO WS-NUMBER
                             WS-SAVE.
           MOVE "N" TO NEW-NO.
       RNX-999.
           EXIT.
      *
       READ-PREVIOUS SECTION.
       RPREV-001.
           MOVE 0 TO WS-PULLERS-ST1.
       RPREV-005.
           READ PULLER-MASTER PREVIOUS WITH LOCK
             AT END
              MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO PU-INITIAL
                           WS-NUMBER
                           WS-SAVE
               MOVE "Y" TO WS-END
               GO TO RPREV-999.
           IF WS-PULLERS-ST1 = 23 OR = 35 OR 49 OR 51
               MOVE "PULLBY BUSY ON READ-PREVIOUS, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-PULLERS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-PULLERS-ST1
               GO TO RPREV-005.
           IF WS-PULLERS-ST1 NOT = 0
               MOVE "PULLBY BUSY ON READ-PREV, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-PULLERS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-PULLERS-ST1
               PERFORM START-RECORD
               GO TO RPREV-005.
           MOVE PU-INITIAL TO WS-NUMBER
                             WS-SAVE.
           MOVE "N" TO NEW-NO.
       RPREV-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O PULLER-MASTER.
            IF WS-PULLERS-ST1 NOT = 0
               MOVE "PULLBY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-PULLERS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-COPULLBY TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-PULLERS-ST1
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlPuByMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE PULLER-MASTER.
      *      STOP RUN.
           EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
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
