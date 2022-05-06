        IDENTIFICATION DIVISION.
        PROGRAM-ID. CoRpParm.
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
               FILE STATUS IS WS-DELIVER-STATUS
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
       01  WS-DELIVER-STATUS.
           03  WS-DR-ST1   PIC 99.
      *     03  WS-DR-ST2   PIC X.
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
            MOVE 8 TO PA-TYPE.
        GET-001.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "QUTCODE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
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
            MOVE "QUTCODE" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE PA-RECORD TO F-EDNAMEFIELDFAX
            MOVE 1         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FAX.
        GET-005.
            MOVE "QUTDESC"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE QUOTE-DESC TO F-NAMEFIELD60
            MOVE 60         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-60ALPHA.
       GET-999.
            EXIT.
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF NEW-NO = "Y"
                GO TO DDR-999.
       DDR-010.
            DELETE PARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-DR-ST1 NOT = 0
               MOVE 0 TO WS-DR-ST1
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QUTDESC"     TO F-FIELDNAME.
            MOVE 7             TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 60            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-60ALPHA.
            MOVE F-NAMEFIELD60 TO QUOTE-DESC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF QUOTE-DESC = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A CHARACTER"
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

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO QUOTE-DESC.
             MOVE WS-NUMBER TO PA-RECORD.
       CLSC-999.
             EXIT.      
      *
       REWRITE-RECORD SECTION.
       RDR-005.
            IF NEW-NO = "Y"
               GO TO RDR-020.
       RDR-010.
            REWRITE PARAMETER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-DR-ST1 NOT = 0
                MOVE 0 TO WS-DR-ST1
                MOVE "PARAMETER BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-020.
            GO TO RDR-999.
       RDR-020.
            WRITE PARAMETER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-DR-ST1 NOT = 0
                MOVE 0 TO WS-DR-ST1
                MOVE "PARAMETER BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-010.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE " " TO WS-DR-ST1.
           MOVE 8   TO PA-TYPE.
           MOVE PA-RECORD TO WS-NUMBER.
           START PARAMETER-FILE KEY NOT < PA-KEY.
        RD-010.
           READ PARAMETER-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-DR-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-DR-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-NO
                MOVE WS-NUMBER TO PA-RECORD
                GO TO RD-999.
           IF WS-DR-ST1 NOT = 0
                MOVE 0 TO WS-DR-ST1
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
           MOVE 8         TO PA-TYPE.
           MOVE WS-NUMBER TO PA-RECORD.
           START PARAMETER-FILE KEY NOT < PA-KEY.
       STR-999.
             EXIT.
      *
       READ-NEXT SECTION.
       RNX-001.
           MOVE 0 TO WS-DR-ST1.
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
           IF PA-TYPE NOT = 8
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO PARAMETER-REC
               MOVE 0   TO PA-RECORD
                           WS-NUMBER
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-DR-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-DR-ST1
               MOVE "PARAMETER BUSY ON READ-NEXT, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNX-005.
           IF WS-DR-ST1 NOT = 0
               MOVE 0 TO WS-DR-ST1
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
            IF WS-DR-ST1 NOT = 0
               MOVE 0 TO WS-DR-ST1
               MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CoRpParm"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE PARAMETER-FILE.
       END-900.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadField60Alpha".
       Copy "WriteFieldAlpha".
       Copy "WriteField60Alpha".
       Copy "WriteFieldFax".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ClearScreen".
       Copy "ErrorMessage".
      *
      * END-OF-JOB
