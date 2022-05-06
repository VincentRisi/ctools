        IDENTIFICATION DIVISION.
        PROGRAM-ID. CoStffMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT STAFF-FILE ASSIGN TO Ws-CoStaffInOut
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-STAFF-STATUS
               RECORD KEY IS STAFF-KEY.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCoStaff.

       WORKING-STORAGE SECTION.
       77  NEW-NO             PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-NUMBER          PIC X(25) VALUE " ".
       77  WS-NAME            PIC X(25) VALUE " ".
       01  WS-TIME-DISPLAY.
           03  SPLIT-HR           PIC 99.
           03  SPLIT-HR-FIL       PIC X.
           03  SPLIT-MN           PIC 99.
       01  WS-STAFF-STATUS.
           03  WS-STAFF-ST1   PIC 99.
      *     03  WS-STAFF-ST2   PIC X.
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
            MOVE " " TO STAFF-RECORD.
            MOVE "N" TO NEW-NO
                        WS-END.
       GET-001.
            MOVE "NAME"      TO F-FIELDNAME.
            MOVE 4           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STAFF-NAME.
            
            IF STAFF-NAME = "** RESET INPUTS **"
                 PERFORM RESET-INPUTS
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-NAME   TO STAFF-NAME
                 PERFORM START-RECORD
                 PERFORM READ-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-003 THRU GET-005
                 GO TO GET-001.
            IF STAFF-NAME NOT > "  "
               MOVE "ENTER A VALID FULL NAME, <CANCEL> TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
            PERFORM READ-RECORD.
            IF NEW-NO = "Y"
               GO TO GET-003.
            GO TO GET-005.
       GET-003.
            MOVE "NAME"        TO F-FIELDNAME.
            MOVE 4             TO F-CBFIELDNAME.
            MOVE STAFF-NAME    TO F-NAMEFIELD.
            MOVE 25            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-005.
            MOVE "DATE"          TO F-FIELDNAME.
            MOVE 4               TO F-CBFIELDNAME.
            MOVE STAFF-LAST-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TIME"          TO F-FIELDNAME.
            MOVE 4               TO F-CBFIELDNAME.
            MOVE STAFF-LAST-TIME TO F-NAMEFIELD
            MOVE 5               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STATUS"     TO F-FIELDNAME.
            MOVE 6            TO F-CBFIELDNAME.
            IF STAFF-STATUS = "D"
                MOVE "SMOKING" TO F-NAMEFIELD.
            IF STAFF-STATUS = "I"
                MOVE "IN" TO F-NAMEFIELD.
            IF STAFF-STATUS = "O"
                MOVE "OUT" TO F-NAMEFIELD.
            IF STAFF-STATUS = "L"
                MOVE "ON LEAVE" TO F-NAMEFIELD.
            IF STAFF-STATUS = "I"
                MOVE "NOT YET IN" TO F-NAMEFIELD.
            IF STAFF-STATUS = "S"
                MOVE "SICK LEAVE" TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DOB"        TO F-FIELDNAME.
            MOVE 3            TO F-CBFIELDNAME.
            MOVE STAFF-DOB    TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                  GO TO FILL-999.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE"  TO F-FIELDNAME.
            MOVE 4       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-020.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE      TO STAFF-LAST-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-020.

            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
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
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-020.
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TIME" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STAFF-LAST-TIME.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
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
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STATUS" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STAFF-STATUS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF STAFF-STATUS NOT = "I" AND NOT = "O" AND NOT = "L"
                        AND NOT = "S" AND NOT = "N" AND NOT = "D"
               MOVE
          "THIS FIELD MUST BE D, I, O, L, N OR S, <CANCEL> TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
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
               GO TO FILL-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-030.
       FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DOB"  TO F-FIELDNAME.
            MOVE 3      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10     TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-040.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE      TO STAFF-DOB.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-040.

            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
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
               GO TO FILL-040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO FILL-040.

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE 0 TO STAFF-NAME
                       STAFF-LAST-DATE
                       STAFF-LAST-TIME
                       STAFF-STATUS
                       STAFF-DOB.
             MOVE " " TO STAFF-KEY
                         STAFF-STATUS.
             MOVE WS-NUMBER    TO STAFF-KEY.
             UNLOCK STAFF-FILE.
       CLSC-999.
             EXIT.      
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF NEW-NO = "Y"
                GO TO DDR-999.
       DDR-010.
            DELETE STAFF-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-STAFF-ST1 NOT = 0
               MOVE 0 TO WS-STAFF-ST1
               MOVE "STAFF FILE BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       REWRITE-RECORD SECTION.
       RDR-005.
            IF NEW-NO = "Y"
               GO TO RDR-020.
       RDR-010.
            REWRITE STAFF-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-STAFF-ST1 NOT = 0
                MOVE 0 TO WS-STAFF-ST1
                MOVE "STAFF FILE BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-020.
            GO TO RDR-999.
       RDR-020.
            WRITE STAFF-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-STAFF-ST1 NOT = 0
                MOVE 0 TO WS-STAFF-ST1
                MOVE "STAFF FILE BUSY ON REWRITE,  'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-010.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE 0 TO WS-STAFF-ST1.
           MOVE STAFF-NAME   TO WS-NAME
           START STAFF-FILE KEY NOT < STAFF-KEY
              INVALID KEY NEXT SENTENCE.
        RD-010.
           READ STAFF-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-STAFF-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-STAFF-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-NO
                MOVE WS-NAME      TO STAFF-NAME
                MOVE WS-DATE      TO STAFF-LAST-DATE
                ACCEPT WS-TIME FROM TIME
                MOVE WS-HR           TO SPLIT-HR
                MOVE ":"             TO SPLIT-HR-FIL
                MOVE WS-MIN          TO SPLIT-MN
                MOVE WS-TIME-DISPLAY TO STAFF-LAST-TIME
                MOVE "I"             TO STAFF-STATUS
                GO TO RD-999.
           IF WS-STAFF-ST1 NOT = 0
                MOVE 0 TO WS-STAFF-ST1
                MOVE "STAFF FILE Busy ON READ,  'ESC' To Retry."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.
           MOVE "N" TO NEW-NO.
           MOVE STAFF-NAME   TO WS-NAME.
       RD-999.
             EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE WS-NAME   TO STAFF-NAME.
           START STAFF-FILE KEY NOT < STAFF-KEY
              INVALID KEY
              MOVE SPACES TO STAFF-NAME
                                WS-NAME
           START STAFF-FILE KEY NOT < STAFF-KEY.
       STR-999.
             EXIT.
      *
       READ-NEXT SECTION.
       RNX-001.
           MOVE 0 TO WS-STAFF-ST1.
       RNX-005.
           READ STAFF-FILE NEXT WITH LOCK
             AT END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "  " TO STAFF-NAME
                            WS-NAME
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-STAFF-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-STAFF-ST1
               MOVE "STAFF FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNX-005.
           IF WS-STAFF-ST1 NOT = 0
               MOVE 0 TO WS-STAFF-ST1
               PERFORM START-RECORD
               GO TO RNX-005.
           MOVE STAFF-NAME   TO WS-NAME.
           MOVE "N" TO NEW-NO.
       RNX-999.
           EXIT.
      *
       RESET-INPUTS SECTION.
       RESET-005.
           READ STAFF-FILE NEXT WITH LOCK
             AT END
               GO TO RESET-999.
           IF WS-STAFF-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-STAFF-ST1
               MOVE "STAFF BUSY ON RESET-INPUTS, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RESET-005.
           IF WS-STAFF-ST1 NOT = 0
               MOVE 0 TO WS-STAFF-ST1
               PERFORM START-RECORD
               GO TO RESET-005.
               
           IF STAFF-STATUS NOT = "L" AND NOT = "S"
                MOVE "N"             TO STAFF-STATUS
                MOVE "N"             TO NEW-NO
                ACCEPT WS-TIME FROM TIME
                MOVE WS-HR           TO SPLIT-HR
                MOVE ":"             TO SPLIT-HR-FIL
                MOVE WS-MIN          TO SPLIT-MN
                MOVE WS-TIME-DISPLAY TO STAFF-LAST-TIME
                PERFORM REWRITE-RECORD.
           
           GO TO RESET-005.
       RESET-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STAFF-FILE.
            IF WS-STAFF-ST1 NOT = 0
               MOVE 0 TO WS-STAFF-ST1
               MOVE "STAFF FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT.

           PERFORM GET-USER-MAIL-NAME.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CoStffMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STAFF-FILE.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldDate".
       Copy "GetSystemY2KDate".
       Copy "GetUserMailName".
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
