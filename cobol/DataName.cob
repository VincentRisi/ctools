       IDENTIFICATION DIVISION.
       PROGRAM-ID. DataName.
       AUTHOR. CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-FILE ASSIGN TO WS-DATA
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-DATA-STATUS
               RECORD KEY IS DATA-KEY.
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDataName.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).

       WORKING-STORAGE SECTION.
       77  WS-1STPRINT          PIC 9 VALUE 0.
       77  NEW-NO               PIC X VALUE " ".      
       77  WS-END               PIC X VALUE " ".      
       77  WS-ANSWER            PIC XX VALUE " ".
       77  WS-PROGRAM           PIC X(12) VALUE " ".
       77  WS-NUMBER            PIC 999 VALUE 0.
       77  WS-SAVE              PIC 999 VALUE 0.
       77  WS-NAME              PIC X(25) VALUE " ".
       01  WS-DATA-STATUS.
           03  WS-DATA-ST1       PIC 99.
       01  HEAD1-LINE.
           03  FILLER         PIC X(5) VALUE "DATE:".
           03  H1-DATE        PIC X(10) VALUE " ".
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(114) VALUE
              "** DATA-NAME REPORT WITH CODE-NUMBERS. **".
       01  HEAD2-LINE.
           03  FILLER         PIC X(20) VALUE " ".
           03  FILLER         PIC X(41) VALUE ALL "*".
           03  FILLER         PIC X(50) VALUE " ".
       01  HEADER-LINE.
           03  H-NUM         PIC X(8) VALUE "NUMBER".
           03  H-SUB         PIC X(25) VALUE "DATA NAME".
           03  FILLER        PIC X(100) VALUE " ".
       01  DETAIL-LINE.
           03  D-NUMBER      PIC ZZ9 BLANK WHEN ZERO.
           03  FILLER        PIC X(5) VALUE " ".
           03  D-NAME        PIC X(25).
           03  FILLER        PIC X(100).
       Copy "WsDateInfo".
      *
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
          MOVE 0310 TO POS
          DISPLAY "***** DATANAME ENTRY/EDIT PROGRAM *****" AT POS
          MOVE 0410 TO POS
          DISPLAY "***************************************" AT POS.
       CONT-003.
          Copy "PrinterAccept".
       CONTROL-010.
          PERFORM DISPLAY-FORM
          PERFORM GET-DATA
          GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-010.
            MOVE "N" TO NEW-NO
                        WS-END.
       GET-020.
            PERFORM ERROR-020.
            PERFORM ERROR1-020.
            MOVE 2920 TO POS
            DISPLAY "ENTER ALL 'X' TO PRINT ALL DATA-NAME INFO" AT POS.
            MOVE SPACES TO F-NAMEFIELD
                           ALPHA-RATE.
       GET-021.
            MOVE ALL SPACES TO F-NAMEFIELD.
            MOVE "SELECTION" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 3           TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = "XXX"
               PERFORM PRINT-DATA
               GO TO GET-999.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 PERFORM START-RECORD
                 PERFORM READ-NEXT-DATAFILE
              IF WS-END NOT = "Y"
                 GO TO GET-030
              ELSE
                 GO TO GET-999.
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE "SELECTION"  TO F-FIELDNAME
            MOVE NUMERIC-RATE TO DATA-NUMBER WS-NUMBER
            MOVE WS-NUMBER    TO F-EDNAMEFIELDGROUP
            MOVE 3            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-GROUP.
            IF F-NAMEFIELD = " "
                GO TO GET-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"1D"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
            PERFORM READ-DATAFILE.
       GET-030.
            MOVE "SELECTION" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE WS-NUMBER   TO F-EDNAMEFIELDGROUP
            MOVE 3           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-GROUP.

            MOVE ALL SPACES TO F-NAMEFIELD.
            MOVE "NAME"  TO F-FIELDNAME
            MOVE 4       TO F-CBFIELDNAME
            MOVE WS-NAME TO F-NAMEFIELD
            MOVE 25      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM ERROR-020.
            PERFORM ERROR1-020.
       GET-035.
            MOVE 2910 TO POS
            DISPLAY 
            "PRESS 'F10' TO DELETE, 'GO' TO RE-WRITE, 'PgDn' "
            AT POS
            MOVE 3015 TO POS
            DISPLAY "FOR NEXT ITEM,  'ESC' TO CLEAR THE FORM."
            AT POS.

            MOVE ALL SPACES TO F-NAMEFIELD.
            MOVE "NAME"     TO F-FIELDNAME
            MOVE 4          TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-020.
            IF F-EXIT-CH = X"07"
                UNLOCK DATA-FILE
                GO TO GET-999.
            MOVE 25 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DATA-NAME.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               GO TO GET-999.
            IF F-EXIT-CH = X"1B"
                PERFORM REWRITE-DATAFILE
                GO TO GET-999.
            IF F-EXIT-CH = X"0C"
                 PERFORM REWRITE-DATAFILE
                 PERFORM READ-NEXT-DATAFILE
              IF WS-END NOT = "Y"
                 GO TO GET-030
              ELSE
                 GO TO GET-999.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"1D"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-035.
            GO TO GET-035.
       GET-999.
           EXIT.
      *
       PRINT-DATA SECTION.
       PP-010.
           PERFORM ERROR-020.
           PERFORM ERROR1-020.
           MOVE 2720 TO POS
           DISPLAY "Printing of Data Names in Progress....." AT POS
           OPEN OUTPUT PRINT-FILE
           MOVE Ws-Co-Name To Co-Name
           MOVE 1 TO WS-1STPRINT.
           MOVE 0 TO DATA-NUMBER.
           START DATA-FILE KEY NOT < DATA-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-DATA-ST1 NOT = 0
              MOVE "INVALID START, 'ESC' TO EXIT." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PP-999.
       PP-020.
           READ DATA-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-DATA-ST1 = 10
               MOVE " " TO WS-DATA-ST1
               GO TO PP-900.
           IF WS-DATA-ST1 NOT = 0
               MOVE " " TO WS-DATA-ST1
               GO TO PP-020.
           IF WS-1STPRINT = "1"
               WRITE PRINT-REC FROM COMPANY-LINE
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD1-LINE AFTER 2
               WRITE PRINT-REC FROM HEAD2-LINE AFTER 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE 0 TO WS-1STPRINT
               WRITE PRINT-REC FROM HEADER-LINE AFTER 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC.
           MOVE DATA-NUMBER  TO D-NUMBER
           MOVE DATA-NAME    TO D-NAME
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           GO TO PP-020.
       PP-900.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           MOVE 2720 TO POS
           DISPLAY "                                        " AT POS.
       PP-999.
           EXIT.
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF NEW-NO = "Y"
                GO TO DDR-999.
       DDR-010.
            DELETE DATA-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-DATA-ST1 NOT = 0
               MOVE "ERROR ON DELETE, 'ESC' TO EXIT" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DATA-ST1
               GO TO DDR-999.
       DDR-999.
           EXIT.
      *
       START-RECORD SECTION.
       SR-005.
           MOVE WS-SAVE TO WS-NUMBER DATA-NUMBER.
           START DATA-FILE KEY NOT < DATA-KEY
               INVALID KEY NEXT SENTENCE.
       SR-999.
           EXIT.
      *
       READ-DATAFILE SECTION.
       RC-005.
           MOVE WS-NUMBER TO DATA-NUMBER.
           START DATA-FILE KEY NOT < DATA-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DATA-ST1 NOT = 0
               MOVE " " TO WS-NAME
                         DATA-NAME
               GO TO RC-999.
       RC-010.
           READ DATA-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-DATA-ST1 = 23 OR 35 OR 49
               MOVE WS-NUMBER TO WS-SAVE
               MOVE " " TO WS-NAME
                           DATA-NAME
               GO TO RC-999.
           IF WS-DATA-ST1 NOT = 0
               MOVE "DATAFILE BUSY ON READ, GOING TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RC-010.
           MOVE DATA-NUMBER TO WS-NUMBER WS-SAVE
           MOVE DATA-NAME   TO WS-NAME.
       RC-999.
           EXIT.
      *
       READ-NEXT-DATAFILE SECTION.
       RND-010.
           READ DATA-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-DATA-ST1 = 10
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-NUMBER
                         WS-SAVE
               MOVE " " TO WS-NAME
               PERFORM START-RECORD
               MOVE "Y" TO WS-END
               GO TO RND-999.
           IF WS-DATA-ST1 NOT = 0
               MOVE "DATAFILE BUSY ON READ-NEXT, GOING TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               
               MOVE DATA-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-SAVE TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               
               GO TO RND-010.
           MOVE DATA-NUMBER TO WS-NUMBER WS-SAVE
           MOVE DATA-NAME   TO WS-NAME.
       RND-999.
           EXIT.
      *
       REWRITE-DATAFILE SECTION.
       RWC-005.
           IF WS-DATA-ST1 = 23 OR 35 OR 49
               GO TO RWC-020.
       RWC-010.
           REWRITE DATA-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-DATA-ST1 NOT = 0
               MOVE "DATAFILE BUSY ON REWRITE, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RWC-020.
           GO TO RWC-999.
       RWC-020.
           WRITE DATA-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-DATA-ST1 NOT = 0
               MOVE "DATAFILE BUSY ON WRITE, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RWC-010.
       RWC-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-001.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *      ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-005.
           OPEN I-O DATA-FILE.
           IF WS-DATA-ST1 NOT = 0
               MOVE "DATAFILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-DATA-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-DATA TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DATA-ST1
               GO TO OPEN-005.
       OPEN-010.
            Move Ws-Forms-Name   TO F-FileName
            Move Ws-cbForms-name TO F-CbFileName.
            MOVE "DataName"      TO F-FORMNAME
            MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE DATA-FILE.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldGroup".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
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
