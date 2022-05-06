        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlMastIq.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlParameter".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdGlMast.
           COPY ChlfdGlParam.

       WORKING-STORAGE SECTION.
       77  WS-TYPE            PIC X VALUE " ".
       77  WS-BUDGET          PIC S9(8)V99 VALUE 0.
       77  WS-YEAR-BU         PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN          PIC S9(8)V99 VALUE 0.
       77  WS-TOTALMV         PIC S9(8)V99 VALUE 0.
       77  WS-TOTALBU         PIC S9(8)V99 VALUE 0.
       77  WS-TOTALVA         PIC S9(8)V99 VALUE 0.
       77  WS-TOTALLY         PIC S9(8)V99 VALUE 0.
       77  WS-INQUIRY         PIC X(8) VALUE "GlNameIq".
       77  NEW-GLNO           PIC X VALUE " ".
       77  WS-END             PIC X VALUE " ".
       77  WS-NUMBER          PIC X(12) VALUE " ".
       77  SUB-1-SAVE         PIC S9(5) VALUE 0.
       01  WS-GLACC.
           03  WS-SUBHEADER.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-REST             PIC X(6).
       01  WS-MONTH-BUDGET.
           03  WS-MONTH-BU         PIC S9(8)V99 OCCURS 13.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1       PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
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
            MOVE "N" TO NEW-GLNO
                        WS-END.
            MOVE 0 TO WS-TOTALMV WS-TOTALBU WS-TOTALVA.

            MOVE "                        " TO F-NAMEFIELD ALPHA-RATE.
       GET-001.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "ACCNUM" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM DISPLAY-FORM
               GO TO GET-000.
            IF ALPHA-RATE > SPACES
               PERFORM NUMBER-CHECK
               MOVE WS-GLNO-CHECK TO GL-NUMBER.
            IF SIGN-FOUND = 9
               MOVE 0 TO SIGN-FOUND
               GO TO GET-001.
            IF F-EXIT-CH = X"0C"
                PERFORM READ-GL-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               PERFORM DISPLAY-FORM
               GO TO GET-000.
            IF F-EXIT-CH = X"05"
               PERFORM READ-GL-PREVIOUS
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               GO TO GET-000.
            IF F-NAMEFIELD = " "
               CLOSE GL-MASTER
               CALL WS-INQUIRY USING WS-LINKAGE
               PERFORM OPEN-000
               PERFORM CLEAR-SCREEN
               PERFORM DISPLAY-FORM
               GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B" AND NOT = X"1B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-GL.
            IF NEW-GLNO = "Y"
               PERFORM CLEAR-FORM
               MOVE "Invalid account, re-enter OR 'PgDn' FOR NEXT ACC."
               TO GL-DESCRIPTION.
        GET-003.
            MOVE "ACCNUM"  TO F-FIELDNAME.
            MOVE 6         TO F-CBFIELDNAME.
            MOVE GL-NUMBER TO F-NAMEFIELD.
            MOVE 12        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "DESCRIPTION" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE GL-DESCRIPTION TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PB" TO F-FIELDNAME.
            MOVE 2 TO F-CBFIELDNAME.
            MOVE GL-P-B TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CRDATE"     TO F-FIELDNAME.
            MOVE 6            TO F-CBFIELDNAME.
            MOVE GL-DATE      TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BALANCE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE GL-BALANCE TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "OPENPERBAL" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE GL-OPEN-PER-BAL TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "OPENYEARBAL" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE GL-OPEN-YEAR-BAL TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "LASTYEARBAL" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE GL-LAST-YEAR-BAL TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE 1 TO SUB-1 F-INDEX.
       GET-006.
            MOVE "PER" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE GL-PER (SUB-1) TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-006.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-020.
            MOVE "PERBU" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE GL-PER-BU (SUB-1) TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-020.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-025.
            IF SUB-1 = 1
               MOVE 0 TO WS-TOTALVA.

            MOVE "VAR" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            IF SUB-1 NOT > GLPA-CURRENT-GLPER
               COMPUTE WS-MARGIN = GL-PER (SUB-1) - GL-PER-BU (SUB-1)
               MOVE WS-MARGIN TO F-EDNAMEFIELDREC
               ADD WS-MARGIN TO WS-TOTALVA
            ELSE
               MOVE 0 TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-025.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-030.
            MOVE "LASTPER" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE GL-LAST-PER (SUB-1) TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-030.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-050.
            PERFORM ADD-TOTALS.

            MOVE "TOTALMV" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-TOTALMV TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TOTALBU" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-TOTALBU TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TOTALVA" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-TOTALVA TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TOTALLY" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-TOTALLY TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            GO TO GET-001.
       GET-999.
            EXIT.
      *
       ADD-TOTALS SECTION.
       ATS-001.
            MOVE 0 TO WS-TOTALMV WS-TOTALBU WS-TOTALLY.
            MOVE 1 TO SUB-1.
       ATS-005.
            COMPUTE WS-TOTALMV = WS-TOTALMV + GL-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                GO TO ATS-005.
            MOVE 1 TO SUB-1.
       ATS-010.
            COMPUTE WS-TOTALBU = WS-TOTALBU + GL-PER-BU (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                GO TO ATS-010.
            MOVE 1 TO SUB-1.
       ATS-030.
            COMPUTE WS-TOTALLY = WS-TOTALLY + GL-LAST-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                GO TO ATS-030.
            MOVE 1 TO SUB-1.
       ATS-999.
            EXIT.
      *
       CALCULATE-YEAR-BU SECTION.
       CYB-001.
            MOVE 0 TO SUB-1.
       CYB-005.
            COMPUTE WS-BUDGET ROUNDED = WS-YEAR-BU / 12.
       CYB-010.
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
               MOVE WS-BUDGET TO WS-MONTH-BU (SUB-1)
               GO TO CYB-010.
            COMPUTE WS-MARGIN = WS-YEAR-BU - (WS-BUDGET * 12).
            MOVE 12 TO SUB-1.
            ADD WS-MARGIN TO WS-MONTH-BU (SUB-1).
            MOVE 1 TO SUB-1.
       CYB-020.
            MOVE WS-MONTH-BU (SUB-1) TO GL-PER-BU (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
               GO TO CYB-020.

            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM GET-020 THRU GET-025.
            PERFORM GET-050.
       CYB-999.
            EXIT.
      *
       READ-GL SECTION.
       R-GL-000.
             MOVE GL-NUMBER TO WS-NUMBER.
             START GL-MASTER KEY NOT < GL-KEY.
       R-GL-010.
             READ GL-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-GLMAST-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-GLNO
                MOVE WS-NUMBER TO GL-NUMBER
                GO TO R-GL-999.
             IF WS-GLMAST-ST1 NOT = 0
                MOVE "GL-MASTER BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-GLMAST-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-GLMAST-ST1
                GO TO R-GL-010.
             MOVE "N" TO NEW-GLNO.
       R-GL-999.
             EXIT.
      *
       START-GL SECTION.
       GL-GL-000.
              MOVE WS-NUMBER TO GL-NUMBER.
              START GL-MASTER KEY NOT LESS GL-NUMBER.
       GL-GL-999.
             EXIT.
      *
       READ-GL-NEXT SECTION.
       RSN-005. 
           READ GL-MASTER NEXT
             AT END 
               MOVE " " TO GL-NUMBER
                           WS-NUMBER
               MOVE "Y" TO WS-END
               PERFORM CLEAR-FORM
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GL-MASTER BUSY23 ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO RSN-005.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GL-MASTER BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
      *         PERFORM START-GL
               GO TO RSN-005.
           MOVE GL-NUMBER TO WS-NUMBER.
           MOVE "N" TO NEW-GLNO.
       RSN-999.
             EXIT.
      *
       READ-GL-PREVIOUS SECTION.
       RDPR-005. 
           READ GL-MASTER PREVIOUS
             AT END 
               MOVE " " TO GL-NUMBER
                           WS-NUMBER
               MOVE "Y" TO WS-END
               PERFORM CLEAR-FORM
            MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDPR-999.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
           MOVE "GL-MASTER BUSY23, ON READ-PREV-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO RDPR-005.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               PERFORM START-GL
               GO TO RDPR-005.
           MOVE GL-NUMBER TO WS-NUMBER.
           MOVE "N" TO NEW-GLNO.
       RDPR-999.
             EXIT.
      *
       READ-PARAMETER-RECORD SECTION.
       RPR-000.
           MOVE 1 TO GLPA-KEY.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RPR-010.
           READ GLPARAMETER-FILE
                 INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
              MOVE "PARAMETER RECORD NOT THERE, 'ESC' TO EXIT"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RPR-999.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER RECORD BUSY, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RPR-010.
       RPR-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO GL-NUMBER
                         GL-DESCRIPTION
                         GL-P-B.
             MOVE 1 TO SUB-1.
       CLSC-005.
             MOVE 0   TO GL-PER (SUB-1)
                         GL-PER-BU (SUB-1)
                         GL-LAST-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                 GO TO CLSC-005.
             MOVE 1 TO SUB-1.
       CLSC-010.
           MOVE 0 TO GL-DATE
                     GL-BALANCE
                     GL-OPEN-PER-BAL
                     GL-OPEN-YEAR-BAL
                     GL-LAST-YEAR-BAL.
       CLSC-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO OPEN-000.
        OPEN-001.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-001.
            PERFORM READ-PARAMETER-RECORD.
            IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               CLOSE GLPARAMETER-FILE
               PERFORM END-OFF.
            CLOSE GLPARAMETER-FILE.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *      ACCEPT WS-DATE FROM DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "GlMastIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE GL-MASTER.
           EXIT PROGRAM.
      *      STOP RUN.
       END-999.
          EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldDate".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldRec".
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "NumberCheck".
       Copy "GetSystemY2KDate".
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
