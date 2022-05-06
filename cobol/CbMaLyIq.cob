        IDENTIFICATION DIVISION.
        PROGRAM-ID. CbMaLyIq.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectCbMasterLy".
        Copy "SelectGlParameter".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCbMastLy.
           COPY ChlfdGlParam.

       WORKING-STORAGE SECTION.
       77  WS-TYPE            PIC X VALUE " ".
       77  WS-BUDGET          PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN          PIC S9(8)V99 VALUE 0.
       77  WS-TOTALMV         PIC S9(8)V99 VALUE 0.
       77  WS-TOTALLY         PIC S9(8)V99 VALUE 0.
       77  NEW-GLNO           PIC X VALUE " ".
       77  WS-END             PIC X VALUE " ".
       77  WS-NUMBER          PIC X(12) VALUE " ".
       77  SUB-1-SAVE         PIC S9(5) VALUE 0.
       01  WS-MONTH-BUDGET.
           03  WS-MONTH-BU        PIC S9(8)V99 OCCURS 13.
       01  WS-CB-LY-STATUS.
           03  WS-CB-LY-ST1       PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
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
            MOVE 0 TO WS-TOTALMV.

            MOVE "                        " TO F-NAMEFIELD ALPHA-RATE.
       GET-001.
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
               MOVE WS-GLNO-CHECK TO CB-LY-NUMBER.
            IF SIGN-FOUND = 9
               MOVE 0 TO SIGN-FOUND
               GO TO GET-001.
            IF F-EXIT-CH = X"0C"
                PERFORM READ-CB-LY-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               PERFORM DISPLAY-FORM
               GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B" AND NOT = X"1B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-CB.
            IF NEW-GLNO = "Y"
               PERFORM CLEAR-FORM
               MOVE "INVALID ACCOUNT, RE-ENTER OR 'PGDN' FOR NEXT ACC."
               TO CB-LY-DESCRIPTION.
        GET-003.
            MOVE "ACCNUM" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CB-LY-NUMBER TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "DESCRIPTION" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE CB-LY-DESCRIPTION TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BALANCE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CB-LY-BALANCE TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "OPENYEARBAL" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE CB-LY-OPEN-YEAR-BAL TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "LYOPENBAL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE CB-LY-LAST-YEAR-BAL TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE 1 TO SUB-1 F-INDEX.
       GET-006.
            MOVE "PER" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE CB-LY-PER (SUB-1) TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-006.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-030.
            MOVE "LASTPER" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CB-LY-LAST-PER (SUB-1) TO F-EDNAMEFIELDREC.
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
            MOVE 0 TO WS-TOTALMV WS-TOTALLY.
            MOVE 1 TO SUB-1.
       ATS-005.
            COMPUTE WS-TOTALMV = WS-TOTALMV + CB-LY-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                GO TO ATS-005.
            MOVE 1 TO SUB-1.
       ATS-030.
            COMPUTE WS-TOTALLY = WS-TOTALLY + CB-LY-LAST-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                GO TO ATS-030.
            MOVE 1 TO SUB-1.
       ATS-999.
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
       READ-CB SECTION.
       R-CB-LY-000.
             MOVE CB-LY-NUMBER TO WS-NUMBER.
             START CB-LY-MASTER KEY NOT < CB-LY-KEY.
       R-CB-LY-010.
             READ CB-LY-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-CB-LY-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-GLNO
                MOVE WS-NUMBER TO CB-LY-NUMBER
                GO TO R-CB-LY-999.
             IF WS-CB-LY-ST1 NOT = 0
                MOVE "CBMAST-LY RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CB-LY-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CB-LY-ST1
                GO TO R-CB-LY-010.
             MOVE "N" TO NEW-GLNO.
       R-CB-LY-999.
             EXIT.
      *
       START-GL SECTION.
       CB-LY-CB-LY-000.
              MOVE WS-NUMBER TO CB-LY-NUMBER.
              START CB-LY-MASTER KEY NOT LESS CB-LY-NUMBER
                 INVALID KEY NEXT SENTENCE.
       CB-LY-CB-LY-999.
             EXIT.
      *
       READ-CB-LY-NEXT SECTION.
       RSN-005. 
           READ CB-LY-MASTER NEXT
             AT END 
               MOVE " " TO CB-LY-NUMBER
                           WS-NUMBER
               MOVE "Y" TO WS-END
               PERFORM CLEAR-FORM
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-CB-LY-ST1 = 23 OR 35 OR 49
               MOVE "CB-LY-MASTER FILE BUSY, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CB-LY-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CB-LY-ST1
                GO TO RSN-005.
           IF WS-CB-LY-ST1 NOT = 0
               MOVE 0 TO WS-CB-LY-ST1
               PERFORM START-GL
               GO TO RSN-005.
           MOVE CB-LY-NUMBER TO WS-NUMBER.
           MOVE "N" TO NEW-GLNO.
       RSN-999.
             EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO CB-LY-NUMBER
                         CB-LY-DESCRIPTION.
             MOVE 1 TO SUB-1.
       CLSC-005.
             MOVE 0   TO CB-LY-PER (SUB-1)
                         CB-LY-LAST-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                 GO TO CLSC-005.
             MOVE 1 TO SUB-1.
       CLSC-010.
           MOVE 0 TO CB-LY-BALANCE
                     CB-LY-OPEN-YEAR-BAL
                     CB-LY-LAST-YEAR-BAL.
       CLSC-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O CB-LY-MASTER.
           IF WS-CB-LY-ST1 NOT = 0
              MOVE "GLMASTERLY FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-LY-ST1
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
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CbMaLyIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CB-LY-MASTER.
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
      * END-OF-JOB
