        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlBudgMt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlMast.
      *
       WORKING-STORAGE SECTION.
       77  WS-ACCEPT           PIC X.
       77  WS-LAST-ACC         PIC X(12) VALUE " ".
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1   PIC 99.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-HEADER   PIC X(2).
               05  WS-SUB      PIC X(4).
           03  WS-REST         PIC X(6). 
       01  WS-HEAD-DETAILS.
           03  WS-HEAD-ACC     PIC X(12).
           03  WS-HD-BU        PIC S9(8)V99 OCCURS 12.
       01  WS-SUBHEAD-DETAILS.
           03  WS-SUBHEAD-ACC  PIC X(12).
           03  WS-SUBHD-BU     PIC S9(8)V99 OCCURS 12.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 321 TO POS.
           DISPLAY "** GENERAL LEDGER BUDGET UPDATE ROUTINE **" AT POS.
           MOVE 421 TO POS.
           DISPLAY "******************************************" AT POS.
       CONTROL-005.
           MOVE 1510 TO POS.
           DISPLAY "PRESS <RETURN> TO CONTINUE, <END> TO EXIT."
           AT POS.
           ADD 50 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 60        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

      *     ACCEPT WS-ACCEPT AT POS.
           IF W-ESCAPE-KEY = 1
              GO TO CONTROL-010.
           IF W-ESCAPE-KEY = 3
              PERFORM END-900.
           GO TO CONTROL-005.
       CONTROL-010.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       CLEAR-HEADFIELDS SECTION.
       CHF-001.
            MOVE 0 TO SUB-1.
       CHF-020.
            ADD 1 TO SUB-1.
            IF SUB-1 > 12
               GO TO CHF-030.
            MOVE 0 TO WS-HD-BU (SUB-1).
            GO TO CHF-020.
       CHF-030.
            MOVE " " TO WS-HEAD-ACC.
       CHF-999.
            EXIT.
      *
       CLEAR-SUBFIELDS SECTION.
       CF-001.
            MOVE 0 TO SUB-1.
       CF-020.
            ADD 1 TO SUB-1.
            IF SUB-1 > 12
               GO TO CF-030.
            MOVE 0 TO WS-SUBHD-BU (SUB-1).
            GO TO CF-020.
       CF-030.
            MOVE " " TO WS-SUBHEAD-ACC.
       CF-999.
            EXIT.
      *
       READ-NEXT-ACC SECTION.
       RNA-010.
            START GL-MASTER KEY NOT < GL-KEY.
            READ GL-MASTER NEXT 
               AT END NEXT SENTENCE.
       RNA-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE 3010 TO POS.
            DISPLAY "Update in Progress, Please Be patient...." AT POS.
       PRR-001.
            PERFORM CLEAR-HEADFIELDS.
            PERFORM CLEAR-SUBFIELDS.
            MOVE " " TO GL-NUMBER.
            START GL-MASTER KEY NOT < GL-KEY.
       PRR-002.
            READ GL-MASTER NEXT 
               AT END NEXT SENTENCE.
            IF WS-GLMAST-ST1 = 10
               MOVE 0 TO WS-GLMAST-ST1
               PERFORM REWRITE-SUB-HEADER
               PERFORM REWRITE-HEADER
               PERFORM CLEAR-HEADFIELDS
               PERFORM CLEAR-SUBFIELDS
               GO TO PRR-999.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE "GL-MASTER BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO PRR-002.
           MOVE GL-NUMBER TO WS-GLNUMBER WS-LAST-ACC.

           MOVE 2610 TO POS.
           DISPLAY "NEXT ACCOUNT READ:" AT POS.
           ADD 20 TO POS.
           DISPLAY GL-NUMBER AT POS.

           IF WS-SUB = " "
            IF WS-HEAD-ACC = " "
               MOVE GL-NUMBER TO WS-HEAD-ACC
               MOVE " " TO WS-SUBHEAD-ACC
               GO TO PRR-002
            ELSE
               PERFORM REWRITE-SUB-HEADER
               PERFORM REWRITE-HEADER
               PERFORM CLEAR-HEADFIELDS
               PERFORM CLEAR-SUBFIELDS
               MOVE WS-LAST-ACC TO WS-HEAD-ACC GL-NUMBER
               PERFORM READ-NEXT-ACC
               GO TO PRR-002.
           IF WS-REST = " "
            IF WS-SUBHEAD-ACC = " "
               MOVE GL-NUMBER TO WS-SUBHEAD-ACC
               GO TO PRR-002
            ELSE
               PERFORM REWRITE-SUB-HEADER
               PERFORM CLEAR-SUBFIELDS
               MOVE WS-LAST-ACC TO WS-SUBHEAD-ACC GL-NUMBER
               PERFORM READ-NEXT-ACC
               GO TO PRR-002.
           MOVE 0 TO SUB-1.
       PRR-020.
           ADD 1 TO SUB-1.
           IF SUB-1 > 12
              MOVE 1 TO SUB-1
              GO TO PRR-050.
           ADD GL-PER-BU (SUB-1) TO WS-HD-BU (SUB-1)
                                    WS-SUBHD-BU (SUB-1).
           GO TO PRR-020.
       PRR-050.
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       REWRITE-HEADER SECTION.
       RH-001.
           MOVE WS-HEAD-ACC TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       RH-010.
            READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "HEADER ACCOUNT DOES NOT EXIST." TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               MOVE WS-HEAD-ACC TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RH-010.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE 3010 TO POS
               MOVE "GLMASTER BUSY ON READ-LOCK, WS-GLMAST-ST1 = "
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO RH-010.
           MOVE 0 TO SUB-1.
       RH-030.
           ADD 1 TO SUB-1.
           IF SUB-1 > 12
              MOVE 1 TO SUB-1
              GO TO RH-050.
           MOVE WS-HD-BU (SUB-1) TO GL-PER-BU (SUB-1).
           GO TO RH-030.
       RH-050.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
            IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "HEADER ACCOUNT NOT RE-WRITTEN." TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               MOVE WS-HEAD-ACC TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RH-999.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE "GL-MASTER HEADER BUSY ON REWRITE, E'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO RH-050.
       RH-999.
           EXIT.
      *
       REWRITE-SUB-HEADER SECTION.
       RSH-001.
           MOVE WS-SUBHEAD-ACC TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       RSH-010.
            READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "SUBHEADER ACCOUNT DOES NOT EXIST" TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               MOVE WS-SUBHEAD-ACC TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSH-010.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE "GL-MAST SUBHEAD BUSY ON REWRITE, E'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO RSH-010.
           MOVE 0 TO SUB-1.
       RSH-030.
           ADD 1 TO SUB-1.
           IF SUB-1 > 12
              MOVE 1 TO SUB-1
              GO TO RSH-050.
           MOVE WS-SUBHD-BU (SUB-1) TO GL-PER-BU (SUB-1).
           GO TO RSH-030.
       RSH-050.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
            IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "SUBHEADER ACCOUNT NOT RE-WRITTEN." TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               MOVE WS-SUBHEAD-ACC TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSH-999.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE "GL-MAST SUBHEAD BUSY ON REWRITE, E'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO RSH-050.
       RSH-999.
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
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *    ACCEPT WS-DATE FROM DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-500.
           CLOSE GL-MASTER.
       END-900.
          EXIT PROGRAM.
      *    STOP RUN.
       END-999.
           EXIT.
      *
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
       Copy "CTOSCobolAccept".
      *END-OF-JOB.
