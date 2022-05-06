        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlJrnLRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlParameter".
        Copy "SelectGlJrn".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlMast.
           COPY ChlfdGlJrn.
           COPY ChlfdGlParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(10) VALUE " ".
       77  WS-RANGE2            PIC X VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-NORM-CNT          PIC 9(4) VALUE 0.
       77  WS-REC-CNT           PIC 9(4) VALUE 0.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1       PIC 99.
       01  WS-GLJRN-STATUS.
           03  WS-GLJRN-ST1        PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(8) VALUE " ".
           03  FILLER         PIC X(35) VALUE
           "GENERAL LEDGER JOURNAL FILE LISTING".
           03  FILLER         PIC X(10) VALUE " ".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(4).
           03  H1-PER-BEG     PIC X(10).
           03  FILLER         PIC X(4) VALUE " TO ".
           03  H1-PER-END     PIC X(13).
           03  FILLER         PIC X(19) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(1) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(23) VALUE " ".
           03  FILLER         PIC X(35) VALUE ALL "*".
           03  FILLER         PIC X(74) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(15) VALUE "JOURNAL".
           03  FILLER         PIC X(15) VALUE "ACCOUNT NO:".
           03  FILLER         PIC X(45) VALUE "ACCOUNT DESCRIPTION".
           03  FILLER         PIC X(20) VALUE "AMOUNT".
           03  FILLER         PIC X(37) VALUE "LINE DESCRIPTION".
       01  DETAIL-LINE.
           03  FILLER         PIC X(15).
           03  D-ACCOUNT      PIC X(15).
           03  D-DESC         PIC X(40).
           03  D-AMOUNT       PIC Z(7)9.99-.
           03  FILLER         PIC X(13) VALUE " ".
           03  D-LINE-DESC    PIC X(37).
       01  JOURNAL-LINE.
           03  J-JRN          PIC X(15).
           03  J-DATE         PIC X(10).
           03  J-ACTION       PIC X(3).
           03  J-PERIOD       PIC X(4).
           03  J-DESC         PIC X(35).
           03  J-POSTED       PIC X(65).
       01  TOTAL-LINE.
           03  T-NAME         PIC X(27).
           03  T-QTY          PIC Z(3)9.
           03  FILLER         PIC X(101) VALUE " ".
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
           DISPLAY "** GLMASTER JOURNAL FILE LISTING **" AT POS.
           MOVE 421 TO POS.           
           DISPLAY "***********************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM OPEN-FILES.
           PERFORM GET-DATA.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE 1010 TO POS.
           DISPLAY
            "Enter A JOURNAL No, Leave BLANK For ALL :[          ]"
                      AT POS.
           MOVE 1052 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
           MOVE 1210 TO POS.
           DISPLAY "Enter D=Detail, S=Summary  :[ ]" AT POS.
           MOVE 1239 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 38        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           IF W-ESCAPE-KEY = 4
               GO TO GET-000.
           IF WS-RANGE2 NOT = "D" AND NOT = "S"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-020.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE 2510 TO POS.
           DISPLAY "The Report is being compiled.........." AT POS.
       GET-999.
            EXIT.
      *
       READ-GLMASTER SECTION.
       RGL-000.
            MOVE GLJRN-GLNUMBER (SUB-1) TO GL-NUMBER.
            START GL-MASTER KEY NOT < GL-KEY.
       RGL-002.
            READ GL-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE " **INVALID ACCOUNT NUMBER**" TO GL-DESCRIPTION
               MOVE 0 TO WS-GLMAST-ST1
               GO TO RGL-999.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLMASTER FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO RGL-002.
       RGL-999.
            EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
       RP-010.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
              MOVE "NO GLPARAMETER23 RECORD READ, CALL THE SUPERVISOR."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-010.
       RP-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE WS-RANGE1 TO GLJRN-REFERENCE.
            START GLJRN-FILE KEY NOT < GLJRN-KEY.
       PRR-002.
            READ GLJRN-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-GLJRN-ST1 = 10
               MOVE 0 TO WS-GLJRN-ST1
               GO TO PRR-999.
            IF WS-GLJRN-ST1 NOT = 0
              MOVE "GLJRN FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLJRN-ST1
              GO TO PRR-002.
            IF GLJRN-COMPLETE = "Y"
               GO TO PRR-002.
            IF WS-RANGE1 = "    "
               GO TO PRR-005.
            IF GLJRN-REFERENCE < WS-RANGE1
               GO TO PRR-002.
            IF GLJRN-REFERENCE > WS-RANGE1
               GO TO PRR-999.
       PRR-005.
            MOVE 2310 TO POS.
            DISPLAY "JOURNAL BEING PROCESSED:" AT POS.
            ADD 25 TO POS.
            DISPLAY GLJRN-REFERENCE AT POS.

            IF LINE-CNT > 60
               PERFORM PRR-010.
            MOVE GLJRN-REFERENCE             TO J-JRN.
            MOVE GLJRN-DATE                  TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE                TO J-DATE.
            MOVE GLJRN-ACTION                TO J-ACTION.
            MOVE GLJRN-PERIOD                TO J-PERIOD.
            IF GLJRN-COMPLETE = "R"
                 MOVE "**READY TO POST**   " TO J-POSTED.
            IF GLJRN-COMPLETE = "Y"
                 MOVE "**ALREADY POSTED**  " TO J-POSTED.
            IF GLJRN-COMPLETE = "P"
                 MOVE "**HELD BY OPERATOR**" TO J-POSTED.
            MOVE GLJRN-MAIN-DESC             TO J-DESC.
            WRITE PRINT-REC FROM JOURNAL-LINE.
            MOVE " " TO PRINT-REC.
            ADD 1 TO LINE-CNT.
            IF GLJRN-ACTION = "R"
               ADD 1 TO WS-REC-CNT.
            IF GLJRN-ACTION = " "
               ADD 1 TO WS-NORM-CNT.
            MOVE 1 TO SUB-1.
            IF LINE-CNT < 61
               GO TO PRR-020.
       PRR-010.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE.
            MOVE " " TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
            WRITE PRINT-REC FROM HEAD1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD2.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD3.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE 5 TO LINE-CNT.
       PRR-020.
           IF WS-RANGE2 = "S"
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO LINE-CNT
               GO TO PRR-002.
           IF LINE-CNT > 60
               PERFORM PRR-010.
           IF GLJRN-GLNUMBER (SUB-1) = " "
                 MOVE " " TO PRINT-REC
                 WRITE PRINT-REC
                 ADD 1 TO LINE-CNT
                 GO TO PRR-002.
           PERFORM READ-GLMASTER.
           MOVE GLJRN-GLNUMBER (SUB-1)  TO D-ACCOUNT.
           MOVE GL-DESCRIPTION          TO D-DESC.
           MOVE GLJRN-AMOUNT (SUB-1)    TO D-AMOUNT.
           MOVE GLJRN-LINE-DESC (SUB-1) TO D-LINE-DESC.

           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           ADD 1 TO LINE-CNT SUB-1.
           IF SUB-1 < 51
              GO TO PRR-020
           ELSE
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC
              ADD 1 TO LINE-CNT
              GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO OPEN-000.
       OPEN-005.
           OPEN I-O GLJRN-FILE.
           IF WS-GLJRN-ST1 NOT = 0
              MOVE "JRN FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLJRN-ST1
              GO TO OPEN-005.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-012.

           PERFORM READ-PARAMETER.
           MOVE GLPA-NAME TO CO-NAME.
           PERFORM ENTER-PERIOD-DATES.

           MOVE GLPA-CURRENT-GLPER              TO H1-PERIOD
           MOVE GL-BEGDATE (GLPA-CURRENT-GLPER) TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE                    TO H1-PER-BEG
           MOVE GL-ENDDATE (GLPA-CURRENT-GLPER) TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE                    TO H1-PER-END.

           CLOSE GLPARAMETER-FILE.
       OPEN-999.
          EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 55
                PERFORM PRR-010.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
           WRITE PRINT-REC.

           MOVE "No. of Normal Journals   :" TO T-NAME.
           MOVE WS-NORM-CNT                  TO T-QTY.
           WRITE PRINT-REC FROM TOTAL-LINE.

           MOVE "No. of Recurring Journals:" TO T-NAME.
           MOVE WS-REC-CNT                   TO T-QTY.
           WRITE PRINT-REC FROM TOTAL-LINE.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE GL-MASTER
                 GLJRN-FILE.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
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
      *
      * END-OF-JOB.
