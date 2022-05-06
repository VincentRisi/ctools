        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlMastRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlMast.
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
       77  WS-RANGE1            PIC X(12) VALUE " ".
       77  WS-RANGE2            PIC X(12) VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-HEAD-CNT          PIC 9(4) VALUE 0.
       77  WS-SUB-CNT           PIC 9(4) VALUE 0.
       77  WS-DETAIL-CNT        PIC 9(4) VALUE 0.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1      PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-HEADER   PIC X(2).
               05  WS-SUB      PIC X(4).
           03  WS-REST         PIC X(6).
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(8) VALUE " ".
           03  FILLER         PIC X(32) VALUE
           "GENERAL LEDGER CHART OF ACCOUNTS".
           03  FILLER         PIC X(20) VALUE " ".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(3).
           03  H1-PER-BEG     PIC X(10).
           03  FILLER         PIC X(4) VALUE " TO ".
           03  H1-PER-END     PIC X(13).
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(7) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(23) VALUE " ".
           03  FILLER         PIC X(32) VALUE ALL "*".
           03  FILLER         PIC X(74) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(15) VALUE "ACCOUNT".
           03  FILLER         PIC X(45) VALUE "DESCRIPTION".
           03  FILLER         PIC X(10) VALUE "TYPE".
           03  FILLER         PIC X(10) VALUE "BALANCE".
           03  FILLER         PIC X(15) VALUE "CURRENT PER".
           03  FILLER         PIC X(15) VALUE "OPENING BAL".
           03  FILLER         PIC X(22) VALUE "L/YEAR BAL".
       01  DETAIL-LINE.
           03  D-ACCOUNT      PIC X(15).
           03  D-DESC         PIC X(47).
           03  D-TYPE         PIC X(4).
           03  D-BALANCE      PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-CURRENT      PIC Z(7)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-OPEN         PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-LAST         PIC Z(7)9.99-.
           03  FILLER         PIC X(11) VALUE " ".
       01  TOTAL-LINE.
           03  T-NAME         PIC X(22).
           03  T-QTY          PIC Z(3)9.
           03  FILLER         PIC X(106) VALUE " ".
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
           DISPLAY "** GLMASTER CHART OF ACCOUNTS LISTING **" AT POS.
           MOVE 421 TO POS.           
           DISPLAY "****************************************" AT POS.
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
           MOVE " " TO WS-RANGE1 WS-RANGE2.
           MOVE 1010 TO POS.
           DISPLAY "         FROM GLMASTER NUMBER: [            ]"
                      AT POS.
           MOVE 1042 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 12        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
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
            DISPLAY "           TO GLMASTER NUMBER: [            ]"
                      AT POS.
            MOVE 1242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 12        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           IF W-ESCAPE-KEY = 4
               GO TO GET-000.
           IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
           MOVE 1410 TO POS.
           DISPLAY "Print Balances for Accounts   :[ ]" AT POS.
           ADD 32 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               GO TO GET-010.
           IF WS-ANSWER NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO GET-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-050.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE 2510 TO POS.
           DISPLAY "The Report is being compiled.........." AT POS.
       GET-999.
            EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
       RP-010.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO PARAMETER RECORD, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
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
            MOVE WS-RANGE1 TO GL-NUMBER.
            START GL-MASTER KEY NOT < GL-KEY.
       PRR-002.
            READ GL-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-GLMAST-ST1 = 10
               MOVE 0 TO WS-GLMAST-ST1
               GO TO PRR-999.
            IF WS-GLMAST-ST1 NOT = 0
            MOVE "GLMASTER BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO PRR-002.
            IF GL-NUMBER < WS-RANGE1
               GO TO PRR-002.
            IF GL-NUMBER > WS-RANGE2
               GO TO PRR-999.
       PRR-010.
            IF LINE-CNT < 58
               GO TO PRR-020.
            ADD 1 TO PAGE-CNT.
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
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE 5 TO LINE-CNT.
       PRR-020.
           MOVE GL-NUMBER TO WS-GLNUMBER.
           IF WS-SUB = "    "
            IF WS-REST = "      "
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO LINE-CNT
               GO TO PRR-050.
           IF WS-SUB NOT = "    "
            IF WS-REST = "      "
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO WS-SUB-CNT
               ADD 2 TO LINE-CNT.
       PRR-050.
           MOVE GL-NUMBER        TO D-ACCOUNT.
           MOVE GL-DESCRIPTION   TO D-DESC.
           MOVE GL-P-B           TO D-TYPE.
           IF WS-ANSWER = "Y"
               MOVE GL-BALANCE       TO D-BALANCE
               MOVE GL-OPEN-PER-BAL  TO D-CURRENT
               MOVE GL-OPEN-YEAR-BAL TO D-OPEN
               MOVE GL-LAST-YEAR-BAL TO D-LAST.

           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           ADD 1 TO LINE-CNT.
           IF WS-SUB = "    "
            IF WS-REST = "      "
               MOVE "************" TO PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO LINE-CNT
               ADD 1 TO WS-HEAD-CNT
               GO TO PRR-002.
           IF WS-SUB NOT = "    "
            IF WS-REST = "      "
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO LINE-CNT
               GO TO PRR-002.
           ADD 1 TO WS-DETAIL-CNT.
           GO TO PRR-002.
       PRR-999.
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
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-012.

           PERFORM READ-PARAMETER.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM ENTER-PERIOD-DATES.
           
           MOVE GLPA-CURRENT-GLPER              TO H1-PERIOD
           MOVE GL-BEGDATE (GLPA-CURRENT-GLPER) TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE                    TO H1-PER-BEG
           MOVE GL-ENDDATE (GLPA-CURRENT-GLPER) TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE                    TO H1-PER-END.

           CLOSE GLPARAMETER-FILE.
        OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
           WRITE PRINT-REC.

           MOVE "No. of Header Acc's  :" TO T-NAME
           MOVE WS-HEAD-CNT              TO T-QTY
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE "No. of Sub-Head Acc's:" TO T-NAME
           MOVE WS-SUB-CNT               TO T-QTY
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE "No. of Detail Acc's  :" TO T-NAME
           MOVE WS-DETAIL-CNT            TO T-QTY
           WRITE PRINT-REC FROM TOTAL-LINE.
           
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE GL-MASTER.
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
       Copy "NumberCheck".
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
      * END-OF-JOB.
