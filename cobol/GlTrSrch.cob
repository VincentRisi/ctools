        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlTrSrch.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlParameter".
        Copy "SelectGlTrans".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlMast.
           COPY ChlfdGlTrans.
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
       77  WS-BATCH             PIC X(10) VALUE " ".
       77  WS-FUTURE            PIC X VALUE " ".
       77  WS-ACC-SAVE          PIC X(12) VALUE " ".
       77  WS-PERIOD            PIC 99 VALUE 0.
       77  WS-TR-CODE           PIC 99 VALUE 0.
       77  WS-TOTAL-DEBIT       PIC S9(8)V99 VALUE 0.
       77  WS-TOTAL-CREDIT      PIC S9(8)V99 VALUE 0.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1      PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1     PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  BATCH-TOTALS.
           03  WS-BATCH-TOTALS OCCURS 50.
               05  WS-BATCH-NAME  PIC X(10).
               05  WS-DEBITS      PIC S9(8)V99.
               05  WS-CREDITS     PIC S9(8)V99.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-HEADER      PIC X(2).
               05  WS-SUB         PIC X(4).
           03  WS-REST            PIC X(6).
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(40) VALUE
           "GENERAL LEDGER TRANSACTION SEARCH".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(12) VALUE " ".
           03  H1-PER-BEG     PIC X(10).
           03  FILLER         PIC X(4) VALUE " TO ".
           03  H1-PER-END     PIC X(10).
           03  FILLER         PIC X(8) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z(2)9.
           03  FILLER         PIC X(6) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(27) VALUE " ".
           03  FILLER         PIC X(33) VALUE ALL "*".
           03  FILLER         PIC X(72) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(18) VALUE "ACCOUNT     TRANS".
           03  FILLER         PIC X(26) VALUE "TYPE   PD   DATE".
           03  FILLER         PIC X(29) VALUE "DESCRIPTION".
           03  FILLER         PIC X(12) VALUE "DEBITS".
           03  FILLER         PIC X(12) VALUE "CREDITS".
           03  FILLER         PIC X(35) VALUE " ".
       01  DETAIL-LINE.
           03  D-ACCOUNT         PIC X(19).
           03  D-TYPE            PIC X(3).
           03  D-DESC            PIC X(46).
           03  D-DEBIT           PIC Z(7)9.99- BLANK WHEN ZERO.
           03  FILLER            PIC X(1) VALUE " ".
           03  D-CREDIT          PIC Z(7)9.99- BLANK WHEN ZERO.
           03  FILLER            PIC X(26) VALUE " ".
       01  TRANS-LINE.
           03  TRANS-REF         PIC X(11).
           03  TRANS-TRANS       PIC Z(5)9.
           03  FILLER            PIC X(2) VALUE " ".
           03  TRANS-TYPE        PIC X(5).
           03  TRANS-PERIOD      PIC X(5).
           03  TRANS-DATE        PIC X(10).
           03  FILLER            PIC X(5) VALUE " ".
           03  TRANS-DESC        PIC X(24).
           03  TRANS-DEBIT       PIC Z(7)9.99- BLANK WHEN ZERO.
           03  FILLER            PIC X(1) VALUE " ".
           03  TRANS-CREDIT      PIC Z(7)9.99- BLANK WHEN ZERO.
           03  FILLER            PIC X(26) VALUE " ".
       01  UNDER-LINE.
           03  FILLER            PIC X(19) VALUE " ".
           03  UNDER-DESC        PIC X(48) VALUE " ".
           03  FILLER            PIC X(12) VALUE "============".
           03  FILLER            PIC X(1) VALUE " ".
           03  FILLER            PIC X(12) VALUE "============".
           03  FILLER            PIC X(40) VALUE " ".
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
           PERFORM CLEAR-SCREEN
           MOVE 321 TO POS
           DISPLAY "** GENERAL LEDGER TRANSACTION SEARCH **" AT POS
           MOVE 421 TO POS
           DISPLAY "***************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           PERFORM READ-ALL-TRANS.
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
           DISPLAY "Print A Period, Leave BLANK for ALL:[  ]" AT POS.
           ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PERIOD.

           IF W-ESCAPE-KEY = 4
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-025.
           MOVE 1610 TO POS.
           DISPLAY "Enter Trans-Code Type To Search For:[  ]" AT POS.
           ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TR-CODE.

           IF W-ESCAPE-KEY = 4
               GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-025.
       GET-030.
           MOVE 1810 TO POS.
           DISPLAY "Enter Batch To Check, BLANK For ALL:[          ]"
               AT POS.
           ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-BATCH.

           IF W-ESCAPE-KEY = 4
               GO TO GET-025.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-040.
           MOVE 2010 TO POS
           DISPLAY "Include FUTURE Trans, Y=Yes N=No O=Only Future :[ ]"
           AT POS
           ADD 49 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 58        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FUTURE.

           IF W-ESCAPE-KEY = 4
               GO TO GET-030.
           IF WS-FUTURE NOT = "Y" AND NOT = "N" AND NOT = "O"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
       GET-050.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE 2910 TO POS.
           DISPLAY "The Report is being compiled.........." AT POS.
       GET-999.
            EXIT.
      *
       READ-ALL-TRANS SECTION.
       RALT-005.
           MOVE ALL "X" TO WS-ACC-SAVE.
           MOVE WS-RANGE1 TO GLTRANS-ACCOUNT-NUMBER.
           START GLTRANS-FILE KEY NOT < GLTRANS-ACCOUNT-NUMBER
               INVALID KEY NEXT SENTENCE.
       RALT-010.
           READ GLTRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 10
               GO TO RALT-900.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GL-TRANS BUSY ON READ-ALL-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE GLTRANS-KEY TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              
           MOVE GLTRANS-KEY TO WS-MESSAGE
           PERFORM ERROR-MESSAGE
           MOVE GLTRANS-PERIOD TO WS-MESSAGE
           PERFORM ERROR-000
           MOVE GLTRANS-ACC-DATE TO WS-MESSAGE
           PERFORM ERROR1-MESSAGE
           
           

              MOVE 0 TO WS-GLTRANS-ST1
              GO TO RALT-010.
           IF WS-RANGE1 = WS-RANGE2
            IF GLTRANS-ACCOUNT-NUMBER NOT = WS-RANGE1
               GO TO RALT-900.
           IF GLTRANS-ACCOUNT-NUMBER < WS-RANGE1
               GO TO RALT-010.
           IF GLTRANS-ACCOUNT-NUMBER > WS-RANGE2
               GO TO RALT-900.
               
           MOVE 2510 TO POS
           DISPLAY "Transaction Ref:" AT POS
           ADD 17 TO POS
           DISPLAY GLTRANS-REFERENCE AT POS.
               
           IF WS-BATCH NOT = " "
            IF GLTRANS-REFERENCE NOT = WS-BATCH
               GO TO RALT-010.
               
           IF WS-FUTURE = "N"
            IF GLTRANS-FUTURE = "F"
               GO TO RALT-010.
           IF WS-FUTURE = "O"
            IF GLTRANS-FUTURE NOT = "F"
               GO TO RALT-010.
               
           IF WS-PERIOD NOT = 0
            IF GLTRANS-NO NOT = WS-PERIOD
               GO TO RALT-010.
               
           IF WS-TR-CODE NOT = 0
            IF GLTRANS-TYPE NOT = WS-TR-CODE
               GO TO RALT-010.
               
           MOVE 2610 TO POS.
           DISPLAY "Processing Transactions For Account:" AT POS.
           ADD 37 TO POS.
           DISPLAY GLTRANS-ACCOUNT-NUMBER AT POS.
       RALT-020.
            IF LINE-CNT > 58
               PERFORM RALT-600.
           IF GLTRANS-ACCOUNT-NUMBER NOT = WS-ACC-SAVE
               PERFORM RALT-700
               MOVE GLTRANS-ACCOUNT-NUMBER TO WS-ACC-SAVE.
           MOVE GLTRANS-REFERENCE     TO TRANS-REF
           MOVE GLTRANS-TRANS         TO TRANS-TRANS
           MOVE GLTRANS-DATE          TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE          TO TRANS-DATE
           MOVE GLTRANS-TYPE          TO TRANS-TYPE
           MOVE GLTRANS-PERIOD        TO TRANS-PERIOD
           MOVE GLTRANS-LINE-DESC     TO TRANS-DESC
           IF GLTRANS-AMOUNT < 0
                MOVE 0                TO TRANS-DEBIT
                ADD GLTRANS-AMOUNT    TO WS-TOTAL-CREDIT
                MOVE GLTRANS-AMOUNT   TO TRANS-CREDIT
           ELSE
                MOVE 0                TO TRANS-CREDIT
                ADD GLTRANS-AMOUNT    TO WS-TOTAL-DEBIT
                MOVE GLTRANS-AMOUNT   TO TRANS-DEBIT.
           WRITE PRINT-REC FROM TRANS-LINE
           MOVE " "                   TO PRINT-REC
           ADD 1 TO LINE-CNT
           PERFORM ADD-TO-BATCH-TOTAL.
           GO TO RALT-010.
       RALT-600.
            ADD 1                                    TO PAGE-CNT.
            MOVE PAGE-CNT                            TO H1-PAGE.
            IF WS-PERIOD = 0
                 MOVE "ALL PERIODS"                  TO H1-PERIOD
            ELSE
                 MOVE WS-PERIOD                      TO H1-PERIOD.
            IF WS-PERIOD NOT = 0
                MOVE GL-BEGDATE (WS-PERIOD)          TO SPLIT-DATE
            ELSE
                MOVE GL-BEGDATE (GLPA-CURRENT-GLPER) TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE                        TO H1-PER-BEG.
            IF WS-PERIOD NOT = 0
               MOVE GL-ENDDATE (WS-PERIOD)           TO SPLIT-DATE
            ELSE
                MOVE GL-ENDDATE (GLPA-CURRENT-GLPER) TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE                        TO H1-PER-END.
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
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE 7 TO LINE-CNT.
       RALT-700.
            MOVE " " TO PRINT-REC DETAIL-LINE.
            MOVE GLTRANS-ACCOUNT-NUMBER TO D-ACCOUNT GL-NUMBER.
            PERFORM READ-GLNUMBER.
            MOVE GL-DESCRIPTION         TO D-DESC.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            ADD 2 TO LINE-CNT.
       RALT-900.
           CLOSE GLTRANS-FILE.
       RALT-999.
           EXIT.
      *
       ADD-TO-BATCH-TOTAL SECTION.
       ATBT-005.
            MOVE 1 TO SUB-1.
       ATBT-010.
            IF WS-BATCH-NAME (SUB-1) = " "
               GO TO ATBT-100.
            IF WS-BATCH-NAME (SUB-1) = GLTRANS-REFERENCE
               GO TO ATBT-200.
            ADD 1 TO SUB-1.
            IF SUB-1 < 50
               GO TO ATBT-010.
           GO TO ATBT-999.
       ATBT-100.
           MOVE GLTRANS-REFERENCE TO WS-BATCH-NAME (SUB-1).
           MOVE 0 TO WS-DEBITS (SUB-1) WS-CREDITS (SUB-1).
       ATBT-200.
           IF GLTRANS-AMOUNT < 0
                ADD GLTRANS-AMOUNT    TO WS-CREDITS (SUB-1)
           ELSE
                ADD GLTRANS-AMOUNT    TO WS-DEBITS (SUB-1).
       ATBT-999.
            EXIT.
      *
       READ-GLNUMBER SECTION.
       RGN-010.
           START GL-MASTER KEY NOT < GL-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "*** INVALID ACCOUNT ***" TO GL-DESCRIPTION
              GO TO RGN-900.
       RGN-020.
           READ GL-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "*** INVALID ACCOUNT ***" TO GL-DESCRIPTION.
       RGN-900.
           IF GL-NUMBER = SPACES
               MOVE "*ACC# BLANK*" TO D-ACCOUNT.
       RGN-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
       RP-010.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO GLPARAMETER RECORD, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON READ, 'ESC' TO RETRY."
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
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-005.
            PERFORM READ-PARAMETER.
            MOVE GLPA-NAME TO CO-NAME.
            PERFORM ENTER-PERIOD-DATES.
            PERFORM OPEN-010.
            CLOSE GLPARAMETER-FILE.
        OPEN-008.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GLTRANS BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO OPEN-008.
        OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 55
                PERFORM RALT-600.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
           WRITE PRINT-REC.
           WRITE PRINT-REC FROM UNDER-LINE.

           MOVE " "                    TO PRINT-REC DETAIL-LINE.
           MOVE " "                    TO D-ACCOUNT.
           MOVE "Total General Ledger" TO D-DESC.
           MOVE WS-TOTAL-DEBIT         TO D-DEBIT.
           MOVE WS-TOTAL-CREDIT        TO D-CREDIT.
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " "                    TO PRINT-REC DETAIL-LINE.

           COMPUTE WS-TOTAL-CREDIT = WS-TOTAL-CREDIT * -1.
           IF WS-TOTAL-DEBIT NOT = WS-TOTAL-CREDIT
               MOVE "*SEARCH TRANS. IN IM-BALANCE*" TO UNDER-DESC.
           WRITE PRINT-REC FROM UNDER-LINE.
           MOVE " "                    TO PRINT-REC UNDER-DESC.
           ADD 3 TO LINE-CNT.
           MOVE 1 TO SUB-1.
       END-100.
           IF WS-BATCH-NAME (SUB-1) = " "
                  GO TO END-500.
           MOVE WS-BATCH-NAME (SUB-1) TO D-DESC.
           MOVE WS-DEBITS (SUB-1)     TO D-DEBIT.
           MOVE WS-CREDITS (SUB-1)    TO D-CREDIT.
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " "                   TO PRINT-REC DETAIL-LINE.
           IF SUB-1 < 50
               ADD 1 TO SUB-1
               GO TO END-100.
       END-500.
           MOVE " " TO PRINT-REC.

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
