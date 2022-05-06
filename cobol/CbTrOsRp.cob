        IDENTIFICATION DIVISION.
        PROGRAM-ID. CbTrOsRp.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectCbMaster".
        Copy "SelectCbTrans".
        Copy "SelectGlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCbMast.
           COPY ChlfdCbTrans.
           COPY ChlfdGlParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-RANGE1            PIC X(12) VALUE " ".
       77  WS-RANGE2            PIC X(2) VALUE " ".
       77  WS-RANGE3            PIC X(1) VALUE " ".
       77  WS-RANGE4            PIC X(1) VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-DR-AMT-CR         PIC S9(8)V99 VALUE 0.
       77  WS-DR-AMT-DR         PIC S9(8)V99 VALUE 0.
       77  WS-CR-AMT-CR         PIC S9(8)V99 VALUE 0.
       77  WS-CR-AMT-DR         PIC S9(8)V99 VALUE 0.
       77  WS-GL-AMT-CR         PIC S9(8)V99 VALUE 0.
       77  WS-GL-AMT-DR         PIC S9(8)V99 VALUE 0.
       77  WS-OS-AMT-DR         PIC S9(8)V99 VALUE 0.
       77  WS-OS-AMT-CR         PIC S9(8)V99 VALUE 0.
       77  WS-HD-AMT-DR         PIC S9(8)V99 VALUE 0.
       77  WS-HD-AMT-CR         PIC S9(8)V99 VALUE 0.
       77  WS-AMOUNT-OS         PIC S9(8)V99 VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  LINES-DISPLAYED      PIC 9(5) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-NO-OF-TRANS       PIC 9(5) VALUE 0.
       01  WS-CB-STATUS.
           03  WS-CB-ST1          PIC 99.
       01  WS-CBTRANS-STATUS.
           03  WS-CBTRANS-ST1     PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-PERIOD.
           03  WS-1ST-CHAR        PIC X.
           03  WS-PER             PIC 99.
       01  WS-TRANS-TYPE.
           03  FILLER        PIC X(7) VALUE "CHQ PAY".
           03  FILLER        PIC X(7) VALUE "MTX PAY".
           03  FILLER        PIC X(7) VALUE "TRN PAY".
           03  FILLER        PIC X(7) VALUE "R/D DEP".
           03  FILLER        PIC X(7) VALUE "STL CHQ".
           03  FILLER        PIC X(7) VALUE "BNK CHG".
           03  FILLER        PIC X(7) VALUE "BNK INT".
           03  FILLER        PIC X(7) VALUE "TRN 1DY".
           03  FILLER        PIC X(7) VALUE "CR ADJ ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "CHQ DEP".
           03  FILLER        PIC X(7) VALUE "TRN DEP".
           03  FILLER        PIC X(7) VALUE "R/D PAY".
           03  FILLER        PIC X(7) VALUE "STL CHQ".
           03  FILLER        PIC X(7) VALUE "BNK REV".
           03  FILLER        PIC X(7) VALUE "BNK INT".
           03  FILLER        PIC X(7) VALUE "TRN 1DY".
           03  FILLER        PIC X(7) VALUE "DR ADJ ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
       01  WS-TRANS-DESCRED REDEFINES WS-TRANS-TYPE.
           03  WS-TRANS-DESC   PIC X(7) OCCURS 30.
       01  WS-CBNUMBER.
           03  WS-HEAD-SUB.
               05  WS-CBHEADER     PIC X(2).
               05  WS-CBSUBHEADER  PIC X(4).
           03  WS-RESTOFACCOUNT    PIC X(6).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(55) VALUE
           "** CASHBOOK OUTSTANDING TRANSACTIONS REPORT**".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(13).
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(20) VALUE "ACCOUNT            :".
           03  H3-ACC         PIC X(112).
       01  HEAD2-1.
           03  FILLER         PIC X(20) VALUE "NAME               :".
           03  H3-NAME        PIC X(112).
       01  HEAD2-2.
           03  H3-DESC        PIC X(20).
           03  H3-BALANCE     PIC Z(7)9.99-.
           03  FILLER         PIC X(100) VALUE " ".
       01  HEAD2-3.
           03  FILLER         PIC X(20) VALUE "PERIOD READ        :".
           03  H3-PERIOD-READ PIC X(112).
       01  HEAD3.
           03  FILLER         PIC X(11) VALUE " PD TRANS#".
           03  FILLER         PIC X(13) VALUE "JRN No:".
           03  FILLER         PIC X(14) VALUE "TYPE    LGR".
           03  FILLER         PIC X(22) VALUE "STM  ACC NUMBER".
           03  FILLER         PIC X(14) VALUE "DATE".
           03  FILLER         PIC X(17) VALUE "O/S AMT".
           03  FILLER         PIC X(12) VALUE "AMOUNT".
           03  FILLER         PIC X(26) VALUE "DESCRIPTION".
           03  FILLER         PIC X(21) VALUE " ".
       01  DETAIL-LINE.
           03  D-PERIOD       PIC X(4).
           03  D-TRANS        PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-JRNNO        PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-TYPE         PIC X(10).
           03  FILLER         PIC X VALUE " ".
           03  D-TYPE-OF-POST PIC X(6).
           03  D-ALLOCATED    PIC X(4).
           03  D-ACCNO        PIC X(15).
           03  D-DATE         PIC X(10).
           03  FILLER         PIC X(2) VALUE " ".
           03  D-OS-AMT       PIC Z(7)9.99- BLANK WHEN ZERO.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-AMOUNT       PIC Z(7)9.99-.
           03  FILLER         PIC X(5) VALUE " ".
           03  D-DESC         PIC X(25).
        01  TOTAL-LINE.
           03  TOT-DESC       PIC X(22).
           03  TOT-AMOUNT     PIC Z(7)9.99-.
           03  FILLER         PIC X(98) VALUE " ".
        01  TOTAL-LINE1.
           03  TOT-DESC1      PIC X(25).
           03  TOT-AMOUNT1    PIC Z(4)9.
           03  FILLER         PIC X(106) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
           MOVE 308 TO POS
           DISPLAY "** CASHBOOK OUTSTANDING TRANSACTIONS REPORT **"
               AT POS
           MOVE 408 TO POS
           DISPLAY "**********************************************"
               AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
            PERFORM DISPLAY-CB-TOP-INFO.
            PERFORM GET-DATA.
            PERFORM PRINT-ROUTINE.
            PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE " " TO WS-RANGE1 WS-RANGE2 WS-RANGE3.
       GET-001.
           MOVE "750301500" TO WS-RANGE1.
           MOVE 1010 TO POS
           DISPLAY "ENTER THE CASHBOOK MASTER ACC: [            ]"
           AT POS
           MOVE 1042 TO POS.

           MOVE WS-RANGE1 TO CDA-DATA.
           MOVE 12        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

      *     ACCEPT WS-RANGE1 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF WS-RANGE1 > SPACES
                MOVE WS-RANGE1 TO ALPHA-RATE
                PERFORM NUMBER-CHECK.
           MOVE WS-GLNO-CHECK TO WS-RANGE1
           DISPLAY WS-RANGE1 AT POS.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-005
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.
       GET-005.
            PERFORM READ-CBMASTER.
            IF CB-DESCRIPTION = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-001.
            MOVE 1110 TO POS
            DISPLAY "CashBook Acc Name :" AT POS
            ADD 20 TO POS
            DISPLAY CB-DESCRIPTION AT POS.
       GET-010.
            MOVE 1310 TO POS
            DISPLAY "Enter the PERIOD to print for: [  ]" AT POS
            MOVE 1410 TO POS
            DISPLAY " Leave BLANK for all PERIODS." AT POS
            MOVE 1342 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

      *      ACCEPT WS-RANGE2 AT POS.
            IF W-ESCAPE-KEY = 4
                GO TO GET-001.
            MOVE WS-RANGE2 TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-PER
            MOVE 1342 TO POS
            DISPLAY WS-PER AT POS.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-015
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
       GET-015.
            MOVE 2910 TO POS.
            DISPLAY "The Report Is Being Compiled, Please Be Patient."
              AT POS.
            MOVE 3010 TO POS.
            DISPLAY "                                        " AT POS.
       GET-999.
            EXIT.
      *
       READ-CBMASTER SECTION.
       RD-010.
           MOVE WS-RANGE1 TO CB-NUMBER.
           START CB-MASTER KEY NOT < CB-KEY.
       RD-015.
           READ CB-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
               MOVE "UNKNOWN" TO CB-DESCRIPTION
               GO TO RD-999.
           IF WS-CB-ST1 NOT = 0
              MOVE"CBMASTER BUSY ON READ, RD-015, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-ST1
              GO TO RD-015.
       RD-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-001.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE CB-NUMBER           TO CBTRANS-CBMASTER.
      *     MOVE GL-BEGDATE (1)      TO CBTRANS-DATE.
           MOVE 0                   TO CBTRANS-DATE.
           START CBTRANS-FILE KEY NOT < CBTRANS-ALT-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
               MOVE 88 TO WS-CBTRANS-ST1
               GO TO PRR-999.
       PRR-002.
           READ CBTRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CBTRANS-ST1 = 10
               MOVE 0 TO WS-CBTRANS-ST1
               GO TO PRR-900.
            IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CB-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO PRR-002.
            IF CBTRANS-CBMASTER < CB-NUMBER
               GO TO PRR-002.
            IF CBTRANS-CBMASTER > CB-NUMBER
               GO TO PRR-900.
           IF CBTRANS-FUTURE = "F"
               GO TO PRR-002.
           IF CBTRANS-ALLOCATED NOT = "N" AND NOT = "H"
               GO TO PRR-002.
           IF WS-PER NOT = 0
            IF WS-PER < CBTRANS-NO
             IF CBTRANS-DATE > GL-ENDDATE (WS-PER)
               GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           MOVE CBTRANS-PERIOD               TO D-PERIOD
           MOVE CBTRANS-TRANS                TO D-TRANS
           MOVE CBTRANS-REFERENCE            TO D-JRNNO
           MOVE WS-TRANS-DESC (CBTRANS-TYPE) TO D-TYPE
           MOVE CBTRANS-TYPE-OF-POST         TO D-TYPE-OF-POST
           MOVE CBTRANS-ALLOCATED            TO D-ALLOCATED
           MOVE CBTRANS-ACCOUNT-NUMBER       TO D-ACCNO
           MOVE CBTRANS-DATE                 TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE                 TO D-DATE.
           IF CBTRANS-ALLOCATED = "H" OR = "N"
               MOVE CBTRANS-AMOUNT           TO D-OS-AMT.
           MOVE CBTRANS-AMOUNT               TO D-AMOUNT
           MOVE CBTRANS-LINE-DESC            TO D-DESC.

           IF CBTRANS-NO NOT = WS-PER
             GO TO PRR-030.

           IF CBTRANS-TYPE-OF-POST = "D"
            IF CBTRANS-AMOUNT < 0
               ADD CBTRANS-AMOUNT TO WS-DR-AMT-CR
            ELSE
               ADD CBTRANS-AMOUNT TO WS-DR-AMT-DR.
           IF CBTRANS-TYPE-OF-POST = "C"
            IF CBTRANS-AMOUNT < 0
               ADD CBTRANS-AMOUNT TO WS-CR-AMT-CR
            ELSE
               ADD CBTRANS-AMOUNT TO WS-CR-AMT-DR.
           IF CBTRANS-TYPE-OF-POST = "S"
            IF CBTRANS-AMOUNT < 0
               ADD CBTRANS-AMOUNT TO WS-GL-AMT-CR
            ELSE
               ADD CBTRANS-AMOUNT TO WS-GL-AMT-DR.
       PRR-030.
           IF CBTRANS-ALLOCATED = "N"
            IF CBTRANS-AMOUNT < 0
               ADD CBTRANS-AMOUNT TO WS-OS-AMT-CR
            ELSE
               ADD CBTRANS-AMOUNT TO WS-OS-AMT-DR.
           IF CBTRANS-ALLOCATED = "H"
            IF CBTRANS-AMOUNT < 0
               ADD CBTRANS-AMOUNT TO WS-HD-AMT-CR
            ELSE
               ADD CBTRANS-AMOUNT TO WS-HD-AMT-DR.

           MOVE 2510 TO POS
           DISPLAY "Date Being Read:" AT POS
           ADD 18 TO POS
           DISPLAY DISPLAY-DATE AT POS.

           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           ADD 1 TO LINE-CNT WS-NO-OF-TRANS
           GO TO PRR-002.
       PRR-060.
            ADD 1                   TO PAGE-CNT
            MOVE PAGE-CNT           TO H1-PAGE
            MOVE GLPA-CURRENT-GLPER TO H1-PERIOD.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
            MOVE " "                TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " "                TO PRINT-REC
            MOVE CB-NUMBER          TO H3-ACC
            WRITE PRINT-REC FROM HEAD2 AFTER 2
            MOVE " "                TO PRINT-REC
            MOVE CB-DESCRIPTION     TO H3-NAME
            WRITE PRINT-REC FROM HEAD2-1 AFTER 1
            MOVE " " TO PRINT-REC
            MOVE "CURRENT BALANCE    :" TO H3-DESC
            MOVE CB-BALANCE             TO H3-BALANCE
            WRITE PRINT-REC FROM HEAD2-2 AFTER 1
            MOVE " "                    TO PRINT-REC
            MOVE "PERIOD OPEN BALANCE:" TO H3-DESC
            MOVE CB-OPEN-PER-BAL        TO H3-BALANCE
            WRITE PRINT-REC FROM HEAD2-2 AFTER 1
            MOVE " "                    TO PRINT-REC
            MOVE "YEAR OPEN BALANCE  :" TO H3-DESC
            MOVE CB-OPEN-YEAR-BAL       TO H3-BALANCE
            WRITE PRINT-REC FROM HEAD2-2 AFTER 1
            MOVE " "                    TO PRINT-REC
            MOVE "LAST YEAR BALANCE  :" TO H3-DESC
            MOVE CB-LY-OPEN-BAL         TO H3-BALANCE
            WRITE PRINT-REC FROM HEAD2-2 AFTER 1
            MOVE " "                    TO PRINT-REC.
            IF WS-PER = 0
                MOVE "ALL PERIODS"  TO H3-PERIOD-READ
            ELSE
                MOVE WS-PER         TO H3-PERIOD-READ.
            WRITE PRINT-REC FROM HEAD2-3 AFTER 1
            MOVE " "                TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " "                TO PRINT-REC
            WRITE PRINT-REC
            MOVE " "                TO PRINT-REC
            MOVE 11 TO LINE-CNT.
       PRR-900.
            MOVE " " TO PRINT-REC.
       PRR-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO GLPARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CB-MASTER.
            IF WS-CB-ST1 NOT = 0
              MOVE "CBMASTER BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-ST1
              GO TO OPEN-000.
       OPEN-005.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-005.
           PERFORM READ-PARAMETER.
           MOVE GLPA-NAME TO CO-NAME.
           PERFORM ENTER-PERIOD-DATES.
           PERFORM OPEN-006.
           CLOSE GLPARAMETER-FILE.
       OPEN-006.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           OPEN I-O CBTRANS-FILE.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO OPEN-010.
        OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            IF WS-CBTRANS-ST1 NOT = 88
               GO TO END-100.
            IF LINE-CNT > 60
               PERFORM PRR-060.
            MOVE
             "*** NO TRANSACTIONS FOUND FOR THIS ACCOUNT AND PERIOD.***"
               TO PRINT-REC
            WRITE PRINT-REC AFTER 2.
            GO TO END-800.
       END-100.
            IF LINE-CNT > 54
               PERFORM PRR-060.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE "DATA FOR THIS PERIOD."  TO TOT-DESC1
            WRITE PRINT-REC FROM TOTAL-LINE1
            MOVE " " TO PRINT-REC
            MOVE "TOTAL NUMBER OF TRANS:" TO TOT-DESC1
            MOVE WS-NO-OF-TRANS           TO TOT-AMOUNT1
            WRITE PRINT-REC FROM TOTAL-LINE1
            MOVE " " TO PRINT-REC
            IF WS-PER NOT = 0
               MOVE "TRANS ONLY FOR PERIOD:" TO TOT-DESC1
               MOVE WS-PER                   TO TOT-AMOUNT1
               WRITE PRINT-REC FROM TOTAL-LINE1
               MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.

            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE "OUT/STDG DEBIT-AMT   :" TO TOT-DESC
            MOVE WS-OS-AMT-DR             TO TOT-AMOUNT
            WRITE PRINT-REC FROM TOTAL-LINE
            MOVE "OUT/STDG CREDIT-AMT  :" TO TOT-DESC
            MOVE WS-OS-AMT-CR             TO TOT-AMOUNT
            WRITE PRINT-REC FROM TOTAL-LINE.

            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE "HELD TRANS DEBIT-AMT :" TO TOT-DESC
            MOVE WS-HD-AMT-DR             TO TOT-AMOUNT
            WRITE PRINT-REC FROM TOTAL-LINE
            MOVE "HELD TRANS CREDIT-AMT:" TO TOT-DESC
            MOVE WS-HD-AMT-CR             TO TOT-AMOUNT
            WRITE PRINT-REC FROM TOTAL-LINE.

            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            COMPUTE WS-AMOUNT-OS = CB-BALANCE -
                 (WS-OS-AMT-DR + WS-OS-AMT-CR).
            MOVE "BAL AS PER BANK STATE:" TO TOT-DESC
            MOVE WS-AMOUNT-OS                TO TOT-AMOUNT
            WRITE PRINT-REC FROM TOTAL-LINE.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-800.
            CLOSE PRINT-FILE.
       END-900.
            CLOSE CB-MASTER
                  CBTRANS-FILE.
            EXIT PROGRAM.
      *      STOP RUN.
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
       Copy "DisplayCBTopInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
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
       Copy "CTOSCobolAccept".
      * END-OF-JOB.
