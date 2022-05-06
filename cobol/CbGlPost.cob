        IDENTIFICATION DIVISION.
        PROGRAM-ID. CbGlPost.
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
        Copy "SelectGlMaster".
        Copy "SelectGlTrans".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCbMast.
           COPY ChlfdCbTrans.
           COPY ChlfdGlParam.
           COPY ChlfdGlMast.
           COPY ChlfdGlTrans.
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
       77  WS-RANGE3            PIC X(8) VALUE " ".
       77  WS-RANGE4            PIC X(1) VALUE " ".
       77  WS-TOT-AMOUNT        PIC S9(8)V99 VALUE 0.
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
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1      PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1     PIC 99.
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
       01  WS-BATCH.
           03  WS-BATCH-1STCHAR  PIC X(2) VALUE "CB".
           03  WS-BATCH-REST     PIC X(8).
       01  LINE-DESCRIPTION.
           03  WS-CR1              PIC X(15).
           03  WS-CRINV            PIC X(10).
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-RESTOFACCOUNT    PIC X(6).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(23) VALUE " ".
           03  FILLER         PIC X(63) VALUE
           "** CASHBOOK POSTING TO GENERAL LEDGER REPORT **".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(10).
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
           03  FILLER         PIC X(17) VALUE "  DEBIT".
           03  FILLER         PIC X(12) VALUE "CREDIT".
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
           03  D-DR-AMT       PIC Z(7)9.99- BLANK WHEN ZERO.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-CR-AMT       PIC Z(7)9.99- BLANK WHEN ZERO.
           03  FILLER         PIC X(5) VALUE " ".
           03  D-DESC         PIC X(25).
        01  TOTAL-LINE.
           03  TOT-DESC       PIC X(22).
           03  TOT-AMOUNT     PIC Z(7)9.99-.
           03  FILLER         PIC X(98) VALUE " ".
        01  TOTAL-LINE1.
           03  TOT-DESC1      PIC X(25).
           03  TOT-BATCH.
               05  TOT-AMOUNT1    PIC Z(4)9 BLANK WHEN ZERO.
               05  FILLER         PIC X(106) VALUE " ".
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
           DISPLAY "** CASHBOOK POSTING TO GENERAL LEDGER REPORT **"
             AT POS
           MOVE 408 TO POS
           DISPLAY "***********************************************"
              AT POS.
           IF GLPA-CURRENT-GLPER NOT = GLPA-CURRENT-CBPER
               MOVE 
               "GL PERIOD SHOULD = CASHBOOK PERIOD, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
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
                MOVE 2801 TO POS
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-001.
            MOVE 1110 TO POS
            DISPLAY "CashBook Acc Name :" AT POS
            ADD 20 TO POS
            DISPLAY CB-DESCRIPTION AT POS.
       GET-010.
            MOVE GLPA-CURRENT-CBPER TO WS-RANGE2.
            MOVE 1310 TO POS.
            DISPLAY "Enter the PERIOD to print for: [  ]" AT POS.
            MOVE 1342 TO POS.

           MOVE WS-RANGE2 TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

            IF W-ESCAPE-KEY = 4
                GO TO GET-001.
            IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
            MOVE WS-RANGE2 TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-PER
            MOVE 1342 TO POS
            DISPLAY WS-PER AT POS.
            IF WS-PER NOT = GLPA-CURRENT-CBPER
               MOVE 
               "POSTING PERIOD SHOULD = CASHBOOK PERIOD, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-020
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
       GET-020.
            MOVE "Y" TO WS-RANGE4.
            MOVE 1810 TO POS
            DISPLAY "Enter 'N'=Print Only, 'Y'=Print & Post All Trans."
                AT POS
            ADD 51 TO POS
            DISPLAY ": [ ]" AT POS
            ADD 3 TO POS.

           MOVE WS-RANGE4 TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 63        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE4.

            IF W-ESCAPE-KEY = 4
                GO TO GET-010.
            IF WS-RANGE4 NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-025
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
       GET-025.
            MOVE 2010 TO POS
            DISPLAY "Enter The BATCH Name for this run: [CB        ]"
                AT POS
            ADD 38 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 8         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE3.

            IF W-ESCAPE-KEY = 4
                GO TO GET-020.
            IF WS-RANGE3 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-025.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-030
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-025.
       GET-030.
            MOVE WS-RANGE3 TO WS-BATCH-REST.
            IF WS-RANGE4 = "Y"
             IF GLPA-CB-POST = "Y"
              MOVE "C/BOOK ENTRIES ALREADY POSTED TO G/LEDGER, RE-ENTER"
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO GET-020.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
            MOVE 2910 TO POS.
            DISPLAY "The Report Is Being Compiled.........." AT POS.
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
               MOVE"CBMASTER BUSY ON READ, RD-015 'ESC' TO RETRY."
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
       REWRITE-CBTRANS SECTION.
       WCBT-010.
           MOVE "G" TO CBTRANS-TYPE-OF-POST.
       WCBT-018.
           REWRITE CBTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO WCBT-018.
       WCBT-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-001.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE CB-NUMBER           TO CBTRANS-CBMASTER.
           MOVE GL-BEGDATE (WS-PER) TO CBTRANS-DATE.
      *     MOVE GL-BEGDATE (1)      TO CBTRANS-DATE.
           START CBTRANS-FILE KEY NOT < CBTRANS-ALT-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
               MOVE 88 TO WS-CBTRANS-ST1
               GO TO PRR-999.
       PRR-002.
           READ CBTRANS-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-CBTRANS-ST1 = 10
               MOVE 0 TO WS-CBTRANS-ST1
               GO TO PRR-900.
            IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CB-TRANS BUSY ON READ-NEXT, 'ESC' TO RETRY."
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

      *     IF CBTRANS-NO NOT = WS-PER
      *         GO TO PRR-002.
           IF CBTRANS-NO > WS-PER
               GO TO PRR-900.

           IF CBTRANS-ALLOCATED = "H"
               GO TO PRR-002.
           IF CBTRANS-TYPE-OF-POST NOT = "S"
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
           IF CBTRANS-AMOUNT < 0
               ADD CBTRANS-AMOUNT            TO WS-GL-AMT-CR
               MOVE CBTRANS-AMOUNT           TO D-CR-AMT
           ELSE
               ADD CBTRANS-AMOUNT            TO WS-GL-AMT-DR
               MOVE CBTRANS-AMOUNT           TO D-DR-AMT.
           MOVE CBTRANS-LINE-DESC            TO D-DESC.


           MOVE 2510 TO POS
           DISPLAY "Date Being Read:" AT POS
           ADD 18 TO POS
           DISPLAY DISPLAY-DATE AT POS.

           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           ADD 1 TO LINE-CNT WS-NO-OF-TRANS.

           IF WS-RANGE4 = "N"
              GO TO PRR-002.

           PERFORM WRITE-GLTRANS.
           PERFORM REWRITE-CBTRANS.
           MOVE GLPA-CURRENT-CBPER TO SUB-3.
           IF GLPA-CURRENT-CBPER = GLPA-CURRENT-GLPER
               PERFORM UPDATE-GLMASTER
               PERFORM UPDATE-GLHEADER
               PERFORM UPDATE-GLSUBHEADER.
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
            IF WS-RANGE4 = "N"
               GO TO PRR-999.
           PERFORM WRITE-CB-GLTRANS.
           IF GLPA-CURRENT-CBPER = GLPA-CURRENT-GLPER
               PERFORM UPDATE-GLMASTER
               PERFORM UPDATE-GLHEADER
               PERFORM UPDATE-GLSUBHEADER.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE "Y" TO GLPA-CB-POST.
           PERFORM REWRITE-GLPARAMETER.
       PRR-999.
           EXIT.
      *
       WRITE-GLTRANS SECTION.
       WRTR-010.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO         TO GLTRANS-TRANS
           ADD 1                       TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE CBTRANS-REFERENCE      TO GLTRANS-REFERENCE
           MOVE 1                      TO GLTRANS-TYPE.
           IF GLPA-CURRENT-GLPER = GLPA-CURRENT-CBPER
               MOVE " "                TO GLTRANS-FUTURE
           ELSE
               MOVE "F"                TO GLTRANS-FUTURE.
           MOVE CBTRANS-NO             TO GLTRANS-NO
           MOVE CBTRANS-DATE           TO GLTRANS-DATE
           MOVE CBTRANS-ACCOUNT-NUMBER TO GLTRANS-ACCOUNT-NUMBER
           MOVE CBTRANS-AMOUNT         TO GLTRANS-AMOUNT
           COMPUTE GLTRANS-AMOUNT = GLTRANS-AMOUNT * -1
           MOVE CBTRANS-LINE-DESC      TO GLTRANS-LINE-DESC.
       WRTR-015.
           WRITE GLTRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
              MOVE "GLTRANS WRITE ERC23, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO WRTR-015.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GLTRANS FILE BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO WRTR-015.
       WRTR-999.
           EXIT.
      *
       WRITE-CB-GLTRANS SECTION.
       WRCBTR-010.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO         TO GLTRANS-TRANS
           ADD 1                       TO GLPA-GLTRANSNO
           PERFORM REWRITE-GLPARAMETER
           MOVE WS-BATCH               TO GLTRANS-REFERENCE
           MOVE 1                      TO GLTRANS-TYPE.
           IF GLPA-CURRENT-GLPER = GLPA-CURRENT-CBPER
               MOVE " "                TO GLTRANS-FUTURE
           ELSE
               MOVE "F"                TO GLTRANS-FUTURE.
           MOVE GLPA-CURRENT-CBPER     TO GLTRANS-NO
           MOVE GL-ENDDATE (SUB-3)     TO GLTRANS-DATE
           MOVE WS-RANGE1              TO GLTRANS-ACCOUNT-NUMBER
           COMPUTE GLTRANS-AMOUNT = WS-GL-AMT-DR + WS-GL-AMT-CR
           MOVE "C/B BATCH POST "      TO WS-CR1
           MOVE WS-BATCH               TO WS-CRINV
           MOVE LINE-DESCRIPTION       TO GLTRANS-LINE-DESC
           PERFORM WRTR-015.
       WRCBTR-999.
           EXIT.
      *
       UPDATE-GLMASTER SECTION.
       UPGL-005.
           MOVE GLTRANS-ACCOUNT-NUMBER TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGL-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
              MOVE "GLMASTER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGL-010.
       UPGL-020.
           ADD GLTRANS-AMOUNT TO GL-BALANCE
                                 GL-PER (SUB-3).
       UPGL-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGL-900.
       UPGL-999.
           EXIT.
     *
       UPDATE-GLHEADER SECTION.
       UPGLH-005.
           MOVE GLTRANS-ACCOUNT-NUMBER TO WS-GLNUMBER.
           MOVE WS-GLHEADER            TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
              MOVE "GLHEADER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLHEADER FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGLH-010.
       UPGLH-020.
           ADD GLTRANS-AMOUNT TO GL-BALANCE
                                 GL-PER (SUB-3).
       UPGLH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLHEADER RECORD BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGLH-900.
       UPGLH-999.
           EXIT.
     *
       UPDATE-GLSUBHEADER SECTION.
       UPGLSH-005.
           MOVE GLTRANS-ACCOUNT-NUMBER TO WS-GLNUMBER.
           MOVE WS-HEAD-SUB            TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLSH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
              MOVE "GLSUBHEAD FILE DOES'NT EXIST, CALL YOUR SUPERVISOR"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLSUBHEADER FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGLSH-010.
       UPGLSH-020.
           ADD GLTRANS-AMOUNT TO GL-BALANCE
                                 GL-PER (SUB-3).
       UPGLSH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLSUBHEAD FILE BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGLSH-900.
       UPGLSH-999.
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
              MOVE "GLPARAMETER BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       READ-GLPARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 1 TO GLPA-RECORD.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RPL-010.
           READ GLPARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RPL-010.
       RPL-999.
           EXIT.
      *
       REWRITE-GLPARAMETER SECTION.
       REWP-000.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-000.
       REWP-999.
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
              MOVE "GLPARAMETER FILE BUSY ON OPEN,  'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-005.
            PERFORM READ-PARAMETER
            MOVE GLPA-NAME TO CO-NAME
            PERFORM ENTER-PERIOD-DATES.
            PERFORM OPEN-006.
       OPEN-006.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           OPEN I-O CBTRANS-FILE.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO OPEN-010.
       OPEN-015.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO OPEN-015.
       OPEN-020.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0 
              MOVE "GL-TRANS BUSY ON OPEN, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO OPEN-020.
        OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            IF WS-CBTRANS-ST1 NOT = 88
               GO TO END-100.
            IF LINE-CNT > 58
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
            MOVE "DATA FOR THIS PERIOD." TO TOT-DESC1
            WRITE PRINT-REC FROM TOTAL-LINE1
            MOVE " " TO PRINT-REC
            MOVE "TOTAL NUMBER OF TRANS:" TO TOT-DESC1
            MOVE WS-NO-OF-TRANS           TO TOT-AMOUNT1
            WRITE PRINT-REC FROM TOTAL-LINE1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC.

            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE "G/LEDGER DEBIT-AMT   :" TO TOT-DESC
            MOVE WS-GL-AMT-DR             TO TOT-AMOUNT
            WRITE PRINT-REC FROM TOTAL-LINE
            MOVE "G/LEDGER CREDIT-AMT  :" TO TOT-DESC
            MOVE WS-GL-AMT-CR             TO TOT-AMOUNT
            WRITE PRINT-REC FROM TOTAL-LINE.

            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            COMPUTE WS-TOT-AMOUNT = (WS-GL-AMT-DR + WS-GL-AMT-CR).
            MOVE "AMT TO POST TO GLBANK:" TO TOT-DESC
            MOVE WS-TOT-AMOUNT            TO TOT-AMOUNT
            WRITE PRINT-REC FROM TOTAL-LINE.
            MOVE " " TO PRINT-REC TOTAL-LINE.

            MOVE "BATCH NAME FOR RUN   :" TO TOT-DESC1
            MOVE WS-BATCH                 TO TOT-BATCH
            WRITE PRINT-REC FROM TOTAL-LINE1.
            MOVE " " TO PRINT-REC TOTAL-LINE.
            
            IF WS-RANGE4 = "Y"
              MOVE "AMOUNTS POSTED TO G/LEDGER" TO PRINT-REC
            ELSE
              MOVE "AMOUNTS NOT POSTED TO G/LEDGER" TO PRINT-REC.
           WRITE PRINT-REC AFTER 2.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-800.
            CLOSE PRINT-FILE.
       END-900.
            CLOSE CB-MASTER
                  CBTRANS-FILE
                  GL-MASTER
                  GLTRANS-FILE
                  GLPARAMETER-FILE.
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
