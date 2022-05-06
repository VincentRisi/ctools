        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrAgeARp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrMaster".
          Copy "SelectCrTrans".
          Copy "SelectGlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCrTrans.
           COPY ChlfdGlParam.
           COPY ChlfdCreditor.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-RANGE1            PIC 9(7) VALUE 0.
       77  WS-RANGE2            PIC 9(7) VALUE 0.
       77  WS-CRKEY-SAVE        PIC X(7) VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-AMT-OF-INVOICE    PIC S9(8)V99 VALUE 0.
       77  WS-TOT-BALANCE       PIC S9(8)V99 VALUE 0.
       77  WS-TOT-CURRENT       PIC S9(8)V99 VALUE 0.
       77  WS-TOT-30DAY         PIC S9(8)V99 VALUE 0.
       77  WS-TOT-60DAY         PIC S9(8)V99 VALUE 0.
       77  WS-TOT-90DAY         PIC S9(8)V99 VALUE 0.
       77  WS-TOT-120DAY        PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-BALANCE     PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-CURRENT     PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-30DAY       PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-60DAY       PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-90DAY       PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-120DAY      PIC S9(8)V99 VALUE 0.
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       77  WS-SUMMARY           PIC X VALUE " ".
       77  WS-FOR-LOC           PIC X VALUE " ".
       77  WS-TOTAL-OWED         PIC S9(8)V99.
       77  WS-TOTAL-LY          PIC S9(8)V99.
       01  WS-CURRENCY-FIELD-NAMES.
         02  WS-CURRENCY-FIELDS OCCURS 50.
           03  WS-CURRENCY      PIC X(8).
           03  WS-BAL-OWED      PIC S9(8)V99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1     PIC 99.
       01  WS-CRTRANS-STATUS.
           03  WS-CRTRANS-ST1      PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-TYPES.
           03  FILLER           PIC X(7) VALUE "INVOICE".
           03  FILLER           PIC X(7) VALUE "PAYMENT".
           03  FILLER           PIC X(7) VALUE "PAYMENT".
           03  FILLER           PIC X(7) VALUE "JRN.DR.".
           03  FILLER           PIC X(7) VALUE "JRN.CR.".
           03  FILLER           PIC X(7) VALUE "C/NOTE ".
           03  FILLER           PIC X(7) VALUE "INTREST".
           03  FILLER           PIC X(7) VALUE "DISCNT.".
           03  FILLER           PIC X(7) VALUE "FOREX  ".
       01  WS-TYPES-RED REDEFINES WS-TYPES.
           03  WS-TYPE-DESC     PIC X(7) OCCURS 9.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE " DATE:".
           03  H-DATE           PIC X(10).
           03  FILLER           PIC X(22) VALUE " ".
           03  FILLER           PIC X(50) VALUE 
           "C R E D I T O R S   A G E   A N A L Y S I S".
           03  FILLER           PIC X(8) VALUE "PERIOD:".
           03  H-PERIOD         PIC X(20).
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(7) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(39) VALUE " ".
           03  FILLER           PIC X(93) VALUE 
           "*******************************************".
       01  HEAD3.
           03  FILLER           PIC X(40) VALUE " ACCOUNT  NAME".
           03  FILLER           PIC X(92) VALUE "DATE     ACCOUNT".
       01  HEAD4.
           03  FILLER           PIC X(37) VALUE 
           " NUMBER   TELEPHONE No".
           03  FILLER           PIC X(24) VALUE "LAST PAID   BALANCE".
           03  FILLER           PIC X(13) VALUE " CURRENT".
           03  FILLER           PIC X(14) VALUE "31-60 DAY".
           03  FILLER           PIC X(13) VALUE "61-90 DAY".
           03  FILLER           PIC X(16) VALUE "91-120 DAY".
           03  FILLER           PIC X(11) VALUE "121+ DAY".
       01  HEAD5.
           03  FILLER           PIC X(18) VALUE " ".
           03  FILLER           PIC X(111) VALUE 
           "TYPE   REF.NO.     INV DATE   DUE DATE".
       01  CREDITOR-LINE1.
           03  FILLER           PIC X VALUE " ".
           03  DEBT-ACCNO       PIC 9(7).
           03  FILLER           PIC X(2) VALUE " ".
           03  DEBT-NAME        PIC X(45) VALUE " ".
           03  FILLER           PIC X(77) VALUE " ".
       01  CREDITOR-LINE2.
           03  FILLER           PIC X(10) VALUE " ".
           03  DEBT-TELEPHONE   PIC X(26) VALUE " ".
           03  DEBT-PAID-DATE   PIC X(10).
           03  FILLER           PIC X VALUE " ".
           03  DEBT-BALANCE     PIC Z(6)9.99-.
           03  FILLER           PIC X(75) VALUE " ".
       01  TRANS-LINE.
           03  FILLER           PIC X(16) VALUE " ".
           03  TRANS-TYPE       PIC X(7) VALUE " ".
           03  FILLER           PIC X(2) VALUE " ".
           03  TRANS-REFNO      PIC X(11).
           03  TRANS-DATE       PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  TRANS-DUEDATE    PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  TRANS-CURRENT    PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  TRANS-30DAY      PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  TRANS-60DAY      PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  TRANS-90DAY      PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  TRANS-120DAY     PIC Z(7)9.99-.
           03  FILLER           PIC X(6) VALUE " ".
       01  P-UNDERLINE.
           03  FILLER           PIC X(58) VALUE " ".
           03  FILLER           PIC X(14) VALUE "-----------".
           03  FILLER           PIC X(14) VALUE "-----------".
           03  FILLER           PIC X(14) VALUE "-----------".
           03  FILLER           PIC X(14) VALUE "-----------".
           03  FILLER           PIC X(14) VALUE "-----------".
           03  FILLER           PIC X(4) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER           PIC X(31) VALUE " ".
           03  TOT-DESC         PIC X(14) VALUE " ".
           03  TOT-BALANCE      PIC Z(7)9.99-.
           03  FILLER           PIC X(1) VALUE " ".
           03  TOT-CURRENT      PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  TOT-30DAY        PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  TOT-60DAY        PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  TOT-90DAY        PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  TOT-120DAY       PIC Z(7)9.99-.
           03  FILLER           PIC X(6) VALUE " ".
       01  GRAND-TOTAL-LINE.
           03  FILLER           PIC X(25) VALUE " ".
           03  FILLER           PIC X(20) VALUE "*** GRAND TOTAL ***".
           03  GR-TOT-BALANCE   PIC Z(7)9.99-.
           03  FILLER           PIC X(1) VALUE " ".
           03  GR-TOT-CURRENT   PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  GR-TOT-30DAY     PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  GR-TOT-60DAY     PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  GR-TOT-90DAY     PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  GR-TOT-120DAY    PIC Z(7)9.99-.
           03  FILLER           PIC X(8) VALUE " ".
       01  WS-TEMP-LINE.
           03  FILLER           PIC X(5) VALUE " ".
           03  T-PERIOD         PIC X(10) VALUE " ".
           03  T-TRANS-DATE     PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
           03  T-TRANS-DUEDATE  PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
       01  SUMMARY-HEADING.
           03  FILLER           PIC X(40) VALUE
            "CURRENCY  BALANCE OWED".
       01  SUMMARY-LINE.
           03  S-CURRENCY       PIC X(11) VALUE " ".
           03  S-BAL-OWED       PIC Z(7)9.99-.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "**Creditors Age Analysis Report **" AT POS
           MOVE 415 TO POS
           DISPLAY "**********************************" AT POS
           PERFORM ERROR-020.
       CONTROL-003.
           Copy "PrinterAcceptCr".
       CONTROL-010.
           MOVE "       " TO WS-RANGE1.
           MOVE 1010 TO POS.
           DISPLAY "           FROM ACCOUNT NUMBER: [       ]" AT POS.
           MOVE 1043 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

           MOVE 1043 TO POS.
           DISPLAY WS-RANGE1 AT POS.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-015.
           MOVE " " TO WS-RANGE2.
           MOVE 1210 TO POS.
           DISPLAY "             TO ACCOUNT NUMBER: [       ]" AT POS.
           MOVE 1243 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           MOVE 1243 TO POS.
           DISPLAY WS-RANGE2 AT POS.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
           MOVE 1410 TO POS.
           DISPLAY "ENTER: A=ALL, D=DEBIT C=CREDIT: [ ]" AT POS.
           MOVE 1443 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF WS-ANSWER = "A" OR = "D" OR = "C"
               GO TO CONTROL-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-025.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-030.
           MOVE 2610 TO POS.
           DISPLAY "The Report Is Being compiled, Please Be Patient"
               AT POS.
       CONTROL-040.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           PERFORM SUB-015.
           
           MOVE " "       TO TRANS-LINE.
           MOVE WS-RANGE1 TO CRTR-ACC-NUMBER.
           MOVE 0         TO CRTR-DATE CR-ACCOUNT-NUMBER.
           START CRTR-FILE KEY NOT < CRTR-ACC-DATE
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 NOT = 0 AND NOT = 23
               MOVE "CR-TRANS FILE ERROR IN START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRTRANS-ST1
                         CR-BALANCE
                         CRTR-UNAPPLIED-AMT
               PERFORM SUBTOTALS
               GO TO PR-999.
       PR-002.
           READ CRTR-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 10 OR = 23
               PERFORM SUBTOTALS
               GO TO PR-999.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE "CR-TRANS BUSY ON READ, IN 1 SECOND GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE 0 TO WS-CRTRANS-ST1
               CALL "C$SLEEP" USING 1
               GO TO PR-002.
           IF WS-MESSAGE NOT = " "
               MOVE " " TO WS-MESSAGE
               PERFORM ERROR1-020
               PERFORM ERROR-020.
           IF CRTR-ACC-NUMBER < WS-RANGE1
               GO TO PR-002.
           IF CRTR-ACC-NUMBER > WS-RANGE2
               PERFORM SUBTOTALS
               GO TO PR-999.
               
           IF CRTR-UNAPPLIED-AMT = 0
               GO TO PR-002.
           IF CRTR-ACC-NUMBER NOT > 0
               GO TO PR-002.
           IF CRTR-ACC-NUMBER NOT > " "
               GO TO PR-002.

           IF WS-ANSWER = "A"
               GO TO PR-003.
           IF WS-ANSWER = "D"
            IF CRTR-UNAPPLIED-AMT < 0
               GO TO PR-002.
           IF WS-ANSWER = "C"
            IF CRTR-UNAPPLIED-AMT > 0
               GO TO PR-002.
       PR-003.
           IF CRTR-ACC-NUMBER = CR-ACCOUNT-NUMBER
               GO TO PR-020.
           IF CR-ACCOUNT-NUMBER > 0
               PERFORM SUBTOTALS.
       PR-005.
           MOVE CRTR-ACC-NUMBER TO CR-ACCOUNT-NUMBER.
           READ CREDITOR-MASTER 
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
               MOVE "UNKNOWN CREDITOR" TO CR-NAME
               GO TO PR-010.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITORS BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PR-005.
           
           MOVE 2510 TO POS
           DISPLAY "Account Number Being read:" at POS
           ADD 27 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
               
           IF WS-ANSWER = "A"
               GO TO PR-010.
           IF WS-ANSWER = "D"
               AND CR-BALANCE > 0
               GO TO PR-010.
           IF WS-ANSWER = "C"
               AND CR-BALANCE < 0
               GO TO PR-010.
           IF WS-MESSAGE NOT = " "
               MOVE " " TO WS-MESSAGE
               PERFORM ERROR-020.
           GO TO PR-002.
       PR-010.
           IF CR-CURRENCY = " "
              MOVE "RAND" TO CR-CURRENCY.
           PERFORM PR-031 THRU PR-035.
           IF WS-LINE > 56
               PERFORM PRINT-HEADINGS.
           MOVE CR-ACCOUNT-NUMBER TO DEBT-ACCNO
           MOVE CR-NAME           TO DEBT-NAME
           WRITE PRINT-REC FROM CREDITOR-LINE1 AFTER 1
           MOVE " "               TO CREDITOR-LINE1
           MOVE CR-TELEPHONE      TO DEBT-TELEPHONE
           
           MOVE CR-DATE-LAST-PAY  TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO DEBT-PAID-DATE
           
           MOVE CR-BALANCE        TO DEBT-BALANCE
           WRITE PRINT-REC FROM CREDITOR-LINE2 AFTER 1
           MOVE " "               TO CREDITOR-LINE2
           WRITE PRINT-REC FROM HEAD5 AFTER 1
           ADD 3 TO WS-LINE.
       PR-020.
           MOVE WS-TYPE-DESC (CRTR-TYPE) TO TRANS-TYPE
           MOVE CRTR-INV-NO              TO TRANS-REFNO
           
           MOVE CRTR-DATE                TO WS-AGE-DATE  SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE             TO TRANS-DATE
           PERFORM COMPUTE-DATE-PERIOD.
           
      * USED TO CHECK DATE OF TRANS & PERIOD - PROGRAM DEBUGGING
      *     MOVE WS-CALC-PERIOD TO T-PERIOD
      *     MOVE CRTR-DATE      TO T-TRANS-DATE
      *     MOVE WS-TEMP-LINE   TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
           MOVE CRTR-DUE-DATE            TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE             TO TRANS-DUEDATE
           MOVE CRTR-UNAPPLIED-AMT       TO WS-AMT-OF-INVOICE.

           IF WS-CALC-PERIOD = 0
               ADD WS-AMT-OF-INVOICE TO WS-TOT-CURRENT
                                        WS-GRTOT-CURRENT
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-CURRENT
               GO TO PR-030.
           IF WS-CALC-PERIOD = 1
               ADD WS-AMT-OF-INVOICE TO WS-TOT-30DAY
                                        WS-GRTOT-30DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-30DAY
               GO TO PR-030.
           IF WS-CALC-PERIOD = 2
               ADD WS-AMT-OF-INVOICE TO WS-TOT-60DAY
                                        WS-GRTOT-60DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-60DAY
               GO TO PR-030.
           IF WS-CALC-PERIOD = 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-90DAY
                                        WS-GRTOT-90DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-90DAY
               GO TO PR-030.
           IF WS-CALC-PERIOD > 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-120DAY
                                        WS-GRTOT-120DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-120DAY
               GO TO PR-030.
       PR-030.
           IF WS-LINE > 60
               PERFORM PRINT-HEADINGS.
           WRITE PRINT-REC FROM TRANS-LINE AFTER 1.
           MOVE " " TO TRANS-LINE.
           ADD 1 TO WS-LINE.
           
           GO TO PR-002.
       PR-031.
           MOVE 0 TO SUB-1.
       PR-035.
           IF SUB-1 < 50
              ADD 1 TO SUB-1.
           IF CR-CURRENCY NOT = WS-CURRENCY (SUB-1)
            IF WS-CURRENCY (SUB-1) NOT = " "
              GO TO PR-035.
           IF WS-CURRENCY (SUB-1) = " "
              MOVE CR-CURRENCY  TO WS-CURRENCY (SUB-1).
           ADD CR-BALANCE       TO WS-BAL-OWED (SUB-1).
       PR-999.
           EXIT.
      *
       SUBTOTALS SECTION.
       SUB-000.
           IF WS-LINE > 60
               PERFORM PRINT-HEADINGS.

           IF CR-ACCOUNT-NUMBER NOT > 0
               GO TO SUB-015.

           WRITE PRINT-REC FROM P-UNDERLINE AFTER 1
           MOVE "ACCOUNT TOTAL:" TO TOT-DESC
           MOVE WS-TOT-BALANCE   TO TOT-BALANCE
           MOVE WS-TOT-CURRENT   TO TOT-CURRENT
           MOVE WS-TOT-30DAY     TO TOT-30DAY
           MOVE WS-TOT-60DAY     TO TOT-60DAY
           MOVE WS-TOT-90DAY     TO TOT-90DAY
           MOVE WS-TOT-120DAY    TO TOT-120DAY
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           IF CRTR-ACC-NUMBER < " "
               GO TO SUB-010.
           IF CRTR-ACC-NUMBER < 0
               GO TO SUB-010.
           IF CR-ACCOUNT-NUMBER > 0
            IF WS-TOT-BALANCE NOT = CR-BALANCE
              MOVE "************ ACCOUNT IN IM-BALANCE **" TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              ADD 1 TO WS-LINE.
       SUB-010.
           MOVE " " TO TOTAL-LINE PRINT-REC
           WRITE PRINT-REC AFTER 1
           ADD 3 TO WS-LINE.
       SUB-015.
           MOVE 0 TO WS-TOT-BALANCE
                     WS-TOT-CURRENT
                     WS-TOT-30DAY
                     WS-TOT-60DAY
                     WS-TOT-90DAY
                     WS-TOT-120DAY.
       SUB-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           ADD 1        TO WS-PAGE
           MOVE WS-PAGE TO H-PAGE
           MOVE GLPA-CURRENT-CRPER TO H-PERIOD
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF WS-PAGE = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           WRITE PRINT-REC FROM HEAD3 AFTER 2
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE 7 TO WS-LINE.
       PH-999.
           EXIT.
      *
       GRAND-TOTALS SECTION.
       GT-000.
           IF WS-LINE > 60
               PERFORM PRINT-HEADINGS.
           MOVE WS-GRTOT-BALANCE TO GR-TOT-BALANCE
           MOVE WS-GRTOT-CURRENT TO GR-TOT-CURRENT
           MOVE WS-GRTOT-30DAY   TO GR-TOT-30DAY
           MOVE WS-GRTOT-60DAY   TO GR-TOT-60DAY
           MOVE WS-GRTOT-90DAY   TO GR-TOT-90DAY
           MOVE WS-GRTOT-120DAY  TO GR-TOT-120DAY
           WRITE PRINT-REC FROM GRAND-TOTAL-LINE AFTER 1.
       GT-999.
           EXIT.
      *
       READ-GLPARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RP-010.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER BUSY ON READ, RP-010, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RP-010.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE      TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H-DATE.
       OPEN-010.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-012.

           PERFORM READ-GLPARAMETER
           MOVE GLPA-NAME TO CO-NAME
           PERFORM ENTER-PERIOD-DATES.
       OPEN-020.
           OPEN I-O CREDITOR-MASTER.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CR MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-020.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
       OPEN-030.
           OPEN I-O CRTR-FILE.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CRTRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-030.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           PERFORM GRAND-TOTALS.
       END-010.
           IF WS-LINE > 45
               PERFORM PRINT-HEADINGS.
           MOVE "** SUMMARY OF CURRENCIES IN RAND AMOUNTS **"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 2.
           WRITE PRINT-REC FROM SUMMARY-HEADING AFTER 2.
           MOVE 0 TO SUB-1.
       END-020.
           IF SUB-1 < 50 
              ADD 1 TO SUB-1.
           IF WS-CURRENCY (SUB-1) = " "
               GO TO END-800.
           MOVE WS-CURRENCY (SUB-1)  TO S-CURRENCY
           MOVE WS-BAL-OWED (SUB-1)  TO S-BAL-OWED
           WRITE PRINT-REC FROM SUMMARY-LINE AFTER 1
           ADD WS-BAL-OWED (SUB-1)   TO WS-TOTAL-OWED
           GO TO END-020.
       END-800.
           MOVE "TOTAL:"      TO S-CURRENCY
           MOVE WS-TOTAL-OWED TO S-BAL-OWED
           WRITE PRINT-REC FROM SUMMARY-LINE AFTER 2.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-850.
           CLOSE PRINT-FILE
                 CRTR-FILE
                 GLPARAMETER-FILE
                 CREDITOR-MASTER.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ComputeCRDatePeriod".
       Copy "EnterPeriodDates".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
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
      * END-OF-JOB
