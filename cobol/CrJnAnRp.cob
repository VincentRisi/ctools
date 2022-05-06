        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrJnAnRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrMaster".
          Copy "SelectCrJrn".
          Copy "SelectGlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCrJrn.
           COPY ChlfdGlParam.
           COPY ChlfdCreditor.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-RANGE1            PIC 9(7) VALUE 0.
       77  WS-RANGE2            PIC 9(7) VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-DATEANSWER        PIC X(10) VALUE " ".
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
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1     PIC 99.
       01  WS-CRJRN-STATUS.
           03  WS-CRJRN-ST1        PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-CHECKDATE.
           03  WS-CHECKYY      PIC 9999.
           03  WS-CHECKMM      PIC 99.
           03  WS-CHECKDD      PIC 99.
       01  WS-TYPES.
           03  FILLER           PIC X(3) VALUE "INV".
           03  FILLER           PIC X(3) VALUE "PAY".
           03  FILLER           PIC X(3) VALUE "PAY".
           03  FILLER           PIC X(3) VALUE "DR.".
           03  FILLER           PIC X(3) VALUE "CR.".
           03  FILLER           PIC X(3) VALUE "C/N".
           03  FILLER           PIC X(3) VALUE "INT".
           03  FILLER           PIC X(3) VALUE "DIS".
           03  FILLER           PIC X(3) VALUE "FRX".
       01  WS-TYPES-RED REDEFINES WS-TYPES.
           03  WS-TYPE-DESC     PIC X(3) OCCURS 9.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE " DATE:".
           03  H-DATE           PIC X(10).
           03  FILLER           PIC X(22) VALUE " ".
           03  FILLER           PIC X(50) VALUE 
           "CREDITORS OUTSTANDING JOURNAL PRINT-OUT".
           03  FILLER           PIC X(8) VALUE "PERIOD:".
           03  H-PERIOD         PIC X(20).
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(7) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(39) VALUE " ".
           03  FILLER           PIC X(93) VALUE 
           "***************************************".
       01  HEAD3.
           03  FILLER           PIC X(37) VALUE "ACCOUNT  NAME".
           03  FILLER           PIC X(95) VALUE "DATE     ACCOUNT".
       01  HEAD4.
           03  FILLER           PIC X(34) VALUE "NUMBER   TELEPHONE No".
           03  FILLER           PIC X(27) VALUE "LAST PAID   BALANCE".
           03  FILLER           PIC X(13) VALUE " CURRENT".
           03  FILLER           PIC X(14) VALUE "31-60 DAY".
           03  FILLER           PIC X(13) VALUE "61-90 DAY".
           03  FILLER           PIC X(16) VALUE "91-120 DAY".
           03  FILLER           PIC X(11) VALUE "121+ DAY".
       01  HEAD5.
           03  FILLER           PIC X(23) VALUE
           "REFERENCE  TRANS# TYPE".
           03  FILLER           PIC X(109) VALUE 
           "C REF.NO.   INV DATE   DUE DATE".
       01  CREDITOR-LINE1.
           03  FILLER           PIC X VALUE " ".
           03  DEBT-ACCNO       PIC 9(7).
           03  FILLER           PIC X(2) VALUE " ".
           03  DEBT-NAME        PIC X(45) VALUE " ".
           03  FILLER           PIC X(77) VALUE " ".
       01  CREDITOR-LINE2.
           03  FILLER           PIC X(10) VALUE " ".
           03  DEBT-TELEPHONE   PIC X(24) VALUE " ".
           03  DEBT-PAID-DATE   PIC X(10).
      *     03  FILLER           PIC X(1) VALUE " ".
           03  DEBT-BALANCE     PIC Z(5)9.99-.
           03  FILLER           PIC X(77) VALUE " ".
       01  TRANS-LINE.
           03  TRANS-REFERENCE  PIC X(11).
           03  TRANS-TRANS      PIC X(8).
           03  TRANS-TYPE       PIC X(4).
           03  TRANS-COMPLETE   PIC X(2).
           03  TRANS-REFNO      PIC X(9).
           03  TRANS-DATE       PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  TRANS-DUEDATE    PIC X(10).
           03  FILLER           PIC X(3) VALUE " ".
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
           03  FILLER           PIC X(28) VALUE " ".
           03  TOT-DESC         PIC X(16) VALUE " ".
           03  TOT-BALANCE      PIC Z(5)9.99-.
           03  FILLER           PIC X(4) VALUE " ".
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
           03  FILLER           PIC X(22) VALUE " ".
           03  FILLER           PIC X(20) VALUE "*** GRAND TOTAL ***".
           03  GR-TOT-BALANCE   PIC Z(7)9.99-.
           03  FILLER           PIC X(4) VALUE " ".
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
           03  FILLER           PIC X(10) VALUE " ".
           03  T-TRANS-DATE     PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
           03  T-TRANS-DUEDATE  PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
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
           DISPLAY "**Creditors OutStanding Journal Print-Out**" AT POS
           MOVE 415 TO POS
           DISPLAY "*******************************************" AT POS
           PERFORM ERROR-020.
       CONTROL-003.
           Copy "PrinterAcceptCr".
       CONTROL-010.
           MOVE " " TO WS-RANGE1.
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
           MOVE 11         TO CDA-ROW.
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
               GO TO CONTROL-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-040.
           MOVE 1610 TO POS.
           DISPLAY "ENTER A DATE FROM WHICH NOT TO PRINT: [          ]"
              AT POS.
           ADD 39 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 48        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATEANSWER.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF WS-DATEANSWER = " "
               GO TO CONTROL-040.
           MOVE WS-DATEANSWER TO CONVERT-DATE.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-CHECKDATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE 1649 TO POS.
           DISPLAY DISPLAY-DATE AT POS.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
       CONTROL-050.
           MOVE 2610 TO POS.
           DISPLAY "The Report Is Being compiled, Please Be Patient"
               AT POS.
       CONTROL-060.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           MOVE " "       TO TRANS-LINE.
           MOVE 0         TO CR-ACCOUNT-NUMBER.
           MOVE WS-RANGE1 TO CRJRN-CRACC-NUMBER.
           START CRJRN-FILE KEY NOT < CRJRN-CRACC-NUMBER
               INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 NOT = 0
               MOVE 0 TO WS-CRJRN-ST1
               MOVE "CR-JRN FILE ERROR IN START, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PR-999.
       PR-002.
           READ CRJRN-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
               PERFORM SUBTOTALS
               GO TO PR-999.
           IF WS-CRJRN-ST1 NOT = 0
               MOVE "CR-JRN BUSY ON READ, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRJRN-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR-020
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRJRN-ST1
               GO TO PR-002.
           IF WS-MESSAGE NOT = " "
               MOVE " " TO WS-MESSAGE
               PERFORM ERROR-020.
           IF CRJRN-CRACC-NUMBER < WS-RANGE1
               GO TO PR-002.
           IF CRJRN-CRACC-NUMBER > WS-RANGE2
               PERFORM SUBTOTALS
               GO TO PR-999.
           IF CRJRN-INV-DATE > WS-CHECKDATE
               GO TO PR-002.
           IF CRJRN-UNAPPLIED-AMT = 0
               GO TO PR-002.
               
           IF CRJRN-CRACC-NUMBER NOT > " "
               GO TO PR-002.
           IF CRJRN-CRACC-NUMBER NOT > 0
               GO TO PR-002.
               
           IF CRJRN-CRACC-NUMBER = CR-ACCOUNT-NUMBER
               GO TO PR-020.
           PERFORM SUBTOTALS.
       PR-005.
           MOVE CRJRN-CRACC-NUMBER TO CR-ACCOUNT-NUMBER.
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
           MOVE " "               TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           ADD 4 TO WS-LINE.
       PR-020.
      *     IF CRJRN-DUE-DATE > WS-CHECKDATE
           IF CRJRN-INV-DATE > WS-CHECKDATE
               GO TO PR-002.
           IF CRJRN-COMPLETE = "N"
               GO TO PR-002.
       
           MOVE CRJRN-REFERENCE           TO TRANS-REFERENCE
           MOVE CRJRN-TRANS               TO TRANS-TRANS
           MOVE WS-TYPE-DESC (CRJRN-TYPE) TO TRANS-TYPE
           MOVE CRJRN-COMPLETE            TO TRANS-COMPLETE
           MOVE CRJRN-INV-NO              TO TRANS-REFNO
           MOVE CRJRN-INV-DATE            TO WS-AGE-DATE SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE              TO TRANS-DATE
           PERFORM COMPUTE-DATE-PERIOD.
                      
           MOVE CRJRN-DUE-DATE            TO SPLIT-DATE
      *     MOVE CRJRN-INV-DATE            TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE              TO TRANS-DUEDATE
           MOVE CRJRN-UNAPPLIED-AMT       TO WS-AMT-OF-INVOICE.

      *     MOVE WS-CALC-PERIOD TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE CRJRN-TRANS TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR1-020.

           IF WS-CALC-PERIOD = 0
               ADD WS-AMT-OF-INVOICE TO WS-TOT-CURRENT
                                        WS-GRTOT-CURRENT
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-CURRENT
               GO TO PR-030.
           IF WS-CALC-PERIOD = 1
               ADD WS-AMT-OF-INVOICE TO WS-TOT-CURRENT
                                        WS-GRTOT-CURRENT
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-CURRENT
               GO TO PR-030.
           IF WS-CALC-PERIOD = 2
               ADD WS-AMT-OF-INVOICE TO WS-TOT-30DAY
                                        WS-GRTOT-30DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-30DAY
               GO TO PR-030.
           IF WS-CALC-PERIOD = 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-60DAY
                                        WS-GRTOT-60DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-60DAY
               GO TO PR-030.
           IF WS-CALC-PERIOD > 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-90DAY
                                        WS-GRTOT-90DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-90DAY
               GO TO PR-030.
       PR-030.
           IF WS-LINE > 60
               PERFORM PRINT-HEADINGS.
           WRITE PRINT-REC FROM TRANS-LINE AFTER 1.
           MOVE " " TO TRANS-LINE.
           ADD 1 TO WS-LINE.
           GO TO PR-002.
       PR-999.
           EXIT.
      *
       SUBTOTALS SECTION.
       SUB-000.
           IF WS-LINE > 65
               PERFORM PRINT-HEADINGS.

           IF CR-ACCOUNT-NUMBER NOT > 0
               GO TO SUB-005.

           WRITE PRINT-REC FROM P-UNDERLINE AFTER 1
           MOVE "ACCOUNT TOTAL:" TO TOT-DESC
           MOVE WS-TOT-BALANCE   TO TOT-BALANCE
           MOVE WS-TOT-CURRENT   TO TOT-CURRENT
           MOVE WS-TOT-30DAY     TO TOT-30DAY
           MOVE WS-TOT-60DAY     TO TOT-60DAY
           MOVE WS-TOT-90DAY     TO TOT-90DAY
           MOVE WS-TOT-120DAY    TO TOT-120DAY
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO TOTAL-LINE PRINT-REC.
           IF CR-ACCOUNT-NUMBER > 0
            IF WS-TOT-BALANCE NOT = CR-BALANCE
               MOVE "***ACCOUNT IN IM-BALANCE***" TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE " " TO PRINT-REC
               ADD 1 TO WS-LINE.
           WRITE PRINT-REC AFTER 1.
           ADD 3 TO WS-LINE.
       SUB-005.
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

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
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
           MOVE WS-DATE      TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H-DATE.
       OPEN-010.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-012.

           PERFORM READ-GLPARAMETER.
           MOVE GLPA-NAME TO CO-NAME.
           PERFORM ENTER-PERIOD-DATES.
       OPEN-020.
           OPEN I-O CREDITOR-MASTER.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CR MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-020.
       OPEN-030.
           OPEN I-O CRJRN-FILE.
           IF WS-CRJRN-ST1 NOT = 0
               MOVE 0 TO WS-CRJRN-ST1
               MOVE "CRJRN FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-030.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           PERFORM GRAND-TOTALS.
           CLOSE PRINT-FILE
                 CRJRN-FILE
                 GLPARAMETER-FILE
                 CREDITOR-MASTER.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
      *     STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ComputeCRDatePeriod".
       Copy "EnterCRPeriodDates".
      * Copy "EnterPeriodDates".
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
      * END-OF-JOB
