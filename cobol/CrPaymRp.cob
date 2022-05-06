       IDENTIFICATION DIVISION.
       PROGRAM-ID. CrPaymRp.
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
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-ACCEPT            PIC X.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-BATCH             PIC X(10) VALUE " ".
       77  WS-CHEQUE-ACCEPT     PIC X(6) VALUE " ".
       77  WS-CHEQUE-NO         PIC 9(6) VALUE 0.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(10) VALUE " ".
       77  WS-RANGE2            PIC X(7) VALUE " ".
       77  WS-RANGE3            PIC X(7) VALUE " ".
       77  WS-FOR-LOC           PIC X VALUE " ".
       77  WS-PRINT-ONLY        PIC X VALUE " ".
       77  WS-TOTAL             PIC S9(7)V99.
       77  WS-DISC-TOTAL        PIC S9(7)V99.
       77  WS-ACCNUMBER         PIC 9(7) VALUE 0.
       77  WS-ACCTOTAL          PIC S9(7)V99.
       77  WS-ACCDISC           PIC S9(7)V99.
       77  WS-NO-ACCS           PIC 9(5) VALUE 0.
       01  WS-CRTRANS-STATUS.
           03  WS-CRTRANS-ST1      PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1     PIC 99.
       01  WS-DUE-DATE.
           03  WS-DUE-YY           PIC 9999.
           03  WS-DUE-MM           PIC 99.
           03  WS-DUE-DD           PIC 99.
       01  WS-CHECK-DATE.
           03  WS-CHECK-YY         PIC 9999.
           03  WS-CHECK-MM         PIC 99.
           03  WS-CHECK-DD         PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE:".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(25) VALUE
            "CASH REQUIRED REPORT BY:".
           03  H1-DUE-DATE    PIC X(10).
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(8) VALUE "PERIOD:".
           03  H1-PERIOD      PIC 99.
           03  FILLER         PIC X(5) VALUE " ".
           03  H1-TYPE        PIC X(18) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ.
           03  FILLER         PIC X(36) VALUE " ".
       01  HEAD1-1.
           03  FILLER         PIC X(18) VALUE " ".
           03  FILLER         PIC X(35) VALUE ALL "*".
           03  FILLER         PIC X(81) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(10) VALUE "ACCOUNT".
           03  FILLER         PIC X(52) VALUE
           "TRANS No          TRANS DATE      DUE DATE".
           03  FILLER         PIC X(68) VALUE "AMOUNT          DISC".
       01  HEAD3.
           03  H-ACC          PIC X(10).
           03  H-NAME         PIC X(45).
           03  FILLER         PIC X(77).
       01  DETAIL-LINE.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-TRANS        PIC X(19) VALUE " ".
           03  D-DATE         PIC X(14).
           03  D-DUE-DATE     PIC X(15) VALUE " ".
           03  D-AMT          PIC Z(6)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-DISC         PIC Z(6)9.99-.
           03  FILLER         PIC X(49) VALUE " ".
       01  TOTAL-LINE.
           03  T-ERRORS.
               05  T-ERR1     PIC X(15) VALUE " ".
               05  T-ERR2     PIC X(16) VALUE " ".
           03  T-NAME         PIC X(26) VALUE " ".
           03  T-AMT          PIC Z(7)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  T-DISC         PIC Z(6)9.99-.
           03  FILLER         PIC X(49) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN
           MOVE 321 TO POS
           DISPLAY "** CREDITORS CASH REQUIRED REPORT **" AT POS
           MOVE 421 TO POS
           DISPLAY "************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAcceptCr".
       CONTROL-010.
           PERFORM GET-DATA.
           IF WS-PRINT-ONLY = "Y"
               PERFORM PRINT-ROUTINE
           ELSE
               PERFORM WRITE-TRANSACTIONS.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE 1113 TO POS.
           DISPLAY "DATE SHOULD BE ENTERED AS DD/MM/YYYY" AT POS.
           MOVE " " TO WS-RANGE1.
           MOVE 1010 TO POS.
           DISPLAY
           "ENTER DUE DATE TO PAY                   : [          ]"
               AT POS.
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-015.
            MOVE WS-RANGE1 TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               MOVE "1st CHECK" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-000.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE 1053 TO POS.
            DISPLAY DISPLAY-DATE AT POS.
            MOVE DISPLAY-DATE    TO H1-DUE-DATE.
            MOVE WS-CH-DATE      TO SPLIT-DATE.
            
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-CHECK-DATE WS-DATE
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
      *         MOVE "2ND CHECK" TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               GO TO GET-000.
            MOVE 2510 TO POS.
            DISPLAY "                                          " AT POS.
       GET-020.
           MOVE " " TO WS-RANGE2.
           MOVE 1310 TO POS.
           DISPLAY "           FROM ACCOUNT NUMBER: [       ]" AT POS.
           MOVE 1343 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

          IF W-ESCAPE-KEY = 4
               GO TO GET-000.
          IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO GET-022
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-022.
           MOVE " " TO WS-RANGE3.
           MOVE 1510 TO POS.
           DISPLAY "             TO ACCOUNT NUMBER: [       ]" AT POS.
           MOVE 1543 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE3.

           IF W-ESCAPE-KEY = 4
               GO TO GET-020.
           IF WS-RANGE3 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-022.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO GET-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-022.
       GET-025.
           MOVE 1710 TO POS.
           DISPLAY
           "F=Foreign, L=Local, Leave Blank For ALL : [ ]" AT POS.
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FOR-LOC.

           IF W-ESCAPE-KEY = 4
               GO TO GET-000.
           IF WS-FOR-LOC NOT = "F" AND NOT = "L" AND NOT = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-025.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-025.
       GET-030.
           MOVE 1910 TO POS.
           DISPLAY
           "Print Cash Required Report Only, Y or N : [ ]" AT POS.
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PRINT-ONLY.

           IF W-ESCAPE-KEY = 4
               GO TO GET-025.
      *     IF WS-PRINT-ONLY NOT = "Y" AND NOT = "N"
           IF WS-PRINT-ONLY NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-040.
           IF WS-PRINT-ONLY = "Y"
               GO TO GET-060.
           MOVE 2110 TO POS.
           DISPLAY
           "Enter The Beginning Cheque Number       : [      ]"
               AT POS.
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 6         TO CDA-DATALEN.
           MOVE 18        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CHEQUE-ACCEPT.

           IF W-ESCAPE-KEY = 4
               GO TO GET-030.
           IF WS-CHEQUE-ACCEPT = "     "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-045
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
       GET-045.
           MOVE WS-CHEQUE-ACCEPT TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WS-CHEQUE-NO.
           MOVE 2053 TO POS.
           DISPLAY WS-CHEQUE-NO AT POS.
       GET-050.
           IF WS-PRINT-ONLY = "Y"
               GO TO GET-050.
           MOVE 2310 TO POS.
           DISPLAY
           "Enter a New Batch Number For This Run   : [          ]"
               AT POS.
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 20        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-BATCH.

           IF W-ESCAPE-KEY = 4
               GO TO GET-040.
           IF WS-BATCH = "     "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-060
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
       GET-060.
           IF WS-PRINT-ONLY = "Y"
               PERFORM GET-USER-PRINT-NAME
               OPEN OUTPUT PRINT-FILE.
           MOVE 2510 TO POS.
           DISPLAY "The Report is being compiled.........." AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE 0 TO WS-NO-ACCS CR-ACCOUNT-NUMBER 
                      CRTR-DATE CRTR-ACC-NUMBER.
            MOVE WS-RANGE2 TO CRTR-ACC-NUMBER
            START CRTR-FILE KEY NOT < CRTR-ACC-DATE
               INVALID KEY NEXT SENTENCE.
       PRR-002.
            READ CRTR-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-CRTRANS-ST1 = 10
               MOVE 0 TO WS-CRTRANS-ST1
               PERFORM PRINT-TOTAL-LINE
               GO TO PRR-999.
            IF WS-CRTRANS-ST1 NOT = 0
              MOVE "CR-TRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CRTRANS-ST1
              GO TO PRR-002.
               
           IF CRTR-ACC-NUMBER < WS-RANGE2
               GO TO PRR-002.
           IF CRTR-ACC-NUMBER > WS-RANGE3
               MOVE SPACES TO CREDITOR-RECORD
               PERFORM PRINT-TOTAL-LINE
               GO TO PRR-999.
               
            IF CRTR-UNAPPLIED-AMT = 0
               GO TO PRR-002.
       PRR-008.
           IF WS-FOR-LOC = " "
               GO TO PRR-010.
           IF CRTR-ACC-NUMBER NOT = CR-ACCOUNT-NUMBER
               PERFORM READ-CREDITOR
               MOVE WS-ACCNUMBER TO CR-ACCOUNT-NUMBER.
           IF WS-FOR-LOC = "F"
            IF CR-FOREIGN-LOCAL = "F"
               GO TO PRR-010.
           IF WS-FOR-LOC = "L"
            IF CR-FOREIGN-LOCAL = "L"
               GO TO PRR-010.
               
      *     MOVE SPACES TO CREDITOR-RECORD
           GO TO PRR-002.
       PRR-010.
           IF CRTR-DUE-DATE > WS-CHECK-DATE
               GO TO PRR-002.
           IF LINE-CNT < 56
               GO TO PRR-050.
       PRR-040.
           ADD 1 TO PAGE-CNT.
           IF WS-FOR-LOC = "F"
                MOVE "FOREIGN ONLY" TO H1-TYPE.
           IF WS-FOR-LOC = "L"
                MOVE "LOCAL ONLY"   TO H1-TYPE.
           IF WS-FOR-LOC = " "
                MOVE "ALL ACC'S"    TO H1-TYPE.
           MOVE WS-CO-NAME         TO CO-NAME
           MOVE GLPA-CURRENT-CRPER TO H1-PERIOD
           MOVE PAGE-CNT           TO H1-PAGE
           MOVE " "                TO PRINT-REC.
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
           WRITE PRINT-REC FROM HEAD1-1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE 6 TO LINE-CNT.
        PRR-050.
           IF CRTR-ACC-NUMBER = CR-ACCOUNT-NUMBER
               GO TO PRR-060.
           PERFORM PRINT-TOTAL-LINE
           PERFORM READ-CREDITOR
           MOVE CR-ACCOUNT-NUMBER  TO WS-ACCNUMBER
           IF CR-TERMS = "2"
               MOVE "**TERMS=2**"           TO T-ERR1.
           IF CR-PAY-METHOD = "M"
               MOVE "**TYPE=M**"            TO T-ERR2.
           MOVE CRTR-ACC-NUMBER    TO H-ACC
           MOVE CR-NAME            TO H-NAME
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " "                TO PRINT-REC HEAD3
           MOVE CR-TELEPHONE       TO H-NAME
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC HEAD3
           WRITE PRINT-REC
           ADD 1 TO WS-NO-ACCS
           ADD 3 TO LINE-CNT.
        PRR-060.
           MOVE CRTR-INV-NO        TO D-TRANS
           MOVE CRTR-DATE          TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-DATE
           MOVE CRTR-DUE-DATE      TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-DUE-DATE
           MOVE CRTR-UNAPPLIED-AMT TO D-AMT
           MOVE CRTR-SETT-DISC     TO D-DISC

           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC DETAIL-LINE
           ADD CRTR-UNAPPLIED-AMT TO WS-ACCTOTAL
                                      WS-TOTAL.
           SUBTRACT CRTR-SETT-DISC FROM WS-ACCTOTAL.
           ADD CRTR-SETT-DISC TO WS-ACCDISC
                                 WS-DISC-TOTAL.
           ADD 1 TO LINE-CNT
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       PRINT-TOTAL-LINE SECTION.
       PT-005.
           IF WS-NO-ACCS = 0
                GO TO PT-999.
           MOVE "Total to pay for Account:" TO T-NAME
           MOVE WS-ACCTOTAL                 TO T-AMT
           MOVE WS-ACCDISC                  TO T-DISC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC TOTAL-LINE
           WRITE PRINT-REC.
           MOVE 0 TO WS-ACCTOTAL
                     WS-ACCDISC
           ADD 2 TO LINE-CNT.
       PT-999.
           EXIT.
      *
       WRITE-TRANSACTIONS SECTION.
       WTR-000.
            MOVE 0 TO WS-NO-ACCS CR-ACCOUNT-NUMBER 
                               CRTR-DATE
                               CRTR-ACC-NUMBER.
            START CRTR-FILE KEY NOT < CRTR-ACC-DATE
                INVALID KEY NEXT SENTENCE.
       WTR-002.
            READ CRTR-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-CRTRANS-ST1 = 10
               MOVE 0 TO WS-CRTRANS-ST1
               GO TO WTR-999.
            IF WS-CRTRANS-ST1 NOT = 0
              MOVE "CR-TRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CRTRANS-ST1
              GO TO WTR-002.
       WTR-008.
            IF WS-FOR-LOC = " "
               GO TO WTR-010.
            IF WS-FOR-LOC = "F"
             IF CRTR-FOR-AMT > 0
               GO TO WTR-010.
            IF WS-FOR-LOC = "L"
             IF CRTR-FOR-AMT = 0
               GO TO WTR-010.
            GO TO WTR-002.
           IF CRTR-ACC-NUMBER NOT = CR-ACCOUNT-NUMBER
            IF CR-ACCOUNT-NUMBER NOT = 0
                PERFORM WTR-100.

           IF WS-CHEQUE-NO = 3
               GO TO WTR-999.
       WTR-010.
           MOVE CRTR-DUE-DATE TO WS-DUE-DATE.
           IF WS-DUE-YY > WS-CH-YY
               GO TO WTR-002.
           IF WS-DUE-MM > WS-CH-MM
               GO TO WTR-002.
           IF WS-DUE-DD > WS-CH-DD
               GO TO WTR-002.
        WTR-060.
           ADD CRTR-LOC-AMT   TO WS-ACCTOTAL.
           ADD CRTR-SETT-DISC TO WS-DISC-TOTAL.
           GO TO WTR-002.
       WTR-100.
           MOVE 2910 TO POS.
           DISPLAY "Processing Payment For Account :" AT POS.
           ADD 33 TO POS.
           DISPLAY CRTR-ACC-NUMBER AT POS.
           PERFORM READ-CREDITOR.
           PERFORM REWRITE-CRJRN.
           ADD 1 TO WS-CHEQUE-NO.
           MOVE 0 TO WS-ACCTOTAL
                     WS-DISC-TOTAL.
       WTR-999.
           EXIT.
      *
       REWRITE-CRJRN SECTION.
       RWCR-001.
           MOVE 3010 TO POS.
           DISPLAY "                                           " AT POS.
       RWCR-005.
           PERFORM READ-PARAMETER-LOCK.
           MOVE GLPA-CRTRANSNO     TO CRTR-TRANS.
           ADD 1                   TO GLPA-CRTRANSNO.
           PERFORM REWRITE-PARAMETER.

           MOVE WS-BATCH           TO CRTR-REFERENCE.
           MOVE 2                  TO CRTR-TYPE.
           MOVE GLPA-CURRENT-CRPER TO CRTR-NO.
           MOVE WS-CHEQUE-NO       TO CRTR-INV-NO
                                      CRTR-DNOTE-NO.
           MOVE CR-ACCOUNT-NUMBER  TO CRTR-ACC-NUMBER.
           MOVE WS-CH-DATE         TO CRTR-DATE
                                      CRTR-DUE-DATE.
           MOVE WS-ACCTOTAL        TO CRTR-LOC-AMT.
           MOVE WS-DISC-TOTAL      TO CRTR-SETT-DISC.
      *     MOVE "N"                TO CRTR-COMPLETE.
       RWCR-019.
      *     START CRTR-FILE KEY NOT < CRTR-KEY.
           WRITE CRTR-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 NOT = 0
             MOVE "CR-TRANS BUSY ON WRITE -019, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO RWCR-019.
       RWCR-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
       RP-005.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO GLPARAMETER RECORD, 'ESC' TO EXIT."
               CALL "LOCKKBD" USING WS-PRINTER
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RP-005.
       RP-999.
           EXIT.
      *
       READ-CREDITOR SECTION.
       RCR-000.
           MOVE CRTR-ACC-NUMBER TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-KEY.
       RCR-010.
           READ CREDITOR-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "** UNKNOWN **" TO CR-NAME
                MOVE 0 TO WS-CREDITOR-ST1
                GO TO RCR-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITOR RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCR-010.
           MOVE 2910 TO POS
           DISPLAY "Account Number Being Processed:" AT POS
           ADD 32 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
       RCR-999.
             EXIT.
      *
       READ-PARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO GLPARAMETER FILE, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RPL-000.
       RPL-999.
           EXIT.
      *
       REWRITE-PARAMETER SECTION.
       REWP-000.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "GLPARAMETER FILE NOT UPDATED, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER BUSY ON RE-WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO REWP-000.
       REWP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O CRTR-FILE.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE "CRJRN FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-005.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.

           PERFORM READ-PARAMETER.
       OPEN-008.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
           IF WS-PRINT-ONLY = "N"
               GO TO END-500.
           IF LINE-CNT > 56
               PERFORM PRR-040.

           MOVE " " TO PRINT-REC TOTAL-LINE.
           WRITE PRINT-REC.
           WRITE PRINT-REC.
           WRITE PRINT-REC.
           MOVE "Total cash before Discnt:" TO T-NAME.
           MOVE WS-TOTAL                    TO T-AMT.
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC TOTAL-LINE.

           MOVE "Total Settlement Discnt.:" TO T-NAME.
           MOVE WS-DISC-TOTAL               TO T-AMT.
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC TOTAL-LINE.

           COMPUTE WS-TOTAL = WS-TOTAL - WS-DISC-TOTAL.
           MOVE "Total Nett cash required:" TO T-NAME.
           MOVE WS-TOTAL                    TO T-AMT.
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC TOTAL-LINE.

           MOVE "Total Qty of Payments   :" TO T-NAME.
           MOVE WS-NO-ACCS                  TO T-AMT.
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC TOTAL-LINE.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
           CLOSE CRTR-FILE
                 CREDITOR-MASTER
                 GLPARAMETER-FILE.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
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
      *
      * END-OF-JOB.
