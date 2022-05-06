        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrAgeARp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrTrans".
         Copy "SelectSlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).
      *
       COPY ChlfdDebtor.
       COPY ChlfdDrTrans.
       COPY ChlfdParam.
      *
       WORKING-STORAGE SECTION.
       77  WS-RANGE1            PIC 9(7) VALUE 0.
       77  WS-RANGE2            PIC 9(7) VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-DETAIL            PIC X VALUE " ".
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
       77  WS-UNDELV-BALANCE    PIC S9(8)V99 VALUE 0.
       77  WS-UNDELV-CURRENT    PIC S9(8)V99 VALUE 0.
       77  WS-UNDELV-30DAY      PIC S9(8)V99 VALUE 0.
       77  WS-UNDELV-60DAY      PIC S9(8)V99 VALUE 0.
       77  WS-UNDELV-90DAY      PIC S9(8)V99 VALUE 0.
       77  WS-UNDELV-120DAY     PIC S9(8)V99 VALUE 0.
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       77  WS-SKIPACCOUNT       PIC X VALUE " ".
       77  WS-TERMOFSALE        PIC X(11) VALUE " ".
       77  WS-TERM-SUB          PIC 9 VALUE 0.    
       01  WS-PRINT-VOL-DIR.
           03  Ws-FxVol-Dir     PIC X(10).
           03  WS-FILE-NAME     PIC X(10).
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1  PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1      PIC 99.
       01  SPLIT-TERMOFSALE.
           03  WSTE-CODE        PIC X VALUE " ".
           03  WSTE-REST        PIC X(10) VALUE " ".
       01  STORE-TERM.
         02  WS-TERM-OCCUR OCCURS 10.
           03  WS-ST-TYPE       PIC X.
           03  WS-ST-CODE       PIC X.
           03  WS-ST-TERM       PIC X(11).
       01  WS-TYPES.
           03  FILLER           PIC X(7) VALUE "INVOICE".
           03  FILLER           PIC X(7) VALUE "PAYMENT".
           03  FILLER           PIC X(7) VALUE "R/D CHQ".
           03  FILLER           PIC X(7) VALUE "JRN.DR.".
           03  FILLER           PIC X(7) VALUE "JRN.CR.".
           03  FILLER           PIC X(7) VALUE "C/NOTE ".
           03  FILLER           PIC X(7) VALUE "INTREST".
           03  FILLER           PIC X(7) VALUE "DISCNT.".
           03  FILLER           PIC X(7) VALUE "B-DEBT.".
           03  FILLER           PIC X(7) VALUE "CH. REF".
           03  FILLER           PIC X(7) VALUE "INT REV".
       01  WS-TYPES-RED REDEFINES WS-TYPES.
           03  WS-TYPE-DESC     PIC X(7) OCCURS 11.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE "  DATE".
           03  H-DATE           PIC X(10) VALUE " ".
           03  FILLER           PIC X(22) VALUE " ".
           03  FILLER           PIC X(78) VALUE 
           "O P E N   I T E M   A G E   A N A L Y S I S".
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(7) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(39) VALUE " ".
           03  FILLER           PIC X(93) VALUE 
           "*******************************************".
       01  HEAD3.
           03  FILLER           PIC X(39) VALUE " ACCOUNT  NAME".
           03  FILLER           PIC X(93) VALUE "DATE      ACCOUNT".
       01  HEAD4.
           03  FILLER           PIC X(36) VALUE 
           " NUMBER   TELEPHONE No".
           03  FILLER           PIC X(25) VALUE "LAST PAID    BALANCE".
           03  FILLER           PIC X(13) VALUE " CURRENT".
           03  FILLER           PIC X(14) VALUE "31-60 DAY".
           03  FILLER           PIC X(13) VALUE "61-90 DAY".
           03  FILLER           PIC X(16) VALUE "91-120 DAY".
           03  FILLER           PIC X(15) VALUE "121+ DAY".
       01  HEAD5.
           03  FILLER           PIC X(3) VALUE "* ".
           03  H5-TERMS         PIC X(12) VALUE " ".
           03  FILLER           PIC X(4) VALUE "*".
           03  FILLER           PIC X(109) VALUE 
           "TYPE   REF.NO     DATE     DEL DATE".
       01  DEBTOR-LINE1.
           03  FILLER           PIC X VALUE " ".
           03  DEBT-ACCNO       PIC 9(7).
           03  FILLER           PIC X(2) VALUE " ".
           03  DEBT-NAME        PIC X(40) VALUE " ".
           03  FILLER           PIC X(82) VALUE " ".
       01  DEBTOR-LINE2.
           03  FILLER           PIC X(8) VALUE " ".
           03  DEBT-TELEPHONE   PIC X(26) VALUE " ".
           03  DEBT-PAID-DATE   PIC X(10) VALUE " ".
           03  DEBT-BALANCE     PIC Z(7)9.99-.
           03  FILLER           PIC X(76) VALUE " ".
       01  TRANS-LINE.
           03  FILLER           PIC X(17) VALUE " ".
           03  TRANS-TYPE       PIC X(7) VALUE " ".
           03  FILLER           PIC X(2) VALUE " ".
           03  TRANS-REFNO      PIC Z(5)9.
           03  FILLER           PIC X(2) VALUE " ".
           03  TRANS-DATE       PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  TRANS-DEL-DATE   PIC X(10).
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
           03  FILLER           PIC X(5) VALUE " ".
       01  P-UNDERLINE.
           03  FILLER           PIC X(58) VALUE " ".
           03  FILLER           PIC X(14) VALUE "-----------".
           03  FILLER           PIC X(14) VALUE "-----------".
           03  FILLER           PIC X(14) VALUE "-----------".
           03  FILLER           PIC X(14) VALUE "-----------".
           03  FILLER           PIC X(14) VALUE "-----------".
           03  FILLER           PIC X(4) VALUE " ".
       01  P-DBLUNDERLINE.
           03  FILLER           PIC X(58) VALUE " ".
           03  FILLER           PIC X(14) VALUE "===========".
           03  FILLER           PIC X(14) VALUE "===========".
           03  FILLER           PIC X(14) VALUE "===========".
           03  FILLER           PIC X(14) VALUE "===========".
           03  FILLER           PIC X(14) VALUE "===========".
           03  FILLER           PIC X(4) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER           PIC X(31) VALUE " ".
           03  TOT-DESC         PIC X(13) VALUE " ".
           03  TOT-BALANCE      PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
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
           03  GR-TOT-TERMS     PIC X(30) VALUE " ".
           03  GR-TOT-DESC      PIC X(14) VALUE "GRAND TOTAL **".
           03  GR-TOT-BALANCE   PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  GR-TOT-CURRENT   PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  GR-TOT-30DAY     PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  GR-TOT-60DAY     PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  GR-TOT-90DAY     PIC Z(7)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  GR-TOT-120DAY    PIC Z(7)9.99-.
           03  FILLER           PIC X(6) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "**Open Item Age Analysis Report **" AT POS
           MOVE 415 TO POS
           DISPLAY "**********************************" AT POS
           PERFORM ERROR-020.
       CONTROL-003.
           Copy "PrinterAcceptDr".
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
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
           MOVE 1410 TO POS
           DISPLAY
           "ENTER: A=ALL ACCS, D=DEBIT, C=CREDIT, P=C.O.D. ACCS: [ ]"
                AT POS
           ADD 54 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 63        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF WS-ANSWER = "A" OR = "C" OR = "D" OR = "P"
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
           MOVE 1610 TO POS
           DISPLAY
           "ENTER: D=DETAIL OF ACCOUNTS, S=SUMMARY (NO DETAIL): [ ]"
                AT POS
           ADD 53 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 62        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DETAIL.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-020.
           IF WS-DETAIL = "D" OR = "S"
               GO TO CONTROL-035
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-030.
       CONTROL-035.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-030.
       CONTROL-050.
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
       READ-TERMS-FILE SECTION.
       RTERM-000.
            MOVE 1 TO SUB-1
                      PA-RECORD.
            MOVE 2 TO PA-TYPE.
       RTERM-010.
            READ PARAMETER-FILE NEXT
                AT END NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 10
                GO TO RTERM-999.
            IF PA-TYPE < 2
                GO TO RTERM-010.
            IF PA-TYPE > 2
                GO TO RTERM-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER TERMS FILE BUSY, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RTERM-010.
            IF PARAMETER-REC = "           "
               GO TO RTERM-010.           
            MOVE PARAMETER-REC TO WS-TERM-OCCUR (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 = 10
               PERFORM ERROR-020
               GO TO RTERM-999.
            GO TO RTERM-010.
       RTERM-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           MOVE "N" TO WS-SKIPACCOUNT.
           MOVE " " TO TRANS-LINE.
           MOVE SPACES    TO DEBTOR-RECORD
           MOVE WS-RANGE1 TO DRTR-ACC-KEY.
           MOVE 0         TO DRTR-DATE.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE 88 TO WS-DRTRANS-ST1
               MOVE 0  TO WS-DRTRANS-ST1
                          DR-BALANCE
                        DRTR-AMT-OUTSTANDING
               PERFORM SUBTOTALS
               GO TO PR-999.
       PR-002.
           READ DEBTOR-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
            IF WS-SKIPACCOUNT = "N"
               PERFORM SUBTOTALS
               GO TO PR-999
            ELSE
               GO TO PR-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DRTRANS BUSY ON START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO PR-002.
               
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           IF DRTR-ACC-KEY < WS-RANGE1
               GO TO PR-002.
           IF DRTR-ACC-KEY > WS-RANGE2
            IF WS-SKIPACCOUNT = "N"
               PERFORM SUBTOTALS
               GO TO PR-999
            ELSE
               GO TO PR-999.
               
           IF DRTR-ACCOUNT-NUMBER = " "
               GO TO PR-002.
           IF DRTR-ACCOUNT-NUMBER = 0
               GO TO PR-002.
           IF DRTR-AMT-OUTSTANDING = 0
               GO TO PR-002.

           IF DR-ACCOUNT-NUMBER = 0
               GO TO PR-005.
           IF DRTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
            IF WS-SKIPACCOUNT = "N"
               GO TO PR-020
            ELSE
               GO TO PR-002.
           IF WS-SKIPACCOUNT = "N"
            IF DR-ACCOUNT-NUMBER > 0
              PERFORM SUBTOTALS.
       PR-005.
           MOVE "N" TO WS-SKIPACCOUNT.
           MOVE DRTR-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
           READ DEBTOR-MASTER 
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "UNKNOWN DEBTOR" TO DR-NAME
               GO TO PR-010.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PR-005.
               
           MOVE 2310 TO POS
           DISPLAY "Account Being Read:" AT POS
           ADD 20 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
           
           IF WS-ANSWER = "A"
               GO TO PR-010.
           IF WS-ANSWER = "D"
               AND DR-BALANCE > 0
               GO TO PR-010.
           IF WS-ANSWER = "C"
               AND DR-BALANCE < 0
               GO TO PR-010.
           IF WS-ANSWER = "P"
            IF DR-TERMS-CODE = "2"
               GO TO PR-010.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           MOVE "Y" TO WS-SKIPACCOUNT
           GO TO PR-002.
       PR-010.
           IF WS-LINE > 56
               PERFORM PRINT-HEADINGS.
           MOVE DR-ACCOUNT-NUMBER TO DEBT-ACCNO
           MOVE DR-NAME           TO DEBT-NAME
           WRITE PRINT-REC FROM DEBTOR-LINE1 AFTER 1
           MOVE " " TO DEBTOR-LINE1
           MOVE DR-TELEPHONE     TO DEBT-TELEPHONE
           MOVE DR-DATE-LAST-PAY TO WS-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE     TO DEBT-PAID-DATE
           MOVE DR-BALANCE       TO DEBT-BALANCE
           WRITE PRINT-REC FROM DEBTOR-LINE2 AFTER 1
           MOVE " " TO DEBTOR-LINE2
           MOVE DR-TERMS-CODE TO WS-TERM-SUB
           MOVE WS-ST-TERM (WS-TERM-SUB) TO WS-TERMOFSALE
           MOVE WS-TERMOFSALE TO H5-TERMS
           WRITE PRINT-REC FROM HEAD5 AFTER 2
           ADD 4 TO WS-LINE.
       PR-020.
           IF WS-SKIPACCOUNT = "Y"
              GO TO PR-002.
           MOVE WS-TYPE-DESC (DRTR-TYPE) TO TRANS-TYPE.
           MOVE DRTR-REFERENCE2          TO TRANS-REFNO.
           MOVE DRTR-DATE                TO WS-AGE-DATE
                                            SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE             TO TRANS-DATE.
           MOVE WS-BEG-DATE              TO WS-DATE.
           IF WS-AGE-YY NOT = WS-YY
               COMPUTE WS-MM = (((WS-YY - WS-AGE-YY) * 12)
                                   + WS-MM).
           SUBTRACT WS-AGE-MM FROM WS-MM.
           MOVE DRTR-AMT-OUTSTANDING TO WS-AMT-OF-INVOICE.
           IF DRTR-TYPE = 2 OR = 5 OR = 6 OR = 8 OR = 9 OR = 11
               COMPUTE WS-AMT-OF-INVOICE = WS-AMT-OF-INVOICE * -1. 

           IF WS-MM = 0
               ADD WS-AMT-OF-INVOICE TO WS-TOT-CURRENT
                                        WS-GRTOT-CURRENT
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-CURRENT
               GO TO PR-030.
           IF WS-MM = 1
               ADD WS-AMT-OF-INVOICE TO WS-TOT-30DAY
                                        WS-GRTOT-30DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-30DAY
               GO TO PR-030.
           IF WS-MM = 2
               ADD WS-AMT-OF-INVOICE TO WS-TOT-60DAY
                                        WS-GRTOT-60DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-60DAY
               GO TO PR-030.
           IF WS-MM = 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-90DAY
                                        WS-GRTOT-90DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-90DAY
               GO TO PR-030.
           IF WS-MM > 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-120DAY
                                        WS-GRTOT-120DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-120DAY.
       PR-030.
           IF WS-LINE > 60
               PERFORM PRINT-HEADINGS.
           IF WS-DETAIL = "S"
               GO TO PR-002.
           IF DRTR-DEL-DATE > 0
              MOVE DRTR-DEL-DATE   TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE TO TRANS-DEL-DATE
           ELSE
              PERFORM PR-800
              MOVE " "          TO TRANS-DEL-DATE.
           
           WRITE PRINT-REC FROM TRANS-LINE AFTER 1
           MOVE " " TO TRANS-LINE
           ADD 1 TO WS-LINE
           GO TO PR-002.
       PR-800.
           IF DRTR-TYPE = 1
            IF WS-MM = 0
               ADD WS-AMT-OF-INVOICE TO WS-UNDELV-CURRENT.
           IF DRTR-TYPE = 1
            IF WS-MM = 1
               ADD WS-AMT-OF-INVOICE TO WS-UNDELV-30DAY.
           IF DRTR-TYPE = 1
            IF WS-MM = 2
               ADD WS-AMT-OF-INVOICE TO WS-UNDELV-60DAY.
           IF DRTR-TYPE = 1
            IF WS-MM = 3
               ADD WS-AMT-OF-INVOICE TO WS-UNDELV-90DAY.
           IF DRTR-TYPE = 1
            IF WS-MM > 3
               ADD WS-AMT-OF-INVOICE TO WS-UNDELV-120DAY.
       PR-999.
           EXIT.
      *
       SUBTOTALS SECTION.
       SUB-000.
           IF WS-LINE > 60
               PERFORM PRINT-HEADINGS.
           WRITE PRINT-REC FROM P-UNDERLINE AFTER 1
           MOVE "ACCOUNT TOTAL:" TO TOT-DESC
           MOVE WS-TOT-BALANCE   TO TOT-BALANCE
           MOVE WS-TOT-CURRENT   TO TOT-CURRENT
           MOVE WS-TOT-30DAY     TO TOT-30DAY
           MOVE WS-TOT-60DAY     TO TOT-60DAY
           MOVE WS-TOT-90DAY     TO TOT-90DAY
           MOVE WS-TOT-120DAY    TO TOT-120DAY
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           IF DR-ACCOUNT-NUMBER NOT = 0 
            IF WS-TOT-BALANCE NOT = DR-BALANCE
              MOVE "************ ACCOUNT IN IM-BALANCE **" TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              ADD 1 TO WS-LINE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           ADD 3 TO WS-LINE.
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
           MOVE WS-PAGE TO H-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF WS-PAGE = 1
              WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC
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
           IF WS-ANSWER = "A"
               MOVE "* ALL ACCOUNTS PRINTED *" TO GR-TOT-TERMS.
           IF WS-ANSWER = "C"
               MOVE "* CREDIT BALANCES ONLY *" TO GR-TOT-TERMS.
           IF WS-ANSWER = "D"
               MOVE "* DEBIT BALANCES  ONLY *" TO GR-TOT-TERMS.
           IF WS-ANSWER = "P"
               MOVE "* C.O.D. ACCOUNTS ONLY *" TO GR-TOT-TERMS.
           WRITE PRINT-REC FROM P-UNDERLINE AFTER 1
           MOVE WS-GRTOT-BALANCE TO GR-TOT-BALANCE
           MOVE WS-GRTOT-CURRENT TO GR-TOT-CURRENT
           MOVE WS-GRTOT-30DAY   TO GR-TOT-30DAY
           MOVE WS-GRTOT-60DAY   TO GR-TOT-60DAY
           MOVE WS-GRTOT-90DAY   TO GR-TOT-90DAY
           MOVE WS-GRTOT-120DAY  TO GR-TOT-120DAY
           WRITE PRINT-REC FROM GRAND-TOTAL-LINE AFTER 1
           WRITE PRINT-REC FROM P-DBLUNDERLINE AFTER 1.
           
           MOVE "* UNDELIVERED INVOICES *" TO GR-TOT-TERMS.
           WRITE PRINT-REC FROM P-UNDERLINE AFTER 2
           COMPUTE WS-UNDELV-BALANCE =
            WS-UNDELV-CURRENT +
             WS-UNDELV-30DAY +
              WS-UNDELV-60DAY +
               WS-UNDELV-90DAY + 
                WS-UNDELV-120DAY.
           MOVE WS-UNDELV-BALANCE TO GR-TOT-BALANCE
           MOVE WS-UNDELV-CURRENT TO GR-TOT-CURRENT
           MOVE WS-UNDELV-30DAY   TO GR-TOT-30DAY
           MOVE WS-UNDELV-60DAY   TO GR-TOT-60DAY
           MOVE WS-UNDELV-90DAY   TO GR-TOT-90DAY
           MOVE WS-UNDELV-120DAY  TO GR-TOT-120DAY
           WRITE PRINT-REC FROM GRAND-TOTAL-LINE AFTER 1
           WRITE PRINT-REC FROM P-DBLUNDERLINE AFTER 1.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       GT-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO WS-BEG-DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H-DATE.
       OPEN-010.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-014.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
           MOVE ALL "X" TO STORE-TERM
           PERFORM READ-TERMS-FILE
           CLOSE PARAMETER-FILE.
       OPEN-020.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE 0 TO WS-DEBTOR-ST1
              MOVE "DR MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-020.
           PERFORM ERROR-020.
       OPEN-030.
           OPEN I-O DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-DRTRANS-ST1
               MOVE "DR-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-030.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           PERFORM GRAND-TOTALS.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      ******************
      *Mandatory Copies*
      ******************
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
