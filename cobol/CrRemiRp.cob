       IDENTIFICATION DIVISION.
        PROGRAM-ID. CrRemiRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrMaster".
          Copy "SelectCrRemittance".
          Copy "SelectCrRemitTrans".
          Copy "SelectCrTrans".
          Copy "SelectCrJrn".
          Copy "SelectCrCheques".
          Copy "SelectGlMaster".
          Copy "SelectGlTrans".
          Copy "SelectGlParameter".
          Copy "SelectCbMaster".
          Copy "SelectCbTrans".
          Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS WS-SPOOLER-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrRemit.
           COPY ChlfdCrRemiTrans.
           COPY ChlfdCrTrans.
           COPY ChlfdCrCheques.
           COPY ChlfdCrJrn.
           COPY ChlfdGlMast.
           COPY ChlfdGlTrans.
           COPY ChlfdGlParam.
           COPY ChlfdDaily.
           COPY ChlfdCbMast.
           COPY ChlfdCbTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-FOUND             PIC X VALUE " ".
       77  WS-HAVE-PRINTED      PIC X VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  REMI-CNT             PIC 9(4) VALUE 0.
       77  WS-RANGE1            PIC X(7) VALUE " ".
       77  WS-RANGE2            PIC X(7) VALUE " ".
       77  WS-ANSWER1           PIC X(10) VALUE " ".
       77  WS-AUTO-REMIT        PIC X VALUE " ".
       77  WS-ANSWER3           PIC X(10) VALUE " ".
       77  WS-CAMS-CHEQUE       PIC X VALUE " ".
       77  WS-FOR-LOC           PIC X VALUE " ".
       77  WS-CHEQUE-ACCEPT     PIC X(6) VALUE " ".
       77  WS-BATCH-TOTAL       PIC 9(7)V99 VALUE 0.
       77  WS-PAYAMT            PIC S9(7)V99 VALUE 0.
       77  WS-SETT-DISC         PIC S9(7)V99 VALUE 0.
       77  WS-CRJRN-DISC        PIC S9(7)V99 VALUE 0.
       77  WS-CRJRN-CALC-DISC   PIC S9(7)V99 VALUE 0.
       77  WS-CRJRN-ALLOC-DISC  PIC S9(7)V99 VALUE 0.
       77  WS-NETT-AMOUNT       PIC S9(7)V99 VALUE 0.
       77  WS-TRANSTYPE         PIC X VALUE " ".
       77  WS-CHEQUENO          PIC 9(6) VALUE 0.
       77  WS-WORK-FIELD        PIC S9(7)V99 VALUE 0.
       77  RUN-TOTAL            PIC S9(8)V99 VALUE 0.
       77  WS-WORKTOTAL         PIC S9(7)V99 VALUE 0.
       77  WS-WORKTOTAL-DR      PIC S9(7)V99 VALUE 0.
       77  WS-WORKTOTAL-CR      PIC S9(7)V99 VALUE 0.
       77  WS-TYPEOFPROCESS     PIC X VALUE " ".
       77  WS-CURRENTGLPER      PIC 99 VALUE 0.
       77  WS-CURRENTPER        PIC 99 VALUE 0.
       77  WS-UNAPPLIED-AMT     PIC S9(7)V99 VALUE 0.
       77  WS-RUN-PAYMENTS      PIC S9(7)V99 VALUE 0.
       77  WS-RUN-DISCOUNT      PIC S9(7)V99 VALUE 0.
       77  WS-RUN-DEBITS        PIC S9(7)V99 VALUE 0.
       77  WS-RUN-CREDITS       PIC S9(7)V99 VALUE 0.
       77  WS-TOTAL             PIC S9(7)V99.
       77  WS-DISC-TOTAL        PIC S9(7)V99.
       77  WS-AMT-SUMMARY       PIC S9(7)V99.
       77  WS-ACCNUMBER         PIC 9(7) VALUE 0.
       77  WS-ACCTOTAL          PIC S9(7)V99.
       77  WS-ACCDISC           PIC S9(7)V99.
       77  WS-NO-ACCS           PIC 9(5) VALUE 0.
       77  WS-NO-ACCS-DISC      PIC 9(5) VALUE 0.
       77  NEW-CRREMNO          PIC X VALUE " ".
       77  WS-REMI-F-L          PIC X.
       01  SPLIT-DIS-DATE.
           03  SPLIT-DIS-DD         PIC 99.
           03  SPLIT-DIS-FILD1      PIC X.
           03  SPLIT-DIS-MM         PIC 99.
           03  SPLIT-DIS-FILD2      PIC X.
           03  SPLIT-DIS-YY         PIC 9999.
       01  SPLIT-TIME.
           03  SPLIT-HR         PIC 99.
           03  SPLIT-FIL1       PIC X.
           03  SPLIT-MN         PIC 99.
           03  SPLIT-FIL2       PIC X.
           03  SPLIT-SC         PIC 99.
       01  WS-REMIT-PERIOD.
           03  WS-REMI-YY          PIC 99.
           03  WS-REMI-MM          PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1     PIC 99.
       01  WS-CRTRANS-STATUS.
           03  WS-CRTRANS-ST1      PIC 99.
       01  WS-CRCHEQUE-STATUS.
           03  WS-CRCHEQUE-ST1     PIC 99.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1       PIC 99.
       01  WS-CRJRN-STATUS.
           03  WS-CRJRN-ST1        PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1      PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-SPOOLER-STATUS.
           03  WS-SPOOLER-ST1      PIC 99.
       01  WS-CBTRANS-STATUS.
           03  WS-CBTRANS-ST1      PIC 99.
       01  WS-CB-STATUS.
           03  WS-CB-ST1           PIC 99.
       01  WS-REMI-STATUS.
           03  WS-REMI-ST1         PIC 99.
       01  WS-REMITTRANS-STATUS.
           03  WS-REMITTRANS-ST1   PIC 99.
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 401.
               05  WS-APPLY           PIC X.
               05  WS-REFNUM          PIC X(10).
               05  WS-TYPE            PIC X(7).
               05  WS-BODDATE.
                   07  WS-BOD-DD     PIC 99.
                   07  WS-BOD-MM     PIC 99.
                   07  WS-BOD-YY     PIC 9999.
               05  WS-BODDATE-RED REDEFINES WS-BODDATE.
                   07  WS-BODDATENUM PIC 9(8).
               05  WS-AMTOFTYPE      PIC S9(7)V99.
               05  WS-PAYMENT        PIC S9(7)V99.
               05  WS-DISCOUNT       PIC S9(7)V99.
               05  WS-CRTYPE         PIC 99.
               05  WS-CR-TRANSNO     PIC 9(6).
       01  ACC-SUMMARY-LINES.
           03  SUMMARY-LINES OCCURS 201.
               05  A-SUM-ACC     PIC 9(7).
               05  A-SUM-NAME    PIC X(40) VALUE " ".
               05  A-SUM-CHEQUE  PIC Z9(5).
               05  A-SUM-AMT     PIC Z(6)9.99-.
               05  A-SUM-DISC    PIC Z(6)9.99-.
       01  WS-BATCH.
           03  WS-BATCH-1STCHAR  PIC X(2) VALUE "PC".
           03  WS-BATCH-REST     PIC X(8).
       01  WS-DIST-TOTALS.
           03  WS-DIST-PAYMENT    PIC S9(7)V99 VALUE 0.
           03  WS-DIST-JOURNALDR  PIC S9(7)V99 VALUE 0.
           03  WS-DIST-JOURNALCR  PIC S9(7)V99 VALUE 0.
           03  WS-DIST-DISCOUNT   PIC S9(7)V99 VALUE 0.
       01  LINE-DESCRIPTION.
           03  WS-CR1              PIC X(4).
           03  WS-CRACC            PIC X(8).
           03  WS-CR2              PIC X(3).
           03  WS-CRINV            PIC X(10).
       01  WS-BATCHPERIOD-IND.
           03  WS-1STPER           PIC X.
           03  WS-REST             PIC 99.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-RESTOFACCOUNT    PIC X(6).
       01  WS-TYPES.
           03  FILLER          PIC X(7) VALUE "INVOICE".
           03  FILLER          PIC X(7) VALUE "PAYMENT".
           03  FILLER          PIC X(7) VALUE "PAYMENT".
           03  FILLER          PIC X(7) VALUE "JRN.DR.".
           03  FILLER          PIC X(7) VALUE "JRN.CR.".
           03  FILLER          PIC X(7) VALUE "C/NOTE ".
           03  FILLER          PIC X(7) VALUE "INTREST".
           03  FILLER          PIC X(7) VALUE "DISCNT.".
           03  FILLER          PIC X(7) VALUE "FOREX  ".
       01  WS-TYPES-RED REDEFINES WS-TYPES.
           03  WS-TYPE-DESC    PIC X(7) OCCURS 9.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH.
              05  WS-DAILY-4TH-1   PIC X(10) VALUE " ".
              05  WS-DAILY-4TH-2   PIC X(10) VALUE " ".
       01  WS-DUE-DATE.
           03  WS-DUE-YY           PIC 9999.
           03  WS-DUE-MM           PIC 99.
           03  WS-DUE-DD           PIC 99.
       01  WS-CHECK-DATE.
           03  WS-CHECK-YY           PIC 9999.
           03  WS-CHECK-MM           PIC 99.
           03  WS-CHECK-DD           PIC 99.
       01  REMITTANCE-LINE.
           03  REMI-EIGHT        PIC X(19) VALUE " ".
           03  REMI-NAME         PIC X(61) VALUE
           "*** R E M I T T A N C E    A D V I C E ***".
       01  COMPANY-DETAIL-LINE.
      *     03  CO-DETAIL          PIC X(6).
           03  CO-DIG1            PIC X.
           03  COMPANY            PIC X(46).
           03  CO-DIG2            PIC X.
       01  DETAIL-LINE.
           03  D-TO               PIC X(5).
           03  D-NAME             PIC X(43).
       01  ACCOUNT-LINE.
           03  FILLER             PIC X(11) VALUE "YOUR ACC.#:".
           03  A-ACC1             PIC X(15).
           03  FILLER             PIC X(10) VALUE "OUR ACC.#:".
           03  A-ACC2             PIC X(15).
           03  FILLER             PIC X VALUE "#".
           03  A-NO               PIC Z(3)9.
           03  FILLER             PIC X(5) VALUE " ".
           03  FILLER             PIC X(10) VALUE "RUN DATE:".
           03  A-DATE             PIC X(10).
           03  FILLER             PIC X(9) VALUE " ".
       01  TRANS-HEAD.
           03  FILLER             PIC X(42) VALUE
           "DUE DATE   TYPE       TRANSACTION #".
           03  FILLER             PIC X(38) VALUE
           "    AMOUNT   SETT-DISC    NETT-AMOUNT".
       01  TRANS-LINE.
           03  TRANS-DATE         PIC X(10).
           03  FILLER             PIC X(1) VALUE " ".
           03  TRANS-TYPE         PIC X(7) VALUE " ".
           03  FILLER             PIC X(4) VALUE " ".
           03  TRANS-NO           PIC X(20) VALUE " ".
           03  TRANS-GROSS-AMT    PIC Z(6)9.99-.
           03  FILLER             PIC X(1) VALUE " ".
           03  TRANS-SETT-DISC    PIC Z(6)9.99-.
           03  FILLER             PIC X(4) VALUE " ".
           03  TRANS-NETT-AMT     PIC Z(6)9.99-.
       01  PAYMENT-LINE.
           03  PAYMENT-DATE       PIC X(10).
           03  FILLER             PIC X(1) VALUE " ".
           03  PAYMENT-TYPE       PIC X(10) VALUE " ".
           03  FILLER             PIC X(1) VALUE " ".
           03  PAYMENT-NO         PIC X(25) VALUE " ".
           03  FILLER             PIC X(7) VALUE " ".
           03  PAYMENT-SETT-DISC  PIC Z(6)9.99-.
           03  FILLER             PIC X(4) VALUE " ".
           03  PAYMENT-AMT        PIC Z(6)9.99-.
           03  FILLER             PIC X VALUE " ".
       01  UNDER-LINE.
           03  FILLER             PIC X(54) VALUE " ".
           03  FILLER             PIC X(10) VALUE "----------".
           03  FILLER             PIC X(5) VALUE " ".
           03  FILLER             PIC X(10) VALUE "----------".
           03  FILLER             PIC X VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(22) VALUE " ".
           03  T-ACC          PIC X(10) VALUE " ".
           03  T-NAME         PIC X(26) VALUE " ".
           03  T-AMT          PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  T-DISC         PIC Z(6)9.99-.
           03  FILLER         PIC X(48) VALUE " ".
       01  A-COMPANY-DETAIL-LINE.
           03  A-FILLER         PIC X(20) VALUE " ".
           03  A-COMPANY        PIC X(50).
           03  A-FILLER         PIC X(62) VALUE " ".
       01  A-HEAD1.
           03  A-FILLER          PIC X(7) VALUE "  DATE".
           03  A-H1-DATE         PIC X(10).
           03  A-FILLER          PIC X(18) VALUE " ".
           03  A-FILLER          PIC X(50) VALUE
           "CREDITORS AUTOMATIC PAYMENTS AUDIT TRAIL".
           03  A-FILLER          PIC X(14) VALUE "PAYMENT DATE:".
           03  A-H1-PAY-DATE     PIC X(10).
           03  A-FILLER          PIC X(14) VALUE " ".
           03  A-FILLER          PIC X(5) VALUE "PAGE:".
           03  A-H1-PAGE         PIC Z9.
           03  A-FILLER          PIC X(1) VALUE " ".
       01  A-HEAD2.
           03  A-FILLER          PIC X(35) VALUE " ".
           03  A-FILLER          PIC X(40) VALUE ALL "*".
           03  A-FILLER          PIC X(16) VALUE " ".
           03  A-FILLER          PIC X(8) VALUE "PERIOD:".
           03  A-H1-PERIOD       PIC X(2).
           03  A-FILLER          PIC X(31) VALUE " ".
       01  A-HEAD3.
           03  A-FILLER          PIC X(10) VALUE "ACCOUNT".
           03  A-FILLER          PIC X(9) VALUE "TRANS".
           03  A-FILLER          PIC X(10) VALUE "TRANS".
           03  A-FILLER          PIC X(9) VALUE "TRANS".
           03  A-FILLER          PIC X(25) VALUE "BATCH NAME".
           03  A-FILLER          PIC X(13) VALUE "TRANS #".
           03  A-FILLER          PIC X(14) VALUE "DATE".
           03  A-FILLER          PIC X(11) VALUE "BEGIN".
           03  A-FILLER          PIC X(27) VALUE "REMAIN".
       01  A-HEAD4.
           03  A-FILLER          PIC X(10) VALUE "NUMBER".
           03  A-FILLER          PIC X(20) VALUE "CODE     DESC".
           03  A-FILLER          PIC X(59) VALUE "NO.".
           03  A-FILLER          PIC X(12) VALUE "AMOUNT".
           03  A-FILLER          PIC X(10) VALUE "AMOUNT".
           03  A-FILLER          PIC X(11) VALUE "DISCOUNT".
           03  A-FILLER          PIC X(9) VALUE "APPLIED".
       01  A-DETAIL-LINE.
           03  A-D-ACCOUNTNO     PIC X(7).
           03  A-FILLER          PIC X(5) VALUE " ".
           03  A-D-TRANSCODE     PIC 9.
           03  A-FILLER          PIC X(5) VALUE " ".
           03  A-D-TRANSDESC     PIC X(10).
           03  A-D-TRANSNO       PIC Z(5)9.
           03  A-FILLER          PIC X(4) VALUE " ".
           03  A-D-REFERENCE1    PIC X(25).
           03  A-D-REFERENCE2    PIC X(11).
           03  A-D-DATE          PIC X(10).
           03  A-FILLER          PIC X(1) VALUE " ".
           03  A-D-BEGIN         PIC Z(6)9.99-.
           03  A-FILLER          PIC X(1) VALUE " ".
           03  A-D-OUTST         PIC Z(6)9.99-.
      *    03  A-FILLER          PIC X(1) VALUE " ".
           03  A-D-DISCOUNT      PIC Z(6)9.99-.
      *    03  A-FILLER          PIC X(1) VALUE " ".
           03  A-D-APPLIED       PIC Z(6)9.99-.
           03  A-FILLER          PIC X(2) VALUE " ".
       01  A-TOTAL-LINE.
           03  A-FILLER          PIC X(20) VALUE " ".
           03  A-TOT-DESC        PIC X(20) VALUE " ".
           03  A-FILLER          PIC X(2) VALUE " ".
           03  A-TOT-AMOUNT      PIC Z(6)9.99-.
           03  A-FILLER          PIC X(81) VALUE " ".
       01  WS-TEMP-LINE.
           03  FILLER           PIC X(8) VALUE " ".
           03  T-TRANS-DATE     PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
           03  T-TRANS-DUEDATE  PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
       01  SUMMARY-LINE.
           03  SUM-ACC          PIC X(10) VALUE " ".
           03  SUM-NAME         PIC X(41) VALUE " ".
           03  SUM-CHEQUE       PIC Z(5)9.
           03  FILLER           PIC X VALUE " ".
           03  SUM-AMT          PIC Z(7)9.99-.
           03  FILLER           PIC X VALUE " ".
           03  SUM-DISC         PIC Z(6)9.99-.
       Copy "WsDateInfo".
       Copy "WStore".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 310 TO POS
           DISPLAY
           "* CREDITORS AUTO PAYMENT ALLOCATIONS / REMITTANCE REPORT *"
                  AT POS
           MOVE 405 TO POS
           DISPLAY 
           "**********************************************************"
             AT POS.
       CONTROL-003.
           Copy "PrinterAcceptCr".
       CONTROL-010.
           PERFORM ERROR1-020.
           PERFORM ERROR2-020.
           MOVE "N" TO WS-HAVE-PRINTED
           MOVE "2" TO WS-TYPEOFPROCESS WS-TRANSTYPE.

           PERFORM OPEN-FILES
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-YY  TO SPLIT-DIS-YY
           MOVE ":"    TO SPLIT-DIS-FILD1
           MOVE WS-MM  TO SPLIT-DIS-MM
           MOVE ":"    TO SPLIT-DIS-FILD2
           MOVE WS-DD  TO SPLIT-DIS-DD.
           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR  TO SPLIT-HR
           MOVE ":"    TO SPLIT-FIL1
           MOVE WS-MIN TO SPLIT-MN
           MOVE ":"    TO SPLIT-FIL2
           MOVE WS-SEC TO SPLIT-SC.
           
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO A-DATE A-H1-DATE.

           PERFORM GET-DATA.
           IF WS-AUTO-REMIT = "A"
              PERFORM READ-GLPARAMETER
              PERFORM WRITE-TYPE-AUDIT
              PERFORM DELETE-CHEQUES-FROM-LAST-RUN
              PERFORM ALLOCATE-ITEMS
              PERFORM READ-PARAMETER-LOCK
              PERFORM AUTO-END-OFF.
           IF WS-AUTO-REMIT = "R"
              PERFORM PRINT-ROUTINE
              PERFORM WRITE-TYPE-AUDIT
              PERFORM END-OFF.
       CONTROL-999.
           EXIT PROGRAM.
      *
       GET-DATA SECTION.
       GET-015.
           MOVE 810 TO POS.
           DISPLAY 
           "Enter The Due Date As DD/MM/YYYY          : [          ]"
           AT POS.
           ADD 45 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

           IF W-ESCAPE-KEY = 3
               PERFORM END-900.
           IF WS-ANSWER1 NOT > " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-020.
            MOVE WS-ANSWER1 TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               MOVE "1ST WRONG" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-015.
           MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE DISPLAY-DATE    TO A-H1-PAY-DATE A-DATE PAYMENT-DATE.
           MOVE 855 TO POS.
           DISPLAY DISPLAY-DATE AT POS.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-CHECK-DATE WS-DATE
           
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
               MOVE "2ND DATE WRONG" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-015.

            MOVE WS-CURRENTPER TO SUB-1.
            MOVE GL-BEGDATE (SUB-1) TO WS-BEG-DATE.
            MOVE GL-ENDDATE (SUB-1) TO WS-END-DATE.
            IF WS-CHECK-DATE NOT < WS-BEG-DATE
             IF WS-CHECK-DATE NOT > WS-END-DATE
                GO TO GET-025.
            IF WS-CHECK-DATE > WS-END-DATE
            MOVE "THE JOURNAL DATE MUST BE < OR = THE END PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-015.
            IF WS-CHECK-DATE < WS-BEG-DATE
            MOVE "THE JOURNAL DATE MUST > OR = THE BEGIN PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-015.
       GET-025.
           MOVE 1010 TO POS.
           DISPLAY 
           "F=Foreign, L=Local, Leave Blank For ALL   : [ ]" AT POS.
           ADD 45 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FOR-LOC.

           IF W-ESCAPE-KEY = 4
               GO TO GET-015.
           IF WS-FOR-LOC NOT = "F" AND NOT = "L" AND NOT = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-025.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-025.
       GET-040.
           MOVE 1210 TO POS.
           DISPLAY
           "Enter The Beginning Cheque Number         : [      ]" AT POS.
           ADD 45 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 6         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CHEQUE-ACCEPT.

           IF W-ESCAPE-KEY = 4
               GO TO GET-025.
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
           MOVE NUMERIC-RATE TO WS-CHEQUENO.
           MOVE 1255 TO POS.
           DISPLAY WS-CHEQUENO AT POS.
       GET-050.
           MOVE 1410 TO POS
           DISPLAY
           "Enter 'R'=Remittance, 'A'=Auto Cash Post  : [ ]" AT POS
           MOVE 1513 TO POS
           DISPLAY
           "NB! Print Remittances Before Auto Cash Posting is done."
             AT POS
           MOVE 1455 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-AUTO-REMIT.

           IF W-ESCAPE-KEY = 4
               GO TO GET-040.
           IF WS-AUTO-REMIT NOT = "R" AND NOT = "A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-060
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
       GET-060.
           IF WS-AUTO-REMIT = "R"
                GO TO GET-065
           IF WS-AUTO-REMIT = "A"
            IF WS-FOR-LOC NOT = "L"
               MOVE "L" TO WS-FOR-LOC
               MOVE 1052 TO POS
               DISPLAY WS-FOR-LOC AT POS.
           MOVE 1610 TO POS.
           DISPLAY
           "Enter the Batch name for this RUN      : [PC        ]"
               AT POS.
           ADD 42 TO POS.
           DISPLAY WS-BATCH-1STCHAR AT POS
           ADD 2 TO POS

           MOVE " "       TO CDA-DATA.
           MOVE 8         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-BATCH-REST.

           IF W-ESCAPE-KEY = 4
               GO TO GET-050.
           IF WS-BATCH-REST = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-060.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-065
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-060.
               
           GO TO GET-070.
       GET-065.
           MOVE 1810 TO POS
           DISPLAY
           "Enter 'P'=Cheque Printing, T=CAMS Transfer: [ ]" AT POS
           ADD 45 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CAMS-CHEQUE.

           IF W-ESCAPE-KEY = 4
               GO TO GET-050.
           IF WS-CAMS-CHEQUE NOT = "P" AND NOT = "T"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-065.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-070
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-065.
       GET-070.
           IF WS-AUTO-REMIT = "A"
               MOVE 1652 TO POS
               DISPLAY WS-BATCH AT POS.
       GET-075.
           MOVE 2010 TO POS
           DISPLAY 
           "ENTER YY=YEAR, MM=MONTH FOR THE REMITTANCE : [    ]" AT POS
           ADD 46 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 4         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-REMIT-PERIOD.

           IF W-ESCAPE-KEY = 4
               GO TO GET-065.
           IF WS-REMI-YY = 0
           OR WS-REMI-MM = 0
               MOVE "THE YY AND MM MUST BE FILLED IN CORRECTLY."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-075.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-080
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-075.
       GET-080.
           MOVE 2110 TO POS
           DISPLAY 
           "ENTER F=FOREIGN, L=LOCAL REMITTANCES       : [ ]" AT POS
           ADD 46 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 18        TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-REMI-F-L.

           IF W-ESCAPE-KEY = 4
               GO TO GET-075.
           IF WS-REMI-F-L NOT = "F" AND NOT = "L"
               MOVE "'F' OR 'L' MUST BE FILLED IN CORRECTLY."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-080.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-900
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-075.
       GET-900.
            PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       ALLOCATE-ITEMS SECTION.
       AI-001.
            MOVE 0 TO CR-KEY.
            START CREDITOR-MASTER KEY NOT < CR-KEY.
       AI-005.
           MOVE 0 TO WS-DIST-PAYMENT
                     WS-DIST-JOURNALDR
                     WS-DIST-JOURNALCR
                     WS-DIST-DISCOUNT
                     WS-UNAPPLIED-AMT
                     WS-SETT-DISC
                     WS-PAYAMT
                     WS-DIST-DISCOUNT.
           PERFORM CLEAR-FIELDS.
           MOVE 0 TO RUN-TOTAL
                     WS-WORKTOTAL.
       AI-020.
           PERFORM READ-NEXT-CREDITOR.
           IF WS-CREDITOR-ST1 = 10
               GO TO AI-999.
           IF CR-FOREIGN-LOCAL = "L"
            IF WS-FOR-LOC = "F"
              GO TO AI-020.
           IF CR-FOREIGN-LOCAL = "F"
            IF WS-FOR-LOC = "L"
              GO TO AI-020.
           MOVE 1 TO SUB-1.
           PERFORM READ-CREDITOR-TRANS.
           MOVE 1 TO SUB-1.
           IF WS-REFNUM (SUB-1) = " "
              GO TO AI-020.
           IF WS-PAYAMT NOT > 0
            IF WS-SETT-DISC NOT > 0
              PERFORM WRITE-BACK-TRANSACTIONS
              GO TO AI-005.
           PERFORM CHECK-AMT-TO-PAY.
           MOVE 1 TO SUB-1.
           PERFORM WRITE-PAYMENT-TRANSACTION.
           PERFORM WRITE-CHEQUE-TRANSACTION
           PERFORM WRITE-CASHBOOK.
           PERFORM APPLY-AMOUNTS.
           ADD WS-PAYAMT    TO WS-RUN-PAYMENTS.
           ADD WS-SETT-DISC TO WS-RUN-DISCOUNT.
           PERFORM WRITE-GLSETT-DISCOUNTS.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           ADD 1 TO LINE-CNT WS-CHEQUENO.
           GO TO AI-005.
       AI-999.
           EXIT.
      *
       WRITE-BACK-TRANSACTIONS SECTION.
       WBT-005.
           MOVE 0 TO SUB-1. 
       WBT-010.
           ADD 1 TO SUB-1.
           IF SUB-1 > 400
               GO TO WBT-999.
           IF WS-REFNUM (SUB-1) = " "
               GO TO WBT-999.
           SUBTRACT WS-PAYMENT (SUB-1)  FROM WS-PAYAMT
           SUBTRACT WS-DISCOUNT (SUB-1) FROM WS-SETT-DISC
           SUBTRACT WS-DISCOUNT (SUB-1) FROM WS-DIST-DISCOUNT.
           GO TO WBT-010.
       WBT-999.
           EXIT.
      *
       CHECK-AMT-TO-PAY SECTION.
       CATP-005.
           MOVE 0 TO SUB-1 WS-WORKTOTAL-CR WS-WORKTOTAL-DR.
       CATP-010.
           ADD 1 TO SUB-1.
           IF SUB-1 > 400
               GO TO CATP-500.
           IF WS-REFNUM (SUB-1) = " "
               GO TO CATP-500.
           ADD WS-PAYMENT (SUB-1)  TO WS-WORKTOTAL-CR.
           ADD WS-DISCOUNT (SUB-1) TO WS-WORKTOTAL-DR.
           GO TO CATP-010.
       CATP-500.
           IF WS-WORKTOTAL-CR = WS-PAYAMT
            IF WS-WORKTOTAL-DR = WS-SETT-DISC
               GO TO CATP-999.
           COMPUTE WS-WORKTOTAL-CR = WS-WORKTOTAL-CR - WS-PAYAMT.
           IF WS-WORKTOTAL-CR = 0
               GO TO CATP-800.
       CATP-550.
           MOVE 0 TO SUB-1. 
       CATP-551.
           ADD 1 TO SUB-1.
           IF SUB-1 > 400
               GO TO CATP-800.
           IF WS-REFNUM (SUB-1) = " "
               GO TO CATP-800.
           IF WS-WORKTOTAL-CR = 0
               GO TO CATP-800.
           IF WS-WORKTOTAL-CR NOT > WS-PAYMENT (SUB-1)
               SUBTRACT WS-WORKTOTAL-CR FROM WS-PAYMENT (SUB-1)
               MOVE 0                     TO WS-WORKTOTAL-CR
               PERFORM CATP-555
               GO TO CATP-800
           ELSE
               SUBTRACT WS-PAYMENT (SUB-1) FROM WS-WORKTOTAL-CR
               MOVE 0                        TO WS-PAYMENT (SUB-1).
               PERFORM CATP-555
               GO TO CATP-551.
       CATP-555.
           IF WS-PAYMENT (SUB-1) = 0
            IF WS-DISCOUNT (SUB-1) = 0
               MOVE " " TO WS-APPLY (SUB-1).
       CATP-800.
           COMPUTE WS-WORKTOTAL-DR = WS-WORKTOTAL-DR - WS-SETT-DISC.
           IF WS-WORKTOTAL-DR = 0
               GO TO CATP-999.
       CATP-850.
           MOVE 0 TO SUB-1. 
       CATP-851.
           ADD 1 TO SUB-1.
           IF SUB-1 > 400
               GO TO CATP-999.
           IF WS-REFNUM (SUB-1) = " "
               GO TO CATP-999.
           IF WS-WORKTOTAL-DR = 0
               GO TO CATP-999.
           IF WS-WORKTOTAL-DR NOT > WS-DISCOUNT (SUB-1)
               SUBTRACT WS-WORKTOTAL-DR FROM WS-DISCOUNT (SUB-1)
               MOVE 0                     TO WS-WORKTOTAL-DR
               PERFORM CATP-555
               GO TO CATP-999
           ELSE
               SUBTRACT WS-DISCOUNT (SUB-1) FROM WS-WORKTOTAL-DR
               MOVE 0                         TO WS-DISCOUNT (SUB-1)
               PERFORM CATP-555
               GO TO CATP-851.
       CATP-999.
           EXIT.
      *
       APPLY-AMOUNTS SECTION.
       AP-000.
           MOVE 3010 TO POS
           DISPLAY "REWRITING CR-TRANS WITH CASH ALLOCATED....." AT POS.
           MOVE 0 TO SUB-1.   
       AP-010.     
           ADD 1 TO SUB-1.
           IF SUB-1 > 400
               GO TO AP-999.
           IF WS-REFNUM (SUB-1) = " "
               GO TO AP-999.
           IF WS-APPLY (SUB-1) = "A"
              GO TO AP-020.
           GO TO AP-010.
       AP-020.
           MOVE WS-CRTYPE (SUB-1)     TO CRTR-TYPE.
           MOVE WS-CR-TRANSNO (SUB-1) TO CRTR-TRANS.
           START CRTR-FILE KEY NOT < CRTR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE 0 TO WS-CRTRANS-ST1
              MOVE "CR-TRANS BUSY ON START,AP-020, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO AP-020.
       AP-030.               
           READ CRTR-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
               GO TO AP-010.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE 0 TO WS-CRTRANS-ST1
              MOVE "CR TRANS BUSY ON READ-LOCK AP-030, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO AP-030.
       AP-035.
            IF CRTR-UNAPPLIED-AMT NOT <
                 WS-PAYMENT (SUB-1) + WS-DISCOUNT (SUB-1)
               SUBTRACT WS-PAYMENT (SUB-1)  FROM CRTR-UNAPPLIED-AMT
               SUBTRACT WS-DISCOUNT (SUB-1) FROM CRTR-UNAPPLIED-AMT
            ELSE
               MOVE 0                         TO CRTR-UNAPPLIED-AMT.
           PERFORM UPDATE-CREDITOR.
       AP-037.
           PERFORM PRINT-AUTO-ALLOCATIONS.
       AP-038.
           REWRITE CRTR-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
               MOVE "REWRITE CRTRANS, AP038 STATUS=2" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO AP-010.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE 0 TO WS-CRTRANS-ST1
              MOVE "CRTRANS BUSY ON REWRITE, AP-038, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO AP-038.
           IF WS-DISCOUNT (SUB-1) NOT = 0
              PERFORM WRITE-DISCOUNT-TRANSACTION.
       AP-040.
           MOVE WS-REFNUM (SUB-1) TO CRTR-INV-NO.
           PERFORM READ-CRJRN-TRANSACTIONS.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CRJRN BUSY ON READ, AP-040 ST1=2, 'ESC' TO EXIT."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE CRJRN-INV-NO TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO AP-010.
            IF CRJRN-UNAPPLIED-AMT NOT <
                   WS-PAYMENT (SUB-1) + WS-DISCOUNT (SUB-1)
              SUBTRACT WS-PAYMENT (SUB-1)  FROM CRJRN-UNAPPLIED-AMT
              SUBTRACT WS-DISCOUNT (SUB-1) FROM CRJRN-UNAPPLIED-AMT
            ELSE
              MOVE 0                         TO CRJRN-UNAPPLIED-AMT.
           IF CRJRN-UNAPPLIED-AMT = 0
               MOVE "Y" TO CRJRN-COMPLETE.
           REWRITE CRJRN-REC.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CRJRN BUSY ON REWRITE, 'ESC' FOR JRN-NO"
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE CRJRN-INV-NO TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
           GO TO AP-010.
       AP-999.
           EXIT.
      *
       READ-CREDITOR-TRANS SECTION.
       RDT-005.
           MOVE 3010 TO POS
           DISPLAY "READING CR-TRANS FOR ACCOUNT......         " AT POS.
           MOVE CR-ACCOUNT-NUMBER TO CRTR-ACC-NUMBER.
           MOVE 0                 TO CRTR-DATE.
           START CRTR-FILE KEY NOT < CRTR-ACC-DATE
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
               GO TO RDT-999.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CRTRANS BUSY ON START,RDT-005, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDT-005.
       RDT-010.
           READ CRTR-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 10
               CLOSE CRTR-FILE
               PERFORM OPEN-014
               GO TO RDT-999.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CRTRANS BUSY ON READ-NEXT,RDT-010, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDT-010.
           IF CRTR-ACC-NUMBER NOT = CR-ACCOUNT-NUMBER
              GO TO RDT-999.
           IF CRTR-UNAPPLIED-AMT = 0
              GO TO RDT-010.
           MOVE CRTR-DUE-DATE TO WS-DUE-DATE.
           IF WS-DUE-DATE > WS-CHECK-DATE
              GO TO RDT-010.
           IF CRTR-TYPE = 2 OR = 5 OR = 6 OR = 8
              PERFORM RDT-020
              GO TO RDT-010.
           MOVE CRTR-TYPE                TO WS-CRTYPE (SUB-1)
           MOVE CRTR-TRANS               TO WS-CR-TRANSNO (SUB-1)
           MOVE CRTR-INV-NO              TO WS-REFNUM (SUB-1)
           MOVE WS-TYPE-DESC (CRTR-TYPE) TO WS-TYPE (SUB-1)
           MOVE CRTR-DATE                TO WS-BODDATE (SUB-1)
           MOVE CRTR-UNAPPLIED-AMT       TO WS-AMTOFTYPE (SUB-1)
           MOVE "A"                      TO WS-APPLY (SUB-1).
        RDT-020.
           COMPUTE WS-PAYMENT (SUB-1) =
                    CRTR-UNAPPLIED-AMT - CRTR-SETT-DISC
           MOVE CRTR-SETT-DISC           TO WS-DISCOUNT (SUB-1)

           ADD WS-PAYMENT (SUB-1)  TO WS-PAYAMT
           ADD WS-DISCOUNT (SUB-1) TO WS-SETT-DISC
           COMPUTE WS-DIST-DISCOUNT =
                WS-DIST-DISCOUNT + WS-DISCOUNT (SUB-1).
        RDT-030.
      **SUB-9 = TOTAL NO OF CR-TRANS****
           ADD 1 TO SUB-1
                    SUB-9.
           IF SUB-1 < 401
               GO TO RDT-010.
       RDT-999.
           EXIT.
      *
       DELETE-CHEQUES-FROM-LAST-RUN SECTION.
       DCFLR-005.
           MOVE 3010 TO POS
           DISPLAY "DELETING CHEQUES FROM LAST RUN.....       " AT POS.
           MOVE 0 TO CRCH-ACC-NUMBER.
           START CRCH-FILE KEY NOT < CRCH-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-CRCHEQUE-ST1 = 23 OR 35 OR 49 OR 91
               GO TO DCFLR-999.
           IF WS-CRCHEQUE-ST1 NOT = 0
               MOVE 0 TO WS-CRCHEQUE-ST1
               MOVE "CHEQUE DELETE BUSY AT START, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DCFLR-005.
       DCFLR-010.
           READ CRCH-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-CRCHEQUE-ST1 = 10
               CLOSE CRCH-FILE
               PERFORM OPEN-018
               GO TO DCFLR-999.
           IF WS-CRCHEQUE-ST1 NOT = 0
               MOVE "CRCHEQUE BUSY ON READ, DCFLR-010, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DCFLR-010.
       DCFLR-020.
           DELETE CRCH-FILE
             INVALID KEY NEXT SENTENCE.
           IF WS-CRCHEQUE-ST1 NOT = 0
               MOVE "CRCHEQUE BUSY ON DELETE, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DCFLR-020.
           GO TO DCFLR-010.
       DCFLR-999.
           EXIT.
      *
       WRITE-PAYMENT-TRANSACTION SECTION.
       WRPTR-000.
           MOVE 3010 TO POS
           DISPLAY "WRITING PAYMENT TRANSACTION..........              "
             AT POS
           PERFORM READ-GLPARAMETER-LOCK
           MOVE GLPA-CRTRANSNO    TO CRTR-TRANS
           ADD 1                  TO GLPA-CRTRANSNO
           PERFORM REWRITE-GLPARAMETER.
           MOVE 2                 TO CRTR-TYPE
           MOVE " "               TO CRTR-FUTURE
           MOVE WS-CURRENTPER     TO CRTR-NO
           MOVE CR-ACCOUNT-NUMBER TO CRTR-ACC-NUMBER
           MOVE WS-BATCH          TO CRTR-REFERENCE
           MOVE WS-CHEQUENO       TO CRTR-INV-NO
                                     CRTR-DNOTE-NO
           MOVE WS-DATE           TO CRTR-DATE
                                     CRTR-DUE-DATE
           MOVE WS-PAYAMT         TO CRTR-LOC-AMT
           COMPUTE CRTR-LOC-AMT = CRTR-LOC-AMT * -1
           MOVE WS-WORKTOTAL      TO CRTR-UNAPPLIED-AMT
           COMPUTE CRTR-UNAPPLIED-AMT = CRTR-UNAPPLIED-AMT * -1
           MOVE 0                 TO CRTR-SETT-DISC
                                     CRTR-FOR-AMT
                                     CRTR-VAT-AMT
                                     CRTR-EXCHANGE.
           PERFORM PRINT-AUTO-ALLOCATIONS.
       WRPTR-010.
           WRITE CRTR-REC
                 INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
                 GO TO WRPTR-000.
            IF WS-CRTRANS-ST1 = 91
                MOVE 0 TO WS-CRTRANS-ST1
                MOVE "TRANS FILE BUSY ON WRITE ERC=91, 'ESC' TO RE-TRY."
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WRPTR-010.
           PERFORM ERROR-020.
       WRPTR-999.
              EXIT.
      *
       WRITE-CHEQUE-TRANSACTION SECTION.
       WRCHETR-000.
           MOVE 3010 TO POS
           DISPLAY "WRITING CHEQUE TRANSACTION..........              "
             AT POS
           MOVE CR-ACCOUNT-NUMBER TO CRCH-ACC-NUMBER
           MOVE WS-CHEQUENO       TO CRCH-CHEQUE-NO
           MOVE WS-DATE           TO CRCH-DATE
           MOVE WS-PAYAMT         TO CRCH-CHEQUE-AMT.
       WRCHETR-010.
           WRITE CRCH-REC
                 INVALID KEY NEXT SENTENCE.
           IF WS-CRCHEQUE-ST1 NOT = 0
               MOVE "CHEQUE TRANS NOT WRITTEN, 'ESC' TO SEE ACC#."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE CR-ACCOUNT-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
           PERFORM ERROR-020.
       WRCHETR-999.
           EXIT.
      *
       WRITE-DISCOUNT-TRANSACTION SECTION.
       WRDTR-000.
             PERFORM READ-GLPARAMETER-LOCK
             MOVE GLPA-CRTRANSNO      TO CRTR-TRANS
             ADD 1                    TO GLPA-CRTRANSNO
             PERFORM REWRITE-GLPARAMETER.
             MOVE 8                   TO CRTR-TYPE
             MOVE " "                 TO CRTR-FUTURE
             MOVE WS-CURRENTPER       TO CRTR-NO
             MOVE CR-ACCOUNT-NUMBER   TO CRTR-ACC-NUMBER
             MOVE WS-BATCH            TO CRTR-REFERENCE
             MOVE WS-CHEQUENO         TO CRTR-INV-NO
                                         CRTR-DNOTE-NO
             MOVE WS-DATE             TO CRTR-DATE
                                         CRTR-DUE-DATE
             MOVE WS-DISCOUNT (SUB-1) TO CRTR-LOC-AMT
             COMPUTE CRTR-LOC-AMT = CRTR-LOC-AMT * -1
             MOVE 0                   TO CRTR-SETT-DISC
                                         CRTR-UNAPPLIED-AMT
            PERFORM PRINT-AUTO-ALLOCATIONS.
       WRDTR-010.
             WRITE CRTR-REC
                 INVALID KEY NEXT SENTENCE.
            IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
                 GO TO WRDTR-000.
            IF WS-CRTRANS-ST1 NOT = 0
                MOVE 0 TO WS-CRTRANS-ST1
                MOVE "CRTRANS BUSY ON WRITE, WRDTR-010, 'ESC' TO RETRY."
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WRDTR-010.
       WRDTR-999.
              EXIT.
      *
       UPDATE-CREDITOR SECTION.
       UPCR-000.
            PERFORM READ-CREDITORS-LOCK
            IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                GO TO UPCR-900.
            MOVE WS-BODDATE (SUB-1) TO WS-AGE-DATE.
       UPCR-005.
             IF WS-AGE-MM = 0
                 MOVE WS-DATE TO WS-AGE-DATE.
             PERFORM COMPUTE-DATE-PERIOD.

             IF WS-CALC-PERIOD = 0
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-CURRENT
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-CURRENT
                 GO TO UPCR-010.
             IF WS-CALC-PERIOD = 1
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-30DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-30DAY
                 GO TO UPCR-010.
             IF WS-CALC-PERIOD = 2
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-60DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-60DAY
                 GO TO UPCR-010.
             IF WS-CALC-PERIOD = 3
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-90DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-90DAY
                 GO TO UPCR-010.
             SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-120DAY.
             SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-120DAY.
       UPCR-010.
             MOVE WS-DATE TO CR-DATE-LAST-PAY.
             COMPUTE CR-BALANCE = CR-30DAY + CR-60DAY
                    + CR-90DAY + CR-120DAY + CR-CURRENT.
       UPCR-900.
             PERFORM REWRITE-CREDITOR.
       UPCR-999.
            EXIT.
      *
       READ-CREDITORS-LOCK SECTION.
       RDL-000.
           START CREDITOR-MASTER KEY NOT < CR-KEY.
       RDL-010.
           READ CREDITOR-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
               MOVE "CREDITOR BUSY ON READ-LOCK, 'ESC' TO EXIT."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDL-999.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITORS BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDL-010.
       RDL-999.
           EXIT.
      *
       REWRITE-CREDITOR SECTION.
       REWRDB-000.
             REWRITE CREDITOR-RECORD
                 INVALID KEY NEXT SENTENCE.
             IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                 MOVE "ACC.NUMBER :PAYMENT" TO WS-DAILY-1ST
                 MOVE CR-ACCOUNT-NUMBER     TO WS-DAILY-2ND
                 MOVE "NOT UPDATED"         TO WS-DAILY-3RD
                 MOVE " "                   TO WS-DAILY-4TH
                 PERFORM WRITE-DAILY.
             IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITORS BUSY ON RE-WRITE, 'ESC' TO RETRY" 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO REWRDB-000.
       REWRDB-999.
            EXIT.
      *
       READ-CRJRN-TRANSACTIONS SECTION.
       RCINV-005.
           MOVE CRTR-INV-NO TO CRJRN-INV-NO.
           START CRJRN-FILE KEY NOT < CRJRN-INV-NO
              INVALID KEY NEXT SENTENCE.
       RCINV-010.
           READ CRJRN-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
              MOVE "NO CR-JRN RECORD, DISCOUNT NOT TAKEN FROM GL"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE CRJRN-INV-NO TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 23 TO WS-CRJRN-ST1
              GO TO RCINV-999.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CR-JRN BUSY ON READ-NEXT-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RCINV-010.
           IF CRJRN-INV-NO > CRTR-INV-NO
              MOVE 23 TO WS-CRJRN-ST1
              GO TO RCINV-999.
           IF CRJRN-INV-NO NOT = CRTR-INV-NO
              GO TO RCINV-010.
           IF CRJRN-INV-NO = CRTR-INV-NO
            IF CRJRN-CRACC-NUMBER NOT = CR-ACCOUNT-NUMBER
              GO TO RCINV-010.
           IF CRJRN-INV-NO = CRTR-INV-NO
            IF CRJRN-CRACC-NUMBER = CR-ACCOUNT-NUMBER
              GO TO RCINV-999.
           GO TO RCINV-010.
       RCINV-999.
           EXIT.
      *
       WRITE-GLSETT-DISCOUNTS SECTION.
       WGLSD-000.
           IF WS-DIST-DISCOUNT = 0
               GO TO WGLSD-999.
           MOVE 3010 TO POS
           DISPLAY "WRITING GL-ACCOUNT DISCOUNTS .........     " AT POS.
           MOVE 0 TO SUB-1.
       WGLSD-001.
           ADD 1 TO SUB-1.
           IF SUB-1 > 400
               MOVE 1 TO SUB-1
               GO TO WGLSD-900.
           IF WS-REFNUM (SUB-1) = " "
               GO TO WGLSD-900.
           IF WS-APPLY (SUB-1) = "A"
            IF WS-DISCOUNT (SUB-1) > 0
              GO TO WGLSD-002.
           GO TO WGLSD-001.
       WGLSD-002.
           MOVE WS-REFNUM (SUB-1) TO CRTR-INV-NO.
           PERFORM READ-CRJRN-TRANSACTIONS.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
               MOVE "CAN'T FIND CRJRN, GOING TO WRITE TO 50-200-05-00"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE GLPA-GLTRANSNO    TO GLTRANS-TRANS
               ADD 1                  TO GLPA-GLTRANSNO
               PERFORM REWRITE-GLPARAMETER
               MOVE WS-BATCH          TO GLTRANS-REFERENCE
               MOVE 8                 TO GLTRANS-TYPE
               MOVE WS-DATE           TO GLTRANS-DATE
               MOVE 1 TO SUB-2
               MOVE "50-200-05-00"    TO CRJRN-GLACC (SUB-2)
               MOVE "50-200-05-00"    TO GLTRANS-ACCOUNT-NUMBER
               COMPUTE GLTRANS-AMOUNT = WS-DISCOUNT (SUB-1) * -1
               MOVE "SUP"             TO WS-CR1
               MOVE CR-ACCOUNT-NUMBER TO WS-CRACC
               MOVE "DS"              TO WS-CR2
               MOVE CRTR-INV-NO       TO WS-CRINV
               MOVE LINE-DESCRIPTION  TO GLTRANS-LINE-DESC
               IF WS-CURRENTGLPER = WS-CURRENTPER
                  MOVE CRTR-PERIOD    TO GLTRANS-PERIOD
                  PERFORM WGLSD-015
                  PERFORM UPGL-005 THRU UPGL-010
                  SUBTRACT WS-DISCOUNT (SUB-1) FROM GL-BALANCE
                                                    GL-PER (SUB-3)
                  PERFORM UPGL-900
                  PERFORM UPGLH-005 THRU UPGLH-010
                  SUBTRACT WS-DISCOUNT (SUB-1) FROM GL-BALANCE
                                                    GL-PER (SUB-3)
                  PERFORM UPGLH-900
                  PERFORM UPGLSH-005 THRU UPGLSH-010
                  SUBTRACT WS-DISCOUNT (SUB-1) FROM GL-BALANCE
                                                    GL-PER (SUB-3)
                  PERFORM UPGLSH-900
                  GO TO WGLSD-001
               ELSE
                  MOVE "F"            TO GLTRANS-FUTURE
                  MOVE CRTR-NO        TO GLTRANS-NO
                  PERFORM WGLSD-015
                  GO TO WGLSD-001.
           MOVE 0 TO SUB-2.
           MOVE WS-DISCOUNT (SUB-1) TO WS-WORKTOTAL-CR.
       WGLSD-003.
           ADD 1 TO SUB-2.
           IF SUB-2 > 10
              MOVE 1 TO SUB-2
              GO TO WGLSD-001.
           IF WS-WORKTOTAL-CR = 0
              MOVE 1 TO SUB-2
              GO TO WGLSD-001.
           IF CRJRN-GLACC (SUB-2) > " "
            IF CRJRN-GLDISC (SUB-2) > 0
              GO TO WGLSD-010.
           GO TO WGLSD-003.
       WGLSD-010.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO       TO GLTRANS-TRANS.
           ADD 1                     TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE WS-BATCH             TO GLTRANS-REFERENCE.
           MOVE 8                    TO GLTRANS-TYPE.
           IF WS-CURRENTGLPER = WS-CURRENTPER
               MOVE " "              TO GLTRANS-FUTURE
           ELSE
               MOVE "F"              TO GLTRANS-FUTURE.
           MOVE WS-CURRENTPER        TO GLTRANS-NO.
           MOVE WS-DATE              TO GLTRANS-DATE.
           MOVE CRJRN-GLACC (SUB-2)  TO GLTRANS-ACCOUNT-NUMBER.

           IF CRJRN-GLDISC (SUB-2) NOT > WS-WORKTOTAL-CR
             SUBTRACT CRJRN-GLDISC (SUB-2) FROM WS-WORKTOTAL-CR
             MOVE CRJRN-GLDISC (SUB-2) TO GLTRANS-AMOUNT
           ELSE
             MOVE WS-WORKTOTAL-CR    TO GLTRANS-AMOUNT
             MOVE 0                  TO WS-WORKTOTAL-CR.

           COMPUTE GLTRANS-AMOUNT = GLTRANS-AMOUNT * -1
           MOVE "SUP"                TO WS-CR1
           MOVE CR-ACCOUNT-NUMBER    TO WS-CRACC
           MOVE "DS"                 TO WS-CR2
           MOVE CRJRN-INV-NO         TO WS-CRINV
           MOVE LINE-DESCRIPTION     TO GLTRANS-LINE-DESC.
       WGLSD-015.
           WRITE GLTRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS WRITE ERROR WGLSD-015, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WGLSD-015.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS DISC FILE BUSY ON WRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WGLSD-015.
           COMPUTE GLTRANS-AMOUNT = GLTRANS-AMOUNT * -1.
       WGLSD-020.
           IF WS-CURRENTPER NOT = WS-CURRENTGLPER
                   GO TO WGLSD-030.
           MOVE WS-CURRENTGLPER TO SUB-3
           PERFORM UPGL-005 THRU UPGL-010
           SUBTRACT GLTRANS-AMOUNT FROM GL-BALANCE
           SUBTRACT GLTRANS-AMOUNT FROM GL-PER (SUB-3)
           PERFORM UPGL-900
           PERFORM UPGLH-005 THRU UPGLH-010
           SUBTRACT GLTRANS-AMOUNT FROM GL-BALANCE
           SUBTRACT GLTRANS-AMOUNT FROM GL-PER (SUB-3)
           PERFORM UPGLH-900
           PERFORM UPGLSH-005 THRU UPGLSH-010
           SUBTRACT GLTRANS-AMOUNT FROM GL-BALANCE
           SUBTRACT GLTRANS-AMOUNT FROM GL-PER (SUB-3)
           PERFORM UPGLSH-900.
       WGLSD-030.
           GO TO WGLSD-003.
       WGLSD-900.
           UNLOCK CRJRN-FILE.
       WGLSD-999.
           EXIT.
      *
       RECALC-GLDISC SECTION.
       RGD-005.
           MOVE 0 TO SUB-2.
           MOVE 0 TO WS-CRJRN-CALC-DISC WS-CRJRN-ALLOC-DISC.
           MOVE WS-DISCOUNT (SUB-1) TO WS-CRJRN-DISC.
       RDG-010.
           ADD 1 TO SUB-2.
           IF SUB-2 > 10
              MOVE 1 TO SUB-2
              GO TO RDG-999.
           IF CRJRN-GLACC (SUB-2) = " "
              GO TO RDG-999.
           COMPUTE WS-CRJRN-CALC-DISC = 
                  WS-CRJRN-DISC - WS-CRJRN-ALLOC-DISC.
           IF WS-CRJRN-CALC-DISC NOT > CRJRN-GLDISC (SUB-2)
               SUBTRACT WS-CRJRN-CALC-DISC FROM CRJRN-GLDISC (SUB-2)
               ADD WS-CRJRN-CALC-DISC        TO WS-CRJRN-ALLOC-DISC
           ELSE
               ADD CRJRN-GLDISC (SUB-2)       TO WS-CRJRN-ALLOC-DISC
               MOVE 0                         TO CRJRN-GLDISC (SUB-2).
           IF WS-CRJRN-DISC = WS-CRJRN-ALLOC-DISC
               GO TO RDG-999.
           GO TO RDG-010.
       RDG-999.
           EXIT.
      *
       UPDATE-DISTRIBUTION SECTION.
       UPDIS-000.
            IF WS-RUN-PAYMENTS = 0
             IF WS-RUN-DEBITS = 0
              IF WS-RUN-CREDITS = 0
               IF WS-RUN-DISCOUNT = 0
                  MOVE "AMOUNTS = ZERO AT DISTRIBUTION" TO WS-MESSAGE
                  PERFORM ERROR-MESSAGE
                  GO TO UPDIS-999.
           MOVE 3010 TO POS.
           DISPLAY "UPDATING GL-CREDITORS CONTROL & BANK A/C......     "
           AT POS.
       UPDIS-950.
           PERFORM UPDATE-GL-CREDITOR-ACC
           IF WS-CURRENTPER NOT = WS-CURRENTGLPER
               GO TO UPDIS-960.
           MOVE GL-NUMBER   TO WS-GLNUMBER
           MOVE WS-GLHEADER TO GL-NUMBER
           PERFORM UGLCA-010 THRU UGLCA-900
           MOVE WS-HEAD-SUB TO GL-NUMBER
           PERFORM UGLCA-010 THRU UGLCA-900.
       UPDIS-960.
           PERFORM UPDATE-GL-BANK-ACCOUNT
           IF WS-CURRENTPER NOT = WS-CURRENTGLPER
               GO TO UPDIS-970.
           MOVE GL-NUMBER   TO WS-GLNUMBER
           MOVE WS-GLHEADER TO GL-NUMBER
           PERFORM UPGLBANK-010 THRU UPGLBANK-900
           MOVE WS-HEAD-SUB TO GL-NUMBER
           PERFORM UPGLBANK-010 THRU UPGLBANK-900.
       UPDIS-970.
      *    PERFORM WRTR-900.
           MOVE 3010 TO POS.
           DISPLAY "                                                   "
           AT POS.
       UPDIS-999.
           EXIT.
      *
       WRITE-GLTRANS SECTION.
       WRTR-010.
           PERFORM READ-GLPARAMETER-LOCK
           MOVE GLPA-GLTRANSNO      TO GLTRANS-TRANS
           ADD 1                    TO GLPA-GLTRANSNO
           PERFORM REWRITE-GLPARAMETER.
           MOVE WS-BATCH            TO GLTRANS-REFERENCE
           MOVE 2                   TO GLTRANS-TYPE
           IF WS-CURRENTGLPER = WS-CURRENTPER
               MOVE " "             TO GLTRANS-FUTURE
           ELSE
               MOVE "F"             TO GLTRANS-FUTURE.
           MOVE WS-CURRENTGLPER     TO GLTRANS-NO
           MOVE CRTR-DATE           TO GLTRANS-DATE
           MOVE CRJRN-GLACC (SUB-2) TO GLTRANS-ACCOUNT-NUMBER
           COMPUTE GLTRANS-AMOUNT = CRJRN-GLAMT (SUB-2) * -1
           MOVE "SUP"               TO WS-CR1
           MOVE CRTR-ACC-NUMBER     TO WS-CRACC
           MOVE "PY"                TO WS-CR2
           MOVE CRTR-INV-NO         TO WS-CRINV
           MOVE LINE-DESCRIPTION    TO GLTRANS-LINE-DESC.
       WRTR-015.
           WRITE GLTRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS WRITE ERROR WRTR-015, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRTR-015.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS FILE BUSY ON WRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRTR-015.
       WRTR-900.
      *     CLOSE GLTRANS-FILE.
       WRTR-999.
           EXIT.
      *
       UPDATE-GLMASTER SECTION.
       UPGL-000.
           IF WS-CURRENTPER NOT = WS-CURRENTGLPER
               GO TO UPGL-999.
           MOVE WS-CURRENTGLPER TO SUB-3.
       UPGL-005.
           MOVE CRJRN-GLACC (SUB-2) TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGL-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLMASTER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLMASTER RECORD BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGL-010.
       UPGL-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLMASTER RECORD BUSY ON WRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGL-900.
       UPGL-999.
           EXIT.
     *
       UPDATE-GLHEADER SECTION.
       UPGLH-005.
           MOVE CRJRN-GLACC (SUB-2) TO WS-GLNUMBER.
           MOVE WS-GLHEADER TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLHEADER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLHEADER FILE BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLH-010.
       UPGLH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLHEADER RECORD BUSY ON WRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLH-900.
       UPGLH-999.
           EXIT.
     *
       UPDATE-GLSUBHEADER SECTION.
       UPGLSH-005.
           MOVE CRJRN-GLACC (SUB-2) TO WS-GLNUMBER
           MOVE WS-HEAD-SUB         TO GL-NUMBER
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLSH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLSUBHEAD FILE DOES'NT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLSUBHEADER FILE BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLSH-010.
       UPGLSH-020.
           SUBTRACT CRJRN-GLDISC (SUB-2) FROM GL-BALANCE
           SUBTRACT CRJRN-GLDISC (SUB-2) FROM GL-PER (SUB-3).
       UPGLSH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLSUBHEAD FILE BUSY ON WRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLSH-900.
       UPGLSH-999.
           EXIT.
      *
       UPDATE-GL-CREDITOR-ACC SECTION.
       UGLCA-000.
           IF WS-CURRENTPER NOT = WS-CURRENTGLPER
               GO TO UGLCA-950.
           MOVE WS-CURRENTGLPER TO SUB-3.
       UGLCA-005.
           MOVE GLPA-GLCRED-NO TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UGLCA-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLCRED-ACC DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UGLCA-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLCRED-ACC BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE GL-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UGLCA-010.
           ADD WS-RUN-PAYMENTS      TO GL-BALANCE
                                       GL-PER (SUB-3).
           ADD WS-RUN-DISCOUNT      TO GL-BALANCE
                                       GL-PER (SUB-3).
           ADD WS-RUN-CREDITS       TO GL-BALANCE
                                       GL-PER (SUB-3).
           ADD WS-RUN-DEBITS        TO GL-BALANCE
                                       GL-PER (SUB-3).
       UGLCA-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UGLCA-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLCRED-ACC BUSY ON REWRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UGLCA-900.
       UGLCA-950.
           IF WS-RUN-PAYMENTS = 0
              GO TO UGLCA-957.
       UGLCA-955.
           PERFORM READ-GLPARAMETER-LOCK
           MOVE GLPA-GLTRANSNO     TO GLTRANS-TRANS
           ADD 1 TO GLPA-GLTRANSNO
           PERFORM REWRITE-GLPARAMETER.
           MOVE 2                  TO GLTRANS-TYPE
           MOVE WS-BATCH           TO GLTRANS-REFERENCE.
           IF WS-CURRENTGLPER = WS-CURRENTPER
               MOVE " "            TO GLTRANS-FUTURE
           ELSE
               MOVE "F"            TO GLTRANS-FUTURE.
           MOVE WS-CURRENTPER      TO GLTRANS-NO
           MOVE WS-DATE            TO GLTRANS-DATE
           MOVE GLPA-GLCRED-NO     TO GLTRANS-ACCOUNT-NUMBER
           MOVE WS-RUN-PAYMENTS    TO GLTRANS-AMOUNT
           MOVE "PAY"              TO WS-CR1
           MOVE WS-BATCH           TO WS-CRACC
           MOVE "  "               TO WS-CR2
           MOVE "BATCH POST"       TO WS-CRINV
           MOVE LINE-DESCRIPTION   TO GLTRANS-LINE-DESC

           PERFORM WRTR-015.
       UGLCA-957.
           IF WS-RUN-DISCOUNT = 0
              GO TO UGLCA-960.
           PERFORM READ-GLPARAMETER-LOCK
           MOVE GLPA-GLTRANSNO     TO GLTRANS-TRANS
           ADD 1                   TO GLPA-GLTRANSNO
           PERFORM REWRITE-GLPARAMETER.
           MOVE 8                  TO GLTRANS-TYPE
           MOVE WS-BATCH           TO GLTRANS-REFERENCE.
           IF WS-CURRENTGLPER = WS-CURRENTPER
               MOVE " "            TO GLTRANS-FUTURE
           ELSE
               MOVE "F"            TO GLTRANS-FUTURE.
           MOVE WS-CURRENTPER      TO GLTRANS-NO
           MOVE WS-DATE            TO GLTRANS-DATE
           MOVE GLPA-GLCRED-NO     TO GLTRANS-ACCOUNT-NUMBER
           MOVE WS-RUN-DISCOUNT    TO GLTRANS-AMOUNT
           MOVE "PAY"              TO WS-CR1
           MOVE WS-BATCH           TO WS-CRACC
           MOVE "DS"               TO WS-CR2
           MOVE "BATCH POST"       TO WS-CRINV
           MOVE LINE-DESCRIPTION   TO GLTRANS-LINE-DESC

           PERFORM WRTR-015.
       UGLCA-960.
           IF WS-RUN-DEBITS = 0
              GO TO UGLCA-965.
           PERFORM READ-GLPARAMETER-LOCK
           MOVE GLPA-GLTRANSNO     TO GLTRANS-TRANS
           ADD 1                   TO GLPA-GLTRANSNO
           PERFORM REWRITE-GLPARAMETER.
           MOVE 4                  TO GLTRANS-TYPE
           MOVE WS-BATCH           TO GLTRANS-REFERENCE.
           IF WS-CURRENTGLPER = WS-CURRENTPER
               MOVE " "            TO GLTRANS-FUTURE
           ELSE
               MOVE "F"            TO GLTRANS-FUTURE.
           MOVE WS-CURRENTPER      TO GLTRANS-NO
           MOVE WS-DATE            TO GLTRANS-DATE
           MOVE GLPA-GLCRED-NO     TO GLTRANS-ACCOUNT-NUMBER
           MOVE WS-RUN-DEBITS      TO GLTRANS-AMOUNT
           MOVE "JRN"              TO WS-CR1
           MOVE WS-BATCH           TO WS-CRACC
           MOVE "DR"               TO WS-CR2
           MOVE "BATCH POST"       TO WS-CRINV
           MOVE LINE-DESCRIPTION   TO GLTRANS-LINE-DESC

           PERFORM WRTR-015.
       UGLCA-965.
           IF WS-RUN-CREDITS = 0
              GO TO UGLCA-999.
           PERFORM READ-GLPARAMETER-LOCK
           MOVE GLPA-GLTRANSNO     TO GLTRANS-TRANS
           ADD 1                   TO GLPA-GLTRANSNO
           PERFORM REWRITE-GLPARAMETER.
           MOVE 5                  TO GLTRANS-TYPE
           MOVE WS-BATCH           TO GLTRANS-REFERENCE.
           IF WS-CURRENTGLPER = WS-CURRENTPER
               MOVE " "            TO GLTRANS-FUTURE
           ELSE
               MOVE "F"            TO GLTRANS-FUTURE.
           MOVE WS-CURRENTPER      TO GLTRANS-NO
           MOVE WS-DATE            TO GLTRANS-DATE
           MOVE GLPA-GLCRED-NO     TO GLTRANS-ACCOUNT-NUMBER
           MOVE WS-RUN-CREDITS     TO GLTRANS-AMOUNT
           MOVE "JRN"              TO WS-CR1
           MOVE WS-BATCH           TO WS-CRACC
           MOVE "CR"               TO WS-CR2
           MOVE "BATCH POST"       TO WS-CRINV
           MOVE LINE-DESCRIPTION   TO GLTRANS-LINE-DESC

           PERFORM WRTR-015.
       UGLCA-999.
           EXIT.
      *
       UPDATE-GL-BANK-ACCOUNT SECTION.
       UPGLBANK-000.
           IF WS-RUN-PAYMENTS = 0
               GO TO UPGLBANK-999.
           IF WS-CURRENTPER NOT = WS-CURRENTGLPER
               GO TO UPGLBANK-955.
           MOVE WS-CURRENTGLPER TO SUB-3.
       UPGLBANK-005.
           MOVE GLPA-GLBANK TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLBANK-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLBANK-ACC DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLBANK-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLBANK-ACC BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLBANK-010.
           SUBTRACT WS-RUN-PAYMENTS FROM GL-BALANCE
                                         GL-PER (SUB-3).
       UPGLBANK-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLBANK-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLBANK-ACC BUSY ON REWRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLBANK-900.
       UPGLBANK-955.
           PERFORM READ-GLPARAMETER-LOCK
           MOVE GLPA-GLTRANSNO     TO GLTRANS-TRANS
           ADD 1                   TO GLPA-GLTRANSNO
           PERFORM REWRITE-GLPARAMETER.
           MOVE 2                  TO GLTRANS-TYPE
           MOVE WS-BATCH           TO GLTRANS-REFERENCE.
           IF WS-CURRENTGLPER = WS-CURRENTPER
               MOVE " "            TO GLTRANS-FUTURE
           ELSE
               MOVE "F"            TO GLTRANS-FUTURE.
           MOVE WS-CURRENTPER      TO GLTRANS-NO
           MOVE WS-DATE            TO GLTRANS-DATE
           MOVE GLPA-GLBANK        TO GLTRANS-ACCOUNT-NUMBER
           COMPUTE GLTRANS-AMOUNT = WS-RUN-PAYMENTS * -1
           MOVE "PAY"              TO WS-CR1
           MOVE WS-BATCH           TO WS-CRACC
           MOVE "  "               TO WS-CR2
           MOVE "BATCH POST"       TO WS-CRINV
           MOVE LINE-DESCRIPTION   TO GLTRANS-LINE-DESC

           PERFORM WRTR-015.
       UPGLBANK-999.
           EXIT.
      *
       WRITE-CASHBOOK SECTION.
       WRCB-000.
           MOVE 2910 TO POS
           DISPLAY "WRITING CASH-BOOK ENTRY........                "
               AT POS.
           PERFORM READ-GLPARAMETER.
           MOVE GLPA-CURRENT-CRPER TO SUB-3.
           IF GLPA-CURRENT-CRPER NOT = GLPA-CURRENT-CBPER
               GO TO WRCB-030.
           MOVE GLPA-GLBANK TO CB-NUMBER.
           START CB-MASTER KEY NOT < CB-KEY.
       WRCB-010.
           READ CB-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
               MOVE "CR-BANK-ACC DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WRCB-999.
           IF WS-CB-ST1 NOT = 0
               MOVE 0 TO WS-CB-ST1
               MOVE "CR-BANK-ACC BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WRCB-010.
           SUBTRACT WS-PAYAMT FROM CB-BALANCE
                                   CB-PER (SUB-3).
       WRCB-020.
           REWRITE CB-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
               GO TO WRCB-999.
           IF WS-CB-ST1 NOT = 0
               MOVE 0 TO WS-CB-ST1
               MOVE "CR-BANK-ACC BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WRCB-020.
       WRCB-030.
           PERFORM READ-GLPARAMETER-LOCK
           MOVE GLPA-CBTRANSNO     TO CBTRANS-TRANS
           ADD 1                   TO GLPA-CBTRANSNO
           PERFORM REWRITE-GLPARAMETER.
           MOVE 1                  TO CBTRANS-TYPE
           MOVE WS-BATCH           TO CBTRANS-REFERENCE.
           IF GLPA-CURRENT-CRPER NOT = GLPA-CURRENT-CBPER
                MOVE "F"           TO CBTRANS-FUTURE
           ELSE
                MOVE " "           TO CBTRANS-FUTURE.
           MOVE SUB-3              TO CBTRANS-NO
           MOVE GLPA-GLBANK        TO CBTRANS-CBMASTER
           MOVE WS-DATE            TO CBTRANS-DATE
           MOVE "C"                TO CBTRANS-TYPE-OF-POST
           MOVE "N"                TO CBTRANS-ALLOCATED
           MOVE GLPA-GLCRED-NO     TO CBTRANS-ACCOUNT-NUMBER
           MOVE WS-PAYAMT          TO CBTRANS-AMOUNT
           COMPUTE CBTRANS-AMOUNT = CBTRANS-AMOUNT * -1
           MOVE "A/C"              TO WS-CR1
           MOVE CR-ACCOUNT-NUMBER  TO WS-CRACC
           MOVE "PMT"              TO WS-CR2
           MOVE WS-CHEQUENO        TO WS-CRINV
           MOVE LINE-DESCRIPTION   TO CBTRANS-LINE-DESC

           PERFORM WRITE-CBTRANS.
           MOVE 2910 TO POS
           DISPLAY "                                                 "
              AT POS.
       WRCB-999.
           EXIT.
      *
       WRITE-CBTRANS SECTION.
       WCBTR-015.
           WRITE CBTRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-CBTRANS-ST1
              MOVE "CBTRANS WRITE ERROR WCBTR-015, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WCBTR-015.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE 0 TO WS-CBTRANS-ST1
              MOVE "CBTRANS FILE BUSY ON WRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WCBTR-015.
       WCBTR-999.
           EXIT.
      *
       REWRITE-GLPARAMETER SECTION.
       REWP-000.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER BUSY REWP-000, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RP-000.
       REWP-999.
           EXIT.
      *
       PRINT-AUTO-ALLOCATIONS SECTION.
       PAA-000.
           IF WS-HAVE-PRINTED = "Y"
              GO TO PAA-010.
       PAA-002.
           MOVE "Y" TO WS-HAVE-PRINTED.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       PAA-010.
           IF LINE-CNT < 60
              GO TO PAA-020.
           ADD 1              TO PAGE-CNT
           MOVE PAGE-CNT      TO A-H1-PAGE
           MOVE WS-CURRENTPER TO A-H1-PERIOD.
           MOVE COMP-DIG1     TO CO-DIG1
           MOVE COMP-DIG2     TO CO-DIG2.
           
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM A-HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM A-HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM A-HEAD3 AFTER 2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM A-HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE 7 TO LINE-CNT.
       PAA-020.
           MOVE CRTR-TRANS               TO A-D-TRANSNO
           MOVE CRTR-TYPE                TO A-D-TRANSCODE
           MOVE WS-TYPE-DESC (CRTR-TYPE) TO A-D-TRANSDESC
           MOVE CRTR-ACC-NUMBER          TO A-D-ACCOUNTNO
           MOVE CRTR-REFERENCE           TO A-D-REFERENCE1
           MOVE CRTR-INV-NO              TO A-D-REFERENCE2
           MOVE CRTR-DATE                TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE             TO A-D-DATE
           MOVE CRTR-LOC-AMT             TO A-D-BEGIN
           MOVE CRTR-UNAPPLIED-AMT       TO A-D-OUTST.
           IF WS-TRANSTYPE = "2"
            IF CRTR-TYPE = 2
              COMPUTE CRTR-SETT-DISC = WS-SETT-DISC * -1
              MOVE CRTR-SETT-DISC      TO A-D-DISCOUNT.
           IF WS-TRANSTYPE = "2"
            IF CRTR-TYPE = 9
               MOVE 0                  TO A-D-DISCOUNT
               MOVE CRTR-LOC-AMT       TO A-D-APPLIED.
           IF WS-TRANSTYPE = "2"
            IF CRTR-TYPE = 8
               MOVE WS-DISCOUNT (SUB-1) TO A-D-DISCOUNT
              COMPUTE WS-WORK-FIELD = CRTR-LOC-AMT
                                    - CRTR-UNAPPLIED-AMT
              MOVE WS-WORK-FIELD        TO A-D-APPLIED.
           IF WS-TRANSTYPE = "2"
            IF CRTR-TYPE = 1 OR = 4 OR = 7
             MOVE WS-PAYMENT (SUB-1)  TO A-D-APPLIED
             MOVE WS-DISCOUNT (SUB-1) TO A-D-DISCOUNT.

           IF CRTR-TYPE = 2 OR = 5
              COMPUTE WS-WORK-FIELD = CRTR-LOC-AMT
                                    - CRTR-UNAPPLIED-AMT
              MOVE WS-WORK-FIELD       TO A-D-APPLIED.
           WRITE PRINT-REC FROM A-DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT.
       PAA-999.
           EXIT.
      *
       PRINT-AUTO-TOTALS SECTION.
       PAUT-000.
           IF WS-HAVE-PRINTED = "N"
                 MOVE "WS-HAVE-PRINTED = N." TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
              GO TO PAUT-999.
           IF LINE-CNT > 60
              PERFORM PAA-010.
           IF WS-RUN-PAYMENTS = 0
           AND WS-RUN-DISCOUNT = 0 
              AND WS-RUN-DEBITS = 0
              AND WS-RUN-CREDITS = 0
                 MOVE "AMOUNTS = ZERO AT PRINT-AUTO." TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO PAUT-999.
           MOVE "**** TOTAL PAYMENTS:" TO A-TOT-DESC
           COMPUTE WS-RUN-PAYMENTS = WS-RUN-PAYMENTS * -1
           MOVE WS-RUN-PAYMENTS        TO A-TOT-AMOUNT
           WRITE PRINT-REC FROM A-TOTAL-LINE AFTER 2
           MOVE " " TO PRINT-REC
           MOVE "**** TOTAL DISCOUNT:" TO A-TOT-DESC
           COMPUTE WS-RUN-DISCOUNT = WS-RUN-DISCOUNT * -1
           MOVE WS-RUN-DISCOUNT        TO A-TOT-AMOUNT
           WRITE PRINT-REC FROM A-TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           MOVE "**** TOTAL DEBITS  :" TO A-TOT-DESC
           MOVE WS-RUN-DEBITS          TO A-TOT-AMOUNT
           WRITE PRINT-REC FROM A-TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           MOVE "**** TOTAL CREDITS :" TO A-TOT-DESC
           MOVE WS-RUN-CREDITS         TO A-TOT-AMOUNT
           WRITE PRINT-REC FROM A-TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           MOVE WS-BATCH               TO A-TOT-DESC
           MOVE 0                      TO A-TOT-AMOUNT
           WRITE PRINT-REC FROM A-TOTAL-LINE AFTER 2
           MOVE " " TO PRINT-REC.
           
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
        PAUT-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE 2910 TO POS
           DISPLAY "PRINTING REMITTANCE ADVICES....." AT POS.
           MOVE " " TO WS-FOUND.
           MOVE 1 TO SUB-10.
           
           IF WS-PRINTERSELECTED = 1
               MOVE "/ctools/spl/CrRemitCo" TO WS-PRINTER
               MOVE WS-PRINTER              TO ALPHA-RATE
               MOVE 20                      TO SUB-1
               MOVE WS-CO-NUMBER            TO AL-RATE (SUB-1)
               MOVE ALPHA-RATE              TO WS-PRINTER W-FILENAME.
       PRR-001.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            IF WS-SPOOLER-ST1 NOT = 0
               MOVE
           "Print File is held by another Terminal, 'ESC' to Retry."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PRR-001.
           IF WS-PRINTERSELECTED = 1
               MOVE WTELL-PAUSE TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC BEFORE PAGE.
            MOVE 0 TO WS-NO-ACCS CR-ACCOUNT-NUMBER CRTR-ACC-NUMBER.
            MOVE 0 TO CRTR-DATE.
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
            GO TO PRR-002.
       PRR-010.
           MOVE CRTR-DUE-DATE TO WS-DUE-DATE.
           IF WS-DUE-DATE > WS-CHECK-DATE
               GO TO PRR-002.
      * CHANGED ABOVE AND BELOW LINES 22/1/2016 AS THE REMI PART WAS
      * OMITTING OLDER DATES THAN THE DUE DATE.  ONLY IF DUE = CHECK
      * THEN IT PRINTED THE ITEM IN REMI BUT IN CASH ALLOC ALWAYS WORKED
      *     MOVE CRTR-DUE-DATE TO WS-DUE-DATE.
      *     IF WS-DUE-YY > WS-CH-YY
      *         GO TO PRR-002.
      *     IF WS-DUE-MM > WS-CH-MM
      *         GO TO PRR-002.
      *     IF WS-DUE-DD > WS-CH-DD
      *         GO TO PRR-002.
           IF CRTR-ACC-NUMBER = CR-ACCOUNT-NUMBER
               GO TO PRR-060.
       PRR-040.
           PERFORM PRINT-TOTAL-LINE
           PERFORM READ-CREDITOR
      *
      *NEXT LINE USED FOR SETTING PAGE LENGTH TO 8 INCH 
      *
           MOVE WS-PRINT-8 TO REMI-EIGHT
           
           WRITE PRINT-REC FROM REMITTANCE-LINE AFTER PAGE
           MOVE " " TO PRINT-REC.
           Move Ws-Print-Bold   To CO-DIG1
           Move Ws-Print-Unbold To CO-DIG2.

           MOVE GLPA-NAME          TO COMPANY
           WRITE PRINT-REC FROM COMPANY-DETAIL-LINE AFTER 3
           MOVE " " TO PRINT-REC COMPANY-DETAIL-LINE
           MOVE GLPA-ADD1          TO COMPANY
           WRITE PRINT-REC FROM COMPANY-DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           MOVE GLPA-ADD2          TO COMPANY
           WRITE PRINT-REC FROM COMPANY-DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           MOVE GLPA-ADD3          TO COMPANY
           WRITE PRINT-REC FROM COMPANY-DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC.

           MOVE CR-ACCOUNT-NUMBER  TO A-SUM-ACC (SUB-10)
           MOVE "TO:"              TO D-TO
           MOVE CR-NAME            TO D-NAME
                                      A-SUM-NAME (SUB-10)
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 4
           MOVE " " TO PRINT-REC DETAIL-LINE

           MOVE CR-ADDRESS1        TO D-NAME
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC DETAIL-LINE

           MOVE CR-ADDRESS2        TO D-NAME
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC DETAIL-LINE.

           IF CR-ADDRESS3 = "      "
                MOVE " " TO PRINT-REC
                MOVE CR-POST-CODE  TO D-NAME
                WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
                GO TO PRR-045.
           MOVE " " TO PRINT-REC
           MOVE CR-ADDRESS3        TO D-NAME
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC DETAIL-LINE
           MOVE CR-POST-CODE       TO D-NAME
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
        PRR-045.
           MOVE " " TO PRINT-REC DETAIL-LINE
           ADD 1                   TO REMI-CNT
           MOVE REMI-CNT           TO A-NO
           MOVE CR-SUPPLIER-NUMBER TO A-ACC1
           MOVE CR-ACCOUNT-NUMBER  TO A-ACC2
           WRITE PRINT-REC FROM ACCOUNT-LINE AFTER 4
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM TRANS-HEAD AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           ADD 1 TO WS-NO-ACCS.
        PRR-060.
           MOVE CRTR-DUE-DATE            TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE             TO TRANS-DATE
           MOVE CRTR-INV-NO              TO TRANS-NO
           MOVE WS-TYPE-DESC (CRTR-TYPE) TO TRANS-TYPE
           MOVE CRTR-UNAPPLIED-AMT       TO TRANS-GROSS-AMT
           MOVE CRTR-SETT-DISC           TO TRANS-SETT-DISC
           COMPUTE WS-NETT-AMOUNT = CRTR-UNAPPLIED-AMT - CRTR-SETT-DISC
           MOVE WS-NETT-AMOUNT           TO TRANS-NETT-AMT.

           WRITE PRINT-REC FROM TRANS-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD CRTR-UNAPPLIED-AMT TO WS-ACCTOTAL
                                     WS-TOTAL.
           SUBTRACT CRTR-SETT-DISC FROM WS-ACCTOTAL.
           ADD CRTR-SETT-DISC TO WS-ACCDISC
                                 WS-DISC-TOTAL.
           IF WS-FOUND = " "
               MOVE "Y" TO WS-FOUND.
           ADD 1 TO LINE-CNT.
           
           PERFORM WRITE-CRREMI-TRANS.
           
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       PRINT-TOTAL-LINE SECTION.
       PT-005.
           IF WS-NO-ACCS = 0
                GO TO PT-999.
           WRITE PRINT-REC FROM UNDER-LINE AFTER 1
           MOVE " " TO PRINT-REC.
           IF WS-CAMS-CHEQUE = "P"
                MOVE "CHEQUE # :"  TO PAYMENT-TYPE
           ELSE
                MOVE " "           TO PAYMENT-DATE
                MOVE "CAMS PMT#:"  TO PAYMENT-TYPE.
           MOVE WS-CHEQUENO  TO PAYMENT-NO
           MOVE WS-ACCTOTAL  TO PAYMENT-AMT
           MOVE WS-ACCDISC   TO PAYMENT-SETT-DISC
           WRITE PRINT-REC FROM PAYMENT-LINE AFTER 1.
           
           MOVE WS-CHEQUENO        TO A-SUM-CHEQUE (SUB-10)
           MOVE WS-ACCTOTAL        TO A-SUM-AMT (SUB-10)
           MOVE WS-ACCDISC         TO A-SUM-DISC (SUB-10).
           
           IF WS-ACCDISC > 0
              ADD WS-ACCTOTAL TO WS-AMT-SUMMARY
              ADD 1           TO WS-NO-ACCS-DISC.
           
           IF WS-CAMS-CHEQUE = "T"
              MOVE " " TO PRINT-REC
              MOVE
           "** AN FNB CAMS PAYMENT WILL BE MADE WITHIN 14 DAYS OF **"
              TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              MOVE 
           "* THIS REMITTANCE, PLEASE CHECK YOUR BANK STATEMENT.  **"
              TO PRINT-REC
              WRITE PRINT-REC AFTER 1.

           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           MOVE 0 TO WS-ACCTOTAL
                     WS-ACCDISC.
           ADD 1 TO WS-CHEQUENO.
           ADD 1 TO SUB-10.
       PT-999.
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
           MOVE 2510 TO POS.
           DISPLAY "Account Number Being Processed:" AT POS.
           ADD 32 TO POS.
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
       RCR-999.
             EXIT.
      *
       READ-NEXT-CREDITOR SECTION.
       RCNL-010.
           READ CREDITOR-MASTER NEXT
                AT END NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 10
                GO TO RCNL-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITOR RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCNL-010.
           MOVE 2510 TO POS.
           DISPLAY "Account Number Being Processed:" AT POS.
           ADD 32 TO POS.
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
       RCNL-999.
             EXIT.
      *
       WRITE-TYPE-AUDIT SECTION.
       WTA-005.
           IF WS-AUTO-REMIT = "A"
              MOVE "CREDITOR AUTO PAYMNT" TO WS-DAILY-1ST
              MOVE "ALLOCATION RUN.     " TO WS-DAILY-2ND
              MOVE "Start of process    " TO WS-DAILY-3RD
              MOVE SPLIT-DIS-DATE         TO WS-DAILY-4TH-1
              MOVE SPLIT-TIME             TO WS-DAILY-4TH-2.
           IF WS-AUTO-REMIT = "R"
              MOVE "CREDITOR AUTO REMIT " TO WS-DAILY-1ST
              MOVE "RUN SUCCESSFULLY.   " TO WS-DAILY-2ND
              MOVE "Start of process    " TO WS-DAILY-3RD
              MOVE SPLIT-DIS-DATE         TO WS-DAILY-4TH-1
              MOVE SPLIT-TIME             TO WS-DAILY-4TH-2.
                 
           PERFORM WRITE-DAILY.
       WTA-999.
          EXIT.
      *
       READ-GLPARAMETER SECTION.
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
           IF GLPA-CR-REMIT = "Y"
            IF WS-AUTO-REMIT = "A"
              MOVE
           "REMITTANCES HAVE BEEN PRINTED THIS MONTH, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM END-900.
       RP-999.
           EXIT.
      *
       READ-GLPARAMETER-LOCK SECTION.
       RGLPL-000.
           MOVE 1 TO GLPA-RECORD.
       RGLPL-005.
           READ GLPARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO GLPARAMETER RECORD, 'ESC' TO EXT."
               CALL "LOCKKBD" USING WS-PRINTER
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RGLPL-005.
       RGLPL-999.
           EXIT.
      *
       READ-PARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 1 TO GLPA-RECORD.
       RPL-005.
           READ GLPARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO GLPARAMETER RECORD, 'ESC' TO RETRY."
               CALL "LOCKKBD" USING WS-PRINTER
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RPL-005.
           IF GLPA-CR-REMIT = "Y"
            IF WS-AUTO-REMIT = "A"
              MOVE
           "REMITTANCES HAVE BEEN PRINTED THIS MONTH, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM END-900.
       RPL-500.
           MOVE "Y" TO GLPA-CR-REMIT.
       RPL-550.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RPL-550.
       RPL-999.
           EXIT.
      *
       REWRITE-CRREM-RECORD SECTION.
       RSR-010.
           MOVE 2610 TO POS
           DISPLAY "WRITING CR-REMITTANCE RECORD: " AT POS
           ADD 30 TO POS
           DISPLAY CRREM-KEY AT POS.
           
           MOVE A-SUM-ACC (SUB-10)    TO CRREM-ACC-NUMBER.

           PERFORM READ-REMIT.
           
           MOVE A-SUM-ACC (SUB-10)    TO CRREM-ACC-NUMBER.
           MOVE WS-REMI-YY            TO CRREM-YY
           MOVE WS-REMI-MM            TO CRREM-MM
           MOVE WS-REMI-F-L           TO CRREM-F-L
           MOVE A-SUM-CHEQUE (SUB-10) TO CRREM-PMT-REF
           MOVE A-SUM-AMT (SUB-10)    TO CRREM-BAL-FROM-REMIT
           MOVE "N"                   TO CRREM-COMPLETE.
           IF NEW-CRREMNO = "Y"
              GO TO RSR-020.
       RSR-015.
          REWRITE CRREM-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-REMI-ST1 NOT = 0
              MOVE "CRREM RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSR-015.
          GO TO RSR-999.
       RSR-020.
          WRITE CRREM-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-REMI-ST1 = 23 OR 35 OR 49
              MOVE "REWRITING CRREM BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSR-015.
          IF WS-REMI-ST1 NOT = 0
              MOVE "CRREM RECORD BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-REMI-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE CRREM-ACC-NUMBER  TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RSR-020.
       RSR-999.
          EXIT.
      *
       WRITE-CRREMI-TRANS SECTION.
       WCRT-010.
           MOVE WS-REMI-YY            TO CRREMTR-YY
           MOVE WS-REMI-MM            TO CRREMTR-MM
           MOVE CR-ACCOUNT-NUMBER     TO CRREMTR-ACC-NUMBER
           MOVE CRTR-INV-NO           TO CRREMTR-INVNO
           MOVE CRTR-UNAPPLIED-AMT    TO CRREMTR-INV-AMT
           MOVE CRTR-SETT-DISC        TO CRREMTR-DISC-AMT
           MOVE "N"                   TO CRREMTR-COMPLETE.
       WCRT-020.
          WRITE CRREMTR-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-REMI-ST1 NOT = 0
              MOVE "CRREM TRANS BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WCRT-020.
       WCRT-999.
          EXIT.
      *
       READ-REMIT SECTION.
       R-GL-000.
             MOVE "Y"              TO NEW-CRREMNO.
             
             MOVE WS-REMI-YY       TO CRREM-YY
             MOVE WS-REMI-MM       TO CRREM-MM
             MOVE WS-REMI-F-L      TO CRREM-F-L
             START CRREMIT-FILE KEY NOT < CRREM-KEY
                INVALID KEY NEXT SENTENCE.
             IF WS-REMI-ST1 NOT = 0
                MOVE SPACES TO CRREM-RECORD
                PERFORM CLEAR-FORM
                GO TO R-GL-999.
       R-GL-010.
             READ CRREMIT-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-REMI-ST1 = 23 OR 35 OR 49
                MOVE SPACES             TO CRREM-RECORD
                PERFORM CLEAR-FORM
                MOVE WS-REMI-YY         TO CRREM-YY
                MOVE WS-REMI-MM         TO CRREM-MM
                MOVE A-SUM-ACC (SUB-10) TO CRREM-ACC-NUMBER
                MOVE "Y"                TO NEW-CRREMNO 
                MOVE SPACES             TO CRREM-RECORD
                GO TO R-GL-999.
             IF WS-REMI-ST1 NOT = 0
                MOVE "CRREM RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-GL-010.
                
             MOVE "N" TO NEW-CRREMNO.
       R-GL-999.
             EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
            MOVE 1 TO SUB-1.
       CLSC-005.
            MOVE 0   TO CRREM-AMT (SUB-1)
            MOVE " " TO CRREM-DESC (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 41
                 GO TO CLSC-005.
            MOVE 1 TO SUB-1.
       CLSC-010.
            MOVE 0 TO  CRREM-BAL-LAST
                       CRREM-PREV-PAID
                       CRREM-BAL-NOW
                       CRREM-BAL-FROM-REMIT.
           MOVE " " TO CRREM-COMPLETE.
       CLSC-500.
            PERFORM RELEASE-CRREM-RECORD.
       CLSC-999.
           EXIT.
      *
       RELEASE-CRREM-RECORD SECTION.
       REL-000.
           UNLOCK CRREMIT-FILE.
       REL-999.
           EXIT.
      *
       CHECK-SPOOLER SECTION.
       CP-000.          
           MOVE " " TO WS-MESSAGE
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 3010 TO POS
           DISPLAY "Queuing Print File........" AT POS.
                
           PERFORM OPEN-PRINT-FILES
           PERFORM QUEUE-PRINT-FILE
           MOVE SPACE TO W-SPOOLST
           MOVE SPACE TO W-SPOOLST2
           PERFORM CHECK-FOR-PAUSE
           PERFORM ERROR2-020
           PERFORM ERROR-020.

           MOVE "     Load Remittances, Set Printer To '6', Then"
               TO WS-MESSAGE1.
           PERFORM ERROR2-000.
           MOVE "  Switch Printer 'OFF And ON', Then Press 'ESC'"
               TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.

           PERFORM SEND-CONTROL-CHAR.
           PERFORM ERROR2-020
           PERFORM ERROR-020.
           MOVE 3010 TO POS.
           DISPLAY "Printing of Remittances in progress ......." AT POS.
      *
      * PRINTING COMPLETE
      *
           PERFORM CHECK-PAUSE-PRINT.
           PERFORM ERROR-020.
       
           MOVE "    Load Normal Paper, Set Printer To'7' Then"
               TO WS-MESSAGE1.
               PERFORM ERROR2-020
           MOVE "  Switch Printer 'OFF And ON' Then Press 'ESC'"
               TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
       
           PERFORM SEND-CONTROL-CHAR.
           PERFORM ERROR2-020
           PERFORM ERROR-020.
       CP-999.
           EXIT.
      *
       QUEUE-PRINT-FILE SECTION.
           MOVE 20         TO W-CBSPOOLERFILESPEC.
           MOVE Ws-Printer TO W-SPOOLERFILESPEC.
      *     MOVE "/ctools/spl/CrRemitPrt" TO W-SPOOLERFILESPEC.
           CALL "ADDQUEUEENTRY" USING
                                W-ERROR
                                W-QUEUENAME
                                W-QUEUENAMELENGTH
                                W-Q-NO-SERVER
                                W-PRIORITY
                                W-QUEUETYPE
                                W-QUEUEENTRYBUFFERSEND
                                W-QUEUEENTRYBUFFERLENGTH
                                W-DATE-TIME
                                W-REPEATTIME.
       QUEUE-PRINT-EXIT.
           EXIT.
      *
       OPEN-PRINT-FILES SECTION.
           MOVE SPACE                  TO W-QUEUEENTRYBUFFERSEND
      *     MOVE "[Win]<Spl>CrRemitPrt" TO W-FILENAME
           MOVE X"00"                  TO W-FDELETEAFTERPROC
           MOVE 0       TO W-CBFORMNAME
           MOVE 0       TO W-CBWHEELNAME
           MOVE 1       TO W-BYTE1
           MOVE 0       TO W-BYTE2
           MOVE X"00" TO W-BPRINTMODE
           MOVE X"00" TO W-FALIGNFORM
           MOVE X"00" TO W-FSECURITYMODE
           MOVE "SPL" TO W-QUEUENAME
           MOVE 3     TO W-QUEUENAMELENGTH
           MOVE 0     TO W-QUEUEENTRYHANDLE
           MOVE 123   TO W-QUEUEENTRYBUFFERLENGTH
           MOVE 11    TO W-STATUSBLOCKLENGTH
           MOVE "PARALLELCONTROL" TO W-PAR-QUEUENAME
           MOVE 15                TO W-PAR-QUEUENAMELENGTH
           MOVE "SPOOLERSTATUS"   TO W-STATUS-QUEUENAME
           MOVE 13                TO W-STATUS-QUEUENAMELEN
           MOVE "PARALLEL"        TO W-PRINTERNAME
           MOVE 8                 TO W-PRINTERNAMELEN
           MOVE X"FF"       TO W-Q-NO-SERVER
           MOVE 5           TO W-PRIORITY
           MOVE 1           TO W-QUEUETYPE
           MOVE X"00000000" TO W-DATE-TIME
           MOVE 0           TO W-REPEATTIME
           MOVE 100         TO W-DELAY
           MOVE 0           TO W-ZERO
           MOVE 255         TO W-SPOOLST-LEN.
       OPEN-PRINT-999.
           EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 1 TO SUB-1.
             MOVE 0 TO WS-PAYAMT
                       RUN-TOTAL
                       WS-WORKTOTAL
                       WS-UNAPPLIED-AMT.
       CF-010.
             IF WS-REFNUM (SUB-1) = " "
                GO TO CF-999.
             MOVE " " TO WS-APPLY (SUB-1)
                         WS-REFNUM (SUB-1)
                         WS-TYPE (SUB-1).
             MOVE 0   TO WS-BODDATE (SUB-1)
                         WS-AMTOFTYPE (SUB-1)
                         WS-PAYMENT (SUB-1)
                         WS-DISCOUNT (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 401
                 GO TO CF-010.
       CF-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O CREDITOR-MASTER.
           IF WS-CREDITOR-ST1 NOT = 0
              MOVE 0 TO WS-CREDITOR-ST1
              MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-001.
           OPEN I-O CRREMIT-FILE.
           IF WS-REMI-ST1 NOT = 0
               MOVE "CRREMIT-FILE BUSY ON OPEN, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-CRREMIT TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-001.
       OPEN-002.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
       OPEN-003.
           OPEN I-O CRREMIT-TRANS-FILE.
           IF WS-REMITTRANS-ST1 NOT = 0
               MOVE "CRREMIT-TRANS-FILE BUSY ON OPEN, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-CRREMITTRANS TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-003.
       OPEN-005.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.

           PERFORM READ-GLPARAMETER
           MOVE GLPA-NAME          TO COMPANY A-COMPANY CO-NAME
           MOVE GLPA-CURRENT-GLPER TO WS-CURRENTGLPER
           MOVE GLPA-CURRENT-CRPER TO WS-CURRENTPER
           PERFORM ENTER-PERIOD-DATES.
       OPEN-014.
           OPEN I-O CRTR-FILE.
           IF WS-CRTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-CRTRANS-ST1
              MOVE "CRTRANS FILE BUSY ON OPEN,  'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
       OPEN-015.
           OPEN I-O CRJRN-FILE.
           IF WS-CRJRN-ST1 NOT = 0 
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CRJRN FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-015.
       OPEN-016.
           OPEN I-O CB-MASTER.
           IF WS-CB-ST1 NOT = 0
               MOVE "CBMASTER FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-CB-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-016.
       OPEN-017.
           OPEN I-O CBTRANS-FILE.
           IF WS-CBTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-CBTRANS-ST1
              MOVE "CB-TRANS BUSY ON OPEN, PRESS 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-017.
       OPEN-018.
           OPEN I-O CRCH-FILE.
           IF WS-CRCHEQUE-ST1 NOT = 0 
              MOVE "CR-CHEQUES FILE BUSY ON OPEN,  'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-CRCHEQUES TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-018.
       OPEN-020.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-GLTRANS-ST1
              CLOSE GLTRANS-FILE
              MOVE "GL-TRANS BUSY ON OPEN, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-020.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-100.
           WRITE PRINT-REC BEFORE PAGE
           WRITE PRINT-REC FROM COMPANY-LINE
           MOVE "**** SUMMARY OF CHEQUE PAYMENT DETAILS ****"
            TO REMI-NAME
           WRITE PRINT-REC FROM REMITTANCE-LINE
           MOVE " " TO PRINT-REC.
           
           MOVE
           "ACC #     ACCOUNT NAME                          " &
           "CAMS PMT#      AMOUNT    DISCOUNT" TO PRINT-REC.
           WRITE PRINT-REC AFTER 2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 2.
           MOVE 1 TO SUB-10.
       END-110.
           MOVE A-SUM-ACC (SUB-10)    TO SUM-ACC
           MOVE A-SUM-NAME (SUB-10)   TO SUM-NAME
           MOVE A-SUM-CHEQUE (SUB-10) TO SUM-CHEQUE
           MOVE A-SUM-AMT (SUB-10)    TO SUM-AMT
           MOVE A-SUM-DISC (SUB-10)   TO SUM-DISC.

           WRITE PRINT-REC FROM SUMMARY-LINE AFTER 1
           MOVE " " TO PRINT-REC SUMMARY-LINE.
           
           PERFORM REWRITE-CRREM-RECORD.
           
           ADD 1 TO SUB-10.
           IF SUB-10 < 201
            IF A-SUM-NAME (SUB-10) NOT = " "
             GO TO END-110.           
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 2.
       END-200.
           MOVE "Total cash before disc. :" TO T-NAME
           MOVE WS-TOTAL                    TO T-AMT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC TOTAL-LINE

           MOVE "Total Settlement Disc.  :" TO T-NAME
           MOVE WS-DISC-TOTAL               TO T-AMT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC TOTAL-LINE

           COMPUTE WS-TOTAL = WS-TOTAL - WS-DISC-TOTAL
           MOVE "Total Nett cash required:" TO T-NAME
           MOVE WS-TOTAL                    TO T-AMT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC TOTAL-LINE

           MOVE "Value Of Discounted Pmts:" TO T-NAME
           MOVE WS-AMT-SUMMARY              TO T-AMT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC TOTAL-LINE

           MOVE "Qty of Discount Payments:" TO T-NAME
           MOVE WS-NO-ACCS-DISC             TO T-AMT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC TOTAL-LINE.

           MOVE "Total Qty of Payments   :" TO T-NAME
           MOVE WS-NO-ACCS                  TO T-AMT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC TOTAL-LINE.
           
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 2.
           
           IF WS-PRINTERSELECTED = 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC BEFORE PAGE
               MOVE W-NULL TO PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC
               MOVE WTELL-PAUSE TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
      *
      *NEXT TWO LINES USED FOR RE-SETTING PAGE LENGTH TO 11INCH 
      *
           MOVE WS-PRINT-11 TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
       END-500.
           CLOSE CREDITOR-MASTER
                 CRREMIT-FILE
                 CRREMIT-TRANS-FILE
                 CRTR-FILE
                 CRCH-FILE.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
      *      IF WS-FOUND = "Y"
      *      IF WS-PRINTERSELECTED = 1
      *        PERFORM CHECK-SPOOLER.
           CLOSE GLPARAMETER-FILE.
       END-900.
           EXIT PROGRAM.
      *    STOP RUN.
       END-999.
           EXIT.
      *
       AUTO-END-OFF SECTION.
       AUTO-END-000.
           MOVE 2910 TO POS
           DISPLAY "ENDING AUTO ALLOCATION OF CASH......      " AT POS.
           IF WS-HAVE-PRINTED = "Y"
              PERFORM UPDATE-DISTRIBUTION
              PERFORM PRINT-AUTO-TOTALS
              CLOSE PRINT-FILE.
           MOVE 66 TO LINE-CNT.
           MOVE 0 TO PAGE-CNT REMI-CNT.
           CLOSE CRJRN-FILE
                 GL-MASTER
                 GLTRANS-FILE
                 GLPARAMETER-FILE
                 CREDITOR-MASTER
                 CRTR-FILE
                 CB-MASTER
                 CBTRANS-FILE.
           EXIT PROGRAM.
       AUTO-END-999.
           EXIT.
      *
       Copy "EnterPeriodDates".
       Copy "ComputeCRDatePeriod".
       Copy "CheckForPause".
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
       Copy "Error2Message".
       Copy "CTOSCobolAccept".
       Copy "WriteDailyExcep1".
      * END-OF-PROGRAM
