        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlChckRp.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectSlRegister".
         Copy "SelectSlMaster".
         Copy "SelectSlDistributions".
         Copy "SelectDrMaster".
         Copy "SelectDrTrans".
         Copy "SelectSlParameter".
           SELECT RANDOM-FILE ASSIGN TO WS-RANDOM-FILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-RANDOM-STATUS
               RECORD KEY IS RANDOM-KEY.
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdRegister.
           COPY ChlfdSales.
           COPY ChlfdDisTot.
           COPY ChlfdDebtor.
           COPY ChlfdDrTrans.
           COPY ChlfdParam.
           
       FD  RANDOM-FILE.
       01  RANDOM-REC.
           03  RANDOM-KEY.
               05  RANDOM-INVOICE       PIC 9(7).
           
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(255).

      *
       WORKING-STORAGE SECTION.
       77  WS-RANDOM-FILE       PIC X(30) VALUE
              "/ctools/spl/RandomInvCheck".
       77  WS-RANDOM-FILE-ind   PIC X(30) VALUE
              "/ctools/spl/RandomInvCheck.Ind".
       77  WS-VATABLE           PIC S9(8)V99 VALUE 0.
       77  WS-ACCEPT            PIC XX VALUE " ".
       77  WS-YYACCEPT          PIC X(4) VALUE " ".
       77  WS-VAT-PERC          PIC 99V99 VALUE 0.
       77  WS-PERC-DIS          PIC Z(8)9.99.
       77  WS-PERC              PIC S9(8)V99 VALUE 0.
       77  WS-RAND-ACCEPT       PIC X(11) VALUE " ".
       77  WS-RAND-DIS          PIC Z(8)9.99.
       77  WS-RAND              PIC S9(8)V99 VALUE 0.
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(7) VALUE " ".
       77  WS-RANGE2            PIC X(7) VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-RANDOM-WRITTEN    PIC X.
       01  W-DD                 PIC Z9.
       01  W-YY                 PIC ZZZ9.
       01  WS-VATABLE-INVOICES.
           03  WS-INVNO             PIC 9(5) VALUE 0.
           03  WS-TOTI-INVOICE      PIC S9(8)V99 VALUE 0.
           03  WS-TOTI-TAX          PIC S9(8)V99 VALUE 0.
           03  WS-TOTI-ADDON        PIC S9(8)V99 VALUE 0.
           03  WS-TOTI-NETT         PIC S9(8)V99 VALUE 0.
           03  WS-TOTI-COST         PIC S9(8)V99 VALUE 0.
           03  WS-IMARGIN           PIC S9(8)V99 VALUE 0.
           03  WS-IPERC             PIC S9(8)V99 VALUE 0.
       01  WS-VATABLE-CREDITS.
           03  WS-CRNO              PIC 9(5) VALUE 0.
           03  WS-TOTC-INVOICE      PIC S9(8)V99 VALUE 0.
           03  WS-TOTC-TAX          PIC S9(8)V99 VALUE 0.
           03  WS-TOTC-ADDON        PIC S9(8)V99 VALUE 0.
           03  WS-TOTC-NETT         PIC S9(8)V99 VALUE 0.
           03  WS-TOTC-COST         PIC S9(8)V99 VALUE 0.
           03  WS-CMARGIN           PIC S9(8)V99 VALUE 0.
           03  WS-CPERC             PIC S9(8)V99 VALUE 0.
       01  WS-EXPORT-INVOICES.
           03  WS-XPORT-INVNO       PIC 9(5) VALUE 0.
           03  WS-XPORTI-INVOICE    PIC S9(8)V99 VALUE 0.
           03  WS-XPORTI-TAX        PIC S9(8)V99 VALUE 0.
           03  WS-XPORTI-ADDON      PIC S9(8)V99 VALUE 0.
           03  WS-XPORTI-NETT       PIC S9(8)V99 VALUE 0.
           03  WS-XPORTI-COST       PIC S9(8)V99 VALUE 0.
           03  WS-XPORT-MARGIN      PIC S9(8)V99 VALUE 0.
           03  WS-XPORT-PERC        PIC S9(8)V99 VALUE 0.
       01  WS-EXPORT-CREDITS.
           03  WS-XPORT-CRNO        PIC 9(5) VALUE 0.
           03  WS-XPORTC-INVOICE    PIC S9(8)V99 VALUE 0.
           03  WS-XPORTC-TAX        PIC S9(8)V99 VALUE 0.
           03  WS-XPORTC-ADDON      PIC S9(8)V99 VALUE 0.
           03  WS-XPORTC-NETT       PIC S9(8)V99 VALUE 0.
           03  WS-XPORTC-COST       PIC S9(8)V99 VALUE 0.
       01 WS-SALES-ANALYSIS.
           03  WS-SALESAMT           PIC S9(8)V99 VALUE 0.
           03  WS-COST               PIC S9(8)V99 VALUE 0.
           03  WS-MARGIN             PIC S9(8)V99 VALUE 0.
           03  WS-PERC               PIC S9(4)V99.
           03  WS-VATABLE-SALES-PTD  PIC S9(8)V99 VALUE 0.
           03  WS-VATABLE-COST-PTD   PIC S9(8)V99 VALUE 0.
           03  WS-EXPORT-SALES-PTD   PIC S9(8)V99 VALUE 0.
           03  WS-EXPORT-COST-PTD    PIC S9(8)V99 VALUE 0.
           03  ANAL-TOT-SALESPTD     PIC S9(8)V99 VALUE 0.
           03  ANAL-TOT-COSTPTD      PIC S9(8)V99 VALUE 0.
           03  ANAL-TOT-MARGIN       PIC S9(8)V99 VALUE 0.
           03  ANAL-TOT-PERC         PIC S9(4)V99.
       01  DEBTOR-FIELDS.
           03  WS-DEBTOR-IMBALANCE  PIC X VALUE "N".
           03  WS-AMT-OF-INVOICE    PIC S9(8)V99 VALUE 0.
           03  WS-TOT-BALANCE       PIC S9(8)V99 VALUE 0.
           03  WS-TOT-CURRENT       PIC S9(8)V99 VALUE 0.
           03  WS-TOT-30DAY         PIC S9(8)V99 VALUE 0.
           03  WS-TOT-60DAY         PIC S9(8)V99 VALUE 0.
           03  WS-TOT-90DAY         PIC S9(8)V99 VALUE 0.
           03  WS-TOT-120DAY        PIC S9(8)V99 VALUE 0.
           03  WS-GRTOT-BALANCE     PIC S9(8)V99 VALUE 0.
           03  WS-GRTOT-CURRENT     PIC S9(8)V99 VALUE 0.
           03  WS-GRTOT-30DAY       PIC S9(8)V99 VALUE 0.
           03  WS-GRTOT-60DAY       PIC S9(8)V99 VALUE 0.
           03  WS-GRTOT-90DAY       PIC S9(8)V99 VALUE 0.
           03  WS-GRTOT-120DAY      PIC S9(8)V99 VALUE 0.
       01  WS-SALES-STATUS.
           03  WS-SALES-ST1         PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1          PIC 99.
       01  WS-DISTRIBUTION-STATUS.
           03  WS-DISTRIBUTION-ST1  PIC 99.
       01  WS-RANDOM-STATUS.
           03  WS-RANDOM-ST1        PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1        PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1   PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1       PIC 99.
       01  WEEK-DATE.
           03  WEEK-YY         PIC 9999.
           03  WEEK-MM         PIC 99.
           03  WEEK-DD         PIC 99.
       01  COMP-DATE.
           03  COMP-YY         PIC 9999.
           03  COMP-MM         PIC 99.
           03  COMP-DD         PIC 99.
       01  STORE-TERM.
         02  WS-TERM-OCCUR OCCURS 10.
           03  WS-ST-TYPE       PIC X.
           03  WS-ST-CODE       PIC X.
           03  WS-ST-TERM       PIC X(11).
       01  WS-TYPES.
           03  FILLER           PIC X(2) VALUE "IN".
           03  FILLER           PIC X(2) VALUE "  ".
           03  FILLER           PIC X(2) VALUE "  ".
           03  FILLER           PIC X(2) VALUE "PO".
           03  FILLER           PIC X(2) VALUE "  ".
           03  FILLER           PIC X(2) VALUE "CN".
           03  FILLER           PIC X(2) VALUE "  ".
           03  FILLER           PIC X(2) VALUE "  ".
           03  FILLER           PIC X(2) VALUE "  ".
       01  WS-TYPES-RED REDEFINES WS-TYPES.
           03  WS-TYPE-DESC     PIC X(2) OCCURS 9.
       01  PERCENT-LINE.
           03  WS-PERC-COMMENT      PIC X(23) VALUE " ".
           03  WS-PERC-NUM          PIC X(12).
           03  WS-PERC-COMMENT2     PIC X(25) VALUE " ".
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE "  DATE".
           03  H-DATE           PIC X(10).
           03  FILLER           PIC X(32) VALUE " ".
           03  FILLER           PIC X(70) VALUE 
           "SALES LEDGER CHECKING REPORT    ".
           03  FILLER           PIC X(6) VALUE "PAGE: ".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(4) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(49) VALUE " ".
           03  FILLER           PIC X(103) VALUE 
           "****************************   ".
       01  HEAD3.
           03  FILLER           PIC X(55) VALUE " ".
           03  FILLER           PIC X(18) VALUE "  INVOICE".
           03  FILLER           PIC X(16) VALUE " VAT     ADD-ON".
           03  FILLER           PIC X(30) VALUE
                  " NETT SALES        COST".
           03  FILLER           PIC X(33) VALUE "GROSS     %".
       01  HEAD4.
           03  FILLER           PIC X(55) VALUE " ".
           03  FILLER           PIC X(16) VALUE "   AMOUNT".
           03  FILLER           PIC X(23) VALUE "AMOUNT     AMOUNT".
           03  FILLER           PIC X(22) VALUE "AMOUNT      AMOUNT".
           03  FILLER           PIC X(39) VALUE "  AMOUNT  MARGIN".
       01  DETAIL-LINE.
           03  D-INVNO          PIC Z(5)9.
           03  FILLER           PIC X(1) VALUE " ".
           03  D-DATE           PIC X(10).
       01  TOTAL-REG-LINE.
           03  FILLER               PIC X(13) VALUE " ".
           03  TOT-REG-DESC         PIC X(22) VALUE " ".
           03  TOT-REG-NO           PIC Z(4)9.
           03  FILLER               PIC X(5) VALUE " ".
           03  FILLER               PIC X(8) VALUE "TOTALS:".
           03  TOT-REG-INVAMT       PIC Z(7)9.99.
           03  FILLER               PIC X(1) VALUE " ".
           03  TOT-REG-TAX          PIC Z(7)9.99.
           03  TOT-REG-TAX-ERR      PIC X(1) VALUE " ".
           03  TOT-REG-ADDON        PIC Z(7)9.99.
           03  FILLER               PIC X(1) VALUE " ".
           03  TOT-REG-NETT         PIC Z(7)9.99.
           03  FILLER               PIC X(1) VALUE " ".
           03  TOT-REG-COST         PIC Z(7)9.99.
           03  FILLER               PIC X(1) VALUE " ".
           03  TOT-REG-MARGIN       PIC Z(7)9.99-.
           03  TOT-REG-PERC         PIC Z(3)9.99-.
           03  FILLER               PIC X(6) VALUE " ".
       01  ANAL-HEAD4.
           03  FILLER         PIC X(64) VALUE " ".
           03  FILLER         PIC X(19) VALUE "SALES".
           03  FILLER         PIC X(13) VALUE "COSTS".
           03  FILLER         PIC X(20) VALUE "RAND MARGIN".
           03  FILLER         PIC X(16) VALUE "MARGIN %".
       01  ANAL-TOTALS.
           03  FILLER         PIC X(23) VALUE " ".
           03  D-DIVTOT       PIC X(20) VALUE " ".
           03  D-NAMETOT      PIC X(15) VALUE " ".
           03  D-SALESTOT     PIC Z(7)9.99-.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-COSTTOT      PIC Z(7)9.99-.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-MARGINTOT    PIC Z(7)9.99-.
           03  FILLER         PIC X(9) VALUE " ".
           03  D-PERCTOT      PIC Z(3)9.99-.
           03  FILLER         PIC X(6) VALUE " ".
       01  DIS-HEAD3.
           03  FILLER         PIC X(20) VALUE "TYPE ".
           03  FILLER         PIC X(9) VALUE "AMOUNT".
           03  FILLER         PIC X(20) VALUE "TYPE".
           03  FILLER         PIC X(9) VALUE "AMOUNT".
           03  FILLER         PIC X(20) VALUE "TYPE".
           03  FILLER         PIC X(14) VALUE "AMOUNT".
       01  DIS-DETAIL-LINE.
           03  DIS-NAME1      PIC X(15) VALUE " ".
           03  DIS-AMT1       PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  DIS-NAME2      PIC X(15) VALUE " ".
           03  DIS-AMT2       PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  DIS-NAME3      PIC X(15) VALUE " ".
           03  DIS-AMT3       PIC Z(7)9.99-.
           03  DIS-ERR        PIC X(10) VALUE " ".
           03  FILLER         PIC X(25) VALUE " ".
       01  DEBTOR-HEAD4.
           03  FILLER           PIC X(36) VALUE " ".
           03  FILLER           PIC X(25) VALUE "             BALANCE".
           03  FILLER           PIC X(13) VALUE " CURRENT".
           03  FILLER           PIC X(14) VALUE "31-60 DAY".
           03  FILLER           PIC X(13) VALUE "61-90 DAY".
           03  FILLER           PIC X(16) VALUE "91-120 DAY".
           03  FILLER           PIC X(15) VALUE "121+ DAY".
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
       01  GL-PRINT-LINE.
           03  GL-FILL1         PIC X(50) VALUE " ".
           03  FILLER           PIC X(5) VALUE " ".
           03  GL-FILL2         PIC X(50) VALUE " ".
           03  FILLER           PIC X(5) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 320 TO POS.
           DISPLAY "** Sales Ledger Checking Report **" AT POS.
           MOVE 420 TO POS.
           DISPLAY "**********************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-008.
           PERFORM OPEN-500.
       CONTROL-009.
           MOVE 0810 TO POS.
           DISPLAY "      The starting DAY to print from : [  ]" AT POS.
           ADD 40 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

           MOVE WS-ACCEPT TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WEEK-DD.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF WEEK-DD NOT > 0 AND NOT < 32
              DISPLAY "Enter a day between 1 & 31" AT POS
              MOVE "  " TO WS-ACCEPT
              GO TO CONTROL-009.
           MOVE WEEK-DD TO W-DD.
           DISPLAY W-DD AT POS.
       CONTROL-010.
           MOVE 1010 TO POS.
           DISPLAY "               The MONTH to print is : [  ]" AT POS.
           ADD 40 TO POS.
           MOVE WS-MM TO W-DD WS-ACCEPT.
           DISPLAY W-DD AT POS.

           MOVE W-DD      TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

           MOVE WS-ACCEPT TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WEEK-MM.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-009.
           IF WEEK-MM NOT > 0 AND NOT < 13
              MOVE 2510 TO POS
              DISPLAY "Enter a month between 1 & 12" AT POS
              MOVE "  " TO WS-ACCEPT
              GO TO CONTROL-010.
              
           MOVE WEEK-MM TO W-DD.
           MOVE 1050 TO POS
           DISPLAY W-DD AT POS.
       CONTROL-011.
           MOVE 1210 TO POS.
           DISPLAY "                The YEAR to print is : [    ]"
               AT POS.
           ADD 40 TO POS.
           MOVE WS-YY TO W-YY WS-YYACCEPT.
           DISPLAY W-YY AT POS.

           MOVE W-YY      TO CDA-DATA.
           MOVE 4         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-YYACCEPT.

           MOVE WS-YYACCEPT TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WEEK-YY.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           MOVE WEEK-YY TO W-YY.
           MOVE 1250 TO POS
           DISPLAY W-YY AT POS.
       CONTROL-015.
           MOVE 2510 TO POS.
           DISPLAY "The report is being compiled ......." AT POS.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM OPEN-FILES.
           PERFORM READ-BY-DATE.
      *     IF WS-RANDOM-WRITTEN = "Y"
               PERFORM PRINT-REG-ROUTINE
               PERFORM PRINT-REG-TOTALS.
      *         PERFORM DELETE-TRANS.
               
           PERFORM CONTROL-020.
           PERFORM PRINT-ANAL-ROUTINE.
           PERFORM PRINT-ANAL-TOTALS.
               
           PERFORM CONTROL-020.
           PERFORM PRINT-DISTRIBUTIONS.

           PERFORM CONTROL-020.
           PERFORM PRINT-DEBTOR-HEADINGS.
           PERFORM PRINT-DEBTOR-ROUTINE.
           PERFORM DEBTOR-GRAND-TOTALS.

           PERFORM CONTROL-020.
           PERFORM PRINT-GL.

           PERFORM END-OFF.
       CONTROL-020.
           MOVE 0810 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 0910 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1010 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1110 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1210 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2310 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2410 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS.
       CONTROL-999.
           EXIT.
      *
       PRINT-REG-ROUTINE SECTION.
       PREG-001.
           START INCR-REGISTER KEY NOT < INCR-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
              MOVE 88 TO WS-INCR-ST1
              GO TO PREG-999.
       PREG-002.
           PERFORM READ-RANDOM-RECORD.
            IF WS-RANDOM-ST1 = 10
              CLOSE RANDOM-FILE
              GO TO PREG-999.
       PREG-003.
           READ INCR-REGISTER
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
              MOVE "REG-ST1 = 2" TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PREG-002.
           IF WS-INCR-ST1 NOT = 0
             MOVE
           "REGISTER PREG LOCKED AT ANOTHER STATION, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE "LAST RECORD READ WAS:" TO WS-MESSAGE
             PERFORM ERROR-000
             MOVE 2855 TO POS
             DISPLAY D-INVNO AT POS
             PERFORM ERROR-010
             PERFORM ERROR1-020
             PERFORM ERROR-020
             MOVE WS-INCR-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO PREG-003.
           MOVE INCR-INVOICE TO D-INVNO.
       PREG-010.
           IF LINE-CNT > 60
               PERFORM PRINT-REG-HEADINGS.
       PREG-020.
           IF INCR-TRANS = 1
            IF INCR-SALES NOT = 6 AND NOT = 52
              ADD INCR-INVCRED-AMT  TO WS-TOTI-INVOICE
              ADD INCR-TAX          TO WS-TOTI-TAX
              ADD INCR-ADDONS       TO WS-TOTI-ADDON
              COMPUTE WS-TOTI-NETT = WS-TOTI-INVOICE - WS-TOTI-TAX - 
                   WS-TOTI-ADDON
              ADD INCR-INVCRED-COST TO WS-TOTI-COST
              ADD 1                 TO WS-INVNO
              GO TO PREG-900.
           IF INCR-TRANS = 6
            IF INCR-SALES NOT = 6 AND NOT = 52
              ADD INCR-INVCRED-AMT  TO WS-TOTC-INVOICE
              ADD INCR-TAX          TO WS-TOTC-TAX
              ADD INCR-ADDONS       TO WS-TOTC-ADDON
              COMPUTE WS-TOTC-NETT = WS-TOTC-INVOICE - WS-TOTC-TAX - 
                   WS-TOTC-ADDON
              ADD INCR-INVCRED-COST TO WS-TOTC-COST
              ADD 1                 TO WS-CRNO.
                
       PREG-900.
            MOVE 2410 TO POS
            DISPLAY "Invoices Being Read:           " AT POS
            ADD 25 TO POS
            DISPLAY RANDOM-INVOICE AT POS.

           MOVE 2450 TO POS
           DISPLAY "DATE:" AT POS
           ADD 6 TO POS
           DISPLAY D-DATE AT POS.
       PREG-950.
           IF INCR-SALES = 6 OR = 52
             GO TO PREG-960
           ELSE
             GO TO PREG-002.
       PREG-960.
           IF INCR-TRANS = 1
              ADD INCR-INVCRED-AMT  TO WS-XPORTI-INVOICE
              ADD INCR-TAX          TO WS-XPORTI-TAX
              ADD INCR-ADDONS       TO WS-XPORTI-ADDON
              COMPUTE WS-XPORTI-NETT = WS-XPORTI-INVOICE -
                   WS-XPORTI-TAX - WS-XPORTI-ADDON
              ADD INCR-INVCRED-COST TO WS-XPORTI-COST
              ADD 1                 TO WS-XPORT-INVNO.
           IF INCR-TRANS = 6
              ADD INCR-INVCRED-AMT  TO WS-XPORTC-INVOICE
              ADD INCR-TAX          TO WS-XPORTC-TAX
              ADD INCR-ADDONS       TO WS-XPORTC-ADDON
              COMPUTE WS-XPORTC-NETT = WS-XPORTC-INVOICE -
                   WS-XPORTC-TAX - WS-XPORTC-ADDON

              ADD INCR-INVCRED-COST TO WS-XPORTC-COST
              ADD 1                 TO WS-XPORT-CRNO.
           GO TO PREG-002.
       PREG-999.
           EXIT.
      *
       READ-BY-DATE SECTION.
       RBD-001.
           MOVE 2010 TO POS
           DISPLAY "1. READING INVOICE REGISTER SECTION..." AT POS.
           MOVE 2310 TO POS
           DISPLAY "READING BY DATE FIRST..." AT POS.
           MOVE WEEK-DATE TO INCR-DATE.
           START INCR-REGISTER KEY NOT < INCR-DATE
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
              MOVE 88 TO WS-INCR-ST1
      *        PERFORM PRINT-REG-HEADINGS
              GO TO RBD-900.
       RBD-003.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10 OR = 23
              PERFORM ERROR-020
              MOVE 2210 TO POS
              DISPLAY WS-MESSAGE AT POS
              PERFORM ERROR-000
              GO TO RBD-900.
           IF WS-INCR-ST1 NOT = 0
             MOVE 
         "RECORD RBD LOCKED AT ANOTHER STATION, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE "LAST RECORD READ WAS:" TO WS-MESSAGE
             PERFORM ERROR-000
             MOVE 2855 TO POS
             DISPLAY D-INVNO AT POS
             PERFORM ERROR-010
             PERFORM ERROR1-020
             PERFORM ERROR-020
             MOVE WS-INCR-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO RBD-003.
           MOVE INCR-INVOICE TO D-INVNO.
           IF INCR-TRANS NOT = 1 AND NOT = 6
              GO TO RBD-003.
           MOVE INCR-DATE TO COMP-DATE D-DATE.
           IF COMP-YY NOT = WEEK-YY
              GO TO RBD-900.
           IF COMP-MM NOT = WEEK-MM
              GO TO RBD-900.
           IF COMP-DD < WEEK-DD
              GO TO RBD-003.
       RBD-010.
           MOVE 2220 TO POS
           DISPLAY "DATE:" AT POS
           ADD 6 TO POS
           DISPLAY D-DATE AT POS
           ADD 15 TO POS
           DISPLAY "TRANS No:" AT POS
           ADD 10 TO POS
           DISPLAY D-INVNO AT POS.
           
           PERFORM WRITE-RANDOM-RECORD.
           
           GO TO RBD-003.
       RBD-900.
           CLOSE RANDOM-FILE
           PERFORM OPEN-035
           CLOSE INCR-REGISTER
           PERFORM OPEN-014.
       RBD-999.
           EXIT.
      *
       READ-RANDOM-RECORD SECTION.
       RRR-002.
            READ RANDOM-FILE NEXT
               AT END NEXT SENTENCE. 
            IF WS-RANDOM-ST1 = 10
               GO TO RRR-999.
            IF WS-RANDOM-ST1 NOT = 0
               MOVE "RANDOM RECORD BUSY ON READ." TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE RANDOM-REC TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-RANDOM-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RRR-002.
            
            MOVE RANDOM-INVOICE TO INCR-KEY.
       RRR-999.
            EXIT.
      *
       WRITE-RANDOM-RECORD SECTION.
       WRR-005.
           IF WS-RANDOM-WRITTEN NOT = "Y"
               MOVE "Y" TO WS-RANDOM-WRITTEN.
           MOVE INCR-KEY TO RANDOM-INVOICE.
           WRITE RANDOM-REC
              INVALID KEY NEXT SENTENCE.
              
           IF WS-RANDOM-ST1 NOT = 0
             MOVE "WE HAVE A PROBLEM @ WRR-005, WRITING RANDOM RECORD."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       WRR-999.
            EXIT.
      *
       PRINT-REG-HEADINGS SECTION.
       PH-000.
           ADD 1        TO WS-PAGE
           MOVE WS-PAGE TO H-PAGE
           MOVE 7       TO LINE-CNT.

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC.

           IF WS-PAGE = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE "**** Invoice Register Section (SL1) *******" 
            TO PRINT-REC
             WRITE PRINT-REC AFTER 1.
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC.
      *     WRITE PRINT-REC AFTER 1.
       PH-999.
           EXIT.
      *
       PRINT-REG-TOTALS SECTION.
       P-REG-TOTALS000.
             
           IF LINE-CNT > 55
              PERFORM PRINT-REG-HEADINGS.
           IF WS-INCR-ST1 = 88
             MOVE "NO RECORDS FROM THE DATE ENTERED, NO SALES TO PRINT."
             TO PRINT-REC
             WRITE PRINT-REC AFTER 1
             MOVE " " TO PRINT-REC
             WRITE PRINT-REC.
             
           MOVE WS-TOTI-INVOICE  TO TOT-REG-INVAMT
           MOVE WS-TOTI-TAX      TO TOT-REG-TAX
           MOVE WS-TOTI-ADDON    TO TOT-REG-ADDON
           MOVE WS-TOTI-NETT     TO TOT-REG-NETT
           MOVE WS-TOTI-COST     TO TOT-REG-COST.
           
           COMPUTE WS-VATABLE ROUNDED = WS-TOTI-INVOICE / 
              ((WS-VAT-PERC + 100) * WS-VAT-PERC).
           IF WS-VATABLE NOT = WS-TOTI-TAX
              MOVE "V"            TO TOT-REG-TAX-ERR
           ELSE
              MOVE " "            TO TOT-REG-TAX-ERR.
              
           COMPUTE WS-IMARGIN = WS-TOTI-INVOICE - WS-TOTI-COST
                                  - WS-TOTI-TAX - WS-TOTI-ADDON
           MOVE WS-IMARGIN       TO TOT-REG-MARGIN
           COMPUTE WS-IPERC =
               (WS-IMARGIN / WS-TOTI-COST) * 100
           MOVE WS-IPERC         TO TOT-REG-PERC
           MOVE "VATABLE INVOICES    :" TO TOT-REG-DESC
           MOVE WS-INVNO         TO TOT-REG-NO
           WRITE PRINT-REC FROM TOTAL-REG-LINE AFTER 1
           MOVE " " TO PRINT-REC.

           MOVE WS-TOTC-INVOICE  TO TOT-REG-INVAMT
           MOVE WS-TOTC-TAX      TO TOT-REG-TAX
           MOVE WS-TOTC-ADDON    TO TOT-REG-ADDON
           MOVE WS-TOTC-NETT     TO TOT-REG-NETT
           MOVE WS-TOTC-COST     TO TOT-REG-COST.
           
           COMPUTE WS-VATABLE ROUNDED = WS-TOTC-INVOICE /
              ((WS-VAT-PERC + 100) * WS-VAT-PERC).
           IF WS-VATABLE NOT = WS-TOTC-TAX
              MOVE "V"           TO TOT-REG-TAX-ERR
           ELSE
              MOVE " "           TO TOT-REG-TAX-ERR.

           COMPUTE WS-CMARGIN = WS-TOTC-INVOICE - WS-TOTC-COST
                                  - WS-TOTC-TAX - WS-TOTC-ADDON
           MOVE WS-CMARGIN       TO TOT-REG-MARGIN
           COMPUTE WS-CPERC =
               (WS-CMARGIN / WS-TOTC-COST) * 100
           MOVE WS-CPERC         TO TOT-REG-PERC
           MOVE "VATABLE CREDITS     :" TO TOT-REG-DESC
           MOVE WS-CRNO          TO TOT-REG-NO
           WRITE PRINT-REC FROM TOTAL-REG-LINE AFTER 1
           MOVE " " TO PRINT-REC.

           SUBTRACT WS-TOTC-INVOICE  FROM WS-TOTI-INVOICE
           SUBTRACT WS-TOTC-TAX      FROM WS-TOTI-TAX
           SUBTRACT WS-TOTC-ADDON    FROM WS-TOTI-ADDON
           SUBTRACT WS-TOTC-NETT     FROM WS-TOTI-NETT
           SUBTRACT WS-TOTC-COST     FROM WS-TOTI-COST
           SUBTRACT WS-CRNO          FROM WS-INVNO
           
           MOVE WS-TOTI-INVOICE  TO TOT-REG-INVAMT
           MOVE WS-TOTI-TAX      TO TOT-REG-TAX
           MOVE WS-TOTI-ADDON    TO TOT-REG-ADDON
           MOVE WS-TOTI-NETT     TO TOT-REG-NETT
           MOVE WS-TOTI-COST     TO TOT-REG-COST.
           
           COMPUTE WS-VATABLE ROUNDED = WS-TOTI-INVOICE /
              ((WS-VAT-PERC + 100) * WS-VAT-PERC).
           IF WS-VATABLE NOT = WS-TOTI-TAX
              MOVE "V"           TO TOT-REG-TAX-ERR
           ELSE
              MOVE " "           TO TOT-REG-TAX-ERR.
           
           COMPUTE WS-IMARGIN = WS-TOTI-INVOICE - WS-TOTI-COST
                                  - WS-TOTI-TAX - WS-TOTI-ADDON
           MOVE WS-IMARGIN       TO TOT-REG-MARGIN
           COMPUTE WS-IPERC =
               (WS-IMARGIN / WS-TOTI-COST) * 100
           MOVE WS-IPERC         TO TOT-REG-PERC
           MOVE "NETT VATABLE INVOICES" TO TOT-REG-DESC
           MOVE WS-INVNO         TO TOT-REG-NO
           WRITE PRINT-REC FROM TOTAL-REG-LINE AFTER 1
           MOVE " " TO PRINT-REC.
       P-REG-TOTALS010.
           MOVE WS-XPORTI-INVOICE  TO TOT-REG-INVAMT
           MOVE WS-XPORTI-TAX      TO TOT-REG-TAX
           MOVE WS-XPORTI-ADDON    TO TOT-REG-ADDON
           MOVE WS-XPORTI-NETT     TO TOT-REG-NETT
           MOVE WS-XPORTI-COST     TO TOT-REG-COST
           COMPUTE WS-IMARGIN = WS-XPORTI-INVOICE - WS-XPORTI-COST
                                  - WS-XPORTI-TAX - WS-XPORTI-ADDON
           MOVE WS-IMARGIN       TO TOT-REG-MARGIN
           COMPUTE WS-IPERC =
               (WS-IMARGIN / WS-XPORTI-COST) * 100
           MOVE WS-IPERC         TO TOT-REG-PERC
           MOVE "NUMBER OF XPORT INVS:" TO TOT-REG-DESC
           MOVE WS-XPORT-INVNO   TO TOT-REG-NO
           WRITE PRINT-REC FROM TOTAL-REG-LINE AFTER 2
           MOVE " " TO PRINT-REC.

           MOVE WS-XPORTC-INVOICE  TO TOT-REG-INVAMT
           MOVE WS-XPORTC-TAX      TO TOT-REG-TAX
           MOVE WS-XPORTC-ADDON    TO TOT-REG-ADDON
           MOVE WS-XPORTC-NETT     TO TOT-REG-NETT
           MOVE WS-XPORTC-COST     TO TOT-REG-COST
           COMPUTE WS-CMARGIN = WS-XPORTC-INVOICE - WS-XPORTC-COST
                                  - WS-XPORTC-TAX - WS-XPORTC-ADDON
           MOVE WS-CMARGIN       TO TOT-REG-MARGIN
           COMPUTE WS-CPERC =
               (WS-CMARGIN / WS-XPORTC-COST) * 100
           MOVE WS-CPERC         TO TOT-REG-PERC
           MOVE "NUMBER XPORT CREDITS:" TO TOT-REG-DESC
           MOVE WS-XPORT-CRNO    TO TOT-REG-NO
           WRITE PRINT-REC FROM TOTAL-REG-LINE AFTER 1
           MOVE " " TO PRINT-REC.

           SUBTRACT WS-XPORTC-INVOICE  FROM WS-XPORTI-INVOICE
           SUBTRACT WS-XPORTC-TAX      FROM WS-XPORTI-TAX
           SUBTRACT WS-XPORTC-ADDON    FROM WS-XPORTI-ADDON
           SUBTRACT WS-XPORTC-NETT     FROM WS-XPORTI-NETT
           SUBTRACT WS-XPORTC-COST     FROM WS-XPORTI-COST
           SUBTRACT WS-XPORT-CRNO      FROM WS-XPORT-INVNO

           MOVE WS-XPORTI-INVOICE  TO TOT-REG-INVAMT
           MOVE WS-XPORTI-TAX      TO TOT-REG-TAX
           MOVE WS-XPORTI-ADDON    TO TOT-REG-ADDON
           MOVE WS-XPORTI-NETT     TO TOT-REG-NETT
           MOVE WS-XPORTI-COST     TO TOT-REG-COST
           COMPUTE WS-IMARGIN = WS-XPORTI-INVOICE - WS-XPORTI-COST
                                  - WS-XPORTI-TAX - WS-XPORTI-ADDON
           MOVE WS-IMARGIN       TO TOT-REG-MARGIN
           COMPUTE WS-IPERC =
               (WS-IMARGIN / WS-XPORTI-COST) * 100
           MOVE WS-IPERC         TO TOT-REG-PERC
           MOVE "NETT EXPORT INVOICES:" TO TOT-REG-DESC
           MOVE WS-XPORT-INVNO   TO TOT-REG-NO

           WRITE PRINT-REC FROM TOTAL-REG-LINE AFTER 1
           MOVE " " TO PRINT-REC.
       P-REG-TOTALS020.
           ADD WS-XPORTI-INVOICE  TO WS-TOTI-INVOICE
           ADD WS-XPORTI-TAX      TO WS-TOTI-TAX
           ADD WS-XPORTI-ADDON    TO WS-TOTI-ADDON
           ADD WS-XPORTI-NETT     TO WS-TOTI-NETT
           ADD WS-XPORTI-COST     TO WS-TOTI-COST
           ADD WS-XPORT-INVNO     TO WS-INVNO

           MOVE WS-TOTI-INVOICE  TO TOT-REG-INVAMT
           MOVE WS-TOTI-TAX      TO TOT-REG-TAX
           MOVE WS-TOTI-ADDON    TO TOT-REG-ADDON
           MOVE WS-TOTI-NETT     TO TOT-REG-NETT
           MOVE WS-TOTI-COST     TO TOT-REG-COST
           COMPUTE WS-IMARGIN = WS-TOTI-INVOICE - WS-TOTI-COST
                                  - WS-TOTI-TAX - WS-TOTI-ADDON
           MOVE WS-IMARGIN       TO TOT-REG-MARGIN
           COMPUTE WS-IPERC =
               (WS-IMARGIN / WS-TOTI-COST) * 100
           MOVE WS-IPERC         TO TOT-REG-PERC
           MOVE "TOTAL NETT INVOICES :" TO TOT-REG-DESC
           MOVE WS-INVNO   TO TOT-REG-NO

           WRITE PRINT-REC FROM TOTAL-REG-LINE AFTER 1
           MOVE " " TO PRINT-REC.
             
           MOVE "*******************************************" 
            TO PRINT-REC
             WRITE PRINT-REC AFTER 1.
       P-REG-TOTALS999.
           EXIT.
      *
       PRINT-ANAL-ROUTINE SECTION.
       P-ANAL-000.
           MOVE 2010 TO POS
           DISPLAY "2. READING SALES ANALYSIS SECTION....." AT POS.
           MOVE 0 TO SA-KEY LINE-CNT.
           START SALES-ANALYSIS KEY NOT < SA-KEY
              INVALID KEY NEXT SENTENCE.
       P-ANAL-005.
           READ SALES-ANALYSIS NEXT
               AT END NEXT SENTENCE.
           IF WS-SALES-ST1 = 10
               GO TO P-ANAL-999.
           IF WS-SALES-ST1 NOT = 0
              MOVE 0 TO WS-SALES-ST1
              MOVE "SALES ANALYSIS BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO P-ANAL-005.
            IF LINE-CNT = 0
               GO TO P-ANAL-010.
            IF LINE-CNT < 60
               GO TO P-ANAL-020.
       P-ANAL-010.
           MOVE "**** Sales Analysys Section (SL5) *******" 
            TO PRINT-REC
             WRITE PRINT-REC AFTER 1.
             
           WRITE PRINT-REC FROM ANAL-HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE 7 TO LINE-CNT.
       P-ANAL-020.
            MOVE 2410 TO POS
            DISPLAY "Sales Analysis Read:           " AT POS
            ADD 25 TO POS
            DISPLAY SA-ANALYSIS-CODE AT POS.

           IF SA-ANALYSIS-CODE = 6 OR = 52
               ADD SA-COST-PTD      TO WS-EXPORT-COST-PTD
               ADD SA-SALES-PTD     TO WS-EXPORT-SALES-PTD
           ELSE
               ADD SA-COST-PTD      TO WS-VATABLE-COST-PTD
               ADD SA-SALES-PTD     TO WS-VATABLE-SALES-PTD.

           GO TO P-ANAL-005.
       P-ANAL-999.
           EXIT.
      *
       PRINT-ANAL-TOTALS SECTION.
       P-ANAL-TOT-000.
           MOVE "VATABLE SALES     " TO D-DIVTOT

           MOVE "MTD TOTALS :"       TO D-NAMETOT
           MOVE WS-VATABLE-SALES-PTD TO D-SALESTOT
           MOVE WS-VATABLE-COST-PTD  TO D-COSTTOT
           COMPUTE ANAL-TOT-MARGIN = WS-VATABLE-SALES-PTD -
                 WS-VATABLE-COST-PTD
           MOVE ANAL-TOT-MARGIN           TO D-MARGINTOT
           COMPUTE ANAL-TOT-PERC ROUNDED =
               (ANAL-TOT-MARGIN / WS-VATABLE-COST-PTD) * 100
           MOVE ANAL-TOT-PERC             TO D-PERCTOT
           WRITE PRINT-REC FROM ANAL-TOTALS.
           MOVE " " TO PRINT-REC
                       ANAL-TOTALS
           MOVE 0 TO ANAL-TOT-PERC
                     ANAL-TOT-MARGIN.
       P-ANAL-TOT-500.
           MOVE "EXPORT SALES      " TO D-DIVTOT

           MOVE "            "      TO D-NAMETOT
           MOVE WS-EXPORT-COST-PTD  TO D-COSTTOT
           MOVE WS-EXPORT-SALES-PTD TO D-SALESTOT
           COMPUTE ANAL-TOT-MARGIN = WS-EXPORT-SALES-PTD - 
               WS-EXPORT-COST-PTD
           MOVE ANAL-TOT-MARGIN          TO D-MARGINTOT
           COMPUTE ANAL-TOT-PERC ROUNDED =
               (ANAL-TOT-MARGIN / WS-EXPORT-COST-PTD) * 100
           MOVE ANAL-TOT-PERC            TO D-PERCTOT
           WRITE PRINT-REC FROM ANAL-TOTALS.
           MOVE " " TO PRINT-REC
                       ANAL-TOTALS
           MOVE 0 TO ANAL-TOT-PERC
                     ANAL-TOT-MARGIN.
       P-ANAL-TOT-700.
           MOVE "VATABLE & EXPORT  "  TO D-DIVTOT
           COMPUTE ANAL-TOT-SALESPTD = WS-VATABLE-SALES-PTD +
                     WS-EXPORT-SALES-PTD.
           COMPUTE ANAL-TOT-COSTPTD = WS-VATABLE-COST-PTD +
                     WS-EXPORT-COST-PTD.

           MOVE "MTD TOTALS :" TO D-NAMETOT
           MOVE ANAL-TOT-SALESPTD   TO D-SALESTOT
           MOVE ANAL-TOT-COSTPTD    TO D-COSTTOT
           COMPUTE ANAL-TOT-MARGIN = ANAL-TOT-SALESPTD -
                 ANAL-TOT-COSTPTD
           MOVE ANAL-TOT-MARGIN     TO D-MARGINTOT
           COMPUTE ANAL-TOT-PERC ROUNDED =
               (ANAL-TOT-MARGIN / ANAL-TOT-COSTPTD) * 100
           MOVE ANAL-TOT-PERC       TO D-PERCTOT
           WRITE PRINT-REC FROM ANAL-TOTALS AFTER 1.
           MOVE " " TO PRINT-REC
           MOVE 0 TO ANAL-TOT-PERC
                     ANAL-TOT-MARGIN.
             
           MOVE "*******************************************" 
            TO PRINT-REC
             WRITE PRINT-REC AFTER 1.
       P-ANAL-TOT-999.
           EXIT.
      *
       PRINT-DISTRIBUTIONS SECTION.
       P-DIS-006.
           MOVE 2010 TO POS
           DISPLAY "3. READING DISTRIBUTIONS SECTION......" AT POS.
           MOVE "1" TO DIST-KEY.
           START DISTRIBUTIONS KEY NOT < DIST-KEY
              INVALID KEY NEXT SENTENCE.
       P-DIS-008.
           READ DISTRIBUTIONS
               INVALID KEY NEXT SENTENCE.
           IF WS-DISTRIBUTION-ST1 = 10
              MOVE "DISTRIBUTIONS RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-DISTRIBUTION-ST1
              GO TO P-DIS-008.
           IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
              MOVE "DISTRIBUTIONS RECORD NOT FOUND, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-DISTRIBUTION-ST1
              EXIT PROGRAM.
           PERFORM ERROR-020.
       P-DIS-010.
           MOVE "**** Sales Distribution Section (SL2) *******" 
            TO PRINT-REC
             WRITE PRINT-REC AFTER 1.

            WRITE PRINT-REC FROM DIS-HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            MOVE 8 TO LINE-CNT.
       P-DIS-015.
           MOVE "INVOICES      :" TO DIS-NAME1
           MOVE DIST-INVOICEPTD   TO DIS-AMT1
           MOVE "PAYMENTS      :" TO DIS-NAME2
           MOVE DIST-PAYMENTPTD   TO DIS-AMT2.
           COMPUTE WS-RAND = DIST-ACCRECYTD - DIST-ACCRECPTD
           MOVE "DR OPEN BAL   :" TO DIS-NAME3
           MOVE WS-RAND           TO DIS-AMT3.
           WRITE PRINT-REC FROM DIS-DETAIL-LINE AFTER 2
           MOVE " " TO PRINT-REC
                       DIS-DETAIL-LINE.

           MOVE "JOURNAL DEBIT :" TO DIS-NAME1
           MOVE DIST-JOURNALDRPTD TO DIS-AMT1
           MOVE "JOURNAL CREDIT:" TO DIS-NAME2
           MOVE DIST-JOURNALCRPTD TO DIS-AMT2
           MOVE "CREDIT NOTES  :" TO DIS-NAME3
           MOVE DIST-CNOTEPTD     TO DIS-AMT3
           WRITE PRINT-REC FROM DIS-DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DIS-DETAIL-LINE.
 
           MOVE "INTEREST      :" TO DIS-NAME1
           MOVE DIST-INTERESTPTD  TO DIS-AMT1
           MOVE "DISCOUNT      :" TO DIS-NAME2
           MOVE DIST-DISCOUNTPTD  TO DIS-AMT2
           MOVE "ADD-ONS       :" TO DIS-NAME3
           MOVE DIST-ADDONPTD     TO DIS-AMT3
           WRITE PRINT-REC FROM DIS-DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DIS-DETAIL-LINE.

           MOVE "BAD-DEBTS     :" TO DIS-NAME1
           MOVE DIST-BDEBTPTD     TO DIS-AMT1
           MOVE "R/D CHEQUES   :" TO DIS-NAME2
           MOVE DIST-RDCHEQUEPTD  TO DIS-AMT2
           MOVE "ACCS RECVD MV :" TO DIS-NAME3
           MOVE DIST-ACCRECPTD    TO DIS-AMT3
           WRITE PRINT-REC FROM DIS-DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
       P-DIS-020.
           MOVE "VAT COLLECTED :"      TO DIS-NAME1
           MOVE GST-AMT-TAXED-PTD      TO DIS-AMT1
           MOVE "TAXABLE SALES :"      TO DIS-NAME2
           MOVE GST-AMT-TAXABLE-PTD    TO DIS-AMT2
           MOVE "DR CLOSE BAL  :"      TO DIS-NAME3.
           PERFORM P-DIS-500.
           IF WS-RAND NOT = DIST-ACCRECYTD
               MOVE "ERR" TO DIS-ERR.
           MOVE DIST-ACCRECYTD         TO DIS-AMT3
           WRITE PRINT-REC FROM DIS-DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DIS-DETAIL-LINE.

           MOVE "EXPORT SALES       :" TO DIS-NAME1
           MOVE GST-AMT-EXPORT-PTD     TO DIS-AMT1
           MOVE "NON TAXABLE   :"      TO DIS-NAME2
           MOVE GST-AMT-NONTAXABLE-PTD TO DIS-AMT2
           WRITE PRINT-REC FROM DIS-DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DIS-DETAIL-LINE
             
           MOVE "*******************************************" 
            TO PRINT-REC
             WRITE PRINT-REC AFTER 1.
             
             GO TO P-DIS-999.
       P-DIS-500.
           COMPUTE WS-RAND = (DIST-ACCRECYTD - DIST-ACCRECPTD) +
               DIST-INVOICEPTD +
               DIST-JOURNALDRPTD +
               DIST-INTERESTPTD +
               DIST-ADDONPTD +
               DIST-RDCHEQUEPTD +
               GST-AMT-TAXED-PTD - 
               DIST-PAYMENTPTD -
               DIST-JOURNALCRPTD -
               DIST-CNOTEPTD - 
               DIST-DISCOUNTPTD.
       P-DIS-999.
           EXIT.
      *
       PRINT-DEBTOR-ROUTINE SECTION.
       PDR-000.
           MOVE 2010 TO POS
           DISPLAY "4. READING DEBTOR AGE ANALYSIS SECTION..." AT POS.
           MOVE 0         TO WS-RANGE1
           MOVE 9999999   TO WS-RANGE2.
           MOVE WS-RANGE1 TO DRTR-ACC-KEY.
           MOVE 0         TO DRTR-DATE.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE 88 TO WS-DRTRANS-ST1
               PERFORM DEBTOR-SUBTOTALS
               GO TO PDR-999.
       PDR-002.
           READ DEBTOR-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
               PERFORM DEBTOR-SUBTOTALS
               GO TO PDR-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-DRTRANS-ST1
               MOVE "DR-TRANS FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PDR-002.

           IF DRTR-AMT-OUTSTANDING = 0
               GO TO PDR-002.
           IF DR-ACCOUNT-NUMBER = 0
               GO TO PDR-005.
           IF DRTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
               GO TO PDR-010
           ELSE
               PERFORM DEBTOR-SUBTOTALS.
       PDR-005.
           MOVE DRTR-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
           READ DEBTOR-MASTER 
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "UNKNOWN DEBTOR" TO DR-NAME
               GO TO PDR-010.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PDR-005.
               
           MOVE 2310 TO POS
           DISPLAY "Account Being Read:" AT POS
           ADD 20 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
       PDR-010.
           MOVE DRTR-DATE                TO WS-AGE-DATE
                                            SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
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
               GO TO PDR-030.
           IF WS-MM = 1
               ADD WS-AMT-OF-INVOICE TO WS-TOT-30DAY
                                        WS-GRTOT-30DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               GO TO PDR-030.
           IF WS-MM = 2
               ADD WS-AMT-OF-INVOICE TO WS-TOT-60DAY
                                        WS-GRTOT-60DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               GO TO PDR-030.
           IF WS-MM = 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-90DAY
                                        WS-GRTOT-90DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               GO TO PDR-030.
           IF WS-MM > 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-120DAY
                                        WS-GRTOT-120DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE.
       PDR-030.
           GO TO PDR-002.
       PDR-999.
           EXIT.
      *
       DEBTOR-SUBTOTALS SECTION.
       SUB-000.
      *     IF WS-TOT-BALANCE NOT = DR-BALANCE
      *      IF DR-ACCOUNT-NUMBER NOT > 0
      *        MOVE "S" TO WS-DEBTOR-IMBALANCE.
      *
           IF WS-TOT-BALANCE NOT = DR-BALANCE
            IF DR-ACCOUNT-NUMBER > 0
              MOVE "Y" TO WS-DEBTOR-IMBALANCE.
       SUB-010.
           MOVE 0 TO WS-TOT-BALANCE
                     WS-TOT-CURRENT
                     WS-TOT-30DAY
                     WS-TOT-60DAY
                     WS-TOT-90DAY
                     WS-TOT-120DAY.
       SUB-999.
           EXIT.
      *
       PRINT-DEBTOR-HEADINGS SECTION.
       PDH-000.
           MOVE "**** Debtor Age Analysis Section (SL11) *****" 
            TO PRINT-REC
             WRITE PRINT-REC AFTER 1.

           WRITE PRINT-REC FROM DEBTOR-HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE 7 TO LINE-CNT.
       PDH-999.
           EXIT.
      *
       DEBTOR-GRAND-TOTALS SECTION.
       DGT-000.
           IF WS-DEBTOR-IMBALANCE = "Y"
               MOVE "SOME ACCS ARE IMBALANCED"  TO GR-TOT-TERMS.
      *     IF WS-DEBTOR-IMBALANCE = "S"
      *         MOVE "A TRANS HAS A BLANK ACC "  TO GR-TOT-TERMS.
           IF WS-DEBTOR-IMBALANCE = "N"
               MOVE "* ALL ACC'S IN BALANCE *"  TO GR-TOT-TERMS.
           WRITE PRINT-REC FROM P-UNDERLINE AFTER 1
           MOVE WS-GRTOT-BALANCE TO GR-TOT-BALANCE
           MOVE WS-GRTOT-CURRENT TO GR-TOT-CURRENT
           MOVE WS-GRTOT-30DAY   TO GR-TOT-30DAY
           MOVE WS-GRTOT-60DAY   TO GR-TOT-60DAY
           MOVE WS-GRTOT-90DAY   TO GR-TOT-90DAY
           MOVE WS-GRTOT-120DAY  TO GR-TOT-120DAY
           WRITE PRINT-REC FROM GRAND-TOTAL-LINE AFTER 1
           WRITE PRINT-REC FROM P-DBLUNDERLINE AFTER 1.
           MOVE " " TO PRINT-REC.
           
           IF WS-GRTOT-BALANCE NOT = DIST-ACCRECYTD
              MOVE 
           "** AGE ANALYSIS AND DISTRIBUTIONS DO NOT BALANCE **"
                TO PRINT-REC 
           WRITE PRINT-REC AFTER 1.
           
           MOVE "***************************************************" 
            TO PRINT-REC
             WRITE PRINT-REC AFTER 1.
           MOVE " " TO PRINT-REC.
       DGT-999.
           EXIT.
      *
       PRINT-GL SECTION.
       PGLS-000.
           MOVE 2010 TO POS
           DISPLAY "5. GENERAL LEDGER ANALYSIS SECTION..." AT POS.

           MOVE "**** General Ledger Section *****" 
            TO PRINT-REC
             WRITE PRINT-REC AFTER 1.
           MOVE "STOCK VALUATION  :" TO GL-FILL1
           MOVE "DEBTORS CONTROL  :" TO GL-FILL2
           WRITE PRINT-REC FROM GL-PRINT-LINE AFTER 2
           MOVE " " TO PRINT-REC.

           MOVE "CR AGE ANALYSIS  :" TO GL-FILL1
           MOVE "JN AGE ANALYSIS  :" TO GL-FILL2
           WRITE PRINT-REC FROM GL-PRINT-LINE AFTER 2
           MOVE " " TO PRINT-REC.

           MOVE "CB BANK ACCOUNT  :" TO GL-FILL1
           MOVE "GL BANK ACCOUNT  :" TO GL-FILL2
           WRITE PRINT-REC FROM GL-PRINT-LINE AFTER 2
           MOVE " " TO PRINT-REC.

           MOVE "SL19 GROSS PROFIT:" TO GL-FILL1
           MOVE "GL GROSS PROFIT  :" TO GL-FILL2
           WRITE PRINT-REC FROM GL-PRINT-LINE AFTER 2
           MOVE " " TO PRINT-REC.
           
           MOVE "*******************************************" 
            TO PRINT-REC
             WRITE PRINT-REC AFTER 1.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
       PGLS-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           READ PARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO PARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RP-000.
            MOVE PA-GST-PERCENT TO WS-VAT-PERC.
       RP-999.
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
               MOVE "PARAMETER TERMS BUSY, PRESS 'ESC' TO RETRY."
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
       OPEN-FILES SECTION.
       OPEN-010.
           MOVE 2910 TO POS
           DISPLAY "Opening files ......" AT POS.
           OPEN I-O DISTRIBUTIONS.
           IF WS-DISTRIBUTION-ST1 NOT = 0
               MOVE "DISTRIBUTION FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-010.
       OPEN-014.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-015.
           OPEN I-O SALES-ANALYSIS.
           IF WS-SALES-ST1 NOT = 0
               MOVE 0 TO WS-SALES-ST1
               MOVE "SALES ANALYSIS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-015.
       OPEN-018.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY, BE PATIENT!" TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-018.
           MOVE ALL "X" TO STORE-TERM
           PERFORM READ-PARAMETER.
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
       OPEN-0301.
            GO TO OPEN-036.
       OPEN-035.
           OPEN I-O RANDOM-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE "RANDOM FILE OPEN I-O ERROR, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-RANDOM-ST1
              GO TO OPEN-035.
       OPEN-036.
           OPEN OUTPUT RANDOM-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE "RANDOM FILE OPEN OUTOUT ERROR, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-RANDOM-ST1
              GO TO OPEN-036.
       OPEN-500.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H-DATE.

           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO WS-BEG-DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           
           PERFORM ERROR1-020.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 60
               PERFORM PRINT-REG-HEADINGS.
           IF WS-RANDOM-WRITTEN NOT = "Y"
               MOVE "*** NOTHING TO PRINT IN THE RANGE SELECTED ***"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 1.
               
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE
                 SALES-ANALYSIS
                 DISTRIBUTIONS
                 DEBTOR-TRANS-FILE
                 DEBTOR-MASTER
                 INCR-REGISTER.
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
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
