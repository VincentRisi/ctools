        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlInRgRp.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectSlRegister".
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
              "/ctools/spl/RandomInvoice".
       77  WS-RANDOM-FILE-ind   PIC X(30) VALUE
              "/ctools/spl/RandomInvoice.Ind".
       77  WS-TOTI-INVOICE      PIC S9(8)V99 VALUE 0.
       77  WS-TOTI-TAX          PIC S9(8)V99 VALUE 0.
       77  WS-TOTI-ADDON        PIC S9(8)V99 VALUE 0.
       77  WS-TOTI-DISCOUNT     PIC S9(8)V99 VALUE 0.
       77  WS-TOTI-COST         PIC S9(8)V99 VALUE 0.
       77  WS-INVNO             PIC 9(5) VALUE 0.
       77  WS-CRNO              PIC 9(5) VALUE 0.
       77  WS-IMARGIN           PIC S9(8)V99 VALUE 0.
       77  WS-IPERC             PIC S9(8)V99 VALUE 0.
       77  WS-VAT-PERC          PIC 99V99 VALUE 0.
       77  WS-XPORTI-INVOICE    PIC S9(8)V99 VALUE 0.
       77  WS-XPORTI-TAX        PIC S9(8)V99 VALUE 0.
       77  WS-XPORTI-ADDON      PIC S9(8)V99 VALUE 0.
       77  WS-XPORTI-DISCOUNT   PIC S9(8)V99 VALUE 0.
       77  WS-XPORTI-COST       PIC S9(8)V99 VALUE 0.
       77  WS-XPORTC-INVOICE    PIC S9(8)V99 VALUE 0.
       77  WS-XPORTC-TAX        PIC S9(8)V99 VALUE 0.
       77  WS-XPORTC-ADDON      PIC S9(8)V99 VALUE 0.
       77  WS-XPORTC-DISCOUNT   PIC S9(8)V99 VALUE 0.
       77  WS-XPORTC-COST       PIC S9(8)V99 VALUE 0.
       77  WS-XPORT-INVNO       PIC 9(5) VALUE 0.
       77  WS-XPORT-CRNO        PIC 9(5) VALUE 0.
       77  WS-XPORT-MARGIN      PIC S9(8)V99 VALUE 0.
       77  WS-XPORT-PERC        PIC S9(8)V99 VALUE 0.
       77  WS-TOTC-INVOICE      PIC S9(8)V99 VALUE 0.
       77  WS-VATABLE           PIC S9(8)V99 VALUE 0.
       77  WS-TOTC-TAX          PIC S9(8)V99 VALUE 0.
       77  WS-TOTC-ADDON        PIC S9(8)V99 VALUE 0.
       77  WS-TOTC-DISCOUNT     PIC S9(8)V99 VALUE 0.
       77  WS-TOTC-COST         PIC S9(8)V99 VALUE 0.
       77  WS-CMARGIN           PIC S9(8)V99 VALUE 0.
       77  WS-CPERC             PIC S9(8)V99 VALUE 0.
       77  WS-ACCEPT            PIC XX VALUE " ".
       77  WS-YYACCEPT          PIC X(4) VALUE " ".
       77  WS-PERC-ACCEPT       PIC X(11) VALUE " ".
       77  WS-PERC-DIS          PIC Z(8)9.99.
       77  WS-PERC              PIC S9(8)V99 VALUE 0.
       77  WS-RAND-ACCEPT       PIC X(11) VALUE " ".
       77  WS-RAND-DIS          PIC Z(8)9.99.
       77  WS-RAND              PIC S9(8)V99 VALUE 0.
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-RANDOM-WRITTEN    PIC X.
       01  W-DD                 PIC Z9.
       01  W-YY                 PIC ZZZ9.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-RANDOM-STATUS.
           03  WS-RANDOM-ST1    PIC 99.
       01  WEEK-DATE.
           03  WEEK-YY          PIC 9999.
           03  WEEK-MM          PIC 99.
           03  WEEK-DD          PIC 99.
       01  COMP-DATE.
           03  COMP-YY          PIC 9999.
           03  COMP-MM          PIC 99.
           03  COMP-DD          PIC 99.
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
           "I N V O I C E   R E G I S T E R ".
           03  FILLER           PIC X(6) VALUE "PAGE: ".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(4) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(49) VALUE " ".
           03  FILLER           PIC X(103) VALUE 
           "*******************************".
       01  HEAD3.
           03  FILLER           PIC X(10) VALUE " TRANS TY".
           03  FILLER           PIC X(30) VALUE " ACCOUNT".
           03  FILLER           PIC X(33) VALUE
            "INVOICE   SA     INVOICE".
           03  FILLER           PIC X(22) VALUE "TAX      ADD-ON".
           03  FILLER           PIC X(24) VALUE "DISC.        COST".
           03  FILLER           PIC X(33) VALUE "GROSS     %".
       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "NUMBER PE".
           03  FILLER           PIC X(30) VALUE " NUMBER NAME".
           03  FILLER           PIC X(28) VALUE
           "  DATE    LE      AMOUNT".
           03  FILLER           PIC X(24) VALUE "  AMOUNT      AMOUNT".
           03  FILLER           PIC X(25) VALUE "  AMOUNT      AMOUNT".
           03  FILLER           PIC X(36) VALUE
            " AMOUNT  MARGIN".
       01  DETAIL-LINE.
           03  D-INVNO          PIC Z(5)9.
           03  FILLER           PIC X(1) VALUE " ".
           03  D-TYPE           PIC X(3) VALUE " ".
           03  D-CUSTNO         PIC X(8) VALUE " ".
           03  D-NAME           PIC X(20) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  D-DATE           PIC X(10).
           03  FILLER           PIC X VALUE " ".
           03  D-SALES          PIC X(3) VALUE " ".
           03  D-INVAMT         PIC Z(7)9.99.
           03  FILLER           PIC X(1) VALUE " ".
           03  D-TAX            PIC Z(7)9.99.
           03  D-TAX-ERROR      PIC X(1) VALUE " ".
           03  D-ADDON          PIC Z(7)9.99.
           03  FILLER           PIC X(1) VALUE " ".
           03  D-DISCOUNT       PIC Z(7)9.99.
           03  FILLER           PIC X(1) VALUE " ".
           03  D-COST           PIC Z(7)9.99.
           03  FILLER           PIC X(1) VALUE " ".
           03  D-MARGIN         PIC Z(7)9.99-.
           03  D-PERC           PIC Z(3)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER           PIC X(13) VALUE " ".
           03  TOT-DESC         PIC X(22) VALUE " ".
           03  TOT-NO           PIC Z(4)9.
           03  FILLER           PIC X(5) VALUE " ".
           03  FILLER           PIC X(8) VALUE "TOTALS:".
           03  TOT-INVAMT       PIC Z(7)9.99.
           03  FILLER           PIC X(1) VALUE " ".
           03  TOT-TAX          PIC Z(7)9.99.
           03  TOT-REG-TAX-ERR  PIC X(1) VALUE " ".
           03  TOT-ADDON        PIC Z(7)9.99.
           03  FILLER           PIC X(1) VALUE " ".
           03  TOT-DISCOUNT     PIC Z(7)9.99.
           03  FILLER           PIC X(1) VALUE " ".
           03  TOT-COST         PIC Z(7)9.99.
           03  FILLER           PIC X(1) VALUE " ".
           03  TOT-MARGIN       PIC Z(7)9.99-.
           03  TOT-PERC         PIC Z(3)9.99-.
           03  FILLER           PIC X(6) VALUE " ".
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
           DISPLAY "** Invoice Register Report **" AT POS.
           MOVE 420 TO POS.
           DISPLAY "*****************************" AT POS.
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

      *     ACCEPT WS-ACCEPT AT POS.
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

           MOVE WS-MM     TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

      *     ACCEPT WS-ACCEPT AT POS.
           MOVE WS-ACCEPT TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WEEK-MM.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-009.
           IF WEEK-MM NOT > 0 AND NOT < 12
              MOVE 2510 TO POS
              DISPLAY "Enter a month between 1 & 12" AT POS
              MOVE "  " TO WS-ACCEPT
              GO TO CONTROL-010.
           MOVE WEEK-MM TO W-DD.
           DISPLAY W-DD AT POS.
       CONTROL-011.
           MOVE 1210 TO POS.
           DISPLAY "                The YEAR to print is : [    ]"
               AT POS.
           ADD 40 TO POS.
           MOVE WS-YY TO W-YY WS-YYACCEPT.
           DISPLAY W-YY AT POS.

           MOVE WS-YY     TO CDA-DATA.
           MOVE 4         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-YYACCEPT.

      *     ACCEPT WS-YYACCEPT AT POS.
           MOVE WS-YYACCEPT TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WEEK-YY.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           MOVE WEEK-YY TO W-YY.
           DISPLAY W-YY AT POS.
       CONTROL-012.
           MOVE 1410 TO POS.
           DISPLAY
          "ENTER A M/U % BELOW WHICH WILL PRINT, :[           ]"
            AT POS.
           MOVE 1515 TO POS
           DISPLAY "LEAVE BLANK TO PRINT ALL" AT POS
           MOVE 1450 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 11        TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PERC-ACCEPT.

      *     ACCEPT WS-PERC-ACCEPT AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-011.
           IF WS-PERC-ACCEPT = " "
               GO TO CONTROL-013.
           MOVE WS-PERC-ACCEPT TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WS-PERC.
           MOVE WS-PERC      TO WS-PERC-DIS.
           DISPLAY WS-PERC-DIS AT POS.
       CONTROL-013.
           MOVE 1610 TO POS.
           DISPLAY
          "ENTER A RAND AMT ABOVE WHICH TO PRINT,:[           ]"
            AT POS.
           MOVE 1715 TO POS
           DISPLAY "LEAVE BLANK TO PRINT ALL" AT POS
           MOVE 1650 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 11        TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RAND-ACCEPT.

      *     ACCEPT WS-RAND-ACCEPT AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-012.
           IF WS-RAND-ACCEPT = " "
               GO TO CONTROL-015.
           MOVE WS-RAND-ACCEPT TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WS-RAND.
           MOVE WS-RAND      TO WS-RAND-DIS.
           DISPLAY WS-RAND-DIS AT POS.
       CONTROL-015.
           MOVE 2510 TO POS.
           DISPLAY "The report is being compiled ......." AT POS.
           
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.

           PERFORM OPEN-FILES.
           PERFORM READ-BY-DATE.
           IF WS-RANDOM-WRITTEN = "Y"
               PERFORM PRINT-ROUTINE
               PERFORM PRINT-TOTALS.
      *         PERFORM DELETE-TRANS.
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PR-001.
           MOVE 2310 TO POS
           DISPLAY "READING BY INVOICE NUMBER SECOND..." AT POS.
           START INCR-REGISTER KEY NOT < INCR-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
              MOVE 88 TO WS-INCR-ST1
              GO TO PR-999.
       PR-002.
           PERFORM READ-RANDOM-RECORD.
            IF WS-RANDOM-ST1 = 10
              CLOSE RANDOM-FILE
              GO TO PR-999.
       PR-003.
           READ INCR-REGISTER
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
              MOVE "REGISTER BUSY ON READ,REG-ST1 = 23, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PR-002.
           IF WS-INCR-ST1 NOT = 0
             MOVE 
           "REGISTER LOCKED PR-003 AT ANOTHER STATION, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE "LAST RECORD READ WAS:" TO WS-MESSAGE
             PERFORM ERROR-000
             MOVE 2837 TO POS
             DISPLAY D-INVNO AT POS
             PERFORM ERROR-010
             PERFORM ERROR1-020
             PERFORM ERROR-020
             GO TO PR-003.
           MOVE INCR-INVOICE TO D-INVNO.
       PR-010.
           IF WS-LINE > 60
               PERFORM PRINT-HEADINGS.
           MOVE INCR-INVOICE              TO D-INVNO
           MOVE WS-TYPE-DESC (INCR-TRANS) TO D-TYPE
           MOVE INCR-DATE                 TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-DATE
           MOVE INCR-ACCOUNT      TO D-CUSTNO
           MOVE INCR-NAME         TO D-NAME
           MOVE INCR-SALES        TO D-SALES
           MOVE INCR-INVCRED-AMT  TO D-INVAMT
           MOVE INCR-TAX          TO D-TAX.
      *     IF INCR-SALES NOT = 6 AND NOT = 52
           COMPUTE WS-VATABLE ROUNDED = 
              (INCR-INVCRED-AMT / (100 + WS-VAT-PERC)) * WS-VAT-PERC.
           IF WS-VATABLE NOT = INCR-TAX
              MOVE "V"            TO D-TAX-ERROR
           ELSE
              MOVE " "            TO D-TAX-ERROR.
                      
      *     MOVE WS-VATABLE TO TOT-TAX
      *     MOVE TOT-TAX TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE D-TAX TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR1-020.
                      
                      
           MOVE INCR-ADDONS       TO D-ADDON
           MOVE INCR-DISCOUNT     TO D-DISCOUNT
           MOVE INCR-INVCRED-COST TO D-COST.
       PR-020.
           IF INCR-TRANS = 1
              COMPUTE WS-IMARGIN = INCR-INVCRED-AMT -
                  (INCR-INVCRED-COST + INCR-TAX + INCR-ADDONS)
              MOVE WS-IMARGIN       TO D-MARGIN
              COMPUTE WS-IPERC =
                  (WS-IMARGIN / INCR-INVCRED-COST) * 100
              MOVE WS-IPERC         TO D-PERC
              ADD INCR-INVCRED-AMT  TO WS-TOTI-INVOICE
              ADD INCR-TAX          TO WS-TOTI-TAX
              ADD INCR-ADDONS       TO WS-TOTI-ADDON
              ADD INCR-DISCOUNT     TO WS-TOTI-DISCOUNT
              ADD INCR-INVCRED-COST TO WS-TOTI-COST
              ADD 1                 TO WS-INVNO
              GO TO PR-900.
           IF INCR-TRANS = 6
              COMPUTE WS-IMARGIN = INCR-INVCRED-AMT -
                  (INCR-INVCRED-COST + INCR-TAX + INCR-ADDONS)
              MOVE WS-CMARGIN       TO D-MARGIN
              COMPUTE WS-CPERC =
                   (WS-CMARGIN / INCR-INVCRED-COST) * 100
              MOVE WS-CPERC         TO D-PERC
              ADD INCR-INVCRED-AMT  TO WS-TOTC-INVOICE
              ADD INCR-TAX          TO WS-TOTC-TAX
              ADD INCR-ADDONS       TO WS-TOTC-ADDON
              ADD INCR-DISCOUNT     TO WS-TOTC-DISCOUNT
              ADD INCR-INVCRED-COST TO WS-TOTC-COST
              ADD 1                 TO WS-CRNO.
       PR-900.
           IF WS-PERC-ACCEPT NOT = " "
            IF INCR-TRANS = 1
             IF WS-PERC < WS-IPERC
                SUBTRACT 1                 FROM WS-INVNO
                SUBTRACT INCR-INVCRED-AMT  FROM WS-TOTI-INVOICE
                SUBTRACT INCR-TAX          FROM WS-TOTI-TAX
                SUBTRACT INCR-ADDONS       FROM WS-TOTI-ADDON
                SUBTRACT INCR-DISCOUNT     FROM WS-TOTI-DISCOUNT
                SUBTRACT INCR-INVCRED-COST FROM WS-TOTI-COST
                GO TO PR-002.
           IF WS-PERC-ACCEPT NOT = " "
            IF INCR-TRANS = 6
                SUBTRACT 1                 FROM WS-CRNO
                SUBTRACT INCR-INVCRED-AMT  FROM WS-TOTC-INVOICE
                SUBTRACT INCR-TAX          FROM WS-TOTC-TAX
                SUBTRACT INCR-ADDONS       FROM WS-TOTC-ADDON
                SUBTRACT INCR-DISCOUNT     FROM WS-TOTC-DISCOUNT
                SUBTRACT INCR-INVCRED-COST FROM WS-TOTC-COST
                GO TO PR-002.
           IF WS-RAND-ACCEPT NOT = " "
            IF INCR-TRANS = 1
             IF WS-RAND > INCR-INVCRED-AMT
                SUBTRACT 1                 FROM WS-INVNO
                SUBTRACT INCR-INVCRED-AMT  FROM WS-TOTI-INVOICE
                SUBTRACT INCR-TAX          FROM WS-TOTI-TAX
                SUBTRACT INCR-ADDONS       FROM WS-TOTI-ADDON
                SUBTRACT INCR-DISCOUNT     FROM WS-TOTI-DISCOUNT
                SUBTRACT INCR-INVCRED-COST FROM WS-TOTI-COST
                GO TO PR-002.
           IF WS-RAND-ACCEPT NOT = " "
            IF INCR-TRANS = 6
                SUBTRACT 1                 FROM WS-CRNO
                SUBTRACT INCR-INVCRED-AMT  FROM WS-TOTC-INVOICE
                SUBTRACT INCR-TAX          FROM WS-TOTC-TAX
                SUBTRACT INCR-ADDONS       FROM WS-TOTC-ADDON
                SUBTRACT INCR-DISCOUNT     FROM WS-TOTC-DISCOUNT
                SUBTRACT INCR-INVCRED-COST FROM WS-TOTC-COST
                GO TO PR-002.
                
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC D-TAX-ERROR
           ADD 1 TO WS-LINE

            
            MOVE 2410 TO POS
            DISPLAY "Invoices Being Read:           " AT POS
            ADD 25 TO POS
            DISPLAY RANDOM-INVOICE AT POS.

           MOVE 2450 TO POS
           DISPLAY "DATE:" AT POS
           ADD 6 TO POS
           DISPLAY D-DATE AT POS.
       PR-950.
           IF INCR-SALES = 6 OR = 52
             GO TO PR-960
           ELSE
             GO TO PR-002.
       PR-960.
           IF INCR-TRANS = 1
              ADD INCR-INVCRED-AMT  TO WS-XPORTI-INVOICE
              ADD INCR-TAX          TO WS-XPORTI-TAX
              ADD INCR-ADDONS       TO WS-XPORTI-ADDON
              ADD INCR-DISCOUNT     TO WS-XPORTI-DISCOUNT
              ADD INCR-INVCRED-COST TO WS-XPORTI-COST
              ADD 1                 TO WS-XPORT-INVNO.
           IF INCR-TRANS = 6
              ADD INCR-INVCRED-AMT  TO WS-XPORTC-INVOICE
              ADD INCR-TAX          TO WS-XPORTC-TAX
              ADD INCR-ADDONS       TO WS-XPORTC-ADDON
              ADD INCR-DISCOUNT     TO WS-XPORTC-DISCOUNT
              ADD INCR-INVCRED-COST TO WS-XPORTC-COST
              ADD 1                 TO WS-XPORT-CRNO.
           GO TO PR-002.
       PR-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           ADD 1        TO WS-PAGE
           MOVE WS-PAGE TO H-PAGE
           MOVE 7       TO WS-LINE.

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC.

           IF WS-PAGE = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           WRITE PRINT-REC FROM HEAD3 AFTER 2
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
       PH-999.
           EXIT.
      *
       PRINT-TOTALS SECTION.
       PT-000.
           IF WS-LINE > 55
              PERFORM PRINT-HEADINGS.
           IF WS-INCR-ST1 = 88
             MOVE "NO RECORDS FROM THE DATE ENTERED, NO SALES TO PRINT."
             TO PRINT-REC
             WRITE PRINT-REC AFTER 1
             MOVE " " TO PRINT-REC
             WRITE PRINT-REC.
           MOVE WS-TOTI-INVOICE  TO TOT-INVAMT
           MOVE WS-TOTI-TAX      TO TOT-TAX
           MOVE WS-TOTI-ADDON    TO TOT-ADDON
           MOVE WS-TOTI-DISCOUNT TO TOT-DISCOUNT
           MOVE WS-TOTI-COST     TO TOT-COST
           COMPUTE WS-IMARGIN = WS-TOTI-INVOICE - WS-TOTI-COST
                                  - WS-TOTI-TAX - WS-TOTI-ADDON
           MOVE WS-IMARGIN       TO TOT-MARGIN
           COMPUTE WS-IPERC =
               (WS-IMARGIN / WS-TOTI-COST) * 100
           MOVE WS-IPERC         TO TOT-PERC
           MOVE "NUMBER OF INVOICES  :" TO TOT-DESC
           MOVE WS-INVNO         TO TOT-NO
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
           MOVE " " TO PRINT-REC.

           MOVE WS-TOTC-INVOICE  TO TOT-INVAMT
           MOVE WS-TOTC-TAX      TO TOT-TAX
           MOVE WS-TOTC-ADDON    TO TOT-ADDON
           MOVE WS-TOTC-DISCOUNT TO TOT-DISCOUNT
           MOVE WS-TOTC-COST     TO TOT-COST
           COMPUTE WS-CMARGIN = WS-TOTC-INVOICE - WS-TOTC-COST
                                  - WS-TOTC-TAX - WS-TOTC-ADDON
           MOVE WS-CMARGIN       TO TOT-MARGIN
           COMPUTE WS-CPERC =
               (WS-CMARGIN / WS-TOTC-COST) * 100
           MOVE WS-CPERC         TO TOT-PERC
           MOVE "NUMBER OF CREDITS   :" TO TOT-DESC
           MOVE WS-CRNO          TO TOT-NO
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC.

           SUBTRACT WS-TOTC-INVOICE  FROM WS-TOTI-INVOICE
           SUBTRACT WS-TOTC-TAX      FROM WS-TOTI-TAX
           SUBTRACT WS-TOTC-ADDON    FROM WS-TOTI-ADDON
           SUBTRACT WS-TOTC-DISCOUNT FROM WS-TOTI-DISCOUNT
           SUBTRACT WS-TOTC-COST     FROM WS-TOTI-COST
           SUBTRACT WS-CRNO          FROM WS-INVNO
           MOVE WS-TOTI-INVOICE  TO TOT-INVAMT
           MOVE WS-TOTI-TAX      TO TOT-TAX
           MOVE WS-TOTI-ADDON    TO TOT-ADDON
           MOVE WS-TOTI-DISCOUNT TO TOT-DISCOUNT
           MOVE WS-TOTI-COST     TO TOT-COST
           COMPUTE WS-IMARGIN = WS-TOTI-INVOICE - WS-TOTI-COST
                                  - WS-TOTI-TAX - WS-TOTI-ADDON
           MOVE WS-IMARGIN       TO TOT-MARGIN
           COMPUTE WS-IPERC =
               (WS-IMARGIN / WS-TOTI-COST) * 100
           MOVE WS-IPERC         TO TOT-PERC
           MOVE "NETT AMT. FOR PERIOD:" TO TOT-DESC
           MOVE WS-INVNO         TO TOT-NO
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC.
       PT-010.
           MOVE WS-XPORTI-INVOICE  TO TOT-INVAMT
           MOVE WS-XPORTI-TAX      TO TOT-TAX
           MOVE WS-XPORTI-ADDON    TO TOT-ADDON
           MOVE WS-XPORTI-DISCOUNT TO TOT-DISCOUNT
           MOVE WS-XPORTI-COST     TO TOT-COST
           COMPUTE WS-IMARGIN = WS-XPORTI-INVOICE - WS-XPORTI-COST
                                  - WS-XPORTI-TAX - WS-XPORTI-ADDON
           MOVE WS-IMARGIN       TO TOT-MARGIN
           COMPUTE WS-IPERC =
               (WS-IMARGIN / WS-XPORTI-COST) * 100
           MOVE WS-IPERC         TO TOT-PERC
           MOVE "NUMBER OF XPORT INVS:" TO TOT-DESC
           MOVE WS-XPORT-INVNO   TO TOT-NO
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
           MOVE " " TO PRINT-REC.

           MOVE WS-XPORTC-INVOICE  TO TOT-INVAMT
           MOVE WS-XPORTC-TAX      TO TOT-TAX
           MOVE WS-XPORTC-ADDON    TO TOT-ADDON
           MOVE WS-XPORTC-DISCOUNT TO TOT-DISCOUNT
           MOVE WS-XPORTC-COST     TO TOT-COST
           COMPUTE WS-CMARGIN = WS-XPORTC-INVOICE - WS-XPORTC-COST
                                  - WS-XPORTC-TAX - WS-XPORTC-ADDON
           MOVE WS-CMARGIN       TO TOT-MARGIN
           COMPUTE WS-CPERC =
               (WS-CMARGIN / WS-XPORTC-COST) * 100
           MOVE WS-CPERC         TO TOT-PERC
           MOVE "NUMBER XPORT CREDITS:" TO TOT-DESC
           MOVE WS-XPORT-CRNO    TO TOT-NO
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
       PT-999.
           EXIT.
      *
       READ-BY-DATE SECTION.
       RBD-001.
           MOVE "N" TO WS-RANDOM-WRITTEN.
           MOVE 2310 TO POS
           DISPLAY "READING BY DATE FIRST..." AT POS.
           MOVE WEEK-DATE TO INCR-DATE.
           START INCR-REGISTER KEY NOT < INCR-DATE
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
              MOVE 88 TO WS-INCR-ST1
              CLOSE RANDOM-FILE
              CLOSE INCR-REGISTER
              MOVE "N" TO WS-RANDOM-WRITTEN
              GO TO RBD-999.
       RBD-003.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10 OR = 23
              PERFORM ERROR-020
              MOVE 2210 TO POS
              DISPLAY WS-MESSAGE AT POS
              GO TO RBD-900.
           IF WS-INCR-ST1 NOT = 0
             MOVE 
           "REGISTER LOCKED RBD AT ANOTHER STATION, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE "LAST RECORD READ WAS:" TO WS-MESSAGE
             PERFORM ERROR-000
             MOVE 3055 TO POS
             DISPLAY D-INVNO AT POS
             PERFORM ERROR1-020
             PERFORM ERROR-010
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
           PERFORM OPEN-015.
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
               MOVE "RANDOM RECORD BUSY ON READ, 'ESC' TO SEE STATUS."
                TO WS-MESSAGE
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
              PERFORM ERROR-MESSAGE.
       WRR-999.
            EXIT.
      *
       CHECK-DATA-SIZE SECTION.         
       CDS-005.
           Move " " To Alpha-Rate.
           Move 0   To Sub-1.
       CDS-015.
           Add 1 To Sub-1.
           If Al-Rate (Sub-1) Not = " "
            If Sub-1 Not > 60
            Go To CDS-015.
          Subtract 1 from Sub-1.
       CDS-999.
          EXIT.
      *
       DELETE-TRANS SECTION.
       DST-010.
           CLOSE RANDOM-FILE.
           PERFORM CDS-005.
           Move Ws-Random-file To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE WS-random-file TO F-FILENAME
           MOVE SUB-1          TO F-CBFILENAME.
           CALL "OPENFILE" USING  F-ERROR1
                                  F-FH
                                  F-FILENAME
                                  F-CBFILENAME
                                  F-FILENAME
                                  F-INTEGERZERO
                                  F-OPENMODE-MM.
               
           CALL "DELETEFILE" USING F-ERROR1
                                   F-FH.
              
           PERFORM CDS-005.
           Move Ws-Random-file-IND To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE Ws-Random-file-Ind TO F-FILENAME
           MOVE Sub-1              TO F-CBFILENAME.
           CALL "OPENFILE" USING  F-ERROR1
                                  F-FH
                                  F-FILENAME
                                  F-CBFILENAME
                                  F-FILENAME
                                  F-INTEGERZERO
                                  F-OPENMODE-MM.
           CALL "DELETEFILE" USING F-ERROR1
                                   F-FH.
       DST-999.
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
       OPEN-FILES SECTION.
       OPEN-004.
           MOVE 2910 TO POS
           DISPLAY "Opening files ......" AT POS.
       OPEN-006.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.

           PERFORM READ-PARAMETER.
           CLOSE PARAMETER-FILE.
       OPEN-015.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-015.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-016.
           GO TO OPEN-036.
       OPEN-035.
           OPEN I-O RANDOM-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE
             "RANDOM FILE OPEN I-O ERROR, 'ESC' TO RETRY."
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
              MOVE
             "RANDOM FILE OPEN OUTPUT ERROR, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-RANDOM-ST1
              GO TO OPEN-036.
       OPEN-500.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H-DATE.
           PERFORM ERROR1-020.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF WS-LINE > 60
               PERFORM PRINT-HEADINGS.
           IF WS-RANDOM-WRITTEN NOT = "Y"
               MOVE "*** NOTHING TO PRINT IN THE RANGE SELECTED ***"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 1.
           IF WS-PERC-ACCEPT NOT = " "
               MOVE "*** ONLY PRINTED BELOW" TO WS-PERC-COMMENT
               MOVE "PERCENT AS ENTERED ***" TO WS-PERC-COMMENT2
               MOVE WS-PERC-DIS TO WS-PERC-NUM
               MOVE PERCENT-LINE TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE " " TO PRINT-REC.
           IF WS-RAND-ACCEPT NOT = " "
               MOVE "*** ONLY PRINTED ABOVE" TO WS-PERC-COMMENT
               MOVE "RAND AMT AS ENTERED **" TO WS-PERC-COMMENT2
               MOVE WS-RAND-DIS  TO WS-PERC-NUM
               MOVE PERCENT-LINE TO PRINT-REC
               WRITE PRINT-REC AFTER 1.
               
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE
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
