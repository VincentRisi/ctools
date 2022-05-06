        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlRgAcRp.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectSlRegister".
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
           
       FD  RANDOM-FILE.
       01  RANDOM-REC.
           03  RANDOM-KEY.
               05  RANDOM-INVOICE       PIC 9(7).
           
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).

      *
       WORKING-STORAGE SECTION.
       77  WS-RANDOM-FILE       PIC X(30) VALUE
              "/ctools/spl/RandomInvPrt".
       77  WS-RANDOM-FILE-ind   PIC X(30) VALUE
              "/ctools/spl/RandomInvPrt.Ind".
       77  WS-TOTI-INVOICE      PIC S9(7)V99 VALUE 0.
       77  WS-TOTI-TAX          PIC S9(7)V99 VALUE 0.
       77  WS-TOTI-ADDON        PIC S9(7)V99 VALUE 0.
       77  WS-TOTI-DISCOUNT     PIC S9(7)V99 VALUE 0.
       77  WS-TOTI-COST         PIC S9(7)V99 VALUE 0.
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-ACCOUNT-ACCEPT    PIC X(7) VALUE " ".
       77  WS-INVNO             PIC 9(5) VALUE 0.
       77  WS-CRNO              PIC 9(5) VALUE 0.
       77  WS-IMARGIN           PIC S9(7)V99 VALUE 0.
       77  WS-IPERC             PIC S9(7)V99 VALUE 0.
       77  WS-XPORTI-INVOICE    PIC S9(7)V99 VALUE 0.
       77  WS-XPORTI-TAX        PIC S9(7)V99 VALUE 0.
       77  WS-XPORTI-ADDON      PIC S9(7)V99 VALUE 0.
       77  WS-XPORTI-DISCOUNT   PIC S9(7)V99 VALUE 0.
       77  WS-XPORTI-COST       PIC S9(7)V99 VALUE 0.
       77  WS-XPORTC-INVOICE    PIC S9(7)V99 VALUE 0.
       77  WS-XPORTC-TAX        PIC S9(7)V99 VALUE 0.
       77  WS-XPORTC-ADDON      PIC S9(7)V99 VALUE 0.
       77  WS-XPORTC-DISCOUNT   PIC S9(7)V99 VALUE 0.
       77  WS-XPORTC-COST       PIC S9(7)V99 VALUE 0.
       77  WS-XPORT-INVNO       PIC 9(5) VALUE 0.
       77  WS-XPORT-CRNO        PIC 9(5) VALUE 0.
       77  WS-XPORT-MARGIN      PIC S9(7)V99 VALUE 0.
       77  WS-XPORT-PERC        PIC S9(7)V99 VALUE 0.
       77  WS-TOTC-INVOICE      PIC S9(7)V99 VALUE 0.
       77  WS-TOTC-TAX          PIC S9(7)V99 VALUE 0.
       77  WS-TOTC-ADDON        PIC S9(7)V99 VALUE 0.
       77  WS-TOTC-DISCOUNT     PIC S9(7)V99 VALUE 0.
       77  WS-TOTC-COST         PIC S9(7)V99 VALUE 0.
       77  WS-CMARGIN           PIC S9(7)V99 VALUE 0.
       77  WS-CPERC             PIC S9(7)V99 VALUE 0.
       77  WS-ACCEPT            PIC X(10) VALUE " ".
       77  WS-YYACCEPT          PIC X(4) VALUE " ".
       77  WS-PERC-ACCEPT       PIC X(11) VALUE " ".
       77  WS-PERC-DIS          PIC Z(7)9.99.
       77  WS-PERC              PIC S9(7)V99 VALUE 0.
       77  WS-RAND-ACCEPT       PIC X(11) VALUE " ".
       77  WS-RAND-DIS          PIC Z(7)9.99.
       77  WS-RAND              PIC S9(7)V99 VALUE 0.
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-LINE-PRINTED      PIC X.
       01  W-DD                 PIC Z9.
       01  W-YY                 PIC ZZZ9.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
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
           03  FILLER           PIC X(2) VALUE "RP".
           03  FILLER           PIC X(2) VALUE "PS".
           03  FILLER           PIC X(2) VALUE "  ".
           03  FILLER           PIC X(2) VALUE "CN".
           03  FILLER           PIC X(2) VALUE "  ".
           03  FILLER           PIC X(2) VALUE "QU".
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
           "INVOICE REGISTER BY ACCOUNT NUMBER".
           03  FILLER           PIC X(6) VALUE "PAGE: ".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(4) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(49) VALUE " ".
           03  FILLER           PIC X(103) VALUE 
           "**********************************".
       01  HEAD3.
           03  FILLER           PIC X(10) VALUE " TRANS TY".
           03  FILLER           PIC X(35) VALUE "  ACC".
           03  FILLER           PIC X(29) VALUE
            "INVOICE   SA   INVOICE".
           03  FILLER           PIC X(18) VALUE "TAX    ADD-ON".
           03  FILLER           PIC X(20) VALUE "DISC.      COST".
           03  FILLER           PIC X(33) VALUE "GROSS      %     SO".
       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "NUMBER PE".
           03  FILLER           PIC X(35) VALUE " NUMBER NAME".
           03  FILLER           PIC X(26) VALUE
           "  DATE    LE    AMOUNT".
           03  FILLER           PIC X(20) VALUE "AMOUNT    AMOUNT".
           03  FILLER           PIC X(19) VALUE "AMOUNT    AMOUNT".
           03  FILLER           PIC X(34) VALUE " AMOUNT   MARGIN   LD".
       01  DETAIL-LINE.
           03  D-INVNO          PIC Z(5)9.
           03  FILLER           PIC X(1) VALUE " ".
           03  D-TYPE           PIC X(3) VALUE " ".
           03  D-CUSTNO         PIC X(8) VALUE " ".
           03  D-NAME           PIC X(25) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  D-DATE           PIC X(10).
           03  FILLER           PIC X VALUE " ".
           03  D-SALES          PIC X(3) VALUE " ".
           03  D-INVAMT         PIC Z(5)9.99.
           03  FILLER           PIC X VALUE " ".
           03  D-TAX            PIC Z(5)9.99.
           03  FILLER           PIC X VALUE " ".
           03  D-ADDON          PIC Z(5)9.99.
           03  FILLER           PIC X VALUE " ".
           03  D-DISCOUNT       PIC Z(5)9.99.
           03  FILLER           PIC X VALUE " ".
           03  D-COST           PIC Z(5)9.99.
           03  FILLER           PIC X VALUE " ".
           03  D-MARGIN         PIC Z(5)9.99-.
           03  FILLER           PIC X VALUE " ".
           03  D-PERC           PIC Z(3)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  D-SOLDBY         PIC XX VALUE " ".
       01  TOTAL-LINE.
           03  FILLER           PIC X(15) VALUE " ".
           03  TOT-DESC         PIC X(22) VALUE " ".
           03  TOT-NO           PIC Z(4)9.
           03  FILLER           PIC X(5) VALUE " ".
           03  FILLER           PIC X(10) VALUE "TOTALS:".
           03  TOT-INVAMT       PIC Z(6)9.99.
           03  FILLER           PIC X VALUE " ".
           03  TOT-TAX          PIC Z(5)9.99.
           03  FILLER           PIC XX VALUE " ".
           03  TOT-ADDON        PIC Z(4)9.99.
           03  FILLER           PIC X VALUE " ".
           03  TOT-DISCOUNT     PIC Z(5)9.99.
           03  FILLER           PIC X VALUE " ".
           03  TOT-COST         PIC Z(5)9.99.
           03  FILLER           PIC X VALUE " ".
           03  TOT-MARGIN       PIC Z(5)9.99-.
           03  FILLER           PIC X VALUE " ".
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
           DISPLAY "** Invoice Register By Account Report **" AT POS.
           MOVE 420 TO POS.
           DISPLAY "****************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-008.
           PERFORM OPEN-500.
       CONTROL-009.
           MOVE 0810 TO POS.
           DISPLAY "Enter A Beginning Date To Print From : [          ]"
            AT POS.
           ADD 40 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

      *     ACCEPT WS-ACCEPT AT POS.
           MOVE WS-ACCEPT TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
               GO TO CONTROL-009.
           MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE
           DISPLAY DISPLAY-DATE AT POS
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE      TO WS-BEG-DATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
               GO TO CONTROL-009.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF WS-BEG-DATE NOT > 0
              DISPLAY "Enter a Valid Date to Start From." AT POS
              MOVE " " TO WS-ACCEPT
              GO TO CONTROL-009.
           MOVE " " TO WS-ACCEPT.
       CONTROL-010.
           MOVE 1010 TO POS.
           DISPLAY "Enter An End Date To Print Up To     : [          ]"
            AT POS.
           ADD 40 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

      *     ACCEPT WS-ACCEPT AT POS.
           MOVE WS-ACCEPT TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
               GO TO CONTROL-010.
           MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE
           DISPLAY DISPLAY-DATE AT POS
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE      TO WS-END-DATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
               GO TO CONTROL-010.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-009.
           IF WS-END-DATE NOT > 0
              MOVE 2510 TO POS
              DISPLAY "Enter a Valid Date to Print up To." AT POS
              MOVE " " TO WS-ACCEPT
              GO TO CONTROL-010.
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
               GO TO CONTROL-010.
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
          "ENTER A RAND AMT ABOVE WHICH TO PRINT :[           ]"
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
               GO TO CONTROL-014.
           MOVE WS-RAND-ACCEPT TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WS-RAND.
           MOVE WS-RAND      TO WS-RAND-DIS.
           DISPLAY WS-RAND-DIS AT POS.
       CONTROL-014.
           MOVE 1810 TO POS.
           DISPLAY "ENTER AN ACCOUNT NUMBER TO PRINT.     :[       ]"
            AT POS.
           ADD 40 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCOUNT-ACCEPT.

      *     ACCEPT WS-ACCOUNT-ACCEPT AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-013.
           IF WS-ACCOUNT-ACCEPT = " "
               GO TO CONTROL-014.
           MOVE WS-ACCOUNT-ACCEPT TO WS-ACCOUNT-NUMBER.
       CONTROL-015.
           MOVE 2510 TO POS.
           DISPLAY "The report is being compiled ......." AT POS.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE
           PERFORM PRINT-TOTALS
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PR-001.
           MOVE " " TO WS-LINE-PRINTED.
           MOVE 2310 TO POS
           DISPLAY "READING INVOICE REGISTER BY NUMBER..." AT POS.
           MOVE WS-BEG-DATE TO INCR-DATE
           START INCR-REGISTER KEY NOT < INCR-DATE
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
              MOVE 88 TO WS-INCR-ST1
              GO TO PR-999.
       PR-003.
           READ INCR-REGISTER NEXT
              AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
              GO TO PR-999.
           IF WS-INCR-ST1 NOT = 0
             MOVE "RECORD LOCKED AT ANOTHER STATION, 'ESC' TO RETRY."
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
           IF INCR-DATE < WS-BEG-DATE
               GO TO PR-003.
           IF INCR-DATE > WS-END-DATE
               GO TO PR-999.
             
      *     IF INCR-TRANS NOT = 1 AND NOT = 4 AND NOT = 6
      *         GO TO PR-003.
           
           MOVE INCR-INVOICE TO D-INVNO.
           
           IF INCR-ACCOUNT NOT = WS-ACCOUNT-NUMBER
               GO TO PR-003.
       PR-010.
           IF WS-LINE > 60
               PERFORM PRINT-HEADINGS.
           MOVE INCR-INVOICE              TO D-INVNO
           MOVE WS-TYPE-DESC (INCR-TRANS) TO D-TYPE
           MOVE INCR-DATE                 TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-DATE
           MOVE INCR-SB-TYPE      TO D-SOLDBY
           MOVE INCR-ACCOUNT      TO D-CUSTNO
           MOVE INCR-NAME         TO D-NAME
           MOVE INCR-SALES        TO D-SALES
           MOVE INCR-INVCRED-AMT  TO D-INVAMT
           MOVE INCR-TAX          TO D-TAX
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
                GO TO PR-003.
           IF WS-PERC-ACCEPT NOT = " "
            IF INCR-TRANS = 6
                SUBTRACT 1                 FROM WS-CRNO
                SUBTRACT INCR-INVCRED-AMT  FROM WS-TOTC-INVOICE
                SUBTRACT INCR-TAX          FROM WS-TOTC-TAX
                SUBTRACT INCR-ADDONS       FROM WS-TOTC-ADDON
                SUBTRACT INCR-DISCOUNT     FROM WS-TOTC-DISCOUNT
                SUBTRACT INCR-INVCRED-COST FROM WS-TOTC-COST
                GO TO PR-003.
           IF WS-RAND-ACCEPT NOT = " "
            IF INCR-TRANS = 1
             IF WS-RAND > INCR-INVCRED-AMT
                SUBTRACT 1                 FROM WS-INVNO
                SUBTRACT INCR-INVCRED-AMT  FROM WS-TOTI-INVOICE
                SUBTRACT INCR-TAX          FROM WS-TOTI-TAX
                SUBTRACT INCR-ADDONS       FROM WS-TOTI-ADDON
                SUBTRACT INCR-DISCOUNT     FROM WS-TOTI-DISCOUNT
                SUBTRACT INCR-INVCRED-COST FROM WS-TOTI-COST
                GO TO PR-003.
           IF WS-RAND-ACCEPT NOT = " "
            IF INCR-TRANS = 6
                SUBTRACT 1                 FROM WS-CRNO
                SUBTRACT INCR-INVCRED-AMT  FROM WS-TOTC-INVOICE
                SUBTRACT INCR-TAX          FROM WS-TOTC-TAX
                SUBTRACT INCR-ADDONS       FROM WS-TOTC-ADDON
                SUBTRACT INCR-DISCOUNT     FROM WS-TOTC-DISCOUNT
                SUBTRACT INCR-INVCRED-COST FROM WS-TOTC-COST
                GO TO PR-003.
                
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO WS-LINE

           IF WS-LINE-PRINTED = " "
               MOVE "Y" TO WS-LINE-PRINTED.
            
           MOVE 2410 TO POS
           DISPLAY "Invoices Being Read:           " AT POS
           ADD 21 TO POS
           DISPLAY INCR-INVOICE AT POS.

           MOVE 2440 TO POS
           DISPLAY "DATE:" AT POS
           ADD 6 TO POS
           DISPLAY D-DATE AT POS.
       PR-950.
           IF INCR-SALES = 6 OR = 52
             GO TO PR-960
           ELSE
             GO TO PR-003.
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
           GO TO PR-003.
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
       OPEN-FILES SECTION.
       OPEN-014.
           MOVE 2910 TO POS
           DISPLAY "Opening files ......" AT POS.
           
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-500.
           PERFORM GET-SYSTEM-Y2K-DATE.
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
           IF WS-LINE-PRINTED NOT = "Y"
               MOVE "*** NOTHING TO PRINT IN THE RANGE SELECTED ***"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 1.
           
           MOVE "ONLY INV & C/NOTE TRANS INCLUDED IN TOTALS."
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
