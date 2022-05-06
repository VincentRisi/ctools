        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrCredRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectDrMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
           Select HIGH-File Assign To WS-HIGH-FILE
               Organization Is Indexed
               Access Mode Is Dynamic
               File Status Is Ws-RANDOM-Status
               Record Key Is HIGH-Key.
           Select RANDOM-File Assign To WS-RANDOM-FILE
               Organization Is Indexed
               Access Mode Is Dynamic
               File Status Is Ws-RANDOM-Status
               Record Key Is RANDOM-Key.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
       FD  RANDOM-FILE.
       01  RANDOM-REC.
           03  RANDOM-KEY.
              05  RANDOM-NUMBER     PIC 9(7).
              05  RANDOM-INDEX      PIC 9(5).
           03  RANDOM-ACCOUNT       PIC 9(7).
           
       FD  HIGH-FILE.
       01  HIGH-REC.
           03  HIGH-KEY.
              05  HIGH-NUMBER     PIC 9(7).
           03  HIGH-ACCOUNT       PIC 9(7).
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(255).
      *
       WORKING-STORAGE SECTION.
       77  WS-RANDOM-FILE       PIC X(35) VALUE
              "/ctools/spl/RandomHighAccSales".
       77  WS-RANDOM-FILE-ind   PIC X(35) VALUE
              "/ctools/spl/RandomHighAccSales.Ind".
       77  WS-High-FILE         PIC X(35) VALUE
              "/ctools/spl/DebtorHighSales".
       77  WS-High-FILE-ind     PIC X(35) VALUE
              "/ctools/spl/DebtorHighSales.Ind".
       77  WS-RANDOM-WRITTEN    PIC X.
       77  WS-ACCEPT            PIC X VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-TOP               PIC X(5) VALUE " ".
       77  WS-PERIOD            PIC X VALUE " ".
       77  WS-SALESMAN          PIC X VALUE " ".
       77  WS-MARGIN-SALES      PIC X VALUE " ".
       77  WS-OVER-LIMIT        PIC X VALUE " ".
       77  WS-TOP-DIS           PIC Z(4)9.
       77  WS-SALESMAN-ACC      PIC 9(5) VALUE 0.
       77  WS-NO-ACC            PIC 9(5) VALUE 0.
       77  WS-NO-PRINTED        PIC 9(5) VALUE 0.
       77  WS-NO-READ           PIC 9(5) VALUE 0.
       77  WS-DISPLAY-PRINTED   PIC Z(4)9.
       77  WS-MARGIN            PIC S9(9)V99 VALUE 0.
       77  WS-TOTAL-SMAN-YTD    PIC S9(9)V99 VALUE 0.
       77  WS-TOTAL-SMAN-LAST   PIC S9(9)V99 VALUE 0.
       77  WS-TOTAL-SALES       PIC S9(9)V99 VALUE 0.
       77  WS-TOTAL-PERC        PIC S9(3)V9999 VALUE 0.
       77  WS-AVESALES          PIC S9(7)V99 VALUE 0.
       77  WS-CALCLIMIT         PIC S9(7) VALUE 0.
       77  WS-PERIODS           PIC 99 VALUE 0.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-RANDOM-STATUS.
           03  WS-RANDOM-ST1    PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(23) VALUE " ".
           03  FILLER         PIC X(7) VALUE "THE TOP".
           03  H1-NO-ACC      PIC Z(4)9.
           03  FILLER         PIC X(20) VALUE " ACCOUNTS".
           03  FILLER         PIC X(9) VALUE "BASED ON".
           03  H1-YEAR        PIC X(39) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(4) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(40) VALUE " ".
           03  FILLER         PIC X(21) VALUE ALL "*".
           03  FILLER         PIC X(60) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(39) VALUE
            "PLACE NUMBER   ACCOUNT NAME".
           03  FILLER         PIC X(14) VALUE "REP DISC".
           03  H3-BAL-SALE    PIC X(12) VALUE " ".
           03  H3-DESC        PIC X(43) VALUE " ".
           03  FILLER         PIC X(40) VALUE 
           "   LIMIT    AVERAGE  CALC LM".
       01  DETAIL-LINE.
           03  D-PLACE        PIC Z(4)9.
           03  FILLER         PIC X VALUE " ".
           03  D-ACCOUNT      PIC X(9) VALUE " ".
           03  D-NAME         PIC X(25) VALUE " ".
           03  FILLER         PIC X VALUE " ".
           03  D-REP          PIC X(4) VALUE " ".
           03  D-DISCOUNT     PIC X(7) VALUE " ".
           03  D-SALESPTD     PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SALESYTD     PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SALESLAST    PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PERC         PIC Z(2)9.9999.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-MARGIN       PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-LIMIT        PIC Z(6)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-AVESALES     PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-CALCLIMIT    PIC Z(6)9.
       01  TOTAL-LINE.
           03  FILLER           PIC X(37) VALUE " ".
           03  TOT-NAME         PIC X(37) VALUE " ".
           03  TOT-SALES        PIC Z(8)9.99-.
           03  FILLER           PIC X(44) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "** DEBTORS CREDIT LIMIT REPORT **" AT POS
           MOVE 415 TO POS
           DISPLAY "*********************************" AT POS.
       CONTROL-003.
           Copy "PrinterAcceptDr".
       CONTROL-010.
            MOVE 1210 TO POS.
            DISPLAY "How many Accounts should we print ? :   [     ]"
             AT POS.
            MOVE 1251 TO POS.
            MOVE "     " TO WS-TOP.

           MOVE ' '       TO CDA-DATA.
           MOVE 5         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TOP.

      *      ACCEPT WS-TOP AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
            IF WS-TOP = "    "
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-015
            ELSE
               MOVE 3010 TO POS
                DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-015.
            MOVE WS-TOP TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-TOP-DIS WS-NO-ACC.
            MOVE 1251 TO POS.
            DISPLAY WS-TOP-DIS AT POS.
            IF NUMERIC-RATE NOT > 0
                MOVE 3010 TO POS
                DISPLAY "THE NUMBER CANNOT BE NEGATIVE OR 0, RE-ENTER"
                AT POS
                GO TO CONTROL-010.
       CONTROL-025.
            PERFORM ERROR-020.
            MOVE 1410 TO POS.
            DISPLAY "Base Selection on; T=This Year, L=Last Year [ ]"
                AT POS.
            MOVE 1529 TO POS
            DISPLAY "B=Balance Owed on Account." AT POS
            MOVE 1455 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PERIOD.

      *      ACCEPT WS-PERIOD AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
            IF WS-PERIOD NOT = "T" AND NOT = "L" AND NOT = "B"
                DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-025.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-027
            ELSE
                DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-025.
       CONTROL-027.
            MOVE 1610 TO POS.
            DISPLAY "Base Selection on; P=Profit, S=Sales        [ ]"
                AT POS.
            MOVE 1655 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-MARGIN-SALES.

      *      ACCEPT WS-MARGIN-SALES AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-025.
            IF WS-MARGIN-SALES NOT = "P" AND NOT = "S"
                DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-027.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-030
            ELSE
                DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-027.
       CONTROL-030.
            MOVE 1710 TO POS.
            DISPLAY "Enter a SALESMAN No, leave blank for ALL    [ ]"
                AT POS.
            MOVE 1810 TO POS.
            DISPLAY "or Enter 'Z' for A/c's with NO salesman."
                AT POS.
            MOVE 1755 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SALESMAN.

      *      ACCEPT WS-SALESMAN AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-025.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-032
            ELSE
               MOVE 3010 TO POS
                DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-030.
       CONTROL-032.
            MOVE 2010 TO POS.
            DISPLAY "Print ONLY Acc's OVER The Credit Limit??    [ ]"
                AT POS.
            MOVE 2055 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-OVER-LIMIT.

      *      ACCEPT WS-OVER-LIMIT AT POS.
            IF WS-OVER-LIMIT NOT = "N" AND NOT = "Y"
                DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-032.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-030.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-035
            ELSE
               MOVE 3010 TO POS
                DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-032.
       CONTROL-035.
           MOVE 2910 TO POS
           DISPLAY "The Report Is Being Compiled......" AT POS
           PERFORM OPEN-FILES
           PERFORM READ-ALL-ACCOUNTS
           PERFORM READ-RANDOM-FILE.

           PERFORM PRINT-ROUTINE
      *     PERFORM DELETE-TRANS.
           PERFORM END-OFF.
      *
       READ-ALL-ACCOUNTS SECTION.
       RAA-001.
           MOVE 2910 TO POS
           DISPLAY "Reading ALL debtors for GROSS totals." AT POS
           START DEBTOR-MASTER KEY NOT < DR-KEY.
       RAA-005.
           READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
               GO TO RAA-900.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               GO TO RAA-005.
           IF WS-PERIOD = "B"
              ADD DR-BALANCE    TO WS-TOTAL-SALES.
           IF WS-PERIOD = "T"
              ADD DR-SALES-YTD  TO WS-TOTAL-SALES.
           IF WS-PERIOD = "L"
              ADD DR-SALES-LAST TO WS-TOTAL-SALES.
              
           MOVE 2510 TO POS
           DISPLAY "ACCOUNT BEING READ:" AT POS
           ADD 20 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
 
               
           IF WS-PERIOD = "B"
               MOVE DR-BALANCE    TO RANDOM-NUMBER.
               
           IF WS-PERIOD = "T"
            IF WS-MARGIN-SALES = "S"
               MOVE DR-SALES-YTD  TO RANDOM-NUMBER
            ELSE
               COMPUTE WS-MARGIN = DR-SALES-YTD - DR-COST-YTD
               MOVE WS-MARGIN     TO RANDOM-NUMBER.
               
           IF WS-PERIOD = "L"
            IF WS-MARGIN-SALES = "S"
               MOVE DR-SALES-LAST TO RANDOM-NUMBER
            ELSE
               COMPUTE WS-MARGIN = DR-SALES-LAST - DR-COST-LAST
               MOVE WS-MARGIN     TO RANDOM-NUMBER.
               
           IF RANDOM-NUMBER > 0
               MOVE 1 TO RANDOM-INDEX
               PERFORM WRITE-RANDOM-RECORD.
           GO TO RAA-005.
       RAA-900.
           CLOSE DEBTOR-MASTER.
           PERFORM OPEN-000.
       RAA-999.
           EXIT.
      *
       READ-RANDOM-FILE SECTION.
       RRF-000.
           PERFORM ERROR1-020.
           IF WS-RANDOM-WRITTEN NOT = "Y"
              MOVE "NOTHING TO PRINT IN THAT RANGE." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM END-850
              EXIT PROGRAM.
           CLOSE RANDOM-FILE.
           PERFORM OPEN-035.
           MOVE 2910 TO POS
           DISPLAY "READING ACCOUNTS BY LOWEST VALUES.          " AT POS.
           MOVE 99999 TO HIGH-NUMBER.
           MOVE 0     TO RANDOM-NUMBER
                         RANDOM-INDEX.
           START RANDOM-FILE KEY NOT < RANDOM-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "BAD START ON READ-RANDOM, 'ESC' TO SEE STATUS."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              EXIT PROGRAM.
       RRF-005.
           READ RANDOM-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-RANDOM-ST1 = 10
              GO TO RRF-999.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "READ RANDOM BUSY, 'ESC' TO RETRY." TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE RANDOM-NUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RRF-005.
              
           SUBTRACT 1 FROM HIGH-NUMBER
              
           MOVE 2510 TO POS
           DISPLAY "RANDOM NUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY RANDOM-ACCOUNT AT POS
           ADD 10 TO POS
           DISPLAY "COUNT DOWN" AT POS
           ADD 11 TO POS
           DISPLAY HIGH-NUMBER AT POS.
           
           PERFORM WRITE-HIGH-RECORD.
           GO TO RRF-005.
       RRF-999.
           EXIT.
      *
       WRITE-RANDOM-RECORD SECTION.
       WRR-005.
           IF WS-RANDOM-WRITTEN NOT = "Y"
               MOVE "Y" TO WS-RANDOM-WRITTEN.
           MOVE DR-ACCOUNT-NUMBER TO RANDOM-ACCOUNT.
           WRITE RANDOM-REC
              INVALID KEY NEXT SENTENCE.
              
           IF WS-RANDOM-ST1 NOT = 0
              ADD 1 TO RANDOM-INDEX
              GO TO WRR-005.
              
           GO TO WRR-999.
              
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "RANDOM RECORD INVALID ON WRITE, 'ESC' TO EXIT."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       WRR-999.
            EXIT.
      *
       WRITE-HIGH-RECORD SECTION.
       WRR-005.
           MOVE RANDOM-ACCOUNT TO HIGH-ACCOUNT.
           WRITE HIGH-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "HIGH RECORD INVALID ON WRITE, 'ESC' TO SEE STATUS."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       WRR-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-001.
           CLOSE HIGH-FILE.
           PERFORM OPEN-037.
           
           PERFORM ERROR1-020.
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2910 TO POS.
           DISPLAY "Processing of HIGHEST accounts in progress." AT POS.
           MOVE 0 TO HIGH-NUMBER
           START HIGH-FILE KEY NOT < HIGH-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "BAD START ON HIGH-FILE, 'ESC' TO EXIT."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              EXIT PROGRAM.
       PRR-006.
           READ HIGH-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-RANDOM-ST1 = 10
               GO TO PRR-999.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "DR-RECORD HIGH BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE 3010 TO POS
              DISPLAY "DEBTOR RECORD BUSY PRR-006 :" AT POS
              ADD 28 TO POS
              DISPLAY DR-ACCOUNT-NUMBER AT POS
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-RANDOM-ST1
              GO TO PRR-006.

           MOVE 2510 TO POS
           DISPLAY "DEBTOR NUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY HIGH-ACCOUNT AT POS.
           
           MOVE HIGH-ACCOUNT TO DR-ACCOUNT-NUMBER
           PERFORM READ-DEBTOR.

           IF WS-PERIOD = "B"
            IF DR-BALANCE NOT > 0
              GO TO PRR-006.
           IF WS-PERIOD = "T"
            IF DR-SALES-YTD = 0
              GO TO PRR-006.
           IF WS-PERIOD = "L"
            IF DR-SALES-LAST = 0
              GO TO PRR-006.
      *       
      *Z=A/C WITH NO SALESMAN
           IF WS-SALESMAN = "Z"
            IF DR-SALESMAN NOT = " "
               GO TO PRR-006
            ELSE
               GO TO PRR-010.

           IF WS-SALESMAN NOT = " "
             IF DR-SALESMAN NOT = WS-SALESMAN
              ADD 1 TO WS-NO-READ
              IF WS-NO-PRINTED = WS-NO-ACC
               GO TO PRR-999
              ELSE
               GO TO PRR-006.
       PRR-010.
           PERFORM ERROR-020.
           IF WS-OVER-LIMIT = "Y"
            IF DR-BALANCE NOT > DR-CREDIT-LIMIT
               GO TO PRR-006.
       
           IF LINE-CNT < 60
               GO TO PRR-020.
       PRR-015.
            ADD 1          TO PAGE-CNT.
            MOVE WS-NO-ACC TO H1-NO-ACC.
            IF WS-PERIOD = "B"
                MOVE "O/S BALANCES"       TO H1-YEAR
                MOVE "  BALANCE"          TO H3-BAL-SALE.
            IF WS-PERIOD = "T"
                MOVE "THIS YEARS FIGURES" TO H1-YEAR
                MOVE "SALES PTD"          TO H3-BAL-SALE.
            IF WS-PERIOD = "L"
                MOVE "LAST YEARS FIGURES" TO H1-YEAR
                MOVE "SALES PTD"          TO H3-BAL-SALE.
            MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC.
            
            IF WS-PERIOD NOT = "L"
            MOVE "SALES YTD   SALES L/Y   % TOTAL MARGIN YTD"
                TO H3-DESC
            ELSE
            MOVE "SALES YTD   SALES L/Y   % TOTAL MARGIN L/Y"
                TO H3-DESC.
            WRITE PRINT-REC FROM HEAD3
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE 7 TO LINE-CNT.
       PRR-020.
           ADD 1                  TO WS-NO-READ
                                     WS-NO-PRINTED
           MOVE WS-NO-READ        TO D-PLACE
           MOVE DR-ACCOUNT-NUMBER TO D-ACCOUNT
           MOVE DR-NAME           TO D-NAME
           MOVE DR-SALESMAN       TO D-REP
           MOVE DR-DISCOUNT-CODE  TO D-DISCOUNT
           MOVE DR-CREDIT-LIMIT   TO D-LIMIT.
           IF WS-PERIOD = "B"
              MOVE DR-BALANCE     TO D-SALESPTD
           ELSE
              MOVE DR-SALES-PTD   TO D-SALESPTD.
           MOVE DR-SALES-YTD      TO D-SALESYTD
           MOVE DR-SALES-LAST     TO D-SALESLAST.
           IF WS-PERIOD = "T" OR = "B"
              COMPUTE WS-MARGIN    = DR-SALES-YTD - DR-COST-YTD
              COMPUTE WS-AVESALES  = DR-SALES-YTD / WS-PERIODS
              COMPUTE WS-CALCLIMIT = WS-AVESALES * 2
           ELSE
              COMPUTE WS-MARGIN    = DR-SALES-LAST - DR-COST-LAST
              COMPUTE WS-AVESALES  = DR-SALES-LAST / WS-PERIODS
              COMPUTE WS-CALCLIMIT = WS-AVESALES * 2.

           MOVE WS-MARGIN         TO D-MARGIN
           MOVE WS-AVESALES       TO D-AVESALES
           MOVE WS-CALCLIMIT      TO D-CALCLIMIT.
           
     
           IF WS-PERIOD = "B"
              COMPUTE WS-TOTAL-PERC =
                 (DR-BALANCE / WS-TOTAL-SALES) * 100.
           IF WS-PERIOD = "T"
              COMPUTE WS-TOTAL-PERC =
                 (DR-SALES-YTD / WS-TOTAL-SALES) * 100.
           IF WS-PERIOD = "L"
              COMPUTE WS-TOTAL-PERC =
                 (DR-SALES-LAST / WS-TOTAL-SALES) * 100.
                 
           ADD DR-SALES-YTD  TO WS-TOTAL-SMAN-YTD
           ADD DR-SALES-LAST TO WS-TOTAL-SMAN-LAST.
           
           IF WS-SALESMAN NOT = " "
               ADD 1 TO WS-SALESMAN-ACC.
               
           MOVE WS-TOTAL-PERC TO D-PERC
           
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           ADD 1 TO LINE-CNT
           MOVE 2610 TO POS
           DISPLAY "TOTAL NUMBER OF ACCOUNTS READ:" AT POS
           MOVE WS-NO-PRINTED TO WS-DISPLAY-PRINTED
           ADD 31 TO POS
           DISPLAY WS-DISPLAY-PRINTED AT POS.
           IF WS-NO-PRINTED = WS-NO-ACC
               GO TO PRR-999.
           GO TO PRR-006.
       PRR-999.
           EXIT.
      *
       READ-DEBTOR SECTION.
       RD-005.
           READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
              GO TO RD-005.
       RD-999.
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
           IF F-ERROR1 NOT = 0
             MOVE "ERROR IN DELETING RANDOM FILE" TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE F-ERROR1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE F-FILENAME TO WS-MESSAGE
             PERFORM ERROR-MESSAGE.
              
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
           IF F-ERROR1 NOT = 0
             MOVE "ERROR IN DELETING RANDOM.IND FILE" TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE F-ERROR1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE F-FILENAME TO WS-MESSAGE
             PERFORM ERROR-MESSAGE.
       DST-020.
           CLOSE HIGH-FILE.
           PERFORM CDS-005.
           Move Ws-HIGH-file To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE WS-HIGH-file    TO F-FILENAME
           MOVE SUB-1           TO F-CBFILENAME.
           CALL "OPENFILE" USING  F-ERROR1
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE-MM.
           CALL "DELETEFILE" USING F-ERROR1
                                    F-FH.
           IF F-ERROR1 NOT = 0
             MOVE "ERROR IN DELETING HIGH FILE" TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE F-ERROR1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE F-FILENAME TO WS-MESSAGE
             PERFORM ERROR-MESSAGE.
              
           PERFORM CDS-005.
           Move Ws-HIGH-file-IND To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE Ws-HIGH-file-Ind TO F-FILENAME
           MOVE Sub-1        TO F-CBFILENAME.
           CALL "OPENFILE" USING  F-ERROR1
                                  F-FH
                                  F-FILENAME
                                  F-CBFILENAME
                                  F-FILENAME
                                  F-INTEGERZERO
                                  F-OPENMODE-MM.
           CALL "DELETEFILE" USING F-ERROR1
                                   F-FH.
           IF F-ERROR1 NOT = 0
             MOVE "ERROR IN DELETING HIGH.IND FILE" TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE F-ERROR1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE F-FILENAME TO WS-MESSAGE
             PERFORM ERROR-MESSAGE.
       DST-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
           
           COMPUTE WS-PERIODS = SPLIT-MM - 2.
           
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE
           MOVE Ws-Co-Name To Co-Name.
           
           GO TO OPEN-036.
       OPEN-035.
           OPEN I-O RANDOM-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE
             "RANDOM FILE OPEN I-O AT ANOTHER COMPUTER, 'ESC' TO EXIT."
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
             "RANDOM OUTPUT OPEN AT ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-RANDOM-ST1
             GO TO OPEN-036.
       OPEN-0361.
             GO TO OPEN-038.
       OPEN-037.
           OPEN I-O HIGH-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE
             "HIGH FILE OPEN I-O AT ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO OPEN-037.
       OPEN-038.
           OPEN OUTPUT HIGH-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE
             "HIGH OUTPUT OPEN AT ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
             GO TO OPEN-038.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 55
               PERFORM PRR-015.
           IF WS-SALESMAN = " "
                GO TO END-600.
           MOVE " " TO PRINT-REC DETAIL-LINE
           MOVE WS-SALESMAN-ACC    TO D-PLACE
           MOVE WS-SALESMAN        TO D-REP
           MOVE WS-TOTAL-SMAN-YTD  TO D-SALESYTD
           MOVE WS-TOTAL-SMAN-LAST TO D-SALESLAST.
           IF WS-PERIOD = "T"
             COMPUTE WS-TOTAL-PERC =
                (WS-TOTAL-SMAN-YTD / WS-TOTAL-SALES) * 100
           ELSE
             COMPUTE WS-TOTAL-PERC =
                (WS-TOTAL-SMAN-LAST / WS-TOTAL-SALES) * 100.
           MOVE WS-TOTAL-PERC TO D-PERC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE.
       END-600.
           MOVE "TOTAL SALES - ALL ACC'S           :R" TO TOT-NAME
           MOVE WS-TOTAL-SALES                         TO TOT-SALES.
           WRITE PRINT-REC
           WRITE PRINT-REC FROM TOTAL-LINE.
           
           
           MOVE "TOTAL SALES FOR ACC'S PRINTED YTD :R" TO TOT-NAME
           MOVE WS-TOTAL-SMAN-YTD                      TO TOT-SALES
           WRITE PRINT-REC FROM TOTAL-LINE.
           
           MOVE "TOTAL SALES FOR ACC'S PRINTED L/YR:R" TO TOT-NAME
           MOVE WS-TOTAL-SMAN-LAST                     TO TOT-SALES.
           WRITE PRINT-REC FROM TOTAL-LINE.

           IF WS-MARGIN-SALES = "S"
              MOVE "ORDER BASED ON HIGHEST SALES." TO PRINT-REC
           ELSE
              MOVE "ORDER BASED ON HIGHEST MARGIN." TO PRINT-REC.
           WRITE PRINT-REC AFTER 2.
           
           IF WS-OVER-LIMIT = "Y"
              MOVE 
           "** ONLY ACC'S OVER THE CREDIT LIMIT HAVE BEEN PRINTED. **"
              TO PRINT-REC
           ELSE
              MOVE "** ALL ACCOUNTS HAVE BEEN PRINTED. **"
              TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-800.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-850.
           CLOSE DEBTOR-MASTER.
       END-900.
      *     STOP RUN.
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
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
