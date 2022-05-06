        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlSaleRp.
        AUTHOR.     STEVE CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        REPOSITORY. 
           FUNCTION ALL INTRINSIC.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectSlSbRep".
         Copy "SelectSlMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdSbRep.
           COPY ChlfdSales.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(80).
      *
       WORKING-STORAGE SECTION.
       77  WS-MESSAGE-PART      PIC X(79) VALUE " ".
       77  WS-MESS              PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-ACCEPT            PIC X(10) VALUE " ".
       77  WS-RANGE             PIC X VALUE " ".
       77  WS-TOTALS-ONLY       PIC X VALUE "N".
       77  WS-DAYS-ACCEPT       PIC X(2) VALUE " ".
       77  WS-DAYS-DISPLAY      PIC Z9.
       77  WS-DAYS              PIC 9(2) VALUE 0.
       77  WS-STORE             PIC X(3) VALUE " ".
       77  TOT-SALES-BUDGET     PIC S9(8)V99 VALUE 0.
       77  TOT-PROFIT-BUDGET    PIC S9(8)V99 VALUE 0.
       77  TOT-SALESAMT-PTD     PIC S9(8)V99 VALUE 0.
       77  TOT-SALESAMT-YTD     PIC S9(8)V99 VALUE 0.
       77  TOT-SALESAMT-LAST    PIC S9(8)V99 VALUE 0.
       77  TOT-COST-PTD         PIC S9(8)V99 VALUE 0.
       77  TOT-COST-YTD         PIC S9(8)V99 VALUE 0.
       77  TOT-COST-LAST        PIC S9(8)V99 VALUE 0.
       77  WS-SALESAMT          PIC S9(8)V99 VALUE 0.
       77  WS-COST              PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-BU         PIC S9(8)V99 VALUE 0.
       77  WS-PERC              PIC S9(4)V99 VALUE 0.
       77  WS-PERC-BU           PIC S9(3) VALUE 0.
       77  WS-CO-SALES-BU       PIC S9(8)V99 VALUE 0.
       77  WS-CO-PROFIT-BU      PIC S9(8)V99 VALUE 0.
       77  WS-BUDGET-SOFAR      PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-SALES-WEEK PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-SALES-PTD  PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-SALES-YTD  PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-COST-WEEK  PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-COST-PTD   PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-COST-YTD   PIC S9(8)V99 VALUE 0.
       77  TOT-SALESWEEK        PIC S9(8)V99 VALUE 0.
       77  TOT-SALESPTD         PIC S9(8)V99 VALUE 0.
       77  TOT-SALESYTD         PIC S9(8)V99 VALUE 0.
       77  TOT-COSTWEEK         PIC S9(8)V99 VALUE 0.
       77  TOT-COSTPTD          PIC S9(8)V99 VALUE 0.
       77  TOT-COSTYTD          PIC S9(8)V99 VALUE 0.
       77  TOT-MARGIN           PIC S9(8)V99 VALUE 0.
       77  TOT-PERC             PIC S9(4)V99.
       01  WS-SALESREP-NAMES.
         02  WS-SALESREP-INFO OCCURS 20.
           03  WS-REP-NUM          PIC X.
           03  WS-SALESAMT-PTD     PIC S9(8)V99 VALUE 0.
           03  WS-SALESAMT-YTD     PIC S9(8)V99 VALUE 0.
           03  WS-SALESAMT-LAST    PIC S9(8)V99 VALUE 0.
           03  WS-COST-PTD         PIC S9(8)V99 VALUE 0.
           03  WS-COST-YTD         PIC S9(8)V99 VALUE 0.
           03  WS-COST-LAST        PIC S9(8)V99 VALUE 0.
       01  WS-WEEK-DAYS.
           03  FILLER            PIC X(3) VALUE "Mon".
           03  FILLER            PIC X(3) VALUE "Tue".
           03  FILLER            PIC X(3) VALUE "Wed".
           03  FILLER            PIC X(3) VALUE "Thu".
           03  FILLER            PIC X(3) VALUE "Fri".
           03  FILLER            PIC X(3) VALUE "Sat".
           03  FILLER            PIC X(3) VALUE "Sun".
       01  WS-DAYS-DIS REDEFINES WS-WEEK-DAYS.
           03  WS-DAYS-DESC      PIC X(3) OCCURS 7.
       01  WS-SALES-STATUS.
           03  WS-SALES-ST1      PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1     PIC 99.
       01  WS-SBREP-STATUS.
           03  WS-SBREP-ST1      PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(6) VALUE " ".
           03  FILLER         PIC X(47) VALUE
           "ANALYSIS OF SALES BY REP AND COMPANY".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
       01  HEAD2.
           03  FILLER         PIC X(23) VALUE " ".
           03  FILLER         PIC X(36) VALUE ALL "*".
           03  FILLER         PIC X(11) VALUE " ".
           03  FILLER         PIC X(6) VALUE "DAYS:".
           03  H2-DAYS        PIC Z9.
       01  HEAD3.
           03  FILLER         PIC X(20) VALUE " ".
           03  FILLER         PIC X(32) VALUE
            "BUDGET MONTH / YEAR TO DATE".
           03  H3-NAME        PIC X(25) VALUE
           " MONTH / YEAR / LAST YEAR".
       01  HEAD4.
           03  H4-NAME        PIC X(22) VALUE "SALESMAN # & NAME".
           03  FILLER         PIC X(24) VALUE "SALES   %     PROFIT".
           03  FILLER         PIC X(8) VALUE "%".
           03  FILLER         PIC X(11) VALUE "SALES".
           03  FILLER         PIC X(10) VALUE "PROFIT".
           03  FILLER         PIC X(3) VALUE "%".
       01  DETAIL-LINE.
           03  D-CATEGORY.
               05  D-NUMBER       PIC X(2) VALUE " ".
               05  D-SALESMAN     PIC X(14) VALUE " ".
           03  D-SALES-BU         PIC Z(7)9.99.
           03  FILLER             PIC X VALUE " ".
           03  D-SALES-BU-PERC    PIC ZZ9-.
           03  D-PROFIT-BU        PIC Z(7)9.99.
           03  FILLER             PIC X VALUE " ".
           03  D-PROFIT-BU-PERC   PIC ZZ9-.
           03  D-SALESAMT         PIC Z(7)9.99-.
           03  D-MARGIN           PIC Z(7)9.99-.
           03  D-PERC             PIC Z(2)9.99.
       01  TOTAL-LINE.
           03 TOT-DESC        PIC X(28).
           03 TOT-DATE        PIC X(10).
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
            PERFORM CLEAR-SCREEN.
            MOVE 315 TO POS.
            DISPLAY "** SALES ANALYSIS OF SALESMAN & PROFITS **" AT POS
            MOVE 415 TO POS
            DISPLAY "******************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-015.
            MOVE 1510 TO POS.
            DISPLAY
            "How many DAYS have been worked this month : [  ]" AT POS
            ADD 45 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DAYS-ACCEPT.

      *      ACCEPT WS-DAYS-ACCEPT AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-003.
            MOVE WS-DAYS-ACCEPT TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-DAYS WS-DAYS-DISPLAY H2-DAYS
            DISPLAY WS-DAYS-DISPLAY AT POS.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-020
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
            PERFORM OPEN-FILES.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            MOVE 2510 TO POS
            DISPLAY "The Report Is Being Compiled, Please Be Patient."
            AT POS.
       CONTROL-045.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE 2210 TO POS
           DISPLAY "Reading ALL Debtors for Salesman Info....." AT POS.
           MOVE 0 TO DR-SALESMAN.
           START DEBTOR-MASTER KEY NOT < DR-SALESMAN
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE 0 TO WS-DEBTOR-ST1
              GO TO PRR-999.
       PRR-005.
           READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
               PERFORM ERROR1-020
               MOVE 2310 TO POS
               DISPLAY WS-MESSAGE AT POS
               MOVE 2210 TO POS
               DISPLAY WS-MESSAGE AT POS
               MOVE 1 TO SUB-1
               GO TO PRR-010.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PRR-005.
              
           IF DR-SALESMAN = " "
              GO TO PRR-005.
              
           MOVE 2310 TO POS
           DISPLAY "Reading account Number :" AT POS
           ADD 25 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS
           ADD 10 TO POS
           DISPLAY "SALESMAN:" AT POS
           ADD 10 TO POS
           DISPLAY DR-SALESMAN AT POS.
              
           MOVE 1 TO SUB-1.
           PERFORM PRR-020.
           GO TO PRR-005.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-025.
       PRR-015.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
               
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 1.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD4 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC AFTER 1.

            MOVE 7 TO LINE-CNT.
       PRR-016.
           GO TO PRR-025.
       PRR-020.
           IF WS-REP-NUM (SUB-1) NOT = " "
            IF WS-REP-NUM (SUB-1) NOT = DR-SALESMAN
               ADD 1 TO SUB-1
               GO TO PRR-020.
               
           IF WS-REP-NUM (SUB-1) = " "
            MOVE DR-SALESMAN TO WS-REP-NUM (SUB-1).
            
           ADD DR-SALES-PTD  TO WS-SALESAMT-PTD (SUB-1)
           ADD DR-SALES-YTD  TO WS-SALESAMT-YTD (SUB-1)
           ADD DR-SALES-LAST TO WS-SALESAMT-LAST (SUB-1)
           ADD DR-COST-PTD   TO WS-COST-PTD (SUB-1)
           ADD DR-COST-YTD   TO WS-COST-YTD (SUB-1)
           ADD DR-COST-LAST  TO WS-COST-LAST (SUB-1).
       PRR-025.
           PERFORM READ-SBREP.
           ADD SBREP-SALES-BUDGET   TO TOT-SALES-BUDGET
           ADD SBREP-PROFIT-BUDGET  TO TOT-PROFIT-BUDGET.
       
           MOVE SBREP-REP               TO D-NUMBER
           MOVE WS-SALESAMT-PTD (SUB-1) TO D-SALESAMT
           COMPUTE WS-MARGIN =
                WS-SALESAMT-PTD (SUB-1) - WS-COST-PTD (SUB-1)
           MOVE WS-MARGIN               TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
                WS-MARGIN / WS-COST-PTD (SUB-1) * 100
           MOVE WS-PERC                 TO D-PERC.
           
           COMPUTE SBREP-SALES-BUDGET = 
                (SBREP-SALES-BUDGET / 21) * WS-DAYS.
           MOVE SBREP-SALES-BUDGET       TO D-SALES-BU
           COMPUTE WS-MARGIN-BU =
              (WS-SALESAMT-PTD (SUB-1) - SBREP-SALES-BUDGET).
           COMPUTE WS-PERC-BU ROUNDED = 
               (WS-MARGIN-BU / SBREP-SALES-BUDGET) * 100.
               MOVE WS-PERC-BU          TO D-SALES-BU-PERC.
           
           COMPUTE SBREP-PROFIT-BUDGET =
                (SBREP-PROFIT-BUDGET / 21) * WS-DAYS.
           MOVE SBREP-PROFIT-BUDGET     TO D-PROFIT-BU
           COMPUTE WS-MARGIN-BU =
               WS-MARGIN - SBREP-PROFIT-BUDGET
           COMPUTE WS-PERC-BU ROUNDED = 
               (WS-MARGIN-BU / SBREP-PROFIT-BUDGET) * 100.
               MOVE WS-PERC-BU          TO D-PROFIT-BU-PERC.
           
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           MOVE " " TO D-CATEGORY DETAIL-LINE.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.

           MOVE WS-SALESAMT-YTD (SUB-1) TO D-SALESAMT
           COMPUTE WS-MARGIN =
                WS-SALESAMT-YTD (SUB-1) - WS-COST-YTD (SUB-1)
           MOVE WS-MARGIN         TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
                WS-MARGIN / WS-COST-YTD (SUB-1) * 100
           MOVE WS-PERC           TO D-PERC.
           
           PERFORM READ-SBREP.
           MOVE " " TO D-CATEGORY.

           COMPUTE SBREP-SALES-BUDGET =
                SBREP-SALES-BUDGET * SPLIT-MM.
           MOVE SBREP-SALES-BUDGET       TO D-SALES-BU
           COMPUTE WS-MARGIN-BU =
              (WS-SALESAMT-YTD (SUB-1) - SBREP-SALES-BUDGET)
           COMPUTE WS-PERC-BU ROUNDED = 
               WS-MARGIN-BU / SBREP-SALES-BUDGET * 100.
               MOVE WS-PERC-BU          TO D-SALES-BU-PERC.
           
           COMPUTE SBREP-PROFIT-BUDGET =
                SBREP-PROFIT-BUDGET * SPLIT-MM.
           MOVE SBREP-PROFIT-BUDGET     TO D-PROFIT-BU
           COMPUTE WS-MARGIN-BU =
              WS-MARGIN - SBREP-PROFIT-BUDGET
           COMPUTE WS-PERC-BU ROUNDED = 
               WS-MARGIN-BU / SBREP-PROFIT-BUDGET * 100.
               MOVE WS-PERC-BU          TO D-PROFIT-BU-PERC.

           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.

           MOVE WS-SALESAMT-LAST (SUB-1) TO D-SALESAMT
           COMPUTE WS-MARGIN =
                WS-SALESAMT-LAST (SUB-1) - WS-COST-LAST (SUB-1)
           MOVE WS-MARGIN         TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
                WS-MARGIN / WS-COST-LAST (SUB-1) * 100
           MOVE WS-PERC           TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0 TO WS-MARGIN
                     WS-PERC.
            
           ADD WS-SALESAMT-PTD (SUB-1)  TO TOT-SALESAMT-PTD
           ADD WS-SALESAMT-YTD (SUB-1)  TO TOT-SALESAMT-YTD
           ADD WS-SALESAMT-LAST (SUB-1) TO TOT-SALESAMT-LAST
           ADD WS-COST-PTD (SUB-1)   TO TOT-COST-PTD
           ADD WS-COST-YTD (SUB-1)   TO TOT-COST-YTD
           ADD WS-COST-LAST (SUB-1)  TO TOT-COST-LAST.

           WRITE PRINT-REC AFTER 1.
           
           ADD 4 TO LINE-CNT
           MOVE 0 TO WS-MARGIN
                     WS-PERC.
              
           IF SUB-1 < 22
               ADD 1 TO SUB-1.
           IF WS-REP-NUM (SUB-1) NOT = " "
               GO TO PRR-010.
       PRR-999.
           EXIT.
      *
       READ-SBREP SECTION.
       RSB-020.
           MOVE WS-REP-NUM (SUB-1) TO SBREP-REP.
           START SBREP-MASTER KEY NOT < SBREP-KEY
             INVALID KEY NEXT SENTENCE.
       RSB-030.
           READ SBREP-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-SBREP-ST1 = 23 OR 35 OR 49
              MOVE "**UNKNOWN**" TO D-SALESMAN
              GO TO RSB-999.
           MOVE SBREP-REPNAME TO D-SALESMAN.
       RSB-999.
            EXIT.
      *
       PRINT-SALES-ROUTINE SECTION.
       PRS-000.
           MOVE 0 TO SA-KEY.
           START SALES-ANALYSIS KEY NOT < SA-KEY
              INVALID KEY NEXT SENTENCE.
       PRS-005.
           READ SALES-ANALYSIS NEXT
               AT END NEXT SENTENCE.
           IF WS-SALES-ST1 = 10
              GO TO PRS-999.
           IF WS-SALES-ST1 NOT = 0
             MOVE "SALES BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-SALES-ST1
              GO TO PRS-005.
       PRS-020.
      * 53= BRANCH OFFICE, 57= ASSOCIATE COMPANIES
           IF SA-ANALYSIS-CODE = 53 OR = 57
               ADD SA-SALES-WEEK        TO WS-BRANCH-SALES-WEEK
               ADD SA-SALES-PTD         TO WS-BRANCH-SALES-PTD
               ADD SA-SALES-YTD         TO WS-BRANCH-SALES-YTD
               ADD SA-COST-WEEK         TO WS-BRANCH-COST-WEEK
               ADD SA-COST-PTD          TO WS-BRANCH-COST-PTD
               ADD SA-COST-YTD          TO WS-BRANCH-COST-YTD.

           ADD SA-COST-WEEK           TO TOT-COSTWEEK
           ADD SA-COST-PTD            TO TOT-COSTPTD
           ADD SA-COST-YTD            TO TOT-COSTYTD
           ADD SA-SALES-WEEK          TO TOT-SALESWEEK
           ADD SA-SALES-PTD           TO TOT-SALESPTD
           ADD SA-SALES-YTD           TO TOT-SALESYTD.

           GO TO PRS-005.
       PRS-999.
           EXIT.
      *
       CLEAR-SCREEN-PART SECTION.
       CSP-005.
           MOVE 0301 TO POS
           DISPLAY WS-MESSAGE-PART AT POS
           MOVE 0 TO LINE-CNT.
       CSP-010.
           ADD 1 TO LINE-CNT
           ADD 80 TO POS
           DISPLAY WS-MESSAGE-PART AT POS.
           IF LINE-CNT < 29
              GO TO CSP-010.
           MOVE 0280 TO POS
           DISPLAY WS-MESS AT POS.
       CSP-999.
           EXIT.
      *
       END-SALES-OFF SECTION.
       END-SALES-000.
            MOVE " WEEK / MONTH / THIS YEAR TO DATE" TO H3-NAME.
            MOVE SPACES                              TO H4-NAME.
            
            IF LINE-CNT < 46
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD3 AFTER 2
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD4 AFTER 1
               MOVE " " TO PRINT-REC
               ADD 3 TO LINE-CNT
               GO TO END-SALES-010
             ELSE
               PERFORM PRR-015.
        END-SALES-010.
           IF LINE-CNT > 56
              PERFORM PRR-015.
        END-SALES-020.
           MOVE "TOTAL COMPANY"         TO D-CATEGORY
           MOVE TOT-SALESWEEK           TO D-SALESAMT
           COMPUTE TOT-MARGIN = TOT-SALESWEEK - TOT-COSTWEEK
           MOVE TOT-MARGIN              TO D-MARGIN
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTWEEK) * 100
           MOVE TOT-PERC       TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0 TO TOT-PERC
                     TOT-MARGIN

           MOVE TOT-SALESPTD    TO D-SALESAMT
           COMPUTE TOT-MARGIN = TOT-SALESPTD - TOT-COSTPTD
           MOVE TOT-MARGIN      TO D-MARGIN.
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTPTD) * 100
           MOVE TOT-PERC        TO D-PERC.
           
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0 TO TOT-PERC
                     TOT-MARGIN.

           MOVE TOT-SALESYTD   TO D-SALESAMT
           COMPUTE TOT-MARGIN = TOT-SALESYTD - TOT-COSTYTD
           MOVE TOT-MARGIN     TO D-MARGIN
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTYTD) * 100
           MOVE TOT-PERC       TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC
           ADD 4 TO LINE-CNT
           MOVE 0 TO TOT-PERC
                     TOT-MARGIN.
        END-SALES-100.
           IF LINE-CNT > 56
              PERFORM PRR-015.
        END-SALES-120.
           MOVE "BRANCHES"           TO D-CATEGORY
           MOVE WS-BRANCH-SALES-WEEK TO D-SALESAMT
           COMPUTE WS-MARGIN =
               WS-BRANCH-SALES-WEEK - WS-BRANCH-COST-WEEK
           MOVE WS-MARGIN            TO D-MARGIN
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / WS-BRANCH-COST-WEEK) * 100
           MOVE WS-PERC       TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0 TO WS-PERC
                     WS-MARGIN

           MOVE WS-BRANCH-SALES-PTD TO D-SALESAMT
           COMPUTE WS-MARGIN =
               WS-BRANCH-SALES-PTD - WS-BRANCH-COST-PTD
           MOVE WS-MARGIN            TO D-MARGIN
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / WS-BRANCH-COST-PTD) * 100
           MOVE WS-PERC       TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0 TO WS-PERC
                     WS-MARGIN.

           MOVE WS-BRANCH-SALES-YTD TO D-SALESAMT
           COMPUTE WS-MARGIN =
               WS-BRANCH-SALES-YTD - WS-BRANCH-COST-YTD
           MOVE WS-MARGIN            TO D-MARGIN
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / WS-BRANCH-COST-YTD) * 100
           MOVE WS-PERC       TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0 TO WS-PERC
                     WS-MARGIN.
                     
           ADD 4 TO LINE-CNT.
        END-SALES-700.
           IF LINE-CNT > 56
              PERFORM PRR-015.
        END-SALES-720.
           MOVE "NETT COMPANY" TO D-CATEGORY
           
           SUBTRACT WS-BRANCH-SALES-WEEK  FROM TOT-SALESWEEK
           SUBTRACT WS-BRANCH-SALES-PTD   FROM TOT-SALESPTD
           SUBTRACT WS-BRANCH-SALES-YTD   FROM TOT-SALESYTD
           SUBTRACT WS-BRANCH-COST-WEEK   FROM TOT-COSTWEEK
           SUBTRACT WS-BRANCH-COST-PTD    FROM TOT-COSTPTD
           SUBTRACT WS-BRANCH-COST-YTD    FROM TOT-COSTYTD.
           
           MOVE TOT-SALESWEEK   TO D-SALESAMT
           COMPUTE TOT-MARGIN = TOT-SALESWEEK - TOT-COSTWEEK
           MOVE TOT-MARGIN     TO D-MARGIN
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTWEEK) * 100
           MOVE TOT-PERC       TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 2.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0 TO TOT-PERC
           
           MOVE WS-CO-SALES-BU  TO SBREP-SALES-BUDGET
           MOVE WS-CO-PROFIT-BU TO SBREP-PROFIT-BUDGET.

           MOVE TOT-SALESPTD   TO D-SALESAMT
           COMPUTE TOT-MARGIN = TOT-SALESPTD - TOT-COSTPTD
           MOVE TOT-MARGIN     TO D-MARGIN
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTPTD) * 100
           MOVE TOT-PERC       TO D-PERC

           COMPUTE SBREP-SALES-BUDGET = 
                SBREP-SALES-BUDGET / 21 * WS-DAYS.
           MOVE SBREP-SALES-BUDGET       TO D-SALES-BU
           COMPUTE WS-MARGIN-BU =
              (TOT-SALESPTD - SBREP-SALES-BUDGET)
           COMPUTE WS-PERC-BU ROUNDED = 
               WS-MARGIN-BU / SBREP-SALES-BUDGET * 100.
               MOVE WS-PERC-BU          TO D-SALES-BU-PERC.
           
           COMPUTE SBREP-PROFIT-BUDGET =
                SBREP-PROFIT-BUDGET / 21 * WS-DAYS.
           MOVE SBREP-PROFIT-BUDGET     TO D-PROFIT-BU
           COMPUTE WS-MARGIN-BU =
              (TOT-MARGIN - SBREP-PROFIT-BUDGET)
           COMPUTE WS-PERC-BU ROUNDED = 
               WS-MARGIN-BU / SBREP-PROFIT-BUDGET * 100.
               MOVE WS-PERC-BU          TO D-PROFIT-BU-PERC.

           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0 TO TOT-PERC

           MOVE WS-CO-SALES-BU  TO SBREP-SALES-BUDGET
           MOVE WS-CO-PROFIT-BU TO SBREP-PROFIT-BUDGET.

           MOVE TOT-SALESYTD   TO D-SALESAMT
           COMPUTE TOT-MARGIN = TOT-SALESYTD - TOT-COSTYTD
           MOVE TOT-MARGIN     TO D-MARGIN
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COSTYTD) * 100
           MOVE TOT-PERC       TO D-PERC

           COMPUTE SBREP-SALES-BUDGET = 
                SBREP-SALES-BUDGET * SPLIT-MM
           MOVE SBREP-SALES-BUDGET       TO D-SALES-BU
           COMPUTE WS-MARGIN-BU =
              (TOT-SALESYTD - SBREP-SALES-BUDGET)
           COMPUTE WS-PERC-BU ROUNDED = 
               WS-MARGIN-BU / SBREP-SALES-BUDGET * 100.
               MOVE WS-PERC-BU          TO D-SALES-BU-PERC.
           
           COMPUTE SBREP-PROFIT-BUDGET =
                SBREP-PROFIT-BUDGET * SPLIT-MM.
           MOVE SBREP-PROFIT-BUDGET     TO D-PROFIT-BU
           COMPUTE WS-MARGIN-BU =
              (TOT-MARGIN - SBREP-PROFIT-BUDGET)
           COMPUTE WS-PERC-BU ROUNDED = 
               WS-MARGIN-BU / SBREP-PROFIT-BUDGET * 100.
               MOVE WS-PERC-BU          TO D-PROFIT-BU-PERC.

           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
       END-SALES-999.
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
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-008.
           OPEN I-O SBREP-MASTER.
           IF WS-SBREP-ST1 NOT = 0 
              MOVE 0 TO WS-SBREP-ST1
              MOVE "SBREP FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-008.
       OPEN-009.
           OPEN I-O SALES-ANALYSIS.
           IF WS-SALES-ST1 NOT = 0
               MOVE 0 TO WS-SALES-ST1
               MOVE "SALES ANALYSIS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-009.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
           
           IF SPLIT-MM = 1 OR = 2
              ADD 12 TO SPLIT-MM.
           SUBTRACT 2 FROM SPLIT-MM.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
       END-010.
           IF LINE-CNT > 56
              PERFORM PRR-015.
       END-020.
           MOVE "S/MAN TOTALS**"     TO D-CATEGORY.
           MOVE TOT-SALESAMT-PTD     TO D-SALESAMT.
           COMPUTE WS-MARGIN = TOT-SALESAMT-PTD - TOT-COST-PTD.
           MOVE WS-MARGIN            TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / TOT-COST-PTD * 100.
           MOVE WS-PERC              TO D-PERC.
           
           COMPUTE TOT-SALES-BUDGET = 
                (TOT-SALES-BUDGET / 21) * WS-DAYS.
           MOVE TOT-SALES-BUDGET       TO D-SALES-BU
           COMPUTE WS-MARGIN-BU =
              (TOT-SALESAMT-PTD - TOT-SALES-BUDGET).
           COMPUTE WS-PERC-BU ROUNDED = 
               (WS-MARGIN-BU / TOT-SALES-BUDGET) * 100.
               MOVE WS-PERC-BU         TO D-SALES-BU-PERC.
           
           COMPUTE TOT-PROFIT-BUDGET =
                (TOT-PROFIT-BUDGET / 21) * WS-DAYS.
           MOVE TOT-PROFIT-BUDGET     TO D-PROFIT-BU
           COMPUTE WS-MARGIN-BU =
               WS-MARGIN - TOT-PROFIT-BUDGET
           COMPUTE WS-PERC-BU ROUNDED = 
               (WS-MARGIN-BU / TOT-PROFIT-BUDGET) * 100.
               MOVE WS-PERC-BU        TO D-PROFIT-BU-PERC.

           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.
           MOVE TOT-SALESAMT-YTD TO D-SALESAMT.
           COMPUTE WS-MARGIN = TOT-SALESAMT-YTD - TOT-COST-YTD.
           MOVE WS-MARGIN        TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / TOT-COST-YTD * 100.
           MOVE WS-PERC          TO D-PERC.
           
           COMPUTE TOT-SALES-BUDGET = 
                (TOT-SALES-BUDGET * 21) / WS-DAYS * SPLIT-MM.
           MOVE TOT-SALES-BUDGET       TO D-SALES-BU
           COMPUTE WS-MARGIN-BU =
              (TOT-SALESAMT-YTD - TOT-SALES-BUDGET).
           COMPUTE WS-PERC-BU ROUNDED = 
               (WS-MARGIN-BU / TOT-SALES-BUDGET) * 100.
               MOVE WS-PERC-BU         TO D-SALES-BU-PERC.
           
           COMPUTE TOT-PROFIT-BUDGET =
                (TOT-PROFIT-BUDGET * 21) / WS-DAYS * SPLIT-MM.
           MOVE TOT-PROFIT-BUDGET     TO D-PROFIT-BU
           COMPUTE WS-MARGIN-BU =
               WS-MARGIN - TOT-PROFIT-BUDGET
           COMPUTE WS-PERC-BU ROUNDED = 
               (WS-MARGIN-BU / TOT-PROFIT-BUDGET) * 100.
               MOVE WS-PERC-BU        TO D-PROFIT-BU-PERC.

           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.

           MOVE TOT-SALESAMT-LAST TO D-SALESAMT.
           COMPUTE WS-MARGIN = TOT-SALESAMT-LAST - TOT-COST-LAST.
           MOVE WS-MARGIN         TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / TOT-COST-LAST * 100.
           MOVE WS-PERC           TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           
           ADD 4 TO LINE-CNT.

      **************************
      *  READING SALES-MASTER  *
      **************************
           PERFORM PRINT-SALES-ROUTINE.
      **************************************************************
      * SBREP-REP = $.  $ IS A REP NAME FOR COMPANY BUDGET FIGURES *
      **************************************************************
           MOVE "$" TO SBREP-REP
           PERFORM RSB-030.
           MOVE SBREP-SALES-BUDGET  TO WS-CO-SALES-BU
           MOVE SBREP-PROFIT-BUDGET TO WS-CO-PROFIT-BU.
           
           PERFORM END-SALES-OFF.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
               
           CLOSE PRINT-FILE.
           
      *     MOVE WS-PRINTERNUMBER (21) TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
           IF WS-PRINTERNUMBER (21) NOT = 20 AND NOT = 0
                PERFORM SEND-REPORT-TO-PRINTER
                GO TO END-900.
           IF WS-PRINTERNUMBER (21) = 0
                GO TO END-900.
           
            MOVE "When Finished Viewing The Report, Press Q to Quit."
              TO WS-MESSAGE
            PERFORM ERROR-MESSAGE. 
              
            MOVE 
            CONCATENATE('less ', ' ', TRIM(WS-PRINTER))
                TO WS-COMMAND-LINE.
      
            CALL "SYSTEM" USING WS-COMMAND-LINE.
      *      RETURNING W-STATUS.
       END-900.
           CLOSE DEBTOR-MASTER
                 SBREP-MASTER
                 SALES-ANALYSIS.
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
