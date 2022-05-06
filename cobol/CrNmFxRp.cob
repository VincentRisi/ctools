        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrNmFxRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
    
        SPECIAL-NAMES.
        CLASS WS-VALID-EMAIL IS
          '@' '_' '.' '-'
          'a' THRU 'i'
          'j' THRU 'r'
          's' THRU 'z'
          '0' THRU '9'.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT HIGH-FILE ASSIGN TO WS-HIGH-FILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-RANDOM-STATUS
               RECORD KEY IS HIGH-KEY.
           SELECT RANDOM-FILE ASSIGN TO WS-RANDOM-FILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-RANDOM-STATUS
               RECORD KEY IS RANDOM-KEY.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCreditor.

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
              "/ctools/spl/CreditHighSales".
       77  WS-High-FILE-ind     PIC X(35) VALUE
              "/ctools/spl/CreditHighSales.Ind".
       77  WS-XFAX-MAN3000      PIC X VALUE " ".
       77  WS-ONLY-VALID        PIC X VALUE " ".
       77  WS-ONLY-BAL          PIC X VALUE " ".
       77  WS-FOR-LOC           PIC X VALUE " ".
       77  WS-ACC-SALES         PIC X VALUE " ".
       77  WS-RANGE1            PIC X(7) VALUE " ".
       77  WS-RANGE2            PIC X(7) VALUE " ".
       77  WS-RANDOM-WRITTEN    PIC X.
       77  WS-ACCEPT            PIC X VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-TOP               PIC X(5) VALUE " ".
       77  WS-SORT              PIC X VALUE " ".
       77  WS-PERIOD            PIC X VALUE " ".
       77  WS-SALESMAN          PIC X VALUE " ".
       77  WS-MARGIN-SALES      PIC X VALUE " ".
       77  WS-TOP-DIS           PIC Z(4)9.
       77  WS-SALESMAN-ACC      PIC 9(5) VALUE 0.
       77  WS-NO-ACC            PIC 9(5) VALUE 0.
       77  WS-NO-PRINTED        PIC 9(5) VALUE 0.
       77  WS-DISPLAY-PRINTED   PIC Z(4)9.
       77  WS-MARGIN            PIC S9(9)V99 VALUE 0.
       77  WS-TOTAL-SMAN-YTD    PIC S9(9)V99 VALUE 0.
       77  WS-TOTAL-SMAN-LAST   PIC S9(9)V99 VALUE 0.
       77  WS-TOTAL-SALES       PIC S9(9)V99 VALUE 0.
       77  WS-TOTAL-PERC        PIC S9(4)V9999 VALUE 0.
       77  WS-ACC-ERROR         PIC X VALUE " ".      
       01  WS-EMAIL             PIC X(50).
       01  WS-SPACE-CNT         PIC 9(2) VALUE ZEROES.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1  PIC 99.
       01  WS-RANDOM-STATUS.
           03  WS-RANDOM-ST1    PIC 99.
       01  DETAIL-LINE.
           03  FILLER           PIC X(80) VALUE " ".
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(15).
           03  FILLER         PIC X(45) VALUE
           "CREDITOR MASTER LIST WITH FAX & PHONE NUMBERS".
           03  FILLER         PIC X(9) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(22) VALUE " ".
           03  FILLER         PIC X(44) VALUE ALL "*".
           03  FILLER         PIC X(52) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(10) VALUE "ACCOUNT".
           03  FILLER         PIC X(41) VALUE "NAME".
           03  FILLER         PIC X(21) VALUE "PHONE NUMBER".
           03  FILLER         PIC X(25) VALUE "FAX NUMBER           ER".
           03  FILLER         PIC X(41) VALUE "ACCOUNT E-MAIL ADDRESS".
       01  DETAIL-LINE1.
           03  D-ACCOUNT      PIC 9(7).
           03  FILLER         PIC X(3) VALUE " ".
           03  D-NAME         PIC X(41).
           03  D-PHONE        PIC X(21).
           03  D-FAX          PIC X(22).
           03  D-ERROR        PIC X(3).
           03  D-EMAIL        PIC X(40).
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY 
           "** CREDITOR NAME & FAX IN XFAX & MAN3000 FORMAT REPORT **"
           AT POS
           MOVE 415 TO POS
           DISPLAY 
           "********************************************************"
            AT POS.
       CONTROL-003.
           Copy "PrinterAcceptCr".
       CONTROL-010.
            MOVE "N" TO WS-SORT
            GO TO CONTROL-011.
       
            MOVE 810 TO POS.
            DISPLAY "Should This Print Be Sorted by Highest sales: [ ]"
             AT POS.
            MOVE 857 TO POS.
            MOVE " " TO WS-SORT.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SORT.

            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
            IF WS-SORT NOT = "Y" AND NOT = "N"
               MOVE
                "Please Enter Y or N in this field, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CONTROL-010.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-011
            ELSE
                DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-011.
            IF WS-SORT = "Y"
               GO TO CONTROL-014.
            MOVE " " TO WS-RANGE1 WS-RANGE2.
            MOVE 1010 TO POS.
            DISPLAY "         FROM CREDITOR NUMBER: [       ]" AT POS.
            MOVE 1042 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-012
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-011.
       CONTROL-012.
            MOVE 1210 TO POS.
            DISPLAY "           TO CREDITOR NUMBER: [       ]" AT POS.
            MOVE 1242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.
           
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-011.
           IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
       CONTROL-014.
            MOVE 1810 TO POS.
            DISPLAY "How many Accounts should we print ? :   [     ]"
             AT POS.
            MOVE 1851 TO POS.
            MOVE "     " TO WS-TOP.

           MOVE ' '       TO CDA-DATA.
           MOVE 5         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TOP.

            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
            IF WS-TOP = "    "
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-014.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-015
            ELSE
               MOVE 3010 TO POS
                DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-014.
       CONTROL-015.
            MOVE WS-TOP TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-TOP-DIS WS-NO-ACC.
            MOVE 1851 TO POS.
            DISPLAY WS-TOP-DIS AT POS.
            IF NUMERIC-RATE NOT > 0
                MOVE 3010 TO POS
                DISPLAY "THE NUMBER CANNOT BE NEGATIVE OR 0, RE-ENTER"
                AT POS
                GO TO CONTROL-014.
       CONTROL-025.
            PERFORM ERROR-020.
            MOVE 2010 TO POS.
            DISPLAY "Base Selection on; T=This Year, L=Last Year [ ]"
                AT POS.
            MOVE 2129 TO POS
            DISPLAY "B=Balance Owed on Account." AT POS
            MOVE 2055 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PERIOD.

            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-014.
            IF WS-PERIOD NOT = "T" AND NOT = "L" AND NOT = "B"
                DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-025.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-027
            ELSE
                DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-025.
       CONTROL-027.
            MOVE 2210 TO POS.
            DISPLAY "Base Selection on; P=Profit, S=Sales        [ ]"
                AT POS.
            MOVE 2255 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 19        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-MARGIN-SALES.

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
            MOVE 2310 TO POS.
            DISPLAY "Enter a SALESMAN No, leave blank for ALL    [ ]"
                AT POS.
            MOVE 2410 TO POS.
            DISPLAY "or Enter 'Z' for A/c's with NO salesman."
                AT POS.
            MOVE 2355 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 20        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SALESMAN.

            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-025.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-035
            ELSE
               MOVE 3010 TO POS
                DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-030.
       CONTROL-035.
           MOVE 2910 TO POS
           DISPLAY "The Report Is Being Compiled......" AT POS
           PERFORM OPEN-FILES
           PERFORM READ-ALL-ACCOUNTS
           PERFORM READ-RANDOM-FILE.

           PERFORM PRINT-ROUTINE
           PERFORM DELETE-TRANS.
           GO TO CONTROL-050.
       CONTROL-040.
           MOVE 1610 TO POS.
           DISPLAY "M = MAN3000 FORMAT," AT POS.
           MOVE 1710 TO POS.
           DISPLAY "R = REPORT WITH PHONE, FAX & EMAIL," AT POS.
           MOVE 1810 TO POS.
           DISPLAY "X = XFAX FORMAT, Y=XFAX BUT NO EMAIL ADDR.[ ]"
            AT POS.
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-XFAX-MAN3000.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-012.
           IF WS-XFAX-MAN3000 NOT = "X" AND NOT = "M" AND NOT = "R"
                          AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-041
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
       CONTROL-041.
           IF WS-XFAX-MAN3000 NOT = "R"
               GO TO CONTROL-043.
               
           MOVE 2010 TO POS.
           DISPLAY "PRINT ACC'S ONLY WITH:" AT POS.
           MOVE 2110 TO POS.
           DISPLAY "P=PHONE, F=FAX, E=EMAIL, BLANK=ALL ACC'S. [ ]"
            AT POS.
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 18        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ONLY-VALID.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-040.
           IF WS-ONLY-VALID NOT = "E" AND NOT = "F" AND NOT = "P"
                AND NOT = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-041.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-043
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-041.
       CONTROL-043.
           MOVE 2310 TO POS.
           DISPLAY "PRINT ONLY ACC'S WITH BAL OR BAL-LAST  = Y/N,"
            AT POS.
           MOVE 2410 TO POS.
           DISPLAY "PRINT ONLY ACC'S PURCHASES YTD OR L/YR.=P [ ]"
            AT POS.
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 21        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ONLY-BAL.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-041.
           IF WS-ONLY-BAL NOT = "N" AND NOT = "Y" AND NOT = "P"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-043.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-044
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-043.
       CONTROL-044.
           MOVE 2610 TO POS.
           DISPLAY "PRINT ONLY: L=LOCAL, F=FOREIGN, BLANK=ALL [ ]"
            AT POS.
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 23        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FOR-LOC.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-043.
           IF WS-FOR-LOC NOT = "L" AND NOT = "F" AND NOT = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-044.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-045
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-044.
       CONTROL-045.
           PERFORM OPEN-000 THRU OPEN-010.
           IF WS-XFAX-MAN3000 = "X" OR = "Y"
               PERFORM PRINT-XFAX-ROUTINE.
           IF WS-XFAX-MAN3000 = "M"
               PERFORM PRINT-MAN3000-ROUTINE.
           IF WS-XFAX-MAN3000 = "R"
               PERFORM PRINT-EMAIL-ROUTINE.
       CONTROL-050.
           PERFORM END-OFF.
      *
       READ-ALL-ACCOUNTS SECTION.
       RAA-001.
           MOVE 2910 TO POS
           DISPLAY "Reading ALL Creditor GROSS totals." AT POS
           START CREDITOR-MASTER KEY NOT < CR-KEY.
       RAA-005.
           READ CREDITOR-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 10
               GO TO RAA-900.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE
           "ACC NUMBER LOCKED AT ANOTHER TERMINAL, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE CR-ACCOUNT-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RAA-005.
           IF WS-PERIOD = "B"
              ADD CR-BALANCE    TO WS-TOTAL-SALES.
           IF WS-PERIOD = "T"
              ADD CR-PURCHASE-YTD  TO WS-TOTAL-SALES.
           IF WS-PERIOD = "L"
              ADD CR-PURCHASE-LAST TO WS-TOTAL-SALES.
              
           MOVE 2510 TO POS
           DISPLAY "ACCOUNT BEING READ:" AT POS
           ADD 20 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
 
               
           IF WS-PERIOD = "B"
               MOVE CR-BALANCE    TO RANDOM-NUMBER.
               
           IF WS-PERIOD = "T"
            IF WS-MARGIN-SALES = "S"
               MOVE CR-PURCHASE-YTD  TO RANDOM-NUMBER.
               
           IF WS-PERIOD = "L"
            IF WS-MARGIN-SALES = "S"
               MOVE CR-PURCHASE-LAST TO RANDOM-NUMBER.
               
           IF RANDOM-NUMBER > 0
               MOVE 1 TO RANDOM-INDEX
               PERFORM WRITE-RANDOM-RECORD.
           GO TO RAA-005.
       RAA-900.
           CLOSE CREDITOR-MASTER.
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
              MOVE "BAD START ON RANDOM" TO WS-MESSAGE
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
              MOVE 3010 TO POS
              DISPLAY "RANDOM RECORD BUSY  :" AT POS
              ADD 25 TO POS
              DISPLAY RANDOM-NUMBER AT POS
              ADD 20 TO POS
              DISPLAY WS-RANDOM-ST1 AT POS
              ADD 5 TO POS
              PERFORM ERROR-010
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
           MOVE CR-ACCOUNT-NUMBER TO RANDOM-ACCOUNT.
           WRITE RANDOM-REC
              INVALID KEY NEXT SENTENCE.
              
           IF WS-RANDOM-ST1 NOT = 0
              ADD 1 TO RANDOM-INDEX
              GO TO WRR-005.
              
           GO TO WRR-999.
              
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "RANDOM RECORD INVALID ON WRITE" TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       WRR-999.
            EXIT.
      *
       WRITE-HIGH-RECORD SECTION.
       WRR-005.
           MOVE RANDOM-ACCOUNT TO HIGH-ACCOUNT.
           WRITE HIGH-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "HIGH RECORD INVALID ON WRITE" TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       WRR-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-001.
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
              MOVE "BAD START ON HIGH AT PRR-001." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              EXIT PROGRAM.
       PRR-006.
           READ HIGH-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-RANDOM-ST1 = 10
               GO TO PRR-999.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "CR-RECORD BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 3010 TO POS
              DISPLAY "CREDITOR FILE BUSY PRR-006 :" AT POS
              ADD 28 TO POS
              DISPLAY CR-ACCOUNT-NUMBER AT POS
              ADD 20 TO POS
              DISPLAY WS-RANDOM-ST1 AT POS
              ADD 5 TO POS
              PERFORM ERROR-010
              MOVE 0 TO WS-RANDOM-ST1
              GO TO PRR-006.

           MOVE 2510 TO POS
           DISPLAY "CREDITOR NUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY HIGH-ACCOUNT AT POS.
           
           MOVE HIGH-ACCOUNT TO CR-ACCOUNT-NUMBER
           PERFORM READ-CREDITOR.

           IF WS-PERIOD = "B"
            IF CR-BALANCE NOT > 0
              GO TO PRR-006.
           IF WS-PERIOD = "T"
            IF CR-PURCHASE-YTD < 0
              GO TO PRR-006.
           IF WS-PERIOD = "L"
            IF CR-PURCHASE-LAST < 0
              GO TO PRR-006.
       PRR-010.
           PERFORM CHECK-BLANK-ON-NAME.

           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC

           MOVE 2610 TO POS
           DISPLAY "TOTAL NUMBER OF ACCOUNTS READ:" AT POS
           MOVE WS-NO-PRINTED TO WS-DISPLAY-PRINTED
           ADD 31 TO POS
           DISPLAY WS-DISPLAY-PRINTED AT POS.

           ADD 1 TO WS-NO-PRINTED.

           IF WS-NO-PRINTED = WS-NO-ACC
               GO TO PRR-999.
           GO TO PRR-006.
       PRR-999.
           EXIT.
      *
       PRINT-XFAX-ROUTINE SECTION.
       PNFR-000.
            MOVE WS-RANGE1 TO CR-ACCOUNT-NUMBER.
            START CREDITOR-MASTER KEY NOT < CR-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE "BAD START ON CREDITORS, 'ESC' TO EXIT PROGRAM."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PNFR-999.
       PNFR-002.
            READ CREDITOR-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-CREDITOR-ST1 = 10
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO PNFR-999.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITORS BUSY ON XFAX READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO PNFR-002.
            IF CR-ACCOUNT-NUMBER < WS-RANGE1
               GO TO PNFR-002.
            IF CR-ACCOUNT-NUMBER > WS-RANGE2
               GO TO PNFR-999.
               
            IF CR-FAX = " " 
              GO TO PNFR-002.
              
           MOVE 2910 TO POS
           DISPLAY "Account Number Being Read:" AT POS
           ADD 27 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
           
           IF WS-XFAX-MAN3000 = "X"
              GO TO PNFR-005.
           IF WS-XFAX-MAN3000 = "Y"
            IF CR-ACC-EMAIL = " "
              GO TO PNFR-005.
            
           GO TO PNFR-002.
       PNFR-005.
           IF WS-ONLY-BAL = "N"
              GO TO PNFR-006.
              
            IF WS-ONLY-BAL = "Y"
             IF CR-BALANCE NOT = 0
              OR CR-BAL-LAST-STATE NOT = 0
              GO TO PNFR-006.
            IF WS-ONLY-BAL = "P"
             IF CR-PURCHASE-YTD NOT = 0
              OR CR-PURCHASE-LAST NOT = 0
              GO TO PNFR-006.
            
            GO TO PNFR-002.
       PNFR-006.
           IF WS-FOR-LOC = " "
              GO TO PNFR-010.
              
            IF WS-FOR-LOC = "F"
             IF CR-FOREIGN-LOCAL = "F"
              GO TO PNFR-010.
            IF WS-FOR-LOC = "L"
             IF CR-FOREIGN-LOCAL = "L"
              GO TO PNFR-010.
            
            GO TO PNFR-002.
       PNFR-010.
           PERFORM CHECK-BLANK-ON-NAME.

           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           GO TO PNFR-002.
       PNFR-999.
           EXIT.
      *
       PRINT-MAN3000-ROUTINE SECTION.
       PMAN-000.
            MOVE WS-RANGE1 TO CR-ACCOUNT-NUMBER.
            START CREDITOR-MASTER KEY NOT < CR-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE "BAD START ON CREDITOR, 'ESC' TO EXIT PROGRAM."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PMAN-999.
       PMAN-002.
            READ CREDITOR-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-CREDITOR-ST1 = 10
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO PMAN-999.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITORS BUSY ON PMAN READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO PMAN-002.
            IF CR-ACCOUNT-NUMBER < WS-RANGE1
               GO TO PMAN-002.
            IF CR-ACCOUNT-NUMBER > WS-RANGE2
               GO TO PMAN-999.
            IF CR-TELEPHONE = " "
               GO TO PMAN-002.
 
           MOVE 2910 TO POS
           DISPLAY "Account Number Being Read:" AT POS
           ADD 27 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
           
           IF WS-ONLY-VALID = " "
              GO TO PMAN-005.
           IF WS-ONLY-VALID = "E"
             IF CR-ACC-EMAIL > " "
              GO TO PMAN-005.
           IF WS-ONLY-VALID = "F"
             IF CR-FAX > " "
              GO TO PMAN-005.
           IF WS-ONLY-VALID = "P"
             IF CR-TELEPHONE > " "
              GO TO PMAN-005.

          GO TO PMAN-002.
       PMAN-005.
           IF WS-ONLY-BAL = "N"
              GO TO PMAN-006.
              
            IF WS-ONLY-BAL = "Y"
             IF CR-BALANCE NOT = 0
              OR CR-BAL-LAST-STATE NOT = 0
              GO TO PMAN-006.
            IF WS-ONLY-BAL = "P"
             IF CR-PURCHASE-YTD NOT = 0
              OR CR-PURCHASE-LAST NOT = 0
              GO TO PMAN-006.
            
            GO TO PMAN-002.
       PMAN-006.
           IF WS-FOR-LOC = " "
              GO TO PMAN-010.
              
            IF WS-FOR-LOC = "F"
             IF CR-FOREIGN-LOCAL = "F"
              GO TO PMAN-010.
            IF WS-FOR-LOC = "L"
             IF CR-FOREIGN-LOCAL = "L"
              GO TO PMAN-010.
            
            GO TO PMAN-002.
       PMAN-010.
           PERFORM REMOVE-DASH-FROM-PHONE.
           PERFORM ADD-IN-COMMAS.

           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           GO TO PMAN-002.
       PMAN-999.
           EXIT.
      *
       PRINT-EMAIL-ROUTINE SECTION.
       PPFE-000.
            MOVE WS-RANGE1 TO CR-ACCOUNT-NUMBER.
            START CREDITOR-MASTER KEY NOT < CR-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE "BAD START ON CREDITORS, 'ESC' TO EXIT PROGRAM."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PPFE-999.
       PPFE-002.
            READ CREDITOR-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-CREDITOR-ST1 = 10
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO PPFE-999.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITORS BUSY ON EMAIL READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO PPFE-002.
            IF CR-ACCOUNT-NUMBER < WS-RANGE1
               GO TO PPFE-002.
            IF CR-ACCOUNT-NUMBER > WS-RANGE2
               GO TO PPFE-999.
               
           MOVE 2910 TO POS
           DISPLAY "Account Number Being Read:" AT POS
           ADD 27 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
           
           IF WS-ONLY-VALID = " "
              GO TO PPFE-005.
           IF WS-ONLY-VALID = "E"
             IF CR-ACC-EMAIL > " "
              GO TO PPFE-005.
           IF WS-ONLY-VALID = "F"
             IF CR-FAX > " "
              GO TO PPFE-005.
           IF WS-ONLY-VALID = "P"
             IF CR-TELEPHONE > " "
              GO TO PPFE-005.

          GO TO PPFE-002.
       PPFE-005.
           IF WS-ONLY-BAL = "N"
              GO TO PPFE-006.
              
            IF WS-ONLY-BAL = "Y"
             IF CR-BALANCE NOT = 0
              OR CR-BAL-LAST-STATE NOT = 0
              GO TO PPFE-006.
            IF WS-ONLY-BAL = "P"
             IF CR-PURCHASE-YTD NOT = 0
              OR CR-PURCHASE-LAST NOT = 0
              GO TO PPFE-006.
            
            GO TO PPFE-002.
       PPFE-006.
           IF WS-FOR-LOC = " "
              GO TO PPFE-010.
              
            IF WS-FOR-LOC = "F"
             IF CR-FOREIGN-LOCAL = "F"
              GO TO PPFE-010.
            IF WS-FOR-LOC = "L"
             IF CR-FOREIGN-LOCAL = "L"
              GO TO PPFE-010.

            GO TO PPFE-002.
       PPFE-010.
            IF LINE-CNT < 56
               GO TO PPFE-020.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE
            MOVE " " TO PRINT-REC.
            IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
               WRITE PRINT-REC BEFORE PAGE
               WRITE PRINT-REC FROM COMPANY-LINE.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 2
            MOVE " " TO PRINT-REC
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC AFTER 1
            MOVE 5 TO LINE-CNT.
       PPFE-020.
           MOVE 0 TO WS-SPACE-CNT.
           MOVE "  " TO D-ERROR.
           MOVE CR-ACC-EMAIL TO F-NAMEFIELD.
           MOVE FUNCTION LOWER-CASE(F-NAMEFIELD) TO CR-ACC-EMAIL
                                                    WS-EMAIL.
           INSPECT WS-EMAIL TALLYING WS-SPACE-CNT FOR CHARACTERS
               BEFORE INITIAL SPACE.
            
           IF WS-EMAIL(1:(WS-SPACE-CNT)) IS NOT WS-VALID-EMAIL
                MOVE "*1" TO D-ERROR
                GO TO PPFE-025. 
            PERFORM CHECK-EMAIL-FOR-VALIDITY.
            IF WS-ACC-ERROR = "Y"
                MOVE "*2" TO D-ERROR
                GO TO PPFE-025. 
            IF WS-SPACE-CNT < 10
             IF WS-ACC-ERROR = "Y"
                MOVE "*3" TO D-ERROR.
       PPFE-025.
           MOVE CR-ACCOUNT-NUMBER   TO D-ACCOUNT
           MOVE CR-NAME             TO D-NAME
           MOVE CR-TELEPHONE        TO D-PHONE
           MOVE CR-FAX              TO D-FAX
           MOVE CR-ACC-EMAIL        TO D-EMAIL
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE1 AFTER 1
           ADD 1 TO LINE-CNT
           GO TO PPFE-002.
       PPFE-999.
           EXIT.
      *
       CHECK-EMAIL-FOR-VALIDITY SECTION.
       CEFV-005.
             MOVE 0 TO SUB-1.
             MOVE SPACES      TO ALPHA-RATE
             MOVE F-NAMEFIELD TO ALPHA-RATE.
             MOVE "N"         TO WS-ACC-ERROR.
       CEFV-010.
             ADD 1 TO SUB-1.
             IF SUB-1 > 42
                MOVE "Y" TO WS-ACC-ERROR
                GO TO CEFV-900.
             IF AL-RATE (SUB-1) = "@"
                MOVE 0 TO SUB-1
                GO TO CEFV-020.
             GO TO CEFV-010.
       CEFV-020.
             ADD 1 TO SUB-1.
             IF SUB-1 > 42
                MOVE "Y" TO WS-ACC-ERROR
                GO TO CEFV-900.
             IF AL-RATE (SUB-1) = "."
                GO TO CEFV-025.
             GO TO CEFV-020.
       CEFV-025.
      *ADDED THIS NEXT LINE SO THAT WE DON'T CHECK FOR AN EXTRA . OR COM
             GO TO CEFV-999.
       
             ADD 1 TO SUB-1.
             IF AL-RATE (SUB-1) = "c"
                GO TO CEFV-026
             ELSE
                SUBTRACT 1 FROM SUB-1
                GO TO CEFV-030.
             MOVE "Y" TO WS-ACC-ERROR.
       CEFV-026.
             ADD 1 TO SUB-1.
             IF AL-RATE (SUB-1) = "o"
                GO TO CEFV-027.
             SUBTRACT 2 FROM SUB-1
             GO TO CEFV-030.
       CEFV-027.
             ADD 1 TO SUB-1.
             IF AL-RATE (SUB-1) = "m"
                GO TO CEFV-040.
             SUBTRACT 3 FROM SUB-1.
       CEFV-030.
             ADD 1 TO SUB-1.
             IF SUB-1 > 42
                MOVE "Y" TO WS-ACC-ERROR
                GO TO CEFV-900.
             IF AL-RATE (SUB-1) = "."
                GO TO CEFV-040.
             GO TO CEFV-030.
        CEFV-040.
             MOVE "N" TO WS-ACC-ERROR
             GO TO CEFV-999.
       CEFV-900.
           MOVE "*4" TO D-ERROR.
       CEFV-999.
           EXIT.
      *
       REMOVE-DASH-FROM-PHONE SECTION.
       RDFP-001.
           MOVE " " TO ALPHA-RATE FAX-RATE.
           MOVE CR-TELEPHONE TO ALPHA-RATE.
           MOVE 1 TO SUB-1 SUB-2.
       RDFP-005.
           IF AL-RATE (SUB-1) = "/"
              MOVE " " TO AL-RATE (SUB-1)
              GO TO RDFP-006.
           IF SUB-1 < 20
              ADD 1 TO SUB-1
              GO TO RDFP-005.
           MOVE 1 TO SUB-1 SUB-2.
           GO TO RDFP-010.
       RDFP-006.
             IF SUB-1 < 20
              ADD 1 TO SUB-1
              MOVE " " TO AL-RATE (SUB-1)
              GO TO RDFP-006.
           MOVE 1 TO SUB-1 SUB-2.
       RDFP-010. 
           IF AL-RATE (SUB-1) NOT = " "
            IF AL-RATE (SUB-1) NOT = "-"
              MOVE AL-RATE (SUB-1) TO FX-RATE (SUB-2)
             IF SUB-1 < 20
              ADD 1 TO SUB-1 SUB-2
              GO TO RDFP-010.
           IF AL-RATE (SUB-1) = "-"
              ADD 1 TO SUB-1
              MOVE AL-RATE (SUB-1) TO FX-RATE (SUB-2)
              ADD 1 TO SUB-1 SUB-2
              GO TO RDFP-010.
       RDFP-030.
           MOVE FAX-RATE TO CR-TELEPHONE.
       RDFP-999.
           EXIT.
      *
       ADD-IN-COMMAS SECTION.
       AIC-005.
           MOVE " " TO ALPHA-RATE FAX-RATE.
           MOVE CR-TELEPHONE TO ALPHA-RATE.
           MOVE 20 TO SUB-1.
       AIC-010.
           IF AL-RATE (SUB-1) = " "
            IF SUB-1 > 1
              SUBTRACT 1 FROM SUB-1
              GO TO AIC-010.
           ADD 1 TO SUB-1.
           MOVE "," TO AL-RATE (SUB-1).
           MOVE 1 TO SUB-2.
           MOVE CR-NAME TO FAX-RATE.
           ADD 1 TO SUB-1.
       AIC-020.
           MOVE FX-RATE (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 21
              ADD 1 TO SUB-1 SUB-2
              GO TO AIC-020.
           MOVE "," TO AL-RATE (SUB-1).
       AIC-030.
           ADD 1 TO SUB-1
           MOVE "3" TO AL-RATE (SUB-1).

           MOVE ALPHA-RATE TO DETAIL-LINE.
       AIC-999.
           EXIT.
      *
       CHECK-BLANK-ON-NAME SECTION.
       CBON-005.
           MOVE " " TO ALPHA-RATE FAX-RATE.
           MOVE CR-NAME TO ALPHA-RATE.
           MOVE 40 TO SUB-1.
       CBON-010.
           IF AL-RATE (SUB-1) = " "
            IF SUB-1 > 1
              SUBTRACT 1 FROM SUB-1
              GO TO CBON-010.
           ADD 1 TO SUB-1.
           MOVE "(" TO AL-RATE (SUB-1).
           MOVE 1 TO SUB-2.
           MOVE CR-FAX TO FAX-RATE.
           ADD 1 TO SUB-1.
       CBON-020.
           MOVE FX-RATE (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 21
              ADD 1 TO SUB-1 SUB-2
            IF FX-RATE (SUB-2) NOT = " "
              GO TO CBON-020.
           MOVE ")" TO AL-RATE (SUB-1).
       CBON-030.
           ADD 1 TO SUB-1
           MOVE "/" TO AL-RATE (SUB-1).
           ADD 1 TO SUB-1
           MOVE "X" TO AL-RATE (SUB-1).
           ADD 1 TO SUB-1
           MOVE "F" TO AL-RATE (SUB-1).
           ADD 1 TO SUB-1
           MOVE "A" TO AL-RATE (SUB-1).
           ADD 1 TO SUB-1
           MOVE "X" TO AL-RATE (SUB-1).

           MOVE ALPHA-RATE TO DETAIL-LINE.
       CBON-999.
           EXIT.
      *
       READ-CREDITOR SECTION.
       RD-005.
           READ CREDITOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 NOT = 0
              MOVE 3010 TO POS
              DISPLAY "CREDITOR RECORD BUSY ON READ, RD-005:" AT POS
              MOVE 3038 TO POS
              DISPLAY CR-ACCOUNT-NUMBER AT POS
              PERFORM ERROR-010.
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
       DST-999.
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
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE Ws-Co-Name To Co-Name.
       OPEN-011.
           GO TO OPEN-036.
       OPEN-035.
           OPEN I-O RANDOM-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE
             "RANDOM FILE OPEN AT ANOTHER COMPUTER, 'ESC' TO EXIT."
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
           "RANDOM FILE OPEN OUTPUT AT ANOTHER COMPUTER, 'ESC' TO EXIT."
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
             "HIGH FILE OPEN OUTPUT AT ANOTHER COMPUTER, 'ESC' TO EXIT."
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
           IF WS-XFAX-MAN3000 NOT = "R"
              GO TO END-800.
           MOVE " " TO PRINT-REC.
           
            IF LINE-CNT > 60
               PERFORM PPFE-010.

           MOVE 
           "** ERROR *1-4 NEXT TO EMAIL ADDRESS SHOWS ERROR IN EMAIL **"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 2.
           MOVE 
           "** ERROR *1=ILLEGAL CHAR, *2=MISSING @ OR . IN ADDRESS," & 
           " *3=LESS THAN 10 CHAR, *4=OTHER ERROR **"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 1.

           IF WS-ACC-SALES = "S"
            IF WS-ONLY-VALID = " "
                MOVE "** ALL ACCOUNTS PRINTED WITH SALES INFO **"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 2.

           IF WS-ONLY-VALID = " "
                MOVE "** ALL ACCOUNTS PRINTED WITH ACCOUNTS INFO **"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 2.
           IF WS-ONLY-VALID = "E"
                MOVE "** ONLY ACCOUNTS PRINTED WITH ACCOUNTS EMAIL **"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 2.
           IF WS-ONLY-VALID = "F"
                MOVE "** ONLY ACCOUNTS PRINTED WITH ACCOUNTS FAX #'S **"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 2.
           IF WS-ONLY-VALID = "P"
              MOVE "** ONLY ACCOUNTS PRINTED WITH ACCOUNTS PHONE #'S **"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 2.

           IF WS-ONLY-BAL = "Y"
                MOVE
            "** ONLY ACCOUNTS WITH A BALANCE OR BAL-LAST PRINTED **"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 1.
           IF WS-ONLY-BAL = "P"
                MOVE
            "** ONLY ACCOUNTS WITH PURCHASES YTD OR L/YR PRINTED **"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 1.
                
           IF WS-FOR-LOC = "F"
                MOVE "** ONLY FOREIGN ACCOUNTS PRINTED **"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 1.
           IF WS-FOR-LOC = "L"
                MOVE "** ONLY LOCAL ACCOUNTS PRINTED **"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 1.
           IF WS-FOR-LOC = " "
                MOVE "** BOTH FOREIGN & LOCAL ACCOUNTS PRINTED **"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 1.


           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-800.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-850.
           CLOSE CREDITOR-MASTER.
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
