        IDENTIFICATION DIVISION.
        PROGRAM-ID. StPrCoRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
         Copy "SelectSlParameter".
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
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdParam.
      *    
       FD  RANDOM-FILE.
       01  RANDOM-REC.
           03  RANDOM-KEY.
              05  RANDOM-NUMBER     PIC 9(7).
              05  RANDOM-INDEX      PIC 9(5).
           03  RANDOM-STOCK         PIC X(15).
           
       FD  HIGH-FILE.
       01  HIGH-REC.
           03  HIGH-KEY.
              05  HIGH-NUMBER     PIC 9(7).
           03  HIGH-STOCK         PIC X(15).
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(250).
      *
       WORKING-STORAGE SECTION.
       77  WS-RANDOM-FILE       PIC X(35) VALUE
              "/ctools/spl/RandomPriceCost".
       77  WS-RANDOM-FILE-ind   PIC X(35) VALUE
              "/ctools/spl/RandomPriceCost.Ind".
       77  WS-HIGH-FILE       PIC X(35) VALUE
              "/ctools/spl/StockPriceCost".
       77  WS-HIGH-FILE-ind   PIC X(35) VALUE
              "/ctools/spl/StockPriceCost.Ind".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANDOM-WRITTEN    PIC X.
       77  WS-TOP               PIC X(8) VALUE " ".
       77  WS-DISP-ANS          PIC Z(7)9.
       77  WS-TYPE              PIC X VALUE " ".
       77  WS-PERIOD            PIC X VALUE " ".
       77  WS-NUMBER-TO-PRINT   PIC 9(7).
       77  WS-MONTH             PIC 99.
       77  WS-MONTH-ACCEPT      PIC XX.
       77  WS-RAND-UNIT         PIC X VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-QUES-MU-GP-PERC   PIC X VALUE " ".
       77  WS-M-UP              PIC X(6) VALUE " ".
       77  WS-OVER-BELOW        PIC X VALUE " ".
       77  WS-DATE-ACCEPT       PIC X(10) VALUE " ".
       77  WS-DATE-PRICE        PIC X(10) VALUE " ".
       77  WS-MIN-PERC          PIC S9(3)V99 VALUE 0.
       77  WS-PERC-DIS          PIC Z(2)9.99.
       77  WS-PRINT-DISC        PIC X(1) VALUE " ".
       77  WS-PRINT-QTY         PIC X(1) VALUE " ".
       77  WS-CAT               PIC X(3) VALUE " ".
       77  WS-MARGIN            PIC S9(6)V99 VALUE 0.
       77  WS-PERC              PIC S9(6)V99 VALUE 0.
       77  WS-PERC-D9           PIC S9(6)V99 VALUE 0.
       77  WS-PRICETOT          PIC 9(7)9V99 VALUE 0.
       77  WS-COSTTOT           PIC 9(7)9V99 VALUE 0.
       77  WS-AVE               PIC 9(7)9V99 VALUE 0.
       77  WS-LAST              PIC 9(7)9V99 VALUE 0.
       77  WS-COST              PIC 9(7)9V99 VALUE 0.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-RANDOM-STATUS.
           03  WS-RANDOM-ST1      PIC 99.
       01  WS-DATE-ENTER.
           03  WS-YYE             PIC 9999.
           03  WS-MME             PIC 99.
           03  WS-DDE             PIC 99.
       01  WS-DATE-ENTER-PRICE.
           03  WS-YYP             PIC 9999.
           03  WS-MMP             PIC 99.
           03  WS-DDP             PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(23) VALUE " ".
           03  FILLER         PIC X(19) VALUE "P R I C E   C O S T".
           03  FILLER         PIC X(25) VALUE "   R E P O R T".
           03  H1-MU-GP       PIC X(9) VALUE " ".
           03  H1-MIN-PERC    PIC Z99.99.
           03  H1-TYPE        PIC X(12) VALUE " ".
           03  H1-TOP-DESC.
              05  FILLER      PIC X(4) VALUE "TOP:".
              05  H1-TOP      PIC Z(6)9.
           03  FILLER         PIC X(2) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
       01  HEAD2.
           03  FILLER         PIC X(40) VALUE " ".
           03  FILLER         PIC X(33) VALUE ALL "*".
           03  FILLER         PIC X(59) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(10) VALUE "CATEGORY:".
           03  HD-CAT         PIC X(3) VALUE " ".
           03  FILLER         PIC X(119) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(16) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(40) VALUE "DESCRIPTION".
           03  FILLER         PIC X(9) VALUE "SUPPLIER".
           03  FILLER         PIC X(9) VALUE " A/S".
           03  FILLER         PIC X(10) VALUE "PRICE".
           03  FILLER         PIC X(9) VALUE "AVE.".
           03  FILLER         PIC X(7) VALUE "REP.".
           03  FILLER         PIC X(11) VALUE "MARGIN".
           03  H4-MU-GP       PIC X(7) VALUE " ".
           03  H4-MU-D9       PIC X(8) VALUE " ".
           03  H4-MIN-PERC    PIC X(8) VALUE "  MIN-% ".
           03  FILLER         PIC X(20) VALUE "CREATED   PRICE-CHG".
           03  H4-EXTRAS.
              05  FILLER      PIC X(38) VALUE
                 "SU-DISC DISC1 DISC2 DISC3 DISC4 DISC5".
              05  FILLER      PIC X(35) VALUE
                 "DISC6 DISC7 DISC8 DISC9 CALC-COST".
       01  DETAIL-LINE.
           03  D-STOCKNO      PIC X(16) VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(21) VALUE " ".
           03  D-SUPPLIER     PIC X(10) VALUE " ".
           03  D-ANALYSIS     PIC X(3) VALUE " ".
           03  D-PRICE        PIC Z(5)9.99.
           03  D-AVE          PIC Z(5)9.99.
           03  D-REP          PIC Z(5)9.99.
           03  D-MARGIN       PIC Z(5)9.99-.
           03  D-PERC         PIC Z(4)9.99-.
           03  D-PERC-D9      PIC Z(4)9.99-.
           03  D-MIN-PERC     PIC Z(4)9.99-.
           03  D-CREATED      PIC X(11) VALUE " ".
           03  D-PRICE-CHNG   PIC X(10) VALUE " ".
           03 DETAIL-EXTRAS.
               05  FILLER         PIC X(2) VALUE " ".
               05  D-SU-DISC      PIC Z9.99.
               05  FILLER         PIC X(1) VALUE " ".
               05  D-DISC1        PIC Z9.99.
               05  FILLER         PIC X(1) VALUE " ".
               05  D-DISC2        PIC Z9.99.
               05  FILLER         PIC X(1) VALUE " ".
               05  D-DISC3        PIC Z9.99.
               05  FILLER         PIC X(1) VALUE " ".
               05  D-DISC4        PIC Z9.99.
               05  FILLER         PIC X(1) VALUE " ".
               05  D-DISC5        PIC Z9.99.
               05  FILLER         PIC X(1) VALUE " ".
               05  D-DISC6        PIC Z9.99.
               05  FILLER         PIC X(1) VALUE " ".
               05  D-DISC7        PIC Z9.99.
               05  FILLER         PIC X(1) VALUE " ".
               05  D-DISC8        PIC Z9.99.
               05  FILLER         PIC X(1) VALUE " ".
               05  D-DISC9        PIC Z9.99.
               05  FILLER         PIC X(1) VALUE " ".
               05  D-CALC-COST    PIC Z(5)9.99.
       01  TOTAL-LINE.
           03  FILLER         PIC X(45) VALUE " ".
           03  FILLER         PIC X(52) VALUE
           "RAND MARGIN & % MARGIN = TOTAL MU / GP FOR RANGE : ".
           03  TOT-MARGIN     PIC Z(5)9.99-. 
           03  TOT-PERC       PIC Z(4)9.99-. 
           03  FILLER         PIC X(11) VALUE " ".
       01  DATE-LINE.
           03  DATE-FILL      PIC X(23) VALUE " ".
           03  DL-DATE        PIC X(10).
           03  FILLER         PIC X(23) VALUE
           " HAVE BEEN PRINTED **".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 324 TO POS
           DISPLAY "** PRICE / COST REPORT **" AT POS
           MOVE 424 TO POS
           DISPLAY "*************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           IF WS-TOP = " "
              PERFORM PRINT-ROUTINE
              PERFORM END-OFF.
           PERFORM PRINT-ALL-ROUTINE.
           PERFORM READ-RANDOM-FILE.
           PERFORM PRINT-SORT-ROUTINE.
           PERFORM DELETE-TRANS.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE " " TO WS-RANGE1 WS-RANGE2 WS-TYPE.
           MOVE 811 TO POS.
           DISPLAY 
           "PRINT REPORT BY S=STOCK OR BY U=SUPPLIER              :[ ]"
           AT POS.
           MOVE 867 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 66        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TYPE.

           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-005.
           IF WS-TYPE NOT = "S" AND NOT = "U"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-000.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-005
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-005.
           MOVE 921 TO POS.
           IF WS-TYPE = "S"
           DISPLAY "            FROM STOCK NUMBER: [               ]"
                     AT POS
           ELSE
           DISPLAY "           FROM SUPPLIER NAME: [               ]"
                     AT POS.
           MOVE 953 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 6         TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

           IF W-ESCAPE-KEY = 4
              GO TO GET-000.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
           MOVE 1021 TO POS.
           IF WS-TYPE = "S"
           DISPLAY "              TO STOCK NUMBER: [               ]"
                     AT POS
           ELSE
           DISPLAY "             TO SUPPLIER NAME: [               ]"
                     AT POS.
           MOVE 1053 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           IF W-ESCAPE-KEY = 4
              GO TO GET-005.
            IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
           MOVE 1210 TO POS
           DISPLAY
           "SHOULD THIS BE BASED ON STOCK M/U OVER A %, OR BELOW A %"
           AT POS
           MOVE 1310 TO POS
           DISPLAY
           "DIFFERENCE IS BETWEEN AVE & LAST COST BY A PERCENT."
           AT POS
           MOVE 1415 TO POS
           DISPLAY 
           "ENTER O=OVER, B=BELOW, D=DIFFERENCE, M=BELOW MIN% :[ ]"
            AT POS
           ADD 52 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 66        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-OVER-BELOW.

           IF W-ESCAPE-KEY = 4
              GO TO GET-010.
           IF WS-OVER-BELOW NOT = "O" AND NOT = "B" AND NOT = "D"
                        AND NOT = "M"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-020
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-015.
       GET-020.
           IF WS-OVER-BELOW = "M"
               GO TO GET-040.
           MOVE " " TO ALPHA-RATE.
           MOVE 1715 TO POS.
           DISPLAY 
           "LEAVE BLANK TO PRINT ALL STOCK-NUMBERS" AT POS.
           MOVE 1610 TO POS.
           DISPLAY 
           "ENTER A M/U % TO BASE THIS REPORT ON              :[      ]"
                      AT POS.
           ADD 52 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 6         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 61        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-M-UP.

           IF W-ESCAPE-KEY = 4
              GO TO GET-015.
           IF WS-OVER-BELOW = "M"
               GO TO GET-030.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-030.
            MOVE WS-M-UP TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-MIN-PERC.
            IF WS-MIN-PERC < 0
              MOVE "THE PERCENTAGE CANNOT BE NEGATIVE, RE-ENTER"
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO GET-020.
            MOVE WS-MIN-PERC TO WS-PERC-DIS.
            MOVE 1662 TO POS.
            DISPLAY WS-PERC-DIS AT POS.
       GET-040.
           MOVE " " TO ALPHA-RATE.
           MOVE 1910 TO POS.
           DISPLAY
           "ENTER A DATE-CREATED FROM WHICH TO PRINT      :[          ]"
            AT POS.
           MOVE 2015 TO POS.
           DISPLAY "LEAVE BLANK TO PRINT ALL IN THE RANGE." AT POS.
           MOVE 1958 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATE-ACCEPT.

           IF W-ESCAPE-KEY = 4
            IF WS-OVER-BELOW = "M"
               GO TO GET-015
            ELSE
               GO TO GET-020.
           IF WS-DATE-ACCEPT = " "
               GO TO GET-045.
           MOVE WS-DATE-ACCEPT TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO GET-040.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE 1958 TO POS.
           DISPLAY DISPLAY-DATE AT POS.
           MOVE DISPLAY-DATE TO WS-DATE-ACCEPT
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-DATE-ENTER.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO GET-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-045
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
       GET-045.
           MOVE " " TO ALPHA-RATE.
           MOVE 2210 TO POS.
           DISPLAY
           "PRICES CHANGED BEFORE THIS DATE WILL PRINT    :[          ]"
            AT POS.
           MOVE 2315 TO POS.
           DISPLAY "LEAVE BLANK TO PRINT ALL IN THE RANGE." AT POS.
           MOVE 2258 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 19        TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATE-PRICE.

           IF W-ESCAPE-KEY = 4
              GO TO GET-040.
           IF WS-DATE-PRICE = " "
               GO TO GET-050.
           MOVE WS-DATE-PRICE TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO GET-045.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE 2258 TO POS.
           DISPLAY DISPLAY-DATE AT POS.
           MOVE DISPLAY-DATE TO WS-DATE-PRICE
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-DATE-ENTER-PRICE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO GET-045.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-045.
       GET-050.
           MOVE 2410 TO POS.
           DISPLAY
           "PRINT SUPPLIER DISC & DISC1-9,  Y OR N                 :[ ]"
            AT POS.
           ADD 57 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 21        TO CDA-ROW.
           MOVE 66        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PRINT-DISC.

           IF W-ESCAPE-KEY = 4
              GO TO GET-045.
           IF WS-PRINT-DISC NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
            PERFORM ERROR-020.
       GET-060.
           MOVE 2610 TO POS.
           DISPLAY
           "PRINT ONLY IF QTY-ONHAND OR QTY-ONRES > 0, Y OR N      :[ ]"
            AT POS.
           ADD 57 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 23        TO CDA-ROW.
           MOVE 66        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PRINT-QTY.

           IF W-ESCAPE-KEY = 4
              GO TO GET-050.
           IF WS-PRINT-QTY NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-060.
       GET-080.
           MOVE 2710 TO POS
           DISPLAY
           "ENTER TOP ? 50, 100, ETC.  Leave Blank NO SORT  :[        ]"
                AT POS
           ADD 50 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 8         TO CDA-DATALEN.
           MOVE 24        TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TOP.

           IF W-ESCAPE-KEY = 4
               GO TO GET-060.
           IF WS-TOP = " "
               MOVE " " TO H1-TOP-DESC
               GO TO GET-085.
           MOVE WS-TOP TO ALPHA-RATE
           PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE < 0
              MOVE "YOU MUST ENTER A POSITIVE NUMBER, PLEASE RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO GET-080.
           MOVE NUMERIC-RATE TO WS-DISP-ANS
           MOVE NUMERIC-RATE TO H1-TOP WS-NUMBER-TO-PRINT
           DISPLAY WS-DISP-ANS AT POS.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-085
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-080.
       GET-085.
           IF WS-TOP = " "
               GO TO GET-100.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           MOVE 2810 TO POS
           DISPLAY
           "PRINT BASED ON: R=RAND AMT, U=UNIT AMT          :[ ]" AT POS
           ADD 50 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 25        TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RAND-UNIT.

           IF W-ESCAPE-KEY = 4
              GO TO GET-080.
           IF WS-RAND-UNIT NOT = "R" AND NOT = "U"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-085.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-090
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-085.
       GET-090.
           MOVE 2910 TO POS
           DISPLAY 
           "PRINT BASED ON: M=MTD, T=THIS YEAR, L=LAST YEAR :[ ]"
               AT POS
           ADD 50 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PERIOD.

           IF W-ESCAPE-KEY = 4
              GO TO GET-085.
           IF WS-PERIOD NOT = "T" AND NOT = "L" AND NOT = "M"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-090.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-100
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-090.
       GET-100.
            PERFORM ERROR-020.
            MOVE 3010 TO POS.
            DISPLAY "Report Is Being Compiled, Please Be Patient."
            AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           PERFORM ERROR-020.
           IF WS-TYPE = "S"
              MOVE WS-RANGE1 TO ST-STOCKNUMBER
              START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE
           ELSE
              MOVE WS-RANGE1 TO ST-SUPPLIER
              START STOCK-MASTER KEY NOT < ST-SUPPLIER
              INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "EXITING PROGRAM ON BAD START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               EXIT PROGRAM.
       PRR-002.
            READ STOCK-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE. 
            IF WS-STOCK-ST1 = 10
               MOVE 0 TO WS-STOCK-ST1
               PERFORM TOTALS
               GO TO PRR-999.
            IF WS-STOCK-ST1 NOT = 0
             MOVE "STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRR-002.
            IF WS-TYPE = "S"
             IF ST-STOCKNUMBER < WS-RANGE1
               GO TO PRR-002.
            IF WS-TYPE = "S"
             IF ST-STOCKNUMBER > WS-RANGE2
               PERFORM TOTALS
               GO TO PRR-999.
            IF WS-TYPE = "U"
             IF ST-SUPPLIER < WS-RANGE1
               GO TO PRR-002.
            IF WS-TYPE = "U"
             IF ST-SUPPLIER > WS-RANGE2
               PERFORM TOTALS
               GO TO PRR-999.
            IF WS-CAT = "   "
               MOVE ST-CATEGORY TO WS-CAT.
               
            IF WS-TYPE = "S"
              MOVE 3010 TO POS
              DISPLAY "Stock Number Being Read:" AT POS
              ADD 25 TO POS
              DISPLAY ST-STOCKNUMBER AT POS
            ELSE
              MOVE 3010 TO POS
              DISPLAY "Supplier Name Being Read:" AT POS
              ADD 26 TO POS
              DISPLAY ST-SUPPLIER AT POS.
               
            IF WS-PRINT-QTY = "Y"
               IF ST-QTYONHAND = 0
                IF ST-QTYONRESERVE = 0
                  GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-020.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE
            MOVE " " TO PRINT-REC
            MOVE WS-MIN-PERC TO H1-MIN-PERC.
            IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF WS-PRINT-DISC = "Y"
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
               
            IF WS-OVER-BELOW = "B" OR = "M"
             IF WS-QUES-MU-GP-PERC = "N"
                MOVE "MIN M/U%"   TO H1-MU-GP
                MOVE " BELOW M/U" TO H1-TYPE
             ELSE
                MOVE "MIN G/P%"   TO H1-MU-GP
                MOVE " BELOW G/P" TO H1-TYPE.
                
            IF WS-OVER-BELOW = "O"
             IF WS-QUES-MU-GP-PERC = "N"
                MOVE "MAX M/U%"   TO H1-MU-GP
                MOVE " OVER M/U" TO H1-TYPE
             ELSE
                MOVE "MAX G/P%"   TO H1-MU-GP
                MOVE " OVER G/P" TO H1-TYPE.
                
            IF WS-OVER-BELOW = "D"
             IF WS-QUES-MU-GP-PERC = "N"
                MOVE " DIFF M/U" TO H1-TYPE
             ELSE
                MOVE " DIFF G/P" TO H1-TYPE.
                
            IF WS-QUES-MU-GP-PERC = "N"
               MOVE "M/U%"   TO H4-MU-GP
               MOVE "MU%-D9" TO H4-MU-D9
            ELSE
               MOVE "G/P%"   TO H4-MU-GP
               MOVE "GP%-D9" TO H4-MU-D9.
               
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2
            MOVE " " TO PRINT-REC.
            IF WS-PRINT-DISC = "N"
               MOVE " " TO H4-EXTRAS.
            WRITE PRINT-REC FROM HEAD4
            MOVE " " TO PRINT-REC
            MOVE WS-CAT TO HD-CAT
            WRITE PRINT-REC FROM HEAD3
            MOVE " " TO PRINT-REC
            MOVE 6 TO LINE-CNT.
       PRR-020.
           IF WS-DATE-ACCEPT NOT = " "
            IF ST-DATE-CREATED < WS-DATE-ENTER
                GO TO PRR-002.
           IF WS-DATE-PRICE NOT = " "
            IF ST-LASTPRICECHANGE > WS-DATE-ENTER-PRICE
                GO TO PRR-002.
                   
           IF WS-OVER-BELOW = "D"
              PERFORM CHECK-COSTS.
           IF WS-OVER-BELOW = "D"
            IF WS-PERC NOT > WS-MIN-PERC
                GO TO PRR-002.
           IF WS-OVER-BELOW = "D"
                GO TO PRR-021.
           
           COMPUTE WS-MARGIN = ST-PRICE - ST-AVERAGECOST
           IF WS-QUES-MU-GP-PERC = "N"
             COMPUTE WS-PERC ROUNDED =
                   (WS-MARGIN / ST-AVERAGECOST) * 100
           ELSE
             COMPUTE WS-PERC ROUNDED =
                   (WS-MARGIN / ST-PRICE) * 100.
                   
           IF WS-OVER-BELOW = "B"
            IF WS-MIN-PERC NOT = 0
             IF WS-PERC NOT < WS-MIN-PERC
                GO TO PRR-002.
           IF WS-OVER-BELOW = "O"
            IF WS-MIN-PERC NOT = 0
             IF WS-PERC NOT > WS-MIN-PERC
                GO TO PRR-002.

      *     IF WS-OVER-BELOW = "M"
            IF WS-QUES-MU-GP-PERC = "N"
               COMPUTE WS-PERC-D9 ROUNDED = (((ST-PRICE - (ST-PRICE *
                       ST-DISCOUNT9 / 100)) - ST-AVERAGECOST)
                        / ST-AVERAGECOST) * 100
            ELSE
               COMPUTE WS-PERC-D9 ROUNDED = (((ST-PRICE - (ST-PRICE *
                       ST-DISCOUNT9 / 100)) - ST-AVERAGECOST) 
                       / ST-PRICE) * 100.
           IF WS-OVER-BELOW = "M"
             IF WS-PERC-D9 NOT < ST-MIN-PERC
                GO TO PRR-002.
       PRR-021.
           IF WS-QUES-MU-GP-PERC = "Y"
               GO TO PRR-022.
            IF ST-CATEGORY NOT = WS-CAT
               PERFORM TOTALS
            IF WS-OVER-BELOW = "D"
               PERFORM CHECK-COSTS
               MOVE ST-CATEGORY TO WS-CAT
               MOVE WS-CAT      TO HD-CAT
               WRITE PRINT-REC FROM HEAD3
               MOVE " " TO PRINT-REC
               ADD 1 TO LINE-CNT
            ELSE
               COMPUTE WS-MARGIN = ST-PRICE - ST-AVERAGECOST
               COMPUTE WS-PERC ROUNDED =
                   (WS-MARGIN / ST-AVERAGECOST) * 100
               MOVE ST-CATEGORY TO WS-CAT
               MOVE WS-CAT      TO HD-CAT
               WRITE PRINT-REC FROM HEAD3
               MOVE " " TO PRINT-REC
               ADD 1 TO LINE-CNT.
               GO TO PRR-023.
       PRR-022.
            IF ST-CATEGORY NOT = WS-CAT
               PERFORM TOTALS
             IF WS-OVER-BELOW = "D"
               PERFORM CHECK-COSTS
               MOVE ST-CATEGORY TO WS-CAT
               MOVE WS-CAT      TO HD-CAT
               WRITE PRINT-REC FROM HEAD3
               MOVE " " TO PRINT-REC
               ADD 1 TO LINE-CNT
            ELSE
               COMPUTE WS-MARGIN = ST-PRICE - ST-AVERAGECOST
               COMPUTE WS-PERC ROUNDED =
                   (WS-MARGIN / ST-PRICE) * 100
               MOVE ST-CATEGORY TO WS-CAT
               MOVE WS-CAT      TO HD-CAT
               WRITE PRINT-REC FROM HEAD3
               MOVE " " TO PRINT-REC
               ADD 1 TO LINE-CNT.
       PRR-023.
           MOVE ST-STOCKNUMBER     TO D-STOCKNO
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE ST-SUPPLIER        TO D-SUPPLIER
           MOVE ST-PRICE           TO D-PRICE
           MOVE ST-ANALYSIS        TO D-ANALYSIS
           MOVE ST-AVERAGECOST     TO D-AVE
           MOVE ST-LASTCOST        TO D-REP
           MOVE WS-MARGIN          TO D-MARGIN
           MOVE WS-PERC            TO D-PERC
           MOVE ST-MIN-PERC        TO D-MIN-PERC.
      *      IF WS-QUES-MU-GP-PERC = "N"
      *         COMPUTE WS-PERC ROUNDED = (((ST-PRICE - (ST-PRICE *
      *                 ST-DISCOUNT9 / 100)) - ST-AVERAGECOST)
      *                  / ST-AVERAGECOST) * 100
      *      ELSE
      *         COMPUTE WS-PERC ROUNDED = (((ST-PRICE - (ST-PRICE *
      *                 ST-DISCOUNT9 / 100)) - ST-AVERAGECOST) 
      *                 / ST-PRICE) * 100.
           MOVE WS-PERC-D9         TO D-PERC-D9.
           MOVE ST-DATE-CREATED    TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-CREATED
           MOVE ST-LASTPRICECHANGE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-PRICE-CHNG.
           
           IF WS-PRINT-DISC = "Y"
              MOVE ST-SUPPLIERDISC TO D-SU-DISC
              MOVE ST-DISCOUNT1    TO D-DISC1
              MOVE ST-DISCOUNT2    TO D-DISC2
              MOVE ST-DISCOUNT3    TO D-DISC3
              MOVE ST-DISCOUNT4    TO D-DISC4
              MOVE ST-DISCOUNT5    TO D-DISC5
              MOVE ST-DISCOUNT6    TO D-DISC6
              MOVE ST-DISCOUNT7    TO D-DISC7
              MOVE ST-DISCOUNT8    TO D-DISC8
              MOVE ST-DISCOUNT9    TO D-DISC9
              COMPUTE WS-COST = ST-PRICE -
                  (ST-PRICE * (ST-SUPPLIERDISC / 100))
              MOVE WS-COST TO D-CALC-COST
           ELSE
              MOVE " " TO DETAIL-EXTRAS.
           
           WRITE PRINT-REC FROM DETAIL-LINE.
           
           IF WS-OVER-BELOW = "D"
              ADD ST-AVERAGECOST      TO WS-PRICETOT
              ADD ST-LASTCOST         TO WS-COSTTOT
           ELSE
              ADD ST-PRICE            TO WS-PRICETOT
              ADD ST-AVERAGECOST      TO WS-COSTTOT.
           MOVE " " TO PRINT-REC
           ADD 1    TO LINE-CNT
           MOVE 0   TO WS-PERC
                       WS-MARGIN.
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       CHECK-COSTS SECTION.
       CC-005.
      *THIS IS USED TO MAKE SURE THAT THE DIFFERENCE IS ALWAYS POSITIVE
      *WHEN USED FOR D.
      
           MOVE ST-LASTCOST    TO WS-LAST
           MOVE ST-AVERAGECOST TO WS-AVE.
              
           COMPUTE WS-MARGIN = WS-AVE - WS-LAST.
           IF WS-QUES-MU-GP-PERC = "N"
              COMPUTE WS-PERC ROUNDED =
                 (WS-MARGIN / ST-AVERAGECOST) * 100
           ELSE
              COMPUTE WS-PERC ROUNDED =
                 (WS-MARGIN / ST-PRICE) * 100.
              
           IF WS-PERC < 0
              COMPUTE WS-PERC = WS-PERC * -1
              COMPUTE WS-MARGIN = WS-MARGIN * -1.
       CC-999.
           EXIT.
      *
       TOTALS SECTION.
       T-010.
           COMPUTE WS-MARGIN = WS-PRICETOT - WS-COSTTOT.
           MOVE WS-MARGIN TO TOT-MARGIN.
           IF WS-QUES-MU-GP-PERC = "N"
              COMPUTE WS-PERC ROUNDED =
                  (WS-MARGIN / WS-COSTTOT) * 100
           ELSE
              COMPUTE WS-PERC ROUNDED =
                  (WS-MARGIN / WS-PRICETOT) * 100.
                  
           MOVE WS-PERC   TO TOT-PERC.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC.

           ADD 3  TO LINE-CNT.
           MOVE 0 TO WS-PERC
                     WS-MARGIN
                     WS-PRICETOT
                     WS-COSTTOT.
        T-999.
           EXIT.
      *
       PRINT-ALL-ROUTINE SECTION.
       PRINT-000.
           MOVE 3010 TO POS
           DISPLAY "READING ALL STOCK ITEMS FOR TOTALS FIRST." AT POS.
           MOVE WS-RANGE1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BAD START,'ESC TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               EXIT PROGRAM.
       PRINT-005.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
              GO TO PRINT-999.
           IF WS-STOCK-ST1 NOT = 0
             MOVE "STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRINT-005.
           IF ST-STOCKNUMBER < WS-RANGE1
              GO TO PRINT-005.
           IF ST-STOCKNUMBER > WS-RANGE2
              GO TO PRINT-999.

           MOVE " " TO WS-MESSAGE
           PERFORM ERROR-020.
           MOVE 3010 TO POS
           DISPLAY "STOCK NUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
           
           IF WS-PERIOD = "M"
            IF ST-SALESRANDSMTD NOT > 0
                 GO TO PRINT-005.
           IF WS-PERIOD = "T"
            IF ST-SALESRANDSYTD NOT > 0
                 GO TO PRINT-005.
           IF WS-PERIOD = "L"
            IF ST-SALESRANDSLAST NOT > 0
                 GO TO PRINT-005.
           
           IF WS-PERIOD = "M"
            IF WS-RAND-UNIT = "R"
               MOVE ST-SALESRANDSMTD TO RANDOM-NUMBER.
           IF WS-PERIOD = "T"
            IF WS-RAND-UNIT = "R"
               MOVE ST-SALESRANDSYTD TO RANDOM-NUMBER.
           IF WS-PERIOD = "L"
            IF WS-RAND-UNIT = "R"
               MOVE ST-SALESRANDSLAST TO RANDOM-NUMBER.
               
           IF WS-PERIOD = "M"
            IF WS-RAND-UNIT = "U"
               MOVE ST-SALESUNITMTD TO RANDOM-NUMBER.
           IF WS-PERIOD = "T"
            IF WS-RAND-UNIT = "U"
               MOVE ST-SALESUNITSYTD TO RANDOM-NUMBER.
           IF WS-PERIOD = "L"
            IF WS-RAND-UNIT = "U"
               MOVE ST-SALESUNITSLAST TO RANDOM-NUMBER.
               
           IF RANDOM-NUMBER > 0
               ADD 1 TO RANDOM-INDEX
               PERFORM WRITE-RANDOM-RECORD.
           GO TO PRINT-005.
       PRINT-999.
           EXIT.
      *
       PRINT-SORT-ROUTINE SECTION.
       PRS-000.
           CLOSE HIGH-FILE.
           PERFORM OPEN-037.
           
           MOVE " " TO WS-MESSAGE
           PERFORM ERROR-020.
           MOVE 3010 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 3010 TO POS
           DISPLAY "READING STOCK BY HIGHEST VALUES.          " AT POS.
           MOVE 0 TO HIGH-NUMBER
           START HIGH-FILE KEY NOT < HIGH-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "BAD START ON HIGH" TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-RANDOM-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-RANDOM-ST1
               EXIT PROGRAM.
       PRS-005.
           READ HIGH-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-RANDOM-ST1 = 10
              PERFORM TOTALS
              GO TO PRS-999.
           IF WS-RANDOM-ST1 NOT = 0
             MOVE "RANDOM BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-RANDOM-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-RANDOM-ST1
               GO TO PRS-005.
              
           MOVE 3010 TO POS
           DISPLAY "STOCK NUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY HIGH-STOCK AT POS.
           
           MOVE HIGH-STOCK TO ST-STOCKNUMBER
           PERFORM READ-STOCK.
               
            IF WS-PRINT-QTY = "Y"
               IF ST-QTYONHAND = 0
                IF ST-QTYONRESERVE = 0
                  GO TO PRS-005.
       PRS-010.
            IF LINE-CNT < 60
               GO TO PRS-020.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE
            MOVE " " TO PRINT-REC
            MOVE WS-MIN-PERC TO H1-MIN-PERC.
            IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF WS-PRINT-DISC = "Y"
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
               
            IF WS-OVER-BELOW = "B" OR = "M"
             IF WS-QUES-MU-GP-PERC = "N"
                MOVE "MIN M/U%"   TO H1-MU-GP
                MOVE " BELOW M/U" TO H1-TYPE
             ELSE
                MOVE "MIN G/P%"   TO H1-MU-GP
                MOVE " BELOW G/P" TO H1-TYPE.
                
            IF WS-OVER-BELOW = "O"
             IF WS-QUES-MU-GP-PERC = "N"
                MOVE "MAX M/U%"   TO H1-MU-GP
                MOVE " OVER M/U" TO H1-TYPE
             ELSE
                MOVE "MAX G/P%"   TO H1-MU-GP
                MOVE " OVER G/P" TO H1-TYPE.
                
            IF WS-OVER-BELOW = "D"
             IF WS-QUES-MU-GP-PERC = "N"
                MOVE " DIFF M/U" TO H1-TYPE
             ELSE
                MOVE " DIFF G/P" TO H1-TYPE.
                
            IF WS-QUES-MU-GP-PERC = "N"
               MOVE "M/U%"   TO H4-MU-GP
               MOVE "MU%-D9" TO H4-MU-D9
            ELSE
               MOVE "G/P%"   TO H4-MU-GP
               MOVE "GP%-D9" TO H4-MU-D9.
               
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2
            MOVE " " TO PRINT-REC.
            IF WS-PRINT-DISC = "N"
               MOVE " " TO H4-EXTRAS.
            WRITE PRINT-REC FROM HEAD4
            MOVE " " TO PRINT-REC
            MOVE WS-CAT TO HD-CAT
            WRITE PRINT-REC FROM HEAD3
            MOVE " " TO PRINT-REC
            MOVE 6 TO LINE-CNT.
       PRS-020.
           IF WS-DATE-ACCEPT NOT = " "
            IF ST-DATE-CREATED < WS-DATE-ENTER
                GO TO PRS-005.
           IF WS-DATE-PRICE NOT = " "
            IF ST-LASTPRICECHANGE > WS-DATE-ENTER-PRICE
                GO TO PRS-005.
                   
           IF WS-OVER-BELOW = "D"
              PERFORM CHECK-COSTS.
           IF WS-OVER-BELOW = "D"
            IF WS-PERC NOT > WS-MIN-PERC
                GO TO PRS-005.
           IF WS-OVER-BELOW = "D"
                GO TO PRS-021.
           
           COMPUTE WS-MARGIN = ST-PRICE - ST-AVERAGECOST
           IF WS-QUES-MU-GP-PERC = "N"
             COMPUTE WS-PERC ROUNDED =
                   (WS-MARGIN / ST-AVERAGECOST) * 100
           ELSE
             COMPUTE WS-PERC ROUNDED =
                   (WS-MARGIN / ST-PRICE) * 100.
                   
           IF WS-OVER-BELOW = "B"
            IF WS-MIN-PERC NOT = 0
             IF WS-PERC NOT < WS-MIN-PERC
                GO TO PRS-005.
           IF WS-OVER-BELOW = "O"
            IF WS-MIN-PERC NOT = 0
             IF WS-PERC NOT > WS-MIN-PERC
                GO TO PRS-005.

           IF WS-QUES-MU-GP-PERC = "N"
               COMPUTE WS-PERC-D9 ROUNDED = (((ST-PRICE - (ST-PRICE *
                       ST-DISCOUNT9 / 100)) - ST-AVERAGECOST)
                        / ST-AVERAGECOST) * 100
            ELSE
               COMPUTE WS-PERC-D9 ROUNDED = (((ST-PRICE - (ST-PRICE *
                       ST-DISCOUNT9 / 100)) - ST-AVERAGECOST) 
                       / ST-PRICE) * 100.
           IF WS-OVER-BELOW = "M"
             IF WS-PERC-D9 NOT < ST-MIN-PERC
                GO TO PRS-005.
       PRS-021.
           MOVE ST-STOCKNUMBER     TO D-STOCKNO
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE ST-SUPPLIER        TO D-SUPPLIER
           MOVE ST-PRICE           TO D-PRICE
           MOVE ST-ANALYSIS        TO D-ANALYSIS
           MOVE ST-AVERAGECOST     TO D-AVE
           MOVE ST-LASTCOST        TO D-REP
           MOVE WS-MARGIN          TO D-MARGIN
           MOVE WS-PERC            TO D-PERC
           MOVE ST-MIN-PERC        TO D-MIN-PERC.
           MOVE WS-PERC-D9         TO D-PERC-D9.
           MOVE ST-DATE-CREATED    TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-CREATED
           MOVE ST-LASTPRICECHANGE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-PRICE-CHNG.
           
           IF WS-PRINT-DISC = "Y"
              MOVE ST-SUPPLIERDISC TO D-SU-DISC
              MOVE ST-DISCOUNT1    TO D-DISC1
              MOVE ST-DISCOUNT2    TO D-DISC2
              MOVE ST-DISCOUNT3    TO D-DISC3
              MOVE ST-DISCOUNT4    TO D-DISC4
              MOVE ST-DISCOUNT5    TO D-DISC5
              MOVE ST-DISCOUNT6    TO D-DISC6
              MOVE ST-DISCOUNT7    TO D-DISC7
              MOVE ST-DISCOUNT8    TO D-DISC8
              MOVE ST-DISCOUNT9    TO D-DISC9
              COMPUTE WS-COST = ST-PRICE -
                  (ST-PRICE * (ST-SUPPLIERDISC / 100))
              MOVE WS-COST TO D-CALC-COST
           ELSE
              MOVE " " TO DETAIL-EXTRAS.
           
           WRITE PRINT-REC FROM DETAIL-LINE.
           
           IF WS-OVER-BELOW = "D"
              ADD ST-AVERAGECOST      TO WS-PRICETOT
              ADD ST-LASTCOST         TO WS-COSTTOT
           ELSE
              ADD ST-PRICE            TO WS-PRICETOT
              ADD ST-AVERAGECOST      TO WS-COSTTOT.
           MOVE " " TO PRINT-REC
           ADD 1    TO LINE-CNT
           MOVE 0   TO WS-PERC
                       WS-MARGIN.
           GO TO PRS-005.
       PRS-999.
           EXIT.
      *
       READ-RANDOM-FILE SECTION.
       RRF-000.
           IF WS-RANDOM-WRITTEN NOT = "Y"
              MOVE "NOTHING TO PRINT IN THAT RANGE." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              EXIT PROGRAM.
           CLOSE RANDOM-FILE.
           PERFORM OPEN-035.
           MOVE " " TO WS-MESSAGE
           PERFORM ERROR-020.
           MOVE 3010 TO POS
           DISPLAY "READING STOCK BY LOWEST VALUES.          " AT POS.
           MOVE 99999 TO HIGH-NUMBER.
           MOVE 1     TO RANDOM-NUMBER
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
             MOVE "RANDOM BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-RANDOM-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-RANDOM-ST1
               GO TO RRF-005.
              
           SUBTRACT 1 FROM HIGH-NUMBER
              
           MOVE " " TO WS-MESSAGE
           PERFORM ERROR-020.
           MOVE 3010 TO POS
           DISPLAY "RANDOM NUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY RANDOM-STOCK AT POS
           ADD 20 TO POS
           DISPLAY HIGH-NUMBER AT POS.
           
           PERFORM WRITE-HIGH-RECORD.
           GO TO RRF-005.
       RRF-999.
           EXIT.
      *
       WRITE-RANDOM-RECORD SECTION.
       WRR-000.
           IF WS-RANDOM-WRITTEN NOT = "Y"
               MOVE "Y" TO WS-RANDOM-WRITTEN.
       WRR-005.
           MOVE ST-STOCKNUMBER TO RANDOM-STOCK.
           WRITE RANDOM-REC
              INVALID KEY NEXT SENTENCE.
              
           IF WS-RANDOM-ST1 NOT = 0
              ADD 1 TO RANDOM-INDEX
              MOVE RANDOM-INDEX TO WS-MESSAGE
              MOVE 2855 TO POS
              DISPLAY "RETRY:" AT POS
              MOVE 2862 TO POS
              DISPLAY WS-MESSAGE AT POS
              GO TO WRR-005.
              
           GO TO WRR-999.
              
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "RANDOM RECORD INVALID ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-RANDOM-ST1.
       WRR-999.
            EXIT.
      *
       WRITE-HIGH-RECORD SECTION.
       WRR-005.
           MOVE RANDOM-STOCK TO HIGH-STOCK.
           WRITE HIGH-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "HIGH RECORD INVALID ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-RANDOM-ST1.
       WRR-999.
            EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           READ PARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO SLPARAMETER RECORD ON FILE, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "SLPARAMETER BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RP-000.
       RP-999.
           EXIT.
      *
       READ-STOCK SECTION.
       RS-005.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RS-005.
       RS-999.
            EXIT.
      *
       READ-INVQUES-FILE SECTION.
       RINVQUES-000.
            MOVE 1 TO PA-RECORD.
            MOVE 6 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RINVQUES-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "N" TO WS-QUES-MU-GP-PERC
               GO TO RINVQUES-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY RINVQUES, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RINVQUES-010.
       RINVQUES-900.
            MOVE INVQUES-MU-GP-PERC TO WS-QUES-MU-GP-PERC.
       RINVQUES-999.
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
          OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0 
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-008.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-008.
           PERFORM READ-INVQUES-FILE.
           PERFORM READ-PARAMETER.
           CLOSE PARAMETER-FILE.
           
           GO TO OPEN-036.
       OPEN-035.
           OPEN I-O RANDOM-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE 0 TO WS-RANDOM-ST1
              MOVE
             "RANDOM FILE OPEN AT ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
             GO TO OPEN-035.
       OPEN-036.
           OPEN OUTPUT RANDOM-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE 0 TO WS-RANDOM-ST1
              MOVE
             "RANDOM FILE OPEN AT ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
             GO TO OPEN-036.
       OPEN-0361.
            GO TO OPEN-038.
       OPEN-037.
           OPEN I-O HIGH-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE
             "HIGH FILE OPEN AT ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-037.
       OPEN-038.
           OPEN OUTPUT HIGH-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE
             "HIGH FILE OPEN AT ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-038.
       OPEN-020.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF WS-OVER-BELOW = "D"
              MOVE
           "* ONLY ITEMS WHERE THE COSTS VARY BY > INPUT PERC PRINTED *"
              TO PRINT-REC
             WRITE PRINT-REC AFTER 1
             GO TO END-400.
           IF WS-OVER-BELOW = "B"
              MOVE
           "* ONLY ITEMS BELOW THE MINIMUM MU% / GP% HAVE PRINTED *"
              TO PRINT-REC
             WRITE PRINT-REC AFTER 1
             GO TO END-400.
           IF WS-OVER-BELOW = "M"
              MOVE
           "* ONLY ITEMS WHERE PRICE - DISC9 < MIN-PERC HAVE PRINTED *"
              TO PRINT-REC
             WRITE PRINT-REC AFTER 1
             GO TO END-400.
           IF WS-OVER-BELOW = "O"
              MOVE
           "* ONLY ITEMS ABOVE THE MAXIMUM MU% / GP% HAVE PRINTED *"
              TO PRINT-REC
             WRITE PRINT-REC AFTER 1.
       END-400.
           IF WS-DATE-ACCEPT NOT = " "
             MOVE "** ONLY CREATE-DATES > " TO DATE-FILL
             MOVE WS-DATE-ACCEPT TO DL-DATE
             WRITE PRINT-REC FROM DATE-LINE AFTER 2.
           IF WS-DATE-PRICE NOT = " "
             MOVE "** ONLY PRICE-CHANGE < " TO DATE-FILL
             MOVE WS-DATE-PRICE TO DL-DATE
             WRITE PRINT-REC FROM DATE-LINE AFTER 2.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
           IF WS-PRINT-DISC = "Y"
               MOVE WS-PRINT-NORMAL TO PRINT-REC
               WRITE PRINT-REC.
       END-500.
           CLOSE STOCK-MASTER
                 PRINT-FILE.
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
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      *END-OF-JOB.
