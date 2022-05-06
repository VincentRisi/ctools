        IDENTIFICATION DIVISION.
        PROGRAM-ID. StHiSlRp.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
         Copy "SelectBmMaster".
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
           COPY ChlfdToolkit.
           
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
           03  FILLER           PIC X(255).
      *
       WORKING-STORAGE SECTION.
       77  WS-RANDOM-FILE       PIC X(35) VALUE
              "/ctools/spl/RandomHighSales".
       77  WS-RANDOM-FILE-ind   PIC X(35) VALUE
              "/ctools/spl/RandomHighSales.Ind".
       77  WS-HIGH-FILE       PIC X(35) VALUE
              "/ctools/spl/StockHighSales".
       77  WS-HIGH-FILE-ind   PIC X(35) VALUE
              "/ctools/spl/StockHighSales.Ind".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(15) VALUE " ".
       77  WS-TOP               PIC X(8) VALUE " ".
       77  WS-RAND-UNIT         PIC X VALUE " ".
       77  WS-PERIOD            PIC X VALUE " ".
       77  WS-INCLUDE-BM        PIC X VALUE " ".
       77  WS-DISP-ANS          PIC Z(7)9.
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-PRINTED           PIC S9(7) VALUE 0.
       77  WS-SALESQTY-YTD      PIC S9(7) VALUE 0.
       77  WS-SALESAMT-YTD      PIC S9(7)V99 VALUE 0.
       77  WS-LABOUR-QTY        PIC S9(7)V99 VALUE 0.
       77  WS-OVERHEAD-QTY      PIC S9(7)V99 VALUE 0.
       77  WS-LABOUR-VALUE      PIC S9(7)V99 VALUE 0.
       77  WS-OVERHEAD-VALUE    PIC S9(7)V99 VALUE 0.
       77  WS-STOCK-VALUE       PIC S9(7)V99 VALUE 0.
       77  WS-LABOUR-COST       PIC S9(7)V99 VALUE 0.
       77  WS-OVERHEAD-COST     PIC S9(7)V99 VALUE 0.
       77  TOT-LABOUR-VALUE     PIC S9(7)V99 VALUE 0.
       77  TOT-OVERHEAD-VALUE   PIC S9(7)V99 VALUE 0.
       77  TOT-STOCK-VALUE      PIC S9(7)V99 VALUE 0.
       77  WS-COST-YTD          PIC S9(7)V99 VALUE 0.
       77  WS-MARGIN-YTD        PIC S9(7)V99 VALUE 0.
       77  WS-PERC-YTD          PIC S9(4)V99.
       77  WS-VALUE             PIC S9(7)V99.
       77  WS-VALUE-MAX         PIC S9(7)V99.
       77  WS-MAX               PIC S9(7)V99.
       77  WS-NUMBER-TO-PRINT   PIC 9(7).
       77  WS-MONTH             PIC 99.
       77  WS-MONTH-ACCEPT      PIC XX.
       77  WS-RANDOM-WRITTEN    PIC X.
       77  TOT-SALESQTY         PIC S9(7) VALUE 0.
       77  TOT-ITEMS            PIC S9(7) VALUE 0.
       77  TOT-SALESAMT         PIC S9(7)V99 VALUE 0.
       77  TOT-COST             PIC S9(7)V99 VALUE 0.
       77  TOT-PERC             PIC S9(4)V99.
       77  TOT-SALESQTY-YTD     PIC S9(7) VALUE 0.
       77  TOT-SALESAMT-YTD     PIC S9(7)V99 VALUE 0.
       77  TOT-COST-YTD         PIC S9(7)V99 VALUE 0.
       77  TOT-VALUE            PIC S9(7)V99.
       77  TOT-VALUE-MAX        PIC S9(7)V99.
       77  TOT-VALUE-ONHAND     PIC S9(7)V99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-TOOLKIT-ST1   PIC 99.
       01  WS-RANDOM-STATUS.
           03  WS-RANDOM-ST1    PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(15) VALUE "S T O C K   A N".
           03  FILLER         PIC X(17) VALUE " A L Y S I S   B ".
           03  FILLER         PIC X(14) VALUE "Y    H I G H E".
           03  FILLER         PIC X(20) VALUE " S T   S A L E S".
           03  FILLER         PIC X(4) VALUE "TOP:".
           03  H1-TOP         PIC Z(6)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  H1-TYPE        PIC X(15).
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(62) VALUE ALL "*".
           03  FILLER         PIC X(41) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(6) VALUE "LINE#".
           03  FILLER         PIC X(16) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(44) VALUE "DESCRIPTION".
           03  FILLER         PIC X(5) VALUE "QTY".
           03  FILLER         PIC X(14) VALUE "%/TOTAL".
           03  FILLER         PIC X(7) VALUE "SALES".
           03  FILLER         PIC X(11) VALUE "%/TOTAL".
           03  FILLER         PIC X(19) VALUE "% M/U    VAL O/HND".
           03  FILLER         PIC X(15) VALUE "  VAL @MAX".
           03  H3-BM-LINE.
               05  FILLER     PIC X(50) VALUE
           "MATERIAL     LABOUR   LAB-%  OVERHEADS    OV-%".
       01  DETAIL-LINE.
           03  D-LINEQTY      PIC Z(4)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-STOCK        PIC X(16) VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(21) VALUE " ".
           03  D-SALESQTY     PIC Z(5)9-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PERCTOTQTY   PIC Z(3)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SALESAMT     PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PERCTOTSALES PIC Z(3)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PERC         PIC Z(3)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-VALUE        PIC Z(7)9.99.
           03  D-VALUE-MAX    PIC Z(7)9.99.
           03  D-BM-LINE.
              05  FILLER         PIC X(2) VALUE " ".
              05  D-BM-STOCK     PIC Z(7)9.99.
              05  D-BM-LABOUR    PIC Z(7)9.99.
              05  FILLER         PIC X(2) VALUE " ".
              05  D-BM-LAB-PERC  PIC Z(2)9.99.
              05  D-BM-OVERHEAD  PIC Z(7)9.99.
              05  FILLER         PIC X(2) VALUE " ".
              05  D-BM-OV-PERC   PIC Z(2)9.99.
       01  TOTAL-LINE.
           03  FILLER         PIC X(1) VALUE " ".
           03  T-LINE.
              05  FILLER      PIC X(1).
              05  T-R1DESC    PIC X(12).
              05  T-RANGE1    PIC X(15).
              05  T-R2DESC    PIC X(3).
              05  T-RANGE2    PIC X(14).
              05  T-LINE2.
                 07  T-DESC   PIC X(16).
           03  T-SALESQTY     PIC Z(7)9-.
      *     03  FILLER         PIC X(1) VALUE " ".
           03  T-SALESPERC    PIC Z(3)9.99-.
           03  T-SALESAMT     PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  T-SALESAMTPERC PIC Z(3)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  T-PERC         PIC Z(3)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  T-VALUE        PIC Z(7)9.99.
           03  T-VALUE-MAX    PIC Z(7)9.99.
           03  TOT-BM-LINE.
              05  FILLER           PIC X(2) VALUE " ".
              05  TOT-BM-STOCK     PIC Z(7)9.99 BLANK WHEN ZERO.
              05  TOT-BM-LABOUR    PIC Z(7)9.99 BLANK WHEN ZERO.
              05  FILLER           PIC X(2) VALUE " ".
              05  TOT-BM-LAB-PERC  PIC Z(2)9.99 BLANK WHEN ZERO.
              05  TOT-BM-OVERHEAD  PIC Z(7)9.99 BLANK WHEN ZERO.
              05  FILLER           PIC X(2) VALUE " ".
              05  TOT-BM-OV-PERC   PIC Z(2)9.99 BLANK WHEN ZERO.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 317 TO POS
           DISPLAY "** SALES REPORT BY HIGHEST SALES **" AT POS
           MOVE 417 TO POS
           DISPLAY "***********************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1010 TO POS
           DISPLAY "   FROM STOCK NUMBER          : [               ]"
                AT POS
           MOVE 1043 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

      *     ACCEPT WS-ANSWER1 AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-015.
           MOVE 1210 TO POS
           DISPLAY 
           "     TO STOCK NUMBER          : [               ]"
                AT POS
           MOVE 1243 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

      *     ACCEPT WS-ANSWER2 AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-010.
           IF WS-ANSWER2 = " "
              GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
           MOVE 1410 TO POS
           DISPLAY
           "ENTER TOP ? 50, 100, ETC                        :[        ]"
                AT POS
           ADD 50 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 8         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TOP.

      *     ACCEPT WS-TOP AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF WS-TOP = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
           MOVE WS-TOP TO ALPHA-RATE
           PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE < 0
              MOVE "YOU MUST ENTER A POSITIVE NUMBER, PLEASE RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO CONTROL-020.
           MOVE NUMERIC-RATE TO WS-DISP-ANS
           MOVE NUMERIC-RATE TO H1-TOP WS-NUMBER-TO-PRINT
           DISPLAY WS-DISP-ANS AT POS.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-021
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-021.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           MOVE 1610 TO POS
           DISPLAY
           "PRINT BASED ON: R=RAND AMT, U=UNIT AMT          :[ ]" AT POS
           ADD 50 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RAND-UNIT.

      *     ACCEPT WS-RAND-UNIT AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-020.
           IF WS-RAND-UNIT NOT = "R" AND NOT = "U"
              DISPLAY " " AT 3079 WITH BELL
              GO TO CONTROL-021.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-021.
       CONTROL-025.
           MOVE 1810 TO POS
           DISPLAY 
           "PRINT BASED ON: M=MTD, T=THIS YEAR, L=LAST YEAR :[ ]"
               AT POS
           ADD 50 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PERIOD.

      *     ACCEPT WS-PERIOD AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-021.
           IF WS-PERIOD NOT = "T" AND NOT = "L" AND NOT = "M"
              DISPLAY " " AT 3079 WITH BELL
              GO TO CONTROL-025.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-026
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-025.
       CONTROL-026.
           MOVE 2010 TO POS
           DISPLAY 
           "DO YOU WISH TO HAVE A B-MATERIAL BREAKDOWN      :[ ]"
               AT POS
           ADD 50 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-INCLUDE-BM.

      *     ACCEPT WS-INCLUDE-BM AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-025.
           IF WS-INCLUDE-BM NOT = "Y" AND NOT = "N"
              DISPLAY " " AT 3079 WITH BELL
              GO TO CONTROL-026.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-026.
       CONTROL-030.
           MOVE 2710 TO POS
           DISPLAY "Report Is Being Compiled, Please Be Patient."
               AT POS
           PERFORM OPEN-FILES
           PERFORM PRINT-ALL-ROUTINE.
           PERFORM READ-RANDOM-FILE.
           PERFORM PRINT-ROUTINE.
      *     PERFORM DELETE-TRANS.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       READ-RANDOM-RECORD SECTION.
       RRR-002.
            READ RANDOM-FILE
               INVALID KEY NEXT SENTENCE. 
            IF WS-RANDOM-ST1 = 23 OR 35 OR 49
               MOVE "NO SUCH RANDOM RECORD ON READ-23." TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-RANDOM-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE RANDOM-REC TO WS-MESSAGE
               PERFORM ERROR1-020
               GO TO RRR-999.
            IF WS-RANDOM-ST1 NOT = 0
               MOVE "RANDOM RECORD BUSY ON READ." TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-RANDOM-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE RANDOM-REC TO WS-MESSAGE
               PERFORM ERROR1-020
               GO TO RRR-002.
            MOVE 2510 TO POS
            DISPLAY "Random Stock Being Read:" AT POS
            ADD 25 TO POS
            DISPLAY RANDOM-STOCK AT POS.
            MOVE RANDOM-STOCK TO ST-STOCKNUMBER.
       RRR-999.
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
              MOVE 2555 TO POS
              DISPLAY "RANDOM WRITE, RETRY:" AT POS
              MOVE 2576 TO POS
              DISPLAY WS-MESSAGE AT POS
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
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
           MOVE RANDOM-STOCK TO HIGH-STOCK.
           WRITE HIGH-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "HIGH RECORD INVALID ON WRITE" TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       WRR-999.
            EXIT.
      *
       PRINT-ALL-ROUTINE SECTION.
       PRINT-000.
           MOVE 1 TO RANDOM-INDEX.
           MOVE 2310 TO POS
           DISPLAY "READING ALL STOCK ITEMS FOR TOTALS FIRST." AT POS.
           MOVE WS-ANSWER1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK RECORD BAD START,'ESC' TO EXIT."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
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
           IF ST-STOCKNUMBER < WS-ANSWER1
              GO TO PRINT-005.
           IF ST-STOCKNUMBER > WS-ANSWER2
              GO TO PRINT-999.

           MOVE 2510 TO POS
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
              ADD ST-SALESRANDSMTD TO TOT-SALESAMT
              ADD ST-SALESUNITMTD  TO TOT-SALESQTY
              ADD ST-SALESCOSTMTD  TO TOT-COST
              ADD 1                TO TOT-ITEMS.
           IF WS-PERIOD = "T"
              ADD ST-SALESRANDSYTD TO TOT-SALESAMT
              ADD ST-SALESUNITSYTD TO TOT-SALESQTY
              ADD ST-SALESCOSTYTD  TO TOT-COST
              ADD 1                TO TOT-ITEMS.
           IF WS-PERIOD = "L"
              ADD ST-SALESRANDSLAST TO TOT-SALESAMT
              ADD ST-SALESUNITSLAST TO TOT-SALESQTY
              ADD ST-SALESCOSTLAST  TO TOT-COST
              ADD 1                 TO TOT-ITEMS.
           COMPUTE TOT-VALUE-ONHAND = TOT-VALUE-ONHAND + 
               ((ST-QTYONHAND + ST-QTYONRESERVE) * ST-AVERAGECOST)
           COMPUTE TOT-VALUE-MAX = TOT-VALUE-MAX + 
               (ST-MAXIMUMLEVEL * ST-AVERAGECOST).
               
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
       PRINT-ROUTINE SECTION.
       PRR-000.
           CLOSE HIGH-FILE.
           PERFORM OPEN-037.
           
           PERFORM ERROR-020
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS.
           IF WS-INCLUDE-BM = "Y"
               PERFORM GET-LABOUR-OVERHEAD-COST.
               
           MOVE 2310 TO POS
           DISPLAY "READING STOCK BY HIGHEST VALUES.          " AT POS.
           
           MOVE 1   TO HIGH-NUMBER
           MOVE " " TO HIGH-STOCK.
           START HIGH-FILE KEY NOT < HIGH-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "BAD START ON HIGH, 'ESC' TO EXIT." TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              EXIT PROGRAM.
       PRR-005.
           READ HIGH-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-RANDOM-ST1 = 10
              GO TO PRR-999.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE 3010 TO POS
              DISPLAY "STOCK RECORD BUSY PRR-005 :" AT POS
              ADD 28 TO POS
              DISPLAY ST-STOCKNUMBER AT POS
              ADD 20 TO POS
              DISPLAY WS-RANDOM-ST1 AT POS
              ADD 5 TO POS
              PERFORM ERROR-010
              GO TO PRR-005.
           MOVE 2510 TO POS
           DISPLAY "STOCK NUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY HIGH-STOCK AT POS.
           
           MOVE HIGH-STOCK TO ST-STOCKNUMBER
           PERFORM READ-STOCK.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-020.
               
            IF WS-INCLUDE-BM = "Y"
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
            ELSE
               MOVE " " TO H3-BM-LINE
                            D-BM-LINE
                          TOT-BM-LINE.
                          
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE
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
            WRITE PRINT-REC FROM HEAD2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE 6 TO LINE-CNT.
       PRR-020.
           MOVE ST-STOCKNUMBER   TO D-STOCK
           MOVE ST-DESCRIPTION1  TO D-DESC1
           MOVE ST-DESCRIPTION2  TO D-DESC2.
           IF WS-INCLUDE-BM = "Y"
               PERFORM READ-TOOLKIT.
           
           IF WS-PERIOD NOT = "T"
              GO TO PRR-025.
              
           MOVE ST-SALESUNITSYTD TO D-SALESQTY
           COMPUTE TOT-PERC =
             (ST-SALESUNITSYTD / TOT-SALESQTY) * 100
           MOVE TOT-PERC         TO D-PERCTOTQTY
           MOVE ST-SALESRANDSYTD TO D-SALESAMT
           COMPUTE TOT-PERC =
              (ST-SALESRANDSYTD / TOT-SALESAMT) * 100.
           MOVE TOT-PERC         TO D-PERCTOTSALES
           COMPUTE WS-MARGIN-YTD ROUNDED =
               (ST-SALESRANDSYTD - ST-SALESCOSTYTD).
           COMPUTE WS-PERC-YTD ROUNDED =
               (WS-MARGIN-YTD / ST-SALESCOSTYTD) * 100.
           MOVE WS-PERC-YTD      TO D-PERC.
           
           IF WS-INCLUDE-BM = "N"
              GO TO PRR-050.
              
           COMPUTE WS-LABOUR-VALUE =
              WS-LABOUR-COST * WS-LABOUR-QTY * ST-SALESUNITSYTD
           COMPUTE WS-OVERHEAD-VALUE =
              WS-OVERHEAD-COST * WS-OVERHEAD-QTY * ST-SALESUNITSYTD
           COMPUTE WS-STOCK-VALUE = ST-SALESCOSTYTD - 
               (WS-LABOUR-VALUE + WS-OVERHEAD-VALUE).
           MOVE WS-STOCK-VALUE    TO D-BM-STOCK
           MOVE WS-LABOUR-VALUE   TO D-BM-LABOUR
           MOVE WS-OVERHEAD-VALUE TO D-BM-OVERHEAD.
           COMPUTE TOT-PERC =
             (WS-LABOUR-VALUE / WS-STOCK-VALUE) * 100
           MOVE TOT-PERC         TO D-BM-LAB-PERC.
           COMPUTE TOT-PERC =
             (WS-OVERHEAD-VALUE / WS-STOCK-VALUE) * 100
           MOVE TOT-PERC         TO D-BM-OV-PERC.
           GO TO PRR-050.
       PRR-025.
           IF WS-PERIOD = "L"
              GO TO PRR-030.
              
           MOVE ST-SALESUNITMTD TO D-SALESQTY
           COMPUTE TOT-PERC =
             (ST-SALESUNITMTD / TOT-SALESQTY) * 100
           MOVE TOT-PERC         TO D-PERCTOTQTY
           MOVE ST-SALESRANDSMTD TO D-SALESAMT
           COMPUTE TOT-PERC =
              (ST-SALESRANDSMTD / TOT-SALESAMT) * 100.
           MOVE TOT-PERC         TO D-PERCTOTSALES
           COMPUTE WS-MARGIN-YTD ROUNDED =
               (ST-SALESRANDSMTD - ST-SALESCOSTMTD).
           COMPUTE WS-PERC-YTD ROUNDED =
               (WS-MARGIN-YTD / ST-SALESCOSTMTD) * 100.
           MOVE WS-PERC-YTD      TO D-PERC.
           
           IF WS-INCLUDE-BM = "N"
              GO TO PRR-050.
              
           COMPUTE WS-LABOUR-VALUE =
              WS-LABOUR-COST * WS-LABOUR-QTY * ST-SALESUNITMTD
           COMPUTE WS-OVERHEAD-VALUE =
              WS-OVERHEAD-COST * WS-OVERHEAD-QTY * ST-SALESUNITMTD
           COMPUTE WS-STOCK-VALUE = ST-SALESCOSTMTD - 
               (WS-LABOUR-VALUE + WS-OVERHEAD-VALUE).
           MOVE WS-STOCK-VALUE    TO D-BM-STOCK
           MOVE WS-LABOUR-VALUE   TO D-BM-LABOUR
           MOVE WS-OVERHEAD-VALUE TO D-BM-OVERHEAD.
           COMPUTE TOT-PERC =
             (WS-LABOUR-VALUE / WS-STOCK-VALUE) * 100
           MOVE TOT-PERC         TO D-BM-LAB-PERC.
           COMPUTE TOT-PERC =
             (WS-OVERHEAD-VALUE / WS-STOCK-VALUE) * 100
           MOVE TOT-PERC         TO D-BM-OV-PERC.
           GO TO PRR-050.
       PRR-030.
           MOVE ST-SALESUNITSLAST TO D-SALESQTY
           COMPUTE TOT-PERC =
             (ST-SALESUNITSLAST / TOT-SALESQTY) * 100
           MOVE TOT-PERC         TO D-PERCTOTQTY
           MOVE ST-SALESRANDSLAST TO D-SALESAMT
           COMPUTE TOT-PERC =
              (ST-SALESRANDSLAST / TOT-SALESAMT) * 100.
           MOVE TOT-PERC         TO D-PERCTOTSALES
           COMPUTE WS-MARGIN-YTD ROUNDED =
               (ST-SALESRANDSLAST - ST-SALESCOSTLAST).
           COMPUTE WS-PERC-YTD ROUNDED =
               (WS-MARGIN-YTD / ST-SALESCOSTLAST) * 100.
           MOVE WS-PERC-YTD      TO D-PERC.
           
           IF WS-INCLUDE-BM = "N"
              GO TO PRR-050.
              
           COMPUTE WS-LABOUR-VALUE =
              WS-LABOUR-COST * WS-LABOUR-QTY * ST-SALESUNITSLAST
           COMPUTE WS-OVERHEAD-VALUE =
              WS-OVERHEAD-COST * WS-OVERHEAD-QTY * ST-SALESUNITSLAST
           COMPUTE WS-STOCK-VALUE = ST-SALESCOSTLAST - 
               (WS-LABOUR-VALUE + WS-OVERHEAD-VALUE).
           MOVE WS-STOCK-VALUE    TO D-BM-STOCK
           MOVE WS-LABOUR-VALUE   TO D-BM-LABOUR
           MOVE WS-OVERHEAD-VALUE TO D-BM-OVERHEAD.
           COMPUTE TOT-PERC =
             (WS-LABOUR-VALUE / WS-STOCK-VALUE) * 100
           MOVE TOT-PERC         TO D-BM-LAB-PERC.
           COMPUTE TOT-PERC =
             (WS-OVERHEAD-VALUE / WS-STOCK-VALUE) * 100
           MOVE TOT-PERC         TO D-BM-OV-PERC.
       PRR-050.
           COMPUTE WS-VALUE =
               (ST-QTYONHAND + ST-QTYONRESERVE) * ST-AVERAGECOST
           MOVE WS-VALUE         TO D-VALUE
           COMPUTE WS-VALUE-MAX =
               (ST-MAXIMUMLEVEL * ST-AVERAGECOST)
           MOVE WS-VALUE-MAX     TO D-VALUE-MAX
           ADD 1                 TO WS-PRINTED
           MOVE WS-PRINTED       TO D-LINEQTY.
           IF WS-INCLUDE-BM = "Y"
               ADD WS-STOCK-VALUE    TO TOT-STOCK-VALUE
               ADD WS-LABOUR-VALUE   TO TOT-LABOUR-VALUE
               ADD WS-OVERHEAD-VALUE TO TOT-OVERHEAD-VALUE.

           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC.
           MOVE 0                TO WS-MARGIN-YTD
                                    WS-PERC-YTD
                                    WS-STOCK-VALUE
                                    WS-LABOUR-VALUE
                                    WS-OVERHEAD-VALUE.
           IF WS-PERIOD = "M"
              ADD ST-SALESUNITMTD   TO TOT-SALESQTY-YTD
              ADD ST-SALESRANDSMTD  TO TOT-SALESAMT-YTD
              ADD ST-SALESCOSTMTD   TO TOT-COST-YTD.
           IF WS-PERIOD = "T"
              ADD ST-SALESUNITSYTD  TO TOT-SALESQTY-YTD
              ADD ST-SALESRANDSYTD  TO TOT-SALESAMT-YTD
              ADD ST-SALESCOSTYTD   TO TOT-COST-YTD.
           IF WS-PERIOD = "L"
              ADD ST-SALESUNITSLAST  TO TOT-SALESQTY-YTD
              ADD ST-SALESRANDSLAST  TO TOT-SALESAMT-YTD
              ADD ST-SALESCOSTLAST   TO TOT-COST-YTD.
           ADD WS-VALUE              TO TOT-VALUE
           ADD WS-VALUE-MAX          TO WS-MAX.
           ADD 1 TO LINE-CNT.
           IF WS-PRINTED < WS-NUMBER-TO-PRINT
              GO TO PRR-005.
       PRR-999.
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
           MOVE 2310 TO POS
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
           DISPLAY RANDOM-STOCK AT POS
           ADD 20 TO POS
           DISPLAY HIGH-NUMBER AT POS.
           
           PERFORM WRITE-HIGH-RECORD.
           GO TO RRF-005.
       RRF-999.
           EXIT.
      *
       READ-TOOLKIT SECTION.
       RKIT-001.
            MOVE ST-STOCKNUMBER TO TO-TOOLKIT-NUMBER
            MOVE "ZZZ LABOUR1"  TO TO-COMPONENT-NUMBER
            START TOOLKITS KEY NOT < TO-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-LABOUR-QTY
               GO TO RKIT-500.
       RKIT-002.
            READ TOOLKITS
               INVALID KEY NEXT SENTENCE.
            IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE 0 TO WS-LABOUR-QTY
               GO TO RKIT-500.
            IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "KIT BUSY ON LABOUR READ, 'ESC' TO RE-TRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO RKIT-002.
            MOVE TO-QUANTITY TO WS-LABOUR-QTY.
       RKIT-500.
            MOVE ST-STOCKNUMBER  TO TO-TOOLKIT-NUMBER
            MOVE "ZZZ OVERHEADS" TO TO-COMPONENT-NUMBER
            START TOOLKITS KEY NOT < TO-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-OVERHEAD-QTY
               GO TO RKIT-999.
       RKIT-510.
            READ TOOLKITS
               INVALID KEY NEXT SENTENCE.
            IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE 0 TO WS-OVERHEAD-QTY
               GO TO RKIT-999.
            IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "KIT BUSY ON OVER-HEADS READ, 'ESC' TO RE-TRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO RKIT-510.
            MOVE TO-QUANTITY TO WS-OVERHEAD-QTY.
       RKIT-999.
            EXIT.
      *
       GET-LABOUR-OVERHEAD-COST SECTION.
       GLOC-001.
           MOVE " " TO ST-STOCKNUMBER.
       GLOC-005.
           IF ST-STOCKNUMBER = " "
              MOVE "ZZZ LABOUR1" TO ST-STOCKNUMBER
           ELSE
              MOVE "ZZZ OVERHEADS" TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       GLOC-015.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
            IF ST-STOCKNUMBER = "ZZZ LABOUR1"
              GO TO GLOC-999
            ELSE
              MOVE WS-STOCK-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO GLOC-005.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK RECORD BUSY ON READ NEXT" TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE ST-STOCKNUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO GLOC-015.
           IF ST-STOCKNUMBER = "ZZZ LABOUR1"
              MOVE ST-AVERAGECOST TO WS-LABOUR-COST.
           IF ST-STOCKNUMBER = "ZZZ OVERHEADS"
              MOVE ST-AVERAGECOST TO WS-OVERHEAD-COST.
           IF ST-STOCKNUMBER = "ZZZ LABOUR1"
              GO TO GLOC-005.
       GLOC-999.
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
           CALL "&OPENFILE" USING  F-ERROR1
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE-MM.
               
           CALL "&DELETEFILE" USING F-ERROR1
                                    F-FH.
              
           PERFORM CDS-005.
           Move Ws-Random-file-IND To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE Ws-Random-file-Ind TO F-FILENAME
           MOVE Sub-1              TO F-CBFILENAME.
           CALL "&OPENFILE" USING  F-ERROR1
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE-MM.
           CALL "&DELETEFILE" USING F-ERROR1
                                    F-FH.
       DST-020.
           CLOSE HIGH-FILE.
           PERFORM CDS-005.
           Move Ws-HIGH-file To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE WS-HIGH-file    TO F-FILENAME
           MOVE SUB-1           TO F-CBFILENAME.
           CALL "&OPENFILE" USING  F-ERROR1
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE-MM.
           CALL "&DELETEFILE" USING F-ERROR1
                                    F-FH.
              
           PERFORM CDS-005.
           Move Ws-HIGH-file-IND To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE Ws-HIGH-file-Ind TO F-FILENAME
           MOVE Sub-1        TO F-CBFILENAME.
           CALL "&OPENFILE" USING  F-ERROR1
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE-MM.
           CALL "&DELETEFILE" USING F-ERROR1
                                    F-FH.
       DST-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-030.
           MOVE 3010 TO POS
           DISPLAY "Opening Files......" AT POS.
       OPEN-031.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-030.
       OPEN-032.
           OPEN I-O TOOLKITS.
           IF WS-TOOLKIT-ST1 NOT = 0 
              MOVE 0 TO WS-TOOLKIT-ST1
              MOVE "KIT FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-032.
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
           "RANDOM FILE OPEN OUTPUT AT ANOTHER COMPUTER, 'ESC' TO EXIT."
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
             "HIGH FILE OPEN OUTPUT AT ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
             GO TO OPEN-038.
       OPEN-040.
           MOVE Ws-Co-Name TO CO-NAME
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM ERROR-020.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 60
               PERFORM PRR-010.
           MOVE "   ITEMS PRINTED" TO T-DESC
           MOVE TOT-SALESQTY-YTD   TO T-SALESQTY
           MOVE TOT-SALESAMT-YTD   TO T-SALESAMT
           COMPUTE TOT-PERC = (TOT-SALESQTY-YTD / TOT-SALESQTY) * 100
           MOVE TOT-PERC           TO T-SALESPERC
           COMPUTE TOT-PERC = (TOT-SALESAMT-YTD / TOT-SALESAMT) * 100
           MOVE TOT-PERC           TO T-SALESAMTPERC
           COMPUTE TOT-PERC =
            ((TOT-SALESAMT-YTD - TOT-COST-YTD) / TOT-COST-YTD) * 100
           MOVE TOT-PERC           TO T-PERC
           MOVE TOT-VALUE          TO T-VALUE
           MOVE WS-MAX             TO T-VALUE-MAX
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
           WRITE PRINT-REC FROM TOTAL-LINE.

           MOVE "RANGE FROM :"     TO T-R1DESC
           MOVE WS-ANSWER1         TO T-RANGE1
           MOVE "TO:"              TO T-R2DESC
           MOVE WS-ANSWER2         TO T-RANGE2
           MOVE "TOTALS FOR RANGE" TO T-DESC
           MOVE TOT-SALESQTY       TO T-SALESQTY
           MOVE TOT-SALESAMT       TO T-SALESAMT
           COMPUTE TOT-PERC = (TOT-SALESQTY / TOT-SALESQTY) * 100
           MOVE TOT-PERC           TO T-SALESPERC
           COMPUTE TOT-PERC = (TOT-SALESAMT / TOT-SALESAMT) * 100
           MOVE TOT-PERC           TO T-SALESAMTPERC
           COMPUTE TOT-PERC =
            ((TOT-SALESAMT - TOT-COST) / TOT-COST) * 100
           MOVE TOT-PERC           TO T-PERC
           MOVE TOT-VALUE-ONHAND   TO T-VALUE
           MOVE TOT-VALUE-MAX      TO T-VALUE-MAX.
           
           IF WS-INCLUDE-BM = "Y"
               MOVE TOT-STOCK-VALUE    TO TOT-BM-STOCK
               MOVE TOT-LABOUR-VALUE   TO TOT-BM-LABOUR
               MOVE TOT-OVERHEAD-VALUE TO TOT-BM-OVERHEAD
           ELSE
               MOVE " "                TO TOT-BM-LINE.
               
           COMPUTE TOT-PERC =
             (TOT-LABOUR-VALUE / TOT-STOCK-VALUE) * 100
           MOVE TOT-PERC         TO TOT-BM-LAB-PERC
           COMPUTE TOT-PERC =
             (WS-OVERHEAD-VALUE / TOT-STOCK-VALUE) * 100
           MOVE TOT-PERC         TO TOT-BM-OV-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE.

           MOVE " "                TO TOTAL-LINE
           IF WS-PERIOD = "T"
              MOVE "PRINT BASED ON THIS YEARS FIGURES." TO T-LINE.
           IF WS-PERIOD = "M"
              MOVE "PRINT BASED ON THIS MONTHS FIGURES." TO T-LINE.
           IF WS-PERIOD = "L"
              MOVE "PRINT BASED ON LAST YEARS FIGURES." TO T-LINE.
           MOVE "TOTAL ITEMS READ" TO T-DESC
           MOVE TOT-ITEMS          TO T-SALESQTY
               
           WRITE PRINT-REC FROM TOTAL-LINE.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           IF WS-INCLUDE-BM = "Y"
               MOVE WS-PRINT-NORMAL TO PRINT-REC
               WRITE PRINT-REC.
       END-500.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE STOCK-MASTER
                  TOOLKITS.
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
