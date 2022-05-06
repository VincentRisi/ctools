        IDENTIFICATION DIVISION.
        PROGRAM-ID. StSGmRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStMaster1".
          Copy "SelectStMaster2".
          Copy "SelectStMaster3".
          Copy "SelectStMaster4".
          Copy "SelectSlParameter".
          Copy "SelectCoDataName".
          
           SELECT COMPANY-MENU ASSIGN TO "CoCompany"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-MENU-STATUS
               RECORD KEY IS PTY-KEY.
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStockBr1.
           COPY ChlfdStockBr2.
           COPY ChlfdStockBr3.
           COPY ChlfdStockBr4.
           Copy ChlfdDataName.
           COPY ChlfdParam.
           COPY ChlfdCompany.
     *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(15) VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-SALESQTY          PIC S9(6) VALUE 0.
       77  WS-SALESAMT          PIC S9(7)V99 VALUE 0.
       77  WS-COST              PIC S9(7)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(7)V99 VALUE 0.
       77  WS-PERC              PIC S9(4)V99.
       77  WS-SALESQTY-YTD      PIC S9(6) VALUE 0.
       77  WS-SALESAMT-YTD      PIC S9(8)V99 VALUE 0.
       77  WS-COST-YTD          PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-YTD        PIC S9(8)V99 VALUE 0.
       77  WS-PERC-YTD          PIC S9(4)V99.
       77  WS-SALES-DIS         PIC Z(7)9.99-.
       77  WS-SALESQTY-LAST     PIC S9(6) VALUE 0.
       77  WS-SALESAMT-LAST     PIC S9(8)V99 VALUE 0.
       77  WS-COST-LAST         PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-LAST       PIC S9(8)V99 VALUE 0.
       77  WS-PERC-LAST         PIC S9(4)V99.
       77  WSI-SALESQTY         PIC S9(6) VALUE 0.
       77  WSI-SALESAMT         PIC S9(7)V99 VALUE 0.
       77  WSI-COST             PIC S9(7)V99 VALUE 0.
       77  WSI-MARGIN           PIC S9(7)V99 VALUE 0.
       77  WSI-PERC             PIC S9(4)V99.
       77  WSI-SALESQTY-YTD     PIC S9(6) VALUE 0.
       77  WSI-SALESAMT-YTD     PIC S9(8)V99 VALUE 0.
       77  WSI-COST-YTD         PIC S9(8)V99 VALUE 0.
       77  WSI-MARGIN-YTD        PIC S9(8)V99 VALUE 0.
       77  WSI-PERC-YTD          PIC S9(4)V99.
       77  WSI-SALESQTY-LAST    PIC S9(6) VALUE 0.
       77  WSI-SALESAMT-LAST    PIC S9(8)V99 VALUE 0.
       77  WSI-COST-LAST        PIC S9(8)V99 VALUE 0.
       77  WSI-MARGIN-LAST       PIC S9(8)V99 VALUE 0.
       77  WSI-PERC-LAST         PIC S9(4)V99.
       77  TOT-SALESQTY         PIC S9(6) VALUE 0.
       77  TOT-SALESAMT         PIC S9(8)V99 VALUE 0.
       77  TOT-COST             PIC S9(8)V99 VALUE 0.
       77  TOT-MARGIN           PIC S9(8)V99 VALUE 0.
       77  TOT-SALESQTY-LAST    PIC S9(6) VALUE 0.
       77  TOT-SALESAMT-LAST    PIC S9(8)V99 VALUE 0.
       77  TOT-COST-LAST        PIC S9(8)V99 VALUE 0.
       77  TOT-MARGIN-LAST      PIC S9(8)V99 VALUE 0.
       77  TOT-PERC-LAST        PIC S9(4)V99.
       77  TOT-PERC             PIC S9(4)V99.
       77  TOT-SALESQTY-YTD     PIC S9(6) VALUE 0.
       77  TOT-SALESAMT-YTD     PIC S9(8)V99 VALUE 0.
       77  TOT-COST-YTD         PIC S9(8)V99 VALUE 0.
       77  TOT-MARGIN-YTD       PIC S9(8)V99 VALUE 0.
       77  TOT-PERC-YTD         PIC S9(4)V99.
       77  WS-END               PIC X VALUE " ".
       77  WS-QUES-MU-GP-PERC   PIC X VALUE " ".
       77  WS-BARE-STOCK        PIC X(20) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-STOCK1-STATUS.
           03  WS-STOCK1-ST1      PIC 99.
       01  WS-STOCK2-STATUS.
           03  WS-STOCK2-ST1      PIC 99.
       01  WS-STOCK3-STATUS.
           03  WS-STOCK3-ST1      PIC 99.
       01  WS-STOCK4-STATUS.
           03  WS-STOCK4-ST1      PIC 99.
       01  WS-MENU-STATUS.
           03  WS-MENU-ST1        PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-DATA-STATUS.
           03  WS-DATA-ST1        PIC 99.
       01 COMPANIES-NAME-LIST.
         02  COMPANIES-LIST OCCURS 20.
           03  LIST-NAME        PIC X(40).
           03  LIST-NUMBER      PIC 99.
           03  LIST-VOL-DIR     PIC X(25).
       01  BRANCH-NAME-INFO.
         02  WS-BRANCH-INFO OCCURS 10.
           03  WS-BRANCH-TYPE          PIC 9.
           03  WS-BRANCH-NUMBER        PIC 9.
           03  WS-BRANCH-NOT-THERE     PIC X(2).
           03  WS-BRANCH-NAME          PIC X(3).
           03  WS-BRANCH-STOCK-VOL-DIR PIC X(40).
           03  WS-BRANCH-STOCK         PIC X(15).
           03  WS-BRANCH-QTYSOLDMTD    PIC 9(6).
           03  WS-BRANCH-QTYSOLDYTD    PIC 9(6).
           03  WS-BRANCH-QTYSOLDLYR    PIC 9(6).
           03  WS-BRANCH-RANDSOLDMTD   PIC 9(6).
           03  WS-BRANCH-RANDSOLDYTD   PIC 9(6).
           03  WS-BRANCH-RANDSOLDLYR   PIC 9(6).
           03  WS-BRANCH-RANDCOSTMTD   PIC 9(6).
           03  WS-BRANCH-RANDCOSTYTD   PIC 9(6).
           03  WS-BRANCH-RANDCOSTLYR   PIC 9(6).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(15) VALUE "S A L E S   A N".
           03  FILLER         PIC X(17) VALUE " D   G R O S S   ".
           03  FILLER         PIC X(14) VALUE "M A R G I N   ".
           03  FILLER         PIC X(41) VALUE "A N A L Y S I S".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(8) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(61) VALUE ALL "*".
           03  FILLER         PIC X(42) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(89) VALUE " ".
           03  FILLER         PIC X(14) VALUE "PERIOD / YEAR ".
           03  FILLER         PIC X(27) VALUE "/ LAST YEAR TO DATE".
       01  HEAD4.
           03  FILLER         PIC X(18) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(50) VALUE "DESCRIPTION".
           03  FILLER         PIC X(17) VALUE "SALES QTY".
           03  FILLER         PIC X(13) VALUE "SALES".
           03  FILLER         PIC X(11) VALUE "COSTS".
           03  FILLER         PIC X(12) VALUE "MARGIN".
           03  FILLER         PIC X(11) VALUE "%".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(18) VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(30) VALUE " ".
           03  D-SALESQTY     PIC Z(5)9-.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-SALESAMT     PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-COSTSAMT     PIC Z(7)9.99-.
           03  D-MARGIN       PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PERC         PIC Z(3)9.99-.
           03  FILLER         PIC X(9) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(57) VALUE " ".
           03  T-CATEGORY     PIC X(10) VALUE " ".
           03  T-SALESQTY     PIC Z(6)9-.
           03  FILLER         PIC X(4) VALUE " ".
           03  T-SALESAMT     PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  T-COST         PIC Z(7)9.99-.
           03  T-MARGIN       PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  T-PERC         PIC Z(3)9.99-.
           03  FILLER         PIC X(9) VALUE " ".
       01  DETAILG-LINE.
           03  FILLER         PIC X(60) VALUE " ".
           03  DG-BRANCH-NAME PIC X(8) VALUE " ".
           03  DG-SALESQTY    PIC Z(5)9-.
           03  FILLER         PIC X(4) VALUE " ".
           03  DG-SALESAMT    PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  DG-COSTSAMT    PIC Z(7)9.99-.
           03  DG-MARGIN      PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  DG-PERC        PIC Z(3)9.99-.
           03  FILLER         PIC X(9) VALUE " ".
       01  TOTALG-LINE.
           03  FILLER         PIC X(57) VALUE " ".
           03  TG-CATEGORY    PIC X(10) VALUE " ".
           03  TG-SALESQTY    PIC Z(6)9-.
           03  FILLER         PIC X(4) VALUE " ".
           03  TG-SALESAMT    PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  TG-COST        PIC Z(7)9.99-.
           03  TG-MARGIN      PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  TG-PERC        PIC Z(3)9.99-.
           03  FILLER         PIC X(9) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 317 TO POS.
           DISPLAY "** SALES AND GROSS MARGIN REPORT **" AT POS.
           MOVE 417 TO POS.
           DISPLAY "***********************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1010 TO POS.
           DISPLAY "   FROM STOCK NUMBER : [               ]" AT POS.
           MOVE 1034 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 33        TO CDA-COL.
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
           MOVE 1210 TO POS.
           DISPLAY "     TO STOCK NUMBER : [               ]" AT POS.
           MOVE 1234 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 33        TO CDA-COL.
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
           MOVE 1410 TO POS.
           DISPLAY "CATEGORY TOTALS ONLY : [ ]" AT POS.
           MOVE 1510 TO POS.
           DISPLAY "Y / N, G=GROUP TOTALS OR P=GROUP BY PART NUMBER."
            AT POS.
           MOVE 1434 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 33        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER3.

      *     ACCEPT WS-ANSWER3 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF WS-ANSWER3 NOT = "Y" AND NOT = "N" 
                     AND NOT = "G" AND NOT = "P"
               MOVE " " TO WS-ANSWER3
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO CONTROL-022
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-022.
           MOVE 2810 TO POS.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           DISPLAY "Report Is Being Compiled, Please Be Patient."
              AT POS.
           PERFORM OPEN-FILES.
           
           IF WS-ANSWER3 = "G" OR = "P"
               GO TO CONTROL-040.
           
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
       CONTROL-040.
           Move 3010 to Pos
           Display "Reading Next-Company........" At Pos.
           Perform Read-Next-Company.

           Move 3010 to Pos
           Display "Getting Branch Stock file Names.. " at Pos.
           Perform Check-Branch-Data-Names
           Perform Error-020.
           
           Move 3020 to Pos
           Display "Opening Branch Files.............." At Pos.
           Perform Open-Branch-Stock.

           PERFORM ERROR-020.
           
           PERFORM PRINT-ROUTINE-GROUP.
           PERFORM END-OFF.
        CONTROL-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE WS-ANSWER1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       PRR-005.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
              PERFORM PRR-025
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
               GO TO PRR-005.
           IF ST-STOCKNUMBER < WS-ANSWER1
              GO TO PRR-005.
           IF ST-STOCKNUMBER > WS-ANSWER2
              PERFORM PRR-025
              GO TO PRR-999.

           MOVE 2510 TO POS
           DISPLAY "STOCKNUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

           IF WS-STORE = "   "
              MOVE ST-CATEGORY TO WS-STORE.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-020.
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
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC AFTER 1
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD4 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC AFTER 1
            MOVE 8 TO LINE-CNT.
       PRR-020.
           IF ST-CATEGORY NOT = WS-STORE
               PERFORM PRR-025.
           ADD ST-SALESUNITMTD   TO WS-SALESQTY
                                    TOT-SALESQTY.
           ADD ST-SALESUNITSYTD  TO WS-SALESQTY-YTD
                                    TOT-SALESQTY-YTD.
           ADD ST-SALESUNITSLAST TO WS-SALESQTY-LAST
                                    TOT-SALESQTY-LAST.
           ADD ST-SALESRANDSMTD  TO WS-SALESAMT
                                    TOT-SALESAMT.
           ADD ST-SALESRANDSYTD  TO WS-SALESAMT-YTD
                                    TOT-SALESAMT-YTD.
           ADD ST-SALESRANDSLAST TO WS-SALESAMT-LAST
                                    TOT-SALESAMT-LAST.
           ADD ST-SALESCOSTMTD   TO WS-COST
                                    TOT-COST.
           ADD ST-SALESCOSTYTD   TO WS-COST-YTD
                                    TOT-COST-YTD.
           ADD ST-SALESCOSTLAST  TO WS-COST-LAST
                                    TOT-COST-LAST.
       PRR-022.
           IF WS-ANSWER3 = "Y"
               GO TO PRR-005.
           MOVE ST-STOCKNUMBER   TO D-STOCK.
           MOVE ST-DESCRIPTION1  TO D-DESC1.
           MOVE ST-DESCRIPTION2  TO D-DESC2.
           MOVE ST-SALESUNITMTD  TO D-SALESQTY.
           MOVE ST-SALESRANDSMTD TO D-SALESAMT.
           MOVE ST-SALESCOSTMTD  TO D-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST-SALESRANDSMTD - ST-SALESCOSTMTD).
           MOVE WS-MARGIN        TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST-SALESCOSTMTD) * 100.
           MOVE WS-PERC          TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.

           MOVE " "              TO D-STOCK
                                    D-DESC1
                                    D-DESC2.
           MOVE 0                TO WS-MARGIN
                                    WS-PERC.
           MOVE ST-SALESUNITSYTD TO D-SALESQTY.
           MOVE ST-SALESRANDSYTD TO D-SALESAMT.
           MOVE ST-SALESCOSTYTD  TO D-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST-SALESRANDSYTD - ST-SALESCOSTYTD).
           MOVE WS-MARGIN        TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST-SALESCOSTYTD) * 100.
           MOVE WS-PERC          TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           MOVE 0                 TO WS-MARGIN
                                     WS-PERC.
           MOVE ST-SALESUNITSLAST TO D-SALESQTY.
           MOVE ST-SALESRANDSLAST TO D-SALESAMT.
           MOVE ST-SALESCOSTLAST  TO D-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST-SALESRANDSLAST - ST-SALESCOSTLAST).
           MOVE WS-MARGIN        TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST-SALESCOSTLAST) * 100.
           MOVE WS-PERC          TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           ADD 4 TO LINE-CNT.
           MOVE 0                TO WS-MARGIN
                                    WS-PERC.
           
           GO TO PRR-005.
       PRR-025.
           MOVE WS-STORE    TO T-CATEGORY.
           MOVE WS-SALESQTY TO T-SALESQTY.
           MOVE WS-SALESAMT TO T-SALESAMT.
           MOVE WS-COST     TO T-COST.
           COMPUTE WS-MARGIN = (WS-SALESAMT - WS-COST).
           MOVE WS-MARGIN   TO T-MARGIN.
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / WS-COST) * 100.
           MOVE WS-PERC     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.

           MOVE WS-SALESQTY-YTD TO T-SALESQTY.
           MOVE WS-SALESAMT-YTD TO T-SALESAMT.
           MOVE WS-COST-YTD     TO T-COST.
           COMPUTE WS-MARGIN-YTD = (WS-SALESAMT-YTD - WS-COST-YTD).
           MOVE WS-MARGIN-YTD   TO T-MARGIN.
           COMPUTE WS-PERC-YTD ROUNDED =
               (WS-MARGIN-YTD / WS-COST-YTD) * 100.
           MOVE WS-PERC-YTD     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           MOVE WS-SALESQTY-LAST TO T-SALESQTY.
           MOVE WS-SALESAMT-LAST TO T-SALESAMT.
           MOVE WS-COST-LAST     TO T-COST.
           COMPUTE WS-MARGIN-LAST = (WS-SALESAMT-LAST - WS-COST-LAST).
           MOVE WS-MARGIN-LAST   TO T-MARGIN.
           COMPUTE WS-PERC-LAST ROUNDED =
               (WS-MARGIN-LAST / WS-COST-LAST) * 100.
           MOVE WS-PERC-LAST     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           WRITE PRINT-REC AFTER 1.

           ADD 4 TO LINE-CNT.
           MOVE 1610 TO POS.
           DISPLAY "Category: " AT POS.
           MOVE 1620 TO POS.
           DISPLAY WS-STORE AT POS.
           MOVE 1710 TO POS.
           DISPLAY "Sales For The Month : R" AT POS.
           MOVE 1733 TO POS.
           MOVE WS-SALESAMT TO WS-SALES-DIS.
           DISPLAY WS-SALES-DIS AT POS.
           MOVE 0 TO WS-SALESQTY
                     WS-SALESAMT
                     WS-COST
                     WS-MARGIN
                     WS-PERC
                     WS-SALESQTY-YTD
                     WS-SALESAMT-YTD
                     WS-COST-YTD
                     WS-MARGIN-YTD
                     WS-PERC-YTD
                     WS-SALESQTY-LAST
                     WS-SALESAMT-LAST
                     WS-COST-LAST
                     WS-MARGIN-LAST
                     WS-PERC-LAST.
           IF WS-STORE = "XRN"
             IF WS-ANSWER3 = "Y"
               PERFORM END-000.
           MOVE ST-CATEGORY TO WS-STORE.  
       PRR-999.
           EXIT.
      *
       PRINT-ROUTINE-GROUP SECTION.
       PRG-000.
           MOVE WS-ANSWER1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       PRG-005.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
              PERFORM PRG-055
              GO TO PRG-999.
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
               GO TO PRG-005.
           IF ST-STOCKNUMBER < WS-ANSWER1
              GO TO PRG-005.
           IF ST-STOCKNUMBER > WS-ANSWER2
              PERFORM PRG-055
              GO TO PRG-999.

           MOVE 2510 TO POS
           DISPLAY "STOCKNUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

           IF WS-STORE = "   "
              MOVE ST-CATEGORY TO WS-STORE.
       PRG-010.
            IF LINE-CNT < 60
               GO TO PRG-020.
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
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC AFTER 1
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD4 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC AFTER 1
            MOVE 8 TO LINE-CNT.
       PRG-020.
           IF ST-CATEGORY NOT = WS-STORE
               PERFORM PRG-055.
   
           PERFORM PRG-031.

           ADD ST-SALESUNITMTD   TO WS-SALESQTY
                                   WSI-SALESQTY
                                   TOT-SALESQTY.
           ADD ST-SALESUNITSYTD  TO WS-SALESQTY-YTD
                                   WSI-SALESQTY-YTD
                                   TOT-SALESQTY-YTD.
           ADD ST-SALESUNITSLAST TO WS-SALESQTY-LAST
                                   WSI-SALESQTY-LAST
                                   TOT-SALESQTY-LAST.
           ADD ST-SALESRANDSMTD  TO WS-SALESAMT
                                   WSI-SALESAMT
                                   TOT-SALESAMT.
           ADD ST-SALESRANDSYTD  TO WS-SALESAMT-YTD
                                   WSI-SALESAMT-YTD
                                   TOT-SALESAMT-YTD.
           ADD ST-SALESRANDSLAST TO WS-SALESAMT-LAST
                                   WSI-SALESAMT-LAST
                                   TOT-SALESAMT-LAST.
           ADD ST-SALESCOSTMTD   TO WS-COST
                                   WSI-COST
                                   TOT-COST.
           ADD ST-SALESCOSTYTD   TO WS-COST-YTD
                                   WSI-COST-YTD
                                   TOT-COST-YTD.
           ADD ST-SALESCOSTLAST  TO WS-COST-LAST
                                   WSI-COST-LAST
                                   TOT-COST-LAST.
               
           PERFORM READ-BRANCH-STOCK.

           MOVE 1  TO SUB-1.               
           ADD ST1-SALESUNITMTD   TO WS-SALESQTY
                                    WSI-SALESQTY
                                    TOT-SALESQTY.
           ADD ST1-SALESUNITSYTD  TO WS-SALESQTY-YTD
                                    WSI-SALESQTY-YTD
                                    TOT-SALESQTY-YTD.
           ADD ST1-SALESUNITSLAST TO WS-SALESQTY-LAST
                                    WSI-SALESQTY-LAST
                                    TOT-SALESQTY-LAST.
           ADD ST1-SALESRANDSMTD  TO WS-SALESAMT
                                    WSI-SALESAMT
                                    TOT-SALESAMT.
           ADD ST1-SALESRANDSYTD  TO WS-SALESAMT-YTD
                                    WSI-SALESAMT-YTD
                                    TOT-SALESAMT-YTD.
           ADD ST1-SALESRANDSLAST TO WS-SALESAMT-LAST
                                    WSI-SALESAMT-LAST
                                    TOT-SALESAMT-LAST.
           ADD ST1-SALESCOSTMTD   TO WS-COST
                                    WSI-COST
                                    TOT-COST.
           ADD ST1-SALESCOSTYTD   TO WS-COST-YTD
                                    WSI-COST-YTD
                                    TOT-COST-YTD.
           ADD ST1-SALESCOSTLAST  TO WS-COST-LAST
                                    WSI-COST-LAST
                                    TOT-COST-LAST.

           MOVE 2  TO SUB-1.               
           ADD ST2-SALESUNITMTD   TO WS-SALESQTY
                                    WSI-SALESQTY
                                    TOT-SALESQTY.
           ADD ST2-SALESUNITSYTD  TO WS-SALESQTY-YTD
                                    WSI-SALESQTY-YTD
                                    TOT-SALESQTY-YTD.
           ADD ST2-SALESUNITSLAST TO WS-SALESQTY-LAST
                                    WSI-SALESQTY-LAST
                                    TOT-SALESQTY-LAST.
           ADD ST2-SALESRANDSMTD  TO WS-SALESAMT
                                    WSI-SALESAMT
                                    TOT-SALESAMT.
           ADD ST2-SALESRANDSYTD  TO WS-SALESAMT-YTD
                                    WSI-SALESAMT-YTD
                                    TOT-SALESAMT-YTD.
           ADD ST2-SALESRANDSLAST TO WS-SALESAMT-LAST
                                    WSI-SALESAMT-LAST
                                    TOT-SALESAMT-LAST.
           ADD ST2-SALESCOSTMTD   TO WS-COST
                                    WSI-COST
                                    TOT-COST.
           ADD ST2-SALESCOSTYTD   TO WS-COST-YTD
                                    WSI-COST-YTD
                                    TOT-COST-YTD.
           ADD ST2-SALESCOSTLAST  TO WS-COST-LAST
                                    WSI-COST-LAST
                                    TOT-COST-LAST.
               
           MOVE 3  TO SUB-1.               
           ADD ST3-SALESUNITMTD   TO WS-SALESQTY
                                    WSI-SALESQTY
                                    TOT-SALESQTY.
           ADD ST3-SALESUNITSYTD  TO WS-SALESQTY-YTD
                                    WSI-SALESQTY-YTD
                                    TOT-SALESQTY-YTD.
           ADD ST3-SALESUNITSLAST TO WS-SALESQTY-LAST
                                    WSI-SALESQTY-LAST
                                    TOT-SALESQTY-LAST.
           ADD ST3-SALESRANDSMTD  TO WS-SALESAMT
                                    WSI-SALESAMT
                                    TOT-SALESAMT.
           ADD ST3-SALESRANDSYTD  TO WS-SALESAMT-YTD
                                    WSI-SALESAMT-YTD
                                    TOT-SALESAMT-YTD.
           ADD ST3-SALESRANDSLAST TO WS-SALESAMT-LAST
                                    WSI-SALESAMT-LAST
                                    TOT-SALESAMT-LAST.
           ADD ST3-SALESCOSTMTD   TO WS-COST
                                    WSI-COST
                                    TOT-COST.
           ADD ST3-SALESCOSTYTD   TO WS-COST-YTD
                                    WSI-COST-YTD
                                    TOT-COST-YTD.
           ADD ST3-SALESCOSTLAST  TO WS-COST-LAST
                                    WSI-COST-LAST
                                    TOT-COST-LAST.
               
            IF WS-STOCK4 = " "
                GO TO PRG-022.

           MOVE 4  TO SUB-1.               
           ADD ST4-SALESUNITMTD   TO WS-SALESQTY
                                    WSI-SALESQTY
                                    TOT-SALESQTY.
           ADD ST4-SALESUNITSYTD  TO WS-SALESQTY-YTD
                                    WSI-SALESQTY-YTD
                                    TOT-SALESQTY-YTD.
           ADD ST4-SALESUNITSLAST TO WS-SALESQTY-LAST
                                    WSI-SALESQTY-LAST
                                    TOT-SALESQTY-LAST.
           ADD ST4-SALESRANDSMTD  TO WS-SALESAMT
                                    WSI-SALESAMT
                                    TOT-SALESAMT.
           ADD ST4-SALESRANDSYTD  TO WS-SALESAMT-YTD
                                    WSI-SALESAMT-YTD
                                    TOT-SALESAMT-YTD.
           ADD ST4-SALESRANDSLAST TO WS-SALESAMT-LAST
                                    WSI-SALESAMT-LAST
                                    TOT-SALESAMT-LAST.
           ADD ST4-SALESCOSTMTD   TO WS-COST
                                    WSI-COST
                                    TOT-COST.
           ADD ST4-SALESCOSTYTD   TO WS-COST-YTD
                                    WSI-COST-YTD
                                    TOT-COST-YTD.
           ADD ST4-SALESCOSTLAST  TO WS-COST-LAST
                                    WSI-COST-LAST
                                    TOT-COST-LAST.
       PRG-022.
           IF WS-ANSWER3 = "G"
               GO TO PRG-005.

           MOVE ST-STOCKNUMBER   TO D-STOCK.
           MOVE ST-DESCRIPTION1  TO D-DESC1.
           MOVE ST-DESCRIPTION2  TO D-DESC2.
           MOVE ST-SALESUNITMTD  TO D-SALESQTY.
           MOVE ST-SALESRANDSMTD TO D-SALESAMT.
           MOVE ST-SALESCOSTMTD  TO D-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST-SALESRANDSMTD - ST-SALESCOSTMTD).
           MOVE WS-MARGIN        TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST-SALESCOSTMTD) * 100.
           MOVE WS-PERC          TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.

           MOVE " "              TO D-STOCK
                                    D-DESC1
                                    D-DESC2.
           MOVE 0                TO WS-MARGIN
                                    WS-PERC.
           MOVE ST-SALESUNITSYTD TO D-SALESQTY.
           MOVE ST-SALESRANDSYTD TO D-SALESAMT.
           MOVE ST-SALESCOSTYTD  TO D-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST-SALESRANDSYTD - ST-SALESCOSTYTD).
           MOVE WS-MARGIN        TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST-SALESCOSTYTD) * 100.
           MOVE WS-PERC          TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           MOVE 0                 TO WS-MARGIN
                                     WS-PERC.
           MOVE ST-SALESUNITSLAST TO D-SALESQTY.
           MOVE ST-SALESRANDSLAST TO D-SALESAMT.
           MOVE ST-SALESCOSTLAST  TO D-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST-SALESRANDSLAST - ST-SALESCOSTLAST).
           MOVE WS-MARGIN        TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST-SALESCOSTLAST) * 100.
           MOVE WS-PERC          TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           ADD 4 TO LINE-CNT.
           MOVE 0                TO WS-MARGIN
                                    WS-PERC.
           MOVE 1 TO SUB-1.           
       PRG-023.
           MOVE WS-BRANCH-NAME (SUB-1) TO DG-BRANCH-NAME.
           MOVE ST1-SALESUNITMTD  TO DG-SALESQTY.
           MOVE ST1-SALESRANDSMTD TO DG-SALESAMT.
           MOVE ST1-SALESCOSTMTD  TO DG-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST1-SALESRANDSMTD - ST1-SALESCOSTMTD).
           MOVE WS-MARGIN         TO DG-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST1-SALESCOSTMTD) * 100.
           MOVE WS-PERC          TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC DG-BRANCH-NAME.

           MOVE 0                 TO WS-MARGIN
                                     WS-PERC.
           MOVE ST1-SALESUNITSYTD TO DG-SALESQTY.
           MOVE ST1-SALESRANDSYTD TO DG-SALESAMT.
           MOVE ST1-SALESCOSTYTD  TO DG-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST1-SALESRANDSYTD - ST1-SALESCOSTYTD).
           MOVE WS-MARGIN         TO DG-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST1-SALESCOSTYTD) * 100.
           MOVE WS-PERC           TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           MOVE 0                  TO WS-MARGIN
                                      WS-PERC.
           MOVE ST1-SALESUNITSLAST TO DG-SALESQTY.
           MOVE ST1-SALESRANDSLAST TO DG-SALESAMT.
           MOVE ST1-SALESCOSTLAST  TO DG-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST1-SALESRANDSLAST - ST1-SALESCOSTLAST).
           MOVE WS-MARGIN        TO DG-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST1-SALESCOSTLAST) * 100.
           MOVE WS-PERC          TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           ADD 4 TO LINE-CNT.
           MOVE 0                TO WS-MARGIN
                                    WS-PERC.
           MOVE 2 TO SUB-1.           
       PRG-024.
           MOVE WS-BRANCH-NAME (SUB-1) TO DG-BRANCH-NAME.
           MOVE ST2-SALESUNITMTD  TO DG-SALESQTY.
           MOVE ST2-SALESRANDSMTD TO DG-SALESAMT.
           MOVE ST2-SALESCOSTMTD  TO DG-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST2-SALESRANDSMTD - ST2-SALESCOSTMTD).
           MOVE WS-MARGIN        TO DG-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST2-SALESCOSTMTD) * 100.
           MOVE WS-PERC          TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC DG-BRANCH-NAME.

           MOVE 0                TO WS-MARGIN
                                    WS-PERC.
           MOVE ST2-SALESUNITSYTD TO DG-SALESQTY.
           MOVE ST2-SALESRANDSYTD TO DG-SALESAMT.
           MOVE ST2-SALESCOSTYTD  TO DG-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST2-SALESRANDSYTD - ST2-SALESCOSTYTD).
           MOVE WS-MARGIN        TO DG-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST2-SALESCOSTYTD) * 100.
           MOVE WS-PERC          TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           MOVE 0                 TO WS-MARGIN
                                     WS-PERC.
           MOVE ST2-SALESUNITSLAST TO DG-SALESQTY.
           MOVE ST2-SALESRANDSLAST TO DG-SALESAMT.
           MOVE ST2-SALESCOSTLAST  TO DG-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST2-SALESRANDSLAST - ST2-SALESCOSTLAST).
           MOVE WS-MARGIN        TO DG-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST2-SALESCOSTLAST) * 100.
           MOVE WS-PERC          TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           ADD 4 TO LINE-CNT.
           MOVE 0                TO WS-MARGIN
                                    WS-PERC.
           MOVE 3 TO SUB-1.           
       PRG-025.
           MOVE WS-BRANCH-NAME (SUB-1) TO DG-BRANCH-NAME.
           MOVE ST3-SALESUNITMTD  TO DG-SALESQTY.
           MOVE ST3-SALESRANDSMTD TO DG-SALESAMT.
           MOVE ST3-SALESCOSTMTD  TO DG-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST3-SALESRANDSMTD - ST3-SALESCOSTMTD).
           MOVE WS-MARGIN        TO DG-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST3-SALESCOSTMTD) * 100.
           MOVE WS-PERC          TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC DG-BRANCH-NAME.

           MOVE 0                TO WS-MARGIN
                                    WS-PERC.
           MOVE ST3-SALESUNITSYTD TO DG-SALESQTY.
           MOVE ST3-SALESRANDSYTD TO DG-SALESAMT.
           MOVE ST3-SALESCOSTYTD  TO DG-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST3-SALESRANDSYTD - ST3-SALESCOSTYTD).
           MOVE WS-MARGIN        TO DG-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST3-SALESCOSTYTD) * 100.
           MOVE WS-PERC          TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           MOVE 0                 TO WS-MARGIN
                                     WS-PERC.
           MOVE ST3-SALESUNITSLAST TO DG-SALESQTY.
           MOVE ST3-SALESRANDSLAST TO DG-SALESAMT.
           MOVE ST3-SALESCOSTLAST  TO DG-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST3-SALESRANDSLAST - ST3-SALESCOSTLAST).
           MOVE WS-MARGIN        TO DG-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST3-SALESCOSTLAST) * 100.
           MOVE WS-PERC          TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           ADD 4 TO LINE-CNT.
           MOVE 0                TO WS-MARGIN
                                    WS-PERC.
           MOVE 4 TO SUB-1.           
       PRG-026.
           IF WS-BRANCH-NAME (SUB-1) NOT > " "
                GO TO PRG-030.
           MOVE WS-BRANCH-NAME (SUB-1) TO DG-BRANCH-NAME.
           MOVE ST4-SALESUNITMTD  TO DG-SALESQTY.
           MOVE ST4-SALESRANDSMTD TO DG-SALESAMT.
           MOVE ST4-SALESCOSTMTD  TO DG-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST4-SALESRANDSMTD - ST4-SALESCOSTMTD).
           MOVE WS-MARGIN        TO DG-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST4-SALESCOSTMTD) * 100.
           MOVE WS-PERC          TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC.

           MOVE 0                TO WS-MARGIN
                                    WS-PERC.
           MOVE ST4-SALESUNITSYTD TO DG-SALESQTY.
           MOVE ST4-SALESRANDSYTD TO DG-SALESAMT.
           MOVE ST4-SALESCOSTYTD  TO DG-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST4-SALESRANDSYTD - ST4-SALESCOSTYTD).
           MOVE WS-MARGIN        TO DG-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST4-SALESCOSTYTD) * 100.
           MOVE WS-PERC          TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           MOVE 0                 TO WS-MARGIN
                                     WS-PERC.
           MOVE ST4-SALESUNITSLAST TO DG-SALESQTY.
           MOVE ST4-SALESRANDSLAST TO DG-SALESAMT.
           MOVE ST4-SALESCOSTLAST  TO DG-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST4-SALESRANDSLAST - ST4-SALESCOSTLAST).
           MOVE WS-MARGIN        TO DG-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST4-SALESCOSTLAST) * 100.
           MOVE WS-PERC          TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           ADD 4 TO LINE-CNT.
           MOVE 0                TO WS-MARGIN.
       PRG-030.
           MOVE "TOTAL"      TO DG-BRANCH-NAME.
           MOVE WSI-SALESQTY TO DG-SALESQTY.
           MOVE WSI-SALESAMT TO DG-SALESAMT.
           MOVE WSI-COST     TO DG-COSTSAMT.
           COMPUTE WS-MARGIN = (WSI-SALESAMT - WSI-COST).
           MOVE WS-MARGIN    TO DG-MARGIN.
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / WSI-COST) * 100.
           MOVE WS-PERC      TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAILG-LINE.

           MOVE WSI-SALESQTY-YTD TO DG-SALESQTY.
           MOVE WSI-SALESAMT-YTD TO DG-SALESAMT.
           MOVE WSI-COST-YTD     TO DG-COSTSAMT.
           COMPUTE WS-MARGIN-YTD = (WSI-SALESAMT-YTD - WSI-COST-YTD).
           MOVE WS-MARGIN-YTD   TO DG-MARGIN.
           COMPUTE WS-PERC-YTD ROUNDED =
               (WS-MARGIN-YTD / WSI-COST-YTD) * 100.
           MOVE WS-PERC-YTD     TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAILG-LINE.
           MOVE WSI-SALESQTY-LAST TO DG-SALESQTY.
           MOVE WSI-SALESAMT-LAST TO DG-SALESAMT.
           MOVE WSI-COST-LAST     TO DG-COSTSAMT.
           COMPUTE WS-MARGIN-LAST = (WSI-SALESAMT-LAST - WSI-COST-LAST).
           MOVE WS-MARGIN-LAST   TO DG-MARGIN.
           COMPUTE WS-PERC-LAST ROUNDED =
               (WS-MARGIN-LAST / WSI-COST-LAST) * 100.
           MOVE WS-PERC-LAST     TO DG-PERC.
           WRITE PRINT-REC FROM DETAILG-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAILG-LINE.
           WRITE PRINT-REC AFTER 1.

           ADD 4 TO LINE-CNT.
       PRG-031.
           MOVE 0 TO WSI-SALESQTY
                     WSI-SALESAMT
                     WSI-COST
                     WSI-MARGIN
                     WSI-PERC.
           MOVE 0 TO WSI-SALESQTY-YTD
                     WSI-SALESAMT-YTD
                     WSI-COST-YTD
                     WSI-MARGIN-YTD
                     WSI-PERC-YTD.
           MOVE 0 TO WSI-SALESQTY-LAST
                     WSI-SALESAMT-LAST
                     WSI-COST-LAST
                     WSI-MARGIN-LAST
                     WSI-PERC-LAST.
       PRG-032.
           GO TO PRG-005.
       PRG-055.
           MOVE WS-STORE    TO T-CATEGORY.
           MOVE WS-SALESQTY TO T-SALESQTY.
           MOVE WS-SALESAMT TO T-SALESAMT.
           MOVE WS-COST     TO T-COST.
           COMPUTE WS-MARGIN = (WS-SALESAMT - WS-COST).
           MOVE WS-MARGIN   TO T-MARGIN.
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / WS-COST) * 100.
           MOVE WS-PERC     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.

           MOVE WS-SALESQTY-YTD TO T-SALESQTY.
           MOVE WS-SALESAMT-YTD TO T-SALESAMT.
           MOVE WS-COST-YTD     TO T-COST.
           COMPUTE WS-MARGIN-YTD = (WS-SALESAMT-YTD - WS-COST-YTD).
           MOVE WS-MARGIN-YTD   TO T-MARGIN.
           COMPUTE WS-PERC-YTD ROUNDED =
               (WS-MARGIN-YTD / WS-COST-YTD) * 100.
           MOVE WS-PERC-YTD     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           MOVE WS-SALESQTY-LAST TO T-SALESQTY.
           MOVE WS-SALESAMT-LAST TO T-SALESAMT.
           MOVE WS-COST-LAST     TO T-COST.
           COMPUTE WS-MARGIN-LAST = (WS-SALESAMT-LAST - WS-COST-LAST).
           MOVE WS-MARGIN-LAST   TO T-MARGIN.
           COMPUTE WS-PERC-LAST ROUNDED =
               (WS-MARGIN-LAST / WS-COST-LAST) * 100.
           MOVE WS-PERC-LAST     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           WRITE PRINT-REC AFTER 1.

           ADD 4 TO LINE-CNT.

           MOVE 1610 TO POS.
           DISPLAY "Category: " AT POS.
           MOVE 1620 TO POS.
           DISPLAY WS-STORE AT POS.
           MOVE 1710 TO POS.
           DISPLAY "Sales For The Month : R" AT POS.
           MOVE 1733 TO POS.
           MOVE WS-SALESAMT TO WS-SALES-DIS.
           DISPLAY WS-SALES-DIS AT POS.

           MOVE 0 TO WS-SALESQTY
                     WS-SALESAMT
                     WS-COST
                     WS-MARGIN
                     WS-PERC.
           MOVE 0 TO WS-SALESQTY-YTD
                     WS-SALESAMT-YTD
                     WS-COST-YTD
                     WS-MARGIN-YTD
                     WS-PERC-YTD.
           MOVE 0 TO WS-SALESQTY-LAST
                     WS-SALESAMT-LAST
                     WS-COST-LAST
                     WS-MARGIN-LAST
                     WS-PERC-LAST.
           IF WS-STORE = "XRN"
             IF WS-ANSWER3 = "Y"
               PERFORM END-000.
           MOVE ST-CATEGORY TO WS-STORE.  
       PRG-999.
           EXIT.
      *
       READ-BRANCH-STOCK SECTION.
       RBRST-100.
           PERFORM CLEAR-BRANCH-AMOUNTS.
           IF WS-END = "Y"
               GO TO RBRST-900.
            IF WS-STOCK1 = " "
                GO TO RBRST-200.
            MOVE ST-STOCKNUMBER TO ST1-STOCKNUMBER
            START STOCK-MASTER1 KEY NOT < ST1-KEY
                INVALID KEY NEXT SENTENCE.
            MOVE "Reading 1st Branch Stock File....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 1 TO SUB-1.
            MOVE ST-STOCKNUMBER TO WS-BRANCH-STOCK (SUB-1).
       RBRST-110.
            READ STOCK-MASTER1
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK1-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN        " TO WS-BRANCH-STOCK (SUB-1)
                MOVE 0         TO WS-BRANCH-QTYSOLDMTD (SUB-1)
                                  WS-BRANCH-QTYSOLDYTD (SUB-1)
                                  WS-BRANCH-QTYSOLDLYR (SUB-1)
                                  WS-BRANCH-RANDSOLDMTD (SUB-1)
                                  WS-BRANCH-RANDSOLDYTD (SUB-1)
                                  WS-BRANCH-RANDSOLDLYR (SUB-1)
                                  WS-BRANCH-RANDCOSTMTD (SUB-1)
                                  WS-BRANCH-RANDCOSTYTD (SUB-1)
                                  ST1-SALESUNITMTD
                                  ST1-SALESUNITSYTD
                                  ST1-SALESUNITSLAST
                                  ST1-SALESRANDSMTD
                                  ST1-SALESRANDSYTD
                                  ST1-SALESRANDSLAST
                                  ST1-SALESCOSTMTD
                                  ST1-SALESCOSTYTD
                                  ST1-SALESCOSTLAST
                GO TO RBRST-200.
             IF WS-STOCK1-ST1 NOT = 0
                MOVE "STOCK RECORD1 BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK1-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK1-ST1
                GO TO RBRST-110.
           MOVE ST1-SALESUNITMTD   TO WS-BRANCH-QTYSOLDMTD (SUB-1)
           MOVE ST1-SALESUNITSYTD  TO WS-BRANCH-QTYSOLDYTD (SUB-1)
           MOVE ST1-SALESUNITSLAST TO WS-BRANCH-QTYSOLDLYR (SUB-1)
           MOVE ST1-SALESRANDSMTD  TO WS-BRANCH-RANDSOLDMTD (SUB-1)
           MOVE ST1-SALESRANDSYTD  TO WS-BRANCH-RANDSOLDYTD (SUB-1)
           MOVE ST1-SALESRANDSLAST TO WS-BRANCH-RANDSOLDLYR (SUB-1)
           MOVE ST1-SALESCOSTMTD   TO WS-BRANCH-RANDCOSTMTD (SUB-1)
           MOVE ST1-SALESCOSTYTD   TO WS-BRANCH-RANDCOSTYTD (SUB-1)
           MOVE ST1-SALESCOSTLAST  TO WS-BRANCH-RANDCOSTLYR (SUB-1).
       RBRST-200.
            IF WS-STOCK2 = " "
                GO TO RBRST-300.
            MOVE ST-STOCKNUMBER TO ST2-STOCKNUMBER
            START STOCK-MASTER2 KEY NOT < ST2-KEY
                INVALID KEY NEXT SENTENCE.
            MOVE "Reading 2nd Branch Stock File....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 2 TO SUB-1.
       RBRST-210.
            READ STOCK-MASTER2
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK2-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN        " TO WS-BRANCH-STOCK (SUB-1)
                MOVE 0         TO WS-BRANCH-QTYSOLDMTD (SUB-1)
                                  WS-BRANCH-QTYSOLDYTD (SUB-1)
                                  WS-BRANCH-QTYSOLDLYR (SUB-1)
                                  WS-BRANCH-RANDSOLDMTD (SUB-1)
                                  WS-BRANCH-RANDSOLDYTD (SUB-1)
                                  WS-BRANCH-RANDSOLDLYR (SUB-1)
                                  WS-BRANCH-RANDCOSTMTD (SUB-1)
                                  WS-BRANCH-RANDCOSTYTD (SUB-1)
                                  ST2-SALESUNITMTD
                                  ST2-SALESUNITSYTD
                                  ST2-SALESUNITSLAST
                                  ST2-SALESRANDSMTD
                                  ST2-SALESRANDSYTD
                                  ST2-SALESRANDSLAST
                                  ST2-SALESCOSTMTD
                                  ST2-SALESCOSTYTD
                                  ST2-SALESCOSTLAST

                GO TO RBRST-300.
             IF WS-STOCK2-ST1 NOT = 0
                MOVE "STOCK RECORD2 BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK2-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK2-ST1
                GO TO RBRST-210.
           MOVE ST2-SALESUNITMTD   TO WS-BRANCH-QTYSOLDMTD (SUB-1)
           MOVE ST2-SALESUNITSYTD  TO WS-BRANCH-QTYSOLDYTD (SUB-1)
           MOVE ST2-SALESUNITSLAST TO WS-BRANCH-QTYSOLDLYR (SUB-1)
           MOVE ST2-SALESRANDSMTD  TO WS-BRANCH-RANDSOLDMTD (SUB-1)
           MOVE ST2-SALESRANDSYTD  TO WS-BRANCH-RANDSOLDYTD (SUB-1)
           MOVE ST2-SALESRANDSLAST TO WS-BRANCH-RANDSOLDLYR (SUB-1)
           MOVE ST2-SALESCOSTMTD   TO WS-BRANCH-RANDCOSTMTD (SUB-1)
           MOVE ST2-SALESCOSTYTD   TO WS-BRANCH-RANDCOSTYTD (SUB-1)
           MOVE ST2-SALESCOSTLAST  TO WS-BRANCH-RANDCOSTLYR (SUB-1).
       RBRST-300.
            IF WS-STOCK3 = " "
                GO TO RBRST-400.
            MOVE ST-STOCKNUMBER TO ST3-STOCKNUMBER
            START STOCK-MASTER3 KEY NOT < ST3-KEY
                INVALID KEY NEXT SENTENCE.
            MOVE "Reading 3rd Branch Stock File....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 3 TO SUB-1.
       RBRST-310.
            READ STOCK-MASTER3
                INVALID KEY NEXT SENTENCE.
             IF WS-STOCK3-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN        " TO WS-BRANCH-STOCK (SUB-1)
                MOVE 0         TO WS-BRANCH-QTYSOLDMTD (SUB-1)
                                  WS-BRANCH-QTYSOLDYTD (SUB-1)
                                  WS-BRANCH-QTYSOLDLYR (SUB-1)
                                  WS-BRANCH-RANDSOLDMTD (SUB-1)
                                  WS-BRANCH-RANDSOLDYTD (SUB-1)
                                  WS-BRANCH-RANDSOLDLYR (SUB-1)
                                  WS-BRANCH-RANDCOSTMTD (SUB-1)
                                  WS-BRANCH-RANDCOSTYTD (SUB-1)
                                  ST3-SALESUNITMTD
                                  ST3-SALESUNITSYTD
                                  ST3-SALESUNITSLAST
                                  ST3-SALESRANDSMTD
                                  ST3-SALESRANDSYTD
                                  ST3-SALESRANDSLAST
                                  ST3-SALESCOSTMTD
                                  ST3-SALESCOSTYTD
                                  ST3-SALESCOSTLAST
                GO TO RBRST-400.
             IF WS-STOCK3-ST1 NOT = 0
                MOVE "STOCK RECORD3 BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK3-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK3-ST1
                GO TO RBRST-310.
           MOVE ST3-SALESUNITMTD   TO WS-BRANCH-QTYSOLDMTD (SUB-1)
           MOVE ST3-SALESUNITSYTD  TO WS-BRANCH-QTYSOLDYTD (SUB-1)
           MOVE ST3-SALESUNITSLAST TO WS-BRANCH-QTYSOLDLYR (SUB-1)
           MOVE ST3-SALESRANDSMTD  TO WS-BRANCH-RANDSOLDMTD (SUB-1)
           MOVE ST3-SALESRANDSYTD  TO WS-BRANCH-RANDSOLDYTD (SUB-1)
           MOVE ST3-SALESRANDSLAST TO WS-BRANCH-RANDSOLDLYR (SUB-1)
           MOVE ST3-SALESCOSTMTD   TO WS-BRANCH-RANDCOSTMTD (SUB-1)
           MOVE ST3-SALESCOSTYTD   TO WS-BRANCH-RANDCOSTYTD (SUB-1)
           MOVE ST3-SALESCOSTLAST  TO WS-BRANCH-RANDCOSTLYR (SUB-1).
       RBRST-400.
            IF WS-STOCK4 = " "
                GO TO RBRST-900.
            MOVE ST-STOCKNUMBER TO ST4-STOCKNUMBER
            START STOCK-MASTER4 KEY NOT < ST4-KEY
                INVALID KEY NEXT SENTENCE.
            MOVE "Reading 4th Branch Stock File....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 4 TO SUB-1.
       RBRST-410.
            READ STOCK-MASTER4
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK4-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN        " TO WS-BRANCH-STOCK (SUB-1)
                MOVE 0         TO WS-BRANCH-QTYSOLDMTD (SUB-1)
                                  WS-BRANCH-QTYSOLDYTD (SUB-1)
                                  WS-BRANCH-QTYSOLDLYR (SUB-1)
                                  WS-BRANCH-RANDSOLDMTD (SUB-1)
                                  WS-BRANCH-RANDSOLDYTD (SUB-1)
                                  WS-BRANCH-RANDSOLDLYR (SUB-1)
                                  WS-BRANCH-RANDCOSTMTD (SUB-1)
                                  WS-BRANCH-RANDCOSTYTD (SUB-1)
                                  ST4-SALESUNITMTD
                                  ST4-SALESUNITSYTD
                                  ST4-SALESUNITSLAST
                                  ST4-SALESRANDSMTD
                                  ST4-SALESRANDSYTD
                                  ST4-SALESRANDSLAST
                                  ST4-SALESCOSTMTD
                                  ST4-SALESCOSTYTD
                                  ST4-SALESCOSTLAST
                GO TO RBRST-900.
             IF WS-STOCK4-ST1 NOT = 0
                MOVE "STOCK RECORD4 BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK4-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK4-ST1
                GO TO RBRST-410.
           MOVE ST4-SALESUNITMTD   TO WS-BRANCH-QTYSOLDMTD (SUB-1)
           MOVE ST4-SALESUNITSYTD  TO WS-BRANCH-QTYSOLDYTD (SUB-1)
           MOVE ST4-SALESUNITSLAST TO WS-BRANCH-QTYSOLDLYR (SUB-1)
           MOVE ST4-SALESRANDSMTD  TO WS-BRANCH-RANDSOLDMTD (SUB-1)
           MOVE ST4-SALESRANDSYTD  TO WS-BRANCH-RANDSOLDYTD (SUB-1)
           MOVE ST4-SALESRANDSLAST TO WS-BRANCH-RANDSOLDLYR (SUB-1)
           MOVE ST4-SALESCOSTMTD   TO WS-BRANCH-RANDCOSTMTD (SUB-1)
           MOVE ST4-SALESCOSTYTD   TO WS-BRANCH-RANDCOSTYTD (SUB-1)
           MOVE ST4-SALESCOSTLAST  TO WS-BRANCH-RANDCOSTLYR (SUB-1).
       RBRST-900.
             PERFORM ERROR4-020.
       RBRST-999.
             EXIT.
      *
       CLEAR-BRANCH-AMOUNTS SECTION.
       CBA-010.
            MOVE 1 TO SUB-1.
       CBA-020.
            MOVE 0 TO  WS-BRANCH-QTYSOLDMTD (SUB-1)
                       WS-BRANCH-QTYSOLDYTD (SUB-1)
                       WS-BRANCH-QTYSOLDLYR (SUB-1)
                       WS-BRANCH-RANDSOLDMTD (SUB-1)
                       WS-BRANCH-RANDSOLDYTD (SUB-1)
                       WS-BRANCH-RANDSOLDLYR (SUB-1)
                       WS-BRANCH-RANDCOSTMTD (SUB-1)
                       WS-BRANCH-RANDCOSTYTD (SUB-1)
                       WS-BRANCH-RANDCOSTLYR (SUB-1).
           IF SUB-1 < 10
               ADD 1 TO SUB-1
               GO TO CBA-020.
       CBA-999.
            EXIT.
      *
       READ-NEXT-COMPANY SECTION.
       RNC-005.
           OPEN I-O COMPANY-MENU.
           IF WS-MENU-ST1 NOT = 0
               MOVE "COMPANY FILE BUSY ON OPEN, GOING TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-MENU-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-MENU-ST1
               GO TO RNC-005.
           MOVE 1 TO PTY-NUMBER.
           START COMPANY-MENU KEY NOT < PTY-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-MENU-ST1 NOT = 0
               GO TO RNC-900.
           MOVE 1 TO SUB-20.
       RNC-010.
           READ COMPANY-MENU NEXT
               AT END NEXT SENTENCE.
           IF WS-MENU-ST1 = 10
               GO TO RNC-900.
           IF WS-MENU-ST1 NOT = 0
               MOVE "COMPANY FILE BUSY ON READ-NEXT, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-MENU-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-MENU-ST1
               GO TO RNC-010.
           MOVE PTY-VOL-DIR TO LIST-VOL-DIR (SUB-20)
           MOVE PTY-NUMBER  TO LIST-NUMBER (SUB-20)
           MOVE PTY-CO-NAME TO LIST-NAME (SUB-20).
           
           IF SUB-20 < 20
              ADD 1 TO SUB-20
              GO TO RNC-010.
       RNC-900.
           CLOSE COMPANY-MENU.
       RNC-999.
           EXIT.
      *
       START-PARAM-RECORD SECTION.
       STR-000.
           MOVE 1         TO PA-RECORD.
           MOVE 7         TO PA-TYPE.
           START PARAMETER-FILE KEY NOT < PA-KEY
              INVALID KEY NEXT SENTENCE.
       STR-999.
             EXIT.
      *
       READ-PARAM-NEXT SECTION.
       RNX-001.
           MOVE 0   TO SUB-1.
           MOVE 0 TO WS-SLPARAMETER-ST1.
           PERFORM START-PARAM-RECORD.
       RNX-005.
           READ PARAMETER-FILE NEXT
             AT END
               GO TO RNX-999.
           IF PA-TYPE NOT = 7
               GO TO RNX-999.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "PARAM FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RNX-999.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "GOING TO RESTART PARAM FILE, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               PERFORM START-PARAM-RECORD
               GO TO RNX-005.
           ADD 1 TO SUB-1.
       RNX-010.
           MOVE PARAMETER-REC TO WS-BRANCH-INFO (SUB-1).
           
           GO TO RNX-005.
       RNX-999.
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
       RP-999.
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
       CHECK-BRANCH-DATA-NAMES SECTION.
       CBDN-005.
          MOVE 1 TO SUB-20.
          PERFORM STRIP-STOCKNAME.
       CBDN-006.
          MOVE " " TO ALPHA-RATE
                      DATA-RATE.
          MOVE 0   TO SUB-1.
          MOVE WS-BRANCH-NUMBER (SUB-20) TO SUB-25.
       CBDN-010.
          MOVE LIST-VOL-DIR (SUB-25) TO ALPHA-RATE.
       CBDN-015.
          ADD 1 TO SUB-1.
          IF SUB-1 NOT > 60
           IF AL-RATE (SUB-1) NOT = " "
            GO TO CBDN-015.
       CBDN-020.
          MOVE WS-BARE-STOCK TO DATA-RATE.
          MOVE 1             TO SUB-2.
       CBDN-025.
          MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
          ADD 1 TO SUB-1 SUB-2.
          IF DAT-RATE (SUB-2) NOT = " "
           IF SUB-1 NOT > 60
             GO TO CBDN-025.
       CBDN-030.
          MOVE ALPHA-RATE TO WS-BRANCH-STOCK-VOL-DIR (SUB-20).
          ADD 1 TO SUB-20.
          IF WS-BRANCH-NUMBER (SUB-20) > 0
              GO TO CBDN-006.
       CBDN-040.
          MOVE WS-BRANCH-STOCK-VOL-DIR (1) TO WS-STOCK1.
          MOVE WS-BRANCH-STOCK-VOL-DIR (2) TO WS-STOCK2.
          MOVE WS-BRANCH-STOCK-VOL-DIR (3) TO WS-STOCK3.
          MOVE WS-BRANCH-STOCK-VOL-DIR (4) TO WS-STOCK4.
       CBDN-999.
          EXIT.
      * 
       STRIP-STOCKNAME SECTION.
       STRIP-000.
      * SUB-10 IS TO COUNT HOW MANY "/" HAVE BEEN FOUND.  WE NEED 3
          MOVE 0 TO SUB-2 SUB-10.
          MOVE 1 TO SUB-1.
          MOVE " " TO ALPHA-RATE
                      DATA-RATE.
          MOVE WS-STOCK TO ALPHA-RATE.
       STRIP-015.
          ADD 1 TO SUB-2.
          IF SUB-2 NOT > 60
           IF AL-RATE (SUB-2) NOT = "/"
            GO TO STRIP-015.
           IF AL-RATE (SUB-2) = "/"
            IF SUB-10 < 2
              ADD 1 TO SUB-10
            GO TO STRIP-015.

           ADD 1 TO SUB-2.
       STRIP-020.
           MOVE AL-RATE (SUB-2) TO DAT-RATE (SUB-1).
           ADD 1 TO SUB-2 SUB-1.
           IF AL-RATE (SUB-2) NOT = " "
              GO TO STRIP-020.
       STRIP-030.
           MOVE DATA-RATE TO WS-BARE-STOCK.
       STRIP-999.
           EXIT.
      *
       CHECK-DATA-SIZE SECTION.         
       CDS-005.
           MOVE " " TO ALPHA-RATE.
           MOVE 0   TO SUB-2.
       CDS-015.
           ADD 1 TO SUB-2.
           IF AL-RATE (SUB-2) NOT = " "
            IF SUB-2 NOT > 60
            GO TO CDS-015.
          SUBTRACT 1 FROM SUB-2.
       CDS-999.
          EXIT.
      *
       OPEN-NODE-FILE SECTION.
       ONF-001.
           IF WS-STOCK1 = " "
               GO TO ONF-002.
           MOVE 1 TO SUB-1.

           PERFORM CDS-005.
           Move Ws-STOCK1 To Alpha-Rate.
           PERFORM CDS-015.

           MOVE WS-STOCK1        TO F-FILENAME
           MOVE SUB-2            TO F-CBFILENAME.
           CALL "OPENFILE" USING    F-ERROR5
                                    F-FH
                                    F-FILENAME
                                    F-CBFILENAME
                                    F-FILENAME
                                    F-INTEGERZERO
                                    F-OPENMODE.
                                    
           IF F-ERROR5 NOT = 0
            IF F-ERROR5 NOT = 220
               MOVE " " TO WS-STOCK1
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO ONF-002.

           IF F-ERROR5 NOT = 210
               CALL "CLOSEFILE" USING  F-ERROR5
                                       F-FH.
       ONF-002.
           IF WS-STOCK2 = " "
               GO TO ONF-003.
           MOVE 2 TO SUB-1.

           PERFORM CDS-005.
           Move Ws-STOCK2 To Alpha-Rate.
           PERFORM CDS-015.

           MOVE WS-STOCK2        TO F-FILENAME
           MOVE SUB-2            TO F-CBFILENAME.
           CALL "OPENFILE" USING    F-ERROR5
                                    F-FH
                                    F-FILENAME
                                    F-CBFILENAME
                                    F-FILENAME
                                    F-INTEGERZERO
                                    F-OPENMODE.
                                    
           IF F-ERROR5 NOT = 0
            IF F-ERROR5 NOT = 220
               MOVE " " TO WS-STOCK2
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO ONF-003.

           IF F-ERROR5 NOT = 210
               CALL "CLOSEFILE" USING  F-ERROR5
                                       F-FH.
       ONF-003.
           IF WS-STOCK3 = " "
               GO TO ONF-004.
           MOVE 3 TO SUB-1.
           
           PERFORM CDS-005.
           Move Ws-STOCK3 To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE WS-STOCK3        TO F-FILENAME
           MOVE SUB-2            TO F-CBFILENAME.
           CALL "OPENFILE" USING    F-ERROR5
                                    F-FH
                                    F-FILENAME
                                    F-CBFILENAME
                                    F-FILENAME
                                    F-INTEGERZERO
                                    F-OPENMODE.
                                    
           IF F-ERROR5 NOT = 0
            IF F-ERROR5 NOT = 220
               MOVE " " TO WS-STOCK3
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO ONF-004.

           IF F-ERROR5 NOT = 210
               CALL "CLOSEFILE" USING  F-ERROR5
                                       F-FH.
       ONF-004.
           IF WS-STOCK4 = " "
               GO TO ONF-999.
           MOVE 4 TO SUB-1.
           
           PERFORM CDS-005.
           Move Ws-STOCK4 To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE WS-STOCK4        TO F-FILENAME
           MOVE SUB-2            TO F-CBFILENAME.
           CALL "OPENFILE" USING    F-ERROR5
                                    F-FH
                                    F-FILENAME
                                    F-CBFILENAME
                                    F-FILENAME
                                    F-INTEGERZERO
                                    F-OPENMODE.
                                    
           IF F-ERROR5 NOT = 0
            IF F-ERROR5 NOT = 220
               MOVE " " TO WS-STOCK4
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO ONF-999.

           IF F-ERROR5 NOT = 210
               CALL "CLOSEFILE" USING  F-ERROR5
                                       F-FH.
       ONF-999.
            EXIT.
      *
       OPEN-BRANCH-STOCK SECTION.
       OBS-000.
            PERFORM OPEN-NODE-FILE.
       OBS-001.
            IF WS-STOCK1 = " "
                GO TO OBS-002.
            MOVE "Opening Branch1 Stock File ....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 1 TO SUB-1.

            OPEN I-O STOCK-MASTER1.
            IF WS-STOCK1-ST1 = 91
               MOVE "STOCK FILE1 CAN'T BE OPENED, 'ESC' TO IGNORE."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-BRANCH-NAME (SUB-1) TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE " " TO WS-STOCK1
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO OBS-002.
            IF WS-STOCK1-ST1 NOT = 0
               MOVE "STOCK FILE1 BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK1-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK1-ST1
               GO TO OBS-001.
       OBS-002.
            IF WS-STOCK2 = " "
                GO TO OBS-003.
            MOVE "Opening Branch2 Stock File ....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 2 TO SUB-1.

            OPEN I-O STOCK-MASTER2.
            IF WS-STOCK2-ST1 = 91
               MOVE "STOCK FILE2 CAN'T BE OPENED, 'ESC' TO IGNORE."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-BRANCH-NAME (SUB-1) TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE " " TO WS-STOCK2
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO OBS-003.
            IF WS-STOCK2-ST1 NOT = 0
               MOVE "STOCK FILE2 BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK2-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK2-ST1
               GO TO OBS-002.
       OBS-003.
            IF WS-STOCK3 = " "
                GO TO OBS-004.
            MOVE "Opening Branch3 Stock File ....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 3 TO SUB-1.

            OPEN I-O STOCK-MASTER3.
            IF WS-STOCK3-ST1 = 91
               MOVE "STOCK FILE3 CAN'T BE OPENED, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-BRANCH-NAME (SUB-1) TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE " " TO WS-STOCK3
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO OBS-004.
            IF WS-STOCK3-ST1 NOT = 0
               MOVE "STOCK FILE3 BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK3-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK3-ST1
               GO TO OBS-003.
       OBS-004.
            IF WS-STOCK4 = " "
                GO TO OBS-900.
            MOVE "Opening Branch4 Stock File ....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 4 TO SUB-1.

            OPEN I-O STOCK-MASTER4.
            IF WS-STOCK4-ST1 = 91
               MOVE "STOCK FILE4 CAN'T BE OPENED, 'ESC' TO IGNORE."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-BRANCH-NAME (SUB-1) TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE " " TO WS-STOCK4
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO OBS-999.
            IF WS-STOCK4-ST1 NOT = 0
               MOVE "STOCK FILE4 BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK4-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK4-ST1
               GO TO OBS-004.
       OBS-900.
             PERFORM ERROR1-020.
       OBS-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-022.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-022.
       OPEN-005.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
               
           PERFORM READ-PARAM-NEXT.
           PERFORM READ-INVQUES-FILE.
           PERFORM READ-PARAMETER.
           CLOSE PARAMETER-FILE.
       OPEN-040.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           MOVE "TOTALS***"  TO T-CATEGORY.
           MOVE TOT-SALESQTY TO T-SALESQTY.
           MOVE TOT-SALESAMT TO T-SALESAMT.
           MOVE TOT-COST     TO T-COST.
           COMPUTE TOT-MARGIN = (TOT-SALESAMT - TOT-COST).
           MOVE TOT-MARGIN   TO T-MARGIN.
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COST) * 100.
           MOVE TOT-PERC     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.

           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           MOVE 0   TO TOT-MARGIN
                       TOT-PERC.

           MOVE TOT-SALESQTY-YTD TO T-SALESQTY.
           MOVE TOT-SALESAMT-YTD TO T-SALESAMT.
           MOVE TOT-COST-YTD     TO T-COST.
           COMPUTE TOT-MARGIN-YTD =
               (TOT-SALESAMT-YTD - TOT-COST-YTD).
           MOVE TOT-MARGIN-YTD   TO T-MARGIN.
           COMPUTE TOT-PERC-YTD ROUNDED =
               (TOT-MARGIN-YTD / TOT-COST-YTD) * 100.
           MOVE TOT-PERC-YTD     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           MOVE 0   TO TOT-MARGIN
                       TOT-PERC.

           MOVE TOT-SALESQTY-LAST TO T-SALESQTY.
           MOVE TOT-SALESAMT-LAST TO T-SALESAMT.
           MOVE TOT-COST-LAST     TO T-COST.
           COMPUTE TOT-MARGIN-LAST =
               (TOT-SALESAMT-LAST - TOT-COST-LAST).
           MOVE TOT-MARGIN-LAST   TO T-MARGIN.
           COMPUTE TOT-PERC-LAST ROUNDED =
               (TOT-MARGIN-LAST / TOT-COST-LAST) * 100.
           MOVE TOT-PERC-LAST     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           ADD 4 TO LINE-CNT.
           
           IF WS-ANSWER3 = "G" OR = "P"
               MOVE "** REPORT RUN FOR THE GROUP OF COMPANIES **"
                 TO PRINT-REC 
               WRITE PRINT-REC AFTER 2
           ELSE 
               MOVE "** REPORT RUN FOR INDIVIDUAL COMPANY **"
                 TO PRINT-REC 
               WRITE PRINT-REC AFTER 2.
       END-101.
            IF WS-STOCK1 = " "
                GO TO END-102.
            CLOSE STOCK-MASTER1.
       END-102.
            IF WS-STOCK2 = " "
                GO TO END-103.
            CLOSE STOCK-MASTER2.
       END-103.
            IF WS-STOCK3 = " "
                GO TO END-104.
            CLOSE STOCK-MASTER3.
       END-104.
            IF WS-STOCK4 = " "
                GO TO END-450.
            CLOSE STOCK-MASTER4.
       END-450.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
           CLOSE PRINT-FILE.
           
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE STOCK-MASTER.
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
       Copy "Error4Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
