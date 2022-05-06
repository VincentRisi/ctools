        IDENTIFICATION DIVISION.
        PROGRAM-ID. StAnalRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-CAT               PIC X(3) VALUE " ".
       77  WS-ANALYSIS          PIC X VALUE " ".
       77  WS-TOTANSWER         PIC X VALUE " ".
       77  WS-MTDSALETOT        PIC S9(8)V99 VALUE 0.
       77  WS-YTDSALETOT        PIC S9(8)V99 VALUE 0.
       77  WS-LYSALETOT         PIC S9(8)V99 VALUE 0.
       77  WS-MTDCOSTTOT        PIC S9(8)V99 VALUE 0.
       77  WS-YTDCOSTTOT        PIC S9(8)V99 VALUE 0.
       77  WS-LYCOSTTOT         PIC S9(8)V99 VALUE 0.
       77  WS-MTDMARGINTOT      PIC S9(8)V99 VALUE 0.
       77  WS-YTDMARGINTOT      PIC S9(8)V99 VALUE 0.
       77  WS-LYMARGINTOT       PIC S9(8)V99 VALUE 0.
       77  WS-MTDPERCTOT        PIC S999V99 VALUE 0.
       77  WS-YTDPERCTOT        PIC S999V99 VALUE 0.
       77  WS-LYPERCTOT         PIC S999V99 VALUE 0.
       77  WS-MTDSALES          PIC S9(8)V99 VALUE 0.
       77  WS-YTDSALES          PIC S9(8)V99 VALUE 0.
       77  WS-LYSALES           PIC S9(8)V99 VALUE 0.
       77  WS-MTDCOST           PIC S9(8)V99 VALUE 0.
       77  WS-YTDCOST           PIC S9(8)V99 VALUE 0.
       77  WS-LYCOSTT           PIC S9(8)V99 VALUE 0.
       77  WS-MTDMARGIN         PIC S9(8)V99 VALUE 0.
       77  WS-YTDMARGIN         PIC S9(8)V99 VALUE 0.
       77  WS-LYMARGIN          PIC S9(8)V99 VALUE 0.
       77  WS-MTDPERC           PIC S999V99 VALUE 0.
       77  WS-YTDPERC           PIC S999V99 VALUE 0.
       77  WS-LYPERC            PIC S999V99 VALUE 0.
       77  WS-MTDGROSSSALE      PIC S9(8)V99 VALUE 0.
       77  WS-YTDGROSSSALE      PIC S9(8)V99 VALUE 0.
       77  WS-LYGROSSSALE       PIC S9(8)V99 VALUE 0.
       77  WS-MTDGROSSCOST      PIC S9(8)V99 VALUE 0.
       77  WS-YTDGROSSCOST      PIC S9(8)V99 VALUE 0.
       77  WS-LYGROSSCOST       PIC S9(8)V99 VALUE 0.
       77  WS-MTDGROSSMARGIN    PIC S9(8)V99 VALUE 0.
       77  WS-YTDGROSSMARGIN    PIC S9(8)V99 VALUE 0.
       77  WS-LYGROSSMARGIN     PIC S9(8)V99 VALUE 0.
       77  WS-MTDGROSSPERC      PIC S999V99 VALUE 0.
       77  WS-YTDGROSSPERC      PIC S999V99 VALUE 0.
       77  WS-LYGROSSPERC       PIC S999V99 VALUE 0.
       77  WS-TOTAL-VALUE       PIC S9(9)V99 VALUE 0.
       77  WS-RUNNING-TOTAL     PIC S9(9)V99 VALUE 0.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(48) VALUE
           "S T O C K   A N A L Y S I S   R E P O R T".
           03  FILLER         PIC X(15) VALUE "ANALYSIS TYPE: ".
           03  H1-TYPE        PIC X VALUE " ". 
           03  FILLER         PIC X(24) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(41) VALUE ALL "*".
           03  FILLER         PIC X(7) VALUE " ".
           03  FILLER         PIC X(16) VALUE ALL "*".
           03  FILLER         PIC X(38) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(17) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(55) VALUE "DESCRIPTION".
           03  FILLER         PIC X(14) VALUE "M O N T H  / ".
           03  FILLER         PIC X(12) VALUE "Y E A R  / ".
           03  FILLER         PIC X(11) VALUE "L A S T".
           03  FILLER         PIC X(23) VALUE "Y E A R.".
       01  HEAD5.
           03  FILLER         PIC X(67) VALUE " ".
           03  FILLER         PIC X(12) VALUE "UNITS ".
           03  FILLER         PIC X(15) VALUE "SALES ".
           03  FILLER         PIC X(12) VALUE "COSTS ".
           03  FILLER         PIC X(11) VALUE "MARGINS".
           03  FILLER         PIC X(16) VALUE "    % ".
       01  MTD-LINE.
           03  D-STOCK        PIC X(17).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(27).
           03  D-MTDUNIT      PIC Z(5)9-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-MTDSALE      PIC Z(7)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-MTDCOST      PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-MTDMARGIN    PIC Z(7)9.99-.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-MTDPERC      PIC ZZ9.99-.
           03  FILLER         PIC X(7) VALUE " ".
       01  YTD-LINE.
           03  FILLER         PIC X(64) VALUE " ".
           03  D-YTDUNIT      PIC Z(5)9-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-YTDSALE      PIC Z(7)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-YTDCOST      PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-YTDMARGIN    PIC Z(7)9.99-.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-YTDPERC      PIC ZZ9.99-.
           03  FILLER         PIC X(7) VALUE " ".
       01  LY-LINE.
           03  FILLER         PIC X(64) VALUE " ".
           03  D-LYUNIT       PIC Z(5)9-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-LYSALE       PIC Z(7)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-LYCOST       PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-LYMARGIN     PIC Z(7)9.99-.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-LYPERC       PIC ZZ9.99-.
           03  FILLER         PIC X(7) VALUE " ".
       01  TOT-LINE.
           03  FILLER         PIC X(60) VALUE " ".
           03  TOT-DESC       PIC X(13) VALUE " ".
           03  TOT-SALE       PIC Z(7)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  TOT-COST       PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  TOT-MARGIN     PIC Z(7)9.99-.
           03  FILLER         PIC X(4) VALUE " ".
           03  TOT-PERC       PIC ZZ9.99-.
           03  FILLER         PIC X(7) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 321 TO POS
           DISPLAY "** STOCK ANALYSIS REPORT **" AT POS
           MOVE 421 TO POS
           DISPLAY "***************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA
           PERFORM OPEN-FILES
           PERFORM PRINT-ROUTINE
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO WS-RANGE1 WS-RANGE2
            MOVE 1010 TO POS
            DISPLAY "            FROM STOCK NUMBER: [               ]"
                      AT POS
            MOVE 1042 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

      *      ACCEPT WS-RANGE1 AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
            MOVE 1210 TO POS
            DISPLAY "              TO STOCK NUMBER: [               ]"
                      AT POS
            MOVE 1242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

      *      ACCEPT WS-RANGE2 AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO GET-000.
            IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
            MOVE 1610 TO POS
            DISPLAY
            "T=ToolKit, P=Production, B=Both, N=Non Supplier Stock."
               AT POS.
       GET-020.
            MOVE 1410 TO POS
            DISPLAY "         ENTER ANALYSIS FIELD: [ ]" AT POS
            MOVE 1442 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANALYSIS.

      *      ACCEPT WS-ANALYSIS AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO GET-010.
            IF WS-ANALYSIS = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
            MOVE WS-ANALYSIS TO H1-TYPE.
       GET-030.
            MOVE 2010 TO POS
            DISPLAY "Gross Totals Only, Y = YES N = NO " AT POS
            MOVE 1810 TO POS
            DISPLAY "            GROSS TOTALS ONLY: [ ]" AT POS
            MOVE 1842 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TOTANSWER.

      *      ACCEPT WS-TOTANSWER AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO GET-020.
            IF WS-TOTANSWER NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-040
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-040.
            MOVE 2810 TO POS
            DISPLAY "The Report Is Being Compiled, Please Be Patient."
               AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE WS-RANGE1 TO ST-STOCKNUMBER
            START STOCK-MASTER KEY NOT < ST-STOCKNUMBER.
       PRR-002.
           READ STOCK-MASTER NEXT
               AT END
               PERFORM PRINT-TOTALS
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
           IF ST-STOCKNUMBER < WS-RANGE1
               GO TO PRR-002.
           IF ST-STOCKNUMBER > WS-RANGE2
               PERFORM PRINT-TOTALS
               GO TO PRR-999.

           IF WS-ANALYSIS = "T" OR = "P"
            IF ST-ANALYSIS = "B"
               GO TO PRR-006.
           IF ST-ANALYSIS NOT = WS-ANALYSIS
               GO TO PRR-002.
       PRR-006.
           IF WS-CAT = "   "
               MOVE ST-CATEGORY TO WS-CAT.
           IF ST-CATEGORY NOT = WS-CAT
               PERFORM PRINT-TOTALS.
       PRR-010.
           IF LINE-CNT < 56
               GO TO PRR-020.
            ADD 1            TO PAGE-CNT
            MOVE PAGE-CNT    TO H1-PAGE
            MOVE WS-ANALYSIS TO H1-TYPE
            MOVE " " TO PRINT-REC.
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
            WRITE PRINT-REC FROM HEAD4
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD5
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE 7 TO LINE-CNT.
       PRR-020.
           IF WS-TOTANSWER  = "Y"
               GO TO PRR-030.
           MOVE ST-STOCKNUMBER     TO D-STOCK
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE ST-SALESUNITMTD    TO D-MTDUNIT
           MOVE ST-SALESUNITSYTD   TO D-YTDUNIT
           MOVE ST-SALESUNITSLAST  TO D-LYUNIT
           MOVE ST-SALESRANDSMTD   TO D-MTDSALE
           MOVE ST-SALESRANDSYTD   TO D-YTDSALE
           MOVE ST-SALESRANDSLAST  TO D-LYSALE
           MOVE ST-SALESCOSTMTD    TO D-MTDCOST
           MOVE ST-SALESCOSTYTD    TO D-YTDCOST
           MOVE ST-SALESCOSTLAST   TO D-LYCOST.

           COMPUTE WS-MTDMARGIN ROUNDED =
                     (ST-SALESRANDSMTD - ST-SALESCOSTMTD)
           COMPUTE WS-YTDMARGIN ROUNDED =
                     (ST-SALESRANDSYTD - ST-SALESCOSTYTD)
           COMPUTE WS-LYMARGIN ROUNDED =
                     (ST-SALESRANDSLAST - ST-SALESCOSTLAST)
           COMPUTE WS-MTDPERC ROUNDED =
                     (WS-MTDMARGIN / ST-SALESCOSTMTD) * 100
           COMPUTE WS-YTDPERC ROUNDED =
                     (WS-YTDMARGIN / ST-SALESCOSTYTD) * 100
           COMPUTE WS-LYPERC ROUNDED =
                     (WS-LYMARGIN / ST-SALESCOSTLAST) * 100
           MOVE WS-MTDMARGIN TO D-MTDMARGIN
           MOVE WS-YTDMARGIN TO D-YTDMARGIN
           MOVE WS-LYMARGIN  TO D-LYMARGIN
           MOVE WS-MTDPERC   TO D-MTDPERC
           MOVE WS-YTDPERC   TO D-YTDPERC
           MOVE WS-LYPERC    TO D-LYPERC.
       PRR-030.
           ADD ST-SALESRANDSMTD  TO WS-MTDSALETOT
           ADD ST-SALESRANDSYTD  TO WS-YTDSALETOT
           ADD ST-SALESRANDSLAST TO WS-LYSALETOT
           ADD ST-SALESCOSTMTD   TO WS-MTDCOSTTOT
           ADD ST-SALESCOSTYTD   TO WS-YTDCOSTTOT
           ADD ST-SALESCOSTLAST  TO WS-LYCOSTTOT.
           
           MOVE 2610 TO POS
           DISPLAY "STOCKNUMBER BEING READ:" AT POS
           ADD 24 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
       PRR-040.
           IF WS-TOTANSWER = "Y"
              GO TO PRR-050.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM MTD-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM YTD-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM LY-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           ADD 4 TO LINE-CNT
           MOVE 0 TO WS-MTDPERC
                     WS-YTDPERC
                     WS-LYPERC.
       PRR-050.
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       PRINT-TOTALS SECTION.
       PT-000.
           IF LINE-CNT > 56
              PERFORM PRR-010.
           COMPUTE WS-MTDMARGINTOT ROUNDED =
               (WS-MTDSALETOT - WS-MTDCOSTTOT)
           COMPUTE WS-YTDMARGINTOT ROUNDED =
               (WS-YTDSALETOT - WS-YTDCOSTTOT)
           COMPUTE WS-LYMARGINTOT ROUNDED =
                 (WS-LYSALETOT - WS-LYCOSTTOT)
           COMPUTE WS-MTDPERCTOT =
                 (WS-MTDMARGINTOT / WS-MTDCOSTTOT) * 100
           COMPUTE WS-YTDPERCTOT =
                 (WS-YTDMARGINTOT / WS-YTDCOSTTOT) * 100
           COMPUTE WS-LYPERCTOT =
                 (WS-LYMARGINTOT / WS-LYCOSTTOT) * 100.
       PT-005.
           MOVE 2319 TO POS
           DISPLAY "Sales In Category :      MTD :R" AT POS
           MOVE 2339 TO POS
           DISPLAY WS-CAT AT POS

           MOVE " " TO PRINT-REC
           MOVE WS-CAT          TO TOT-DESC
           MOVE WS-MTDSALETOT   TO TOT-SALE
           MOVE WS-MTDCOSTTOT   TO TOT-COST
           MOVE WS-MTDMARGINTOT TO TOT-MARGIN
           MOVE WS-MTDPERCTOT   TO TOT-PERC
           WRITE PRINT-REC FROM TOT-LINE
           MOVE " " TO PRINT-REC
           MOVE 2419 TO POS
           DISPLAY "                         YTD :R" AT POS
           MOVE 2350 TO POS
           DISPLAY TOT-SALE AT POS

           MOVE " "             TO TOT-DESC
           MOVE WS-YTDSALETOT   TO TOT-SALE
           MOVE WS-YTDCOSTTOT   TO TOT-COST
           MOVE WS-YTDMARGINTOT TO TOT-MARGIN
           MOVE WS-YTDPERCTOT   TO TOT-PERC
           WRITE PRINT-REC FROM TOT-LINE
           MOVE " " TO PRINT-REC
           MOVE 2519 TO POS
           DISPLAY "                          LY :R" AT POS
           MOVE 2450 TO POS
           DISPLAY TOT-SALE AT POS

           MOVE WS-LYSALETOT   TO TOT-SALE
           MOVE WS-LYCOSTTOT   TO TOT-COST
           MOVE WS-LYMARGINTOT TO TOT-MARGIN
           MOVE WS-LYPERCTOT   TO TOT-PERC
           WRITE PRINT-REC FROM TOT-LINE
           MOVE " " TO PRINT-REC
           MOVE 2619 TO POS
           DISPLAY "       TOTAL GROSS SALES YTD :R" AT POS
           MOVE 2550 TO POS
           DISPLAY TOT-SALE AT POS
           WRITE PRINT-REC.
       PT-010.
           ADD WS-MTDSALETOT TO WS-MTDGROSSSALE
           ADD WS-YTDSALETOT TO WS-YTDGROSSSALE
           ADD WS-LYSALETOT  TO WS-LYGROSSSALE

           ADD WS-MTDCOSTTOT TO WS-MTDGROSSCOST
           ADD WS-YTDCOSTTOT TO WS-YTDGROSSCOST
           ADD WS-LYCOSTTOT  TO WS-LYGROSSCOST

           ADD WS-MTDMARGINTOT TO WS-MTDGROSSMARGIN
           ADD WS-YTDMARGINTOT TO WS-YTDGROSSMARGIN
           ADD WS-LYMARGINTOT  TO WS-LYGROSSMARGIN

           MOVE WS-YTDGROSSSALE TO TOT-SALE
           MOVE 2650 TO POS
           DISPLAY TOT-SALE AT POS.
       PT-020.
           MOVE 0 TO WS-MTDSALETOT
                     WS-MTDCOSTTOT
                     WS-MTDMARGINTOT
                     WS-MTDPERCTOT
                     WS-YTDSALETOT
                     WS-YTDCOSTTOT
                     WS-YTDMARGINTOT
                     WS-YTDPERCTOT
                     WS-LYSALETOT
                     WS-LYCOSTTOT
                     WS-LYMARGINTOT
                     WS-LYPERCTOT.
       PT-030.
           MOVE ST-CATEGORY TO WS-CAT
           ADD 4 TO LINE-CNT.
       PT-999.
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
        OPEN-010.
           MOVE Ws-Co-Name TO CO-NAME
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 56
              PERFORM PRR-010.
           MOVE "GROSS TOTALS :"  TO TOT-DESC
           MOVE WS-MTDGROSSSALE   TO TOT-SALE
           MOVE WS-MTDGROSSCOST   TO TOT-COST
           MOVE WS-MTDGROSSMARGIN TO TOT-MARGIN
           COMPUTE WS-MTDGROSSPERC =
               (WS-MTDGROSSMARGIN / WS-MTDGROSSCOST) * 100
           MOVE WS-MTDGROSSPERC TO TOT-PERC
           WRITE PRINT-REC FROM TOT-LINE
           MOVE " "               TO PRINT-REC
                                     TOT-DESC

           MOVE WS-YTDGROSSSALE   TO TOT-SALE
           MOVE WS-YTDGROSSCOST   TO TOT-COST
           MOVE WS-YTDGROSSMARGIN TO TOT-MARGIN
           COMPUTE WS-YTDGROSSPERC =
               (WS-YTDGROSSMARGIN / WS-YTDGROSSCOST) * 100
           MOVE WS-YTDGROSSPERC TO TOT-PERC
           WRITE PRINT-REC FROM TOT-LINE
           MOVE " " TO PRINT-REC

           MOVE WS-LYGROSSSALE   TO TOT-SALE
           MOVE WS-LYGROSSCOST   TO TOT-COST
           MOVE WS-LYGROSSMARGIN TO TOT-MARGIN
           COMPUTE WS-LYGROSSPERC =
                (WS-LYGROSSMARGIN / WS-LYGROSSCOST) * 100
           MOVE WS-LYGROSSPERC TO TOT-PERC
           WRITE PRINT-REC FROM TOT-LINE
           MOVE " " TO PRINT-REC.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-040.
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
      * END-OF-JOB.
