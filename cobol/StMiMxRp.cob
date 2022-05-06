        IDENTIFICATION DIVISION.
        PROGRAM-ID. StMiMxRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B26.
        OBJECT-COMPUTER. B26.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
         Copy "SelectStMaster1".
         Copy "SelectStMaster2".
         Copy "SelectStMaster3".
         Copy "SelectStMaster4".
         Copy "SelectSlParameter".
         Copy "SelectCoDataName".
         Copy "SelectCoCompany".
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
           COPY ChlfdParam.
           COPY ChlfdCompany.
           Copy ChlfdDataName.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-CATVALUE          PIC 9(8)V99 VALUE 0.
       77  WS-TOTVALUE          PIC 9(8)V99 VALUE 0.
       77  WS-QTYONHAND         PIC S9(6).
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(15) VALUE " ".
       77  WS-BELOW-OVER        PIC X VALUE " ".
       77  WS-CAT-ONLY          PIC X VALUE " ".
       77  WS-SEL-TYPE          PIC X VALUE " ".
       77  WS-F-L               PIC X VALUE " ".
       77  WS-CHECK-GROUP       PIC X VALUE " ".
       77  WS-END               PIC X VALUE " ".
       77  WS-QTY               PIC S9(6)V99 VALUE 0.
       77  WS-PasswordSaved     Pic X(10).
       77  PSW-SUB1             PIC S9(5) VALUE 0.
       77  PSW-SUB2             PIC S9(5) VALUE 0.
       77  WS-BARE-STOCK        PIC X(20) VALUE " ".
       01  W-READ-KEY           PIC X.
       01  WS-PASSWORD-KEY.
           03  WS-PA-KEY          PIC X OCCURS 11.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
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
       01  WS-DATA-STATUS.
           03  WS-DATA-ST1        PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  COMPANIES-LIST-NAMES.
         02  COMPANIES-LIST OCCURS 20.
           03  LIST-NAME          PIC X(40).
           03  LIST-NUMBER        PIC 99.
           03  LIST-VOL-DIR       PIC X(25).
       01  WS-BRANCH-INFO-NAMES.
         02  WS-BRANCH-INFO OCCURS 10.
           03  WS-BRANCH-TYPE          PIC 9.
           03  WS-BRANCH-NUMBER        PIC 9.
           03  WS-BRANCH-NOT-THERE     PIC X(2).
           03  WS-BRANCH-NAME          PIC X(3).
           03  WS-BRANCH-STOCK-VOL-DIR PIC X(40).
           03  WS-BRANCH-STOCK         PIC X(15).
           03  WS-BRANCH-ONHAND        PIC 9(6).
           03  WS-BRANCH-ONRES         PIC 9(6).
           03  WS-BRANCH-ONORDER       PIC 9(6).
           03  WS-BRANCH-ONBO          PIC 9(6).
       01  WS-MERGE-STOCK-NAMES.
         02  WS-MERGE-STOCK OCCURS 10.
           03  WS-BRQTYONHAND        PIC 9(6).
           03  WS-QTYONRESERVE       PIC 9(6).
           03  WS-QTYONORDER         PIC 9(6).
           03  WS-QTYONBORDER        PIC 9(6).
           03  WS-QTY-ST-TAKE        PIC 9(6).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(52) VALUE
           "S T O C K   L E V E L   B E L O W   M I N I M U M".
           03  FILLER         PIC X(28) VALUE "R E P O R T".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(15) VALUE " ".
       01  HEAD1-1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-1-DATE      PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(52) VALUE
           "S T O C K   L E V E L   A B O V E   M A X I M U M".
           03  FILLER         PIC X(28) VALUE "R E P O R T".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(14) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(63) VALUE ALL "*".
           03  FILLER         PIC X(40) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(61) VALUE " ".
           03  FILLER         PIC X(71) VALUE
           "     STOCK QUANTITY ON:       SALE        S A L E S:".
       01  HEAD4.
           03  FILLER         PIC X(60) VALUE
           "STOCK NUMBER      DESCRIPTION".
           03  FILLER         PIC X(60) VALUE
           "   MIN   HAND  ORDER    B/O    DATE      MTD    YTD   LAST".
           03  FILLER         PIC X(12) VALUE "SUPPLIER".
       01  HEAD4-1.
           03  FILLER         PIC X(60) VALUE
           "STOCK NUMBER      DESCRIPTION".
           03  FILLER         PIC X(60) VALUE
           "   MAX   HAND  ORDER    B/O    DATE      MTD    YTD   LAST".
           03  FILLER         PIC X(12) VALUE "SUPPLIER".
       01  HEAD4-2.
           03  FILLER         PIC X(60) VALUE
           "STOCK NUMBER      DESCRIPTION".
           03  FILLER         PIC X(41) VALUE
           "   MIN   HAND  ORDER    B/O    DATE".
           03  H42-CO1        PIC X(3).
           03  FILLER         PIC X(5) VALUE " ".
           03  H42-CO2        PIC X(3).
           03  FILLER         PIC X(4) VALUE " ".
           03  H42-CO3        PIC X(3).
           03  FILLER         PIC X(4) VALUE " ".
           03  H42-CO4        PIC X(3).
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(16) VALUE " ".
           03  D-BELOW        PIC X.
           03  FILLER         PIC X VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(22) VALUE " ".
           03  D-MIN          PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-ONHAND       PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-ORDER        PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-BO           PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-DATE         PIC X(10).
           03  D-MTD          PIC Z(5)9-.
           03  D-YTD          PIC Z(5)9-.
           03  D-LAST         PIC Z(5)9-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SUPPLIER     PIC X(10).
       01  DETAIL-GROUP-LINE.
           03  D-GP-STOCK     PIC X(16) VALUE " ".
           03  D-GP-BELOW     PIC X.
           03  FILLER         PIC X VALUE " ".
           03  D-GP-DESC1     PIC X(20) VALUE " ".
           03  D-GP-DESC2     PIC X(22) VALUE " ".
           03  D-GP-MIN       PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-GP-ONHAND    PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-GP-ORDER     PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-GP-BO        PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-GP-DATE      PIC X(10).
           03  D-GP-CO1       PIC X(7).
           03  D-GP-CO2       PIC X(7).
           03  D-GP-CO3       PIC X(7).
           03  D-GP-CO4       PIC X(7).
       01  TOTAL-LINE.
           03  FILLER         PIC X(45).
           03  T-CAT          PIC X(5).
           03  T-NAME         PIC X(15) VALUE "EXCESS STOCK: R".
           03  T-VALUE        PIC Z(7)9.99.
           03  FILLER         PIC X(56).
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 310 TO POS
           DISPLAY
             "** STOCK LEVEL BELOW MINIMUM/OVER MAXIMUM REPORT **"
              AT POS
           MOVE 410 TO POS
           DISPLAY
             "***************************************************"
              AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1210 TO POS.
           DISPLAY "FROM STOCK NUMBER : [               ]" AT POS.
           MOVE 1231 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-012
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-012.
           MOVE 1310 TO POS.
           DISPLAY "  TO STOCK NUMBER : [               ]" AT POS.
           MOVE 1331 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           IF WS-ANSWER2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
       CONTROL-015.
           MOVE 1510 TO POS.
           DISPLAY "ENTER 'B'=BELOW MIN, 'O'=OVER MAX: [ ]" AT POS.
           MOVE 1546 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 45        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-BELOW-OVER.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-012.
           IF WS-BELOW-OVER = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
           IF WS-BELOW-OVER NOT = "B" AND NOT = "O"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
           IF WS-BELOW-OVER = "B"
               MOVE "N" TO WS-CAT-ONLY
               GO TO CONTROL-025.
           MOVE 1710 TO POS.
           DISPLAY "PRINT CATEGORIES ONLY, ENTER Y OR N: [ ]" AT POS.
           ADD 38 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CAT-ONLY.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF WS-CAT-ONLY NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               MOVE "N" TO WS-CHECK-GROUP
               GO TO CONTROL-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-025.
           MOVE 1710 TO POS.
           DISPLAY "ENTER Y= ITEMS WHERE (ON-HAND + ON-ORDER <" &
           " ON-BORDER." AT POS.
           MOVE 1810 TO POS.
           DISPLAY "ENTER N= ITEMS WHERE (ON-HAND + ON-ORDER - " &
           "ON-BORDER < MINIMUM." AT POS.
           MOVE 1910 TO POS.
           DISPLAY 
          "ENTER A= FOR ALL ITEMS ONHAND BELOW MINIMUM.    [ ]" AT POS.
           ADD 49 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 58        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SEL-TYPE.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF WS-SEL-TYPE NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-025.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-027
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-025.
       CONTROL-027.
           MOVE 2110 TO POS.
           DISPLAY "ENTER F=FOREIGN, L=LOCAL, A=ALL ITEMS : [ ]"
              AT POS.
           ADD 41 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 18        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-F-L.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-025.
           IF WS-F-L NOT = "F" AND NOT = "L" AND NOT = "A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-027.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-028
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-027.
       CONTROL-028.
           IF WS-BELOW-OVER NOT = "B"
              MOVE "N" TO WS-CHECK-GROUP
              GO TO CONTROL-030
           ELSE
              MOVE "Y" TO WS-CHECK-GROUP.
           MOVE 2310 TO POS.
           DISPLAY "CHECK GROUP COMPANY STOCK, Y OR N ?   : [ ]"
              AT POS.
           ADD 41 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 20        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CHECK-GROUP.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-027.
           IF WS-CHECK-GROUP NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-028.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-028.
       CONTROL-030.
           MOVE 2710 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
              AT POS.
           PERFORM OPEN-FILES.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           IF WS-CHECK-GROUP = "N"
               PERFORM PRINT-ROUTINE
           ELSE
               Move 2810 to Pos
               Display "Reading Next-Company........" At Pos
               Perform Read-Next-Company

               Move 2810 to Pos
               Display "Getting Branch Stock file Names.. " at Pos
               Perform Check-Branch-Data-Names
               Perform Error-020
           
               Move 2720 to Pos
               Display "Opening Branch Files.............." At Pos
               Perform Open-Branch-Stock

               MOVE WS-BRANCH-NAME (1) TO H42-CO1
               MOVE WS-BRANCH-NAME (2) TO H42-CO2
               MOVE WS-BRANCH-NAME (3) TO H42-CO3
               MOVE WS-BRANCH-NAME (4) TO H42-CO4

               PERFORM ERROR-020
               PERFORM PRINT-GROUP-ROUTINE.
           PERFORM END-OFF.
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
              GO TO PRR-999.
           IF WS-STORE = "   "
              MOVE ST-CATEGORY TO WS-STORE.

           MOVE 2510 TO POS
           DISPLAY "Stock Number Being Read:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

           COMPUTE ST-QTYONHAND = ST-QTYONHAND + ST-QTYONRESERVE.

           IF WS-BELOW-OVER = "O"
             COMPUTE WS-QTYONHAND = ST-QTYONHAND - ST-QTYONBORDER
           IF WS-QTYONHAND > ST-MAXIMUMLEVEL
              GO TO PRR-008.

           IF WS-BELOW-OVER = "B"
            IF WS-SEL-TYPE = "Y"
               COMPUTE WS-QTY = ST-QTYONHAND + ST-QTYONORDER
             IF WS-QTY < ST-QTYONBORDER
              GO TO PRR-008.
           IF WS-BELOW-OVER = "B"
            IF WS-SEL-TYPE = "N"
              COMPUTE WS-QTY = ST-QTYONHAND - ST-QTYONBORDER
                                + ST-QTYONORDER
              IF WS-QTY < ST-MINIMUMLEVEL
               GO TO PRR-008.
           IF WS-BELOW-OVER = "B"
            IF WS-SEL-TYPE = "A"
              COMPUTE WS-QTY = ST-QTYONHAND - ST-QTYONBORDER
              IF WS-QTY < ST-MINIMUMLEVEL
               GO TO PRR-008.
               
           GO TO PRR-005.
       PRR-008.
           IF WS-F-L = "A"
               GO TO PRR-010.
           IF WS-F-L = "F"
            IF ST-FOREIGNCOST > 0
               GO TO PRR-010.
           IF WS-F-L = "L"
            IF ST-FOREIGNCOST = 0
               GO TO PRR-010.
               
           GO TO PRR-005.
       PRR-010.
           IF LINE-CNT < 60
               GO TO PRR-020.
           ADD 1 TO PAGE-CNT.
           IF WS-BELOW-OVER = "B"
               MOVE PAGE-CNT TO H1-PAGE
            ELSE
               MOVE PAGE-CNT TO H1-1-PAGE.
            MOVE " " TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC.
            IF PAGE-CNT > 1
               WRITE PRINT-REC BEFORE PAGE.
            WRITE PRINT-REC FROM COMPANY-LINE.
            MOVE " " TO PRINT-REC.
            IF WS-BELOW-OVER = "B"
                WRITE PRINT-REC FROM HEAD1
            ELSE
                WRITE PRINT-REC FROM HEAD1-1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD2.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD3.
            MOVE " " TO PRINT-REC.
            IF WS-BELOW-OVER = "B"
               WRITE PRINT-REC FROM HEAD4
            ELSE
               WRITE PRINT-REC FROM HEAD4-1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE 7 TO LINE-CNT.
       PRR-020.
           IF ST-CATEGORY NOT = WS-STORE
               MOVE WS-STORE TO T-CAT
               MOVE " " TO PRINT-REC
               MOVE ST-CATEGORY   TO WS-STORE
            IF WS-BELOW-OVER = "B"
               WRITE PRINT-REC
               ADD 1 TO LINE-CNT
            ELSE
               MOVE WS-CATVALUE TO T-VALUE
               WRITE PRINT-REC FROM TOTAL-LINE
               ADD WS-CATVALUE TO WS-TOTVALUE
               MOVE 0 TO WS-CATVALUE
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 2 TO LINE-CNT.
           IF WS-CAT-ONLY = "Y"
               GO TO PRR-025.
           IF WS-BELOW-OVER = "B"
            IF ST-QTYONHAND + ST-QTYONORDER < ST-QTYONBORDER
               MOVE "*"           TO D-BELOW.
           IF ST-ANALYSIS = "D" OR = "N" OR = "S"
               MOVE ST-ANALYSIS   TO D-BELOW.
           MOVE ST-STOCKNUMBER    TO D-STOCK.
           MOVE ST-DESCRIPTION1   TO D-DESC1.
           MOVE ST-DESCRIPTION2   TO D-DESC2.
           IF WS-BELOW-OVER = "B"
             MOVE ST-MINIMUMLEVEL TO D-MIN
           ELSE
             MOVE ST-MAXIMUMLEVEL TO D-MIN.
           MOVE ST-QTYONHAND      TO D-ONHAND.
           MOVE ST-QTYONORDER     TO D-ORDER.
           MOVE ST-QTYONBORDER    TO D-BO.
           MOVE ST-LASTSALEDATE   TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE      TO D-DATE.
           MOVE ST-SALESUNITMTD   TO D-MTD.
           MOVE ST-SALESUNITSYTD  TO D-YTD.
           MOVE ST-SALESUNITSLAST TO D-LAST.
           MOVE ST-SUPPLIER       TO D-SUPPLIER.
           WRITE PRINT-REC FROM DETAIL-LINE.
           ADD 1 TO LINE-CNT.
           MOVE " " TO PRINT-REC DETAIL-LINE.
       PRR-025.
           IF WS-BELOW-OVER = "B"
              GO TO PRR-005.
           COMPUTE WS-CATVALUE = WS-CATVALUE +
               ((WS-QTYONHAND - ST-MAXIMUMLEVEL) * ST-AVERAGECOST).
           GO TO PRR-005.
       PRR-999.
           EXIT.
      *
       PRINT-GROUP-ROUTINE SECTION.
       PGR-000.
           MOVE WS-ANSWER1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       PGR-005.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               GO TO PGR-999.
           IF WS-STOCK-ST1 NOT = 0
             MOVE 
           "GROUP STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO PGR-005.
           IF ST-STOCKNUMBER < WS-ANSWER1
              GO TO PGR-005.
           IF ST-STOCKNUMBER > WS-ANSWER2
              GO TO PGR-999.
           IF WS-STORE = "   "
              MOVE ST-CATEGORY TO WS-STORE.

           MOVE 2510 TO POS
           DISPLAY "Stock Number Being Read:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

           COMPUTE ST-QTYONHAND = ST-QTYONHAND + ST-QTYONRESERVE.

           IF WS-SEL-TYPE = "Y"
               COMPUTE WS-QTY = ST-QTYONHAND + ST-QTYONORDER
             IF WS-QTY < ST-QTYONBORDER
              GO TO PGR-008.
           IF WS-SEL-TYPE = "N"
              COMPUTE WS-QTY = ST-QTYONHAND - ST-QTYONBORDER
                                + ST-QTYONORDER
              IF WS-QTY < ST-MINIMUMLEVEL
               GO TO PGR-008.
           IF WS-SEL-TYPE = "A"
              COMPUTE WS-QTY = ST-QTYONHAND - ST-QTYONBORDER
              IF WS-QTY < ST-MINIMUMLEVEL
               GO TO PGR-008.
               
           GO TO PGR-005.
       PGR-008.
           IF WS-F-L = "A"
               GO TO PGR-010.
           IF WS-F-L = "F"
            IF ST-FOREIGNCOST > 0
               GO TO PGR-010.
           IF WS-F-L = "L"
            IF ST-FOREIGNCOST = 0
               GO TO PGR-010.
               
           GO TO PGR-005.
       PGR-010.
           IF LINE-CNT < 60
               GO TO PGR-020.
               
           ADD 1 TO PAGE-CNT.
           IF WS-BELOW-OVER = "B"
               MOVE PAGE-CNT TO H1-PAGE
            ELSE
               MOVE PAGE-CNT TO H1-1-PAGE.
            MOVE " " TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC.
            IF PAGE-CNT > 1
               WRITE PRINT-REC BEFORE PAGE.
            WRITE PRINT-REC FROM COMPANY-LINE.
            MOVE " " TO PRINT-REC.
            IF WS-BELOW-OVER = "B"
                WRITE PRINT-REC FROM HEAD1
            ELSE
                WRITE PRINT-REC FROM HEAD1-1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD2.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD3.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD4-2
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE 7 TO LINE-CNT.
       PGR-020.
           IF ST-CATEGORY NOT = WS-STORE
               MOVE WS-STORE TO T-CAT
               MOVE " " TO PRINT-REC
               MOVE ST-CATEGORY   TO WS-STORE.
               
           WRITE PRINT-REC
           ADD 1 TO LINE-CNT.
           
           IF ST-QTYONHAND + ST-QTYONORDER < ST-QTYONBORDER
                MOVE "*"          TO D-GP-BELOW.
           IF ST-ANALYSIS = "D" OR = "N" OR = "S"
               MOVE ST-ANALYSIS   TO D-GP-BELOW.
           MOVE ST-STOCKNUMBER    TO D-GP-STOCK
           MOVE ST-DESCRIPTION1   TO D-GP-DESC1
           MOVE ST-DESCRIPTION2   TO D-GP-DESC2
           MOVE ST-MINIMUMLEVEL   TO D-GP-MIN
           MOVE ST-QTYONHAND      TO D-GP-ONHAND
           MOVE ST-QTYONORDER     TO D-GP-ORDER
           MOVE ST-QTYONBORDER    TO D-GP-BO
           MOVE ST-LASTSALEDATE   TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-GP-DATE.

           MOVE " " TO WS-END.
           PERFORM READ-BRANCH-STOCK.
           MOVE 1 TO SUB-1.
       PGR-022.
           IF WS-BRANCH-NAME (1) = " "
               GO TO PGR-023.
           IF WS-BRANCH-NOT-THERE (SUB-1) = "XX"
               MOVE "    O/L"                 TO D-GP-CO1
               GO TO PGR-023.
           IF WS-BRANCH-ONHAND (SUB-1) = 999999
               MOVE " NONSTK"                  TO D-GP-CO1
           ELSE
               MOVE WS-BRANCH-ONHAND (SUB-1)  TO D-ONHAND
               MOVE D-ONHAND                  TO D-GP-CO1.
       PGR-023.
           IF WS-BRANCH-NAME (2) = " "
               GO TO PGR-024.
           ADD 1 TO SUB-1.
           IF WS-BRANCH-NOT-THERE (SUB-1) = "XX"
               MOVE "    O/L"                 TO D-GP-CO2
               GO TO PGR-024.
           IF WS-BRANCH-ONHAND (SUB-1) = 999999
               MOVE " NONSTK"                  TO D-GP-CO2
           ELSE
               MOVE WS-BRANCH-ONHAND (SUB-1)  TO D-ONHAND
               MOVE D-ONHAND                  TO D-GP-CO2.
       PGR-024.
           IF WS-BRANCH-NAME (3) = " "
               GO TO PGR-025.
           ADD 1 TO SUB-1.
           IF WS-BRANCH-NOT-THERE (SUB-1) = "XX"
               MOVE "    O/L"                 TO D-GP-CO3
               GO TO PGR-025.
           IF WS-BRANCH-ONHAND (SUB-1) = 999999
               MOVE " NONSTK"                  TO D-GP-CO3
           ELSE
               MOVE WS-BRANCH-ONHAND (SUB-1)  TO D-ONHAND
               MOVE D-ONHAND                  TO D-GP-CO3.
       PGR-025.
           IF WS-BRANCH-NAME (4) = " "
               GO TO PGR-030.
           ADD 1 TO SUB-1.
           IF WS-BRANCH-NOT-THERE (SUB-1) = "XX"
               MOVE "    O/L"                 TO D-GP-CO4
               GO TO PGR-030.
           IF WS-BRANCH-ONHAND (SUB-1) = 999999
               MOVE " NONSTK"                  TO D-GP-CO4
           ELSE
               MOVE WS-BRANCH-ONHAND (SUB-1)  TO D-ONHAND
               MOVE D-ONHAND                  TO D-GP-CO4.
       PGR-030.
           WRITE PRINT-REC FROM DETAIL-GROUP-LINE.
           ADD 1 TO LINE-CNT.
           MOVE " " TO PRINT-REC DETAIL-GROUP-LINE.
       PGR-055.
           GO TO PGR-005.
       PGR-999.
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
               MOVE 
             "COMPANY FILE BUSY ON READ-NEXT, IN 1 SEC GOING TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-MENU-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
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
       READ-BRANCH-STOCK SECTION.
       RBRST-100.
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
       RBRST-110.
            READ STOCK-MASTER1
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK1-ST1 = 23 OR 35 OR 49
                MOVE 999999 TO WS-BRANCH-ONHAND (SUB-1)
                               WS-BRANCH-ONRES (SUB-1)
                               WS-BRANCH-ONORDER (SUB-1)
                               WS-BRANCH-ONBO (SUB-1)
                GO TO RBRST-200.
             IF WS-STOCK1-ST1 NOT = 0
                MOVE "STOCK RECORD1 BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RBRST-110.
             MOVE ST1-QTYONHAND    TO WS-BRANCH-ONHAND (SUB-1)
             MOVE ST1-QTYONRESERVE TO WS-BRANCH-ONRES (SUB-1)
             MOVE ST1-QTYONORDER   TO WS-BRANCH-ONORDER (SUB-1)
             MOVE ST1-QTYONBORDER  TO WS-BRANCH-ONBO (SUB-1).
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
                MOVE 999999 TO WS-BRANCH-ONHAND (SUB-1)
                               WS-BRANCH-ONRES (SUB-1)
                               WS-BRANCH-ONORDER (SUB-1)
                               WS-BRANCH-ONBO (SUB-1)
                GO TO RBRST-300.
             IF WS-STOCK2-ST1 NOT = 0
                MOVE "STOCK RECORD2 BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RBRST-210.
             MOVE ST2-QTYONHAND    TO WS-BRANCH-ONHAND (SUB-1)
             MOVE ST2-QTYONRESERVE TO WS-BRANCH-ONRES (SUB-1)
             MOVE ST2-QTYONORDER   TO WS-BRANCH-ONORDER (SUB-1)
             MOVE ST2-QTYONBORDER  TO WS-BRANCH-ONBO (SUB-1).
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
                MOVE 999999 TO WS-BRANCH-ONHAND (SUB-1)
                               WS-BRANCH-ONRES (SUB-1)
                               WS-BRANCH-ONORDER (SUB-1)
                               WS-BRANCH-ONBO (SUB-1)
                GO TO RBRST-400.
             IF WS-STOCK3-ST1 NOT = 0
                MOVE "STOCK RECORD3 BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RBRST-310.
             MOVE ST3-QTYONHAND    TO WS-BRANCH-ONHAND (SUB-1)
             MOVE ST3-QTYONRESERVE TO WS-BRANCH-ONRES (SUB-1)
             MOVE ST3-QTYONORDER   TO WS-BRANCH-ONORDER (SUB-1)
             MOVE ST3-QTYONBORDER  TO WS-BRANCH-ONBO (SUB-1).
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
                MOVE 999999 TO WS-BRANCH-ONHAND (SUB-1)
                               WS-BRANCH-ONRES (SUB-1)
                               WS-BRANCH-ONORDER (SUB-1)
                               WS-BRANCH-ONBO (SUB-1)
                GO TO RBRST-900.
             IF WS-STOCK4-ST1 NOT = 0
                MOVE "STOCK RECORD4 BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RBRST-410.
             MOVE ST4-QTYONHAND    TO WS-BRANCH-ONHAND (SUB-1)
             MOVE ST4-QTYONRESERVE TO WS-BRANCH-ONRES (SUB-1)
             MOVE ST4-QTYONORDER   TO WS-BRANCH-ONORDER (SUB-1)
             MOVE ST4-QTYONBORDER  TO WS-BRANCH-ONBO (SUB-1).
       RBRST-900.
             PERFORM ERROR4-020.
       RBRST-999.
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
               MOVE "SLPARAMETER NOT ON FILE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RNX-999.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "SLPARAMETER BUSY ON READ-NEXT, 'ESC' TO RETRY."
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
               DISPLAY "NO PARAMETER RECORD ON FILE, 'ESC' TO RETRY."
               CALL "LOCKKBD" USING F-FIELDNAME
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
       Check-Branch-Data-Names Section.
       CBDN-005.
          MOVE 1 TO SUB-20.
          PERFORM STRIP-STOCKNAME.
       CBDN-006.
          Move " " To Alpha-Rate
                      Data-Rate.
          Move 0   To Sub-1.
          MOVE WS-BRANCH-NUMBER (SUB-20) TO SUB-25.
       CBDN-010.
          Move List-Vol-Dir (SUB-25) To Alpha-Rate.
       CBDN-015.
          Add 1 To Sub-1.
          If Sub-1 Not > 60
           If Al-Rate (SUB-1) Not = " "
            Go To CBDN-015.
       CBDN-020.
          Move Ws-Bare-Stock To Data-Rate.
          Move 1             To Sub-2.
       CBDN-025.
          Move Dat-Rate (Sub-2) To Al-Rate (SUB-1)
          Add 1 To Sub-1 Sub-2.
          If Dat-Rate (Sub-2) Not = " "
           If Sub-1 Not > 60
             Go To CBDN-025.
       CBDN-030.
          Move Alpha-Rate To Ws-BRANCH-STOCK-VOL-DIR (SUB-20).
          ADD 1 TO SUB-20.
          IF WS-BRANCH-NUMBER (SUB-20) > 0
              GO TO CBDN-006.
       CBDN-040.
          MOVE WS-BRANCH-STOCK-VOL-DIR (1) TO WS-STOCK1.
          MOVE WS-BRANCH-STOCK-VOL-DIR (2) TO WS-STOCK2.
          MOVE WS-BRANCH-STOCK-VOL-DIR (3) TO WS-STOCK3.
          MOVE WS-BRANCH-STOCK-VOL-DIR (4) TO WS-STOCK4.
       CBDN-999.
          Exit.
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
           Move " " To Alpha-Rate.
           Move 0   To SUB-2.
       CDS-015.
           Add 1 To SUB-2.
           If Al-Rate (SUB-2) Not = " "
            If SUB-2 Not > 60
            Go To CDS-015.
          Subtract 1 from SUB-2.
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
           CALL "OPENFILE" USING   F-ERROR5
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
           CALL "OPENFILE" USING   F-ERROR5
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
           CALL "OPENFILE" USING   F-ERROR5
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
           CALL "OPENFILE" USING   F-ERROR5
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
               GO TO OBS-900.
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
       OPEN-005.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "SLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
               
           PERFORM READ-PARAM-NEXT.
           PERFORM READ-PARAMETER.
           CLOSE PARAMETER-FILE.
       OPEN-035.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "MAIN STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-035.
       OPEN-050.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE H1-1-DATE
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-999.
           EXIT.
      *   
       END-OFF SECTION.
       END-000.
           IF LINE-CNT = 66
               PERFORM PRR-010
              MOVE "NOTHING TO PRINT IN THAT RANGE" TO PRINT-REC
              WRITE PRINT-REC.
           IF WS-BELOW-OVER = "O"
              MOVE WS-STORE          TO T-CAT
              MOVE WS-CATVALUE       TO T-VALUE
              WRITE PRINT-REC FROM TOTAL-LINE
              MOVE " "               TO TOTAL-LINE
              ADD WS-CATVALUE        TO WS-TOTVALUE
              MOVE WS-TOTVALUE       TO T-VALUE
              MOVE "TOTAL EXCESS: R" TO T-NAME
              WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " "                  TO PRINT-REC.
           WRITE PRINT-REC.
           IF WS-BELOW-OVER = "B"
              MOVE "'*' = ON HAND + ON ORDER < B/ORDER" TO PRINT-REC
              WRITE PRINT-REC.
           IF WS-BELOW-OVER = "B"
            IF WS-CHECK-GROUP = "Y"
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC
              MOVE 
              "   O/L = BRANCH OFFLINE AT TIME OF REPORT" TO PRINT-REC
              WRITE PRINT-REC
              MOVE 
              "NONSTK = NON STOCK ITEM IN THAT BRANCH" TO PRINT-REC
              WRITE PRINT-REC.
              
              
           IF WS-F-L = "A"
              MOVE "ALL ITEMS PRINTED FOR THIS RUN." TO PRINT-REC
              WRITE PRINT-REC.
           IF WS-F-L = "F"
              MOVE "ONLY ITEMS WITH FOREIGN COST PRINTED FOR THIS RUN."
               TO PRINT-REC
              WRITE PRINT-REC.
           IF WS-F-L = "L"
              MOVE "ONLY LOCAL ITEMS PRINTED FOR THIS RUN."
               TO PRINT-REC
              WRITE PRINT-REC.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE STOCK-MASTER.
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
       Copy "Error4Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
