        IDENTIFICATION DIVISION.
        PROGRAM-ID. StPrLoRp.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStNewPrices".
          Copy "SelectStChanges".
          Copy "SelectSlParameter".
          Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRICELIST-NAME
               FILE STATUS IS WS-SPOOL-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStNewPrice.
           COPY ChlfdStockChanges.
           COPY ChlfdDaily.
           COPY ChlfdParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(255).
      *
       WORKING-STORAGE SECTION.
       77  WS-FOUND             PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  W-CALC               PIC 9(8) VALUE 0.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-MARGIN            PIC S9(5)V99 VALUE 0.
       77  WS-CAT-NEW-PAGE      PIC X VALUE " ".
       77  WS-PRINT-COSTS       PIC X VALUE " ".
       77  WS-FLAG-MAX          PIC X VALUE " ".
       77  WS-QTY-ZERO          PIC X VALUE " ".
       77  WS-RANGE7            PIC X(8) VALUE " ".
       77  WS-DATE-RANGE        PIC X VALUE " ".
       77  WS-TYPE-OF-TRANS     PIC X VALUE " ".
       77  WS-PRINT-OLD-PRICE   PIC X VALUE " ".
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-COPIES            PIC 99 VALUE 0.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-STNWPR-STATUS.
           03  WS-STNWPR-ST1      PIC 99.
       01  WS-SPOOL-STATUS.
           03  WS-SPOOL-ST1       PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-STCHANGE-STATUS.
           03  WS-STCHANGE-ST1    PIC 99.
       01  WS-PRICELIST-NAME.
           03  WS-PRICELIST       PIC X(24) VALUE 
                       "/ctools/spl/PriceListsCo".
           03  WS-PRICELIST-CO    PIC XX.  
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST       PIC X(20) VALUE " ".
           03  WS-DAILY-2ND       PIC X(20) VALUE " ".
           03  WS-DAILY-3RD       PIC X(20) VALUE " ".
           03  WS-DAILY-4TH       PIC X(20) VALUE " ".
       01  WS-NO-COPIES-GRP.
           03  WS-NO-COPIES       PIC 99 VALUE 0.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "DATE :".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(15) VALUE " ".
           03  FILLER         PIC X(19) VALUE "P R I C E   L I S T".
           03  FILLER         PIC X(19) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(54) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(32) VALUE " ".
           03  FILLER         PIC X(19) VALUE ALL "*".
           03  FILLER         PIC X(81) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(22) VALUE " ".
           03  FILLER         PIC X(10) VALUE "CATEGORY:".
           03  H3-CATEGORY    PIC X(3) VALUE " ".
           03  FILLER         PIC X(81) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(2) VALUE " ".
           03  FILLER         PIC X(16) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(41) VALUE "DESCRIPTION".
           03  FILLER         PIC X(14) VALUE "UNIT  BIN".
           03  FILLER         PIC X(59) VALUE "PRICE".
       01  HEAD4-1.
           03  FILLER         PIC X(2) VALUE " ".
           03  FILLER         PIC X(16) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(41) VALUE "DESCRIPTION".
           03  FILLER         PIC X(14) VALUE "UNIT BIN".
           03  FILLER         PIC X(42) VALUE "PRICE     COST".
       01  HEAD4-2.
           03  FILLER         PIC X(2) VALUE " ".
           03  FILLER         PIC X(16) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(41) VALUE "DESCRIPTION".
           03  FILLER         PIC X(14) VALUE "UNIT BIN".
           03  FILLER         PIC X(42) VALUE
            "PRICE      COST OLD-PRICE".
       01  HEAD4-3.
           03  FILLER         PIC X(2) VALUE " ".
           03  FILLER         PIC X(16) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(41) VALUE "DESCRIPTION".
           03  FILLER         PIC X(14) VALUE "UNIT BIN".
           03  FILLER         PIC X(42) VALUE
            "PRICE OLD-PRICE  MARGIN %".
       01  DETAIL-LINE.
           03  D-MAX          PIC X(2).
           03  D-STOCKNO      PIC X(16).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(21).
           03  D-UOM          PIC X(5).
           03  D-BIN          PIC X(5).
           03  D-PRICE        PIC Z(5)9.99.
           03  FILLER         PIC X VALUE " ".
           03  D-COST         PIC Z(5)9.99 BLANK WHEN ZERO.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-OLD-PRICE    PIC Z(5)9.99- BLANK WHEN ZERO.
           03  FILLER         PIC X(35) VALUE " ".
       01  DATE-LINE.
           03  DATE-LINE1     PIC X(23) VALUE " ".
           03  DL-DATE        PIC X(10).
           03  DATE-LINE2     PIC X(23) VALUE " ".
       Copy "WsDateInfo".
       Copy "WStore".
      *
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "** STOCK PRICELIST REPORT BY NUMBER **" AT POS
           MOVE 415 TO POS
           DISPLAY "**************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".

           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN
           PERFORM DISPLAY-FORM
           PERFORM GET-DATA
           PERFORM OPEN-DATA-FILES.
       CONTROL-030.
           PERFORM ERROR-020.
       CONTROL-032.
           IF WS-TYPE-OF-TRANS NOT = "P" AND NOT = "R"
               PERFORM NUMBER-OF-COPIES
               GO TO CONTROL-950.

           MOVE WS-CO-NUMBER      TO WS-PRICELIST-CO.
           MOVE WS-PRINTER        TO WS-PRINTER-SAVE.
           MOVE WS-PRICELIST-NAME TO WS-PRINTER. 
           OPEN OUTPUT PRINT-FILE.
      *     IF WS-SPOOL-ST1 NOT = 0
      *        MOVE "SPOOLER STATUS BUSY ON OPEN, 'ESC' TO RETRY."
      *        TO WS-MESSAGE
      *        PERFORM ERROR1-000
      *        MOVE WS-SPOOL-ST1 TO WS-MESSAGE
      *        PERFORM ERROR-MESSAGE
      *        PERFORM ERROR1-020
      *        GO TO CONTROL-032.
      *     MOVE WTELL-PAUSE TO PRINT-REC.
      *     WRITE PRINT-REC.
      *     MOVE " " TO PRINT-REC.
      *     IF WS-TYPE-OF-TRANS = "R"
      *        MOVE "  " TO PRINT-REC
      *        WRITE PRINT-REC.
           MOVE " " TO PRINT-REC.
       CONTROL-035.
           PERFORM NUMBER-OF-COPIES.
           IF WS-FOUND = " "
               MOVE "NOTHING TO PRINT IN THAT RANGE." TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
           PERFORM END-000.
           
      * NEW GO TO COMMAND FOR LINUX AS WE DO NOT HAVE CONTROL OF THE 
      * PRINTER LIKE WE HAD IN CTOS.
           CLOSE PRINT-FILE.
           GO TO CONTROL-038.
           
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC
           WRITE PRINT-REC
           MOVE W-NULL TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC
           MOVE WTELL-PAUSE TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC.
           CLOSE PRINT-FILE.
           
       CONTROL-038.    
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE STOCK-MASTER
                 STOCKCHANGE-MASTER
                 StNwPr-Master.
                 
           GO TO CONTROL-950.
       CONTROL-040.
           IF WS-FOUND NOT = " "
               PERFORM CHECK-SPOOLER.
       CONTROL-950.
           PERFORM END-900.
       CONTROL-999.
            EXIT.
      *
       GET-DATA SECTION.
       GET-030.
            MOVE "RANGE1" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
               CLOSE STOCK-MASTER
               PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"0B"
               GO TO GET-030.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-RANGE1.
            IF WS-RANGE1 = " "
               GO TO GET-030.
       GET-040.
            MOVE "RANGE2" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"0B"
               GO TO GET-040.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-RANGE2.
            IF WS-RANGE2 = " "
               GO TO GET-040.
       GET-045.
            MOVE "RANGE7" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"0B"
               GO TO GET-045.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-TYPE-OF-TRANS.
            IF WS-TYPE-OF-TRANS NOT = "P" AND NOT = "W" AND NOT = "U"
                     AND NOT = "R"
               GO TO GET-045.
            IF WS-TYPE-OF-TRANS NOT = "P" AND NOT = "R"
               GO TO GET-950.
      ************************************************************
      * P=PRINT FROM ST-MASTER,  W=WRITE NEW PRICELIST TO DISK   *
      * U=UPDATE FROM PRICELIST, R=REPRINT FROM PRICELIST        *
      ************************************************************
       GET-050.
            MOVE "COPIES" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-045.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"0B"
               GO TO GET-050.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-COPIES.
            IF WS-COPIES = 0
               GO TO GET-050.
       GET-060.
            MOVE "RANGE3" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-050.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"0B"
               GO TO GET-060.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-CAT-NEW-PAGE.
            IF WS-CAT-NEW-PAGE NOT = "Y" AND NOT = "N"
               GO TO GET-060.
       GET-070.
            MOVE "RANGE4" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-060.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"0B"
               GO TO GET-070.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-PRINT-COSTS.
            IF WS-PRINT-COSTS NOT = "Y" AND NOT = "N"
               GO TO GET-070.
       GET-080.
            MOVE "RANGE5" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-070.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"0B"
               GO TO GET-080.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-FLAG-MAX.
            IF WS-FLAG-MAX NOT = "Y" AND NOT = "N"
               GO TO GET-080.
       GET-085.
            MOVE "RANGE6" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-080.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"0B"
               GO TO GET-085.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-QTY-ZERO.
            IF WS-QTY-ZERO NOT = "Y" AND NOT = "N"
               GO TO GET-085.
       GET-090.
            MOVE "Enter N=Ignore Date, C=CreateDate, P=PriceChange."
              TO WS-MESSAGE
              PERFORM ERROR-000.
            MOVE "RANGE8" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-085.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"0B"
               GO TO GET-090.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DATE-RANGE.
            IF WS-DATE-RANGE NOT = "C" AND NOT = "N" AND NOT = "P"
               GO TO GET-090.
       GET-950.
           PERFORM ERROR-020.
            
           MOVE "DATE"       TO F-FIELDNAME
           MOVE 4            TO F-CBFIELDNAME
           MOVE DISPLAY-DATE TO F-NAMEFIELD
           MOVE 10           TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
            
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
                GO TO GET-090.
           IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"0B"
               GO TO GET-950.
           MOVE 10 TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD TO ALPHA-RATE
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
               GO TO GET-950.
           MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           IF WS-DATE-RANGE = "N"
              MOVE DISPLAY-DATE    TO H1-DATE F-NAMEFIELD
           ELSE
              MOVE DISPLAY-DATE    TO DL-DATE
              MOVE WS-DATE         TO SPLIT-DATE
              PERFORM CONVERT-SPLIT-FORMAT
      *        MOVE DISPLAY-DATE    TO H1-DATE F-NAMEFIELD
              MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.

           MOVE WS-CONVERT-DATE    TO DISPLAY-DATE
           MOVE DISPLAY-DATE       TO F-NAMEFIELD.
           MOVE 10                 TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
           
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DD         TO WS-BEG-DD WS-CH-DD.
           MOVE SPLIT-MM         TO WS-BEG-MM WS-CH-MM.
           MOVE SPLIT-YY         TO WS-BEG-YY WS-CH-YY.
           
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
               GO TO GET-950.
       GET-955.
           IF WS-TYPE-OF-TRANS NOT = "R"
               GO TO GET-960.
           PERFORM CLEAR-010.
           MOVE " " TO WS-PRINT-OLD-PRICE.
           MOVE 2625 TO POS.
           DISPLAY "PRINT OLD-PRICE, Y OR N      : [ ]" AT POS.
           MOVE 2657 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 23        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PRINT-OLD-PRICE.

      *     ACCEPT WS-PRINT-OLD-PRICE AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO GET-950.
           IF WS-PRINT-OLD-PRICE NOT = "Y" AND NOT = "N"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-955.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-960
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-955.
       GET-960.
            MOVE 2715 TO POS
            DISPLAY "The Report Is Being Compiled, Please Be Patient."
                 AT POS.
       GET-999.
            EXIT.
      *
       NUMBER-OF-COPIES SECTION.
       NOC-000.
       NOC-001.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO NOC-001.
            MOVE " " TO WS-FOUND.
            
            IF WS-TYPE-OF-TRANS = "W"
                PERFORM NOC-003
                GO TO NOC-004.
      *         PERFORM DELETE-TRANS.
       NOC-002.
            OPEN I-O STNWPR-MASTER.
            IF WS-STNWPR-ST1 NOT = 0
               MOVE 0 TO WS-STNWPR-ST1
               MOVE "NEW PRICES FILE BUSY ON OPEN I-O, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO NOC-003.
       NOC-0021.
            GO TO NOC-004.
       NOC-003.
            OPEN OUTPUT STNWPR-MASTER.
            IF WS-STNWPR-ST1 NOT = 0
               MOVE 0 TO WS-STNWPR-ST1
               MOVE "NEW PRICES BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO NOC-003.
       NOC-004.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "SLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO NOC-004.
           PERFORM READ-INVQUES-FILE.
           CLOSE PARAMETER-FILE.
       NOC-005.
            OPEN I-O STOCKCHANGE-MASTER.
            IF WS-STCHANGE-ST1 NOT = 0
               MOVE 0 TO WS-STCHANGE-ST1
               MOVE "STCHANGE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO NOC-005.
       NOC-006.
            IF WS-TYPE-OF-TRANS = "W"
                MOVE 1 TO WS-COPIES
                PERFORM WRITE-ROUTINE
                GO TO NOC-999.
            IF WS-TYPE-OF-TRANS = "U"
                MOVE 1 TO WS-COPIES
                PERFORM UPDATE-ROUTINE
                GO TO NOC-999.
       NOC-010.
            IF WS-TYPE-OF-TRANS = "R"
                PERFORM REPRINT-ROUTINE.
            IF WS-TYPE-OF-TRANS = "P"
                PERFORM PRINT-ROUTINE.
            ADD 1 TO WS-NO-COPIES.
            IF WS-NO-COPIES-GRP = WS-COPIES
               GO TO NOC-999.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC AFTER PAGE.
            MOVE 0 TO PAGE-CNT.
            MOVE 66 TO LINE-CNT.
            GO TO NOC-010.
       NOC-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-002.
           MOVE WS-RANGE1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
           MOVE "   " TO WS-STORE.
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
           IF ST-STOCKNUMBER < WS-RANGE1
              GO TO PRR-005.
           IF ST-STOCKNUMBER > WS-RANGE2
              GO TO PRR-999.
              
           IF WS-STORE = "   "
              MOVE ST-CATEGORY TO WS-STORE.
           MOVE 2620 TO POS
           DISPLAY "StockNumber Being Read:" AT POS
           ADD 24 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

           IF ST-CATEGORY NOT = WS-STORE
              MOVE ST-CATEGORY TO WS-STORE
            IF WS-CAT-NEW-PAGE = "Y"
              PERFORM PRR-050
            ELSE
             IF LINE-CNT < 59
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC
              ADD 1 TO LINE-CNT
             ELSE
              PERFORM PRR-050.

           IF WS-DATE-RANGE = "C"
            IF ST-DATE-CREATED < WS-BEG-DATE
              GO TO PRR-005.
           IF WS-DATE-RANGE = "P"
            IF ST-LASTPRICECHANGE < WS-BEG-DATE
              GO TO PRR-005.
       PRR-010.
           IF LINE-CNT > 60
               PERFORM PRR-050.
       PRR-020.
           IF WS-FOUND = " "
               MOVE "Y" TO WS-FOUND.
           MOVE ST-STOCKNUMBER   TO D-STOCKNO.
           IF WS-FLAG-MAX = "Y"
            IF ST-MAXIMUMLEVEL < (ST-QTYONHAND + ST-QTYONRESERVE)
                                   - ST-QTYONBORDER
              MOVE "*"           TO D-MAX.
           IF WS-QTY-ZERO = "Y"
            IF ST-QTYONHAND = 0
             IF ST-QTYONRESERVE = 0
             GO TO PRR-005.
           MOVE ST-DESCRIPTION1  TO D-DESC1
           MOVE ST-DESCRIPTION2  TO D-DESC2
           MOVE ST-UNITOFMEASURE TO D-UOM
           MOVE ST-BINLOCATION   TO D-BIN
           MOVE ST-PRICE         TO D-PRICE.
           IF WS-PRINT-COSTS = "Y"
             MOVE ST-AVERAGECOST TO D-COST.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           ADD 1 TO LINE-CNT.
           GO TO PRR-005.
       PRR-050.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.
            IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE AFTER 1
           ELSE
      *         WRITE PRINT-REC BEFORE PAGE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC FROM HEAD1 AFTER 1.
           MOVE " " TO PRINT-REC.
           IF WS-CAT-NEW-PAGE = "Y"
               WRITE PRINT-REC FROM HEAD2 AFTER 1
               MOVE " " TO PRINT-REC
               MOVE WS-STORE TO H3-CATEGORY
               WRITE PRINT-REC FROM HEAD3 AFTER 1
               MOVE " " TO PRINT-REC
             IF WS-PRINT-COSTS = "Y"
               WRITE PRINT-REC FROM HEAD4-1 AFTER 1
               MOVE " " TO PRINT-REC
             ELSE
               WRITE PRINT-REC FROM HEAD4-1 AFTER 1
               MOVE " " TO PRINT-REC.
           IF WS-CAT-NEW-PAGE = "N"
            IF WS-PRINT-COSTS = "Y"
               WRITE PRINT-REC FROM HEAD4-1 AFTER 1
               MOVE " " TO PRINT-REC
            ELSE
               WRITE PRINT-REC FROM HEAD4 AFTER 1
               MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           IF WS-CAT-NEW-PAGE = "Y"
               MOVE 7 TO LINE-CNT
           ELSE
               MOVE 4 TO LINE-CNT.
       PRR-999.
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
               MOVE "N" TO INVQUES-STOCK-CHANGE
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
       RINVQUES-999.
            EXIT.
      *
       READ-STOCK SECTION.
       RS-005.
           READ STOCK-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
              GO TO RS-999.
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
       REPRINT-ROUTINE SECTION.
       RPRR-002.
           CLOSE STNWPR-MASTER.
           PERFORM NOC-002.
           MOVE "   " TO WS-STORE.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
           MOVE WS-RANGE1 TO STNWPR-STOCKNUMBER.
           START STNWPR-MASTER KEY NOT < STNWPR-KEY
              INVALID KEY NEXT SENTENCE.
       RPRR-005.
           READ STNWPR-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-STNWPR-ST1 = 10
              GO TO RPRR-999.
           IF WS-STNWPR-ST1 NOT = 0
              MOVE "STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STNWPR-ST1
               GO TO RPRR-005.
           IF STNWPR-STOCKNUMBER < WS-RANGE1
              GO TO RPRR-005.
           IF STNWPR-STOCKNUMBER > WS-RANGE2
              GO TO RPRR-999.
              
           MOVE STNWPR-STOCKNUMBER TO ST-STOCKNUMBER.
           PERFORM READ-STOCK.
           IF WS-STOCK-ST1 NOT = 0
              MOVE
              "STOCK NUMBER DOES NOT EXIST, 'ESC' To READ-NEXT ITEM."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE ST-STOCKNUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RPRR-005.
           
           IF WS-STORE = "   "
              MOVE ST-CATEGORY TO WS-STORE.
           MOVE 2620 TO POS
           DISPLAY "StockNumber Being Read:" AT POS
           ADD 24 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

           IF ST-CATEGORY NOT = WS-STORE
              MOVE ST-CATEGORY TO WS-STORE
            IF WS-CAT-NEW-PAGE = "Y"
              PERFORM RPRR-050
            ELSE
             IF LINE-CNT < 59
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC
              ADD 1 TO LINE-CNT
             ELSE
              PERFORM RPRR-050.
       RPRR-010.
            IF LINE-CNT > 60
               PERFORM RPRR-050.
       RPRR-020.
           IF WS-FOUND = " "
               MOVE "Y" TO WS-FOUND.
           MOVE ST-STOCKNUMBER   TO D-STOCKNO.
           IF WS-FLAG-MAX = "Y"
            IF ST-MAXIMUMLEVEL < (ST-QTYONHAND + ST-QTYONRESERVE)
                                   - ST-QTYONBORDER
              MOVE "*"           TO D-MAX.
           IF WS-QTY-ZERO = "Y"
            IF ST-QTYONHAND = 0
             IF ST-QTYONRESERVE = 0
             GO TO RPRR-005.
           MOVE ST-DESCRIPTION1  TO D-DESC1
           MOVE ST-DESCRIPTION2  TO D-DESC2
           MOVE ST-UNITOFMEASURE TO D-UOM
           MOVE ST-BINLOCATION   TO D-BIN
           MOVE STNWPR-PRICE     TO D-PRICE.
           
           COMPUTE WS-MARGIN = ((STNWPR-PRICE - ST-AVERAGECOST)
                      / ST-AVERAGECOST) * 100.
           
           IF WS-PRINT-COSTS = "Y"
            IF WS-PRINT-OLD-PRICE = "Y"
             MOVE ST-AVERAGECOST  TO D-COST
             MOVE ST-PRICE        TO D-OLD-PRICE.
           IF WS-PRINT-COSTS = "N"
            IF WS-PRINT-OLD-PRICE = "Y"
             MOVE ST-PRICE        TO D-COST
             MOVE WS-MARGIN       TO D-OLD-PRICE.
             
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           ADD 1 TO LINE-CNT.
           GO TO RPRR-005.
       RPRR-050.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.
           IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE AFTER 1
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC.
           IF WS-CAT-NEW-PAGE = "Y"
               WRITE PRINT-REC FROM HEAD2 AFTER 1
               MOVE " " TO PRINT-REC
               MOVE WS-STORE TO H3-CATEGORY
               WRITE PRINT-REC FROM HEAD3 AFTER 1
               MOVE " " TO PRINT-REC
               
             IF WS-PRINT-COSTS = "N"
              IF WS-PRINT-OLD-PRICE = "N"
               WRITE PRINT-REC FROM HEAD4 AFTER 1
               MOVE " " TO PRINT-REC.
             IF WS-PRINT-COSTS = "Y"
              IF WS-PRINT-OLD-PRICE = "N"
               WRITE PRINT-REC FROM HEAD4-1 AFTER 1
               MOVE " " TO PRINT-REC.
             IF WS-PRINT-COSTS = "Y"
              IF WS-PRINT-OLD-PRICE = "Y"
               WRITE PRINT-REC FROM HEAD4-2 AFTER 1
               MOVE " " TO PRINT-REC.
             IF WS-PRINT-COSTS = "N"
              IF WS-PRINT-OLD-PRICE = "Y"
               WRITE PRINT-REC FROM HEAD4-3 AFTER 1
               MOVE " " TO PRINT-REC.

      *    IF WS-CAT-NEW-PAGE = "N"
      *     IF WS-PRINT-COSTS = "Y"
      *        WRITE PRINT-REC FROM HEAD4-1 AFTER 1
      *        MOVE " " TO PRINT-REC
      *     ELSE
      *        WRITE PRINT-REC FROM HEAD4 AFTER 1
      *        MOVE " " TO PRINT-REC
      *    WRITE PRINT-REC AFTER 1.
           IF WS-CAT-NEW-PAGE = "Y"
               MOVE 7 TO LINE-CNT
           ELSE
               MOVE 4 TO LINE-CNT.
       RPRR-999.
           EXIT.
      *
       WRITE-ROUTINE SECTION.
       WRITE-002.
           MOVE WS-RANGE1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       WRITE-005.
           READ STOCK-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
              GO TO WRITE-999.
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
               GO TO WRITE-005.
           IF ST-STOCKNUMBER < WS-RANGE1
              GO TO WRITE-005.
           IF ST-STOCKNUMBER > WS-RANGE2
              GO TO WRITE-999.
           MOVE 2620 TO POS
           DISPLAY "StockNumber Being Read:" AT POS
           ADD 24 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
       WRITE-010.
           MOVE ST-STOCKNUMBER   TO STNWPR-STOCKNUMBER
           MOVE ST-PRICE         TO STNWPR-PRICE
           MOVE ST-ANALYSIS      TO STNWPR-ANALYSIS
           MOVE SPLIT-DATE       TO STNWPR-DATE.
       WRITE-020.
           WRITE STNWPR-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-STNWPR-ST1 = 23 OR 35 OR 49
               GO TO WRITE-030.
           IF WS-STNWPR-ST1 NOT = 0
               MOVE "WRITING OF NEW PRICE LIST IN ERROR, 'ESC' TO RETRY"
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STNWPR-ST1
               GO TO WRITE-020.
           GO TO WRITE-005.
       WRITE-030.
           REWRITE STNWPR-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-STNWPR-ST1 = 23 OR 35 OR 49
               GO TO WRITE-020.
           IF WS-STNWPR-ST1 NOT = 0
            MOVE "REWRITING OF NEW PRICE LIST IN ERROR, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STNWPR-ST1
               GO TO WRITE-030.
       WRITE-050.
           GO TO WRITE-005.
       WRITE-999.
           EXIT.
      *
       UPDATE-ROUTINE SECTION.
       UPDATE-002.
           MOVE WS-RANGE1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       UPDATE-005.
           READ STOCK-MASTER NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
              GO TO UPDATE-999.
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
               GO TO UPDATE-005.
           IF ST-STOCKNUMBER < WS-RANGE1
              GO TO UPDATE-005.
           IF ST-STOCKNUMBER > WS-RANGE2
              GO TO UPDATE-999.
       UPDATE-010.
           MOVE 2620 TO POS
           DISPLAY "StockNumber Being Read:" AT POS
           ADD 24 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
           MOVE ST-STOCKNUMBER   TO STNWPR-STOCKNUMBER.
       UPDATE-020.
           READ STNWPR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STNWPR-ST1 = 23 OR 35 OR 49
      *          MOVE "STOCK RECORD:       " TO WS-DAILY-1ST
      *          MOVE ST-STOCKNUMBER         TO WS-DAILY-2ND
      *          MOVE "NOT UPDATED         " TO WS-DAILY-3RD
      *          MOVE "ON PRICE-UPDATE.    " TO WS-DAILY-4TH
      *          PERFORM WRITE-DAILY
                GO TO UPDATE-005.
           IF WS-STNWPR-ST1 NOT = 0
            MOVE "UPDATING OF NEW PRICES BUSY ON READ, 'ESC' TO  RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               GO TO UPDATE-020.
               
           IF STNWPR-PRICE NOT = ST-PRICE
               MOVE ST-PRICE     TO ST-OLDPRICE
               MOVE STNWPR-PRICE TO ST-PRICE
               MOVE SPLIT-DATE   TO ST-LASTPRICECHANGE
               GO TO UPDATE-030.
           GO TO UPDATE-005.
       UPDATE-030.
           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               GO TO UPDATE-020.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "REWRITING OF STOCKFILE IN ERROR, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               GO TO UPDATE-030.
           IF INVQUES-STOCK-CHANGE = "Y"
               PERFORM WRITE-STOCK-CHANGES.
           GO TO UPDATE-005.
       UPDATE-999.
           EXIT.
      *
       WRITE-STOCK-CHANGES SECTION.
       WSTCH-000.
             MOVE ST-STOCKNUMBER TO STCH-STOCKNUMBER.
             START STOCKCHANGE-MASTER KEY NOT < STCH-KEY
               INVALID KEY NEXT SENTENCE.
       WSTCH-005.
             READ STOCKCHANGE-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STCHANGE-ST1 = 23 OR 35 OR 49
                GO TO WSTCH-006.
             IF WS-STCHANGE-ST1 NOT = 0
                MOVE "STOCKCHANGE BUSY ON READ-LOCK, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCHANGE-ST1
               GO TO WSTCH-005.
       WSTCH-006.
          MOVE ST-DESCRIPTION1     TO STCH-DESCRIPTION1
          MOVE ST-DESCRIPTION2     TO STCH-DESCRIPTION2
          MOVE ST-CATEGORY         TO STCH-CATEGORY
          MOVE ST-SUPPLIER         TO STCH-SUPPLIER
          MOVE ST-FOREIGNCOST      TO STCH-FOREIGNCOST
          MOVE ST-PRICE            TO STCH-PRICE
          MOVE ST-UNITOFMEASURE    TO STCH-UNITOFMEASURE
          MOVE ST-DISCOUNT1        TO STCH-DISCOUNT1
          MOVE ST-DISCOUNT2        TO STCH-DISCOUNT2
          MOVE ST-DISCOUNT3        TO STCH-DISCOUNT3
          MOVE ST-DISCOUNT4        TO STCH-DISCOUNT4
          MOVE ST-DISCOUNT5        TO STCH-DISCOUNT5
          MOVE ST-DISCOUNT6        TO STCH-DISCOUNT6
          MOVE ST-DISCOUNT7        TO STCH-DISCOUNT7
          MOVE ST-DISCOUNT8        TO STCH-DISCOUNT8
          MOVE ST-DISCOUNT9        TO STCH-DISCOUNT9
          MOVE ST-MINBUYQTY        TO STCH-MINBUYQTY
          MOVE ST-ANALYSIS         TO STCH-ANALYSIS
          MOVE ST-DUTYPERCENT      TO STCH-DUTYPERCENT
          MOVE ST-DUTYTARIFF       TO STCH-DUTYTARIFF
          MOVE ST-SURCHARGE        TO STCH-SURCHARGE
          MOVE ST-PERMIT           TO STCH-PERMIT
          MOVE "C"                 TO STCH-TYPE-OF-CHANGE.
       WSTCH-010.
           IF WS-STCHANGE-ST1 = 23 OR 35 OR 49
              GO TO WSTCH-020.
           REWRITE STOCKCHANGE-RECORD
              INVALID KEY NEXT SENTENCE.
           IF WS-STCHANGE-ST1 NOT = 0
              MOVE "STOCKCHANGE BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCHANGE-ST1
               GO TO WSTCH-010.
          GO TO WSTCH-999.
       WSTCH-020.
          WRITE STOCKCHANGE-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STCHANGE-ST1 NOT = 0
              MOVE "STOCKCHANGE BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCHANGE-ST1
               GO TO WSTCH-020.
       WSTCH-999.
           EXIT.
      *
       CHECK-SPOOLER SECTION.
       CP-000.              
           PERFORM QUEUE-PRINT-FILE
           MOVE SPACE TO W-SPOOLST
           MOVE SPACE TO W-SPOOLST2
           PERFORM CHECK-FOR-PAUSE
           MOVE " " TO WS-MESSAGE
           MOVE 2615 TO POS
           DISPLAY WS-MESSAGE AT POS.

           MOVE " Load The Special 'PRICELIST' Paper," TO WS-MESSAGE.
           MOVE "Then Press 'ESC' To BEGIN Printing" TO WS-MESSAGE1.
           PERFORM ERROR1-MESSAGE.

           PERFORM SEND-CONTROL-CHAR.
           MOVE 2615 TO POS.
           DISPLAY "Printing Of The PriceLists In Progress ......"
               AT POS.
      *
      * PRINTING COMPLETE
      *
           PERFORM CHECK-PAUSE-PRINT.
           MOVE " " TO WS-MESSAGE.
           MOVE 2615 TO POS.
           DISPLAY WS-MESSAGE AT POS.
       
           MOVE "Load Normal Paper, The Printing Has FINISHED,"
               TO WS-MESSAGE.
           MOVE "    Press 'ESC' To Release The Printer."
               TO WS-MESSAGE1.
           PERFORM ERROR1-MESSAGE.
       
           PERFORM SEND-CONTROL-CHAR.
       CP-999.
           EXIT.
      *
       DELETE-TRANS SECTION.
       DST-010.
           PERFORM CDS-005.
           Move Ws-StNewPrice To Alpha-Rate
           PERFORM CDS-015.
           
           MOVE Ws-StNewPrice  TO F-FILENAME
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
       DST-015.
           PERFORM CDS-005.
           Move Ws-StNewPrice To Alpha-Rate.
           PERFORM CDS-015.
           PERFORM CDS-010.
       DST-025.
           MOVE Alpha-Rate       TO F-FILENAME
           MOVE Sub-1            TO F-CBFILENAME.
           CALL "OPENFILE" USING   F-ERROR1
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE-MM.
           CALL "DELETEFILE" USING F-ERROR1
                                   F-FH.
           If F-ERROR1 = 0
              GO TO DST-999.
       DST-025.
           PERFORM FIND-IND-DIR.
             
           MOVE Alpha-Rate       TO F-FILENAME
           MOVE Sub-2            TO F-CBFILENAME.
           CALL "OPENFILE" USING   F-ERROR1
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE-MM.
           CALL "DELETEFILE" USING F-ERROR1
                                   F-FH.
           If F-ERROR1 NOT = 0
             MOVE "THE INDEX DIR CANNOR BE FOUND, CANCEL TO CONTINUE."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE.
       DST-999.
           EXIT.
      *
       CHECK-DATA-SIZE SECTION.         
       CDS-005.
           Move " " To Alpha-Rate.
           Move 0   To Sub-1.
       CDS-010.
           Add 1 to Sub-1.
           Move "." to Al-Rate (Sub-1)
           Add 1 to Sub-1
           Move "I" to Al-Rate (Sub-1)
           Add 1 to Sub-1
           Move "n" to Al-Rate (Sub-1)
           Add 1 to Sub-1
           Move "d" to Al-Rate (Sub-1).
       CDS-015.
           Add 1 To Sub-1.
           If Al-Rate (Sub-1) Not = " "
            If Sub-1 Not > 60
            Go To CDS-015.
          Subtract 1 from Sub-1.
       CDS-999.
          EXIT.
      *
       FIND-IND-DIR SECTION.         
       FID-005.
           MOVE 0 TO SUB-1 SUB-2.
       FID-015.
           Add 1 To Sub-1 SUB-2.
           If Al-Rate (Sub-1) Not = ">"
            If Sub-1 Not > 60
            MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2)
            Go To FID-015.
       FID-025.
           Move "I" to DAT-Rate (Sub-2)
           Add 1 to Sub-2
           Move "n" to DAT-Rate (Sub-2)
           Add 1 to Sub-2
           Move "d" to DAT-Rate (Sub-2)
           Add 1 to Sub-2.
           Move Al-Rate (Sub-1) TO DAT-RATE (SUB-2).
       FID-035.
           Add 1 To Sub-1 SUB-2.
           If Al-Rate (Sub-1) Not = " "
            If Sub-1 Not > 60
            MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2)
            Go To FID-035.
            
           SUBTRACT 1 FROM SUB-1 SUB-2.
           MOVE SPACES TO ALPHA-RATE
           MOVE DATA-RATE TO ALPHA-RATE.
       FID-999.
          EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StPrLoRp"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF WS-DATE-RANGE = "C"
             MOVE "** ONLY CREATE-DATES > " TO DATE-LINE1
             MOVE " HAVE BEEN PRINTED **" TO DATE-LINE2
             WRITE PRINT-REC FROM DATE-LINE AFTER 2.
           IF WS-DATE-RANGE = "P"
             MOVE "* ONLY PRICE-CHANGES >" TO DATE-LINE1
             MOVE " HAVE BEEN PRINTED **" TO DATE-LINE2
             WRITE PRINT-REC FROM DATE-LINE AFTER 2.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-900.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "CheckForPause".
       Copy "QueuePrintFilePricelists".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "WriteDailyExcep1".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
