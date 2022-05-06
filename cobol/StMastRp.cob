        IDENTIFICATION DIVISION.
        PROGRAM-ID. StMastRp.
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
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-ANALYSIS          PIC X VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(64) VALUE
           "S T O C K   M A S T E R   L I S T".
           03  FILLER         PIC X(9) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(45) VALUE " ".
           03  FILLER         PIC X(33) VALUE ALL "*".
           03  FILLER         PIC X(54) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(17) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(22) VALUE "DESCRIPTION".
           03  FILLER         PIC X(10) VALUE "CATEGORY".
           03  FILLER         PIC X(10) VALUE "BIN".
           03  FILLER         PIC X(8) VALUE "DISC.1".
           03  FILLER         PIC X(8) VALUE "DISC.6".
           03  FILLER         PIC X(10) VALUE "AVE. COST".
           03  FILLER         PIC X(8) VALUE "MAXIMUM".
           03  FILLER         PIC X(8) VALUE " SOLD".
           03  FILLER         PIC X(10) VALUE "UNIT PTD".
           03  FILLER         PIC X(10) VALUE "RAND PTD".
           03  FILLER         PIC X(11) VALUE "COST PTD".
       01  HEAD4.
           03  FILLER         PIC X(39) VALUE " ".
           03  FILLER         PIC X(10) VALUE "SUPPLIER".
           03  FILLER         PIC X(10) VALUE "PERMIT".
           03  FILLER         PIC X(8) VALUE "DISC.2".
           03  FILLER         PIC X(8) VALUE "DISC.7".
           03  FILLER         PIC X(10) VALUE "LAST COST".
           03  FILLER         PIC X(8) VALUE "MINIMUM".
           03  FILLER         PIC X(8) VALUE "RECEIVE".
           03  FILLER         PIC X(10) VALUE "UNIT YTD".
           03  FILLER         PIC X(10) VALUE "RAND YTD".
           03  FILLER         PIC X(11) VALUE "COST YTD".
       01  HEAD5.
           03  FILLER         PIC X(17) VALUE " ".
           03  FILLER         PIC X(22) VALUE "QTY ST-TAKE".
           03  FILLER         PIC X(10) VALUE "PRICE".
           03  FILLER         PIC X(10) VALUE "ANALYSIS".
           03  FILLER         PIC X(8) VALUE "DISC.3".
           03  FILLER         PIC X(8) VALUE "DISC.8".
           03  FILLER         PIC X(10) VALUE "DUTY %".
           03  FILLER         PIC X(8) VALUE "ON HAND".
           03  FILLER         PIC X(8) VALUE "ORDERED".
           03  FILLER         PIC X(10) VALUE "UNIT LAST".
           03  FILLER         PIC X(10) VALUE "RAND LAST".
           03  FILLER         PIC X(11) VALUE "COST LAST".
       01  HEAD6.
           03  FILLER         PIC X(39) VALUE " ".
           03  FILLER         PIC X(10) VALUE "UNIT".
           03  FILLER         PIC X(10) VALUE "FOREIGN".
           03  FILLER         PIC X(8) VALUE "DISC.4".
           03  FILLER         PIC X(8) VALUE "DISC.9".
           03  FILLER         PIC X(10) VALUE "TARIFF".
           03  FILLER         PIC X(8) VALUE "ORDER".
           03  FILLER         PIC X(8) VALUE "ST-TAKE".
           03  FILLER         PIC X(10) VALUE "RECVD MTD".
           03  FILLER         PIC X(10) VALUE "RECVD YTD".
           03  FILLER         PIC X(11) VALUE "RECVD LAST".
       01  HEAD7.
           03  FILLER         PIC X(39) VALUE " ".
           03  FILLER         PIC X(10) VALUE "OLD PRICE".
           03  FILLER         PIC X(10) VALUE "CURRENCY".
           03  FILLER         PIC X(8) VALUE "DISC.5".
           03  FILLER         PIC X(8) VALUE "SUP-DISC".
           03  FILLER         PIC X(10) VALUE "SURCHARGE".
           03  FILLER         PIC X(8) VALUE "BORDER".
           03  FILLER         PIC X(8) VALUE "BUY QTY".
           03  FILLER         PIC X(10) VALUE " ADJ MTD".
           03  FILLER         PIC X(10) VALUE " ADJ YTD".
           03  FILLER         PIC X(11) VALUE " ADJ LAST".
       01  HEAD8.
           03  FILLER         PIC X(130) VALUE ALL "-".
           03  FILLER         PIC X(2) VALUE " ".
       01  DETAIL-LINE1.
           03  D-STOCK        PIC X(17).
           03  D-DESC1        PIC X(22).
           03  D-CAT          PIC X(10).
           03  D-BIN          PIC X(10).
           03  D-DISC1        PIC Z9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-DISC6        PIC Z9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-AVE          PIC Z(4)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-MAX          PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SALEDATE     PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-MTDUNIT      PIC Z(5)9-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-MTDSALE      PIC Z(6)9.99-.
           03  D-MTDCOST      PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
       01  DETAIL-LINE2.
           03  FILLER         PIC X(17) VALUE " ".
           03  D-DESC2        PIC X(22).
           03  D-SUPPLIER     PIC X(10).
           03  D-PERMIT       PIC X(10).
           03  D-DISC2        PIC Z(1)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-DISC7        PIC Z(1)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-LAST         PIC Z(4)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-MIN          PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-RECEIPTDATE  PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-YTDUNIT      PIC Z(5)9-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-YTDSALE      PIC Z(6)9.99-.
           03  D-YTDCOST      PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
       01  DETAIL-LINE3.
           03  FILLER         PIC X(17) VALUE " ".
           03  D-ST-TAKE      PIC Z(5)9.
           03  FILLER         PIC X(15) VALUE " ".
           03  D-PRICE        PIC Z(5)9.99.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-ANALYSIS     PIC X(10).
           03  D-DISC3        PIC Z(1)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-DISC8        PIC Z(1)9.99.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-PERC         PIC Z9.9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-HAND         PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-ORDERDATE    PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-LYUNIT       PIC Z(5)9-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-LYSALE       PIC Z(6)9.99-.
           03  D-LYCOST       PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
       01  DETAIL-LINE4.
           03  FILLER         PIC X(39) VALUE " ".
           03  D-UNIT         PIC X(7).
           03  D-FOREIGN      PIC Z(6)9.999.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-DISC4        PIC Z9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-DISC9        PIC Z9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-TARIFF       PIC X(9).
           03  D-ORDER        PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-OLDPRICEDATE PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-REC-MTD      PIC Z(5)9-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-REC-YTD      PIC Z(5)9-.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-REC-LAST     PIC Z(5)9-.
           03  FILLER         PIC X(5) VALUE " ".
       01  DETAIL-LINE5.
           03  FILLER         PIC X(38) VALUE " ".
           03  D-OLDPRICE     PIC Z(5)9.99.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-CURRENCY     PIC X(6).
           03  FILLER         PIC X(4) VALUE " ".
           03  D-DISC5        PIC Z9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-SUPP-DISC    PIC Z9.99.
           03  FILLER         PIC X(6) VALUE " ".
           03  D-SURCHARGE    PIC Z9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-BORDER       PIC Z(5)9.
           03  FILLER         PIC X(5) VALUE " ".
           03  D-MINBUY       PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-ADJ-MTD      PIC Z(5)9-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-ADJ-YTD      PIC Z(5)9-.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-ADJ-LAST     PIC Z(5)9-.
           03  FILLER         PIC X(5) VALUE " ".
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
           DISPLAY "** STOCK MASTER LIST **" AT POS
           MOVE 421 TO POS
           DISPLAY "***********************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO WS-RANGE1 WS-RANGE2.
            MOVE 1010 TO POS.
            DISPLAY "            FROM STOCK NUMBER: [               ]"
                      AT POS.
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
            MOVE 1210 TO POS.
            DISPLAY "              TO STOCK NUMBER: [               ]"
                      AT POS.
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
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
            MOVE 1610 TO POS.
            DISPLAY "Leave BLANK to print 'ALL' stock items"
                 AT POS.
            MOVE 1710 TO POS.
            DISPLAY "Enter 'P' to print only items needing a PERMIT"
                 AT POS.
       GET-020.
            MOVE 1410 TO POS.
            DISPLAY "         ENTER ANALYSIS FIELD: [ ]" AT POS.
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
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO GET-050
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-050.
            MOVE 2710 TO POS.
            DISPLAY "Report Is Being Compiled, Please Be Patient."
            AT POS.
       GET-060.
            MOVE 2810 TO POS.
            DISPLAY "                                        " AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE WS-RANGE1 TO ST-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY.
       PRR-002.
            READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
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
               GO TO PRR-999.

           MOVE 2510 TO POS
           DISPLAY "STOCKNUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

            IF WS-ANALYSIS = " "
               GO TO PRR-010.
            IF WS-ANALYSIS = "P"
             IF ST-PERMIT = "Y"
               GO TO PRR-010.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-020.
            ADD 1         TO PAGE-CNT.
            MOVE PAGE-CNT TO H1-PAGE.
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
            WRITE PRINT-REC FROM HEAD3
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD4
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD5
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD6
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD7
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD8
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE 11 TO LINE-CNT.
       PRR-020.
           MOVE ST-STOCKNUMBER     TO D-STOCK
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE ST-CATEGORY        TO D-CAT
           MOVE ST-SUPPLIER        TO D-SUPPLIER
           MOVE ST-FOREIGNCOST     TO D-FOREIGN
           MOVE ST-SUPPLIERDISC    TO D-SUPP-DISC
           MOVE ST-CURRENCY        TO D-CURRENCY
           MOVE ST-PRICE           TO D-PRICE
           MOVE ST-OLDPRICE        TO D-OLDPRICE
           MOVE ST-UNITOFMEASURE   TO D-UNIT
           MOVE ST-DISCOUNT1       TO D-DISC1
           MOVE ST-DISCOUNT2       TO D-DISC2
           MOVE ST-DISCOUNT3       TO D-DISC3
           MOVE ST-DISCOUNT4       TO D-DISC4
           MOVE ST-DISCOUNT5       TO D-DISC5
           MOVE ST-DISCOUNT6       TO D-DISC6
           MOVE ST-DISCOUNT7       TO D-DISC7
           MOVE ST-DISCOUNT8       TO D-DISC8
           MOVE ST-DISCOUNT9       TO D-DISC9
           MOVE ST-AVERAGECOST     TO D-AVE
           MOVE ST-LASTCOST        TO D-LAST
           MOVE ST-DUTYPERCENT     TO D-PERC
           MOVE ST-SURCHARGE       TO D-SURCHARGE
           MOVE ST-DUTYTARIFF      TO D-TARIFF
           MOVE ST-MINBUYQTY       TO D-MINBUY
           MOVE ST-BINLOCATION     TO D-BIN
           MOVE ST-MAXIMUMLEVEL    TO D-MAX
           MOVE ST-MINIMUMLEVEL    TO D-MIN
           MOVE ST-QTYONHAND       TO D-HAND
           MOVE ST-QTYONORDER      TO D-ORDER
           MOVE ST-QTYONBORDER     TO D-BORDER
           MOVE ST-QTY-ST-TAKE     TO D-ST-TAKE
           MOVE ST-LASTPRICECHANGE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-OLDPRICEDATE
           MOVE ST-LASTSALEDATE    TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-SALEDATE
           MOVE ST-LASTRECEIPTDATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-RECEIPTDATE
           MOVE ST-LASTORDERDATE   TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-ORDERDATE
           MOVE ST-QTYADJMTD       TO D-ADJ-MTD
           MOVE ST-QTYADJYTD       TO D-ADJ-YTD
           MOVE ST-QTYADJLAST      TO D-ADJ-LAST
           MOVE ST-QTYRECMTD       TO D-REC-MTD
           MOVE ST-QTYRECYTD       TO D-REC-YTD
           MOVE ST-QTYRECLAST      TO D-REC-LAST
           MOVE ST-SALESUNITMTD    TO D-MTDUNIT
           MOVE ST-SALESUNITSYTD   TO D-YTDUNIT
           MOVE ST-SALESUNITSLAST  TO D-LYUNIT
           MOVE ST-SALESRANDSMTD   TO D-MTDSALE
           MOVE ST-SALESRANDSYTD   TO D-YTDSALE
           MOVE ST-SALESRANDSLAST  TO D-LYSALE
           MOVE ST-SALESCOSTMTD    TO D-MTDCOST
           MOVE ST-SALESCOSTYTD    TO D-YTDCOST
           MOVE ST-SALESCOSTLAST   TO D-LYCOST
           MOVE ST-PERMIT          TO D-PERMIT
           MOVE ST-ANALYSIS        TO D-ANALYSIS.

           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE3
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE4
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE5
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           ADD 6 TO LINE-CNT
           GO TO PRR-002.
       PRR-999.
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
      *     PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           WRITE PRINT-REC.
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
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
       Copy "PrintReportInfo".
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
