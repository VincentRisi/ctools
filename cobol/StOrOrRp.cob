        IDENTIFICATION DIVISION.
        PROGRAM-ID. StOrOrRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
         Copy "SelectStOrders".
         Copy "SelectSlParameter".
         Copy "SelectCrMaster".
         Copy "SelectCrAlias".
         Copy "SelectCrFBCTrans".
         Copy "SelectCrCurrency".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS WS-PRINT-STATUS.
           SELECT PRINT-FAX-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS WS-PRINT-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdOutOrd.
           COPY ChlfdParam.
           COPY ChlfdCreditor.
           COPY ChlfdCrAlias.
           COPY ChlfdCrCurr.
           COPY ChlfdCrFBCTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(135).
      *
       FD  PRINT-FAX-FILE.
       01  PRINT-FAX-REC.
           03  FILLER           PIC X(80).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(20) VALUE " ".
       77  WS-RANGE2            PIC X(20) VALUE " ".
       77  WS-COMM-MESSAGE      PIC X(60) VALUE " ".
       77  WS-PRINT-ZERO-QTY    PIC X VALUE " ".
       77  WS-CATEGORY          PIC X(3) VALUE " ".
       77  WS-COMPRESS-PRINT    PIC X VALUE " ".
       77  WS-SUMMARY           PIC X VALUE " ".
       77  WS-FOR-LOCAL         PIC X VALUE " ".
       77  WS-WRITE-FBC         PIC X VALUE " ".
       77  WS-ORDERPRINTED      PIC X VALUE " ".
       77  WS-SUPPLIER          PIC X(7) VALUE " ".
       77  WS-ORDERNO           PIC X(20) VALUE " ".
       77  WS-TOTALITEMS        PIC 9(6) VALUE 0.
       77  WS-VALUE             PIC 9(6)V99 VALUE 0.
       77  WS-FOREIGN           PIC 9(7)V999 VALUE 0.
       77  WS-FOREIGN-VALUE     PIC 9(8)V999 VALUE 0.
       77  WS-ORDER-VALUE       PIC 9(7)V99 VALUE 0.
       77  WS-RUNNING-TOTAL     PIC 9(7)V99 VALUE 0.
       77  WS-DELIVERVIA        PIC X(20) VALUE " ".    
       77  WS-DEL-SUB           PIC 9 VALUE 0.    
       01  SPLIT-STOCK.
           03  SP-1STCHAR3      PIC X(3) VALUE " ".
           03  SP-REST          PIC X(12) VALUE " ".
       01  WS-COMMENT-NAMES.
         02  WS-COMMENT OCCURS 5.
           03  WS-COMM-LINE     PIC X(60).
       01  SPLIT-DELIVERVIA.
           03  WSDE-CODE        PIC X VALUE " ".
           03  WSDE-REST        PIC X(19) VALUE " ".
       01  STORE-DEL.
         02  WS-DEL-OCCUR OCCURS 10.
           03  WS-DEL-TYPE       PIC X.
           03  WS-DEL-CODE       PIC X.
           03  WS-DEL-TERM       PIC X(20).
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1      PIC 99.
       01  WS-PRINT-STATUS.
           03  WS-PRINT-ST1       PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1    PIC 99.
       01  WS-ALIAS-STATUS.
           03  WS-ALIAS-ST1       PIC 99.
       01  WS-FBCTRANS-STATUS.
           03  WS-FBCTRANS-ST1    PIC 99.
       01  WS-CURRENCY-STATUS.
           03  WS-CU-ST1  PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(23) VALUE " ".
           03  FILLER         PIC X(17) VALUE "O R D E R S   O N".
           03  FILLER         PIC X(20) VALUE "   S U P P L I E R S".
           03  FILLER         PIC X(45) VALUE "   R E P O R T".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(40) VALUE " ".
           03  FILLER         PIC X(51) VALUE ALL "*".
           03  FILLER         PIC X(41) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(17) VALUE "ORDER".
           03  FILLER         PIC X(19) VALUE "STOCK".
           03  FILLER         PIC X(6) VALUE "QTY".
           03  FILLER         PIC X(13) VALUE "ORDER".
           03  FILLER         PIC X(7) VALUE "DUE".
           03  FILLER         PIC X(13) VALUE "DELIVERY".
           03  FILLER         PIC X(9) VALUE "SUPPLIER".
           03  FILLER         PIC X(14) VALUE "RAND COST".
           03  FILLER         PIC X(9) VALUE "FOREIGN".
           03  FILLER         PIC X(8) VALUE "  TARIFF".
           03  FILLER         PIC X(20) VALUE " DUTY% SUR% CFM PMT".
       01  HEAD5.
           03  FILLER         PIC X(17) VALUE "NUMBER".
           03  FILLER         PIC X(26) VALUE "NUMBER".
           03  FILLER         PIC X(11) VALUE "DATE".
           03  FILLER         PIC X(8) VALUE "DATE".
           03  FILLER         PIC X(74) VALUE "METHOD".
       01  DETAIL-LINE.
           03  D-ORDER        PIC X(17) VALUE " ".
           03  D-STOCKNO      PIC X(17) VALUE " ".
           03  D-QTY          PIC Z(4)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-DATE         PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-DUEDATE      PIC X(10).
           03  D-REST.
             04  FILLER         PIC X(1) VALUE " ".
             04  D-DELIVERY     PIC X(13) VALUE " ".
             04  FILLER         PIC X(1).
             04  D-SUPPLIER     PIC X(7) VALUE " ".
             04  FILLER         PIC X(1).
             04  D-VALUE        PIC Z(6)9.99 BLANK WHEN ZERO.
             04  FILLER         PIC X(1) VALUE " ".
             04  D-FOREIGN      PIC Z(6)9.999 BLANK WHEN ZERO.
             04  FILLER         PIC X(1) VALUE " ".
             04  D-TARIFF       PIC X(9).
             04  D-DUTY         PIC Z9.99.
             04  FILLER         PIC X(1).
             04  D-SURCH        PIC Z9.99.
             04  FILLER         PIC X(2).
             04  D-CONFIRMED    PIC X(1).
             04  FILLER         PIC X(3).
             04  D-PERMIT       PIC X(3).
       01  TOTAL-LINE.
           03  FILLER         PIC X(69) VALUE " ".
           03  FILLER         PIC X(15) VALUE "  ORDER TOTAL:".
           03  TOT-VALUE      PIC Z(6)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  TOT-FOREIGN    PIC Z(6)9.999.
           03  FILLER         PIC X(30) VALUE " ".
       01  RUNNING-LINE.
           03  FILLER         PIC X(69) VALUE " ".
           03  FILLER         PIC X(15) VALUE "RUNNING TOTAL:".
           03  RUN-VALUE      PIC Z(6)9.99.
           03  FILLER         PIC X(42) VALUE " ".
       01  UNDER-LINE.
           03  FILLER         PIC X(83) VALUE " ".
           03  FILLER         PIC X(23) VALUE ALL "-".
           03  FILLER         PIC X(25) VALUE " ".
       01  FAX-NAME.
           03  FILLER         PIC X(7) VALUE "FROM:".
           03  N-FAX-NAME     PIC X(53) VALUE " ".
           03  FILLER         PIC X(10) VALUE "SUPPLIER:".
           03  N-FAX-SUPPLIER PIC X(10) VALUE " ".
       01  FAX-HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-FAX-DATE    PIC X(10).
           03  FILLER         PIC X(2) VALUE " ".
           03  FILLER         PIC X(20) VALUE "O R D E R S   O N".
           03  FILLER         PIC X(20) VALUE "S U P P L I E R S".
           03  FILLER         PIC X(14) VALUE "R E P O R T".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-FAX-PAGE    PIC Z9.
           03  FILLER         PIC X(2) VALUE " ".
       01  FAX-HEAD2.
           03  FILLER         PIC X(17) VALUE " ".
           03  FILLER         PIC X(51) VALUE ALL "*".
           03  FILLER         PIC X(12) VALUE " ".
       01  FAX-HEAD4.
           03  FILLER         PIC X(16) VALUE "ORDER".
           03  FILLER         PIC X(19) VALUE "STOCK".
           03  FILLER         PIC X(6) VALUE "QTY".
           03  FILLER         PIC X(9) VALUE "ORDER".
           03  FILLER         PIC X(8) VALUE "DUE".
           03  FILLER         PIC X(13) VALUE "DELIVERY".
       01  FAX-HEAD5.
           03  FILLER         PIC X(16) VALUE "NUMBER".
           03  FILLER         PIC X(25) VALUE "NUMBER".
           03  FILLER         PIC X(10) VALUE " DATE".
           03  FILLER         PIC X(10) VALUE "DATE".
           03  FILLER         PIC X(22) VALUE "METHOD".
       01  DETAIL-FAX-LINE.
           03  D-FAX-ORDER    PIC X(16) VALUE " ".
           03  D-FAX-STOCKNO  PIC X(17) VALUE " ".
           03  D-FAX-QTY      PIC Z(4)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-FAX-DATE     PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-FAX-DUEDATE  PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-FAX-DELIVERY PIC X(13) VALUE " ".
           03  FILLER         PIC X(10) VALUE " ".
       01  TOTAL-FAX-LINE.
           03  FILLER          PIC X(16) VALUE " ".
           03  FILLER          PIC X(12) VALUE "ORDER TOTAL:".
           03  TOT-FAX-FOREIGN PIC Z(6)9.99.
           03  FILLER          PIC X(42) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 320 TO POS
           DISPLAY "** SUPPLIERS ORDERS BY ORDER # REPORT **" AT POS
           MOVE 420 TO POS
           DISPLAY "****************************************" AT POS.
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
            MOVE 1020 TO POS.
            DISPLAY "     FROM ORDER NUMBER: [                    ]"
            AT POS.
            MOVE 1045 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 44        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

            IF W-ESCAPE-KEY = 4
                GO TO CONTROL-005.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-010
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
       GET-010.
            MOVE 1220 TO POS.
            DISPLAY "       TO ORDER NUMBER: [                    ]"
            AT POS.
            MOVE 1245 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 44        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

            IF W-ESCAPE-KEY = 4
                GO TO GET-000.
            IF WS-RANGE2 = " "
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-020
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
       GET-020.
            MOVE 1410 TO POS.
            DISPLAY "PRINT ITEMS WITH ZERO QUANTITY ?: [ ]" AT POS.
            MOVE 1445 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 44        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PRINT-ZERO-QTY.

            IF W-ESCAPE-KEY = 4
                GO TO GET-010.
            IF WS-PRINT-ZERO-QTY = " "
               GO TO GET-020.
            IF WS-PRINT-ZERO-QTY NOT = "Y" AND NOT = "N"
               MOVE 1610 TO POS
               DISPLAY "ENTER ONLY Y OR N." AT POS
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-030
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
       GET-030.
            MOVE 1610 TO POS.
            DISPLAY
             "ENTER CATEGORY TO PRINT, LEAVE BLANK FOR ALL :[   ]."
                  AT POS.
            MOVE 1657 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 3         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CATEGORY.

            IF W-ESCAPE-KEY = 4
                GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-050
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
       GET-050.
            MOVE 1810 TO POS.
            DISPLAY
           "COMPRESSED PRINTING: FAX THIS REPORT TO SUPPLIER. Y/N [ ]"
                AT POS.
            MOVE 1865 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 64        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMPRESS-PRINT.

            IF W-ESCAPE-KEY = 4
                GO TO GET-030.
            IF WS-COMPRESS-PRINT = " "
               GO TO GET-050.
            IF WS-COMPRESS-PRINT NOT = "Y" AND NOT = "N"
               MOVE 2010 TO POS
               DISPLAY "ENTER ONLY Y OR N." AT POS
               GO TO GET-050.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-055
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-050.
       GET-055.
            IF WS-COMPRESS-PRINT = "N"
               GO TO GET-125.
            MOVE 1 TO SUB-1.
       GET-060.
            MOVE 2010 TO POS.
            DISPLAY
            "Enter A Comment To Be Printed, 5 Lines Of 60 Characters"
                AT POS.
            MOVE 2109 TO POS
            DISPLAY "[" AT POS.
            MOVE 2170 TO POS
            DISPLAY "]" AT POS.
       GET-061.
            MOVE 2101 TO POS
            DISPLAY "Line" AT POS
            ADD 4 TO POS
            MOVE SUB-1 TO F-EDNAMEFIELDIND
            DISPLAY F-EDNAMEFIELDIND AT POS.
            MOVE 2110 TO POS.
            MOVE WS-COMM-LINE (SUB-1) TO WS-COMM-MESSAGE
            DISPLAY WS-COMM-MESSAGE AT POS.

           MOVE WS-COMM-MESSAGE TO CDA-DATA.
           MOVE 60        TO CDA-DATALEN.
           MOVE 18        TO CDA-ROW.
           MOVE 09        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMM-MESSAGE.

            MOVE WS-COMM-MESSAGE TO WS-COMM-LINE (SUB-1).
            MOVE " " TO WS-COMM-MESSAGE.
            DISPLAY WS-COMM-MESSAGE AT POS.
            IF W-ESCAPE-KEY = 4
             IF SUB-1 = 1
                GO TO GET-050.
            IF W-ESCAPE-KEY = 4
               SUBTRACT 1 FROM SUB-1
                MOVE WS-COMM-LINE (SUB-1) TO WS-COMM-MESSAGE
                DISPLAY WS-COMM-MESSAGE AT POS
                GO TO GET-061.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-100
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-061.
       GET-100.
            IF SUB-1 < 5
               ADD 1 TO SUB-1
               GO TO GET-061.
       GET-125.
            MOVE 2310 TO POS
            DISPLAY "PRINT A SUMMARY OF THE ORDERS. Y/N [ ]" AT POS
            ADD 36 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 20        TO CDA-ROW.
           MOVE 45        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SUMMARY.

            IF W-ESCAPE-KEY = 4
             IF WS-SUMMARY = "N"
                GO TO GET-050
             ELSE
                GO TO GET-060.
            IF WS-SUMMARY NOT = "Y" AND NOT = "N"
               MOVE "ENTER ONLY Y OR N." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-125.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-130
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-125.
       GET-130.
            MOVE 2510 TO POS
            DISPLAY
            "PRINT: L=LOCAL ONLY, F=FOREIGN ONLY, BLANK=ALL. [ ]"
            AT POS
            ADD 49 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 22        TO CDA-ROW.
           MOVE 58        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FOR-LOCAL.
      *     MOVE CDA-DATA TO WS-COMM-MESSAGE.

      *      ACCEPT WS-FOR-LOCAL AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO GET-125.
            IF WS-FOR-LOCAL NOT = "L" AND NOT = "F" AND NOT = " "
               MOVE "ENTER ONLY L, F OR LEAVE BLANK." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-130.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-140
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-130.
       GET-140.
            MOVE "N" TO WS-WRITE-FBC.
            MOVE 2610 TO POS
            DISPLAY "WRITE FBC RECORDS, Y OR N. [ ]" AT POS
            ADD 28 TO POS.

           MOVE WS-WRITE-FBC TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 23        TO CDA-ROW.
           MOVE 37        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-WRITE-FBC.

            IF W-ESCAPE-KEY = 4
                GO TO GET-130.
            IF WS-WRITE-FBC NOT = "N" AND NOT = "Y"
               MOVE "ENTER ONLY N=NO, Y=YES." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-140.
            IF WS-WRITE-FBC = "Y"
             IF WS-FOR-LOCAL NOT = "F"
               MOVE
             "TO WRITE FBC RECORDS THE PREVIOUS ANSWER MUST BE 'F'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-140.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-150
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-140.
       GET-150.
            PERFORM ERROR-020
            MOVE 2910 TO POS
            DISPLAY "The Report Is Being Compiled." AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE "N"       TO WS-ORDERPRINTED
            MOVE WS-RANGE1 TO OO-ORDER-NUMBER.
            START OUTSTANDING-ORDERS KEY NOT < OO-KEY
              INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE "BAD START ST1 NOT =0, GOING TO EXIT." TO WS-MESSAGE
              GO TO PRR-999.
       PRR-002.
            READ OUTSTANDING-ORDERS NEXT
               AT END NEXT SENTENCE.
            IF WS-OUTORD-ST1 = 10
               PERFORM TOTALS
               GO TO PRR-999.
            IF WS-OUTORD-ST1 NOT = 0
            MOVE "ST-ORDERS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO PRR-002.
            IF OO-ORDER-NUMBER < WS-RANGE1
               GO TO PRR-002.
            IF OO-ORDER-NUMBER > WS-RANGE2
               PERFORM TOTALS
               GO TO PRR-999.
            IF WS-PRINT-ZERO-QTY = "N"
             IF OO-QUANTITY = 0
               GO TO PRR-002.
            IF WS-CATEGORY = "  "
               GO TO PRR-004.
            MOVE OO-STOCK-NUMBER TO SPLIT-STOCK.
            IF SP-1STCHAR3 = WS-CATEGORY
               GO TO PRR-004
            ELSE
               GO TO PRR-002.
        PRR-004.
            MOVE 2815 TO POS
            DISPLAY "P/Order Being Read:" AT POS
            ADD 20 TO POS
            DISPLAY OO-ORDER-NUMBER AT POS.
            
            IF WS-ORDERNO = " "
               MOVE OO-ORDER-NUMBER          TO WS-ORDERNO
               MOVE OO-DELIVERY-METHOD       TO SPLIT-DELIVERVIA
               MOVE WSDE-CODE                TO WS-DEL-SUB
               MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA.

            IF OO-ORDER-NUMBER NOT = WS-ORDERNO
               PERFORM TOTALS
               MOVE OO-ORDER-NUMBER          TO WS-ORDERNO
               MOVE " "                      TO WS-SUPPLIER
               MOVE OO-DELIVERY-METHOD       TO SPLIT-DELIVERVIA
               MOVE WSDE-CODE                TO WS-DEL-SUB
               MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA.
       PRR-006.
            MOVE OO-STOCK-NUMBER TO ST-KEY.
            READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE. 
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE " " TO D-REST
               GO TO PRR-010.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRR-006.
           IF WS-SUPPLIER = " "
              MOVE ST-SUPPLIER TO WS-SUPPLIER.
           IF WS-FOR-LOCAL = "F"
            IF ST-FOREIGNCOST = 0
              GO TO PRR-002. 
           IF WS-FOR-LOCAL = "L"
            IF ST-FOREIGNCOST > 0
              GO TO PRR-002. 
       PRR-010.
            IF LINE-CNT < 58
               GO TO PRR-020.
            ADD 1 TO PAGE-CNT.
            MOVE PAGE-CNT TO H1-PAGE H1-FAX-PAGE.
       PRR-012.
           IF WS-COMPRESS-PRINT = "Y"
               GO TO PRR-016.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
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
            MOVE 7 TO LINE-CNT
            GO TO PRR-020.
       PRR-016.
           MOVE OO-SUPPLIER-NUMBER TO N-FAX-SUPPLIER.
           IF PAGE-CNT = 1
                WRITE PRINT-FAX-REC FROM FAX-NAME
            ELSE
                WRITE PRINT-FAX-REC BEFORE PAGE
                WRITE PRINT-FAX-REC FROM FAX-NAME.
            MOVE " " TO PRINT-FAX-REC
            WRITE PRINT-FAX-REC
            WRITE PRINT-FAX-REC FROM FAX-HEAD1
            MOVE " " TO PRINT-FAX-REC
            WRITE PRINT-FAX-REC FROM FAX-HEAD2
            MOVE " " TO PRINT-FAX-REC
            WRITE PRINT-FAX-REC
            WRITE PRINT-FAX-REC FROM FAX-HEAD4
            MOVE " " TO PRINT-FAX-REC
            WRITE PRINT-FAX-REC FROM FAX-HEAD5
            MOVE " " TO PRINT-FAX-REC
            WRITE PRINT-FAX-REC
            MOVE 8 TO LINE-CNT.
       PRR-020.
           IF WS-COMPRESS-PRINT = "Y"
               GO TO PRR-050.
           MOVE " " TO PRINT-REC
           MOVE OO-ORDER-NUMBER    TO D-ORDER
           IF WS-WRITE-FBC = "Y"
              MOVE OO-ORDER-NUMBER TO CRFXTRANS-PORDER-NUMBER.
           MOVE OO-STOCK-NUMBER    TO D-STOCKNO
           MOVE OO-QUANTITY        TO D-QTY
           MOVE OO-ORDERDATE       TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-DATE.
           IF OO-DUEDATE > 0
              MOVE OO-DUEDATE         TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE       TO D-DUEDATE
           ELSE
              MOVE " "                TO D-DUEDATE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
             MOVE "*** STOCK NUMBER NOT ON STOCK RECORD ***" TO D-REST
             GO TO PRR-025.
           MOVE WS-DELIVERVIA      TO D-DELIVERY
           MOVE OO-SUPPLIER-NUMBER TO D-SUPPLIER.
       PRR-022.
           COMPUTE WS-VALUE ROUNDED = OO-QUANTITY * ST-AVERAGECOST
           MOVE WS-VALUE           TO D-VALUE
           COMPUTE WS-FOREIGN ROUNDED = OO-QUANTITY * ST-FOREIGNCOST
           MOVE WS-FOREIGN         TO D-FOREIGN
           ADD WS-VALUE TO WS-ORDER-VALUE
                           WS-RUNNING-TOTAL.
           ADD WS-FOREIGN TO WS-FOREIGN-VALUE.
           IF WS-COMPRESS-PRINT = "N"
            IF ST-DUTYTARIFF > 0
               MOVE ST-DUTYTARIFF  TO D-TARIFF
               MOVE ST-DUTYPERCENT TO D-DUTY
               MOVE ST-SURCHARGE   TO D-SURCH
               MOVE ST-PERMIT      TO D-PERMIT
            ELSE
               MOVE 0              TO D-TARIFF
                                      D-DUTY
                                      D-SURCH
               MOVE " "            TO D-PERMIT.
           IF WS-COMPRESS-PRINT = "N"
               MOVE OO-UPDATED     TO D-CONFIRMED
           ELSE
               MOVE " "            TO D-CONFIRMED.
       PRR-025.
           IF WS-SUMMARY = "Y"
             MOVE "Y" TO WS-ORDERPRINTED
      *       ADD OO-QUANTITY TO WS-TOTALITEMS
             ADD 1  TO WS-TOTALITEMS
             MOVE 0 TO WS-VALUE
                       WS-FOREIGN
             GO TO PRR-002. 
               
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC D-REST.
           ADD 1    TO LINE-CNT.
           MOVE "Y" TO WS-ORDERPRINTED.
           MOVE 0   TO WS-VALUE
                       WS-FOREIGN.
           GO TO PRR-002.
       PRR-050.
           MOVE " "                TO PRINT-FAX-REC
           MOVE OO-ORDER-NUMBER    TO D-FAX-ORDER
           IF WS-WRITE-FBC = "Y"
              MOVE OO-ORDER-NUMBER TO CRFXTRANS-PORDER-NUMBER.
           MOVE OO-STOCK-NUMBER    TO D-FAX-STOCKNO
           MOVE OO-QUANTITY        TO D-FAX-QTY
           MOVE OO-ORDERDATE       TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-FAX-DATE.
           IF OO-DUEDATE > 0
              MOVE OO-DUEDATE      TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE    TO D-FAX-DUEDATE
           ELSE
              MOVE " "             TO D-FAX-DUEDATE.
           MOVE WS-DELIVERVIA      TO D-FAX-DELIVERY.
        PRR-055.
           IF WS-FOR-LOCAL = "F"
              COMPUTE WS-FOREIGN ROUNDED = OO-QUANTITY * ST-FOREIGNCOST
              ADD WS-FOREIGN TO WS-FOREIGN-VALUE
           ELSE
           COMPUTE WS-VALUE ROUNDED = OO-QUANTITY * ST-AVERAGECOST
              ADD WS-VALUE TO WS-FOREIGN-VALUE.

           IF WS-SUMMARY = "Y"
             MOVE "Y" TO WS-ORDERPRINTED
             MOVE 0 TO WS-VALUE
                       WS-FOREIGN
             GO TO PRR-002. 
           WRITE PRINT-FAX-REC FROM DETAIL-FAX-LINE
           MOVE " " TO PRINT-FAX-REC
           ADD 1    TO LINE-CNT
           MOVE "Y" TO WS-ORDERPRINTED
           MOVE 0   TO WS-FOREIGN
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       TOTALS SECTION.
       TS-010.
            IF WS-ORDERPRINTED = "N"
               GO TO TS-999.
            IF WS-COMPRESS-PRINT = "Y"
               GO TO TS-020.
            IF WS-SUMMARY = "Y"
             IF WS-ORDERPRINTED = "Y"
               MOVE WS-TOTALITEMS TO D-QTY
               MOVE " "           TO D-STOCKNO
                                     D-VALUE
                                     D-FOREIGN
               WRITE PRINT-REC FROM DETAIL-LINE
               MOVE " " TO PRINT-REC D-REST.
            IF WS-SUMMARY = "N"
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM UNDER-LINE.
            MOVE " " TO PRINT-REC
            MOVE WS-ORDER-VALUE   TO TOT-VALUE
            MOVE WS-FOREIGN-VALUE TO TOT-FOREIGN
            WRITE PRINT-REC FROM TOTAL-LINE
            MOVE " " TO PRINT-REC
            MOVE WS-RUNNING-TOTAL TO RUN-VALUE
            WRITE PRINT-REC FROM RUNNING-LINE
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            GO TO TS-030.
       TS-020.
            IF WS-SUMMARY = "Y"
             IF WS-ORDERPRINTED = "Y"
               MOVE WS-TOTALITEMS TO D-QTY
               MOVE " "           TO D-STOCKNO
               WRITE PRINT-REC FROM DETAIL-LINE
               MOVE " " TO PRINT-REC D-REST.
            MOVE " " TO PRINT-FAX-REC
            WRITE PRINT-FAX-REC
            MOVE WS-FOREIGN-VALUE TO TOT-FAX-FOREIGN
            WRITE PRINT-FAX-REC FROM TOTAL-FAX-LINE
            MOVE " " TO PRINT-FAX-REC
            WRITE PRINT-FAX-REC.
       TS-030.
            IF WS-WRITE-FBC = "Y"
               PERFORM READ-CREDITOR-ALIAS.
            MOVE 0 TO WS-FOREIGN-VALUE
                      WS-ORDER-VALUE
                      WS-TOTALITEMS
            MOVE "N" TO WS-ORDERPRINTED.
            IF WS-SUMMARY = "Y"
               ADD 4 TO LINE-CNT
            ELSE
               ADD 5 TO LINE-CNT.
       TS-999.
           EXIT.
      *
       WRITE-FBC-RECORD SECTION.
       ROR-001.
            GO TO ROR-020.
       ROR-010.
            REWRITE CRFXTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-FBCTRANS-ST1 NOT = 0
                MOVE "FBC-TRANS BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-FBCTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                MOVE CRFXTRANS-PORDER-NUMBER TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO ROR-020.
            GO TO ROR-999.
       ROR-020.
            WRITE CRFXTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-FBCTRANS-ST1 = 23 OR 35 OR 49
                GO TO ROR-999.
                MOVE "FBC-TRANS BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-FBCTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                MOVE CRFXTRANS-PORDER-NUMBER TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO ROR-010.
       ROR-999.
            EXIT.
      *
       READ-CREDITOR SECTION.
       RD-000.
           MOVE CRAL-ACCOUNT-NUMBER TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-ACCOUNT-NUMBER
              INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE.
        RD-010.
           READ CREDITOR-MASTER
                 INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN" TO CR-NAME
                GO TO RD-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE "CREDITORS BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CREDITOR-ST1
                GO TO RD-010.
       RD-500.
           MOVE " "                     TO CRFXTRANS-FBC-NUMBER
           MOVE CRAL-ACCOUNT-NUMBER     TO CRFXTRANS-ACC-NUMBER
           MOVE CR-CURRENCY             TO CRFXTRANS-CURRENCY-TYPE
           COMPUTE WS-FOREIGN-VALUE = WS-FOREIGN-VALUE - 
                (WS-FOREIGN-VALUE * CR-TRADE-DISC / 100)
           MOVE WS-FOREIGN-VALUE        TO CRFXTRANS-INITIAL-FOREIGN-AMT
                                           CRFXTRANS-UNAPPLIED-AMT
           PERFORM READ-CURRENCY
           MOVE CU-VALUE                TO CRFXTRANS-EXCHANGE-RATE
           COMPUTE CRFXTRANS-LOCAL-AMT =
               CRFXTRANS-UNAPPLIED-AMT / CU-VALUE
           MOVE OO-ORDERDATE            TO CRFXTRANS-ORDER-DATE.
           IF OO-DUEDATE > 0
             MOVE OO-DUEDATE            TO CRFXTRANS-RECEIPT-DUE-DATE
             PERFORM COMPUTE-PAY-DATE
             MOVE SPLIT-DATE            TO CRFXTRANS-PAY-DUE-DATE
           ELSE
             MOVE 0                     TO CRFXTRANS-RECEIPT-DUE-DATE
                                           CRFXTRANS-PAY-DUE-DATE.
           
           PERFORM WRITE-FBC-RECORD.
       RD-999.
           EXIT.
      *
       READ-CURRENCY SECTION.
       RCU-005.
           MOVE CR-CURRENCY TO CU-KEY.
           START CURRENCY-MASTER KEY NOT < CU-KEY.
       RCU-010.
           READ CURRENCY-MASTER
                 INVALID KEY NEXT SENTENCE.
           IF WS-CU-ST1 = 23 OR 35 OR 49
                MOVE 1 TO CU-VALUE
                MOVE 0 TO WS-CU-ST1
                GO TO RCU-999.
           IF WS-CU-ST1 NOT = 0
                MOVE "CURRENCY BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CU-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE CR-CURRENCY TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CU-ST1
                GO TO RCU-010.
       RCU-999.
           EXIT.
      *
       COMPUTE-PAY-DATE SECTION.
       PD-005.
           MOVE CRFXTRANS-RECEIPT-DUE-DATE TO SPLIT-DATE.
           IF CR-TERMS = "1"
              ADD 1 TO SPLIT-MM.
           IF CR-TERMS = "2"
              ADD 0 TO SPLIT-MM.
           IF CR-TERMS = "3"
              ADD 0 TO SPLIT-MM.
           IF CR-TERMS = "4"
              ADD 2 TO SPLIT-MM.
           IF CR-TERMS = "5"
              ADD 3 TO SPLIT-MM.
           IF CR-TERMS = "6"
              ADD 4 TO SPLIT-MM.
           IF CR-TERMS = "7"
              ADD 3 TO SPLIT-MM.
           IF CR-TERMS = "8"
              ADD 1 TO SPLIT-MM.
           IF CR-TERMS = "9"
              ADD 1 TO SPLIT-MM.
       PD-020.
           IF SPLIT-MM > 12
             COMPUTE SPLIT-MM = SPLIT-MM - 12
             ADD 1 TO SPLIT-YY.
       PD-999.
           EXIT.
      *
       READ-CREDITOR-ALIAS SECTION.
       RDAL-000.
           MOVE WS-SUPPLIER TO CRAL-ALIAS.
           START CRALIAS-MASTER KEY NOT < CRAL-ALIAS.
       RDAL-010.
           READ CRALIAS-MASTER
                 INVALID KEY NEXT SENTENCE.
           IF WS-ALIAS-ST1 = 23 OR 35 OR 49
                GO TO RDAL-999.
           IF WS-ALIAS-ST1 NOT = 0
                MOVE "ALIAS FILE BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-ALIAS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-ALIAS-ST1
                GO TO RDAL-010.
           PERFORM READ-CREDITOR.
       RDAL-999.
             EXIT.
      *
       READ-DELIVERY-FILE SECTION.
       RDELIV-000.
            MOVE 1 TO SUB-1
                      PA-RECORD.
            MOVE 3 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RDELIV-010.
            READ PARAMETER-FILE NEXT
                AT END NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 10
                GO TO RDELIV-900.
            IF PA-TYPE < 3
               GO TO RDELIV-010.
            IF PA-TYPE > 3
                GO TO RDELIV-900.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "SLPARAMETER DELV BUSY ON READ, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RDELIV-010.
            IF PARAMETER-REC = "           "
               GO TO RDELIV-010.           
            MOVE PARAMETER-REC TO WS-DEL-OCCUR (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 = 10
               GO TO RDELIV-900.
            GO TO RDELIV-010.
       RDELIV-900.
            PERFORM ERROR-020.
       RDELIV-999.
            EXIT.
      *
       READ-PARAMETER-FILE SECTION.
       PARAM-000.
            MOVE 1 TO PA-RECORD.
            MOVE 0 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       PARAM-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
                MOVE "SLPARAMETER FILE UNKNOWN" TO PA-NAME
                GO TO PARAM-900.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "SLPARAMETER FILE BUSY ON READ, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO PARAM-010.
       PARAM-900.
            MOVE Ws-Co-Name TO CO-NAME N-FAX-NAME.
            PERFORM ERROR-020.
       PARAM-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR FILE BUSY ON OPEN,  'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-002.
            OPEN I-O CRFXTRANS-FILE
            IF WS-FBCTRANS-ST1 NOT = 0
               MOVE 0 TO WS-FBCTRANS-ST1
               MOVE "FBC-TRANS FILE BUSY OPEN I-O, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               OPEN OUTPUT CRFXTRANS-FILE.
       OPEN-003.
            OPEN I-O CRALIAS-MASTER.
            IF WS-ALIAS-ST1 NOT = 0
               MOVE 0 TO WS-ALIAS-ST1
               MOVE "ALIAS FILE BUSY ON OPEN,  'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-003.
       OPEN-004.
           OPEN I-O CURRENCY-MASTER.
           IF WS-CU-ST1 NOT = 0 
              MOVE 0 TO WS-CU-ST1
              MOVE "CURRENCY FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-004.
       OPEN-005.
           OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-008.
           OPEN I-O OUTSTANDING-ORDERS.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO WS-OUTORD-ST1
               MOVE "SORDERS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-008.
       OPEN-010.
           OPEN I-O PARAMETER-FILE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-010.
           PERFORM READ-PARAMETER-FILE.
           MOVE ALL "X" TO STORE-DEL.
           PERFORM READ-DELIVERY-FILE.
           CLOSE PARAMETER-FILE.
           
           IF WS-COMPRESS-PRINT = "N"
              PERFORM GET-USER-PRINT-NAME
              OPEN OUTPUT PRINT-FILE
           ELSE
              PERFORM GET-USER-PRINT-NAME
              OPEN OUTPUT PRINT-FAX-FILE.
           
           IF WS-PRINT-ST1 NOT = 0
             MOVE "ERROR IN OPENING PRINT-FILE, 'ESC' TO SEE STATUS"
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE WS-PRINT-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE.
       OPEN-050.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE  FROM DATE.
           MOVE WS-DATE      TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE
                                H1-FAX-DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF WS-COMPRESS-PRINT = "N"
               GO TO END-500.
           MOVE
           "PLEASE ADVISE DELIVERY STATUS OF ABOVE OUTSTANDING " &
           "ORDERS, ACCORDING" TO PRINT-FAX-REC.
           WRITE PRINT-FAX-REC.
           MOVE
           "TO OUR RECORDS THESE ORDERS HAVE NOT BEEN RECEIVED BY" &
           " OUR STORES." TO PRINT-FAX-REC.
           WRITE PRINT-FAX-REC.
           MOVE " " TO PRINT-FAX-REC.
           WRITE PRINT-FAX-REC.
           MOVE 1 TO SUB-1.
       END-010.
           IF WS-COMM-LINE (SUB-1) = " "
               MOVE "**** END OF PRINT FILE ****" TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE " " TO PRINT-REC
               GO TO END-600.
           MOVE WS-COMM-LINE (SUB-1) TO PRINT-FAX-REC.
           WRITE PRINT-FAX-REC.
           IF SUB-1 < 5
              ADD 1 TO SUB-1
              GO TO END-010.
       END-500.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-600.
           CLOSE STOCK-MASTER
                 OUTSTANDING-ORDERS
                 CURRENCY-MASTER
                 CREDITOR-MASTER
                 CRALIAS-MASTER
                 CRFXTRANS-FILE.
           IF WS-COMPRESS-PRINT = "N"
              CLOSE PRINT-FILE
           ELSE
              CLOSE PRINT-FAX-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
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
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
