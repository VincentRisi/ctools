        IDENTIFICATION DIVISION.
        PROGRAM-ID. StOrStRp.
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
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdOutOrd.
           COPY ChlfdParam.
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
       77  WS-ZERO              PIC X VALUE " ".
       77  WS-COMP-PRINT        PIC X VALUE " ".
       77  WS-CAT               PIC X(3) VALUE " ".
       77  WS-SUPPLIER          PIC X(7) VALUE " ".
       77  WS-ORDERNO           PIC X(20) VALUE " ".
       77  WS-VALUE             PIC 9(6)V99 VALUE 0.
       77  WS-STOCKNUMBER       PIC X(15) VALUE " ".
       77  WS-FOREIGN           PIC 9(7)V999 VALUE 0.
       77  WS-FOREIGN-VALUE     PIC 9(8)V999 VALUE 0.
       77  WS-ORDER-VALUE       PIC 9(7)V99 VALUE 0.
       77  WS-RUNNING-TOTAL     PIC 9(7)V99 VALUE 0.
       77  WS-DELIVERVIA        PIC X(20) VALUE " ".    
       77  WS-DEL-SUB           PIC 9 VALUE 0.    
       01  SPLIT-DELIVERVIA.
           03  WSDE-CODE        PIC X VALUE " ".
           03  WSDE-REST        PIC X(19) VALUE " ".
       01  STORE-DEL.
         02  WS-DEL-OCCUR OCCURS 10.
           03  WS-DEL-TYPE        PIC X.
           03  WS-DEL-CODE        PIC X.
           03  WS-DEL-TERM        PIC X(20).
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1      PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(17) VALUE "O R D E R S   O N".
           03  FILLER         PIC X(20) VALUE "   S U P P L I E R S".
           03  FILLER         PIC X(51) VALUE
           "   R E P O R T   B Y   S T O C K N U M B E R".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(6) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(81) VALUE ALL "*".
           03  FILLER         PIC X(21) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(17) VALUE "STOCK".
           03  FILLER         PIC X(18) VALUE "ORDER".
           03  FILLER         PIC X(14) VALUE "  QTY  CFRM".
           03  FILLER         PIC X(12) VALUE "ORDER".
           03  FILLER         PIC X(10) VALUE "DUE".
           03  FILLER         PIC X(13) VALUE "DELIVERY".
           03  FILLER         PIC X(14) VALUE "SUPPLIER".
           03  FILLER         PIC X(14) VALUE "RAND COST".
           03  FILLER         PIC X(19) VALUE "FOREIGN COST".
       01  HEAD5.
           03  FILLER         PIC X(17) VALUE "NUMBER".
           03  FILLER         PIC X(32) VALUE "NUMBER".
           03  FILLER         PIC X(11) VALUE " DATE".
           03  FILLER         PIC X(12) VALUE " DATE".
           03  FILLER         PIC X(52) VALUE "METHOD".
       01  DETAIL-LINE.
           03  D-STOCKNO      PIC X(17) VALUE " ".
           03  D-ORDER        PIC X(18) VALUE " ".
           03  D-QTY          PIC Z(4)9.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-CONFIRMED    PIC X.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-DATE         PIC X(10).
           03  FILLER         PIC X(2) VALUE " ".
           03  D-DUEDATE      PIC X(10).
           03  D-REST.
             04  FILLER         PIC X(1) VALUE " ".
             04  D-DELIVERY     PIC X(13) VALUE " ".
             04  FILLER         PIC X(1).
             04  D-SUPPLIER     PIC X(14) VALUE " ".
             04  D-VALUE        PIC Z(5)9.99.
             04  FILLER         PIC X(6) VALUE " ".
             04  D-FOREIGN      PIC Z(6)9.999.
             04  FILLER         PIC X(7) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(82) VALUE " ".
           03  FILLER         PIC X(15) VALUE "CATEGORY TOTAL:".
           03  TOT-VALUE      PIC Z(6)9.99.
           03  FILLER         PIC X(5) VALUE " ".
           03  TOT-FOREIGN    PIC Z(7)9.999.
           03  FILLER         PIC X(6) VALUE " ".
       01  RUNNING-LINE.
           03  FILLER         PIC X(82) VALUE " ".
           03  FILLER         PIC X(15) VALUE " RUNNING TOTAL:".
           03  RUN-VALUE      PIC Z(6)9.99.
           03  FILLER         PIC X(28) VALUE " ".
       01  UNDER-LINE.
           03  FILLER         PIC X(82) VALUE " ".
           03  FILLER         PIC X(42) VALUE ALL "-".
           03  FILLER         PIC X(8) VALUE " ".
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
           DISPLAY "** ORDERS ON SUPPLIERS REPORT BY STOCKNUMBER **"
               AT POS
           MOVE 420 TO POS
           DISPLAY "***********************************************"
               AT POS.
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
            DISPLAY "    FROM STOCK NUMBER : [               ]" AT POS.
            MOVE 1045 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
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
            DISPLAY "      TO STOCK NUMBER : [               ]" AT POS.
            MOVE 1245 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
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
            DISPLAY "Print Items With ZERO QUANTITY ?: [ ]" AT POS.
            MOVE 1445 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 44        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ZERO.

            IF W-ESCAPE-KEY = 4
                GO TO GET-010.
            IF WS-ZERO = " "
               GO TO GET-020.
            IF WS-ZERO NOT = "Y" AND NOT = "N"
               MOVE 1610 TO POS
               DISPLAY "ENTER ONLY Y OR N." AT POS
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-050
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
       GET-050.
            MOVE 1610 TO POS.
            DISPLAY
           "COMPRESSED PRINTING: FAX THIS REPORT TO SUPPLIER. Y/N [ ]"
                AT POS.
            MOVE 1665 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 64        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMP-PRINT.

            IF W-ESCAPE-KEY = 4
                GO TO GET-020.
            IF WS-COMP-PRINT = " "
               GO TO GET-050.
            IF WS-COMP-PRINT NOT = "Y" AND NOT = "N"
               MOVE 1810 TO POS
               DISPLAY "ENTER ONLY Y OR N." AT POS
               GO TO GET-050.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-055
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-050.
       GET-055.
            MOVE 1810 TO POS.
            DISPLAY "PRINT A SUPPLIER ONLY, BLANK FOR ALL:[       ]"
                AT POS.
            ADD 38 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SUPPLIER.

            IF W-ESCAPE-KEY = 4
                GO TO GET-050.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-150
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-055.
       GET-150.
            MOVE 2510 TO POS.
            DISPLAY "The Report Is Being Compiled, Please Be Patient."
                 AT POS.
            PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE WS-RANGE1 TO OO-STOCK-NUMBER.
            START OUTSTANDING-ORDERS KEY NOT < OO-STOCK-NUMBER.
       PRR-002.
            READ OUTSTANDING-ORDERS NEXT WITH LOCK
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
            IF OO-STOCK-NUMBER < WS-RANGE1
               GO TO PRR-002.
            IF OO-STOCK-NUMBER > WS-RANGE2
               PERFORM TOTALS
               GO TO PRR-999.
               
            MOVE 2910 TO POS.
            DISPLAY OO-STOCK-NUMBER AT POS.
            ADD 20 TO POS.
            DISPLAY  OO-ORDER-NUMBER AT POS.
            
            IF WS-SUPPLIER NOT = " "
             IF WS-SUPPLIER NOT = OO-SUPPLIER-NUMBER
                GO TO PRR-002.
            
            IF WS-ZERO = "N"
             IF OO-QUANTITY = 0
               GO TO PRR-002.
            IF OO-ORDER-NUMBER NOT = WS-ORDERNO
               MOVE OO-ORDER-NUMBER TO WS-ORDERNO
               MOVE OO-DELIVERY-METHOD TO SPLIT-DELIVERVIA
               MOVE WSDE-CODE TO WS-DEL-SUB
               MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA.
       PRR-006.
            IF OO-STOCK-NUMBER = WS-STOCKNUMBER
               GO TO PRR-010.
            MOVE OO-STOCK-NUMBER TO ST-KEY.
            START STOCK-MASTER KEY NOT < ST-KEY.
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
       PRR-010.
            IF WS-CAT = "   "
               MOVE ST-CATEGORY TO WS-CAT.
            IF ST-CATEGORY NOT = WS-CAT
               MOVE ST-CATEGORY TO WS-CAT
               PERFORM TOTALS.
            IF LINE-CNT < 60
               GO TO PRR-020.
            ADD 1 TO PAGE-CNT.
            MOVE PAGE-CNT TO H1-PAGE.

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.

            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
            WRITE PRINT-REC FROM HEAD1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD2.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD4.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD5.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE 7 TO LINE-CNT.
       PRR-020.
           MOVE " " TO PRINT-REC.
           IF OO-STOCK-NUMBER NOT = WS-STOCKNUMBER
              MOVE OO-STOCK-NUMBER TO WS-STOCKNUMBER
                                      D-STOCKNO.
           MOVE OO-ORDER-NUMBER    TO D-ORDER.
           MOVE OO-QUANTITY        TO D-QTY.
           MOVE OO-UPDATED         TO D-CONFIRMED.
           MOVE OO-ORDERDATE       TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE       TO D-DATE.
           IF OO-DUEDATE > 0
              MOVE OO-DUEDATE         TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE       TO  D-DUEDATE
           ELSE
              MOVE " "                TO  D-DUEDATE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
             MOVE "*** STOCK NUMBER NOT ON STOCK RECORD ***" TO D-REST
             GO TO PRR-025.
           MOVE WS-DELIVERVIA      TO D-DELIVERY.
           MOVE OO-SUPPLIER-NUMBER TO D-SUPPLIER.
           COMPUTE WS-VALUE ROUNDED = OO-QUANTITY * ST-AVERAGECOST.
           MOVE WS-VALUE           TO D-VALUE.
           COMPUTE WS-FOREIGN ROUNDED = OO-QUANTITY * ST-FOREIGNCOST.
           MOVE WS-FOREIGN         TO D-FOREIGN.
           ADD WS-VALUE TO WS-ORDER-VALUE
                           WS-RUNNING-TOTAL.
           ADD WS-FOREIGN TO WS-FOREIGN-VALUE.
       PRR-025.
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC D-REST D-STOCKNO.
           ADD 1 TO LINE-CNT.
           MOVE 0 TO WS-VALUE
                     WS-FOREIGN.
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       TOTALS SECTION.
       TS-010.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC FROM UNDER-LINE.
           MOVE " " TO PRINT-REC.
           MOVE WS-ORDER-VALUE   TO TOT-VALUE.
           MOVE WS-FOREIGN-VALUE TO TOT-FOREIGN.
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC.
           MOVE WS-RUNNING-TOTAL TO RUN-VALUE.
           WRITE PRINT-REC FROM RUNNING-LINE.
           MOVE " " TO PRINT-REC.
           MOVE 0 TO WS-ORDER-VALUE
                     WS-FOREIGN-VALUE.
           ADD 3 TO LINE-CNT.
           IF WS-COMP-PRINT = "N"
               GO TO TS-999.
           MOVE
           "PLEASE ADVISE DELIVERY STATUS OF ABOVE OUTSTANDING ORDERS,"
                TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE
           "AS DUE DATES NEED TO BE UPDATED & STOCK IS URGENTLY NEEDED."
                TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
           ADD 3 TO LINE-CNT.
       TS-999.
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
                GO TO RDELIV-999.
            IF PA-TYPE < 3
               GO TO RDELIV-010.
            IF PA-TYPE > 3
                GO TO RDELIV-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "SLPARAMETER DELV BUSY ON READ-NEXT, 'ESC' TO RETRY."
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
               GO TO RDELIV-999.
            GO TO RDELIV-010.
       RDELIV-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-002.
           OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCKMASTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-002.
       OPEN-003.
           OPEN I-O OUTSTANDING-ORDERS.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE "SORDERS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-OUTORD-ST1
               GO TO OPEN-003.
       OPEN-010.
           OPEN I-O PARAMETER-FILE
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-010.
           MOVE ALL "X" TO STORE-DEL.
           PERFORM READ-DELIVERY-FILE.
           CLOSE PARAMETER-FILE.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-500.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-400.
            IF WS-SUPPLIER NOT = " "
              MOVE "** ITEMS PRINTED FOR A PARTICULAR SUPPLIER ONLY. **"
              TO PRINT-REC
              WRITE PRINT-REC
              MOVE " " TO PRINT-REC.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
            CLOSE STOCK-MASTER
                  OUTSTANDING-ORDERS
                  PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
        END-900.
           EXIT PROGRAM.
      *      STOP RUN.
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
