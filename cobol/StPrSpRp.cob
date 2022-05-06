        IDENTIFICATION DIVISION.
        PROGRAM-ID. StPrSpRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B26.
        OBJECT-COMPUTER. B26.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
         Copy "SelectStSpecPr".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStPrice.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-QTY               PIC S9(6).
       77  WS-MARGIN            PIC S9(6)V999.
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(15) VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
       77  WS-ANSWER4           PIC X(7) VALUE " ".
       77  WS-ANSWER5           PIC X(2) VALUE " ".
       77  WS-MONTHS            PIC 99 VALUE 0.
       77  WS-FACTOR            PIC 9(3)V999 VALUE 0.
       77  WS-FACTOR-DIS        PIC Z(2)9.999.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-STPR-STATUS.
           03  WS-STPR-ST1      PIC 99.
       01  SPLIT-STOCK.
           03  SP-1ST3          PIC X(3).
           03  SP-REST          PIC X(12).
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(14) VALUE " ".
           03  FILLER         PIC X(45) VALUE
           "PRICE LIST - GOODS ON SPECIAL".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(22) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(29) VALUE ALL "*".
           03  FILLER         PIC X(74) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(58) VALUE
           "STOCK NUMBER    DESCRIPTION".
           03  FILLER         PIC X(74) VALUE
           "ON/H      LIST   SPECIAL".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(16) VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(20) VALUE " ".
           03  D-ONHAND       PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PRICE        PIC Z(5)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SPRICE       PIC Z(5)9.99.
           03  FILLER         PIC X(50) VALUE " ".
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
           DISPLAY "** SPECIALS PRICE LIST GENERATION REPORT **" AT POS
           MOVE 410 TO POS
           DISPLAY "*******************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1230 TO POS.
           DISPLAY "FROM STOCK NUMBER : [               ]" AT POS.
           MOVE 1251 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 50        TO CDA-COL.
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
           MOVE 1330 TO POS.
           DISPLAY "  TO STOCK NUMBER : [               ]" AT POS.
           MOVE 1351 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
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
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
       CONTROL-020.
           MOVE 1510 TO POS.
           DISPLAY 
            "Y=Print & Write File To Disk, N=Print Only," AT POS
           MOVE 1610 TO POS
           DISPLAY
        "P=Print From Existing Specials & Delete Non Stock Specials [ ]"
                AT POS
           MOVE 1710 TO POS
           DISPLAY
            "Answer Y/N = Non Current Specials will be Deleted."
                AT POS.
           MOVE 1810 TO POS
           DISPLAY
            "Answer Y/N = Non Current Specials will be Deleted."
                AT POS.
           MOVE 1670 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 69        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER3.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-012.
           IF WS-ANSWER3 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
           IF WS-ANSWER3 NOT = "N" AND NOT = "Y" AND NOT = "P"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-025.
           IF WS-ANSWER3 = "P"
                GO TO CONTROL-035.
           MOVE 1810 TO POS.
           DISPLAY
           "ENTER A FACTOR X AVERAGE COST FOR SPECIAL PRICE" &
           "      [       ]" AT POS.
           MOVE 1910 TO POS.
           DISPLAY
           "E.G. 25% = 1.25                FORMAT FOR FACTOR" &
           "    = 999.999" AT POS.
           MOVE 1864 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 63        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER4.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-020.
           IF WS-ANSWER4 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-025.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-025.
       CONTROL-030.
           MOVE WS-ANSWER4 TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WS-FACTOR WS-FACTOR-DIS.
           MOVE 1864 TO POS.
           DISPLAY WS-FACTOR-DIS AT POS.
       CONTROL-032.
           MOVE 2110 TO POS
           DISPLAY
           "Enter Number Of Months Last Received Items Into Stock"
              AT POS
           MOVE 2210 TO POS
           DISPLAY
           " Before The Item Should Be Printed As On Special." &
           "         [  ]" AT POS
           ADD 59 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 19        TO CDA-ROW.
           MOVE 68        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER5.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-025.
           MOVE WS-ANSWER5 TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-MONTHS.
           IF WS-MONTHS NOT > 0
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-032.
           IF WS-MONTHS > 35
               MOVE " MONTHS CANNOT BE > 35, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CONTROL-032.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-035
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-032.
       CONTROL-035.
           MOVE 2910 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
                AT POS.
           PERFORM OPEN-FILES.
           IF WS-ANSWER3 = "P"
              PERFORM SPECIAL-PRINT-ROUTINE
           ELSE
              PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE WS-DATE TO SPLIT-DATE.
           IF WS-MONTHS > 24
              SUBTRACT 2 FROM SPLIT-YY
              SUBTRACT 24 FROM WS-MONTHS
              GO TO PRR-001.
           IF WS-MONTHS > 12
              SUBTRACT 1 FROM SPLIT-YY
              SUBTRACT 12 FROM WS-MONTHS
              GO TO PRR-001.
              
           IF SPLIT-MM < WS-MONTHS
              ADD 12 TO SPLIT-MM
              SUBTRACT 1 FROM SPLIT-YY.
           COMPUTE SPLIT-MM = SPLIT-MM - WS-MONTHS.
       PRR-001.
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

           MOVE 2510 TO POS
           DISPLAY "StockNumber Being Read:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

           IF WS-STORE = "   "
              MOVE ST-CATEGORY TO WS-STORE.
           IF ST-QTYONHAND = 0
              PERFORM DELETE-SPECIAL-PRICE
              GO TO PRR-005.

           COMPUTE WS-QTY = (ST-QTYONHAND + ST-QTYONRESERVE) - 
                  ST-QTYONBORDER.
           IF WS-QTY = 0
              PERFORM DELETE-SPECIAL-PRICE
              GO TO PRR-005.
           COMPUTE STPR-PRICE = ST-AVERAGECOST * WS-FACTOR.
           IF STPR-PRICE NOT < ST-PRICE
              PERFORM DELETE-SPECIAL-PRICE
              GO TO PRR-005.
           IF ST-LASTRECEIPTDATE > SPLIT-DATE
              PERFORM DELETE-SPECIAL-PRICE
              GO TO PRR-005.
           IF ST-MAXIMUMLEVEL = 0
            IF WS-QTY > 0
              GO TO PRR-010.
           COMPUTE WS-MARGIN = WS-QTY / ST-MAXIMUMLEVEL.
           IF WS-MARGIN NOT < 2
              GO TO PRR-010.
           PERFORM DELETE-SPECIAL-PRICE.
           GO TO PRR-005.
       PRR-010.
           IF LINE-CNT < 64
               GO TO PRR-020.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE
           MOVE " " TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC AFTER 1
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE 5 TO LINE-CNT.
       PRR-020.
           IF ST-CATEGORY NOT = WS-STORE
               MOVE " " TO PRINT-REC
               MOVE ST-CATEGORY TO WS-STORE
               WRITE PRINT-REC
               ADD 1 TO LINE-CNT.
           MOVE ST-STOCKNUMBER    TO D-STOCK
           MOVE ST-DESCRIPTION1   TO D-DESC1
           MOVE ST-DESCRIPTION2   TO D-DESC2
           MOVE ST-QTYONHAND      TO D-ONHAND
           MOVE ST-PRICE          TO D-PRICE
           COMPUTE STPR-PRICE = ST-AVERAGECOST * WS-FACTOR
           MOVE STPR-PRICE        TO D-SPRICE
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           ADD 1 TO LINE-CNT
           MOVE " " TO PRINT-REC DETAIL-LINE.
       PRR-025.
           IF WS-ANSWER3 = "Y"
               PERFORM WRITE-SPECIAL-PRICE-LIST.
           GO TO PRR-005.
       PRR-999.
           EXIT.
      *
       WRITE-SPECIAL-PRICE-LIST SECTION.
       WSPL-010.
           MOVE ST-STOCKNUMBER TO STPR-STOCKNUMBER.
           MOVE WS-DATE TO STPR-DATE.
       WSPL-020.
           WRITE STPR-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 = 22 OR 23 OR 35 OR 49
               GO TO WSPL-030.
           IF WS-STPR-ST1 NOT = 0
               MOVE "WRITING OF NEW PRICE LIST IN ERROR, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               GO TO WSPL-030.
           GO TO WSPL-999.
       WSPL-030.
           REWRITE STPR-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
               GO TO WSPL-999.
           IF WS-STPR-ST1 NOT = 0
            MOVE "REWRITING OF NEW PRICE LIST IN ERROR, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               GO TO WSPL-020.
       WSPL-999.
           EXIT.
      *
       SPECIAL-PRINT-ROUTINE SECTION.
       SPR-000.
           MOVE WS-ANSWER1 TO STPR-STOCKNUMBER.
           START STPR-MASTER KEY NOT < STPR-KEY
              INVALID KEY NEXT SENTENCE.
       SPR-005.
           READ STPR-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-STPR-ST1 = 10
               GO TO SPR-999.
           IF WS-STPR-ST1 NOT = 0
             MOVE "ST-PRICE BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STPR-ST1
               GO TO SPR-005.
           IF STPR-STOCKNUMBER < WS-ANSWER1
              GO TO SPR-005.
           IF STPR-STOCKNUMBER > WS-ANSWER2
              GO TO SPR-999.

           MOVE 2510 TO POS
           DISPLAY "StockNumber Being Read:" AT POS
           ADD 25 TO POS
           DISPLAY STPR-STOCKNUMBER AT POS.

           MOVE STPR-STOCKNUMBER TO SPLIT-STOCK.
           IF WS-STORE = "   "
              MOVE SP-1ST3 TO WS-STORE.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
       SPR-010.
           IF LINE-CNT < 64
               GO TO SPR-020.
           ADD 1 TO PAGE-CNT
           MOVE STPR-DATE    TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE
           MOVE PAGE-CNT     TO H1-PAGE
           MOVE " " TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC AFTER 1
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE 5 TO LINE-CNT.
       SPR-020.
           IF SP-1ST3 NOT = WS-STORE
               MOVE " " TO PRINT-REC
               MOVE SP-1ST3 TO WS-STORE
               WRITE PRINT-REC
               ADD 1 TO LINE-CNT.
           PERFORM READ-STOCK.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               PERFORM DSP-105
               GO TO SPR-005.
           IF ST-QTYONHAND = 0
               PERFORM DSP-105
               GO TO SPR-005.
      
           COMPUTE WS-QTY = (ST-QTYONHAND + ST-QTYONRESERVE).
      
           IF WS-QTY < ST-QTYONBORDER
              PERFORM DSP-105
              GO TO SPR-005.
           IF STPR-PRICE NOT < ST-PRICE
              PERFORM DSP-105
              GO TO SPR-005.
      *     IF ST-MAXIMUMLEVEL = 0
      *      IF WS-QTY - ST-QTYONBORDER > 0
      *        GO TO SPR-021.
      *     COMPUTE WS-MARGIN = WS-QTY / ST-MAXIMUMLEVEL.
      *     IF WS-MARGIN < 2
      *        PERFORM DSP-105
      *        GO TO SPR-005.
       SPR-021.
           MOVE ST-STOCKNUMBER    TO D-STOCK
           MOVE ST-DESCRIPTION1   TO D-DESC1
           MOVE ST-DESCRIPTION2   TO D-DESC2
           MOVE ST-QTYONHAND      TO D-ONHAND
           MOVE ST-PRICE          TO D-PRICE
           MOVE STPR-PRICE        TO D-SPRICE
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           ADD 1 TO LINE-CNT
           MOVE " " TO PRINT-REC DETAIL-LINE.
       SPR-025.
           GO TO SPR-005.
       SPR-999.
           EXIT.
      *
       DELETE-SPECIAL-PRICE SECTION.
       DSP-000.
           MOVE ST-STOCKNUMBER TO STPR-STOCKNUMBER.
           START STPR-MASTER KEY NOT < STPR-KEY
              INVALID KEY NEXT SENTENCE.
       DSP-005.
           READ STPR-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
               GO TO DSP-999.
           IF WS-STPR-ST1 NOT = 0
              MOVE "ST-PRICES BUSY ON READ AT DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STPR-ST1 TO WS-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STPR-ST1
              GO TO DSP-005.
       DSP-105.
           DELETE STPR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
               GO TO DSP-999.
           IF WS-STPR-ST1 NOT = 0
              MOVE "ST-PRICES BUSY ON DELETE, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STPR-ST1 TO WS-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STPR-ST1
              GO TO DSP-105.
       DSP-999.
           EXIT.
      *
       READ-STOCK SECTION.
       RS-001.
           MOVE STPR-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       RS-005.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "**INVALID NUMBER**" TO ST-DESCRIPTION1
               MOVE " "                  TO ST-DESCRIPTION2
               MOVE 0                    TO STPR-PRICE
                                            ST-QTYONHAND
               GO TO RS-999.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK BUSY ON READ, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STOCK-ST1 TO WS-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STOCK-ST1
              GO TO RS-005.
       RS-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-035.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-035.
       OPEN-040.
            OPEN I-O STPR-MASTER.
            IF WS-STPR-ST1 NOT = 0
               MOVE 0 TO WS-STPR-ST1
               MOVE "STOCK PRICE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-040.
       OPEN-070.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE STPR-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       OPEN-999.
            EXIT.
      *   
       END-OFF SECTION.
       END-500.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           CLOSE STOCK-MASTER
                 STPR-MASTER.
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
