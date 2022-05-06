        IDENTIFICATION DIVISION.
        PROGRAM-ID. StByGdRp.
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
           03  FILLER           PIC X(150).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(15) VALUE " ".
       77  WS-ANALYSIS          PIC X(1) VALUE " ".
       77  WS-STORE             PIC X(3) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10) VALUE " ".
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(15) VALUE "B U Y E R S   G".
           03  FILLER         PIC X(17) VALUE " U I D E   R E P ".
           03  FILLER         PIC X(5) VALUE "O R T".
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(15) VALUE "ANALYSIS TYPE: ".
           03  H1-ANALYSIS    PIC X(3) VALUE " ".
           03  FILLER         PIC X(33) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(37) VALUE ALL "*".
           03  FILLER         PIC X(66) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(64) VALUE " ".
           03  FILLER         PIC X(10) VALUE "LEVEL".
           03  FILLER         PIC X(19) VALUE "STOCK QTY ON:".
           03  FILLER         PIC X(23) VALUE "SALES HISTORY".
           03  FILLER         PIC X(19) VALUE "LAST DATE OF:".
       01  HEAD4.
           03  FILLER         PIC X(18) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(43) VALUE "DESCRIPTION".
           03  FILLER         PIC X(11) VALUE "MAX   MIN".
           03  FILLER         PIC X(19) VALUE "HAND  ORDER   B/O".
           03  FILLER         PIC X(19) VALUE "PTD    YTD   L/YR".
           03  FILLER         PIC X(26) VALUE 
           "RECEIPT    ORDER      SALE".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(15) VALUE " ".
           03  D-BELOW        PIC XX VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(20) VALUE " ".
           03  D-MAX          PIC Z(5)9.
           03  D-MIN          PIC Z(5)9.
           03  D-HAND         PIC Z(5)9.
           03  D-ORDER        PIC Z(5)9.
           03  D-BO           PIC Z(5)9.
           03  D-PTD          PIC Z(5)9-.
           03  D-YTD          PIC Z(5)9-.
           03  D-LYR          PIC Z(5)9-.
           03  D-RECEIPT      PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-ORDERED      PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SALE         PIC X(10).
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 322 TO POS
           DISPLAY "** STOCK BUYERS GUIDE REPORT **" AT POS
           MOVE 422 TO POS
           DISPLAY "*******************************" AT POS.
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
           MOVE 1510 TO POS.
           DISPLAY "Enter An Analysis Type To Be Printed" AT POS.
           MOVE 1610 TO POS.
           DISPLAY "Leave BLANK To Print All Stock Items" AT POS.
           MOVE 1410 TO POS.
           DISPLAY "       ANALYSIS TYPE : [ ]" AT POS.
           MOVE 1434 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 33        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANALYSIS.

      *     ACCEPT WS-ANALYSIS AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF WS-ANALYSIS = " "
              MOVE "ALL"      TO H1-ANALYSIS
           ELSE
              MOVE WS-ANALYSIS TO H1-ANALYSIS.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-022
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-022.
           MOVE 2810 TO POS.
           DISPLAY "Report Is Being Compiled, Please Be Patient."
           AT POS.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE
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
              
           MOVE 2520 TO POS
           DISPLAY "Stock Number Being Read:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
              
           IF WS-STORE = "   "
              MOVE ST-CATEGORY TO WS-STORE.
           IF WS-ANALYSIS = " "
              GO TO PRR-010.
           IF WS-ANALYSIS = "T"
            IF ST-ANALYSIS = "T" OR = "B"
              GO TO PRR-010.
           IF WS-ANALYSIS = "P"
            IF ST-ANALYSIS = "P" OR = "B"
              GO TO PRR-010.
           IF ST-ANALYSIS NOT = WS-ANALYSIS
              GO TO PRR-005.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-020.
            ADD 1         TO PAGE-CNT.
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
            WRITE PRINT-REC
            MOVE 8 TO LINE-CNT.
       PRR-020.
           IF ST-CATEGORY NOT = WS-STORE
               PERFORM PRR-030.
           MOVE ST-STOCKNUMBER     TO D-STOCK
           MOVE ST-ANALYSIS        TO D-BELOW
           IF (ST-QTYONHAND + ST-QTYONRESERVE) < ST-MINIMUMLEVEL
               MOVE "*"            TO D-BELOW.
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE ST-MAXIMUMLEVEL    TO D-MAX
           MOVE ST-MINIMUMLEVEL    TO D-MIN
           COMPUTE ST-QTYONHAND = ST-QTYONHAND + ST-QTYONRESERVE
           MOVE ST-QTYONHAND       TO D-HAND
           MOVE ST-QTYONORDER      TO D-ORDER
           MOVE ST-QTYONBORDER     TO D-BO
           MOVE ST-SALESUNITMTD    TO D-PTD
           MOVE ST-SALESUNITSYTD   TO D-YTD
           MOVE ST-SALESUNITSLAST  TO D-LYR
           MOVE ST-LASTRECEIPTDATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-RECEIPT
           MOVE ST-LASTORDERDATE   TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-ORDERED
           MOVE ST-LASTSALEDATE    TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D-SALE

           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC DETAIL-LINE
           ADD 1 TO LINE-CNT
           GO TO PRR-005.
       PRR-030.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE
            WRITE PRINT-REC BEFORE PAGE
            WRITE PRINT-REC FROM COMPANY-LINE
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
            WRITE PRINT-REC
            MOVE 8 TO LINE-CNT
            MOVE ST-CATEGORY TO WS-STORE.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-025.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-025.
       OPEN-040.
           MOVE Ws-Co-Name TO CO-NAME.
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
            IF LINE-CNT > 60
               PERFORM PRR-010.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE "'D' = STOCK TO BE DELETED WHEN ON HAND = ZERO."
              TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC
          MOVE "'*' = ON-HAND + ON-RESERVE - ON-BORDER < MINIMUM LEVEL."
              TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC
           MOVE "'N' = STOCK NOT TO BE RE-ORDERED, INTERNAL NUMBER."
              TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC
           MOVE "'T' = TOOLKIT ITEM, MIN/MAX LEVELS ARE 30% HIGHER."
              TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC
           MOVE "'P' = PRODUCTION EQUIPMENT ITEM." TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC
           MOVE "'B' = BOTH PRODUCTION EQUIPMENT & TOOLKIT ITEM." 
              TO PRINT-REC
           WRITE PRINT-REC.
           MOVE " " TO PRINT-REC
           MOVE 
           "'S' = ITEM IS A 'SPECIAL ORDER', ORDER ONLY SUFFICIENT " &
           "FOR B/O'S." TO PRINT-REC
           WRITE PRINT-REC.
           
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
           CLOSE PRINT-FILE
                 STOCK-MASTER.
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
