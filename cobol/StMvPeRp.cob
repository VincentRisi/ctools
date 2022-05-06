        IDENTIFICATION DIVISION.
        PROGRAM-ID. StMvPeRp.
        AUTHOR. CHRISTENSEN.
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
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(15) VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-UNITS-MTD         PIC S9(6) VALUE 0.
       77  WS-UNITS-YTD         PIC S9(6) VALUE 0.
       77  WS-VALUE             PIC S9(7)V99 VALUE 0.
       77  WS-VALUE-MTD         PIC S9(7)V99 VALUE 0.
       77  WS-VALUE-YTD         PIC S9(7)V99 VALUE 0.
       77  TOT-UNITS-MTD        PIC S9(6) VALUE 0.
       77  TOT-UNITS-YTD        PIC S9(6) VALUE 0.
       77  TOT-VALUE-MTD        PIC S9(7)V99 VALUE 0.
       77  TOT-VALUE-YTD        PIC S9(7)V99 VALUE 0.
       77  WS-VALUE-DIS         PIC Z(6)9.99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(10) VALUE " ".
           03  FILLER         PIC X(61) VALUE
           "S T O C K   M O V E D   R E P O R T".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(15) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(35) VALUE ALL "*".
           03  FILLER         PIC X(68) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(71) VALUE " ".
           03  FILLER         PIC X(61) VALUE "PERIOD/YEAR TO DATE".
       01  HEAD4.
           03  FILLER         PIC X(18) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(43) VALUE "DESCRIPTION".
           03  FILLER         PIC X(8) VALUE "CAT".
           03  FILLER         PIC X(17) VALUE " UNITS".
           03  FILLER         PIC X(46) VALUE "VALUE".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(18) VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(30) VALUE " ".
           03  D-UNIT         PIC Z(5)9-.
           03  FILLER         PIC X(6) VALUE " ".
           03  D-VALUE        PIC Z(6)9.99-.
           03  FILLER         PIC X(51) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(61) VALUE " ".
           03  T-CATEGORY     PIC X(6) VALUE " ".
           03  T-UNIT         PIC Z(6)9-.
           03  FILLER         PIC X(5) VALUE " ".
           03  T-VALUE        PIC Z(7)9.99-.
           03  FILLER         PIC X(41) VALUE " ".
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
           DISPLAY "** STOCK MOVED THIS PERIOD/YEAR REPORT **" AT POS
           MOVE 417 TO POS
           DISPLAY "*****************************************" AT POS.
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
               MOVE " " TO WS-ANSWER3
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO CONTROL-022
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-022.
           MOVE 2810 TO POS
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
              AT POS.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE
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
              PERFORM PRR-025
              GO TO PRR-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ-NEXT, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
               PERFORM PRR-025.
           ADD ST-SALESUNITMTD  TO WS-UNITS-MTD
                                   TOT-UNITS-MTD.
           ADD ST-SALESUNITSYTD TO WS-UNITS-YTD
                                   TOT-UNITS-YTD.
           COMPUTE WS-VALUE = ST-SALESUNITMTD * ST-AVERAGECOST.
           ADD WS-VALUE     TO WS-VALUE-MTD
                               TOT-VALUE-MTD.
           COMPUTE WS-VALUE = ST-SALESUNITSYTD * ST-AVERAGECOST.
           ADD WS-VALUE     TO WS-VALUE-YTD
                               TOT-VALUE-YTD.
           IF WS-ANSWER3 = "Y"
               GO TO PRR-005.
       PRR-022.
           MOVE ST-STOCKNUMBER   TO D-STOCK
           MOVE ST-DESCRIPTION1  TO D-DESC1
           MOVE ST-DESCRIPTION2  TO D-DESC2
           MOVE ST-SALESUNITMTD  TO D-UNIT
           COMPUTE WS-VALUE = ST-SALESUNITMTD * ST-AVERAGECOST
           MOVE WS-VALUE         TO D-VALUE
           WRITE PRINT-REC FROM DETAIL-LINE

           MOVE " " TO PRINT-REC DETAIL-LINE
           MOVE ST-SALESUNITSYTD TO D-UNIT
           COMPUTE WS-VALUE = ST-SALESUNITSYTD * ST-AVERAGECOST
           MOVE WS-VALUE         TO D-VALUE
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE

           WRITE PRINT-REC
           ADD 3 TO LINE-CNT
           GO TO PRR-005.
       PRR-025.
           MOVE WS-STORE     TO T-CATEGORY
           MOVE WS-UNITS-MTD TO T-UNIT
           MOVE WS-VALUE-MTD TO T-VALUE
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE " " TO PRINT-REC TOTAL-LINE

           MOVE WS-UNITS-YTD TO T-UNIT
           MOVE WS-VALUE-YTD TO T-VALUE
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE " " TO PRINT-REC TOTAL-LINE
           WRITE PRINT-REC.

           ADD 3 TO LINE-CNT
           MOVE 1610 TO POS
           DISPLAY "Category: " AT POS
           MOVE 1620 TO POS
           DISPLAY WS-STORE AT POS
           MOVE 1710 TO POS
           DISPLAY "Value Sold This Month: R" AT POS
           MOVE 1734 TO POS
           MOVE WS-VALUE-MTD TO WS-VALUE-DIS
           DISPLAY WS-VALUE-DIS AT POS.
           MOVE 0 TO WS-UNITS-MTD
                     WS-UNITS-YTD
                     WS-VALUE-MTD
                     WS-VALUE-YTD.
           IF WS-STORE = "XRN"
             IF WS-ANSWER3 = "Y"
               PERFORM END-000.
           MOVE ST-CATEGORY TO WS-STORE.  
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-022.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-022.
       OPEN-040.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE Ws-Co-Name TO CO-NAME
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           MOVE "TOTALS*"     TO T-CATEGORY
           MOVE TOT-UNITS-MTD TO T-UNIT
           MOVE TOT-VALUE-MTD TO T-VALUE
           WRITE PRINT-REC FROM TOTAL-LINE

           MOVE " " TO PRINT-REC TOTAL-LINE

           MOVE TOT-UNITS-YTD TO T-UNIT
           MOVE TOT-VALUE-YTD TO T-VALUE
           WRITE PRINT-REC FROM TOTAL-LINE

           MOVE " " TO PRINT-REC TOTAL-LINE
           WRITE PRINT-REC AFTER 2.
           ADD 4 TO LINE-CNT.
       END-500.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-800.
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
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
