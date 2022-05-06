        IDENTIFICATION DIVISION.
        PROGRAM-ID. StQyAdRp.
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
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(15) VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
       77  WS-ANSWER4           PIC X VALUE " ".
       77  WS-LINE-PRINTED      PIC X VALUE " ".
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-ADJ               PIC Z(6)9.99-.
       77  WS-ADJAMT            PIC S9(7)V99 VALUE 0.
       77  WS-ADJQTY-MTD        PIC S9(6) VALUE 0.
       77  WS-ADJQTY-YTD        PIC S9(6) VALUE 0.
       77  WS-ADJQTY-LAST       PIC S9(6) VALUE 0.
       77  WS-ADJAMT-MTD        PIC S9(7)V99 VALUE 0.
       77  WS-ADJAMT-YTD        PIC S9(7)V99 VALUE 0.
       77  WS-ADJAMT-LAST       PIC S9(7)V99 VALUE 0.
       77  TOT-ADJQTY-MTD       PIC S9(6) VALUE 0.
       77  TOT-ADJQTY-YTD       PIC S9(6) VALUE 0.
       77  TOT-ADJQTY-LAST      PIC S9(6) VALUE 0.
       77  TOT-ADJAMT-MTD       PIC S9(7)V99 VALUE 0.
       77  TOT-ADJAMT-YTD       PIC S9(7)V99 VALUE 0.
       77  TOT-ADJAMT-LAST      PIC S9(7)V99 VALUE 0.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(61) VALUE
            "STOCK LOSS(-)/GAIN DUE TO STOCK ADJUSTMENTS".
           03  H1-CHECK       PIC X(26).
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(8) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(43) VALUE ALL "*".
           03  FILLER         PIC X(60) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(50) VALUE " ".
           03  FILLER         PIC X(17) VALUE "CATEGORY".
           03  FILLER         PIC X(32) VALUE "ADJUSTED QUANTITY".
           03  FILLER         PIC X(27) VALUE "ADJUSTED VALUE".
       01  HEAD4.
           03  FILLER         PIC X(18) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(48) VALUE "DESCRIPTION".
           03  FILLER         PIC X(28) VALUE "MTD     YTD  L/YEAR".
           03  FILLER         PIC X(38) VALUE
            "MTD         YTD      L/YEAR".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(18) VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(25) VALUE " ".
           03  D-ADJQTY-MTD   PIC Z(5)9-.
           03  FILLER         PIC X VALUE " ".
           03  D-ADJQTY-YTD   PIC Z(5)9-.
           03  FILLER         PIC X VALUE " ".
           03  D-ADJQTY-LAST  PIC Z(5)9-.
           03  FILLER         PIC X VALUE " ".
           03  D-ADJAMT-MTD   PIC Z(6)9.99-.
           03  FILLER         PIC X VALUE " ".
           03  D-ADJAMT-YTD   PIC Z(6)9.99-.
           03  FILLER         PIC X VALUE " ".
           03  D-ADJAMT-LAST  PIC Z(6)9.99-.
           03  FILLER         PIC X(10) VALUE " ".
       01  UNDER-LINE.
           03  FILLER         PIC X(63) VALUE " ".
           03  FILLER         PIC X(7) VALUE "------".
           03  FILLER         PIC X VALUE " ".
           03  FILLER         PIC X(7) VALUE "------".
           03  FILLER         PIC X VALUE " ".
           03  FILLER         PIC X(7) VALUE "------".
           03  FILLER         PIC X VALUE " ".
           03  FILLER         PIC X(11) VALUE "----------".
           03  FILLER         PIC X VALUE " ".
           03  FILLER         PIC X(11) VALUE "----------".
           03  FILLER         PIC X VALUE " ".
           03  FILLER         PIC X(11) VALUE "----------".
           03  FILLER         PIC X(10) VALUE " ".
       01  DBLUNDER-LINE.
           03  FILLER         PIC X(63) VALUE " ".
           03  FILLER         PIC X(7) VALUE "======".
           03  FILLER         PIC X VALUE " ".
           03  FILLER         PIC X(7) VALUE "======".
           03  FILLER         PIC X VALUE " ".
           03  FILLER         PIC X(7) VALUE "======".
           03  FILLER         PIC X VALUE " ".
           03  FILLER         PIC X(11) VALUE "==========".
           03  FILLER         PIC X VALUE " ".
           03  FILLER         PIC X(11) VALUE "==========".
           03  FILLER         PIC X VALUE " ".
           03  FILLER         PIC X(11) VALUE "==========".
           03  FILLER         PIC X(10) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(52) VALUE " ".
           03  T-CATEGORY     PIC X(10) VALUE " ".
           03  T-ADJQTY-MTD   PIC Z(6)9-.
           03  T-ADJQTY-YTD   PIC Z(6)9-.
           03  T-ADJQTY-LAST  PIC Z(6)9-.
           03  T-ADJAMT-MTD   PIC Z(7)9.99-.
           03  T-ADJAMT-YTD   PIC Z(7)9.99-.
           03  T-ADJAMT-LAST  PIC Z(7)9.99-.
           03  FILLER         PIC X(10) VALUE " ".
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
           DISPLAY "** STOCK LOSS DUE TO ADJUSTMENTS REPORT **" AT POS.
           MOVE 417 TO POS.
           DISPLAY "******************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1010 TO POS.
           DISPLAY "   FROM STOCK NUMBER : [               ]" AT POS.
           MOVE 1034 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7        TO CDA-ROW.
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
           MOVE 9        TO CDA-ROW.
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
               GO TO CONTROL-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-025.
           MOVE 1610 TO POS.
           DISPLAY "SEARCH FOR: L=L/YEAR; Y=YTD; M=MTD [ ]" AT POS.
           ADD 36 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 45        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER4.

      *     ACCEPT WS-ANSWER4 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-020.
           IF WS-ANSWER4 NOT = "L" AND NOT = "Y" AND NOT = "M"
               MOVE " " TO WS-ANSWER4
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-025.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO CONTROL-027
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-025.
       CONTROL-027.
           MOVE 3010 TO POS
           DISPLAY "The Report is in progress......." AT POS.
           PERFORM OPEN-FILES
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE "N" TO WS-LINE-PRINTED.
           MOVE WS-ANSWER1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       PRR-005.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
            IF WS-LINE-PRINTED = "N"
              GO TO PRR-999
            ELSE
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
 
           MOVE 2510 TO POS
           DISPLAY "STOCKNUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

          IF ST-STOCKNUMBER < WS-ANSWER1
              GO TO PRR-005.
           IF ST-STOCKNUMBER > WS-ANSWER2
              PERFORM PRR-025
              GO TO PRR-999.
           IF WS-STORE = "   "
              MOVE ST-CATEGORY TO WS-STORE.
           IF WS-ANSWER4 = "L"
            IF ST-QTYADJLAST NOT = 0
              GO TO PRR-010.
           IF WS-ANSWER4 = "Y"
            IF ST-QTYADJYTD NOT = 0
              GO TO PRR-010.
           IF WS-ANSWER4 = "M"
            IF ST-QTYADJMTD NOT = 0
              GO TO PRR-010.
           GO TO PRR-005.
       PRR-010.
           IF LINE-CNT < 60
              GO TO PRR-020.
           ADD 1         TO PAGE-CNT.
           MOVE PAGE-CNT TO H1-PAGE.
           IF WS-ANSWER4 = "L"
              MOVE "SEARCH BASED ON LAST YEAR" TO H1-CHECK.
           IF WS-ANSWER4 = "Y"
              MOVE "SEARCH BASED ON YTD"       TO H1-CHECK.
           IF WS-ANSWER4 = "M"
              MOVE "SEARCH BASED ON MTD"       TO H1-CHECK.
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
            IF WS-LINE-PRINTED = "N"
              MOVE ST-CATEGORY TO WS-STORE
            ELSE
              PERFORM PRR-025.
           ADD ST-QTYADJMTD  TO WS-ADJQTY-MTD
           ADD ST-QTYADJYTD  TO WS-ADJQTY-YTD
           ADD ST-QTYADJLAST TO WS-ADJQTY-LAST
           COMPUTE WS-ADJAMT-MTD = WS-ADJAMT-MTD +
                   (ST-QTYADJMTD * ST-AVERAGECOST)
           COMPUTE WS-ADJAMT-YTD = WS-ADJAMT-YTD +
                   (ST-QTYADJYTD * ST-AVERAGECOST)
           COMPUTE WS-ADJAMT-LAST = WS-ADJAMT-LAST +
                  (ST-QTYADJLAST * ST-AVERAGECOST).
       PRR-022.
           IF WS-ANSWER3 = "Y"
               MOVE "Y" TO WS-LINE-PRINTED
               GO TO PRR-005.
           MOVE ST-STOCKNUMBER   TO D-STOCK
           MOVE ST-DESCRIPTION1  TO D-DESC1
           MOVE ST-DESCRIPTION2  TO D-DESC2
           MOVE ST-QTYADJMTD     TO D-ADJQTY-MTD
           MOVE ST-QTYADJYTD     TO D-ADJQTY-YTD
           MOVE ST-QTYADJLAST    TO D-ADJQTY-LAST
           COMPUTE WS-ADJAMT = ST-QTYADJMTD * ST-AVERAGECOST
           MOVE WS-ADJAMT        TO D-ADJAMT-MTD
           COMPUTE WS-ADJAMT = ST-QTYADJYTD * ST-AVERAGECOST
           MOVE WS-ADJAMT        TO D-ADJAMT-YTD
           COMPUTE WS-ADJAMT = ST-QTYADJLAST * ST-AVERAGECOST
           MOVE WS-ADJAMT        TO D-ADJAMT-LAST
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           MOVE "Y" TO WS-LINE-PRINTED
           GO TO PRR-005.
       PRR-025.
           WRITE PRINT-REC FROM UNDER-LINE
           MOVE WS-STORE       TO T-CATEGORY
           MOVE WS-ADJQTY-MTD  TO T-ADJQTY-MTD
           MOVE WS-ADJQTY-YTD  TO T-ADJQTY-YTD
           MOVE WS-ADJQTY-LAST TO T-ADJQTY-LAST
           MOVE WS-ADJAMT-MTD  TO T-ADJAMT-MTD
           MOVE WS-ADJAMT-YTD  TO T-ADJAMT-YTD
           MOVE WS-ADJAMT-LAST TO T-ADJAMT-LAST
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           WRITE PRINT-REC FROM DBLUNDER-LINE
           ADD WS-ADJQTY-MTD  TO TOT-ADJQTY-MTD
           ADD WS-ADJQTY-YTD  TO TOT-ADJQTY-YTD
           ADD WS-ADJQTY-LAST TO TOT-ADJQTY-LAST
           ADD WS-ADJAMT-MTD  TO TOT-ADJAMT-MTD
           ADD WS-ADJAMT-YTD  TO TOT-ADJAMT-YTD
           ADD WS-ADJAMT-LAST TO TOT-ADJAMT-LAST
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.

           ADD 4 TO LINE-CNT
           MOVE 2010 TO POS
           DISPLAY "Category: " AT POS
           MOVE 2020 TO POS
           DISPLAY WS-STORE AT POS
           MOVE 2110 TO POS
           DISPLAY "Adjustments This Month : R" AT POS
           MOVE 2136 TO POS
           MOVE WS-ADJAMT-MTD TO WS-ADJ
           DISPLAY WS-ADJ AT POS.
           MOVE 0 TO WS-ADJQTY-MTD
                     WS-ADJQTY-YTD
                     WS-ADJQTY-LAST
                     WS-ADJAMT-MTD
                     WS-ADJAMT-YTD
                     WS-ADJAMT-LAST.
          MOVE "N" TO WS-LINE-PRINTED.
          MOVE ST-CATEGORY TO WS-STORE.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-030.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-030.
       OPEN-040.
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
           IF LINE-CNT > 58
                 MOVE 66 TO LINE-CNT
                 PERFORM PRR-010.
           WRITE PRINT-REC
           WRITE PRINT-REC FROM UNDER-LINE
           MOVE "TOTALS***"     TO T-CATEGORY
           MOVE TOT-ADJQTY-MTD  TO T-ADJQTY-MTD
           MOVE TOT-ADJQTY-YTD  TO T-ADJQTY-YTD
           MOVE TOT-ADJQTY-LAST TO T-ADJQTY-LAST
           MOVE TOT-ADJAMT-MTD  TO T-ADJAMT-MTD
           MOVE TOT-ADJAMT-YTD  TO T-ADJAMT-YTD
           MOVE TOT-ADJAMT-LAST TO T-ADJAMT-LAST
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DBLUNDER-LINE.
           
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE STOCK-MASTER.
       END-900.
      *     STOP RUN.
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
