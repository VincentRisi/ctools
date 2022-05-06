        IDENTIFICATION DIVISION.
        PROGRAM-ID. BmKtLiRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
       Copy "SelectStMaster".
       Copy "SelectBmMaster".
       Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdToolkit.
           COPY ChlfdDaily.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(200).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(35) VALUE " ".
       77  WS-NO-COPIES-ACCEPT  PIC X(2) VALUE " ".
       77  WS-NO-COPIES         PIC 9(2) VALUE 0.
       77  WS-NO-PRINTED        PIC 9(2) VALUE 0.
       77  WS-KIT-NAME          PIC X(15) VALUE " ".
       77  WS-KIT-DESC1         PIC X(20) VALUE " ".
       77  WS-KIT-DESC2         PIC X(20) VALUE " ".
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1     PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-ST-ST1        PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-TOOLKIT-ST1   PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(64) VALUE
           "T O O L K I T   I T E M   C H E C K-L I S T   R E P O R T".
           03  FILLER         PIC X(24) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(7) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(57) VALUE ALL "*".
           03  FILLER         PIC X(45) VALUE " ".
       01  HEAD3.
           03  H3-NAME        PIC X(18) VALUE " ".
           03  N-PRINT-SW1    PIC X(2).
           03  H3-SUB-LINE.
               05  H3-DESC1   PIC X(20) VALUE " ".
               05  H3-DESC2   PIC X(90) VALUE " ".
           03  N-PRINT-SW2    PIC X(2).
           03  FILLER         PIC X(107) VALUE " ".
       01  HEAD5.
           03  FILLER         PIC X(17) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(45) VALUE "DESCRIPTION".
           03  FILLER         PIC X(8) VALUE "QTY.".
           03  FILLER         PIC X(40) VALUE "CHECKED".
           03  FILLER         PIC X(17) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(45) VALUE "DESCRIPTION".
           03  FILLER         PIC X(8) VALUE "QTY.".
           03  FILLER         PIC X(20) VALUE "CHECKED".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(17).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(25).
           03  D-QTY          PIC Z99.
           03  FILLER         PIC X(5) VALUE " " .
           03  FILLER         PIC X(40) VALUE "-------".
           03  DETAIL-LINE-1.
               05  D-STOCK-1      PIC X(17).
               05  D-DESC1-1      PIC X(20).
               05  D-DESC2-1      PIC X(25).
               05  D-QTY-1        PIC Z99.
               05  FILLER         PIC X(5) VALUE " " .
               05  D-LINES        PIC X(20) VALUE "-------".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 325 TO POS
           DISPLAY "** TOOLKIT ITEM CHECK LIST FOR CUSTOMERS **" AT POS
           MOVE 425 TO POS
           DISPLAY "*******************************************" AT POS.
           MOVE 2705 TO POS
           DISPLAY 
           "THIS REPORT MUST GO TO A WIDE PRINTER (255CHARS), " &
           "SELECT CAREFULLY. "
           AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM ERROR1-020.
           MOVE 2901 TO POS
           DISPLAY WS-MESSAGE AT POS.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO WS-RANGE1 WS-RANGE2.
            MOVE 1010 TO POS.
            DISPLAY "TOOLKIT NUMBER     : [               ]" AT POS.
            MOVE 1032 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 31        TO CDA-COL.
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
            MOVE 1210 TO POS.
            DISPLAY
            "ENTER CUSTOMER NAME: [                                   ]"
                      AT POS.
            MOVE 1232 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 35        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 31        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-020.
           MOVE 1410 TO POS.
           DISPLAY "NUMBER OF COPIES   : [  ]" AT POS.
           MOVE 1432 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 31        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-NO-COPIES-ACCEPT.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
        GET-030.
            MOVE WS-NO-COPIES-ACCEPT TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-NO-COPIES F-EDNAMEFIELDANAL
            MOVE 1432 TO POS
            DISPLAY F-EDNAMEFIELDANAL AT POS.
            IF WS-NO-COPIES NOT > 0
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
       GET-550.
            MOVE 2910 TO POS.
            DISPLAY "The report is being compiled........." AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE WS-RANGE1 TO TO-TOOLKIT-NUMBER.
            MOVE " "       TO TO-COMPONENT-NUMBER.
            START TOOLKITS KEY NOT < TO-KEY
                INVALID KEY NEXT SENTENCE.
       PRR-002.
            READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
            IF WS-TOOLKIT-ST1 = 10
               GO TO PRR-600.
            IF WS-TOOLKIT-ST1 NOT = 0
            MOVE "TOOLKITS BUSY READ-NEXTN, GOING TO RETRY IN 1 SECOND"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR-020
              CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO PRR-002.
            IF TO-TOOLKIT-NUMBER < WS-RANGE1
               GO TO PRR-002.
            IF TO-TOOLKIT-NUMBER > WS-RANGE1
               GO TO PRR-600.

            IF WS-KIT-NAME = " "
               MOVE TO-TOOLKIT-NUMBER TO WS-KIT-NAME
                                         ST-STOCKNUMBER
               PERFORM RS-010
               MOVE ST-DESCRIPTION1  TO WS-KIT-DESC1
               MOVE ST-DESCRIPTION2  TO WS-KIT-DESC2
               GO TO PRR-002.
            PERFORM READ-STOCK.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-020.
            ADD 1         TO PAGE-CNT.
            MOVE PAGE-CNT TO H1-PAGE.
            MOVE " "      TO PRINT-REC.
            IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2
            MOVE " " TO PRINT-REC.

            MOVE WS-PRINT-BOLD       TO N-PRINT-SW1
            MOVE WS-PRINT-UNBOLD     TO N-PRINT-SW2
            MOVE "CUSTOMER        :" TO H3-NAME
            MOVE WS-RANGE2           TO H3-SUB-LINE
            WRITE PRINT-REC FROM HEAD3.

            MOVE " "                 TO PRINT-REC
            MOVE "TOOLKIT NUMBER  :" TO H3-NAME
            MOVE WS-KIT-NAME         TO H3-SUB-LINE
            WRITE PRINT-REC FROM HEAD3.

            MOVE " "                 TO PRINT-REC HEAD3
            MOVE "KIT DESCRIPTION :" TO H3-NAME
            MOVE WS-KIT-DESC1        TO H3-DESC1
            MOVE WS-KIT-DESC2        TO H3-DESC2
            WRITE PRINT-REC FROM HEAD3
            MOVE " "                 TO PRINT-REC HEAD3
            MOVE WS-PRINT-UNBOLD     TO N-PRINT-SW2
            MOVE WS-PRINT-COMP       TO N-PRINT-SW1
            WRITE PRINT-REC FROM HEAD3
            MOVE " "                 TO PRINT-REC HEAD3
            WRITE PRINT-REC FROM HEAD5
            MOVE " "                 TO PRINT-REC
            MOVE 7 TO LINE-CNT.
       PRR-020.
           MOVE ST-STOCKNUMBER     TO D-STOCK
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE TO-QUANTITY        TO D-QTY.
       PRR-030.
            READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
            IF WS-TOOLKIT-ST1 = 10
               MOVE " " TO DETAIL-LINE-1
               PERFORM PRR-040
               GO TO PRR-600.
            IF WS-TOOLKIT-ST1 NOT = 0
             MOVE "TOOLKITS BUSY READ-NEXT, GOING TO RETRY IN 1 SECOND"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR-020
              CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO PRR-030.
            IF TO-TOOLKIT-NUMBER > WS-RANGE1
               MOVE " " TO DETAIL-LINE-1
               PERFORM PRR-040
               GO TO PRR-600.
            PERFORM READ-STOCK.
       PRR-035.
           MOVE ST-STOCKNUMBER     TO D-STOCK-1
           MOVE ST-DESCRIPTION1    TO D-DESC1-1
           MOVE ST-DESCRIPTION2    TO D-DESC2-1
           MOVE TO-QUANTITY        TO D-QTY-1.
       PRR-040.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT.
       PRR-050.
           GO TO PRR-002.
       PRR-600.
           MOVE WS-PRINT-NORMAL TO N-PRINT-SW2.
           WRITE PRINT-REC FROM HEAD3.
           MOVE " " TO PRINT-REC HEAD3.
           WRITE PRINT-REC.
           MOVE WS-PRINT-BOLD       TO N-PRINT-SW1
           MOVE WS-PRINT-UNBOLD     TO N-PRINT-SW2.
           MOVE "CHECKED BY:      " TO H3-SUB-LINE.
           WRITE PRINT-REC FROM HEAD3.
       
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           
           ADD 1 TO WS-NO-PRINTED.
           IF WS-NO-PRINTED = WS-NO-COPIES
               GO TO PRR-999.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           CLOSE TOOLKITS.
           PERFORM OPEN-005.
           MOVE " " TO WS-KIT-NAME
                       WS-KIT-DESC1
                       WS-KIT-DESC2.
           MOVE 66 TO LINE-CNT.
           MOVE 0 TO PAGE-CNT.
           MOVE "-------" TO D-LINES.
           GO TO PRR-000.
       PRR-999.
           EXIT.
      *
       READ-STOCK SECTION.
       RS-005.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
           MOVE TO-COMPONENT-NUMBER TO ST-STOCKNUMBER.
       RS-010.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-ST-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-ST-ST1
               MOVE "** INVALID STOCK ITE" TO ST-DESCRIPTION1
               MOVE "M IN TOOLKIT FILE **" TO ST-DESCRIPTION2
               GO TO RS-999.
          IF WS-ST-ST1 NOT = 0
              MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-ST-ST1
               GO TO RS-010.
       RS-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O STOCK-MASTER.
           IF WS-ST-ST1 NOT = 0 
              MOVE 0 TO WS-ST-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-005.
           OPEN I-O TOOLKITS.
           IF WS-TOOLKIT-ST1 NOT = 0 
              MOVE 0 TO WS-TOOLKIT-ST1
              MOVE "KIT FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.
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
           CLOSE STOCK-MASTER
                 TOOLKITS.
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
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
