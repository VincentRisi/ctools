        IDENTIFICATION DIVISION.
        PROGRAM-ID. StTrDuRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT STOCK-MASTER ASSIGN TO Ws-Stock
                ORGANIZATION IS INDEXED
                LOCK MANUAL
                ACCESS MODE IS DYNAMIC
                FILE STATUS IS WS-STOCK-STATUS
                RECORD KEY IS ST-KEY.
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
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-RANGE3            PIC X VALUE " ".
       77  WS-PERMIT            PIC X VALUE " ".
       77  WS-ANALYSIS          PIC X VALUE " ".
       77  WS-CAT               PIC XXX VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1        PIC 99.
       01  WS-TARIFF-HEADINGS-NAME.
         02  WS-TARIFF-HEADING OCCURS 500.
           03  WS-TARIFF-NUM    PIC 9(8).
           03  WS-TARIFF-DUTY   PIC 99V9.
           03  WS-TARIFF-QTY    PIC 9(6).
           03  WS-TARIFF-ERR    PIC X.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(34) VALUE
           "** STOCK REPORT DUTIES & TARIFF **".
           03  FILLER         PIC X(9) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(6) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(22) VALUE " ".
           03  FILLER         PIC X(34) VALUE ALL "*".
           03  FILLER         PIC X(40) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(10) VALUE "  TARIFF".
           03  FILLER         PIC X(8) VALUE "DUTY %".
           03  FILLER         PIC X(8) VALUE "ERROR".
           03  FILLER         PIC X(10) VALUE "QUANTITY".
       01  DETAIL-LINE.
           03  D-TARIFF       PIC Z(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PERC         PIC Z(1)9.9.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-ERROR        PIC X.
           03  FILLER         PIC X(6) VALUE " ".
           03  D-QTY          PIC Z(5)9.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "** STOCK REPORT ON DUTIES & TARIFF ERRORS **" AT POS
           MOVE 415 TO POS
           DISPLAY "********************************************"
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
           MOVE 15         TO CDA-DATALEN.
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
                DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-060
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
       GET-060.
           MOVE 2510 TO POS.
           DISPLAY "Report in progress.........." AT POS.
           PERFORM ERROR-020.
       GET-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE WS-RANGE1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
       PRR-002.
            READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-STOCK-ST1 = 10
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRR-008.
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
               GO TO PRR-008.
               
            MOVE 2310 TO POS
            DISPLAY "STOCKNUMBER BEING READ:" AT POS
            ADD 24 TO POS
            DISPLAY ST-STOCKNUMBER AT POS.
            
            PERFORM ENTER-TARIFF-NUMBER.
            GO TO PRR-002.
       PRR-008.
           IF LINE-CNT < 60
               GO TO PRR-015.
       PRR-010.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE
           MOVE " " TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
              MOVE WS-PRINT-COMP TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
              WRITE PRINT-REC FROM COMPANY-LINE AFTER 1
           ELSE
               WRITE PRINT-REC BEFORE PAGE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER 1.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE 6 TO LINE-CNT.
       PRR-015.
           MOVE 1 TO SUB-4.
       PRR-020.
           IF WS-TARIFF-NUM (SUB-4) = 0
               GO TO PRR-999.
               
           PERFORM CHECK-TARIFF-CODE.
       
      *     MOVE WS-TARIFF-NUM (SUB-4)  TO D-TARIFF
           MOVE WS-TARIFF-DUTY (SUB-4) TO D-PERC
           MOVE WS-TARIFF-QTY (SUB-4)  TO D-QTY
           MOVE WS-TARIFF-ERR (SUB-4)  TO D-ERROR.

           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC.
           
           ADD 1 TO LINE-CNT SUB-4.
           IF SUB-4 > 500
              GO TO PRR-999.
           
           IF LINE-CNT > 60
               PERFORM PRR-010.
           GO TO PRR-020.
       PRR-999.
           EXIT.
      *
       CHECK-TARIFF-CODE SECTION.
       CTC-005.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
           MOVE WS-TARIFF-NUM (SUB-4) TO ALPHA-RATE.
           MOVE 1 TO SUB-1 SUB-2.
       CTC-010.
           IF SUB-1 > 8
             GO TO CTC-020.
             
           IF AL-RATE (SUB-1) = 0
            IF SUB-2 = 1
              ADD 1 TO SUB-1
              GO TO CTC-010.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           ADD 1 TO SUB-1 SUB-2
           
           IF SUB-2 = 5 OR = 8
              MOVE "." TO DAT-RATE (SUB-2)
              ADD 1 TO SUB-2.
           GO TO CTC-010.
       CTC-020.
           IF DAT-RATE (6) = " "
              MOVE " " TO DAT-RATE (5).
           IF DAT-RATE (9) = " "
              MOVE " " TO DAT-RATE (8).
              
           MOVE DATA-RATE TO D-TARIFF.
       CTC-999.
           EXIT.
      *
       ENTER-TARIFF-NUMBER SECTION.
       ETN-005.
           MOVE 1 TO SUB-1.
       ETN-010.
           IF SUB-1 > 500
              MOVE "SUB-1 > 500, THEREFORE GOING TO EXIT." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO ETN-999.
              
           IF WS-TARIFF-NUM (SUB-1) NOT = 0
            IF WS-TARIFF-NUM (SUB-1) = ST-DUTYTARIFF
             IF ST-DUTYPERCENT NOT = WS-TARIFF-DUTY (SUB-1)
               MOVE "Y" TO WS-TARIFF-ERR (SUB-1)
               ADD 1    TO WS-TARIFF-QTY (SUB-1)
               GO TO ETN-999
             ELSE
               ADD 1 TO WS-TARIFF-QTY (SUB-1)
               GO TO ETN-999.
               
           IF WS-TARIFF-NUM (SUB-1) NOT = 0
            IF WS-TARIFF-NUM (SUB-1) NOT = ST-DUTYTARIFF
               ADD 1 TO SUB-1
               GO TO ETN-010.
           IF WS-TARIFF-NUM (SUB-1) = 0
              MOVE ST-DUTYTARIFF  TO WS-TARIFF-NUM (SUB-1)
              MOVE ST-DUTYPERCENT TO WS-TARIFF-DUTY (SUB-1)
              MOVE 1              TO WS-TARIFF-QTY (SUB-1)
              MOVE "N"            TO WS-TARIFF-ERR (SUB-1).
       ETN-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-0000.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
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
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 2.
           
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE STOCK-MASTER.
           CLOSE PRINT-FILE.
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
