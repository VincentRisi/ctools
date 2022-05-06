        IDENTIFICATION DIVISION.
        PROGRAM-ID. StTariRp.
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
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-RANGE3            PIC X VALUE " ".
       77  WS-PERMIT            PIC X VALUE " ".
       77  WS-ANALYSIS          PIC X VALUE " ".
       77  WS-TARIFF            PIC X(8) VALUE " ".
       77  WS-TARIFF-NUM        PIC 9(8) VALUE 0.
       77  WS-CAT               PIC XXX VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(18) VALUE " ".
           03  FILLER         PIC X(74) VALUE
           "S T O C K   R E P O R T,    D U T I E S   &   T A R I F F".
           03  FILLER         PIC X(9) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(6) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(35) VALUE " ".
           03  FILLER         PIC X(57) VALUE ALL "*".
           03  FILLER         PIC X(40) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(10) VALUE "CATEGORY: ".
           03  H3-CAT         PIC X(3).
           03  FILLER         PIC X(119) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(17) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(41) VALUE "DESCRIPTION".
           03  FILLER         PIC X(10) VALUE "SUPPLIER".
           03  FILLER         PIC X(8) VALUE "DUTY %".
           03  FILLER         PIC X(12) VALUE " TARIFF".
           03  FILLER         PIC X(10) VALUE "PERMIT".
           03  FILLER         PIC X(12) VALUE "SURCH. %".
           03  FILLER         PIC X(26) VALUE "ANALYSIS".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(17).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(22).
           03  D-SUPPLIER     PIC X(10).
           03  D-PERC         PIC Z(1)9.9.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-TARIFF       PIC X(15).
           03  D-PERMIT       PIC X.
           03  FILLER         PIC X(8) VALUE " ".
           03  D-SURCH        PIC Z(1)9.99.
           03  FILLER         PIC X(8) VALUE " ".
           03  D-ANAL         PIC X(24) VALUE " ".
       01  FINAL-LINE.
           03  FILLER         PIC X(16) VALUE "ANALYSIS FIELD:".
           03  F-ANAL         PIC X.
       01  TARIFF-LINE.
           03  FILLER         PIC X(16) VALUE "TARIFF NUMBER:".
           03  F-TARIFF       PIC X(10).
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
           DISPLAY "** STOCK REPORT ON DUTIES & TARIFF **" AT POS
           MOVE 415 TO POS
           DISPLAY "*************************************" AT POS.
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
                DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-015
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
       GET-015.
            MOVE 1510 TO POS.
            DISPLAY "Leave BLANK to print 'ALL' stock items" AT POS.
            MOVE 1610 TO POS.
            DISPLAY "Enter 'P' to print only items needing a PERMIT"
                 AT POS.
       GET-020.
            MOVE 1410 TO POS.
            DISPLAY 
            "PRINT ONLY ITEMS NEEDING A PERMIT             :[ ]" AT POS.
            MOVE 1458 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PERMIT.

      *      ACCEPT WS-PERMIT AT POS.
            IF W-ESCAPE-KEY = 4
                GO TO GET-010.
            IF WS-PERMIT NOT = " " AND NOT = "P"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
                GO TO GET-040
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
       GET-040.
           MOVE 1810 TO POS.
           DISPLAY 
           "PRINT NEW CATEGORIES ON A NEW PAGE, Y OR N    :[ ]" AT POS.
           MOVE 1858 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE3.

      *     ACCEPT WS-RANGE3 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-020.
           IF WS-RANGE3 NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
              GO TO GET-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
       GET-050.
           MOVE 2010 TO POS.
           DISPLAY
           "PRINT A SPECIFIC ANALYSIS FIELD, BLANK FOR ALL:[ ]" AT POS.
           MOVE 2058 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANALYSIS.

      *     ACCEPT WS-ANALYSIS AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-055
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
       GET-055.
           MOVE 2210 TO POS.
           DISPLAY
           "PRINT A SPECIFIC TARIFF NUMBER, BLANK FOR ALL :[        ]"
            AT POS.
           MOVE 2258 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 8         TO CDA-DATALEN.
           MOVE 19        TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TARIFF.

      *     ACCEPT WS-TARIFF AT POS.
           IF WS-TARIFF NOT = " "
              MOVE WS-TARIFF    TO ALPHA-RATE
              PERFORM DECIMALISE-RATE
              MOVE NUMERIC-RATE TO WS-TARIFF-NUM
              DISPLAY WS-TARIFF-NUM AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-050.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-060
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-055.
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
               GO TO PRR-002.
            IF ST-STOCKNUMBER < WS-RANGE1
               GO TO PRR-002.
            IF ST-STOCKNUMBER > WS-RANGE2
               GO TO PRR-999.
               
            IF WS-TARIFF-NUM > 0
             IF ST-DUTYTARIFF NOT = WS-TARIFF-NUM
               GO TO PRR-002.
       PRR-003.
            MOVE 2310 TO POS
            DISPLAY "STOCKNUMBER BEING READ:" AT POS
            ADD 24 TO POS
            DISPLAY ST-STOCKNUMBER AT POS.
               
            IF WS-CAT = "   "
                MOVE ST-CATEGORY TO WS-CAT.
            IF WS-CAT NOT = ST-CATEGORY
               MOVE ST-CATEGORY TO WS-CAT
             IF WS-RANGE3 = "Y"
               PERFORM PRR-010 THRU PRR-015
             ELSE
               WRITE PRINT-REC
               PERFORM PRR-015.
               
            IF WS-PERMIT = "P"
             IF ST-PERMIT NOT = "Y"
               GO TO PRR-002.
               
            IF WS-ANALYSIS NOT = " "
             IF ST-ANALYSIS NOT = WS-ANALYSIS
               GO TO PRR-002.
       PRR-008.
            IF LINE-CNT < 60
               GO TO PRR-020.
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
            MOVE 3 TO LINE-CNT.
       PRR-015.
            MOVE WS-CAT TO H3-CAT
            WRITE PRINT-REC FROM HEAD4 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            ADD 2 TO LINE-CNT.
       PRR-020.
           MOVE ST-STOCKNUMBER     TO D-STOCK
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE ST-SUPPLIER        TO D-SUPPLIER
           MOVE ST-DUTYPERCENT     TO D-PERC.
           
           PERFORM CHECK-TARIFF-CODE.
           
      *     MOVE ST-DUTYTARIFF      TO D-TARIFF
           MOVE ST-PERMIT          TO D-PERMIT
           MOVE ST-SURCHARGE       TO D-SURCH
           MOVE ST-ANALYSIS        TO D-ANAL.

           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       CHECK-TARIFF-CODE SECTION.
       CTC-005.
           IF ST-DUTYTARIFF = 0
              MOVE " " TO D-TARIFF
              GO TO CTC-999.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
           MOVE ST-DUTYTARIFF TO ALPHA-RATE.
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
       OPEN-FILES SECTION.
       OPEN-0000.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       OPEN-000.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
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
           IF WS-PERMIT NOT = " "
              MOVE "** ONLY ITEMS REQUIRING A PERMIT HAVE PRINTED **"
               TO PRINT-REC
              WRITE PRINT-REC AFTER 2.
           IF WS-ANALYSIS = " "
              MOVE "** ALL ITEMS IN STOCK RANGE PRINTED **" TO PRINT-REC
              WRITE PRINT-REC AFTER 2
           ELSE
              MOVE "** ONLY STOCK PRINTED FOR A SPECIFIC ANALYSIS **"
               TO PRINT-REC
              WRITE PRINT-REC AFTER 2
              MOVE WS-ANALYSIS TO F-ANAL
              WRITE PRINT-REC FROM FINAL-LINE AFTER 1.

           IF WS-TARIFF NOT = " "
              MOVE 
            "** ONLY ITEMS WITH A SPECIFIC TARIFF CODE HAVE PRINTED **"
               TO PRINT-REC
              WRITE PRINT-REC AFTER 2
              MOVE D-TARIFF TO F-TARIFF
              WRITE PRINT-REC FROM TARIFF-LINE AFTER 1.
              
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

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
