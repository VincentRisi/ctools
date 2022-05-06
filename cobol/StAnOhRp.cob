        IDENTIFICATION DIVISION.
        PROGRAM-ID. StAnOhRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
         Copy "SelectStChanges".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStockChanges.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(250).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-CAT               PIC X(3) VALUE " ".
       77  WS-QTY               PIC S9(6) VALUE 0.
       77  WS-ONLY-ONHAND       PIC X VALUE " ".
       77  WS-ANALYSIS          PIC X VALUE " ".
       77  WS-DELETE-ITEMS      PIC X VALUE " ".
       77  WS-STOCK-CHANGE      PIC X VALUE " ".
       77  NEW-STOCKNO          PIC X VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-STCHANGE-STATUS.
           03  WS-STCHANGE-ST1  PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(57) VALUE
           "S T O C K   A N A L Y S I S   O N   H A N D   R E P O R T".
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(15) VALUE "ANALYSIS TYPE: ".
           03  H1-TYPE        PIC X VALUE " ". 
           03  FILLER         PIC X(14) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(57) VALUE ALL "*".
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(16) VALUE ALL "*".
           03  FILLER         PIC X(24) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(17) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(40) VALUE "DESCRIPTION".
           03  FILLER         PIC X(8) VALUE "DELETED".
           03  FILLER         PIC X(67) VALUE
           "MAX  MIN    HAND   RES ORDER   B/O   LAST SOLD   CREATED".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(17).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(22).
           03  D-DELETE       PIC X(2).
           03  D-MAX          PIC Z(5)9.
           03  D-MIN          PIC Z(5)9.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-HAND         PIC Z(5)9.
           03  D-RES          PIC Z(5)9.
           03  D-ORDER        PIC Z(5)9.
           03  D-BO           PIC Z(5)9.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-DATE         PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-CREATE-DATE  PIC X(10).
           03  FILLER         PIC X(8) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 321 TO POS.
           DISPLAY "** STOCK ANALYSIS ON HAND REPORT **" AT POS.
           MOVE 421 TO POS.
           DISPLAY "***********************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA
           PERFORM OPEN-FILES
           PERFORM PRINT-ROUTINE
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
           MOVE 1605 TO POS.
           DISPLAY
           "T= ToolKit, P= Production, B= Both: N= Non Stock, " &
           "D= Item To Be Deleted" AT POS.
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

      *     ACCEPT WS-ANALYSIS AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO GET-010.
           IF WS-ANALYSIS = " "
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-025
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-020.
           MOVE WS-ANALYSIS TO H1-TYPE.
       GET-025.
           MOVE 1810 TO POS.
           DISPLAY "PRINT ONLY IF B/O > ON-HAND  : [ ]" AT POS.
           MOVE 1842 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ONLY-ONHAND.

      *     ACCEPT WS-ONLY-ONHAND AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO GET-020.
           IF WS-ONLY-ONHAND NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-025.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-027
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-025.
       GET-027.
           IF WS-ANALYSIS NOT = "D"
              MOVE "N" TO WS-DELETE-ITEMS
              GO TO GET-040.
           MOVE 2010 TO POS.
           DISPLAY "DELETE ITEMS IF ON-HAND & ON-RES = 0: [ ]" AT POS.
           MOVE 2049 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 48        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DELETE-ITEMS.

      *     ACCEPT WS-DELETE-ITEMS AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO GET-025.
           IF WS-DELETE-ITEMS NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-027.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-030
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-027.
       GET-030.
           IF WS-DELETE-ITEMS NOT = "Y"
              MOVE "N" TO WS-STOCK-CHANGE
              GO TO GET-040.
           MOVE 2210 TO POS.
           DISPLAY "WRITE STOCK CHANGES RECORD, Y OR N  : [ ]" AT POS.
           MOVE 2249 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 19        TO CDA-ROW.
           MOVE 48        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-STOCK-CHANGE.

      *     ACCEPT WS-STOCK-CHANGE AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO GET-027.
           IF WS-STOCK-CHANGE NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-030.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-040
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-030.
       GET-040.
           MOVE 2810 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
               AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE WS-RANGE1 TO ST-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-STOCKNUMBER.
       PRR-002.
           READ STOCK-MASTER NEXT
               AT END
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

           MOVE 2510 TO POS
           DISPLAY "STOCKNUMBER BEING READ:" AT POS
           ADD 24 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

           IF WS-ANALYSIS = "T" OR = "P"
            IF ST-ANALYSIS = "B"
               GO TO PRR-006.
           IF ST-ANALYSIS NOT = WS-ANALYSIS
               GO TO PRR-002.
           IF WS-ONLY-ONHAND = "Y"
               COMPUTE WS-QTY = ST-QTYONHAND + ST-QTYONRESERVE
                     + ST-QTYONORDER - ST-QTYONBORDER
            IF WS-QTY NOT < ST-MINIMUMLEVEL
               GO TO PRR-002.
       PRR-006.
           IF WS-CAT = "   "
               MOVE ST-CATEGORY TO WS-CAT.
           IF LINE-CNT < 60
               GO TO PRR-020.
       PRR-010.
            ADD 1            TO PAGE-CNT.
            MOVE PAGE-CNT    TO H1-PAGE.
            MOVE WS-ANALYSIS TO H1-TYPE.
            IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC.
            MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD2.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD3.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE 6 TO LINE-CNT.
       PRR-020.
           IF ST-CATEGORY NOT = WS-CAT
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC
              MOVE ST-CATEGORY TO WS-CAT
              ADD 1 TO LINE-CNT.
           MOVE ST-STOCKNUMBER     TO D-STOCK
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE ST-MAXIMUMLEVEL    TO D-MAX
           MOVE ST-MINIMUMLEVEL    TO D-MIN
           MOVE ST-QTYONHAND       TO D-HAND
           MOVE ST-QTYONRESERVE    TO D-RES
           MOVE ST-QTYONORDER      TO D-ORDER
           MOVE ST-QTYONBORDER     TO D-BO
           MOVE ST-LASTSALEDATE    TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE       TO D-DATE.
           MOVE ST-DATE-CREATED    TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE       TO D-CREATE-DATE.
           
           IF WS-ANALYSIS = "D"
            IF WS-DELETE-ITEMS = "Y"
                PERFORM DELETE-ITEM.
           
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC.
           ADD 1 TO LINE-CNT.


           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       DELETE-ITEM SECTION.
       DI-000.
           IF ST-QTYONHAND = 0
            AND ST-QTYONRESERVE = 0
             AND ST-QTYONORDER = 0 
              AND ST-QTYONBORDER = 0 
                MOVE "Y" TO D-DELETE
                GO TO DI-005
            ELSE
                MOVE "Q" TO D-DELETE
                GO TO DI-999.
       DI-005.
           IF ST-SALESUNITSYTD = 0
            AND ST-SALESUNITSLAST = 0
             AND ST-QTYRECYTD = 0
              AND ST-QTYRECLAST = 0
               AND ST-QTYADJYTD = 0
                AND ST-QTYADJLAST = 0
                 AND ST-SALESCOSTYTD = 0
                  AND ST-SALESCOSTLAST = 0
                   MOVE "Y" TO D-DELETE
                   GO TO DI-010
            ELSE
                   MOVE "S" TO D-DELETE
                   GO TO DI-999.
           GO TO DI-999.
       DI-010.
           DELETE STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
           MOVE "PROBLEM IN DELETING STOCK ITEM, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO DI-010. 
               
           IF WS-STOCK-CHANGE NOT = "Y"
               GO TO DI-999.

           MOVE "N"   TO NEW-STOCKNO.
           MOVE X"1F" TO F-EXIT-CH.
               
           PERFORM WRITE-STOCK-CHANGES.
       DI-999.
           EXIT.
      *
        OPEN-FILES SECTION.
        OPEN-000.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0 
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-STOCK-ST1
              GO TO OPEN-000.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-005.
            OPEN I-O STOCKCHANGE-MASTER.
            IF WS-STCHANGE-ST1 NOT = 0
               MOVE "STCHANGE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STCHANGE-ST1
               GO TO OPEN-005.
        OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
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
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
           IF WS-ONLY-ONHAND = "Y"
               MOVE "** ONLY ITEMS WHERE ONHAND < MIN-LEVEL PRINTED **"
               TO PRINT-REC
           ELSE
               MOVE "** ALL ITEMS PRINTED **" TO PRINT-REC.
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC.
           
           IF WS-DELETE-ITEMS = "Y"
               MOVE 
          "**ITEMS DELETED WHERE NO MOVEMENT YTD AND LYR ARE PRINTED**"
               TO PRINT-REC
           ELSE
               MOVE "** NO ITEMS DELETED THIS RUN **" TO PRINT-REC.
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC.
           
          MOVE 
          "* DELETED HEADING: Q=ITEM HAS A QTY, S=SALES UNITS " &
          "YTD/LYR, Y=ITEM DELETED *" TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC.
           
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE STOCK-MASTER
                 STOCKCHANGE-MASTER
                 PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
      *     STOP RUN.
           EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "WriteStockChanges".
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
