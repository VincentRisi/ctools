        IDENTIFICATION DIVISION.
        PROGRAM-ID. StQyChRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B26.
        OBJECT-COMPUTER. B26.
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
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-CATVALUE          PIC 9(8)V99 VALUE 0.
       77  WS-TOTVALUE          PIC 9(8)V99 VALUE 0.
       77  WS-QTYONHAND         PIC S9(6).
       77  WS-CALC-QTY          PIC S9(6).
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-PRINTED           PIC X VALUE " ".
       77  WS-FORM-FEED         PIC X VALUE " ".
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(15) VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
       77  WS-ANSWER4           PIC X VALUE " ".
       77  WS-ANSWER5           PIC X VALUE " ".
       77  WS-QTY               PIC S9(6)V99 VALUE 0.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(48) VALUE
           "S T O C K   Q U A N T I T Y   C H E C K I N G".
           03  FILLER         PIC X(32) VALUE "R E P O R T".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(15) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(59) VALUE ALL "*".
           03  FILLER         PIC X(44) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(55) VALUE
           "STOCK NUMBER      DESCRIPTION".
           03  FILLER         PIC X(55) VALUE
           "    ON-HAND  CALC-QTY (+)S/TAKE  (+)RECVD   (-)SOLD".
           03  FILLER         PIC X(22) VALUE "(+)ADJ".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(18) VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(22) VALUE " ".
           03  D-ONHAND       PIC Z(5)9.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-CALC         PIC Z(5)9-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-S-TAKE       PIC Z(5)9.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-REC          PIC Z(5)9-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-SOLD         PIC Z(5)9-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-ADJ          PIC Z(5)9-.
           03  FILLER         PIC X(3) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(45).
           03  T-CAT          PIC X(5).
           03  T-NAME         PIC X(15).
           03  T-VALUE        PIC Z(7)9.99-.
           03  FILLER         PIC X(56).
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
           DISPLAY "** STOCK QUANTITY CHECKING VALUES REPORT **" AT POS
           MOVE 410 TO POS
           DISPLAY "*******************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1210 TO POS.
           DISPLAY "FROM STOCK NUMBER : [               ]" AT POS.
           MOVE 1231 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 30        TO CDA-COL.
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
           MOVE 1310 TO POS.
           DISPLAY "  TO STOCK NUMBER : [               ]" AT POS.
           MOVE 1331 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 30        TO CDA-COL.
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
               GO TO CONTROL-013
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
       CONTROL-013.
           MOVE 1510 TO POS.
           DISPLAY "FORM FEED THE PAGE: [ ]" AT POS.
           MOVE 1531 TO POS.

           MOVE 'Y'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FORM-FEED.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-012.
           IF WS-FORM-FEED NOT = "Y" AND NOT = "N"
               MOVE "ANSWER MUST BE Y OR N, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-013.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-013.
       CONTROL-015.
           MOVE 2510 TO POS
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
               AT POS
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
           IF WS-STOCK-ST1 NOT = 0
              MOVE 0 TO WS-STOCK-ST1
              GO TO PRR-999.
       PRR-005.
           READ STOCK-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               GO TO PRR-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE
           "NEXT STOCK FILE BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRR-005.
           IF ST-STOCKNUMBER < WS-ANSWER1
              GO TO PRR-005.
           IF ST-STOCKNUMBER > WS-ANSWER2
              GO TO PRR-999.
           IF WS-STORE = "   "
              MOVE ST-CATEGORY TO WS-STORE.
           MOVE 2310 TO POS
           DISPLAY "Stock Number Being Read:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

           MOVE 0 TO WS-CALC-QTY WS-QTYONHAND.
           COMPUTE WS-CALC-QTY = (ST-QTYADJYTD + ST-QTYRECYTD
                 + ST-QTY-ST-TAKE) - ST-SALESUNITSYTD.
           COMPUTE WS-QTYONHAND = ST-QTYONHAND + ST-QTYONRESERVE.
           IF WS-QTYONHAND NOT = WS-CALC-QTY
                 GO TO PRR-010.
           GO TO PRR-005.
       PRR-010.
           IF LINE-CNT < 60
               GO TO PRR-020.
           ADD 1 TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE
           MOVE " " TO PRINT-REC.
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
           WRITE PRINT-REC
           MOVE 6 TO LINE-CNT.
       PRR-020.
           IF ST-CATEGORY NOT = WS-STORE
               MOVE " " TO PRINT-REC
               MOVE ST-CATEGORY   TO WS-STORE
               WRITE PRINT-REC
               ADD 1 TO LINE-CNT.
           MOVE ST-STOCKNUMBER    TO D-STOCK
           MOVE ST-DESCRIPTION1   TO D-DESC1
           MOVE ST-DESCRIPTION2   TO D-DESC2
           MOVE WS-QTYONHAND      TO D-ONHAND
           MOVE WS-CALC-QTY       TO D-CALC
           MOVE ST-QTY-ST-TAKE    TO D-S-TAKE
           MOVE ST-QTYRECYTD      TO D-REC
           MOVE ST-SALESUNITSYTD  TO D-SOLD
           MOVE ST-QTYADJYTD      TO D-ADJ
           WRITE PRINT-REC FROM DETAIL-LINE
           ADD 1 TO LINE-CNT
           MOVE " " TO PRINT-REC DETAIL-LINE.
           MOVE "Y" TO WS-PRINTED.
       PRR-025.
           COMPUTE WS-TOTVALUE = WS-TOTVALUE +
               ((WS-CALC-QTY - WS-QTYONHAND) * ST-AVERAGECOST).
           GO TO PRR-005.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-010.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-035.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-035.
       OPEN-050.
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
           IF WS-PRINTED NOT = "Y"
              MOVE "**** NOTHING TO PRINT IN THAT RANGE" TO PRINT-REC
              WRITE PRINT-REC
              GO TO END-450.
           MOVE WS-TOTVALUE       TO T-VALUE
           MOVE "TOTAL EXCESS: R" TO T-NAME
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " "               TO PRINT-REC.
           WRITE PRINT-REC.
       END-450.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE.
           IF WS-FORM-FEED = "Y"
               PERFORM PRINT-REPORT-INFO
           ELSE
               PERFORM PRINT-REPORT-INFO-NFF.
       END-500.
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
       Copy "PrintReportInfoNoFormFeed".
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
      * END-OF-JOB.
