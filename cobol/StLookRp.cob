        IDENTIFICATION DIVISION.
        PROGRAM-ID. StLookRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStockLookup".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStockLookup.
           COPY ChlfdStock.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-ST-LOOK           PIC X VALUE " ".
       77  WS-ONLY-INVALID      PIC X VALUE " ".
       77  WS-DELETE-INVALID    PIC X VALUE " ".
       01  WS-LOOK-STATUS.
           03  WS-LOOK-ST1      PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-DELETE-MESSAGE.
           03  WS-FILLER            PIC X(20) VALUE " ".
           03  WS-DELETE-LOOKREC    PIC X(15) VALUE " ".
       01  HEAD1.
           03  FILLER         PIC X(6) VALUE "DATE:".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(25) VALUE "STOCK LOOKUP LIST".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
       01  HEAD2.
           03  FILLER         PIC X(21) VALUE " ".
           03  FILLER         PIC X(17) VALUE ALL "*".
       01  HEAD3.
           03  FILLER         PIC X(17) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(17) VALUE "SUPPLIER NUMBER".
           03  FILLER         PIC X(10) VALUE "INVALID".
       01  DETAIL-LINE1.
           03  D-STOCK        PIC X(17).
           03  D-SUPPLIER     PIC X(20).
           03  D-INVALID      PIC X(5).
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 321 TO POS
           DISPLAY "** STOCK LOOKUP LIST **" AT POS
           MOVE 421 TO POS
           DISPLAY "***********************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           IF WS-ST-LOOK = "L"
               PERFORM PRINT-ROUTINE
           ELSE
               PERFORM PRINT-STOCK-ROUTINE.
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
           MOVE 7        TO CDA-ROW.
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
               GO TO GET-020
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-020.
            MOVE 1410 TO POS.
            DISPLAY "PRINT BY: S=STOCK, L-LOOKUP  : [ ]" AT POS.
            MOVE 1442 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ST-LOOK.

      *      ACCEPT WS-ST-LOOK AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO GET-000.
            IF WS-ST-LOOK NOT = "S" AND NOT = "L"
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-030.
            MOVE 1610 TO POS.
            DISPLAY "PRINT ONLY INVALID ITEMS     : [ ]" AT POS.
            MOVE 1642 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13       TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ONLY-INVALID.

      *      ACCEPT WS-ONLY-INVALID AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO GET-020.
            IF WS-ONLY-INVALID NOT = "N" AND NOT = "Y"
               GO TO GET-030.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-040
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-040.
            IF WS-ONLY-INVALID = "N"
               GO TO GET-050.
            IF WS-ST-LOOK = "S"
               MOVE "N" TO WS-DELETE-INVALID
               GO TO GET-050.
            MOVE 1810 TO POS.
            DISPLAY "DELETE INVALID ITEMS         : [ ]" AT POS.
            MOVE 1842 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15       TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DELETE-INVALID.

      *      ACCEPT WS-DELETE-INVALID AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO GET-030.
            IF WS-DELETE-INVALID NOT = "N" AND NOT = "Y"
               GO TO GET-040.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-050
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
       GET-050.
            MOVE 2510 TO POS.
            DISPLAY "Report Is Being Compiled, Please Be Patient."
            AT POS.
       GET-060.
            MOVE 3010 TO POS.
            DISPLAY "                                        " AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE WS-RANGE1 TO STLOOK-STOCKNUMBER.
            START STLOOK-MASTER KEY NOT < STLOOK-KEY
                INVALID KEY NEXT SENTENCE.
       PRR-002.
            READ STLOOK-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-LOOK-ST1 = 10
               MOVE 0 TO WS-LOOK-ST1
               GO TO PRR-999.
            IF WS-LOOK-ST1 NOT = 0
             MOVE "LOOKUP BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-LOOK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-LOOK-ST1
               GO TO PRR-002.
               
            IF STLOOK-STOCKNUMBER < WS-RANGE1
               GO TO PRR-002.
            IF STLOOK-STOCKNUMBER > WS-RANGE2
               GO TO PRR-999.
               
           MOVE 2010 TO POS
           DISPLAY "STOCK NUMBER :" AT POS
           ADD 15 TO POS 
           DISPLAY STLOOK-KEY AT POS.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-020.
            ADD 1         TO PAGE-CNT.
            MOVE PAGE-CNT TO H1-PAGE.
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
            MOVE 5 TO LINE-CNT.
       PRR-020.
           PERFORM READ-STOCK.
           IF WS-ONLY-INVALID = "Y"
            IF D-INVALID = "     "
               GO TO PRR-002.
           MOVE STLOOK-STOCKNUMBER     TO D-STOCK
           MOVE STLOOK-SUPPLIERNUMBER  TO D-SUPPLIER

           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE1
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       PRINT-STOCK-ROUTINE SECTION.
       PRS-000.
            MOVE WS-RANGE1 TO ST-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY NEXT SENTENCE.
       PRS-002.
            READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-STOCK-ST1 = 10
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRS-999.
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
               GO TO PRS-002.
               
            IF ST-STOCKNUMBER < WS-RANGE1
               GO TO PRS-002.
            IF ST-STOCKNUMBER > WS-RANGE2
               GO TO PRS-999.
               
           MOVE 2010 TO POS
           DISPLAY "STOCK NUMBER :" AT POS
           ADD 15 TO POS 
           DISPLAY ST-KEY AT POS.
       PRS-010.
            IF LINE-CNT < 60
               GO TO PRS-020.
            ADD 1         TO PAGE-CNT.
            MOVE PAGE-CNT TO H1-PAGE.
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
            MOVE 5 TO LINE-CNT.
       PRS-020.
           PERFORM READ-LOOKUP.
           IF WS-ONLY-INVALID = "Y"
            IF D-INVALID = "     "
               GO TO PRS-002.
           MOVE ST-STOCKNUMBER         TO D-STOCK
           MOVE STLOOK-SUPPLIERNUMBER  TO D-SUPPLIER

           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE1
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRS-002.
       PRS-999.
           EXIT.
      *
       READ-LOOKUP SECTION.
       RLU-000.
            MOVE ST-KEY TO STLOOK-STOCKNUMBER.
            START STLOOK-MASTER KEY NOT < STLOOK-KEY
                INVALID KEY NEXT SENTENCE.
       RLU-002.
            READ STLOOK-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-LOOK-ST1 = 23 OR 35 OR 49
               MOVE " "   TO WS-LOOK-ST1
               MOVE " "   TO STLOOK-SUPPLIERNUMBER
               MOVE "YES" TO D-INVALID
               GO TO RLU-999.
            MOVE "     " TO D-INVALID.
            GO TO RLU-999.
       RLU-999.
           EXIT.
      *
       READ-STOCK SECTION.
       RST-000.
            MOVE STLOOK-KEY TO ST-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY NEXT SENTENCE.
       RST-002.
            READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-STOCK-ST1
               MOVE "YES" TO D-INVALID
               GO TO RST-900.
            MOVE "     " TO D-INVALID.
            MOVE " "     TO ST-STOCKNUMBER
            GO TO RST-999.
       RST-900.
           IF WS-DELETE-INVALID = "Y"
            IF WS-ONLY-INVALID = "Y"
               GO TO RST-950.

            MOVE " " TO STLOOK-SUPPLIERNUMBER
            GO TO RST-999.
       RST-950.
            DELETE STLOOK-MASTER
              INVALID KEY NEXT SENTENCE.
            IF WS-LOOK-ST1 NOT = 0
               MOVE STLOOK-STOCKNUMBER     TO WS-DELETE-LOOKREC
               MOVE "RECORD NOT DELETED, " TO WS-FILLER
               MOVE WS-DELETE-MESSAGE      TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-LOOK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020.
       RST-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O STLOOK-MASTER.
           IF WS-LOOK-ST1 NOT = 0 
              MOVE 0 TO WS-LOOK-ST1
              MOVE "STOCKLOOKUP FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-005.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0 
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.
       OPEN-020.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
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
           IF WS-ST-LOOK = "S"
              MOVE "*** REPORT RUN IN STOCK-MASTER SEQUENCE ***" 
              TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              MOVE "** INVALID MEANS NO LOOKUP TABLE FOR THIS ITEM **"
              TO PRINT-REC
              WRITE PRINT-REC AFTER 1
           ELSE
              MOVE "*** REPORT RUN IN LOOKUP SEQUENCE ***" 
              TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              MOVE "** INVALID MEANS NO STOCK NUMBER FOR THIS ITEM **"
              TO PRINT-REC
              WRITE PRINT-REC AFTER 1.
           IF WS-DELETE-INVALID = "Y"
              MOVE "*** INVALID LOOKUP TABLE ITEMS DELETED ***" 
              TO PRINT-REC
              WRITE PRINT-REC AFTER 1
           ELSE
              MOVE "*** INVALID LOOKUP TABLE ITEMS NOT DELETED ***" 
              TO PRINT-REC
              WRITE PRINT-REC AFTER 1.
       
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           WRITE PRINT-REC.
           
           CLOSE STLOOK-MASTER
                 STOCK-MASTER.
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
