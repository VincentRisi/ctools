        IDENTIFICATION DIVISION.
        PROGRAM-ID. StBranch.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT STNWPR-MASTER ASSIGN TO WS-ORIGINAL
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STNWPR-KEY
               FILE STATUS IS WS-STNWPR-STATUS.
           SELECT STNWPR1-MASTER ASSIGN TO WS-CHECK
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STNWPR1-KEY
               FILE STATUS IS WS-STNWPR-STATUS.
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
              ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStNewPrice.
           COPY ChlfdStNewPrice-Branch.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-EOF               PIC X(3) VALUE "   ".
       77  WS-ORIGINAL          PIC X(30) VALUE " ".
       77  WS-CHECK             PIC X(30) VALUE " ".
       01  WS-STNWPR-STATUS.
           03  WS-STAT1         PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(35) VALUE
            "STOCK BRANCH CHECKING REPORT".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
       01  HEAD2.
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(28) VALUE ALL "*".
           03  FILLER         PIC X(7) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(20) VALUE "STOCK TESTED IN   :".
           03  H3-BRANCH      PIC X(50) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(20) VALUE "CHECKED AGAINST   :".
           03  H4-ORIGINAL    PIC X(50) VALUE " ".
       01  HEAD5.
           03  FILLER         PIC X(27) VALUE "STOCK ITEM       CODE".
           03  FILLER         PIC X(8) VALUE "PRICE".
           03  FILLER         PIC X(15) VALUE "ERROR REASON".
       01  DETAIL-LINE.
           03  D-STOCKNO      PIC X(16).
           03  FILLER         PIC X(2) VALUE " ".
           03  D-ANALYSIS     PIC X.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-PRICE        PIC Z(7)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-WRONG        PIC X(20).
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
        PROCEDURE DIVISION USING WS-LINKAGE.
        CONTROL-PARAGRAPH SECTION.
           PERFORM A-INIT.
           PERFORM OPEN-FILES.
           PERFORM B-MAIN.
           PERFORM C-END.
        CONTROL-000.
           EXIT. 
      *    
       A-INIT SECTION.
       A-000.
           PERFORM CLEAR-SCREEN
           MOVE 0315 TO POS
           DISPLAY "** BRANCH STOCK ITEM CHECKING PROGRAM **" AT POS
           MOVE 0415 TO POS
           DISPLAY "****************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       A-010.
           MOVE 2010 TO POS
           DISPLAY 
           "MAKE SURE YOU HAVE RUN A NEW LOCAL PRICELIST TO DISK FIRST"
               AT POS.
           MOVE 2111 TO POS
           DISPLAY 
           "FOR BOTH HEAD OFFICE AND THE BRANCH UNDER TEST."
               AT POS.
       
           MOVE 0810 TO POS
           DISPLAY 
              "Enter the FULL path for the PriceList file:" AT POS
           MOVE 0931 TO POS
           DISPLAY "e.g. /ctools/data01/StNewPrices" AT POS.
                      
           MOVE 1010 TO POS
           DISPLAY "DATA TO USE AS ORIGINAL :" AT POS
           ADD 26 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 30        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 35        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ORIGINAL.

           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-003.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO A-020
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO A-010.
       A-020.
           MOVE 1210 TO POS
           DISPLAY "DATA TO BE CHECKED      :" AT POS
           ADD 26 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 30        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 35        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CHECK.

           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-003.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO A-EXIT
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO A-020.
       A-EXIT.
           EXIT.
      *    
       B-MAIN SECTION.
       B-000.
           MOVE " " TO STNWPR-KEY.
           START STNWPR-MASTER KEY NOT < STNWPR-KEY
              INVALID KEY
              MOVE 88 TO STNWPR-KEY
              MOVE "BAD START ON ORIGINAL-FILE, 'ESC' TO EXIT"
                  TO WS-MESSAGE
                  PERFORM ERROR-MESSAGE
                  EXIT PROGRAM.
       B-005.
           READ STNWPR-MASTER NEXT WITH LOCK
               AT END GO TO B-EXIT.
           MOVE 2520 TO POS
           DISPLAY "STOCK ITEM BEING CHECKED:" AT POS
           ADD 25 TO POS
           DISPLAY STNWPR-STOCKNUMBER AT POS.
       B-006.
           MOVE STNWPR-STOCKNUMBER TO STNWPR1-STOCKNUMBER.
           READ STNWPR1-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STAT1 = 23 OR 35 OR 49
              MOVE 2565 TO POS
              DISPLAY "NOT ON FILE" AT POS
              MOVE "ITEM MISSING" TO D-WRONG
              PERFORM PRINT-ROUTINE
              GO TO B-005.
           IF STNWPR-PRICE NOT = STNWPR1-PRICE
              MOVE 2565 TO POS
              DISPLAY "WRONG PRICE" AT POS
              MOVE "PRICE DIFFERENCE" TO D-WRONG
              PERFORM PRINT-ROUTINE
              GO TO B-005.
       B-010.
      *    DELETE STNWPR-MASTER
      *          INVALID KEY
      *          DISPLAY "INVALID DELETE"
      *          STOP RUN.

           MOVE 2565 TO POS
           DISPLAY "                                            " AT POS
           GO TO B-005.
       B-EXIT.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-010.
           IF LINE-CNT < 60
              GO TO PRR-020.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE
            IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            MOVE " "      TO PRINT-REC.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE WS-CHECK    TO H3-BRANCH
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE WS-ORIGINAL TO H4-ORIGINAL
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD5 AFTER 2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE 6 TO LINE-CNT.
       PRR-020.
           MOVE STNWPR-STOCKNUMBER TO D-STOCKNO
           MOVE STNWPR-PRICE       TO D-PRICE
           MOVE STNWPR-ANALYSIS    TO D-ANALYSIS
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.

           ADD 1 TO LINE-CNT.
       PRR-999.
           EXIT.
      *    
       C-END SECTION.
       C-000.
           CLOSE STNWPR-MASTER
                 STNWPR1-MASTER.
           IF LINE-CNT = 66
              PERFORM PRR-010
           MOVE "*** NOTHING TO PRINT IN THAT RANGE ***" TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
       
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
           
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           EXIT PROGRAM.
       C-EXIT.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O STNWPR-MASTER.
           IF WS-STAT1 NOT = 0
              MOVE "ORIGINAL DATA-FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STAT1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STAT1
              GO TO OPEN-000.
       OPEN-005.
           OPEN I-O STNWPR1-MASTER.
           IF WS-STAT1 NOT = 0
              MOVE "CHECKING DATA-FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STAT1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STAT1
              GO TO OPEN-005.
       OPEN-015.
           MOVE Ws-Co-Name TO CO-NAME.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
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
      * END-OF-JOB.
