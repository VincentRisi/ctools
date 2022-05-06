        IDENTIFICATION DIVISION.
        PROGRAM-ID. StCataRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStCatalogue".
          Copy "SelectStMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStCatalogue.
           COPY ChlfdStock.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(250).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-DELETE            PIC X VALUE " ".
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       01  WS-STCAT-STATUS.
           03  WS-STCAT-ST1     PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(18) VALUE " ".
           03  FILLER         PIC X(60) VALUE
           "S T O C K   C A T A L O G U E   I N D E X   R E P O R T".
           03  FILLER         PIC X(31) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
       01  HEAD2.
           03  FILLER         PIC X(33) VALUE " ".
           03  FILLER         PIC X(55) VALUE ALL "*".
           03  FILLER         PIC X(78) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(27) VALUE "STOCK NUMBER & PAGE #".
           03  FILLER         PIC X(27) VALUE "STOCK NUMBER & PAGE #".
           03  FILLER         PIC X(27) VALUE "STOCK NUMBER & PAGE #".
           03  FILLER         PIC X(27) VALUE "STOCK NUMBER & PAGE #".
           03  FILLER         PIC X(27) VALUE "STOCK NUMBER & PAGE #".
       01  DETAIL-LINE.
           03  D-STOCK1       PIC X(16).
           03  D-PAGE1        PIC X(5).
           03  FILLER         PIC X(6).
           03  D-STOCK2       PIC X(16).
           03  D-PAGE2        PIC X(5).
           03  FILLER         PIC X(6).
           03  D-STOCK3       PIC X(16).
           03  D-PAGE3        PIC X(5).
           03  FILLER         PIC X(6).
           03  D-STOCK4       PIC X(16).
           03  D-PAGE4        PIC X(5).
           03  FILLER         PIC X(6).
           03  D-STOCK5       PIC X(16).
           03  D-PAGE5        PIC X(5).
           03  FILLER         PIC X(6).
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 321 TO POS
           DISPLAY "** STOCK CATALOGUE INDEX REPORT **" AT POS
           MOVE 421 TO POS
           DISPLAY "**********************************" AT POS.
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
            MOVE 0910 TO POS
            DISPLAY
            "NB! THIS REPORT SHOULD GO ONLY TO A WIDE PAPER PRINTER"
               AT POS.
       
            MOVE " " TO WS-RANGE1 WS-RANGE2.
            MOVE 1010 TO POS.
            DISPLAY "            FROM Stock NUMBER: [               ]"
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
            DISPLAY "              TO Stock NUMBER: [               ]"
                      AT POS.
            MOVE 1242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9        TO CDA-ROW.
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
           MOVE 2510 TO POS
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
           AT POS.
       GET-999.
           EXIT.
      *
       DELETE-CATALOGUE-RECORD SECTION.
       DSR-000.
            IF WS-DELETE NOT = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE STCAT-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-STCAT-ST1 NOT = 0
              MOVE "ST-CATALOGUE BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE WS-RANGE1 TO STCAT-STOCKNUMBER.
            START STCAT-MASTER KEY NOT < STCAT-KEY
                 INVALID KEY NEXT SENTENCE.
            IF WS-STCAT-ST1 NOT = 0
               GO TO PRR-999.
            MOVE " " TO DETAIL-LINE.
       PRR-002.
            READ STCAT-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-STCAT-ST1 = 10
               MOVE 0 TO WS-STCAT-ST1
               PERFORM PRR-500
               GO TO PRR-999.
            IF WS-STCAT-ST1 NOT = 0
               MOVE "CATALOGUE FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               GO TO PRR-002.
            IF STCAT-STOCKNUMBER < WS-RANGE1
               GO TO PRR-002.
            IF STCAT-STOCKNUMBER > WS-RANGE2
               PERFORM PRR-500
               GO TO PRR-999.
               
            MOVE 2310 TO POS
            DISPLAY "STOCKNUMBER BEING READ:" AT POS
            ADD 25 TO POS
            DISPLAY STCAT-STOCKNUMBER AT POS.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-020.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE
            MOVE " " TO PRINT-REC.
            MOVE WS-PRINT-COMP TO PRINT-REC
            WRITE PRINT-REC.
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
           IF WS-DELETE = "Y"
              PERFORM DELETE-CATALOGUE-RECORD
              GO TO PRR-002.
           IF D-STOCK1 = " "
              MOVE STCAT-STOCKNUMBER  TO D-STOCK1
              MOVE STCAT-PAGE-NUM     TO D-PAGE1
              GO TO PRR-002.
           IF D-STOCK2 = " "
              MOVE STCAT-STOCKNUMBER  TO D-STOCK2
              MOVE STCAT-PAGE-NUM     TO D-PAGE2
              GO TO PRR-002.
           IF D-STOCK3 = " "
              MOVE STCAT-STOCKNUMBER  TO D-STOCK3
              MOVE STCAT-PAGE-NUM     TO D-PAGE3
              GO TO PRR-002.
           IF D-STOCK4 = " "
              MOVE STCAT-STOCKNUMBER  TO D-STOCK4
              MOVE STCAT-PAGE-NUM     TO D-PAGE4
              GO TO PRR-002.
           IF D-STOCK5 = " "
              MOVE STCAT-STOCKNUMBER  TO D-STOCK5
              MOVE STCAT-PAGE-NUM     TO D-PAGE5.
       PRR-500.
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           ADD 1 TO LINE-CNT.
       PRR-600.
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       READ-STOCK SECTION.
       RS-000.
            MOVE STCAT-STOCKNUMBER TO ST-STOCKNUMBER
            START STOCK-MASTER KEY NOT < ST-KEY.
            MOVE " " TO WS-DELETE.
       RS-010.
            READ STOCK-MASTER
                INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE "Y" TO WS-DELETE
                GO TO RS-999.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RS-010.
       RS-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O STCAT-MASTER.
           IF WS-STCAT-ST1 NOT = 0 
              MOVE 0 TO WS-STCAT-ST1
              MOVE "STCATALOGUE FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-005.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK-MASTER FILE BUSY ON OPEN 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-010.
           MOVE Ws-Co-Name TO CO-NAME
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
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
           
           MOVE WS-PRINT-NORMAL TO PRINT-REC
           WRITE PRINT-REC.
           CLOSE STCAT-MASTER
                 STOCK-MASTER
                 PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
      *      STOP RUN.
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
