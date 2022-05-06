        IDENTIFICATION DIVISION.
        PROGRAM-ID. StReImRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStImports".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdImpReceipts.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-LINE-NO           PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(7) VALUE " ".
       77  WS-RANGE2            PIC X(10) VALUE " ".
       77  WS-STORE             PIC X(17) VALUE " ".
       01  WS-IMPRECEIPT-STATUS.
           03  WS-IMPORT-ST1    PIC 99.
       01  WS-DATE-RANGE        PIC 9(8).
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(10) VALUE " ".
           03  FILLER         PIC X(55) VALUE
           "IMPORT  COSTING  ANALYSIS  REPORT".
           03  FILLER         PIC X(5) VALUE " ".
           03  H1-COMMENT     PIC X(36).
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(25) VALUE " ".
           03  FILLER         PIC X(33) VALUE ALL "*".
           03  FILLER         PIC X(28) VALUE " ".
           03  H2-COMMENT.
               04  H2-COMM1   PIC X(21).
               04  H2-COMM2   PIC X(15).
           03  FILLER         PIC X(11) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(10) VALUE "SUPPLIER".
           03  FILLER         PIC X(15) VALUE "INVOICE #".
           03  FILLER         PIC X(15) VALUE "DATE COSTED".
           03  FILLER         PIC X(21) VALUE "START TRANS #".
           03  FILLER         PIC X(8) VALUE "LINES".
           03  FILLER         PIC X(69) VALUE "RECOSTED".
       01  DETAIL-LINE.
           03  D-SUPPLIER     PIC X(10).
           03  D-INV-NO       PIC X(16).
           03  D-DATE         PIC X(18).
           03  D-TRANS        PIC Z(5)9.
           03  FILLER         PIC X(9) VALUE " ".
           03  D-LINES        PIC Z(5)9.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-RECOSTED     PIC X(64).
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 325 TO POS
           DISPLAY "** IMPORT COSTING ANALYSIS REPORT **" AT POS
           MOVE 425 TO POS
           DISPLAY "************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           IF WS-IMPORT-ST1 NOT = 0 
              GO TO CONTROL-020.
       CONTROL-020.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE " " TO WS-RANGE1 WS-RANGE2.
           MOVE 1010 TO POS
           DISPLAY "Enter SUPPLIER, Leave Blank for ALL  : [       ]"
                      AT POS
           MOVE 1050 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
           ELSE
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
           MOVE 1210 TO POS.
           DISPLAY "Enter START DATE, Leave Blank For ALL: [          ]"
                AT POS
           MOVE 1336 TO POS
           DISPLAY "Date Format:  DDMMYYYY" AT POS
           MOVE 1250 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           IF W-ESCAPE-KEY = 4
               GO TO GET-000.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-020.
           IF WS-RANGE2 = " "
              MOVE 0 TO WS-DATE-RANGE
              GO TO GET-550.
           MOVE WS-RANGE2       TO SPLIT-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE
           MOVE DISPLAY-DATE    TO H2-COMM2.
           MOVE 1250 TO POS.
           DISPLAY DISPLAY-DATE AT POS.
           MOVE CONVERT-DATE    TO WS-DATE-RANGE SPLIT-DATE.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-DATE-RANGE.
       GET-550.
            MOVE 2710 TO POS.
            DISPLAY "The report is being compiled........." AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            IF WS-RANGE1 NOT = " "
                GO TO PRR-001.
            MOVE 0 TO IMRE-TRANSACTION-NUMBER.
            START IMPRECEIPTS-FILE KEY NOT < IMRE-KEY
                INVALID KEY NEXT SENTENCE.
            IF WS-IMPORT-ST1 NOT = 0
               MOVE "NO TRANS WITHIN RANGE ENTERED, 'ESC' TO EXIT."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-IMPORT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-IMPORT-ST1
                GO TO PRR-999.
            IF WS-RANGE1 = " "
               GO TO PRR-002.
       PRR-001.
            MOVE WS-RANGE1 TO IMRE-SUPPLIER
            MOVE " "       TO IMRE-INVOICENUM.
            START IMPRECEIPTS-FILE KEY NOT < IMRE-ALT-KEY
                INVALID KEY NEXT SENTENCE.
            IF WS-IMPORT-ST1 NOT = 0
               MOVE "NO SUPPLIER WITHIN RANGE, 'ESC' TO EXIT."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-IMPORT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-IMPORT-ST1
                GO TO PRR-999.
       PRR-002.
            READ IMPRECEIPTS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-IMPORT-ST1 = 10
               PERFORM PRR-025
               MOVE 0 TO WS-IMPORT-ST1
               GO TO PRR-999.
            IF WS-IMPORT-ST1 NOT = 0
               MOVE "IMPORTS FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-IMPORT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-IMPORT-ST1
               GO TO PRR-002.

            IF WS-RANGE1 NOT = " "
             IF IMRE-SUPPLIER < WS-RANGE1
               GO TO PRR-002.
            IF WS-RANGE1 NOT = " "
             IF IMRE-SUPPLIER > WS-RANGE1
               GO TO PRR-999.
            IF WS-DATE-RANGE NOT = 0
             IF IMRE-DATERECEIVED < WS-DATE-RANGE
               GO TO PRR-002.

            IF WS-STORE = " "
               MOVE IMRE-ALT-KEY TO WS-STORE
               MOVE 2820 TO POS
               DISPLAY "IMPORTS BEING READ:" AT POS
               ADD 20 TO POS
               DISPLAY IMRE-SUPPLIER AT POS
               ADD 10 TO POS
               DISPLAY IMRE-INVOICENUM AT POS
               MOVE 1 TO WS-LINE-NO
               PERFORM PRR-015
               PERFORM PRR-020
               GO TO PRR-002.

            IF IMRE-ALT-KEY NOT = WS-STORE
               PERFORM PRR-025
               MOVE IMRE-ALT-KEY TO WS-STORE
               MOVE 2820 TO POS
               DISPLAY "IMPORTS BEING READ:" AT POS
               ADD 20 TO POS
               DISPLAY IMRE-SUPPLIER AT POS
               ADD 10 TO POS
               DISPLAY IMRE-INVOICENUM AT POS
               PERFORM PRR-020
               MOVE 1 TO WS-LINE-NO
               GO TO PRR-002.
       PRR-010.
            ADD 1 TO WS-LINE-NO.
            IF LINE-CNT > 60
               PERFORM PRR-015.
            GO TO PRR-002.
       PRR-015.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE
            MOVE " "      TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC.
            MOVE " " TO PRINT-REC.
            IF WS-RANGE1 = " "
                MOVE "*ALL SUPPLIERS PRINTED*"          TO H1-COMMENT
            ELSE
                MOVE "*ONLY SELECTED SUPPLIER PRINTED*" TO H1-COMMENT.
            IF WS-RANGE2 = " "
                MOVE "*ALL DATES PRINTED*"              TO H2-COMMENT
            ELSE
                MOVE "*USED START DATE OF:"             TO H2-COMM1.
            WRITE PRINT-REC FROM HEAD1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE 4 TO LINE-CNT.
       PRR-020.
           MOVE IMRE-SUPPLIER           TO D-SUPPLIER
           MOVE IMRE-INVOICENUM         TO D-INV-NO
           MOVE IMRE-DATERECEIVED       TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-DATE
           MOVE IMRE-TRANSACTION-NUMBER TO D-TRANS
           MOVE IMRE-UPDATED-YN         TO D-RECOSTED.
       PRR-025.
           MOVE WS-LINE-NO              TO D-LINES
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           OPEN I-O IMPRECEIPTS-FILE.
           IF WS-IMPORT-ST1 NOT = 0
              MOVE "IMPORTS FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-IMPORT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-IMPORT-ST1
              GO TO OPEN-005.
       OPEN-010.
           MOVE Ws-Co-Name TO CO-NAME.
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

            CLOSE IMPRECEIPTS-FILE.
            CLOSE PRINT-FILE.
            PERFORM SEND-REPORT-TO-PRINTER.
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
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
