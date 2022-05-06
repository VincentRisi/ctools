        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlPuByRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectCoPullBy".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdPullBy.
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  Ws-PullByInd         PIC X(60) VALUE " ".
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       77  WS-LINE-ANSWER       PIC X VALUE " ".
       77  WS-LINECNT           PIC 9(4) VALUE 0.
       77  WS-ORDERCNT          PIC S9(4) VALUE 0.
       77  WS-ORDERTOT          PIC S9(6) VALUE 0.
       77  WS-LINETOT           PIC S9(6) VALUE 0.
       77  PRINT-SB             PIC XX VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-RANGE             PIC X VALUE " ".
       77  WS-ANSWER1           PIC X VALUE " ".
       77  WS-ANSWER2           PIC X VALUE " ".
       77  WS-SALE              PIC S9(7)V99 VALUE 0.
       77  WS-SALE-AMT          PIC S9(7)V99 VALUE 0.
       77  WS-SALE-TOT          PIC S9(7)V99 VALUE 0.
       77  WS-AVE               PIC S9(7)V99 VALUE 0.
       01  WS-PULLBY-STATUS.
           03  WS-PULLBY-ST1    PIC 99.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE "  DATE".
           03  H1-DATE          PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
           03  FILLER           PIC X(52) VALUE 
           "S T O C K   P U L L E D   B Y   R E P O R T".
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(53) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(19) VALUE " ".
           03  FILLER           PIC X(113) VALUE 
           "*******************************************".
       01  HEAD3.
           03  FILLER           PIC X(9) VALUE " ".
           03  FILLER           PIC X(8) VALUE "PULLBY: ".
           03  H3-PULLBY        PIC XX.
           03  FILLER           PIC X(113) VALUE " ".
       01  HEAD4.
           03  FILLER           PIC X(9) VALUE " ". 
           03  FILLER           PIC X(123) VALUE "**********".
       01  HEAD5.
         02  HEAD5-BLANK.
           03  FILLER           PIC X(9) VALUE  " ".
           03  H5-1             PIC X(9) VALUE "Invoice".
           03  FILLER           PIC X(10) VALUE " ".
           03  H5-2             PIC X(12) VALUE "PullBy".
           03  FILLER           PIC X(18) VALUE "Line #".
           03  H5-3             PIC X(13) VALUE  "Inv Date".
           03  FILLER           PIC X(14) VALUE "Sale Amt".
           03  FILLER           PIC X(47) VALUE " ".
       01  DETAIL-LINE.
           03  FILLER           PIC X(9) VALUE " ".
           03  P-INVOICE        PIC Z(6).
           03  FILLER           PIC X(15) VALUE " ".
           03  P-PULLBY         PIC X(2).
           03  FILLER           PIC X(6) VALUE " ".
           03  P-LINECNT        PIC Z(4)9.
           03  FILLER           PIC X(14) VALUE " ".
           03  P-DATE           PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
           03  P-SALE           PIC Z(6)9.99.
           03  FILLER           PIC X(49) VALUE " ".
       01  SUB-TOTAL-LINE.
           03  FILLER           PIC X(16) VALUE "No. Of Orders:".
           03  P-ORD-CNT        PIC Z(3)9-.
           03  FILLER           PIC X(3) VALUE " ".
           03  FILLER           PIC X(14) VALUE "No. Of Lines:".
           03  P-SUB-CNT        PIC Z(4)9.
           03  FILLER           PIC X(3) VALUE " ".
           03  FILLER           PIC X(4) VALUE "Ave:".
           03  P-AVE            PIC Z(2)9.
           03  FILLER           PIC X(6) VALUE " ".
           03  FILLER           PIC X(10) VALUE "Totals :R".
           03  P-SUB-SALE       PIC Z(6)9.99.
           03  FILLER           PIC X(6) VALUE " ".
       01  TOT-TOTAL-LINE.
           03  FILLER           PIC X(13) VALUE "Total Orders:".
           03  P-TOT-ORD        PIC Z(5)9-.
           03  FILLER           PIC X(5) VALUE " ".
           03  FILLER           PIC X(12) VALUE "Total Lines:".
           03  P-TOT-CNT        PIC Z(5)9.
           03  FILLER           PIC X(3) VALUE " ".
           03  FILLER           PIC X(4) VALUE "Ave:".
           03  P-AVE-TOT        PIC Z(2)9.
           03  FILLER           PIC X(6) VALUE " ".
           03  FILLER           PIC X(10) VALUE " Total :R".
           03  P-TOT-SALE       PIC Z(6)9.99.
           03  FILLER           PIC X(3) VALUE " ".
       Copy "WsDateInfo".
      *
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 320 TO POS
           DISPLAY "***** Stores Pull By Invoice Report *****" AT POS
           MOVE 420 TO POS
           DISPLAY "*****************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-030.
           MOVE 1010 TO POS.    
           DISPLAY "Enter The Initials To Be Printed" AT POS.
           MOVE 1054 TO POS.
           DISPLAY "[  ]" AT POS.
           MOVE 1210 TO POS.
           DISPLAY "Leave Blank To Print All Initials" AT POS.
           MOVE 1055 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO PRINT-SB.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF PRINT-SB = " " 
              MOVE "Y" TO WS-RANGE.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-030.
       CONTROL-040.
           MOVE 1510 TO POS.
           DISPLAY "Print Only Totals Of Each Storeman ?" AT POS.
           MOVE 1554 TO POS.
           DISPLAY "[ ]" AT POS.
           MOVE 1555 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-030.
           IF WS-ANSWER1 NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-045
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
       CONTROL-045.
           MOVE 1710 TO POS.
           DISPLAY "Do you wish to DELETE the file after Printing: [ ]"
           AT POS.
           MOVE 1758 TO POS.
           
           MOVE 'N'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-040.
           IF WS-ANSWER2 NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-045.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-045.
       CONTROL-050.
           MOVE 2510 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
           AT POS.
       CONTROL-070.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           IF PRINT-SB NOT = " "
               MOVE PRINT-SB TO PB-INITIAL
               MOVE 0        TO PB-INVOICE
           ELSE
               MOVE " "      TO PB-INITIAL
               MOVE 0        TO PB-INVOICE.
           START PULL-BY KEY NOT < PB-KEY
             INVALID KEY NEXT SENTENCE.
           IF WS-PULLBY-ST1 NOT = 0
               MOVE 88 TO WS-PULLBY-ST1
               PERFORM PRINT-HEADINGS
               MOVE "**** NOTHING TO PRINT IN THAT RANGE ****"
                TO PRINT-REC
               WRITE PRINT-REC AFTER 2
               MOVE "PULL-BY FILE BAD START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO PR-999.
       PR-001.
           READ PULL-BY NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-PULLBY-ST1 = 10 OR = 23
               PERFORM SUB-LINE
               PERFORM PRINT-TOTALS
               GO TO PR-999.
           IF WS-PULLBY-ST1 NOT = 0
               MOVE "PULL BY FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-PULLBY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-PULLBY-ST1
               GO TO PR-001.
       PR-005.
           IF PRINT-SB = " "
               GO TO PR-010.
           IF WS-RANGE NOT = "Y"
            IF PB-INITIAL NOT = PRINT-SB 
               PERFORM SUB-LINE
               GO TO PR-999.
       PR-010.
           IF WS-LINE > 60
               MOVE "N" TO WS-LINE-ANSWER
               PERFORM PRINT-HEADINGS.
           IF PB-INITIAL NOT = PRINT-SB
             IF PRINT-SB NOT = " "
               PERFORM SUB-LINE.
       PR-030.
           IF WS-ANSWER1 = "Y"
               GO TO PR-033.
           MOVE PB-INVOICE  TO P-INVOICE
           MOVE PB-INV-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO P-DATE
           MOVE PB-INITIAL         TO P-PULLBY
           MOVE PB-LINE-CNT        TO P-LINECNT
           MOVE PB-SALE-AMOUNT     TO P-SALE.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           ADD 1                   TO WS-LINE.
       PR-033.
           ADD PB-SALE-AMOUNT TO WS-SALE-AMT.
       PR-035.
           ADD 1           TO WS-ORDERCNT
           ADD PB-LINE-CNT TO WS-LINECNT.
           IF PRINT-SB = " " 
               MOVE PB-INITIAL TO PRINT-SB.
           IF WS-ANSWER2 = "Y"
                PERFORM PR-050.
               
           GO TO PR-001.
       PR-050.
           DELETE PULL-BY
               INVALID KEY NEXT SENTENCE.
           IF WS-PULLBY-ST1 NOT = 0
               MOVE "PULL BY FILE BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-PULLBY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-PULLBY-ST1
               GO TO PR-050.
       PR-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           IF WS-LINE-ANSWER = "Y"
               MOVE PB-INITIAL TO H3-PULLBY.
           IF WS-PAGE = 0
               MOVE PB-INITIAL TO H3-PULLBY.
           ADD 1        TO WS-PAGE
           MOVE WS-PAGE TO H-PAGE
           MOVE " "     TO PRINT-REC.

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF WS-PAGE = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC.
           IF WS-ANSWER1 = "Y"
             MOVE " " TO H5-1 H5-2 H5-3.
           WRITE PRINT-REC FROM HEAD5 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE 7 TO WS-LINE.       
        PH-999.
           EXIT.
      *
       SUB-LINE SECTION.
       SL-000.
           MOVE WS-SALE-AMT  TO P-SUB-SALE
           MOVE WS-LINECNT   TO P-SUB-CNT
           MOVE WS-ORDERCNT  TO P-ORD-CNT
           COMPUTE WS-AVE ROUNDED = WS-LINECNT / WS-ORDERCNT
           MOVE WS-AVE       TO P-AVE
           WRITE PRINT-REC FROM SUB-TOTAL-LINE AFTER 1
           ADD 1 TO WS-LINE.
       SL-010.
           MOVE PB-INITIAL TO H3-PULLBY PRINT-SB.
           ADD WS-SALE-AMT TO WS-SALE-TOT
           ADD WS-LINECNT  TO WS-LINETOT
           ADD WS-ORDERCNT TO WS-ORDERTOT.
           MOVE 0 TO WS-SALE-AMT
                     WS-ORDERCNT
                     WS-LINECNT.
           IF WS-LINE > 56
               PERFORM PRINT-HEADINGS
               GO TO SL-999.
           IF WS-RANGE NOT = "Y"
               GO TO SL-999.
           IF WS-PULLBY-ST1 NOT = 10
              WRITE PRINT-REC FROM HEAD3 AFTER 2
              WRITE PRINT-REC FROM HEAD4 AFTER 1
              MOVE " " TO PRINT-REC
              ADD 3 TO WS-LINE.
           MOVE "Y" TO  WS-LINE-ANSWER.
       SL-999.
           EXIT.
      *
       PRINT-TOTALS SECTION.
       PT-010.
           MOVE WS-SALE-TOT TO P-TOT-SALE
           MOVE WS-LINETOT  TO P-TOT-CNT
           MOVE WS-ORDERTOT TO P-TOT-ORD
           COMPUTE WS-AVE ROUNDED = WS-LINETOT / WS-ORDERTOT
           MOVE WS-AVE      TO P-AVE-TOT.
           WRITE PRINT-REC FROM TOT-TOTAL-LINE AFTER 2
           ADD 2 TO WS-LINE.
       PT-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-060.
           MOVE 2910 TO POS
           DISPLAY "Opening files....." AT POS.
           OPEN I-O PULL-BY.
           IF WS-PULLBY-ST1 NOT = 0
               MOVE "PULL BY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-PULLBY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-PULLBY-ST1
               GO TO OPEN-060.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-070.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
           PERFORM ERROR1-020.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           IF WS-ANSWER2 = "N"
              CLOSE PULL-BY.
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
      * END-OF-JOB
