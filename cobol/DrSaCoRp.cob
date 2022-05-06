        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrSaCoRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrContact".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdDrCont.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-TOTAL             PIC X VALUE " ".
       77  WS-SALESMAN          PIC X VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-PRINTED           PIC X VALUE " ".
       77  WS-PRINT-COMPRESSED  PIC X VALUE " ".
       77  WS-COMMENT           PIC X(40) VALUE " ".
       01  WS-AREA.
           03  WS-WEEK          PIC 9.
           03  WS-DAY           PIC 9.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1        PIC 99.
       01  WS-CONTACT-STATUS.
           03  WS-DC-ST1        PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(20) VALUE " ".
           03  FILLER         PIC X(38) VALUE
           "D E B T O R   M A S T E R   L I S T".
           03  FILLER         PIC X(26) VALUE
            "C A L L   S C H E D U L E".
           03  FILLER         PIC X(19) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(7) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(35) VALUE " ".
           03  FILLER         PIC X(63) VALUE ALL "*".
           03  FILLER         PIC X(34) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(10) VALUE "ACCOUNT".
           03  FILLER         PIC X(42) VALUE "NAME & ADDRESS".
           03  FILLER         PIC X(45) VALUE "TELEPHONE & CONTACT".
           03  FILLER         PIC X(35) VALUE "COMMENTS ON THE ACCOUNT".
       01  HEAD4.
           03  FILLER         PIC X(127) VALUE ALL "-".
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD5.
           03  FILLER         PIC X(4) VALUE "REP:".
           03  H2-REP         PIC X(6).
           03  FILLER         PIC X(5) VALUE "WEEK:".
           03  H2-WEEK        PIC 9.
           03  FILLER         PIC X(6) VALUE " ".
           03  FILLER         PIC X(4) VALUE "DAY:".
           03  H2-DAY         PIC 9.
           03  FILLER         PIC X(3) VALUE " ".
           03  H2-COMMENT     PIC X(102).
       01  DETAIL-LINE.
           03  D-ACCOUNT      PIC 9(7).
           03  FILLER         PIC X(3) VALUE " ".
           03  D-NAME         PIC X(42).
           03  D-PHONE        PIC X(80).
       01  SALES-LINE.
           03  FILLER         PIC X(11) VALUE "LAST SOLD:".
           03  S-LAST-SOLD    PIC X(10) VALUE " ".
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(11) VALUE "SALES MTD:".
           03  S-MTD          PIC Z(6)9.99-.
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(11) VALUE "SALES YTD:".
           03  S-YTD          PIC Z(6)9.99-.
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(14) VALUE "SALES L/YEAR:".
           03  S-LAST         PIC Z(6)9.99-.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN
           MOVE 321 TO POS
           DISPLAY "** DEBTOR MASTER LIST CALL SCHEDULES**" AT POS
           MOVE 421 TO POS
           DISPLAY "**************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAcceptDr".
       CONTROL-010.
           PERFORM GET-DATA.
           IF WS-TOTAL = "N"
              PERFORM PRINT-ROUTINE
           ELSE
              PERFORM PRINT-SUB-ROUTINE.
       CONTROL-050.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-001.
           MOVE 910 TO POS
           DISPLAY "DO YOU WISH TO PRINT ALL CALL SCHEDULES : [ ]"
                AT POS
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 6         TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TOTAL.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
            IF WS-TOTAL NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.
           IF W-ESCAPE-KEY =  0 OR 1 OR 2 OR 5
               GO TO GET-002
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.
       GET-002.
            IF WS-TOTAL = "Y"
                MOVE 0 TO WS-WEEK WS-DAY
                GO TO GET-020.
       GET-005.
           MOVE 0 TO WS-WEEK.
           MOVE 1010 TO POS.
           DISPLAY "      ENTER THE WEEK NUMBER  : [ ]" AT POS.
           MOVE 1042 TO POS.

           MOVE WS-WEEK   TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-WEEK.

           IF W-ESCAPE-KEY = 4
               GO TO GET-001.
           IF WS-WEEK = 0
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
           ELSE
                DISPLAY " " AT 3079 WITH BELL
               GO TO GET-005.
       GET-010.
           MOVE 0 TO WS-DAY.
           MOVE 1210 TO POS.
           DISPLAY "      ENTER THE DAY NUMBER   : [ ]" AT POS.
           MOVE 1242 TO POS.

           MOVE WS-DAY    TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DAY.

           IF W-ESCAPE-KEY = 4
               GO TO GET-001.
           IF WS-DAY = 0
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-020.
           MOVE " " TO WS-COMMENT.
           MOVE 1410 TO POS.
           DISPLAY "  ENTER A HEADER COMMENT OF 40 DIGITS" AT POS.
           MOVE 1510 TO POS
           DISPLAY "LEAVE BLANK TO USE THE AREA NAME INSTEAD." AT POS
           MOVE 1610 TO POS.
           DISPLAY "[                                        ]" AT POS.
           MOVE 1611 TO POS.

           MOVE WS-COMMENT TO CDA-DATA.
           MOVE 40         TO CDA-DATALEN.
           MOVE 13         TO CDA-ROW.
           MOVE 10         TO CDA-COL.
           MOVE CDA-WHITE  TO CDA-COLOR.
           MOVE 'F'        TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMMENT.

           IF W-ESCAPE-KEY = 4
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-050
           ELSE
               MOVE 3010 TO POS
                DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-050.
           MOVE " " TO WS-SALESMAN.
           MOVE 1810 TO POS.
           DISPLAY "Enter a Salesman code.       : [ ]" AT POS.
           MOVE 1842 TO POS.

           MOVE WS-SALESMAN TO CDA-DATA.
           MOVE 1           TO CDA-DATALEN.
           MOVE 15          TO CDA-ROW.
           MOVE 41          TO CDA-COL.
           MOVE CDA-WHITE   TO CDA-COLOR.
           MOVE 'F'         TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SALESMAN.

           IF W-ESCAPE-KEY = 4
            IF WS-TOTAL = "N"
               GO TO GET-020
            ELSE
               GO TO GET-001.
           IF WS-SALESMAN = " "
                DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-060
           ELSE
                DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
       GET-060.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE WS-SALESMAN TO DC-SALESMAN.
           MOVE WS-AREA     TO DC-AREA.
           START DRCONT-MASTER KEY NOT < DC-ALT-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE "BAD START" TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
               GO TO PRR-999.
       PRR-010.
           READ DRCONT-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-DC-ST1 = 10
               GO TO PRR-999.
           IF WS-DC-ST1 NOT = 0
              MOVE "DR-CONT FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DC-ST1
              GO TO PRR-010.
            IF DC-AREA NOT = WS-AREA
               GO TO PRR-999.
       PRR-020.
           MOVE 2510 TO POS
           DISPLAY "ACCOUNT LAST PROCESSED:" AT POS
           ADD 25 TO POS
           DISPLAY DC-ACCOUNT-NUMBER AT POS.
            IF LINE-CNT < 56
               GO TO PRR-050.
       PRR-025.
           ADD 1            TO PAGE-CNT
           MOVE WS-SALESMAN TO H2-REP
           MOVE WS-WEEK     TO H2-WEEK
           MOVE WS-DAY      TO H2-DAY.
           IF WS-COMMENT = " "
              MOVE DC-AREA-NAME TO H2-COMMENT
           ELSE
              MOVE WS-COMMENT   TO H2-COMMENT.
           MOVE PAGE-CNT    TO H1-PAGE
           MOVE " " TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
              MOVE WS-PRINT-COMP TO PRINT-REC
              WRITE PRINT-REC
              MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
              WRITE PRINT-REC FROM Company-Line
           ELSE
              WRITE PRINT-REC BEFORE PAGE
              WRITE PRINT-REC FROM Company-Line.
           WRITE PRINT-REC FROM HEAD1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD5
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD3
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD4
           MOVE " " TO PRINT-REC
      *    WRITE PRINT-REC
           MOVE 7 TO LINE-CNT.
       PRR-050.
           PERFORM READ-DEBTOR.
           MOVE DR-ACCOUNT-NUMBER  TO D-ACCOUNT
           MOVE DR-NAME            TO D-NAME
           MOVE DR-TELEPHONE       TO D-PHONE
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE.

           MOVE DR-DEL-ADDRESS1    TO D-NAME
           MOVE DC-NAME            TO D-PHONE
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           MOVE DR-DEL-ADDRESS2    TO D-NAME
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           MOVE DR-DEL-ADDRESS3    TO D-NAME
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           WRITE PRINT-REC
           MOVE DR-DATE-LAST-SALE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE WS-CONVERT-DATE   TO DISPLAY-DATE
           MOVE DISPLAY-DATE  TO S-LAST-SOLD.
           MOVE DR-SALES-PTD  TO S-MTD
           MOVE DR-SALES-YTD  TO S-YTD
           MOVE DR-SALES-LAST TO S-LAST
           WRITE PRINT-REC FROM SALES-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD4
           MOVE " " TO PRINT-REC
      *     WRITE PRINT-REC
           ADD 7 TO LINE-CNT
           GO TO PRR-010.
       PRR-999.
           EXIT.
      *
       PRINT-SUB-ROUTINE SECTION.
       PRS-000.
           MOVE WS-SALESMAN TO DC-SALESMAN
           MOVE 0           TO DC-AREA.
           START DRCONT-MASTER KEY NOT < DC-ALT-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "BAD START FOR PRINT, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PRS-999.
       PRS-010.
           READ DRCONT-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-DC-ST1 = 10
               GO TO PRS-999.
           IF WS-DC-ST1 NOT = 0
              MOVE "DR-CONT FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DC-ST1
              GO TO PRS-010.
           IF DC-SALESMAN NOT = WS-SALESMAN
               GO TO PRS-999.
           IF DC-AREA NOT = WS-AREA
              MOVE DC-AREA TO WS-AREA
              PERFORM PRS-025.
       PRS-020.
           IF LINE-CNT < 56
               GO TO PRS-050.
       PRS-025.
           ADD 1             TO PAGE-CNT
           MOVE WS-SALESMAN  TO H2-REP
           MOVE WS-WEEK      TO H2-WEEK
           MOVE WS-DAY       TO H2-DAY
            IF WS-COMMENT = " "
               MOVE DC-AREA-NAME TO H2-COMMENT
            ELSE
               MOVE WS-COMMENT   TO H2-COMMENT.
           MOVE PAGE-CNT     TO H1-PAGE
           MOVE " " TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
              WRITE PRINT-REC FROM Company-Line
           ELSE
              WRITE PRINT-REC BEFORE PAGE
              WRITE PRINT-REC FROM Company-Line.
           WRITE PRINT-REC FROM HEAD1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD5
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD3
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD4
           MOVE " " TO PRINT-REC
      *     WRITE PRINT-REC
           MOVE 7 TO LINE-CNT.
       PRS-050.
           PERFORM READ-DEBTOR.
           MOVE DR-ACCOUNT-NUMBER  TO D-ACCOUNT
           MOVE DR-NAME            TO D-NAME
           MOVE DR-TELEPHONE       TO D-PHONE
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE

           MOVE DR-DEL-ADDRESS1    TO D-NAME
           MOVE DC-NAME            TO D-PHONE
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           MOVE DR-DEL-ADDRESS2    TO D-NAME
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           MOVE DR-DEL-ADDRESS3    TO D-NAME
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           WRITE PRINT-REC
           MOVE DR-DATE-LAST-SALE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE WS-CONVERT-DATE   TO DISPLAY-DATE
           MOVE DISPLAY-DATE  TO S-LAST-SOLD.
           MOVE DR-SALES-PTD  TO S-MTD
           MOVE DR-SALES-YTD  TO S-YTD
           MOVE DR-SALES-LAST TO S-LAST
           WRITE PRINT-REC FROM SALES-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD4
           MOVE " " TO PRINT-REC
      *     WRITE PRINT-REC
           ADD 7 TO LINE-CNT
           GO TO PRS-010.
       PRS-999.
           EXIT.
      *
       READ-DEBTOR SECTION.
       RD-001.
            MOVE DC-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
            START DEBTOR-MASTER KEY NOT < DR-KEY
                 INVALID KEY NEXT SENTENCE.
       RD-002.
            READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "*** INVALID ACCOUNT ***" TO DR-NAME
               GO TO RD-010.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-002.
       RD-010.
           MOVE 2510 TO POS
           DISPLAY "ACCOUNT LAST PROCESSED:" AT POS
           ADD 25 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS
           ADD 10 TO POS
           DISPLAY "AREA BEING PROCESSED:" AT POS
           ADD 25 TO POS
           DISPLAY DC-AREA AT POS.
       RD-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTORS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-002.
           OPEN I-O DRCONT-MASTER.
           IF WS-DC-ST1 NOT = 0
               MOVE 0 TO WS-DC-ST1
               MOVE "DR-CONTACT BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE
           MOVE DISPLAY-DATE TO H1-DATE
           Move Ws-Co-Name To Co-Name.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT = 66
               PERFORM PRR-025
               MOVE "**** NOTHING TO PRINT IN THAT RANGE ****"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE " " TO PRINT-REC.
               
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE DEBTOR-MASTER
                 DRCONT-MASTER.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
      *     STOP RUN.
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
