        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrLaSoRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrMasterOld".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdDebtorOld.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC 9(8) VALUE 0.
       77  WS-ANSWER            PIC X(10) VALUE " ".
       77  WS-DATE-TYPE         PIC X VALUE " ".
       77  WS-TYPE              PIC X VALUE " ".
       77  WS-DELETE            PIC X VALUE " ".
       77  WS-REPNO             PIC X VALUE " ".
       77  WS-ACCEPT-DATE       PIC X(10) VALUE " ".
       77  W-DATE-DISPLAY       PIC X(10) VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-VALIDDATE         PIC 9(8) VALUE 0.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1        PIC 99.
       01  WS-DEBTOROLD-STATUS.
           03  WS-DEBTOROLD-ST1   PIC 99.
       01  WS-LAST-DATE         PIC 9(8).
       01  SALESMAN-LINE.
           03  WS-SMAN-COMMENT      PIC X(28) VALUE " ".
           03  WS-SMAN-NUM          PIC XX.
           03  WS-SMAN-COMMENT2     PIC X(25) VALUE " ".
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(10) VALUE " ".
           03  H1-TYPE        PIC X(28) VALUE
           "DEBTORS NOT PURCHASED SINCE".
           03  H1-DR-DATE     PIC X(10).
           03  FILLER         PIC X(8) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(58) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(27) VALUE " ".
           03  FILLER         PIC X(38) VALUE ALL "*".
           03  FILLER         PIC X(76) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(8) VALUE "ACCOUNT".
           03  FILLER         PIC X(46) VALUE "NAME".
           03  FILLER         PIC X(8) VALUE "STATUS".
           03  FILLER         PIC X(22) VALUE "SALE DATE  OPEN DATE".
           03  FILLER         PIC X(67) VALUE "TELEPHONE".
       01  DETAIL-LINE.
           03  D-NUMBER       PIC 9(7).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-NAME         PIC X(46) VALUE " ".
           03  D-STATUS       PIC X(8) VALUE " ".
           03  D-SALE-DATE    PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-OPEN-DATE    PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PHONE        PIC X(13).
           03  FILLER         PIC X(51) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 312 TO POS
           DISPLAY "** DEBTORS NOT PURCHASED SINCE XX/XX/XXXX REPORT **"
               AT POS
           MOVE 412 TO POS
           DISPLAY "***************************************************"
               AT POS.
       CONTROL-003.
           Copy "PrinterAcceptDr".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-0000.
            MOVE 0   TO WS-RANGE1
            MOVE " " TO WS-DATE-TYPE
            MOVE "N" TO WS-DELETE.
       GET-000.
            MOVE 1010 TO POS.
            DISPLAY
              "ENTER A DATE BEFORE WHICH ACCOUNTS WILL BE PRINTED,"
               AT POS.
            MOVE 1110 TO POS.
            DISPLAY 
            "                      ENTER DATE AS DDMMYYYY :[          ]"
                AT POS.
            MOVE 1157 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 8         TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT-DATE.

      *     ACCEPT WS-ACCEPT-DATE AT POS.
           IF W-ESCAPE-KEY = 4
                GO TO CONTROL-005.
           MOVE WS-ACCEPT-DATE TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
               GO TO GET-000.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE DISPLAY-DATE    TO H1-DR-DATE
           DISPLAY DISPLAY-DATE AT POS.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-VALIDDATE WS-LAST-DATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO GET-000.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-010
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
                
      *      ACCEPT WS-ANSWER AT POS.
      *      MOVE WS-ANSWER       TO SPLIT-DATE CONVERT-DATE
      *      MOVE WS-CONVERT-DATE TO DISPLAY-DATE
      *      MOVE DISPLAY-DATE    TO H1-DR-DATE
      *      DISPLAY DISPLAY-DATE AT POS
      *      PERFORM CONVERT-SPLIT-FORMAT.
      *      MOVE SPLIT-DATE TO WS-LAST-DATE.
            IF WS-LAST-DATE NOT > 0
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
      *      IF W-ESCAPE-KEY = 4
      *          GO TO CONTROL-005.
      *      IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
      *          GO TO GET-010
      *      ELSE
      *          DISPLAY " " AT 3079 WITH BELL
      *          GO TO GET-000.
       GET-010.
            MOVE 1210 TO POS.
            DISPLAY
            "TYPE: N=NOT purchased since, P=Purchased since.:[ ]"
             AT POS.
            MOVE 1259 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 58        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TYPE.

      *      ACCEPT WS-TYPE AT POS.
            IF W-ESCAPE-KEY = 4
                GO TO GET-000.
            IF WS-TYPE NOT = "N" AND NOT = "P"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-020
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
       GET-020.
            MOVE 1410 TO POS.
            DISPLAY
            "                       PRINT DATES THAT = 0.   :[ ]"
             AT POS.
            MOVE 1511 TO POS
            DISPLAY "A=ALL - ZERO AS WELL, N=NO, Y=ZERO ONLY" AT POS
            MOVE 1459 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 58        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATE-TYPE.

      *      ACCEPT WS-DATE-TYPE AT POS.
            IF W-ESCAPE-KEY = 4
                GO TO GET-010.
            IF WS-DATE-TYPE NOT = "Y" AND NOT = "N" AND NOT = "A"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-025
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
       GET-025.
            MOVE 1710 TO POS.
            DISPLAY 
            "DO YOU WISH TO DELETE ACCOUNTS, Y OR N.        :[ ]"
             AT POS
            MOVE 1759 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 58        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DELETE.

      *      ACCEPT WS-DELETE AT POS.
            IF W-ESCAPE-KEY = 4
                GO TO GET-020.
            IF WS-DELETE NOT = "Y" AND NOT = "N"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-025.
            IF WS-TYPE = "P"
             IF WS-DELETE = "Y"
             MOVE "YOU CANNOT SELECT 'Y' IF TYPE = 'P'." TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO GET-025.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-030
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-025.
       GET-030.
            MOVE 1910 TO POS.
            DISPLAY 
            "ENTER A REP #, ENTER 0 FOR ACC'S WITH          :[ ]" AT POS
            MOVE 2010 TO POS
            DISPLAY "  NO REP OR BLANK FOR ALL ACC'S" AT POS
            MOVE 1959 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 58        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-REPNO.

      *      ACCEPT WS-REPNO AT POS.
            IF W-ESCAPE-KEY = 4
                GO TO GET-025.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-035
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-030.
       GET-035.
            MOVE 2510 TO POS
            DISPLAY "The Report Is Being Compiled......" AT POS
            PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE 0 TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PRR-999.
       PRR-002.
           READ DEBTOR-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
               GO TO PRR-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PRR-002.
               
           MOVE 2310 TO POS
           DISPLAY "Account Number Being Processed:" AT POS
           ADD 35 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
           
           IF WS-TYPE = "N"
            IF WS-DATE-TYPE = "N"
             IF DR-DATE-LAST-SALE = 0
               GO TO PRR-002.
           IF WS-TYPE = "N"
            IF WS-DATE-TYPE = "N"
             IF DR-DATE-LAST-SALE NOT = 0
              IF DR-DATE-LAST-SALE NOT > WS-LAST-DATE
               IF DR-BALANCE = 0
                IF DR-BAL-LAST-STATE = 0
                GO TO PRR-005.
           IF WS-TYPE = "N"
            IF WS-DATE-TYPE = "Y"
             IF DR-DATE-LAST-SALE = 0
              IF DR-BALANCE = 0
                IF DR-BAL-LAST-STATE = 0
               GO TO PRR-005.
           IF WS-TYPE = "N"
            IF WS-DATE-TYPE = "A"
             IF DR-DATE-LAST-SALE NOT > WS-LAST-DATE
              IF DR-BALANCE = 0
              AND DR-BAL-LAST-STATE = 0
               GO TO PRR-005
              ELSE
               GO TO PRR-002.
           IF WS-TYPE = "P"
            IF DR-DATE-LAST-SALE NOT < WS-LAST-DATE
               GO TO PRR-005.
       PRR-004.
           IF WS-TYPE = "N"
            IF WS-DATE-TYPE = "Y"
             IF DR-DATE-LAST-SALE = 0
              IF DR-BALANCE = 0
               GO TO PRR-005.
           IF WS-TYPE = "N"
            IF WS-DATE-TYPE = "Y"
              IF DR-DATE-LAST-SALE = 0
            IF DR-BAL-LAST-STATE = 0
               GO TO PRR-005.
               
           GO TO PRR-002.
       PRR-005.
           IF WS-REPNO = "0"
            IF DR-SALESMAN = " "
                GO TO PRR-010.
           IF WS-REPNO = " "
                GO TO PRR-010.
           IF WS-REPNO NOT = DR-SALESMAN
                GO TO PRR-002.
       PRR-010.
           IF LINE-CNT < 60
               GO TO PRR-020.
           MOVE " "      TO PRINT-REC
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
            IF WS-TYPE = "N"
               MOVE "DEBTORS NOT PURCHASED SINCE" TO H1-TYPE
            ELSE
               MOVE "DEBTORS WHO PURCHASED SINCE" TO H1-TYPE.

            WRITE PRINT-REC FROM HEAD1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE 5 TO LINE-CNT.
       PRR-020.
           MOVE DR-ACCOUNT-NUMBER TO D-NUMBER
           MOVE DR-NAME           TO D-NAME.
           IF DR-SUPPLY-Y-N = "N"
              MOVE "ON-HOLD"      TO D-STATUS.
           IF DR-SUPPLY-Y-N = "S"
              MOVE "SUSPEND"      TO D-STATUS.
           IF DR-SUPPLY-Y-N = "Y"
              MOVE "SUPPLY"       TO D-STATUS.
           MOVE DR-DATE-LAST-SALE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-SALE-DATE
           MOVE DR-DATE-CREATED   TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-OPEN-DATE
           MOVE DR-TELEPHONE      TO D-PHONE
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           ADD 1    TO LINE-CNT.
           IF WS-DELETE = "Y"
              PERFORM DELETE-ACCOUNT.
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       DELETE-ACCOUNT SECTION.
       DA-001.
            IF DR-BALANCE NOT = 0
                 GO TO DA-999.
            IF DR-BAL-LAST-STATE NOT = 0
                 GO TO DA-999.
       
            PERFORM WRITE-DEBTOR-OLD-RECORD.
       DA-005.
           DELETE DEBTOR-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR RECORD BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO DA-005.
       DA-999.
           EXIT.
      *
       WRITE-DEBTOR-OLD-RECORD SECTION.
       WOR-010.
            MOVE DEBTOR-RECORD TO DEBTOROLD-RECORD.
            GO TO WOR-020.
       WOR-015.
            REWRITE DEBTOROLD-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOROLD-ST1 NOT = 0
                MOVE 
             "OLD DEBTOR RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOROLD-ST1
                GO TO WOR-020.
            GO TO WOR-999.
       WOR-020.
            WRITE DEBTOROLD-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOROLD-ST1 NOT = 0
                MOVE "OLD DEBTOR REC BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOROLD-ST1
                GO TO WOR-015.
       WOR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
          OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-002.
            OPEN I-O DEBTOROLD-MASTER.
            IF WS-DEBTOROLD-ST1 = 35 OR 49 
              OPEN OUTPUT DEBTOROLD-MASTER.
              
            IF WS-DEBTOROLD-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOROLD-ST1
               MOVE "DEBTOROLD FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
       OPEN-009.
          PERFORM GET-USER-PRINT-NAME.
          OPEN OUTPUT PRINT-FILE.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-100.
            IF LINE-CNT > 59
               PERFORM PRR-010.
            IF WS-TYPE = "N"
             IF WS-REPNO = " "
               MOVE "*** ALL ACCOUNTS BEFORE DATE SOLD PRINTED ***"
                 TO PRINT-REC
                 GO TO END-200.
            IF WS-TYPE = "P"
             IF WS-REPNO = " "
               MOVE "*** ALL ACCOUNTS AFTER DATE SOLD PRINTED ***"
                 TO PRINT-REC
                 GO TO END-200.
             IF WS-REPNO = "0"
               MOVE "*** ACCOUNTS WITH NO SALESMAN PRINTED ***"
                 TO PRINT-REC
                 GO TO END-200.
            MOVE "*** ACCOUNTS FOR SALESMAN #" TO WS-SMAN-COMMENT
            MOVE "ONLY PRINTED ***" TO WS-SMAN-COMMENT2
            MOVE WS-REPNO TO WS-SMAN-NUM
            MOVE SALESMAN-LINE TO PRINT-REC.
           WRITE PRINT-REC AFTER 2.
       END-200.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-800.
            CLOSE DEBTOR-MASTER
                  DEBTOROLD-MASTER
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
