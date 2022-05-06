        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrLaPuRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrMaster".
          Copy "SelectCrMasterOld".
          SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCreditorOld.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(250).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC 9(8) VALUE 0.
       77  WS-ANSWER            PIC X(10) VALUE " ".
       77  WS-PRINT-DATES       PIC X VALUE " ".
       77  WS-DELETE-ACCS       PIC X VALUE " ".
       77  WS-BEFORE-AFTER      PIC X VALUE " ".
       77  WS-LOCAL-FOREIGN     PIC X VALUE " ".
       77  WS-BANK-PURCHASE     PIC X VALUE " ".
       77  W-DATE-DISPLAY       PIC X(10) VALUE " ".
       77  WS-PRINTANSWER       PIC X(20) VALUE " ".
       77  WS-TOTAL-YTD         PIC S9(8)V99.
       77  WS-TOTAL-LY          PIC S9(8)V99.
       77  WS-TOTAL-BALANCE     PIC S9(8)V99.
       01  WS-CURRENCY-FIELD-NAMES.
         02  WS-CURRENCY-FIELDS OCCURS 50.
           03  WS-CURRENCY      PIC X(8).
           03  WS-PURCH-YTD     PIC S9(8)V99.
           03  WS-PURCH-LY      PIC S9(8)V99.
           03  WS-BALANCE       PIC S9(8)V99.
       01  WS-CREDITOR-STATUS.
           03  WS-CR-ST1           PIC 99.
       01  WS-CREDITOROLD-STATUS.
           03  WS-CREDITOROLD-ST1  PIC 99.
       01  WS-LAST-DATE            PIC 9(8).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(10) VALUE " ".
           03  H1-DESC        PIC X(31) VALUE " ".
           03  H1-CR-DATE     PIC X(10).
           03  FILLER         PIC X(49) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(17) VALUE " ".
       01  HEAD1-1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-1-DATE      PIC X(10).
           03  FILLER         PIC X(10) VALUE " ".
           03  H1-1-DESC      PIC X(31) VALUE " ".
           03  H1-1-CR-DATE   PIC X(10).
           03  FILLER         PIC X(49) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-1-PAGE      PIC Z9.
           03  FILLER         PIC X(17) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(27) VALUE " ".
           03  FILLER         PIC X(40) VALUE ALL "*".
           03  FILLER         PIC X(76) VALUE " ".
       01  HEAD2-1.
           03  FILLER         PIC X(32) VALUE " ".
           03  FILLER         PIC X(36) VALUE ALL "*".
           03  FILLER         PIC X(76) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(8) VALUE "ACCOUNT".
           03  FILLER         PIC X(11) VALUE "CRED #".
           03  FILLER         PIC X(21) VALUE "SHORT NAME".
           03  FILLER         PIC X(22) VALUE "PURCH DATE  OPEN DATE".
           03  FILLER         PIC X(42) VALUE
           "TELEPHONE            BANK NAME".
           03  FILLER         PIC X(21) VALUE "BANK ACC    BANK CODE".
       01  HEAD3-1.
           03  FILLER         PIC X(8) VALUE "ACCOUNT".
           03  FILLER         PIC X(11) VALUE "CRED #".
           03  FILLER         PIC X(21) VALUE "SHORT NAME".
           03  FILLER         PIC X(22) VALUE "PURCH DATE  OPEN DATE".
           03  FILLER         PIC X(68) VALUE
           "TELEPHONE            EMAIL ADDRESS".
           03  FILLER         PIC X(21) VALUE "YTD       L/YR".
       01  DETAIL-LINE.
           03  D-NUMBER       PIC 9(7).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-CRED-NUM     PIC X(11) VALUE " ".
           03  D-NAME         PIC X(20) VALUE " ".
           03  FILLER         PIC X VALUE " ".
           03  D-PURCH-DATE   PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-OPEN-DATE    PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PHONE        PIC X(21).
           03  D-BANK-NAME    PIC X(21).
           03  D-BANK-ACC     PIC X(12).
           03  D-BANK-CODE    PIC X(6).
           03  FILLER         PIC X(12) VALUE " ".
       01  DETAIL2-LINE.
           03  D2-NUMBER       PIC 9(7).
           03  FILLER          PIC X(1) VALUE " ".
           03  D2-CRED2-NUM    PIC X(11) VALUE " ".
           03  D2-NAME         PIC X(20) VALUE " ".
           03  FILLER          PIC X VALUE " ".
           03  D2-PURCH-DATE   PIC X(10).
           03  FILLER          PIC X(1) VALUE " ".
           03  D2-OPEN-DATE    PIC X(10).
           03  FILLER          PIC X(1) VALUE " ".
           03  D2-PHONE        PIC X(21).
           03  D2-EMAIL        PIC X(40).
           03  D2-YTD          PIC Z(6)9.99-.
           03  D2-LYR          PIC Z(6)9.99-.
       01  SUMMARY-HEADING.
           03  FILLER         PIC X(90) VALUE
            "CURRENCY     PURCH-YTD   PURCH-LAST   BALANCE OWED".
       01  SUMMARY-LINE.
           03  S-CURRENCY     PIC X(11) VALUE " ".
           03  S-PURCH-YTD    PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  S-PURCH-LY     PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  S-BALANCE      PIC Z(7)9.99-.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 312 TO POS
           DISPLAY
            "** CREDITORS NOT PURCHASED SINCE XX/XX/XXXX REPORT **"
               AT POS
           MOVE 412 TO POS
           DISPLAY
            "*****************************************************"
               AT POS.
       CONTROL-003.
           Copy "PrinterAcceptCr".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-0000.
            MOVE " " TO WS-PRINT-DATES
            MOVE "N" TO WS-DELETE-ACCS.
       GET-000.
            MOVE 1010 TO POS.
            DISPLAY
              "ENTER A DATE BEFORE WHICH ACCOUNTS WILL BE PRINTED,"
               AT POS.
            MOVE 1110 TO POS.
            DISPLAY "        ENTER DATE AS DDMMYYYY :[          ]"
                AT POS.
            MOVE 1143 TO POS.

            MOVE ' '       TO CDA-DATA.
            MOVE 10        TO CDA-DATALEN.
            MOVE 8        TO CDA-ROW.
            MOVE 42        TO CDA-COL.
            MOVE CDA-WHITE TO CDA-COLOR.
            MOVE 'F'       TO CDA-ATTR.
            PERFORM CTOS-ACCEPT.
            MOVE CDA-DATA TO WS-ANSWER.

            MOVE WS-ANSWER       TO SPLIT-DATE CONVERT-DATE
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE
            MOVE DISPLAY-DATE    TO H1-CR-DATE H1-1-CR-DATE
            MOVE 1143 TO POS.
            DISPLAY DISPLAY-DATE AT POS
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-LAST-DATE.
            IF WS-LAST-DATE NOT > 0
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
            IF W-ESCAPE-KEY = 4
                GO TO CONTROL-005.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-020
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
       GET-020.
            MOVE 1410 TO POS.
            DISPLAY "PRINT DATES THAT = 0.   : [ ]" AT POS.
            MOVE 1511 TO POS
            DISPLAY "A=ALL - ZERO AS WELL, N=NO, Y=ZERO ONLY" AT POS
            MOVE 1437 TO POS.

            MOVE ' '       TO CDA-DATA.
            MOVE 1         TO CDA-DATALEN.
            MOVE 11       TO CDA-ROW.
            MOVE 36        TO CDA-COL.
            MOVE CDA-WHITE TO CDA-COLOR.
            MOVE 'F'       TO CDA-ATTR.
            PERFORM CTOS-ACCEPT.
            MOVE CDA-DATA TO WS-PRINT-DATES.

            IF W-ESCAPE-KEY = 4
                GO TO GET-000.
            IF WS-PRINT-DATES NOT = "Y" AND NOT = "N" AND NOT = "A"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-025
            ELSE
                MOVE 3010 TO POS
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
       GET-025.
            MOVE 1710 TO POS.
            DISPLAY "DO YOU WISH TO DELETE ACCOUNTS, Y OR N. [ ]" AT POS
            MOVE 1751 TO POS.

            MOVE ' '       TO CDA-DATA.
            MOVE 1         TO CDA-DATALEN.
            MOVE 14       TO CDA-ROW.
            MOVE 50        TO CDA-COL.
            MOVE CDA-WHITE TO CDA-COLOR.
            MOVE 'F'       TO CDA-ATTR.
            PERFORM CTOS-ACCEPT.
            MOVE CDA-DATA TO WS-DELETE-ACCS.

            IF W-ESCAPE-KEY = 4
                GO TO GET-020.
            IF WS-DELETE-ACCS NOT = "Y" AND NOT = "N"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-025.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-030
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-025.
       GET-030.
            MOVE 1910 TO POS.
            DISPLAY "S=PURCHASED SINCE, N=NOT SINCE, N OR S. [ ]" AT POS
            MOVE 1951 TO POS.

            MOVE ' '       TO CDA-DATA.
            MOVE 1         TO CDA-DATALEN.
            MOVE 16       TO CDA-ROW.
            MOVE 50        TO CDA-COL.
            MOVE CDA-WHITE TO CDA-COLOR.
            MOVE 'F'       TO CDA-ATTR.
            PERFORM CTOS-ACCEPT.
            MOVE CDA-DATA TO WS-BEFORE-AFTER.

            IF W-ESCAPE-KEY = 4
                GO TO GET-025.
            IF WS-BEFORE-AFTER NOT = "S" AND NOT = "N"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-030.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-035
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-030.
       GET-035.
            MOVE 2110 TO POS.
            DISPLAY "L=LOCAL, F=FOREIGN ACCOUNTS ONLY        [ ]" AT POS
            MOVE 2151 TO POS.

            MOVE ' '       TO CDA-DATA.
            MOVE 1         TO CDA-DATALEN.
            MOVE 18       TO CDA-ROW.
            MOVE 50        TO CDA-COL.
            MOVE CDA-WHITE TO CDA-COLOR.
            MOVE 'F'       TO CDA-ATTR.
            PERFORM CTOS-ACCEPT.
            MOVE CDA-DATA TO WS-LOCAL-FOREIGN.

            IF W-ESCAPE-KEY = 4
                GO TO GET-030.
            IF WS-LOCAL-FOREIGN NOT = "F" AND NOT = "L"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-035.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-040
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-035.
       GET-040.
            MOVE 2310 TO POS.
            DISPLAY "B=BANK INFO, P=PURCHASE INFO            [ ]" AT POS
            MOVE 2351 TO POS.

            MOVE ' '       TO CDA-DATA.
            MOVE 1         TO CDA-DATALEN.
            MOVE 20        TO CDA-ROW.
            MOVE 50        TO CDA-COL.
            MOVE CDA-WHITE TO CDA-COLOR.
            MOVE 'F'       TO CDA-ATTR.
            PERFORM CTOS-ACCEPT.
            MOVE CDA-DATA TO WS-BANK-PURCHASE.

            IF W-ESCAPE-KEY = 4
                GO TO GET-035.
            IF WS-BANK-PURCHASE NOT = "B" AND NOT = "P"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-040.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-045
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-040.
       GET-045.
            MOVE 2510 TO POS
            DISPLAY "The Report Is Being Compiled......" AT POS
            PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE 0 TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-CR-ST1 = 23 OR 35 OR 49
               MOVE 88 TO WS-CR-ST1
               GO TO PRR-999.
       PRR-002.
           READ CREDITOR-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-CR-ST1 = 10
               GO TO PRR-999.
           IF WS-CR-ST1 NOT = 0
               MOVE "CREDITOR BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CR-ST1
               GO TO PRR-002.
               
           MOVE 2410 TO POS
           DISPLAY "Account Number Being Processed:" AT POS
           ADD 35 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
           
           IF WS-PRINT-DATES = "A"
               GO TO PRR-005.
           IF WS-PRINT-DATES = "N"
            IF CR-DATE-LAST-INVOICE = 0
               GO TO PRR-002.
           IF WS-PRINT-DATES = "Y"
            IF CR-DATE-LAST-INVOICE NOT = 0
               GO TO PRR-002.
           IF WS-BEFORE-AFTER = "N"
            IF CR-DATE-LAST-INVOICE NOT > WS-LAST-DATE
               GO TO PRR-005.
           IF WS-BEFORE-AFTER = "S"
            IF CR-DATE-LAST-INVOICE NOT < WS-LAST-DATE
               GO TO PRR-005.
           GO TO PRR-002.
       PRR-005.
           IF WS-LOCAL-FOREIGN = "F"
            IF CR-FOREIGN-LOCAL = "L"
                GO TO PRR-002.
           IF WS-LOCAL-FOREIGN = "L"
            IF CR-FOREIGN-LOCAL = "F"
                GO TO PRR-002.
           IF WS-BEFORE-AFTER = "S"
            IF CR-DATE-LAST-INVOICE NOT < WS-LAST-DATE
               GO TO PRR-010.
           IF WS-BEFORE-AFTER = "N"
            IF CR-DATE-LAST-INVOICE NOT > WS-LAST-DATE
               GO TO PRR-010.
           GO TO PRR-002.
       PRR-010.
           IF LINE-CNT < 60
               GO TO PRR-020.
       PRR-011.
           MOVE " "      TO PRINT-REC
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE H1-1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
           IF WS-BEFORE-AFTER = "S"
             MOVE "     SUPPLIERS PURCHASED SINCE" TO H1-1-DESC
           ELSE
             MOVE "SUPPLIERS NOT PURCHASED SINCE " TO H1-DESC.

            MOVE " " TO PRINT-REC.
           IF WS-BEFORE-AFTER = "N"
               WRITE PRINT-REC FROM HEAD1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD2
            ELSE
               WRITE PRINT-REC FROM HEAD1-1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD2-1.
               
           IF WS-BANK-PURCHASE = "B"
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD3
            ELSE
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD3-1.
               
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE 5 TO LINE-CNT.
       PRR-020.
           IF WS-BANK-PURCHASE = "P"
              GO TO PRR-025.
           IF CR-CURRENCY = " "
               MOVE "RAND "          TO CR-CURRENCY.
           MOVE CR-ACCOUNT-NUMBER    TO D-NUMBER
           MOVE CR-SUPPLIER-NUMBER   TO D-CRED-NUM
           MOVE CR-NAME              TO D-NAME
           MOVE CR-DATE-LAST-INVOICE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE         TO D-PURCH-DATE
           MOVE CR-DATE-CREATED      TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE         TO D-OPEN-DATE
           MOVE CR-TELEPHONE         TO D-PHONE
           MOVE CR-CAMS-BANK-NAME    TO D-BANK-NAME
           MOVE CR-CAMS-BANK-NUM     TO D-BANK-ACC
           MOVE CR-CAMS-BRANCH-NUM   TO D-BANK-CODE.
           
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC.

           GO TO PRR-030.
       PRR-025.
           IF CR-CURRENCY = " "
               MOVE "RAND "          TO CR-CURRENCY.
           MOVE CR-ACCOUNT-NUMBER    TO D2-NUMBER
           MOVE CR-SUPPLIER-NUMBER   TO D2-CRED2-NUM
           MOVE CR-NAME              TO D2-NAME
           MOVE CR-DATE-LAST-INVOICE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE         TO D2-PURCH-DATE
           MOVE CR-DATE-CREATED      TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE         TO D2-OPEN-DATE
           MOVE CR-TELEPHONE         TO D2-PHONE
           MOVE CR-ACC-EMAIL         TO D2-EMAIL
           MOVE CR-PURCHASE-YTD      TO D2-YTD
           MOVE CR-PURCHASE-LAST     TO D2-LYR.
           
           WRITE PRINT-REC FROM DETAIL2-LINE
           MOVE " " TO PRINT-REC.
       PRR-030.
           ADD 1 TO LINE-CNT.
           IF WS-DELETE-ACCS = "Y"
              PERFORM DELETE-ACCOUNT.
           MOVE 0 TO SUB-1.
       PRR-035.
           IF SUB-1 < 50
              ADD 1 TO SUB-1.
           IF CR-CURRENCY NOT = WS-CURRENCY (SUB-1)
            IF WS-CURRENCY (SUB-1) NOT = " "
              GO TO PRR-035.
           IF WS-CURRENCY (SUB-1) = " "
              MOVE CR-CURRENCY  TO WS-CURRENCY (SUB-1).
           ADD CR-PURCHASE-YTD  TO WS-PURCH-YTD (SUB-1)
           ADD CR-PURCHASE-LAST TO WS-PURCH-LY (SUB-1)
           ADD CR-BALANCE       TO WS-BALANCE (SUB-1).
           
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       DELETE-ACCOUNT SECTION.
       DA-001.
            PERFORM WRITE-CREDITOR-OLD-RECORD.
       DA-005.
           DELETE CREDITOR-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-CR-ST1 NOT = 0
               MOVE 0 TO WS-CR-ST1
               MOVE "CREDITOR RECORD BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DA-005.
       DA-999.
           EXIT.
      *
       WRITE-CREDITOR-OLD-RECORD SECTION.
       WOR-010.
            MOVE CREDITOR-RECORD TO CREDITOROLD-RECORD.
            GO TO WOR-020.
       WOR-015.
            REWRITE CREDITOROLD-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOROLD-ST1 NOT = 0
                MOVE 
             "OLD CREDITOR RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CREDITOROLD-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CREDITOROLD-ST1
                GO TO WOR-015.
            GO TO WOR-999.
       WOR-020.
            WRITE CREDITOROLD-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOROLD-ST1 NOT = 0
                MOVE
             "OLD CREDITOR REC BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CREDITOROLD-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CREDITOROLD-ST1
                GO TO WOR-015.
       WOR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
          OPEN I-O CREDITOR-MASTER.
           IF WS-CR-ST1 NOT = 0
               MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-CR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO Open-000.
       OPEN-002.
            OPEN I-O CREDITOROLD-MASTER.
            IF WS-CREDITOROLD-ST1 NOT = 0
               MOVE "CREDITOROLD FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CREDITOROLD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CREDITOROLD-ST1
               GO TO OPEN-003.
       OPEN-0021.
             GO TO OPEN-010.
       OPEN-003.
            OPEN OUTPUT CREDITOROLD-MASTER.
            IF WS-CREDITOROLD-ST1 NOT = 0
               MOVE "CREDITOROLD BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CREDITOROLD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CREDITOROLD-ST1
               GO TO OPEN-002.
       OPEN-010.
          PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE H1-1-DATE.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-001.
           IF WS-CR-ST1 = 88
              PERFORM PRR-011
              MOVE "*** NOTHING TO PRINT IN THAT RANGE ***" TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              GO TO END-150.
           IF LINE-CNT = 66
              PERFORM PRR-011
              MOVE "*** NOTHING TO PRINT IN THAT RANGE ***" TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              GO TO END-150.
           IF LINE-CNT > 45
              PERFORM PRR-011.
           MOVE "** SUMMARY OF CURRENCIES IN RAND AMOUNTS **"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 2.
           WRITE PRINT-REC FROM SUMMARY-HEADING AFTER 1.
           MOVE 0 TO SUB-1.
       END-020.
           IF SUB-1 < 50 
              ADD 1 TO SUB-1.
           IF WS-CURRENCY (SUB-1) = " "
               GO TO END-100.
           MOVE WS-CURRENCY (SUB-1)  TO S-CURRENCY
           MOVE WS-PURCH-YTD (SUB-1) TO S-PURCH-YTD
           MOVE WS-PURCH-LY (SUB-1)  TO S-PURCH-LY
           MOVE WS-BALANCE (SUB-1)   TO S-BALANCE
           WRITE PRINT-REC FROM SUMMARY-LINE AFTER 1
           ADD WS-PURCH-YTD (SUB-1) TO WS-TOTAL-YTD
           ADD WS-PURCH-LY (SUB-1)  TO WS-TOTAL-LY
           ADD WS-BALANCE (SUB-1)   TO WS-TOTAL-BALANCE
           GO TO END-020.
       END-100.
           MOVE "TOTAL:"         TO S-CURRENCY
           MOVE WS-TOTAL-YTD     TO S-PURCH-YTD
           MOVE WS-TOTAL-LY      TO S-PURCH-LY
           MOVE WS-TOTAL-BALANCE TO S-BALANCE
           WRITE PRINT-REC FROM SUMMARY-LINE AFTER 2.
       END-150.
           IF WS-LOCAL-FOREIGN = "F"
              MOVE "*** ONLY FOREIGN ACCOUNTS HAVE BEEN SELECTED ***"
                 TO PRINT-REC
           ELSE
              MOVE "*** ONLY LOCAL ACCOUNTS HAVE BEEN SELECTED ***"
                 TO PRINT-REC.
            WRITE PRINT-REC AFTER 2.
            IF WS-BEFORE-AFTER = "N"
              MOVE 
           "* ALL ACCOUNTS WE HAVE NOT PURCHASED FROM, ARE PRINTED *"
                 TO PRINT-REC
            ELSE
              MOVE 
           "*ACCOUNTS WE HAVE PURCHASED FROM SINCE DATE, ARE PRINTED*"
                 TO PRINT-REC.
            WRITE PRINT-REC AFTER 1.
       END-200.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-800.
            CLOSE CREDITOR-MASTER
                  CREDITOROLD-MASTER
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
