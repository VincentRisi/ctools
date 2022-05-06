        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrMastRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCreditor.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(7) VALUE " ".
       77  WS-RANGE2            PIC X(7) VALUE " ".
       77  WS-NIL-BAL           PIC X VALUE " ".
       77  WS-NO-DETAIL         PIC X VALUE " ".
       77  WS-CURRENCY-TYPE     PIC X(5) VALUE " ".
       77  WS-SUMMARY           PIC X VALUE " ".
       77  WS-FOR-LOC           PIC X VALUE " ".
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
           03  WS-CR-ST1        PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(64) VALUE
           "C R E D I T O R   M A S T E R   L I S T".
           03  FILLER         PIC X(14) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(1) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(45) VALUE " ".
           03  FILLER         PIC X(39) VALUE ALL "*".
           03  FILLER         PIC X(48) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(10) VALUE "ACCOUNT".
           03  FILLER         PIC X(27) VALUE "NAME".
           03  FILLER         PIC X(27) VALUE "PHYSICAL ADDRESS".
           03  FILLER         PIC X(21) VALUE "PHONE / EMAIL".
           03  FILLER         PIC X(11) VALUE "  CURRENT".
           03  FILLER         PIC X(12) VALUE "   BALANCE".
           03  FILLER         PIC X(13) VALUE " PURCH PTD".
       01  HEAD4.
           03  FILLER         PIC X(10) VALUE "NUMBER".
           03  FILLER         PIC X(54) VALUE "ADDRESS".
           03  FILLER         PIC X(21) VALUE "FAX No".
           03  FILLER         PIC X(11) VALUE "   30 DAY".
           03  FILLER         PIC X(12) VALUE "BAL. L/MNT".
           03  FILLER         PIC X(13) VALUE " PURCH YTD".
           03  FILLER         PIC X(11) VALUE "TERMS CODE".
       01  HEAD5.
           03  FILLER         PIC X(64) VALUE " ".
           03  FILLER         PIC X(21) VALUE "CURRENCY".
           03  FILLER         PIC X(11) VALUE "   60 DAY".
           03  FILLER         PIC X(12) VALUE "PURCH DATE".
           03  FILLER         PIC X(13) VALUE " PURCH L/Y".
           03  FILLER         PIC X(11) VALUE "FOR./LOCAL".
       01  HEAD6.
           03  FILLER         PIC X(64) VALUE " ".
           03  FILLER         PIC X(21) VALUE "SUPPLIER No".
           03  FILLER         PIC X(11) VALUE "   90 DAY".
           03  FILLER         PIC X(12) VALUE "PAYMT DATE".
           03  FILLER         PIC X(13) VALUE "SETT. DISC".
           03  FILLER         PIC X(11) VALUE "PAY METHOD".
       01  HEAD7.
           03  FILLER         PIC X(37) VALUE " ".
           03  FILLER         PIC X(27) VALUE "CONTACT NAME".
           03  FILLER         PIC X(21) VALUE " ".
           03  FILLER         PIC X(11) VALUE "  120 DAY".
           03  FILLER         PIC X(12) VALUE "START DATE".
           03  FILLER         PIC X(13) VALUE "TRADE DISC".
           03  FILLER         PIC X(11) VALUE " ".
       01  HEAD8.
           03  FILLER         PIC X(131) VALUE ALL "-".
           03  FILLER         PIC X(1) VALUE " ".
       01  DETAIL-LINE1.
           03  D-ACCOUNT      PIC 9(7).
           03  FILLER         PIC X(3) VALUE " ".
           03  D-NAME         PIC X(54).
           03  D-PHONE        PIC X(20).
           03  D-CURRENT      PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-BALANCE      PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PURCHASEPTD  PIC Z(6)9.99-.
           03  FILLER         PIC X(13) VALUE " ".
       01  DETAIL-LINE2.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-ADD1         PIC X(27).
           03  D-DEL1         PIC X(27).
           03  D-FAX          PIC X(20).
           03  D-30DAY        PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-BAL-LAST     PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PURCHASEYTD  PIC Z(6)9.99-.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-TERMS        PIC X(3).
       01  DETAIL-LINE3.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-ADD2         PIC X(27).
           03  D-DEL2         PIC X(27).
           03  D-CURRENCY     PIC X(20).
           03  D-60DAY        PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PURCHDATE    PIC X(10).
           03  FILLER         PIC X(2) VALUE " ".
           03  D-PURCHASELAST PIC Z(6)9.99-.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-FOR-LOCAL    PIC X(3).
       01  DETAIL-LINE4.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-ADD3         PIC X(27).
           03  D-DEL3         PIC X(27).
           03  D-SUPPLIER     PIC X(20).
           03  D-90DAY        PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PAYDATE      PIC X(10).
           03  FILLER         PIC X(7) VALUE " ".
           03  D-SETTDISC     PIC Z9.99.
           03  FILLER         PIC X(11) VALUE " ".
           03  D-PAYMETHOD    PIC X(3).
       01  DETAIL-LINE5.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-CODE         PIC 9(4).
           03  FILLER         PIC X(23) VALUE " ".
           03  D-CONTACT      PIC X(47) VALUE " ".
           03  D-120DAY       PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-CREATE       PIC X(10).
           03  FILLER         PIC X(7) VALUE " ".
           03  D-TRADEDISC    PIC Z9.99.
           03  FILLER         PIC X(14) VALUE " ".
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
       01  CURRENCY-LINE.
           03  FILLER         PIC X(39) VALUE
            "** ONLY ACCOUNTS LISTED WITH CURRENCY :". 
           03  C-CURRENCY     PIC X(6).
           03  FILLER         PIC X(10) VALUE "PRINTED **".
       01  EMAIL-LINE.
           03  FILLER         PIC X(64).
           03  D-EMAIL        PIC X(40).
      *
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
           DISPLAY "** CREDITOR MASTER LIST **" AT POS
           MOVE 421 TO POS
           DISPLAY "**************************" AT POS.
       CONTROL-003.
           Copy "PrinterAcceptCr".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE " " TO WS-RANGE1 WS-RANGE2
           MOVE 1010 TO POS
           DISPLAY "         FROM CREDITOR NUMBER: [       ]"
                      AT POS
           MOVE 1042 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

      *     ACCEPT WS-RANGE1 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
           MOVE 1210 TO POS
           DISPLAY "           TO CREDITOR NUMBER: [       ]"
                     AT POS
           MOVE 1242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

      *     ACCEPT WS-RANGE2 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-000.
           IF WS-RANGE2 = " "
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
           MOVE 1410 TO POS
           DISPLAY "Print only 'NIL' balances ? Y=Yes ; N=No: [ ]"
               AT POS
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-NIL-BAL.

      *     ACCEPT WS-NIL-BAL AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-010.
           IF WS-NIL-BAL NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-025.
           MOVE 1610 TO POS
           DISPLAY "F=Foreign, L=Local, Leave Blank For ALL : [ ]"
               AT POS
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FOR-LOC.

      *     ACCEPT WS-FOR-LOC AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-015.
           IF WS-FOR-LOC NOT = "F" AND NOT = "L" AND NOT = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-025.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-025.
       GET-050.
           MOVE 1810 TO POS
           DISPLAY "Print only A/C No & NAME ?, Y=Yes ; N=No: [ ]"
               AT POS
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-NO-DETAIL.

      *     ACCEPT WS-NO-DETAIL  AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-025.
           IF WS-NO-DETAIL NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-055
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
       GET-055.
           MOVE 2010 TO POS
           DISPLAY "Should a SUMMARY of currencies be printed:[ ]"
               AT POS
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SUMMARY.

      *     ACCEPT WS-SUMMARY AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-050.
           IF WS-SUMMARY NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-055.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-057
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-055.
       GET-057.
           MOVE 2210 TO POS
           DISPLAY "Enter a PARTICULAR currency, BLANK for ALL:[     ]"
               AT POS
           ADD 44 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 5         TO CDA-DATALEN.
           MOVE 19        TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CURRENCY-TYPE.

      *     ACCEPT WS-CURRENCY-TYPE AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-055.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-060
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-057.
       GET-060.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE
           MOVE 2510 TO POS
           DISPLAY "The Report is being compiled.........." AT POS
           PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE WS-RANGE1 TO CR-ACCOUNT-NUMBER.
            START CREDITOR-MASTER KEY NOT < CR-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-CR-ST1 NOT = 0
               GO TO PRR-999.
       PRR-002.
            READ CREDITOR-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-CR-ST1 = 10
               MOVE 0 TO WS-CR-ST1
               GO TO PRR-999.
            IF WS-CR-ST1 NOT = 0
               MOVE 
               "CREDITOR BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CR-ST1 TO WS-MESSAGE
                PERFORM ERROR-000
                CALL "C$SLEEP" USING 1
                PERFORM ERROR1-020
                PERFORM ERROR-020
                MOVE 0 TO WS-CR-ST1
                GO TO PRR-002.
            IF CR-ACCOUNT-NUMBER < WS-RANGE1
               GO TO PRR-002.
            IF CR-ACCOUNT-NUMBER > WS-RANGE2
               GO TO PRR-999.
            IF WS-NIL-BAL = "Y"
             IF CR-BALANCE = 0
               GO TO PRR-008.
            IF WS-NIL-BAL = "N"
               GO TO PRR-008.
            GO TO PRR-002.
       PRR-008.
            MOVE 2610 TO POS
            DISPLAY "Account Number Being Read:" AT POS
            ADD 27 TO POS
            DISPLAY CR-ACCOUNT-NUMBER AT POS.
            IF WS-FOR-LOC = " "
               GO TO PRR-010.
            IF WS-FOR-LOC = "F"
             IF CR-FOREIGN-LOCAL = "F"
               GO TO PRR-010.
            IF WS-FOR-LOC = "L"
             IF CR-FOREIGN-LOCAL = "L"
               GO TO PRR-010.
            GO TO PRR-002.
       PRR-010.
            IF WS-CURRENCY-TYPE NOT = " "
             IF CR-CURRENCY NOT = WS-CURRENCY-TYPE
               GO TO PRR-002.
            IF WS-NO-DETAIL   = "N"
             IF LINE-CNT < 56
               GO TO PRR-020.
            IF WS-NO-DETAIL   = "Y"
             IF LINE-CNT < 60
               GO TO PRR-020.
       PRR-015.
            ADD 1         TO PAGE-CNT
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
            MOVE " " TO PRINT-REC.
            IF WS-NO-DETAIL = "Y"
                WRITE PRINT-REC
                MOVE 5 TO LINE-CNT
            ELSE
               WRITE PRINT-REC FROM HEAD4
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD5
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD6
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD7
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD8
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE 10 TO LINE-CNT.
       PRR-020.
           MOVE CR-ACCOUNT-NUMBER    TO D-ACCOUNT
           MOVE CR-NAME              TO D-NAME
           MOVE CR-ADDRESS1          TO D-ADD1
           MOVE CR-ADDRESS2          TO D-ADD2
           MOVE CR-ADDRESS3          TO D-ADD3
           MOVE CR-POST-CODE         TO D-CODE
           MOVE CR-DEL-ADDRESS1      TO D-DEL1
           MOVE CR-DEL-ADDRESS2      TO D-DEL2
           MOVE CR-DEL-ADDRESS3      TO D-DEL3
           MOVE CR-TELEPHONE         TO D-PHONE
           MOVE CR-FAX               TO D-FAX.
           IF CR-CURRENCY = " "
               MOVE "SA RANDS"       TO D-CURRENCY
           ELSE
               MOVE CR-CURRENCY      TO D-CURRENCY.
           MOVE CR-TERMS             TO D-TERMS
           MOVE CR-FOREIGN-LOCAL     TO D-FOR-LOCAL
           MOVE CR-PAY-METHOD        TO D-PAYMETHOD
           MOVE CR-SETT-DISC         TO D-SETTDISC
           MOVE CR-TRADE-DISC        TO D-TRADEDISC
           MOVE CR-SUPPLIER-NUMBER   TO D-SUPPLIER
           MOVE CR-DATE-CREATED      TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE         TO D-CREATE
           MOVE CR-DATE-LAST-INVOICE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE         TO D-PURCHDATE
           MOVE CR-DATE-LAST-PAY     TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE         TO D-PAYDATE
           MOVE CR-PURCHASE-PTD      TO D-PURCHASEPTD
           MOVE CR-PURCHASE-YTD      TO D-PURCHASEYTD
           MOVE CR-PURCHASE-LAST     TO D-PURCHASELAST
           MOVE CR-BALANCE           TO D-BALANCE
           MOVE CR-BAL-LAST-STATE    TO D-BAL-LAST
           MOVE CR-CURRENT           TO D-CURRENT
           MOVE CR-30DAY             TO D-30DAY
           MOVE CR-60DAY             TO D-60DAY
           MOVE CR-90DAY             TO D-90DAY
           MOVE CR-120DAY            TO D-120DAY
           MOVE CR-CONTACT-NAME      TO D-CONTACT.
           MOVE CR-EMAIL             TO D-EMAIL.

           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE1
           MOVE " " TO PRINT-REC.
           IF WS-NO-DETAIL   = "Y"
                WRITE PRINT-REC FROM EMAIL-LINE
                MOVE " " TO PRINT-REC
                WRITE PRINT-REC
                ADD 3 TO LINE-CNT
                GO TO PRR-030.
           WRITE PRINT-REC FROM DETAIL-LINE2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE3
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE4
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE5
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           ADD 6 TO LINE-CNT.
      *    GO TO PRR-002.
       PRR-030.
           IF WS-SUMMARY = "N"
              GO TO PRR-002.
           MOVE 0 TO SUB-1.
       PRR-035.
           IF SUB-1 < 50
              ADD 1 TO SUB-1.
           IF D-CURRENCY NOT = WS-CURRENCY (SUB-1)
            IF WS-CURRENCY (SUB-1) NOT = " "
              GO TO PRR-035.
           IF WS-CURRENCY (SUB-1) = " "
              MOVE D-CURRENCY   TO WS-CURRENCY (SUB-1).
           ADD CR-PURCHASE-YTD  TO WS-PURCH-YTD (SUB-1)
           ADD CR-PURCHASE-LAST TO WS-PURCH-LY (SUB-1)
           ADD CR-BALANCE       TO WS-BALANCE (SUB-1).
           
           IF CR-CURRENCY = "RAND" OR "RANDS"
              MOVE " " TO CR-CURRENCY
              PERFORM REWRITE-CREDITOR-RECORD.
           
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       REWRITE-CREDITOR-RECORD SECTION.
       RCR-010.
            REWRITE CREDITOR-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-CR-ST1 NOT = 0
                MOVE 0 TO WS-CR-ST1
                MOVE "CREDITORS BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCR-010.
       RCR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O CREDITOR-MASTER.
           IF WS-CR-ST1 NOT = 0
             MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO OPEN-000.
       OPEN-020.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
          EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 60
              PERFORM PRR-015.
           IF WS-NIL-BAL = "Y"
               MOVE "**** ONLY NIL BALANCE ACCOUNTS PRINTED ****"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 1.
           IF WS-CURRENCY-TYPE NOT = " "
               MOVE WS-CURRENCY-TYPE TO C-CURRENCY
               WRITE PRINT-REC FROM CURRENCY-LINE AFTER 1
           ELSE
               MOVE "**** ALL CURRENCIES FOR ACCOUNTS PRINTED ****"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 1.
       END-010.
           IF WS-SUMMARY = "N"
               GO TO END-850.
           IF LINE-CNT > 45
              PERFORM PRR-015.
           MOVE "** SUMMARY OF CURRENCIES IN RAND AMOUNTS **"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 2.
           WRITE PRINT-REC FROM SUMMARY-HEADING AFTER 1.
           MOVE 0 TO SUB-1.
       END-020.
           IF SUB-1 < 50 
              ADD 1 TO SUB-1.
           IF WS-CURRENCY (SUB-1) = " "
               GO TO END-800.
           MOVE WS-CURRENCY (SUB-1)  TO S-CURRENCY
           MOVE WS-PURCH-YTD (SUB-1) TO S-PURCH-YTD
           MOVE WS-PURCH-LY (SUB-1)  TO S-PURCH-LY
           MOVE WS-BALANCE (SUB-1)   TO S-BALANCE
           WRITE PRINT-REC FROM SUMMARY-LINE AFTER 1
           ADD WS-PURCH-YTD (SUB-1) TO WS-TOTAL-YTD
           ADD WS-PURCH-LY (SUB-1)  TO WS-TOTAL-LY
           ADD WS-BALANCE (SUB-1)   TO WS-TOTAL-BALANCE
           GO TO END-020.
       END-800.
           MOVE "TOTAL:"         TO S-CURRENCY
           MOVE WS-TOTAL-YTD     TO S-PURCH-YTD
           MOVE WS-TOTAL-LY      TO S-PURCH-LY
           MOVE WS-TOTAL-BALANCE TO S-BALANCE
           WRITE PRINT-REC FROM SUMMARY-LINE AFTER 2.
       END-850.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE CREDITOR-MASTER.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
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
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      * END-OF-JOB.
