        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrMastRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(255).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-FROM              PIC X(7) VALUE " ".
       77  WS-TO                PIC X(7) VALUE " ".
       77  WS-SMAN              PIC X VALUE " ".
       77  WS-SCODE             PIC XX VALUE " ".
       77  WS-RANGE5            PIC X VALUE " ".
       77  WS-NIL               PIC X VALUE " ".
       77  WS-ONE-LINE          PIC X VALUE " ".
       77  WS-CODE-DIS          PIC ZZ.
       77  WS-CODE              PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1        PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(44) VALUE
           "D E B T O R   M A S T E R   L I S T".
           03  FILLER         PIC X(12) VALUE "SALESMAN  :".
           03  H1-SMAN        PIC X(10) VALUE " ".
           03  FILLER         PIC X(7) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(45) VALUE " ".
           03  FILLER         PIC X(35) VALUE ALL "*".
           03  FILLER         PIC X(9) VALUE " ".
           03  FILLER         PIC X(12) VALUE "SALES CODE:".
           03  H2-CODE        PIC X(10) VALUE " ".
           03  FILLER         PIC X(52) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(10) VALUE "ACCOUNT".
           03  FILLER         PIC X(27) VALUE "NAME".
           03  FILLER         PIC X(27) VALUE "DELIVERY ADDRESS".
           03  FILLER         PIC X(15) VALUE "TELEPHONE".
           03  FILLER         PIC X(13) VALUE "  CURRENT".
           03  FILLER         PIC X(13) VALUE "BALANCE".
           03  FILLER         PIC X(13) VALUE "SALES PTD".
           03  FILLER         PIC X(14) VALUE "DISCOUNT".
       01  HEAD4.
           03  FILLER         PIC X(10) VALUE "NUMBER".
           03  FILLER         PIC X(54) VALUE "ADDRESS".
           03  FILLER         PIC X(15) VALUE "FAX".
           03  FILLER         PIC X(13) VALUE "   30 DAY".
           03  FILLER         PIC X(13) VALUE "LAST BAL.".
           03  FILLER         PIC X(13) VALUE "SALES YTD".
           03  FILLER         PIC X(14) VALUE "TERMS".
       01  HEAD5.
           03  FILLER         PIC X(64) VALUE " ".
           03  FILLER         PIC X(15) VALUE "VAT No.".
           03  FILLER         PIC X(13) VALUE "   60 DAY".
           03  FILLER         PIC X(13) VALUE "SALE DATE".
           03  FILLER         PIC X(13) VALUE "SALES L/Y".
           03  FILLER         PIC X(14) VALUE "DELIVERY".
       01  HEAD6.
           03  FILLER         PIC X(64) VALUE " ".
           03  FILLER         PIC X(15) VALUE "ANALYSIS".
           03  FILLER         PIC X(13) VALUE "   90 DAY".
           03  FILLER         PIC X(13) VALUE "PAYM DATE".
           03  FILLER         PIC X(13) VALUE "COSTS PTD".
           03  FILLER         PIC X(14) VALUE "SUPPLY".
       01  HEAD7.
           03  FILLER         PIC X(37) VALUE " ".
           03  FILLER         PIC X(27) VALUE "ACCOUNT CONTACT".
           03  FILLER         PIC X(15) VALUE "CREDIT LIMIT".
           03  FILLER         PIC X(13) VALUE "  120 DAY".
           03  FILLER         PIC X(13) VALUE "OPEN DATE".
           03  FILLER         PIC X(13) VALUE "COSTS YTD".
           03  FILLER         PIC X(14) VALUE "SALESMAN".
       01  HEAD8.
           03  FILLER         PIC X(37) VALUE " ".
           03  FILLER         PIC X(27) VALUE "SALES CONTACT".
           03  FILLER         PIC X(15) VALUE "CORP-GROUP".
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(13) VALUE "COSTS L/Y".
           03  FILLER         PIC X(14) VALUE "PART-ORDERS".
       01  HEAD9.
           03  FILLER         PIC X(127) VALUE ALL "-".
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD-ONELINE.
           03  FILLER         PIC X(8) VALUE "ACCOUNT".
           03  FILLER         PIC X(40) VALUE "NAME".
           03  FILLER         PIC X(21) VALUE "TELEPHONE".
           03  FILLER         PIC X(21) VALUE "FAX ".
           03  FILLER         PIC X(10) VALUE "EMAIL".
       01  DETAIL-ONELINE1.
           03  DO-ACCOUNT     PIC 9(7).
           03  FILLER         PIC X(1) VALUE " ".
           03  DO-NAME        PIC X(40).
           03  DO-PHONE       PIC X(21).
           03  DO-FAX         PIC X(21).
           03  DO-EMAIL       PIC X(20).
       01  DETAIL-ONELINE2.
           03  DO-DEL1        PIC X(26).
           03  DO-DEL2        PIC X(26).
           03  DO-DEL3        PIC X(25).
       01  DETAIL-LINE1.
           03  D-ACCOUNT      PIC 9(7).
           03  FILLER         PIC X(3) VALUE " ".
           03  D-NAME         PIC X(54).
           03  D-PHONE        PIC X(14).
           03  D-CURRENT      PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-BALANCE      PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-SALESPTD     PIC Z(6)9.99-.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-DISCOUNT     PIC X.
           03  FILLER         PIC X(10) VALUE " ".
       01  DETAIL-LINE2.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-ADD1         PIC X(27).
           03  D-DEL1         PIC X(27).
           03  D-TELEX        PIC X(14).
           03  D-30DAY        PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-BAL-LAST     PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-SALESYTD     PIC Z(6)9.99-.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-TERMS        PIC X.
           03  FILLER         PIC X(10) VALUE " ".
       01  DETAIL-LINE3.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-ADD2         PIC X(27).
           03  D-DEL2         PIC X(27).
           03  D-GSTNO        PIC X(14).
           03  D-60DAY        PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-SALEDATE     PIC X(10).
           03  FILLER         PIC X(3) VALUE " ".
           03  D-SALESLAST    PIC Z(6)9.99-.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-DELIVERY     PIC X.
           03  FILLER         PIC X(10) VALUE " ".
       01  DETAIL-LINE4.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-ADD3         PIC X(27).
           03  D-DEL3         PIC X(30).
           03  D-ANALYSIS     PIC 99.
           03  FILLER         PIC X(9) VALUE " ".
           03  D-90DAY        PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-PAYDATE      PIC X(10).
           03  FILLER         PIC X(3) VALUE " ".
           03  D-COSTSPTD     PIC Z(6)9.99-.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-SUPPLY       PIC X.
           03  FILLER         PIC X(10) VALUE " ".
       01  DETAIL-LINE5.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-CODE         PIC 9(4).
           03  FILLER         PIC X(23) VALUE " ".
           03  D-ACC-CONTACT  PIC X(27).
           03  D-LIMIT        PIC Z(4)9.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-120DAY       PIC Z(5)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-CREATE       PIC X(10).
           03  FILLER         PIC X(3) VALUE " ".
           03  D-COSTSYTD     PIC Z(6)9.99-.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-SALESMAN     PIC X.
           03  FILLER         PIC X(9) VALUE " ".
       01  DETAIL-LINE6.
           03  FILLER          PIC X(37) VALUE " ".
           03  D-SALES-CONTACT PIC X(29).
           03  D-CORP-GROUP    PIC Z(2)9.
           03  FILLER          PIC X(35) VALUE " ".
           03  D-COSTSLAST     PIC Z(6)9.99-.
           03  FILLER          PIC X(7) VALUE " ".
           03  D-PART-ORDERS   PIC X.
           03  FILLER          PIC X(9) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 321 TO POS
           DISPLAY "** DEBTOR MASTER LIST **" AT POS
           MOVE 421 TO POS
           DISPLAY "************************" AT POS.
       CONTROL-003.
           Copy "PrinterAcceptDr".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           IF WS-ONE-LINE = "Y"
              PERFORM PRINT-ONELINE-ROUTINE
           ELSE
              PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO WS-FROM WS-TO WS-SMAN WS-SCODE.
            MOVE 1010 TO POS.
            DISPLAY "           FROM DEBTOR NUMBER: [       ]" AT POS.
            MOVE 1042 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FROM.

      *      ACCEPT WS-FROM AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO GET-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
            MOVE 1210 TO POS.
            DISPLAY "             TO DEBTOR NUMBER: [       ]" AT POS.
            MOVE 1242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TO.

      *      ACCEPT WS-TO AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-000.
           IF WS-TO = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO GET-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
           MOVE 1410 TO POS.
           DISPLAY "Print only 'NIL' balances ? Y=Yes ; N=No: [ ]"
               AT POS.
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-NIL.

      *     ACCEPT WS-NIL AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-010.
           IF WS-NIL NOT = "Y" AND NOT = "N"
                DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO GET-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-050.
           MOVE 1710 TO POS.
           DISPLAY
           "Enter a Salesman code Or leave BLANK to print ALL A/C's:"
                AT POS.
           MOVE 1767 TO POS.
           DISPLAY "[ ]" AT POS.
           MOVE 1768 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 67        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SMAN.

      *     ACCEPT WS-SMAN AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO GET-055
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-050.
       GET-055.
           MOVE 1910 TO POS
           DISPLAY
           "Enter a Sales Code Or leave BLANK to print ALL A/C's:"
                AT POS
           MOVE 1967 TO POS
           DISPLAY "[  ]" AT POS
           MOVE 1968 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 67        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SCODE.

      *     ACCEPT WS-SCODE AT POS.
           MOVE WS-SCODE TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-CODE WS-CODE-DIS
           MOVE 1968 TO POS
           DISPLAY WS-CODE-DIS AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-050.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO GET-060
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-055.
       GET-060.
           MOVE 2110 TO POS
           DISPLAY "Print ONLY ONE line of Info, Y or N: [ ]" AT POS
           MOVE 2148 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 18        TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ONE-LINE.

      *     ACCEPT WS-ONE-LINE AT POS.
           IF WS-ONE-LINE NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-060.
           IF W-ESCAPE-KEY = 4
               GO TO GET-050.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO GET-066
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-060.
       GET-066.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE
           MOVE 2910 TO POS
           DISPLAY "The Report is being compiled.........." AT POS
           MOVE 3010 TO POS
           DISPLAY " " AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE WS-FROM TO DR-ACCOUNT-NUMBER.
            START DEBTOR-MASTER KEY NOT < DR-KEY.
       PRR-002.
            READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 10
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PRR-999.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PRR-002.
            IF DR-ACCOUNT-NUMBER < WS-FROM
               GO TO PRR-002.
            IF DR-ACCOUNT-NUMBER > WS-TO
               GO TO PRR-999.
           MOVE 2510 TO POS
           DISPLAY "Account Number Being Read:" AT POS
           ADD 27 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
            IF WS-NIL = "Y"
             IF DR-BALANCE = 0
               GO TO PRR-005.
            IF WS-NIL = "N"
               GO TO PRR-005.
            GO TO PRR-002.
       PRR-005.
            IF WS-SMAN = " "
               GO TO PRR-008.
            IF WS-SMAN NOT = DR-SALESMAN
               GO TO PRR-002.
       PRR-008.
            IF WS-SCODE = " "
               GO TO PRR-010.
            IF WS-CODE NOT = DR-SALES-ANALYSIS
               GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 56
               GO TO PRR-020.
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
               
            IF WS-SMAN > " "
                MOVE WS-SMAN TO H1-SMAN
            ELSE
                MOVE "ALL"   TO H1-SMAN.
            IF WS-SCODE > " "
                MOVE WS-SCODE TO H2-CODE
            ELSE
                MOVE "ALL"   TO H2-CODE.
               
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3
            MOVE " " TO PRINT-REC
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
            WRITE PRINT-REC FROM HEAD9
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE 11 TO LINE-CNT.
       PRR-020.
           MOVE DR-ACCOUNT-NUMBER   TO D-ACCOUNT
           MOVE DR-NAME             TO D-NAME
           MOVE DR-ADDRESS1         TO D-ADD1
           MOVE DR-ADDRESS2         TO D-ADD2
           MOVE DR-ADDRESS3         TO D-ADD3
           MOVE DR-POST-CODE        TO D-CODE
           MOVE DR-DEL-ADDRESS1     TO D-DEL1
           MOVE DR-DEL-ADDRESS2     TO D-DEL2
           MOVE DR-DEL-ADDRESS3     TO D-DEL3
           MOVE DR-TELEPHONE        TO D-PHONE
           MOVE DR-TELEX            TO D-TELEX
           MOVE DR-GSTNO            TO D-GSTNO
           MOVE DR-SALES-ANALYSIS   TO D-ANALYSIS
           MOVE DR-CREDIT-LIMIT     TO D-LIMIT
           MOVE DR-BALANCE          TO D-BALANCE
           MOVE DR-BAL-LAST-STATE   TO D-BAL-LAST
           MOVE DR-CURRENT          TO D-CURRENT
           MOVE DR-30DAY            TO D-30DAY
           MOVE DR-60DAY            TO D-60DAY
           MOVE DR-90DAY            TO D-90DAY
           MOVE DR-120DAY           TO D-120DAY

           MOVE DR-DATE-CREATED     TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE        TO D-CREATE

           MOVE DR-DATE-LAST-SALE   TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE        TO D-SALEDATE

           MOVE DR-DATE-LAST-PAY    TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE        TO D-PAYDATE

           MOVE DR-SALES-PTD        TO D-SALESPTD
           MOVE DR-SALES-YTD        TO D-SALESYTD
           MOVE DR-SALES-LAST       TO D-SALESLAST
           MOVE DR-COST-PTD         TO D-COSTSPTD
           MOVE DR-COST-YTD         TO D-COSTSYTD
           MOVE DR-COST-LAST        TO D-COSTSLAST
           MOVE DR-DISCOUNT-CODE    TO D-DISCOUNT
           MOVE DR-TERMS-CODE       TO D-TERMS
           MOVE DR-DELIVERY-CODE    TO D-DELIVERY
           MOVE DR-SUPPLY-Y-N       TO D-SUPPLY
           MOVE DR-SALESMAN         TO D-SALESMAN
           MOVE DR-PART-ORDERS      TO D-PART-ORDERS
           MOVE DR-CORPORATE-GROUP  TO D-CORP-GROUP
           MOVE DR-ACCOUNTS-CONTACT TO D-ACC-CONTACT
           MOVE DR-SALES-CONTACT    TO D-SALES-CONTACT.

           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE3
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE4
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE5
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE6
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           ADD 7 TO LINE-CNT
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       PRINT-ONELINE-ROUTINE SECTION.
       POL-000.
            MOVE WS-FROM TO DR-ACCOUNT-NUMBER.
            START DEBTOR-MASTER KEY NOT < DR-KEY.
       POL-002.
            READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 10
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO POL-999.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO POL-002.
            IF DR-ACCOUNT-NUMBER < WS-FROM
               GO TO POL-002.
            IF DR-ACCOUNT-NUMBER > WS-TO
               GO TO POL-999.
           MOVE 2510 TO POS
           DISPLAY "Account Number Being Read:" AT POS
           ADD 27 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
            IF WS-NIL = "Y"
             IF DR-BALANCE = 0
               GO TO POL-005.
            IF WS-NIL = "N"
               GO TO POL-005.
            GO TO POL-002.
       POL-005.
            IF WS-SMAN = " "
               GO TO POL-008.
            IF WS-SMAN NOT = DR-SALESMAN
               GO TO POL-002.
       POL-008.
            IF WS-SCODE = " "
               GO TO POL-010.
            IF WS-CODE NOT = DR-SALES-ANALYSIS
               GO TO POL-002.
       POL-010.
           IF LINE-CNT < 56
               GO TO POL-020.
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
               
            IF WS-SMAN > " "
                MOVE WS-SMAN TO H1-SMAN
            ELSE
                MOVE "ALL"   TO H1-SMAN.
            IF WS-SCODE > " "
                MOVE WS-SCODE TO H2-CODE
            ELSE
                MOVE "ALL"   TO H2-CODE.
               
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD-ONELINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE 5 TO LINE-CNT.
       POL-020.
           MOVE DR-ACCOUNT-NUMBER   TO DO-ACCOUNT
           MOVE DR-NAME             TO DO-NAME
           MOVE DR-DEL-ADDRESS1     TO DO-DEL1
           MOVE DR-DEL-ADDRESS2     TO DO-DEL2
           MOVE DR-DEL-ADDRESS3     TO DO-DEL3
           MOVE DR-TELEPHONE        TO DO-PHONE
           MOVE DR-TELEX            TO DO-FAX
           MOVE DR-SALES-EMAIL      TO DO-EMAIL.

           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-ONELINE1
           
           MOVE SPACES TO ALPHA-RATE DATA-RATE
           MOVE DETAIL-ONELINE2 TO ALPHA-RATE
           PERFORM CONSOLIDATE-DEL-ADDRESS
           
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-ONELINE2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           ADD 3 TO LINE-CNT
           GO TO POL-002.
       POL-999.
           EXIT.
      *
       CONSOLIDATE-DEL-ADDRESS SECTION.
       CDA-005.
           MOVE 1 TO SUB-1 SUB-2.
       CDA-010.
           IF SUB-1 > 90
              GO TO CDA-020.
           IF AL-RATE (SUB-1) NOT = " "
              GO TO CDA-015.
              
           IF AL-RATE (SUB-1) = " "
              MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2)
              ADD 1 TO SUB-1 SUB-2.
       CDA-013.
           IF SUB-1 > 90
              GO TO CDA-020.
            IF AL-RATE (SUB-1) = " "
              ADD 1 TO SUB-1
              GO TO CDA-013.
       CDA-015.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 90
              GO TO CDA-010.
       CDA-020.
           MOVE DATA-RATE TO DETAIL-ONELINE2.
       CDA-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTORFILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
           Move Ws-Co-Name To Co-Name.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF WS-NIL = "Y"
               MOVE "**** ONLY NIL BALANCE ACCOUNTS PRINTED ****"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 1
           ELSE
               MOVE "**** ALL BALANCE TYPE ACCOUNTS PRINTED ****"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 1.
           IF WS-SMAN NOT = " "
               MOVE "*** ONLY A SPECIFIC SALESMANS ACCOUNTS PRINTED ***"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 1
           ELSE
               MOVE "*** ALL SALESMEN ACCOUNTS PRINTED ***"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 1.
           IF WS-SCODE NOT = " "
               MOVE "*** ONLY A SPECIFIC SALES-ANALYSIS CODE PRINTED **"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 1
           ELSE
               MOVE "*** ALL SALES-ANALYSIS CODES PRINTED **"
               TO PRINT-REC
               WRITE PRINT-REC AFTER 1.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE DEBTOR-MASTER
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
