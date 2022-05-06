        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrFBCPRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrFBCTrans".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCrFBCTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(255).
      *
       WORKING-STORAGE SECTION.
       77  WS-RANGE1            PIC X(20) VALUE " ".
       77  WS-RANGE2            PIC X(20) VALUE " ".
       77  WS-CURRENCY          PIC X(5) VALUE " ".
       77  WS-FBC-NUM           PIC X(20) VALUE " ".
       77  WS-FBC-LOC-AMT       PIC 9(8)V99 VALUE 0.
       77  WS-FBC-FOR-AMT       PIC 9(8)V99 VALUE 0.
       77  WS-FBC-PO-NUM        PIC 9(3) VALUE 0.
       77  WS-DATEANSWER        PIC X(10) VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       01  WS-FBCTRANS-STATUS.
           03  WS-FBCTRANS-ST1  PIC 99.
       01  WS-CHECKDATE.
           03  WS-CHECKYY       PIC 9999.
           03  WS-CHECKMM       PIC 99.
           03  WS-CHECKDD       PIC 99.
       01  WS-COMMENT-LINE.
           03  WSCOM1           PIC X(59) VALUE " ".
           03  WSCOM2           PIC X(59) VALUE " ".
           03  WSCOM3           PIC X(14) VALUE " ".
       01  WS-PO-INFO-NAMES.
         02  WS-PO-INFO OCCURS 15.
           03  WS-PO-CURRENCY      PIC X(5).
           03  WS-PO-AMT           PIC 9(8)V99.
           03  WS-PO-LOC-AMT       PIC 9(8)V99.
           03  WS-PAY-DUE-DATE-INFO OCCURS 12.
              05 WS-PAY-DUE-DATE.
                 07  WS-PAY-YEAR      PIC 9999.
                 07  WS-PAY-MONTH     PIC 99.
                 07  WS-PAY-DAY       PIC 99.
              05  WS-PAY-AMOUNT       PIC 9(8)V99.
       01  WS-DATE-DESCRIPTIONS.
           03  FILLER          PIC X(9) VALUE "  JANUARY".
           03  FILLER          PIC X(9) VALUE " FEBRUARY".
           03  FILLER          PIC X(9) VALUE "    MARCH".
           03  FILLER          PIC X(9) VALUE "    APRIL".
           03  FILLER          PIC X(9) VALUE "      MAY".
           03  FILLER          PIC X(9) VALUE "     JUNE".
           03  FILLER          PIC X(9) VALUE "     JULY".
           03  FILLER          PIC X(9) VALUE "   AUGUST".
           03  FILLER          PIC X(9) VALUE "SEPTEMBER".
           03  FILLER          PIC X(9) VALUE "  OCTOBER".
           03  FILLER          PIC X(9) VALUE " NOVEMBER".
           03  FILLER          PIC X(9) VALUE " DECEMBER".
           03  FILLER          PIC X(9) VALUE " PAST DUE".
       01  WS-DATE-DESCRED REDEFINES WS-DATE-DESCRIPTIONS.
           03  WS-DATE-DESC   PIC X(9) OCCURS 13.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE " DATE:".
           03  H-DATE           PIC X(10).
           03  FILLER           PIC X(22) VALUE " ".
           03  FILLER           PIC X(50) VALUE 
           "FOREX PORDER LISTING FOR ALL CURRENCIES".
           03  FILLER           PIC X(20) VALUE " ".
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(10) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(39) VALUE " ".
           03  FILLER           PIC X(93) VALUE 
           "****************************************".
       01  HEAD3.
           03  FILLER           PIC X(55) VALUE
           "PORDER NUMBER       ACCOUNT  CURRENCY     EXCHANGE".
           03  FILLER           PIC X(45) VALUE
           "FOREIGN   REMAINING       LOCAL    PORDER".
           03  FILLER           PIC X(24) VALUE
           "REC-DUE     PAY-DUE".
       01  HEAD4.
           03  FILLER           PIC X(22) VALUE " ".
           03  FILLER           PIC X(46) VALUE
           "NUMBER    TYPE          RATE      AMOUNT".
           03  FILLER           PIC X(66) VALUE
           "AMOUNT      AMOUNT    DATE       DATE       DATE".
       01  CRFXTRANS-LINE1.
           03  FOREX-PORDER-NUM     PIC X(20).
           03  FILLER               PIC X(1) VALUE " ".
           03  FOREX-ACC-NUMBER     PIC X(12) VALUE " ".
           03  FOREX-CURRENCY       PIC X(7) VALUE " ".
           03  FILLER               PIC X(1) VALUE " ".
           03  FOREX-EXCHANGE-RATE  PIC Z(2)9.9(5).
           03  FILLER               PIC X(1) VALUE " ".
           03  FOREX-FOREIGN        PIC Z(7)9.99 VALUE " ".
           03  FILLER               PIC X(1) VALUE " ".
           03  FOREX-UNAPPLIED      PIC Z(7)9.99.
           03  FILLER               PIC X(1) VALUE " ".
           03  FOREX-LOCAL          PIC Z(7)9.99.
           03  FILLER               PIC X(2) VALUE " ".
           03  FOREX-ORDER-DATE     PIC X(10).
           03  FILLER               PIC X(1) VALUE " ".
           03  FOREX-REC-DUE-DATE   PIC X(10).
           03  FILLER               PIC X(1) VALUE " ".
           03  FOREX-PAY-DUE-DATE   PIC X(10).
       01  INFO-DATE-LINE.
           03  INFO-DESC            PIC X(40) VALUE " ".
           03  INFO-DATE-DESC OCCURS 15.
              05  INFO-DATE-NAME    PIC X(15) VALUE " ".
       01  PO-LINE.
           03  PO-DESC              PIC X(15) VALUE " ".
           03  PO-CURRENCY          PIC X(6) VALUE " ".
           03  PO-VALUE             PIC Z(7)9.99.
           03  FILLER               PIC X(6) VALUE " ".
           03  PO-DATES OCCURS 13.
               05  PO-AMOUNT        PIC Z(7)9.99.
               05  FILLER           PIC X(4) VALUE " ".
       01  FBC-LINE.
           03  FILLER               PIC X(16) VALUE "FBC CONTRACT #:".
           03  FBC-NUMBER           PIC X(20) VALUE " ".
       01  FBC-TOT-LINE.
           03  FILLER               PIC X(21) VALUE " ".
           03  FILLER               PIC X(30) VALUE
           "       TOTAL LOCAL FBC VALUE :".
           03  FBC-TOT-AMOUNT       PIC Z(7)9.99.
           03  FILLER               PIC X(10) VALUE " ".
       01  FBC-TOT-FOR-LINE.
           03  FILLER               PIC X(21) VALUE " ".
           03  FILLER               PIC X(30) VALUE
           "     TOTAL FOREIGN FBC VALUE :".
           03  FBC-TOT-FOR-AMOUNT   PIC Z(7)9.99.
           03  FILLER               PIC X(10) VALUE " ".
       01  FBC-TOT-LINE2.
           03  FILLER               PIC X(21) VALUE " ".
           03  FILLER               PIC X(30) VALUE
           " TOTAL NUMBER OF PO'S ON FBC :".
           03  FBC-TOT-NUMBER       PIC Z(7)9.
           03  FILLER               PIC X(10).
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "**FOREX PORDER Details Report**" AT POS
           MOVE 415 TO POS
           DISPLAY "*******************************" AT POS.
           PERFORM ERROR-020.
       CONTROL-003.
           Copy "PrinterAcceptCr".
       CONTROL-010.
           MOVE " " TO WS-RANGE1.
           MOVE 1010 TO POS.
           DISPLAY "FROM PORDER NUMBER    : [                    ]"
            AT POS.
           MOVE 1035 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 34        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

      *     ACCEPT WS-RANGE1 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-015.
           MOVE " " TO WS-RANGE2.
           MOVE 1210 TO POS.
           DISPLAY "  TO PORDER NUMBER    : [                    ]"
            AT POS.
           MOVE 1235 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 34        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

      *     ACCEPT WS-RANGE2 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
           MOVE 1410 TO POS.
           DISPLAY "ENTER CURRENCY TO PRINT, BLANK FOR ALL: [     ]"
            AT POS.
           MOVE 1451 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 5         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CURRENCY.

      *     ACCEPT WS-CURRENCY AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-040.
           MOVE 1710 TO POS
           DISPLAY
            "ONLY INV's with DUE dates AFTER this date will print."
            AT POS
           MOVE 1610 TO POS.
           DISPLAY "ENTER A PAY-DUE DATE TO PRINT FROM : [          ]"
              AT POS.
           ADD 38 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATEANSWER.

      *     ACCEPT WS-DATEANSWER AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF WS-DATEANSWER = " "
               GO TO CONTROL-040.
           MOVE WS-DATEANSWER   TO ALPHA-RATE
           PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
              GO TO CONTROL-040.
           MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE
           MOVE 1648 TO POS
           DISPLAY DISPLAY-DATE AT POS.
           PERFORM CONVERT-SPLIT-FORMAT
           MOVE SPLIT-DATE TO WS-CHECKDATE.
           PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
              GO TO CONTROL-040.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
       CONTROL-050.
           MOVE 1810 TO POS.
           DISPLAY
           "ENTER AN FBC #, BLANK FOR ALL: [                    ]"
            AT POS.
           MOVE 1842 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FBC-NUM.

      *     ACCEPT WS-FBC-NUM AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-040.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-060
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-050.
       CONTROL-060.
           MOVE 2610 TO POS.
           DISPLAY "The Report Is Being compiled, Please Be Patient"
               AT POS.
       CONTROL-070.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       CHECK-MONTH-NAMES SECTION.
       CDNS-005.
           ACCEPT WS-DATE FROM DATE.
           MOVE 1     TO SUB-6
           MOVE WS-MM TO SUB-7.
       CDNS-010.
           MOVE WS-DATE-DESC (SUB-7) TO INFO-DATE-NAME (SUB-6).
           ADD 1 TO SUB-6 SUB-7.
           IF SUB-7 > 12
             SUBTRACT 12 FROM SUB-7.
           IF SUB-6 NOT > 12
              GO TO CDNS-010.
           MOVE WS-DATE-DESC (13) TO INFO-DATE-NAME (13).
       CDNS-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           MOVE WS-FBC-NUM TO CRFXTRANS-FBC-NUMBER.
           START CRFXTRANS-FILE KEY NOT < CRFXTRANS-FBC-NUMBER
               INVALID KEY NEXT SENTENCE.
           IF WS-FBCTRANS-ST1 NOT = 0
               MOVE "CRFXTRANS-FILE ERROR IN START, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-FBCTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE CRFXTRANS-FBC-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 88 TO WS-FBCTRANS-ST1
               GO TO PR-999.
       PR-002.
           READ CRFXTRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-FBCTRANS-ST1 = 10
               PERFORM PR-500
               GO TO PR-999.
           IF WS-FBCTRANS-ST1 NOT = 0
               MOVE 0 TO WS-FBCTRANS-ST1
               MOVE "CRFXTRANS FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PR-002.
           IF CRFXTRANS-PORDER-NUMBER < WS-RANGE1
               GO TO PR-002.
           IF CRFXTRANS-PORDER-NUMBER > WS-RANGE2
               PERFORM PR-500
               GO TO PR-999.
               
           IF WS-CURRENCY NOT = " "
            IF CRFXTRANS-CURRENCY-TYPE NOT = WS-CURRENCY
               GO TO PR-002.
           IF CRFXTRANS-PAY-DUE-DATE < WS-CHECKDATE
               GO TO PR-002.
      
           MOVE 2510 TO POS
           DISPLAY "P/O NUMBER BEING READ :" AT POS
           ADD 24 TO POS
           DISPLAY CRFXTRANS-PORDER-NUMBER AT POS.
           
           IF WS-FBC-NUM NOT = " "
            IF CRFXTRANS-FBC-NUMBER NOT = WS-FBC-NUM
               GO TO PR-002
            ELSE
               GO TO PR-005.
      **************************************************
      * PRINTING A RANGE OF FBC NUMBERS - ALL          *
      **************************************************
           IF WS-LINE > 60
            IF CRFXTRANS-FBC-NUMBER = " "
               PERFORM PRINT-HEADINGS
               MOVE "** NO CONTRACT **" TO FBC-NUMBER
               WRITE PRINT-REC FROM FBC-LINE AFTER 2
               WRITE PRINT-REC FROM HEAD3 AFTER 1
               WRITE PRINT-REC FROM HEAD4 AFTER 1
               ADD 4 TO WS-LINE
               GO TO PR-015.
           IF WS-LINE > 58
            IF CRFXTRANS-FBC-NUMBER NOT = " "
             AND FBC-NUMBER = "** NO CONTRACT **"
               PERFORM PR-500
               MOVE CRFXTRANS-FBC-NUMBER TO FBC-NUMBER
               WRITE PRINT-REC FROM FBC-LINE AFTER 2
               WRITE PRINT-REC FROM HEAD3 AFTER 1
               WRITE PRINT-REC FROM HEAD4 AFTER 1
               ADD 4 TO WS-LINE
               GO TO PR-015.
           IF WS-LINE > 58
            IF CRFXTRANS-FBC-NUMBER NOT = " "
             IF FBC-NUMBER NOT = "** NO CONTRACT **" 
               PERFORM PR-500
               MOVE CRFXTRANS-FBC-NUMBER TO FBC-NUMBER
               WRITE PRINT-REC FROM FBC-LINE AFTER 2
               WRITE PRINT-REC FROM HEAD3 AFTER 1
               WRITE PRINT-REC FROM HEAD4 AFTER 1
               ADD 4 TO WS-LINE
               GO TO PR-015.
           IF WS-LINE > 58
            IF CRFXTRANS-FBC-NUMBER = " "
             IF FBC-NUMBER = "** NO CONTRACT **" 
               PERFORM PRINT-HEADINGS
               WRITE PRINT-REC FROM FBC-LINE AFTER 2
               WRITE PRINT-REC FROM HEAD3 AFTER 1
               WRITE PRINT-REC FROM HEAD4 AFTER 1
               ADD 4 TO WS-LINE
               GO TO PR-015.
               
           IF FBC-NUMBER NOT = CRFXTRANS-FBC-NUMBER
            IF FBC-NUMBER = "** NO CONTRACT **" 
              AND CRFXTRANS-FBC-NUMBER = " "
              GO TO PR-015
            ELSE
               PERFORM PR-500
               MOVE CRFXTRANS-FBC-NUMBER TO FBC-NUMBER
               WRITE PRINT-REC FROM FBC-LINE AFTER 2
               WRITE PRINT-REC FROM HEAD3 AFTER 1
               WRITE PRINT-REC FROM HEAD4 AFTER 1
               ADD 4 TO WS-LINE.
           GO TO PR-015.
       PR-005.
      **************************************************
      * PRINTING ONLY A SPECIFIED FBC NUMBER           *
      **************************************************
           IF WS-FBC-NUM NOT = CRFXTRANS-FBC-NUMBER
               PERFORM PR-500
               MOVE CRFXTRANS-FBC-NUMBER TO FBC-NUMBER
               WRITE PRINT-REC FROM FBC-LINE AFTER 2
               WRITE PRINT-REC FROM HEAD3 AFTER 1
               WRITE PRINT-REC FROM HEAD4 AFTER 1
               ADD 4 TO WS-LINE.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       PR-010.
           IF WS-LINE > 60
               PERFORM PRINT-HEADINGS
               MOVE CRFXTRANS-FBC-NUMBER TO FBC-NUMBER
               WRITE PRINT-REC FROM FBC-LINE AFTER 2
               WRITE PRINT-REC FROM HEAD3 AFTER 1
               WRITE PRINT-REC FROM HEAD4 AFTER 1
               ADD 4 TO WS-LINE.
       PR-015.
           MOVE CRFXTRANS-PORDER-NUMBER       TO FOREX-PORDER-NUM
           MOVE CRFXTRANS-ACC-NUMBER          TO FOREX-ACC-NUMBER
           MOVE CRFXTRANS-CURRENCY-TYPE       TO FOREX-CURRENCY
           MOVE CRFXTRANS-INITIAL-FOREIGN-AMT TO FOREX-FOREIGN
           MOVE CRFXTRANS-UNAPPLIED-AMT       TO FOREX-UNAPPLIED
           MOVE CRFXTRANS-EXCHANGE-RATE       TO FOREX-EXCHANGE-RATE
           MOVE CRFXTRANS-LOCAL-AMT           TO FOREX-LOCAL
           MOVE CRFXTRANS-ORDER-DATE          TO WS-AGE-DATE SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE                  TO FOREX-ORDER-DATE
           
           MOVE CRFXTRANS-RECEIPT-DUE-DATE    TO WS-AGE-DATE SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE                  TO FOREX-REC-DUE-DATE
           
           MOVE CRFXTRANS-PAY-DUE-DATE        TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE                  TO FOREX-PAY-DUE-DATE.
           
           WRITE PRINT-REC FROM CRFXTRANS-LINE1 AFTER 1.
           MOVE " " TO CRFXTRANS-LINE1.
           ADD CRFXTRANS-LOCAL-AMT           TO WS-FBC-LOC-AMT
           ADD CRFXTRANS-INITIAL-FOREIGN-AMT TO WS-FBC-FOR-AMT
           ADD 1 TO WS-LINE WS-FBC-PO-NUM.
           IF FBC-NUMBER = "** NO CONTRACT **"
              PERFORM ADD-TO-PO.

           GO TO PR-002.
       PR-500.
          IF WS-LINE > 58
               PERFORM PRINT-HEADINGS.
           MOVE WS-FBC-LOC-AMT TO FBC-TOT-AMOUNT
           MOVE WS-FBC-FOR-AMT TO FBC-TOT-FOR-AMOUNT
           MOVE WS-FBC-PO-NUM  TO FBC-TOT-NUMBER
           WRITE PRINT-REC       FROM FBC-TOT-LINE AFTER 1
           WRITE PRINT-REC       FROM FBC-TOT-FOR-LINE AFTER 1
           WRITE PRINT-REC       FROM FBC-TOT-LINE2 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE 0   TO WS-FBC-LOC-AMT
                       WS-FBC-FOR-AMT
                       WS-FBC-PO-NUM.
           WRITE PRINT-REC AFTER 1.
           ADD 5 TO WS-LINE.
       PR-999.
           EXIT.
      *
       ADD-TO-PO SECTION.
       ATPO-005.
           MOVE 0 TO SUB-4.
       ATPO-010.
           ADD 1 TO SUB-4.
           IF WS-PO-CURRENCY (SUB-4) NOT = " "
            IF WS-PO-CURRENCY (SUB-4) NOT = CRFXTRANS-CURRENCY-TYPE
             IF SUB-4 < 15
               GO TO ATPO-010.
           IF WS-PO-CURRENCY (SUB-4) = " "
               MOVE CRFXTRANS-CURRENCY-TYPE  TO WS-PO-CURRENCY (SUB-4).
           IF WS-PO-CURRENCY (SUB-4) = CRFXTRANS-CURRENCY-TYPE
               ADD CRFXTRANS-INITIAL-FOREIGN-AMT TO WS-PO-AMT (SUB-4)
               ADD CRFXTRANS-LOCAL-AMT  TO WS-PO-LOC-AMT (SUB-4).
           MOVE 0 TO SUB-5.
           MOVE CRFXTRANS-PAY-DUE-DATE TO SPLIT-DATE.
       ATPO-020.
           ADD 1 TO SUB-5.
           IF WS-PAY-MONTH (SUB-5 SUB-4) NOT = 0
            IF WS-PAY-MONTH (SUB-5 SUB-4) NOT = SPLIT-MM
             IF SUB-5 < 12
               GO TO ATPO-020.
           IF WS-PAY-MONTH (SUB-5 SUB-4) = 0
            MOVE CRFXTRANS-PAY-DUE-DATE TO WS-PAY-DUE-DATE (SUB-5 SUB-4)
            MOVE CRFXTRANS-INITIAL-FOREIGN-AMT
                             TO WS-PAY-AMOUNT (SUB-5 SUB-4)
            MOVE 30          TO WS-PAY-DAY (SUB-5 SUB-4)
            GO TO ATPO-999.
           IF WS-PAY-MONTH (SUB-5 SUB-4) = SPLIT-MM
           MOVE CRFXTRANS-PAY-DUE-DATE TO WS-PAY-DUE-DATE (SUB-5 SUB-4).
           ADD CRFXTRANS-INITIAL-FOREIGN-AMT
                             TO WS-PAY-AMOUNT (SUB-5 SUB-4)
           MOVE 30           TO WS-PAY-DAY (SUB-5 SUB-4).
       ATPO-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           ADD 1        TO WS-PAGE
           MOVE WS-PAGE TO H-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF WS-PAGE = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           WRITE PRINT-REC FROM HEAD2 AFTER 1.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE 6   TO WS-LINE.
       PH-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE  FROM DATE.
           MOVE WS-DATE      TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H-DATE.
           
           MOVE WS-CO-NAME TO CO-NAME.
       OPEN-010.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       OPEN-020.
           OPEN I-O CRFXTRANS-FILE.
           IF WS-FBCTRANS-ST1 NOT = 0
               MOVE 0 TO WS-FBCTRANS-ST1
               MOVE "CRFBCTRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-020.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF WS-FBCTRANS-ST1 = 88
            IF WS-LINE > 56
               PERFORM PRINT-HEADINGS
               MOVE "** NOTHING TO PRINT IN THAT RANGE **" TO PRINT-REC
               WRITE PRINT-REC AFTER 2
               GO TO END-500.
           MOVE 1 TO SUB-4.
           MOVE 0 TO WS-FBC-LOC-AMT.
              
           MOVE WS-PRINT-COMP TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           IF WS-LINE > 54
               PERFORM PRINT-HEADINGS.
       END-200.
           IF WS-PO-CURRENCY (SUB-4) = " "
              GO TO END-250.
           IF SUB-4 = 1
              MOVE "ANALYSIS OF P/O'S WITH NO CONTRACT:" TO INFO-DESC
              PERFORM CHECK-MONTH-NAMES
              WRITE PRINT-REC FROM INFO-DATE-LINE AFTER 1.
      ****************************************
      * SUB-4 = CURRENCY POSITION
      * SUB-5 = MONTH POSITION
      * SUB-2 = CURRENT MONTH
      * SUB-1 = PRINT AMOUNT POSITION 
      ****************************************
           MOVE " " TO PRINT-REC.
           MOVE "CURRENCY NAME  :"     TO PO-DESC
           MOVE WS-PO-CURRENCY (SUB-4) TO PO-CURRENCY
           MOVE WS-PO-AMT (SUB-4)      TO PO-VALUE
           ADD WS-PO-LOC-AMT (SUB-4)   TO WS-FBC-LOC-AMT.
           MOVE 0     TO SUB-5.
           MOVE WS-MM TO SUB-2.
           SUBTRACT 1 FROM SUB-2.
           MOVE 1     TO SUB-1.
       END-210.
           ADD 1 TO SUB-2 SUB-5.
           IF SUB-2 > 12
               SUBTRACT 12 FROM SUB-2.
           IF WS-PAY-MONTH (SUB-5 SUB-4) = 0
              MOVE 999999 TO WS-PAY-DUE-DATE (SUB-5 SUB-4)
            IF SUB-5 > 1
              PERFORM END-230
              GO TO END-240
            ELSE
              GO TO END-240.
      ************************************
      * THIS SECTION FOR OVER DUE DATES  *
      ************************************
           IF WS-PAY-YEAR (SUB-5 SUB-4) = WS-YY
            IF WS-PAY-MONTH (SUB-5 SUB-4) < WS-MM
              MOVE WS-PAY-AMOUNT (SUB-5 SUB-4)   TO PO-AMOUNT (13)
             IF SUB-5 < 12
                MOVE WS-MM TO SUB-2
                SUBTRACT 1 FROM SUB-2
                MOVE 1     TO SUB-1
                GO TO END-210
             ELSE
                GO TO END-230.
        END-215.
           IF SUB-2 > 12
               SUBTRACT 12 FROM SUB-2.
           IF WS-PAY-MONTH (SUB-5 SUB-4) NOT = SUB-2
              ADD 1 TO SUB-2 SUB-1
              GO TO END-215.
           MOVE WS-PAY-AMOUNT (SUB-5 SUB-4)   TO PO-AMOUNT (SUB-1).
           IF SUB-5 < 12
              MOVE WS-MM TO SUB-2
              SUBTRACT 1 FROM SUB-2
              MOVE 1     TO SUB-1
              GO TO END-210.
       END-230.
           WRITE PRINT-REC FROM PO-LINE AFTER 1.
           MOVE " " TO PRINT-REC PO-LINE.
       END-240.
           IF SUB-4 < 15
              ADD 1 TO SUB-4
              GO TO END-200.
       END-250.
           IF SUB-4 = 1
               GO TO END-300.
           MOVE " " TO PO-LINE.
           MOVE "TOTAL LOCAL AMT:" TO PO-DESC
           MOVE WS-FBC-LOC-AMT     TO PO-VALUE
           WRITE PRINT-REC FROM PO-LINE AFTER 1.
       END-300.
           IF WS-PRINT-TYPE NOT = 2
              MOVE WS-PRINT-NORMAL TO PRINT-REC
              WRITE PRINT-REC AFTER 1.
           IF WS-CURRENCY NOT = " "
              MOVE " *** ONLY A SPECIFIED CURRENCY PRINTED ****"
               TO PRINT-REC
              WRITE PRINT-REC AFTER 1.
      *     
      *     MOVE 1 TO SUB-4.
      * END-400.
      *     MOVE WS-PO-CURRENCY (SUB-4) TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     MOVE WS-PO-AMT (SUB-4) TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     IF SUB-4 < 15
      *        ADD 1 TO SUB-4
      *        GO TO END-400.
      *        
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
           CLOSE PRINT-FILE
                 CRFXTRANS-FILE.
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
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
