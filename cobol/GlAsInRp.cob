        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlAsInRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        REPOSITORY. 
           FUNCTION ALL INTRINSIC.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(80).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-COMMENT           PIC X(15) VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-CAPITAL           PIC S9(8)V99 VALUE 0.
       77  WS-CAPITAL-STORE     PIC S9(8)V99 VALUE 0.
       77  WS-CAPITAL-PORTION   PIC S9(8)V99 VALUE 0.
       77  WS-CAPITAL-PAID      PIC S9(8)V99 VALUE 0.
       77  WS-PAYMENT           PIC S9(8)V99 VALUE 0.
       77  WS-PAYMENT-SAVE      PIC S9(8)V99 VALUE 0.
       77  WS-PAID              PIC S9(8)V99 VALUE 0.
       77  WS-TOTAL-PAID        PIC S9(8)V99 VALUE 0.
       77  WS-INTEREST          PIC S9(6)V9999 VALUE 0.
       77  WS-INT-SAVE          PIC S9(6)V9999 VALUE 0.
       77  WS-INTEREST-BAL      PIC S9(8)V99 VALUE 0.
       77  WS-INTEREST-RATE     PIC S9(6)V9999 VALUE 0.
       77  WS-MONTH-PERC        PIC S9(6)V9999 VALUE 0.
       77  WS-PERC-ASCII        PIC X(11) VALUE " ".
       77  WS-CHECK-YEAR        PIC S9(2)V9999 VALUE 0.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-ANSWER            PIC X(12) VALUE " ".
       77  WS-GROWTH-PAYOFF     PIC X(1) VALUE " ".
       77  WS-FIXED-AMT         PIC X(1) VALUE " ".
       77  WS-ANSWER-DISPLAY    PIC Z(7)9.99-.
       77  WS-INTEREST-DISPLAY  PIC Z(5)9.9999-.
       77  WS-MONTHS            PIC 9(3) VALUE 0.
       77  WS-MONTHS-SAVE       PIC 9(3) VALUE 0.
       77  WS-MONTHS-DISPLAY    PIC Z(2)9.
       77  WS-MONTH             PIC 9(2) VALUE 0.
       77  WS-MONTH-SAVE        PIC 9(2) VALUE 0.
       77  WS-YEAR              PIC 9(4) VALUE 0.
       77  WS-YEAR-SAVE         PIC 9(4) VALUE 0.
       77  WS-MESS              PIC X VALUE " ".
       77  WS-MESSAGE-PART      PIC X(79) VALUE " ".
       01  WS-MONTH-NAMES.
           03  FILLER         PIC X(4) VALUE "JAN".
           03  FILLER         PIC X(4) VALUE "FEB".
           03  FILLER         PIC X(4) VALUE "MAR".
           03  FILLER         PIC X(4) VALUE "APR".
           03  FILLER         PIC X(4) VALUE "MAY".
           03  FILLER         PIC X(4) VALUE "JUN".
           03  FILLER         PIC X(4) VALUE "JUL".
           03  FILLER         PIC X(4) VALUE "AUG".
           03  FILLER         PIC X(4) VALUE "SEP".
           03  FILLER         PIC X(4) VALUE "OCT".
           03  FILLER         PIC X(4) VALUE "NOV".
           03  FILLER         PIC X(4) VALUE "DEC".
       01  WS-MONTH-RED REDEFINES WS-MONTH-NAMES.
           03  WS-MNTH-RED    PIC X(4) OCCURS 12.
           03  FILLER         PIC X(71).
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  FILLER         PIC X(34) VALUE
            "CAPITAL / INTEREST PAYMENT REPORT".
           03  H1-COMMENT     PIC X(19) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
       01  HEAD2.
           03  FILLER         PIC X(16) VALUE " ".
           03  FILLER         PIC X(33) VALUE ALL "*".
           03  FILLER         PIC X(25) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(8) VALUE "CAPITAL:".
           03  H3-CAPITAL     PIC Z(7)9.99-.
           03  FILLER         PIC X(4) VALUE " ".
           03  FILLER         PIC X(8) VALUE "PAYMENT:".
           03  H3-PAYMENT     PIC Z(7)9.99-.
           03  FILLER         PIC X(4) VALUE " ".
           03  FILLER         PIC X(7) VALUE "MONTHS:".
           03  H3-MONTHS      PIC Z(2)9.
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(7) VALUE "INT. %:".
           03  H3-INTEREST    PIC Z9.9999.
       01  HEAD4.
           03  FILLER         PIC X(52) VALUE
           "PMT    DATE   CAPITAL PAID     INTEREST PAID".
           03  FILLER         PIC X(28) VALUE
           "AMT. PAID        BALANCE".
       01  DETAIL-LINE.
           03  D-MONTH        PIC Z(2)9.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-MNTH-TYPE    PIC X(4) VALUE " ".
           03  D-YEAR         PIC X(6) VALUE " ".
           03  D-CAPITAL      PIC Z(7)9.99-.
           03  FILLER         PIC X(6) VALUE " ".
           03  D-INTEREST     PIC Z(7)9.99-.
           03  FILLER         PIC X(5) VALUE " ".
           03  D-TOTAL-PAID   PIC Z(7)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-BALANCE      PIC Z(7)9.99-.
           03  FILLER         PIC X(7) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(8) VALUE " ".
           03  FILLER         PIC X(7) VALUE "TOTALS:".
           03  T-CAPITAL      PIC Z(7)9.99-.
           03  FILLER         PIC X(6) VALUE " ".
           03  T-INTEREST     PIC Z(7)9.99-.
           03  FILLER         PIC X(5) VALUE " ".
           03  T-TOTAL-PAID   PIC Z(7)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  T-BALANCE      PIC Z(7)9.99-.
           03  FILLER         PIC X(7) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 317 TO POS.
           DISPLAY "** ASSET / INTEREST REPAYMENT REPORT **" AT POS.
           MOVE 417 TO POS.
           DISPLAY "***************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 810 TO POS.
           DISPLAY "ASSET GROWTH=G, ASSET PAY-OFF=P: [ ]" AT POS.
           ADD 34 TO POS.

           MOVE WS-GROWTH-PAYOFF TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-GROWTH-PAYOFF.

           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-005.
           IF WS-GROWTH-PAYOFF NOT = "G" AND NOT = "P"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-012
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-012.
           IF WS-GROWTH-PAYOFF = "P"
            IF WS-CAPITAL-STORE > 0
              MOVE H3-CAPITAL TO WS-ANSWER.
           IF WS-GROWTH-PAYOFF = "G"
              MOVE H3-CAPITAL TO WS-ANSWER.
           MOVE 1010 TO POS.
           DISPLAY "ENTER THE TOTAL CAPITAL AMOUNT : [            ]"
                 AT POS.
           ADD 34 TO POS.

           MOVE H3-CAPITAL TO CDA-DATA.
           MOVE 12        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-010.
           MOVE WS-ANSWER TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           IF WS-GROWTH-PAYOFF = "P"
            IF NUMERIC-RATE NOT > 0
               MOVE "YOU MUST ENTER AN AMOUNT > 0, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO CONTROL-012.
           MOVE NUMERIC-RATE TO WS-ANSWER-DISPLAY WS-CAPITAL
                                                  WS-CAPITAL-STORE.
           MOVE WS-CAPITAL TO H3-CAPITAL.
           DISPLAY WS-ANSWER-DISPLAY AT POS.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
       CONTROL-015.
           MOVE " " TO WS-ANSWER.
           IF WS-MONTHS-SAVE > 0
              MOVE H3-MONTHS TO WS-ANSWER.
           MOVE 1210 TO POS.
           DISPLAY "REPAYMENT PERIOD IN MONTHS     : [            ]"
                 AT POS.
           ADD 34 TO POS.

           MOVE WS-MONTHS TO CDA-DATA.
           MOVE 12        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-012.
           MOVE WS-ANSWER TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE NOT > 0
               MOVE "YOU MUST ENTER AN AMOUNT > 0, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO CONTROL-015.
           MOVE NUMERIC-RATE TO WS-MONTHS-DISPLAY WS-MONTHS
                                                  WS-MONTHS-SAVE.
           MOVE " " TO WS-ANSWER.
           DISPLAY WS-ANSWER AT POS.
           ADD 8 TO POS.
           DISPLAY WS-MONTHS-DISPLAY AT POS.
           MOVE WS-MONTHS TO H3-MONTHS.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
           MOVE 1410 TO POS.
           DISPLAY "FIXED AMOUNT THROUGH PERIOD Y/N: [ ]" AT POS.
           ADD 34 TO POS.
           IF WS-GROWTH-PAYOFF = "G"
               MOVE "Y" TO WS-FIXED-AMT.

           MOVE WS-FIXED-AMT TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FIXED-AMT.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF WS-GROWTH-PAYOFF = "G"
            IF WS-FIXED-AMT NOT = "Y"
              MOVE "FOR GROWTH CALCULATIONS THIS MUST BE 'Y', RE-ENTER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO CONTROL-020.
           IF WS-FIXED-AMT NOT = "Y" AND NOT = "N"
               MOVE " " TO WS-FIXED-AMT
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-025.
           IF WS-FIXED-AMT = "N"
               GO TO CONTROL-030.
           MOVE " " TO WS-ANSWER.
           IF WS-PAYMENT-SAVE NOT = 0
               MOVE H3-PAYMENT TO WS-ANSWER.
           MOVE 1610 TO POS.
           DISPLAY "MAX. AMOUNT TO PAY EACH MONTH  : [            ]"
               AT POS.
           ADD 34 TO POS.

           MOVE H3-PAYMENT TO CDA-DATA.
           MOVE 12        TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-020.
           MOVE WS-ANSWER TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           IF WS-GROWTH-PAYOFF = "P"
            IF NUMERIC-RATE NOT > 0
               MOVE "YOU MUST ENTER AN AMOUNT > 0, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO CONTROL-025.
           MOVE NUMERIC-RATE TO WS-ANSWER-DISPLAY WS-PAYMENT
                                                  WS-PAYMENT-SAVE.
           MOVE WS-PAYMENT TO H3-PAYMENT.
           DISPLAY WS-ANSWER-DISPLAY AT POS.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-026
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-025.
       CONTROL-026.
      *     IF WS-GROWTH-PAYOFF NOT = "G"
      *         GO TO CONTROL-030.
           MOVE " " TO WS-ANSWER.
           IF WS-MONTH-PERC > 0
               MOVE WS-MONTH-PERC TO WS-ANSWER-DISPLAY
               MOVE WS-ANSWER-DISPLAY TO WS-ANSWER.
           MOVE 1710 TO POS.
           DISPLAY "UP MONTHLY PMT EACH YEAR BY %  : [            ]"
               AT POS.
           ADD 34 TO POS.

           MOVE WS-PERC-ASCII TO CDA-DATA.
           MOVE 12        TO CDA-DATALEN.
           MOVE 14         TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-025.
           MOVE WS-ANSWER TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WS-ANSWER-DISPLAY WS-MONTH-PERC.
           MOVE WS-ANSWER-DISPLAY TO WS-PERC-ASCII.
           DISPLAY WS-ANSWER-DISPLAY AT POS.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-026.
       CONTROL-030.
           MOVE " " TO WS-ANSWER.
           IF WS-INT-SAVE > 0
               MOVE H3-INTEREST TO WS-ANSWER.
           MOVE 1810 TO POS.
           DISPLAY "ENTER INTEREST RATE AS PERCENT : [            ]"
               AT POS. 
           ADD 34 TO POS.

           MOVE WS-INTEREST-DISPLAY TO CDA-DATA.
           MOVE 12        TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
            IF WS-GROWTH-PAYOFF NOT = "G"
               GO TO CONTROL-025
            ELSE
               GO TO CONTROL-026.
           MOVE WS-ANSWER TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE NOT > 0
               MOVE "YOU MUST ENTER AN AMOUNT > 0, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO CONTROL-030.
           MOVE NUMERIC-RATE TO WS-INTEREST-DISPLAY WS-INTEREST-RATE
                                                    WS-INT-SAVE.
           MOVE WS-INTEREST-RATE TO H3-INTEREST.
           DISPLAY WS-INTEREST-DISPLAY AT POS.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-035
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-030.
       CONTROL-035.
           MOVE " " TO WS-ANSWER.
           IF WS-MONTH-SAVE > 0
             MOVE WS-MONTH-SAVE TO WS-ANSWER.
           MOVE 2010 TO POS.
           DISPLAY "ENTER THE STARTING MONTH NUMBER: [            ]"
               AT POS. 
           ADD 34 TO POS.

           MOVE WS-MONTH TO CDA-DATA.
           MOVE 12        TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-030.
           MOVE WS-ANSWER TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE NOT > 0
               MOVE "YOU MUST ENTER AN AMOUNT > 0, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO CONTROL-035.
           MOVE NUMERIC-RATE TO WS-ANSWER-DISPLAY WS-MONTH
                                                  WS-MONTH-SAVE.
           DISPLAY WS-ANSWER-DISPLAY AT POS.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-037
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-035.
       CONTROL-037.
           MOVE " " TO WS-ANSWER.
           IF WS-YEAR-SAVE > 0
             MOVE WS-YEAR-SAVE TO WS-ANSWER.
           MOVE 2210 TO POS.
           DISPLAY "ENTER THE STARTING YEAR NUMBER : [            ]"
               AT POS. 
           ADD 34 TO POS.

           MOVE WS-YEAR TO CDA-DATA.
           MOVE 12        TO CDA-DATALEN.
           MOVE 19         TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-035.
           MOVE WS-ANSWER TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE NOT > 0
               MOVE "YOU MUST ENTER AN AMOUNT > 0, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO CONTROL-037.
           MOVE NUMERIC-RATE TO WS-ANSWER-DISPLAY WS-YEAR
                                                  WS-YEAR-SAVE.
           DISPLAY WS-ANSWER-DISPLAY AT POS.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-037.
       CONTROL-040.
           MOVE 2410 TO POS.
           DISPLAY "ENTER A HEADING COMMENT TO PRINT:[               ]"
           AT POS.
           ADD 34 TO POS.

           MOVE WS-COMMENT TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 21        TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMMENT.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-037.
           MOVE WS-COMMENT TO H1-COMMENT.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
       CONTROL-050.
           MOVE 3010 TO POS
           DISPLAY "The Report is in progress......." AT POS
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE
           MOVE 2510 TO POS.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           IF WS-FIXED-AMT = "Y"
              PERFORM PRINT-ROUTINE
           ELSE
              PERFORM COMPUTE-PRINT-ROUTINE.
           
           PERFORM END-OFF.
           PERFORM CONTROL-000
           GO TO CONTROL-006.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE 0 TO SUB-1
                     SUB-25.
       PRR-005.
           IF WS-GROWTH-PAYOFF = "G"
              GO TO PRR-008.
           ADD 1 TO SUB-1 SUB-25.
           IF SUB-1 > WS-MONTHS
               GO TO PRR-999.
           IF WS-CAPITAL = 0
               GO TO PRR-999.

      *CHECKING FOR 1ST MONTH OF NEXT YEAR TO ADD YEARLY INCREASE
      * IN CONTRIBUTION.
           IF SUB-25 NOT > 1
               GO TO PRR-006.
           COMPUTE WS-CHECK-YEAR = (SUB-25 - 1) / 12.
           MOVE WS-CHECK-YEAR TO ALPHA-RATE
           IF AL-RATE (3) = 0
            IF AL-RATE (4) = 0
              COMPUTE WS-PAYMENT = WS-PAYMENT +
               (WS-PAYMENT * WS-MONTH-PERC / 100).
       PRR-006.
           IF WS-CAPITAL > 0
              COMPUTE WS-INTEREST ROUNDED =
                  (((WS-CAPITAL * WS-INTEREST-RATE) / 100) / 12)
              ELSE
                  MOVE 0 TO WS-INTEREST.
           IF WS-FIXED-AMT = "Y"
            IF WS-INTEREST NOT = 0
             IF WS-INTEREST NOT > WS-PAYMENT
              COMPUTE WS-CAPITAL-PORTION = (WS-PAYMENT - WS-INTEREST).

           IF WS-CAPITAL-PORTION NOT > WS-CAPITAL
             SUBTRACT WS-CAPITAL-PORTION FROM WS-CAPITAL
             ADD WS-CAPITAL-PORTION        TO WS-CAPITAL-PAID
           ELSE
             MOVE WS-CAPITAL      TO WS-CAPITAL-PORTION
             ADD WS-CAPITAL       TO WS-CAPITAL-PAID
             MOVE 0               TO WS-CAPITAL.

           ADD WS-INTEREST        TO WS-INTEREST-BAL
                                     WS-TOTAL-PAID.
           ADD WS-CAPITAL-PORTION TO WS-TOTAL-PAID.
           COMPUTE WS-PAID = WS-CAPITAL-PORTION + WS-INTEREST.
           GO TO PRR-010.
       PRR-008.
           ADD 1 TO SUB-1 SUB-25.
           IF SUB-1 > WS-MONTHS
               GO TO PRR-999.
      *CHECKING FOR 1ST MONTH OF NEXT YEAR TO ADD YEARLY INCREASE
      * IN CONTRIBUTION.
           IF SUB-25 NOT > 1
              GO TO PRR-009.
           COMPUTE WS-CHECK-YEAR = (SUB-25 - 1) / 12.
           MOVE WS-CHECK-YEAR TO ALPHA-RATE
           IF AL-RATE (3) = 0
            IF AL-RATE (4) = 0
              COMPUTE WS-PAYMENT = WS-PAYMENT +
               (WS-PAYMENT * WS-MONTH-PERC / 100).
       PRR-009.
           ADD WS-PAYMENT TO WS-CAPITAL WS-CAPITAL-PORTION
                             WS-PAID    WS-CAPITAL-PAID.
           COMPUTE WS-INTEREST ROUNDED =
                  (((WS-CAPITAL * WS-INTEREST-RATE) / 100) / 12).

           ADD WS-INTEREST        TO WS-INTEREST-BAL
                                     WS-TOTAL-PAID
                                     WS-CAPITAL.
       PRR-010.
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
               GO TO PRR-012.
           IF LINE-CNT > 25
             MOVE 3010 TO POS
              DISPLAY "Press ANY key for NEXT-PAGE OR 'END' to EXIT"
              AT POS
              ADD 60 TO POS

              MOVE " "       TO CDA-DATA
              MOVE 1         TO CDA-DATALEN
              MOVE 27        TO CDA-ROW
              MOVE 70        TO CDA-COL
              MOVE CDA-WHITE TO CDA-COLOR
              MOVE 'F'       TO CDA-ATTR
              PERFORM CTOS-ACCEPT
              MOVE CDA-DATA TO WS-ACCEPT

      *        ACCEPT WS-ACCEPT AT POS
            ELSE
              GO TO PRR-020.
           IF W-ESCAPE-KEY = 3
              GO TO END-900
           ELSE
              GO TO PRR-015.
       PRR-012.
            IF LINE-CNT < 60
               GO TO PRR-020.
       PRR-015.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINTER = "[VID]" OR = "[Vid]"
               PERFORM CLEAR-SCREEN-PART.
               
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
            IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
            IF PAGE-CNT = 1
              WRITE PRINT-REC FROM HEAD1 AFTER 1
            ELSE
              WRITE PRINT-REC FROM HEAD1 AFTER PAGE.
              
           IF WS-PRINTER = "[VID]" OR = "[Vid]"
               WRITE PRINT-REC FROM HEAD1 AFTER 1.
      *      ELSE
      *         WRITE PRINT-REC FROM HEAD1 AFTER 1.
               
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           IF WS-PRINTER = "[VID]" OR = "[Vid]"
              MOVE 9 TO LINE-CNT
           ELSE
              MOVE 6 TO LINE-CNT.
       PRR-020.
           MOVE SUB-1                  TO D-MONTH
           MOVE WS-MNTH-RED (WS-MONTH) TO D-MNTH-TYPE
           MOVE WS-YEAR                TO D-YEAR
           MOVE WS-CAPITAL-PORTION     TO D-CAPITAL
           MOVE WS-INTEREST            TO D-INTEREST
           MOVE WS-PAID                TO D-TOTAL-PAID
           MOVE WS-CAPITAL             TO D-BALANCE
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           MOVE 0   TO WS-PAID
           ADD 1    TO WS-MONTH.
           IF WS-MONTH > 12
               MOVE 1 TO WS-MONTH
               ADD 1 TO WS-YEAR.
           ADD 1 TO LINE-CNT
           GO TO PRR-005.
       PRR-999.
           EXIT.
      *
       COMPUTE-PRINT-ROUTINE SECTION.
       CPR-000.
           MOVE 0 TO SUB-1.
           COMPUTE WS-CAPITAL-PORTION = WS-CAPITAL / WS-MONTHS.
       CPR-005.
           ADD 1 TO SUB-1.
           IF SUB-1 > WS-MONTHS
               GO TO CPR-999.
           IF WS-CAPITAL = 0
               GO TO CPR-999.
           COMPUTE WS-INTEREST ROUNDED =
                  (((WS-CAPITAL * WS-INTEREST-RATE) / 100) / 12).

           IF WS-CAPITAL-PORTION NOT > WS-CAPITAL
             SUBTRACT WS-CAPITAL-PORTION FROM WS-CAPITAL
             ADD WS-CAPITAL-PORTION        TO WS-CAPITAL-PAID
           ELSE
             MOVE WS-CAPITAL      TO WS-CAPITAL-PORTION
             ADD WS-CAPITAL       TO WS-CAPITAL-PAID
             MOVE 0               TO WS-CAPITAL.

           ADD WS-INTEREST        TO WS-INTEREST-BAL
                                     WS-TOTAL-PAID.
           ADD WS-CAPITAL-PORTION TO WS-TOTAL-PAID.
           COMPUTE WS-PAID = WS-CAPITAL-PORTION + WS-INTEREST.
       CPR-010.
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
               GO TO CPR-012.
           IF LINE-CNT > 25
             MOVE 3010 TO POS
              DISPLAY "Press ANY key for NEXT-PAGE OR 'END' to EXIT"
              AT POS
              ADD 60 TO POS

              MOVE " "       TO CDA-DATA
              MOVE 1         TO CDA-DATALEN
              MOVE 27        TO CDA-ROW
              MOVE 70        TO CDA-COL
              MOVE CDA-WHITE TO CDA-COLOR
              MOVE 'F'       TO CDA-ATTR
              PERFORM CTOS-ACCEPT
              MOVE CDA-DATA TO WS-ACCEPT
            ELSE
              GO TO CPR-020.
           IF W-ESCAPE-KEY = 3
              GO TO END-900
           ELSE
              GO TO CPR-015.
       CPR-012.
            IF LINE-CNT < 60
               GO TO CPR-020.
       CPR-015.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINTER = "[VID]" OR = "[Vid]"
               PERFORM CLEAR-SCREEN-PART.
               
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
            IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
            IF PAGE-CNT = 1
              WRITE PRINT-REC FROM HEAD1 AFTER 1
            ELSE
              WRITE PRINT-REC FROM HEAD1 AFTER PAGE.
              
           IF WS-PRINTER = "[VID]" OR = "[Vid]"
               WRITE PRINT-REC FROM HEAD1 AFTER 1.
      *      ELSE
      *         WRITE PRINT-REC FROM HEAD1 AFTER 1.
               
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           IF WS-PRINTER = "[VID]" OR = "[Vid]"
               MOVE 9 TO LINE-CNT
           ELSE
               MOVE 6 TO LINE-CNT.
       CPR-020.
           MOVE SUB-1                  TO D-MONTH
           MOVE WS-MNTH-RED (WS-MONTH) TO D-MNTH-TYPE
           MOVE WS-YEAR                TO D-YEAR
           MOVE WS-CAPITAL-PORTION     TO D-CAPITAL
           MOVE WS-INTEREST            TO D-INTEREST
           MOVE WS-CAPITAL             TO D-BALANCE
           MOVE WS-PAID                TO D-TOTAL-PAID
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           MOVE 0   TO WS-PAID
           ADD 1    TO WS-MONTH.
           IF WS-MONTH > 12
               MOVE 1 TO WS-MONTH
               ADD 1 TO WS-YEAR.
           ADD 1    TO LINE-CNT
           GO TO CPR-005.
       CPR-999.
           EXIT.
      *
       CLEAR-SCREEN-PART SECTION.
       CSP-005.
           PERFORM CLEAR-SCREEN.
              GO TO CSP-999.
           MOVE 0301 TO POS
           DISPLAY WS-MESSAGE-PART AT POS
           MOVE 0 TO LINE-CNT.
       CSP-010.
           ADD 1 TO LINE-CNT
           ADD 80 TO POS
           DISPLAY WS-MESSAGE-PART AT POS.
           IF LINE-CNT < 29
              GO TO CSP-010.
           MOVE 0280 TO POS
           DISPLAY WS-MESS AT POS.
       CSP-999.
           EXIT.
      *
      *
       END-OFF SECTION.
       END-000.
           IF WS-PRINTER NOT = "[VID]" AND NOT  = "[Vid]"
               GO TO END-010.
           IF LINE-CNT < 25
                 GO TO END-020.
              MOVE 3010 TO POS
              DISPLAY "Press ANY key for NEXT-PAGE OR 'END' to EXIT"
              AT POS
              ADD 60 TO POS

              MOVE " "       TO CDA-DATA
              MOVE 1         TO CDA-DATALEN
              MOVE 27        TO CDA-ROW
              MOVE 70        TO CDA-COL
              MOVE CDA-WHITE TO CDA-COLOR
              MOVE 'F'       TO CDA-ATTR
              PERFORM CTOS-ACCEPT
              MOVE CDA-DATA TO WS-ACCEPT.

           IF W-ESCAPE-KEY = 3
              GO TO END-900
           ELSE
              PERFORM PRR-015
              GO TO END-020.
        END-010.
           IF LINE-CNT > 56
              PERFORM PRR-015.
        END-020.
           MOVE WS-CAPITAL-PAID     TO T-CAPITAL
           MOVE WS-INTEREST-BAL     TO T-INTEREST
           IF WS-GROWTH-PAYOFF = "P"
               MOVE WS-TOTAL-PAID   TO T-TOTAL-PAID
               MOVE WS-CAPITAL      TO T-BALANCE
           ELSE
               COMPUTE WS-TOTAL-PAID = WS-CAPITAL-PAID + WS-INTEREST-BAL
                       + WS-CAPITAL-STORE
               MOVE WS-CAPITAL-PAID TO T-TOTAL-PAID
               MOVE WS-TOTAL-PAID   TO T-BALANCE.

           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC.
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
                GO TO END-030.
           IF WS-PRINTER = "[VID]" OR = "[Vid]"
              MOVE 3010 TO POS
              DISPLAY "Press ANY key to RE-ENTER OR 'END' to EXIT. "
              AT POS
              ADD 60 TO POS

              MOVE " "       TO CDA-DATA
              MOVE 1         TO CDA-DATALEN
              MOVE 27        TO CDA-ROW
              MOVE 70        TO CDA-COL
              MOVE CDA-WHITE TO CDA-COLOR
              MOVE 'F'       TO CDA-ATTR
              PERFORM CTOS-ACCEPT
              MOVE CDA-DATA TO WS-ACCEPT.

           IF W-ESCAPE-KEY = 3
              GO TO END-900.
           IF WS-PRINTER = "[VID]" OR = "[Vid]"
              GO TO END-500.
       END-030.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
           CLOSE PRINT-FILE.
           
           IF WS-PRINTERNUMBER (21) NOT = 20 AND NOT = 0
                PERFORM SEND-REPORT-TO-PRINTER
                GO TO END-900.
           IF WS-PRINTERNUMBER (21) = 0
                GO TO END-900.
           
            MOVE "When Finished Viewing The Report, Press Q to Quit."
              TO WS-MESSAGE
            PERFORM ERROR-MESSAGE. 
              
            MOVE 
            CONCATENATE('less ', ' ', TRIM(WS-PRINTER))
                TO WS-COMMAND-LINE.
      
            CALL "SYSTEM" USING WS-COMMAND-LINE.
      *        RETURNING W-STATUS.
      *     PERFORM SEND-REPORT-TO-PRINTER.
           MOVE 66 TO LINE-CNT
           MOVE 0  TO PAGE-CNT
                      WS-CAPITAL
                      WS-CAPITAL-PORTION
                      WS-CAPITAL-PAID
                      WS-PAYMENT
                      WS-PAID
                      WS-TOTAL-PAID
                      WS-INTEREST
                      WS-INTEREST-BAL
                      WS-INTEREST-RATE.
           GO TO END-999.
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
      * END-OF-JOB.
