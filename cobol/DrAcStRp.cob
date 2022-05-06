        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrAcStRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdDaily.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(145).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER       PIC X(12) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(7) VALUE " ".
       77  WS-RANGE2            PIC X(7) VALUE " ".
       77  WS-TYPE              PIC X(1) VALUE " ".
       77  WS-SUPPLY-Y-N        PIC X(1) VALUE " ".
       77  WS-PLACE-ON-HOLD     PIC X(1) VALUE " ".
       77  WS-SALESMAN          PIC X(1) VALUE " ".
       77  WS-TAKE-OFF-HOLD     PIC X(1) VALUE " ".
       77  WS-AMT-OWED-PERIOD   PIC S9(8)V99 VALUE 0.
       77  WS-TOTAL-BALANCE     PIC S9(8)V99.
       77  WS-TOTAL-CURRENT     PIC S9(8)V99.
       77  WS-TOTAL-30          PIC S9(8)V99.
       77  WS-TOTAL-60          PIC S9(8)V99.
       77  WS-TOTAL-90          PIC S9(8)V99.
       77  WS-TOTAL-120         PIC S9(8)V99.
       77  WS-BRANCH-BALANCE    PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-CURRENT    PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-30         PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-60         PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-90         PIC S9(8)V99 VALUE 0.
       77  WS-BRANCH-120        PIC S9(8)V99 VALUE 0.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND.
               05 WS-DAILY-2ND-1   PIC X(12) VALUE " ".
               05 WS-DAILY-2ND-2   PIC X(8) VALUE " ".
           03  WS-DAILY-3RD.
               05 WS-DAILY-3RD-1   PIC X(12) VALUE " ".
               05 WS-DAILY-3RD-2   PIC X(8) VALUE " ".
           03  WS-DAILY-4TH.
               05 WS-DAILY-4TH-1   PIC X(12) VALUE " ".
               05 WS-DAILY-4TH-2   PIC X(8) VALUE " ".
       01  WS-REPORT-DATE-STRIP.
           03  WS-STRIP1          PIC X(4).
           03  WS-STRIP2          PIC X(18).
           03  WS-STRIP3          PIC X(3).
       01  WS-DEBTOR-STATUS.
           03  WS-DR-ST1          PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1       PIC 99.
       01  HEAD1-SUPPLY.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-SUPP-DATE   PIC X(10).
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(13) VALUE "D E B T O R S".
           03  FILLER         PIC X(21) VALUE "     O N     D O   ".
           03  FILLER         PIC X(54) VALUE "  N O T     S U P P L Y".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-SUPP-PAGE   PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD1-LIMIT.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-LIM-DATE    PIC X(10).
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(13) VALUE "D E B T O R S".
           03  FILLER         PIC X(21) VALUE "   O V E R   T H E ".
           03  FILLER         PIC X(39) VALUE "C R E D I T   L I M I T".
           03  FILLER         PIC X(15) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-LIM-PAGE    PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD1-AGE.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-AGE-DATE    PIC X(10).
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(13) VALUE "D E B T O R S".
           03  FILLER         PIC X(21) VALUE "   A G E  A N A L Y ".
           03  FILLER         PIC X(39) VALUE "S I S    R E P O R T ".
           03  FILLER         PIC X(15) VALUE " ".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-AGE-PAGE    PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(57) VALUE ALL "*".
           03  FILLER         PIC X(45) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(8) VALUE "ACCOUNT".
           03  FILLER         PIC X(26) VALUE "NAME".
           03  FILLER         PIC X(4) VALUE "SP".
           03  FILLER         PIC X(16) VALUE "TELEPHONE".
           03  FILLER         PIC X(9) VALUE "CREDIT".
           03  FILLER         PIC X(11) VALUE "ACCOUNT".
           03  FILLER         PIC X(12) VALUE "CURRENT".
           03  FILLER         PIC X(11) VALUE "30 DAY".
           03  FILLER         PIC X(11) VALUE "60 DAY".
           03  FILLER         PIC X(10) VALUE "90 DAY".
           03  FILLER         PIC X(11) VALUE "120 DAY".
           03  FILLER         PIC X(15) VALUE "LAST".
       01  HEAD5.
           03  FILLER         PIC X(34) VALUE "NUMBER".
           03  FILLER         PIC X(4) VALUE "LY".
           03  FILLER         PIC X(17) VALUE "NUMBER".
           03  FILLER         PIC X(8) VALUE "LIMIT".
           03  FILLER         PIC X(12) VALUE "BALANCE".
           03  FILLER         PIC X(11) VALUE "AMOUNT".
           03  FILLER         PIC X(11) VALUE "AMOUNT".
           03  FILLER         PIC X(11) VALUE "AMOUNT".
           03  FILLER         PIC X(11) VALUE "AMOUNT".
           03  FILLER         PIC X(10) VALUE "AMOUNT".
           03  FILLER         PIC X(15) VALUE "PAID".
       01  DETAIL-LINE.
           03  D-NUMBER       PIC 9(7).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-NAME         PIC X(25) VALUE " ".
           03  FILLER         PIC X(1) VALUE " ".
           03  D-HOLD         PIC X(1) VALUE " ".
           03  D-COD          PIC X(3) VALUE " ".
           03  D-PHONE        PIC X(15) VALUE " ".
           03  D-LIMIT        PIC Z(7).
      *     03  FILLER         PIC X(1) VALUE " ".
           03  D-BALANCE      PIC Z(6)9.99-.
      *     03  FILLER         PIC X(1) VALUE " ".
           03  D-CURRENT      PIC Z(6)9.99-.
      *     03  FILLER         PIC X(1) VALUE " ".
           03  D-30DAY        PIC Z(6)9.99-.
      *     03  FILLER         PIC X(1) VALUE " ".
           03  D-60DAY        PIC Z(6)9.99-.
      *     03  FILLER         PIC X(1) VALUE " ".
           03  D-90DAY        PIC Z(6)9.99-.
      *     03  FILLER         PIC X(1) VALUE " ".
           03  D-120DAY       PIC Z(6)9.99-.
      *     03  FILLER         PIC X(1) VALUE " ".
           03  D-LASTPAID     PIC X(10).
       01  TOTAL-LINE.
           03  FILLER         PIC X(29) VALUE " ".
           03  TOT-DESC       PIC X(31) VALUE
            "** TOTAL O/STANDING AMOUNT **".
           03  TOT-BALANCE    PIC Z(6)9.99-.
           03  TOT-CURRENT    PIC Z(6)9.99-.
           03  TOT-30DAY      PIC Z(6)9.99-.
           03  TOT-60DAY      PIC Z(6)9.99-.
           03  TOT-90DAY      PIC Z(6)9.99-.
           03  TOT-120DAY     PIC Z(6)9.99-.
           03  FILLER         PIC X(9) VALUE " ".
       01  SALESMAN-LINE.
           03  FILLER         PIC X(7) VALUE " ".
           03  SALES-DESC     PIC X(28).
           03  SALES-MAN      PIC X.
           03  FILLER         PIC X(98) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
           MOVE 305 TO POS.
           DISPLAY
         "** DEBTORS ON DO NOT SUPPLY / OVER THE CREDIT LIMIT REPORT **"
               AT POS.
           MOVE 405 TO POS.
           DISPLAY
         "*************************************************************"
               AT POS.
       CONTROL-003.
           Copy "PrinterAcceptDr".
       CONTROL-010.
           PERFORM GET-DATA
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO WS-RANGE1 WS-RANGE2 WS-TYPE.
            MOVE 1010 TO POS.
            DISPLAY "          FROM ACCOUNT NUMBER: [       ]" AT POS.
            MOVE 1042 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

      *      ACCEPT WS-RANGE1 AT POS.
            IF W-ESCAPE-KEY = 4
                GO TO GET-999.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-010
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
       GET-010.
            MOVE 1210 TO POS.
            DISPLAY "            TO ACCOUNT NUMBER: [       ]" AT POS.
            MOVE 1242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

      *      ACCEPT WS-RANGE2 AT POS.
            IF W-ESCAPE-KEY = 4
                GO TO GET-000.
            IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-015
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
       GET-015.
            MOVE 1410 TO POS
            DISPLAY
               "ENTER O=OVER THE LIMIT, D=DO NOT SUPPLY, S=SUSPENDED,"
                      AT POS
            MOVE 1510 TO POS
            DISPLAY 
            "ENTER P=91 DAYS AND OLDER OR ENTER A=AGE ANAYSIS SUMMARY." 
             AT POS
            MOVE 1610 TO POS
            DISPLAY
            "NB! EXTERNAL COD A/C'S IN 30DAYS & OLDER WILL GO ON HOLD."
                 AT POS
            MOVE 1710 TO POS
            DISPLAY "                             : [ ]" AT POS.

            MOVE 2410 TO POS
            DISPLAY
           "TO PUT ACC'S BACK ON SUPPLY YOU MUST USE 'D' AS AN OPTION."
             AT POS.
            
            MOVE 1742 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TYPE.

      *      ACCEPT WS-TYPE AT POS.
            IF W-ESCAPE-KEY = 4
                GO TO GET-010.
            IF WS-TYPE NOT = "D" AND NOT = "O" AND NOT = "P"
                   AND NOT = "S" AND NOT = "A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
            IF WS-TYPE = " "
               GO TO GET-015.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-020
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-015.
       GET-020.
            IF WS-TYPE = "D" OR = "O" OR = "S" OR = "A"
                GO TO GET-025.
            MOVE 1810 TO POS.
            DISPLAY "DO YOU WISH TO PLACE A/C'S OVER 91 DAYS ON HOLD?"
                AT POS.
            MOVE 1939 TO POS.
            DISPLAY ": [ ]" AT POS.
            MOVE 1942 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PLACE-ON-HOLD.

      *      ACCEPT WS-PLACE-ON-HOLD AT POS. 
            IF W-ESCAPE-KEY = 4
                GO TO GET-015.
            IF WS-PLACE-ON-HOLD NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
            IF WS-PLACE-ON-HOLD = " "
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-025
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
       GET-025.
            IF WS-TYPE NOT = "D"
               GO TO GET-026.
            MOVE 2010 TO POS
            DISPLAY 
            "IF A/C=DO-NOT-SUPPLY BUT 90=120 DAY = 0, TAKE OFF HOLD [ ]"
                AT POS
            ADD 56 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 65        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TAKE-OFF-HOLD.

      *      ACCEPT WS-TAKE-OFF-HOLD AT POS. 
            IF W-ESCAPE-KEY = 4
                GO TO GET-015.
            IF WS-TAKE-OFF-HOLD NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-025.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-026
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-025.
       GET-026.
            MOVE 2210 TO POS
            DISPLAY "ENTER A SALESMAN CODE, BLANK FOR ALL ACCOUNTS: [ ]"
                AT POS
            ADD 48 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 19        TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SALESMAN.

      *      ACCEPT WS-SALESMAN AT POS. 
            IF W-ESCAPE-KEY = 4
                GO TO GET-015.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
                GO TO GET-030
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-026.
       GET-030.
            MOVE 2510 TO POS.
            DISPLAY "The Report Is Being Compiled, Please Be Patient."
              AT POS.
            MOVE 3010 TO POS.
            DISPLAY "                                        " AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE WS-RANGE1 TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
           IF WS-DR-ST1 NOT = 0
               MOVE 88 TO WS-DR-ST1
               GO TO PRR-999.
       PRR-002.
           READ DEBTOR-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-DR-ST1 = 10 OR = 91
               GO TO PRR-999.
           IF WS-DR-ST1 NOT = 0
             MOVE "DEBTORMASTER BUSY ON READ-NEXT, 'ESC' TO RE-TRY."
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-DR-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             GO TO PRR-002.
           IF DR-ACCOUNT-NUMBER < WS-RANGE1
               GO TO PRR-002.
           IF DR-ACCOUNT-NUMBER > WS-RANGE2
               GO TO PRR-999.
           MOVE 2310 TO POS
           DISPLAY "Debtor Account Being Read :" AT POS
           ADD 28 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
           
           MOVE DR-SUPPLY-Y-N TO WS-SUPPLY-Y-N.
           
           IF WS-TYPE = "D"
            IF DR-SUPPLY-Y-N = "N"
              GO TO PRR-005.
           IF WS-TYPE = "S"
            IF DR-SUPPLY-Y-N = "S"
              GO TO PRR-005.
           IF WS-TYPE = "O"
            IF DR-BALANCE > DR-CREDIT-LIMIT
              GO TO PRR-005.
              
           IF WS-TYPE = "A"
            IF DR-BALANCE NOT = 0
                GO TO PRR-006.
           IF WS-TYPE = "P"
            IF DR-BALANCE NOT > 0
                GO TO PRR-002.
      *****************************************************************
      * THIS NEW SECTION IS TO MAKE SURE THAT UNALLOCATED
      * CREDITS IN THE PERIOD UNDER ASSESMENT IS NOT > THAN THE INVOICES
      * AS THIS WOULD MEAN THE ACCOUNT WOULD GO ON HOLD PURELY BECAUSE
      * A PERIOD 60DAY EG. HAD AN INVOICE BUT A CREDIT IN 90DAY EG.
      * WOULD COVER THIS.
      *****************************************************************

           IF WS-TYPE = "P"
            IF DR-DISCOUNT-CODE = "3" OR = "4" OR = "8" OR = "9"
               COMPUTE WS-AMT-OWED-PERIOD = DR-60DAY +
                    DR-90DAY + DR-120DAY
             IF WS-AMT-OWED-PERIOD > 0
                GO TO PRR-005.
           IF WS-TYPE = "P"
            IF DR-TERMS-CODE = "2"
               COMPUTE WS-AMT-OWED-PERIOD = DR-30DAY + DR-60DAY +
                    DR-90DAY + DR-120DAY
             IF WS-AMT-OWED-PERIOD > 0
                GO TO PRR-005.
           IF WS-TYPE = "P"
                 COMPUTE WS-AMT-OWED-PERIOD = DR-90DAY + DR-120DAY.
             IF WS-AMT-OWED-PERIOD > 0
              GO TO PRR-005.

           GO TO PRR-002.
       PRR-005.
           IF WS-TAKE-OFF-HOLD = "Y"
               PERFORM TAKE-ACC-OFF-HOLD
               IF DR-SUPPLY-Y-N = "Y"
                 GO TO PRR-002.
      *
      *INTERNAL CASH A/C'S AND BRANCH A/C'S
           IF WS-TYPE = "P"
             IF DR-ACCOUNT-NUMBER = "0300087" OR = "0300090"
                OR = "0300100" OR = "0300150" OR = "0300200"
                OR = "9999999"
                GO TO PRR-002.
           IF WS-TYPE = "P"
            IF DR-SALES-ANALYSIS = 53 OR = 57
                  GO TO PRR-002.
       PRR-006.
           IF WS-SALESMAN = " "
               GO TO PRR-010.
           IF DR-SALESMAN NOT = WS-SALESMAN
              GO TO PRR-002.
       PRR-010.
           IF LINE-CNT < 60
               GO TO PRR-020.
           ADD 1 TO PAGE-CNT.
           MOVE PAGE-CNT TO H1-SUPP-PAGE
                            H1-LIM-PAGE
                            H1-AGE-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           MOVE WS-PRINT-BOLD   TO COMP-DIG1
           MOVE WS-PRINT-UNBOLD TO COMP-DIG2.
           IF PAGE-CNT < 2
              GO TO PRR-015.
           IF WS-TYPE = "A"
               WRITE PRINT-REC BEFORE PAGE
               WRITE PRINT-REC FROM COMPANY-LINE
               WRITE PRINT-REC FROM HEAD1-AGE
               GO TO PRR-016.
           IF WS-TYPE = "O" OR = "P"
               WRITE PRINT-REC BEFORE PAGE
               WRITE PRINT-REC FROM COMPANY-LINE
               WRITE PRINT-REC FROM HEAD1-LIMIT
           ELSE
               WRITE PRINT-REC BEFORE PAGE
               WRITE PRINT-REC FROM COMPANY-LINE
               WRITE PRINT-REC FROM HEAD1-SUPPLY.
           GO TO PRR-016.
       PRR-015.
           IF WS-TYPE = "A"
               WRITE PRINT-REC FROM COMPANY-LINE
               WRITE PRINT-REC FROM HEAD1-AGE
               GO TO PRR-016.
           IF WS-TYPE = "O" OR = "P"
               WRITE PRINT-REC FROM COMPANY-LINE
               WRITE PRINT-REC FROM HEAD1-LIMIT
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE
               WRITE PRINT-REC FROM HEAD1-SUPPLY.
       PRR-016.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD4
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD5
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE 7 TO LINE-CNT.
       PRR-020.
           MOVE " "               TO PRINT-REC
           MOVE DR-ACCOUNT-NUMBER TO D-NUMBER
           MOVE DR-NAME           TO D-NAME
           MOVE DR-SUPPLY-Y-N     TO D-HOLD
           MOVE DR-TELEPHONE      TO D-PHONE
           MOVE DR-CREDIT-LIMIT   TO D-LIMIT
           MOVE DR-BALANCE        TO D-BALANCE
           MOVE DR-CURRENT        TO D-CURRENT
           MOVE DR-30DAY          TO D-30DAY
           MOVE DR-60DAY          TO D-60DAY
           MOVE DR-90DAY          TO D-90DAY
           MOVE DR-120DAY         TO D-120DAY.
           IF DR-TERMS-CODE = "2"
               MOVE "*"           TO D-COD
           ELSE
               MOVE " "           TO D-COD.
           MOVE DR-DATE-LAST-PAY  TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-LASTPAID.

           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC.
           
           ADD DR-BALANCE   TO WS-TOTAL-BALANCE
           ADD DR-CURRENT   TO WS-TOTAL-CURRENT
           ADD DR-30DAY     TO WS-TOTAL-30
           ADD DR-60DAY     TO WS-TOTAL-60
           ADD DR-90DAY     TO WS-TOTAL-90
           ADD DR-120DAY    TO WS-TOTAL-120.
           IF DR-SALES-ANALYSIS = 53 OR = 57
             ADD DR-BALANCE   TO WS-BRANCH-BALANCE
             ADD DR-CURRENT   TO WS-BRANCH-CURRENT
             ADD DR-30DAY     TO WS-BRANCH-30
             ADD DR-60DAY     TO WS-BRANCH-60
             ADD DR-90DAY     TO WS-BRANCH-90
             ADD DR-120DAY    TO WS-BRANCH-120.
           
           IF WS-PLACE-ON-HOLD = "Y"
              PERFORM REWRITE-DEBTOR.
           ADD 1 TO LINE-CNT.
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       TAKE-ACC-OFF-HOLD SECTION.
       TAOH-005.
           IF DR-BALANCE NOT > 0
            IF DR-SUPPLY-Y-N NOT = "S"
                MOVE "Y" TO DR-SUPPLY-Y-N
                PERFORM RD-010
                GO TO TAOH-999.
                
           IF DR-TERMS-CODE = "2"
            IF DR-30DAY NOT > 0
                AND DR-60DAY NOT > 0
                AND DR-90DAY NOT > 0
                AND DR-120DAY NOT > 0
                MOVE "Y" TO DR-SUPPLY-Y-N
                PERFORM RD-010
                GO TO TAOH-999
            ELSE
                GO TO TAOH-999.
                
           IF DR-DISCOUNT-CODE = "3" OR = "4" OR = "8" OR = "9"
            IF DR-60DAY NOT > 0
             AND DR-90DAY NOT > 0
               AND DR-120DAY NOT > 0
                MOVE "Y" TO DR-SUPPLY-Y-N
                PERFORM RD-010
                GO TO TAOH-999
            ELSE
                GO TO TAOH-999.
             
           IF DR-DISCOUNT-CODE NOT = "3" AND NOT = "4" 
                           AND NOT = "8" AND NOT = "9"
            IF DR-90DAY NOT > 0
                AND DR-120DAY NOT > 0
                MOVE "Y" TO DR-SUPPLY-Y-N
                PERFORM RD-010.
       TAOH-999.
           EXIT.
      *
       REWRITE-DEBTOR SECTION.
       RD-005.
           IF DR-SUPPLY-Y-N NOT = "S"
               MOVE "N" TO DR-SUPPLY-Y-N.
       RD-010.
           REWRITE DEBTOR-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-DR-ST1 = 23 OR 35 OR 49
               GO TO RD-999.
           IF WS-DR-ST1 NOT = 0
               MOVE 0 TO WS-DR-ST1
               MOVE "DEBTORS RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RD-010.

           PERFORM PREP-DAILY.
       RD-999.
           EXIT.
      *
       PREP-DAILY SECTION.
       PREP-005.
              MOVE "SUPPLY YES/NO AUTO  " TO WS-DAILY-1ST
              MOVE "ACC NUMBER:"          TO WS-DAILY-2ND-1
              MOVE DR-ACCOUNT-NUMBER      TO WS-DAILY-2ND-2
              MOVE "STATUS WAS:"          TO WS-DAILY-3RD-1
              MOVE WS-SUPPLY-Y-N          TO WS-DAILY-3RD-2
              MOVE "STATUS NOW:"          TO WS-DAILY-4TH-1
              MOVE DR-SUPPLY-Y-N          TO WS-DAILY-4TH-2
              PERFORM WRITE-DAILY.

              PERFORM GET-USER-MAIL-NAME
              PERFORM GET-REPORT-Y2K-DATE
              MOVE "ABOVE CHANGED BY   :" TO WS-DAILY-1ST
              MOVE WS-pbValue             TO WS-DAILY-2ND
              MOVE "DATE & TIME CHANGED:" TO WS-DAILY-3RD
              MOVE pbRet                  TO WS-REPORT-DATE-STRIP
              MOVE WS-STRIP2              TO WS-DAILY-4TH
              PERFORM WRITE-DAILY.
       PREP-999.
           EXIT.       
      *
       OPEN-FILES SECTION.
       OPEN-000.
          OPEN I-O DEBTOR-MASTER.
           IF WS-DR-ST1 NOT = 0 
              MOVE 0 TO WS-DR-ST1
              MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-020.
           Move Ws-Co-Name To Co-Name
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-SUPP-DATE H1-LIM-DATE H1-AGE-DATE.
       OPEN-999.
           EXIT.
      *
        END-OFF SECTION.
        END-010.
           IF LINE-CNT NOT > 58
              GO TO END-020.
           ADD 1 TO PAGE-CNT.
           MOVE PAGE-CNT TO H1-SUPP-PAGE
                            H1-LIM-PAGE
                            H1-AGE-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           MOVE WS-PRINT-BOLD   TO COMP-DIG1
           MOVE WS-PRINT-UNBOLD TO COMP-DIG2.
           IF WS-TYPE = "A"
               WRITE PRINT-REC FROM COMPANY-LINE
               WRITE PRINT-REC FROM HEAD1-AGE
               GO TO END-016.
           IF WS-TYPE = "O" OR = "P"
               WRITE PRINT-REC FROM COMPANY-LINE
               WRITE PRINT-REC FROM HEAD1-LIMIT
           ELSE
               WRITE PRINT-REC BEFORE PAGE
               WRITE PRINT-REC FROM COMPANY-LINE
               WRITE PRINT-REC FROM HEAD1-SUPPLY.
       END-016.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD4
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD5
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE 7 TO LINE-CNT.
           IF WS-DR-ST1 = 88
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC
               MOVE "**** INVALID STARTING NUMBER FOR ACCOUNT *****"
                  TO PRINT-REC
               WRITE PRINT-REC
               GO TO END-800.
       END-020.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.

           MOVE "** TOTAL O/STANDING AMOUNT **" TO TOT-DESC
           MOVE WS-TOTAL-BALANCE   TO TOT-BALANCE
           MOVE WS-TOTAL-CURRENT   TO TOT-CURRENT
           MOVE WS-TOTAL-30        TO TOT-30DAY
           MOVE WS-TOTAL-60        TO TOT-60DAY
           MOVE WS-TOTAL-90        TO TOT-90DAY
           MOVE WS-TOTAL-120       TO TOT-120DAY
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE " " TO PRINT-REC.

           MOVE "  ** ASSOCIATE CO'S AMOUNT **" TO TOT-DESC
           MOVE WS-BRANCH-BALANCE   TO TOT-BALANCE
           MOVE WS-BRANCH-CURRENT   TO TOT-CURRENT
           MOVE WS-BRANCH-30        TO TOT-30DAY
           MOVE WS-BRANCH-60        TO TOT-60DAY
           MOVE WS-BRANCH-90        TO TOT-90DAY
           MOVE WS-BRANCH-120       TO TOT-120DAY
           WRITE PRINT-REC FROM TOTAL-LINE.
           MOVE " " TO PRINT-REC.

           MOVE "    ** NETT COMPANY AMOUNT **" TO TOT-DESC
           SUBTRACT WS-BRANCH-BALANCE FROM WS-TOTAL-BALANCE
           SUBTRACT WS-BRANCH-CURRENT FROM WS-TOTAL-CURRENT
           SUBTRACT WS-BRANCH-30      FROM WS-TOTAL-30
           SUBTRACT WS-BRANCH-60      FROM WS-TOTAL-60
           SUBTRACT WS-BRANCH-90      FROM WS-TOTAL-90
           SUBTRACT WS-BRANCH-120     FROM WS-TOTAL-120
           MOVE WS-TOTAL-BALANCE   TO TOT-BALANCE
           MOVE WS-TOTAL-CURRENT   TO TOT-CURRENT
           MOVE WS-TOTAL-30        TO TOT-30DAY
           MOVE WS-TOTAL-60        TO TOT-60DAY
           MOVE WS-TOTAL-90        TO TOT-90DAY
           MOVE WS-TOTAL-120       TO TOT-120DAY
           WRITE PRINT-REC FROM TOTAL-LINE.
           
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC
               MOVE 
          "ACCOUNTS WITH '*' NEXT TO SUPPLY COLUMN = C.O.D. ACCOUNTS."
                  TO PRINT-REC
               WRITE PRINT-REC.
               
            IF WS-TYPE = "P"
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE "ALL A/C'S WITH AMOUNTS IN 90 DAYS AND OLDER."
                  TO PRINT-REC
               WRITE PRINT-REC
               MOVE "NB!! NOT ALL 'D' ACCOUNTS ARE PRINTED.  TYPE='P'"
                  TO PRINT-REC
               WRITE PRINT-REC.
            IF WS-TYPE = "S"
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE "ALL A/C'S WHICH HAVE BEEN SUSPENDED."
                  TO PRINT-REC
               WRITE PRINT-REC.
            IF WS-TYPE = "D"
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE "ALL A/C'S WHICH ARE ON DO-NOT-SUPPLY.  TYPE='D'"
                  TO PRINT-REC
               WRITE PRINT-REC.
            IF WS-TYPE = "D"
             IF WS-TAKE-OFF-HOLD ="Y"
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE "VALID A/C'S TAKEN OFF DO-NOT-SUPPLY.  TYPE='D'"
                  TO PRINT-REC
               WRITE PRINT-REC.
        END-025.
            IF WS-PLACE-ON-HOLD = "Y"
               MOVE " " TO PRINT-REC
               MOVE "ALL A/C'S ON LIST NOW ON ** DO NOT SUPPLY. **"
                    TO PRINT-REC
               WRITE PRINT-REC.
            IF WS-PLACE-ON-HOLD = "N"
               MOVE " " TO PRINT-REC
               MOVE "A/C'S STATUS NOT CHANGED TO ** DO NOT SUPPLY. **"
                   TO PRINT-REC
               WRITE PRINT-REC.
            IF WS-SALESMAN NOT = " "
               MOVE " " TO PRINT-REC
               MOVE "A/C'S PRINTED FOR SALESMAN :" TO SALES-DESC
               MOVE WS-SALESMAN                    TO SALES-MAN
               WRITE PRINT-REC FROM SALESMAN-LINE
            ELSE
               MOVE " " TO PRINT-REC
               MOVE "A/C'S PRINTED FOR ALL S/MEN " TO SALES-DESC
               MOVE " "                            TO SALES-MAN
               WRITE PRINT-REC FROM SALESMAN-LINE.
       END-800.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE DEBTOR-MASTER
                 PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
      *       STOP RUN.
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
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
       Copy "WriteDailyExcep1".
      * END-OF-JOB.
