        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrArJhRp.
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
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  SUB-1-SAVE           PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-MARGIN            PIC S9(7)V99 VALUE 0.
       77  WS-PERC              PIC S999V99.
       77  TOT-SALESMTD         PIC S9(7)V99 VALUE 0.
       77  TOT-COSTMTD          PIC S9(7)V99 VALUE 0.
       77  TOT-MARGINMTD        PIC S9(7)V99 VALUE 0.
       77  TOT-SALESYTD         PIC S9(7)V99 VALUE 0.
       77  TOT-COSTYTD          PIC S9(7)V99 VALUE 0.
       77  TOT-SALESLAST        PIC S9(7)V99 VALUE 0.
       77  TOT-COSTLAST         PIC S9(7)V99 VALUE 0.
       77  TOT-AREA-NO          PIC 9(5) VALUE 0.
       01  WS-POST-CODE.
           03  WS-CODE OCCURS 200.
               05  W-CODENO     PIC 9(4).
               05  W-ACCNO      PIC 9(4).
               05  W-ADD        PIC X(25).
               05  W-SALESMTD   PIC S9(7)V99.
               05  W-COSTMTD    PIC S9(7)V99.
               05  W-SALESYTD   PIC S9(7)V99.
               05  W-COSTYTD    PIC S9(7)V99.
               05  W-SALESLAST  PIC S9(7)V99.
               05  W-COSTLAST   PIC S9(7)V99.
       01  WS-AREA-CODE.
           03  WS-AREA OCCURS 6.
               05  W-AREA-NO     PIC 9(4).
               05  W-NO-IN-AREA  PIC 9(4).
               05  W-AREA-NAME   PIC X(25).
               05  W-SALESMTD-A  PIC S9(7)V99.
               05  W-COSTMTD-A   PIC S9(7)V99.
               05  W-SALESYTD-A  PIC S9(7)V99.
               05  W-COSTYTD-A   PIC S9(7)V99.
               05  W-SALESLAST-A PIC S9(7)V99.
               05  W-COSTLAST-A  PIC S9(7)V99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(89) VALUE
           "D E B T O R S   S A L E S   I N   T H E   J H B   A R E A".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(57) VALUE ALL "*".
           03  FILLER         PIC X(46) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(85) VALUE " ".
           03  FILLER         PIC X(35) VALUE
            "PERIOD / YEAR / LAST YEAR TO DATE".
       01  HEAD4.
           03  FILLER         PIC X(18) VALUE "POST CODE".
           03  FILLER         PIC X(40) VALUE "AREA NAME".
           03  FILLER         PIC X(24) VALUE "No OF ACC'S".
           03  FILLER         PIC X(18) VALUE "SALES AMT".
           03  FILLER         PIC X(10) VALUE " COST".
           03  FILLER         PIC X(15) VALUE " MARGIN".
           03  FILLER         PIC X(8) VALUE "%".
       01  DETAIL-LINE.
           03  D-CATEGORY.
               05  D-POST     PIC 9(4).
               05  FILLER     PIC X(14) VALUE " ".
               05  D-FILL.
                  07  D-NAME     PIC X(40).
                  07  FILLER     PIC X(2) VALUE " ".
                  07  D-NO       PIC Z(4)9.
                  07  FILLER     PIC X(12) VALUE " ".
           03  FILLER         PIC X(4) VALUE " ".
           03  D-SALES        PIC Z(6)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-COST         PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-MARGIN       PIC Z(6)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-PERC         PIC Z99.99-.
           03  FILLER         PIC X(5) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONT-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "** DETAIL DEBTORS SALES REPORT IN JHB AREA **"
             AT POS
           MOVE 415 TO POS
           DISPLAY "*********************************************"
             AT POS.
       CONT-003.
           Copy "PrinterAcceptDr".
        CONT-010.
           PERFORM OPEN-FILES.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE
           MOVE 2910 TO POS
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
             AT POS.
       CONT-020.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE 88 TO WS-DEBTOR-ST1
              GO TO PRR-999.
       PRR-005.
           READ DEBTOR-MASTER NEXT
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
               GO TO PRR-005.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           IF DR-ACCOUNT-NUMBER = "0000000"
              GO TO PRR-005.
           MOVE 2510 TO POS
           DISPLAY "Debtor File Being Read:" AT POS
           ADD 25 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.

           IF DR-POST-CODE > 1999 AND < 2200
              GO TO PRR-010.

           GO TO PRR-005.
       PRR-010.
            IF LINE-CNT < 60
               MOVE 0 TO SUB-1
               GO TO PRR-020.
       PRR-015.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE.
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
            WRITE PRINT-REC FROM HEAD1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD4
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE 7 TO LINE-CNT.
       PRR-020.
           ADD 1 TO SUB-1.
           IF W-CODENO (SUB-1) NOT = 0
              GO TO PRR-025.
           MOVE DR-POST-CODE TO W-CODENO (SUB-1)
           MOVE 1            TO W-ACCNO (SUB-1).
           IF DR-ADDRESS3 NOT = " "
                      AND NOT = "JOHANNESBURG            "
               MOVE DR-ADDRESS3 TO W-ADD (SUB-1)
           ELSE
               MOVE DR-ADDRESS2 TO W-ADD (SUB-1).
           ADD DR-SALES-PTD     TO W-SALESMTD (SUB-1)
                                 TOT-SALESMTD
           ADD DR-COST-PTD      TO W-COSTMTD (SUB-1)
                                 TOT-COSTMTD
           ADD DR-SALES-YTD     TO W-SALESYTD (SUB-1)
                                 TOT-SALESYTD
           ADD DR-COST-YTD      TO W-COSTYTD (SUB-1)
                                 TOT-COSTYTD
           ADD DR-SALES-LAST    TO W-SALESLAST (SUB-1)
                                 TOT-SALESLAST
           ADD DR-COST-LAST     TO W-COSTLAST (SUB-1)
                                 TOT-COSTLAST
           GO TO PRR-005.
       PRR-025.
           IF DR-POST-CODE = W-CODENO (SUB-1)
              ADD 1            TO W-ACCNO (SUB-1)
              ADD DR-SALES-PTD TO W-SALESMTD (SUB-1)
                                  TOT-SALESMTD
              ADD DR-COST-PTD  TO W-COSTMTD (SUB-1)
                                  TOT-COSTMTD
              ADD DR-SALES-YTD TO W-SALESYTD (SUB-1)
                                  TOT-SALESYTD
              ADD DR-COST-YTD  TO W-COSTYTD (SUB-1)
                                  TOT-COSTYTD
              ADD DR-SALES-LAST TO W-SALESLAST (SUB-1)
                                 TOT-SALESLAST
              ADD DR-COST-LAST  TO W-COSTLAST (SUB-1)
                                 TOT-COSTLAST
              GO TO PRR-005.
           IF DR-POST-CODE NOT = W-CODENO (SUB-1)
               GO TO PRR-020.
       PRR-999.
           EXIT.
      *
       CHECK-TOTAL-IN-AREA SECTION.
       CTIA-010.
      *WS-AREA-CODE. 1=CENTRAL, 2=NORTH, 3=SOUTH, 4=WEST, 5=EAST.
           IF W-CODENO (SUB-1) = 2000 OR = 2001 OR = 2013 OR = 2017
                  OR = 2020 OR = 2023 OR = 2028 OR = 2038 OR = 2043
                  OR = 2044 OR = 2045 OR = 2048 OR = 2107 OR = 2111
              MOVE 1 TO SUB-1-SAVE
              ADD W-ACCNO (SUB-1)     TO W-NO-IN-AREA (SUB-1-SAVE)
              ADD W-SALESMTD (SUB-1)  TO W-SALESMTD-A (SUB-1-SAVE)
              ADD W-COSTMTD (SUB-1)   TO W-COSTMTD-A (SUB-1-SAVE)
              ADD W-SALESYTD (SUB-1)  TO W-SALESYTD-A (SUB-1-SAVE)
              ADD W-COSTYTD (SUB-1)   TO W-COSTYTD-A (SUB-1-SAVE)
              ADD W-SALESLAST (SUB-1) TO W-SALESLAST-A (SUB-1-SAVE)
              ADD W-COSTLAST (SUB-1)  TO W-COSTLAST-A (SUB-1-SAVE)
              GO TO CTIA-999.
           IF W-CODENO (SUB-1) = 2010 OR = 2012 OR = 2018 OR = 2021
                  OR = 2032 OR = 2034 OR = 2037 OR = 2040 OR = 2041
                  OR = 2052 OR = 2054 OR = 2063 OR = 2090 OR = 2104
                  OR = 2106 OR = 2115 OR = 2117 OR = 2121 OR = 2123
                  OR = 2125 OR = 2128 OR = 2129 OR = 2132 OR = 2143
                  OR = 2144 OR = 2146 OR = 2152 OR = 2157 OR = 2160
                  OR = 2194 OR = 2195 OR = 2199
              MOVE 2 TO SUB-1-SAVE
              ADD W-ACCNO (SUB-1)     TO W-NO-IN-AREA (SUB-1-SAVE)
              ADD W-SALESMTD (SUB-1)  TO W-SALESMTD-A (SUB-1-SAVE)
              ADD W-COSTMTD (SUB-1)   TO W-COSTMTD-A (SUB-1-SAVE)
              ADD W-SALESYTD (SUB-1)  TO W-SALESYTD-A (SUB-1-SAVE)
              ADD W-COSTYTD (SUB-1)   TO W-COSTYTD-A (SUB-1-SAVE)
              ADD W-SALESLAST (SUB-1) TO W-SALESLAST-A (SUB-1-SAVE)
              ADD W-COSTLAST (SUB-1)  TO W-COSTLAST-A (SUB-1-SAVE)
              GO TO CTIA-999.
           IF W-CODENO (SUB-1) = 2016 OR = 2025 OR = 2039 OR = 2049
                  OR = 2091 OR = 2105 OR = 2135 OR = 2136 OR = 2137
              MOVE 3 TO SUB-1-SAVE
              ADD W-ACCNO (SUB-1)     TO W-NO-IN-AREA (SUB-1-SAVE)
              ADD W-SALESMTD (SUB-1)  TO W-SALESMTD-A (SUB-1-SAVE)
              ADD W-COSTMTD (SUB-1)   TO W-COSTMTD-A (SUB-1-SAVE)
              ADD W-SALESYTD (SUB-1)  TO W-SALESYTD-A (SUB-1-SAVE)
              ADD W-COSTYTD (SUB-1)   TO W-COSTYTD-A (SUB-1-SAVE)
              ADD W-SALESLAST (SUB-1) TO W-SALESLAST-A (SUB-1-SAVE)
              ADD W-COSTLAST (SUB-1)  TO W-COSTLAST-A (SUB-1-SAVE)
              GO TO CTIA-999.
           IF W-CODENO (SUB-1)= 2006 OR = 2019 OR = 2042 OR = 2093
                 OR = 2102 OR = 2114 OR = 2141
              MOVE 4 TO SUB-1-SAVE
              ADD W-ACCNO (SUB-1)     TO W-NO-IN-AREA (SUB-1-SAVE)
              ADD W-SALESMTD (SUB-1)  TO W-SALESMTD-A (SUB-1-SAVE)
              ADD W-COSTMTD (SUB-1)   TO W-COSTMTD-A (SUB-1-SAVE)
              ADD W-SALESYTD (SUB-1)  TO W-SALESYTD-A (SUB-1-SAVE)
              ADD W-COSTYTD (SUB-1)   TO W-COSTYTD-A (SUB-1-SAVE)
              ADD W-SALESLAST (SUB-1) TO W-SALESLAST-A (SUB-1-SAVE)
              ADD W-COSTLAST (SUB-1)  TO W-COSTLAST-A (SUB-1-SAVE)
              GO TO CTIA-999.
           IF W-CODENO (SUB-1)= 2008 OR = 2011 OR = 2022 OR = 2027
                 OR = 2047 OR = 2094 OR = 2100 OR = 2101 OR = 2119
              MOVE 5 TO SUB-1-SAVE
              ADD W-ACCNO (SUB-1)     TO W-NO-IN-AREA (SUB-1-SAVE)
              ADD W-SALESMTD (SUB-1)  TO W-SALESMTD-A (SUB-1-SAVE)
              ADD W-COSTMTD (SUB-1)   TO W-COSTMTD-A (SUB-1-SAVE)
              ADD W-SALESYTD (SUB-1)  TO W-SALESYTD-A (SUB-1-SAVE)
              ADD W-COSTYTD (SUB-1)   TO W-COSTYTD-A (SUB-1-SAVE)
              ADD W-SALESLAST (SUB-1) TO W-SALESLAST-A (SUB-1-SAVE)
              ADD W-COSTLAST (SUB-1)  TO W-COSTLAST-A (SUB-1-SAVE).
           CTIA-999.
              EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTORS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE
           MOVE Ws-Co-Name TO CO-Name.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           MOVE 2510 TO POS
           DISPLAY "ALLOCATING ACCOUNTS TO VARIOUS AREAS...." AT POS.
           MOVE 1   TO SUB-1
           MOVE " " TO DETAIL-LINE.
       END-010.
           MOVE W-CODENO (SUB-1)   TO D-POST
           MOVE W-ACCNO (SUB-1)    TO D-NO
           ADD W-ACCNO (SUB-1)     TO TOT-AREA-NO
           MOVE W-ADD (SUB-1)      TO D-NAME
           MOVE W-SALESMTD (SUB-1) TO D-SALES
           MOVE W-COSTMTD (SUB-1)  TO D-COST
           COMPUTE WS-MARGIN = W-SALESMTD (SUB-1) - W-COSTMTD (SUB-1)
           COMPUTE WS-PERC = (WS-MARGIN / W-COSTMTD (SUB-1)) * 100
           MOVE WS-MARGIN  TO D-MARGIN
           MOVE WS-PERC    TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.

           MOVE W-SALESYTD (SUB-1) TO D-SALES
           MOVE W-COSTYTD (SUB-1)  TO D-COST
           COMPUTE WS-MARGIN = W-SALESYTD (SUB-1) - W-COSTYTD (SUB-1)
           COMPUTE WS-PERC = (WS-MARGIN / W-COSTYTD (SUB-1)) * 100
           MOVE WS-MARGIN          TO D-MARGIN
           MOVE WS-PERC            TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.

           MOVE W-SALESLAST (SUB-1) TO D-SALES
           MOVE W-COSTLAST (SUB-1)  TO D-COST
           COMPUTE WS-MARGIN = W-SALESLAST (SUB-1) - W-COSTLAST (SUB-1)
           COMPUTE WS-PERC = (WS-MARGIN / W-COSTLAST (SUB-1)) * 100
           MOVE WS-MARGIN          TO D-MARGIN
           MOVE WS-PERC            TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           WRITE PRINT-REC.

           MOVE 0 TO WS-MARGIN
                     WS-PERC.
           ADD 4 TO LINE-CNT.
           IF LINE-CNT > 58
             PERFORM PRR-015.

           PERFORM CHECK-TOTAL-IN-AREA.

           ADD 1 TO SUB-1.
           IF W-CODENO (SUB-1) = 0
              MOVE 1 TO SUB-1-SAVE
              GO TO END-500.
           GO TO END-010.
           MOVE 1 TO SUB-1-SAVE.
       END-500.
           MOVE 2510 TO POS
           DISPLAY "ENDING OFF NOW............................" AT POS.
           IF SUB-1-SAVE = 1
               MOVE " TOTAL SALES:CENTRE MTD:" TO D-NAME.
           IF SUB-1-SAVE = 2
               MOVE " TOTAL SALES: NORTH MTD:" TO D-NAME.
           IF SUB-1-SAVE = 3
               MOVE " TOTAL SALES: SOUTH MTD:" TO D-NAME.
           IF SUB-1-SAVE = 4
               MOVE " TOTAL SALES:  WEST MTD:" TO D-NAME.
           IF SUB-1-SAVE = 5
               MOVE " TOTAL SALES:  EAST MTD:" TO D-NAME.
           MOVE W-NO-IN-AREA (SUB-1-SAVE) TO D-NO
           MOVE W-SALESMTD-A (SUB-1-SAVE) TO D-SALES
           MOVE W-COSTMTD-A (SUB-1-SAVE)  TO D-COST
           COMPUTE WS-MARGIN =
               W-SALESMTD-A (SUB-1-SAVE) - W-COSTMTD-A (SUB-1-SAVE)
           COMPUTE WS-PERC =
              (WS-MARGIN / W-COSTMTD-A (SUB-1-SAVE)) * 100
           MOVE WS-MARGIN  TO D-MARGIN
           MOVE WS-PERC    TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.
           MOVE "                    YTD:" TO D-NAME
           MOVE W-SALESYTD-A (SUB-1-SAVE)  TO D-SALES
           MOVE W-COSTYTD-A (SUB-1-SAVE)   TO D-COST
           COMPUTE WS-MARGIN =
              W-SALESYTD-A (SUB-1-SAVE) - W-COSTYTD-A (SUB-1-SAVE)
           COMPUTE WS-PERC =
              (WS-MARGIN / W-COSTYTD-A (SUB-1-SAVE)) * 100
           MOVE WS-MARGIN                  TO D-MARGIN
           MOVE WS-PERC                    TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.

           MOVE "                   LAST:" TO D-NAME
           MOVE W-SALESLAST-A (SUB-1-SAVE) TO D-SALES
           MOVE W-COSTLAST-A (SUB-1-SAVE)  TO D-COST
           COMPUTE WS-MARGIN =
              W-SALESLAST-A (SUB-1-SAVE) - W-COSTLAST-A (SUB-1-SAVE)
           COMPUTE WS-PERC =
              (WS-MARGIN / W-COSTLAST-A (SUB-1-SAVE)) * 100
           MOVE WS-MARGIN                  TO D-MARGIN
           MOVE WS-PERC                    TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.
           WRITE PRINT-REC.

           ADD 4 TO LINE-CNT
           IF LINE-CNT > 58
             PERFORM PRR-015.
           ADD 1 TO SUB-1-SAVE
           IF SUB-1-SAVE < 6
               GO TO END-500.
       END-800.
           MOVE "** GRAND TOTALS *** MTD:" TO D-NAME
           MOVE TOT-AREA-NO  TO D-NO
           MOVE TOT-SALESMTD TO D-SALES
           MOVE TOT-COSTMTD  TO D-COST
           COMPUTE WS-MARGIN = TOT-SALESMTD - TOT-COSTMTD
           COMPUTE WS-PERC = (WS-MARGIN / TOT-COSTMTD) * 100
           MOVE WS-MARGIN  TO D-MARGIN
           MOVE WS-PERC    TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.
           MOVE "                    YTD:" TO D-NAME
           MOVE TOT-SALESYTD TO D-SALES
           MOVE TOT-COSTYTD  TO D-COST
           COMPUTE WS-MARGIN = TOT-SALESYTD - TOT-COSTYTD
           COMPUTE WS-PERC = (WS-MARGIN / TOT-COSTYTD) * 100
           MOVE WS-MARGIN  TO D-MARGIN
           MOVE WS-PERC    TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.
           MOVE "                   LAST:" TO D-NAME
           MOVE TOT-SALESLAST              TO D-SALES
           MOVE TOT-COSTLAST               TO D-COST
           COMPUTE WS-MARGIN = TOT-SALESLAST - TOT-COSTLAST
           COMPUTE WS-PERC = (WS-MARGIN / TOT-COSTLAST) * 100
           MOVE WS-MARGIN                  TO D-MARGIN
           MOVE WS-PERC                    TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.

           ADD 4 TO LINE-CNT
           IF LINE-CNT > 58
             PERFORM PRR-015.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE DEBTOR-MASTER.
       END-900.
           EXIT PROGRAM.
       END-999.
           EXIT.
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      *
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
