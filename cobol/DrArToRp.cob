        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrArToRp.
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
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-TOTAL             PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-RANGE             PIC X VALUE " ".
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-ACC-TOT           PIC 9(5) VALUE 0.
       77  WS-MARGIN            PIC S9(7)V99 VALUE 0.
       77  WS-PERC              PIC S999V99.
       77  WS-SALE-PTD-TOT      PIC S9(8)V99 VALUE 0.
       77  WS-COST-PTD-TOT      PIC S9(8)V99 VALUE 0.
       77  WS-SALE-YTD-TOT      PIC S9(8)V99 VALUE 0.
       77  WS-COST-YTD-TOT      PIC S9(8)V99 VALUE 0.
       77  WS-SALE-LAST-TOT     PIC S9(8)V99 VALUE 0.
       77  WS-COST-LAST-TOT     PIC S9(8)V99 VALUE 0.
       77  WS-GROSSSALES        PIC S9(8)V99 VALUE 0.
       77  WS-GROSSNO           PIC S9(5) VALUE 0.
       77  WS-GROSSSALES-LAST   PIC S9(8)V99 VALUE 0.
       77  WS-GROSSNO-LAST      PIC S9(5) VALUE 0.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  AREA-FIELDS.
           03  AREA-LINE OCCURS 20.
               05  WS-NO            PIC 9(4).
               05  WS-SALES-PTD     PIC S9(8)V99.
               05  WS-COST-PTD      PIC S9(8)V99.
               05  WS-SALES-YTD     PIC S9(8)V99.
               05  WS-COST-YTD      PIC S9(8)V99.
               05  WS-SALES-LAST    PIC S9(8)V99.
               05  WS-COST-LAST     PIC S9(8)V99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(89) VALUE
           "D E B T O R S   S A L E S   B Y   A R E A   C O D E".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(51) VALUE ALL "*".
           03  FILLER         PIC X(52) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(65) VALUE " ".
           03  FILLER         PIC X(60) VALUE
            "PERIOD / YEAR / LAST YEAR TO DATE".
       01  HEAD4.
           03  FILLER         PIC X(10) VALUE " ".
           03  FILLER         PIC X(50) VALUE
           "CODE      ACC'S    % OF ACC'S    % OF SALES".
           03  FILLER         PIC X(50) VALUE
           "SALES         COSTS       MARGIN       %".
           03  FILLER         PIC X(22) VALUE " ".
       01  DETAIL-LINE.
           03  D-TOTAL        PIC X(10) VALUE " ".
           03  FILLER         PIC X(2) VALUE " ".
           03  H1-AREA-CODE   PIC X.
           03  FILLER         PIC X(6) VALUE " ".
           03  H1-NO          PIC Z(4)9.
           03  FILLER         PIC X(8) VALUE " ".
           03  H1-NOPERC      PIC Z99.99-.
           03  FILLER         PIC X(7) VALUE " ".
           03  H1-SALEPERC    PIC Z99.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-SALES        PIC Z(6)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-COST         PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-MARGIN       PIC Z(6)9.99-.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-PERC         PIC Z99.99-.
           03  FILLER         PIC X(31) VALUE " ".
       01  TVL-LINE.
           03  FILLER         PIC X(44) VALUE
           "TVL   : (A)JHB=2000-2199; (B)PTA=0001-0199;".
           03  FILLER         PIC X(20) VALUE
            "(C)N.TVL=0400-0999,".
           03  FILLER         PIC X(68) VALUE
           "(D)E.RAND=1400-1699; (E)W.RAND=1700-1799".
       01  TVL2-LINE.
           03  FILLER         PIC X(38) VALUE
           "      : (F)W.TVL=0200-0399,2500-2899;".
           03  FILLER         PIC X(30) VALUE
            "(G)E.TVL=1000-1399,2200-2399;".
           03  FILLER         PIC X(64) VALUE
           "(H)S.TVL=1800-1999,2400-2499".
       01  NATAL-LINE.
           03  FILLER         PIC X(56) VALUE
           "NATAL : (I)DURBAN=4000-4099; (J)REST OF NATAL=2900-3999,".
           03  FILLER         PIC X(76) VALUE
           "4100-4899".
       01  OFS-LINE.
           03  FILLER         PIC X(132) VALUE
           "OFS   : (K)BLOEM.=9300-9399; (L)REST OF OFS=9400-9999".
       01  CAPE-LINE.
           03  FILLER         PIC X(39) VALUE
           "CAPE  : (M)W.CAPE=6700-6899,7100-8099;".
           03  FILLER         PIC X(93) VALUE
           "(N)E.CAPE=4900-6699; (O)REST OF CAPE=6900-7099,8100-8999".
       01  EXPORT-LINE.
           03  FILLER         PIC X(132) VALUE
           "EXPORT: (P)ALL FOREIGN COUNTRIES".
       01  OTHER-LINE.
           03  FILLER         PIC X(132) VALUE
           "OTHER : (Q)OTHER ACC'S - NO POSTAL CODE".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONT-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "** DEBTORS SALES REPORT BY AREA CODE - TOTALS **"
                 AT POS
           MOVE 415 TO POS
           DISPLAY "************************************************"
                 AT POS.
       CONT-003.
           Copy "PrinterAcceptDr".
       CONT-010.
           PERFORM OPEN-FILES
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE
           MOVE 2910 TO POS
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
           AT POS.
       CONT-020.
            PERFORM ADD-GROSS-FIGURES
            PERFORM PRINT-ROUTINE
            PERFORM END-OFF.
      *
       ADD-GROSS-FIGURES SECTION.
       AGF-000.
           MOVE 0 TO WS-GROSSSALES
                     WS-GROSSNO.
           MOVE 0 TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE 88 TO WS-DEBTOR-ST1
              GO TO AGF-900.
           MOVE 2410 TO POS
           DISPLAY "Debtors Being Read for Gross Totals.  " AT POS.
       AGF-005.
           READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
              MOVE 0 TO WS-DEBTOR-ST1
              GO TO AGF-900.
           IF WS-DEBTOR-ST1 NOT = 0
              Move "DEBTOR RECORD BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO AGF-005.
       AGF-020.
           ADD DR-SALES-YTD TO WS-GROSSSALES
           ADD 1            TO WS-GROSSNO
           GO TO AGF-005.
       AGF-900.
           CLOSE DEBTOR-MASTER
           PERFORM OPEN-000.
       AGF-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE 0 TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE 88 TO WS-DEBTOR-ST1
              GO TO PRR-999.
           MOVE 2410 TO POS
           DISPLAY "Debtors Being Sorted by Postal Code.  " AT POS.
       PRR-005.
           READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
              MOVE 0 TO WS-DEBTOR-ST1
              GO TO PRR-030.
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

           IF DR-POST-CODE > 1999 AND < 2200
              MOVE 1 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 0000 AND < 0200
              MOVE 2 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 0399 AND < 1000
              MOVE 3 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 1399 AND < 1700
              MOVE 4 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 1699 AND < 1800
              MOVE 5 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 0199 AND < 0400
              MOVE 6 TO SUB-1
              GO TO PRR-010.
           IF DR-POST-CODE > 2499 AND < 2900
              MOVE 6 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 0999 AND < 1400
              MOVE 7 TO SUB-1
              GO TO PRR-010.
           IF DR-POST-CODE > 2199 AND < 2400
              MOVE 7 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 1799 AND < 2000
              MOVE 8 TO SUB-1
              GO TO PRR-010.
           IF DR-POST-CODE > 2399 AND < 2500
              MOVE 8 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 3999 AND < 4100
              MOVE 9 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 2899 AND < 4000
              MOVE 10 TO SUB-1
              GO TO PRR-010.
           IF DR-POST-CODE > 4099 AND < 4900
              MOVE 10 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 9299 AND < 9400
              MOVE 11 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 9399 AND < 10000
              MOVE 12 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 6699 AND < 6900
              MOVE 13 TO SUB-1
              GO TO PRR-010.
           IF DR-POST-CODE > 7099 AND < 8100
              MOVE 13 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 4899 AND < 6700
              MOVE 14 TO SUB-1
              GO TO PRR-010.

           IF DR-POST-CODE > 6899 AND < 7100
              MOVE 15 TO SUB-1
              GO TO PRR-010.
           IF DR-POST-CODE > 8099 AND < 9000
              MOVE 15 TO SUB-1
              GO TO PRR-010.

           IF DR-GSTNO = "EXPORT       "
              MOVE 16 TO SUB-1
              GO TO PRR-010.
           IF DR-SALES-ANALYSIS = 6 OR = 4
              MOVE 16 TO SUB-1
              GO TO PRR-010.

           MOVE 17 TO SUB-1.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-020.
       PRR-015.
            ADD 1         TO PAGE-CNT.
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
           MOVE 2510 TO POS
           DISPLAY "Debtor File Being Read:" AT POS
           ADD 25 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
       
           ADD DR-SALES-PTD   TO WS-SALES-PTD (SUB-1)
           ADD DR-SALES-YTD   TO WS-SALES-YTD (SUB-1)
           ADD DR-SALES-LAST  TO WS-SALES-LAST (SUB-1)
           ADD DR-COST-PTD    TO WS-COST-PTD (SUB-1)
           ADD DR-COST-YTD    TO WS-COST-YTD (SUB-1)
           ADD DR-COST-LAST   TO WS-COST-LAST (SUB-1)
           ADD 1              TO WS-NO (SUB-1)

           GO TO PRR-005.
       PRR-030.
           MOVE 0 TO SUB-1.
           MOVE 2510 TO POS
           DISPLAY "Printing Area Totals for Accounts Read.  " AT POS.
       PRR-035.
           ADD 1 TO SUB-1.
           IF SUB-1 = 18
              GO TO PRR-999.

           IF SUB-1 = 1
                MOVE "A" TO H1-AREA-CODE.
           IF SUB-1 = 2
                MOVE "B" TO H1-AREA-CODE.
           IF SUB-1 = 3
                MOVE "C" TO H1-AREA-CODE.
           IF SUB-1 = 4
                MOVE "D" TO H1-AREA-CODE.
           IF SUB-1 = 5
                MOVE "E" TO H1-AREA-CODE.
           IF SUB-1 = 6
                MOVE "F" TO H1-AREA-CODE.
           IF SUB-1 = 7
                MOVE "G" TO H1-AREA-CODE.
           IF SUB-1 = 8
                MOVE "H" TO H1-AREA-CODE.
           IF SUB-1 = 9
                MOVE "I" TO H1-AREA-CODE.
           IF SUB-1 = 10
                MOVE "J" TO H1-AREA-CODE.
           IF SUB-1 = 11
                MOVE "K" TO H1-AREA-CODE.
           IF SUB-1 = 12
                MOVE "L" TO H1-AREA-CODE.
           IF SUB-1 = 13
                MOVE "M" TO H1-AREA-CODE.
           IF SUB-1 = 14
                MOVE "N" TO H1-AREA-CODE.
           IF SUB-1 = 15
                MOVE "O" TO H1-AREA-CODE.
           IF SUB-1 = 16
                MOVE "P" TO H1-AREA-CODE.
           IF SUB-1 = 17
                MOVE "Q" TO H1-AREA-CODE.

           MOVE WS-NO (SUB-1)         TO H1-NO.
           MOVE WS-SALES-PTD (SUB-1)  TO D-SALES.
           MOVE WS-COST-PTD  (SUB-1)  TO D-COST.
           COMPUTE WS-MARGIN =
                WS-SALES-PTD (SUB-1) - WS-COST-PTD (SUB-1).
           MOVE WS-MARGIN             TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / WS-COST-PTD (SUB-1)) * 100.
           MOVE WS-PERC               TO D-PERC.

           MOVE 0 TO WS-PERC.
           COMPUTE WS-PERC ROUNDED =
            (WS-SALES-YTD (SUB-1) / WS-GROSSSALES) * 100.
           MOVE WS-PERC TO H1-SALEPERC.
           MOVE 0 TO WS-PERC.
           COMPUTE WS-PERC ROUNDED = (WS-NO (SUB-1) / WS-GROSSNO) * 100.
           MOVE WS-PERC TO H1-NOPERC.

           WRITE PRINT-REC FROM DETAIL-LINE.
           ADD WS-SALES-PTD (SUB-1) TO WS-SALE-PTD-TOT.
           ADD WS-COST-PTD (SUB-1)  TO WS-COST-PTD-TOT.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.

           MOVE WS-SALES-YTD (SUB-1)  TO D-SALES.
           MOVE WS-COST-YTD  (SUB-1)  TO D-COST.
           COMPUTE WS-MARGIN =
                WS-SALES-YTD (SUB-1) - WS-COST-YTD (SUB-1).
           MOVE WS-MARGIN             TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
              (WS-MARGIN / WS-COST-YTD (SUB-1)) * 100.
           MOVE WS-PERC               TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE.

           MOVE WS-SALES-LAST (SUB-1)  TO D-SALES.
           MOVE WS-COST-LAST  (SUB-1)  TO D-COST.
           COMPUTE WS-MARGIN =
                WS-SALES-LAST (SUB-1) - WS-COST-LAST (SUB-1).
           MOVE WS-MARGIN             TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
              (WS-MARGIN / WS-COST-LAST (SUB-1)) * 100.
           MOVE WS-PERC               TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE.
           ADD WS-SALES-YTD (SUB-1)  TO WS-SALE-YTD-TOT.
           ADD WS-COST-YTD (SUB-1)   TO WS-COST-YTD-TOT.
           ADD WS-SALES-LAST (SUB-1) TO WS-SALE-LAST-TOT.
           ADD WS-COST-LAST (SUB-1)  TO WS-COST-LAST-TOT.
           ADD WS-NO (SUB-1)         TO WS-ACC-TOT.

           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           WRITE PRINT-REC.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.
           ADD 4 TO LINE-CNT.
           IF LINE-CNT > 60
                PERFORM PRR-015.
           GO TO PRR-035.
       PRR-999.
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
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 54
              PERFORM PRR-015.
       END-010.
           MOVE "*TOTALS*"      TO D-TOTAL
           MOVE WS-ACC-TOT      TO H1-NO
           MOVE WS-SALE-PTD-TOT TO D-SALES
           MOVE WS-COST-PTD-TOT TO D-COST
           COMPUTE WS-MARGIN = WS-SALE-PTD-TOT - WS-COST-PTD-TOT
           MOVE WS-MARGIN       TO D-MARGIN
           COMPUTE WS-PERC ROUNDED =
                 (WS-MARGIN / WS-COST-PTD-TOT) * 100
           MOVE WS-PERC         TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC DETAIL-LINE
           WRITE PRINT-REC.

           MOVE WS-SALE-YTD-TOT TO D-SALES
           MOVE WS-COST-YTD-TOT TO D-COST
           COMPUTE WS-MARGIN = WS-SALE-YTD-TOT - WS-COST-YTD-TOT
           MOVE WS-MARGIN       TO D-MARGIN
           COMPUTE WS-PERC ROUNDED =
                 (WS-MARGIN / WS-COST-YTD-TOT) * 100
           MOVE WS-PERC         TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE WS-SALE-LAST-TOT TO D-SALES
           MOVE WS-COST-LAST-TOT TO D-COST
           COMPUTE WS-MARGIN = WS-SALE-LAST-TOT - WS-COST-LAST-TOT
           MOVE WS-MARGIN       TO D-MARGIN
           COMPUTE WS-PERC ROUNDED =
                 (WS-MARGIN / WS-COST-LAST-TOT) * 100
           MOVE WS-PERC         TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.

           IF LINE-CNT > 56
              PERFORM PRR-015.
           WRITE PRINT-REC FROM TVL-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM TVL2-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM NATAL-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM OFS-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM CAPE-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM EXPORT-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM OTHER-LINE
           MOVE " " TO PRINT-REC.
           
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE
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
