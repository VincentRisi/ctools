        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrArSaRp.
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
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-ACCNO             PIC 9(3) VALUE 0.
       77  WS-MESS              PIC X VALUE " ".
       77  WS-PART-MESSAGE      PIC X(79) VALUE " ".
       77  WS-RANGE             PIC X VALUE " ".
       77  WS-SALESMAN          PIC X VALUE " ".
       77  WS-SALES-CODE        PIC XX VALUE " ".
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-SALESAMT          PIC S9(7)V99 VALUE 0.
       77  WS-COST              PIC S9(7)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(7)V99 VALUE 0.
       77  WS-PERC              PIC S99V99.
       77  WS-SALESAMT-YTD      PIC S9(7)V99 VALUE 0.
       77  WS-COST-YTD          PIC S9(7)V99 VALUE 0.
       77  WS-MARGIN-YTD        PIC S9(7)V99 VALUE 0.
       77  WS-PERC-YTD          PIC S999V99.
       77  WS-SALESAMT-LAST     PIC S9(7)V99 VALUE 0.
       77  WS-COST-LAST         PIC S9(7)V99 VALUE 0.
       77  WS-MARGIN-LAST       PIC S9(7)V99 VALUE 0.
       77  WS-PERC-LAST         PIC S999V99.
       77  TOT-SALESAMT         PIC S9(7)V99 VALUE 0.
       77  TOT-COST             PIC S9(7)V99 VALUE 0.
       77  TOT-MARGIN           PIC S9(7)V99 VALUE 0.
       77  TOT-PERC             PIC S999V99.
       77  TOT-SALESAMT-YTD     PIC S9(7)V99 VALUE 0.
       77  TOT-COST-YTD         PIC S9(7)V99 VALUE 0.
       77  TOT-MARGIN-YTD       PIC S9(7)V99 VALUE 0.
       77  TOT-PERC-YTD         PIC S999V99.
       77  TOT-SALESAMT-LAST    PIC S9(7)V99 VALUE 0.
       77  TOT-COST-LAST        PIC S9(7)V99 VALUE 0.
       77  TOT-MARGIN-LAST      PIC S9(7)V99 VALUE 0.
       77  TOT-PERC-LAST        PIC S999V99.
       77  WS-ACCNO-DISP        PIC Z(3)9.
       01  WS-DEBTORANALYSIS.
           03  WS-SALES-ANALYSIS PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(15).
           03  FILLER         PIC X(50) VALUE
           "DEBTORS SALES BY AREA CODE".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
       01  HEAD2.
           03  FILLER         PIC X(20) VALUE " ".
           03  FILLER         PIC X(26) VALUE ALL "*".
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(10) VALUE "SALESMAN:".
           03  H2-SALESMAN    PIC X(4) VALUE " ".
           03  FILLER         PIC X(12) VALUE "AREA CODE:".
           03  H1-AREA-CODE   PIC X.
       01  HEAD3.
           03  FILLER         PIC X(52) VALUE " ".
           03  FILLER         PIC X(33) VALUE
           "PERIOD/YEAR/L-YEAR TO DATE".
       01  HEAD4.
           03  FILLER         PIC X(35) VALUE "NUMBER  NAME".
           03  FILLER         PIC X(16) VALUE "CODE   BALANCE".
           03  FILLER         PIC X(14) VALUE "SALES AMT".
           03  FILLER         PIC X(10) VALUE "MARGIN".
           03  FILLER         PIC X(11) VALUE "%".
       01  HEAD5.
           03  FILLER         PIC X(65) VALUE " ".
           03  FILLER         PIC X(10) VALUE "SALESCODE:".
           03  H5-SALESCODE.
              05  H5-CODE1         PIC X.
              05  H5-CODE2.
                  07 H5-CODE2-1    PIC X.
                  07 H5-CODE3      PIC X.
       01  DETAIL-LINE.
           03  D-CATEGORY.
               05  D-NAME-CODE.
                  07  D-ACCOUNT  PIC X(9) VALUE " ".
                  07  D-NAME     PIC X(26) VALUE " ".
                  07  D-POST     PIC X(4).
               05  D-BALANCE  PIC Z(6)9.99-.
           03  D-SALESAMT     PIC Z(6)9.99-.
           03  D-MARGIN       PIC Z(6)9.99-.
           03  D-PERC         PIC Z99.99-.
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
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "** DEBTORS SALES REPORT BY AREA CODE **" AT POS
           MOVE 415 TO POS
           DISPLAY "***************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAcceptDr".
       CONTROL-010.
            MOVE 1610 TO POS
            DISPLAY
            " GAUTENG          NATAL           CAPE            OFS"
            AT POS
            MOVE 1710 TO POS
            DISPLAY
            "A = JHB         I = DURBAN      M = W.CAPE     K = BLOEM."
            AT POS
            MOVE 1810 TO POS
            DISPLAY
            "B = PTA         J = REST OF     N = E.CAPE     L = REST OF"
            AT POS
            MOVE 1910 TO POS
            DISPLAY
            "C = N TVL           NATAL       O = REST OF         OFS  "
            AT POS
            MOVE 2010 TO POS
            DISPLAY
            "D = E. RAND                         CAPE" AT POS
            MOVE 2110 TO POS
            DISPLAY "E = W. RAND" AT POS
            MOVE 2210 TO POS
            DISPLAY "F = W.TVL" AT POS
            MOVE 2310 TO POS
            DISPLAY "G = E. TVL" AT POS
            MOVE 2410 TO POS
            DISPLAY "H = S.TVL" AT POS
            MOVE 2515 TO POS
            DISPLAY
            "P = EXPORT SALES       Q = ACC'S - NO POSTAL CODE" AT POS
            MOVE 810 TO POS
            DISPLAY "Enter an AREA CODE between A & Q:" AT POS
            MOVE 854 TO POS
            DISPLAY "[ ]" AT POS
            MOVE 855 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE.

      *      ACCEPT WS-RANGE AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
            IF WS-RANGE = "A" OR = "B" OR = "C" OR = "D" OR = "E"
                     OR = "F" OR = "G" OR = "H" OR = "I" OR = "J"
                     OR = "K" OR = "L" OR = "M" OR = "N" OR = "O"
                     OR = "P" OR = "Q"
               GO TO CONTROL-011
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO CONTROL-010.
       CONTROL-011.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-012
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-012.
            MOVE 1010 TO POS
            DISPLAY "Print GROSS TOTALS only ; Y OR N" AT POS
            MOVE 1054 TO POS
            DISPLAY "[ ]" AT POS
            MOVE 1055 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TOTAL.

      *      ACCEPT WS-TOTAL AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
            IF WS-TOTAL NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-013
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
       CONTROL-013.
            MOVE 1210 TO POS
            DISPLAY "Enter a SALESMAN #, BLANK for ALL." AT POS
            MOVE 1310 TO POS
            DISPLAY "OR Enter 'Z' for Acc's with No Rep." AT POS
            MOVE 1254 TO POS
            DISPLAY "[ ]" AT POS
            MOVE 1255 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SALESMAN.

      *      ACCEPT WS-SALESMAN AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-012.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-014
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-013.
       CONTROL-014.
            MOVE 1510 TO POS
            DISPLAY "Enter a SALES-CODE, BLANK for ALL." AT POS
            MOVE 1554 TO POS
            DISPLAY "[  ]" AT POS
            MOVE 1555 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SALES-CODE.

      *      ACCEPT WS-SALES-CODE AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-013.
            MOVE WS-SALES-CODE TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-SALES-ANALYSIS
            DISPLAY WS-SALES-ANALYSIS AT POS.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-015
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-014.
       CONTROL-015.
           MOVE 2910 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
           AT POS.
           PERFORM GET-USER-PRINT-NAME.
           PERFORM OPEN-FILES
           OPEN OUTPUT PRINT-FILE.
       CONTROL-020.
            PERFORM PRINT-ROUTINE.
            PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE 0 TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE "DEBTOR RECORD BUSY ON START, 'ESC' TO SEE STATUS."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
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
               GO TO PRR-005.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           
           MOVE 2330 TO POS
           DISPLAY "DEBTOR ACCOUNT BEING READ:" AT POS
           ADD 27 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
              
           IF WS-SALESMAN = "Z"
            IF DR-SALESMAN = " "
               GO TO PRR-006.
           IF WS-SALESMAN NOT = " "
            IF DR-SALESMAN NOT = WS-SALESMAN
               GO TO PRR-005.
       PRR-006.
           IF WS-SALES-CODE NOT = "  "
            IF DR-SALES-ANALYSIS NOT = WS-SALES-ANALYSIS
               GO TO PRR-005.
           
           IF WS-RANGE = "A"
             IF DR-POST-CODE > 1999 AND < 2200
              GO TO PRR-010.
      
           IF WS-RANGE = "B"
             IF DR-POST-CODE > 0000 AND < 0200
              GO TO PRR-010.
      
           IF WS-RANGE = "C"
             IF DR-POST-CODE > 0399 AND < 1000
              GO TO PRR-010.
      
           IF WS-RANGE = "D"
             IF DR-POST-CODE > 1399 AND < 1700
              GO TO PRR-010.
      
           IF WS-RANGE = "E"
             IF DR-POST-CODE > 1699 AND < 1800
              GO TO PRR-010.
      
           IF WS-RANGE = "F"
             IF DR-POST-CODE > 0199 AND < 0400
              GO TO PRR-010.
           IF WS-RANGE = "F"
             IF DR-POST-CODE > 2499 AND < 2900
              GO TO PRR-010.
      
           IF WS-RANGE = "G"
             IF DR-POST-CODE > 0999 AND < 1400
              GO TO PRR-010.
           IF WS-RANGE = "G"
             IF DR-POST-CODE > 2199 AND < 2400
              GO TO PRR-010.
      
           IF WS-RANGE = "H"
             IF DR-POST-CODE > 1799 AND < 2000
              GO TO PRR-010.
           IF WS-RANGE = "H"
             IF DR-POST-CODE > 2399 AND < 2500
              GO TO PRR-010.
      
           IF WS-RANGE = "I"
             IF DR-POST-CODE > 3999 AND < 4100
              GO TO PRR-010.
      
           IF WS-RANGE = "J"
             IF DR-POST-CODE > 2899 AND < 4000
              GO TO PRR-010.
           IF WS-RANGE = "J"
             IF DR-POST-CODE > 4099 AND < 4900
              GO TO PRR-010.
      
           IF WS-RANGE = "K"
             IF DR-POST-CODE > 9299 AND < 9400
              GO TO PRR-010.
      
           IF WS-RANGE = "L"
             IF DR-POST-CODE > 9399 AND < 10000
              GO TO PRR-010.
      
           IF WS-RANGE = "M"
             IF DR-POST-CODE > 6699 AND < 6900
              GO TO PRR-010.
           IF WS-RANGE = "M"
             IF DR-POST-CODE > 7099 AND < 8100
              GO TO PRR-010.
      
           IF WS-RANGE = "N"
             IF DR-POST-CODE > 4899 AND < 6700
              GO TO PRR-010.
      
           IF WS-RANGE = "O"
             IF DR-POST-CODE > 6899 AND < 7100
              GO TO PRR-010.
           IF WS-RANGE = "O"
             IF DR-POST-CODE > 8099 AND < 9000
              GO TO PRR-010.

           IF WS-RANGE = "P"
             IF DR-POST-CODE > 8900 AND < 9300
              GO TO PRR-010.
           IF WS-RANGE = "P"
            IF DR-POST-CODE = 0
             IF DR-GSTNO = "EXPORT"
              GO TO PRR-010.
           IF WS-RANGE = "P"
            IF DR-POST-CODE = 0
             IF DR-SALES-ANALYSIS = 6 OR = 52
                 GO TO PRR-010.

           IF WS-RANGE = "Q"
            IF DR-POST-CODE = 0
             IF DR-SALES-ANALYSIS NOT = 6 AND NOT = 52
              GO TO PRR-010.
           IF WS-RANGE = "Q"
            IF DR-POST-CODE = 0
             IF DR-GSTNO NOT = "EXPORT"
              GO TO PRR-010.

           GO TO PRR-005.
       PRR-010.
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
                GO TO PRR-012.
           IF LINE-CNT > 21
              MOVE 3010 TO POS
              DISPLAY "Press ANY key for NEXT-PAGE OR <Finish> to EXIT"
              AT POS
              ADD 60 TO POS
              ACCEPT WS-ACCEPT AT POS
            ELSE
              GO TO PRR-020.
           IF W-ESCAPE-KEY = 3
              GO TO END-950
           ELSE
              PERFORM CLEAR-PART-SCREEN
              GO TO PRR-015.
       PRR-012.
            IF LINE-CNT < 60
               GO TO PRR-020.
       PRR-015.
           IF WS-SALESMAN = " "
              MOVE "ALL"       TO H2-SALESMAN
           ELSE
              MOVE WS-SALESMAN TO H2-SALESMAN.
              
           IF WS-SALES-CODE = " "
              MOVE "ALL"             TO H5-SALESCODE
           ELSE
              MOVE WS-SALES-ANALYSIS TO H5-CODE2.
              
           ADD 1         TO PAGE-CNT
           MOVE WS-RANGE TO H1-AREA-CODE
           MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
            IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF WS-PRINTER = "[VID]" OR = "[Vid]"
                PERFORM CLEAR-PART-SCREEN.
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
            IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD5 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD4 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC.
           IF WS-PRINTER = "[VID]" OR = "[Vid]"
               MOVE 8 TO LINE-CNT
           ELSE
               MOVE 7 TO LINE-CNT.
       PRR-020.
           ADD DR-SALES-PTD TO WS-SALESAMT
                               TOT-SALESAMT.
           ADD DR-SALES-YTD TO WS-SALESAMT-YTD
                               TOT-SALESAMT-YTD.
           ADD DR-SALES-LAST TO WS-SALESAMT-LAST
                               TOT-SALESAMT-LAST.
           ADD DR-COST-PTD  TO WS-COST
                               TOT-COST.
           ADD DR-COST-YTD  TO WS-COST-YTD
                               TOT-COST-YTD.
           ADD DR-COST-LAST  TO WS-COST-LAST
                               TOT-COST-LAST.
           ADD 1 TO WS-ACCNO.
           IF WS-TOTAL = "Y"
                GO TO PRR-005.
       PRR-025.
           MOVE DR-ACCOUNT-NUMBER TO D-ACCOUNT
           MOVE DR-POST-CODE      TO D-POST
           MOVE DR-BALANCE        TO D-BALANCE
           MOVE WS-SALESAMT       TO D-SALESAMT
           COMPUTE WS-MARGIN = WS-SALESAMT - WS-COST
           MOVE WS-MARGIN         TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / WS-COST * 100
           MOVE WS-PERC           TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE DR-NAME           TO D-NAME-CODE
           MOVE WS-SALESAMT-YTD   TO D-SALESAMT
           COMPUTE WS-MARGIN-YTD = WS-SALESAMT-YTD - WS-COST-YTD
           MOVE WS-MARGIN-YTD     TO D-MARGIN
           COMPUTE WS-PERC-YTD ROUNDED = WS-MARGIN-YTD / 
                   WS-COST-YTD * 100
           MOVE WS-PERC-YTD       TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE WS-SALESAMT-LAST   TO D-SALESAMT
           COMPUTE WS-MARGIN-LAST = WS-SALESAMT-LAST - WS-COST-LAST
           MOVE WS-MARGIN-LAST     TO D-MARGIN
           COMPUTE WS-PERC-LAST ROUNDED = WS-MARGIN-LAST / 
                   WS-COST-LAST * 100
           MOVE WS-PERC-LAST       TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           WRITE PRINT-REC AFTER 1
           ADD 4 TO LINE-CNT.
           MOVE 0 TO WS-SALESAMT
                     WS-COST
                     WS-MARGIN
                     WS-PERC
                     WS-SALESAMT-YTD
                     WS-COST-YTD
                     WS-MARGIN-YTD
                     WS-PERC-YTD
                     WS-SALESAMT-LAST
                     WS-COST-LAST
                     WS-MARGIN-LAST
                     WS-PERC-LAST.
           GO TO PRR-005.
       PRR-999.
           EXIT.
      *
       CLEAR-PART-SCREEN SECTION.
       CPS-005.
           MOVE 0301 TO POS
           DISPLAY WS-PART-MESSAGE AT POS
           MOVE 0 TO LINE-CNT.
       CPS-010.
           ADD 1 TO LINE-CNT
           ADD 80 TO POS
           DISPLAY WS-PART-MESSAGE AT POS.
           IF LINE-CNT < 31
              GO TO CPS-010.
           MOVE 0280 TO POS
           DISPLAY WS-MESS AT POS.
       CPS-999.
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
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
              GO TO END-010.
            IF LINE-CNT > 21
              MOVE 3010 TO POS
              DISPLAY "Press ANY key for NEXT-PAGE OR 'END' to EXIT"
              AT POS
              ADD 60 TO POS
              ACCEPT WS-ACCEPT AT POS
            ELSE
              GO TO END-020.
           IF W-ESCAPE-KEY = 3
              GO TO END-900
           ELSE
              PERFORM CLEAR-PART-SCREEN
              PERFORM PRR-015
              GO TO END-050.
       END-010.
           IF LINE-CNT > 56
              PERFORM PRR-015.
       END-020.
           MOVE "     TOTAL No OF ACCOUNTS:" TO D-NAME
           MOVE WS-ACCNO                     TO WS-ACCNO-DISP
           MOVE WS-ACCNO-DISP                TO D-POST
           MOVE TOT-SALESAMT                 TO D-SALESAMT
           COMPUTE TOT-MARGIN = TOT-SALESAMT - TOT-COST
           MOVE TOT-MARGIN                   TO D-MARGIN
           COMPUTE TOT-PERC ROUNDED = (TOT-MARGIN / TOT-COST) * 100
           MOVE TOT-PERC     TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.

           MOVE TOT-SALESAMT-YTD TO D-SALESAMT
           COMPUTE TOT-MARGIN-YTD = TOT-SALESAMT-YTD - TOT-COST-YTD
           MOVE TOT-MARGIN-YTD   TO D-MARGIN
           COMPUTE TOT-PERC-YTD ROUNDED = (TOT-MARGIN-YTD / 
                   TOT-COST-YTD) * 100
           MOVE TOT-PERC-YTD     TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.

           MOVE TOT-SALESAMT-LAST TO D-SALESAMT
           COMPUTE TOT-MARGIN-LAST = TOT-SALESAMT-LAST - TOT-COST-LAST
           MOVE TOT-MARGIN-LAST   TO D-MARGIN
           COMPUTE TOT-PERC-LAST ROUNDED = (TOT-MARGIN-LAST / 
                   TOT-COST-LAST) * 100
           MOVE TOT-PERC-LAST     TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           ADD 5 TO LINE-CNT.

           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
              GO TO END-040.
            IF LINE-CNT > 21
              MOVE 3010 TO POS
              DISPLAY "Press ANY key for NEXT-PAGE OR 'END' to EXIT"
              AT POS
              ADD 60 TO POS
              ACCEPT WS-ACCEPT AT POS
            ELSE
              GO TO END-050.
           IF W-ESCAPE-KEY = 3
              GO TO END-900
           ELSE
              PERFORM CLEAR-PART-SCREEN
              PERFORM PRR-015
              GO TO END-050.
       END-040.
           IF LINE-CNT > 56
              PERFORM PRR-015.
       END-050.
           MOVE " " TO PRINT-REC
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
       END-600.
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
              PERFORM GET-USER-MAIL-NAME
              PERFORM GET-REPORT-Y2K-DATE
              PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE DEBTOR-MASTER.
       END-900.
              MOVE 3010 TO POS
              DISPLAY "Press ANY key To EXIT." AT POS
              ADD 60 TO POS
              ACCEPT WS-ACCEPT AT POS.
       END-950.
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
