        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrCorpRp.
        AUTHOR.     STEVE CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        REPOSITORY. 
           FUNCTION ALL INTRINSIC.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectSlSbRep".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdSbRep.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(80).
      *
       WORKING-STORAGE SECTION.
       77  WS-MESSAGE-PART      PIC X(79) VALUE " ".
       77  WS-MESS              PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-ACCEPT            PIC X(10) VALUE " ".
       77  WS-RANGE             PIC X(3) VALUE " ".
       77  WS-CORPORATE-GROUP   PIC 9(3) VALUE 0.
       77  WS-THIS-YEAR         PIC X VALUE " ".
       77  WS-TOTALS-ONLY       PIC X VALUE " ".
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-MARGIN            PIC S9(7)V99 VALUE 0.
       77  WS-PERC              PIC S999V99.
       77  TOT-SALESAMT-PTD     PIC S9(7)V99 VALUE 0.
       77  TOT-SALESAMT-YTD     PIC S9(7)V99 VALUE 0.
       77  TOT-SALESAMT-LAST    PIC S9(7)V99 VALUE 0.
       77  TOT-COST-PTD         PIC S9(7)V99 VALUE 0.
       77  TOT-COST-YTD         PIC S9(7)V99 VALUE 0.
       77  TOT-COST-LAST        PIC S9(7)V99 VALUE 0.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-SBREP-STATUS.
           03  WS-SBREP-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(6) VALUE " ".
           03  FILLER         PIC X(35) VALUE
           "SALESMAN CORPORATE ACCOUNT ANALYSIS".
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
       01  HEAD2.
           03  FILLER         PIC X(23) VALUE " ".
           03  FILLER         PIC X(35) VALUE ALL "*".
           03  FILLER         PIC X(2) VALUE " ".
           03  FILLER         PIC X(15) VALUE "CORPORATE CODE:".
           03  H1-CORP-GROUP  PIC Z(2)9.
       01  HEAD2-1.
           03  FILLER         PIC X(62) VALUE " ".
           03  H2-SOLD-BY     PIC X(16).
       01  HEAD3.
           03  FILLER         PIC X(43) VALUE " ".
           03  FILLER         PIC X(33) VALUE
           "PERIOD / YEAR / LAST YEAR TO DATE".
       01  HEAD4.
           03  FILLER         PIC X(23) VALUE "ACCOUNT NO & NAME".
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(10) VALUE "BALANCE".
           03  FILLER         PIC X(4) VALUE " ".
           03  FILLER         PIC X(10) VALUE "SALES".
           03  FILLER         PIC X(10) VALUE "   COST".
           03  FILLER         PIC X(14) VALUE "   MARGIN".
           03  FILLER         PIC X(3) VALUE "%".
       01  DETAIL-LINE.
           03  D-CATEGORY.
               05  D-ACCOUNT  PIC X(9) VALUE " ".
               05  FILLER     PIC X(5) VALUE " ".
               05  D-BR1      PIC X VALUE " ".
               05  D-SMAN     PIC X VALUE " ".
               05  D-BR2      PIC X VALUE " ".
               05  FILLER     PIC X(6) VALUE " ".
               05  D-BALANCE  PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SALESAMT     PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-COST         PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-MARGIN       PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PERC         PIC Z(2)9.99-.
       01  TOTAL-LINE.
           03 TOT-DESC        PIC X(28).
           03 TOT-DATE        PIC X(10).
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
            PERFORM CLEAR-SCREEN.
            MOVE 315 TO POS.
            DISPLAY "** DEBTOR CORPORATE A/C ANALYSIS **" AT POS
            MOVE 415 TO POS
            DISPLAY "***********************************" AT POS.
       CONTROL-003.
           Copy "PrinterAcceptDr".
            PERFORM OPEN-FILES.
       CONTROL-010.
            MOVE 1020 TO POS.
            DISPLAY "Enter The Corporate CODE:[   ]" AT POS.
            MOVE 1046 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 3         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 45        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE.

      *      ACCEPT WS-RANGE AT POS.
            MOVE WS-RANGE TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-CORPORATE-GROUP
                                 H1-CORP-GROUP
                                 F-EDNAMEFIELDKITQTY.
            DISPLAY F-EDNAMEFIELDKITQTY AT POS.
            IF WS-CORPORATE-GROUP NOT > 0
               MOVE 
          "PLEASE ENTER A VALID CORPORATE GROUP > ZERO, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CONTROL-010.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-015
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-015.
            MOVE 1510 TO POS.
            DISPLAY
            "Print ONLY from a Specific date created, Y Or N : [ ]"
             AT POS
            ADD 51 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 60        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-THIS-YEAR.

      *      ACCEPT WS-THIS-YEAR AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
            IF WS-THIS-YEAR NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-020
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
            IF WS-THIS-YEAR = "N"
               GO TO CONTROL-030.
            MOVE 1710 TO POS.
            DISPLAY
            "The Account CREATE Date from which to Print: [          ]"
            AT POS
            MOVE 1756 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

      *      ACCEPT WS-ACCEPT AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
            MOVE WS-ACCEPT TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO CONTROL-020.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE
            MOVE 1756 TO POS
            DISPLAY DISPLAY-DATE AT POS
            MOVE "ACCOUNTS ONLY PRINTED FROM:" TO TOT-DESC.
            MOVE DISPLAY-DATE                  TO TOT-DATE.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-AGE-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               MOVE "DATE ENTRY NOT CORRECT, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO CONTROL-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-030
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-030.
            MOVE 1920 TO POS
            DISPLAY "Print TOTALS Only Y/N  :[ ]" AT POS
            MOVE 1945 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 44        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TOTALS-ONLY.

      *      ACCEPT WS-TOTALS-ONLY AT POS.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
            IF WS-TOTALS-ONLY NOT = "Y" AND NOT = "N"
               GO TO CONTROL-030.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-040
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-030.
       CONTROL-040.
            MOVE " " TO WS-ACCEPT.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            MOVE 2510 TO POS
            DISPLAY "The Report Is Being Compiled, Please Be Patient."
            AT POS.
       CONTROL-045.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       READ-SBREP SECTION.
       RSB-020.
           MOVE WS-RANGE TO SBREP-REP.
           START SBREP-MASTER KEY NOT < SBREP-KEY
             INVALID KEY NEXT SENTENCE.
       RSB-030.
           READ SBREP-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-SBREP-ST1 = 23 OR 35 OR 49
              MOVE "  " TO H2-Sold-By
              GO TO RSB-040.
           MOVE SBREP-REPNAME TO H2-Sold-By.
       RSB-040.
           CLOSE SBREP-MASTER.
       RSB-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE WS-CORPORATE-GROUP TO DR-CORPORATE-GROUP.
           START DEBTOR-MASTER KEY NOT < DR-CORPORATE-GROUP
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE
              "BAD START USING THIS GROUP NUMBER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DEBTOR-ST1
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

           IF DR-CORPORATE-GROUP NOT = WS-CORPORATE-GROUP
              GO TO PRR-999.
           IF WS-THIS-YEAR = "Y"
            IF DR-DATE-CREATED < WS-AGE-DATE
              GO TO PRR-005.
       PRR-010.
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
               GO TO PRR-012.
           IF WS-TOTAlS-ONLY = "Y"
               GO TO PRR-012.
           IF LINE-CNT > 23
             MOVE 3010 TO POS
              DISPLAY "Press ANY key for NEXT-PAGE OR 'END' to EXIT"
              AT POS
              ADD 60 TO POS
              ACCEPT WS-ACCEPT AT POS
            ELSE
              GO TO PRR-020.
           IF W-ESCAPE-KEY = 3
              GO TO END-900
           ELSE
      *        PERFORM CLEAR-SCREEN-PART
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
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
            MOVE " " TO PRINT-REC
           IF WS-PRINTER = "[VID]" OR = "[Vid]"
               WRITE PRINT-REC FROM HEAD1 AFTER 1
            ELSE
               WRITE PRINT-REC FROM HEAD1 AFTER 1.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD4 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC AFTER 1
           IF WS-PRINTER = "[VID]" OR = "[Vid]"
               MOVE 9 TO LINE-CNT
           ELSE
               MOVE 7 TO LINE-CNT.
       PRR-020.
           ADD DR-SALES-PTD  TO TOT-SALESAMT-PTD
           ADD DR-SALES-YTD  TO TOT-SALESAMT-YTD
           ADD DR-SALES-LAST TO TOT-SALESAMT-LAST
           ADD DR-COST-PTD   TO TOT-COST-PTD
           ADD DR-COST-YTD   TO TOT-COST-YTD
           ADD DR-COST-LAST  TO TOT-COST-LAST.
       PRR-025.
           MOVE DR-ACCOUNT-NUMBER TO D-ACCOUNT
           MOVE "("               TO D-BR1
           MOVE DR-SALESMAN       TO D-SMAN
           MOVE ")"               TO D-BR2
           MOVE DR-BALANCE        TO D-BALANCE
           MOVE DR-SALES-PTD      TO D-SALESAMT
           MOVE DR-COST-PTD       TO D-COST
           COMPUTE WS-MARGIN = DR-SALES-PTD - DR-COST-PTD
           MOVE WS-MARGIN         TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / DR-COST-PTD * 100
           MOVE WS-PERC           TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0 TO WS-MARGIN
                     WS-PERC
           MOVE DR-NAME           TO D-CATEGORY
           MOVE DR-SALES-YTD      TO D-SALESAMT
           MOVE DR-COST-YTD       TO D-COST
           COMPUTE WS-MARGIN = DR-SALES-YTD - DR-COST-YTD
           MOVE WS-MARGIN         TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / DR-COST-YTD * 100
           MOVE WS-PERC           TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0 TO WS-MARGIN
                     WS-PERC
           MOVE " "               TO D-CATEGORY
           MOVE DR-SALES-LAST     TO D-SALESAMT
           MOVE DR-COST-LAST      TO D-COST
           COMPUTE WS-MARGIN = DR-SALES-LAST - DR-COST-LAST
           MOVE WS-MARGIN         TO D-MARGIN
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / DR-COST-LAST * 100
           MOVE WS-PERC           TO D-PERC
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
                       DETAIL-LINE
           MOVE 0 TO WS-MARGIN
                     WS-PERC
           WRITE PRINT-REC AFTER 1
           ADD 4 TO LINE-CNT
           MOVE 0 TO WS-MARGIN
                     WS-PERC.
           IF WS-PRINTER = "[VID]" OR = "[Vid]"
            IF LINE-CNT > 21
              MOVE 66 TO LINE-CNT.
           GO TO PRR-005.
       PRR-999.
           EXIT.
      *
       CLEAR-SCREEN-PART SECTION.
       CSP-005.
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
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-008.
           OPEN I-O SBREP-MASTER.
           IF WS-SBREP-ST1 NOT = 0 
              MOVE 0 TO WS-SBREP-ST1
              MOVE "SBREP FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-008.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
            EXIT.
      *
        END-OFF SECTION.
        END-000.
           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
              GO TO END-010.
           IF LINE-CNT < 24
              GO TO END-020.
           IF WS-TOTAlS-ONLY = "Y"
              PERFORM PRR-015
              GO TO END-020.
           IF WS-TOTAlS-ONLY = "N"
              MOVE 3010 TO POS
              DISPLAY "Press ANY key for NEXT-PAGE OR 'END' to EXIT"
              AT POS
              ADD 60 TO POS
              ACCEPT WS-ACCEPT AT POS.
           IF W-ESCAPE-KEY = 3
              GO TO END-900
           ELSE
              PERFORM PRR-015
              GO TO END-020.
        END-010.
           IF LINE-CNT > 56
              PERFORM PRR-015.
        END-020.
           MOVE "TOTALS***"      TO D-CATEGORY.
           MOVE TOT-SALESAMT-PTD TO D-SALESAMT.
           MOVE TOT-COST-PTD     TO D-COST.
           COMPUTE WS-MARGIN = TOT-SALESAMT-PTD - TOT-COST-PTD.
           MOVE WS-MARGIN        TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / TOT-COST-PTD * 100.
           MOVE WS-PERC          TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.
           MOVE TOT-SALESAMT-YTD TO D-SALESAMT.
           MOVE TOT-COST-YTD     TO D-COST.
           COMPUTE WS-MARGIN = TOT-SALESAMT-YTD - TOT-COST-YTD.
           MOVE WS-MARGIN        TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / TOT-COST-YTD * 100.
           MOVE WS-PERC          TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE 0 TO WS-MARGIN
                     WS-PERC.
           MOVE TOT-SALESAMT-LAST TO D-SALESAMT.
           MOVE TOT-COST-LAST     TO D-COST.
           COMPUTE WS-MARGIN = TOT-SALESAMT-LAST - TOT-COST-LAST.
           MOVE WS-MARGIN         TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED = WS-MARGIN / TOT-COST-LAST * 100.
           MOVE WS-PERC           TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.

           IF WS-THIS-YEAR = "Y"
               WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.

           IF WS-PRINTER NOT = "[VID]" AND NOT = "[Vid]"
               PERFORM GET-USER-MAIL-NAME
               PERFORM GET-REPORT-Y2K-DATE
               PERFORM PRINT-REPORT-INFO.
               
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
      *      RETURNING W-STATUS.
       END-900.
           CLOSE DEBTOR-MASTER.
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
