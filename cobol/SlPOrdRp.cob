        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlPOrdRp.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectSlRegister".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdRegister.

       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).

      *
       WORKING-STORAGE SECTION.
       77  WS-INVNO            PIC 9(5) VALUE 0.
       77  WS-ACCEPT           PIC XX VALUE " ".
       77  WS-YYACCEPT         PIC X(4) VALUE " ".
       77  WS-VALUE            PIC S9(7)V99.
       77  WS-PAGE             PIC 9(3) VALUE 0.
       77  WS-LINE             PIC 9(3) VALUE 66.
       01  W-DD                PIC Z9.
       01  W-YY                PIC Z(3)9.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1     PIC 99.
       01  WEEK-DATE.
           03  WEEK-YY         PIC 9999.
           03  WEEK-MM         PIC 99.
           03  WEEK-DD         PIC 99.
       01  COMP-DATE.
           03  COMP-YY         PIC 9999.
           03  COMP-MM         PIC 99.
           03  COMP-DD         PIC 99.
       01  WS-TYPES.
           03  FILLER           PIC X(9) VALUE "  JANUARY".
           03  FILLER           PIC X(9) VALUE " FEBRUARY".
           03  FILLER           PIC X(9) VALUE "    MARCH".
           03  FILLER           PIC X(9) VALUE "    APRIL".
           03  FILLER           PIC X(9) VALUE "      MAY".
           03  FILLER           PIC X(9) VALUE "     JUNE".
           03  FILLER           PIC X(9) VALUE "     JULY".
           03  FILLER           PIC X(9) VALUE "   AUGUST".
           03  FILLER           PIC X(9) VALUE "SEPTEMBER".
           03  FILLER           PIC X(9) VALUE "  OCTOBER".
           03  FILLER           PIC X(9) VALUE " NOVEMBER".
           03  FILLER           PIC X(9) VALUE " DECEMBER".
       01  WS-TYPES-RED REDEFINES WS-TYPES.
           03  WS-TYPE-DESC     PIC X(9) OCCURS 12.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE "  DATE".
           03  H1-DATE          PIC X(10).
           03  FILLER           PIC X(22) VALUE " ".
           03  FILLER           PIC X(56) VALUE 
           "I N V O I C E D   O R D E R S    R E G I S T E R    FOR:".
           03  H-MONTH          PIC X(11).
           03  H-YEAR           PIC X(10).
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(8) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(39) VALUE " ".
           03  FILLER           PIC X(71) VALUE ALL"*".
           03  FILLER           PIC X(44) VALUE " ".
       01  HEAD3.
           03  FILLER           PIC X(25) VALUE "P-ORDER NUMBER".
           03  FILLER           PIC X(38) VALUE "ACCOUNT  NAME".
           03  FILLER           PIC X(7) VALUE "DATE".
           03  FILLER           PIC X(26) VALUE "INVOICE  TYPE  P-SLIP".
           03  FILLER           PIC X(40) VALUE "INV TOTAL    SOLD-BY".
       01  DETAIL-LINE.
           03  D-PO             PIC X(25).
           03  D-CUSTNO         PIC X(9) VALUE " ".
           03  D-NAME           PIC X(26) VALUE " ".
           03  D-DATE           PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  D-INV            PIC Z(5)9.
           03  FILLER           PIC X(4) VALUE " ".
           03  D-TYPE           PIC X(3).
           03  FILLER           PIC X(2) VALUE " ".
           03  D-PSLIP          PIC Z(5)9.
           03  FILLER           PIC X(5) VALUE " ".
           03  D-INVAMT         PIC Z(5)9.99.
           03  FILLER           PIC X(7) VALUE " ".
           03  D-SOLDBY         PIC XX VALUE " ".
           03  FILLER           PIC X(22) VALUE " ".
       01  TOTAL-LINE.
           03  TOT-DESC         PIC X(21).
           03  TOT-INV-NO       PIC Z(3)9.
           03  FILLER           PIC X(10) VALUE " ".
           03  FILLER           PIC X(25) VALUE
              "AVE VALUE PER INVOICE: R".
           03  TOT-VALUE        PIC Z(6)9.99-.
           03  FILLER           PIC X(65) VALUE " ".
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
           DISPLAY "** Orders Register Report  by Month**" AT POS
           MOVE 415 TO POS
           DISPLAY "*************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       CONTROL-015.
           MOVE 1710 TO POS
           DISPLAY "               The MONTH to print is : [  ]" AT POS
           ADD 40 TO POS.
           MOVE WS-MM TO W-DD WS-ACCEPT
           DISPLAY W-DD AT POS

           MOVE W-DD      TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

      *     ACCEPT WS-ACCEPT AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           MOVE WS-ACCEPT TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WEEK-MM.
           IF WEEK-MM NOT > 0 AND NOT < 13
              MOVE 2510 TO POS
              DISPLAY "Enter a month between 1 & 12" AT POS
              MOVE "  " TO WS-ACCEPT
              GO TO CONTROL-015.
           MOVE WEEK-MM TO W-DD.
           DISPLAY W-DD AT POS.
       CONTROL-111.
           MOVE 1910 TO POS
           DISPLAY "                The YEAR to print is : [    ]"
            AT POS
           ADD 40 TO POS
           MOVE WS-YY TO W-YY WS-YYACCEPT
           DISPLAY W-YY AT POS.

           MOVE W-YY      TO CDA-DATA.
           MOVE 4         TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

      *     ACCEPT WS-YYACCEPT AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           MOVE WS-YYACCEPT TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WEEK-YY.
           IF WEEK-YY NOT > 1989
              MOVE 2510 TO POS
              DISPLAY "Enter a YEAR > 1989" AT POS
              MOVE "  " TO WS-ACCEPT
              GO TO CONTROL-111.
           MOVE WEEK-YY TO W-YY.
           DISPLAY W-YY AT POS.
       CONTROL-112.
           MOVE WS-TYPE-DESC (WEEK-MM) TO H-MONTH
           MOVE WEEK-YY                TO H-YEAR.
           MOVE 2510 TO POS.
           DISPLAY "The report is being compiled ......." AT POS.
       CONTROL-500.
           PERFORM OPEN-FILES.
           
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.

           PERFORM PRINT-ROUTINE.
           PERFORM PRINT-TOTALS.
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PR-001.
           MOVE " " TO INCR-PORDER.
           START INCR-REGISTER KEY NOT < INCR-PORDER
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
              PERFORM PRINT-HEADINGS
              MOVE "INVALID START IN REGISTER, PROGRAM ABORTED."
              TO PRINT-REC
              WRITE PRINT-REC AFTER 3
              GO TO PR-999.
       PR-003.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10 OR 23
              GO TO PR-999.
           IF WS-INCR-ST1 NOT = 0
              MOVE "RECORD LOCKED AT ANOTHER STATION, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO PR-003.
           IF INCR-TRANS NOT = 1 AND NOT = 6
              GO TO PR-003.
           MOVE INCR-DATE TO COMP-DATE.
           IF COMP-YY NOT = WEEK-YY
             MOVE 2510 TO POS
             DISPLAY "Date of Transaction < Range Entered, reading Next"
             AT POS
             GO TO PR-003.
           IF COMP-MM NOT = WEEK-MM
             MOVE 2510 TO POS
             DISPLAY "Date of Transaction < Range Entered, reading Next"
             AT POS
             GO TO PR-003.
       PR-010.
            MOVE 2510 TO POS
            DISPLAY "                                                 "
               AT POS.
           IF WS-LINE > 59
               PERFORM PRINT-HEADINGS.
           IF INCR-TRANS = 1
                 MOVE "INV"       TO D-TYPE
           ELSE
                 MOVE "C/N"       TO D-TYPE.
           MOVE INCR-PORDER       TO D-PO
           MOVE INCR-ACCOUNT      TO D-CUSTNO
           MOVE INCR-NAME         TO D-NAME
           MOVE INCR-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-DATE
           MOVE INCR-INVOICE      TO D-INV
           MOVE INCR-BO-INV-NO    TO D-PSLIP
           MOVE INCR-INVCRED-AMT  TO D-INVAMT
           MOVE INCR-SB-TYPE      TO D-SOLDBY
           IF INCR-TRANS = 1
             ADD INCR-INVCRED-AMT TO WS-VALUE
             ADD 1                TO WS-INVNO
           ELSE
             SUBTRACT INCR-INVCRED-AMT FROM WS-VALUE
             SUBTRACT 1            FROM WS-INVNO.
       PR-900.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO WS-LINE

           MOVE 2210 TO POS
           DISPLAY "DATE:" AT POS
           ADD 6 TO POS
           DISPLAY D-DATE AT POS
           ADD 10 TO POS
           DISPLAY "TRANS No:" AT POS
           ADD 10 TO POS
           DISPLAY D-INV AT POS
           ADD 10 TO POS
           DISPLAY "PORDER =" AT POS
           ADD 9 TO POS
           DISPLAY D-PO AT POS
           GO TO PR-003.
       PR-999.
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
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE 5 TO WS-LINE
           WRITE PRINT-REC AFTER 1.
       PH-999.
           EXIT.
      *
       PRINT-TOTALS SECTION.
       PT-000.
           IF WS-LINE = 66
               PERFORM PRINT-HEADINGS.
           MOVE "  NUMBER OF INVOICES:" TO TOT-DESC
           MOVE WS-INVNO                TO TOT-INV-NO
           COMPUTE WS-VALUE = WS-VALUE / WS-INVNO
           MOVE WS-VALUE                TO TOT-VALUE
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
       PT-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-120.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-120.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE
                 INCR-REGISTER.
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
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
