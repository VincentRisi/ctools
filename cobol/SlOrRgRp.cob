        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlOrRgRp.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectStTrans".
        Copy "SelectSlRegister".
        Copy "SelectDrMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdRegister.
           COPY ChlfdDebtor.
           COPY ChlfdStTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(255).

      *
       WORKING-STORAGE SECTION.
       77  WS-ANSWER1           PIC X(2) VALUE " ".
       77  WS-DATE-ACCEPT       PIC X(10) VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
       77  WS-QUANTITY          PIC S9(5) VALUE 0.
       77  WS-AMT               PIC 9(6)V99 VALUE 0.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-TRANSNO           PIC 9(3) VALUE 0.
       77  WS-TOTI-INVOICE      PIC S9(8)V99 VALUE 0.
       77  WS-INVNO             PIC 9(5) VALUE 0.
       77  WS-DISPLAY-NO        PIC Z(4)9.
       77  WS-ACCEPT            PIC XX VALUE " ".
       77  WS-REP-ONLY          PIC X VALUE " ".
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1   PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  HEAD1.
           03  FILLER           PIC X(5) VALUE "DATE".
           03  H1-DATE          PIC X(10).
           03  FILLER           PIC X(6) VALUE " ".
           03  FILLER           PIC X(11) VALUE "SALES REP:".
           03  H-CODE           PIC X(17) VALUE " ".
           03  FILLER           PIC X(40) VALUE 
           "O R D E R S     R E G I S T E R".
           03  H-TYPE           PIC X(30) VALUE " ".
           03  FILLER           PIC X(6) VALUE "PAGE: ".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(4) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(21) VALUE " ".
           03  FILLER           PIC X(11) VALUE "SALESMAN#:".
           03  H2-SALESMAN      PIC X(17) VALUE " ".
           03  FILLER           PIC X(103) VALUE 
           "*******************************".
       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "  SLIP".
           03  FILLER           PIC X(31) VALUE " A C C O U N T".
           03  FILLER           PIC X(22) VALUE "ORDER       REMAIN AR".
           03  FILLER           PIC X(65) VALUE
           "STA CUSTOMER              LI CONTACT               PHONE".
           03  FILLER           PIC X(7) VALUE "INVOICE".
       01  HEAD4.
           03  FILLER           PIC X(11) VALUE "   No:".
           03  FILLER           PIC X(31) VALUE "NUMBER NAME".
           03  FILLER           PIC X(21) VALUE
           "DATE       AMOUNT EA".
           03  FILLER           PIC X(67) VALUE
           "TUS ORDER No:             NE NAME                   No:".
           03  FILLER           PIC X(7) VALUE "No:".
       01  DETAIL-LINE.
           03  D-INVNO          PIC Z(5)9.
           03  FILLER           PIC X(1) VALUE "/".
           03  D-COPYNUM        PIC 99.
           03  FILLE            PIC X VALUE " ".
           03  D-CUSTNO         PIC X(8) VALUE " ".
           03  D-NAME           PIC X(20) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  D-DATE           PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  D-INVAMT         PIC Z(5)9.99.
           03  FILLER           PIC X(2) VALUE " ".
           03  D-AREA           PIC X(2) VALUE " ".
           03  D-STATUS         PIC X(4) VALUE " ".
           03  D-PO             PIC X(21) VALUE " ".
           03  D-LINE           PIC Z(2)9.
           03  FILLER           PIC X(1) VALUE " ".
           03  D-CONTACT        PIC X(22) VALUE " ".
           03  D-PHONE          PIC X(16) VALUE " ".
           03  D-INV            PIC Z(5)9.
       01  TOTAL-LINE.
           03  FILLER           PIC X(7) VALUE " ".
           03  TOT-DESC         PIC X(22) VALUE " ".
           03  TOT-NO           PIC Z(4)9.
           03  FILLER           PIC X(3) VALUE " ".
           03  FILLER           PIC X(11) VALUE "  TOTALS: R".
           03  TOT-INVAMT       PIC Z(7)9.99.
           03  FILLER           PIC X(73) VALUE " ".
       01  TOTAL-LINE2.
         02  TOT2-LINE.
           03  TOT2-DESC        PIC X(27) VALUE " ".
           03  TOT2-DATE        PIC X(10).
           03  FILLER           PIC X(23) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 325 TO POS
           DISPLAY "** Orders Register Report **" AT POS
           MOVE 425 TO POS
           DISPLAY "****************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-012.
           MOVE 1310 TO POS.
           DISPLAY "ENTER A REPS INITIALS, BLANK FOR ALL. :     [  ]"
              AT POS.
           ADD 45 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

      *     ACCEPT WS-ANSWER1 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-012.
       CONTROL-015.
           MOVE 1510 TO POS.
           DISPLAY "ENTER A REP # FROM DEBTOR FILE, BLANK FOR ALL [ ]"
           AT POS.
           ADD 47 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-REP-ONLY.

      *     ACCEPT WS-REP-ONLY AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-012.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-016
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-016.
           MOVE 1710 TO POS.
           DISPLAY
           "ENTER A DATE TO PRINT FROM, BLANK FOR ALL. :[          ]"
               AT POS.
           ADD 45 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DATE-ACCEPT.

      *     ACCEPT WS-DATE-ACCEPT AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
               
           IF WS-DATE-ACCEPT  = " "
               GO TO CONTROL-017.
           MOVE WS-DATE-ACCEPT TO ALPHA-RATE
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
               GO TO CONTROL-016.
           MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE
           DISPLAY DISPLAY-DATE AT POS
           MOVE DISPLAY-DATE    TO TOT2-DATE
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE      TO INCR-DATE WS-AGE-DATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
               GO TO CONTROL-016.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-017
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-016.
       CONTROL-017.
           MOVE 1910 TO POS
           DISPLAY "N=NEW ORDERS NOT INVOICED OR PULLED        :[ ]"
               AT POS
           MOVE 2010 TO POS
           DISPLAY "A=ALL ORDERS NOT COMPLETE INCLUDING B/O'S." AT POS
           MOVE 2110 TO POS
           DISPLAY "S=SUSPENDED ORDERS." AT POS
           MOVE 2210 TO POS
           DISPLAY "P=ORDERS PULLED AND AWAITING INVOICING." AT POS
           MOVE 1955 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 16         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER3.

      *     ACCEPT WS-ANSWER3 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-016.
           IF WS-ANSWER3 NOT = "N" AND NOT = "A"
                     AND NOT = "S" AND NOT = "P"
               GO TO CONTROL-017.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-019
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-017.
       CONTROL-019.
           MOVE 2610 TO POS.
           DISPLAY "The report is being compiled ......." AT POS.
       CONTROL-050.
           IF WS-ANSWER3 = "A"
               MOVE "ALL PENDING ORDERS  " TO H-TYPE.
           IF WS-ANSWER3 = "N"
               MOVE "NEW ORDERS NOT PULLD" TO H-TYPE.
           IF WS-ANSWER3 = "S"
               MOVE "SUSPENDED ORDERS.   " TO H-TYPE.
           IF WS-ANSWER3 = "P"
               MOVE "ORDERS AWAITING INV." TO H-TYPE.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE
           PERFORM PRINT-TOTALS
           PERFORM END-OFF.
       CONTROL-999.
           EXIT PROGRAM.
      *
       READ-DEBTOR SECTION.
       RD-005.
           MOVE INCR-ACCOUNT TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "UNKNOWN DEBTOR" TO DR-NAME
               GO TO RD-999.
       RD-010.
           READ DEBTOR-MASTER 
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "UNKNOWN DEBTOR" TO DR-NAME
               GO TO RD-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-010.
       RD-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           IF WS-DATE-ACCEPT = " "
               MOVE 4 TO INCR-TRANS
               MOVE 1 TO INCR-INVOICE
               START INCR-REGISTER KEY NOT < INCR-KEY
                   INVALID KEY NEXT SENTENCE
           ELSE
               START INCR-REGISTER KEY NOT < INCR-DATE
                   INVALID KEY NEXT SENTENCE.
       PR-004.
           IF WS-INCR-ST1 NOT = 0
             MOVE
             "NO ORDERS WITH THOSE INITIALS OR DATE, 'ESC' TO EXIT"
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE 88 TO WS-INCR-ST1
             GO TO PR-999.
       PR-005.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
              GO TO PR-999.
           IF WS-INCR-ST1 NOT = 0
             MOVE "REGISTER BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-INCR-ST1
               GO TO PR-005.

           MOVE 2310 TO POS
           DISPLAY "Record Being Read:" AT POS
           ADD 20 TO POS
           DISPLAY INCR-INVOICE AT POS
           ADD 8 TO POS
           DISPLAY "TYPE:" AT POS
           ADD 6 TO POS
           DISPLAY INCR-TRANS AT POS.

           IF INCR-TRANS NOT = 4
            IF WS-DATE-ACCEPT NOT = " "
              GO TO PR-005
            ELSE
              GO TO PR-999.
           IF INCR-PRINTED = "L" OR = "Y"
              GO TO PR-005.
           IF WS-ANSWER1 = "  "
              GO TO PR-008.
           IF WS-ANSWER1 NOT = "  "
            IF INCR-SB-TYPE = WS-ANSWER1
              GO TO PR-008.
           GO TO PR-005.
       PR-008.
           IF WS-REP-ONLY NOT = " "
                PERFORM READ-DEBTOR
            IF WS-REP-ONLY NOT = DR-SALESMAN
                GO TO PR-005.
           IF WS-DATE-ACCEPT NOT = " "
            IF INCR-DATE NOT < WS-AGE-DATE
              GO TO PR-009
            ELSE
              GO TO PR-005.
       PR-009.
           IF WS-ANSWER3 = "A"
            IF INCR-PRINTED = "N" OR = "P" OR = "S"
              GO TO PR-010.
           IF WS-ANSWER3 = "P"
            IF INCR-PRINTED = "P"
             IF INCR-PULLBY NOT = " "
              GO TO PR-010.
           IF WS-ANSWER3 = "N"
            IF INCR-INVOICE = INCR-BO-INV-NO
             IF INCR-PULLBY = " "
              GO TO PR-010.
           IF WS-ANSWER3 = "S"
            IF INCR-PRINTED = "S"
              GO TO PR-010.
           GO TO PR-005.
       PR-010.
           IF WS-LINE > 59
               PERFORM PRINT-HEADINGS.
           MOVE INCR-INVOICE      TO D-INVNO
           MOVE INCR-COPY-NUMBER  TO D-COPYNUM
           MOVE INCR-ACCOUNT      TO D-CUSTNO
           MOVE INCR-NAME         TO D-NAME
           MOVE INCR-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-DATE
           IF INCR-PRINTED = "S"
              MOVE "SUS"          TO D-STATUS.
           IF INCR-PRINTED = "P"
              MOVE "RDY"          TO D-STATUS.
           IF INCR-PRINTED = "N"
              MOVE "B/O"          TO D-STATUS.
           MOVE INCR-PORDER       TO D-PO
           PERFORM READ-TRANSACTIONS
           MOVE WS-AMT            TO D-INVAMT
           MOVE INCR-AREA         TO D-AREA
           MOVE WS-TRANSNO        TO D-LINE
           MOVE INCR-CONTACT      TO D-CONTACT
           MOVE INCR-PHONE        TO D-PHONE
           MOVE INCR-BO-INV-NO    TO D-INV.
       PR-020.
           ADD WS-AMT    TO WS-TOTI-INVOICE
           ADD 1         TO WS-INVNO
           MOVE 2410 TO POS
           MOVE WS-INVNO TO WS-DISPLAY-NO
           DISPLAY "Number of Orders Printed:" AT POS
           ADD 25 TO POS
           DISPLAY WS-DISPLAY-NO AT POS.
       PR-900.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 2
           MOVE " " TO PRINT-REC
           ADD 2 TO WS-LINE
           GO TO PR-005.
       PR-999.
           EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RT-000.
            MOVE 0 TO WS-AMT
                      WS-TRANSNO.
            MOVE INCR-INVOICE TO STTR-REFERENCE1.
            MOVE 4            TO STTR-TYPE.
            MOVE 1            TO STTR-TRANSACTION-NUMBER.
            START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 NOT = 0
               GO TO RT-999.
       RT-002.
            READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-STTRANS-ST1 = 10
               GO TO RT-999.
            IF WS-STTRANS-ST1 NOT = 0
              MOVE "STTRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RT-002.
            IF STTR-REFERENCE1 NOT = INCR-INVOICE
               GO TO RT-999.
            IF STTR-TYPE NOT = 4
               GO TO RT-002.
            IF STTR-COMPLETE = "Y" OR = "L"
               GO TO RT-002.
            MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
               GO TO RT-002.
       RT-005.
            COMPUTE WS-QUANTITY = STTR-ORDERQTY - STTR-SHIPPEDQTY.
            IF WS-QUANTITY NOT > 0
                 GO TO RT-002.
            COMPUTE WS-AMT = WS-AMT + (WS-QUANTITY * STTR-PRICE).
            ADD 1 TO WS-TRANSNO.
            GO TO RT-002.
       RT-999.
            EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           IF WS-ANSWER1 = "  "
               MOVE "ALL REPS" TO H-CODE
           ELSE
               MOVE WS-ANSWER1 TO H-CODE.
           IF WS-REP-ONLY = " "
               MOVE "ALL SALESMAN" TO H2-SALESMAN
           ELSE
               MOVE WS-REP-ONLY    TO H2-SALESMAN.
           ADD 1                TO WS-PAGE
           MOVE WS-PAGE         TO H-PAGE
           MOVE 6               TO WS-LINE.

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
               
           MOVE WS-PRINT-BOLD   TO COMP-DIG1
           MOVE WS-PRINT-UNBOLD TO COMP-DIG2.
           IF WS-PAGE = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           WRITE PRINT-REC FROM HEAD3 AFTER 2
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
       PH-999.
           EXIT.
      *
       PRINT-TOTALS SECTION.
       PT-000.
           IF WS-LINE > 55
              PERFORM PRINT-HEADINGS.
           MOVE WS-TOTI-INVOICE         TO TOT-INVAMT
           MOVE "    NUMBER OF ORDERS:" TO TOT-DESC
           MOVE WS-INVNO                TO TOT-NO
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
           MOVE " " TO PRINT-REC.
       PT-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-020.
           MOVE 2910 TO POS
           DISPLAY "Opening files ....." AT POS.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-020.
       OPEN-025.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-025.
       OPEN-030.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE 0 TO WS-DEBTOR-ST1
              MOVE "DR MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-030.
           MOVE Ws-Co-Name TO CO-NAME.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.

           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
           PERFORM ERROR1-020.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
      *     IF WS-INCR-ST1 = 88
      *         GO TO END-800.
           IF WS-LINE > 59
               PERFORM PRINT-HEADINGS.
           WRITE PRINT-REC.
           WRITE PRINT-REC.
           IF WS-DATE-ACCEPT NOT = " "
               MOVE "ONLY ORDERS PRINTED FROM :" TO TOT2-DESC
               WRITE PRINT-REC FROM TOTAL-LINE2.
           MOVE " " TO PRINT-REC TOTAL-LINE2.
           IF WS-ANSWER3 = "N"
              MOVE "ONLY NEW ORDERS NOT YET INVOICED HAVE BEEN PRINTED."
               TO TOT2-LINE
               WRITE PRINT-REC FROM TOTAL-LINE2.
           IF WS-ANSWER3 = "A"
               MOVE "ALL INCOMPLETE ORDERS HAVE BEEN PRINTED."
               TO TOT2-LINE
               WRITE PRINT-REC FROM TOTAL-LINE2.
           IF WS-ANSWER3 = "S"
               MOVE "ALL SUSPENDED ORDERS HAVE BEEN PRINTED."
               TO TOT2-LINE
               WRITE PRINT-REC FROM TOTAL-LINE2.
           IF WS-ANSWER3 = "P"
               MOVE "ALL ORDERS AWAITING INVOICING HAVE BEEN PRINTED."
               TO TOT2-LINE
               WRITE PRINT-REC FROM TOTAL-LINE2.
           IF WS-INVNO = 0
              MOVE "NO ORDERS TO PRINT WITHIN GIVEN PARAMETERS."
               TO TOT2-LINE
               WRITE PRINT-REC FROM TOTAL-LINE2.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-800.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE INCR-REGISTER
                 STOCK-TRANS-FILE.
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
