        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlDelvRp.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectSlRegister".
        Copy "SelectSlRegLy".
        Copy "SelectDrMaster".
           SELECT RANDOM-FILE ASSIGN TO WS-RANDOM-FILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-RANDOM-STATUS
               RECORD KEY IS RANDOM-KEY.
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdRegister.
           COPY ChlfdRegisterLy.
           COPY ChlfdDebtor.

       FD  RANDOM-FILE.
       01  RANDOM-REC.
           03  RANDOM-KEY.
              05  RANDOM-AREA       PIC X.
              05  RANDOM-ACCOUNT    PIC 9(7).
              05  RANDOM-NUMBER     PIC 9(6).
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(255).

      *
       WORKING-STORAGE SECTION.
       77  WS-RANDOM-FILE       PIC X(30) VALUE
              "/ctools/spl/DelSchedule".
       77  WS-RANDOM-FILE-ind   PIC X(30) VALUE
              "/ctools/spl/DelSchedule.Ind".
       77  WS-AREA              PIC X VALUE " ".
       77  WS-RANDOM-WRITTEN    PIC X(2) VALUE " ".
       77  WS-DATE-ACCEPT       PIC X(10) VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
       77  WS-QUANTITY          PIC S9(5) VALUE 0.
       77  WS-AMT               PIC 9(6)V99 VALUE 0.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-TRANSNO           PIC 9(3) VALUE 0.
       77  WS-TOTI-INVOICE      PIC S9(6)V99 VALUE 0.
       77  WS-INVNO             PIC 9(5) VALUE 0.
       77  WS-INVNO-PRINTED     PIC 9(5) VALUE 0.
       77  WS-DISPLAY-NO        PIC Z(4)9.
       77  WS-ACCEPT            PIC XX VALUE " ".
       77  WS-REP-ONLY          PIC X VALUE " ".
       77  WS-LY-YTD            PIC X VALUE " ".
       77  WS-TYPE              PIC X VALUE " ".
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-INCR-LY-STATUS.
           03  WS-INCR-LY-ST1   PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-RANDOM-STATUS.
           03  WS-RANDOM-ST1    PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  HEAD1.
           03  FILLER           PIC X(5) VALUE "DATE".
           03  H1-DATE          PIC X(10).
           03  FILLER           PIC X(6) VALUE " ".
           03  FILLER           PIC X(11) VALUE "DEL AREA :".
           03  H-CODE           PIC X(18) VALUE " ".
           03  FILLER           PIC X(70) VALUE 
           "I N V O I C E   D E L I V E R Y    S C H E D U L E ".
           03  FILLER           PIC X(6) VALUE "PAGE: ".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(4) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(21) VALUE " ".
           03  FILLER           PIC X(11) VALUE "SALESMAN :".
           03  H2-SALESMAN      PIC X(18).
           03  FILLER           PIC X(103) VALUE 
           "***************************************************".
       01  HEAD3.
           03  FILLER           PIC X(8) VALUE "INVOICE".
           03  FILLER           PIC X(35) VALUE "  A C C O U N T".
           03  FILLER           PIC X(21) VALUE "INVOICE     INVOICE".
           03  FILLER           PIC X(48) VALUE
           "CUSTOMER           DELIV   NUM OF     C.O.D.".
           03  FILLER           PIC X(22) VALUE "CUSTOMERS     TIME".
       01  HEAD4.
           03  FILLER           PIC X(8) VALUE "    No:".
           03  FILLER           PIC X(35) VALUE " NUMBER NAME".
           03  FILLER           PIC X(21) VALUE "  DATE       AMOUNT".
           03  FILLER           PIC X(48) VALUE
           "ORDER No:          SEQU.   PARCELS     Y/N".
           03  FILLER           PIC X(22) VALUE "SIGNATURE   IN    OUT".
       01  DELIVERY-LINE.
           03  FILLER           PIC X(16) VALUE " ".
           03  D-DELIVERY       PIC X(26) VALUE " ".
           03  FILLER           PIC X(39) VALUE " ".
           03  D-SEQU-UNDER     PIC X(10) VALUE " ".
           03  D-PARCEL-UNDER   PIC X(10) VALUE " ".
           03  D-COD            PIC X(10) VALUE " ".
           03  D-SIG            PIC X(10) VALUE " ".
           03  D-IN             PIC X(11) VALUE " ".
       01  DETAIL-LINE.
           03  D-INVNO          PIC Z(5)9.
           03  FILLER           PIC X(2) VALUE " ".
           03  D-CUSTNO         PIC X(8) VALUE " ".
           03  D-NAME           PIC X(25) VALUE " ".
           03  FILLER           PIC X VALUE " ".
           03  D-DATE           PIC X(10).
           03  FILLER           PIC X VALUE " ".
           03  D-INVAMT         PIC Z(5)9.99.
           03  FILLER           PIC X(2) VALUE " ".
           03  D-PO             PIC X(20) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER           PIC X(10) VALUE " ".
           03  TOT-DESC         PIC X(22) VALUE " ".
           03  TOT-NO           PIC Z(4)9.
           03  FILLER           PIC X(5) VALUE " ".
           03  FILLER           PIC X(11) VALUE "  TOTALS: R".
           03  TOT-INVAMT       PIC Z(5)9.99.
           03  FILLER           PIC X(73) VALUE " ".
       01  TOTAL-LINE2.
         02  TOT2-LINE.
           03  TOT2-DESC        PIC X(27) VALUE " ".
           03  TOT2-DATE        PIC X(10).
           03  FILLER           PIC X(25) VALUE " ".
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
           DISPLAY "** Invoice P/Slip Delivery Schedule **" AT POS
           MOVE 425 TO POS
           DISPLAY "**************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-011.
           MOVE 1110 TO POS.
           DISPLAY "ENTER P=P/SLIP, I=INVOICE TO PRINT     :    [ ]"
              AT POS.
           ADD 45 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 8         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TYPE.

      *     ACCEPT WS-TYPE AT POS.
           IF WS-TYPE NOT = "I" AND NOT = "P"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-011.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-012
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-011.
       CONTROL-012.
           MOVE 1310 TO POS.
           DISPLAY "ENTER AN AREA TO PRINT, BLANK FOR ALL. :    [ ]"
              AT POS.
           ADD 45 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-AREA.

      *     ACCEPT WS-AREA AT POS.
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
               GO TO CONTROL-020.
           MOVE WS-DATE-ACCEPT TO ALPHA-RATE
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
               GO TO CONTROL-016.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE
            DISPLAY DISPLAY-DATE AT POS
            MOVE DISPLAY-DATE TO TOT2-DATE
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO INCR-DATE WS-BEG-DATE
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO CONTROL-016.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-016.
       CONTROL-020.
           MOVE "T" TO WS-LY-YTD.
           MOVE 1910 TO POS.
           DISPLAY "PRINT FROM T=THIS YEAR, L=L/Y FILE.    :    [ ]"
              AT POS.
           ADD 45 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-LY-YTD.

      *     ACCEPT WS-LY-YTD AT POS.
           IF WS-LY-YTD NOT = "T" AND NOT = "L"
               GO TO CONTROL-020.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-016.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-025.
           MOVE 2610 TO POS.
           DISPLAY "The report is being compiled ......." AT POS.
           PERFORM OPEN-FILES.
           
           IF WS-AREA > " "
            IF WS-LY-YTD = "T"
              PERFORM PRINT-ROUTINE
              GO TO CONTROL-900.
           IF WS-AREA > " "
            IF WS-LY-YTD = "L"
              PERFORM PRINT-LY-ROUTINE
              GO TO CONTROL-900.
              
           IF WS-LY-YTD = "T"
            IF WS-TYPE = "I"
               PERFORM READ-TO-WRITE-RANDOM-INVOICE
           ELSE
               PERFORM READ-TO-WRITE-RANDOM-PSLIP.
           IF WS-LY-YTD = "L"
            IF WS-TYPE = "I"
               PERFORM READ-TO-WRITE-RANDOM-LY-INV
           ELSE
               PERFORM READ-TO-WRITE-RANDOM-LY-PS.
               
           IF WS-LY-YTD = "T"
              PERFORM PRINT-FROM-RANDOM.
           IF WS-LY-YTD = "L"
              PERFORM PRINT-FROM-LY-RANDOM.
      *     PERFORM DELETE-TRANS.
       CONTROL-900.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT PROGRAM.
      *
       READ-DEBTOR SECTION.
       RD-005.
           MOVE RANDOM-ACCOUNT TO DR-ACCOUNT-NUMBER.
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
               MOVE 0 TO WS-DEBTOR-ST1
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
       WRITE-RANDOM-RECORD SECTION.
       WRR-005.
           IF WS-RANDOM-WRITTEN NOT = "Y"
               MOVE "Y" TO WS-RANDOM-WRITTEN.
           MOVE INCR-AREA     TO RANDOM-AREA.
           MOVE INCR-ACCOUNT  TO RANDOM-ACCOUNT.
           MOVE INCR-INVOICE  TO RANDOM-NUMBER.
       WRR-010.
           WRITE RANDOM-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "RANDOM RECORD INVALID ON WRITE" TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE RANDOM-NUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       WRR-999.
            EXIT.
      *
       WRITE-RANDOM-LY-RECORD SECTION.
       WLY-005.
           IF WS-RANDOM-WRITTEN NOT = "Y"
               MOVE "Y" TO WS-RANDOM-WRITTEN.
           MOVE INCR-LY-AREA     TO RANDOM-AREA.
           MOVE INCR-LY-ACCOUNT  TO RANDOM-ACCOUNT.
           MOVE INCR-LY-INVOICE  TO RANDOM-NUMBER.
       WLY-010.
           WRITE RANDOM-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "RANDOM RECORD INVALID ON WRITE" TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE RANDOM-NUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       WLY-999.
            EXIT.
      *
       READ-TO-WRITE-RANDOM-INVOICE SECTION.
       RTWR-000.
           MOVE 0 TO WS-INVNO.
           PERFORM ERROR-020
           MOVE 2410 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2110 TO POS
           DISPLAY "SELECTION OF INVOICE BY AREA....." AT POS.
           IF WS-DATE-ACCEPT = " "
               MOVE 1 TO INCR-TRANS
               MOVE 1 TO INCR-INVOICE
               START INCR-REGISTER KEY NOT < INCR-KEY
                   INVALID KEY NEXT SENTENCE
           ELSE
               START INCR-REGISTER KEY NOT < INCR-DATE
                   INVALID KEY NEXT SENTENCE.
       RTWR-004.
           IF WS-INCR-ST1 NOT = 0
             MOVE
             "NO INVOICES WITH THOSE INITIALS OR DATE, 'ESC' TO EXIT"
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE 88 TO WS-INCR-ST1
             GO TO RTWR-999.
       RTWR-005.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10 OR = 23
              GO TO RTWR-999.
           IF WS-INCR-ST1 NOT = 0
            MOVE "INVOICE LOCKED BY ANOTHER TERMINAL, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RTWR-005.

           IF INCR-TRANS NOT = 1
            IF WS-DATE-ACCEPT NOT = " "
              GO TO RTWR-005
            ELSE
              GO TO RTWR-999.
           IF INCR-PULL-DATE NOT = 0
              GO TO RTWR-005.
           IF INCR-PRINTED NOT = "Y"
              GO TO RTWR-005.
           IF WS-AREA NOT = "  "
            IF INCR-AREA NOT = WS-AREA
              GO TO RTWR-005.
       RTWR-008.
           IF WS-REP-ONLY NOT = " "
                PERFORM READ-DEBTOR
            IF WS-REP-ONLY NOT = DR-SALESMAN
                GO TO RTWR-005.
           IF WS-DATE-ACCEPT NOT = " "
            IF INCR-DATE > CONVERT-DATE
              GO TO RTWR-010
            ELSE
              GO TO RTWR-005.
       RTWR-010.
           MOVE 2310 TO POS
           DISPLAY "Last Record Read:" AT POS
           ADD 20 TO POS
           DISPLAY INCR-INVOICE AT POS.

           ADD 1 TO WS-INVNO
           MOVE 2410 TO POS
           MOVE WS-INVNO TO WS-DISPLAY-NO
           DISPLAY "Number of Invoices Selected:" AT POS
           ADD 28 TO POS
           DISPLAY WS-DISPLAY-NO AT POS.
       RTWR-900.
           PERFORM WRITE-RANDOM-RECORD.
           GO TO RTWR-005.
       RTWR-999.
           EXIT.
      *
       READ-TO-WRITE-RANDOM-LY-INV SECTION.
       RTWR-000.
           MOVE 0 TO WS-INVNO.
           PERFORM ERROR-020
           MOVE 2410 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2110 TO POS
           DISPLAY "SELECTION OF INVOICE BY AREA....." AT POS.
           IF WS-DATE-ACCEPT = " "
               MOVE 1 TO INCR-LY-TRANS
               MOVE 1 TO INCR-LY-INVOICE
               START INCR-LY-REGISTER KEY NOT < INCR-LY-KEY
                   INVALID KEY NEXT SENTENCE
           ELSE
               START INCR-LY-REGISTER KEY NOT < INCR-LY-DATE
                   INVALID KEY NEXT SENTENCE.
       RTWR-004.
           IF WS-INCR-LY-ST1 NOT = 0
             MOVE
             "NO INVOICES LY WITH THOSE INITIALS OR DATE, 'ESC' TO EXIT"
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE 88 TO WS-INCR-LY-ST1
             GO TO RTWR-999.
       RTWR-005.
           READ INCR-LY-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-LY-ST1 = 10 OR = 23
              GO TO RTWR-999.
           IF WS-INCR-LY-ST1 NOT = 0
           MOVE "INVOICE LY LOCKED BY ANOTHER TERMINAL, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RTWR-005.

           IF INCR-LY-TRANS NOT = 1
            IF WS-DATE-ACCEPT NOT = " "
              GO TO RTWR-005
            ELSE
              GO TO RTWR-999.
           IF INCR-LY-PULL-DATE NOT = 0
              GO TO RTWR-005.
           IF INCR-LY-PRINTED NOT = "Y"
              GO TO RTWR-005.
           IF WS-AREA NOT = "  "
            IF INCR-LY-AREA NOT = WS-AREA
              GO TO RTWR-005.
       RTWR-008.
           IF WS-REP-ONLY NOT = " "
                PERFORM READ-DEBTOR
            IF WS-REP-ONLY NOT = DR-SALESMAN
                GO TO RTWR-005.
           IF WS-DATE-ACCEPT NOT = " "
            IF INCR-LY-DATE > CONVERT-DATE
              GO TO RTWR-010
            ELSE
              GO TO RTWR-005.
       RTWR-010.
           MOVE 2310 TO POS
           DISPLAY "Last Record Read:" AT POS
           ADD 20 TO POS
           DISPLAY INCR-LY-INVOICE AT POS.

           ADD 1 TO WS-INVNO
           MOVE 2410 TO POS
           MOVE WS-INVNO TO WS-DISPLAY-NO
           DISPLAY "Number of Invoices Selected:" AT POS
           ADD 28 TO POS
           DISPLAY WS-DISPLAY-NO AT POS.
       RTWR-900.
           PERFORM WRITE-RANDOM-LY-RECORD.
           GO TO RTWR-005.
       RTWR-999.
           EXIT.
      *
       READ-TO-WRITE-RANDOM-PSLIP SECTION.
       RTWRP-000.
           MOVE 0 TO WS-INVNO.
           PERFORM ERROR-020
           MOVE 2410 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2110 TO POS
           DISPLAY "SELECTION OF P/SLIP BY AREA....." AT POS.
           IF WS-DATE-ACCEPT = " "
               MOVE 4 TO INCR-TRANS
               MOVE 1 TO INCR-INVOICE
               START INCR-REGISTER KEY NOT < INCR-KEY
                   INVALID KEY NEXT SENTENCE
           ELSE
               START INCR-REGISTER KEY NOT < INCR-DATE
                   INVALID KEY NEXT SENTENCE.
       RTWRP-004.
           IF WS-INCR-ST1 NOT = 0
             MOVE
             "NO P/SLIP WITH THOSE INITIALS OR DATE, 'ESC' TO EXIT"
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE 88 TO WS-INCR-ST1
             GO TO RTWRP-999.
       RTWRP-005.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10 OR = 23
              GO TO RTWRP-999.
           IF WS-INCR-ST1 NOT = 0
            MOVE "P/SLIP LOCKED BY ANOTHER TERMINAL, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RTWRP-005.

           IF INCR-TRANS NOT = 4
            IF WS-DATE-ACCEPT NOT = " "
              GO TO RTWRP-005
            ELSE
              GO TO RTWRP-999.
      *    IF INCR-PULL-DATE NOT = 0
      *       GO TO RTWRP-005.
           IF INCR-PRINTED NOT = "P"
              GO TO RTWRP-005.
           IF WS-AREA NOT = "  "
            IF INCR-AREA NOT = WS-AREA
              GO TO RTWRP-005.
       RTWRP-008.
           IF WS-REP-ONLY NOT = " "
                PERFORM READ-DEBTOR
            IF WS-REP-ONLY NOT = DR-SALESMAN
                GO TO RTWRP-005.
           IF WS-DATE-ACCEPT NOT = " "
            IF INCR-DATE NOT < CONVERT-DATE
              GO TO RTWRP-010
            ELSE
              GO TO RTWRP-005.
       RTWRP-010.
           MOVE 2310 TO POS
           DISPLAY "Last Record Read:" AT POS
           ADD 20 TO POS
           DISPLAY INCR-INVOICE AT POS.

           ADD 1 TO WS-INVNO
           MOVE 2410 TO POS
           MOVE WS-INVNO TO WS-DISPLAY-NO
           DISPLAY "Number of P/Slips Selected:" AT POS
           ADD 28 TO POS
           DISPLAY WS-DISPLAY-NO AT POS.
       RTWRP-900.
           PERFORM WRITE-RANDOM-RECORD.
           GO TO RTWRP-005.
       RTWRP-999.
           EXIT.
      *
       READ-TO-WRITE-RANDOM-LY-PS SECTION.
       RTWRP-000.
           MOVE 0 TO WS-INVNO.
           PERFORM ERROR-020
           MOVE 2410 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2110 TO POS
           DISPLAY "SELECTION OF P/SLIP BY AREA....." AT POS.
           IF WS-DATE-ACCEPT = " "
               MOVE 4 TO INCR-LY-TRANS
               MOVE 1 TO INCR-LY-INVOICE
               START INCR-LY-REGISTER KEY NOT < INCR-LY-KEY
                   INVALID KEY NEXT SENTENCE
           ELSE
               START INCR-LY-REGISTER KEY NOT < INCR-LY-DATE
                   INVALID KEY NEXT SENTENCE.
       RTWRP-004.
           IF WS-INCR-LY-ST1 NOT = 0
             MOVE
             "NO P/SLIP LY WITH THOSE INITIALS OR DATE, 'ESC' TO EXIT"
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE 88 TO WS-INCR-LY-ST1
             GO TO RTWRP-999.
       RTWRP-005.
           READ INCR-LY-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-LY-ST1 = 10 OR = 23
              GO TO RTWRP-999.
           IF WS-INCR-LY-ST1 NOT = 0
            MOVE "P/SLIP LY LOCKED BY ANOTHER TERMINAL, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RTWRP-005.

           IF INCR-LY-TRANS NOT = 4
            IF WS-DATE-ACCEPT NOT = " "
              GO TO RTWRP-005
            ELSE
              GO TO RTWRP-999.
      *    IF INCR-LY-PULL-DATE NOT = 0
      *       GO TO RTWRP-005.
           IF INCR-LY-PRINTED NOT = "P"
              GO TO RTWRP-005.
           IF WS-AREA NOT = "  "
            IF INCR-LY-AREA NOT = WS-AREA
              GO TO RTWRP-005.
       RTWRP-008.
           IF WS-REP-ONLY NOT = " "
                PERFORM READ-DEBTOR
            IF WS-REP-ONLY NOT = DR-SALESMAN
                GO TO RTWRP-005.
           IF WS-DATE-ACCEPT NOT = " "
            IF INCR-LY-DATE NOT < CONVERT-DATE
              GO TO RTWRP-010
            ELSE
              GO TO RTWRP-005.
       RTWRP-010.
           MOVE 2310 TO POS
           DISPLAY "Last Record Read:" AT POS
           ADD 20 TO POS
           DISPLAY INCR-LY-INVOICE AT POS.

           ADD 1 TO WS-INVNO
           MOVE 2410 TO POS
           MOVE WS-INVNO TO WS-DISPLAY-NO
           DISPLAY "Number of P/Slips Selected:" AT POS
           ADD 28 TO POS
           DISPLAY WS-DISPLAY-NO AT POS.
       RTWRP-900.
           PERFORM WRITE-RANDOM-LY-RECORD.
           GO TO RTWRP-005.
       RTWRP-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE 0 TO WS-INVNO.
           PERFORM ERROR-020
           MOVE 2410 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2110 TO POS
           DISPLAY "PRINTING OF INVOICES BY AREA....." AT POS.
           
           MOVE WS-AREA TO INCR-AREA.
           START INCR-REGISTER KEY NOT < INCR-AREA
               INVALID KEY NEXT SENTENCE.
       PRR-004.
           IF WS-INCR-ST1 NOT = 0
             MOVE "NO INVOICES WITH THAT AREA CODE, 'ESC' TO EXIT"
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE 88 TO WS-INCR-ST1
             GO TO PRR-999.
       PRR-005.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10 OR = 23
              GO TO PRR-900.
           IF WS-INCR-ST1 NOT = 0
            MOVE "INVOICE LOCKED BY ANOTHER TERMINAL, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO PRR-005.

           IF INCR-TRANS NOT = 1
              GO TO PRR-005.
              
           IF INCR-PULL-DATE NOT = 0
              GO TO PRR-005.
              
            IF INCR-AREA NOT = WS-AREA
              GO TO PRR-900.
       PRR-008.
           IF WS-REP-ONLY NOT = " "
                PERFORM READ-DEBTOR
            IF WS-REP-ONLY NOT = DR-SALESMAN
                GO TO PRR-005.
           IF WS-DATE-ACCEPT NOT = " "
            IF INCR-DATE NOT < WS-BEG-DATE
              GO TO PRR-010
            ELSE
              GO TO PRR-005.
       PRR-010.
           MOVE 2310 TO POS
           DISPLAY "Last Record Read:" AT POS
           ADD 20 TO POS
           DISPLAY INCR-INVOICE AT POS.
       PRR-011.
           IF WS-LINE > 55
               PERFORM PRINT-HEADINGS.
               
           MOVE INCR-INVOICE      TO D-INVNO
           MOVE INCR-ACCOUNT      TO D-CUSTNO
           MOVE INCR-NAME         TO D-NAME
           MOVE INCR-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-DATE
           MOVE INCR-PORDER       TO D-PO
           MOVE INCR-INVCRED-AMT  TO D-INVAMT
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 2
           MOVE " " TO PRINT-REC DETAIL-LINE.

           MOVE INCR-DEL1        TO D-DELIVERY
           MOVE "-------"        TO D-SEQU-UNDER
           MOVE "-------"        TO D-PARCEL-UNDER
           MOVE "-------"        TO D-COD
           MOVE "-------"        TO D-SIG
           MOVE "-----!-----"    TO D-IN
           WRITE PRINT-REC FROM DELIVERY-LINE AFTER 1
           MOVE " " TO PRINT-REC DELIVERY-LINE.
           
           MOVE INCR-DEL2           TO D-DELIVERY
           WRITE PRINT-REC FROM DELIVERY-LINE AFTER 1
           MOVE " " TO PRINT-REC DELIVERY-LINE.
           
           MOVE INCR-DEL3           TO D-DELIVERY
           WRITE PRINT-REC FROM DELIVERY-LINE AFTER 1
           MOVE " " TO PRINT-REC DELIVERY-LINE.
       PRR-020.
           ADD INCR-INVCRED-AMT TO WS-TOTI-INVOICE
           ADD 1                TO WS-INVNO
           
           MOVE 2410 TO POS
           MOVE WS-INVNO TO WS-DISPLAY-NO
           DISPLAY "Number of Invoices Printed:" AT POS
           ADD 27 TO POS
           DISPLAY WS-DISPLAY-NO AT POS.

           ADD 5 TO WS-LINE
           GO TO PRR-005.
       PRR-900.
           PERFORM PRINT-TOTALS.
       PRR-999.
           EXIT.
      *
       PRINT-LY-ROUTINE SECTION.
       PLY-000.
           MOVE 0 TO WS-INVNO.
           PERFORM ERROR-020
           MOVE 2410 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2110 TO POS
           DISPLAY "PRINTING OF INVOICES BY AREA....." AT POS.
           
           MOVE WS-AREA TO INCR-LY-AREA.
           START INCR-LY-REGISTER KEY NOT < INCR-LY-AREA
               INVALID KEY NEXT SENTENCE.
       PLY-004.
           IF WS-INCR-LY-ST1 NOT = 0
             MOVE "NO INVOICES LY WITH THAT AREA CODE, 'ESC' TO EXIT"
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE 88 TO WS-INCR-LY-ST1
             GO TO PLY-999.
       PLY-005.
           READ INCR-LY-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-LY-ST1 = 10 OR = 23
              GO TO PLY-000.
           IF WS-INCR-LY-ST1 NOT = 0
           MOVE "INVOICE-LY LOCKED BY ANOTHER TERMINAL, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO PLY-005.

           IF INCR-LY-TRANS NOT = 1
              GO TO PLY-005.
              
           IF INCR-LY-PULL-DATE NOT = 0
              GO TO PLY-005.
              
            IF INCR-LY-AREA NOT = WS-AREA
              GO TO PLY-900.
       PLY-008.
           IF WS-REP-ONLY NOT = " "
                PERFORM READ-DEBTOR
            IF WS-REP-ONLY NOT = DR-SALESMAN
                GO TO PLY-005.
           IF WS-DATE-ACCEPT NOT = " "
            IF INCR-LY-DATE NOT < WS-BEG-DATE
              GO TO PLY-010
            ELSE
              GO TO PLY-005.
       PLY-010.
           MOVE 2310 TO POS
           DISPLAY "Last Record Read:" AT POS
           ADD 20 TO POS
           DISPLAY INCR-LY-INVOICE AT POS.
       PLY-011.
           IF WS-LINE > 55
               PERFORM PRINT-HEADINGS.
               
           MOVE INCR-LY-INVOICE      TO D-INVNO
           MOVE INCR-LY-ACCOUNT      TO D-CUSTNO
           MOVE INCR-LY-NAME         TO D-NAME
           MOVE INCR-LY-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE         TO D-DATE
           MOVE INCR-LY-PORDER       TO D-PO
           MOVE INCR-LY-INVCRED-AMT  TO D-INVAMT
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 2
           MOVE " " TO PRINT-REC DETAIL-LINE.

           MOVE INCR-LY-DEL1     TO D-DELIVERY
           MOVE "-------"        TO D-SEQU-UNDER
           MOVE "-------"        TO D-PARCEL-UNDER
           MOVE "-------"        TO D-COD
           MOVE "-------"        TO D-SIG
           MOVE "-----!-----"    TO D-IN
           WRITE PRINT-REC FROM DELIVERY-LINE AFTER 1
           MOVE " " TO PRINT-REC DELIVERY-LINE.
           
           MOVE INCR-LY-DEL2           TO D-DELIVERY
           WRITE PRINT-REC FROM DELIVERY-LINE AFTER 1
           MOVE " " TO PRINT-REC DELIVERY-LINE.
           
           MOVE INCR-LY-DEL3           TO D-DELIVERY
           WRITE PRINT-REC FROM DELIVERY-LINE AFTER 1
           MOVE " " TO PRINT-REC DELIVERY-LINE.
       PLY-020.
           ADD INCR-LY-INVCRED-AMT TO WS-TOTI-INVOICE
           ADD 1                TO WS-INVNO
           
           MOVE 2410 TO POS
           MOVE WS-INVNO TO WS-DISPLAY-NO
           DISPLAY "Number of L/YR Inv Printed:" AT POS
           ADD 27 TO POS
           DISPLAY WS-DISPLAY-NO AT POS.

           ADD 5 TO WS-LINE
           GO TO PLY-005.
       PLY-900.
           PERFORM PRINT-TOTALS.
       PLY-999.
           EXIT.
      *
       PRINT-FROM-RANDOM SECTION.
       PR-000.
           CLOSE RANDOM-FILE.
           PERFORM OPEN-035.
           MOVE 0 TO WS-INVNO.
           PERFORM ERROR-020
           MOVE 2310 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2410 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2110 TO POS
           IF WS-TYPE = "I"
              DISPLAY "PRINTING OF INVOICE BY AREA....." AT POS
           ELSE
              DISPLAY "PRINTING OF P/SLIPS BY AREA....." AT POS.
       PR-005.
           READ RANDOM-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-RANDOM-ST1 = 10 OR = 23
              PERFORM PRINT-TOTALS
              GO TO PR-999.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "RANDOM FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PR-005.

           MOVE 2310 TO POS
           DISPLAY "Record Being Read:" AT POS
           ADD 20 TO POS
           DISPLAY RANDOM-NUMBER AT POS
           ADD 10 TO POS
           DISPLAY "AREA:" AT POS
           ADD 6 TO POS
           DISPLAY RANDOM-AREA AT POS.
           
           IF RANDOM-AREA NOT = WS-AREA
            IF WS-AREA NOT = " "
              PERFORM PRINT-TOTALS
              MOVE 0           TO WS-PAGE
              MOVE RANDOM-AREA TO WS-AREA
              PERFORM PRINT-HEADINGS
            ELSE
              MOVE RANDOM-AREA TO WS-AREA.
            
           IF RANDOM-ACCOUNT NOT = DR-ACCOUNT-NUMBER
              PERFORM READ-DEBTOR.
       PR-010.
           IF WS-LINE > 55
               PERFORM PRINT-HEADINGS.
               
           MOVE RANDOM-NUMBER  TO INCR-INVOICE
           IF WS-TYPE = "I"
              MOVE 1           TO INCR-TRANS
           ELSE
              MOVE 4           TO INCR-TRANS.
           PERFORM READ-REGISTER.
           
           MOVE INCR-INVOICE      TO D-INVNO
           MOVE INCR-ACCOUNT      TO D-CUSTNO
           MOVE INCR-NAME         TO D-NAME
           MOVE INCR-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-DATE
           MOVE INCR-PORDER       TO D-PO
           MOVE INCR-INVCRED-AMT  TO D-INVAMT
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 2
           MOVE " " TO PRINT-REC DETAIL-LINE.

           MOVE INCR-DEL1        TO D-DELIVERY
           MOVE "-------"        TO D-SEQU-UNDER
           MOVE "-------"        TO D-PARCEL-UNDER
           MOVE "-------"        TO D-COD
           MOVE "-------"        TO D-SIG
           MOVE "-----!-----"    TO D-IN
           WRITE PRINT-REC FROM DELIVERY-LINE AFTER 1
           MOVE " " TO PRINT-REC DELIVERY-LINE.
           
           MOVE INCR-DEL2           TO D-DELIVERY
           WRITE PRINT-REC FROM DELIVERY-LINE AFTER 1
           MOVE " " TO PRINT-REC DELIVERY-LINE.
           
           MOVE INCR-DEL3           TO D-DELIVERY
           WRITE PRINT-REC FROM DELIVERY-LINE AFTER 1
           MOVE " " TO PRINT-REC DELIVERY-LINE.
       PR-020.
           ADD INCR-INVCRED-AMT TO WS-TOTI-INVOICE
           ADD 1                TO WS-INVNO
           MOVE 2410 TO POS
           MOVE WS-INVNO TO WS-DISPLAY-NO
           IF WS-TYPE = "I"
              DISPLAY "Number of Invoices Printed:" AT POS
           ELSE
              DISPLAY "Number of P/Slips Printed :" AT POS.
           ADD 27 TO POS
           DISPLAY WS-DISPLAY-NO AT POS.
       PR-900.
           ADD 5 TO WS-LINE
           GO TO PR-005.
       PR-999.
           EXIT.
      *
       PRINT-FROM-LY-RANDOM SECTION.
       PRLY-000.
           CLOSE RANDOM-FILE.
           PERFORM OPEN-035.
           MOVE 0 TO WS-INVNO.
           PERFORM ERROR-020
           MOVE 2310 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2410 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2110 TO POS
           IF WS-TYPE = "I"
              DISPLAY "PRINTING OF INVOICE BY AREA....." AT POS
           ELSE
              DISPLAY "PRINTING OF P/SLIPS BY AREA....." AT POS.
       PRLY-005.
           READ RANDOM-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-RANDOM-ST1 = 10 OR = 23
              PERFORM PRINT-TOTALS
              GO TO PRLY-999.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "RANDOM LY FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PRLY-005.

           MOVE 2310 TO POS
           DISPLAY "Record Being Read:" AT POS
           ADD 20 TO POS
           DISPLAY RANDOM-NUMBER AT POS
           ADD 10 TO POS
           DISPLAY "AREA:" AT POS
           ADD 6 TO POS
           DISPLAY RANDOM-AREA AT POS.
           
           IF RANDOM-AREA NOT = WS-AREA
            IF WS-AREA NOT = " "
              PERFORM PRINT-TOTALS
              MOVE 0           TO WS-PAGE
              MOVE RANDOM-AREA TO WS-AREA
              PERFORM PRINT-HEADINGS
            ELSE
              MOVE RANDOM-AREA TO WS-AREA.
            
           IF RANDOM-ACCOUNT NOT = DR-ACCOUNT-NUMBER
              PERFORM READ-DEBTOR.
       PRLY-010.
           IF WS-LINE > 55
               PERFORM PRINT-HEADINGS.
               
           MOVE RANDOM-NUMBER  TO INCR-LY-INVOICE
           IF WS-TYPE = "I"
              MOVE 1           TO INCR-LY-TRANS
           ELSE
              MOVE 4           TO INCR-LY-TRANS.
           PERFORM READ-LY-REGISTER.
           
           MOVE INCR-LY-INVOICE      TO D-INVNO
           MOVE INCR-LY-ACCOUNT      TO D-CUSTNO
           MOVE INCR-LY-NAME         TO D-NAME
           MOVE INCR-LY-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE         TO D-DATE
           MOVE INCR-LY-PORDER       TO D-PO
           MOVE INCR-LY-INVCRED-AMT  TO D-INVAMT
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 2
           MOVE " " TO PRINT-REC DETAIL-LINE.

           MOVE INCR-LY-DEL1     TO D-DELIVERY
           MOVE "-------"        TO D-SEQU-UNDER
           MOVE "-------"        TO D-PARCEL-UNDER
           MOVE "-------"        TO D-COD
           MOVE "-------"        TO D-SIG
           MOVE "-----!-----"    TO D-IN
           WRITE PRINT-REC FROM DELIVERY-LINE AFTER 1
           MOVE " " TO PRINT-REC DELIVERY-LINE.
           
           MOVE INCR-LY-DEL2           TO D-DELIVERY
           WRITE PRINT-REC FROM DELIVERY-LINE AFTER 1
           MOVE " " TO PRINT-REC DELIVERY-LINE.
           
           MOVE INCR-LY-DEL3           TO D-DELIVERY
           WRITE PRINT-REC FROM DELIVERY-LINE AFTER 1
           MOVE " " TO PRINT-REC DELIVERY-LINE.
       PRLY-020.
           ADD INCR-LY-INVCRED-AMT TO WS-TOTI-INVOICE
           ADD 1                   TO WS-INVNO
           MOVE 2410 TO POS
           MOVE WS-INVNO TO WS-DISPLAY-NO
           IF WS-TYPE = "I"
              DISPLAY "Number of Invoices Printed:" AT POS
           ELSE
              DISPLAY "Number of P/Slips Printed:" AT POS.
           ADD 27 TO POS
           DISPLAY WS-DISPLAY-NO AT POS.
       PRLY-900.
           ADD 5 TO WS-LINE
           GO TO PRLY-005.
       PRLY-999.
           EXIT.
      *
       READ-REGISTER SECTION.
       RR-005.
           READ INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
              GO TO RR-999.
           IF WS-INCR-ST1 NOT = 0
            MOVE "INVOICE LOCKED BY ANOTHER TERMINAL, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RR-005.
       RR-999.
           EXIT.
      *
       READ-LY-REGISTER SECTION.
       RRLY-005.
           READ INCR-LY-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-LY-ST1 = 23 OR 35 OR 49
              GO TO RRLY-999.
           IF WS-INCR-LY-ST1 NOT = 0
           MOVE "INVOICE LY LOCKED BY ANOTHER TERMINAL, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RRLY-005.
       RRLY-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           MOVE WS-AREA TO H-CODE.
           IF WS-REP-ONLY = " "
               MOVE "ALL SALESMAN" TO H2-SALESMAN
           ELSE
               MOVE WS-REP-ONLY    TO H2-SALESMAN.
           ADD 1         TO WS-PAGE
           MOVE WS-PAGE  TO H-PAGE.
           
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC.
           
           MOVE WS-PRINT-BOLD   TO COMP-DIG1
           MOVE WS-PRINT-UNBOLD TO COMP-DIG2.
           IF WS-PAGE > 1
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE
               GO TO PH-500.
           IF WS-PAGE = 1
            IF WS-LINE = 66
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
       PH-500.
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           WRITE PRINT-REC FROM HEAD3 AFTER 2
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           MOVE 6 TO WS-LINE.
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
           
           ADD WS-INVNO TO WS-INVNO-PRINTED
           MOVE 0       TO WS-INVNO
                           WS-TOTI-INVOICE.
           PERFORM END-010.
       PT-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           MOVE 2910 TO POS
           DISPLAY "Opening files ........ " AT POS. 
           OPEN I-O INCR-LY-REGISTER.
           IF WS-INCR-LY-ST1 NOT = 0
              MOVE "REG-LY FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-020.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-020.
       OPEN-030.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE 0 TO WS-DEBTOR-ST1
              MOVE "DR MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-030.
       OPEN-0301.
            GO TO OPEN-036.
       OPEN-035.
           OPEN I-O RANDOM-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE
             "RANDOM FILE OPEN I-O BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-RANDOM-ST1
              GO TO OPEN-035.
       OPEN-036.
           OPEN OUTPUT RANDOM-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE
             "RANDOM FILE OPEN OUTPUT BUSY, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-RANDOM-ST1
              GO TO OPEN-036.

           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-050.
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
           IF WS-LINE > 59
               PERFORM PRINT-HEADINGS.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           WRITE PRINT-REC AFTER 1.
           GO TO END-020.
       END-010.
           IF WS-DATE-ACCEPT NOT = " "
               MOVE "ONLY TRANS. PRINTED FROM :" TO TOT2-DESC
               WRITE PRINT-REC FROM TOTAL-LINE2 AFTER 1.
       END-020.
           MOVE " " TO PRINT-REC TOTAL-LINE2.
           IF WS-INVNO-PRINTED = 0
              MOVE "NO TRANSACTIONS TO PRINT WITHIN GIVEN PARAMETERS."
               TO TOT2-LINE
               WRITE PRINT-REC FROM TOTAL-LINE2.
           
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-800.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE INCR-REGISTER
                 INCR-LY-REGISTER.
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
