        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrStatRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        REPOSITORY. 
           FUNCTION ALL INTRINSIC.
        SPECIAL-NAMES.
          CRT STATUS IS W-CRTSTATUS.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrTrans".
         Copy "SelectSlParameter".
         Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PDF-FILE ASSIGN TO WS-PDFPRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       COPY ChlfdDebtor.
       COPY ChlfdDrTrans.
       COPY ChlfdParam.
       COPY ChlfdDaily.
      *
       FD  PRINT-FILE.
       01  PRINT-REC               PIC X(132).
      *
       FD  PDF-FILE.
       01  PDF-REC                 PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-TRANSACTIONS      PIC 9(4) VALUE 0.
       77  WS-PDFFILE-OPENED    PIC X VALUE " ".
       77  WS-FOUND             PIC X VALUE " ".
       77  WS-TRANS-COUNT       PIC Z(3)9.
       77  WS-ACCPRINTED-COUNT  PIC 9(4) VALUE 0.
       77  WS-BF-BAL            PIC S9(8)V99 LEADING SEPARATE VALUE 0.
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-STATEDATE         PIC 9(8) VALUE 0.
       77  WS-COMMENTLINE       PIC X(30) VALUE " ".
       77  WS-STATE-COMMENT     PIC X(60) VALUE " ".
       77  WS-WORKTOTAL         PIC S9(8)V99 VALUE 0.
       77  WS-DEFERRED          PIC 9(8)99 VALUE 0.
       77  WS-BODY-LINE         PIC Z9.
       77  WS-PAGE              PIC 99 VALUE 0.
       77  WS-TOTAL-PAGES       PIC 9(5) VALUE 0.
       77  WS-TOTAL-PAGE-CNT    PIC Z(4)9.
       77  WS-CURRENT-MM        PIC 99 VALUE 0.
       77  WS-IMM-PRINT         PIC X VALUE " ".
       77  WS-LONG-FORMAT       PIC X VALUE " ".
       77  WS-MONTH-END         PIC X VALUE " ".
       77  WS-PRINTER-TYPE      PIC X VALUE " ".
       77  WS-BFORWARD-OPEN     PIC X VALUE " ".
       77  WS-ALL-DEB-CRED      PIC X VALUE " ".
       77  L-CNT                PIC 9(3) VALUE 0.
       77  WS-ACCNOBEGIN        PIC X(7) VALUE " ".
       77  WS-ACCNOEND          PIC X(7) VALUE " ".
       77  WS-LAST-DATE         PIC XX.
       77  WS-TERM-SUB          PIC 9 VALUE 0.    
       77  WS-TERMOFSALE        PIC X(11) VALUE " ".
       77  WS-EMAIL-NUMBER      PIC X(50) VALUE " ".
       77  WS-SUBJECT-FIXED     PIC X(100) VALUE " ".
       77  WS-PRINTER-PDF       PIC X(100) VALUE " ".
       88  WS-LAST-DAY          VALUES ARE "01" THRU "31".
       01  WS-TEMP-EMAIL-FILE   PIC X(50).
       01  W-CRTSTATUS          PIC 9(4) value 0.
       01  WS-EMAIL-STATEMENT.
           03  WS-ES-FIL        PIC X(15) VALUE "/ctools/eimage/".
           03  WS-EStatement    PIC X(100).
       01  WS-EMAIL-FINAL.
           03  WS-EF-FIL        PIC X(15) VALUE " ".
           03  WS-BAL-OF-NAME   PIC X(100).
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1       PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1      PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1  PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-SPOOL-STATUS.
           03  WS-SPOOL-ST1        PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  STORE-TERM.
         02  WS-TERM-OCCUR OCCURS 10.
           03  WS-ST-TYPE       PIC X.
           03  WS-ST-CODE       PIC X.
           03  WS-ST-TERM       PIC X(11).
       01  P2-DATE-SAVE         PIC X(10).
       01  WS-BF-DATE.
           03  WS-BF-YY         PIC 9999.
           03  WS-BF-MM         PIC 99.
           03  WS-BF-DD         PIC 99.
       01  WS-LAST-PERIOD-DATE.
           03  WS-LP-YY         PIC 9999.
           03  WS-LP-MM         PIC 99.
           03  WS-LP-DD         PIC 99.
       01  WS-SUBJECT.
           03  WS-SUBJECT-LINE1        PIC X(15) VALUE " ".
           03  WS-SUBJECT-LINE2        PIC 9(7).
           03  WS-SUBJECT-LINE3        PIC X(7) VALUE " ".
           03  WS-SUBJECT-LINE4        PIC X(40) VALUE " ".
       01  PDFLIST-LINE.
           03  PDF-GROUP-NUMBER PIC XX.
           03  FILLER           PIC X VALUE ",".
           03  PDF-TYPE         PIC X.
           03  FILLER           PIC X VALUE ",".
           03  PDF-NUMBER       PIC X(6).
           03  FILLER           PIC X VALUE ",".
           03  PDF-ACCOUNT      PIC X(7).
           03  FILLER           PIC X VALUE ",".
           03  PDF-NAME         PIC X(40).
           03  FILLER           PIC X VALUE ",".
           03  PDF-PORDER       PIC X(20).
           03  FILLER           PIC X VALUE ",".
           03  PDF-DATE         PIC X(10).
           03  FILLER           PIC X VALUE ",".
           03  PDF-TOTAL        PIC X(10).
           03  FILLER           PIC X VALUE ",".
       01  HEAD2.
           03  FILLER           PIC X(5) VALUE " ".
           03  P21-NAME         PIC X(65).
           03  P1-TEL           PIC X(21).
           03  P1-REMIT.
               05  P1-REM1         PIC X(15).
               05  P1-REM2         PIC X(41).
       01  PLINE1.
           03  FILLER           PIC X(5) VALUE " ".
           03  P1-NAME          PIC X(128) VALUE " ".
       01  PLINE2.
           03  FILLER           PIC X(5) VALUE " ".
           03  P2-NAME          PIC X(56) VALUE " ".
           03  P2-ACCOUNT-LHS   PIC 9(7).
           03  FILLER           PIC X(4) VALUE " ".
           03  P2-DATE-LHS      PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
           03  P2-PAGE-LHS      PIC Z9.
           03  FILLER           PIC X(15) VALUE " ".
           03  P2-ACCOUNT-RHS   PIC 9(7).
           03  FILLER           PIC X(2) VALUE " ".
           03  P2-DATE-RHS      PIC X(10).
           03  FILLER           PIC X(3) VALUE " ".
           03  P2-PAGE-RHS      PIC Z9.
           03  FILLER           PIC X(21) VALUE " ".
       01  PLINE3.
           03  FILLER           PIC X(5) VALUE " ".
           03  P3-ADDRESS       PIC X(25) VALUE " ".
           03  FILLER           PIC X(61) VALUE " ".
       01  PLINE4.
           03  FILLER           PIC X(5) VALUE " ".
           03  P4-POST-CODE     PIC 9(4).
           03  FILLER           PIC X(81) VALUE " ".
           03  P4-NAME          PIC X(42) VALUE " ".
       01  PLINE4-1.
           03  FILLER           PIC X(8) VALUE " ".
           03  P4-1-STATECOM    PIC X(124) VALUE " ".
       01  PLINE4-2.
           03  FILLER           PIC X(102) VALUE " ".
           03  P4-2-CODE        PIC X(30).
       01  PLINE5.
           03  FILLER           PIC X(3) VALUE " ".
           03  P5-DATE          PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  P5-ORDER-NO      PIC X(23).
           03  P5-REFERENCE-LHS PIC Z(5)9.
           03  FILLER           PIC X(2) VALUE " ".
           03  P5-CODE          PIC Z9.
           03  FILLER           PIC X(3) VALUE " ".
           03  P5-CHARGES       PIC Z(6)9.99-.
           03  FILLER           PIC X(1) VALUE " ".
           03  P5-PAYMENTS      PIC Z(6)9.99-.
           03  FILLER           PIC X(5) VALUE " ".
           03  P5-BALANCE-LHS   PIC Z(6)9.99-.
           03  FILLER           PIC X(4) VALUE " ".
           03  P5-REFERENCE-RHS PIC Z(5)9 BLANK WHEN ZERO.
           03  FILLER           PIC X(9) VALUE " ".
           03  P5-BALANCE-RHS   PIC Z(6)9.99- BLANK WHEN ZERO.
           03  FILLER           PIC X(14) VALUE " ".
       01  PLINE6.
           03  FILLER           PIC X(5) VALUE " ".
           03  FILLER           PIC X(3) VALUE "**".
           03  P6-TERMS         PIC X(12).
           03  FILLER           PIC X(5) VALUE "**".
           03  P6-CURRENT       PIC Z(6)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  P6-30DAY         PIC Z(6)9.99-.
           03  FILLER           PIC X(1) VALUE " ".
           03  P6-60DAY         PIC Z(6)9.99-.
           03  FILLER           PIC X(1) VALUE " ".
           03  P6-90DAY         PIC Z(6)9.99-.
           03  FILLER           PIC X(5) VALUE " ".
           03  P6-120DAY        PIC Z(6)9.99-.
           03  FILLER           PIC X(45) VALUE " ".
       01  PLINE7.
           03  FILLER              PIC X(75) VALUE " ".
           03  P7-BAL-ERROR        PIC X(3).
           03  P7-AMOUNT-TO-PAYLHS PIC Z(6)9.99-.
           03  FILLER              PIC X(14) VALUE " ".
           03  P7-BAL-ERROR1       PIC X(3).
           03  P7-AMOUNT-TO-PAYRHS PIC Z(6)9.99-.
           03  FILLER              PIC X(11) VALUE " ".
       01  PLINE8.
           03  FILLER           PIC X(50) VALUE " ".
           03  FILLER           PIC X(82) VALUE
           " ****  CONTINUED  **** ".
       01  PLINE9.
           03  FILLER           PIC X(49) VALUE " ".
           03  FILLER           PIC X(29) VALUE 
           "BALANCE BROUGHT FORWARD".
           03  P9-BAL-FORW-LHS  PIC Z(6)9.99-.
           03  FILLER           PIC X(19) VALUE " ".
           03  P9-BAL-FORW-RHS  PIC Z(6)9.99-.
           03  FILLER           PIC X(14) VALUE " ". 
       01 WS-FST-LINE.
          05  WS-DELIM-F             PIC  X(2).
          05  WS-DATA-F              PIC  X(86).
          05  WS-DELIM-END1          PIC  X(1).
       01 WS-OTH-LINE-1.
          05  WS-O-L                 PIC  X(8).
          05  WS-O-LINE              PIC  99.
          05  FILLER                 PIC  X(89).
       01 WS-OTH-LINE.
          05  WS-DELIM-O             PIC  X.
          05  WS-DATA-O              PIC  X(87).
          05  WS-DELIM-END2          PIC  X(1).
       01  LASER-PLINE1.
           03  PL1-CHAR         PIC X(2) VALUE " ".
           03  PL1-SPECIAL.
              04  PL1-NAME      PIC X(62).
              04  PL1-FAX.
                05  FILLER      PIC X VALUE " ".
                05  PL1-TEL     PIC X(23).
           03  PL1-END-CHAR     PIC X.
       01  LASER-PLINE2.
           03  PL2-CHAR         PIC X(2) VALUE " ".
           03  PL2-NAME         PIC X(52) VALUE " ".
           03  PL2-ACCOUNT      PIC X(14).
           03  PL2-DATE         PIC X(13).
           03  PL2-PAGE         PIC ZZZZ.
      *     03  PL2-PAGE         PIC Z(3)9.
           03  FILLER           PIC X(3) VALUE " ".
           03  PL2-END-CHAR     PIC X.
       01  LASER-PLINE3.
           03  PL3-CHAR         PIC X(2) VALUE " ".
           03  FILLER           PIC X(10) VALUE " ".
           03  FILLER           PIC X(65) VALUE 
           "BALANCE BROUGHT FORWARD".
           03  PL3-BAL-FORW     PIC Z(6)9.99-.
           03  PL3-END-CHAR     PIC X.
       01  LASER-PLINE4.
           03  PL4-CHAR         PIC X(2) VALUE " ".
           03  PL4-NAME         PIC X(46) VALUE " ".
           03  PL4-DEBTOR       PIC X(40).
           03  PL4-END-CHAR     PIC X.
       01  LASER-PLINE5.
           03  PL5-CHAR          PIC X(2) VALUE " ".
           03  PL5-DATE          PIC X(10).
           03  FILLER            PIC X(2) VALUE " ".
           03  PL5-ORDER-NO      PIC X(24).
           03  PL5-REFERENCE     PIC Z(5)9.
           03  FILLER            PIC X(4) VALUE " ".
           03  PL5-CODE          PIC Z9.
           03  FILLER            PIC X(3) VALUE " ".
           03  PL5-CHARGES       PIC Z(6)9.99-.
      *     03  FILLER            PIC X(1) VALUE " ".
           03  PL5-PAYMENTS      PIC Z(6)9.99-.
           03  FILLER            PIC X(2) VALUE " ".
           03  PL5-BALANCE       PIC Z(6)9.99-.
           03  PL5-END-CHAR      PIC X.
       01  LASER-PLINE6.
           03  PL6-CHAR         PIC X(2).
           03  FILLER           PIC X(3) VALUE "**".
           03  PL6-TERMS        PIC X(12).
           03  FILLER           PIC X(7) VALUE "**".
           03  PL6-CURRENT      PIC Z(6)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  PL6-30DAY        PIC Z(6)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  PL6-60DAY        PIC Z(6)9.99-.
           03  FILLER           PIC X(2) VALUE " ".
           03  PL6-90DAY        PIC Z(6)9.99-.
           03  FILLER           PIC X(3) VALUE " ".
           03  PL6-120DAY       PIC Z(6)9.99-.
           03  PL6-END-CHAR     PIC X.
       01  LASER-PLINE7.
           03  PL7-CHAR            PIC X(2) VALUE " ".
           03  FILLER              PIC X(51) VALUE " ".
           03  PL7-ACCOUNT         PIC X(10).
           03  PL7-DATE            PIC X(11).
           03  PL7-BAL-ERROR       PIC X(3).
           03  PL7-AMOUNT-TO-PAY   PIC Z(6)9.99-.
           03  PL7-END-CHAR        PIC X.
       01  LASER-PLINE8.
           03  PL8-CHAR         PIC X(2) VALUE " ".
           03  FILLER           PIC X(28) VALUE " ".
           03  FILLER           PIC X(25) VALUE
           " ****  Continued To Page ".
           03  PL8-PAGE         PIC Z9.
           03  FILLER           PIC X(31) VALUE " ****".
           03  PL8-END-CHAR     PIC X.
       01  LASER-BLANK-FOOTER.
           03  PLBF-CHAR         PIC X(2) VALUE " ".
           03  FILLER            PIC X(86) VALUE " ".
           03  PLBF-END-CHAR     PIC X.
       Copy "WsDateInfo".
       Copy "WStore".
       Copy "FServer".
      *
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONT-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM OPEN-SPOOLER-FILES.
           MOVE 0          TO WS-ACCPRINTED-COUNT
           MOVE "N"        TO WS-PDFFILE-OPENED.
           MOVE WS-YY      TO WS-LP-YY
           MOVE WS-MM      TO WS-LP-MM
           SUBTRACT 1    FROM WS-LP-MM.
           IF WS-LP-MM = 0
              MOVE 12      TO WS-LP-MM
              SUBTRACT 1 FROM WS-LP-YY.
       CONT-010.
      * NEXT LINE USED TO ZERO SUB-1 AND MOVE SPACES TO ALPHA-RATE
           Perform CDNVD-005.
           
           IF WS-PRINTER-TYPE = "1"
               MOVE "/ctools/spl/DrStateCo" To Alpha-Rate.
           IF WS-PRINTER-TYPE = "2"
               MOVE "/ctools/spl/DrLaserCo" To Alpha-Rate.
           IF WS-PRINTER-TYPE = "3"
               MOVE "/ctools/spl/DrEMailCo" To Alpha-Rate.
           IF WS-PRINTER-TYPE = "4"
               MOVE "/ctools/spl/DrNoMalCo" To Alpha-Rate.
      *     IF WS-PRINTER-TYPE = "5"
      *         PERFORM ADD-USERNAME-TO-FILE
      *         MOVE ALPHA-RATE         TO WS-ESTATEMENT
      *         PERFORM GET-EMAIL-STATEMENT-NAME
      *         MOVE WS-TEMP-EMAIL-FILE TO WS-PRINTER W-FILENAME
      *         GO TO CONT-030.
           IF WS-PRINTER-TYPE = "5" OR = "6"
               GO TO CONT-035.
               
      *NEXT LINE USED TO CHECK THE VALUE OF SUB-1 WHICH IS THE NAME
      * LENGTH PLUS 1 POS SO WE CAN ADD THE COMPANY NUMBER TO THE NAME
           Perform CDNVD-015

           MOVE WS-CO-NUMBER TO WS-COMPANY-DIGITS
           MOVE WS-CO-DIG1   TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1
           MOVE WS-CO-DIG2   TO AL-RATE (SUB-1).
           
           Move Alpha-Rate To Ws-Printer.
      *                        W-SpoolerFileSpec
      *                        W-FileName.
      *     Move Sub-1 To W-CbSpoolerFileSpec.
       CONT-030.
           IF WS-IMM-PRINT = "Y"
            IF WS-PRINTER-TYPE NOT = "1"
              PERFORM WORK-OUT-PRINT-FILE-NAME.
              
           OPEN OUTPUT PRINT-FILE.
           IF WS-SPOOL-ST1 NOT = 0
               MOVE "PRINT FILE OPEN ELSE WHERE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SPOOL-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO CONT-030.
       CONT-035.
           IF WS-PRINTER-TYPE = "5"
              PERFORM PRINT-PDF-ONLY
              CLOSE DEBTOR-MASTER
                    DEBTOR-TRANS-FILE
              GO TO CONT-950.

           IF WS-PRINTER-TYPE = "6"
              PERFORM PRINT-PDF-ONLY
              CLOSE DEBTOR-MASTER
                    DEBTOR-TRANS-FILE
              CLOSE PDF-FILE
              GO TO CONT-950.
              
           IF WS-PRINTER-TYPE = "1"
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC BEFORE PAGE
              PERFORM PRINT-STATEMENT
           ELSE
              PERFORM Z1-HEADINGS
              PERFORM PRINT-LASER-STATEMENT.
           IF WS-FOUND = " "
               MOVE 
             "NOTHING TO PRINT IN THAT RANGE, IN 2SEC'S GOING TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR-020.
               
           IF WS-PRINTER-TYPE = "1"
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC BEFORE PAGE
               MOVE " " TO PRINT-REC.
           CLOSE PRINT-FILE.
           CLOSE DEBTOR-MASTER
                 DEBTOR-TRANS-FILE.
           
           IF WS-FOUND = " "
            IF WS-PRINTER-TYPE = "5"
               PERFORM DELETE-BLANK-EMAIL-STATE-RECORD
               GO TO CONT-950.
       CONT-040.
           IF WS-PRINTER-TYPE = "5"
               PERFORM MOVE-EMAIL-FROM-EIMAGE-SETUP
               PERFORM MOVE-EMAIL-RECORD-FROM-EIMAGE
               GO TO CONT-950.
           IF WS-IMM-PRINT = "N"
              GO TO CONT-900.
           IF WS-FOUND = "Y"
              PERFORM CHECK-SPOOLER.
       CONT-900.
           IF WS-MONTH-END = "Y"
              MOVE "Statements Processed" TO WS-DAILY-1ST
              MOVE WS-TRANS-COUNT         TO WS-DAILY-2ND
              MOVE "Total Pages Printed:" TO WS-DAILY-3RD
              MOVE WS-TOTAL-PAGES         TO WS-TOTAL-PAGE-CNT
              MOVE WS-TOTAL-PAGE-CNT      TO WS-DAILY-4TH
              PERFORM WRITE-DAILY.
       CONT-950.
      *     STOP RUN.
           EXIT PROGRAM.
       CONT-999.
           EXIT.
      *
       MOVE-EMAIL-FROM-EIMAGE-SETUP SECTION.
       MERFES-005.
             MOVE WS-TEMP-EMAIL-FILE TO WS-EMAIL-FINAL.
             
             MOVE "/ctools/estate/" TO WS-EF-FIL.
       MERFES-999.
            EXIT.
      *
       GET-EMAIL-STATEMENT-NAME SECTION.
       GEQN-006.
            MOVE SPACES TO ALPHA-RATE DATA-RATE.
            MOVE "/ctools/eimage/" TO ALPHA-RATE.
            
            ACCEPT WS-USERNAME FROM ENVIRONMENT "USER".
            MOVE WS-USERNAME TO DATA-RATE.
            MOVE 16 TO SUB-45
            MOVE 1 TO SUB-46.
       GEQN-007.
            IF DAT-RATE (SUB-46) = " "
                MOVE 1 TO SUB-46
                MOVE SPACES TO DATA-RATE
                GO TO GEQN-010.
            MOVE DAT-RATE (SUB-46) TO AL-RATE (SUB-45).
            IF SUB-45 < 100
                ADD 1 TO SUB-45 SUB-46
                GO TO GEQN-007.
       GEQN-010.
            ACCEPT WS-DATE FROM DATE YYYYMMDD.
            MOVE WS-DATE TO DATA-RATE.
       GEQN-015.
            IF DAT-RATE (SUB-46) = " "
               MOVE 1 TO SUB-46
               MOVE SPACES TO DATA-RATE
               GO TO GEQN-020.
            MOVE DAT-RATE (SUB-46) TO AL-RATE (SUB-45).
            IF SUB-45 < 100
                ADD 1 TO SUB-45 SUB-46
                GO TO GEQN-015.
       GEQN-020.
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-TIME TO DATA-RATE.
       GEQN-025.
            IF DAT-RATE (SUB-46) = " "
               MOVE 1 TO SUB-46
               MOVE SPACES TO DATA-RATE
               GO TO GEQN-030.
            MOVE DAT-RATE (SUB-46) TO AL-RATE (SUB-45).
            IF SUB-45 < 100
                ADD 1 TO SUB-45 SUB-46
                GO TO GEQN-025.
       GEQN-030.
            MOVE "tmp" TO DATA-RATE.
       GEQN-035.
            IF DAT-RATE (SUB-46) = " "
               MOVE 1 TO SUB-46
               MOVE SPACES TO DATA-RATE
               GO TO GEQN-040.
            MOVE DAT-RATE (SUB-46) TO AL-RATE (SUB-45).
            IF SUB-45 < 100
                ADD 1 TO SUB-45 SUB-46
                GO TO GEQN-035.
       GEQN-040.
            MOVE ALPHA-RATE TO WS-TEMP-EMAIL-FILE.
            
      *      MOVE WS-TEMP-EMAIL-FILE TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.

       GEQN-999.
           EXIT.
      *
       ADD-USERNAME-TO-FILE SECTION.
       AUNTF-000.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
           ACCEPT WS-USERNAME FROM ENVIRONMENT "USER".
       AUNTF-005.
           MOVE WS-USERNAME       TO ALPHA-RATE.
           MOVE 1  TO SUB-1
           MOVE 1  TO SUB-2.
       AUNTF-006.
           IF AL-RATE (SUB-1) NOT = " "
                ADD 1 TO SUB-1
            IF SUB-1 < 100
               GO TO AUNTF-006.
       AUNTF-008.
           MOVE 1             TO SUB-2.
           MOVE WS-ACCNOBEGIN TO DATA-RATE.
       AUNTF-010.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
               GO TO AUNTF-010.
       AUNTF-999.
           EXIT.
      *
       WORK-OUT-PRINT-FILE-NAME SECTION.
       WOPFN-001.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
           ACCEPT WS-USERNAME FROM ENVIRONMENT "USER".
      *     
      *     MOVE "IN WOPFN-001." TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       WOPFN-005.
           MOVE "/ctools/spl/" TO ALPHA-RATE.
           MOVE WS-USERNAME    TO DATA-RATE.
           MOVE 13 TO SUB-1
           MOVE 1  TO SUB-2.
       WOPFN-010.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
               GO TO WOPFN-010.
       WOPFN-020.
           MOVE SPACES TO DATA-RATE.
           IF WS-PRINTER-TYPE = "1"
               MOVE "DrStateCo" To DATA-Rate.
           IF WS-PRINTER-TYPE = "2"
               MOVE "DrLaserCo" To DATA-Rate.
           IF WS-PRINTER-TYPE = "3"
               MOVE "DrEMailCo" To DATA-Rate.
           IF WS-PRINTER-TYPE = "4"
               MOVE "DrNoMalCo" To DATA-Rate.
               
           MOVE 1            TO SUB-2.
       WOPFN-025.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
               GO TO WOPFN-025.
       WOPFN-030.
           MOVE SPACES TO DATA-RATE.
           MOVE WS-CO-NUMBER TO DATA-RATE.
           MOVE 1 TO SUB-2.
       WOPFN-035.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
               GO TO WOPFN-035.
           MOVE ALPHA-RATE   TO WS-PRINTER W-FILENAME.
       WOPFN-999.
            EXIT.
      *
       WORK-OUT-CSV-FILE-NAME SECTION.
       WOCSV-001.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
       WOCSV-005.
           MOVE "/ctools/pdf" TO ALPHA-RATE.
           MOVE WS-CO-NUMBER  TO DATA-RATE.
           MOVE 12 TO SUB-1
           MOVE 1  TO SUB-2.
       WOCSV-010.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
               GO TO WOCSV-010.
           MOVE "/" TO AL-RATE (SUB-1).
           ADD 1 TO SUB-1.
       WOCSV-020.
      *     MOVE "IN WOCSV-020." TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

           MOVE SPACES        TO DATA-RATE.
           MOVE "PDFState.csv" TO DATA-RATE.
           MOVE 1  TO SUB-2.
       WOCSV-025.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
               GO TO WOCSV-025.
       WOCSV-030.
           MOVE ALPHA-RATE   TO WS-PDFPRINTER.
           
      *     MOVE WS-PDFPRINTER TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       WOCSV-999.
            EXIT.
      *
       WORK-OUT-PDF-FILE-NAME SECTION.
       WOPDF-001.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
       WOPDF-005.
           MOVE "/ctools/pdf" TO ALPHA-RATE.
           MOVE WS-CO-NUMBER  TO DATA-RATE.
           MOVE 12 TO SUB-1
           MOVE 1  TO SUB-2.
       WOPDF-010.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
               GO TO WOPDF-010.
           MOVE "/" TO AL-RATE (SUB-1).
           ADD 1 TO SUB-1.
           MOVE "S" TO AL-RATE (SUB-1).
           ADD 1 TO SUB-1.
       WOPDF-020.
      *     MOVE "IN WOPDF-020." TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

           MOVE SPACES     TO DATA-RATE.
           MOVE DR-ACCOUNT-NUMBER TO DATA-RATE.
           MOVE 1  TO SUB-2.
       WOPDF-025.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
               GO TO WOPDF-025.
       WOPDF-030.
           MOVE ALPHA-RATE    TO WS-PRINTER W-FILENAME.
       WOPDF-999.
            EXIT.
      *
       MAKE-PDF-FINAL-FOR-EMAIL SECTION.
       MFPFE-001.
           MOVE SPACES           TO ALPHA-RATE DATA-RATE.
       MFPFE-002.
           MOVE WS-PRINTER       TO DATA-RATE
           MOVE 23 TO SUB-46.
       MFPFE-010.
           MOVE "."  TO DAT-RATE (SUB-46)
                ADD 1 TO SUB-46.
           MOVE "p"  TO DAT-RATE (SUB-46)
                ADD 1 TO SUB-46.
           MOVE "d"  TO DAT-RATE (SUB-46)
                ADD 1 TO SUB-46.
           MOVE "f"  TO DAT-RATE (SUB-46)
                ADD 1 TO SUB-46.
           MOVE DATA-RATE TO WS-PRINTER-PDF.

      *   MOVE WS-PRINTER-PDF TO WS-MESSAGE
      *   PERFORM ERROR-MESSAGE.

           MOVE SPACES    TO ALPHA-RATE DATA-RATE.
       MFPFE-999.
           EXIT.
      *
       WORK-OUT-EMAIL-PDF-FILE-NAME SECTION.
       WOEPDF-001.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
       WOEPDF-005.
           MOVE "/ctools/fax/" TO ALPHA-RATE.
           MOVE WS-CO-NUMBER  TO DATA-RATE.
           MOVE 13 TO SUB-1
           MOVE 1  TO SUB-2.
       WOEPDF-010.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
               GO TO WOEPDF-010.
      *     MOVE "/" TO AL-RATE (SUB-1).
      *     ADD 1 TO SUB-1.
           MOVE "S" TO AL-RATE (SUB-1).
           ADD 1 TO SUB-1.
       WOEPDF-020.
      *     MOVE "IN WOEPDF-020." TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

           MOVE SPACES     TO DATA-RATE.
           MOVE DR-ACCOUNT-NUMBER TO DATA-RATE.
           MOVE 1  TO SUB-2.
       WOEPDF-025.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
               GO TO WOEPDF-025.
       WOEPDF-030.
           MOVE ALPHA-RATE    TO WS-PRINTER W-FILENAME.
       
      *     MOVE WS-PRINTER TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       
       WOEPDF-999.
            EXIT.
      *
       WRITE-INDEX-KEY SECTION.
       WIK-001.
           IF WS-PDFFILE-OPENED = "Y"
              GO TO WIK-005.
           PERFORM WORK-OUT-CSV-FILE-NAME
           OPEN OUTPUT PDF-FILE.
           MOVE 
           "#GROUP NUM, TYPE, TRANS NUM, ACCOUNT NUM, ACCOUNT NAME, " &
           "PORDER NUM, DATE, TRANS TOTAL." 
                TO PDF-REC
           WRITE PDF-REC AFTER 1.
           MOVE "Y" TO WS-PDFFILE-OPENED.
       WIK-005.
           MOVE WS-CO-NUMBER      TO PDF-GROUP-NUMBER.
           MOVE "S"               TO PDF-TYPE.
           MOVE " "               TO PDF-NUMBER.
           MOVE DR-NAME           TO PDF-NAME.
           MOVE DR-ACCOUNT-NUMBER TO PDF-ACCOUNT.
           MOVE " "               TO PDF-PORDER.
           MOVE P2-DATE-LHS       TO PDF-DATE.
           MOVE PL7-AMOUNT-TO-PAY TO PDF-TOTAL.
      * SEE PL-ADD4 MOVE TO PDF-TOTAL IN LASER-PRINT SECTION
      *     MOVE INCR-INVCRED-AMT  TO PDF-TOTAL.
           
      *     MOVE PDFLIST-LINE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
           WRITE PDF-REC FROM PDFLIST-LINE AFTER 1.
       WIK-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO WS-FOUND.
            MOVE "ACCNOBEGIN" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-900.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ACCNOBEGIN.
       GET-010.
            MOVE "ACCNOEND" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-000.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ACCNOEND.
       GET-030.
            MOVE "BFORWARDOPEN" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-010.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-BFORWARD-OPEN.
            IF WS-BFORWARD-OPEN = "B"
               GO TO GET-040.
            IF WS-BFORWARD-OPEN = "O"
               GO TO GET-050.
            GO TO GET-030.
       GET-040.
            MOVE "LASTDATE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-030.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-LAST-DATE.
            IF WS-LAST-DATE = "0 " OR = " 0" OR = "00" OR = "  "
               GO TO GET-040.
            IF WS-LAST-DATE < "32"
               MOVE WS-LAST-DATE TO WS-LP-DD
               GO TO GET-050
            ELSE
               GO TO GET-040.
       GET-050.
            MOVE "ALLDEBCRED" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               IF WS-BFORWARD-OPEN = "B"
               GO TO GET-040
            ELSE
               GO TO GET-030.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ALL-DEB-CRED.
            IF WS-ALL-DEB-CRED  = "A" OR = "D" OR = "C"
              GO TO GET-060.
            GO TO GET-050.
       GET-060.
            MOVE "STATECOMMENT" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-050.
            MOVE 60 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA60.
            MOVE F-NAMEFIELD60 TO WS-STATE-COMMENT.
       GET-065.
            MOVE "SPOOL" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-060.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-IMM-PRINT.
            IF WS-IMM-PRINT = "Y" OR = "N"
               GO TO GET-067.
            GO TO GET-065.
       GET-067.
            MOVE "LONG" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-065.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-LONG-FORMAT.
            IF WS-LONG-FORMAT = "Y" OR = "N"
               GO TO GET-068
            GO TO GET-067.
       GET-068.
            MOVE "M-END" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-067.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-MONTH-END.
            IF WS-MONTH-END = "Y" OR = "N"
               GO TO GET-069.
            GO TO GET-068.
       GET-069.
      ******************************************************************
      * PRINTER TYPES                                                  *
      * TYPE 1=DOTMATRIX                                               *
      *      2=LASER, ALL STATEMENTS PRINTED EVEN IF THEY HAVE AN EMAIL*
      *      3=LASER, E-MAIL ONLY, NO PRINT OF STATEMENTS              *
      *      4=LASER, ALL E-MAILABLE STATEMENTS IGNORED IN THIS PRINT  *
      *      5=laser format but to /estat/, Once off statements        *
      *      1=DrStateCoxx  2=DrLaserCoxx  3=DrEmailCoxx  4=DrNoMalCoxx*
      *      5=Account Number = The File Name                          *
      *      6=.Pdf creation of statements for DocuSearch purposes.    *
      *         this replaces the COLD format files created to send to *
      *         Document Warehouse 22/9/2016                           *
      ******************************************************************
           MOVE "P-TYPE" TO F-FIELDNAME
           MOVE 6        TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
              GO TO GET-068.
           MOVE 1           TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO WS-PRINTER-TYPE.
           IF WS-LONG-FORMAT = "Y"
            IF WS-PRINTER-TYPE = "2" OR = "3" OR = "4" OR = "5" or = "6"
              MOVE
             "YOU CAN'T PRINT LONG FORMAT TO A LASER PRINTER, RE-ENTER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO GET-069.
           IF WS-PRINTER-TYPE = "1" OR = "2" OR = "3" OR = "4"
                           OR = "5" OR = "6"
              GO TO GET-070.
           GO TO GET-069.
       GET-070.
           IF WS-PRINTER-TYPE = "5"
            IF WS-IMM-PRINT = "Y"
              OR WS-LONG-FORMAT = "Y"
               OR WS-MONTH-END = "Y"
                 MOVE
       "FOR PRINTER OPTION 5 YOU MUST ANSWER 'N' TO ALL ABOVE QUESTONS."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-065.
      * NEXT PHRASE ENTERED TO NOT ALLOW THE SENDING OF ACCOUNTS
      * WHERE THERE IS NO EMAIL ADDRESS ON THE DEBTOR MASTER FILE     
      * AT THE MONTHEND RUN.  IF ONE ENTERS THE SAME ACCOUNT FOR
      * BEGIN AND END ACCOUNTS THEN THE PROGRAM WILL ASK THE USER
      * TO ENTER AN EMAIL ADDRESS.
           IF WS-PRINTER-TYPE = "5"
            IF WS-ACCNOEND > WS-ACCNOBEGIN
                MOVE "Y" TO WS-MONTH-END.
       GET-075.
           MOVE WS-ACCNOBEGIN TO DRTR-ACCOUNT-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
               MOVE "DR TRANS FILE MISSING ON START" TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               PERFORM END-OFF.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DEBTOR TRANS. FILE BUSY ON START." TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO GET-075.
       GET-999.
            EXIT.
      *
       PRINT-STATEMENT SECTION.
      * THIS SECTION USED TO PRINT STATEMENTS ON DOT MATRIX PRINTER
       PR-000.
           MOVE WS-ACCNOBEGIN TO DRTR-ACCOUNT-NUMBER.
           MOVE 0             TO DRTR-DATE.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 NOT = 0
              GO TO PR-999.
       PR-001.
           MOVE 0 TO WS-BF-BAL.
           READ DEBTOR-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
      * 10=END OF FILE, 23=KEY NOT FOUND, 46=READ ERC
           IF WS-DRTRANS-ST1 = 10 OR = 23 OR = 46
              GO TO PR-999.
           IF WS-DRTRANS-ST1 = 91
              MOVE 10 TO WS-DRTRANS-ST1
              MOVE " " TO DEBTOR-TRANS-REC
              GO TO PR-999.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DRTRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO PR-001.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           IF DRTR-ACCOUNT-NUMBER < WS-ACCNOBEGIN
              GO TO PR-001.
           IF DRTR-ACC-KEY = SPACES OR DRTR-ACCOUNT-NUMBER = 0
              GO TO PR-001.
       PR-005.
           IF DRTR-ACCOUNT-NUMBER > WS-ACCNOEND
              GO TO PR-999.
            MOVE DRTR-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
            READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               GO TO PR-001.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PR-005.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
            IF DR-BALANCE = 0
             IF DR-BAL-LAST-STATE = 0
              IF DR-SALES-PTD = 0
               IF DR-COST-PTD = 0
               GO TO PR-001.
            IF WS-ALL-DEB-CRED = "A"
               GO TO PR-008.
            IF WS-ALL-DEB-CRED = "D"
               AND DR-BALANCE > 0
               GO TO PR-008.
            IF WS-ALL-DEB-CRED = "C"
               AND DR-BALANCE < 0
               GO TO PR-008.
            GO TO PR-001.
       PR-008.
           MOVE DRTR-DATE TO WS-BF-DATE.
           IF WS-BFORWARD-OPEN = "O"
              GO TO PR-009.
           IF WS-BF-DATE < WS-LAST-PERIOD-DATE
           OR WS-BF-DATE = WS-LAST-PERIOD-DATE
              GO TO PR-0080.
           GO TO PR-009.
       PR-0080.
           IF DRTR-TYPE = 1 OR = 3 OR = 4 OR = 7 OR = 10
             IF WS-BFORWARD-OPEN = "O"
              ADD DRTR-AMT-OUTSTANDING TO WS-BF-BAL
             ELSE
              ADD DRTR-AMT-OF-INVOICE TO WS-BF-BAL.
           IF DRTR-TYPE = 2 OR = 5 OR = 6 OR = 8 OR = 9 OR = 11
             IF WS-BFORWARD-OPEN = "O"
              SUBTRACT DRTR-AMT-OUTSTANDING FROM WS-BF-BAL
             ELSE
              SUBTRACT DRTR-AMT-OF-INVOICE FROM WS-BF-BAL.
           IF WS-BFORWARD-OPEN = "B"
              PERFORM READ-DEBTOR-TRANS.
           IF WS-DRTRANS-ST1 = 10
               GO TO PR-009.
           IF DRTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
              IF WS-BF-DATE < WS-LAST-PERIOD-DATE
              OR WS-BF-DATE = WS-LAST-PERIOD-DATE
                 GO TO PR-008.
       PR-009.
           IF WS-BFORWARD-OPEN = "O"
               MOVE WS-BF-BAL TO P9-BAL-FORW-LHS
                                 WS-WORKTOTAL
            ELSE
               MOVE DR-BAL-LAST-STATE TO P9-BAL-FORW-LHS
                                         P9-BAL-FORW-RHS
                                         WS-WORKTOTAL.
            MOVE 1 TO WS-PAGE
            MOVE 0 TO WS-DEFERRED.
       PR-010.
            MOVE PA-NAME      TO P21-NAME P1-REMIT
            MOVE PA-PHONE     TO P1-TEL
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE PA-ADD1      TO P21-NAME P1-REMIT
            MOVE PA-FAX       TO P1-TEL
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE PA-ADD2      TO P21-NAME P1-REMIT
            MOVE PA-CO-REG-NO TO P1-TEL
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE PA-CO-VAT-NO TO P1-TEL
            MOVE PA-ADD3      TO P21-NAME P1-REM1
            MOVE PA-CODE      TO P1-REM2
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC HEAD2
            MOVE PA-CODE      TO P21-NAME
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC HEAD2
            WRITE PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC
            MOVE " " TO PRINT-REC.

            MOVE DR-NAME     TO P1-NAME
            WRITE PRINT-REC FROM PLINE1 AFTER 1
            MOVE DR-ADDRESS1 TO P2-NAME
            MOVE DR-ACCOUNT-NUMBER TO P2-ACCOUNT-LHS
                                      P2-ACCOUNT-RHS
            MOVE P2-DATE-SAVE  TO P2-DATE-LHS P2-DATE-RHS
            MOVE WS-PAGE       TO P2-PAGE-LHS P2-PAGE-RHS
            WRITE PRINT-REC FROM PLINE2 AFTER 1
            MOVE " " TO PRINT-REC PLINE2
            MOVE DR-ADDRESS2 TO P2-NAME
            WRITE PRINT-REC FROM PLINE2 AFTER 1
            MOVE " " TO PRINT-REC
            MOVE " " TO PRINT-REC PLINE3
            MOVE DR-ADDRESS3 TO P3-ADDRESS
            WRITE PRINT-REC FROM PLINE3 AFTER 1
            MOVE " " TO PRINT-REC PLINE3
            MOVE DR-POST-CODE TO P4-POST-CODE
            MOVE DR-NAME      TO P4-NAME
            WRITE PRINT-REC FROM PLINE4 AFTER 1
            MOVE " " TO PRINT-REC PLINE4
            WRITE PRINT-REC
            MOVE " " TO PRINT-REC.

            WRITE PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC.

            MOVE WS-STATE-COMMENT TO P4-1-STATECOM
            WRITE PRINT-REC FROM PLINE4-1
            MOVE " " TO PRINT-REC PLINE4-1
            WRITE PRINT-REC
            MOVE 1 TO L-CNT.
            IF WS-PAGE = 1 
              AND WS-BFORWARD-OPEN = "B"
                WRITE PRINT-REC FROM PLINE9 AFTER 1
                MOVE " " TO PRINT-REC
                ADD 1 TO L-CNT.
       PR-020.
            IF WS-BFORWARD-OPEN = "O"
              GO TO PR-022.
            IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
                GO TO PR-040.
            MOVE DRTR-DATE       TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE    TO P5-DATE
            MOVE DRTR-REFERENCE1 TO P5-ORDER-NO
            MOVE DRTR-REFERENCE2 TO P5-REFERENCE-LHS
                                    P5-REFERENCE-RHS
            MOVE DRTR-TYPE       TO P5-CODE.
            IF DRTR-TYPE = 1 OR = 3 OR = 4 OR = 7 OR = 10
               MOVE DRTR-AMT-OF-INVOICE TO P5-CHARGES
               ADD DRTR-AMT-OF-INVOICE  TO WS-WORKTOTAL
            ELSE
               MOVE DRTR-AMT-OF-INVOICE       TO P5-PAYMENTS
               SUBTRACT DRTR-AMT-OF-INVOICE FROM WS-WORKTOTAL.
            MOVE WS-WORKTOTAL                 TO P5-BALANCE-LHS.
            IF DRTR-TYPE = 1 OR = 3 OR = 4 OR = 7 OR = 10
               MOVE DRTR-AMT-OUTSTANDING TO P5-BALANCE-RHS
            ELSE 
               MOVE 0 TO P5-BALANCE-RHS
                         P5-REFERENCE-RHS.
            WRITE PRINT-REC FROM PLINE5 AFTER 1
            MOVE " " TO PRINT-REC PLINE5
            ADD 1 TO L-CNT.
       PR-022.
            IF WS-BFORWARD-OPEN = "B"
              GO TO PR-025.
            IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
                GO TO PR-040.
            MOVE DRTR-DATE       TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE    TO P5-DATE
            MOVE DRTR-REFERENCE1 TO P5-ORDER-NO
            MOVE DRTR-REFERENCE2 TO P5-REFERENCE-LHS
                                    P5-REFERENCE-RHS
            MOVE DRTR-TYPE       TO P5-CODE.
            IF DRTR-TYPE = 1 OR = 3 OR = 4 OR = 7 OR = 10
               MOVE DRTR-AMT-OUTSTANDING TO P5-CHARGES
               ADD DRTR-AMT-OUTSTANDING  TO WS-WORKTOTAL
            ELSE
               MOVE DRTR-AMT-OUTSTANDING       TO P5-PAYMENTS
               SUBTRACT DRTR-AMT-OUTSTANDING FROM WS-WORKTOTAL.
            MOVE WS-WORKTOTAL                  TO P5-BALANCE-LHS.
            IF DRTR-TYPE = 1 OR = 3 OR = 4 OR = 7 OR = 10
               MOVE DRTR-AMT-OUTSTANDING TO P5-BALANCE-RHS
            ELSE 
               MOVE 0 TO P5-BALANCE-RHS
                         P5-REFERENCE-RHS.
            WRITE PRINT-REC FROM PLINE5 AFTER 1.
            MOVE " " TO PRINT-REC PLINE5.
            ADD 1 TO L-CNT.
       PR-025.
            PERFORM READ-DEBTOR-TRANS.
            IF WS-DRTRANS-ST1 = 10
              MOVE " " TO DEBTOR-TRANS-REC
               PERFORM PRINT-TRAILING
               GO TO PR-999.
            IF DRTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
                GO TO PR-030.
            IF DRTR-ACCOUNT-NUMBER > WS-ACCNOEND
               PERFORM PRINT-TRAILING
               GO TO PR-999.
            GO TO PR-040.
       PR-030.
            IF WS-LONG-FORMAT = "N"
             IF L-CNT = 15
                WRITE PRINT-REC FROM PLINE8 AFTER 1
                MOVE " " TO PRINT-REC
                WRITE PRINT-REC
                ADD 1 TO WS-PAGE
                         WS-TOTAL-PAGES
                WRITE PRINT-REC AFTER PAGE
                GO TO PR-010.
            IF WS-LONG-FORMAT = "N"
             IF L-CNT < 15
                GO TO PR-020.
            IF WS-LONG-FORMAT = "Y"
                GO TO PR-020.
       PR-040.
            PERFORM PRINT-TRAILING
            MOVE 0 TO WS-BF-BAL
            GO TO PR-005.
       PR-999.
            EXIT.
      *
       PRINT-TRAILING SECTION.
       PT-000.
           IF L-CNT < 16
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              ADD 1 TO L-CNT
              GO TO PT-000.
           IF WS-PAGE > 1 OR WS-PAGE = 1 OR WS-BFORWARD-OPEN = "O"
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC AFTER 1.
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC
           MOVE DR-TERMS-CODE            TO WS-TERM-SUB
           MOVE WS-ST-TERM (WS-TERM-SUB) TO WS-TERMOFSALE
           MOVE WS-TERMOFSALE            TO P6-TERMS
           MOVE DR-CURRENT               TO P6-CURRENT
           MOVE DR-30DAY                 TO P6-30DAY
           MOVE DR-60DAY                 TO P6-60DAY
           MOVE DR-90DAY                 TO P6-90DAY
           MOVE DR-120DAY                TO P6-120DAY
           WRITE PRINT-REC FROM PLINE6 AFTER 1
           MOVE " " TO PRINT-REC
      *     MOVE WS-WORKTOTAL TO P7-AMOUNT-TO-PAYLHS
      *                          P7-AMOUNT-TO-PAYRHS
           MOVE DR-BALANCE   TO P7-AMOUNT-TO-PAYLHS
                                P7-AMOUNT-TO-PAYRHS.
           IF DR-BALANCE NOT = WS-WORKTOTAL
               MOVE "***"                TO P7-BAL-ERROR
                                            P7-BAL-ERROR1
           ELSE
               MOVE "ZAR"                TO P7-BAL-ERROR
                                            P7-BAL-ERROR1.
                                
           WRITE PRINT-REC FROM PLINE7 AFTER 2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC AFTER PAGE.
       PT-010.
           IF WS-FOUND = " "
               MOVE "Y" TO WS-FOUND.

           MOVE 2922 TO POS
           DISPLAY "No Of Statements Processed =" AT POS
           ADD 1 TO WS-ACCPRINTED-COUNT
                    WS-TOTAL-PAGES
           MOVE WS-ACCPRINTED-COUNT TO WS-TRANS-COUNT
           MOVE WS-TRANS-COUNT TO WS-MESSAGE1
           MOVE 2951 TO POS
           DISPLAY WS-MESSAGE1 AT POS.
       PT-999.
           EXIT.
      *
       PRINT-LASER-STATEMENT SECTION.
      * THIS SECTION USED TO PRINT STATEMENTS ON LASER PRINTER
       PRL-000.
           MOVE WS-ACCNOBEGIN TO DRTR-ACCOUNT-NUMBER.
           MOVE 0             TO DRTR-DATE.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 NOT = 0
              GO TO PRL-999.
       PRL-001.
           MOVE 0 TO WS-BF-BAL.
           READ DEBTOR-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
      * 10=END OF FILE, 23=KEY NOT FOUND, 46=READ ERC
           IF WS-DRTRANS-ST1 = 10 OR = 23 OR = 46
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              GO TO PRL-999.
           IF WS-DRTRANS-ST1 = 91
              MOVE 10 TO WS-DRTRANS-ST1
              MOVE " " TO DEBTOR-TRANS-REC
              GO TO PRL-999.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DRTRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DRTRANS-ST1
              GO TO PRL-001.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           IF DRTR-ACCOUNT-NUMBER < WS-ACCNOBEGIN
              GO TO PRL-001.
           IF DRTR-ACC-KEY = SPACES OR DRTR-ACCOUNT-NUMBER = 0
              GO TO PRL-001.
       PRL-005.
           IF DRTR-ACCOUNT-NUMBER > WS-ACCNOEND
              MOVE " " TO PRINT-REC
      *        WRITE PRINT-REC AFTER 1
              GO TO PRL-999.
            MOVE DRTR-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
            
            READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               GO TO PRL-001.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PRL-005.
               
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
              
            IF DR-BALANCE = 0
             IF DR-BAL-LAST-STATE = 0
              IF DR-SALES-PTD = 0
               IF DR-COST-PTD = 0
               GO TO PRL-001.
            IF WS-ALL-DEB-CRED = "A"
               GO TO PRL-008.
            IF WS-ALL-DEB-CRED = "D"
               AND DR-BALANCE > 0
               GO TO PRL-008.
            IF WS-ALL-DEB-CRED = "C"
               AND DR-BALANCE < 0
               GO TO PRL-008.
            GO TO PRL-001.
       PRL-008.
           MOVE DRTR-DATE TO WS-BF-DATE.
           IF WS-BFORWARD-OPEN = "O"
              GO TO PRL-009.
           IF WS-BF-DATE < WS-LAST-PERIOD-DATE
           OR WS-BF-DATE = WS-LAST-PERIOD-DATE
              GO TO PRL-0080.
           GO TO PRL-009.
       PRL-0080.
           IF DRTR-TYPE = 1 OR = 3 OR = 4 OR = 7 OR = 10
             IF WS-BFORWARD-OPEN = "O"
              ADD DRTR-AMT-OUTSTANDING TO WS-BF-BAL
             ELSE
              ADD DRTR-AMT-OF-INVOICE TO WS-BF-BAL.
           IF DRTR-TYPE = 2 OR = 5 OR = 6 OR = 8 OR = 9 OR = 11
             IF WS-BFORWARD-OPEN = "O"
              SUBTRACT DRTR-AMT-OUTSTANDING FROM WS-BF-BAL
             ELSE
              SUBTRACT DRTR-AMT-OF-INVOICE FROM WS-BF-BAL.
           IF WS-BFORWARD-OPEN = "B"
              PERFORM READ-DEBTOR-TRANS.
           IF WS-DRTRANS-ST1 = 10
               GO TO PRL-009.
           IF DRTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
              IF WS-BF-DATE < WS-LAST-PERIOD-DATE
              OR WS-BF-DATE = WS-LAST-PERIOD-DATE
                 GO TO PRL-008.
       PRL-009.
           IF WS-BFORWARD-OPEN = "O"
               MOVE WS-BF-BAL TO PL3-BAL-FORW
                                 WS-WORKTOTAL
            ELSE
               MOVE DR-BAL-LAST-STATE TO PL3-BAL-FORW
                                         WS-WORKTOTAL.
            MOVE 1 TO WS-PAGE
            MOVE 0 TO WS-DEFERRED.
       PRL-010.
      ***************************************
      *STATEMENT BEGIN CHAR = HEXB4=´       *
      *NEXT LINE BEGIN CHAR = HEXB6=¶       *
      *END OF LINE CHAR     = HEXB3=³       *
      ***************************************
           IF WS-PRINTER-TYPE = "3"
            IF DR-ACC-EMAIL = " "
                PERFORM READ-UNTIL-ACC-CHANGES
                GO TO PRL-005.
           IF WS-PRINTER-TYPE = "5"
            IF DR-ACC-EMAIL = " "
               PERFORM ENTER-EMAIL-ADDRESS
               MOVE WS-EMAIL-NUMBER TO DR-ACC-EMAIL.
           IF WS-PRINTER-TYPE = "4"
            IF DR-ACC-EMAIL > SPACES
                PERFORM READ-UNTIL-ACC-CHANGES
                GO TO PRL-005.
      
           MOVE "´¶"         TO WS-DELIM-F.
           IF WS-PRINTER-TYPE = "3" OR = "5"
               MOVE "³"      TO WS-DELIM-END1.

           IF WS-PRINTER-TYPE = "2" OR = "4"
              MOVE " "          TO PL1-NAME
              MOVE PA-PHONE     TO PL1-TEL
              MOVE PL1-SPECIAL  TO WS-DATA-F
              WRITE PRINT-REC FROM WS-FST-LINE AFTER 1.

           IF WS-PRINTER-TYPE = "3" OR = "5"
            IF DR-ACC-EMAIL > " "
              MOVE DR-ACC-EMAIL TO WS-DATA-F
              WRITE PRINT-REC FROM WS-FST-LINE AFTER 1
              
              MOVE "¶"          TO PL1-CHAR
              MOVE "³"          TO PL1-END-CHAR
              MOVE " "          TO PL1-NAME
              MOVE PA-PHONE     TO PL1-TEL
              WRITE PRINT-REC FROM LASER-PLINE1 AFTER 1.

           IF WS-PRINTER-TYPE = "3" OR = "5"
                MOVE "³"      TO PL1-END-CHAR.
            MOVE "¶"          TO PL1-CHAR
            MOVE PA-NAME      TO PL1-NAME
            MOVE PA-FAX       TO PL1-FAX
            WRITE PRINT-REC FROM LASER-PLINE1 AFTER 1
            
            MOVE PA-ADD1      TO PL1-NAME
            MOVE " "          TO PL1-FAX
            WRITE PRINT-REC FROM LASER-PLINE1 AFTER 1
            
            MOVE PA-ADD2      TO PL1-NAME
            MOVE PA-CO-REG-NO TO PL1-TEL
            WRITE PRINT-REC FROM LASER-PLINE1 AFTER 1
            
            MOVE PA-CO-VAT-NO TO PL1-TEL
            MOVE PA-ADD3      TO PL1-NAME
            WRITE PRINT-REC FROM LASER-PLINE1 AFTER 1
            
            MOVE " "          TO PRINT-REC LASER-PLINE1
            IF WS-PRINTER-TYPE = "3" OR = "5"
                MOVE "³"      TO PL1-END-CHAR.
            MOVE "¶"          TO PL1-CHAR
            MOVE PA-CODE      TO PL1-NAME
            WRITE PRINT-REC FROM LASER-PLINE1 AFTER 1
            
            MOVE " "          TO PRINT-REC LASER-PLINE1

      ***************************************
      *DEBTOR INFO STARTS HERE              *
      ***************************************
           IF WS-PRINTER-TYPE = "3" OR = "5"
                MOVE "³"           TO PL2-END-CHAR.
            MOVE "¶"               TO PL2-CHAR
            MOVE DR-NAME           TO PL2-NAME
            WRITE PRINT-REC FROM LASER-PLINE2 AFTER 1
            
            MOVE DR-ADDRESS1       TO PL2-NAME
            MOVE DR-ACCOUNT-NUMBER TO PL2-ACCOUNT
            MOVE P2-DATE-SAVE      TO PL2-DATE
            MOVE WS-PAGE           TO PL2-PAGE
            WRITE PRINT-REC FROM LASER-PLINE2 AFTER 1
            
            MOVE " "               TO PRINT-REC LASER-PLINE2
            IF WS-PRINTER-TYPE = "3" OR = "5"
                 MOVE "³"          TO PL2-END-CHAR.
            MOVE "¶"               TO PL2-CHAR
            MOVE DR-ADDRESS2       TO PL2-NAME
            WRITE PRINT-REC FROM LASER-PLINE2 AFTER 1
            
            MOVE "¶"               TO PL2-CHAR
            MOVE DR-ADDRESS3       TO PL2-NAME
            WRITE PRINT-REC FROM LASER-PLINE2 AFTER 1
            
            MOVE "¶"               TO PL2-CHAR
            MOVE DR-POST-CODE      TO PL2-NAME
            WRITE PRINT-REC FROM LASER-PLINE2 AFTER 1

            MOVE " " TO PRINT-REC 

            IF WS-PRINTER-TYPE = "3" OR = "5"
                 MOVE "³"         TO PL1-END-CHAR.
            MOVE "¶"              TO PL1-CHAR
            MOVE WS-STATE-COMMENT TO PL1-NAME
            WRITE PRINT-REC FROM LASER-PLINE1 AFTER 1
            
            MOVE " "              TO PRINT-REC LASER-PLINE2
            MOVE 0 TO L-CNT.
            
            IF WS-PAGE = 1 
              AND WS-BFORWARD-OPEN = "B"
             IF WS-PRINTER-TYPE = "3" OR = "5"
                   MOVE "³"       TO PL3-END-CHAR.
            IF WS-PAGE = 1 
              AND WS-BFORWARD-OPEN = "B"
                MOVE "¶"          TO PL3-CHAR
                WRITE PRINT-REC FROM LASER-PLINE3 AFTER 1
                MOVE " "          TO PRINT-REC
                ADD 1 TO L-CNT.
       PRL-020.
            IF WS-BFORWARD-OPEN = "O"
              GO TO PRL-022.
            IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
                GO TO PRL-040.
                
            MOVE DRTR-DATE       TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            
            IF WS-PRINTER-TYPE = "3" OR = "5"
                 MOVE "³"        TO PL5-END-CHAR.
            MOVE "¶"             TO PL5-CHAR
            MOVE DISPLAY-DATE    TO PL5-DATE
            MOVE DRTR-REFERENCE1 TO PL5-ORDER-NO
            MOVE DRTR-REFERENCE2 TO PL5-REFERENCE
            MOVE DRTR-TYPE       TO PL5-CODE.
            IF DRTR-TYPE = 1 OR = 3 OR = 4 OR = 7 OR = 10
               MOVE DRTR-AMT-OF-INVOICE TO PL5-CHARGES
               ADD DRTR-AMT-OF-INVOICE  TO WS-WORKTOTAL
            ELSE
               MOVE DRTR-AMT-OF-INVOICE       TO PL5-PAYMENTS
               SUBTRACT DRTR-AMT-OF-INVOICE FROM WS-WORKTOTAL.
            MOVE WS-WORKTOTAL                 TO PL5-BALANCE.
            WRITE PRINT-REC FROM LASER-PLINE5 AFTER 1
            
            MOVE " " TO PRINT-REC LASER-PLINE5
            ADD 1 TO L-CNT.
       PRL-022.
            IF WS-BFORWARD-OPEN = "B"
              GO TO PRL-025.
            IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
                GO TO PRL-040.
                
            MOVE DRTR-DATE       TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            
           IF WS-PRINTER-TYPE = "3" OR = "5"
                  MOVE "³"       TO PL5-END-CHAR.
            MOVE "¶"             TO PL5-CHAR
            MOVE DISPLAY-DATE    TO PL5-DATE
            MOVE DRTR-REFERENCE1 TO PL5-ORDER-NO
            MOVE DRTR-REFERENCE2 TO PL5-REFERENCE
            MOVE DRTR-TYPE       TO PL5-CODE.
            IF DRTR-TYPE = 1 OR = 3 OR = 4 OR = 7 OR = 10
               MOVE DRTR-AMT-OUTSTANDING       TO PL5-CHARGES
               ADD DRTR-AMT-OUTSTANDING        TO WS-WORKTOTAL
            ELSE
               MOVE DRTR-AMT-OUTSTANDING       TO PL5-PAYMENTS
               SUBTRACT DRTR-AMT-OUTSTANDING FROM WS-WORKTOTAL.
            MOVE WS-WORKTOTAL                  TO PL5-BALANCE.
            WRITE PRINT-REC FROM LASER-PLINE5 AFTER 1.

            MOVE " " TO PRINT-REC LASER-PLINE5.
            ADD 1 TO L-CNT.
       PRL-025.
            PERFORM READ-DEBTOR-TRANS.
            IF WS-DRTRANS-ST1 = 10
               MOVE " " TO DEBTOR-TRANS-REC
               PERFORM PRINT-LASER-TRAILING
               GO TO PRL-999.
            IF DRTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
                GO TO PRL-030.
            IF DRTR-ACCOUNT-NUMBER > WS-ACCNOEND
               PERFORM PRINT-LASER-TRAILING
               GO TO PRL-999.
            GO TO PRL-040.
       PRL-030.
            IF L-CNT = 24
             IF WS-PRINTER-TYPE = "3" OR = "5"
                 MOVE "³" TO PL8-END-CHAR.
            IF L-CNT = 24
                MOVE " "   TO PRINT-REC
                MOVE "¶"     TO PL8-CHAR
                ADD 1         TO WS-PAGE
                                 WS-TOTAL-PAGES
                MOVE WS-PAGE TO PL8-PAGE
                WRITE PRINT-REC FROM LASER-PLINE8 AFTER 1
                ADD 1 TO L-CNT
                PERFORM WRITE-BLANK-FOOTER
                GO TO PRL-010.
            IF L-CNT < 24
                GO TO PRL-020.
       PRL-040.
            PERFORM PRINT-LASER-TRAILING
            MOVE 0 TO WS-BF-BAL
            GO TO PRL-005.
       PRL-999.
            EXIT.
      *
       PRINT-PDF-ONLY SECTION.
       PR-PDF-000.
      * THIS SECTION USED FOR PRINTING .PDF FILES FOR EXPORT TO 
      * DOCUMENT WAREHOUSE TO REPLACE THE OLD STYLE COLD DOC'S
      *
      *ADDED THE EMAILING OF MAILGUN STATEMENTS TO THIS SECTION BUT
      *CHANGED THE LOCATON TO /ctools/fax/ FOR EMAIL STATEMENTS.
           MOVE WS-ACCNOBEGIN TO DRTR-ACCOUNT-NUMBER
           MOVE 0             TO DRTR-DATE.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE 10 TO WS-DRTRANS-ST1
              PERFORM PR-PDF-050
              GO TO PR-PDF-999.
       PR-PDF-001.
           MOVE 0 TO WS-BF-BAL.
           READ DEBTOR-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
      * 10=END OF FILE, 23=KEY NOT FOUND, 46=READ ERC
           IF WS-DRTRANS-ST1 = 10
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              PERFORM PRINT-LASER-TRAILING
              PERFORM PR-PDF-050
              GO TO PR-PDF-999.
           IF WS-DRTRANS-ST1 = 91
              MOVE 10  TO WS-DRTRANS-ST1
              MOVE " " TO DEBTOR-TRANS-REC
              PERFORM PRINT-LASER-TRAILING
              PERFORM PR-PDF-050
              GO TO PR-PDF-999.
           IF WS-DRTRANS-ST1 NOT = 0
             MOVE "DRTRANS1 BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DRTRANS-ST1
              GO TO PR-PDF-001.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           IF DRTR-ACCOUNT-NUMBER < WS-ACCNOBEGIN
              GO TO PR-PDF-001.
           IF DRTR-ACC-KEY = SPACES OR DRTR-ACCOUNT-NUMBER = 0
              GO TO PR-PDF-001.
       PR-PDF-005.
           IF DRTR-ACCOUNT-NUMBER > WS-ACCNOEND
              MOVE 10 TO WS-DRTRANS-ST1
              MOVE " " TO PRINT-REC
              GO TO PR-PDF-999.
            MOVE DRTR-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
            
            READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
              MOVE "DEBTOR ST=23 READ, IN 1 SEC GOING TO PR-PDF-001."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PR-PDF-001.

            IF WS-DEBTOR-ST1 NOT = 0
              MOVE "DEBTOR BUSY ON READ, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO PR-PDF-005.
               
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
              
            IF DR-BALANCE = 0
             IF DR-BAL-LAST-STATE = 0
              IF DR-SALES-PTD = 0
               IF DR-COST-PTD = 0
               GO TO PR-PDF-001.
            IF WS-ALL-DEB-CRED = "A"
               GO TO PR-PDF-008.
            IF WS-ALL-DEB-CRED = "D"
               AND DR-BALANCE > 0
               GO TO PR-PDF-008.
            IF WS-ALL-DEB-CRED = "C"
               AND DR-BALANCE < 0
               GO TO PR-PDF-008.
            GO TO PR-PDF-001.
       PR-PDF-008.
           MOVE DRTR-DATE TO WS-BF-DATE.
           IF WS-BFORWARD-OPEN = "O"
              GO TO PR-PDF-009.
           IF WS-BF-DATE < WS-LAST-PERIOD-DATE
           OR WS-BF-DATE = WS-LAST-PERIOD-DATE
              GO TO PR-PDF-0080.
           GO TO PR-PDF-009.
       PR-PDF-0080.
           IF DRTR-TYPE = 1 OR = 3 OR = 4 OR = 7 OR = 10
             IF WS-BFORWARD-OPEN = "O"
              ADD DRTR-AMT-OUTSTANDING TO WS-BF-BAL
             ELSE
              ADD DRTR-AMT-OF-INVOICE TO WS-BF-BAL.
           IF DRTR-TYPE = 2 OR = 5 OR = 6 OR = 8 OR = 9 OR = 11
             IF WS-BFORWARD-OPEN = "O"
              SUBTRACT DRTR-AMT-OUTSTANDING FROM WS-BF-BAL
             ELSE
              SUBTRACT DRTR-AMT-OF-INVOICE FROM WS-BF-BAL.
           IF WS-BFORWARD-OPEN = "B"
              PERFORM READ-DEBTOR-TRANS.
           IF WS-DRTRANS-ST1 = 10
               GO TO PR-PDF-009.

           IF DRTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
              IF WS-BF-DATE < WS-LAST-PERIOD-DATE
              OR WS-BF-DATE = WS-LAST-PERIOD-DATE
                 GO TO PR-PDF-008.
       PR-PDF-009.
           IF WS-BFORWARD-OPEN = "O"
               MOVE WS-BF-BAL TO PL3-BAL-FORW
                                 WS-WORKTOTAL
            ELSE
               MOVE DR-BAL-LAST-STATE TO PL3-BAL-FORW
                                         WS-WORKTOTAL.
            MOVE 1 TO WS-PAGE
            MOVE 0 TO WS-DEFERRED.
            
            PERFORM PR-PDF-045.
       PR-PDF-010.
      ***************************************
      *STATEMENT BEGIN CHAR = HEXB4=´       *
      *NEXT LINE BEGIN CHAR = HEXB6=¶       *
      *                                     *
      ***************************************
           IF WS-PRINTER-TYPE = "5"
            IF DR-ACC-EMAIL = " "
             IF WS-MONTH-END = "N" 
               PERFORM ENTER-EMAIL-ADDRESS
               MOVE WS-EMAIL-NUMBER TO DR-ACC-EMAIL.
           IF WS-PRINTER-TYPE = "5"
            IF DR-ACC-EMAIL = " "
             IF WS-MONTH-END = "Y" 
                MOVE "Info.AccountingOptions.co.za" TO WS-EMAIL-NUMBER 
                  DR-ACC-EMAIL.
      *          PERFORM READ-UNTIL-ACC-CHANGES
      *      IF WS-DEBTOR-ST1 NOT = 10
      *          GO TO PR-PDF-999
      *      ELSE
      *          GO TO PR-PDF-005.

            MOVE "´¶"         TO WS-DELIM-F.
            MOVE " "          TO PL1-NAME
            MOVE PA-PHONE     TO PL1-TEL
            MOVE PL1-SPECIAL  TO WS-DATA-F
            WRITE PRINT-REC FROM WS-FST-LINE AFTER 1.
 
            MOVE "¶"          TO PL1-CHAR
            MOVE PA-NAME      TO PL1-NAME
            MOVE PA-FAX       TO PL1-FAX
            WRITE PRINT-REC FROM LASER-PLINE1 AFTER 1
            
            MOVE PA-ADD1      TO PL1-NAME
            MOVE " "          TO PL1-FAX
            WRITE PRINT-REC FROM LASER-PLINE1 AFTER 1
            
            MOVE PA-ADD2      TO PL1-NAME
            MOVE PA-CO-REG-NO TO PL1-TEL
            WRITE PRINT-REC FROM LASER-PLINE1 AFTER 1
            
            MOVE PA-CO-VAT-NO TO PL1-TEL
            MOVE PA-ADD3      TO PL1-NAME
            WRITE PRINT-REC FROM LASER-PLINE1 AFTER 1
            
            MOVE " "          TO PRINT-REC LASER-PLINE1
            MOVE "¶"          TO PL1-CHAR
            MOVE PA-CODE      TO PL1-NAME
            WRITE PRINT-REC FROM LASER-PLINE1 AFTER 1
            
            MOVE " "          TO PRINT-REC LASER-PLINE1

      ***************************************
      *DEBTOR INFO STARTS HERE              *
      ***************************************
            MOVE "¶"               TO PL2-CHAR
            MOVE DR-NAME           TO PL2-NAME
            WRITE PRINT-REC FROM LASER-PLINE2 AFTER 1
            
            MOVE DR-ADDRESS1       TO PL2-NAME
            MOVE DR-ACCOUNT-NUMBER TO PL2-ACCOUNT
            MOVE P2-DATE-SAVE      TO PL2-DATE
            MOVE WS-PAGE           TO PL2-PAGE
            WRITE PRINT-REC FROM LASER-PLINE2 AFTER 1
            
            MOVE " "               TO PRINT-REC LASER-PLINE2
            MOVE "¶"               TO PL2-CHAR
            MOVE DR-ADDRESS2       TO PL2-NAME
            WRITE PRINT-REC FROM LASER-PLINE2 AFTER 1
            
            MOVE "¶"               TO PL2-CHAR
            MOVE DR-ADDRESS3       TO PL2-NAME
            WRITE PRINT-REC FROM LASER-PLINE2 AFTER 1
            
            MOVE "¶"               TO PL2-CHAR
            MOVE DR-POST-CODE      TO PL2-NAME
            WRITE PRINT-REC FROM LASER-PLINE2 AFTER 1

            MOVE " " TO PRINT-REC 

            MOVE "¶"              TO PL1-CHAR
            MOVE WS-STATE-COMMENT TO PL1-NAME
            WRITE PRINT-REC FROM LASER-PLINE1 AFTER 1
            
            MOVE " "              TO PRINT-REC LASER-PLINE2
            MOVE 0 TO L-CNT.
            
            IF WS-PAGE = 1 
              AND WS-BFORWARD-OPEN = "B"
                MOVE "¶"          TO PL3-CHAR
                WRITE PRINT-REC FROM LASER-PLINE3 AFTER 1
                MOVE " "          TO PRINT-REC
                ADD 1 TO L-CNT.
       PR-PDF-020.
            IF WS-BFORWARD-OPEN = "O"
                GO TO PR-PDF-022.
            IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
                MOVE 66 TO WS-DEBTOR-ST1
                GO TO PR-PDF-040.
                
            MOVE DRTR-DATE       TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            
            MOVE "¶"             TO PL5-CHAR
            MOVE DISPLAY-DATE    TO PL5-DATE
            MOVE DRTR-REFERENCE1 TO PL5-ORDER-NO
            MOVE DRTR-REFERENCE2 TO PL5-REFERENCE
            MOVE DRTR-TYPE       TO PL5-CODE.
            IF DRTR-TYPE = 1 OR = 3 OR = 4 OR = 7 OR = 10
               MOVE DRTR-AMT-OF-INVOICE TO PL5-CHARGES
               ADD DRTR-AMT-OF-INVOICE  TO WS-WORKTOTAL
            ELSE
               MOVE DRTR-AMT-OF-INVOICE       TO PL5-PAYMENTS
               SUBTRACT DRTR-AMT-OF-INVOICE FROM WS-WORKTOTAL.
            MOVE WS-WORKTOTAL                 TO PL5-BALANCE.
            WRITE PRINT-REC FROM LASER-PLINE5 AFTER 1
            
            MOVE " " TO PRINT-REC LASER-PLINE5
            ADD 1 TO L-CNT.
       PR-PDF-022.
            IF WS-BFORWARD-OPEN = "B"
              GO TO PR-PDF-025.
            IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
                MOVE 10 TO WS-DRTRANS-ST1
                GO TO PR-PDF-040.
                
            MOVE DRTR-DATE       TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            
            MOVE "¶"             TO PL5-CHAR
            MOVE DISPLAY-DATE    TO PL5-DATE
            MOVE DRTR-REFERENCE1 TO PL5-ORDER-NO
            MOVE DRTR-REFERENCE2 TO PL5-REFERENCE
            MOVE DRTR-TYPE       TO PL5-CODE.
            IF DRTR-TYPE = 1 OR = 3 OR = 4 OR = 7 OR = 10
               MOVE DRTR-AMT-OUTSTANDING       TO PL5-CHARGES
               ADD DRTR-AMT-OUTSTANDING        TO WS-WORKTOTAL
            ELSE
               MOVE DRTR-AMT-OUTSTANDING       TO PL5-PAYMENTS
               SUBTRACT DRTR-AMT-OUTSTANDING FROM WS-WORKTOTAL.
            MOVE WS-WORKTOTAL                  TO PL5-BALANCE.
            WRITE PRINT-REC FROM LASER-PLINE5 AFTER 1.
            ADD 1 TO L-CNT.
       PR-PDF-025.
            PERFORM READ-DEBTOR-TRANS.
            IF WS-DRTRANS-ST1 = 10
               MOVE " " TO DEBTOR-TRANS-REC
               PERFORM PRINT-LASER-TRAILING
               PERFORM PR-PDF-050
               GO TO PR-PDF-999.
            IF DRTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
                GO TO PR-PDF-030.
            IF DRTR-ACCOUNT-NUMBER > WS-ACCNOEND
               MOVE 10 TO WS-DRTRANS-ST1
               PERFORM PRINT-LASER-TRAILING
               PERFORM PR-PDF-050
               GO TO PR-PDF-999.
            GO TO PR-PDF-040.
       PR-PDF-030.
            IF L-CNT = 24
                MOVE " "     TO PRINT-REC
                MOVE "¶"     TO PL8-CHAR
                ADD 1        TO  WS-PAGE
                                 WS-TOTAL-PAGES
                MOVE WS-PAGE TO PL8-PAGE
                WRITE PRINT-REC FROM LASER-PLINE8 AFTER 1
                ADD 1 TO L-CNT
                PERFORM WRITE-BLANK-FOOTER
                GO TO PR-PDF-010.
            IF L-CNT < 24
                GO TO PR-PDF-020.
       PR-PDF-040.
            PERFORM PRINT-LASER-TRAILING
            MOVE 0 TO WS-BF-BAL.
            
            PERFORM PR-PDF-050.
     
            IF WS-DRTRANS-ST1 NOT = 10
                GO TO PR-PDF-005
            ELSE
                GO TO PR-PDF-999.
       PR-PDF-045.
            IF WS-PRINTER-TYPE = "5"
                 PERFORM WORK-OUT-EMAIL-PDF-FILE-NAME
            ELSE
                 PERFORM WORK-OUT-PDF-FILE-NAME.
            OPEN OUTPUT PRINT-FILE.
            PERFORM Z1-HEADINGS.
       PR-PDF-050.
            CLOSE PRINT-FILE.
            IF WS-PRINTER-TYPE = "5"
                MOVE "YOUR STATEMENT"  TO WS-SUBJECT-LINE1
                MOVE DR-ACCOUNT-NUMBER TO WS-SUBJECT-LINE2
                MOVE " FROM:"          TO WS-SUBJECT-LINE3 
                MOVE WS-CO-NAME        TO WS-SUBJECT-LINE4
                PERFORM TAKE-OUT-BLANKS-IN-CO-NAME
                PERFORM MAKE-PDF-FINAL-FOR-EMAIL
                PERFORM SETUP-STATE-FOR-PDF-EMAIL-ONLY
                PERFORM SETUP-STATEMENT-FOR-PDF-MGEMAIL. 
            IF WS-PRINTER-TYPE = "6"
                PERFORM WRITE-INDEX-KEY
                PERFORM SETUP-STATEMENT-FOR-PDF-ONLY. 
       PR-PDF-999.
            EXIT.
      *
       TAKE-OUT-BLANKS-IN-CO-NAME SECTION.
       TOBICN-005.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
       TOBICN-005.
           MOVE WS-SUBJECT TO DATA-RATE.
           MOVE 1 TO SUB-1
           MOVE 1 TO SUB-2.
           MOVE 0 TO SUB-3.
           MOVE "'" TO AL-RATE (SUB-2).
           MOVE 2 TO SUB-1.
       TOBICN-010.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
                MOVE 0 TO SUB-3
               GO TO TOBICN-010
            ELSE 
               ADD 1 TO SUB-3.
           IF SUB-3 = 1 
              GO TO TOBICN-010.
           MOVE "'" TO AL-RATE (SUB-1).
       TOBICN-030.
           MOVE SPACES       TO WS-SUBJECT-FIXED
           MOVE ALPHA-RATE   TO WS-SUBJECT-FIXED.
           
      *     MOVE WS-SUBJECT TO WS-MESSAGE
      *     PERFORM ERROR1-000.

      *     MOVE WS-SUBJECT-FIXED TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       TOBICN-999.
           EXIT.
      *
       PRINT-LASER-TRAILING SECTION.
       PTL-000.
           MOVE " " TO PRINT-REC.
           IF L-CNT < 24
      *      IF WS-PRINTER-TYPE = "3" OR = "5"
            IF WS-PRINTER-TYPE = "3"
               MOVE "³"     TO PLBF-END-CHAR.
       PTL-005.
           IF L-CNT < 24
              MOVE "¶"      TO PLBF-CHAR
              WRITE PRINT-REC FROM LASER-BLANK-FOOTER AFTER 1
              ADD 1 TO L-CNT
              GO TO PTL-005.
              
           MOVE " "                      TO PRINT-REC
           MOVE DR-TERMS-CODE            TO WS-TERM-SUB
           MOVE WS-ST-TERM (WS-TERM-SUB) TO WS-TERMOFSALE.
      *     IF WS-PRINTER-TYPE = "3" OR = "5"
           IF WS-PRINTER-TYPE = "3"
                MOVE "³"                 TO PL6-END-CHAR.
           MOVE "¶"                      TO PL6-CHAR
           MOVE WS-TERMOFSALE            TO PL6-TERMS
           MOVE DR-CURRENT               TO PL6-CURRENT
           MOVE DR-30DAY                 TO PL6-30DAY
           MOVE DR-60DAY                 TO PL6-60DAY
           MOVE DR-90DAY                 TO PL6-90DAY
           MOVE DR-120DAY                TO PL6-120DAY
           WRITE PRINT-REC FROM LASER-PLINE6 AFTER 1
           ADD 1 TO L-CNT.
           
           MOVE " "                      TO PRINT-REC LASER-PLINE7
      *     IF WS-PRINTER-TYPE = "3" OR = "5"
           IF WS-PRINTER-TYPE = "3"
                 MOVE "³"                TO PL7-END-CHAR.
           MOVE "¶"                      TO PL7-CHAR
      *     MOVE WS-WORKTOTAL            TO PL7-AMOUNT-TO-PAY
           MOVE DR-BALANCE               TO PL7-AMOUNT-TO-PAY.
           IF DR-BALANCE NOT = WS-WORKTOTAL
               MOVE "***"                TO PL7-BAL-ERROR
           ELSE
               MOVE "ZAR"                TO PL7-BAL-ERROR.
           WRITE PRINT-REC FROM LASER-PLINE7 AFTER 1
           ADD 1 TO L-CNT.
           
           MOVE " " TO PRINT-REC
           
      *     IF WS-PRINTER-TYPE = "3" OR = "5"
           IF WS-PRINTER-TYPE = "3"
                 MOVE "³"                TO PL7-END-CHAR.
           MOVE "¶"                      TO PL7-CHAR
           MOVE DR-ACCOUNT-NUMBER        TO PL7-ACCOUNT
           MOVE P2-DATE-SAVE             TO PL7-DATE
           MOVE WS-WORKTOTAL             TO PL7-AMOUNT-TO-PAY
           WRITE PRINT-REC FROM LASER-PLINE7 AFTER 1.
           ADD 1 TO L-CNT.
            
      *     IF WS-PRINTER-TYPE = "3" OR = "5"
           IF WS-PRINTER-TYPE = "3"
                  MOVE "³"   TO PL4-END-CHAR.
           MOVE "¶"          TO PL4-CHAR
           MOVE PA-NAME      TO PL4-NAME
           WRITE PRINT-REC FROM LASER-PLINE4 AFTER 1.
           ADD 1 TO L-CNT.
            
           MOVE PA-ADD1      TO PL4-NAME
           WRITE PRINT-REC FROM LASER-PLINE4 AFTER 1.
           ADD 1 TO L-CNT.
            
           MOVE " "               TO LASER-PLINE4
      *     IF WS-PRINTER-TYPE = "3" OR = "5"
           IF WS-PRINTER-TYPE = "3"
                MOVE "³"          TO PL4-END-CHAR.
           MOVE "¶"               TO PL4-CHAR
           MOVE PA-ADD2           TO PL4-NAME
           MOVE DR-NAME           TO PL4-DEBTOR
           WRITE PRINT-REC FROM LASER-PLINE4 AFTER 1.
           ADD 1 TO L-CNT.
            
           MOVE " "               TO LASER-PLINE4
      *     IF WS-PRINTER-TYPE = "3" OR = "5"
           IF WS-PRINTER-TYPE = "3"
               MOVE "³"           TO PL4-END-CHAR.
           MOVE "¶"               TO PL4-CHAR
           MOVE PA-ADD3           TO PL4-NAME
           WRITE PRINT-REC FROM LASER-PLINE4 AFTER 1.
           ADD 1 TO L-CNT.
            
           MOVE " "               TO LASER-PLINE4
      *     IF WS-PRINTER-TYPE = "3" OR = "5"
           IF WS-PRINTER-TYPE = "3"
                  MOVE "³"        TO PL4-END-CHAR.
           MOVE "¶"               TO PL4-CHAR
           MOVE PA-CODE           TO PL4-NAME
           WRITE PRINT-REC FROM LASER-PLINE4 AFTER 1.
           ADD 1 TO L-CNT.
       PTL-010.
           IF WS-FOUND = " "
               MOVE "Y" TO WS-FOUND.
               
           MOVE 2818 TO POS
           DISPLAY "Account Number Being Processed =" AT POS 
           ADD 33 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
               
           MOVE 2922 TO POS
           DISPLAY "No Of Statements Processed =" AT POS
           ADD 1 TO WS-ACCPRINTED-COUNT
                    WS-TOTAL-PAGES
           MOVE WS-ACCPRINTED-COUNT TO WS-TRANS-COUNT
           MOVE WS-TRANS-COUNT      TO WS-MESSAGE1
           MOVE 2954                TO POS
           DISPLAY WS-MESSAGE1 AT POS.
       PTL-999.
           EXIT.
      *
       ENTER-EMAIL-ADDRESS SECTION.
       EEA-005.
           PERFORM CLEAR-010.
           
           MOVE 2910 TO POS
           DISPLAY 
           "ACC:         DOES NOT HAVE AN EMAIL ADDRESS, PLEASE ENTER."
              AT POS
           MOVE 2915 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS
           MOVE 3010 TO POS
           DISPLAY 
           "EMail :[                                                  ]"
              AT POS.
              MOVE 3018 TO POS
              MOVE WS-EMAIL-NUMBER TO CDA-DATA
              MOVE 50        TO CDA-DATALEN
              MOVE 27        TO CDA-ROW
              MOVE 17        TO CDA-COL
              MOVE CDA-WHITE TO CDA-COLOR
              MOVE 'F'       TO CDA-ATTR
              PERFORM CTOS-ACCEPT
              MOVE CDA-DATA TO WS-EMAIL-NUMBER.
              
           IF WS-EMAIL-NUMBER NOT > " "
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 
           "EMAIL ADDRESS CANNOT BE BLANK AND MUST BE IN lower case."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO EEA-005.
       EEA-999.
           EXIT.
      *
       WRITE-BLANK-FOOTER SECTION.
       WBF-005.
           IF WS-PRINTER-TYPE = "3" OR = "5"
              MOVE "³" TO PLBF-END-CHAR.
           MOVE "¶"    TO PLBF-CHAR.
       WBF-010.
           IF L-CNT < 32
              WRITE PRINT-REC FROM LASER-BLANK-FOOTER AFTER 1
              ADD 1 TO L-CNT
              GO TO WBF-010.
       WBF-999.
           EXIT.
      *
       READ-DEBTOR-TRANS SECTION.
       RDT-000.
           READ DEBTOR-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
              MOVE " " TO DEBTOR-TRANS-REC
              GO TO RDT-999.
           IF WS-DRTRANS-ST1 = 91
              MOVE 10 TO WS-DRTRANS-ST1
              MOVE " " TO DEBTOR-TRANS-REC
              GO TO RDT-999.
           IF WS-DRTRANS-ST1 NOT = 0
             MOVE "DRTRANS2 BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DRTRANS-ST1
              GO TO RDT-000.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
       RDT-999.
           EXIT.
      *
       READ-UNTIL-ACC-CHANGES SECTION.
       RUAC-000.
           IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
              GO TO RUAC-999.

           READ DEBTOR-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
              MOVE " " TO DEBTOR-TRANS-REC
              GO TO RUAC-999.
           IF WS-DRTRANS-ST1 = 91
              MOVE 10 TO WS-DRTRANS-ST1
              MOVE " " TO DEBTOR-TRANS-REC
              GO TO RUAC-999.
           IF WS-DRTRANS-ST1 NOT = 0
             MOVE "DRTRANS BUSY ON READ-NEXT RUAC-000, GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO RUAC-000.
           IF DRTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
              GO TO RUAC-000.
              
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
       RUAC-999.
           EXIT.
      *
       CHECK-PRINTER-NAME SECTION.
       CPN-001.
           MOVE 1 TO SUB-1.
       CPN-005.
           IF WS-PRINTER-TYPE = "1"
            IF WS-PRINTERNUMBER (SUB-1) = 10
               MOVE WS-PRINTERNAME (SUB-1)  TO WS-PRINTER-SAVE
               MOVE WS-PRINTERCHARS (SUB-1) TO WS-PRINT-CHARS
               GO TO CPN-999.
           IF WS-PRINTER-TYPE = "2" OR = "3" OR = "4"
            IF WS-PRINTERNUMBER (SUB-1) = 15
               MOVE WS-PRINTERNAME (SUB-1)  TO WS-PRINTER-SAVE
               GO TO CPN-999.
           IF SUB-1 < 11
             ADD 1 TO SUB-1
             GO TO CPN-005.
       CPN-999.
           EXIT.
      *
       CHECK-SPOOLER SECTION.
       CP-000.
          PERFORM CHECK-PRINTER-NAME.
           IF WS-PRINTER-TYPE = "1"
               PERFORM SEND-REPORT-TO-PRINTER
               GO TO CP-999.
      *         PERFORM QUEUE-PRINT-FILE
      *     ELSE
      *        PERFORM A995-QUEUE-FSD
      *        GO TO CP-999.
      *     MOVE SPACE TO W-SPOOLST
      *     MOVE SPACE TO W-SPOOLST2
      *     PERFORM CHECK-FOR-PAUSE.
      *     IF WS-LONG-FORMAT = "Y"
      *        GO TO CP-005.
      * LASER FORMAT PRINT OUT TO PRINTER NUMBER 15 ONLY
           IF WS-PRINTER-TYPE = "2" OR = "3"
              PERFORM SETUP-STATEMENT-FOR-PDF
              GO TO CP-999.
           IF WS-PRINTER-TYPE = "4"
              PERFORM SETUP-STATEMENT-NOMAIL-FOR-PDF
              GO TO CP-999.

      *     MOVE " Load The 'STATEMENT' Paper, Switch the printer"
      *         TO WS-MESSAGE.
      *     MOVE "   To '6' & Switch It Off & On, Press 'ESC'"
      *         TO WS-MESSAGE1.
      *     PERFORM ERROR1-MESSAGE.

       CP-005.
      *     IF WS-PRINTER-TYPE = "1"
      *        PERFORM SEND-CONTROL-CHAR.
      *     MOVE " Printing of statements in progress ..........."
      *         TO WS-MESSAGE
      *     MOVE 3010 TO POS
      *     DISPLAY WS-MESSAGE AT POS.
      *
      * PRINTING COMPLETE 
      *
      *     IF WS-PRINTER-TYPE NOT = "1"
      *           GO TO CP-999.
      *
      *     PERFORM CHECK-PAUSE-PRINT
      *     IF WS-LONG-FORMAT = "Y"
      *        PERFORM CHECK-PAUSE-010
      *        PERFORM CHECK-PAUSE-010
      *        PERFORM CHECK-PAUSE-010
      *        GO TO CP-900.
      *     PERFORM ERROR-020
      *     MOVE "Load Normal Paper, Switch The Printer To '7',"
      *         TO WS-MESSAGE
      *     MOVE "   & Switch It Off & On,  Press 'ESC' TO Continue."
      *         TO WS-MESSAGE1
      *     PERFORM ERROR1-MESSAGE.
       CP-900.
      *     PERFORM SEND-CONTROL-CHAR.
       CP-999.
           EXIT.
      *
       READ-TERMS-FILE SECTION.
       RTERM-000.
            MOVE 1 TO SUB-1
                      PA-RECORD.
            MOVE 2 TO PA-TYPE.
       RTERM-010.
            READ PARAMETER-FILE NEXT
                AT END NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 10
                GO TO RTERM-999.
            IF PA-TYPE < 2
                GO TO RTERM-010.
            IF PA-TYPE > 2
                GO TO RTERM-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER FILE BUSY, PRESS 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RTERM-010.
            IF PARAMETER-REC = "           "
               GO TO RTERM-010.           
            MOVE PARAMETER-REC TO WS-TERM-OCCUR (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 = 10
               PERFORM ERROR-020
               GO TO RTERM-999.
            GO TO RTERM-010.
       RTERM-999.
            EXIT.
      *-----------------------------------------------------------*
       Z1-HEADINGS SECTION.
      *-----------------------------------------------------------*
       Z1-50.
            MOVE "´¶"  TO WS-DELIM-F.
            MOVE "¶"   TO WS-DELIM-O
            IF WS-PRINTER-TYPE = "3"
              MOVE "³" TO WS-DELIM-END1
                          WS-DELIM-END2.

            MOVE 1             TO SUB-1
            MOVE SUB-1         TO WS-O-LINE
            MOVE "SuppLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-F
            WRITE PRINT-REC FROM WS-FST-LINE AFTER 0.
       Z1-51.
            ADD 1              TO SUB-1
            IF WS-PRINTER-TYPE = "3"
             IF SUB-1 > 7
               MOVE 0 TO SUB-1
               GO TO Z1-52.
            IF WS-PRINTER-TYPE NOT = "3"
             IF SUB-1 > 6
               MOVE 0 TO SUB-1
               GO TO Z1-52.
            MOVE SUB-1         TO WS-O-LINE
            MOVE "SuppLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-51.
       Z1-52.
            ADD 1              TO SUB-1
            IF SUB-1 > 5
               MOVE 0 TO SUB-1
               GO TO Z1-53.
            MOVE SUB-1         TO WS-O-LINE
            MOVE "DebtLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-52.
       Z1-53.
            ADD 1              TO SUB-1
            IF SUB-1 > 1
               MOVE 0 TO SUB-1
               GO TO Z1-54.
            MOVE SUB-1         TO WS-O-LINE
            MOVE "MessLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-53.
       Z1-54.
            ADD 1              TO SUB-1
            IF SUB-1 > 24
               MOVE 0 TO SUB-1
               GO TO Z1-55.
            MOVE SUB-1         TO WS-O-LINE
            MOVE "BodyLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-54.
       Z1-55.
            ADD 1              TO SUB-1
            IF SUB-1 > 2
               MOVE 0 TO SUB-1
               GO TO Z1-56.
            MOVE SUB-1         TO WS-O-LINE
            MOVE "PerdLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-55.
       Z1-56.
            ADD 1              TO SUB-1
            IF SUB-1 > 6
               MOVE 0 TO SUB-1
               GO TO Z1-100.
            MOVE SUB-1         TO WS-O-LINE
            MOVE "RemtLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-56.
       Z1-100.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O PARAMETER-FILE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
            MOVE ALL "X" TO STORE-TERM.
            PERFORM READ-TERMS-FILE.
       OPEN-010.
            MOVE 1 TO PA-RECORD.
            MOVE 0 TO PA-TYPE.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
                MOVE  "NO SLPARAMETER RECORD ON FILE, 'ESC' TO EXITT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                CLOSE PARAMETER-FILE
                EXIT PROGRAM.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-010.
            MOVE PA-CURRENT-PER-MM TO WS-CURRENT-MM.
            CLOSE PARAMETER-FILE.
       OPEN-020.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-020.
       OPEN-030.
            OPEN INPUT DEBTOR-TRANS-FILE.
            IF WS-DRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-DRTRANS-ST1
               MOVE "DEBTOR TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-030.
       OPEN-040.
           MOVE " " TO DRTR-ACC-KEY.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
            MOVE "NO DR-TRANS RECORDS ON START, GOING TO EXIT IN 5 SEC."
              TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 5
              CLOSE DEBTOR-MASTER
                    DEBTOR-TRANS-FILE
              EXIT PROGRAM.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DEBTOR TRANS FILE BUSY ON START, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-DRTRANS-ST1
              GO TO OPEN-040.
           GO TO OPEN-120.
       OPEN-120.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "DrStatRp"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-150.
            MOVE " " TO WS-MESSAGE
            PERFORM ERROR-020
           PERFORM GET-SYSTEM-Y2K-DATE.
      *      ACCEPT WS-DATE FROM DATE
            MOVE WS-DATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE TO P2-DATE-LHS P2-DATE-RHS P2-DATE-SAVE.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-010.
           CLOSE PRINT-FILE.
           CLOSE DEBTOR-MASTER
                 DEBTOR-TRANS-FILE.
       END-900.
      *     STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldNumeric".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldAlpha60".
       Copy "WriteFieldAlpha60".
       Copy "WriteFieldNumeric".
       Copy "CheckForPause".
       Copy "CheckDataNames".
       Copy "QueuePrintFileStatement".
       Copy "QueuePrintLaserStatement".
       Copy "GetSystemY2KDate".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
       Copy "SetupStatementForPDF".
       Copy "SetupStatementForPDFOnly".
       Copy "SetupStatementNoMailForPDF".
       Copy "SetupStatementForPDFEmailOnly".
       Copy "SetupStatementForPDFMgEmail".
       Copy "DeleteBlankEmailStateRecord".
       Copy "MoveEmailRecordFromEimage".
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
       Copy "WriteDailyExcep1".
      *
      *END OF JOB
