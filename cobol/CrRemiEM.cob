        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrRemiEM.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        REPOSITORY. 
           FUNCTION ALL INTRINSIC.
        SPECIAL-NAMES.
          CRT STATUS IS W-CRTSTATUS.
        SPECIAL-NAMES.
        CLASS WS-VALID-EMAIL IS
          '@' '_' '.' '-' '#'
          'a' THRU 'i'
          'j' THRU 'r'
          's' THRU 'z'
          '0' THRU '9'.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrMaster".
          Copy "SelectCrRemittance".
          Copy "SelectCrRemitTrans".
          Copy "SelectGlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrRemit.
           COPY ChlfdCrRemiTrans.
           COPY ChlfdGlParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(90).

       WORKING-STORAGE SECTION.
       77  LINE-CNT           PIC 9(3) VALUE 66.
       77  WS-LINE-NO         PIC 9(3) VALUE 0.
       77  PAGE-CNT           PIC 9(2) VALUE 0.
       77  STATE-CNT          PIC 9(4) VALUE 0.
       77  WS-TYPE            PIC X VALUE " ".
       77  WS-RANGE1          PIC X(7) VALUE " ".
       77  WS-RANGE2          PIC X(7) VALUE " ".
       77  WS-MES-COMMENT     PIC X(60) VALUE " ".
       77  WS-NETT-AMT        PIC S9(8)V99 VALUE 0.
       77  WS-YEAR-BU         PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN          PIC S9(8)V99 VALUE 0.
       77  WS-TOTAL-ADJ       PIC S9(8)V99 VALUE 0.
       77  WS-TOTAL-GROSS     PIC S9(8)V99 VALUE 0.
       77  WS-DISC-PERC       PIC S9(2)V99 VALUE 0.
       77  WS-DISC-AMT        PIC S9(8)V99 VALUE 0.
       77  WS-TOTAL-NETT      PIC S9(8)V99 VALUE 0.
       77  WS-INQUIRY         PIC X(8) VALUE "CrMastIq".
       77  NEW-CRREMNO        PIC X VALUE " ".      
       77  WS-HEAD-VALID      PIC X VALUE " ".
       77  WS-SUB-VALID       PIC X VALUE " ".
       77  WS-END             PIC X VALUE " ".      
       77  WS-FAX-Y-N         PIC X VALUE " ".      
       77  WS-REMIT-F-L       PIC X VALUE " ".      
       77  WS-PRINT-NO        PIC X VALUE " ".      
       77  WS-NUMBER          PIC 9(7) VALUE 0.
       77  SUB-1-SAVE         PIC S9(5) VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-FAX-NUMBER        PIC X(20) VALUE " ".
       77  WSF-MAIL-NUMBER      PIC X(50) VALUE " ".
       77  WS-PRINTER-PAGE1     PIC X(100) VALUE " ".
       77  WS-PRINTER-PAGE2     PIC X(100) VALUE " ".
       77  WS-PRINTER-DOT       PIC X(100) VALUE " ".
       77  WS-PRINTER-PDF       PIC X(100) VALUE " ".
       77  WS-PRINTER-FAX       PIC X(100) VALUE " ".
       77  WS-SUBJECT-FIXED     PIC X(100) VALUE " ".      
       77  WS-ACC-ERROR         PIC X VALUE " ".      
       77  WS-QUOTE-NAME        PIC X(25) VALUE " ".
       01  WS-EMAIL             PIC X(50).
       01  WS-TEMP-EMAIL-FILE   PIC X(50).
       01  WS-SPACE-CNT         PIC 9(2) VALUE ZEROES.
       01  W-CRTSTATUS          PIC 9(4) value 0.
       01  F-RC                 BINARY-SHORT VALUE 0.
       01  WS-COMMENT-LINE.
           03  WS-COMMENT       PIC X(60) OCCURS 4.
       01  WS-REMIT-PERIOD.
           03  WS-REMI-YY          PIC 99.
           03  WS-REMI-MM          PIC 99.
       01  WS-REMITTRANS-STATUS.
           03  WS-REMITTRANS-ST1   PIC 99.
       01  WS-REMI-STATUS.
           03  WS-REMI-ST1         PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-PAR-ST1          PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1     PIC 99.
       01  WS-SPL-STATUS.
           03  WS-SPL-ST1          PIC 99.
       01  WS-MONTH-BUDGET.
           03  WS-MONTH-BU   PIC S9(8)V99 OCCURS 13.
       01  WS-INV-PAY-LINE.
           03  WS1-FIL        PIC X(6) VALUE "*INV:".
           03  WS-IP-INV      PIC X(10) VALUE " ".
           03  WS2-FIL        PIC X(6) VALUE "AMT: R".
           03  WS-IP-AMT      PIC Z(6)9.99-.
           03  WS3-FIL        PIC X(7) VALUE "DISC: R".
           03  WS-IP-DISC     PIC Z(6)9.99-.
           03  WS4-FIL        PIC X(8) VALUE "NETT: R ".
           03  WS-IP-NETT     PIC Z(6)9.99-.
       01  WS-MONTH-DESCRIPTIONS.
           03  FILLER          PIC X(10) VALUE "JANUARY  ".
           03  FILLER          PIC X(10) VALUE "FEBRUARY ".
           03  FILLER          PIC X(10) VALUE "MARCH    ".
           03  FILLER          PIC X(10) VALUE "APRIL    ".
           03  FILLER          PIC X(10) VALUE "MAY      ".
           03  FILLER          PIC X(10) VALUE "JUNE     ".
           03  FILLER          PIC X(10) VALUE "JULY     ".
           03  FILLER          PIC X(10) VALUE "AUGUST   ".
           03  FILLER          PIC X(10) VALUE "SEPTEMBER".
           03  FILLER          PIC X(10) VALUE "OCTOBER  ".
           03  FILLER          PIC X(10) VALUE "NOVEMBER ".
           03  FILLER          PIC X(10) VALUE "DECEMBER ".
           03  FILLER          PIC X(10) VALUE "OCTOBER  ".
           03  FILLER          PIC X(10) VALUE "NOVEMBER ".
           03  FILLER          PIC X(10) VALUE "SEPTEMBER".
       01  WS-MONTH-DESC REDEFINES WS-MONTH-DESCRIPTIONS.
           03  WS-MM-DESC      PIC X(10) OCCURS 15.
       01 WS-EMAIL-REMIT.
           03  WS-ER-FIL        PIC X(11) VALUE "/ctools/cr/".
           03  WS-EREMIT        PIC X(4).
           03  WS-ER-FIL2       PIC X(2) VALUE "Co".
           03  WS-ECOMPANY      PIC 99.
       01  WS-SUBJECT.
           03  WS-SUBJECT-LINE1        PIC X(15) VALUE " ".
           03  WS-SUBJECT-LINE2        PIC X(18).
           03  WS-SUBJECT-LINE3        PIC X(7) VALUE " ".
           03  WS-SUBJECT-LINE4        PIC X(40) VALUE " ".
       01 WS-EMAIL-PDF-REMIT.
           03  WS-PDF-ER-FIL        PIC X(11) VALUE "/ctools/cr/".
           03  WS-PDF-EREMIT        PIC X(4).
           03  WS-PDF-ER-FIL2       PIC X(2) VALUE "Co".
           03  WS-PDF-ECOMPANY      PIC 99.
           03  WS-PDF-ACC-NUM       PIC 9(7).
           03  FILLER               PIC X VALUE "-".
           03  WS-PDF-PAGE          PIC 9.
       01 WS-FST-LINE.
           05  WS-DELIM-F             PIC  X(2).
           05  WS-DATA-F              PIC  X(86).
           05  WS-DELIM-END1          PIC  X(1).
       01 WS-OTH-LINE-1.
           05  WS-O-L                 PIC  X(8).
           05  WS-O-LINE              PIC  99.
           05  FILLER                 PIC  X(76).
       01 WS-OTH-LINE.
           05  WS-DELIM-O             PIC  X.
           05  WS-DATA-O              PIC  X(87).
           05  WS-DELIM-END2          PIC  X(1).
       01  SLIP-PDF-HEAD1.
           03  FILLER          PIC X(16).
           03  S-PDF-NAME1     PIC X(40) VALUE " ".
           03  S-PDF-NAME2.
               05  S-PDF-NAME3  PIC X(11) VALUE " ".
               05  S-PDF-PAGE   PIC Z9 VALUE " ".
               05  S-PDF-NAME4  PIC X(40).
       01  SLIP-PDF-TRAILER.
           03  FILLER           PIC X(12).
           03  S-PDFT-TRAIL.
               05  S-PDFT-PAGE  PIC Z9.
               05  FILLER       PIC X(50) VALUE " ".
       01  SLIP-PDF-ACC.
           03  FILLER           PIC X(16).
           03  FILLER           PIC X(15) VALUE "OUR ACCOUNT #:".
           03  SPC-OUR-ACC      PIC X(10) VALUE " ".
           03  FILLER           PIC X(16) VALUE "YOUR ACCOUNT #:".
           03  SPC-YOUR-ACC     PIC X(10) VALUE " ".
       01  SLIPE-HEAD1.
           03  H1-1            PIC XX.
           03  FILLER          PIC X(5) VALUE "DATE".
           03  SOE1-DATE       PIC X(10).
           03  FILLER          PIC X(5) VALUE " ".
           03  FILLER          PIC X(49) VALUE
            "** R E M I T T A N C E   A D V I C E **".
           03  FILLER          PIC X(17) VALUE " ".
           03  H1-2            PIC X.
       01  SLIPE-HEAD2.
           03  H2-1            PIC XX.
           03  FILLER          PIC X(20) VALUE " ".
           03  FILLER          PIC X(39) VALUE ALL "*".
           03  FILLER          PIC X(27) VALUE " ".
           03  H2-2            PIC X.
       01  SLIPE-HEAD3.
           03  H3-1            PIC XX.
           03  FILLER          PIC X(40) VALUE "SUPPLIED TO:".
           03  FILLER          PIC X(46) VALUE "SUPPLIERS NAME :".
           03  H3-2            PIC X.
       01  SLIPE-HEAD4.
           03  H4-1            PIC XX.
           03  SE4-NAME1       PIC X(40) VALUE " ".
           03  SE4-NAME2.
               05  SE4-FAX     PIC X(12) VALUE " ".
               05  SE4-NO      PIC X(34) VALUE " ".
           03  H4-2            PIC X.
       01  SLIPE-HEAD5.
           03  H5-1            PIC XX.
           03  FILLER          PIC X(44) VALUE
           "Please Make Note Of Our VAT Registration #:".
           03  SE5-VAT-NUM     PIC X(42) VALUE " ".
           03  H5-2            PIC X.
       01  SLIPE-HEAD6.
           03  H6-1              PIC XX.
           03  FILLER            PIC X(27) VALUE " ".
           03  H6-DESC.
               05  H6-DESC-COM   PIC X(20).
               05  H6-DESC-PERC  PIC Z9.99.
           03  FILLER            PIC X(3) VALUE "%: ".
           03  H6-DESC2          PIC X(3) VALUE " ".
           03  H6-AMT            PIC Z(6)9.99-.
           03  FILLER            PIC X(17) VALUE " ".
           03  H6-2              PIC X.
       01  SLIPE-ACC.
           03  H07-1            PIC XX.
           03  FILLER           PIC X(15) VALUE "OUR ACCOUNT #:".
           03  H07-OUR-ACC      PIC X(25) VALUE " ".
           03  FILLER           PIC X(16) VALUE "YOUR ACCOUNT #:".
           03  H07-YOUR-ACC     PIC X(30) VALUE " ".
           03  H07-2            PIC X.
       01  SLIPE-ADJ.
           03  H9-1            PIC XX.
           03  FILLER          PIC X(1) VALUE " ".
           03  SA-LINE-NO      PIC Z9.
           03  SA-LINE-NO-DESC PIC X(2) VALUE " ".
           03  SA-DESC.
               05  SA-DESC1-1  PIC X(26).
               05  SA-DESC1-2  PIC X(24).
      *     03  FILLER          PIC X(1) VALUE " ".
           03  SA-DESC2        PIC X(3) VALUE "   ".
           03  SA-UNIT         PIC Z(6)9.99-.
           03  FILLER          PIC X(17) VALUE " ".
           03  H9-2            PIC X.
       01  SLIPE-MONTH.
           03  H10-1            PIC XX.
           03  FILLER           PIC X(1) VALUE " ".
           03  H10-DESC.
               05  H10-DESC1-1  PIC X(26).
               05  H10-DESC1-2  PIC X(24).
           03  FILLER           PIC X(35) VALUE " ".
           03  H10-2            PIC X.
       01  SLIPE-HEAD11.
           03  H11-1           PIC XX.
           03  FILLER          PIC X(6) VALUE " ".
           03  SLIPE-COMM-LINE PIC X(29) VALUE " ".
           03  SLIPE-COMM-REF  PIC Z(5)9.
           03  FILLER          PIC X(45) VALUE " ".
           03  H11-2           PIC X.
       01  SLIPE-COMMENT.
           03  H12-1           PIC XX.
           03  FILLER          PIC X(6) VALUE " ".
           03  H12-COMMENT     PIC X(80) VALUE " ".
           03  H12-2           PIC X.
       Copy "WsDateInfo".
       Copy "FaxInfo".
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-005.
           MOVE 0310 TO POS.
           DISPLAY "*** CREDITOR REMITTANCE EMAIL PRINTING ****" AT POS
           MOVE 0610 TO POS
           DISPLAY 
           "ENTER YY=YEAR, MM=MONTH FOR THE REMITTANCE : [    ]" AT POS
           ADD 46 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 4         TO CDA-DATALEN.
           MOVE 3         TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-REMIT-PERIOD.

           IF W-ESCAPE-KEY = 3
               PERFORM END-OFF.
       CONTROL-006.
           MOVE "L" TO WS-REMIT-F-L.
           MOVE 0810 TO POS
           DISPLAY 
           "                  ENTER L=LOCAL, F=FOREIGN : [ ]" AT POS
           ADD 46 TO POS.

           MOVE 'L'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-REMIT-F-L.

           IF WS-REMIT-F-L NOT = "F" AND NOT = "L"
              MOVE "FOREIGN / LOCAL FIELD MUST BE F OR L, RE-ENTER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO CONTROL-006.
       CONTROL-010.
           PERFORM GET-DATA.
           IF WS-END = "Y"
              GO TO CONTROL-005.
       CONTROL-020.
           PERFORM GET-EMAIL-FILE-NAME.
           MOVE Spaces         TO WS-PRINTER.
           MOVE WS-EMAIL-REMIT To Ws-Printer.

           MOVE 2410 TO POS.
           DISPLAY "PRINTING OF EMAIL FILE :                   " AT POS.
           ADD 24 TO POS
           DISPLAY WS-PRINTER AT POS.

           PERFORM ERROR-020.
           IF WS-REMIT-F-L = "L" OR = "F"
              PERFORM PRINT-LOCAL-EMAIL-PDF-REMIT
           ELSE
              PERFORM PRINT-FOREIGN-EMAIL-REMIT.
           PERFORM ERROR-020.
           PERFORM ERROR1-020.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE " " TO WS-RANGE1 WS-RANGE2.
           PERFORM CLEAR-010.
           MOVE 1010 TO POS.
           DISPLAY "ENTER BEG. ACC NUMBER   : [       ]" AT POS.
           ADD 27 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 36        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

           IF W-ESCAPE-KEY = 4
               MOVE "Y" TO WS-END
               GO TO GET-999.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-001
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-001.
           MOVE 1210 TO POS.
           DISPLAY "ENTER END ACC NUMBER    : [       ]" AT POS.
           ADD 27 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 36        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           IF W-ESCAPE-KEY = 4
               GO TO GET-000.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               MOVE 1 TO SUB-1
               GO TO GET-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.
           MOVE 1 TO SUB-1.
       GET-010.
           MOVE " " TO WS-COMMENT (SUB-1).
           PERFORM ERROR-020.
           MOVE 1514 TO POS
           DISPLAY WS-MESSAGE AT POS.
           IF SUB-1 < 4
               ADD 1 TO SUB-1
               GO TO GET-010.
           MOVE "PLEASE CONTACT RAY, PENNY OR LIZ ON 011-618-3325"
             TO WS-COMMENT (1)
           MOVE "FOR ANY ACCOUNT QUERIES." TO WS-COMMENT (2).

           MOVE 1 TO SUB-1.
           MOVE 1410 TO POS.
           DISPLAY
           "ENTER A COMMENT TO BE PRINTED ON THE REMITTANCE, 4 " &
           "LINES MAXIMUM." AT POS.
           MOVE 1514 TO POS.
           DISPLAY "[" AT POS.
           ADD 61 TO POS.
           DISPLAY "]" AT POS.
           MOVE " " TO WS-MESSAGE.
       GET-015.
           MOVE 1504 TO POS.
           DISPLAY "No=" AT POS.
           ADD 3 TO POS.
           DISPLAY SUB-1 AT POS.
           MOVE 1515 TO POS.
           
           MOVE WS-COMMENT (SUB-1) TO CDA-DATA.
           MOVE 60        TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 14        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-MES-COMMENT.

           MOVE WS-MES-COMMENT     TO WS-COMMENT (SUB-1).
           MOVE " "                TO WS-MES-COMMENT.
           DISPLAY WS-MES-COMMENT AT POS.
           IF W-ESCAPE-KEY = 4
            IF SUB-1 = 1
               GO TO GET-001
            ELSE
               SUBTRACT 1 FROM SUB-1
               MOVE WS-COMMENT (SUB-1) TO WS-MES-COMMENT
               GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-016
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-016.
           IF WS-COMMENT (SUB-1) NOT = "    "
            IF SUB-1 < 4
              ADD 1 TO SUB-1
              GO TO GET-015.
           MOVE 1 TO SUB-1.
           MOVE 1810 TO POS
           DISPLAY WS-MES-COMMENT AT POS.
           MOVE 1801 TO POS
           DISPLAY WS-MES-COMMENT AT POS.
       GET-030.
           MOVE 2110 TO POS.
           DISPLAY "ENTER : E=EMAIL, N=PRINT ONLY  : [ ]." AT POS.
           ADD 34 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 18        TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FAX-Y-N.

           IF W-ESCAPE-KEY = 4
               GO TO GET-015.
           IF WS-FAX-Y-N NOT = "E"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
           IF WS-FAX-Y-N = "N"
               MOVE "SELECTION MUST BE 'E' FOR NOW." TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-030.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-040.
           MOVE "N" TO WS-PRINT-NO.
           MOVE 2310 TO POS.
           DISPLAY "PRINT ACC'S WITH NO EMAIL Y OR N:[ ]." AT POS.
           ADD 34 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 20        TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PRINT-NO.

           IF W-ESCAPE-KEY = 4
               GO TO GET-030.
           IF WS-PRINT-NO NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
       GET-050.
           PERFORM ERROR-020.
           MOVE 1801 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1900 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1920 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
       GET-999.
           EXIT.
      *
       ADD-TOTALS SECTION.
       ATS-001.
            MOVE 0 TO WS-TOTAL-ADJ WS-TOTAL-GROSS WS-TOTAL-NETT.
            MOVE 1 TO SUB-10.
       ATS-002.
      *      COMPUTE CRREM-BAL-NOW = CRREM-BAL-LAST + CRREM-PREV-PAID.
       ATS-005.
            COMPUTE WS-TOTAL-ADJ = WS-TOTAL-ADJ + CRREM-AMT (SUB-10).
            ADD 1 TO SUB-10.
            IF SUB-10 < 41
                GO TO ATS-005.
            MOVE 1 TO SUB-10.
       ATS-010.
            COMPUTE WS-TOTAL-GROSS = CRREM-BAL-NOW + WS-TOTAL-ADJ.
       ATS-020.
            COMPUTE WS-TOTAL-NETT = WS-TOTAL-GROSS - WS-DISC-AMT.
       ATS-999.
            EXIT.
      *
       GET-EMAIL-PDF-FILE-NAME SECTION.
       GEQNP-002.
           MOVE WS-REMIT-PERIOD    TO WS-PDF-EREMIT
           MOVE Ws-Co-Number       TO WS-PDF-ECOMPANY
           MOVE CR-ACCOUNT-NUMBER  TO WS-PDF-ACC-NUM.
           IF PAGE-CNT = 0 
              MOVE 1               TO WS-PDF-PAGE
           ELSE 
              MOVE PAGE-CNT        TO WS-PDF-PAGE.
           MOVE WS-EMAIL-PDF-REMIT TO WS-PRINTER.
           IF PAGE-CNT = 0 
              MOVE WS-PRINTER TO WS-PRINTER-PAGE1
           ELSE
              MOVE WS-PRINTER TO WS-PRINTER-PAGE2.
       GEQNP-999.
           EXIT.
      *
       PRINT-LOCAL-EMAIL-PDF-REMIT SECTION.
       PR-PDF-000.
           MOVE "E" TO WS-AUTO-FAX
           MOVE "X" TO WS-ANSWER.
           MOVE WS-RANGE1 TO WS-NUMBER.
           GO TO PR-PDF-00002.
       PR-PDF-00001.
           OPEN OUTPUT PRINT-FILE.
       PR-PDF-00002.
           PERFORM START-CRREMIT.
           MOVE 0 TO STATE-CNT.
       PR-PDF-00005.
           MOVE 0 TO PAGE-CNT
                     WS-TOTAL-ADJ
                     WS-TOTAL-GROSS
                     WS-TOTAL-NETT.
           MOVE 66 TO LINE-CNT.
       PR-PDF-00010.
           GO TO PR-PDF-005.
       PR-PDF-00011.
           IF PAGE-CNT = 0
                MOVE CR-ACC-EMAIL TO WS-DATA-F WSF-MAIL-NUMBER
           ELSE
                MOVE SPACES       TO WS-DATA-F WSF-MAIL-NUMBER.
           WRITE PRINT-REC FROM WS-FST-LINE AFTER 1.
       PR-PDF-005.
           PERFORM READ-REMIT-NEXT.
           
           IF WS-END = "Y"
               GO TO PR-PDF-999.
           IF CRREM-ACC-NUMBER < WS-RANGE1
               GO TO PR-PDF-005.
           IF CRREM-ACC-NUMBER > WS-RANGE2
               GO TO PR-PDF-999.
               
      * N=NOT CHECKED, P=CHECKED, PENDING POSTING, Y= COMPLETE - POSTED
           IF CRREM-COMPLETE NOT = "P"
               GO TO PR-PDF-005.
      *******************************************************
      * SEE READ-CREDITOR LATER IN READ-REMIT-NEXT SECTION  *  
      *******************************************************
      *     PERFORM READ-CREDITOR.
           IF CR-NAME = "** UNKNOWN **"
               GO TO PR-PDF-005.
               
           MOVE 2510 TO POS
           DISPLAY "PROCESSING REMITTANCE FOR ACCOUNT:" AT POS
           ADD 35 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS
           ADD 1 TO STATE-CNT
           ADD 10 TO POS
           DISPLAY STATE-CNT AT POS.
           
           PERFORM GET-EMAIL-PDF-FILE-NAME           
           PERFORM PR-PDF-00001.

           IF LINE-CNT < 62
               GO TO PR-PDF-010.
       PR-PDF-006.
           ADD 1                         TO PAGE-CNT.
           IF PAGE-CNT > 1
            IF PAGE-CNT < 3
               CLOSE PRINT-FILE
               PERFORM GET-EMAIL-PDF-FILE-NAME           
               PERFORM PR-PDF-00001
               MOVE " "                TO PRINT-REC
               MOVE CRREM-ACC-NUMBER   TO H07-OUR-ACC
               MOVE CR-SUPPLIER-NUMBER TO H07-YOUR-ACC
               WRITE PRINT-REC FROM SLIPE-ACC AFTER 1.
    
           MOVE " "                    TO SLIP-PDF-HEAD1.
           MOVE CR-NAME                TO S-PDF-NAME1
           WRITE PRINT-REC FROM SLIP-PDF-HEAD1 AFTER 11.

           MOVE GLPA-NAME              TO S-PDF-NAME1
           MOVE PAGE-CNT               TO S-PDF-PAGE
           WRITE PRINT-REC FROM SLIP-PDF-HEAD1 AFTER 2.

           PERFORM GET-REPORT-Y2K-DATE
           MOVE "* REMITTANCE ADVICE *" TO S-PDF-NAME1
           MOVE PBRET                   TO S-PDF-NAME2
           WRITE PRINT-REC FROM SLIP-PDF-HEAD1 AFTER 2
           MOVE " "                     TO SLIP-PDF-HEAD1.

           MOVE CRREM-ACC-NUMBER        TO SPC-OUR-ACC
           MOVE CR-SUPPLIER-NUMBER      TO SPC-YOUR-ACC
           WRITE PRINT-REC FROM SLIP-PDF-ACC AFTER 2.
           

           WRITE PRINT-REC FROM SLIPE-HEAD1 AFTER 2.
           
           WRITE PRINT-REC FROM SLIPE-HEAD2 AFTER 1.

           MOVE " "          TO PRINT-REC
           WRITE PRINT-REC FROM SLIPE-HEAD3 AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE CR-FAX          TO WS-FAX-NUMBER.
           MOVE CR-ACC-EMAIL    TO WS-DATA-F WSF-MAIL-NUMBER.
           IF WS-AUTO-FAX = "E"
              MOVE "REMITTANCE # :"  TO WS-SUBJECT-LINE1
              MOVE CR-ACCOUNT-NUMBER TO WS-SUBJECT-LINE2
              MOVE " FROM:"          TO WS-SUBJECT-LINE3 
              MOVE WS-CO-NAME        TO WS-SUBJECT-LINE4
              PERFORM TAKE-OUT-BLANKS-IN-CO-NAME.
      
           MOVE " "             TO PRINT-REC
           MOVE GLPA-NAME       TO SE4-NAME1
           MOVE CR-NAME         TO SE4-NAME2
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "             TO PRINT-REC
           MOVE GLPA-ADD1       TO SE4-NAME1
           MOVE CR-ADDRESS1     TO SE4-NAME2
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "             TO PRINT-REC
           MOVE GLPA-ADD2       TO SE4-NAME1
           MOVE CR-ADDRESS2     TO SE4-NAME2
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "              TO PRINT-REC
           MOVE GLPA-ADD3        TO SE4-NAME1.
           IF CR-ADDRESS3 = " "
               MOVE CR-POST-CODE TO SE4-NAME2
           ELSE
               MOVE CR-ADDRESS3  TO SE4-NAME2.
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "              TO PRINT-REC.
           MOVE " "              TO SE4-NAME1
           IF CR-ADDRESS3 > " "
               MOVE CR-POST-CODE TO SE4-NAME2
           ELSE
               MOVE " "          TO SE4-NAME2.
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE " "                TO PRINT-REC.
           MOVE CRREM-ACC-NUMBER   TO H07-OUR-ACC
           MOVE CR-SUPPLIER-NUMBER TO H07-YOUR-ACC
           WRITE PRINT-REC FROM SLIPE-ACC AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.

           MOVE " "               TO PRINT-REC
           MOVE GLPA-GLVAT-REG-NO TO SE5-VAT-NUM.
           WRITE PRINT-REC FROM SLIPE-HEAD5 AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE " "                         TO PRINT-REC
                                               SLIPE-ADJ
           MOVE "BALANCE AS PER STATEMENT:" TO SA-DESC1-1
      *****************************************************************
      * IF WS-REMI-MM = 1 IT MEANS WE ARE RUNNING REMITTANCES FOR JAN *
      * THEREFORE 1 - 1 = 0, THE SUBSCRIPT WILL FALL OVER BELOW.      *
      * THE PREVIOUS MONTH FROM JAN AS AN OPENING BALANCE WOULD BE    *
      * DECEMBER SO WE CHANGE THE WS-REMI-MM TO 13 FOR THIS PURPOSE.  *
      *****************************************************************
           SUBTRACT 1 FROM WS-REMI-MM.
           IF WS-REMI-MM = 0
               MOVE 13 TO WS-REMI-MM
           ELSE
               ADD 1   TO WS-REMI-MM.
           IF WS-REMIT-F-L = "L"
             MOVE WS-MM-DESC (WS-REMI-MM - 1) TO SA-DESC1-2
             MOVE " R"                        TO SA-DESC2
             MOVE CRREM-BAL-LAST              TO SA-UNIT
             WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1
           ELSE
             MOVE WS-MM-DESC (WS-REMI-MM - 1) TO SA-DESC1-2
             MOVE CR-CURRENCY                 TO SA-DESC2
             MOVE CRREM-BAL-LAST              TO SA-UNIT
             WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
      *****************************************************************
      * RESET THE WS-REMI-MM BACK TO 1 FOR JAN AS WE CHANGED IT ABOVE *
      * AND WS-REMI-MM = 1 FOR JAN IS USED AGAIN LATER IN THE PROGRAM *
      * THEREFORE IT MUST BE MADE 1 AGAIN ONCE THE PREVIOUS MONTH     *
      * (OPENING BALANCE MONTH NAME) IS FOUND AND USED.               *
      *****************************************************************
           IF WS-REMI-MM = 13
               MOVE 1 TO WS-REMI-MM.
           
           MOVE " "                         TO PRINT-REC
           MOVE "LESS AMOUNTS ALREADY PAID" TO SA-DESC1-1
           MOVE " "                         TO SA-DESC1-2
           IF WS-REMIT-F-L = "L"
              MOVE " R"                     TO SA-DESC2
           ELSE
             MOVE CR-CURRENCY               TO SA-DESC2.
           MOVE CRREM-PREV-PAID             TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
           
           MOVE " "                                TO PRINT-REC
           MOVE "TOTAL BALANCE BEFORE ADJUSTMENTS" TO SA-DESC
           IF WS-REMIT-F-L = "L"
             MOVE " R"                             TO SA-DESC2
           ELSE
             MOVE CR-CURRENCY                      TO SA-DESC2.
           MOVE CRREM-BAL-NOW                      TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
       PR-PDF-0061.
           MOVE " "                         TO PRINT-REC
           MOVE "ADJUSTMENTS FOR THE MONTH" TO H10-DESC1-1
           MOVE WS-MM-DESC (WS-REMI-MM)     TO H10-DESC1-2
           WRITE PRINT-REC FROM SLIPE-MONTH AFTER 1.
       PR-PDF-007.
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
       PR-PDF-0071.
      * SUB-1 HERE IS THE COUNT OF ADJUSTMENTS PROCESSED   - MAX 40
      * LINE-CNT IS THE LINE COUNT STARTING AT ADJUSTMENTS - MAX 26 PAGE
           MOVE 1 TO SUB-1
                     LINE-CNT.
       PR-PDF-008.
      ************************
      * PRINTING ADJ LINES.  *
      ************************
           IF CRREM-DESC (SUB-1) = " "
               PERFORM PR-PDF-900
               PERFORM PR-PDF-800
               MOVE "N" TO WS-END
               GO TO PR-PDF-010.
      
           MOVE " "                 TO PRINT-REC
           MOVE CRREM-DESC (SUB-1)  TO SA-DESC
           MOVE CRREM-AMT (SUB-1)   TO SA-UNIT
           MOVE SUB-1               TO SA-LINE-NO
           MOVE ")"                 TO SA-LINE-NO-DESC
           IF WS-REMIT-F-L = "L"
              MOVE " R"             TO SA-DESC2
           ELSE
              MOVE CR-CURRENCY      TO SA-DESC2.
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.

           ADD CRREM-AMT (SUB-1) TO WS-TOTAL-ADJ.

           ADD 1 TO LINE-CNT SUB-1.
      *    IF WS-END NOT = "Y" 
           IF LINE-CNT > 26
               ADD 1 TO PAGE-CNT
            IF PAGE-CNT < 3
               CLOSE PRINT-FILE
               PERFORM GET-EMAIL-PDF-FILE-NAME           
               PERFORM PR-PDF-00001
               MOVE " "                     TO PRINT-REC
               MOVE "* REMITTANCE ADVICE *" TO S-PDFT-TRAIL
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 1
               MOVE " "                     TO SLIP-PDF-TRAILER
               MOVE PAGE-CNT                TO S-PDFT-PAGE
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 3
               MOVE " "                     TO PRINT-REC
               WRITE PRINT-REC AFTER 3
               MOVE 1 TO LINE-CNT 
               GO TO PR-PDF-008
            ELSE
               MOVE " "                     TO PRINT-REC
               WRITE PRINT-REC AFTER PAGE
               MOVE "* REMITTANCE ADVICE *" TO S-PDFT-TRAIL
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 1
               MOVE " "                     TO SLIP-PDF-TRAILER
               MOVE PAGE-CNT                TO S-PDFT-PAGE
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 3
               MOVE " "                     TO PRINT-REC
               WRITE PRINT-REC AFTER 3
               MOVE 1 TO LINE-CNT 
               GO TO PR-PDF-008.
               
           IF SUB-1 < 40
              GO TO PR-PDF-008
           ELSE
               PERFORM PR-PDF-900
               PERFORM PR-PDF-800
               MOVE "N" TO WS-END
               GO TO PR-PDF-010.
           IF LINE-CNT < 27
               PERFORM PR-PDF-650.
       PR-PDF-010.
      *     IF LINE-CNT > 20
           IF LINE-CNT > 50
               ADD 1                        TO PAGE-CNT
           IF PAGE-CNT > 1
            IF PAGE-CNT < 3
               CLOSE PRINT-FILE
               PERFORM GET-EMAIL-PDF-FILE-NAME           
               PERFORM PR-PDF-00001
               MOVE " "                     TO PRINT-REC
               MOVE "* REMITTANCE ADVICE *" TO S-PDFT-TRAIL
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 1
               MOVE " "                     TO SLIP-PDF-TRAILER
               MOVE PAGE-CNT                TO S-PDFT-PAGE
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 3
               MOVE " "                     TO PRINT-REC
               WRITE PRINT-REC AFTER 3
               MOVE 1 TO LINE-CNT.
       
           MOVE " "                         TO PRINT-REC SLIPE-ADJ
           MOVE " "                         TO SA-DESC1-1
           MOVE "TOTAL ADJUSTMENTS     :"   TO SA-DESC1-2
           IF WS-REMIT-F-L = "F"
              MOVE CR-CURRENCY              TO SA-DESC2
           ELSE
              MOVE " R"                     TO SA-DESC2.
           MOVE WS-TOTAL-ADJ                TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
           
           COMPUTE WS-TOTAL-GROSS = CRREM-BAL-NOW + WS-TOTAL-ADJ.

           MOVE " "                         TO PRINT-REC
           MOVE " "                         TO SA-DESC1-1
           MOVE "TOTAL BEFORE DISCOUNT :"   TO SA-DESC1-2
           IF WS-REMIT-F-L = "F"
              MOVE CR-CURRENCY              TO SA-DESC2
           ELSE
              MOVE " R"                     TO SA-DESC2.
           MOVE WS-TOTAL-GROSS              TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.

           MOVE " "                                  TO PRINT-REC
           MOVE "    LESS DISCOUNT @"                TO H6-DESC-COM
           IF WS-REMIT-F-L = "F"
              MOVE CR-CURRENCY                       TO H6-DESC2
           ELSE
              MOVE " R "                             TO H6-DESC2.
           MOVE CR-SETT-DISC                         TO H6-DESC-PERC
           MOVE CRREM-DISC-AMT                       TO H6-AMT
           WRITE PRINT-REC FROM SLIPE-HEAD6 AFTER 1.
           
           COMPUTE WS-TOTAL-NETT = WS-TOTAL-GROSS - CRREM-DISC-AMT.

           MOVE " "                         TO PRINT-REC
           MOVE "THIS MONTHS PAYMENT   : "  TO SA-DESC1-2
           IF WS-REMIT-F-L = "F"
              MOVE CR-CURRENCY              TO SA-DESC2
           ELSE
              MOVE " R"                     TO SA-DESC2.
           MOVE WS-TOTAL-NETT               TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
       PR-PDF-500.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.

           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           IF WS-REMIT-F-L = "F"
              MOVE "AUTHORISED FOR BANK PAYMENT " TO SLIPE-COMM-LINE
           ELSE
              MOVE "AUTHORISED FOR EFT PAYMENT #" TO SLIPE-COMM-LINE
              MOVE CRREM-PMT-REF                  TO SLIPE-COMM-REF.
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE 1 TO SUB-1.
           MOVE 6 TO LINE-CNT.
       PR-PDF-550.
           IF WS-COMMENT (SUB-1) = " "
               GO TO PR-PDF-570.
           MOVE " "                TO PRINT-REC SLIPE-HEAD11
           MOVE WS-COMMENT (SUB-1) TO H12-COMMENT
           WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.
           
           ADD 1 TO SUB-1 LINE-CNT.
           IF SUB-1 < 5
               GO TO PR-PDF-550.
      ************************************************************
      * THIS GO TO PR-PDF-00005 IS BECAUSE ALL 4 LINES OF COMMENT*
      * MUST HAVE BEEN PRINTED SO DO NOT PRINT AN EXTRA LINE IN  *
      * PR-PDF-570.                                              *
      ************************************************************
      *LINE BELOW TAKEN OUT FOR TEST PURPOSES     
            PERFORM REWRITE-CRREMIT.
           GO TO PR-PDF-00005.
       PR-PDF-570.
      *LINE BELOW TAKEN OUT FOR TEST PURPOSES     
           PERFORM REWRITE-CRREMIT.
           PERFORM PR-PDF-950.
           GO TO PR-PDF-00005.
       PR-PDF-600.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 1 TO LINE-CNT.
           IF LINE-CNT < 11
              GO TO PR-PDF-600.
       PR-PDF-650.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 1 TO LINE-CNT.
       PR-PDF-660.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 1 TO LINE-CNT.
           IF LINE-CNT < 20
              GO TO PR-PDF-660.
       PR-PDF-800.
           PERFORM READ-REMIT-TRANS-NEXT.
      **********************
      * LOCAL REMIT ONLY   *
      **********************
      *INVOICES MARKED FOR PAYMENT
           IF WS-END NOT = "Y"
            IF WS-REMIT-F-L = "F"
              MOVE "*INV:  "   TO WS1-FIL
              MOVE "AMT:   "   TO WS2-FIL
              MOVE "DISC:  "   TO WS3-FIL
              MOVE "NETT:   "  TO WS4-FIL
            ELSE
              MOVE "*INV: R"   TO WS1-FIL
              MOVE "AMT:  R"   TO WS2-FIL
              MOVE "DISC: R"   TO WS3-FIL
              MOVE "NETT: R "  TO WS4-FIL.

           IF WS-END NOT = "Y"
              MOVE CRREMTR-INVNO     TO WS-IP-INV
              MOVE CRREMTR-INV-AMT   TO WS-IP-AMT
              MOVE CRREMTR-DISC-AMT  TO WS-IP-DISC
              COMPUTE WS-NETT-AMT = CRREMTR-INV-AMT - CRREMTR-DISC-AMT
              MOVE WS-NETT-AMT       TO WS-IP-NETT
             
              MOVE " "             TO PRINT-REC WS-OTH-LINE
              MOVE WS-INV-PAY-LINE TO WS-DATA-O
              WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1
              ADD 1 TO LINE-CNT.
           IF PAGE-CNT = 1
           IF WS-END NOT = "Y" 
            IF LINE-CNT > 26
               ADD 1 TO PAGE-CNT
            IF PAGE-CNT < 3
               CLOSE PRINT-FILE
               PERFORM GET-EMAIL-PDF-FILE-NAME           
               PERFORM PR-PDF-00001
               MOVE " "                     TO PRINT-REC
               MOVE "* REMITTANCE ADVICE *" TO S-PDFT-TRAIL
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 1
               MOVE " "                     TO SLIP-PDF-TRAILER
               MOVE PAGE-CNT                TO S-PDFT-PAGE
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 3
               MOVE " "                     TO PRINT-REC
               WRITE PRINT-REC AFTER 3
               MOVE 1 TO LINE-CNT 
               GO TO PR-PDF-800.
           IF PAGE-CNT > 1
           IF WS-END NOT = "Y" 
            IF LINE-CNT > 50
               ADD 1 TO PAGE-CNT
            IF PAGE-CNT < 3
               CLOSE PRINT-FILE
               PERFORM GET-EMAIL-PDF-FILE-NAME           
               PERFORM PR-PDF-00001
               MOVE " "                     TO PRINT-REC
               MOVE "* REMITTANCE ADVICE *" TO S-PDFT-TRAIL
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 1
               MOVE " "                     TO SLIP-PDF-TRAILER
               MOVE PAGE-CNT                TO S-PDFT-PAGE
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 3
               MOVE " "                     TO PRINT-REC
               WRITE PRINT-REC AFTER 3
               MOVE 1 TO LINE-CNT 
               GO TO PR-PDF-800
            ELSE
               MOVE " "                     TO PRINT-REC
               WRITE PRINT-REC AFTER PAGE
               MOVE "* REMITTANCE ADVICE *" TO S-PDFT-TRAIL
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 1
               MOVE " "                     TO SLIP-PDF-TRAILER
               MOVE PAGE-CNT                TO S-PDFT-PAGE
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 3
               MOVE " "                     TO PRINT-REC
               WRITE PRINT-REC AFTER 3
               MOVE 1 TO LINE-CNT 
               GO TO PR-PDF-800.

           IF WS-END NOT = "Y" 
               GO TO PR-PDF-800.

           PERFORM PR-PDF-650.
       PR-PDF-900.
            IF LINE-CNT > 20
               ADD 1 TO PAGE-CNT
           IF PAGE-CNT > 1
            IF PAGE-CNT < 3
               CLOSE PRINT-FILE
               PERFORM GET-EMAIL-PDF-FILE-NAME           
               PERFORM PR-PDF-00001
               MOVE " "                     TO PRINT-REC
               MOVE "* REMITTANCE ADVICE *" TO S-PDFT-TRAIL
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 1
               MOVE " "                     TO SLIP-PDF-TRAILER
               MOVE PAGE-CNT                TO S-PDFT-PAGE
               WRITE PRINT-REC FROM SLIP-PDF-TRAILER AFTER 3
               MOVE " "                     TO PRINT-REC
               WRITE PRINT-REC AFTER 3
               MOVE 1 TO LINE-CNT.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE " "                         TO PRINT-REC
           MOVE "INVOICES PAID THIS MONTH:" TO H10-DESC1-1
           MOVE WS-MM-DESC (WS-REMI-MM)     TO H10-DESC1-2
           WRITE PRINT-REC FROM SLIPE-MONTH AFTER 1.

           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 3 TO LINE-CNT.
           
           PERFORM START-CRREMIT-TRANS.
       PR-PDF-910.
           MOVE " "                         TO PRINT-REC
           MOVE "INVOICES PAID THIS MONTH:" TO H10-DESC1-1
           MOVE WS-MM-DESC (WS-REMI-MM)     TO H10-DESC1-2
           WRITE PRINT-REC FROM SLIPE-MONTH AFTER 1.

           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE 1 TO LINE-CNT
           MOVE 1 TO SUB-1.
       PR-PDF-945.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 2.
       PR-PDF-950.
      ****************************************************************
      * THIS LAST WRITE IS TO PUT THE LAST RETURN CHAR ON THE VERY   *
      * LAST LINE WHICH OTHERWISE WOULD BE OMMITTED.                 *
      ****************************************************************
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           CLOSE PRINT-FILE.
      * IF PAGE-CNT > 2 WE MOVE 2 TO PAGE-CNT AS THERE ARE ONLY 
      * TWO FILES CREATED - 1 AND 2.  2 HAS ALL THE SUBSEQUENT PAGES
      * INSIDE IT.
           IF PAGE-CNT = 0
              MOVE 1 TO PAGE-CNT.
           IF PAGE-CNT > 2 
              MOVE 2 TO PAGE-CNT.

      * In the PrintRemit1 shell script:
      * #1 = WS-CO-NUMBER
      * #2 = WS-PRINTER-SAVE (THE PRINTER NAME) E.G. MP140
      * #3 = WS-REFERENCE-NUMBER E.G. 2002Co010300150-1
           IF PAGE-CNT = 1
                 PERFORM WORK-OUT-PDF-FILE-NAMES
                 MOVE WS-PRINTER-PAGE1   TO WS-PRINTER
                 PERFORM FIND-PDF-TYPE-PRINTER
                 PERFORM SETUP-REMIT-FOR-PDF
           ELSE
                 PERFORM WORK-OUT-PDF-FILE-NAMES
                 MOVE WS-PRINTER-PAGE1   TO WS-PRINTER
                 PERFORM FIND-PDF-TYPE-PRINTER
                 PERFORM SETUP-REMIT-FOR-PDF
                 MOVE WS-PRINTER-PAGE2   TO WS-PRINTER
                 PERFORM SETUP-REMIT2-FOR-PDF
                 PERFORM SETUP-MERGE-REMIT-FOR-PDF.
          IF WS-AUTO-FAX = "E"
              PERFORM MAKE-PDF-FINAL-FOR-EMAIL
              PERFORM SETUP-REMIT-FOR-PDF-MGEMAIL.
              
          CALL "C$SLEEP" USING 1.
              
       PR-PDF-999.
      * 
      *    ' ', '-f', ' ', TRIM(WS-PRINTER-PDF),
      *    ' ', '-F', ' ', TRIM(WS-USEREMAILADD),
      *    ' ', '-r', ' ', TRIM(WS-USEREMAILADD),
      *    ' ', '-s', ' ', TRIM(WS-SUBJECT-FIXED),
      *    ' ', '-t', ' ', 'MGMessageCSCRemit.html',
      *    ' ', '-T', ' ', TRIM(WSF-MAIL-NUMBER))

           EXIT.
      *
       FIND-PDF-TYPE-PRINTER SECTION.
       FPTP-040.
           MOVE 1 TO SUB-45.
       FPTP-045.
           IF WS-PRINTERNAME (SUB-45) = " "
              MOVE 
           "NO PDF PRINTERNUMBER, PRN PARAMETER NOT SET UP."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO FPTP-999.
           IF WS-PRINTERNUMBER (SUB-45) = 15
               MOVE WS-PRINTERNAME (SUB-45)  TO WS-PRINTER-SAVE
               GO TO FPTP-999.
           IF SUB-45 < 25
             ADD 1 TO SUB-45
             GO TO FPTP-045.
           MOVE 
           "CAN'T FIND A PDF PRINTERNUMBER, PRN PARAMETER NOT SET UP."
             TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
       FPTP-999.
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
           
      *     MOVE "TAKE OUT BLANKS" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     MOVE WS-SUBJECT TO WS-MESSAGE
      *     PERFORM ERROR1-000

      *     MOVE WS-SUBJECT-FIXED TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     PERFORM ERROR1-020.
           
       TOBICN-999.
           EXIT.
      *
       WORK-OUT-PDF-FILE-NAMES SECTION.
       WOPFN-001.
      *     MOVE WS-PRINTER TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE WS-PRINTER-PAGE1 TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR1-020.
       
           MOVE SPACES           TO ALPHA-RATE DATA-RATE.
           MOVE WS-PRINTER-PAGE1 TO ALPHA-RATE.
           MOVE 12 TO SUB-45
           MOVE 1  TO SUB-46.
       WOPFN-010.
           MOVE AL-RATE (SUB-45) TO DAT-RATE (SUB-46)
           ADD 1 TO SUB-45 SUB-46.
           IF AL-RATE (SUB-45) NOT = " "
               GO TO WOPFN-010.
           MOVE DATA-RATE TO WS-PRINTER-PAGE1.
           
           MOVE SPACES           TO ALPHA-RATE DATA-RATE
           MOVE WS-PRINTER-PAGE2 TO ALPHA-RATE.
           MOVE 12 TO SUB-45
           MOVE 1  TO SUB-46.
       WOPFN-015.
           MOVE AL-RATE (SUB-45) TO DAT-RATE (SUB-46)
           ADD 1 TO SUB-45 SUB-46.
           IF AL-RATE (SUB-45) NOT = " "
               GO TO WOPFN-015.
           MOVE DATA-RATE        TO WS-PRINTER-PAGE2.
           MOVE SPACES           TO ALPHA-RATE DATA-RATE.

      *     MOVE WS-PRINTER TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE WS-PRINTER-PAGE1 TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR1-020.
       
       WOPFN-999.
           EXIT.
      *
       MAKE-PDF-FINAL-FOR-EMAIL SECTION.
       MFPFE-001.
           MOVE SPACES          TO ALPHA-RATE DATA-RATE.
       MFPFE-002.
           MOVE "/ctools/cr/"   TO ALPHA-RATE
           
           MOVE WS-CO-NUMBER    TO DATA-RATE
           MOVE DAT-RATE(1) TO AL-RATE(12)
           MOVE DAT-RATE(2) TO AL-RATE(13)

           MOVE 1 TO SUB-45
           MOVE 1 TO SUB-50.
       MFPFE-005.
           MOVE AL-RATE (SUB-45) TO DAT-RATE (SUB-50)
           ADD 1 TO SUB-45 SUB-50.
           IF AL-RATE (SUB-45) NOT = " "
               GO TO MFPFE-005.
           MOVE "R"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "e"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "m"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "i"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "t"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           IF PAGE-CNT = 1
              GO TO MFPFE-007.
           MOVE "F"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "i"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "n"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "a"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "l"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
       MFPFE-007.
           MOVE SPACES TO ALPHA-RATE.
           IF PAGE-CNT = 1
              MOVE WS-PRINTER-PAGE1 TO ALPHA-RATE
           ELSE 
              MOVE WS-PRINTER-PAGE2 TO ALPHA-RATE.
           MOVE 1 TO SUB-45.
       MFPFE-010.
           MOVE AL-RATE (SUB-45) TO DAT-RATE (SUB-50)
           ADD 1 TO SUB-45 SUB-50.
           IF AL-RATE (SUB-45) NOT = " "
               GO TO MFPFE-010.
           MOVE "."  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "p"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "d"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "f"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE DATA-RATE TO WS-PRINTER-PDF.

      *     MOVE "MAKE-PDF-FINAL ...." TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE WS-PRINTER-PDF TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR1-020.

           MOVE SPACES           TO ALPHA-RATE DATA-RATE.
       MFPFE-999.
           EXIT.
      *
       PRINT-FOREIGN-EMAIL-REMIT SECTION.
       PREF-000.
           OPEN OUTPUT PRINT-FILE.

           PERFORM Z1-HEADINGS.
           
           PERFORM START-CRREMIT.
           MOVE 0 TO STATE-CNT.
       PREF-00005.
           MOVE 0 TO PAGE-CNT
                     WS-TOTAL-ADJ
                     WS-TOTAL-GROSS
                     WS-TOTAL-NETT.
           MOVE 66 TO LINE-CNT.
       PREF-00010.
           MOVE "´¶"         TO WS-DELIM-F
           MOVE "¶"          TO H1-1
           MOVE "³"          TO H1-2.
           
           GO TO PREF-005.
       PREF-00011.
           IF PAGE-CNT = 0
                MOVE CR-ACC-EMAIL TO WS-DATA-F
           ELSE
                MOVE SPACES       TO WS-DATA-F.
           WRITE PRINT-REC FROM WS-FST-LINE AFTER 1.
       PREF-005.
           PERFORM READ-REMIT-NEXT.
           
           IF WS-END = "Y"
               GO TO PREF-950.
      * N=NOT CHECKED, P=CHECKED, PENDING POSTING, Y= COMPLETE - POSTED
           IF CRREM-COMPLETE NOT = "P"
               GO TO PREF-005.
      *******************************************************
      * SEE READ-CREDITOR LATER IN READ-REMIT-NEXT SECTION  *  
      *******************************************************
      *     PERFORM READ-CREDITOR.
           IF CR-NAME = "** UNKNOWN **"
               GO TO PREF-005.
               
           MOVE 2510 TO POS
           DISPLAY "PROCESSING REMITTANCE FOR ACCOUNT:" AT POS
           ADD 35 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS
           ADD 1 TO STATE-CNT
           ADD 10 TO POS
           DISPLAY STATE-CNT AT POS.
           
           PERFORM PREF-00011.

           IF LINE-CNT < 62
               GO TO PREF-010.
       PREF-006.
           MOVE "¶"                      TO H1-1
           MOVE "³"                      TO H1-2
           ADD 1                         TO PAGE-CNT.
           IF PAGE-CNT > 1
               PERFORM PREF-00011.
           WRITE PRINT-REC FROM SLIPE-HEAD1 AFTER 1.
           
           MOVE "¶"                      TO H2-1
           MOVE "³"                      TO H2-2
           WRITE PRINT-REC FROM SLIPE-HEAD2 AFTER 1.

           MOVE " "          TO PRINT-REC
           MOVE "¶"          TO H3-1
           MOVE "³"          TO H3-2
           WRITE PRINT-REC FROM SLIPE-HEAD3 AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE " "             TO PRINT-REC
           MOVE "¶"             TO H4-1
           MOVE "³"             TO H4-2
           MOVE GLPA-NAME       TO SE4-NAME1
           MOVE CR-NAME         TO SE4-NAME2
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "             TO PRINT-REC
           MOVE "¶"             TO H4-1
           MOVE "³"             TO H4-2
           MOVE GLPA-ADD1       TO SE4-NAME1
           MOVE CR-ADDRESS1     TO SE4-NAME2
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "             TO PRINT-REC
           MOVE "¶"             TO H4-1
           MOVE "³"             TO H4-2
           MOVE GLPA-ADD2       TO SE4-NAME1
           MOVE CR-ADDRESS2     TO SE4-NAME2
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "              TO PRINT-REC
           MOVE "¶"              TO H4-1
           MOVE "³"              TO H4-2
           MOVE GLPA-ADD3        TO SE4-NAME1.
           IF CR-ADDRESS3 = " "
               MOVE CR-POST-CODE TO SE4-NAME2
           ELSE
               MOVE CR-ADDRESS3  TO SE4-NAME2.
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "              TO PRINT-REC.
           MOVE "¶"              TO H4-1
           MOVE "³"              TO H4-2
           MOVE " "              TO SE4-NAME1
           IF CR-ADDRESS3 > " "
               MOVE CR-POST-CODE TO SE4-NAME2
           ELSE
               MOVE " "          TO SE4-NAME2.
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE " "                TO PRINT-REC.
           MOVE "¶"                TO H07-1
           MOVE "³"                TO H07-2
           MOVE CRREM-ACC-NUMBER   TO H07-OUR-ACC
           MOVE CR-SUPPLIER-NUMBER TO H07-YOUR-ACC
           WRITE PRINT-REC FROM SLIPE-ACC AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.

           MOVE " "               TO PRINT-REC
           MOVE "¶"               TO H5-1
           MOVE "³"               TO H5-2.
           MOVE GLPA-GLVAT-REG-NO TO SE5-VAT-NUM.
           WRITE PRINT-REC FROM SLIPE-HEAD5 AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE " "                         TO PRINT-REC
                                               SLIPE-ADJ
           MOVE "¶"                         TO H9-1
           MOVE "³"                         TO H9-2.
           MOVE "BALANCE AS PER STATEMENT:" TO SA-DESC1-1
      *****************************************************************
      * IF WS-REMI-MM = 1 IT MEANS WE ARE RUNNING REMITTANCES FOR JAN *
      * THEREFORE 1 - 1 = 0, THE SUBSCRIPT WILL FALL OVER BELOW.      *
      * THE PREVIOUS MONTH FROM JAN AS AN OPENING BALANCE WOULD BE    *
      * DECEMBER SO WE CHANGE THE WS-REMI-MM TO 13 FOR THIS PURPOSE.  *
      *****************************************************************
           SUBTRACT 1 FROM WS-REMI-MM.
           IF WS-REMI-MM = 0
               MOVE 13 TO WS-REMI-MM
           ELSE
               ADD 1   TO WS-REMI-MM.
           MOVE WS-MM-DESC (WS-REMI-MM - 1) TO SA-DESC1-2
           MOVE CR-CURRENCY                 TO SA-DESC2
           MOVE CRREM-BAL-LAST              TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
      *****************************************************************
      * RESET THE WS-REMI-MM BACK TO 1 FOR JAN AS WE CHANGED IT ABOVE *
      * AND WS-REMI-MM = 1 FOR JAN IS USED AGAIN LATER IN THE PROGRAM *
      * THEREFORE IT MUST BE MADE 1 AGAIN ONCE THE PREVIOUS MONTH     *
      * (OPENING BALANCE MONTH NAME) IS FOUND AND USED.               *
      *****************************************************************
           IF WS-REMI-MM = 13
               MOVE 1 TO WS-REMI-MM.
           
           MOVE " "                         TO PRINT-REC
           MOVE "¶"                         TO H9-1
           MOVE "³"                         TO H9-2.
           MOVE "LESS AMOUNTS ALREADY PAID" TO SA-DESC1-1
           MOVE " "                         TO SA-DESC1-2
           MOVE CR-CURRENCY                 TO SA-DESC2
           MOVE CRREM-PREV-PAID             TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
           
           MOVE " "                                TO PRINT-REC
           MOVE "¶"                                TO H9-1
           MOVE "³"                                TO H9-2.
           MOVE "TOTAL BALANCE BEFORE ADJUSTMENTS" TO SA-DESC
           MOVE CR-CURRENCY                        TO SA-DESC2
           MOVE CRREM-BAL-NOW                      TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
       PREF-0061.
           MOVE " "                         TO PRINT-REC
           MOVE "¶"                         TO H10-1
           MOVE "³"                         TO H10-2.
           MOVE "ADJUSTMENTS FOR THE MONTH" TO H10-DESC1-1
           MOVE WS-MM-DESC (WS-REMI-MM)     TO H10-DESC1-2
           WRITE PRINT-REC FROM SLIPE-MONTH AFTER 1.
       PREF-007.
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.

           MOVE 1 TO SUB-1
                     LINE-CNT.
       PREF-008.
      ************************
      * PRINTING ADJ LINES.  *
      ************************
           IF CRREM-DESC (SUB-1) = " "
               PERFORM PREF-900
               PERFORM PREF-800
               MOVE "N" TO WS-END
               GO TO PREF-010.
           
           MOVE " "                 TO PRINT-REC
           MOVE "¶"                 TO H9-1
           MOVE "³"                 TO H9-2.
           MOVE CRREM-DESC (SUB-1)  TO SA-DESC
           MOVE CRREM-AMT (SUB-1)   TO SA-UNIT
           MOVE SUB-1               TO SA-LINE-NO
           MOVE ")"                 TO SA-LINE-NO-DESC
           MOVE CR-CURRENCY         TO SA-DESC2
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.

           ADD CRREM-AMT (SUB-1) TO WS-TOTAL-ADJ.

           ADD 1 TO LINE-CNT SUB-1.
           IF LINE-CNT > 15
              PERFORM PREF-660
              PERFORM PREF-600
              PERFORM PREF-006
              MOVE 1 TO LINE-CNT
              GO TO PREF-008.
           IF SUB-1 < 41
              GO TO PREF-008.
           IF LINE-CNT < 15
               PERFORM PREF-650.
       PREF-010.
           MOVE " "                         TO PRINT-REC SLIPE-ADJ
           MOVE "¶"                         TO H9-1
           MOVE "³"                         TO H9-2.
           MOVE " "                         TO SA-DESC1-1
           MOVE "TOTAL ADJUSTMENTS     :"   TO SA-DESC1-2
           MOVE CR-CURRENCY                 TO SA-DESC2
           MOVE WS-TOTAL-ADJ                TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
           
           COMPUTE WS-TOTAL-GROSS = CRREM-BAL-NOW + WS-TOTAL-ADJ.

           MOVE " "                         TO PRINT-REC
           MOVE "¶"                         TO H9-1
           MOVE "³"                         TO H9-2.
           MOVE " "                         TO SA-DESC1-1
           MOVE "TOTAL BEFORE DISCOUNT :"   TO SA-DESC1-2
           MOVE CR-CURRENCY                 TO SA-DESC2
           MOVE WS-TOTAL-GROSS              TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.

           MOVE " "                                  TO PRINT-REC
           MOVE "¶"                                  TO H6-1
           MOVE "³"                                  TO H6-2.
           MOVE "    LESS DISCOUNT @"                TO H6-DESC-COM
           MOVE CR-SETT-DISC                         TO H6-DESC-PERC
           MOVE CR-CURRENCY                          TO H6-DESC2
           MOVE CRREM-DISC-AMT                       TO H6-AMT
           WRITE PRINT-REC FROM SLIPE-HEAD6 AFTER 1.
           
           COMPUTE WS-TOTAL-NETT = WS-TOTAL-GROSS - CRREM-DISC-AMT.

           MOVE " "                         TO PRINT-REC
           MOVE "¶"                         TO H9-1
           MOVE "³"                         TO H9-2.
           MOVE "THIS MONTHS PAYMENT   : "  TO SA-DESC1-2
           MOVE CR-CURRENCY                 TO SA-DESC2
           MOVE WS-TOTAL-NETT               TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
       PREF-500.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.

           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           MOVE "AUTHORISED FOR EFT PAYMENT #" TO SLIPE-COMM-LINE
           MOVE CRREM-PMT-REF                  TO SLIPE-COMM-REF
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE 1 TO SUB-1.
           MOVE 6 TO LINE-CNT.
       PREF-550.
           IF WS-COMMENT (SUB-1) = " "
               GO TO PREF-570.
           MOVE " "                TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"                TO H12-1
           MOVE "³"                TO H12-2
           MOVE WS-COMMENT (SUB-1) TO H12-COMMENT
           WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.
           
           ADD 1 TO SUB-1 LINE-CNT.
           IF SUB-1 < 5
               GO TO PREF-550.
      ************************************************************
      * THIS GO TO PREF-00005 IS BECAUSE ALL 4 LINES OF COMMENT   *
      * MUST HAVE BEEN PRINTED SO DO NOT PRINT AN EXTRA LINE IN  *
      * PREF-570.                                                 *
      ************************************************************
      *LINE BELOW TAKEN OUT FOR TEST PURPOSES     
           PERFORM REWRITE-CRREMIT.
           GO TO PREF-00005.
       PREF-570.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 1 TO LINE-CNT.
           IF LINE-CNT < 10
              GO TO PREF-570.
           
           PERFORM REWRITE-CRREMIT.
           GO TO PREF-00005.
       PREF-600.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 1 TO LINE-CNT.
           IF LINE-CNT < 11
              GO TO PREF-600.
       PREF-650.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 1 TO LINE-CNT.
           IF LINE-CNT < 16
              GO TO PREF-650.
       PREF-660.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 1 TO LINE-CNT.
           IF LINE-CNT < 25
              GO TO PREF-660.
       PREF-800.
           PERFORM READ-REMIT-TRANS-NEXT.
      **********************
      * FOREIGN REMIT ONLY *
      **********************
           IF WS-END NOT = "Y"
              MOVE "AMT:   "   TO WS2-FIL
              MOVE "DISC:  "   TO WS3-FIL
              MOVE "NETT:  "   TO WS4-FIL
              
              MOVE CRREMTR-INVNO     TO WS-IP-INV
              MOVE CRREMTR-INV-AMT   TO WS-IP-AMT
              MOVE CRREMTR-DISC-AMT  TO WS-IP-DISC
              COMPUTE WS-NETT-AMT = CRREMTR-INV-AMT - CRREMTR-DISC-AMT
              MOVE WS-NETT-AMT       TO WS-IP-NETT
             
              MOVE " "             TO PRINT-REC WS-OTH-LINE
              MOVE "¶"             TO WS-DELIM-O
              MOVE "³"             TO WS-DELIM-END2
              MOVE WS-INV-PAY-LINE TO WS-DATA-O
              WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1
              ADD 1 TO LINE-CNT.
           IF WS-END NOT = "Y" 
            IF LINE-CNT > 15
              PERFORM PREF-660
              PERFORM PREF-600
              PERFORM PREF-006
              PERFORM PREF-910
              GO TO PREF-800
             ELSE
              GO TO PREF-800.

           PERFORM PREF-650.
       PREF-900.
           IF LINE-CNT > 12
              PERFORM PREF-660
              PERFORM PREF-600
              PERFORM PREF-006
              PERFORM PREF-007
              PERFORM PREF-007
              MOVE 1 TO LINE-CNT.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE " "                         TO PRINT-REC
           MOVE "¶"                         TO H10-1
           MOVE "³"                         TO H10-2.
           MOVE "INVOICES PAID THIS MONTH:" TO H10-DESC1-1
           MOVE WS-MM-DESC (WS-REMI-MM)     TO H10-DESC1-2
           WRITE PRINT-REC FROM SLIPE-MONTH AFTER 1.

           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 3 TO LINE-CNT.
           
           PERFORM START-CRREMIT-TRANS.
       PREF-910.
      *     MOVE " "          TO PRINT-REC SLIPE-HEAD11
      *     MOVE "¶"          TO H11-1
      *     MOVE "³"          TO H11-2
      *     WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE " "                         TO PRINT-REC
           MOVE "¶"                         TO H10-1
           MOVE "³"                         TO H10-2.
           MOVE "INVOICES PAID THIS MONTH:" TO H10-DESC1-1
           MOVE WS-MM-DESC (WS-REMI-MM)     TO H10-DESC1-2
           WRITE PRINT-REC FROM SLIPE-MONTH AFTER 1.

           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE 1 TO LINE-CNT
           MOVE 1 TO SUB-1.
       PREF-950.
      ****************************************************************
      * THIS LAST WRITE IS TO PUT THE LAST RETURN CHAR ON THE VERY   *
      * LAST LINE WHICH OTHERWISE WOULD BE OMMITTED.                 *
      ****************************************************************
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           CLOSE PRINT-FILE.
           CALL "C$SLEEP" USING 1.
       PREF-999.
           EXIT.
      *
       GET-EMAIL-FILE-NAME SECTION.
       GEQN-002.
           MOVE WS-REMIT-PERIOD   TO WS-EREMIT
           MOVE Ws-Co-Number      TO WS-ECOMPANY
           MOVE WS-EMAIL-REMIT    TO WS-PRINTER.
       GEQN-999.
           EXIT.
      *
       REWRITE-CRREMIT SECTION.
       RSR-005.
          MOVE "Y" TO CRREM-COMPLETE.
       RSR-010.
          REWRITE CRREM-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-REMI-ST1 NOT = 0
              MOVE 0 TO WS-REMI-ST1
              MOVE "REMITTANCE RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSR-010.
       RSR-999.
          EXIT.
      *
       PRINT-LOCAL-EMAIL-REMIT SECTION.
       PRE-000.
           OPEN OUTPUT PRINT-FILE.

           PERFORM Z1-HEADINGS.
           
           PERFORM START-CRREMIT.
           MOVE 0 TO STATE-CNT.
       PRE-00005.
           MOVE 0 TO PAGE-CNT
                     WS-TOTAL-ADJ
                     WS-TOTAL-GROSS
                     WS-TOTAL-NETT.
           MOVE 66 TO LINE-CNT.
       PRE-00010.
           MOVE "´¶"         TO WS-DELIM-F
           MOVE "¶"          TO H1-1
           MOVE "³"          TO H1-2.
           
           GO TO PRE-005.
       PRE-00011.
           IF PAGE-CNT = 0
                MOVE CR-ACC-EMAIL TO WS-DATA-F
           ELSE
                MOVE SPACES   TO WS-DATA-F.
           WRITE PRINT-REC FROM WS-FST-LINE AFTER 1.
       PRE-005.
           PERFORM READ-REMIT-NEXT.
           
           IF WS-END = "Y"
               GO TO PRE-950.
      * N=NOT CHECKED, P=CHECKED, PENDING POSTING, Y= COMPLETE - POSTED
           IF CRREM-COMPLETE NOT = "P"
               GO TO PRE-005.
      *******************************************************
      * SEE READ-CREDITOR LATER IN READ-REMIT-NEXT SECTION  *  
      *******************************************************
      *     PERFORM READ-CREDITOR.
           IF CR-NAME = "** UNKNOWN **"
               GO TO PRE-005.
               
           MOVE 2510 TO POS
           DISPLAY "PROCESSING REMITTANCE FOR ACCOUNT:" AT POS
           ADD 35 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS
           ADD 1 TO STATE-CNT
           ADD 10 TO POS
           DISPLAY STATE-CNT AT POS.
           
           PERFORM PRE-00011.

           IF LINE-CNT < 62
               GO TO PRE-010.
       PRE-006.
           MOVE "¶"                      TO H1-1
           MOVE "³"                      TO H1-2
           ADD 1                         TO PAGE-CNT.
           IF PAGE-CNT > 1
               PERFORM PRE-00011.
           WRITE PRINT-REC FROM SLIPE-HEAD1 AFTER 1.
           
           MOVE "¶"                      TO H2-1
           MOVE "³"                      TO H2-2
           WRITE PRINT-REC FROM SLIPE-HEAD2 AFTER 1.

           MOVE " "          TO PRINT-REC
           MOVE "¶"          TO H3-1
           MOVE "³"          TO H3-2
           WRITE PRINT-REC FROM SLIPE-HEAD3 AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE " "             TO PRINT-REC
           MOVE "¶"             TO H4-1
           MOVE "³"             TO H4-2
           MOVE GLPA-NAME       TO SE4-NAME1
           MOVE CR-NAME         TO SE4-NAME2
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "             TO PRINT-REC
           MOVE "¶"             TO H4-1
           MOVE "³"             TO H4-2
           MOVE GLPA-ADD1       TO SE4-NAME1
           MOVE CR-ADDRESS1     TO SE4-NAME2
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "             TO PRINT-REC
           MOVE "¶"             TO H4-1
           MOVE "³"             TO H4-2
           MOVE GLPA-ADD2       TO SE4-NAME1
           MOVE CR-ADDRESS2     TO SE4-NAME2
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "              TO PRINT-REC
           MOVE "¶"              TO H4-1
           MOVE "³"              TO H4-2
           MOVE GLPA-ADD3        TO SE4-NAME1.
           IF CR-ADDRESS3 = " "
               MOVE CR-POST-CODE TO SE4-NAME2
           ELSE
               MOVE CR-ADDRESS3  TO SE4-NAME2.
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "              TO PRINT-REC.
           MOVE "¶"              TO H4-1
           MOVE "³"              TO H4-2
           MOVE " "              TO SE4-NAME1
           IF CR-ADDRESS3 > " "
               MOVE CR-POST-CODE TO SE4-NAME2
           ELSE
               MOVE " "          TO SE4-NAME2.
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE " "                TO PRINT-REC.
           MOVE "¶"                TO H07-1
           MOVE "³"                TO H07-2
           MOVE CRREM-ACC-NUMBER   TO H07-OUR-ACC
           MOVE CR-SUPPLIER-NUMBER TO H07-YOUR-ACC
           WRITE PRINT-REC FROM SLIPE-ACC AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.

           MOVE " "               TO PRINT-REC
           MOVE "¶"               TO H5-1
           MOVE "³"               TO H5-2.
           MOVE GLPA-GLVAT-REG-NO TO SE5-VAT-NUM.
           WRITE PRINT-REC FROM SLIPE-HEAD5 AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE " "                         TO PRINT-REC
                                               SLIPE-ADJ
           MOVE "¶"                         TO H9-1
           MOVE "³"                         TO H9-2.
           MOVE "BALANCE AS PER STATEMENT:" TO SA-DESC1-1
      *****************************************************************
      * IF WS-REMI-MM = 1 IT MEANS WE ARE RUNNING REMITTANCES FOR JAN *
      * THEREFORE 1 - 1 = 0, THE SUBSCRIPT WILL FALL OVER BELOW.      *
      * THE PREVIOUS MONTH FROM JAN AS AN OPENING BALANCE WOULD BE    *
      * DECEMBER SO WE CHANGE THE WS-REMI-MM TO 13 FOR THIS PURPOSE.  *
      *****************************************************************
           SUBTRACT 1 FROM WS-REMI-MM.
           IF WS-REMI-MM = 0
               MOVE 13 TO WS-REMI-MM
           ELSE
               ADD 1   TO WS-REMI-MM.
           MOVE WS-MM-DESC (WS-REMI-MM - 1) TO SA-DESC1-2
           MOVE " R"                        TO SA-DESC2
           MOVE CRREM-BAL-LAST              TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
      *****************************************************************
      * RESET THE WS-REMI-MM BACK TO 1 FOR JAN AS WE CHANGED IT ABOVE *
      * AND WS-REMI-MM = 1 FOR JAN IS USED AGAIN LATER IN THE PROGRAM *
      * THEREFORE IT MUST BE MADE 1 AGAIN ONCE THE PREVIOUS MONTH     *
      * (OPENING BALANCE MONTH NAME) IS FOUND AND USED.               *
      *****************************************************************
           IF WS-REMI-MM = 13
               MOVE 1 TO WS-REMI-MM.
           
           MOVE " "                         TO PRINT-REC
           MOVE "¶"                         TO H9-1
           MOVE "³"                         TO H9-2.
           MOVE "LESS AMOUNTS ALREADY PAID" TO SA-DESC1-1
           MOVE " "                         TO SA-DESC1-2
           MOVE " R"                        TO SA-DESC2
           MOVE CRREM-PREV-PAID             TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
           
           MOVE " "                                TO PRINT-REC
           MOVE "¶"                                TO H9-1
           MOVE "³"                                TO H9-2.
           MOVE "TOTAL BALANCE BEFORE ADJUSTMENTS" TO SA-DESC
           MOVE " R"                               TO SA-DESC2
           MOVE CRREM-BAL-NOW                      TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
       PRE-0061.
           MOVE " "                         TO PRINT-REC
           MOVE "¶"                         TO H10-1
           MOVE "³"                         TO H10-2.
           MOVE "ADJUSTMENTS FOR THE MONTH" TO H10-DESC1-1
           MOVE WS-MM-DESC (WS-REMI-MM)     TO H10-DESC1-2
           WRITE PRINT-REC FROM SLIPE-MONTH AFTER 1.
       PRE-007.
           MOVE " " TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
       PRE-0071.
           MOVE 1 TO SUB-1
                     LINE-CNT.
       PRE-008.
      ************************
      * PRINTING ADJ LINES.  *
      ************************
           IF CRREM-DESC (SUB-1) = " "
               PERFORM PRE-900
               PERFORM PRE-800
               MOVE "N" TO WS-END
               GO TO PRE-010.
           
           MOVE " "                 TO PRINT-REC
           MOVE "¶"                 TO H9-1
           MOVE "³"                 TO H9-2.
           MOVE CRREM-DESC (SUB-1)  TO SA-DESC
           MOVE CRREM-AMT (SUB-1)   TO SA-UNIT
           MOVE SUB-1               TO SA-LINE-NO
           MOVE ")"                 TO SA-LINE-NO-DESC
           MOVE " R"                TO SA-DESC2
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.

           ADD CRREM-AMT (SUB-1) TO WS-TOTAL-ADJ.

           ADD 1 TO LINE-CNT SUB-1.
           IF LINE-CNT > 15
              PERFORM PRE-660
              PERFORM PRE-600
              PERFORM PRE-006
              PERFORM PRE-0061
              PERFORM PRE-007
              MOVE 1 TO LINE-CNT
              GO TO PRE-008.
           IF SUB-1 < 41
              GO TO PRE-008.
           IF LINE-CNT < 15
               PERFORM PRE-650.
       PRE-010.
           MOVE " "                         TO PRINT-REC SLIPE-ADJ
           MOVE "¶"                         TO H9-1
           MOVE "³"                         TO H9-2.
           MOVE " "                         TO SA-DESC1-1
           MOVE "TOTAL ADJUSTMENTS     :"   TO SA-DESC1-2
           MOVE " R"                        TO SA-DESC2
           MOVE WS-TOTAL-ADJ                TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
           
           COMPUTE WS-TOTAL-GROSS = CRREM-BAL-NOW + WS-TOTAL-ADJ.

           MOVE " "                         TO PRINT-REC
           MOVE "¶"                         TO H9-1
           MOVE "³"                         TO H9-2.
           MOVE " "                         TO SA-DESC1-1
           MOVE "TOTAL BEFORE DISCOUNT :"   TO SA-DESC1-2
           MOVE " R"                        TO SA-DESC2
           MOVE WS-TOTAL-GROSS              TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.

           MOVE " "                                  TO PRINT-REC
           MOVE "¶"                                  TO H6-1
           MOVE "³"                                  TO H6-2.
           MOVE "    LESS DISCOUNT @"                TO H6-DESC-COM
           MOVE " R "                                TO H6-DESC2
           MOVE CR-SETT-DISC                         TO H6-DESC-PERC
           MOVE CRREM-DISC-AMT                       TO H6-AMT
           WRITE PRINT-REC FROM SLIPE-HEAD6 AFTER 1.
           
           COMPUTE WS-TOTAL-NETT = WS-TOTAL-GROSS - CRREM-DISC-AMT.

           MOVE " "                         TO PRINT-REC
           MOVE "¶"                         TO H9-1
           MOVE "³"                         TO H9-2.
           MOVE "THIS MONTHS PAYMENT   : "  TO SA-DESC1-2
           MOVE " R"                        TO SA-DESC2
           MOVE WS-TOTAL-NETT               TO SA-UNIT
           WRITE PRINT-REC FROM SLIPE-ADJ AFTER 1.
       PRE-500.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.

           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           MOVE "AUTHORISED FOR EFT PAYMENT #" TO SLIPE-COMM-LINE
           MOVE CRREM-PMT-REF                  TO SLIPE-COMM-REF
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE 1 TO SUB-1.
           MOVE 6 TO LINE-CNT.
       PRE-550.
           IF WS-COMMENT (SUB-1) = " "
               GO TO PRE-570.
           MOVE " "                TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"                TO H12-1
           MOVE "³"                TO H12-2
           MOVE WS-COMMENT (SUB-1) TO H12-COMMENT
           WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.
           
           ADD 1 TO SUB-1 LINE-CNT.
           IF SUB-1 < 5
               GO TO PRE-550.
      ************************************************************
      * THIS GO TO PRE-00005 IS BECAUSE ALL 4 LINES OF COMMENT   *
      * MUST HAVE BEEN PRINTED SO DO NOT PRINT AN EXTRA LINE IN  *
      * PRE-570.                                                 *
      ************************************************************
           PERFORM REWRITE-CRREMIT.
           GO TO PRE-00005.
       PRE-570.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 1 TO LINE-CNT.
           IF LINE-CNT < 10
              GO TO PRE-570.
           
           PERFORM REWRITE-CRREMIT.
           GO TO PRE-00005.
       PRE-600.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 1 TO LINE-CNT.
           IF LINE-CNT < 11
              GO TO PRE-600.
       PRE-650.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 1 TO LINE-CNT.
           IF LINE-CNT < 16
              GO TO PRE-650.
       PRE-660.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 1 TO LINE-CNT.
           IF LINE-CNT < 25
              GO TO PRE-660.
       PRE-800.
           PERFORM READ-REMIT-TRANS-NEXT.
      **********************
      * LOCAL REMIT ONLY   *
      **********************
           IF WS-END NOT = "Y"
              MOVE CRREMTR-INVNO     TO WS-IP-INV
              MOVE CRREMTR-INV-AMT   TO WS-IP-AMT
              MOVE CRREMTR-DISC-AMT  TO WS-IP-DISC
              COMPUTE WS-NETT-AMT = CRREMTR-INV-AMT - CRREMTR-DISC-AMT
              MOVE WS-NETT-AMT       TO WS-IP-NETT
             
              MOVE " "             TO PRINT-REC WS-OTH-LINE
              MOVE "¶"             TO WS-DELIM-O
              MOVE "³"             TO WS-DELIM-END2
              MOVE WS-INV-PAY-LINE TO WS-DATA-O
              WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1
              ADD 1 TO LINE-CNT.
           IF WS-END NOT = "Y" 
            IF LINE-CNT > 15
              PERFORM PRE-660
              PERFORM PRE-600
              PERFORM PRE-006
              PERFORM PRE-910
              GO TO PRE-800
             ELSE
              GO TO PRE-800.

           PERFORM PRE-650.
       PRE-900.
           IF LINE-CNT > 12
              PERFORM PRE-660
              PERFORM PRE-600
              PERFORM PRE-006
              PERFORM PRE-007
              PERFORM PRE-007
              MOVE 1 TO LINE-CNT.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE " "                         TO PRINT-REC
           MOVE "¶"                         TO H10-1
           MOVE "³"                         TO H10-2.
           MOVE "INVOICES PAID THIS MONTH:" TO H10-DESC1-1
           MOVE WS-MM-DESC (WS-REMI-MM)     TO H10-DESC1-2
           WRITE PRINT-REC FROM SLIPE-MONTH AFTER 1.

           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           ADD 3 TO LINE-CNT.
           
           PERFORM START-CRREMIT-TRANS.
       PRE-910.
      *     MOVE " "          TO PRINT-REC SLIPE-HEAD11
      *     MOVE "¶"          TO H11-1
      *     MOVE "³"          TO H11-2
      *     WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE " "                         TO PRINT-REC
           MOVE "¶"                         TO H10-1
           MOVE "³"                         TO H10-2.
           MOVE "INVOICES PAID THIS MONTH:" TO H10-DESC1-1
           MOVE WS-MM-DESC (WS-REMI-MM)     TO H10-DESC1-2
           WRITE PRINT-REC FROM SLIPE-MONTH AFTER 1.

           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 1.
           
           MOVE 1 TO LINE-CNT
           MOVE 1 TO SUB-1.
       PRE-945.
           MOVE " "          TO PRINT-REC SLIPE-HEAD11
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-HEAD11 AFTER 2.
       PRE-950.
      ****************************************************************
      * THIS LAST WRITE IS TO PUT THE LAST RETURN CHAR ON THE VERY   *
      * LAST LINE WHICH OTHERWISE WOULD BE OMMITTED.                 *
      ****************************************************************
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           CLOSE PRINT-FILE.
       PRE-999.
           EXIT.
      *
       READ-REMIT SECTION.
       R-GL-000.
             MOVE "Y"              TO NEW-CRREMNO.
             
             MOVE WS-REMI-YY       TO CRREM-YY
             MOVE WS-REMI-MM       TO CRREM-MM
             MOVE WS-REMIT-F-L     TO CRREM-F-L
             MOVE CRREM-ACC-NUMBER TO WS-NUMBER.
             START CRREMIT-FILE KEY NOT < CRREM-KEY
                INVALID KEY NEXT SENTENCE.
             IF WS-REMI-ST1 NOT = 0
                GO TO R-GL-999.
       R-GL-010.
             READ CRREMIT-FILE
                 INVALID KEY NEXT SENTENCE.
             IF WS-REMI-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-CRREMNO
                MOVE WS-NUMBER TO CRREM-ACC-NUMBER
                GO TO R-GL-999.
             IF WS-REMI-ST1 NOT = 0
                MOVE "CRREM RECORD BUSY ON READ, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-REMI-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO R-GL-010.
                
             IF CRREM-F-L NOT = WS-REMIT-F-L
                PERFORM CLEAR-FORM
                MOVE "Y"       TO NEW-CRREMNO
                MOVE WS-NUMBER TO CRREM-ACC-NUMBER
                GO TO R-GL-999.

           MOVE "N" TO NEW-CRREMNO.
           MOVE CRREM-DISC-AMT TO WS-DISC-AMT.
       R-GL-999.
           EXIT.
      *
       START-CRREMIT SECTION.
       GL-GL-000.
           MOVE WS-REMI-YY   TO CRREM-YY
           MOVE WS-REMI-MM   TO CRREM-MM
           MOVE WS-REMIT-F-L TO CRREM-F-L
           MOVE WS-NUMBER    TO CRREM-ACC-NUMBER.
           START CRREMIT-FILE KEY NOT LESS CRREM-KEY
               INVALID KEY NEXT SENTENCE.
       GL-GL-999.
           EXIT.
      *
       READ-REMIT-NEXT SECTION.
       RSN-005. 
           READ CRREMIT-FILE NEXT
             AT END 
               MOVE 0 TO CRREM-ACC-NUMBER
                            WS-NUMBER
               MOVE "Y" TO WS-END
               PERFORM CLEAR-FORM
               GO TO RSN-999.
           IF WS-REMI-ST1 = 23 OR 35 OR 49
               MOVE "CRREMIT-FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-005.
           IF WS-REMI-ST1 NOT = 0
              MOVE "CR-REMI BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-REMI-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              PERFORM START-CRREMIT
              GO TO RSN-005.
               
           IF CRREM-YY < WS-REMI-YY
               GO TO RSN-005.
           IF CRREM-MM < WS-REMI-MM
               GO TO RSN-005.
           IF CRREM-YY NOT = WS-REMI-YY
               MOVE "Y" TO WS-END
               PERFORM CLEAR-FORM
               GO TO RSN-999.
           IF CRREM-MM NOT = WS-REMI-MM
               MOVE "Y" TO WS-END
               PERFORM CLEAR-FORM
               GO TO RSN-999.

           IF CRREM-F-L NOT = WS-REMIT-F-L
                GO TO RSN-005.
               
           PERFORM READ-CREDITOR.
               
           IF WS-PRINT-NO = "N"
            IF CR-ACC-EMAIL = " "
                GO TO RSN-005.
               
            MOVE "N"              TO NEW-CRREMNO.
            MOVE CRREM-ACC-NUMBER TO WS-NUMBER.
            MOVE CRREM-DISC-AMT   TO WS-DISC-AMT.
       RSN-999.
            EXIT.
      *
       START-CRREMIT-TRANS SECTION.
       ST-CR-TR-000.
           MOVE WS-REMI-YY        TO CRREMTR-YY
           MOVE WS-REMI-MM        TO CRREMTR-MM
           MOVE WS-NUMBER         TO CRREMTR-ACC-NUMBER
           MOVE SPACES            TO CRREMTR-INVNO.
           START CRREMIT-TRANS-FILE KEY NOT LESS CRREMTR-KEY
               INVALID KEY NEXT SENTENCE.
       ST-CR-TR-999.
             EXIT.
      *
       READ-REMIT-TRANS-NEXT SECTION.
       RRT-005. 
           READ CRREMIT-TRANS-FILE NEXT
             AT END 
               MOVE "Y" TO WS-END
               GO TO RRT-999.
           IF WS-REMI-ST1 NOT = 0
               MOVE "CRREMIT-TRANS BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RRT-005.
               
           IF CRREM-YY NOT = WS-REMI-YY
               MOVE "Y" TO WS-END
               GO TO RRT-999.
           IF CRREM-MM NOT = WS-REMI-MM
               MOVE "Y" TO WS-END
               GO TO RRT-999.
           IF CRREMTR-ACC-NUMBER NOT = CR-ACCOUNT-NUMBER
               MOVE "Y" TO WS-END
               GO TO RRT-999.
           IF CRREMTR-COMPLETE = "Y"
               GO TO RRT-005.
      * LINE BELOW TAKEN OUT FOR TEST PURPOSES         
           PERFORM REWRITE-CRREMIT-TRANS.
       RRT-999.
            EXIT.
      *
       REWRITE-CRREMIT-TRANS SECTION.
       RCRT-005.
          MOVE "Y" TO CRREMTR-COMPLETE.
       RCRT-010.
          REWRITE CRREMTR-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-REMITTRANS-ST1 NOT = 0
              MOVE 0 TO WS-REMI-ST1
              MOVE "CRRETRM RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RCRT-010.
       RCRT-999.
          EXIT.
      *
       READ-CREDITOR SECTION.
       RCR-000.
           MOVE CRREM-ACC-NUMBER TO CR-ACCOUNT-NUMBER
           START CREDITOR-MASTER KEY NOT < CR-KEY
               INVALID KEY NEXT SENTENCE.
       RCR-010.
           READ CREDITOR-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "** UNKNOWN **" TO CR-NAME
                GO TO RCR-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITOR BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCR-010.
            MOVE CR-SETT-DISC TO WS-DISC-PERC.
       RCR-999.
             EXIT.
      *
       READ-PARAMETER-RECORD SECTION.
       RPR-000.
           MOVE 1 TO GLPA-KEY.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RPR-010.
           READ GLPARAMETER-FILE
                 INVALID KEY NEXT SENTENCE.
           IF WS-PAR-ST1 = 23 OR 35 OR 49
                MOVE "GLPARAMETER RECORD NOT THERE, 'ESC' TO EXIT"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RPR-999.
           IF WS-PAR-ST1 NOT = 0
                MOVE 0 TO WS-PAR-ST1
                MOVE "GLPARAMETER RECORD BUSY, 'ESC' TO RETRY"
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RPR-010.
       RPR-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
            MOVE 1 TO SUB-1.
       CLSC-005.
            MOVE 0   TO CRREM-AMT (SUB-1)
            MOVE " " TO CRREM-DESC (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 41
                 GO TO CLSC-005.
            MOVE 1 TO SUB-1.
       CLSC-010.
            MOVE 0 TO CRREM-BAL-LAST
                      CRREM-PREV-PAID
                      CRREM-BAL-NOW.
       CLSC-999.
           EXIT.
      *-----------------------------------------------------------*
       Z1-HEADINGS SECTION.
      *-----------------------------------------------------------*
       Z1-50.
            MOVE ALL SPACES TO WS-FST-LINE WS-OTH-LINE-1.
            MOVE "´¶" TO WS-DELIM-F.
            MOVE "¶"  TO WS-DELIM-O
            MOVE "³"  TO WS-DELIM-END1
                         WS-DELIM-END2.
            
            MOVE 1             TO SUB-1
            MOVE SUB-1         TO WS-O-LINE
            MOVE "SuppLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-F
            WRITE PRINT-REC FROM WS-FST-LINE AFTER 0.
       Z1-51.
            ADD 1              TO SUB-1
            IF SUB-1 > 14
               MOVE 0 TO SUB-1
               GO TO Z1-52.
               
            MOVE SUB-1         TO WS-O-LINE
            MOVE "SuppLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-51.
       Z1-52.
            ADD 1              TO SUB-1
            IF SUB-1 > 07
               MOVE 0 TO SUB-1
               GO TO Z1-54.
               
            MOVE SUB-1         TO WS-O-LINE
            MOVE "BalaLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-52.
       Z1-54.
            ADD 1              TO SUB-1
            IF SUB-1 > 15
               MOVE 0 TO SUB-1
               GO TO Z1-60.
            MOVE SUB-1         TO WS-O-LINE
            MOVE "BodyLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-54.
       Z1-60.
            ADD 1              TO SUB-1
            IF SUB-1 > 10
               MOVE 0 TO SUB-1
               GO TO Z1-100.
            MOVE SUB-1         TO WS-O-LINE
            MOVE "PaymLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-60.
       Z1-100.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O CRREMIT-FILE.
           IF WS-REMI-ST1 NOT = 0
               MOVE "CR-REMITTANCE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRREMIT TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO OPEN-000.
        OPEN-001.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-PAR-ST1 NOT = 0
               MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-001.
            PERFORM READ-PARAMETER-RECORD.
            IF WS-PAR-ST1 = 23 OR 35 OR 49
               CLOSE GLPARAMETER-FILE
               PERFORM END-OFF.
            CLOSE GLPARAMETER-FILE.
       OPEN-003.
           OPEN I-O CRREMIT-TRANS-FILE.
           IF WS-REMI-ST1 NOT = 0
               MOVE "CRREMIT-TRANS-FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-CRREMITTRANS TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-003.
       OPEN-004.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-004.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *      ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO SOE1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrRemiMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           MOVE 2910 TO POS.
           DISPLAY "REMITTANCE RUN FINISHED, PRESS 'Return' TO EXIT."
            AT POS.
           ADD 50 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 60        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.

           CLOSE CRREMIT-FILE
                 CRREMIT-TRANS-FILE
                 CREDITOR-MASTER.
           EXIT PROGRAM.
      *      STOP RUN.
       END-999.
          EXIT.
      *
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "PrintReportInfo".
       Copy "SetupRemitForPDF".
       Copy "SetupRemit2ForPDF".
       Copy "SetupMergeRemitForPDF".
       Copy "SetupRemitForPDFMgEmail".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
