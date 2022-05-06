        IDENTIFICATION DIVISION.
        PROGRAM-ID. StOrPrRp.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        REPOSITORY. 
           FUNCTION ALL INTRINSIC.
        SPECIAL-NAMES.
           CRT STATUS IS W-CRTSTATUS.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
    
        SPECIAL-NAMES.
        CLASS WS-VALID-EMAIL IS
          '@' '_' '.' '-' '#'
          'a' THRU 'i'
          'j' THRU 'r'
          's' THRU 'z'
          '0' THRU '9'.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
         Copy "SelectSlParameter".
         Copy "SelectStOrders".
         Copy "SelectCrMaster".
         Copy "SelectSlDaily".
         Copy "SelectCoFaxParam".
           SELECT PRINT-SLIP ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SPL-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdDaily.
           COPY ChlfdOutOrd.
           COPY ChlfdParam.
           COPY ChlfdCreditor.
           COPY ChlfdFaxParam.
      *
       FD  PRINT-SLIP.
       01  PRINT-REC.
           03  FILLER           PIC X(90).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-LINE-NO           PIC 9(3) VALUE 0.
       77  PAGE-CNT             PIC 9(2) VALUE 0.
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-DOTPRINTER        PIC X(100) VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-STILL-ON-ORDER    PIC X VALUE "Y".
       77  WS-SUPPLIER          PIC 9(7) VALUE 0.
       77  WS-SUPPLIER-ACCEPT   PIC X(7) VALUE " ".
       77  WS-SUPPLIER-AMOUNT   PIC 9(7)V99 VALUE 0.
       77  WS-VAT-AMT           PIC 9(7)V99 VALUE 0.
       77  WS-COPIES            PIC 9 VALUE 0.
       77  WS-SLIP-COPIES       PIC 9 VALUE 0.
       77  WS-COPIES-ACCEPT     PIC X VALUE " ".
       77  WS-ORDER-NUMBER      PIC X(20) VALUE " ".
       77  WS-FAX-Y-N           PIC X VALUE " ".
       77  WS-FAX-NUMBER        PIC X(20) VALUE " ".
       77  WS-EMAIL-ADDR        PIC X(40) VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-DELVIA            PIC X(20) VALUE " ".
       77  WS-BODY-LINE         PIC ZZ9.
       77  WS-DEL-SUB           PIC 9 VALUE 0.
       77  WS-CONTACT           PIC X(40) VALUE " ".
       77  WS-COMM-MESSAGE      PIC X(60) VALUE " ".
       77  WS-PRINTER-PAGE1     PIC X(100) VALUE " ".
       77  WS-PRINTER-PAGE2     PIC X(100) VALUE " ".
       77  WS-SUBJECT-FIXED     PIC X(100) VALUE " ".
       77  WS-PRINTER-PDF       PIC X(100) VALUE " ".
       77  WS-PRINTING-TYPE     PIC X VALUE " ".
       77  WS-ACC-ERROR         PIC X VALUE " ".      
       01  WS-EMAIL             PIC X(50).
       01  WS-PAGE-CHANGED      PIC X VALUE "N".
       01  WS-TEMP-EMAIL-FILE   PIC X(50).
       01  WS-SPACE-CNT         PIC 9(2) VALUE ZEROES.
       01  W-CRTSTATUS          PIC 9(4) value 0.
       01  WS-COMMENT-LINE.
           03  WS-COMMENT       PIC X(60) OCCURS 5.
       01  WS-ORDER-CHECK.
           03  WS-O-C           PIC X OCCURS 25.
       01  WS-SP-PRINT.
           03  WS-1ST-11CHAR    PIC X(11).
           03  WS-REST          PIC X(20).
       01  STORE-DEL.
           03  WS-DEL-OCCUR OCCURS 10.
               05  WS-DEL-TYPE      PIC X.
               05  WS-DEL-CODE      PIC X.
               05  WS-DEL-TERM      PIC X(20).
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1         PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1         PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1      PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1   PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1        PIC 99.
       01  WS-SPL-STATUS.
           03  WS-SPL-ST1           PIC 99.
       01  WS-Fax-STATUS.
           03  WS-Fax-ST1           PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST         PIC X(20) VALUE " ".
           03  WS-DAILY-2ND         PIC X(20) VALUE " ".
           03  WS-DAILY-3RD         PIC X(20) VALUE " ".
           03  WS-DAILY-4TH         PIC X(20) VALUE " ".
       01  WS-SUBJECT.
           03  WS-SUBJECT-LINE1        PIC X(15) VALUE " ".
           03  WS-SUBJECT-LINE2        PIC X(18).
           03  WS-SUBJECT-LINE3        PIC X(7) VALUE " ".
           03  WS-SUBJECT-LINE4        PIC X(40) VALUE " ".
       01  BODY-FIELDS.
           03  BODY-LINE.
               05  B-STOCKNUMBER      PIC X(15).
               05  B-DESCRIPTION      PIC X(20).
               05  B-DESCRIPTION2     PIC X(20).
               05  B-QUANTITY         PIC S9(4) LEADING SEPARATE.
               05  B-UNITPRICE        PIC 9(6)V999.
               05  B-TOTALPRICE       PIC 9(7)V999.
               05  B-REFNO            PIC X(20).
               05  B-DATE             PIC 9(8) BLANK WHEN ZERO.
       01  WS-NAME-LENGTH.
           03  WS-NL            PIC X OCCURS 40.
       01  WS-XQS-FAX.
           03  WS-XQS-BEG       PIC X(9) VALUE "(:(FaxTo=".
           03  WS-XQS-END       PIC X(3) VALUE "):)".
           03  WS-XQS-COVERS.
               05  WS-XQS1      PIC X(6) VALUE "Cover=".
               05  WS-XQS2      PIC XX VALUE " ".
               05  WS-XQS3      PIC X(12) VALUE "Quote\Other=".
               05  WS-XQS4      PIC XX VALUE " ".
               05  WS-XQS5      PIC X(6) VALUE "Quote2".
           03  WS-XQS-FROM.
               05  WS-XQS-FROMID    PIC X(6) VALUE "\From=".
               05  WS-XQS-FROM-NAME PIC X(25) VALUE " ".
           03  WS-XQS-COMMENT-LINE.
               05 WS-XQS-COMM-DESC PIC X(34) VALUE
                "(:(Comment=OUR ORDER REFERENCE #: ".
               05 WS-XQS-COMMENT   PIC X(17) VALUE " ".
           03  WS-XQS-PRIORITY     PIC X(26) VALUE
               "Pri=B\OurRef=***ORDER***\".
           03  WS-XQS-USERNAME.
               05  WS-XQS-SENT   PIC X(6) VALUE "NSent=".
               05  WS-XQS-SNAME  PIC X(25) VALUE " ".
               05  WS-XQS-ERROR  PIC X(8) VALUE "\NError=".
               05  WS-XQS-ENAME  PIC X(25) VALUE " ".
       01  WS-XQS-FAX-LINES-NAMES.
         02  WS-XQS-FAX-LINES OCCURS 5.
           03  WS-XQS-LINE     PIC X(100) VALUE " ".
       01  WS-HYLA-TO-LINE.
           03  FILLER             PIC X(15) VALUE " ".
           03  WS-HYLA-TO-NAME    PIC X(40) VALUE " ".
       01 WS-HYLA-FROM-LINE.
           03  FILLER             PIC X(15) VALUE " ".
           03  WS-HYLA-FROM-NAME  PIC X(25) VALUE " ".
           03  FILLER             PIC X(28) VALUE " ".
           03  WS-HYLA-PAGE       PIC Z9 VALUE " ".
       01  WS-HYLA-TYPE-LINE.
           03  FILLER             PIC X(15) VALUE " ".
           03  WS-HYLA-TYPE       PIC X(30) VALUE "***ORDER***".
           03  WS-HYLA-DATE       PIC X(30) VALUE " ".
       01  WS-HYLA-COMMENT-LINE.
           03  FILLER             PIC X(15) VALUE " ".
           03 WS-HYLA-COMM-DESC   PIC X(23) VALUE
              "OUR ORDER REFERENCE #: ".
           03 WS-HYLA-COMMENT     PIC X(17) VALUE " ".
       01  WS-HYLA-TYPE-LINE2.
           03  FILLER             PIC X(15) VALUE " ".
           03  WS-HYLA-TYPE2      PIC X(30) VALUE "***ORDER***".
       01 WS-HYLA-FROM-LINE2.
           03  FILLER             PIC X(7) VALUE " ".
           03  WS-HYLA-PAGE2      PIC Z9 VALUE " ".
       01  WS-FILE-NAME-FOR-FAX.
           03  WS-FOLDER-NAME         PIC X(12) VALUE "/ctools/fax/".
           03  WS-QUOTE-REFERENCE     PIC X(20) VALUE " ".
       01  SLIP-HEAD1.
           03  FILLER          PIC X(5) VALUE "DATE".
           03  SO1-DATE        PIC X(10).
           03  FILLER          PIC X(5) VALUE " ".
           03  FILLER          PIC X(49) VALUE
            "** P U R C H A S E   O R D E R **".
           03  FILLER          PIC X(5) VALUE "PAGE:".
           03  SO1-PAGE        PIC Z9.
           03  FILLER          PIC X(3) VALUE " ".
       01  SLIP-HEAD2.
           03  FILLER          PIC X(20) VALUE " ".
           03  FILLER          PIC X(33) VALUE ALL "*".
           03  FILLER          PIC X(26) VALUE " ".
       01  SLIP-HEAD3.
           03  FILLER          PIC X(40) VALUE "SUPPLY TO :".
           03  FILLER          PIC X(39) VALUE "SUPPLIERS NAME :".
       01  SLIP-HEAD4.
           03  S4-NAME1        PIC X(40) VALUE " ".
           03  S4-NAME2.
               05  S4-FAX      PIC X(12) VALUE " ".
               05  S4-NO       PIC X(27) VALUE " ".
       01  SLIP-HEAD6.
           03  FILLER          PIC X(20) VALUE "PURCHASE ORDER No:".
           03  S6-ORDER        PIC X(20).
           03  FILLER          PIC X(13) VALUE "DELIVER VIA:".
           03  S6-DEL          PIC X(26).
       01  SLIP-HEAD6-1.
           03  FILLER          PIC X(40) VALUE 
           "**                 VAT REGISTRATION No:".
           03  S6-VAT-NO       PIC X(20).
           03  FILLER          PIC X(16) VALUE " ".
           03  FILLER          PIC X(2) VALUE "**".
       01  SLIP-HEAD6-2.
           03  FILLER          PIC X(26) VALUE
           "** THIS ORDER CONSISTS OF".
           03  S6-ITEMS        PIC Z(2)9.
           03  FILLER          PIC X VALUE " ".
           03  FILLER          PIC X(49) VALUE
           "ITEMS, PLEASE ENSURE ALL LINES ARE PRINTED.   **".
       01  SLIP-HEAD7.
           03  FILLER          PIC X(78) VALUE ALL "*".
       01  SLIP-HEAD8.
           03  FILLER          PIC X(4) VALUE " No ".
           03  FILLER          PIC X(15) VALUE "STOCK NUMBER".
           03  FILLER          PIC X(42) VALUE "DESCRIPTION".
           03  FILLER          PIC X(5) VALUE "QTY".
           03  FILLER          PIC X(15) VALUE "PER EACH  DISC".
       01  SLIP-DETAIL.
           03  S-ITEM          PIC Z(2)9.
           03  FILLER          PIC X(1) VALUE " ".
           03  S-STOCKNO       PIC X(15).
           03  S-DESC1         PIC X(20).
           03  S-DESC2         PIC X(20).
           03  S-QTY           PIC Z(4)9.
           03  S-UNIT          PIC Z(5)9.999.
           03  FILLER          PIC X(1) VALUE " ".
           03  S-DISC          PIC Z9.99.
       01  SLIP-TOTAL.
           03  FILLER          PIC X(42) VALUE " ".
           03  SLIP-TOT-COM    PIC X(19) VALUE " ".
           03  TOT-GRV         PIC Z,ZZZ,ZZ9.99.
           03  FILLER          PIC X(1) VALUE " ".
       01  SLIP-COMMENT.
           03  SLIP-COMM-LINE.
               05  SLIP-FILL   PIC X(19) VALUE " ".
               05  SLIP-COMM   PIC X(61) VALUE " ".
       01  SLIP-JOB-LINE.
           03  JOB-FILLER      PIC X(10) VALUE " ".
           03  JOB-NUM         PIC X(4) VALUE " ".
           03  FILLER          PIC X(65) VALUE " ".
       01  WS-EMAIL-PORDER.
         02  WS-EMAIL-SETUP-FILE.
           03  WS-EP-FIL              PIC X(15) VALUE "/ctools/eimage/".
           03  WS-EPORDER             PIC X(7).
         02  WS-EMAIL-TEMP-ORDER-FILE PIC X(50) VALUE " ".
       01  WS-EMAIL-FINAL.
           03  WS-EF-FIL        PIC X(15) VALUE " ".
           03  WS-BAL-OF-NAME   PIC X(35).
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
       01  SLIPE-HEAD1.
           03  H1-1            PIC XX.
           03  FILLER          PIC X(5) VALUE "DATE".
           03  SOE1-DATE       PIC X(10).
           03  FILLER          PIC X(5) VALUE " ".
           03  FILLER          PIC X(49) VALUE
            "** P U R C H A S E   O R D E R **".
           03  FILLER          PIC X(5) VALUE "PAGE:".
           03  SOE1-PAGE       PIC Z9.
           03  FILLER          PIC X(10) VALUE " ".
           03  H1-2            PIC X.
       01  SLIPE-HEAD2.
           03  H2-1            PIC XX.
           03  FILLER          PIC X(20) VALUE " ".
           03  FILLER          PIC X(33) VALUE ALL "*".
           03  FILLER          PIC X(33) VALUE " ".
           03  H2-2            PIC X.
       01  SLIPE-HEAD3.
           03  H3-1            PIC XX.
           03  FILLER          PIC X(40) VALUE "SUPPLY TO :".
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
           03  FILLER          PIC X(40) VALUE " ".
           03  FILLER          PIC X(16) VALUE "Account Number:".
           03  H5-ACC          PIC X(30) VALUE " ".
           03  H5-2            PIC X.
       01  SLIPE-HEAD6.
           03  H6-1            PIC XX.
           03  FILLER          PIC X(20) VALUE "PURCHASE ORDER No:".
           03  SE6-ORDER       PIC X(20).
           03  FILLER          PIC X(13) VALUE "DELIVER VIA:".
           03  SE6-DEL         PIC X(33).
           03  H6-2            PIC X.
       01  SLIPE-HEAD6-1.
           03  H61-1           PIC XX.
           03  FILLER          PIC X(40) VALUE 
           "**                 VAT REGISTRATION No:".
           03  SE6-VAT-NO      PIC X(20).
           03  FILLER          PIC X(16) VALUE " ".
           03  FILLER          PIC X(2) VALUE "**".
           03  FILLER          PIC X(8) VALUE " ".
           03  H61-2           PIC X.
       01  SLIPE-HEAD6-2.
           03  H62-1           PIC XX.
           03  FILLER          PIC X(26) VALUE
           "** THIS ORDER CONSISTS OF".
           03  SE6-ITEMS       PIC Z(2)9.
           03  FILLER          PIC X VALUE " ".
           03  FILLER          PIC X(56) VALUE
           "ITEMS, PLEASE ENSURE ALL LINES ARE PRINTED.   **".
           03  H62-2           PIC X.
       01  SLIPE-HEAD7.
           03  H7-1            PIC XX.
           03  FILLER          PIC X(78) VALUE ALL "*".
           03  FILLER          PIC X(8) VALUE " ".
           03  H7-2            PIC X.
       01  SLIPE-HEAD8.
           03  H8-1            PIC XX.
           03  FILLER          PIC X(19) VALUE "LN# STOCK NUMBER".
           03  FILLER          PIC X(42) VALUE "DESCRIPTION".
           03  FILLER          PIC X(5) VALUE "QTY".
           03  FILLER          PIC X(20) VALUE "PER EACH  DISC".
           03  H8-2            PIC X.
       01  SLIPE-DETAIL.
           03  H9-1            PIC XX.
           03  SE-ITEM         PIC Z99.
           03  FILLER          PIC X.
           03  SE-STOCKNO      PIC X(15).
           03  SE-DESC1        PIC X(20).
           03  SE-DESC2        PIC X(20).
           03  SE-QTY          PIC Z(4)9.
           03  SE-UNIT         PIC Z(5)9.999.
           03  FILLER          PIC X(1) VALUE " ".
           03  SE-DISC         PIC Z9.99.
           03  FILLER          PIC X(6) VALUE " ".
           03  H9-2            PIC X.
       01  SLIPE-TOTAL.
           03  H10-1           PIC XX.
           03  FILLER          PIC X(46) VALUE " ".
           03  SLIPE-TOT-COM   PIC X(17) VALUE " ".
           03  TOTE-GRV        PIC Z(6)9.99.
           03  FILLER          PIC X(13) VALUE " ".
           03  H10-2           PIC X.
       01  SLIPE-COMMENT.
           03  H11-1           PIC XX.
           03  FILLER          PIC X(6) VALUE " ".
           03  SLIPE-COMM-LINE PIC X(80) VALUE " ".
           03  H11-2           PIC X.
       01  SLIPE-JOB-LINE.
           03  H12-1           PIC XX.
           03  JOBE-FILLER     PIC X(10) VALUE " ".
           03  JOBE-NUM        PIC X(4) VALUE " ".
           03  FILLER          PIC X(72) VALUE " ".
           03  H12-2           PIC X.
       Copy "WsDateInfo".
       Copy "WStore".
       Copy "FaxInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-001.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "** STOCK ORDER REPRINT & FAX PROGRAM **" AT POS
           MOVE 415 TO POS
           DISPLAY "***************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           IF WS-ABOVE-BODY NOT = "2"
               MOVE " " TO FAX-FONTFILENAME
               PERFORM OPEN-FILES.
       CONTROL-015.
           PERFORM UPDATE-OUT-ORDERS.
           IF WS-ABOVE-BODY = "2"
               GO TO CONTROL-005.
           PERFORM OPEN-020.
           MOVE 0 TO FAX-JOBNUMBER.
       CONTROL-016.
      * SEND BY EMAIL - OLD VERSION
      *     IF WS-FAX-Y-N = "E"
      *         GO TO CONTROL-020.
      * XQS FAX ONLY
           IF Fax-PaNumber = 3
            IF WS-FAX-Y-N = "F"
               MOVE "[QFax]" TO WS-PRINTER
               GO TO CONTROL-017
            ELSE
               GO TO CONTROL-040.
               
      * ALL BELOW NOW FOR FAX-PANUMBER = 4
      * NO FAX BUT A PDF PRINT OR MAILGUN EMAIL
           IF WS-FAX-Y-N = "N"
            IF WS-PRINTING-TYPE = "P"
               GO TO CONTROL-017.
      * FAX BUT NO PDF PRINT
           IF WS-FAX-Y-N = "F" OR = "E"
            IF WS-PRINTING-TYPE = "N"
               GO TO CONTROL-017.

      * NO FAX NO PDF PRINT - DOT MATRIX PRINT ONLY
           IF WS-FAX-Y-N = "N"
            IF WS-PRINTING-TYPE = "N"
               GO TO CONTROL-040.
           IF Fax-PaNumber = 4
      *         MOVE "HYLAFAX" TO WS-PRINTER
               GO TO CONTROL-017.
           MOVE "S"            TO WS-FAX-Y-N.
           MOVE "/ctools/fx/"  TO WS-1ST-11CHAR.
           MOVE "/ctools/fax/" TO WS-FAX-12CHAR.
           PERFORM CHECK-ORDER.
           PERFORM ERROR-020.
       CONTROL-017.
      *     MOVE 2910 TO POS.
      *     DISPLAY "PRINTING OF FAX FILE TO:                   " AT POS
      *     ADD 24 TO POS
      *     DISPLAY WS-PRINTER AT POS.
           MOVE WS-ORDER-NUMBER TO OO-ORDER-NUMBER.
           MOVE " "             TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
           PERFORM ERROR-020.
           IF Fax-PaNumber = 3 OR = 4
            IF WS-FAX-Y-N = "F"
               OR WS-PRINTING-TYPE = "P"
               PERFORM PRINT-XQSORDER-SLIP
               PERFORM ERROR-020
               PERFORM ERROR1-020
               GO TO CONTROL-018.
           IF Fax-PaNumber = 3 OR = 4
            IF WS-FAX-Y-N = "E"
               PERFORM PRINT-XQSORDER-SLIP
               PERFORM ERROR-020
               PERFORM ERROR1-020
               GO TO CONTROL-018.
           IF Fax-PaNumber = 3 OR = 4
             IF WS-PRINTING-TYPE = "P"
               PERFORM PRINT-XQSORDER-SLIP
               PERFORM ERROR-020
               PERFORM ERROR1-020
               GO TO CONTROL-018.
              
           IF Fax-PaNumber = 3 OR = 4
            IF WS-PRINTING-TYPE = "N"
             IF WS-FAX-Y-N = "N" 
              PERFORM PRINT-ORDER-SLIP.
           PERFORM ERROR-020.
           PERFORM ERROR1-020.
           IF FAX-PANUMBER = 3 OR = 4
               GO TO CONTROL-018.
           MOVE 2910 TO POS.
           DISPLAY "PLACING PURCHASE ORDER IN FAX QUEUE....." AT POS.
           PERFORM PREPARE-FAX-SENDING.
       CONTROL-018.
           CLOSE OUTSTANDING-ORDERS.
           GO TO CONTROL-040.
       CONTROL-020.
           PERFORM GET-EMAIL-PORDR-NAME.
           MOVE Spaces              TO WS-PRINTER.
      *     MOVE Ws-Email-POrder     To Ws-Printer.
           MOVE WS-EMAIL-TEMP-ORDER-FILE TO WS-PRINTER.

      *     MOVE WS-PRINTER TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

           MOVE 2910 TO POS.
           DISPLAY "PRINTING OF EMAIL FILE :                   " AT POS.
           ADD 24 TO POS
           DISPLAY WS-PRINTER AT POS.
           MOVE WS-ORDER-NUMBER TO OO-ORDER-NUMBER.
           MOVE " "             TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
           PERFORM ERROR-020.
           
           PERFORM PRINT-EMAIL-SLIP.
           PERFORM ERROR-020.
           PERFORM ERROR1-020.
        CONTROL-021.
           CLOSE OUTSTANDING-ORDERS.
           MOVE "Y" TO WS-FAX-Y-N.
        CONTROL-040.
           MOVE " " TO WS-PRINTER.
           MOVE WS-PRINTERNAME (21) TO WS-PRINTER.
        CONTROL-041.
           IF WS-LINK-PORDER > " " 
               PERFORM END-OFF.
           IF WS-FAX-Y-N NOT = "N"
               PERFORM CONTROL-001
               PERFORM CONTROL-006
               PERFORM ERROR-020
               GO TO CONTROL-015.
           IF WS-PRINTING-TYPE NOT = "N"
               PERFORM CONTROL-001
               PERFORM CONTROL-006
               PERFORM ERROR-020
               GO TO CONTROL-015.
           
           PERFORM ERROR-020.
           MOVE 2910 TO POS.
           DISPLAY "PRINTING OF ORDER TO:                      " AT POS.
           ADD 22 TO POS
           DISPLAY WS-PRINTER AT POS.
           IF WS-FAX-Y-N = "F" OR = "P" OR = "E"
               PERFORM OPEN-020.
           MOVE WS-ORDER-NUMBER TO OO-ORDER-NUMBER.
           MOVE " "             TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
           PERFORM PRINT-ORDER-SLIP.
           CLOSE OUTSTANDING-ORDERS.
           PERFORM CONTROL-001
           PERFORM CONTROL-006
           PERFORM ERROR-020.
           GO TO CONTROL-015.
       CONTROL-999.
           EXIT.
      *
       UPDATE-OUT-ORDERS SECTION.
       UPOO-000.
           MOVE " " TO WS-ORDER-NUMBER WS-SUPPLIER-ACCEPT
                       WS-COPIES-ACCEPT WS-FAX-Y-N.
           MOVE 0 TO WS-SLIP-COPIES WS-SUPPLIER WS-SLIP-COPIES.
           IF WS-LINK-PORDER > " "
               MOVE WS-LINK-PORDER TO WS-ORDER-NUMBER.
           PERFORM CLEAR-010.
           MOVE 1010 TO POS.
           DISPLAY "ENTER THE ORDER NUMBER   : [                    ]"
            AT POS.
           MOVE 1038 TO POS.

           MOVE WS-ORDER-NUMBER TO CDA-DATA.
           MOVE 20              TO CDA-DATALEN.
           MOVE 7               TO CDA-ROW.
           MOVE 37              TO CDA-COL.
           MOVE CDA-WHITE       TO CDA-COLOR.
           MOVE 'F'             TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ORDER-NUMBER.

           IF W-ESCAPE-KEY = 4
               MOVE "2" TO WS-ABOVE-BODY
               GO TO UPOO-999.
           IF WS-ORDER-NUMBER NOT > " "
               GO TO UPOO-000.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-001
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-000.
       UPOO-001.
      *     MOVE "Y" TO WS-STILL-ON-ORDER.
           MOVE 1210 TO POS.
           DISPLAY
           "Y=ONLY ITEMS STILL ON ORDER, N=ORIG ORDER QTY." AT POS
           MOVE 1110 TO POS.
           DISPLAY "PRINT ONLY ITEMS STILL ON ORDER : [ ]"
            AT POS.
           MOVE 1145 TO POS.

           MOVE 'Y'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 8         TO CDA-ROW.
           MOVE 44        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-STILL-ON-ORDER.

           IF W-ESCAPE-KEY = 4
               GO TO UPOO-000.
           IF WS-STILL-ON-ORDER NOT = "Y" AND NOT = "N"
               GO TO UPOO-001.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-002
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-001.
       UPOO-002.
           PERFORM READ-TO-FIND-NO-OF-ITEMS.
           IF WS-LINE-NO = 0
               MOVE "THERE IS NO SUCH ORDER, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPOO-000.
           PERFORM ERROR-020
           MOVE 1110 TO POS
           DISPLAY WS-MESSAGE AT POS
           DISPLAY "TOTAL LINE ITEM NUMBER   : [   ]" AT POS
           ADD 28 TO POS
           DISPLAY S6-ITEMS AT POS.
       UPOO-004.
           PERFORM ERROR1-020
           MOVE 1210 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           IF WS-LINK-ACCOUNT > 0
              MOVE WS-LINK-ACCOUNT TO WS-SUPPLIER-ACCEPT
           ELSE
              MOVE " "             TO WS-SUPPLIER-ACCEPT.
           DISPLAY "ENTER THE SUPPLIER NUMBER: [       ]" AT POS.
           MOVE 1410 TO POS.
           DISPLAY "Enter the ACCOUNT NUMBER and <Return>, OR" AT POS.
           MOVE 1515 TO POS.
           DISPLAY "Enter a SHORT NAME and <'PgDn'> to Scroll" &
           " through Accounts." AT POS.
           MOVE 1238 TO POS.

           MOVE WS-SUPPLIER-ACCEPT TO CDA-DATA.
           MOVE 7                  TO CDA-DATALEN.
           MOVE 9                  TO CDA-ROW.
           MOVE 37                 TO CDA-COL.
           MOVE CDA-WHITE          TO CDA-COLOR.
           MOVE 'F'                TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SUPPLIER-ACCEPT.

           MOVE WS-SUPPLIER-ACCEPT TO WS-SUPPLIER.
           IF W-ESCAPE-KEY = 4
               GO TO UPOO-000.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-005.

      * "PgDn" KEY PRESSED - READ BY CR-NAME
           IF W-ESCAPE-KEY = 7
                GO TO UPOO-006
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-004.
       UPOO-005.
           IF WS-SUPPLIER = 0 OR = " "
               GO TO UPOO-004.
           PERFORM READ-SUPPLIER.
           IF CR-NAME = "UNKNOWN"
             MOVE "RE-ENTER A SUPPLIER NUMBER OF A VALID SUPPLIER."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO UPOO-004.
           MOVE " " TO WS-MESSAGE.
           MOVE 1401 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 1501 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 1510 TO POS.
           DISPLAY "FAX NUMBER : [                    ]" AT POS
           ADD 14 TO POS
           DISPLAY CR-FAX AT POS.
           MOVE 1620 TO POS.
           DISPLAY CR-NAME AT POS.
           MOVE CR-FAX TO WS-FAX-NUMBER.
           MOVE 1 TO SUB-1.
           GO TO UPOO-010.
       UPOO-006.
           PERFORM READ-CREDITOR-SHORT-NAME.
           IF WS-SUPPLIER-ACCEPT = 0 OR = " "
               GO TO UPOO-004.
           IF CR-NAME = "UNKNOWN"
             MOVE "RE-ENTER SHORT NAME OF A VALID SUPPLIER."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO UPOO-004.
      *     IF W-ESCAPE-KEY = 6
      *         Move " " to Ws-Supplier-Accept
      *         GO TO UPOO-004.
           MOVE 1 TO SUB-1.
       UPOO-010.
           MOVE " " TO WS-COMMENT (SUB-1).
           PERFORM ERROR-020.
      *     DISPLAY WS-MESSAGE AT POS.
           IF SUB-1 < 5
               ADD 1 TO SUB-1
               GO TO UPOO-010.
           MOVE 1 TO SUB-1.
           MOVE 1810 TO POS.
           DISPLAY
           "ENTER A COMMENT TO BE PRINTED ON THE ORDER, 5 LINES MAX."
                AT POS.
           MOVE 1914 TO POS.
           DISPLAY "[" AT POS.
           ADD 61 TO POS.
           DISPLAY "]" AT POS.
           MOVE " " TO WS-MESSAGE.
       UPOO-015.
           MOVE 1901 TO POS.
           DISPLAY "No=" AT POS.
           ADD 4 TO POS.
           DISPLAY SUB-1 AT POS.
           MOVE 1915 TO POS.
           MOVE WS-COMMENT (SUB-1) TO WS-COMM-MESSAGE

           MOVE WS-COMM-MESSAGE TO CDA-DATA.
           MOVE 60        TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 14        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMM-MESSAGE.

           MOVE WS-COMM-MESSAGE TO WS-COMMENT (SUB-1).
           MOVE " " TO WS-COMM-MESSAGE.
           DISPLAY WS-MESSAGE AT POS.
           IF W-ESCAPE-KEY = 4
            IF SUB-1 = 1
               GO TO UPOO-004
            ELSE
               SUBTRACT 1 FROM SUB-1
               MOVE WS-COMMENT (SUB-1) TO WS-MESSAGE
               GO TO UPOO-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-016
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-015.
       UPOO-016.
           IF WS-COMMENT (SUB-1) NOT = "    "
            IF SUB-1 < 5
              ADD 1 TO SUB-1
              GO TO UPOO-015.
           MOVE 1 TO SUB-1.
           MOVE 1810 TO POS
           DISPLAY WS-COMM-MESSAGE AT POS.
           MOVE 1801 TO POS
           DISPLAY WS-COMM-MESSAGE AT POS.
       UPOO-030.
           MOVE 2110 TO POS.
           DISPLAY 
           "ENTER: F=FAX, E=EMAIL, N=DOT MATRIX PRINT, P=PDF: [ ]."
              AT POS.
           MOVE 2222 TO POS
           DISPLAY "ENTER: B=BOTH FAX & PDF PRINTING."
              AT POS.
           MOVE 2151 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 18        TO CDA-ROW.
           MOVE 60        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FAX-Y-N.

           IF W-ESCAPE-KEY = 4
               GO TO UPOO-015.
           IF WS-FAX-Y-N NOT = "E" AND NOT = "F" AND NOT = "N"
                     AND NOT = "B" AND NOT = "P"
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-030.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-034
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-030.
       UPOO-034.
            IF WS-FAX-Y-N = "N"
                MOVE "N"      TO WS-PRINTING-TYPE
                GO TO UPOO-040.
            IF WS-FAX-Y-N = "F" 
                MOVE "N"      TO WS-PRINTING-TYPE
                MOVE CR-FAX   TO WS-FAX-NUMBER
                GO TO UPOO-035.
            IF WS-FAX-Y-N = "P"
                MOVE "P"      TO WS-PRINTING-TYPE
                MOVE "N"      TO WS-FAX-Y-N
                PERFORM ENTER-XQS-DETAILS
                GO TO UPOO-040.
            IF WS-FAX-Y-N = "B"
                MOVE "P"      TO WS-PRINTING-TYPE
                MOVE "F"      TO WS-FAX-Y-N
                MOVE CR-FAX   TO WS-FAX-NUMBER
                GO TO UPOO-035.
            IF WS-FAX-Y-N = "E"
      *          MOVE "P"      TO WS-PRINTING-TYPE
                MOVE CR-EMAIL TO WS-EMAIL-ADDR
                MOVE CR-FAX   TO WS-FAX-NUMBER
                PERFORM ENTER-XQS-DETAILS
                GO TO UPOO-038.
                
      *      MOVE CR-FAX TO WS-FAX-NUMBER.
       UPOO-035.
            PERFORM ERROR1-020.
            MOVE 3010 TO POS.
            DISPLAY
            "Enter Fax Number Only With 'NUMBERS & A DASH' No Brackets."
                AT POS.
            MOVE 2910 TO POS.
            DISPLAY 
            "Fax Number, Re-Enter If Not Correct   : [          " &
            "          ]" AT POS.
            ADD 41 TO POS.
            DISPLAY WS-FAX-NUMBER AT POS.

           MOVE WS-FAX-NUMBER TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FAX-NUMBER.

            IF W-ESCAPE-KEY = 4
               GO TO UPOO-030.
            MOVE WS-FAX-NUMBER TO WS-FAX-CHECK.
            IF WS-FAX-CHECK = " "
               MOVE "THIS FIELD CANNOT BE BLANK, 'ESC' TO RE-ENTER."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPOO-035.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-037
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-035.
       UPOO-037.
           PERFORM CHECK-FAX-NUMBER.
           PERFORM ERROR-020.
           MOVE 1510 TO POS
           DISPLAY WS-MESSAGE AT POS
           DISPLAY "FAX NUMBER : [                    ]"
            AT POS
           ADD 14 TO POS
           DISPLAY WS-FAX-NUMBER AT POS.
           IF Fax-PaNumber = 3 OR = 4
              PERFORM ENTER-XQS-DETAILS.
           GO TO UPOO-040.
       UPOO-038.
            MOVE 0 TO WS-SPACE-CNT.
            PERFORM ERROR1-020.
            MOVE 3010 TO POS
            DISPLAY "ENTER EMAIL ADDRESS IN lower case ONLY." AT POS.
            MOVE 2910 TO POS.
            DISPLAY "[                                        ]" AT POS
            ADD 1 TO POS.
            DISPLAY WS-EMAIL-ADDR AT POS.

           MOVE WS-EMAIL-ADDR TO CDA-DATA.
           MOVE 40            TO CDA-DATALEN.
           MOVE 26            TO CDA-ROW.
           MOVE 10            TO CDA-COL.
           MOVE CDA-WHITE     TO CDA-COLOR.
           MOVE 'F'           TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-EMAIL-ADDR F-NAMEFIELD.
           IF W-ESCAPE-KEY = 4
               GO TO UPOO-030.
               
           MOVE FUNCTION LOWER-CASE(F-NAMEFIELD) TO WS-EMAIL-ADDR
                                                    WS-EMAIL.
           INSPECT WS-EMAIL TALLYING WS-SPACE-CNT FOR CHARACTERS
               BEFORE INITIAL SPACE.
            
           IF WS-EMAIL(1:(WS-SPACE-CNT)) IS NOT WS-VALID-EMAIL
                MOVE "EMAIL ADDRESS HAS AN INVALID CHARACTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO UPOO-038.
 
            PERFORM CHECK-EMAIL-FOR-VALIDITY.
            IF WS-ACC-ERROR = "Y"
                GO TO UPOO-038.

            IF WS-SPACE-CNT < 10
                MOVE 
            "EMAIL ADDRESS INVALID AS IT'S TOO SHORT, 'ESC' TO RETRY." 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO UPOO-038.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-040
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-038.
       UPOO-040.
           IF WS-FAX-Y-N NOT = "N"
                GO TO UPOO-050.
           IF WS-PRINTING-TYPE = "P"
                GO TO UPOO-050.
           MOVE 2810 TO POS.
           DISPLAY "HOW MANY COPIES DO YOU WANT? ENTER 1-9: [ ]."
              AT POS.
           MOVE 2851 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 25        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COPIES-ACCEPT.

           MOVE WS-COPIES-ACCEPT TO WS-SLIP-COPIES.
           IF W-ESCAPE-KEY = 4
            IF WS-FAX-Y-N = "N"
               GO TO UPOO-030
            ELSE
               GO TO UPOO-035.
           IF WS-SLIP-COPIES NOT = 1 AND NOT = 2 AND NOT = 3 AND NOT = 4
                         AND NOT = 5 AND NOT = 6 AND NOT = 7 AND NOT = 8
                         AND NOT = 9
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-040.
       UPOO-050.
           PERFORM ERROR-020.
           MOVE 1801 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1900 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1920 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS.
       UPOO-999.
           EXIT.
      *
       ENTER-XQS-DETAILS SECTION.
       XQS-000.
           PERFORM GET-USER-MAIL-NAME.
           MOVE WS-pbValue TO WS-XQS-SNAME
                              WS-XQS-ENAME
                              WS-XQS-FROM-NAME WS-HYLA-FROM-NAME.
       XQS-001.
           MOVE " "                TO ALPHA-RATE WS-FAX-CHECK
           MOVE WS-XQS-BEG         TO ALPHA-RATE
           MOVE 10                 TO SUB-1
           MOVE CR-NAME            TO WS-CONTACT
           MOVE WS-CONTACT         TO WS-FAX-CHECK WS-HYLA-TO-NAME
           MOVE 40                 TO SUB-2.
       XQS-002.
           IF WS-F-C (SUB-2) = " "
              SUBTRACT 1 FROM SUB-2
              GO TO XQS-002.
              MOVE SUB-2 TO SUB-4.
      *SUB-4 = LENGTH OF THE CONTACT-NAME.
           MOVE 1 TO SUB-2.
       XQS-003.
           IF SUB-2 > SUB-4
              GO TO XQS-004.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 41
              ADD 1 TO SUB-1 SUB-2
              GO TO XQS-003.
       XQS-004.
           MOVE "(" TO AL-RATE (SUB-1).
           ADD 1    TO SUB-1.
           MOVE " "           TO WS-FAX-CHECK
           MOVE WS-FAX-NUMBER TO WS-FAX-CHECK.
           MOVE 1 TO SUB-2.
       XQS-015.
           IF WS-F-C (SUB-2) = " "
              MOVE ")" TO AL-RATE (SUB-1)
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-016.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 41
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-015.
       XQS-016.
           MOVE " "          TO WS-FAX-CHECK
           MOVE WS-XQS-FROM  TO WS-FAX-CHECK
           MOVE 1            TO SUB-2.
       XQS-017.
           IF WS-F-C (SUB-2) = " "
              ADD 1 TO SUB-2
            IF WS-F-C (SUB-2) = " "
              SUBTRACT 1 FROM SUB-2
              GO TO XQS-018
            ELSE
              SUBTRACT 1 FROM SUB-2.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 31
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-017.
       XQS-018.
           MOVE " "        TO WS-FAX-CHECK
           MOVE WS-XQS-END TO WS-FAX-CHECK
           MOVE 1          TO SUB-2.
       XQS-019.
           IF WS-F-C (SUB-2) = " "
              GO TO XQS-021.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 25
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-019.
       XQS-021.
           MOVE ALPHA-RATE          TO WS-XQS-LINE (1).
           MOVE " "   TO WS-FAX-CHECK ALPHA-RATE
           MOVE "(:(" TO ALPHA-RATE
           MOVE 4     TO SUB-1
           MOVE 1     TO SUB-2.
       XQS-022.
           MOVE " "           TO WS-FAX-CHECK
           MOVE WS-CO-NUMBER  TO WS-XQS2 WS-XQS4
           MOVE WS-XQS-COVERS TO WS-FAX-CHECK
           MOVE 1             TO SUB-2.
       XQS-023.
           IF WS-F-C (SUB-2) = " "
              GO TO XQS-025.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 30
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-023.
       XQS-025.
           MOVE " "        TO WS-FAX-CHECK
           MOVE WS-XQS-END TO WS-FAX-CHECK
           MOVE 1          TO SUB-2.
       XQS-026.
           IF WS-F-C (SUB-2) = " "
              GO TO XQS-027.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 25
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-026.
       XQS-027.
           MOVE ALPHA-RATE          TO WS-XQS-LINE (2).
           MOVE " "                 TO WS-FAX-CHECK ALPHA-RATE
           MOVE WS-ORDER-NUMBER     TO WS-XQS-COMMENT
           MOVE " "                 TO WS-FAX-CHECK
           MOVE WS-XQS-COMMENT      TO WS-FAX-CHECK WS-HYLA-COMMENT.
           
           MOVE 1                   TO SUB-2.
           MOVE 1                   TO SUB-1.
       XQS-028.
           IF WS-F-C (SUB-2) NOT = "0"
              GO TO XQS-029.
           MOVE " " TO WS-F-C (SUB-2).
           IF SUB-2 < 60
              ADD 1 TO SUB-2
              GO TO XQS-028.
       XQS-029.
           IF WS-F-C (SUB-2) = " "
              MOVE ALPHA-RATE          TO WS-XQS-COMMENT
              MOVE " "                 TO WS-FAX-CHECK ALPHA-RATE
              MOVE WS-XQS-COMMENT-LINE TO WS-FAX-CHECK ALPHA-RATE
              MOVE 36                  TO SUB-1
              GO TO XQS-032.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 40
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-029.
       XQS-032.
           IF AL-RATE (SUB-1) = " "
              GO TO XQS-033.
           IF SUB-1 < 60
              ADD 1 TO SUB-1
              GO TO XQS-032.
       XQS-033.
           MOVE " "        TO WS-FAX-CHECK
           MOVE WS-XQS-END TO WS-FAX-CHECK
           MOVE 1          TO SUB-2.
       XQS-034.
           IF WS-F-C (SUB-2) = " "
              GO TO XQS-0341.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 20
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-034.
       XQS-0341.
           MOVE ALPHA-RATE          TO WS-XQS-LINE (3).
           MOVE " "   TO WS-FAX-CHECK ALPHA-RATE
           MOVE "(:(" TO ALPHA-RATE
           MOVE 4     TO SUB-1
           MOVE 1     TO SUB-2.
           MOVE WS-XQS-USERNAME TO WS-FAX-CHECK
           MOVE 1               TO SUB-2.
       XQS-035.
           MOVE " "             TO WS-FAX-CHECK
           MOVE WS-XQS-PRIORITY TO WS-FAX-CHECK
           MOVE 1               TO SUB-2.
       XQS-037.
           IF WS-F-C (SUB-2) = " "
              ADD 1 TO SUB-2
            IF WS-F-C (SUB-2) = " "
              SUBTRACT 1 FROM SUB-2
              GO TO XQS-038
            ELSE
              SUBTRACT 1 FROM SUB-2.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 40
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-037.
       XQS-038.
           MOVE " "        TO WS-FAX-CHECK
           MOVE WS-XQS-END TO WS-FAX-CHECK
           MOVE 1          TO SUB-2.
       XQS-039.
           IF WS-F-C (SUB-2) = " "
              GO TO XQS-041.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 25
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-039.
       XQS-041.
           MOVE ALPHA-RATE          TO WS-XQS-LINE (4).
           MOVE " "   TO WS-FAX-CHECK ALPHA-RATE
           MOVE "(:(" TO ALPHA-RATE
           MOVE 4     TO SUB-1
           MOVE 1     TO SUB-2.
           MOVE WS-XQS-USERNAME TO WS-FAX-CHECK
           MOVE 1               TO SUB-2.
       XQS-042.
           IF WS-F-C (SUB-2) = " "
              ADD 1 TO SUB-2
            IF WS-F-C (SUB-2) = " "
              SUBTRACT 1 FROM SUB-2
              GO TO XQS-043
            ELSE
              SUBTRACT 1 FROM SUB-2.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 60
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-042.
       XQS-043.
           IF WS-F-C (SUB-2) = " "
              ADD 1 TO SUB-2
              GO TO XQS-043.
       XQS-044.
           IF WS-F-C (SUB-2) = " "
              ADD 1 TO SUB-2
            IF WS-F-C (SUB-2) = " "
              SUBTRACT 1 FROM SUB-2
              GO TO XQS-116
            ELSE
              SUBTRACT 1 FROM SUB-2.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 60
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-044.
       XQS-116.
           MOVE " "        TO WS-FAX-CHECK
           MOVE WS-XQS-END TO WS-FAX-CHECK
           MOVE 1          TO SUB-2.
       XQS-117.
           IF WS-F-C (SUB-2) = " "
              GO TO XQS-999.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 25
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-117.
       XQS-999.
           MOVE ALPHA-RATE TO WS-XQS-LINE (5).
       XQS-9999.
           EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
           MOVE OO-STOCK-NUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-010.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE " " TO ST-STOCKNUMBER
                           ST-DESCRIPTION2
               MOVE "NOT THERE!!!" TO ST-DESCRIPTION1
               MOVE 0 TO ST-PRICE
                         ST-AVERAGECOST
                         ST-DISCOUNT1
               GO TO R-ST-999.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK RECORD BUSY ON READ R-ST-010, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-010.
       R-ST-999.
             EXIT.
      *
       READ-OUTSTANDING-ORDERS SECTION.
       ROO-112.
           READ OUTSTANDING-ORDERS NEXT
               AT END
               MOVE 10 TO WS-OUTORD-ST1
               GO TO ROO-999.
           IF WS-OUTORD-ST1 = 10
               GO TO ROO-999.
           IF WS-OUTORD-ST1 NOT = 0
            MOVE "ST-ORDERS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO ROO-112.
           IF OO-ORDER-NUMBER NOT = WS-ORDER-NUMBER
               MOVE 10 TO WS-OUTORD-ST1
               GO TO ROO-999.
           IF WS-STILL-ON-ORDER = "Y"
            IF OO-QUANTITY = 0
               GO TO ROO-112.
           PERFORM READ-STOCK.
       ROO-999.
           EXIT.
      *
       READ-TO-FIND-NO-OF-ITEMS SECTION.
       RTFNOI-005.
           PERFORM OPEN-020.
           MOVE 2910 TO POS
           DISPLAY "CHECKING LINE TOTALS ....." AT POS.
           MOVE 0 TO LINE-CNT WS-LINE-NO
           MOVE WS-ORDER-NUMBER TO OO-ORDER-NUMBER
           MOVE " "             TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
               GO TO RTFNOI-900.
       RTFNOI-112.
           READ OUTSTANDING-ORDERS NEXT
               AT END
               MOVE 10 TO WS-OUTORD-ST1
               GO TO RTFNOI-900.
           IF WS-OUTORD-ST1 = 10
               GO TO RTFNOI-900.
           IF WS-OUTORD-ST1 NOT = 0
            MOVE "ST-ORDERS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO RTFNOI-112.
           IF OO-ORDER-NUMBER NOT = WS-ORDER-NUMBER
               MOVE 10 TO WS-OUTORD-ST1
               GO TO RTFNOI-900.
           IF WS-STILL-ON-ORDER = "Y"
            IF OO-QUANTITY = 0
               GO TO RTFNOI-112.
           ADD 1 TO WS-LINE-NO.
           GO TO RTFNOI-112.
       RTFNOI-900.
           MOVE WS-LINE-NO TO S6-ITEMS SE6-ITEMS.
           CLOSE OUTSTANDING-ORDERS.
           MOVE 2910 TO POS.
           DISPLAY "                                        " AT POS.
       RTFNOI-999.
           EXIT.
      *
       PRINT-ORDER-SLIP SECTION.
       POS-000.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-SLIP.
           MOVE 0 TO WS-COPIES
                     PAGE-CNT
                     WS-LINE-NO
                     WS-SUPPLIER-AMOUNT.
           MOVE 66 TO LINE-CNT.
       POS-005.
           PERFORM READ-OUTSTANDING-ORDERS.
           IF WS-OUTORD-ST1 = 10
               GO TO POS-900.
           IF LINE-CNT < 61
               GO TO POS-010.
           MOVE OO-DELIVERY-METHOD       TO WS-DEL-SUB.
           MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELVIA.
           MOVE WS-DELVIA                TO S6-DEL.
           MOVE WS-ORDER-NUMBER          TO S6-ORDER.
           ADD 1 TO PAGE-CNT.
           MOVE PAGE-CNT TO SO1-PAGE.
           IF PAGE-CNT = 1
              WRITE PRINT-REC FROM SLIP-HEAD1
              WRITE PRINT-REC FROM SLIP-HEAD2
           ELSE
              WRITE PRINT-REC FROM SLIP-HEAD1 AFTER PAGE
              WRITE PRINT-REC FROM SLIP-HEAD2 AFTER 1.
           MOVE " "         TO PRINT-REC
           WRITE PRINT-REC FROM SLIP-HEAD3 AFTER 1.
           MOVE " "         TO PRINT-REC
           MOVE PA-NAME     TO S4-NAME1
           MOVE CR-NAME     TO S4-NAME2
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 2.
           MOVE " "             TO PRINT-REC
           MOVE PA-ADD1         TO S4-NAME1
           MOVE CR-DEL-ADDRESS1 TO S4-NAME2
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1.
           MOVE " "             TO PRINT-REC
           MOVE PA-ADD2         TO S4-NAME1
           MOVE CR-DEL-ADDRESS2 TO S4-NAME2
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1.
           MOVE " "             TO PRINT-REC
           MOVE PA-CODE         TO S4-NAME1
           MOVE CR-DEL-ADDRESS3 TO S4-NAME2
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1.
           MOVE " "         TO PRINT-REC.
      ********************************************************
      *   THIS SECTION TAKEN OUT WHEN WE CHANGED TO DEL-ADD  *
      *     FROM PO-ADD                                      *
      *                                                      *
      *     IF CR-FOREIGN-LOCAL = "L"                        *
      *         MOVE PA-ADD3      TO S4-NAME1                *
      *         MOVE CR-POST-CODE TO S4-NAME2                *
      *         WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1      *
      *         MOVE " "           TO PRINT-REC SLIP-HEAD4   *
      *         MOVE "TELEFAX No:" TO S4-FAX                 *
      *         MOVE WS-FAX-NUMBER TO S4-NO                  *
      *     ELSE                                             *
      ********************************************************
               MOVE PA-ADD3       TO S4-NAME1
               MOVE "TELEFAX No:" TO S4-FAX
               MOVE WS-FAX-NUMBER TO S4-NO
               MOVE PA-CO-VAT-NO  TO S6-VAT-NO.

           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1
           MOVE " "               TO PRINT-REC
           WRITE PRINT-REC FROM SLIP-HEAD6 AFTER 2
           MOVE " "               TO PRINT-REC
           WRITE PRINT-REC FROM SLIP-HEAD7 AFTER 1
           MOVE " "               TO PRINT-REC
           WRITE PRINT-REC FROM SLIP-HEAD6-1 AFTER 1
           MOVE " "               TO PRINT-REC
           WRITE PRINT-REC FROM SLIP-HEAD6-2 AFTER 1
           MOVE " "               TO PRINT-REC
           WRITE PRINT-REC FROM SLIP-HEAD7 AFTER 1
           MOVE " "               TO PRINT-REC
           WRITE PRINT-REC FROM SLIP-HEAD8 AFTER 2
           MOVE " "               TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           MOVE 19 TO LINE-CNT.
           IF CR-FOREIGN-LOCAL = "L"
                ADD 1 TO LINE-CNT.
       POS-010.
           ADD 1                   TO WS-LINE-NO
           MOVE OO-STOCK-NUMBER    TO S-STOCKNO
           MOVE ST-DESCRIPTION1    TO S-DESC1
           MOVE ST-DESCRIPTION2    TO S-DESC2
           IF WS-STILL-ON-ORDER = "Y"
              MOVE OO-QUANTITY     TO S-QTY
           ELSE
              MOVE OO-ORIG-QTY     TO S-QTY.
      *********************************************************
      * THIS SECTION TAKEN OUT WHEN THE CHLFDOUTORD FILE HAD  *
      *  OO-COST AND OO-FOR-LOC ENTRIES ADDED.                *
      *     IF CR-FOREIGN-LOCAL = "L"                         *
      *        MOVE ST-LASTCOST     TO S-UNIT                 *
      *        MOVE ST-LASTCOST     TO B-UNITPRICE            *
      *     ELSE                                              *
      *        MOVE ST-FOREIGNCOST  TO S-UNIT                 *
      *        MOVE ST-FOREIGNCOST  TO B-UNITPRICE.           *
      *********************************************************
           MOVE OO-COST             TO S-UNIT
                                       B-UNITPRICE.
      *     IF CR-FOREIGN-LOCAL = "F"   
           MOVE OO-DISC TO S-DISC.
           COMPUTE B-TOTALPRICE ROUNDED = (B-UNITPRICE * OO-QUANTITY).

      *     IF CR-FOREIGN-LOCAL = "F"   
              COMPUTE B-TOTALPRICE = B-TOTALPRICE - 
                 (B-TOTALPRICE * (OO-DISC / 100)).
           COMPUTE WS-SUPPLIER-AMOUNT = 
                   WS-SUPPLIER-AMOUNT + B-TOTALPRICE.

           WRITE PRINT-REC FROM SLIP-DETAIL AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO POS-005.
       POS-900.
           MOVE " " TO PRINT-REC SLIP-DETAIL SLIP-TOTAL.
           MOVE "      ORDER VALUE :"     TO SLIP-TOT-COM.
           MOVE WS-SUPPLIER-AMOUNT        TO TOT-GRV
           WRITE PRINT-REC              FROM SLIP-TOTAL AFTER 1
           MOVE " "                       TO PRINT-REC.
           
      * CHANGED 8/12/2016 SO THAT THE EMAILING OF ORDERS WORKS IN POS
      *     IF CR-FOREIGN-LOCAL = "F"
      *         GO TO POS-901.
           MOVE " " TO PRINT-REC SLIP-DETAIL SLIP-TOTAL
           MOVE "       VAT AMOUNT :"  TO SLIP-TOT-COM.
           IF CR-FOREIGN-LOCAL = "L"
              COMPUTE WS-VAT-AMT =
                  WS-SUPPLIER-AMOUNT * PA-GST-PERCENT / 100
           ELSE
              MOVE 0                   TO WS-VAT-AMT.
           MOVE WS-VAT-AMT             TO TOT-GRV
           WRITE PRINT-REC              FROM SLIP-TOTAL AFTER 1
           MOVE " "                    TO PRINT-REC.
           
           MOVE " " TO PRINT-REC SLIP-DETAIL SLIP-TOTAL
           MOVE "TOTAL ORDER VALUE :"  TO SLIP-TOT-COM
           COMPUTE WS-SUPPLIER-AMOUNT = WS-SUPPLIER-AMOUNT + WS-VAT-AMT
           MOVE WS-SUPPLIER-AMOUNT     TO TOT-GRV
           WRITE PRINT-REC              FROM SLIP-TOTAL AFTER 1
           MOVE " "                    TO PRINT-REC.
           COMPUTE WS-SUPPLIER-AMOUNT = WS-SUPPLIER-AMOUNT - WS-VAT-AMT.
       POS-901.
           MOVE 1 TO SUB-1.
       POS-910.
           MOVE WS-COMMENT (SUB-1)     TO SLIP-COMM-LINE
           WRITE PRINT-REC              FROM SLIP-COMMENT AFTER 1.
           IF SUB-1 < 5
              ADD 1 TO SUB-1
            IF WS-COMMENT (SUB-1) NOT = "   "
              GO TO POS-910.
           MOVE " "             TO PRINT-REC SLIP-COMMENT.
           WRITE PRINT-REC.
           
           MOVE OO-DUEDATE           TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE "EXPECTED DUE-DATE:" TO SLIP-FILL
           MOVE DISPLAY-DATE         TO SLIP-COMM
           WRITE PRINT-REC  FROM SLIP-COMMENT AFTER 1
           MOVE " " TO PRINT-REC SLIP-COMMENT

           MOVE "**** PLEASE ACKNOWLEDGE RECEIPT OF THIS ORDER ****"
                TO SLIP-COMM-LINE.
           WRITE PRINT-REC              FROM SLIP-COMMENT AFTER 1.
           MOVE " "             TO PRINT-REC SLIP-COMMENT.
           ADD 1                       TO WS-COPIES.
           WRITE PRINT-REC.

      *     IF WS-PRINTER NOT = "[QFax]"
                PERFORM GET-USER-MAIL-NAME
                PERFORM GET-REPORT-Y2K-DATE
                PERFORM PRINT-REPORT-INFO.
           
           IF FAX-JOBNUMBER NOT = 0
               MOVE FAX-JOBNUMBER  TO F-NAMEFIELDJOB
               MOVE " JOB No#:"    TO JOB-FILLER
               MOVE F-NAMEFIELDJOB TO JOB-NUM
               WRITE PRINT-REC
               WRITE PRINT-REC FROM SLIP-JOB-LINE
               MOVE " " TO PRINT-REC SLIP-JOB-LINE.
           IF WS-FAX-Y-N NOT = "S"
            IF WS-COPIES NOT = WS-SLIP-COPIES
               CLOSE PRINT-SLIP
               CLOSE OUTSTANDING-ORDERS
               GO TO POS-950.
      
           CLOSE PRINT-SLIP.
           PERFORM SEND-REPORT-TO-PRINTER.
           GO TO POS-999.
       POS-950.
      ***************************************************************
      * CHANGED WHEN GOING TO LINUX                                 *
      * NOW WE SIMPLY TELL THE PROGRAM TO RE-PRINT THE DISK FILE AS *
      * MANY TIMES AS NEEDED - SEE NEW SECTION BELOW                *
      ***************************************************************
      *     MOVE 1 TO SUB-1
      *     MOVE 0 TO PAGE-CNT
      *               WS-SUPPLIER-AMOUNT
      *     MOVE 66 TO LINE-CNT
      *     PERFORM OPEN-020
      *     PERFORM GET-USER-PRINT-NAME
      *     OPEN OUTPUT PRINT-SLIP
      *     MOVE WS-ORDER-NUMBER TO OO-ORDER-NUMBER
      *     MOVE " "             TO OO-STOCK-NUMBER.
      *     START OUTSTANDING-ORDERS KEY NOT < OO-KEY
      *         INVALID KEY NEXT SENTENCE.
      *     GO TO POS-005.

           PERFORM SEND-REPORT-TO-PRINTER.
           ADD 1 TO WS-COPIES.
           IF WS-COPIES NOT = WS-SLIP-COPIES
               GO TO POS-950.
       POS-999.
           EXIT.
      *
       REMOVE-SPACES-IN-FAX-NAME SECTION.
       RSIFN-005.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
           MOVE WS-FILE-NAME-FOR-FAX TO ALPHA-RATE
           MOVE 1 TO SUB-45.
       RSIFN-010.
           IF AL-RATE (SUB-45) NOT = " "
              ADD 1 TO SUB-45 
              GO TO RSIFN-010.
           MOVE "-" TO AL-RATE (SUB-45).
           ADD 1 TO SUB-45.
           IF PAGE-CNT = 1 
              MOVE 1 TO AL-RATE (SUB-45)
           ELSE 
              MOVE 2 TO AL-RATE (SUB-45).
       RSIFN--020.
      * TO REMOVE THE / FROM THE P/O ORDER NUMBER AS IN 
      *  LINUX THIS DENOTES A FOLDER
           MOVE "-" TO AL-RATE (14)
           MOVE ALPHA-RATE TO WS-PRINTER.
       RSIFN-999.
           EXIT.
      *
       PRINT-XQSORDER-SLIP SECTION.
       POSXQS-000.
           MOVE 1 TO PAGE-CNT.
           IF Fax-PaNumber = 4
               MOVE WS-HYLA-COMMENT TO WS-QUOTE-REFERENCE
               PERFORM REMOVE-SPACES-IN-FAX-NAME
               MOVE WS-PRINTER TO WS-PRINTER-PAGE1.
               
          OPEN OUTPUT PRINT-SLIP.
          IF Fax-PaNumber = 3
             WRITE PRINT-REC FROM WS-XQS-LINE (1)
             WRITE PRINT-REC FROM WS-XQS-LINE (2)
             WRITE PRINT-REC FROM WS-XQS-LINE (3)
             WRITE PRINT-REC FROM WS-XQS-LINE (4)
             WRITE PRINT-REC FROM WS-XQS-LINE (5).
               
           IF Fax-PaNumber = 4
               PERFORM GET-REPORT-Y2K-DATE
               MOVE PBRET          TO WS-REPORT-DATE
               MOVE WS-REPORT-DATE TO WS-HYLA-DATE
               MOVE PAGE-CNT       TO WS-HYLA-PAGE
               
               MOVE SPACES TO PRINT-REC
               WRITE PRINT-REC AFTER 11
               WRITE PRINT-REC FROM WS-HYLA-TO-LINE
               MOVE SPACES TO PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC FROM WS-HYLA-FROM-LINE
               MOVE SPACES TO PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC FROM WS-HYLA-TYPE-LINE
               MOVE SPACES TO PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC FROM WS-HYLA-COMMENT-LINE
               MOVE SPACES TO PRINT-REC
               WRITE PRINT-REC.
             
          MOVE 0 TO WS-COPIES
                    PAGE-CNT
                    WS-LINE-NO
                    WS-SUPPLIER-AMOUNT.
          MOVE 66 TO LINE-CNT.
       POSXQS-005.
           PERFORM READ-OUTSTANDING-ORDERS.
           IF WS-OUTORD-ST1 = 10
            IF LINE-CNT = 36
               ADD 1 TO PAGE-CNT
               GO TO POSXQS-900
            ELSE
               GO TO POSXQS-900.
          IF Fax-PaNumber = 3 OR = 4
           IF PAGE-CNT = 1
            IF LINE-CNT < 36
               GO TO POSXQS-010.
          IF Fax-PaNumber = 3 OR = 4
           IF PAGE-CNT > 1
            IF LINE-CNT < 51
               GO TO POSXQS-010.
               
           MOVE OO-DELIVERY-METHOD       TO WS-DEL-SUB.
           MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELVIA.
           MOVE WS-DELVIA                TO S6-DEL.
           MOVE WS-ORDER-NUMBER          TO S6-ORDER.
           MOVE PA-CO-VAT-NO             TO S6-VAT-NO.
           
           ADD 1 TO PAGE-CNT.

          IF Fax-PaNumber = 3 OR = 4
           IF PAGE-CNT = 1
              WRITE PRINT-REC FROM SLIP-HEAD6 AFTER 2
              MOVE " "               TO PRINT-REC
              WRITE PRINT-REC FROM SLIP-HEAD7 AFTER 1
              MOVE " "               TO PRINT-REC
              WRITE PRINT-REC FROM SLIP-HEAD6-1 AFTER 1
              MOVE " "               TO PRINT-REC
              WRITE PRINT-REC FROM SLIP-HEAD6-2 AFTER 1
              MOVE " "               TO PRINT-REC
              WRITE PRINT-REC FROM SLIP-HEAD7 AFTER 1
              MOVE " "               TO PRINT-REC
              WRITE PRINT-REC FROM SLIP-HEAD8 AFTER 2.
              
           IF Fax-PaNumber = 4
            IF PAGE-CNT = 1
             IF LINE-CNT = 66
               GO TO POSXQS-008.
              
           IF Fax-PaNumber = 4
            IF PAGE-CNT = 2
             IF LINE-CNT > 35
                 CLOSE PRINT-SLIP
                 PERFORM REMOVE-SPACES-IN-FAX-NAME
                 MOVE WS-PRINTER TO WS-PRINTER-PAGE2
                 OPEN OUTPUT PRINT-SLIP
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM WS-HYLA-TYPE-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 MOVE PAGE-CNT       TO WS-HYLA-PAGE2
                 WRITE PRINT-REC FROM WS-HYLA-FROM-LINE2 AFTER 1
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM SLIP-HEAD8
                 GO TO POSXQS-008.
           IF Fax-PaNumber = 4
            IF PAGE-CNT > 2
             IF LINE-CNT > 50
                 MOVE " " TO PRINT-REC
                 WRITE PRINT-REC BEFORE PAGE
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM WS-HYLA-TYPE-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 MOVE PAGE-CNT TO WS-HYLA-PAGE2
                 WRITE PRINT-REC FROM WS-HYLA-FROM-LINE2 AFTER 1
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM SLIP-HEAD8.
       POSXQS-008.
           MOVE " "               TO PRINT-REC
           IF PAGE-CNT = 1
              WRITE PRINT-REC AFTER 1
              MOVE 8 TO LINE-CNT
           ELSE
              MOVE 2 TO LINE-CNT.
           IF CR-FOREIGN-LOCAL = "L"
                ADD 1 TO LINE-CNT.
       POSXQS-010.
           ADD 1                   TO WS-LINE-NO
           MOVE WS-LINE-NO         TO S-ITEM
           MOVE OO-STOCK-NUMBER    TO S-STOCKNO
           MOVE ST-DESCRIPTION1    TO S-DESC1
           MOVE ST-DESCRIPTION2    TO S-DESC2
           IF WS-STILL-ON-ORDER = "Y"
              MOVE OO-QUANTITY     TO S-QTY
           ELSE
              MOVE OO-ORIG-QTY     TO S-QTY.
      *********************************************************
      * THIS SECTION TAKEN OUT WHEN THE CHLFDOUTORD FILE HAD  *
      *  OO-COST AND OO-FOR-LOC ENTRIES ADDED.                *
      *     IF CR-FOREIGN-LOCAL = "L"                         *
      *        MOVE ST-LASTCOST     TO S-UNIT                 *
      *        MOVE ST-LASTCOST     TO B-UNITPRICE            *
      *     ELSE                                              *
      *        MOVE ST-FOREIGNCOST  TO S-UNIT                 *
      *        MOVE ST-FOREIGNCOST  TO B-UNITPRICE.           *
      *********************************************************
           MOVE OO-COST             TO S-UNIT
                                       B-UNITPRICE.
      *     IF CR-FOREIGN-LOCAL = "F"
           MOVE OO-DISC TO S-DISC.
           COMPUTE B-TOTALPRICE ROUNDED = (B-UNITPRICE * OO-QUANTITY).
           COMPUTE B-TOTALPRICE = B-TOTALPRICE - 
              (B-TOTALPRICE * (OO-DISC / 100)).
           COMPUTE WS-SUPPLIER-AMOUNT = 
                   WS-SUPPLIER-AMOUNT + B-TOTALPRICE.
           WRITE PRINT-REC FROM SLIP-DETAIL AFTER 1.
           
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT.
           
           GO TO POSXQS-005.
       POSXQS-900.
           IF PAGE-CNT = 1
              ADD 1 TO PAGE-CNT
              MOVE "Y" TO WS-PAGE-CHANGED.
      *          MOVE "POSXQS-900" TO WS-MESSAGE 
      *          MOVE PAGE-CNT TO WS-MESSAGE 
      *          PERFORM ERROR1-000
      *          MOVE LINE-CNT TO WS-MESSAGE
      *          PERFORM ERROR-MESSAGE
      *          PERFORM ERROR1-020.

           IF Fax-PaNumber = 4
            IF PAGE-CNT = 2
             IF LINE-CNT > 35
                 CLOSE PRINT-SLIP
                 PERFORM REMOVE-SPACES-IN-FAX-NAME
                 MOVE WS-PRINTER TO WS-PRINTER-PAGE2
                 OPEN OUTPUT PRINT-SLIP
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM WS-HYLA-TYPE-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 MOVE PAGE-CNT       TO WS-HYLA-PAGE2
                 WRITE PRINT-REC FROM WS-HYLA-FROM-LINE2 AFTER 1
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM SLIP-HEAD8
                 GO TO POSXQS-902.
           IF Fax-PaNumber = 4
            IF PAGE-CNT > 2
             IF LINE-CNT > 50
                 MOVE " " TO PRINT-REC
                 WRITE PRINT-REC BEFORE PAGE
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM WS-HYLA-TYPE-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 MOVE PAGE-CNT TO WS-HYLA-PAGE2
                 WRITE PRINT-REC FROM WS-HYLA-FROM-LINE2 AFTER 1
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM SLIP-HEAD8.
       POSXQS-902.
      * HERE WE DEDUCT 1 FROM PAGE-CNT TO RESET THE POSXQS-900 = ADD 1
           IF PAGE-CNT = 2
            IF WS-PAGE-CHANGED = "Y"
              MOVE "N" TO WS-PAGE-CHANGED
              SUBTRACT 1 FROM PAGE-CNT.
           
           MOVE " " TO PRINT-REC SLIP-DETAIL SLIP-TOTAL
           MOVE "      ORDER VALUE :"     TO SLIP-TOT-COM
           MOVE WS-SUPPLIER-AMOUNT        TO TOT-GRV
           WRITE PRINT-REC              FROM SLIP-TOTAL AFTER 1
           MOVE " "                       TO PRINT-REC.
           
      * CHANGED 8/12/2016 SO THAT THE EMAILING OF ORDERS WORKS IN POS
      *     IF CR-FOREIGN-LOCAL = "F"
      *         GO TO POSXQS-905.
           MOVE " " TO PRINT-REC SLIP-DETAIL SLIP-TOTAL
           MOVE "       VAT AMOUNT :"  TO SLIP-TOT-COM.
           IF CR-FOREIGN-LOCAL = "L"
              COMPUTE WS-VAT-AMT =
                  WS-SUPPLIER-AMOUNT * PA-GST-PERCENT / 100
           ELSE
              MOVE 0                   TO WS-VAT-AMT.
           MOVE WS-VAT-AMT             TO TOT-GRV
           WRITE PRINT-REC              FROM SLIP-TOTAL AFTER 1
           MOVE " "                    TO PRINT-REC.
           
           MOVE " " TO PRINT-REC SLIP-DETAIL SLIP-TOTAL
           MOVE "TOTAL ORDER VALUE :"  TO SLIP-TOT-COM
           COMPUTE WS-SUPPLIER-AMOUNT = WS-SUPPLIER-AMOUNT + WS-VAT-AMT
           MOVE WS-SUPPLIER-AMOUNT     TO TOT-GRV
           WRITE PRINT-REC              FROM SLIP-TOTAL AFTER 1
           MOVE " "                    TO PRINT-REC.
           COMPUTE WS-SUPPLIER-AMOUNT = WS-SUPPLIER-AMOUNT - WS-VAT-AMT.
       POSXQS-905.
           MOVE 1 TO SUB-1.
       POSXQS-910.
           MOVE WS-COMMENT (SUB-1)     TO SLIP-COMM-LINE
           WRITE PRINT-REC           FROM SLIP-COMMENT AFTER 1.
           IF SUB-1 < 5
              ADD 1 TO SUB-1
            IF WS-COMMENT (SUB-1) NOT = "   "
              GO TO POSXQS-910.
           MOVE " "             TO PRINT-REC SLIP-COMMENT.
      *     WRITE PRINT-REC.
           MOVE "**** PLEASE ACKNOWLEDGE RECEIPT OF THIS ORDER ****"
                TO SLIP-COMM-LINE.
           WRITE PRINT-REC              FROM SLIP-COMMENT AFTER 1.
           MOVE " "             TO PRINT-REC SLIP-COMMENT.
           WRITE PRINT-REC.
       POSXQS-950.
           CLOSE PRINT-SLIP.

      * IF PAGE-CNT > 2 WE MOVE 2 TO PAGE-CNT AS THERE ARE ONLY 
      * TWO FILES CREATED - 1 AND 2.  2 HAS ALL THE SUBSEQUENT PAGES
      * INSIDE IT.
           IF PAGE-CNT > 2 
              MOVE 2 TO PAGE-CNT.

           IF Fax-PaNumber = 4
            IF WS-FAX-Y-N = "F"
             IF PAGE-CNT = 1
                 PERFORM WORK-OUT-PDF-FILE-NAMES
                 MOVE WS-PRINTER-PAGE1   TO WS-PRINTER
                 PERFORM FIND-PDF-TYPE-PRINTER
                 PERFORM SETUP-PORDER-FOR-PDF
                 GO TO POSXQS-999
             ELSE
                 PERFORM WORK-OUT-PDF-FILE-NAMES
                 MOVE WS-PRINTER-PAGE1   TO WS-PRINTER
                 PERFORM FIND-PDF-TYPE-PRINTER
                 PERFORM SETUP-PORDER-FOR-PDF
                 MOVE WS-PRINTER-PAGE2   TO WS-PRINTER
                 PERFORM SETUP-PORDER2-FOR-PDF
                 PERFORM SETUP-MERGE-PORDER-FOR-PDF
                 GO TO POSXQS-999.

      *        MOVE PAGE-CNT TO WS-MESSAGE
      *        PERFORM ERROR-MESSAGE.

           IF Fax-PaNumber = 4
      *      IF WS-PRINTING-TYPE = "P"
              IF PAGE-CNT = 1
                 PERFORM WORK-OUT-PDF-FILE-NAMES
                 MOVE WS-PRINTER-PAGE1   TO WS-PRINTER
                 PERFORM FIND-PDF-TYPE-PRINTER
                 PERFORM SETUP-PORDER-FOR-PDF
              ELSE
                 PERFORM WORK-OUT-PDF-FILE-NAMES
                 MOVE WS-PRINTER-PAGE1   TO WS-PRINTER
                 PERFORM FIND-PDF-TYPE-PRINTER
                 PERFORM SETUP-PORDER-FOR-PDF
                 MOVE WS-PRINTER-PAGE2   TO WS-PRINTER
                 PERFORM SETUP-PORDER2-FOR-PDF
                 PERFORM SETUP-MERGE-PORDER-FOR-PDF.

           IF Fax-PaNumber = 4
            IF WS-FAX-Y-N = "E"
                MOVE "PURCHASE ORDER"  TO WS-SUBJECT-LINE1
                MOVE WS-ORDER-NUMBER   TO WS-SUBJECT-LINE2
                MOVE " FROM:"          TO WS-SUBJECT-LINE3 
                MOVE WS-CO-NAME        TO WS-SUBJECT-LINE4
                PERFORM TAKE-OUT-BLANKS-IN-CO-NAME
                PERFORM MAKE-PDF-FINAL-FOR-EMAIL
                PERFORM SETUP-PORDER-FOR-PDF-MGEMAIL. 
       POSXQS-999.
           EXIT.
      *
       MAKE-PDF-FINAL-FOR-EMAIL SECTION.
       MFPFE-001.
           MOVE SPACES           TO ALPHA-RATE DATA-RATE.
       MFPFE-002.
           MOVE "/ctools/fax/"   TO ALPHA-RATE
           
           MOVE WS-CO-NUMBER     TO DATA-RATE
           MOVE DAT-RATE(1) TO AL-RATE(13)
           MOVE DAT-RATE(2) TO AL-RATE(14)

           MOVE 1 TO SUB-45
           MOVE 1 TO SUB-50.
       MFPFE-005.
           MOVE AL-RATE (SUB-45) TO DAT-RATE (SUB-50)
           ADD 1 TO SUB-45 SUB-50.
           IF AL-RATE (SUB-45) NOT = " "
               GO TO MFPFE-005.
           MOVE "P"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "O"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "r"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "d"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "e"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "r"  TO DAT-RATE (SUB-50)
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
           MOVE CR-ACCOUNT-NUMBER TO DATA-RATE.
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
           IF SUB-3 = 1 OR = 2 OR = 3 OR = 4
              GO TO TOBICN-010.
           MOVE "'" TO AL-RATE (SUB-1).
       TOBICN-030.
           MOVE SPACES       TO WS-SUBJECT-FIXED
           MOVE ALPHA-RATE   TO WS-SUBJECT-FIXED.
           
      *     MOVE WS-SUBJECT-FIXED TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       TOBICN-999.
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
       WORK-OUT-PDF-FILE-NAMES SECTION.
       WOPFN-001.
           MOVE SPACES           TO ALPHA-RATE DATA-RATE.
           MOVE WS-PRINTER-PAGE1 TO ALPHA-RATE.
           MOVE 13 TO SUB-45
           MOVE 1  TO SUB-46.
       WOPFN-010.
           MOVE AL-RATE (SUB-45) TO DAT-RATE (SUB-46)
           ADD 1 TO SUB-45 SUB-46.
           IF AL-RATE (SUB-45) NOT = " "
               GO TO WOPFN-010.
           MOVE DATA-RATE TO WS-PRINTER-PAGE1.
           MOVE SPACES           TO ALPHA-RATE DATA-RATE
           MOVE WS-PRINTER-PAGE2 TO ALPHA-RATE.
           MOVE 13 TO SUB-45
           MOVE 1  TO SUB-46.
       WOPFN-015.
           MOVE AL-RATE (SUB-45) TO DAT-RATE (SUB-46)
           ADD 1 TO SUB-45 SUB-46.
           IF AL-RATE (SUB-45) NOT = " "
               GO TO WOPFN-015.
           MOVE DATA-RATE        TO WS-PRINTER-PAGE2.
           MOVE SPACES           TO ALPHA-RATE DATA-RATE.
       WOPFN-999.
           EXIT.
      *
       PRINT-EMAIL-SLIP SECTION.
       PRE-000.
           OPEN OUTPUT PRINT-SLIP.

           PERFORM Z1-HEADINGS.
           MOVE 0 TO WS-COPIES
                     PAGE-CNT
                     WS-LINE-NO
                     WS-SUPPLIER-AMOUNT.
           MOVE 66 TO LINE-CNT.
       PRE-00010.
           MOVE "´¶"         TO WS-DELIM-F
           MOVE "¶"          TO H1-1
           MOVE "³"          TO H1-2.
       PRE-00011.
           IF PAGE-CNT = 0
                MOVE Ws-EMail-Addr TO WS-DATA-F
           ELSE
                MOVE SPACES          TO WS-DATA-F.
           WRITE PRINT-REC FROM WS-FST-LINE AFTER 1.
       PRE-005.
           PERFORM READ-OUTSTANDING-ORDERS.
           IF WS-OUTORD-ST1 = 10
               GO TO PRE-450.
           IF LINE-CNT < 63
               GO TO PRE-010.
       PRE-006.
           MOVE "¶"                      TO H1-1
           MOVE "³"                      TO H1-2
           MOVE OO-DELIVERY-METHOD       TO WS-DEL-SUB
           MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELVIA
           MOVE WS-DELVIA                TO SE6-DEL
           MOVE WS-ORDER-NUMBER          TO SE6-ORDER
           ADD 1                         TO PAGE-CNT.
           
           IF PAGE-CNT > 1
               PERFORM PRE-00011.
           
           MOVE PAGE-CNT                 TO SOE1-PAGE.
           WRITE PRINT-REC FROM SLIPE-HEAD1 AFTER 1.
           
           MOVE "¶"                      TO H2-1
           MOVE "³"                      TO H2-2
           WRITE PRINT-REC FROM SLIPE-HEAD2 AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-COMMENT
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.

           MOVE " "          TO PRINT-REC
           MOVE "¶"          TO H3-1
           MOVE "³"          TO H3-2
           WRITE PRINT-REC FROM SLIPE-HEAD3 AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-COMMENT
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.
           
           MOVE " "             TO PRINT-REC
           MOVE "¶"             TO H4-1
           MOVE "³"             TO H4-2
           MOVE PA-NAME         TO SE4-NAME1
           MOVE CR-NAME         TO SE4-NAME2
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "             TO PRINT-REC
           MOVE "¶"             TO H4-1
           MOVE "³"             TO H4-2
           MOVE PA-ADD1         TO SE4-NAME1
           MOVE CR-DEL-ADDRESS1 TO SE4-NAME2
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "             TO PRINT-REC
           MOVE "¶"             TO H4-1
           MOVE "³"             TO H4-2
           MOVE PA-ADD2         TO SE4-NAME1
           MOVE CR-DEL-ADDRESS2 TO SE4-NAME2
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "             TO PRINT-REC
           MOVE "¶"             TO H4-1
           MOVE "³"             TO H4-2
           MOVE PA-CODE         TO SE4-NAME1
           MOVE CR-DEL-ADDRESS3 TO SE4-NAME2
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.
           
           MOVE " "             TO PRINT-REC.
           MOVE "¶"             TO H4-1
           MOVE "³"             TO H4-2
           MOVE PA-ADD3         TO SE4-NAME1
           MOVE "TELEFAX No:"   TO SE4-FAX
           MOVE WS-FAX-NUMBER   TO SE4-NO
           WRITE PRINT-REC FROM SLIPE-HEAD4 AFTER 1.

           MOVE " "               TO PRINT-REC
           MOVE "¶"               TO H5-1
           MOVE "³"               TO H5-2
           MOVE CR-ACCOUNT-NUMBER TO H5-ACC
           WRITE PRINT-REC FROM SLIPE-HEAD5 AFTER 1.

      *    MOVE " " TO PRINT-REC SLIPE-COMMENT
      *    MOVE "¶"          TO H11-1
      *    MOVE "³"          TO H11-2.
      *    WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.

           MOVE " "          TO PRINT-REC
           MOVE "¶"          TO H6-1
           MOVE "³"          TO H6-2.
           WRITE PRINT-REC FROM SLIPE-HEAD6 AFTER 1.
           
           MOVE " "          TO PRINT-REC
           MOVE "¶"          TO H7-1
           MOVE "³"          TO H7-2.
           WRITE PRINT-REC FROM SLIPE-HEAD7 AFTER 1.
           
           MOVE " "           TO PRINT-REC
           MOVE "¶"           TO H61-1
           MOVE "³"           TO H61-2
           MOVE PA-CO-VAT-NO  TO SE6-VAT-NO.
           WRITE PRINT-REC FROM SLIPE-HEAD6-1 AFTER 1.

           MOVE " "          TO PRINT-REC
           MOVE "¶"          TO H62-1
           MOVE "³"          TO H62-2.
           WRITE PRINT-REC FROM SLIPE-HEAD6-2 AFTER 1.
           
           MOVE " "          TO PRINT-REC
           MOVE "¶"          TO H7-1
           MOVE "³"          TO H7-2.
           WRITE PRINT-REC FROM SLIPE-HEAD7 AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-COMMENT
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.
           
           MOVE " "          TO PRINT-REC
           MOVE "¶"          TO H8-1
           MOVE "³"          TO H8-2.
           WRITE PRINT-REC FROM SLIPE-HEAD8 AFTER 1.
           
           MOVE " " TO PRINT-REC SLIPE-COMMENT
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2.
           WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.

           MOVE 20 TO LINE-CNT.
       PRE-010.
           MOVE "¶"                TO H9-1
           MOVE "³"                TO H9-2
           ADD 1                   TO WS-LINE-NO
           MOVE WS-LINE-NO         TO SE-ITEM
           MOVE OO-STOCK-NUMBER    TO SE-STOCKNO
           MOVE ST-DESCRIPTION1    TO SE-DESC1
           MOVE ST-DESCRIPTION2    TO SE-DESC2.
           IF WS-STILL-ON-ORDER = "Y"
              MOVE OO-QUANTITY     TO SE-QTY
           ELSE
              MOVE OO-ORIG-QTY     TO SE-QTY.
           MOVE OO-COST            TO SE-UNIT
                                      B-UNITPRICE.
      *     IF CR-FOREIGN-LOCAL = "F"
           MOVE OO-DISC  TO SE-DISC.
           COMPUTE B-TOTALPRICE ROUNDED = (B-UNITPRICE * OO-QUANTITY).
           COMPUTE B-TOTALPRICE = B-TOTALPRICE - 
              (B-TOTALPRICE * (OO-DISC / 100)).
           COMPUTE WS-SUPPLIER-AMOUNT = 
                   WS-SUPPLIER-AMOUNT + B-TOTALPRICE.
           WRITE PRINT-REC FROM SLIPE-DETAIL AFTER 1.
           
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRE-005.
       PRE-450.
           IF LINE-CNT > 52
              GO TO PRE-600.       
           IF LINE-CNT < 53
              GO TO PRE-500. 
       PRE-500.
           MOVE " "          TO PRINT-REC SLIPE-COMMENT
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.
           
           ADD 1 TO LINE-CNT.
           GO TO PRE-900.
       PRE-600.
           MOVE " "          TO PRINT-REC SLIPE-COMMENT
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.
           
           ADD 1 TO LINE-CNT.
           IF LINE-CNT < 63
              GO TO PRE-600.
              
           PERFORM PRE-006.
       PRE-900.
           MOVE " " TO PRINT-REC SLIPE-DETAIL SLIPE-TOTAL
           MOVE "¶"                       TO H10-1
           MOVE "³"                       TO H10-2
           MOVE "      ORDER VALUE :"     TO SLIPE-TOT-COM
           MOVE WS-SUPPLIER-AMOUNT        TO TOTE-GRV
           WRITE PRINT-REC              FROM SLIPE-TOTAL AFTER 1
           MOVE " "                       TO PRINT-REC.

      *     MOVE " "          TO PRINT-REC SLIPE-COMMENT
      *     MOVE "¶"          TO H11-1
      *     MOVE "³"          TO H11-2
      *     WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.
      *     WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.
           ADD 1 TO LINE-CNT.
           
      * CHANGED 8/12/2016 SO THAT THE EMAILING OF ORDERS WORKS IN POS
      *     IF CR-FOREIGN-LOCAL = "F"
      *         GO TO POS-901.
           MOVE " " TO PRINT-REC SLIPE-DETAIL SLIPE-TOTAL
           MOVE "¶"                    TO H10-1
           MOVE "³"                    TO H10-2
           MOVE "       VAT AMOUNT :"  TO SLIPE-TOT-COM.
           IF CR-FOREIGN-LOCAL = "L"
              COMPUTE WS-VAT-AMT =
                  WS-SUPPLIER-AMOUNT * PA-GST-PERCENT / 100
           ELSE
              MOVE 0                   TO WS-VAT-AMT.
           MOVE WS-VAT-AMT             TO TOTE-GRV
           WRITE PRINT-REC          FROM SLIPE-TOTAL AFTER 1
           MOVE " "                    TO PRINT-REC.
           
           MOVE " " TO PRINT-REC SLIPE-DETAIL SLIPE-TOTAL
           MOVE "¶"                    TO H10-1
           MOVE "³"                    TO H10-2
           MOVE "TOTAL ORDER VALUE :"  TO SLIPE-TOT-COM
           COMPUTE WS-SUPPLIER-AMOUNT = WS-SUPPLIER-AMOUNT + WS-VAT-AMT
           MOVE WS-SUPPLIER-AMOUNT     TO TOTE-GRV
           WRITE PRINT-REC              FROM SLIPE-TOTAL AFTER 1
           MOVE " "                    TO PRINT-REC.
           COMPUTE WS-SUPPLIER-AMOUNT = WS-SUPPLIER-AMOUNT - WS-VAT-AMT.
           
           ADD 2 TO LINE-CNT.
       PRE-901.
           MOVE 1 TO SUB-1.
       PRE-910.
           MOVE "¶"                    TO H11-1
           MOVE "³"                    TO H11-2
           MOVE WS-COMMENT (SUB-1)     TO SLIPE-COMM-LINE
           WRITE PRINT-REC           FROM SLIPE-COMMENT AFTER 1
           ADD 1 TO LINE-CNT.
           
           IF SUB-1 < 5
              ADD 1 TO SUB-1
            IF WS-COMMENT (SUB-1) NOT = "   "
              GO TO PRE-910.
              
           MOVE " "             TO PRINT-REC SLIPE-COMMENT.
           MOVE "¶"             TO H11-1
           MOVE "³"             TO H11-2
           WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.

           MOVE "**** PLEASE ACKNOWLEDGE RECEIPT OF THIS ORDER ****"
                TO SLIPE-COMM-LINE.
           WRITE PRINT-REC              FROM SLIPE-COMMENT AFTER 1.
           
           MOVE " "             TO PRINT-REC SLIPE-COMMENT.
           MOVE "¶"             TO H11-1
           MOVE "³"             TO H11-2
           WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.

      *     ADD 1                TO WS-COPIES.
           ADD 3 TO LINE-CNT.

      *     PERFORM GET-USER-MAIL-NAME
      *     PERFORM GET-REPORT-Y2K-DATE
      *     PERFORM PRINT-REPORT-INFO.
       PRE-920.
           IF LINE-CNT > 62
              GO TO PRE-950.
           MOVE " "          TO PRINT-REC SLIPE-COMMENT
           MOVE "¶"          TO H11-1
           MOVE "³"          TO H11-2
           WRITE PRINT-REC FROM SLIPE-COMMENT AFTER 1.
           
           ADD 1 TO LINE-CNT.
           IF LINE-CNT < 63
              GO TO PRE-920.
       PRE-950.
           CLOSE PRINT-SLIP.
           PERFORM MOVE-EMAIL-FROM-EIMAGE-SETUP
           PERFORM MOVE-EMAIL-RECORD-FROM-EIMAGE.
       PRE-999.
           EXIT.
      *
       MOVE-EMAIL-FROM-EIMAGE-SETUP SECTION.
       MERFES-005.
             MOVE WS-TEMP-EMAIL-FILE TO WS-EMAIL-FINAL.
             
             MOVE "/ctools/epordr/" TO WS-EF-FIL.
       MERFES-999.
            EXIT.
      *
       GET-EMAIL-PORDR-NAME SECTION.
       GEQN-000.
           MOVE " " TO ALPHA-RATE
                       WS-ORDER-CHECK.
      * NEW SECTION FROM GEQN-006 SO ONE CAN SEND PORDERS BY MAIL
      * MULTIPLE TIMES WITHOUT CONFUSING THE POS SYSTEM..........
      * WE IGNORE THE FIRST TWO SECTIONS OF GEQN-
           GO TO GEQN-006.
       GEQN-002.
           MOVE WS-ORDER-NUMBER TO WS-ORDER-CHECK.

           MOVE 4 TO SUB-1
           MOVE 1 TO SUB-2.
       GEQN-005.
           MOVE WS-O-C (SUB-1) TO AL-RATE (SUB-2).
           IF SUB-2 NOT > 19
              ADD 1 TO SUB-1 SUB-2.
              
           IF WS-O-C (SUB-1) NOT = "."
              GO TO GEQN-005.

           MOVE " " TO WS-EPORDER.
           MOVE ALPHA-RATE TO WS-EPORDER.
           MOVE 1 TO SUB-1 SUB-2.
       GEQN-006.
            MOVE SPACES TO ALPHA-RATE DATA-RATE.
            MOVE "/ctools/eimage/" TO ALPHA-RATE
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
            MOVE ALPHA-RATE TO WS-EMAIL-TEMP-ORDER-FILE
                               WS-TEMP-EMAIL-FILE.

      *      MOVE WS-EMAIL-TEMP-ORDER-FILE TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.

       GEQN-999.
           EXIT.
      *
       CHECK-ORDER SECTION.
       OC-010.
           MOVE WS-ORDER-NUMBER TO WS-ORDER-CHECK.
           MOVE 1 TO SUB-1.
       OC-015.
           IF WS-O-C (SUB-1) = "."
              MOVE "-" TO WS-O-C (SUB-1).
           IF WS-O-C (SUB-1) = " "
              GO TO OC-016.
           IF SUB-1 < 25
              ADD 1 TO SUB-1
              GO TO OC-015.
       OC-016.
           MOVE WS-ORDER-CHECK  TO WS-REST
           MOVE WS-SP-PRINT     TO WS-PRINTER
           MOVE SUB-1 TO SUB-5
           SUBTRACT 1 FROM SUB-1
           ADD 9 TO SUB-1
           MOVE WS-SP-PRINT TO FAX-ASCIIFILENAME
           MOVE SUB-1       TO FAX-CBASCIIFILENAME
           ADD 5 TO SUB-1
           MOVE SUB-1 TO FAX-CBFAXFILENAME
           MOVE SUB-5 TO SUB-1.
       OC-017.
           MOVE "." TO WS-O-C (SUB-1)
           ADD 1 TO SUB-1
           MOVE "F" TO WS-O-C (SUB-1)
           ADD 1 TO SUB-1
           MOVE "a" TO WS-O-C (SUB-1)
           ADD 1 TO SUB-1
           MOVE "x" TO WS-O-C (SUB-1)
           MOVE WS-ORDER-CHECK TO WS-FAX-REST
           MOVE WS-FAX-SEND    TO FAX-FAXFILENAME
           MOVE 1 TO SUB-1.
       OC-999.
           EXIT.
      *
       CHECK-FAX-NUMBER SECTION.
       FNC-010.
           MOVE WS-FAX-NUMBER TO WS-FAX-CHECK.
           MOVE 1 TO SUB-1.
           IF WS-FAX-NUMBER = " "
               GO TO FNC-900.
       FNC-015.
           IF WS-F-C (SUB-1) = "." OR = "-"
              MOVE "," TO WS-F-C (SUB-1).
           IF WS-F-C (SUB-1) = "/"
               GO TO FNC-020.
           IF WS-F-C (SUB-1) = " "
              ADD 1 TO SUB-1
            IF WS-F-C (SUB-1) = " "
              SUBTRACT 1 FROM SUB-1
              GO TO FNC-016
            ELSE
              SUBTRACT 1 FROM SUB-1
              MOVE "," TO WS-F-C (SUB-1).
           IF WS-F-C (SUB-1) NOT = " " AND NOT = "0" AND NOT = "1"
           AND NOT = "2" AND NOT = "3" AND NOT = "4" AND NOT = "5"
           AND NOT = "6" AND NOT = "7" AND NOT = "8" AND NOT = "9"
                         AND NOT = ","
                 MOVE 1 TO SIGN-FOUND
                 GO TO FNC-900.
           IF SUB-1 < 25
              ADD 1 TO SUB-1
              GO TO FNC-015.
       FNC-016.
           MOVE WS-FAX-CHECK TO FAX-PHONENUMBER.
           SUBTRACT 1 FROM SUB-1.
           MOVE SUB-1        TO FAX-CBPHONENUMBER.
           GO TO FNC-999.
       FNC-020.
           MOVE SUB-1 TO SUB-5.
       FNC-021.
           MOVE " " TO WS-F-C (SUB-1).
           IF SUB-1 < 25
              ADD 1 TO SUB-1.
           IF WS-F-C (SUB-1) = " "
              MOVE SUB-5 TO SUB-1
              GO TO FNC-016.
           GO TO FNC-021.
       FNC-900.
           MOVE 2910 TO POS
           DISPLAY "THERE IS AN ERROR IN THE FAX PHONE NUMBER," AT POS
           MOVE 3010 TO POS
           DISPLAY "PRESS <RETURN> TO RE-ENTER A FAX NUMBER." AT POS
           MOVE 2865 TO POS
           ACCEPT WS-ACCEPT AT POS.
       FNC-905.
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 3010 TO POS
           DISPLAY WS-MESSAGE AT POS.
       FNC-910.
           MOVE 2910 TO POS
           DISPLAY "ENTER A NEW FAX NUMBER:" AT POS
           ADD 25 TO POS
           ACCEPT WS-FAX-NUMBER AT POS.
           IF WS-FAX-NUMBER = " "
               GO TO FNC-900.
           MOVE 2910 TO POS
           DISPLAY "                                           " AT POS
           GO TO FNC-010.
       FNC-999.
           EXIT.
      *
       READ-DELIVERY-FILE SECTION.
       RDELIV-000.
            MOVE 1 TO SUB-1
                      PA-RECORD.
            MOVE 3 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RDELIV-010.
            READ PARAMETER-FILE NEXT
                AT END NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 10
                GO TO RDELIV-999.
            IF PA-TYPE < 3
               GO TO RDELIV-010.
            IF PA-TYPE > 3
                GO TO RDELIV-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER DELV BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RDELIV-010.
            IF PARAMETER-REC = "           "
               GO TO RDELIV-010.           
            MOVE PARAMETER-REC TO WS-DEL-OCCUR (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 = 11
               MOVE 1 TO SUB-1
               GO TO RDELIV-999.
            GO TO RDELIV-010.
       RDELIV-999.
            EXIT.
      *
       READ-PARAMETER SECTION.
       RP-005.
            MOVE 0 TO PA-TYPE.
            MOVE 1 TO PA-RECORD.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RP-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
                MOVE " " TO PARAMETER-REC
                GO TO RP-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "SLPARAMETER FILE BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RP-010.
       RP-999.
            EXIT.
      *
       READ-SUPPLIER SECTION.
       RCR-000.
           MOVE WS-SUPPLIER TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-KEY
             INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE "UNKNOWN" TO CR-NAME
                GO TO RCR-999.
       RCR-010.
           READ CREDITOR-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN" TO CR-NAME
                MOVE 0 TO WS-CREDITOR-ST1
                GO TO RCR-999.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITOR RECORD BUSY ON READ, 'ESC' TO RETRY" 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO RCR-010.
       RCR-020.
           MOVE 1238 TO POS.
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
           MOVE 1610 TO POS.
           DISPLAY "SUPPLIER   : " AT POS
           ADD 13 TO POS
           DISPLAY CR-NAME AT POS.

           MOVE " " TO WS-MESSAGE.
           MOVE 1410 TO POS.
           DISPLAY WS-MESSAGE AT POS.
      *     MOVE 1510 TO POS.
      *      DISPLAY WS-MESSAGE AT POS.
           MOVE 1510 TO POS.
           DISPLAY "FAX NUMBER : [                    ]" AT POS
           ADD 14 TO POS
           DISPLAY CR-FAX AT POS.
           MOVE CR-FAX TO WS-FAX-NUMBER.
           
      *     PERFORM ERROR-MESSAGE.
       RCR-999.
             EXIT.
      *
       READ-CREDITOR-SHORT-NAME SECTION.
       RCSN-000.
           MOVE WS-SUPPLIER-ACCEPT TO CR-NAME.
           START CREDITOR-MASTER KEY NOT < CR-NAME
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 NOT = 0
              MOVE "START ERROR, CREDITOR READ-NEXT, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE "UNKNOWN" TO CR-NAME
                GO TO RCSN-999.
       RCSN-010.
           READ CREDITOR-MASTER NEXT
                AT END NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 10
                MOVE "UNKNOWN" TO CR-NAME
                MOVE 0 TO WS-CREDITOR-ST1
                GO TO RCSN-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE "CREDITORS BUSY ON READ BY NAME, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CREDITOR-ST1
                GO TO RCSN-010.
           MOVE CR-ACCOUNT-NUMBER TO WS-SUPPLIER-ACCEPT.
       RCSN-020.
           MOVE 1238 TO POS.
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
           MOVE 1610 TO POS.
           DISPLAY "SUPPLIER   : " AT POS
           ADD 13 TO POS
           DISPLAY CR-NAME AT POS.
           MOVE 1410 TO POS.
           DISPLAY "Press <Return> to ACCEPT This Account,    " AT POS.
           MOVE 1515 TO POS.
           DISPLAY "OR Press 'PgDn' To Scroll Through More Accounts "
              AT POS.
           MOVE 1650 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

           IF W-ESCAPE-KEY = 1 OR = 6
               GO TO RCSN-900.
           IF W-ESCAPE-KEY = 7
               MOVE " " TO WS-SUPPLIER-ACCEPT
               GO TO RCSN-010.
           GO TO RCSN-020.
       RCSN-900.
           MOVE 1238 TO POS.
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
           MOVE 1610 TO POS.
           DISPLAY "SUPPLIER   : " AT POS
           ADD 13 TO POS
           DISPLAY CR-NAME AT POS.

           MOVE " " TO WS-MESSAGE.
           MOVE 1410 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 1510 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 1510 TO POS.
           DISPLAY " FAX NUMBER: [                    ]"
            AT POS
           ADD 14 TO POS
           DISPLAY CR-FAX AT POS.
           MOVE CR-FAX TO WS-FAX-NUMBER.
           
      *     PERFORM ERROR-MESSAGE.
       RCSN-999.
           EXIT.
      *
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
            IF SUB-1 > 11
               MOVE 0 TO SUB-1
               GO TO Z1-52.
               
            MOVE SUB-1         TO WS-O-LINE
            MOVE "SuppLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-51.
       Z1-52.
            ADD 1              TO SUB-1
            IF SUB-1 > 09
               MOVE 0 TO SUB-1
               GO TO Z1-54.
               
            MOVE SUB-1         TO WS-O-LINE
            MOVE "POrdLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-52.
       Z1-54.
            ADD 1              TO SUB-1
            IF SUB-1 > 43
               MOVE 0 TO SUB-1
               GO TO Z1-100.
            MOVE SUB-1         TO WS-O-LINE
            MOVE "BodyLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-54.
       Z1-100.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-001.
            MOVE 2910 TO POS
            DISPLAY "OPENING DATA-FILES ........" AT POS.
       OPEN-005.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-010.
            OPEN I-O PARAMETER-FILE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "SLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-010.
       OPEN-012.
            MOVE ALL "X" TO STORE-DEL.
            PERFORM READ-DELIVERY-FILE.
            CLOSE PARAMETER-FILE.
            PERFORM OPEN-010.
            PERFORM READ-PARAMETER.
            GO TO OPEN-025.
       OPEN-020.
            OPEN I-O OUTSTANDING-ORDERS.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO WS-OUTORD-ST1
               MOVE "S-ORDERS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-020.
       OPEN-025.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "SUPPLIERS FILE BUSY ON OPEN, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-025.
       OPEN-055.
           OPEN I-O Fax-Parameter.
           IF WS-Fax-ST1 NOT = 0
               MOVE "CoFaxParameter BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-COFAXPARAM TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-FAX-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-055.
       OPEN-056.
           MOVE 1 TO FAX-PAKEY.
           START FAX-PARAMETER KEY NOT < FAX-PAKEY.
           READ FAX-PARAMETER
              INVALID KEY NEXT SENTENCE.
           IF WS-FAX-ST1 NOT = 0
               MOVE "CO-FAXPARAMETER BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               MOVE 0 TO WS-FAX-ST1
               PERFORM ERROR-MESSAGE
              GO TO OPEN-056.
           CLOSE FAX-PARAMETER.
       OPEN-065.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO SO1-DATE SOE1-DATE.
           PERFORM ERROR1-020.
        OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER
                 PARAMETER-FILE
                 CREDITOR-MASTER
                 OUTSTANDING-ORDERS.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "GetSystemY2KDate".
       Copy "GetUserMailName".
       Copy "GetReportY2KDate".
       Copy "PrintReportInfo".
       Copy "PrepareFaxSending".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
       Copy "SetupPOrderForPDF".
       Copy "SetupPOrder2ForPDF".
       Copy "SetupMergePOrderForPDF".
       Copy "SetupPOrderForPDFMgEMail".
       Copy "SetupPOrderForPDFEmailOnly".
       Copy "CheckEmailForValidity".
       Copy "MoveEmailRecordFromEimage".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
       Copy "WriteDailyExcep1".
      *
      * END-OF-JOB
