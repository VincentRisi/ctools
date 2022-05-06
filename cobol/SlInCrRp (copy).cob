        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlInCrRp.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        REPOSITORY. 
           FUNCTION ALL INTRINSIC.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
    
        SPECIAL-NAMES.
        CLASS WS-VALID-EMAIL IS
          '@' '_' '.' '-'
          'a' THRU 'i'
          'j' THRU 'r'
          's' THRU 'z'
          '0' THRU '9'.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectStTrans".
         Copy "SelectSlRegister".
         Copy "SelectSlParameter".
         Copy "SelectCrCurrency".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PRINTFILE-STATUS.
           SELECT PDFPRINT-FILE ASSIGN TO WS-PDFPRINTER
                ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PRINTFILE-STATUS.
           SELECT LASER-FILE ASSIGN TO W-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LASERFILE-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdStTrans.
           COPY ChlfdRegister.
           COPY ChlfdParam.
           COPY ChlfdCrCurr.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       FD  PDFPRINT-FILE.
       01  PDFPRINT-REC.
           03  FILLER           PIC X(132).
      *
       FD  LASER-FILE.
       01  LASER-REC.
           03  FILLER           PIC X(140).
      *
       WORKING-STORAGE SECTION.
       77  WS-FOUND             PIC X VALUE " ".
       77  WS-PDFFILE-OPENED    PIC X VALUE " ".
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-INVCRED           PIC X VALUE " ".
       77  WS-PROF-TYPE         PIC X VALUE " ".
       77  WS-ADD-TOGETHER      PIC X VALUE " ".
       77  Ws-EnterOption       PIC X VALUE " ".
       77  WS-PRINT-NUM         PIC 9.
       77  WS-PRINT-Y-N         PIC X VALUE "Y".
       77  WS-TYPE-OF-DOCUMENT  PIC 9.
       77  WS-NUMBER            PIC 9(6) VALUE 0.
       77  WS-RANGE1            PIC 9(6) VALUE 0.
       77  WS-RANGE2            PIC 9(6) VALUE 0.
       77  WS-RANGE3            PIC 9(6) VALUE 0.
       77  WS-RANGE4            PIC 9(6) VALUE 0.
       77  WS-CURRENCY          PIC X(5) VALUE " ".
       77  WS-CURRENCY-SAVE     PIC X(5) VALUE " ".
       77  WS-CURRENCY-TEMP     PIC S9(2)V99999 VALUE 0.
       77  WS-EXCHANGERATE      PIC 9(3)V9(5) VALUE 0.
       77  WS-EXCHANGE-DIS      PIC Z(2)9.99999.
       77  WS-TEMPDATE          PIC 99/99/9999.
       77  WS-SUBTOTAL          PIC 9(7)V99 VALUE 0.
       77  WS-ADDONAMT          PIC 9(7)V99 VALUE 0.
       77  WS-TAXAMT            PIC 9(7)V99 VALUE 0.
       77  WS-INVOICETOTAL      PIC 9(7)V99 VALUE 0.
       77  WS-PAGE              PIC 99 VALUE 0.
       77  WS-CO-VATNO          PIC X(15) VALUE " ".
       77  WS-SPEC-COMMENT      PIC X(60) VALUE " ".
       77  WS-STTRANSNO         PIC 9(6).
       77  WS-COMPLETE          PIC X VALUE " ".
       77  WS-BO-FOUND          PIC X VALUE " ".
       77  WS-BO-QTY            PIC S9(5) VALUE 0.
       77  WS-TRANS-DISPLAY     PIC Z(5)9.
       77  WS-EMAIL-NUMBER      PIC X(50) VALUE " ".
       77  PSW-SUB1             PIC S9(5)9.
       77  PSW-SUB2             PIC S9(5)9.
       77  WS-ACC-ERROR         PIC X VALUE " ".      
       01  WS-EMAIL               PIC X(50).
       01  WS-TEMP-EMAIL-FILE     PIC X(50).
       01  WS-SPACE-CNT           PIC 9(2) VALUE ZEROES.
       01  W-READ-KEY             PIC X(20).
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1     PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1        PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-PRINTFILE-STATUS.
           03  WS-PF-ST1          PIC 99.
       01  WS-LASERFILE-STATUS.
           03  WS-LF-ST1          PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1      PIC 99.
       01  WS-CURRENCY-STATUS.
           03  WS-CURRENCY-ST1    PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X VALUE " ".
           03  SP-REST          PIC X(14) VALUE " ".
       01  WS-BO-REDUCED-MESSAGE.
           03  FILLER           PIC X(7) VALUE " ".
           03  WS-BO-MESSAGE    PIC X(38) VALUE " ".
           03  WS-BO-INVOICE    PIC Z(5)9 BLANK WHEN ZERO.
           03  FILLER           PIC X VALUE " ".
           03  WS-BO-DATE       PIC 99/99/9999.
           03  FILLER           PIC X(72) VALUE " ".
       01  BODY-FIELDS.
           03  BODY-LINE.
               05  B-STOCKNUMBER.
                   07  B-1ST-CHAR      PIC X.
                   07  B-REST          PIC X(14).
               05  B-ORDERQTY          PIC 9(5).
               05  B-SHIPQTY           PIC 9(5).
               05  B-SHIPPEDQTY        PIC 9(5).
               05  B-STOCKDESCRIPTION  PIC X(20).
               05  B-STOCKDESCRIPTION2 PIC X(20).
               05  B-STOCKPRICE        PIC 9(6)V99.
               05  B-STOCKCOST         PIC 9(6)V99.
               05  B-DISCOUNTPERITEM   PIC 9(4)V99.
               05  B-NETT              PIC 9(7)V99.
               05  B-BORDER            PIC X.
               05  B-UNIT              PIC X(4).
       01  COMM-LINES.
           03  C-LINE.
               05  C-NUM.
                   07  C-1STCHAR      PIC X.
                   07  C-NUMREST      PIC X(14).
               05  C-ORDER            PIC X(5).
               05  C-SHIP             PIC X(5).
               05  C-DESC             PIC X(20).
               05  C-UNIT             PIC X(4).
               05  C-PRICE            PIC X(9).
               05  C-COST             PIC X(9).
               05  C-DISC             PIC X(5).
               05  C-REST             PIC X(5).
       01  COMM-CR-LINES.
           03  C-CR-LINE.
               05  C-CR-NUM.
                   07  C-CR-1STCHAR      PIC X.
                   07  C-CR-NUMREST      PIC X(14).
               05  C-CR-SHIP             PIC X(5).
               05  C-CR-DESC             PIC X(20).
               05  C-CR-PRICE            PIC X(8).
               05  C-CR-COST             PIC X(8).
               05  C-CR-DISC             PIC X(5).
               05  C-CR-REST             PIC X(10).
       01  WS-TIME-DISPLAY.
           03  SPLIT-TIME-FIL     PIC X(6) VALUE "Time:".
           03  SPLIT-HR           PIC 99.
           03  SPLIT-HR-FIL       PIC X.
           03  SPLIT-MN           PIC 99.
           03  SPLIT-MN-FIL       PIC X.
           03  SPLIT-SC           PIC 99.
       01  WS-TIMES.
           03  WS-HRS           PIC 99.
           03  WS-MINS          PIC 99.
           03  WS-SECS          PIC 99.
           03  WS-100S          PIC 99.
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
       01  PCREDITLINE.
           03  FILLER           PIC X(50) VALUE " ".
           03  P-XES            PIC X(20) VALUE " ".
           03  FILLER           PIC X(8) VALUE " ".
           03  P-NAME           PIC X(54).
       01  PLINE1.
           03  FILLER           PIC X(13) VALUE " ".
           03  P-GSTNO          PIC X(18) VALUE " ".
           03  P-ACCNO          PIC X(7).
           03  FILLER           PIC X(11) VALUE " ".
           03  P-TYPE           PIC X(20) VALUE " ".
           03  FILLER           PIC X(11) VALUE " ".
           03  P-ADDNAME        PIC X(54).
       01  PLINE2.
           03  FILLER           PIC X(13) VALUE " ".
           03  P-ADD.
               05  P-ADDRESS    PIC X(65) VALUE " ".
           03  SUPP-ADD.
               05  SUPP-DIG10   PIC X(7) VALUE " ".
               05  SUPP-DIG30   PIC X(23) VALUE " ".
               05  SUPP-TIME    PIC X(24).
       01  PLINE4.
           03  FILLER           PIC X(2) VALUE " ".
           03  P-TERMS          PIC X(11) VALUE " ".
           03  FILLER           PIC X(2) VALUE " ".
           03  P-PO             PIC X(20) VALUE " ".
           03  FILLER           PIC X(2) VALUE " ".
           03  P-SOLD           PIC X(2) VALUE " ".
           03  FILLER           PIC X(3) VALUE " ".
           03  P-VIA            PIC X(20) VALUE " ".
           03  FILLER           PIC X(2) VALUE " ".
           03  P-BIN            PIC X(6) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  P-SOLDBY         PIC X(2) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  P-ORDERDATE      PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  P-SLIP           PIC Z(5)9.
           03  FILLER           PIC X(1) VALUE " ".
           03  P-DATE           PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
           03  P-INV            PIC Z(5)9.
           03  FILLER           PIC X(5) VALUE " ".
           03  P-PAGE           PIC Z9.
           03  FILLER           PIC X(15) VALUE " ".
       01  PDET.
           03  FILLER             PIC X(2).
           03  P-NO               PIC Z(2)9.
           03  FILLER             PIC X(1).
           03  PDET-REST.
               05  P-STOCK          PIC X(15).
               05  FILLER           PIC X(1).
               05  P-DESC           PIC X(20).
               05  P-DESC2          PIC X(21).
               05  P-UNIT           PIC X(4).
               05  P-ORDER          PIC Z(4)9.
               05  FILLER           PIC X(1).
               05  P-SHIP           PIC Z(4)9.
               05  FILLER           PIC X(1).
               05  P-BO             PIC Z(4)9.
               05  FILLER           PIC X(1).
               05  P-SHIPPED        PIC Z(4)9.
               05  FILLER           PIC X(1).
               05  P-PRICE          PIC Z(5)9.99.
               05  FILLER           PIC X(2).
               05  P-DISCOUNT       PIC Z9.99.
               05  FILLER           PIC X(1).
               05  P-NETT           PIC Z(7)9.99.
               05  FILLER           PIC X(13).
       01  P-COMMENTLINE.
           03  FILLER           PIC X(11) VALUE " ".
           03  P-BO-MESSAGE     PIC X(38) VALUE " ".
           03  P-REST-OF-LINE.
               05  FILLER       PIC X(5) VALUE " ".
               05  P-DIG1       PIC X.
               05  P-COMM       PIC X(33) VALUE " ".
               05  P-DIG2       PIC X.
               05  FILLER       PIC X(42) VALUE " ".
       01  P-ADDLINE.
           03  FILLER           PIC X(11) VALUE " ".
           03  P-PHONE          PIC X(21) VALUE " ".
           03  P-ADD1           PIC Z(6)9.99. 
           03  FILLER           PIC X(14) VALUE " ".
           03  P-ADD2           PIC Z(6)9.99.
           03  FILLER           PIC X(13) VALUE " ".
           03  P-ADD3           PIC Z(6)9.99.
           03  FILLER           PIC X(13) VALUE " ".
           03  P-CURRENCY       PIC X(5) VALUE " ".
           03  P-ADD4           PIC Z(6)9.99.
           03  FILLER           PIC X(14) VALUE " ".
       01  P-CONTINUED.
           03  FILLER           PIC X(40) VALUE " ".
           03  FILLER           PIC X(22) VALUE "Continued To.....Page".
           03  P-CONT-PAGE      PIC 9.
           03  FILLER           PIC X(63) VALUE " ".
       01  WS-EMAIL-INVOICE.
           03  WS-EI-FIL        PIC X(15) VALUE "/ctools/einvoc/".
           03  WS-EINVOICE      PIC X(6).
       01  WS-EMAIL-CREDIT.
           03  WS-EC-FIL        PIC X(15) VALUE "/ctools/ecredt/".
           03  WS-ECREDIT       PIC X(6).
       01  WS-EMAIL-FINAL.
           03  WS-EF-FIL        PIC X(15) VALUE " ".
           03  WS-BAL-OF-NAME   PIC X(35).
       01 WS-FST-LINE.
          05  WS-DELIM-F             PIC  X(2).
          05  WS-DATA-F              PIC  X(132).
          05  WS-DELIM-END1          PIC  X(1).
       01 WS-OTH-LINE-1.
          05  WS-O-L                 PIC  X(8).
          05  WS-O-LINE              PIC  99.
          05  FILLER                 PIC  X(125).
       01 WS-OTH-LINE.
          05  WS-DELIM-O             PIC  X.
          05  WS-DATA-O              PIC  X(133).
          05  WS-DELIM-END2          PIC  X(1). 
       01  LASER-PCREDITLINE.
           03  PLCR-CHAR1       PIC X(2).
           03  FILLER           PIC X(2) VALUE " ".
           03  PL-TYPE          PIC X(22) VALUE " ".
           03  FILLER           PIC X(14) VALUE " ".
           03  PL-NAME          PIC X(94).
           03  PLCR-CHAR2       PIC X.
       01  LASER-PLINE1.
           03  PL1-CHAR         PIC X(2) VALUE " ".
           03  FILLER           PIC X(6) VALUE " ".
           03  PL-GSTNO         PIC X(23) VALUE " ".
           03  PL-ACCNO         PIC X(7).
           03  FILLER           PIC X(51) VALUE " ".
           03  PL-ADDNAME       PIC X(45).
           03  PL1-2            PIC X(1) VALUE " ".
       01  LASER-PLINE2.
           03  PL2-CHAR          PIC X(2) VALUE " ".
           03  FILLER            PIC X(3) VALUE " ".
           03  PL-ADD.
               05  PL-ADDRESS    PIC X(47) VALUE " ".
               05  SUPPL-TIME    PIC X(21).
               05  PL-PULLBY     PIC X(9) VALUE " ".
               05  PL-AREA       PIC X(7).
           03  SUPPL-ADD.
               05  SUPPL-DIG10   PIC X(13) VALUE " ".
               05  SUPPL-DIG30   PIC X(32) VALUE " ".
           03  PL2-2             PIC X(1) VALUE " ".
       01  LASER-PLINE4.
           03  PL4-CHAR          PIC X(2) VALUE " ".
           03  PL-TERMS          PIC X(11) VALUE " ".
           03  FILLER            PIC X(3) VALUE " ".
           03  PL-PO             PIC X(25) VALUE " ".
           03  FILLER            PIC X(1) VALUE " ".
           03  PL-SOLD           PIC X(2) VALUE " ".
           03  FILLER            PIC X(3) VALUE " ".
           03  PL-VIA            PIC X(20) VALUE " ".
           03  FILLER            PIC X(6) VALUE " ".
           03  PL-BIN            PIC X(7) VALUE " ".
           03  FILLER            PIC X(1) VALUE " ".
           03  PL-SOLDBY         PIC X(2) VALUE " ".
           03  FILLER            PIC X(3) VALUE " ".
           03  PL-ORDERDATE      PIC X(10).
           03  FILLER            PIC X(2) VALUE " ".
           03  PL-SLIP           PIC Z(5)9.
           03  PL-SLIP-SLASH     PIC X.
           03  PL-SLIP-COPY      PIC 99.
           03  FILLER            PIC X(3) VALUE " ".
           03  PL-DATE           PIC X(10).
           03  FILLER            PIC X(4) VALUE " ".
           03  PL-INV            PIC Z(5)9.
           03  FILLER            PIC X(2) VALUE " ".
           03  PL-PAGE           PIC Z9.
           03  PL4-2             PIC X(1) VALUE " ".
       01  LASER-PDET.
           03  PLDET-CHAR            PIC X(2) VALUE " ".
           03  PL-NO                 PIC Z(2)9.
           03  FILLER                PIC X(1).
           03  PL-STOCK              PIC X(15).
           03  PLDET-REST.
               05  FILLER            PIC X(5).
               05  PL-DESC           PIC X(20).
               05  PL-DESC2          PIC X(25).
               05  PL-UNIT           PIC X(6).
               05  PL-ORDER          PIC Z(4)9.
               05  FILLER            PIC X(2).
               05  PL-SHIP           PIC Z(4)9.
               05  FILLER            PIC X(2).
               05  PL-BO             PIC Z(4)9.
               05  FILLER            PIC X(2).
               05  PL-SHIPPED        PIC Z(4)9.
               05  FILLER            PIC X(3).
               05  PL-PRICE          PIC Z(5)9.99.
               05  FILLER            PIC X(2).
               05  PL-DISCOUNT       PIC Z9.99.
               05  FILLER            PIC X(2).
               05  PL-NETT           PIC Z(6)9.99.
           03  PLDET-CHAR2           PIC X(1) VALUE " ".
       01  LASERPL-COMMENTLINE.
           03  PLCOM-CHAR        PIC X(2) VALUE " ".
           03  FILLER            PIC X(11) VALUE " ".
           03  PL-BO-MESSAGE     PIC X(38) VALUE " ".
           03  PL-REST-OF-LINE.
               05  FILLER        PIC X(6).
               05  PL-COMM       PIC X(21) VALUE " ".
               05  FILLER        PIC X(56).
           03  PLCOM-CHAR2       PIC X(1) VALUE " ".
       01  LASERPL-ADDLINE.
           03  PLADD-CHAR        PIC X(2) VALUE " ".
           03  FILLER            PIC X(11) VALUE " ".
           03  PL-PHONE          PIC X(28) VALUE " ".
           03  PL-ADD1           PIC Z(6)9.99. 
           03  FILLER            PIC X(16) VALUE " ".
           03  PL-ADD2           PIC Z(6)9.99.
           03  FILLER            PIC X(17) VALUE " ".
           03  PL-ADD3           PIC Z(6)9.99.
           03  FILLER            PIC X(15) VALUE " ".
           03  PL-CURRENCY       PIC X(5) VALUE " ".
           03  PL-ADD4           PIC Z(6)9.99.
           03  PLADD-CHAR2       PIC X(1) VALUE " ".
       01  PL-CONTINUED.
           03  PLCONT-CHAR     PIC X(2) VALUE " ".
           03  FILLER          PIC X(38) VALUE " ".
           03  PL-CONT-NAME    PIC X(22) VALUE " ".
           03  PL-CONT-PAGE    PIC 9.
           03  FILLER          PIC X(71) VALUE " ".
           03  PLCONT-CHAR2    PIC X(1) VALUE " ".
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
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-DATA-FILES
           PERFORM CLEAR-SCREEN.
       CONT-010.
           PERFORM DISPLAY-FORM
           PERFORM GET-DATA
           PERFORM READ-COMM-FILE
           PERFORM READ-PARAMETER.
           MOVE 2820 TO POS
           DISPLAY "PRINTING IN PROGRESS......." AT POS.
       CONT-030.
           MOVE 1 TO SUB-1.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
       CONT-031.
      *D=DOCUBASE ASCII TEXT,
      *X=NEW PDF FORMAT PRINT
           IF WS-INVCRED = "D" OR = "X"
               GO TO CONT-032.
      *1=MASTER PRINTER - DOT MATRIX
           IF WS-PRINT-NUM = 1
             IF WS-PrinterNumber (sub-1) = 1
               MOVE Ws-PrinterName (Sub-1) TO WS-PRINTER
               Move Ws-PrinterChars (Sub-1) To Ws-Print-Chars
               GO TO CONT-035.
      *         GO TO CONT-032.
      *2=COUNTER PRINTER - DOT MATRIX
           IF WS-PRINT-NUM = 2
            IF WS-PrinterNumber (sub-1) = 7
               MOVE Ws-PrinterName (Sub-1) TO WS-PRINTER
               Move Ws-PrinterChars (Sub-1) To Ws-Print-Chars
               GO TO CONT-035.
      *3=STORES PRINTER - DOT MATRIX
           IF WS-PRINT-NUM = 3
            IF WS-PrinterNumber (sub-1) = 4
               MOVE Ws-PrinterName (Sub-1) TO WS-PRINTER
               Move Ws-PrinterChars (Sub-1) To Ws-Print-Chars
               GO TO CONT-035.
               
      *4=LASER PRINTER
           IF WS-PRINT-NUM = 4
               GO TO CONT-032.
      *5=WRITE EMAIL RECORD
           IF WS-PRINT-NUM = 5
               GO TO CONT-050.
               
           If Sub-1 < 11
             add 1 to Sub-1
             Go To CONT-031.
           Move "Can't Find a PrinterNumber" To Ws-Message
           Perform Error-Message.
           PERFORM END-OFF.
       CONT-032.
           IF WS-INVCRED NOT = "D" AND NOT = "X"
               PERFORM WORK-OUT-PRINT-FILE-NAME
               GO TO CONT-035.
      *     IF WS-INVCRED = "I" OR = "P" OR = "Q"
      *         MOVE "/ctools/spl/InPrintCo" TO WS-PRINTER
      *     ELSE
      *         MOVE "/ctools/spl/CrPrintCo" TO WS-PRINTER.
           IF WS-INVCRED = "D"
               MOVE "/ctools/spl/DBPrintCo" TO WS-PRINTER
               MOVE WS-PRINTER   TO ALPHA-RATE
               MOVE 22           TO SUB-1
               MOVE WS-CO-NUMBER TO WS-COMPANY-DIGITS
               MOVE WS-CO-DIG1   TO AL-RATE (22)
               MOVE WS-CO-DIG2   TO AL-RATE (23)
               MOVE ALPHA-RATE   TO WS-PRINTER W-FILENAME
               OPEN OUTPUT PRINT-FILE
               GO TO CONT-036.
           IF WS-INVCRED = "X"
               GO TO CONT-036.
       CONT-035.
           IF WS-PRINT-NUM NOT = 4 AND NOT = 5
               PERFORM GET-USER-PRINT-NAME
               OPEN OUTPUT PRINT-FILE.
           IF WS-PRINT-NUM NOT = 4 AND NOT = 5
            IF WS-PF-ST1 NOT = 0
               MOVE 
           "PRINT FILE LOCKED BY ANOTHER TERMINAL, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO CONT-035.
           IF WS-PRINT-NUM = 4
               OPEN OUTPUT LASER-FILE
            IF WS-LF-ST1 NOT = 0
               MOVE 
           "LASER FILE LOCKED BY ANOTHER TERMINAL, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO CONT-035.
                
           IF WS-INVCRED = "D" OR = "X"
               GO TO CONT-036.
           IF WS-PRINT-NUM = 4
              PERFORM ZL1-LASER-HEADINGS.
                
           IF WS-INVCRED = "P"
               MOVE WS-RANGE1 TO WS-NUMBER.
       CONT-036.
           PERFORM READ-DATA.
           IF WS-FOUND = " "
            IF WS-INVCRED NOT = "D" AND NOT = "X"
               MOVE "NOTHING TO PRINT IN THAT RANGE!!!" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
           IF WS-INVCRED = "D" OR = "X"
               CLOSE PRINT-FILE
               PERFORM END-OFF.
       CONT-038.
           IF WS-INVCRED NOT = "X"
            IF WS-PRINT-NUM NOT = 4
               CLOSE PRINT-FILE
            ELSE
               CLOSE LASER-FILE. 
       CONT-040.
      * WS-PRINT-NUM = 5.  EMAIL PRINTOUT  SEE CONT-036 NEW ENTRY
      *     IF WS-INVCRED = "D"
      *        PERFORM END-OFF.
      *     IF WS-PRINT-NUM = 1 OR = 4
      *     IF WS-PRINT-NUM = 1
      *      IF WS-FOUND NOT = " "
      *        PERFORM CHECK-SPOOLER
      *        PERFORM END-OFF.
           IF WS-PRINT-NUM = 1 OR = 2 OR = 3
              PERFORM SEND-REPORT-TO-PRINTER
              PERFORM END-OFF.
           MOVE 1 TO SUB-1.
       CONT-041.
      * ALL NOW WS-PRINT-NUM = 4.    LASER INVOICE / CREDIT PRINTING
           IF WS-PRINT-NUM = 4
            IF WS-PrinterNumber (SUB-1) NOT = 15
                ADD 1 TO SUB-1 
                GO TO CONT-041.
           MOVE Ws-PrinterName (SUB-1) TO WS-PRINTER-SAVE.
           MOVE 1 TO SUB-1.
                
           IF WS-PRINT-NUM = 4
            IF WS-INVCRED = "I" OR = "P"
               PERFORM SETUP-INVOICE-FOR-PDF
            ELSE
               PERFORM SETUP-CREDIT-FOR-PDF.

           PERFORM END-OFF.
           
           GO TO CONT-999.
       CONT-050.
      * EMAIL SECTION ONLY.
           IF WS-INVCRED = "I"
               MOVE WS-RANGE1          TO WS-EINVOICE
               PERFORM GET-EMAIL-INVOICE-NAME
               MOVE WS-TEMP-EMAIL-FILE TO WS-PRINTER W-FILENAME.

           IF WS-INVCRED = "P"
               MOVE "/ctools/eprofo/"  TO WS-EI-FIL
               MOVE WS-RANGE1          TO WS-EINVOICE
               PERFORM GET-EMAIL-INVOICE-NAME
               MOVE WS-TEMP-EMAIL-FILE TO WS-PRINTER W-FILENAME.
               
           IF WS-INVCRED = "C"
               MOVE WS-RANGE1          TO WS-ECREDIT
               PERFORM GET-EMAIL-INVOICE-NAME
               MOVE WS-TEMP-EMAIL-FILE TO WS-PRINTER W-FILENAME.
               
           OPEN OUTPUT LASER-FILE.
           PERFORM ZE1-EMAIL-HEADINGS.
               
           PERFORM READ-DATA.

           IF WS-FOUND = " "
            IF WS-PRINT-NUM = 5
             IF WS-INVCRED = "C" 
               PERFORM DELETE-BLANK-EMAIL-CRN-RECORD
             ELSE
               PERFORM DELETE-BLANK-EMAIL-INV-RECORD.
           IF WS-FOUND = " "
            IF WS-PRINT-NUM = 5
               MOVE "NOTHING TO PRINT IN THAT RANGE, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
           IF WS-FOUND NOT = " "
            IF WS-PRINT-NUM = 5
               PERFORM MOVE-EMAIL-FROM-EIMAGE-SETUP
               PERFORM MOVE-EMAIL-RECORD-FROM-EIMAGE.
           PERFORM END-OFF.
       CONT-999.
           EXIT.
      *
       MOVE-EMAIL-FROM-EIMAGE-SETUP SECTION.
       MERFES-005.
             MOVE WS-TEMP-EMAIL-FILE TO WS-EMAIL-FINAL.
             
             IF WS-INVCRED = "I"
                 MOVE "/ctools/einvoc/" TO WS-EF-FIL.
             IF WS-INVCRED = "P"
                 MOVE "/ctools/eprofo/" TO WS-EF-FIL.
             IF WS-INVCRED = "C"
                 MOVE "/ctools/ecredt/" TO WS-EF-FIL.
       MERFES-999.
            EXIT.
      *
       GET-EMAIL-INVOICE-NAME SECTION.
       GEQN-006.
            MOVE SPACES TO ALPHA-RATE DATA-RATE.

            MOVE "/ctools/eimage/" TO ALPHA-RATE.


      *      IF WS-INVCRED = "C"
      *         MOVE "/ctools/ecredt/" TO ALPHA-RATE.
      *      IF WS-INVCRED = "P"
      *         MOVE "/ctools/eprofo/" TO ALPHA-RATE.
      *      IF WS-INVCRED = "I"
      *         MOVE "/ctools/einvoc/" TO ALPHA-RATE.
            
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
       WORK-OUT-PRINT-FILE-NAME SECTION.
       WOPFN-001.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
           ACCEPT WS-USERNAME FROM ENVIRONMENT "USER".
           
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
      *     MOVE "IN WOPFN-020." TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

           MOVE SPACES TO DATA-RATE.
           IF WS-INVCRED = "I" OR = "P"
               MOVE "InPrintCo" TO DATA-RATE
           ELSE
               MOVE "CrPrintCo" TO DATA-RATE.
           MOVE 1  TO SUB-2.
       WOPFN-025.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
               GO TO WOPFN-025.
       WOPFN-030.
           MOVE SPACES TO DATA-RATE.
           MOVE WS-CO-NUMBER TO DATA-RATE.
  
           MOVE 1  TO SUB-2.

      *     MOVE "IN WOPFN-030." TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       WOPFN-035.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
               GO TO WOPFN-035.
           MOVE ALPHA-RATE   TO WS-PRINTER W-FILENAME.
           
      *     MOVE WS-PRINTER TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

      *     MOVE W-FILENAME TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       WOPFN-999.
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
           IF INCR-TRANS = 1
              MOVE "I" TO AL-RATE (SUB-1)
           ELSE
              MOVE "C" TO AL-RATE (SUB-1).
           ADD 1 TO SUB-1.
       WOPDF-020.
      *     MOVE "IN WOPDF-020." TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

           MOVE SPACES     TO DATA-RATE.
           MOVE WS-INVOICE TO DATA-RATE.
           MOVE 1  TO SUB-2.
       WOPDF-025.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
               GO TO WOPDF-025.
       WOPDF-030.
           MOVE ALPHA-RATE   TO WS-PRINTER W-FILENAME.
           
      *     MOVE WS-PRINTER TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

      *     MOVE W-FILENAME TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       WOPDF-999.
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
           MOVE "PDFList.csv" TO DATA-RATE.
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
       READ-DATA SECTION.
       RD-000.
           MOVE " " TO WS-FOUND.
           MOVE "N" TO WS-COMPLETE.
           MOVE 0   TO INCR-INVOICE WS-DATE.
           MOVE 0   TO SUB-1 SUB-20.
       RD-010.
           IF WS-INVCRED = "I" OR = "C"
              PERFORM READ-INVOICE-REGISTER.
           IF WS-INVCRED = "P"
              PERFORM READ-REGISTER.
              
           IF WS-INVCRED = "X"
            IF WS-DATE = 0
              PERFORM GET-SYSTEM-Y2K-DATE
              MOVE 01 TO WS-DD
              MOVE WS-DATE TO INCR-DATE
              START INCR-REGISTER KEY NOT < INCR-DATE
                 INVALID KEY NEXT SENTENCE.
           IF WS-INVCRED = "X"
              PERFORM READ-PDF-REGISTER.
              
           IF WS-INVCRED = "D"
            IF WS-DATE = 0
              PERFORM GET-SYSTEM-Y2K-DATE
              MOVE 01 TO WS-DD
              MOVE WS-DATE TO INCR-DATE
              START INCR-REGISTER KEY NOT < INCR-DATE
                 INVALID KEY NEXT SENTENCE.
           IF WS-INVCRED = "D"
              PERFORM READ-DOCU-REGISTER.
              
      * PRINTING DONE OF ONE TRANSACTION, NOW TO SEE IF THERE ARE MORE
      * IF COMPLETE THEN GO TO RD-999.
      
      *     MOVE INCR-INVOICE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      
      * THIS SECTION TO TEST IF DOCU PRINT ("D") IS FINSHED
      * Y = COMPLETE AND E = ERROR INDATE SO FLAGGED COMPLETE
           IF WS-COMPLETE = "Y" OR = "E"
            IF WS-INVCRED = "X"
               CLOSE PRINT-FILE
               GO TO RD-999
            ELSE
               GO TO RD-999.

           IF WS-INVCRED = "X"
              PERFORM WORK-OUT-PDF-FILE-NAME
              OPEN OUTPUT LASER-FILE
              PERFORM ZL1-LASER-HEADINGS
              PERFORM PDF-PRINT-INVOICE
              MOVE INCR-INVOICE TO WS-INVOICE
              CLOSE LASER-FILE
              PERFORM WRITE-INDEX-KEY
            IF INCR-TRANS = 1
              PERFORM SETUP-INVOICE-FOR-PDF-ONLY
              GO TO RD-010
            ELSE
              PERFORM SETUP-CREDIT-FOR-PDF-ONLY
              GO TO RD-010.

           IF WS-PRINT-NUM NOT = 4 AND NOT = 5
              PERFORM PRINT.
           IF WS-PRINT-NUM = 4
              PERFORM LASER-PRINT-INVOICE.
           IF WS-PRINT-NUM = 5
              PERFORM EMAIL-PRINT-INVOICE.

           IF WS-PROG-TYPE = 3
               MOVE 1 TO WS-TYPE-OF-DOCUMENT.
           IF WS-PROG-TYPE = 2
               MOVE 2 TO WS-TYPE-OF-DOCUMENT.
           
           GO TO RD-010.
       RD-999.
           EXIT.
      *
       WRITE-INDEX-KEY SECTION.
       WIK-001.
            IF WS-PDFFILE-OPENED = "Y"
              GO TO WIK-005.
              PERFORM WORK-OUT-CSV-FILE-NAME
              OPEN OUTPUT PDFPRINT-FILE
              MOVE 
           "#GROUP NUM, TYPE, TRANS NUM, ACCOUNT NUM, ACCOUNT NAME, " &
           "PORDER NUM, DATE, TRANS TOTAL." 
                 TO PDFPRINT-REC
              WRITE PDFPRINT-REC AFTER 1.
           MOVE "Y" TO WS-PDFFILE-OPENED.
       WIK-005.
           MOVE WS-CO-NUMBER      TO PDF-GROUP-NUMBER.
           IF INCR-TRANS = 1
              MOVE "I"            TO PDF-TYPE
           ELSE
              MOVE "C"            TO PDF-TYPE.
           MOVE INCR-INVOICE      TO PDF-NUMBER.
           MOVE INCR-ACCOUNT      TO PDF-ACCOUNT
           MOVE INCR-NAME         TO PDF-NAME.
           MOVE INCR-PORDER       TO PDF-PORDER.
           MOVE DISPLAY-DATE      TO PDF-DATE.
      *     MOVE PL-ADD4           TO PDF-TOTAL.

      * SEE PL-ADD4 MOVE TO PDF-TOTAL IN LASER-PRINT SECTION
      *     MOVE INCR-INVCRED-AMT  TO PDF-TOTAL.
           
      *     MOVE PDFLIST-LINE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
           WRITE PDFPRINT-REC FROM PDFLIST-LINE AFTER 1.
       WIK-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "N" TO WS-ADD-TOGETHER.
            MOVE " " TO WS-INVCRED.
            MOVE "INVCRED" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-000.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-INVCRED.
            
      * NEW SECTION CHECKING PSWD DO THAT GENERAL USERS CAN PRINT
      * PRO-FORM INVOICES.  THIS ALLOWS US TO LOWER THE PSWD NEEDED
      * ON THIS PROGRAM SO TAHT ONLY A SPECIALPSWD ALLOWS ONE TO
      * PRINT INVOICES & C/NOTES.  01/01/2014
            IF WS-INVCRED = "I" OR = "C"
                MOVE X"55" TO F-EXIT-CH
                PERFORM CHECK-PASSWORD
             IF WS-PASSWORD-VALID = "N"
                MOVE "INVALID PASSWORD, 'EASC' TO RETRY." TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-000.
      
            IF WS-INVCRED = "I" OR = "C"
                GO TO GET-010.
            IF WS-INVCRED = "P"
                GO TO GET-005.
            IF WS-INVCRED = "D" OR = "X"
                MOVE 1 TO WS-PRINT-NUM WS-PROG-TYPE
                GO TO GET-900.
            MOVE
            "ENTER I=INVOICE, C=CREDIT, D=D/BASE, P=PROFORMA, " &
            "X=PDF M/END PRINT."
             TO WS-MESSAGE
            PERFORM ERROR-MESSAGE
            GO TO GET-000.
       GET-005.
            MOVE " "        TO WS-PROF-TYPE
            MOVE "PROFTYPE" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-000.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-PROF-TYPE.
            IF WS-PROF-TYPE = "Q" OR = "P" OR = "R"
                GO TO GET-007.
            MOVE "ENTER Q=QUOTE, P=P/SLIP, R=REPAIR, 'ESC' TO RE-ENTER."
             TO WS-MESSAGE
            PERFORM ERROR-MESSAGE
            GO TO GET-005.
       GET-007.
            MOVE " "     TO WS-ADD-TOGETHER
            MOVE "ADDTO" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-005.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ADD-TOGETHER.
            IF WS-ADD-TOGETHER = "N" OR = "Y"
                GO TO GET-010.
            MOVE "ENTER Y=YES, N=NO, 'ESC' TO RE-ENTER."
             TO WS-MESSAGE
            PERFORM ERROR-MESSAGE
            GO TO GET-007.
       GET-010.
            MOVE "OPTION" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
             IF WS-INVCRED = "P"
                GO TO GET-005
             ELSE
                GO TO GET-000.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO Ws-EnterOption.
            IF Ws-EnterOption = "1" OR = "2"
                GO TO GET-020.
            MOVE "OPTIONS ARE ONLY 1 OR 2." TO WS-MESSAGE.
            PERFORM ERROR-MESSAGE.
            GO TO GET-010.
       GET-020.
           IF WS-INVCRED = "D"
              GO TO GET-999.
           MOVE "                    " TO F-NAMEFIELD.
           MOVE "PRINTER" TO F-FIELDNAME.
           MOVE 7 TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
              GO TO GET-010.
           MOVE 1 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WS-PRINT-NUM.
           IF WS-PRINT-NUM NOT = 1 AND NOT = 2 AND NOT = 3
                       AND NOT = 4 AND NOT = 5
             MOVE 
           "THE ENTRY MUST BE 1, 2, 3, 4 OR 5. 'ESC' TO RE-ENTER."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO GET-020.
           MOVE 1 TO F-CBFIELDLENGTH.
           MOVE WS-PRINT-NUM TO F-NAMEFIELD.
           PERFORM WRITE-FIELD-ALPHA.
           IF F-EXIT-CH NOT = X"1D"
              MOVE "ZAR" TO WS-CURRENCY
              MOVE 1     TO WS-EXCHANGERATE
              GO TO GET-025.
       GET-022.
            PERFORM ERROR1-020.
            PERFORM ERROR-020.
            
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "CURRENCY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-020.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-CURRENCY.
            
            PERFORM READ-CURRENCY.
            IF WS-CURRENCY-ST1 = 23 OR 35 OR 49
              GO TO GET-022.
            PERFORM ERROR-020.
            PERFORM ERROR1-020.
       GET-024.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "EXCHANGERATE"    TO F-FIELDNAME
            MOVE 12                TO F-CBFIELDNAME
            MOVE 10                TO F-CBFIELDLENGTH
            MOVE WS-EXCHANGERATE   TO F-EDNAMEFIELDNUMDEC
            MOVE 10                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUM-DEC.
            
            MOVE 2910 TO POS
            DISPLAY "1      = R" AT POS
            COMPUTE WS-CURRENCY-TEMP = 1 / WS-EXCHANGERATE
            ADD 2 TO POS 
            DISPLAY WS-CURRENCY AT POS
            ADD 8 TO POS
            MOVE WS-CURRENCY-TEMP TO WS-EXCHANGE-DIS
            DISPLAY WS-EXCHANGE-DIS AT POS.
            MOVE 3010 TO POS
            DISPLAY
            "ENTER MULTIPLE RAND TO A CURRENCY, PRESS <F8> TO CONVERT."
               AT POS.
            
            MOVE "EXCHANGERATE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-022.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            
            IF F-EXIT-CH = X"1D"
            COMPUTE NUMERIC-RATE = 1 / NUMERIC-RATE.
            
            MOVE NUMERIC-RATE TO WS-EXCHANGERATE
                                 F-EDNAMEFIELDNUMDEC.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUM-DEC.
            IF WS-EXCHANGERATE = 0
               MOVE "EXCHANGE RATE CANNOT BE ZERO" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-024.
            
            MOVE 2910 TO POS
            DISPLAY "1      = R" AT POS
            COMPUTE WS-CURRENCY-TEMP = 1 / WS-EXCHANGERATE
            ADD 2 TO POS 
            DISPLAY WS-CURRENCY AT POS
            ADD 8 TO POS
            MOVE WS-CURRENCY-TEMP TO WS-EXCHANGE-DIS
            DISPLAY WS-EXCHANGE-DIS AT POS.
       GET-025.
            IF WS-PRINT-NUM NOT = 4
               GO TO GET-030.
      *WS-PROG-TYPE = 4 MEANS USE THE C-NOTE FormsServer FILE
            IF WS-INVCRED = "C"
               MOVE 4 TO WS-PROG-TYPE
               GO TO GET-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PROG-TYPE" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-020.
            IF F-EXIT-CH = X"1D"
              GO TO GET-024.
            MOVE 1            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-PROG-TYPE
            IF WS-PROG-TYPE NOT = 1 AND NOT = 2 AND NOT = 3
              MOVE "THE ENTRY MUST BE 1, 2, OR 3, 'ESC' TO RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO GET-025.
            MOVE 1            TO F-CBFIELDLENGTH.
            MOVE WS-PROG-TYPE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            
            IF WS-PROG-TYPE = 1 OR = 3
              MOVE 1 TO WS-TYPE-OF-DOCUMENT
            ELSE
              MOVE 2 TO WS-TYPE-OF-DOCUMENT.
       GET-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "RANGE1" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-025.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-RANGE1.
            IF WS-RANGE1 = 0
               GO TO GET-030.
       GET-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "RANGE2" TO F-FIELDNAME.
                      MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-030.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-RANGE2.
            IF WS-ENTEROPTION = "1"
               GO TO GET-999.
            IF WS-RANGE2 = 0
               GO TO GET-999.
       GET-050.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "RANGE3" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-040.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-RANGE3.
            IF WS-RANGE3 = 0
               GO TO GET-999.
       GET-060.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "RANGE4" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-050.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-RANGE4.
            GO TO GET-999.
       GET-900.
            PERFORM CLEAR-010.
            MOVE 2910 TO POS
            DISPLAY "ENTER 'Y' TO CONTINUE, 'N' TO EXIT [ ]" AT POS
            ADD 36 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 45        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMPLETE.
            
            IF WS-COMPLETE NOT = "Y" AND NOT = "N"
               GO TO GET-900.
            IF WS-COMPLETE = "N"
               PERFORM END-OFF.
       GET-999.
            EXIT.
      *
       READ-CURRENCY SECTION.
       R-CUR-000.
           MOVE WS-CURRENCY TO CU-KEY.
           START CURRENCY-MASTER KEY NOT < CU-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-CURRENCY-ST1 NOT = 0
               MOVE "INVALID START ON CURRENCY FILE, 'ESC' TO EXIT"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO R-CUR-999.
        R-CUR-010.
           READ CURRENCY-MASTER
                 INVALID KEY NEXT SENTENCE.
           IF WS-CURRENCY-ST1 = 23 OR 35 OR 49
               MOVE "NO SUCH CURRENCY, 'ESC' TO RE-ENTER"
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CURRENCY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CURRENCY-ST1
                GO TO R-CUR-999.
           IF WS-CURRENCY-ST1 NOT = 0
                MOVE "CURRENCY BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CURRENCY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CURRENCY-ST1
                GO TO R-CUR-010.
           MOVE CU-VALUE TO WS-EXCHANGERATE.
       R-CUR-999.
             EXIT.
      *
       PDF-PRINT-INVOICE SECTION.
       LP-PDF-000.
           MOVE 1 TO WS-PAGE SUB-1 SUB-2
           MOVE " " TO PDET.
           MOVE INCR-TRANS TO STTR-TYPE
              GO TO LP-PDF-005.
           MOVE 0 TO WS-SUBTOTAL WS-INVOICETOTAL.
       LP-PDF-005.
           MOVE INCR-INVOICE TO STTR-REFERENCE1.
           MOVE 1            TO STTR-TRANSACTION-NUMBER.
           
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
       LP-PDF-010.
           IF WS-PAGE > 1
               MOVE " "                     TO LASER-REC
               MOVE "¶"                     TO PLCONT-CHAR
               MOVE WS-PAGE                 TO PL-CONT-PAGE
               MOVE "Continued To.....Page" TO PL-CONT-NAME
               WRITE LASER-REC FROM PL-CONTINUED
               MOVE " "     TO LASER-REC
               MOVE "¶"     TO LASER-REC
               WRITE LASER-REC
               WRITE LASER-REC
               WRITE LASER-REC
               WRITE LASER-REC.

           MOVE " "     TO LASER-REC
           MOVE "´¶"    TO PLCR-CHAR1
           IF INCR-TRANS = 1
              MOVE "      TAX INVOICE     " TO PL-TYPE
              GO TO LP-PDF-011.
           IF INCR-TRANS = 6
              MOVE "     TAX CREDIT NOTE  " TO PL-TYPE.
       LP-PDF-011.
           MOVE PA-NAME TO PL-NAME.
           WRITE LASER-REC FROM LASER-PCREDITLINE.
           MOVE " "     TO LASER-REC.
       LP-PDF-012.
           MOVE "¶"                      TO PL1-CHAR
           MOVE INCR-GSTNO               TO PL-GSTNO
           MOVE INCR-ACCOUNT             TO PL-ACCNO.
           MOVE PA-ADD1                  TO PL-ADDNAME
           WRITE LASER-REC FROM LASER-PLINE1.
           
           MOVE " "       TO LASER-REC LASER-PLINE2
           MOVE "¶"       TO PL2-CHAR
           MOVE PA-ADD2   TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE INCR-DEL1  TO PL-ADD
           MOVE PA-ADD3    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE INCR-DEL2  TO PL-ADD
           MOVE PA-DEL1    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE INCR-DEL3  TO PL-ADD
           MOVE PA-DEL2    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE PA-DEL3    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.

           MOVE " "             TO LASER-REC LASER-PLINE2.
           MOVE "¶"             TO PL2-CHAR
           MOVE INCR-NAME       TO PL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE INCR-ADD1  TO PL-ADD
           MOVE PA-PHONE   TO SUPPL-DIG30
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "             TO LASER-REC LASER-PLINE2
           MOVE "¶"             TO PL2-CHAR
           MOVE INCR-ADD2       TO PL-ADDRESS
           MOVE PA-FAX          TO SUPPL-DIG30
           WRITE LASER-REC       FROM LASER-PLINE2.
           
           MOVE " "             TO LASER-REC LASER-PLINE2
           MOVE "¶"             TO PL2-CHAR
           MOVE INCR-ADD3       TO PL-ADD
           MOVE PA-CO-REG-NO    TO SUPPL-DIG30
           WRITE LASER-REC       FROM LASER-PLINE2.

           MOVE " "             TO LASER-REC LASER-PLINE2.
           MOVE "¶"             TO PL2-CHAR
           MOVE INCR-CODE       TO PL-ADD
           MOVE PA-CO-VAT-NO    TO SUPPL-DIG30
           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR           TO SPLIT-HR
           MOVE ":"             TO SPLIT-HR-FIL
                                   SPLIT-MN-FIL
           MOVE WS-MIN          TO SPLIT-MN
           MOVE WS-SEC          TO SPLIT-SC
           MOVE WS-TIME-DISPLAY TO SUPPL-TIME
           MOVE INCR-PULLBY     TO PL-PULLBY
           MOVE INCR-AREA       TO PL-AREA
           MOVE PA-CO-VAT-NO    TO SUPPL-DIG30
           WRITE LASER-REC      FROM LASER-PLINE2.

           MOVE " "              TO LASER-REC LASER-PLINE2 LASER-PLINE4.
           MOVE "¶"              TO PL4-CHAR
           IF WS-ADD-TOGETHER = "Y"
            IF PL-PO = " "
               MOVE INCR-TERMS       TO PL-TERMS
               MOVE INCR-PORDER      TO PL-PO
               MOVE INCR-SALES       TO PL-SOLD
               MOVE INCR-DELIVERY    TO PL-VIA
               MOVE INCR-BIN         TO PL-BIN
               MOVE INCR-SB-TYPE     TO PL-SOLDBY.
           IF WS-ADD-TOGETHER NOT = "Y"
               MOVE INCR-TERMS       TO PL-TERMS
               MOVE INCR-PORDER      TO PL-PO
               MOVE INCR-SALES       TO PL-SOLD
               MOVE INCR-DELIVERY    TO PL-VIA
               MOVE INCR-BIN         TO PL-BIN
               MOVE INCR-SB-TYPE     TO PL-SOLDBY.
               
           IF WS-INVCRED = "X"
            IF INCR-BO-INV-NO NOT = WS-INVOICE
             MOVE INCR-BO-DATE     TO SPLIT-DATE
             PERFORM CONVERT-DATE-FORMAT
             MOVE DISPLAY-DATE     TO PL-ORDERDATE
             MOVE INCR-BO-INV-NO   TO PL-SLIP
             MOVE INCR-COPY-NUMBER TO PL-SLIP-COPY.

             MOVE INCR-DATE        TO SPLIT-DATE
             PERFORM CONVERT-DATE-FORMAT
             MOVE DISPLAY-DATE     TO PL-DATE.
             
           MOVE WS-INVOICE         TO PL-INV.
           MOVE WS-PAGE            TO PL-PAGE
           WRITE LASER-REC FROM LASER-PLINE4.
           
           MOVE " "              TO LASER-REC LASER-PLINE4
                                    LASER-PDET.
      *         MOVE "AT LP-PDF-020, STARTING STOCK LINES" TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE.

       LP-PDF-020.
           IF SUB-1 < 299
            IF SUB-1 = SUB-20
               SUBTRACT 1 FROM SUB-2
               GO TO LP-PDF-031.
           IF SUB-2 > 20
               MOVE 1 TO SUB-2
               ADD 1  TO WS-PAGE
               GO TO LP-PDF-010.
           IF SUB-1 > 300
               GO TO LP-PDF-031.
               
      *     MOVE "READING STOCK-TRANS IN PDF SECTION" TO WS-MESSAGE
      *     PERFORM ERROR1-010
      *     MOVE SUB-1 TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR1-020.

           PERFORM READ-STOCK-TRANSACTIONS
           MOVE 0 TO WS-BO-QTY.

           MOVE "¶"                         TO PLDET-CHAR
           MOVE B-STOCKNUMBER TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
            IF INCR-TRANS = 1
               MOVE C-LINE    TO PLDET-REST
               GO TO LP-PDF-025.
           IF SP-1STCHAR = "*"
            IF INCR-TRANS = 6
               MOVE C-CR-LINE TO PLDET-REST
               GO TO LP-PDF-025.

           MOVE B-STOCKNUMBER       TO PL-STOCK
           MOVE B-STOCKDESCRIPTION  TO PL-DESC
           MOVE B-STOCKDESCRIPTION2 TO PL-DESC2
           MOVE B-UNIT              TO PL-UNIT.
           IF INCR-TRANS = 1
             MOVE B-ORDERQTY        TO PL-ORDER.
           MOVE B-SHIPQTY           TO PL-SHIP.
           IF INCR-TRANS = 1
             COMPUTE WS-BO-QTY = B-ORDERQTY - (B-SHIPQTY + B-SHIPPEDQTY)
             MOVE WS-BO-QTY         TO PL-BO
             MOVE B-SHIPPEDQTY      TO PL-SHIPPED.
           MOVE B-STOCKPRICE        TO PL-PRICE
           MOVE B-DISCOUNTPERITEM   TO PL-DISCOUNT
           MOVE B-NETT              TO PL-NETT.
       LP-PDF-025.
           MOVE SUB-1 TO PL-NO
           WRITE LASER-REC FROM LASER-PDET.
           MOVE " " TO LASER-REC
                       LASER-PDET
                       BODY-FIELDS
                       COMM-LINES
                       COMM-CR-LINES.
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 301
              GO TO LP-PDF-020.
       LP-PDF-030.
           IF WS-COMPLETE = "Y"
              GO TO LP-PDF-031
           ELSE
              ADD 1 TO SUB-2
              PERFORM LP-PDF-005
              SUBTRACT 1 FROM SUB-20
              GO TO LP-PDF-020.
       LP-PDF-031.
           IF SUB-2 < 20
              MOVE "¶" TO PLDET-CHAR
              WRITE LASER-REC FROM LASER-PDET
              ADD 1 TO SUB-2
              GO TO LP-PDF-031.
       LP-PDF-035.
           MOVE " "              TO LASERPL-COMMENTLINE
           MOVE "¶"              TO PLCOM-CHAR
           MOVE INCR-COMMENT     TO PL-BO-MESSAGE
           MOVE WS-SPEC-COMMENT  TO PL-REST-OF-LINE
           WRITE LASER-REC       FROM LASERPL-COMMENTLINE.

           MOVE " "              TO LASERPL-COMMENTLINE.
           MOVE "¶"              TO PLCOM-CHAR
           IF WS-BO-FOUND = "Y"
              MOVE INCR-CONTACT  TO PL-BO-MESSAGE.
           MOVE PA-COMMENT       TO PL-COMM
           WRITE LASER-REC       FROM LASERPL-COMMENTLINE.

           MOVE " " TO LASER-REC LASERPL-ADDLINE LASERPL-COMMENTLINE.
           MOVE "¶"              TO PLADD-CHAR
           IF WS-BO-FOUND = "Y"
              MOVE INCR-PHONE    TO PL-PHONE.
           WRITE LASER-REC        FROM LASERPL-ADDLINE.

           MOVE " " TO LASER-REC LASERPL-ADDLINE LASERPL-COMMENTLINE.
           MOVE "¶"              TO PLADD-CHAR
           MOVE INCR-ADDFREIGHT  TO PL-ADD1
           MOVE INCR-ADDLABOUR   TO PL-ADD2.
      **********************************************************
      *NEXT FEW LINES ADDED TO ADD IN THE ADD-ONS FOR PRINTING *
      **********************************************************
           MOVE WS-TAXAMT        TO PL-ADD3
           WRITE LASER-REC     FROM LASERPL-ADDLINE.

           MOVE " "              TO LASERPL-ADDLINE LASER-REC.
           MOVE "¶"              TO PLADD-CHAR
           MOVE INCR-ADDPOST     TO PL-ADD1
           MOVE INCR-ADDMISC     TO PL-ADD2
           MOVE WS-SUBTOTAL      TO PL-ADD3.
           MOVE "ZAR"            TO PL-CURRENCY.
           MOVE WS-INVOICETOTAL  TO PL-ADD4
           MOVE PL-ADD4          TO PDF-TOTAL.
           WRITE LASER-REC     FROM LASERPL-ADDLINE.
       LP-PDF-036.
           MOVE " " TO LASERPL-ADDLINE LASER-REC WS-COMPLETE. 
       LP-PDF-999.
           EXIT.
      *
       LASER-PRINT-INVOICE SECTION.
       LP-000.
           MOVE 1 TO WS-PAGE SUB-1 SUB-2
           MOVE " " TO PDET.
           MOVE INCR-TRANS TO STTR-TYPE
              GO TO LP-005.
           MOVE 0 TO WS-SUBTOTAL WS-INVOICETOTAL.
       LP-003.
           IF WS-PROF-TYPE = "Q"
              MOVE 8         TO STTR-TYPE.
           IF WS-PROF-TYPE = "P"
              MOVE 4         TO STTR-TYPE.
       LP-005.
           MOVE INCR-INVOICE TO STTR-REFERENCE1.
           MOVE 1            TO STTR-TRANSACTION-NUMBER.
           
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
       LP-010.
           IF WS-PAGE > 1
               MOVE " "                     TO LASER-REC
               MOVE "¶"                     TO PLCONT-CHAR
               MOVE WS-PAGE                 TO PL-CONT-PAGE
               MOVE "Continued To.....Page" TO PL-CONT-NAME
               WRITE LASER-REC FROM PL-CONTINUED
               MOVE " "     TO LASER-REC
               MOVE "¶"     TO LASER-REC
               WRITE LASER-REC
               WRITE LASER-REC
               WRITE LASER-REC
               WRITE LASER-REC.
               
           MOVE " "     TO LASER-REC
           MOVE "´¶"    TO PLCR-CHAR1
           IF WS-INVCRED = "P"
              MOVE "    PROFORMA INVOICE  " TO PL-TYPE
              GO TO LP-011.
           IF WS-INVCRED = "I"
            IF WS-TYPE-OF-DOCUMENT = 1
              MOVE "      TAX INVOICE     " TO PL-TYPE
              GO TO LP-011.
           IF WS-INVCRED = "I"
            IF WS-TYPE-OF-DOCUMENT = 2
              MOVE "SUPPLIER DELIVERY NOTE" TO PL-TYPE
              GO TO LP-011.
           IF WS-INVCRED = "I"
            IF WS-TYPE-OF-DOCUMENT = 3
              MOVE "CUSTOMER DELIVERY NOTE" TO PL-TYPE
              GO TO LP-011.
           IF WS-INVCRED = "C"
              MOVE "     TAX CREDIT NOTE  " TO PL-TYPE.
       LP-011.
           MOVE PA-NAME TO PL-NAME.
           WRITE LASER-REC FROM LASER-PCREDITLINE.
           MOVE " "     TO LASER-REC.
       LP-012.
           MOVE "¶"                      TO PL1-CHAR
           MOVE INCR-GSTNO               TO PL-GSTNO
           MOVE INCR-ACCOUNT             TO PL-ACCNO.
           MOVE PA-ADD1                  TO PL-ADDNAME
           WRITE LASER-REC FROM LASER-PLINE1.
           
           MOVE " "       TO LASER-REC LASER-PLINE2
           MOVE "¶"       TO PL2-CHAR
           MOVE PA-ADD2   TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE INCR-DEL1  TO PL-ADD
           MOVE PA-ADD3    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE INCR-DEL2  TO PL-ADD
           MOVE PA-DEL1    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE INCR-DEL3  TO PL-ADD
           MOVE PA-DEL2    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE PA-DEL3    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.

           MOVE " "             TO LASER-REC LASER-PLINE2.
           MOVE "¶"             TO PL2-CHAR
           MOVE INCR-NAME       TO PL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE INCR-ADD1  TO PL-ADD
           MOVE PA-PHONE   TO SUPPL-DIG30
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "             TO LASER-REC LASER-PLINE2
           MOVE "¶"             TO PL2-CHAR
           MOVE INCR-ADD2       TO PL-ADDRESS
           MOVE PA-FAX          TO SUPPL-DIG30
           WRITE LASER-REC       FROM LASER-PLINE2.
           
           MOVE " "             TO LASER-REC LASER-PLINE2
           MOVE "¶"             TO PL2-CHAR
           MOVE INCR-ADD3       TO PL-ADD
           MOVE PA-CO-REG-NO    TO SUPPL-DIG30
           WRITE LASER-REC       FROM LASER-PLINE2.

           MOVE " "             TO LASER-REC LASER-PLINE2.
           MOVE "¶"             TO PL2-CHAR
           MOVE INCR-CODE       TO PL-ADD
           MOVE PA-CO-VAT-NO    TO SUPPL-DIG30
           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR           TO SPLIT-HR
           MOVE ":"             TO SPLIT-HR-FIL
                                   SPLIT-MN-FIL
           MOVE WS-MIN          TO SPLIT-MN
           MOVE WS-SEC          TO SPLIT-SC
           MOVE WS-TIME-DISPLAY TO SUPPL-TIME
           MOVE INCR-PULLBY     TO PL-PULLBY
           MOVE INCR-AREA       TO PL-AREA
           MOVE PA-CO-VAT-NO    TO SUPPL-DIG30
           WRITE LASER-REC      FROM LASER-PLINE2.

           MOVE " "              TO LASER-REC LASER-PLINE2 LASER-PLINE4.
           MOVE "¶"              TO PL4-CHAR
           IF WS-ADD-TOGETHER = "Y"
            IF PL-PO = " "
               MOVE INCR-TERMS       TO PL-TERMS
               MOVE INCR-PORDER      TO PL-PO
               MOVE INCR-SALES       TO PL-SOLD
               MOVE INCR-DELIVERY    TO PL-VIA
               MOVE INCR-BIN         TO PL-BIN
               MOVE INCR-SB-TYPE     TO PL-SOLDBY.
           IF WS-ADD-TOGETHER NOT = "Y"
               MOVE INCR-TERMS       TO PL-TERMS
               MOVE INCR-PORDER      TO PL-PO
               MOVE INCR-SALES       TO PL-SOLD
               MOVE INCR-DELIVERY    TO PL-VIA
               MOVE INCR-BIN         TO PL-BIN
               MOVE INCR-SB-TYPE     TO PL-SOLDBY.
               
           IF WS-INVCRED = "I" OR = "P" OR = "D"
            IF INCR-BO-INV-NO NOT = WS-INVOICE
             MOVE INCR-BO-DATE     TO SPLIT-DATE
             PERFORM CONVERT-DATE-FORMAT
             MOVE DISPLAY-DATE     TO PL-ORDERDATE
             MOVE INCR-BO-INV-NO   TO PL-SLIP
             MOVE INCR-COPY-NUMBER TO PL-SLIP-COPY.
           IF WS-ADD-TOGETHER = "Y"
             IF PL-DATE = " "
              MOVE INCR-DATE        TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE     TO PL-DATE.
           IF WS-ADD-TOGETHER NOT = "Y"
              MOVE INCR-DATE        TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE     TO PL-DATE.
           
           IF WS-ADD-TOGETHER = "Y"
              MOVE WS-NUMBER     TO PL-INV
           ELSE
              MOVE WS-INVOICE    TO PL-INV.
           MOVE WS-PAGE          TO PL-PAGE
           WRITE LASER-REC FROM LASER-PLINE4.
           
           MOVE " "              TO LASER-REC LASER-PLINE4
                                    LASER-PDET.
       LP-020.
           IF SUB-1 < 299
            IF SUB-1 = SUB-20
              SUBTRACT 1 FROM SUB-2
              GO TO LP-030.
           IF SUB-2 > 20
               MOVE 1 TO SUB-2
               ADD 1  TO WS-PAGE
               GO TO LP-010.
           IF SUB-1 > 300
               GO TO LP-030.
               
      *         MOVE "READING STOCK-TRANS" TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE.

           PERFORM READ-STOCK-TRANSACTIONS
           MOVE 0 TO WS-BO-QTY.

           MOVE "¶"                         TO PLDET-CHAR
           MOVE B-STOCKNUMBER TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
            IF INCR-TRANS = 1
               MOVE C-LINE    TO PLDET-REST
               GO TO LP-025.
           IF SP-1STCHAR = "*"
            IF WS-INVCRED = "P"
               MOVE C-LINE    TO PLDET-REST
               GO TO LP-025.
           IF SP-1STCHAR = "*"
            IF INCR-TRANS = 6
               MOVE C-CR-LINE TO PLDET-REST
               GO TO LP-025.

           MOVE B-STOCKNUMBER       TO PL-STOCK
           MOVE B-STOCKDESCRIPTION  TO PL-DESC
           MOVE B-STOCKDESCRIPTION2 TO PL-DESC2
           MOVE B-UNIT              TO PL-UNIT.
           IF WS-INVCRED = "I" OR = "D"
             MOVE B-ORDERQTY        TO PL-ORDER.
           MOVE B-SHIPQTY           TO PL-SHIP.
           IF WS-INVCRED = "I" OR = "D"
             COMPUTE WS-BO-QTY = B-ORDERQTY - (B-SHIPQTY + B-SHIPPEDQTY)
             MOVE WS-BO-QTY         TO PL-BO
             MOVE B-SHIPPEDQTY      TO PL-SHIPPED.
           MOVE B-STOCKPRICE        TO PL-PRICE
           MOVE B-DISCOUNTPERITEM   TO PL-DISCOUNT
           MOVE B-NETT              TO PL-NETT.
             
           IF WS-INVCRED NOT = "P"  
             GO TO LP-025.
             
           MOVE B-ORDERQTY        TO PL-ORDER
                                     PL-SHIP
           COMPUTE B-NETT = B-ORDERQTY *
             (B-STOCKPRICE - (B-STOCKPRICE * B-DISCOUNTPERITEM / 100)).
             
           MOVE B-NETT  TO PL-NETT.
           ADD  B-NETT  TO WS-SUBTOTAL.
       LP-025.
           MOVE SUB-1 TO PL-NO
           WRITE LASER-REC FROM LASER-PDET.
           MOVE " " TO LASER-REC
                       LASER-PDET
                       BODY-FIELDS
                       COMM-LINES
                       COMM-CR-LINES.
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 301
              GO TO LP-020.
       LP-030.
           IF WS-ADD-TOGETHER = "Y"
              PERFORM READ-REGISTER
            IF WS-COMPLETE = "Y"
              GO TO LP-031
            ELSE
              ADD 1 TO SUB-2
              PERFORM LP-003 THRU LP-005
              SUBTRACT 1 FROM SUB-20
              GO TO LP-020.
       LP-031.
           IF SUB-2 < 20
              MOVE "¶" TO PLDET-CHAR
              WRITE LASER-REC FROM LASER-PDET
              ADD 1 TO SUB-2
              GO TO LP-031.
       LP-035.
           MOVE " "              TO LASERPL-COMMENTLINE
           MOVE "¶"              TO PLCOM-CHAR
           MOVE INCR-COMMENT     TO PL-BO-MESSAGE
           MOVE WS-SPEC-COMMENT  TO PL-REST-OF-LINE
           WRITE LASER-REC       FROM LASERPL-COMMENTLINE.

           MOVE " "              TO LASERPL-COMMENTLINE.
           MOVE "¶"              TO PLCOM-CHAR
           IF WS-BO-FOUND = "Y"
              MOVE INCR-CONTACT  TO PL-BO-MESSAGE.
           MOVE PA-COMMENT       TO PL-COMM
           WRITE LASER-REC       FROM LASERPL-COMMENTLINE.

           MOVE " " TO LASER-REC LASERPL-ADDLINE LASERPL-COMMENTLINE.
           MOVE "¶"              TO PLADD-CHAR
           IF WS-BO-FOUND = "Y"
              MOVE INCR-PHONE    TO PL-PHONE.
           WRITE LASER-REC        FROM LASERPL-ADDLINE.

           MOVE " " TO LASER-REC LASERPL-ADDLINE LASERPL-COMMENTLINE.
           MOVE "¶"              TO PLADD-CHAR
           MOVE INCR-ADDFREIGHT  TO PL-ADD1
           MOVE INCR-ADDLABOUR   TO PL-ADD2.
      **********************************************************
      *NEXT FEW LINES ADDED TO ADD IN THE ADD-ONS FOR PRINTING *
      **********************************************************
           IF WS-INVCRED = "P"
              ADD INCR-ADDFREIGHT TO WS-SUBTOTAL
              ADD INCR-ADDLABOUR  TO WS-SUBTOTAL
              ADD INCR-ADDPOST    TO WS-SUBTOTAL
              ADD INCR-ADDMISC    TO WS-SUBTOTAL.
           
           IF WS-INVCRED = "P"
            IF INCR-GSTNO NOT = "EXPORT"
              COMPUTE WS-TAXAMT ROUNDED =
                   WS-SUBTOTAL * PA-GST-PERCENT / 100
            ELSE
              MOVE 0 TO WS-TAXAMT.
           MOVE WS-TAXAMT        TO PL-ADD3
           WRITE LASER-REC     FROM LASERPL-ADDLINE.

           MOVE " "              TO LASERPL-ADDLINE LASER-REC.
           MOVE "¶"              TO PLADD-CHAR
           MOVE INCR-ADDPOST     TO PL-ADD1
           MOVE INCR-ADDMISC     TO PL-ADD2
           MOVE WS-SUBTOTAL      TO PL-ADD3.
           IF WS-INVCRED = "P"
              COMPUTE WS-INVOICETOTAL = WS-SUBTOTAL + WS-TAXAMT.
           MOVE WS-CURRENCY      TO PL-CURRENCY.
           MOVE WS-INVOICETOTAL  TO PL-ADD4.
           WRITE LASER-REC     FROM LASERPL-ADDLINE.
       LP-036.
           MOVE " " TO LASERPL-ADDLINE LASER-REC.
           
      * TO DETERMINE WHAT HEADING FOR THE DOCUMENT TO PRINT SEE BELOW:
      * WS-PROG-TYPE: 1=INVOICE ONLY
      *               2=D/NOTES ONLY
      *               3=INVOICE & D/NOTES
      *               4=CREDIT NOTES
      
      *PRINT INVOICE / C/NOTE ONLY
           IF WS-PROG-TYPE = 1 OR = 4
               GO TO LP-999.
               
      *PRINT DELIVERY NOTES ONLY
           IF WS-PROG-TYPE = 2
            IF WS-TYPE-OF-DOCUMENT = 2
               MOVE 3 TO WS-TYPE-OF-DOCUMENT
               CLOSE STOCK-TRANS-FILE
               PERFORM OPEN-005
               
      *         MOVE "GOING FOR THE 2ND RUN" TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               MOVE WS-INVOICE TO INCR-INVOICE
               MOVE 1          TO INCR-TRANS
      *          WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
           
      *         MOVE 1 TO STTR-REFERENCE1
      *         MOVE 1 TO STTR-TRANSACTION-NUMBER
           
      *         START STOCK-TRANS-FILE KEY NOT < STTR-KEY
      *         MOVE "2ND START ON ST-TRANS FILE" TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               
               GO TO LP-000.
               
           IF WS-PROG-TYPE = 2
            IF WS-TYPE-OF-DOCUMENT = 3
               GO TO LP-999.
               
      *PRINT INVOICE & DELIVERY NOTES
           IF WS-PROG-TYPE = 3
            IF WS-TYPE-OF-DOCUMENT < 3
               ADD 1 TO WS-TYPE-OF-DOCUMENT
               CLOSE STOCK-TRANS-FILE
               PERFORM OPEN-005
               MOVE WS-INVOICE TO INCR-INVOICE
               MOVE 1          TO INCR-TRANS
               GO TO LP-000.
           IF WS-PROG-TYPE = 2 OR = 3
            IF WS-TYPE-OF-DOCUMENT = 3
               GO TO LP-999.
       LP-999.
           EXIT.
      *
       EMAIL-PRINT-INVOICE SECTION.
       LPE-000.
           MOVE 1 TO WS-PAGE SUB-1 SUB-2
           MOVE " " TO PDET.
           MOVE INCR-TRANS TO STTR-TYPE
              GO TO LPE-005.
           MOVE 0 TO WS-SUBTOTAL WS-INVOICETOTAL.
       LPE-003.
           IF WS-PROF-TYPE = "Q"
              MOVE 8         TO STTR-TYPE.
           IF WS-PROF-TYPE = "P"
              MOVE 4         TO STTR-TYPE.
       LPE-005.
           MOVE INCR-INVOICE TO STTR-REFERENCE1.
           MOVE 1            TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
       LPE-010.
           IF WS-PAGE > 1
               MOVE " "                     TO LASER-REC
               MOVE "¶"                     TO PLCONT-CHAR
               MOVE "³"                     TO PLCONT-CHAR2
               MOVE "Continued To.....Page" TO PL-CONT-NAME
               MOVE WS-PAGE                 TO PL-CONT-PAGE
               WRITE LASER-REC            FROM PL-CONTINUED

               MOVE " "                     TO LASER-REC PL-CONTINUED
               MOVE "¶"                     TO PLCONT-CHAR
               MOVE "³"                     TO PLCONT-CHAR2
               WRITE LASER-REC FROM PL-CONTINUED
               WRITE LASER-REC FROM PL-CONTINUED
               WRITE LASER-REC FROM PL-CONTINUED
               WRITE LASER-REC FROM PL-CONTINUED.
               
           MOVE "´¶"  TO WS-DELIM-F
           MOVE "³"   TO WS-DELIM-END1.
           
           IF WS-PAGE = 1
                MOVE WS-EMAIL-NUMBER TO WS-DATA-F
           ELSE
                MOVE SPACES          TO WS-DATA-F.
           WRITE LASER-REC         FROM WS-FST-LINE.
  
           MOVE " "     TO LASER-REC
           MOVE "¶ "    TO PLCR-CHAR1
           MOVE "³"     TO PLCR-CHAR2.
           IF WS-INVCRED = "P"
              MOVE "*PROFORMA INVOICE *" TO PL-TYPE.
       LPE-011.
           MOVE PA-NAME TO PL-NAME.
           WRITE LASER-REC FROM LASER-PCREDITLINE.
           MOVE " "     TO LASER-REC.
       LPE-012.
           MOVE "¶"                      TO PL1-CHAR
           MOVE "³"                      TO PL1-2.
           MOVE INCR-GSTNO               TO PL-GSTNO
           MOVE INCR-ACCOUNT             TO PL-ACCNO.
           MOVE PA-ADD1                  TO PL-ADDNAME
           WRITE LASER-REC FROM LASER-PLINE1.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE "³"        TO PL2-2
           MOVE PA-ADD2    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE "³"        TO PL2-2
           MOVE INCR-DEL1  TO PL-ADD
           MOVE PA-ADD3    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE "³"        TO PL2-2
           MOVE INCR-DEL2  TO PL-ADD
           MOVE PA-DEL1    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE "³"        TO PL2-2
           MOVE INCR-DEL3  TO PL-ADD
           MOVE PA-DEL2    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE "³"        TO PL2-2
           MOVE PA-DEL3    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.

           MOVE " "        TO LASER-REC LASER-PLINE2.
           MOVE "¶"        TO PL2-CHAR
           MOVE "³"        TO PL2-2
           MOVE INCR-NAME  TO PL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE "³"        TO PL2-2
           MOVE INCR-ADD1  TO PL-ADD
           MOVE PA-PHONE   TO SUPPL-DIG30
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "¶"        TO PL2-CHAR
           MOVE "³"        TO PL2-2
           MOVE INCR-ADD2  TO PL-ADDRESS
           MOVE PA-FAX     TO SUPPL-DIG30
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "             TO LASER-REC LASER-PLINE2
           MOVE "¶"             TO PL2-CHAR
           MOVE "³"             TO PL2-2
           MOVE INCR-ADD3       TO PL-ADD
           MOVE PA-CO-REG-NO    TO SUPPL-DIG30
           WRITE LASER-REC       FROM LASER-PLINE2.

           MOVE " "             TO LASER-REC LASER-PLINE2.
           MOVE "¶"             TO PL2-CHAR
           MOVE "³"             TO PL2-2
           MOVE INCR-CODE       TO PL-ADD
           MOVE PA-CO-VAT-NO    TO SUPPL-DIG30
           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR           TO SPLIT-HR
           MOVE ":"             TO SPLIT-HR-FIL
                                   SPLIT-MN-FIL
           MOVE WS-MIN          TO SPLIT-MN
           MOVE WS-SEC          TO SPLIT-SC
           MOVE WS-TIME-DISPLAY TO SUPPL-TIME
           MOVE INCR-PULLBY     TO PL-PULLBY
           MOVE INCR-AREA       TO PL-AREA
           MOVE PA-CO-VAT-NO    TO SUPPL-DIG30
           WRITE LASER-REC      FROM LASER-PLINE2.

           MOVE " "              TO LASER-REC LASER-PLINE2 LASER-PLINE4.
           MOVE "¶"              TO PL4-CHAR
           MOVE "³"              TO PL4-2.
           IF WS-ADD-TOGETHER = "Y"
            IF PL-PO = " "
               MOVE INCR-TERMS       TO PL-TERMS
               MOVE INCR-PORDER      TO PL-PO
               MOVE INCR-SALES       TO PL-SOLD
               MOVE INCR-DELIVERY    TO PL-VIA
               MOVE INCR-BIN         TO PL-BIN
               MOVE INCR-SB-TYPE     TO PL-SOLDBY.
           IF WS-ADD-TOGETHER NOT = "Y"
               MOVE INCR-TERMS       TO PL-TERMS
               MOVE INCR-PORDER      TO PL-PO
               MOVE INCR-SALES       TO PL-SOLD
               MOVE INCR-DELIVERY    TO PL-VIA
               MOVE INCR-BIN         TO PL-BIN
               MOVE INCR-SB-TYPE     TO PL-SOLDBY.
               
           IF WS-INVCRED = "I" OR = "P" OR = "D"
            IF INCR-BO-INV-NO NOT = WS-INVOICE
             MOVE INCR-BO-DATE     TO SPLIT-DATE
             PERFORM CONVERT-DATE-FORMAT
             MOVE DISPLAY-DATE     TO PL-ORDERDATE
             MOVE INCR-BO-INV-NO   TO PL-SLIP
             MOVE INCR-COPY-NUMBER TO PL-SLIP-COPY.
           IF WS-ADD-TOGETHER = "Y"
             IF PL-DATE = " "
              MOVE INCR-DATE        TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE     TO PL-DATE.
           IF WS-ADD-TOGETHER NOT = "Y"
              MOVE INCR-DATE        TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE     TO PL-DATE.
           
           IF WS-ADD-TOGETHER = "Y"
              MOVE WS-NUMBER     TO PL-INV
           ELSE
              MOVE WS-INVOICE    TO PL-INV.
           MOVE WS-PAGE          TO PL-PAGE
           WRITE LASER-REC FROM LASER-PLINE4.
           
           MOVE " "              TO LASER-REC LASER-PLINE4
                                    LASER-PDET.
       LPE-020.
           IF SUB-1 < 299
            IF SUB-1 = SUB-20
              SUBTRACT 1 FROM SUB-2
              GO TO LPE-030.
           IF SUB-2 > 20
               MOVE 1 TO SUB-2
               ADD 1  TO WS-PAGE
               GO TO LPE-010.
           IF SUB-1 > 300
               GO TO LPE-030.

           PERFORM READ-STOCK-TRANSACTIONS
           MOVE 0 TO WS-BO-QTY.

           MOVE "¶"           TO PLDET-CHAR
           MOVE "³"           TO PLDET-CHAR2
           MOVE B-STOCKNUMBER TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
            IF INCR-TRANS = 1
               MOVE C-LINE    TO PLDET-REST
               GO TO LPE-025.
           IF SP-1STCHAR = "*"
            IF WS-INVCRED = "P"
               MOVE C-LINE    TO PLDET-REST
               GO TO LPE-025.
           IF SP-1STCHAR = "*"
            IF INCR-TRANS = 6
               MOVE C-CR-LINE TO PLDET-REST
               GO TO LPE-025.

           MOVE B-STOCKNUMBER       TO PL-STOCK
           MOVE B-STOCKDESCRIPTION  TO PL-DESC
           MOVE B-STOCKDESCRIPTION2 TO PL-DESC2
           MOVE B-UNIT              TO PL-UNIT.
           IF WS-INVCRED = "I" OR = "D"
             MOVE B-ORDERQTY        TO PL-ORDER.
           MOVE B-SHIPQTY           TO PL-SHIP.
           IF WS-INVCRED = "I" OR = "D"
             COMPUTE WS-BO-QTY = B-ORDERQTY - (B-SHIPQTY + B-SHIPPEDQTY)
             MOVE WS-BO-QTY         TO PL-BO
             MOVE B-SHIPPEDQTY      TO PL-SHIPPED.
           MOVE B-STOCKPRICE        TO PL-PRICE
           MOVE B-DISCOUNTPERITEM   TO PL-DISCOUNT
           MOVE B-NETT              TO PL-NETT.
             
           IF WS-INVCRED NOT = "P"
             GO TO LPE-025.
             
           MOVE B-ORDERQTY         TO PL-ORDER
                                      PL-SHIP
           COMPUTE B-NETT = B-ORDERQTY *
             (B-STOCKPRICE - (B-STOCKPRICE * B-DISCOUNTPERITEM / 100))
           MOVE B-NETT  TO PL-NETT
           ADD  B-NETT  TO WS-SUBTOTAL.
       LPE-025.
           MOVE SUB-1 TO PL-NO
           WRITE LASER-REC FROM LASER-PDET.
           MOVE " " TO LASER-REC
                       LASER-PDET
                       BODY-FIELDS
                       COMM-LINES
                       COMM-CR-LINES.
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 301
              GO TO LPE-020.
       LPE-030.
           IF WS-ADD-TOGETHER = "Y"
              PERFORM READ-REGISTER
            IF WS-COMPLETE = "Y"
              GO TO LPE-031
            ELSE
              ADD 1 TO SUB-2
              PERFORM LPE-003 THRU LPE-005
              SUBTRACT 1 FROM SUB-20
              GO TO LPE-020.
       LPE-031.
           IF SUB-2 < 20
              MOVE "¶"          TO PLDET-CHAR
              MOVE "³"          TO PLDET-CHAR2
              WRITE LASER-REC FROM LASER-PDET
              ADD 1 TO SUB-2
              GO TO LPE-031.
       LPE-035.
           MOVE " "              TO LASERPL-COMMENTLINE
           MOVE "¶"              TO PLCOM-CHAR
           MOVE "³"              TO PLCOM-CHAR2
           MOVE INCR-COMMENT     TO PL-BO-MESSAGE
           MOVE WS-SPEC-COMMENT  TO PL-REST-OF-LINE
           WRITE LASER-REC     FROM LASERPL-COMMENTLINE.

           MOVE " "              TO LASERPL-COMMENTLINE.
           MOVE "¶"              TO PLCOM-CHAR
           MOVE "³"              TO PLCOM-CHAR2
           IF WS-BO-FOUND = "Y"
              MOVE INCR-CONTACT TO PL-BO-MESSAGE.
           MOVE PA-COMMENT      TO PL-COMM
           WRITE LASER-REC    FROM LASERPL-COMMENTLINE.

           MOVE " " TO LASER-REC LASERPL-ADDLINE LASERPL-COMMENTLINE.
           MOVE "¶"              TO PLADD-CHAR
           MOVE "³"              TO PLADD-CHAR2
           IF WS-BO-FOUND = "Y"
              MOVE INCR-PHONE    TO PL-PHONE.
           WRITE LASER-REC     FROM LASERPL-ADDLINE.

           MOVE " " TO LASER-REC LASERPL-ADDLINE LASERPL-COMMENTLINE.
           MOVE "¶"              TO PLADD-CHAR
           MOVE "³"              TO PLADD-CHAR2
           MOVE INCR-ADDFREIGHT  TO PL-ADD1
           MOVE INCR-ADDLABOUR   TO PL-ADD2.
           IF WS-INVCRED = "P"
              ADD INCR-ADDFREIGHT TO WS-SUBTOTAL
              ADD INCR-ADDLABOUR  TO WS-SUBTOTAL
              ADD INCR-ADDPOST    TO WS-SUBTOTAL
              ADD INCR-ADDMISC    TO WS-SUBTOTAL.
           
           IF WS-INVCRED = "P"
            IF INCR-GSTNO NOT = "EXPORT"
              COMPUTE WS-TAXAMT ROUNDED =
                   WS-SUBTOTAL * PA-GST-PERCENT / 100
            ELSE
              MOVE 0             TO WS-TAXAMT.
           MOVE WS-TAXAMT        TO PL-ADD3
           WRITE LASER-REC     FROM LASERPL-ADDLINE.

           MOVE " "              TO LASERPL-ADDLINE LASER-REC.
           MOVE "¶"              TO PLADD-CHAR
           MOVE "³"              TO PLADD-CHAR2
           MOVE INCR-ADDPOST     TO PL-ADD1
           MOVE INCR-ADDMISC     TO PL-ADD2
           MOVE WS-SUBTOTAL      TO PL-ADD3.
           IF WS-INVCRED = "P"
              COMPUTE WS-INVOICETOTAL = WS-SUBTOTAL + WS-TAXAMT.
           MOVE WS-CURRENCY      TO PL-CURRENCY.
           MOVE WS-INVOICETOTAL  TO PL-ADD4.
           WRITE LASER-REC     FROM LASERPL-ADDLINE.
       LPE-036.
           MOVE " " TO LASERPL-ADDLINE LASER-REC.
       LPE-999.
           EXIT.
      *
       PRINT SECTION.
       P-000.
           MOVE 1 TO WS-PAGE SUB-1 SUB-2
                       PRINT-REC.
           MOVE " " TO PDET.
           MOVE INCR-TRANS TO STTR-TYPE
              GO TO P-005.
           MOVE 0 TO WS-SUBTOTAL WS-INVOICETOTAL.
       P-003.
           IF WS-PROF-TYPE = "Q"
              MOVE 8         TO STTR-TYPE.
           IF WS-PROF-TYPE = "P"
              MOVE 4         TO STTR-TYPE.
       P-005.
           MOVE INCR-INVOICE TO STTR-REFERENCE1.
           MOVE 1            TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
       P-010.
           IF WS-PAGE > 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE WS-PAGE TO P-CONT-PAGE
               WRITE PRINT-REC FROM P-CONTINUED
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC BEFORE PAGE.
           IF INCR-TRANS = 6
               MOVE "XXXXXXXXXXXXXXXXXXX" TO P-XES
               MOVE "*** CREDIT NOTE ***" TO P-TYPE
           ELSE
               MOVE " " TO P-XES
               MOVE " " TO P-TYPE.
           IF WS-INVCRED = "D"
            IF INCR-TRANS = 1
               MOVE "XXXXXXXXXXXXXXXXXXX" TO P-XES
               MOVE "*COPY TAX INVOICE *" TO P-TYPE.
           IF WS-INVCRED NOT = "D"
            IF INCR-TRANS = 1
               MOVE "XXXXXXXXXXXXXXXXXXX" TO P-XES
               MOVE "*** TAX INVOICE ***" TO P-TYPE.
           IF WS-INVCRED = "P"
               MOVE "XXXXXXXXXXXXXXXXXXX" TO P-XES
               MOVE "*PROFORMA INVOICE *" TO P-TYPE.
               
           MOVE PA-NAME                   TO P-NAME
           WRITE PRINT-REC FROM PCREDITLINE
           MOVE " " TO PRINT-REC.
       P-012.
           MOVE INCR-GSTNO            TO P-GSTNO
           MOVE INCR-ACCOUNT          TO P-ACCNO
           MOVE PA-ADD1               TO P-ADDNAME
           WRITE PRINT-REC FROM PLINE1
           MOVE " "       TO PRINT-REC PLINE2
           MOVE PA-ADD2   TO SUPP-ADD
           WRITE PRINT-REC FROM PLINE2
           MOVE " "       TO PRINT-REC PLINE2
           MOVE INCR-DEL1 TO P-ADD
           MOVE PA-ADD3   TO SUPP-ADD
           WRITE PRINT-REC FROM PLINE2
           MOVE " "       TO PRINT-REC PLINE2
           MOVE INCR-DEL2 TO P-ADD
           MOVE PA-DEL1   TO SUPP-ADD
           WRITE PRINT-REC FROM PLINE2
           MOVE " "       TO PRINT-REC PLINE2
           MOVE INCR-DEL3 TO P-ADD
           MOVE PA-DEL2   TO SUPP-ADD
           WRITE PRINT-REC FROM PLINE2
           MOVE " "       TO PRINT-REC PLINE2
           MOVE PA-DEL3   TO SUPP-ADD
           WRITE PRINT-REC FROM PLINE2
           MOVE " "       TO PRINT-REC PLINE2.

           MOVE INCR-NAME       TO P-ADD
           WRITE PRINT-REC FROM PLINE2
           MOVE " "             TO PRINT-REC PLINE2
           MOVE INCR-ADD1       TO P-ADD
           MOVE PA-PHONE        TO SUPP-DIG30
           WRITE PRINT-REC FROM PLINE2
           MOVE " "             TO PRINT-REC PLINE2
           MOVE INCR-ADD2       TO P-ADDRESS
           MOVE PA-FAX          TO SUPP-DIG30
           WRITE PRINT-REC       FROM PLINE2
           MOVE " "             TO PRINT-REC PLINE2
           MOVE INCR-ADD3       TO P-ADD
           MOVE PA-CO-REG-NO    TO SUPP-DIG30
           WRITE PRINT-REC       FROM PLINE2
           MOVE " "             TO PRINT-REC PLINE2.

           MOVE INCR-CODE       TO P-ADD
           MOVE PA-CO-VAT-NO    TO SUPP-DIG30
           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR           TO SPLIT-HR
           MOVE ":"             TO SPLIT-HR-FIL
                                   SPLIT-MN-FIL
           MOVE WS-MIN          TO SPLIT-MN
           MOVE WS-SEC          TO SPLIT-SC
           MOVE WS-TIME-DISPLAY TO SUPP-TIME
           WRITE PRINT-REC       FROM PLINE2
           MOVE " "             TO PRINT-REC PLINE2 PLINE4

           WRITE PRINT-REC
           WRITE PRINT-REC
           IF WS-ADD-TOGETHER = "Y"
            IF P-PO = " "
               MOVE INCR-TERMS       TO P-TERMS
               MOVE INCR-PORDER      TO P-PO
               MOVE INCR-SALES       TO P-SOLD
               MOVE INCR-DELIVERY    TO P-VIA
               MOVE INCR-BIN         TO P-BIN
               MOVE INCR-SB-TYPE     TO P-SOLDBY.
           IF WS-ADD-TOGETHER NOT = "Y"
               MOVE INCR-TERMS       TO P-TERMS
               MOVE INCR-PORDER      TO P-PO
               MOVE INCR-SALES       TO P-SOLD
               MOVE INCR-DELIVERY    TO P-VIA
               MOVE INCR-BIN         TO P-BIN
               MOVE INCR-SB-TYPE     TO P-SOLDBY.
               
           IF WS-INVCRED = "I" OR = "P" OR = "D"
            IF INCR-BO-INV-NO NOT = WS-INVOICE
             MOVE INCR-BO-DATE   TO SPLIT-DATE
             PERFORM CONVERT-DATE-FORMAT
             MOVE DISPLAY-DATE   TO P-ORDERDATE
             MOVE INCR-BO-INV-NO TO P-SLIP.
           IF WS-ADD-TOGETHER = "Y"
             IF P-DATE = " "
              MOVE INCR-DATE        TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE     TO P-DATE.
           IF WS-ADD-TOGETHER NOT = "Y"
              MOVE INCR-DATE        TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE     TO P-DATE.
           
           IF WS-ADD-TOGETHER = "Y"
              MOVE WS-NUMBER     TO P-INV
           ELSE
              MOVE WS-INVOICE    TO P-INV.
           MOVE WS-PAGE          TO P-PAGE
           WRITE PRINT-REC FROM PLINE4
           MOVE " "              TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PDET.
       P-020.
           IF SUB-1 < 299
            IF SUB-1 = SUB-20
              SUBTRACT 1 FROM SUB-2
              GO TO P-030.
           IF SUB-2 > 20
               MOVE 1 TO SUB-2
               ADD 1 TO WS-PAGE
               GO TO P-010.
           IF SUB-1 > 300
               GO TO P-030.

           PERFORM READ-STOCK-TRANSACTIONS
           MOVE 0 TO WS-BO-QTY.

           MOVE B-STOCKNUMBER TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
            IF INCR-TRANS = 1
               MOVE C-LINE    TO PDET-REST
               GO TO P-025.
           IF SP-1STCHAR = "*"
            IF WS-INVCRED = "P"
               MOVE C-LINE    TO PDET-REST
               GO TO P-025.
           IF SP-1STCHAR = "*"
            IF INCR-TRANS = 6
               MOVE C-CR-LINE TO PDET-REST
               GO TO P-025.

           MOVE B-STOCKNUMBER       TO P-STOCK
           MOVE B-STOCKDESCRIPTION  TO P-DESC
           MOVE B-STOCKDESCRIPTION2 TO P-DESC2
           MOVE B-UNIT              TO P-UNIT.
           IF WS-INVCRED = "I" OR = "D"
             MOVE B-ORDERQTY        TO P-ORDER.
           MOVE B-SHIPQTY           TO P-SHIP.
           IF WS-INVCRED = "I" OR = "D"
             COMPUTE WS-BO-QTY = B-ORDERQTY - (B-SHIPQTY + B-SHIPPEDQTY)
             MOVE WS-BO-QTY         TO P-BO
             MOVE B-SHIPPEDQTY      TO P-SHIPPED.
           MOVE B-STOCKPRICE        TO P-PRICE
           MOVE B-DISCOUNTPERITEM   TO P-DISCOUNT
           MOVE B-NETT              TO P-NETT.
             
           IF WS-INVCRED NOT = "P"
             GO TO P-025.
             
           MOVE B-ORDERQTY        TO P-ORDER
                                     P-SHIP
           COMPUTE B-NETT = B-ORDERQTY *
             (B-STOCKPRICE - (B-STOCKPRICE * B-DISCOUNTPERITEM / 100))
           MOVE B-NETT  TO P-NETT
           ADD B-NETT   TO WS-SUBTOTAL.
       P-025.
           MOVE SUB-1 TO P-NO
           WRITE PRINT-REC FROM PDET.
           MOVE " " TO PRINT-REC
                       PDET
                       BODY-FIELDS
                       COMM-LINES
                       COMM-CR-LINES.
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 301
              GO TO P-020.
       P-030.
           IF WS-ADD-TOGETHER = "Y"
              PERFORM READ-REGISTER
            IF WS-COMPLETE = "Y"
              GO TO P-031
            ELSE
              ADD 1 TO SUB-2
              PERFORM P-003 THRU P-005
              SUBTRACT 1 FROM SUB-20
              GO TO P-020.
       P-031.
           IF SUB-2 < 20
              WRITE PRINT-REC
              ADD 1 TO SUB-2
              GO TO P-031.
       P-035.
           MOVE " "             TO P-COMMENTLINE
           MOVE INCR-COMMENT    TO P-BO-MESSAGE
           MOVE WS-SPEC-COMMENT TO P-REST-OF-LINE
           WRITE PRINT-REC       FROM P-COMMENTLINE
           MOVE " "             TO P-COMMENTLINE.

           IF WS-BO-FOUND = "Y"
              MOVE INCR-CONTACT TO P-BO-MESSAGE.
           MOVE WS-PRINT-BOLD   TO P-DIG1
           MOVE PA-COMMENT      TO P-COMM
           MOVE WS-PRINT-UNBOLD TO P-DIG2
           WRITE PRINT-REC FROM P-COMMENTLINE.

           MOVE " "              TO P-ADDLINE P-COMMENTLINE PRINT-REC
           IF WS-BO-FOUND = "Y"
              MOVE INCR-PHONE    TO P-PHONE.
           MOVE INCR-ADDFREIGHT  TO P-ADD1
           MOVE INCR-ADDLABOUR   TO P-ADD2.
      **********************************************************
      *NEXT FEW LINES ADDED TO ADD IN THE ADD-ONS FOR PRINTING *
      **********************************************************
           IF WS-INVCRED = "P"
              ADD INCR-ADDFREIGHT TO WS-SUBTOTAL
              ADD INCR-ADDLABOUR  TO WS-SUBTOTAL
              ADD INCR-ADDPOST    TO WS-SUBTOTAL
              ADD INCR-ADDMISC    TO WS-SUBTOTAL.
           
           IF WS-INVCRED = "P"
            IF INCR-GSTNO NOT = "EXPORT"
              COMPUTE WS-TAXAMT ROUNDED =
                WS-SUBTOTAL * PA-GST-PERCENT / 100
            ELSE
                MOVE 0 TO WS-TAXAMT.
              
           MOVE WS-TAXAMT        TO P-ADD3.
           WRITE PRINT-REC        FROM P-ADDLINE
           MOVE " "              TO P-ADDLINE PRINT-REC.

           MOVE INCR-ADDPOST     TO P-ADD1
           MOVE INCR-ADDMISC     TO P-ADD2
           MOVE WS-SUBTOTAL      TO P-ADD3.
           IF WS-INVCRED = "P"
              COMPUTE WS-INVOICETOTAL = WS-SUBTOTAL + WS-TAXAMT.
           MOVE "ZAR"            TO P-CURRENCY.
           MOVE WS-INVOICETOTAL  TO P-ADD4.
           WRITE PRINT-REC        FROM P-ADDLINE.
       P-036.
           MOVE " "              TO P-ADDLINE P-ADD PRINT-REC.
           IF Ws-EnterOption = "2"
            IF WS-RANGE1 = 0 AND WS-RANGE2 = 0 AND WS-RANGE3 = 0
                             AND WS-RANGE4 = 0
                 GO TO P-999.
           WRITE PRINT-REC BEFORE PAGE.
       P-999.
           EXIT.
      *
       READ-DOCU-REGISTER SECTION.
       RDUR-000.
           IF WS-INCR-ST1 NOT = 0
               MOVE "Y" TO WS-COMPLETE
               GO TO RDUR-999.
             
           MOVE " " TO WS-BO-REDUCED-MESSAGE
                       WS-BO-FOUND.
           MOVE 0 TO SUB-1.
       RDUR-055.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10 OR = 23
               MOVE "Y" TO WS-COMPLETE
               GO TO RDUR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "ERROR IN REGISTER FILE, PRESS 'ESC' TO EXIT = "
               TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE 3060 TO POS
               DISPLAY WS-INCR-ST1 AT POS
               ADD 5 TO POS
               DISPLAY INCR-INVOICE AT POS
               PERFORM ERROR-010
               PERFORM ERROR-020
               MOVE 0 TO WS-INCR-ST1
               MOVE "E" TO WS-COMPLETE
               GO TO RDUR-055.
           IF INCR-TRANS NOT = 1 AND NOT = 6
              GO TO RDUR-055.
              
           IF INCR-DATE < WS-DATE
              GO TO RDUR-055.
           MOVE INCR-DATE TO SPLIT-DATE.
           IF SPLIT-MM > WS-MM
               MOVE "Y" TO WS-COMPLETE
              GO TO RDUR-999.
           IF SPLIT-YY > WS-YY
               MOVE "Y" TO WS-COMPLETE
              GO TO RDUR-999.
       RDUR-100.
           IF WS-FOUND = " "
              MOVE "Y" TO WS-FOUND.
              
           PERFORM ERROR2-020
           MOVE 2910 TO POS
           DISPLAY "RECORD BEING READ:" AT POS
           ADD 19 TO POS
           DISPLAY INCR-INVOICE AT POS
           ADD 8 TO POS.
           IF INCR-TRANS = 1
             DISPLAY "INVOICE" AT POS
           ELSE
             DISPLAY "C/NOTE" AT POS.
              
           MOVE INCR-INVOICE        TO WS-INVOICE
           MOVE INCR-INVCRED-AMT    TO WS-INVOICETOTAL
           MOVE INCR-TAX            TO WS-TAXAMT
           MOVE INCR-ADDONS         TO WS-ADDONAMT
           COMPUTE WS-SUBTOTAL =
                WS-INVOICETOTAL - (WS-TAXAMT + WS-ADDONAMT)
           MOVE INCR-LINENO         TO SUB-20.
       RDUR-999.
           EXIT.
      *
       READ-PDF-REGISTER SECTION.
       RPDF-000.
           IF WS-INCR-ST1 NOT = 0
               MOVE "Y" TO WS-COMPLETE
               GO TO RPDF-999.
             
           MOVE " " TO WS-BO-REDUCED-MESSAGE
                       WS-BO-FOUND.
           MOVE 0 TO SUB-1.
       RPDF-055.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10 OR = 23
               MOVE "Y" TO WS-COMPLETE
               GO TO RPDF-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "ERROR IN REGISTER FILE, PRESS 'ESC' TO EXIT = "
               TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE 3060 TO POS
               DISPLAY WS-INCR-ST1 AT POS
               ADD 5 TO POS
               DISPLAY INCR-INVOICE AT POS
               PERFORM ERROR-010
               PERFORM ERROR-020
               MOVE 0 TO WS-INCR-ST1
               MOVE "E" TO WS-COMPLETE
               GO TO RPDF-055.
           IF INCR-TRANS NOT = 1 AND NOT = 6
              GO TO RPDF-055.
              
           IF INCR-DATE < WS-DATE
              GO TO RPDF-055.
           MOVE INCR-DATE TO SPLIT-DATE.
           IF SPLIT-MM > WS-MM
               MOVE "Y" TO WS-COMPLETE
              GO TO RPDF-999.
           IF SPLIT-YY > WS-YY
               MOVE "Y" TO WS-COMPLETE
              GO TO RPDF-999.
       RPDF-100.
           IF WS-FOUND = " "
              MOVE "Y" TO WS-FOUND.
              
           PERFORM ERROR2-020
           MOVE 2910 TO POS
           DISPLAY "PDF RECORD BEING READ:" AT POS
           ADD 23 TO POS
           DISPLAY INCR-INVOICE AT POS
           ADD 8 TO POS.
           IF INCR-TRANS = 1
             DISPLAY "INVOICE" AT POS
           ELSE
             DISPLAY "C/NOTE" AT POS.
              
           MOVE INCR-INVOICE        TO WS-INVOICE
           MOVE INCR-INVCRED-AMT    TO WS-INVOICETOTAL
           MOVE INCR-TAX            TO WS-TAXAMT
           MOVE INCR-ADDONS         TO WS-ADDONAMT
           COMPUTE WS-SUBTOTAL =
                WS-INVOICETOTAL - (WS-TAXAMT + WS-ADDONAMT).
           MOVE INCR-LINENO         TO SUB-20.
       RPDF-999.
           EXIT.
      *
       READ-INVOICE-REGISTER SECTION.
       RIR-000.
           MOVE "N" TO WS-COMPLETE.
           IF WS-INVCRED = "I"
               MOVE 1 TO INCR-TRANS
           ELSE
               MOVE 6 TO INCR-TRANS.
           MOVE " "   TO WS-BO-REDUCED-MESSAGE
                         WS-BO-FOUND.
           MOVE 0 TO SUB-1.
           
      * PRINT ALL INVOICE / C-NOTES ONLY IF THEY HAVE NOT YET PRINTED     
           IF WS-ENTEROPTION = "1"
              GO TO RIR-050.
       RIR-002.
           IF WS-RANGE1 NOT = 0
              MOVE WS-RANGE1 TO INCR-INVOICE
              START INCR-REGISTER KEY NOT < INCR-KEY
              MOVE 0 TO WS-RANGE1
              GO TO RIR-005.
           IF WS-RANGE2 NOT = 0
              MOVE WS-RANGE2 TO INCR-INVOICE
              START INCR-REGISTER KEY NOT < INCR-KEY
              MOVE 0 TO WS-RANGE2
              GO TO RIR-005.
           IF WS-RANGE3 NOT = 0
              MOVE WS-RANGE3 TO INCR-INVOICE
              START INCR-REGISTER KEY NOT < INCR-KEY
              MOVE 0 TO WS-RANGE3
              GO TO RIR-005.
           IF WS-RANGE4 NOT = 0
              MOVE WS-RANGE4 TO INCR-INVOICE
              START INCR-REGISTER KEY NOT < INCR-KEY
              MOVE 0 TO WS-RANGE4
              GO TO RIR-005.
              
           MOVE "Y" TO WS-COMPLETE
              GO TO RIR-999.
       RIR-005.
           READ INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO INCR-INVOICE
               GO TO RIR-002.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ, PRESS 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-005.
           IF WS-INVCRED = "I"
            IF INCR-TRANS = 1
               GO TO RIR-100.
           IF WS-INVCRED = "C"
            IF INCR-TRANS = 6
               GO TO RIR-100.
               
           GO TO RIR-002.
       RIR-050.
           IF INCR-INVOICE = 0
              MOVE WS-RANGE1 TO INCR-INVOICE.
           START INCR-REGISTER KEY NOT < INCR-KEY.
       RIR-055.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
               MOVE 0 TO WS-INCR-ST1
               MOVE "Y" TO WS-COMPLETE
               GO TO RIR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ-NEXT, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-055.
           IF INCR-INVOICE > WS-RANGE2
              MOVE "Y" TO WS-COMPLETE
              GO TO RIR-999.
           IF WS-INVCRED = "I"
             IF INCR-TRANS = 1
              GO TO RIR-100.
           IF WS-INVCRED = "C"
             IF INCR-TRANS = 6
              GO TO RIR-100.
              
           GO TO RIR-055.
       RIR-100.
           IF WS-ENTEROPTION = "1"
            IF INCR-PRINTED = "Y" OR = "P"
              GO TO RIR-055.
           IF WS-FOUND = " "
              MOVE "Y" TO WS-FOUND.
              
           MOVE INCR-INVOICE        TO WS-INVOICE
           MOVE INCR-INVCRED-AMT    TO WS-INVOICETOTAL
           MOVE INCR-TAX            TO WS-TAXAMT
           MOVE INCR-ADDONS         TO WS-ADDONAMT
           COMPUTE WS-SUBTOTAL =
                WS-INVOICETOTAL - (WS-TAXAMT + WS-ADDONAMT).
           MOVE INCR-LINENO         TO SUB-20.
           
           IF WS-PRINT-NUM = 4 OR = 5
            IF WS-CURRENCY NOT = "ZAR"
             COMPUTE WS-INVOICETOTAL ROUNDED
                                     = WS-INVOICETOTAL * WS-EXCHANGERATE
             COMPUTE WS-TAXAMT ROUNDED   =   WS-TAXAMT * WS-EXCHANGERATE
             COMPUTE WS-ADDONAMT ROUNDED = WS-ADDONAMT * WS-EXCHANGERATE
             COMPUTE WS-SUBTOTAL =
                  WS-INVOICETOTAL - (WS-TAXAMT + WS-ADDONAMT).

           IF WS-PRINT-NUM = 5
              PERFORM READ-DEBTOR.
       RIR-500.
           MOVE "Y" TO INCR-PRINTED.
           REWRITE INCR-REC
             INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-500.
       RIR-999.
           EXIT.
      *
       READ-REGISTER SECTION.
       RQP-000.
           IF WS-PROF-TYPE = "Q"
               MOVE 8 TO INCR-TRANS.
           IF WS-PROF-TYPE = "P"
               MOVE 4 TO INCR-TRANS.
           IF WS-PROF-TYPE = "R"
               MOVE 3 TO INCR-TRANS.
           MOVE " " TO WS-BO-REDUCED-MESSAGE
                       WS-BO-FOUND.
           IF WS-ADD-TOGETHER NOT = "Y"
               MOVE 0 TO SUB-1 SUB-20.
           IF Ws-EnterOption = "1"
              GO TO RQP-050.
       RQP-002.
           IF WS-RANGE1 NOT = 0
              MOVE WS-RANGE1 TO INCR-INVOICE
              START INCR-REGISTER KEY NOT < INCR-KEY
              MOVE 0 TO WS-RANGE1
              GO TO RQP-005.
           IF WS-RANGE2 NOT = 0
              MOVE WS-RANGE2 TO INCR-INVOICE
              START INCR-REGISTER KEY NOT < INCR-KEY
              MOVE 0 TO WS-RANGE2
              GO TO RQP-005.
           IF WS-RANGE3 NOT = 0
              MOVE WS-RANGE3 TO INCR-INVOICE
              START INCR-REGISTER KEY NOT < INCR-KEY
              MOVE 0 TO WS-RANGE3
              GO TO RQP-005.
           IF WS-RANGE4 NOT = 0
              MOVE WS-RANGE4 TO INCR-INVOICE
              START INCR-REGISTER KEY NOT < INCR-KEY
              MOVE 0 TO WS-RANGE4
              GO TO RQP-005.
           MOVE "Y" TO WS-COMPLETE
              GO TO RQP-999.
       RQP-005.
           READ INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO INCR-INVOICE
               GO TO RQP-002.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RQP-005.
           IF WS-PROF-TYPE = "R"
            IF INCR-TRANS = 3
               GO TO RQP-100.
           IF WS-PROF-TYPE = "P"
            IF INCR-TRANS = 4
               GO TO RQP-100.
           IF WS-PROF-TYPE = "Q"
            IF INCR-TRANS = 8
               GO TO RQP-100.
           GO TO RQP-002.
       RQP-050.
           IF INCR-INVOICE = 0
              MOVE WS-RANGE1 TO INCR-INVOICE.
           START INCR-REGISTER KEY NOT < INCR-KEY.
       RQP-055.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
               MOVE 0 TO WS-INCR-ST1
               MOVE "Y" TO WS-COMPLETE
               GO TO RQP-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ-NEXT, 'ESC' TO EXIT"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE INCR-INVOICE TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               MOVE "E" TO WS-COMPLETE
               GO TO RQP-999.
           IF INCR-INVOICE > WS-RANGE2
              MOVE "Y" TO WS-COMPLETE
              GO TO RQP-999.
           IF WS-PROF-TYPE = "R"
            IF INCR-TRANS = 3
               GO TO RQP-100.
           IF WS-PROF-TYPE = "P"
            IF INCR-TRANS = 4
               GO TO RQP-100.
           IF WS-PROF-TYPE = "Q"
            IF INCR-TRANS = 8
               GO TO RQP-100.
           GO TO RQP-055.
       RQP-100.
           IF WS-FOUND = " "
              MOVE "Y" TO WS-FOUND.
           MOVE INCR-INVOICE     TO WS-INVOICE.
           IF WS-ADD-TOGETHER = "Y"
               ADD INCR-ADDONS   TO WS-ADDONAMT
               ADD INCR-LINENO   TO SUB-20
           ELSE
               MOVE 0 TO WS-SUBTOTAL WS-INVOICETOTAL
               MOVE INCR-ADDONS  TO WS-ADDONAMT
               MOVE INCR-LINENO  TO SUB-20.
               
           IF WS-PRINT-NUM = 5
              PERFORM ENTER-EMAIL-ADDRESS.
       RQP-999.
           EXIT.
      *
       READ-STOCK-TRANSACTIONS SECTION.
       RSTT-010.
           READ STOCK-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              MOVE 0 TO STTR-TYPE
              GO TO RSTT-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS-RECORD BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STTRANS-ST1
              GO TO RSTT-010.
              
      *     MOVE STTR-REFERENCE1 TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE WS-INVOICE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR1-020.
              
           IF STTR-REFERENCE1 NOT = WS-INVOICE
           
      *        MOVE "STTR-REF NOT = INVOICE NUMBER" TO WS-MESSAGE
      *        PERFORM ERROR-MESSAGE
      *        MOVE STTR-KEY TO WS-MESSAGE
      *        PERFORM ERROR-MESSAGE
           
              GO TO RSTT-999.
              
           IF WS-INVCRED = "I"
            IF STTR-TYPE NOT = 1
              GO TO RSTT-010.
           IF WS-INVCRED = "C"
            IF STTR-TYPE NOT = 6
              GO TO RSTT-010.
              
           IF WS-INVCRED = "P"
            IF WS-PROF-TYPE = "R"
             IF STTR-TYPE NOT = 3
              GO TO RSTT-010.
           IF WS-INVCRED = "P"
            IF WS-PROF-TYPE = "P"
             IF STTR-TYPE NOT = 4
              GO TO RSTT-010.
           IF WS-INVCRED = "P"
            IF WS-PROF-TYPE = "Q"
             IF STTR-TYPE NOT = 8
              GO TO RSTT-010.
              
           PERFORM ERROR-020.
           MOVE STTR-STOCK-NUMBER TO B-STOCKNUMBER
           MOVE B-STOCKNUMBER     TO SPLIT-STOCK.
           
      *      MOVE STTR-STOCK-NUMBER TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
           IF SP-1STCHAR = "*"
               GO TO RSTT-030.
       RSTT-020.
           MOVE STTR-ORDERQTY    TO B-ORDERQTY
           MOVE STTR-SHIPQTY     TO B-SHIPQTY
           MOVE STTR-SHIPPEDQTY  TO B-SHIPPEDQTY
           MOVE STTR-DESC1       TO B-STOCKDESCRIPTION
           MOVE STTR-DESC2       TO B-STOCKDESCRIPTION2.
           MOVE STTR-PRICE       TO B-STOCKPRICE
           MOVE STTR-ITEMDISC    TO B-DISCOUNTPERITEM
           MOVE STTR-SALES-VALUE TO B-NETT
           MOVE STTR-UNIT        TO B-UNIT.

           IF WS-PRINT-NUM = 4 OR = 5
            IF WS-CURRENCY NOT = "ZAR"
              COMPUTE B-STOCKPRICE ROUNDED 
                                     = B-STOCKPRICE * WS-EXCHANGERATE
      *        COMPUTE STTR-SALES-VALUE ROUNDED =
      *                         (B-STOCKPRICE * B-SHIPQTY)
      *        MOVE STTR-SALES-VALUE TO B-NETT
           COMPUTE B-NETT = B-SHIPQTY *
              (B-STOCKPRICE - (B-STOCKPRICE * B-DISCOUNTPERITEM / 100)).

      *        COMPUTE B-NETT ROUNDED =
      *            B-NETT - (STTR-SALES-VALUE * B-DISCOUNTPERITEM).

           IF INCR-BO-INV-NO NOT = 0
      *       MOVE "   ORDER PLACED ON INTERNAL ORDER No:"
      *       TO WS-BO-MESSAGE
              MOVE "Y" TO WS-BO-FOUND.
           GO TO RSTT-999.
       RSTT-030.
           IF WS-INVCRED = "D" OR = "X"
             IF STTR-TYPE = 6
              GO TO RSTT-035.
           IF WS-INVCRED = "D" OR = "X"
             IF STTR-TYPE = 1
              GO TO RSTT-031.

           IF WS-INVCRED NOT = "I" AND NOT = "P"
              GO TO RSTT-035.
       RSTT-031.
           MOVE B-STOCKNUMBER TO C-NUM
           MOVE COM-ORDERQTY  TO C-ORDER
           MOVE COM-SHIPQTY   TO C-SHIP
           MOVE COM-DESC      TO C-DESC
           MOVE COM-UNIT      TO C-UNIT
           MOVE COM-PRICE     TO C-PRICE
           MOVE COM-COST      TO C-COST
           MOVE COM-DISC      TO C-DISC
           MOVE " "           TO C-REST
           GO TO RSTT-999.
       RSTT-035.
           MOVE B-STOCKNUMBER TO C-CR-NUM
           MOVE COM-SHIPQTY   TO C-CR-SHIP
           MOVE COM-DESC      TO C-CR-DESC
           MOVE COM-PRICE     TO C-CR-PRICE
           MOVE COM-COST      TO C-CR-COST
           MOVE COM-DISC      TO C-CR-DISC
           MOVE " "           TO C-CR-REST.
       RSTT-999.
           EXIT.
      *
       READ-COMM-FILE SECTION.
       RCOM-005.
            IF WS-INVCRED = "I" OR = "P"
                MOVE 1 TO PA-RECORD
            ELSE
                MOVE 2 TO PA-RECORD.
            MOVE 4 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RCOMM-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE " " TO WS-SPEC-COMMENT
               GO TO RCOMM-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER COMM BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RCOMM-010.
       RCOMM-999.
            MOVE COMM-DESC TO WS-SPEC-COMMENT.
       RCOMM-9999.
            EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-010.
           MOVE " " TO BODY-LINE
                       C-CR-LINE
                       C-LINE.
       CF-999.
           EXIT.
      *
       READ-DEBTOR SECTION.
       RD-000.
           MOVE INCR-ACCOUNT TO DR-ACCOUNT-NUMBER.
           READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "*** UNKNOWN DEBTOR ***" TO DR-ACC-EMAIL
               GO TO RD-010.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-000.
           IF DR-ACC-EMAIL = " "
               PERFORM ENTER-EMAIL-ADDRESS
               GO TO RD-999.
       RD-010.
           MOVE DR-ACC-EMAIL TO WS-EMAIL-NUMBER.
       RD-999.
           EXIT.
      *
       ENTER-EMAIL-ADDRESS SECTION.
       EEA-005.
           PERFORM CLEAR-010.
           MOVE 0 TO WS-SPACE-CNT.
           
           MOVE 2910 TO POS
           DISPLAY 
           "TRANS        DOES NOT HAVE AN EMAIL ADDRESS, PLEASE ENTER."
              AT POS
           MOVE 2916 TO POS
           MOVE INCR-INVOICE TO WS-TRANS-DISPLAY
           DISPLAY WS-TRANS-DISPLAY AT POS
           MOVE 3010 TO POS
           DISPLAY 
           "EMail :[                                                  ]"
              AT POS.
              MOVE 3018 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 50        TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 17        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-EMAIL-NUMBER F-NAMEFIELD.

           MOVE FUNCTION LOWER-CASE(F-NAMEFIELD) TO WS-EMAIL-NUMBER
                                                    WS-EMAIL.
           INSPECT WS-EMAIL TALLYING WS-SPACE-CNT FOR CHARACTERS
               BEFORE INITIAL SPACE.
            
           IF WS-EMAIL(1:(WS-SPACE-CNT)) IS NOT WS-VALID-EMAIL
                MOVE "EMAIL ADDRESS HAS AN INVALID CHARACTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO EEA-005.
 
            PERFORM CHECK-EMAIL-FOR-VALIDITY.
            IF WS-ACC-ERROR = "Y"
                GO TO EEA-005.
            IF WS-SPACE-CNT < 10
                MOVE 
            "EMAIL ADDRESS INVALID AS IT'S TOO SHORT, 'ESC' TO RETRY." 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO EEA-005.
       EEA-999.
           EXIT.
      *
       CHECK-SPOOLER SECTION.
       CSP-000.
           IF WS-PRINT-NUM NOT = 4
               PERFORM OPEN-FILES
               PERFORM QUEUE-PRINT-FILE
          ELSE
               PERFORM A995-QUEUE-FSD
               GO TO CSP-999.
           MOVE SPACE TO W-SPOOLST
           MOVE SPACE TO W-SPOOLST2
           PERFORM CHECK-FOR-PAUSE.

           MOVE "     Load Invoices, Set Printer To 8inch, Then"
               TO WS-MESSAGE1.
           PERFORM ERROR2-000
           MOVE "  Switch Printer 'OFF And ON', Then Press 'ESC'"
               TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.

           PERFORM SEND-CONTROL-CHAR.
           MOVE 3020 TO POS.
           IF WS-INVCRED NOT = "C"
            DISPLAY "Printing of Invoices in progress ......." AT POS
           ELSE
            DISPLAY "Printing of Credits in progress ......." AT POS.
      *
      * PRINTING COMPLETE
      *
           PERFORM CHECK-PAUSE-PRINT
           MOVE 3020 TO POS
           DISPLAY "                                          " AT POS.
       
           MOVE "    Load Normal Paper, Set Printer To 11inch Then"
               TO WS-MESSAGE1.
           PERFORM ERROR2-000
           MOVE "  Switch Printer 'OFF And ON' Then Press 'ESC'"
               TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
       
           PERFORM SEND-CONTROL-CHAR.
       CSP-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           READ PARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO PARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RP-000.
       RP-999.
           EXIT.
      *
       CHECK-EMAIL-FOR-VALIDITY SECTION.
       CEFV-005.
             MOVE 0           TO SUB-15.
             MOVE SPACES      TO ALPHA-RATE
             MOVE F-NAMEFIELD TO ALPHA-RATE.
             MOVE "N"         TO WS-ACC-ERROR.
       CEFV-010.
             ADD 1 TO SUB-15.
             IF SUB-15 > 42
                MOVE "Y" TO WS-ACC-ERROR
                GO TO CEFV-900.
             IF AL-RATE (SUB-15) = "@"
                MOVE 0 TO SUB-15
                GO TO CEFV-020.
             GO TO CEFV-010.
       CEFV-020.
             ADD 1 TO SUB-15.
             IF SUB-15 > 42
                MOVE "Y" TO WS-ACC-ERROR
                GO TO CEFV-900.
             IF AL-RATE (SUB-15) = "."
                GO TO CEFV-025.
             GO TO CEFV-020.
       CEFV-025.
      *ADDED THIS NEXT LINE SO THAT WE DON'T CHECK FOR AN EXTRA . OR COM
             GO TO CEFV-999.
       
             ADD 1 TO SUB-15.
             IF AL-RATE (SUB-15) = "c"
                GO TO CEFV-026
             ELSE
                SUBTRACT 1 FROM SUB-15
                GO TO CEFV-030.
             MOVE "Y" TO WS-ACC-ERROR.
       CEFV-026.
             ADD 1 TO SUB-15.
             IF AL-RATE (SUB-15) = "o"
                GO TO CEFV-027.
             SUBTRACT 2 FROM SUB-15
             GO TO CEFV-030.
       CEFV-027.
             ADD 1 TO SUB-15.
             IF AL-RATE (SUB-15) = "m"
                GO TO CEFV-040.
             SUBTRACT 3 FROM SUB-15.
       CEFV-030.
             ADD 1 TO SUB-15.
             IF SUB-15 > 42
                MOVE "Y" TO WS-ACC-ERROR
                GO TO CEFV-900.
             IF AL-RATE (SUB-15) = "."
                GO TO CEFV-040.
             GO TO CEFV-030.
        CEFV-040.
             MOVE "N" TO WS-ACC-ERROR
             GO TO CEFV-999.
       CEFV-900.
           MOVE
          "THERE IS AN ERROR IN THE EMAIL ADDRESS ENTERED, PLEASE" &
          " FIX, 'ESC' TO RETRY."
            TO WS-MESSAGE
            PERFORM ERROR-MESSAGE.
       CEFV-999.
           EXIT.
      *
       OPEN-DATA-FILES SECTION.
       OPEN-000.
           PERFORM GET-SYSTEM-Y2K-DATE.
       OPEN-001.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0 
              MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DEBTOR-ST1
              GO TO OPEN-001.
       OPEN-005.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0 
              MOVE "STOCK TRANS. FILE BUSY ON OPEN, 'ESC' TO  RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STTRANS-ST1
              GO TO OPEN-005.
       OPEN-007.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0 
              MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO  RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-INCR-ST1
              GO TO OPEN-007.
       OPEN-008.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO  RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SLPARAMETER-ST1
              GO TO OPEN-008.
       OPEN-009.
            OPEN I-O CURRENCY-MASTER.
            IF WS-CURRENCY-ST1 NOT = 0
              MOVE "CURRENCY FILE BUSY ON OPEN, 'CANCEL TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CURRENCY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CURRENCY-ST1
              GO TO OPEN-009.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME
           MOVE "SlInCrRp"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       OPEN-FILES SECTION.
           MOVE SPACE TO W-QUEUEENTRYBUFFERSEND.
           MOVE X"00" TO W-FDELETEAFTERPROC
           MOVE 0     TO W-CBFORMNAME
           MOVE 0     TO W-CBWHEELNAME
           MOVE 1     TO W-BYTE1
           MOVE 0     TO W-BYTE2
           MOVE X"00" TO W-BPRINTMODE
           MOVE X"00" TO W-FALIGNFORM
           MOVE X"00" TO W-FSECURITYMODE
           MOVE "SPL" TO W-QUEUENAME
           MOVE 3     TO W-QUEUENAMELENGTH
           MOVE 0     TO W-QUEUEENTRYHANDLE
           MOVE 123   TO W-QUEUEENTRYBUFFERLENGTH
           MOVE 11    TO W-STATUSBLOCKLENGTH
           MOVE "PARALLELCONTROL" TO W-PAR-QUEUENAME
           MOVE 15                TO W-PAR-QUEUENAMELENGTH
           MOVE "SPOOLERSTATUS"   TO W-STATUS-QUEUENAME
           MOVE 13                TO W-STATUS-QUEUENAMELEN
           MOVE "PARALLEL"        TO W-PRINTERNAME
           MOVE 8                 TO W-PRINTERNAMELEN
           MOVE X"FF"       TO W-Q-NO-SERVER
           MOVE 5           TO W-PRIORITY
           MOVE 1           TO W-QUEUETYPE
           MOVE X"00000000" TO W-DATE-TIME
           MOVE 0           TO W-REPEATTIME
           MOVE 100         TO W-DELAY
           MOVE 0           TO W-ZERO
           MOVE 255         TO W-SPOOLST-LEN.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           CLOSE DEBTOR-MASTER
                 STOCK-TRANS-FILE 
                 INCR-REGISTER
                 PARAMETER-FILE
                 CURRENCY-MASTER.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *      
       Copy "CheckForPause".
       Copy "QueuePrintFileInvoice".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldNumDec".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "CheckDataNames".
       Copy "QueuePrintLaserInvCred".
       Copy "Z1LaserHeadings".
       Copy "Z1EMailHeadings".
       Copy "GetSystemY2KDate".
       Copy "OrderPassword".
       Copy "CTOSCobolAccept".
       Copy "SetupInvoiceForPDF".
       Copy "SetupInvoiceForPDFOnly".
       Copy "SetupCreditForPDF".
       Copy "SetupCreditForPDFOnly".
       Copy "DeleteBlankEmailInvRecord".
       Copy "DeleteBlankEmailCrnRecord".
       Copy "MoveEmailRecordFromEimage".
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
       Copy "Error2Message".
      *
      * END-OF-JOB
