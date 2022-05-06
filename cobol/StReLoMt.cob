        IDENTIFICATION DIVISION.
        PROGRAM-ID. StReLoMt.
        AUTHOR.    CHRISTENSEN.
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
          Copy "SelectStMaster".
          Copy "SelectSlParameter".
          Copy "SelectStReceipt".
          Copy "SelectStOrders".
          Copy "SelectCrMaster".
          Copy "SelectCrJrn".
          Copy "SelectGlMaster".
          Copy "SelectGlParameter".
          Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LABEL-FILE ASSIGN TO WS-LABELPRINTER
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SPL-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdDaily.
           COPY ChlfdParam.
           COPY ChlfdOutOrd.
           COPY ChlfdStkReceipts.
           COPY ChlfdCrJrn.
           COPY ChlfdCreditor.
           COPY ChlfdGlParam.
           COPY ChlfdGlMast.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(80).
       FD  LABEL-FILE.
       01  LABEL-REC.
           03  FILLER           PIC X(250).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-LABELPRINTER      PIC X(100) VALUE " ".
       77  WS-DOTPRINTER        PIC X(100) VALUE " ".
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  PAGE-CNT             PIC 9(2) VALUE 0.
       77  WS-READS             PIC 9(2) VALUE 0.
       77  SLIP-LINE            PIC 9(2) VALUE 66.
       77  SLIP-CNT             PIC 9(2) VALUE 0.
       77  WS-DIVIDE-ACCEPT     PIC X(3) VALUE " ".
       77  WS-DIVIDE-BY         PIC 9(3) VALUE 0.
       77  WS-VAT-AMT           PIC 9(8)V99 VALUE 0.
       77  WS-TYPE              PIC X VALUE " ".
       77  WS-NEWINVOICE        PIC X VALUE " ".
       77  WS-STAFF-SALE        PIC X VALUE " ".
       77  WS-CANCEL            PIC X VALUE " ".
       77  WS-PRINT-COSTS       PIC X VALUE " ".
       77  WS-YN                PIC X VALUE " ".
       77  WS-EMAIL-FAX         PIC X VALUE " ".
       77  WS-CRJRN-INPUT-ONLY  PIC X VALUE " ".
       77  WS-NEXT              PIC X VALUE " ".
       77  WS-OLD-ORDER         PIC X VALUE " ".
       77  WS-PRICE             PIC 9(8)V99 VALUE 0.
       77  WS-NEWCOST           PIC S9(8)V99 VALUE 0.
       77  WS-GLDISC            PIC S9(8)V99 VALUE 0.
       77  WS-GLDISC-DIFF       PIC S9(8)V99 VALUE 0.
       77  WS-STAFFCOST         PIC 9(8)V99 VALUE 0.
       77  WS-TOTALPRICE        PIC 9(8)V99 VALUE 0.
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-AVERAGECOST       PIC 9(8)V99 VALUE 0.
       77  WS-PORDER-PROGRAM     PIC X(8) VALUE "StOrStIq".
       77  WS-PORDER-PRN-PROGRAM PIC X(8) VALUE "StOrPrRp".
       77  WS-PORDERINQ-PROGRAM  PIC X(8) VALUE "StOrOrIq".
       77  WS-STOCK-INQUIRY      PIC X(8) VALUE "StMastIq".
       77  WS-INQUIRY-PROGRAM    PIC X(8) VALUE "StMastIq".
       77  WS-PERCENT           PIC 9(2)V99 VALUE 0.
       77  WS-QTY               PIC 9(5) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-PARAM             PIC X(34) VALUE " ".
       77  WS-SUPPLIER          PIC X(7) VALUE " ".
       77  WS-SUPPLIER-AMOUNT   PIC S9(8)V99 VALUE 0.
       77  WS-TOTAL             PIC S9(8)V99 VALUE 0.
       77  WS-SUPPLIER-ANSWER   PIC X(1) VALUE " ".
       77  WS-CONFIRMED         PIC X(1) VALUE " ".
       77  WS-PORD              PIC X(20) VALUE " ".
       77  WS-COPIES            PIC 9 VALUE 0.
       77  WS-SLIP-COPIES       PIC 9 VALUE 0.
       77  WS-ACCEPT-COPIES     PIC X VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-INVNO             PIC X(10) VALUE " ".
       77  WS-STTRANSNO         PIC 9(6) VALUE 0.
       77  WS-STOCKNUMBER       PIC X(34) VALUE " ".
       77  WS-ORDERNUMBER       PIC X(20) VALUE " ".
       77  WS-CHANGE-ORDER      PIC X VALUE " ".
       77  WS-DELIVERY          PIC X VALUE " ".
       77  WS-DELVIA            PIC X(20) VALUE " ".
       77  WS-ACCEPT-DATE       PIC X(10) VALUE " ".
       77  WS-DUEDATE           PIC 9(8) VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-ST-PRINT-LABELS   PIC X VALUE " ".
       77  WS-1STPRINT          PIC X VALUE " ".
       77  WS-STOCK-STORE       PIC X(15) VALUE " ".
       77  WS-BODY-LINE         PIC ZZ9.
       77  WS-ORDERNOTFOUND     PIC X VALUE " ".
       77  WS-DEL-SUB           PIC 9 VALUE 0.
       77  WS-END-OF-FILE       PIC X VALUE " ".
       77  WS-QTY-EACH-ITEM     PIC 9(5) VALUE 0.
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-START-POS         PIC 9 VALUE 0.
       77  PSW-SUB1             PIC S9(5) VALUE 0.
       77  PSW-SUB2             PIC S9(5) VALUE 0.
       77  WS-GRV-NUMBER        PIC 9(6).
       77  WS-CREDITOR          PIC 9(7).
       77  WS-CREDITOR-ACCEPT   PIC X(7) VALUE " ".
       77  WS-CURRENTGLPER      PIC 99 VALUE 0.
       77  WS-WRITE-MESSAGE     PIC X(60) VALUE " ".
       01  W-CRTSTATUS           PIC 9(4) value 0.
       01  W-READ-KEY           PIC X(11) VALUE " ".
       01  WS-COMMENT-LINE.
           03  WS-COMMENT       PIC X(60) OCCURS 3.
       01  STORE-DEL.
         02  WS-DEL-OCCUR OCCURS 10.
           03  WS-DEL-TYPE      PIC X.
           03  WS-DEL-CODE      PIC X.
           03  WS-DEL-TERM      PIC X(20).
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1        PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1  PIC 99.
       01  WS-STKRECEIPT-STATUS.
           03  WS-STKRECEIPT-ST1   PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1       PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1     PIC 99.
       01  WS-CRJRN-STATUS.
           03  WS-CRJRN-ST1        PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1       PIC 99.
       01  WS-Spl-STATUS.
           03  WS-Spl-ST1          PIC 99.
       01  LINE-DESCRIPTION.
           03  WS-CR1              PIC X(4).
           03  WS-CRACC            PIC X(8).
           03  WS-CR2              PIC X(3).
           03  WS-CRINV            PIC X(10).
       01  WS-PASSWORD-KEY.
           03  WS-PA-KEY         PIC X OCCURS 11.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST     PIC X(20) VALUE " ".
           03  WS-DAILY-2ND     PIC X(20) VALUE " ".
           03  WS-DAILY-3RD     PIC X(20) VALUE " ".
           03  WS-DAILY-4TH     PIC X(20) VALUE " ".
       01  WS-ORDER-CHECK.
           03  WS-O-C           PIC X OCCURS 20.
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 81.
               05  B-STOCKNUMBER      PIC X(15).
               05  B-DESCRIPTION      PIC X(20).
               05  B-DESCRIPTION2     PIC X(20).
               05  B-CURRENCY         PIC X(3).
               05  B-QUANTITY         PIC S9(5).
               05  B-UNITPRICE        PIC 9(8)V99.
               05  B-TOTALPRICE       PIC 9(8)V99.
               05  B-DISC             PIC 9(2)V99.
               05  B-REFNO            PIC X(20).
               05  B-DATE             PIC 9(8).
               05  B-FOR-LOC          PIC X.
       01  WS-ORDER-REFERENCE.
           03  FILLER                 PIC X(3) VALUE "P/O".
           03  WS-ORDERREF            PIC 9(6).
           03  FILLER                 PIC X VALUE ".".
           03  WS-ORDER-MM            PIC 99.
           03  FILLER                 PIC X VALUE ".".
           03  WS-ORDER-YY            PIC 9999.
       01  WS-TRANSACTION-DESCRIPTIONS.
           03  FILLER          PIC X(20) VALUE "RECEIPTS NOT ORDERED".
           03  FILLER          PIC X(20) VALUE "RECEIPTS ORDERED".
           03  FILLER          PIC X(20) VALUE "ORDERS ON SUPPLIERS".
           03  FILLER          PIC X(20) VALUE "RETURNS TO SUPPLIERS".
           03  FILLER          PIC X(20) VALUE "STOCK ADJUSTMENTS".
           03  FILLER          PIC X(20) VALUE "CASH SALES".
           03  FILLER          PIC X(20) VALUE "STOCK LABEL PRINTING".
           03  FILLER          PIC X(20) VALUE "RESERVE TO ONHAND".
           03  FILLER          PIC X(20) VALUE "TOOLKIT ADJUSTMENTS".
       01  WS-TRANS-DESCRED REDEFINES WS-TRANSACTION-DESCRIPTIONS.
           03  WS-TRANS-DESC   PIC X(20) OCCURS 9.
       01  WS-MONTH-DESCRIPTIONS.
           03  FILLER          PIC X(3) VALUE "JAN".
           03  FILLER          PIC X(3) VALUE "FEB".
           03  FILLER          PIC X(3) VALUE "MAR".
           03  FILLER          PIC X(3) VALUE "APR".
           03  FILLER          PIC X(3) VALUE "MAY".
           03  FILLER          PIC X(3) VALUE "JUN".
           03  FILLER          PIC X(3) VALUE "JUL".
           03  FILLER          PIC X(3) VALUE "AUG".
           03  FILLER          PIC X(3) VALUE "SEP".
           03  FILLER          PIC X(3) VALUE "OCT".
           03  FILLER          PIC X(3) VALUE "NOV".
           03  FILLER          PIC X(3) VALUE "DEC".
       01  WS-MONTH-DESC REDEFINES WS-MONTH-DESCRIPTIONS.
           03  WS-MM-DESC      PIC X(3) OCCURS 12.
       01  WS-PRINTER-INFO.
           03  WS-PRN-FIL     PIC X(8) VALUE " ".
           03  WS-PRN-NAME    PIC X(25) VALUE " ".
       01  JOURNAL-DATA.
           03  WS-JRN.
               05  WS-JRN-1STCHAR   PIC X(2) VALUE "PI".
               05  WS-JRN-REST      PIC X(3).
               05  WS-JRN-WEEK      PIC X(2).
               05  WS-FILLER        PIC X(3) VALUE " ".
           03  WS-CRJRN-TRANS       PIC 9(6) VALUE 0.
           03  WS-CRJRN-TYPE        PIC 99 VALUE 0.
           03  WS-INV-TOTAL         PIC 9(3).
           03  WS-CURRENTPER        PIC 99 VALUE 0.
           03  WS-CRJRN-FUTURE      PIC X VALUE " ".
           03  WS-VAT-PERC          PIC 99V99.
           03  WS-INV-AMT           PIC S9(8)V99.
           03  WS-UNAPPLIED         PIC S9(8)V99.
           03  WS-FORAMT            PIC S9(8)V99.
           03  WS-SETT-DISC         PIC S9(8)V99.
           03  WS-INV-NO            PIC X(10).
           03  WS-DNOTE-NO          PIC X(10).
           03  WS-FREIGHT           PIC S9(8)V99.
       01  SLIP-HEAD1.
           03  FILLER          PIC X(5) VALUE "DATE".
           03  SO1-DATE        PIC X(10).
           03  FILLER          PIC X(10) VALUE " ".
           03  SO1-NAME        PIC X(47) VALUE " ".
           03  FILLER          PIC X(5) VALUE "PAGE:".
           03  SO1-PAGE        PIC Z9.
       01  SLIP-HEAD2.
           03  FILLER          PIC X(25) VALUE " ".
           03  SO2-PIC1        PIC X(33) VALUE " ".
           03  SO2-PIC2        PIC X(7) VALUE " ".
           03  FILLER          PIC X(15) VALUE " ".
       01  SLIP-HEAD3.
           03  SO3-PIC1        PIC X(46) VALUE " ".
           03  SO3-PIC2        PIC X(34) VALUE " ".
       01  SLIP-HEAD4.
           03  H4-NAME.
               05  H4-ADD      PIC X(46).
               05  H4-DEL      PIC X(34).
       01  SLIP-HEAD7.
           03  SO7-FIL1             PIC X.
           03  SO7-SUP-FIL          PIC X(10).
           03  SO7-SUPPLIER         PIC X(15).
           03  SO7-FIL2             PIC X.
           03  SO7-DEL-FIL          PIC X(8).
           03  SO7-DEL-VIA          PIC X(20).
           03  SO7-FIL3             PIC X.
           03  SO7-PO-INFO.
               05  SO7-PO-FIL           PIC X(7).
               05  FILLER               PIC X(2) VALUE " ".
               05  SO7-PO-NUM           PIC X(14).
           03  SO7-FIL4             PIC X.
       01  SLIP-HEAD8.
           03  FILLER          PIC X(16) VALUE "STOCK NUMBER".
           03  FILLER          PIC X(44) VALUE "DESCRIPTION".
           03  FILLER          PIC X(6) VALUE "QTY".
           03  FILLER          PIC X(9) VALUE "PER EACH".
           03  SL8-DISC        PIC X(5) VALUE " DISC".
           03  FILLER          PIC X(3) VALUE " ".
       01  SLIP-DETAIL.
           03  S-STOCKNO       PIC X(16).
           03  S-DESC1         PIC X(20).
           03  S-DESC2         PIC X(21).
           03  S-QTY           PIC Z(5)9.
           03  S-UNIT          PIC Z(7)9.99 BLANK WHEN ZERO.
           03  FILLER          PIC X.
           03  S-DISC          PIC Z9.99 BLANK WHEN ZERO.
       01  SLIP-TOTAL.
           03  FILLER          PIC X(42) VALUE " ".
           03  SLIP-TOT-COM    PIC X(21) VALUE " ".
           03  TOT-GRV         PIC Z(7)9.99 BLANK WHEN ZERO.
           03  FILLER          PIC X(6) VALUE " ".
       01  SLIP-COMMENT.
           03  SLIP-COMM-LINE.
               05  SLIP-FILL   PIC X(19) VALUE " ".
               05  SLIP-COMM   PIC X(61) VALUE " ".
       01  PLINE1.
         02  PLINE1-REC OCCURS 4.
           03  P1-COMP          PIC X(1) VALUE " ".
           03  P-CONAME         PIC X(49) VALUE " ".
       01  PLINE2.
         02  PLINE2-REC OCCURS 4.
           03  P2-BOLD          PIC X.
           03  P-STOCK          PIC X(15) VALUE " ".
           03  P2-UNBOLD        PIC X.
           03  FILLER           PIC X(4) VALUE " ".
           03  P2-COMP          PIC X(1) VALUE " ".
           03  P-DATE           PIC X(15) VALUE " ".
       01  PLINE3.
         02  PLINE3-REC OCCURS 4.
           03  P-DESC1          PIC X(20) VALUE " ".
           03  P-DESC2          PIC X(29) VALUE " ".
       01  PLINE4.
         02  PLINE4-REC OCCURS 4.
           03  P-BINDESC        PIC X(6) VALUE " ".
           03  P-BIN            PIC X(5) VALUE " ".
           03  P-UNITDESC       PIC X(5) VALUE " ".
           03  P-UNIT           PIC X(10) VALUE " ".
           03  P-CAT            PIC X(4) VALUE " ".
           03  P-INVNO          PIC X(19) VALUE " ".
       Copy "WsDateInfo".
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
           PERFORM CLEAR-SCREEN.
           MOVE 320 TO POS
           DISPLAY "** STOCK RECEIPTS PROGRAM **" AT POS
           MOVE 420 TO POS
           DISPLAY "****************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
           MOVE WS-PRINTER TO WS-DOTPRINTER.
           MOVE 2510 TO POS
           DISPLAY "Program loading....." AT POS.
       CONTROL-010.
           MOVE "StReLoMt"      TO F-FORMNAME
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-015.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           MOVE "StReLoMt"      TO F-FORMNAME
           PERFORM OPEN-060 THRU OPEN-900.
           GO TO CONTROL-015.
       CONTROL-020.
           Move 9 To Ws-PrinterNumber (21)
           Move 9 To Ws-PrinterType (21).
           Copy "PrinterStSpecial".
           MOVE WS-LABELPRINTER TO WS-PRINTER.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
           PERFORM CLEAR-FIELDS
           PERFORM CLEAR-CRJRN-FIELDS
           PERFORM DISPLAY-FORM.
           MOVE "N" TO WS-CRJRN-INPUT-ONLY.
           MOVE 0 TO CRJRN-LOC-AMT
                     CRJRN-VAT-AMT
                     WS-TOTALPRICE
                     WS-TOTAL.
       GET-001.
            MOVE "Printer:"   TO WS-PRN-FIL
            MOVE Ws-Printer   To WS-PRN-NAME
            MOVE 0404 TO POS
            DISPLAY WS-PRINTER-INFO AT POS.
       GET-005.
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 3001 TO POS
           DISPLAY WS-MESSAGE AT POS.
        GET-010.
           MOVE 2810 TO POS
           DISPLAY "2 & <ALT-F8> TO FIND AN ORDER AND DISPLAY" AT POS.
           MOVE 2911 TO POS
           DISPLAY
           "Suppliers-Order Inq By : X=STOCK, P=PORDER," AT POS
           MOVE 3012 TO POS
           DISPLAY "BLANK=StockInq, OR Enter Numbers From 1-9."
              AT POS.

           MOVE "                   " TO F-NAMEFIELD.
           MOVE "TRANSCODE" TO F-FIELDNAME.
           MOVE 9 TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
           IF F-EXIT-CH = X"0B"
               GO TO GET-010.
           MOVE 1 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           IF F-NAMEFIELD = "X"
               CLOSE STOCK-MASTER
               CLOSE OUTSTANDING-ORDERS
               PERFORM CLEAR-SCREEN
               CALL WS-PORDER-PROGRAM USING WS-LINKAGE
               PERFORM CLEAR-SCREEN
               CANCEL WS-PORDER-PROGRAM
               PERFORM OPEN-000
               PERFORM OPEN-020
               PERFORM DISPLAY-FORM
               GO TO GET-010.
           IF F-NAMEFIELD = "P"
               CLOSE STOCK-MASTER
               CLOSE OUTSTANDING-ORDERS
               PERFORM CLEAR-SCREEN
               CALL WS-PORDERINQ-PROGRAM USING WS-LINKAGE
               PERFORM CLEAR-SCREEN
               CANCEL WS-PORDERINQ-PROGRAM
               PERFORM OPEN-000
               PERFORM OPEN-020
               PERFORM DISPLAY-FORM
               GO TO GET-010.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO STRE-TRANSACTION-CODE SUB-1.
           IF STRE-TRANSACTION-CODE = 0
               CLOSE STOCK-MASTER
               PERFORM CLEAR-SCREEN
               CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
               PERFORM CLEAR-SCREEN
               CANCEL WS-INQUIRY-PROGRAM
               PERFORM OPEN-000
               PERFORM DISPLAY-FORM
               GO TO GET-010.
           IF STRE-TRANSACTION-CODE = 1 OR = 2 OR = 3 OR = 4
                                 OR = 5 OR = 6 OR = 7 OR = 8 OR = 9
               GO TO GET-015.
           MOVE "INVALID TRANSACTION CODE !!" TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
           GO TO GET-000.
       GET-015.
           IF STRE-TRANSACTION-CODE = 7
            IF WS-ST-PRINT-LABELS = "N"
                MOVE
           "YOUR SYSTEM IS NOT SET UP TO PRINT LABELS, CANCEL TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-010.
           PERFORM ERROR-020
           PERFORM ERROR1-020
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS.

           MOVE "TRANSDESC" TO F-FIELDNAME.
           MOVE 9           TO F-CBFIELDNAME.
           MOVE WS-TRANS-DESC (STRE-TRANSACTION-CODE) TO F-NAMEFIELD.
           MOVE 20          TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-ALPHA.

           IF F-EXIT-CH NOT = X"87"
              PERFORM CHECK-PASSWORD.
           IF WS-PASSWORD-VALID = "N"
              GO TO GET-010.
       GET-020.
      ***************************************************************
      * F-EXIT-CH = X"87" = <CODE-CANCEL>; <ALT-F10>=X"9F" IN LINUX *
      * TO ENTER ONLY THE CRJRN                                     *
      * INFO WITHOUT ENTERING STOCK FIRST                           *
      *                                                             *
      * F-EXIT-CH = X"89" = <CODE-TAB>; <ALT-F8>=X"9D" IN LINUX     *
      * TO SEARCH FOR P/O NUMBER AND DISPLAY                        *
      ***************************************************************
           IF STRE-TRANSACTION-CODE = 1 OR 2
            IF F-EXIT-CH = X"87" OR = X"9F"
                MOVE "Y" TO WS-CRJRN-INPUT-ONLY
                PERFORM WRITE-CRJRN-INV-TRANS
                GO TO GET-999.
            IF STRE-TRANSACTION-CODE = 2
             IF F-EXIT-CH = X"89" OR = X"9D"
                 PERFORM GET-SUPPLIER-ORDER-OFF-SYSTEM
                 MOVE "8" TO WS-ABOVE-BODY.
            IF STRE-TRANSACTION-CODE = 3
                 PERFORM CHECK-FOREIGN-LOCAL.
            IF STRE-TRANSACTION-CODE = 6
                 PERFORM CHECK-FOR-STAFF-SALE.
            IF WS-ABOVE-BODY = "4"
                MOVE " " TO WS-ABOVE-BODY
                GO TO GET-000.
       GET-025.
            PERFORM FILL-BODY.
            IF WS-ABOVE-BODY = "1"
                MOVE " " TO WS-ABOVE-BODY
                GO TO GET-000.
            MOVE 1 TO SUB-1.
            IF STRE-TRANSACTION-CODE = 1 OR = 2
                PERFORM CHECK-ALL-ITEMS-VALID
             IF WS-ABOVE-BODY = "8"
                GO TO GET-025.
            IF STRE-TRANSACTION-CODE = 2 OR = 3
                PERFORM UPDATE-OUT-ORDERS.
            IF STRE-TRANSACTION-CODE = 4
                PERFORM UPDATE-RETURNS.
            IF STRE-TRANSACTION-CODE = 7
                MOVE "*REPRINT1*" TO WS-INV-NO
                PERFORM CLEAR-010
                PERFORM CONTROL-020
                PERFORM CONTROL-100
                PERFORM PRINT-LABELS
                GO TO GET-999.
            IF WS-ABOVE-BODY = "2"
                GO TO GET-020.
       GET-950.
            PERFORM UPDATE-STOCK.
            IF STRE-TRANSACTION-CODE = 4
               PERFORM PRINT-RETURN-SLIP
               PERFORM WRITE-CRJRN-INV-TRANS
               PERFORM UPDATE-OUT-ORDERS.
            IF STRE-TRANSACTION-CODE = 3
             IF WS-YN = "Y"
               PERFORM PRINT-ORDER-SLIP.
            IF STRE-TRANSACTION-CODE = 3
             IF WS-EMAIL-FAX = "Y"
               CLOSE STOCK-MASTER
               CLOSE OUTSTANDING-ORDERS
               PERFORM CLEAR-SCREEN
               MOVE WS-ORDERNUMBER TO WS-LINK-PORDER
               MOVE WS-CREDITOR    TO WS-LINK-ACCOUNT
               CALL WS-PORDER-PRN-PROGRAM USING WS-LINKAGE
               PERFORM CLEAR-SCREEN
               CANCEL WS-PORDER-PRN-PROGRAM
               MOVE " " TO WS-LINK-PORDER
               MOVE 0   TO WS-LINK-ACCOUNT
               PERFORM OPEN-000
               PERFORM OPEN-020
               PERFORM DISPLAY-FORM
               GO TO GET-999.
            IF STRE-TRANSACTION-CODE NOT = 1 AND NOT = 2
               GO TO GET-999.
            IF STRE-TRANSACTION-CODE = 1 OR = 2
                PERFORM WRITE-CRJRN-INV-TRANS.
            IF WS-ST-PRINT-LABELS = "N"
               GO TO GET-999.
       GET-960.
            IF STRE-TRANSACTION-CODE = 1 OR = 2
               MOVE " " TO WS-YN
               PERFORM CLEAR-010
               MOVE 2910 TO POS
               DISPLAY "DO YOU WISH TO PRINT STOCK LABELS: [ ]" AT POS
               ADD 36 TO POS

               MOVE ' '       TO CDA-DATA
               MOVE 1         TO CDA-DATALEN
               MOVE 26        TO CDA-ROW
               MOVE 45        TO CDA-COL
               MOVE CDA-GREEN TO CDA-COLOR
               MOVE 'F'       TO CDA-ATTR
               PERFORM CTOS-ACCEPT
               MOVE CDA-DATA TO WS-YN.

            IF WS-YN = "N"
               PERFORM ERROR1-020
               GO TO GET-999.
            IF WS-YN = "Y"
               PERFORM ERROR1-020
               PERFORM CONTROL-020
               PERFORM CONTROL-100
               PERFORM PRINT-LABELS
               GO TO GET-999.
            GO TO GET-960.
       GET-999.
            EXIT.
      *
       CHECK-ALL-ITEMS-VALID SECTION.
       CAIV-010.
           MOVE " " TO WS-ABOVE-BODY.
           MOVE 0   TO SUB-1.
       CAIV-015.
           IF SUB-1 < 80
              ADD 1 TO SUB-1.
           IF B-STOCKNUMBER (SUB-1) = " "
              GO TO CAIV-999.
           IF B-TOTALPRICE (SUB-1) = 0
           OR B-REFNO (SUB-1) = SPACES
             MOVE "8" TO WS-ABOVE-BODY
             MOVE 
          "YOU HAVE OMITTED TO EITHER ENTER OR CANCEL A LINE, RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO CAIV-999.
           GO TO CAIV-015.
       CAIV-999.
           EXIT.
      *
       CALC-POS-OF-CURSOR SECTION.
       CPOC-005.
             IF SUB-1SAVE < 11
                 GO  TO CPOC-500.
       CPOC-010.
            COMPUTE SUB-1 = SUB-1SAVE - F-INDEXSAVE.
            IF SUB-1 < 0
               MOVE 0 TO SUB-1.
            PERFORM SCROLL-NEXT.
       CPOC-500.
            MOVE SUB-1SAVE   TO SUB-1
            MOVE F-INDEXSAVE TO F-INDEX.
       CPOC-999.
           EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
             IF WS-ABOVE-BODY NOT = "8" 
                MOVE 1 TO SUB-1 SUB-25 F-INDEX
             ELSE
                MOVE 1 TO SUB-1 F-INDEX.
             IF WS-ABOVE-BODY = "2" OR = "8"
                MOVE " " TO WS-ABOVE-BODY
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS. 
             PERFORM GET-005.
       FILL-005.
            IF STRE-TRANSACTION-CODE = 3
               MOVE 1236 TO POS
               DISPLAY "Supply Disc" AT POS.

             MOVE 2910 TO POS.
             DISPLAY "    BODY LINE: " AT POS.
             ADD 16     TO POS.
             MOVE SUB-1 TO WS-BODY-LINE.
             DISPLAY WS-BODY-LINE AT POS.
             PERFORM SCROLL-010.
       FILL-010.
            MOVE 2710 TO POS
            DISPLAY
            "PRESS <ALT-Z> TO GO INTO ZOOMBOX MODE TO CALL UP STOCKINQ."
               AT POS.

            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            
            IF B-STOCKNUMBER (SUB-1) NOT = " "
             IF F-NAMEFIELD NOT = B-STOCKNUMBER (SUB-1)
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15                    TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM ERROR-020.
      *************
      * <UP-ARROW>*
      *************
            IF F-EXIT-CH = X"01"
             IF F-NAMEFIELD NOT = B-STOCKNUMBER (SUB-1)
              IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                MOVE 15  TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM ERROR-020
                GO TO FILL-010.
            IF F-EXIT-CH = X"01" AND F-INDEX = 1
                MOVE 3010 TO POS
                MOVE "N" TO WS-CANCEL
                DISPLAY 
           "ARE YOU SURE YOU WISH TO CANCEL YOUR SESSION ? Y OR N : [ ]"
                AT POS
                ADD 57 TO POS
                MOVE 'N'       TO CDA-DATA
                MOVE 1         TO CDA-DATALEN
                MOVE 27        TO CDA-ROW
                MOVE 66        TO CDA-COL
                MOVE CDA-GREEN TO CDA-COLOR
                MOVE 'F'       TO CDA-ATTR
                PERFORM CTOS-ACCEPT
                MOVE CDA-DATA TO WS-CANCEL
                MOVE X"01" TO F-EXIT-CH
              IF WS-CANCEL NOT = "Y"
                  PERFORM ERROR-020
                  GO TO FILL-010
              ELSE
                MOVE "1" TO WS-ABOVE-BODY
                  PERFORM ERROR-020
                GO TO FILL-999.
            IF F-EXIT-CH = X"01" AND F-INDEX > 1
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-005.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
      ***************
      * <DOWN-ARROW>*
      ***************
            IF F-EXIT-CH = X"0B"
             IF F-NAMEFIELD NOT = B-STOCKNUMBER (SUB-1)
              IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                MOVE 15  TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM ERROR-020
                GO TO FILL-010.
            IF F-EXIT-CH = X"0B" AND F-INDEX < 10
             IF F-NAMEFIELD NOT = "   "
                ADD 1 TO F-INDEX SUB-1
                GO TO FILL-005
             ELSE
                GO TO FILL-010.
            IF F-EXIT-CH = X"0B" AND F-INDEX = 10
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                PERFORM SCROLL-NEXT
                GO TO FILL-005
              ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15                    TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM SCROLL-NEXT
                GO TO FILL-005.
            IF F-EXIT-CH = X"0C"
                PERFORM SCROLL-NEXT-PAGE
                GO TO FILL-010.
            IF F-EXIT-CH = X"11"
                PERFORM SCROLL-NEXT
                GO TO FILL-010.
            IF F-EXIT-CH = X"05"
                PERFORM SCROLL-PREVIOUS
                GO TO FILL-010.
            IF F-EXIT-CH = X"13"
                PERFORM SCROLL-DOWN
                GO TO FILL-010.
      *******************************************
      *<CODE-NEXT-PAGE> TO READ NEXT STOCK ITEM.*
      *******************************************
            IF F-EXIT-CH = X"8C"
             IF SUB-1 NOT < SUB-25
                PERFORM READ-NEXT-STOCK-ITEM
             IF WS-STOCK-ST1 = 0
                PERFORM DISPLAY-LINE-ITEMS
                GO TO FILL-010
             ELSE
                GO TO FILL-010.
      ************************************************
      *<CODE-PREV-PAGE> TO READ PREVIOUS STOCK ITEM. *
      ************************************************
            IF F-EXIT-CH = X"85"
             IF SUB-1 NOT < SUB-25
                PERFORM READ-PREV-STOCK-ITEM
             IF WS-STOCK-ST1 = 0
                PERFORM DISPLAY-LINE-ITEMS
                GO TO FILL-010
             ELSE
                GO TO FILL-010.
      ******************
      * <TAB> CHARACTER*
      ******************
            IF F-EXIT-CH = X"09"
             IF F-NAMEFIELD = " "
                GO TO FILL-011.
            IF F-EXIT-CH = X"09"
             IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                GO TO FILL-011
             ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15                    TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM ERROR-020.
       FILL-011.
            IF F-EXIT-CH = X"09"
             IF STRE-TRANSACTION-CODE NOT = 1 AND NOT = 2 AND NOT = 4
                  PERFORM ERROR-020
                  GO TO FILL-999.
            IF F-EXIT-CH = X"09"
             IF STRE-TRANSACTION-CODE = 1 OR = 2 OR = 4
                MOVE 3010 TO POS
                MOVE "N" TO WS-CANCEL
                DISPLAY 
           "ARE YOU SURE THE RUNNING TOTAL IS EXACTLY = INVOICE AMT:[ ]"
                AT POS
                ADD 57 TO POS
                MOVE 'N'       TO CDA-DATA
                MOVE 1         TO CDA-DATALEN
                MOVE 27        TO CDA-ROW
                MOVE 66        TO CDA-COL
                MOVE CDA-GREEN TO CDA-COLOR
                MOVE 'F'       TO CDA-ATTR
                PERFORM CTOS-ACCEPT
                MOVE CDA-DATA TO WS-CANCEL
                MOVE X"09" TO F-EXIT-CH
              IF WS-CANCEL NOT = "Y"
                  PERFORM ERROR-020
                  GO TO FILL-010
              ELSE
                  PERFORM ERROR-020
                  GO TO FILL-999.
      ******************************************************
      * <CODE-ESC>=X"87" IN CTOS, <ALT-F10>=X"9F" IN LINUX *
      ******************************************************
           IF F-EXIT-CH = X"07"
               MOVE 
               "PRESS 'Alt-F10' TO CANCEL A LINE ITEM, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO  FILL-010.
            IF F-EXIT-CH = X"87" OR =X"9F"
                MOVE SUB-1 TO SUB-7
                PERFORM CANCEL-TRANSACTION
                MOVE 1 TO SUB-1
                          F-INDEX
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
              IF SUB-25 > 11
                SUBTRACT 5 FROM SUB-25
                MOVE SUB-25 TO SUB-1
                PERFORM SCROLL-NEXT
                ADD 5 TO SUB-25
                GO TO FILL-005
              ELSE
                GO TO FILL-005.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO B-STOCKNUMBER (SUB-1)
                                WS-STOCKNUMBER.
            IF B-STOCKNUMBER (SUB-1) = "INQ"
                CLOSE STOCK-MASTER
                PERFORM CLEAR-SCREEN
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                MOVE " " TO B-STOCKNUMBER (SUB-1)
                PERFORM DISPLAY-FORM
                MOVE "TRANSCODE"           TO F-FIELDNAME
                MOVE 9                     TO F-CBFIELDNAME
                MOVE 1                     TO F-CBFIELDLENGTH
                MOVE STRE-TRANSACTION-CODE TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                MOVE "TRANSDESC" TO F-FIELDNAME
                MOVE 9                     TO F-CBFIELDNAME
                MOVE WS-TRANS-DESC (STRE-TRANSACTION-CODE)
                     TO F-NAMEFIELD
                MOVE 20                    TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                PERFORM OPEN-000
                PERFORM CALC-POS-OF-CURSOR
                GO TO FILL-005.
      ***********************************************
      *ZOOMBOX MODE                                 *
      * <CODE-z> = X"FA"  <CODE-SHIFT-Z> = X"DA"    *
      ***********************************************
      *IN CTOS: <CODE-Z>;  IN LINUX: <ALT-Z>
           IF F-EXIT-CH = X"FA" OR = X"DA"
                MOVE SUB-1   TO SUB-1SAVE
                MOVE F-INDEX TO F-INDEXSAVE
                CLOSE STOCK-MASTER
                PERFORM CLEAR-SCREEN
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
      *          MOVE " " TO B-STOCKNUMBER (SUB-1)
                PERFORM DISPLAY-FORM
                MOVE "TRANSCODE"           TO F-FIELDNAME
                MOVE 9                     TO F-CBFIELDNAME
                MOVE 1                     TO F-CBFIELDLENGTH
                MOVE STRE-TRANSACTION-CODE TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                MOVE "TRANSDESC" TO F-FIELDNAME
                MOVE 9 TO F-CBFIELDNAME
                MOVE WS-TRANS-DESC (STRE-TRANSACTION-CODE)
                     TO F-NAMEFIELD
                MOVE 20 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                PERFORM OPEN-000
                PERFORM CALC-POS-OF-CURSOR
                GO TO FILL-005.
            IF F-NAMEFIELD = " "
                GO TO FILL-010.
       FILL-012.
            PERFORM READ-STOCK.
            MOVE ST-CURRENCY TO B-CURRENCY (SUB-1).
            IF ST-CURRENCY = " "
               MOVE "ZAR" TO B-CURRENCY (SUB-1).

            IF ST-DESCRIPTION1 = "NOT THERE!!!"
            MOVE " " TO B-STOCKNUMBER (SUB-1)
                       WS-STOCKNUMBER
                 GO TO FILL-010.
            MOVE "DESCRIPTION1"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE ST-DESCRIPTION1 TO F-NAMEFIELD
                                    B-DESCRIPTION (SUB-1)
            MOVE 20              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESCRIPTION2"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE ST-DESCRIPTION2 TO F-NAMEFIELD
                                    B-DESCRIPTION2 (SUB-1)
            MOVE 20 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            IF STRE-TRANSACTION-CODE = 6
              MOVE "UNITPRICE" TO F-FIELDNAME
              MOVE 9           TO F-CBFIELDNAME
             IF WS-STAFF-SALE = "N"
              MOVE ST-PRICE TO F-EDNAMEFIELD99Mil
                               B-UNITPRICE (SUB-1)
             ELSE              
               COMPUTE WS-STAFFCOST = ST-AVERAGECOST * 1.10
               MOVE WS-STAFFCOST TO F-EDNAMEFIELD99Mil
                                    B-UNITPRICE (SUB-1).
                                    
            IF STRE-TRANSACTION-CODE = 6
              MOVE 11 TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-99Mil
              
              PERFORM ERROR1-020
              MOVE 2610 TO POS
              DISPLAY WS-MESSAGE AT POS
              MOVE 2616 TO POS
              DISPLAY "ITEM COST :" AT POS
              ADD 11 TO POS
              MOVE ST-AVERAGECOST TO F-EDNAMEFIELD99Mil
              DISPLAY F-EDNAMEFIELD99Mil AT POS
              
             IF WS-STAFF-SALE = "N"
              MOVE 2516 TO POS
              DISPLAY "STAFF COST:" AT POS
              ADD 11 TO POS
              COMPUTE WS-STAFFCOST = ST-AVERAGECOST * 1.10
              MOVE WS-STAFFCOST TO F-EDNAMEFIELD99Mil
              DISPLAY F-EDNAMEFIELD99Mil AT POS
             ELSE
              MOVE 2514 TO POS
              DISPLAY "NORMAL PRICE:" AT POS
              ADD 13 TO POS
              MOVE ST-PRICE TO F-EDNAMEFIELD99Mil
              DISPLAY F-EDNAMEFIELD99Mil AT POS.
              
           IF STRE-TRANSACTION-CODE = 1 OR = 4
              MOVE "UNITPRICE" TO F-FIELDNAME
              MOVE 9           TO F-CBFIELDNAME
              MOVE ST-LASTCOST TO F-EDNAMEFIELD99Mil
                                  B-UNITPRICE (SUB-1)
              MOVE 11          TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-99Mil.
              
           IF STRE-TRANSACTION-CODE = 2
              MOVE "UNITPRICE" TO F-FIELDNAME
              MOVE 9           TO F-CBFIELDNAME
            IF B-UNITPRICE (SUB-1) = 0
              MOVE ST-LASTCOST TO F-EDNAMEFIELD99Mil
                                  B-UNITPRICE (SUB-1)
              MOVE 11          TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-99Mil
            ELSE
              MOVE B-UNITPRICE (SUB-1) TO F-EDNAMEFIELD99Mil
                                  
              MOVE 11                  TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-99Mil.
            
           IF STRE-TRANSACTION-CODE = 3
              MOVE "UNITPRICE" TO F-FIELDNAME
              MOVE 9           TO F-CBFIELDNAME
              MOVE ST-SUPPLIER TO WS-SUPPLIER
            IF WS-TYPE = "L"
             IF ST-SUPPLIERDISC > 0
              MOVE ST-PRICE    TO F-EDNAMEFIELD99Mil
                                  B-UNITPRICE (SUB-1)
              MOVE 11          TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-99Mil
             ELSE
              MOVE ST-LASTCOST TO F-EDNAMEFIELD99Mil
                                  B-UNITPRICE (SUB-1)
              MOVE 11          TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-99Mil.
              
           IF STRE-TRANSACTION-CODE = 3
            IF WS-TYPE = "L"
              MOVE "TOTALPRICE"    TO F-FIELDNAME
              MOVE 10              TO F-CBFIELDNAME
              MOVE ST-SUPPLIERDISC TO F-EDNAMEFIELD99Mil
                                      B-DISC (SUB-1)
              PERFORM WRITE-FIELD-99Mil
              
              PERFORM ERROR1-020
              MOVE 2601 TO POS
              DISPLAY WS-MESSAGE AT POS
              MOVE 2601 TO POS
              DISPLAY "SELLING PRICE:" AT POS
              ADD 14 TO POS
              MOVE ST-PRICE TO F-EDNAMEFIELD99Mil
              DISPLAY F-EDNAMEFIELD99Mil AT POS
              MOVE 2630 TO POS
              DISPLAY "SUPPLY DISC.:     %" AT POS
              ADD 13 TO POS
              MOVE ST-SUPPLIERDISC TO F-EDNAMEFIELDAMOUNTDIS
              DISPLAY F-EDNAMEFIELDAMOUNTDIS AT POS
              COMPUTE WS-NEWCOST = ST-PRICE - 
              ((ST-PRICE * ST-SUPPLIERDISC) / 100)
              MOVE 2653 TO POS
              DISPLAY "NEW COST:" AT POS
              ADD 9 TO POS
              MOVE WS-NEWCOST TO F-EDNAMEFIELD99mil
              DISPLAY F-EDNAMEFIELD99Mil AT POS
            ELSE
              MOVE ST-FOREIGNCOST TO B-UNITPRICE (SUB-1)
                                     F-EDNAMEFIELD99Mil
              MOVE 11             TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-99Mil
              
              MOVE "TOTALPRICE"    TO F-FIELDNAME
              MOVE 10              TO F-CBFIELDNAME
              MOVE ST-SUPPLIERDISC TO F-EDNAMEFIELD99Mil
                                      B-DISC (SUB-1)
              PERFORM WRITE-FIELD-99Mil

              MOVE 2630 TO POS
              DISPLAY "SUPPLY DISC.:     %" AT POS
              ADD 13 TO POS
              MOVE ST-SUPPLIERDISC TO F-EDNAMEFIELDAMOUNTDIS
              DISPLAY F-EDNAMEFIELDAMOUNTDIS AT POS.

            MOVE "REFERENCENO" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            SUBTRACT 1 FROM SUB-1.
            IF SUB-1 = 0
               MOVE 1 TO SUB-1
               GO TO FILL-013.
            MOVE B-REFNO (SUB-1) TO F-NAMEFIELD
                                    B-REFNO (SUB-1 + 1).
            ADD 1 TO SUB-1
            GO TO FILL-014.
       FILL-013.
            MOVE B-REFNO (SUB-1) TO F-NAMEFIELD
                                    B-REFNO (SUB-1).
       FILL-014.
            MOVE 20              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            MOVE "REFERENCEDATE" TO F-FIELDNAME
            MOVE 13              TO F-CBFIELDNAME.
            SUBTRACT 1 FROM SUB-1.
            IF SUB-1 = 0
               MOVE 1 TO SUB-1
               GO TO FILL-015.

            MOVE B-DATE (SUB-1)  TO B-DATE (SUB-1 + 1) SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE    TO F-NAMEFIELD
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            ADD 1 TO SUB-1
            GO TO FILL-020.
       FILL-015.
            MOVE WS-DATE         TO B-DATE (SUB-1) SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE    TO F-NAMEFIELD
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       FILL-020.
            MOVE "                          " TO F-NAMEFIELD
            MOVE "QUANTITY" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE 6          TO F-CBFIELDLENGTH
            PERFORM USER-FILL-FIELD.
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD        TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE       TO B-QUANTITY (SUB-1)
            MOVE B-QUANTITY (SUB-1) TO F-EDNAMEFIELDNUMNEG
            MOVE 6                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMNEG.

            IF F-EXIT-CH = X"01"
             IF B-QUANTITY (SUB-1) < 1
                MOVE SUB-1 TO SUB-7
                PERFORM CANCEL-TRANSACTION
                MOVE 1 TO SUB-1
                          F-INDEX
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
              IF SUB-25 > 11
                SUBTRACT 5 FROM SUB-25
                MOVE SUB-25 TO SUB-1
                PERFORM SCROLL-NEXT
                ADD 5 TO SUB-25
                GO TO FILL-005
              ELSE
                GO TO FILL-005.

            IF B-QUANTITY (SUB-1) = 0
                MOVE "QUANTITY MUST BE > THAN ZERO !!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-020.
            IF STRE-TRANSACTION-CODE = 1 OR = 2 OR = 3 OR = 4
                                  OR = 7
             IF NUMERIC-RATE < 0
               MOVE "QUANTITY CANNOT BE NEGATIVE FOR THIS TRANSACTION"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
             IF NUMERIC-RATE > 99999
               MOVE "QUANTITY CANNOT BE > 99999 FOR ANY TRANSACTION."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.

            IF F-EXIT-CH = X"01"
             IF STRE-TRANSACTION-CODE = 1 OR = 2 OR = 4 OR = 5
                                   OR = 6 OR = 8 OR = 9
              IF B-REFNO (SUB-1) > " "
                GO TO FILL-010
              ELSE
                MOVE "THE REFERENCE NUMBER FIELD MAY NOT BE BLANK."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO FILL-020.

           IF STRE-TRANSACTION-CODE = 5 OR = 8 OR = 9
            IF B-QUANTITY (SUB-1) < 0
                MOVE B-QUANTITY (SUB-1) TO WS-QTY
             IF WS-QTY > ST-QTYONHAND
                MOVE ST-QTYONHAND TO WS-WORK-FIELD
                MOVE ST-STOCKNUMBER   TO WS-DAILY-1ST
                MOVE "ON HAND IS =  " TO WS-DAILY-2ND
                MOVE WS-WORK-FIELD    TO F-EDNAMEFIELDNUM
                MOVE F-EDNAMEFIELDNUM TO WS-DAILY-3RD
                MOVE " "              TO WS-DAILY-4TH
                MOVE WS-DAILY-MESSAGE TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE " " TO WS-DAILY-MESSAGE
                GO TO FILL-020.
           IF STRE-TRANSACTION-CODE = 4 OR = 6
            IF B-QUANTITY (SUB-1) > ST-QTYONHAND
                MOVE ST-QTYONHAND TO WS-WORK-FIELD
                MOVE ST-STOCKNUMBER   TO WS-DAILY-1ST
                MOVE "ON HAND IS =  " TO WS-DAILY-2ND
                MOVE WS-WORK-FIELD    TO F-EDNAMEFIELDNUM
                MOVE F-EDNAMEFIELDNUM TO WS-DAILY-3RD
                MOVE " "              TO WS-DAILY-4TH
                MOVE WS-DAILY-MESSAGE TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE " " TO WS-DAILY-MESSAGE
                GO TO FILL-020.
           IF STRE-TRANSACTION-CODE = 8
            IF B-QUANTITY (SUB-1) > 0
             IF B-QUANTITY (SUB-1) > ST-QTYONRESERVE
                MOVE ST-QTYONRESERVE  TO WS-WORK-FIELD
                MOVE ST-STOCKNUMBER   TO WS-DAILY-1ST
                MOVE "ON RESERVE IS=" TO WS-DAILY-2ND
                MOVE WS-WORK-FIELD    TO F-EDNAMEFIELDNUM
                MOVE F-EDNAMEFIELDNUM TO WS-DAILY-3RD
                MOVE " "              TO WS-DAILY-4TH
                MOVE WS-DAILY-MESSAGE TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE " " TO WS-DAILY-MESSAGE
                GO TO FILL-020.
                
           IF STRE-TRANSACTION-CODE = 1 OR = 2
                GO TO FILL-040.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
             IF STRE-TRANSACTION-CODE = 5 OR = 8 OR = 9
               MOVE 0 TO B-UNITPRICE (SUB-1) B-TOTALPRICE (SUB-1)
               GO TO FILL-050.
            IF STRE-TRANSACTION-CODE = 7
               MOVE 0 TO B-UNITPRICE (SUB-1) B-TOTALPRICE (SUB-1)
               GO TO FILL-070.

            MOVE "UNITPRICE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO FILL-020.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF NUMERIC-RATE < 0
                MOVE "UNIT PRICE CANNOT BE NEGATIVE, PLEASE RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-030.
            MOVE NUMERIC-RATE TO B-UNITPRICE (SUB-1).
            IF B-UNITPRICE (SUB-1) = 0
                GO TO FILL-040.
                
            MOVE B-UNITPRICE (SUB-1)  TO F-EDNAMEFIELD99Mil
            MOVE 11                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.
            COMPUTE B-TOTALPRICE (SUB-1) =
                B-UNITPRICE (SUB-1) * B-QUANTITY (SUB-1).
            IF STRE-TRANSACTION-CODE NOT = 3
              MOVE "TOTALPRICE"         TO F-FIELDNAME
              MOVE 10                   TO F-CBFIELDNAME
              MOVE B-TOTALPRICE (SUB-1) TO F-EDNAMEFIELD99Mil
              MOVE 11                   TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-99Mil
            ELSE
              COMPUTE B-TOTALPRICE (SUB-1) =
                (B-UNITPRICE (SUB-1) -
                 (B-UNITPRICE (SUB-1) * B-DISC (SUB-1) /100)) *
                        B-QUANTITY (SUB-1).
            
            IF STRE-TRANSACTION-CODE = 3
             IF F-EXIT-CH = X"1D"
                GO TO FILL-040.
            IF STRE-TRANSACTION-CODE = 3
                GO TO FILL-070
            ELSE
                GO TO FILL-050.
       FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TOTALPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO FILL-030.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF NUMERIC-RATE < 0
                MOVE "TOTAL PRICE CANNOT BE NEGATIVE, PLEASE RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-040.
            IF STRE-TRANSACTION-CODE NOT = 3
               MOVE NUMERIC-RATE TO B-TOTALPRICE (SUB-1)
            ELSE
               MOVE NUMERIC-RATE TO B-DISC (SUB-1)
               MOVE "TOTALPRICE"        TO F-FIELDNAME
               MOVE 10                  TO F-CBFIELDNAME
               MOVE B-DISC (SUB-1)      TO F-EDNAMEFIELD99Mil
               MOVE 11                  TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-99Mil
 
             COMPUTE B-TOTALPRICE (SUB-1) =
                (B-UNITPRICE (SUB-1) -
                 (B-UNITPRICE (SUB-1) * B-DISC (SUB-1) /100)) *
                        B-QUANTITY (SUB-1)
               GO TO FILL-070.
               
            IF STRE-TRANSACTION-CODE NOT = 3
             IF B-TOTALPRICE (SUB-1) = 0
                MOVE "ONE OF THE PRICES MUST BE > ZERO !"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-030.
            MOVE B-TOTALPRICE (SUB-1) TO F-EDNAMEFIELD99Mil
            MOVE 11                   TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99Mil.
            COMPUTE B-UNITPRICE (SUB-1) ROUNDED =
                B-TOTALPRICE (SUB-1) / B-QUANTITY (SUB-1).
            MOVE "UNITPRICE"         TO F-FIELDNAME
            MOVE 9                   TO F-CBFIELDNAME
            MOVE B-UNITPRICE (SUB-1) TO F-EDNAMEFIELD99Mil
            MOVE 11                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.
       FILL-050.
            MOVE "                                " TO F-NAMEFIELD.
            MOVE "REFERENCENO" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF STRE-TRANSACTION-CODE = 5 OR = 6 OR = 8 OR = 9
             IF F-EXIT-CH = X"01"
                GO TO FILL-020.
            IF F-EXIT-CH = X"01"
                GO TO FILL-040.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO B-REFNO (SUB-1).
            IF B-REFNO (SUB-1) = "   "
                MOVE "YOU MUST ENTER A REFERENCE NUMBER IN THIS FIELD"
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO FILL-050.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"1D"
                GO TO FILL-060.
            GO TO FILL-070.
       FILL-060.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "REFERENCEDATE" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO FILL-050.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-060.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO B-DATE (SUB-1).
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-060.
       FILL-070.
            PERFORM ERROR1-020
            MOVE 2601 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 2620 TO POS
            DISPLAY WS-MESSAGE AT POS.

            MOVE 0 TO WS-QTY.
            IF STRE-TRANSACTION-CODE = 1 OR = 2 OR = 3 OR = 4 OR = 6
               PERFORM CHECK-TOTALS.
            ADD 1 TO SUB-1 F-INDEX.
            MOVE SUB-1 TO SUB-25.
            IF SUB-1 > 80
                MOVE "80 LINES ARE UP, PRESS 'ESC' TO TAB!" 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-999.
            IF F-INDEX < 11
                GO TO FILL-005.
            SUBTRACT 1 FROM SUB-1.
            PERFORM SCROLL-NEXT.
            GO TO FILL-005.
       FILL-999.
           EXIT.
      *
       PRINT-LABELS SECTION.
       PR-000.
           MOVE WS-LABELPRINTER TO WS-PRINTER-SAVE.
           PERFORM GET-USER-PRINT-NAME.
           MOVE WS-PRINTER TO WS-LABELPRINTER.
           OPEN OUTPUT LABEL-FILE.
           IF WS-Spl-ST1 NOT = 0
               MOVE "Print File Open error, 'ESC' To RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-LABELPRINTER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-PRINTER-SAVE TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SPL-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO PR-000.
       PR-001.
           MOVE " " TO WS-YN
                       WS-END-OF-FILE
           MOVE 2910 TO POS
           DISPLAY "WHAT STARTING POS FOR THE 1ST LABEL :[ ]" AT POS
           ADD 38 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-YN.

           IF WS-YN NOT = "1" AND NOT = "2" AND NOT = "3" AND NOT = "4"
              GO TO PR-001.
           MOVE WS-YN TO WS-START-POS.
           MOVE " " TO WS-YN.
       PR-002.
           MOVE 2910 TO POS
           DISPLAY "DIVIDE THE NO. OF LABELS BY A FACTOR:[ ]" AT POS
           ADD 38 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-YN.

           IF WS-YN NOT = "Y" AND NOT = "N"
              GO TO PR-002.
           IF WS-YN = "N"
              MOVE 1 TO WS-DIVIDE-BY
              GO TO PR-008.
       PR-003.
           MOVE 2910 TO POS
           DISPLAY "ENTER A NUMBER TO DIVIDE BY         :[   ]" AT POS
           ADD 38 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 3         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DIVIDE-ACCEPT.
           
           MOVE WS-DIVIDE-ACCEPT TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-DIVIDE-BY.
           DISPLAY WS-DIVIDE-BY AT POS.
           IF WS-DIVIDE-BY NOT > 0
              GO TO PR-003.
       PR-008.
           MOVE 1 TO SUB-4.
           MOVE 2910 TO POS
           DISPLAY "The LABEL REPORT IS being compiled........." AT POS.
           MOVE B-STOCKNUMBER (SUB-4) TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
           PERFORM ERROR-020.
       PR-010.
           IF WS-END-OF-FILE = " "
               PERFORM READ-MASTER.
       PR-020.
           WRITE LABEL-REC FROM  PLINE1 AFTER 1
           MOVE " " TO LABEL-REC PLINE1
           WRITE LABEL-REC FROM  PLINE2 AFTER 1
           MOVE " " TO LABEL-REC PLINE2
           WRITE LABEL-REC FROM  PLINE3 AFTER 1
           MOVE " " TO LABEL-REC PLINE3
           WRITE LABEL-REC FROM  PLINE4 AFTER 1
           MOVE " " TO LABEL-REC PLINE4
           WRITE LABEL-REC AFTER 2.
       PR-030.
           IF WS-END-OF-FILE = " "
              ADD 1 TO SUB-4
              GO TO PR-010.
       PR-900.
           MOVE 2510 TO POS
           DISPLAY "                                         " AT POS.
           CLOSE LABEL-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PR-999.
           EXIT.
      *
       READ-MASTER SECTION.
       RM-000.
           IF WS-START-POS NOT = 1
              PERFORM FILL-IN-BLANKS.
           MOVE WS-START-POS TO SUB-1
           MOVE 0            TO SUB-2.
       RM-010.
           IF B-STOCKNUMBER (SUB-4) = " "
              MOVE "1" TO WS-END-OF-FILE
              GO TO RM-999.
           MOVE B-STOCKNUMBER (SUB-4) TO ST-STOCKNUMBER.
           READ STOCK-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
              ADD 1 TO SUB-4
            IF B-STOCKNUMBER (SUB-4) = " "
              MOVE "1" TO WS-END-OF-FILE
              GO TO RM-999
            ELSE
              GO TO RM-010.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RM-010.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
      *********************************************************
      *NEW SECTION TO DIVIDE THE PRINTABLE QTY BY A FACTOR.   *
      *********************************************************
           COMPUTE WS-QTY-EACH-ITEM =
               B-QUANTITY (SUB-4) / WS-DIVIDE-BY.
      
      *     MOVE B-QUANTITY (SUB-4) TO WS-QTY-EACH-ITEM.
       RM-020.
           MOVE WS-PRINT-COMP     TO P1-COMP (SUB-1)
                                     P2-COMP (SUB-1)
           MOVE WS-CO-NAME        TO P-CONAME (SUB-1)
           
           MOVE WS-PRINT-BOLD     TO P2-BOLD (SUB-1)
           MOVE ST-STOCKNUMBER    TO P-STOCK (SUB-1)
           MOVE WS-PRINT-UNBOLD   TO P2-UNBOLD (SUB-1)
           MOVE DISPLAY-DATE      TO P-DATE (SUB-1)
           
           MOVE ST-DESCRIPTION1   TO P-DESC1 (SUB-1)
           MOVE ST-DESCRIPTION2   TO P-DESC2 (SUB-1)
           
           MOVE "BIN #:"          TO P-BINDESC (SUB-1)
           MOVE ST-BINLOCATION    TO P-BIN (SUB-1)
           MOVE "PER :"           TO P-UNITDESC (SUB-1)
           MOVE ST-UNITOFMEASURE  TO P-UNIT (SUB-1)
           MOVE ST-CATEGORY       TO P-CAT (SUB-1)
           MOVE WS-INV-NO         TO P-INVNO (SUB-1).
           
           IF SUB-1 < 4
              ADD 1 TO SUB-1
           ELSE
              PERFORM PR-020
              MOVE 1 TO SUB-1.
           ADD 1 TO SUB-2.

           IF SUB-2 = WS-QTY-EACH-ITEM
              MOVE 0 TO SUB-2
              ADD 1  TO SUB-4
              GO TO RM-010
           ELSE
              GO TO RM-020.
       RM-999.
           EXIT.
      *
       FILL-IN-BLANKS SECTION.
       FIB-005.
           MOVE 1 TO SUB-1.
       FIB-010.
           MOVE WS-PRINT-COMP     TO P1-COMP (SUB-1)
                                     P2-COMP (SUB-1)
           MOVE WS-PRINT-BOLD     TO P2-BOLD (SUB-1)
           MOVE WS-PRINT-UNBOLD   TO P2-UNBOLD (SUB-1)
           MOVE " "               TO P-CONAME (SUB-1)
                                     P-STOCK (SUB-1)
                                     P-DATE (SUB-1)
                                     P-DESC1 (SUB-1)
                                     P-DESC2 (SUB-1)
                                     P-BINDESC (SUB-1)
                                     P-BIN (SUB-1)
                                     P-UNITDESC (SUB-1)
                                     P-UNIT (SUB-1).
           
           ADD 1 TO SUB-1.
           IF SUB-1 < WS-START-POS
              GO TO FIB-010.
       FIB-999.
           EXIT.
      *
       CHECK-TOTALS SECTION.
       C-TOT-005.
           MOVE 0 TO SUB-10 WS-TOTALPRICE.
       C-TOT-010.
           IF SUB-10 < 80
              ADD 1 TO SUB-10
           ELSE
              GO TO C-TOT-020.
           IF B-TOTALPRICE (SUB-10) = 0
              GO TO C-TOT-020.
           ADD B-TOTALPRICE (SUB-10) TO WS-TOTALPRICE
           GO TO C-TOT-010.
       C-TOT-020.
           MOVE WS-TOTALPRICE TO WS-SUPPLIER-AMOUNT
                                  F-EDNAMEFIELDREC
           MOVE 2610 TO POS
           DISPLAY "TOTAL AMOUNT ENTERED:" AT POS
           ADD 22 TO POS
           DISPLAY B-CURRENCY (SUB-1) AT POS
           ADD 4 TO POS
           DISPLAY F-EDNAMEFIELDREC AT POS.
       C-TOT-999.
           EXIT.
      *
       READ-NEXT-STOCK-ITEM SECTION.
       RNSI-005.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               GO TO RNSI-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON READ-NEXT, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RNSI-005.
       RNSI-999.
           EXIT.
      *
       READ-PREV-STOCK-ITEM SECTION.
       RPREV-005.
           READ STOCK-MASTER PREVIOUS
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               GO TO RPREV-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD IN USE READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RPREV-005.
       RPREV-999.
           EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
             MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
             START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-010.
             READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                 PERFORM START-FOR-READ-NEXT
                 MOVE " " TO ST-DESCRIPTION2
                 MOVE "NOT THERE!!!" TO ST-DESCRIPTION1
                 MOVE 0 TO ST-PRICE
                           ST-AVERAGECOST
                           ST-DISCOUNT1
                 GO TO R-ST-999.
             IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK BUSY ON READ, 'ESC' TO RETRY."
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
       READ-STOCK-LOCK SECTION.
       RSL-000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       RSL-010.
           READ STOCK-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE 3010 TO POS
               DISPLAY "NO STOCK FILE TO BE UPDATED" AT POS
               ADD 30 TO POS
               DISPLAY WS-STOCKNUMBER AT POS
               CALL "LOCKKBD" USING F-FIELDNAME
               GO TO RSL-999.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RSL-010.
       RSL-999.
           EXIT.
      *
       START-FOR-READ-NEXT SECTION.
       SFRN-001.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
       SFRN-999.
             EXIT.
      *
       UPDATE-STOCK SECTION.
       UPST-000.
            MOVE 2910 TO POS
            DISPLAY "STOCK FILES BEING UPDATED.....                    "
            AT POS.
            MOVE 1 TO SUB-1.
       UPST-010.
            MOVE 0 TO WS-QTY.
            IF B-STOCKNUMBER (SUB-1) = " "
                GO TO UPST-990.
            MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
            PERFORM READ-STOCK-LOCK.

            IF STRE-TRANSACTION-CODE = 6
             IF B-QUANTITY (SUB-1) > ST-QTYONHAND
                MOVE ST-QTYONHAND        TO B-QUANTITY (SUB-1).
            IF STRE-TRANSACTION-CODE = 6
                SUBTRACT B-QUANTITY (SUB-1) FROM ST-QTYONHAND
                ADD B-QUANTITY (SUB-1)   TO ST-SALESUNITMTD
                                            ST-SALESUNITSYTD
                ADD B-TOTALPRICE (SUB-1) TO ST-SALESRANDSMTD
                                            ST-SALESRANDSYTD
                COMPUTE WS-AVERAGECOST =
                       ST-AVERAGECOST * B-QUANTITY (SUB-1)
                ADD WS-AVERAGECOST      TO ST-SALESCOSTMTD
                                           ST-SALESCOSTYTD
                GO TO UPST-900.

           IF STRE-TRANSACTION-CODE = 5 OR = 9
            IF B-QUANTITY (SUB-1) < 0
                MOVE B-QUANTITY (SUB-1) TO WS-QTY.
           IF STRE-TRANSACTION-CODE = 5
            IF WS-QTY > ST-QTYONHAND
                MOVE ST-QTYONHAND      TO WS-QTY
                SUBTRACT WS-QTY      FROM ST-QTYONHAND
                                          ST-QTYADJMTD
                                          ST-QTYADJYTD
                GO TO UPST-900
            ELSE
                ADD B-QUANTITY (SUB-1) TO ST-QTYONHAND
                                          ST-QTYADJMTD
                                          ST-QTYADJYTD
                GO TO UPST-900.
           IF STRE-TRANSACTION-CODE = 9
            IF WS-QTY > ST-QTYONHAND
                MOVE ST-QTYONHAND      TO WS-QTY
                SUBTRACT WS-QTY      FROM ST-QTYONHAND
                ADD WS-QTY             TO ST-SALESUNITMTD
                                          ST-SALESUNITSYTD
                GO TO UPST-900
            ELSE
                ADD B-QUANTITY (SUB-1)        TO ST-QTYONHAND
                SUBTRACT B-QUANTITY (SUB-1) FROM ST-SALESUNITMTD
                                                 ST-SALESUNITSYTD
                GO TO UPST-900.
           
      *TRANS FROM ONHAND TO RESERVE
           IF STRE-TRANSACTION-CODE = 8
            IF B-QUANTITY (SUB-1) < 0
                MOVE B-QUANTITY (SUB-1) TO WS-QTY
             IF WS-QTY > ST-QTYONHAND
                MOVE ST-QTYONHAND      TO WS-QTY
                SUBTRACT WS-QTY      FROM ST-QTYONHAND
                ADD WS-QTY             TO ST-QTYONRESERVE
                GO TO UPST-900
             ELSE
                ADD B-QUANTITY (SUB-1)        TO ST-QTYONHAND
                SUBTRACT B-QUANTITY (SUB-1) FROM ST-QTYONRESERVE
                GO TO UPST-900.
                
      *TRANS FROM RESERVE TO ONHAND
           IF STRE-TRANSACTION-CODE = 8
            IF B-QUANTITY (SUB-1) > 0
             IF B-QUANTITY (SUB-1) > ST-QTYONRESERVE
                MOVE ST-QTYONRESERVE   TO B-QUANTITY (SUB-1)
                MOVE 0                 TO ST-QTYONRESERVE
                ADD B-QUANTITY (SUB-1) TO ST-QTYONHAND
                GO TO UPST-900
              ELSE
                SUBTRACT B-QUANTITY (SUB-1) FROM ST-QTYONRESERVE
                ADD B-QUANTITY (SUB-1)        TO ST-QTYONHAND
                GO TO UPST-900.

           IF STRE-TRANSACTION-CODE = 3
                ADD B-QUANTITY (SUB-1) TO ST-QTYONORDER
                MOVE WS-DATE           TO ST-LASTORDERDATE
                GO TO UPST-900.
      *        IF WS-TYPE = "L"
      *          MOVE ST-LASTCOST TO B-UNITPRICE (SUB-1)
      *          GO TO UPST-900
      *        ELSE
      *          MOVE ST-FOREIGNCOST TO B-UNITPRICE (SUB-1)
      *          GO TO UPST-900.

           IF STRE-TRANSACTION-CODE = 4
                ADD B-QUANTITY (SUB-1) TO ST-QTYONORDER.
           IF STRE-TRANSACTION-CODE = 4
            IF B-QUANTITY (SUB-1) NOT > ST-QTYONHAND
                SUBTRACT B-QUANTITY (SUB-1) FROM ST-QTYONHAND
            ELSE
                MOVE ST-QTYONHAND TO B-QUANTITY (SUB-1)
                MOVE 0            TO ST-QTYONHAND.

            IF STRE-TRANSACTION-CODE = 4
                SUBTRACT B-QUANTITY (SUB-1) FROM ST-QTYRECMTD
                SUBTRACT B-QUANTITY (SUB-1) FROM ST-QTYRECYTD
                GO TO UPST-900.

            PERFORM REAVERAGE-COST.
            IF STRE-TRANSACTION-CODE = 1 OR = 2
             IF ST-QTYONRESERVE < ST-QTYONBORDER
                COMPUTE WS-QTY = (ST-QTYONBORDER - ST-QTYONRESERVE)
              IF WS-QTY > B-QUANTITY (SUB-1)
                ADD B-QUANTITY (SUB-1) TO ST-QTYONRESERVE
                                          ST-QTYRECMTD
                                          ST-QTYRECYTD
                GO TO UPST-850
              ELSE
                ADD B-QUANTITY (SUB-1) TO ST-QTYRECMTD
                                          ST-QTYRECYTD
                ADD WS-QTY             TO ST-QTYONRESERVE
                SUBTRACT WS-QTY       FROM B-QUANTITY (SUB-1)
                ADD B-QUANTITY (SUB-1) TO ST-QTYONHAND
                ADD WS-QTY             TO B-QUANTITY (SUB-1)
                GO TO UPST-850.
            IF STRE-TRANSACTION-CODE = 1 OR = 2
                ADD B-QUANTITY (SUB-1) TO ST-QTYONHAND
                                          ST-QTYRECMTD
                                          ST-QTYRECYTD
                GO TO UPST-850.

       UPST-850.
           MOVE WS-DATE TO ST-LASTRECEIPTDATE.
           IF STRE-TRANSACTION-CODE NOT = 2
                GO TO UPST-900.
           MOVE " " TO WS-ORDERNOTFOUND.
           MOVE B-STOCKNUMBER (SUB-1) TO OO-STOCK-NUMBER.
           READ OUTSTANDING-ORDERS
                INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
                MOVE "1" TO WS-ORDERNOTFOUND
                GO TO UPST-855.
           IF WS-OUTORD-ST1 NOT = 0
                MOVE "SORDERS BUSY ON READ - UPST-850, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO UPST-850.
       UPST-855.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
           IF WS-ORDERNOTFOUND = " "
            IF B-QUANTITY (SUB-1) < ST-QTYONORDER
                SUBTRACT B-QUANTITY (SUB-1) FROM ST-QTYONORDER
            ELSE
                MOVE 0 TO ST-QTYONORDER.
       UPST-900.
            MOVE " " TO WS-ORDERNOTFOUND.
            REWRITE STOCK-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE 3010 TO POS
                DISPLAY "STOCK RECORD NOT THERE!!" AT POS
                ADD 25 TO POS
                DISPLAY ST-STOCKNUMBER AT POS
                CALL "LOCKKBD" USING F-FIELDNAME
                GO TO UPST-950.
            IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK BUSY ON REWRITE UPST-900 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO UPST-900.
       UPST-950.
            PERFORM READ-PARAMETER-LOCK.
            MOVE PA-STOCK-RECEIPT-NUMBER TO STRE-TRANSACTION-NUMBER.
            IF STRE-TRANSACTION-CODE = 4
              IF WS-STTRANSNO = 0
               MOVE PA-STOCK-RECEIPT-NUMBER TO WS-STTRANSNO.
            ADD 1                        TO PA-STOCK-RECEIPT-NUMBER
            PERFORM REWRITE-PARAMETER
            MOVE B-STOCKNUMBER (SUB-1) TO STRE-STOCK-NUMBER
            MOVE B-QUANTITY (SUB-1)    TO STRE-QUANTITY
            MOVE B-UNITPRICE (SUB-1)   TO STRE-UNIT-PRICE
            MOVE B-TOTALPRICE (SUB-1)  TO STRE-TOTAL-PRICE
            MOVE B-REFNO (SUB-1)       TO STRE-REFERENCE-NO
            MOVE B-DATE (SUB-1)        TO STRE-REFERENCE-DATE
            MOVE WS-ORDERNUMBER        TO STRE-ORDER-NUMBER.
       UPST-960.
            WRITE STOCK-RECEIPTS-REC
                 INVALID KEY NEXT SENTENCE.
            IF WS-STKRECEIPT-ST1 = 23 OR 35 OR 49
                 GO TO UPST-010.
            IF WS-STKRECEIPT-ST1 NOT = 0
                MOVE "ST-RECEIPT BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STKRECEIPT-ST1
               GO TO UPST-960.
       UPST-970.
            ADD 1 TO SUB-1.
            IF SUB-1 < 81
                GO TO UPST-010.
       UPST-990.
            PERFORM ERROR1-020.
       UPST-999.
            EXIT.
      *
       REAVERAGE-COST SECTION.
       REAV-000.
           IF B-UNITPRICE (SUB-1) = 0
              COMPUTE WS-PRICE ROUNDED =
                  B-TOTALPRICE (SUB-1) / B-QUANTITY (SUB-1)
              MOVE WS-PRICE TO B-UNITPRICE (SUB-1)
           ELSE
              MOVE B-UNITPRICE (SUB-1) TO WS-PRICE.
           MOVE B-UNITPRICE (SUB-1)    TO ST-LASTCOST.

           IF ST-QTYONHAND > 0
              COMPUTE ST-AVERAGECOST ROUNDED =
                ((((ST-QTYONHAND + ST-QTYONRESERVE) * ST-AVERAGECOST) +
                (B-QUANTITY (SUB-1) * WS-PRICE)) /
                (ST-QTYONHAND + ST-QTYONRESERVE + B-QUANTITY (SUB-1)))
           ELSE
                MOVE WS-PRICE TO ST-AVERAGECOST.
       REAV-999.
            EXIT.
      *
       UPDATE-RETURNS SECTION.
       UPRN-000.
           MOVE " " TO WS-MESSAGE
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE " " TO WS-SUPPLIER
           PERFORM CLEAR-010.
           MOVE 2910 TO POS.
           DISPLAY "ENTER THE SUPPLIER: [       ]" AT POS.
           MOVE 3010 TO POS
           DISPLAY "(ON STOCK FILE)" AT POS
           MOVE 2931 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SUPPLIER.

           IF W-ESCAPE-KEY = 4
               MOVE "2" TO WS-ABOVE-BODY
               GO TO UPRN-999.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPRN-001
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPRN-000.
       UPRN-001.
           IF WS-SUPPLIER = " "
              GO TO UPRN-000.
           MOVE " " TO WS-MESSAGE.
           MOVE 3010 TO POS.
           DISPLAY WS-MESSAGE AT POS.

           PERFORM GET-CREDITOR-INFO.
               
           MOVE 2910 TO POS.
           MOVE " " TO WS-MESSAGE.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2910 TO POS.
           DISPLAY "ENTER THE ORIGINAL INVOICE NUMBER: [          ]"
            AT POS.
           MOVE 2946 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 45        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-INVNO.

           IF W-ESCAPE-KEY = 4
               GO TO UPRN-000.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPRN-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPRN-001.
       UPRN-010.
           IF WS-INVNO = " "
              GO TO UPRN-001.
           MOVE 2910 TO POS.
           MOVE " " TO WS-MESSAGE.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2910 TO POS.
           DISPLAY 
           "ENTER OUR PURCHASE ORDER NUMBER: [                    ]"
            AT POS.
           MOVE 2944 TO POS.
           
           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PORD.

           IF W-ESCAPE-KEY = 4
               GO TO UPRN-001.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               MOVE 1 TO SUB-1
               GO TO UPRN-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPRN-010.
       UPRN-020.
           IF WS-PORD = "      "
               GO TO UPRN-010.
           MOVE 2910 TO POS.
           MOVE " " TO WS-MESSAGE.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2810 TO POS.
           DISPLAY
           "ENTER THE REASON FOR RETURNING THE GOODS: (3 LINES)" AT POS.
           MOVE 2911 TO POS.
           DISPLAY "[" AT POS.
           ADD 61 TO POS.
           DISPLAY "]" AT POS.
           MOVE 1 TO SUB-1.
       UPRN-021.
           MOVE 2901 TO POS.
           DISPLAY "NO=" AT POS.
           ADD 4 TO POS.
           DISPLAY SUB-1 AT POS.
           MOVE 2912 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 60        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 11        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-WRITE-MESSAGE.

           MOVE WS-WRITE-MESSAGE TO WS-COMMENT (SUB-1).
           MOVE " " TO WS-WRITE-MESSAGE.
           IF W-ESCAPE-KEY = 4
            IF SUB-1 = 1
               GO TO UPRN-010
            ELSE
               SUBTRACT 1 FROM SUB-1
               GO TO UPRN-021.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPRN-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPRN-021.
       UPRN-025.
           IF WS-COMMENT (SUB-1) NOT = "     "
            IF SUB-1 < 3
              ADD 1 TO SUB-1
              GO TO UPRN-021.
           MOVE 1 TO SUB-1.
           MOVE 2601 TO POS.
           MOVE " " TO WS-MESSAGE.
           DISPLAY WS-MESSAGE AT POS.
           PERFORM ERROR1-020.
       UPRN-050.
           IF WS-COMMENT (SUB-1) = "      "
               GO TO UPRN-020.
           MOVE "                    " TO F-NAMEFIELD.
           MOVE 2910 TO POS.
           MOVE " " TO WS-MESSAGE.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2910 TO POS.
           DISPLAY "ENTER THE GRV AMOUNT: [                    ]"
            AT POS.
           MOVE 2933 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 32        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO F-NAMEFIELD.

           IF W-ESCAPE-KEY = 4
               GO TO UPRN-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPRN-055
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPRN-050.
       UPRN-055.
           IF F-NAMEFIELD = "                "
               GO TO UPRN-050.
           MOVE 2910 TO POS.
           MOVE " " TO WS-MESSAGE.
           DISPLAY WS-MESSAGE AT POS.
           MOVE " " TO WS-SUPPLIER-ANSWER.
           MOVE 2910 TO POS.
           DISPLAY "IS THE INFORMATION CORRECT, UPDATE Y OR N: [ ]"
           AT POS.
           MOVE 2955 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SUPPLIER-ANSWER.

           IF W-ESCAPE-KEY = 4
               GO TO UPRN-050.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPRN-110
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPRN-055.
       UPRN-110.
           IF WS-SUPPLIER-ANSWER NOT = "Y" AND NOT = "N"
               GO TO UPRN-055.
           IF WS-SUPPLIER-ANSWER = "N"
               PERFORM ERROR1-020
               GO TO UPRN-000.
           IF WS-SUPPLIER-ANSWER = "Y"
               PERFORM ADD-INVOICES
               MOVE F-NAMEFIELD TO ALPHA-RATE
               PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE = WS-SUPPLIER-AMOUNT
               GO TO UPRN-900.
           MOVE WS-SUPPLIER-AMOUNT TO F-EDNAMEFIELDSALE.
           MOVE 2810 TO POS.
           DISPLAY "YOU HAVE ENTERED ITEMS TO THE VALUE OF: R"
               AT POS.
           MOVE 2852 TO POS.
           DISPLAY F-EDNAMEFIELDSALE AT POS.
           MOVE 0 TO F-EDNAMEFIELDSALE.
       UPRN-120.
           MOVE 2910 TO POS.
           MOVE
             "ENTER 'I' TO CHANGE ITEMS, OR 'G' TO CHANGE GRV AMT"
              TO WS-MESSAGE.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2963 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 62        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF WS-ANSWER = "I"
               GO TO GET-020.
           IF WS-ANSWER = "G"
               MOVE 1 TO SUB-1
               GO TO UPRN-050.
           IF WS-ANSWER NOT = "G" AND NOT = "I"
               GO TO UPRN-120.
       UPRN-900.
           MOVE " " TO WS-MESSAGE
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
       UPRN-999.
           EXIT.
      *
       ADD-INVOICES SECTION.
       AI-010.
           MOVE 1 TO SUB-1.
           MOVE 0 TO WS-SUPPLIER-AMOUNT.
       AI-020.
           IF B-STOCKNUMBER (SUB-1) = "    "
                GO TO AI-999.
           COMPUTE WS-SUPPLIER-AMOUNT =
                 WS-SUPPLIER-AMOUNT + B-TOTALPRICE (SUB-1).
           ADD 1 TO SUB-1.
           IF SUB-1 < 81 
               GO TO AI-020.
       AI-999.
           EXIT.
      *
       GET-SUPPLIER-ORDER-OFF-SYSTEM SECTION.
       GSOOS-005.
           PERFORM CLEAR-010.
           MOVE 1 TO SUB-1.
           
           MOVE 2910 TO POS.
           DISPLAY "ENTER THE ORDER NUMBER: [                    ]"
            AT POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 34        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ORDERNUMBER.

           IF W-ESCAPE-KEY = 4
               MOVE "2" TO WS-ABOVE-BODY
               GO TO GSOOS-999.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GSOOS-006
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GSOOS-005.
       GSOOS-006.
           MOVE WS-ORDERNUMBER     TO OO-ORDER-NUMBER
           MOVE " "                TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO OO-QUANTITY
               GO TO GSOOS-999.
       GSOOS-020.
           READ OUTSTANDING-ORDERS NEXT
               AT END NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 10
               MOVE 0 TO OO-QUANTITY
               GO TO GSOOS-999.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE 
               "ST-ORDERS BUSY READ-NEXT GSOOS-020, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO GSOOS-020.
           IF OO-ORDER-NUMBER NOT = WS-ORDERNUMBER
               GO TO GSOOS-999.
           IF OO-QUANTITY = 0
               GO TO GSOOS-020.
               
           MOVE 2810 TO POS
           DISPLAY "READING STOCK-NUMBER:" AT POS
           ADD 22 TO POS
           DISPLAY OO-STOCK-NUMBER AT POS.
           
           MOVE OO-STOCK-NUMBER TO B-STOCKNUMBER (SUB-1) WS-STOCKNUMBER
           MOVE OO-QUANTITY     TO B-QUANTITY (SUB-1).
           MOVE OO-COST         TO B-UNITPRICE (SUB-1)
           COMPUTE B-UNITPRICE (SUB-1) = B-UNITPRICE (SUB-1) - 
               (B-UNITPRICE (SUB-1) * OO-DISC / 100).
           PERFORM READ-STOCK.
           MOVE ST-DESCRIPTION1 TO B-DESCRIPTION (SUB-1)
           MOVE ST-DESCRIPTION2 TO B-DESCRIPTION2 (SUB-1)
           MOVE WS-DATE         TO B-DATE (SUB-1).
           
           IF SUB-1 < 80
              ADD 1 TO SUB-1
              GO TO GSOOS-020
           ELSE
              MOVE "YOU HAVE REACHED 80 LIMIT OF ITEMS" TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              MOVE "YOU WILL HAVE TO <ESC> TO ABORT." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       GSOOS-999.
           EXIT.
      *
       GET-ORDER-OFF-SYSTEM SECTION.
       GOOS-000.
           MOVE 2910 TO POS.
           DISPLAY "DO YOU WISH TO ADD TO AN OLD ORDER: [ ]" AT POS.
           MOVE 2947 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-OLD-ORDER.

           IF W-ESCAPE-KEY = 4
               MOVE "2" TO WS-ABOVE-BODY
               GO TO GOOS-999.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GOOS-005
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GOOS-000.
       GOOS-005.
           IF WS-OLD-ORDER NOT = "Y" AND NOT = "N"
                 GO TO GOOS-000.
           IF WS-OLD-ORDER = "Y"
                 MOVE " " TO WS-ORDERNUMBER
                 GO TO GOOS-999.
           PERFORM READ-PARAMETER-LOCK.
           MOVE PA-SUPPLY-ORDER-NUMBER TO WS-ORDERREF.
           ADD 1 TO PA-SUPPLY-ORDER-NUMBER.
           PERFORM REWRITE-PARAMETER.
           PERFORM CHECK-ORDER-FOR-ZEROS.
           MOVE 2910 TO POS.
           DISPLAY
            "THE ORDER NUMBER FOR THIS ORDER IS :[                    ]"
              AT POS.
           MOVE 2947 TO POS.
           DISPLAY WS-ORDERNUMBER AT POS.
       GOOS-010.
           MOVE 3010 TO POS.
           DISPLAY "PRESS <RETURN> TO ACCEPT THIS ORDER" AT POS.
           ADD 50 TO POS.

           MOVE ' '            TO CDA-DATA.
           MOVE 1              TO CDA-DATALEN.
           MOVE 27             TO CDA-ROW.
           MOVE 50             TO CDA-COL.
           MOVE CDA-GREEN      TO CDA-COLOR.
           MOVE 'A'            TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-YN.

           IF W-ESCAPE-KEY = 4
               GO TO GOOS-000.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               PERFORM ERROR-020
               GO TO GOOS-999
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GOOS-010.
       GOOS-999.
           EXIT.
      *
       CHECK-ORDER-FOR-ZEROS SECTION.
       OC-001.
           MOVE 1 TO SUB-1.
       OC-005.
           MOVE " " TO AL-RATE (SUB-1).
           IF SUB-1 < 20
               ADD 1 TO SUB-1
               GO TO OC-005.
       OC-010.
           MOVE WS-ORDER-REFERENCE TO WS-ORDER-CHECK.
           MOVE 1 TO SUB-1 SUB-2.
       OC-015.
           IF WS-O-C (SUB-1) = "0"
              ADD 1 TO SUB-1
              GO TO OC-015.
           IF SUB-1 < 4
              MOVE WS-O-C (SUB-1) TO AL-RATE (SUB-2)
              ADD 1 TO SUB-1 SUB-2
              GO TO OC-015.
       OC-020.
           MOVE WS-O-C (SUB-1) TO AL-RATE (SUB-2).
           IF SUB-1 < 20
              ADD 1 TO SUB-1 SUB-2
              GO TO OC-020.
           MOVE ALPHA-RATE TO WS-ORDERNUMBER.
       OC-999.
           EXIT.
      *
       UPDATE-OUT-ORDERS SECTION.
       UPOO-000.
           PERFORM CLEAR-010.
           IF STRE-TRANSACTION-CODE = 4
             MOVE WS-PORD        TO WS-ORDERNUMBER
             MOVE 1              TO SUB-1
             GO TO UPOO-110.
           
           IF STRE-TRANSACTION-CODE = 3
               PERFORM GET-ORDER-OFF-SYSTEM.
           IF WS-ABOVE-BODY = "2" 
               GO TO UPOO-999.
           IF WS-ORDERNUMBER NOT = " "
               GO TO UPOO-001.
           MOVE " " TO WS-ORDERNUMBER.
           MOVE 2910 TO POS.
           DISPLAY "ENTER THE ORDER NUMBER: [                    ]"
            AT POS.
           MOVE 2935 TO POS.
 
           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 34        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ORDERNUMBER.

           IF W-ESCAPE-KEY = 4
               MOVE "2" TO WS-ABOVE-BODY
               GO TO UPOO-999.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-001
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-000.
       UPOO-001.
           IF WS-ORDERNUMBER = "           "
               GO TO UPOO-000.
           MOVE WS-ORDERNUMBER TO OO-ORDER-NUMBER.
           IF STRE-TRANSACTION-CODE = 2
               MOVE 1 TO SUB-1
               GO TO UPOO-110.
       UPOO-002.
           MOVE " " TO WS-DELIVERY.
           PERFORM ERROR1-020.
           MOVE 2910 TO POS.
           DISPLAY "ENTER THE DELIVERY METHOD CODE: [ ]" AT POS.
           MOVE 2943 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DELIVERY.

           IF W-ESCAPE-KEY = 4
               GO TO UPOO-000.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-003
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-002.
       UPOO-003.
           IF WS-DELIVERY NOT = "1" AND NOT = "2" AND NOT = "3"
               AND NOT = "4" AND NOT = "5" AND NOT = "6"
               AND NOT = "7" AND NOT = "8" AND NOT = "9" AND NOT = "0"
               GO TO UPOO-002.
           MOVE WS-DELIVERY TO WS-DEL-SUB.
           IF WS-DEL-SUB = 0
               MOVE 1 TO WS-DEL-SUB.
           MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELVIA.
           DISPLAY WS-DELVIA AT POS.
       UPOO-004.
           MOVE 3010 TO POS
           DISPLAY
           "THIS SHOULD BE THE SUPPLIER NAME AS ON THE STOCK FILE." 
                 AT POS.
           MOVE 2910 TO POS.
           MOVE " " TO WS-MESSAGE WS-SUPPLIER.
           DISPLAY WS-MESSAGE AT POS.
           DISPLAY "ENTER THE SUPPLIER: [       ]" AT POS.
           MOVE 2931 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SUPPLIER.

           IF W-ESCAPE-KEY = 4
               GO TO UPOO-002.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-005
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-003.
       UPOO-005.
           IF WS-SUPPLIER = " "
               GO TO UPOO-003.
               
           PERFORM ERROR-020.
           PERFORM GET-CREDITOR-INFO.
               
           MOVE 0    TO WS-DUEDATE.
           MOVE 2910 TO POS.
           MOVE " "  TO WS-MESSAGE WS-ACCEPT-DATE.
           DISPLAY WS-MESSAGE AT POS.
           DISPLAY "ENTER THE DUE DATE: [          ]" AT POS.
           MOVE 2931 TO POS.
           DISPLAY "     " AT POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT-DATE.

           IF WS-ACCEPT-DATE = " "
              MOVE 0 TO WS-DUEDATE
              GO TO UPOO-008.
              
           MOVE WS-ACCEPT-DATE TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO UPOO-005.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           DISPLAY DISPLAY-DATE AT POS.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-DUEDATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO UPOO-005.
           IF W-ESCAPE-KEY = 4
               GO TO UPOO-004.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-008
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-005.
       UPOO-008.
      *     IF WS-DUEDATE = 0 OR = " "
      *         GO TO UPOO-005.
      * ABOVE TWO LINES REMOVED TO ALLOW FOR ZERO DUE DATE
      * RE: CHANGE DATED 23/6/2001
      
           MOVE 2910 TO POS.
           MOVE " " TO WS-MESSAGE WS-YN.
           DISPLAY WS-MESSAGE AT POS.
           DISPLAY "PRINT THE ORDER, Y=YES N=NO : [ ]" AT POS.
           MOVE 3010 TO POS
           DISPLAY 
           "IF YOU WANT TO PRINT & EMAIL/FAX THE ORDER ANSWER 'Y' NOW."
               AT POS
           
           MOVE 2941 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 40        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-YN.

           IF W-ESCAPE-KEY = 4
               GO TO UPOO-005.
           IF WS-YN NOT = "Y" AND NOT = "N"
               GO TO UPOO-008.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-009
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-008.
       UPOO-009.
           IF WS-YN = "Y"
               MOVE 1 TO SUB-1
               GO TO UPOO-010.
           IF WS-YN = "N"
               MOVE 1 TO SUB-1
               GO TO UPOO-060.
           GO TO UPOO-008.
       UPOO-010.
           MOVE " " TO WS-COMMENT (SUB-1).
           IF SUB-1 < 3
               ADD 1 TO SUB-1
               GO TO UPOO-010.
           MOVE 1 TO SUB-1.
           MOVE 2810 TO POS.
           MOVE " " TO WS-MESSAGE.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2910 TO POS.
           MOVE " " TO WS-MESSAGE.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2810 TO POS.
           DISPLAY
           "ENTER A COMMENT TO BE PRINTED ON THE ORDER: (3 LINES)."
                AT POS.
           MOVE 2911 TO POS.
           DISPLAY "[" AT POS.
           ADD 61 TO POS.
           DISPLAY "]" AT POS.
           MOVE 1 TO SUB-1.
       UPOO-015.
           MOVE 2901 TO POS.
           DISPLAY "NO=" AT POS.
           ADD 4 TO POS.
           DISPLAY SUB-1 AT POS.
           MOVE 2912 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 60        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 11        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-WRITE-MESSAGE.

           MOVE WS-WRITE-MESSAGE TO WS-COMMENT (SUB-1).
           MOVE " " TO WS-WRITE-MESSAGE.
           DISPLAY WS-WRITE-MESSAGE AT POS.
           IF W-ESCAPE-KEY = 4
            IF SUB-1 = 1
               GO TO UPOO-008
            ELSE
               SUBTRACT 1 FROM SUB-1
               GO TO UPOO-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-016
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-015.
       UPOO-016.
           IF WS-COMMENT (SUB-1) NOT = "    "
            IF SUB-1 < 3
              ADD 1 TO SUB-1
              GO TO UPOO-015.
           MOVE 1 TO SUB-1.
       UPOO-020.
           IF WS-COMMENT (SUB-1) = "  "
               GO TO UPOO-010.
           MOVE " " TO WS-MESSAGE.
           MOVE 2801 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           PERFORM ERROR1-020.
       UPOO-040.
           MOVE 0 TO WS-SLIP-COPIES
           MOVE " " TO WS-MESSAGE WS-ACCEPT-COPIES
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2910 TO POS
           DISPLAY "HOW MANY COPIES DO YOU WANT ? ENTER 1-9 [ ]."
              AT POS
           MOVE 2951 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT-COPIES.

           MOVE WS-ACCEPT-COPIES TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-SLIP-COPIES.
           IF NUMERIC-RATE > 9 OR < 1
             MOVE "YOU CAN ONLY PRINT FROM 1-9 COPIES, PLEASE RE-ENTER."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO UPOO-040.
            IF W-ESCAPE-KEY = 4
               GO TO UPOO-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-040.
       UPOO-050.
           IF WS-SLIP-COPIES = 0 OR = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-040.
       UPOO-060.
           MOVE " " TO WS-MESSAGE WS-CONFIRMED.
           MOVE 2810 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2910 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2810 TO POS.
           DISPLAY "HAS THIS ORDER BEEN CONFIRMED WITH THE SUPPLIER:"
           AT POS.
           MOVE 2929 TO POS.
           DISPLAY "ENTER Y=YES, N=NO : [ ]" AT POS.
           MOVE 2950 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CONFIRMED.

           IF W-ESCAPE-KEY = 4
               GO TO UPOO-040.
           IF WS-CONFIRMED NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-060.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-100
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-060.
       UPOO-100.
           MOVE " " TO WS-MESSAGE.
           MOVE 2810 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2910 TO POS.
           DISPLAY WS-MESSAGE AT POS.
       UPOO-101.
           MOVE 2910 TO POS.
           MOVE " " TO WS-MESSAGE.
           
           DISPLAY WS-MESSAGE AT POS.
           DISPLAY "SEND EMAIL OR FAX Y=YES N=NO: [ ]" AT POS.
           MOVE 2941 TO POS.

           MOVE 'N'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 40        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-EMAIL-FAX.
           
           IF W-ESCAPE-KEY = 4
               GO TO UPOO-060.
           IF WS-EMAIL-FAX NOT = "Y" AND NOT = "N"
               GO TO UPOO-101.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO UPOO-102
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO UPOO-101.
       UPOO-102.
           MOVE 2910 TO POS.
           DISPLAY "THE ORDER IS BEING PROCESSED, PLEASE WAIT." AT POS.
           
           MOVE 1 TO SUB-1.
       UPOO-110.
           IF STRE-TRANSACTION-CODE = 3
               GO TO UPOO-120.
           IF B-STOCKNUMBER (SUB-1) = "   "
               GO TO UPOO-999.
       UPOO-112.
           MOVE WS-ORDERNUMBER        TO OO-ORDER-NUMBER.
           MOVE B-STOCKNUMBER (SUB-1) TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
           READ OUTSTANDING-ORDERS WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
               MOVE "1" TO WS-ORDERNOTFOUND
               MOVE 2910 TO POS
               DISPLAY " " AT 3079 WITH BELL
               MOVE " " TO WS-MESSAGE
               DISPLAY WS-MESSAGE AT POS
               DISPLAY "ORDER NOT FOUND FOR" AT POS
               ADD 20 TO POS
               DISPLAY B-STOCKNUMBER (SUB-1) AT POS
               CALL "LOCKKBD" USING F-FIELDNAME
               PERFORM CHECK-TO-CHANGE-ORDER
             IF WS-CHANGE-ORDER = "N"
               GO TO UPOO-130
             ELSE
               MOVE " " TO WS-ORDERNOTFOUND
               GO TO UPOO-112.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "STORDER BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO UPOO-112.

           IF STRE-TRANSACTION-CODE = 4
              ADD B-QUANTITY (SUB-1) TO OO-QUANTITY
              GO TO UPOO-114.

           IF B-QUANTITY (SUB-1) < OO-QUANTITY
               SUBTRACT B-QUANTITY (SUB-1) FROM OO-QUANTITY
           ELSE
               MOVE 0 TO OO-QUANTITY.
       UPOO-114.
           REWRITE OUT-ORDER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
               PERFORM ERROR1-020
               DISPLAY " " AT 3079 WITH BELL
               DISPLAY "ORDER NOT UPDATED" AT POS
               CALL "LOCKKBD" USING F-FIELDNAME.
           IF WS-OUTORD-ST1 NOT = 0
                MOVE "SORDERS BUSY ON REWRITE, 'ESC' TO RETRY."
                 TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO UPOO-114.
           GO TO UPOO-130.
       UPOO-120.
           MOVE B-STOCKNUMBER (SUB-1) TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY.
           READ OUTSTANDING-ORDERS WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-OUTORD-ST1
                GO TO UPOO-125.
           IF WS-OUTORD-ST1 NOT = 0
                MOVE "SORDERS BUSY ON START UPOO-120, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO UPOO-120.
           ADD B-QUANTITY (SUB-1) TO OO-QUANTITY
                                     OO-ORIG-QTY.
           MOVE B-DATE (SUB-1)    TO OO-ORDERDATE
           MOVE WS-DUEDATE        TO OO-DUEDATE
           MOVE WS-DELIVERY       TO OO-DELIVERY-METHOD
           MOVE WS-SUPPLIER       TO OO-SUPPLIER-NUMBER
           MOVE B-DISC (SUB-1)    TO OO-DISC.
           
       UPOO-122.
           REWRITE OUT-ORDER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
               MOVE "ORDER NOT UPDATED UPOO-122, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO UPOO-120.
           IF WS-OUTORD-ST1 NOT = 0
                MOVE "STORDER BUSY ON REWRITE IPOO-122, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO UPOO-122.
           GO TO UPOO-130.
       UPOO-125.
           IF B-STOCKNUMBER (SUB-1) = "      "
                GO TO UPOO-130.
           MOVE WS-ORDERNUMBER        TO OO-ORDER-NUMBER.
           MOVE B-STOCKNUMBER (SUB-1) TO OO-STOCK-NUMBER
           MOVE B-QUANTITY (SUB-1)    TO OO-QUANTITY
                                         OO-ORIG-QTY
           MOVE B-DATE (SUB-1)        TO OO-ORDERDATE
           MOVE WS-DUEDATE            TO OO-DUEDATE
           MOVE WS-DELIVERY           TO OO-DELIVERY-METHOD
           MOVE WS-SUPPLIER           TO OO-SUPPLIER-NUMBER
           MOVE WS-CONFIRMED          TO OO-UPDATED
           MOVE B-DISC (SUB-1)        TO OO-DISC
           MOVE B-UNITPRICE (SUB-1)   TO OO-COST
           MOVE WS-TYPE               TO OO-FOR-LOC.
           
           WRITE OUT-ORDER-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
                MOVE "STORDER NOT WRITTEN UPOO-120, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO UPOO-130.
           IF WS-OUTORD-ST1 NOT = 0
                MOVE "STORDER BUSY UPOO-125, PRESS 'ESC' TO RETRY."
                 TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO UPOO-125.
       UPOO-130.
           ADD 1 TO SUB-1.
           IF B-STOCKNUMBER (SUB-1) = "      "
                GO TO UPOO-900.
           IF SUB-1 < 81
               GO TO UPOO-110.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
       UPOO-900.
      *     IF WS-EMAIL-FAX NOT = "Y"
      *         GO TO UPOO-999.
      *     PERFORM CLEAR-SCREEN.
       UPOO-999.
           EXIT.
      *
       CHECK-FOREIGN-LOCAL SECTION.
       CFL-005.
           MOVE " " TO WS-TYPE.
           PERFORM CLEAR-010.
           MOVE 2810 TO POS.
           DISPLAY "IS THIS A LOCAL ORDER OR A FOREIGN ORDER:"
           AT POS.
           MOVE 2910 TO POS.
           DISPLAY "ENTER L=LOCAL ORDER; F=FOREIGN ORDER : [ ]" AT POS.
           MOVE 2950 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TYPE.

           IF W-ESCAPE-KEY = 4
               MOVE "4" TO WS-ABOVE-BODY
               GO TO CFL-999.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CFL-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CFL-005.
       CFL-030.
           IF WS-TYPE NOT = "L" AND NOT = "F"
               GO TO CFL-005.
           PERFORM ERROR1-020.
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS.
       CFL-035.
           MOVE "Y" TO WS-PRINT-COSTS.
           PERFORM CLEAR-010.
           MOVE 2910 TO POS.
           DISPLAY "PRINT COSTS ON THE PRINTED ORDERS ?  : [ ] "
           AT POS.
           MOVE 2950 TO POS.

           MOVE 'Y'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PRINT-COSTS.

           IF W-ESCAPE-KEY = 4
               GO TO CFL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CFL-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CFL-035.
       CFL-040.
           IF WS-PRINT-COSTS NOT = "Y" AND NOT = "N"
               GO TO CFL-035.
           PERFORM ERROR1-020.
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS.
       CFL-999.
           EXIT.
      *
       CHECK-FOR-STAFF-SALE SECTION.
       CFSS-005.
           PERFORM CLEAR-010.
           PERFORM ERROR-020
           PERFORM ERROR1-020.
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS. 
           MOVE " " TO WS-TYPE.

           MOVE 2810 TO POS.
           DISPLAY "IS THIS A STAFF SALE OR NORMAL SALE?" AT POS.
           MOVE 2910 TO POS.
           DISPLAY "ENTER N=NORMAL SALE; S=STAFF SALE    : [ ]" AT POS.
           MOVE 2950 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE "F"       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-STAFF-SALE.

           IF W-ESCAPE-KEY = 4
               MOVE "4" TO WS-ABOVE-BODY
               GO TO CFSS-999.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CFSS-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CFSS-005.
       CFSS-030.
           IF WS-STAFF-SALE NOT = "N" AND NOT = "S"
               GO TO CFSS-005.
           PERFORM ERROR1-020.
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS.
       CFSS-999.
           EXIT.
      *
       CHECK-TO-CHANGE-ORDER SECTION.
       CTCO-005.
            MOVE " " TO WS-MESSAGE.
            MOVE 2910 TO POS.
            DISPLAY WS-MESSAGE AT POS.
       CTCO-006.
            MOVE 3010 TO POS.
            DISPLAY WS-MESSAGE AT POS.
       CTCO-010.
            DISPLAY "THIS IS THE ORDER-NUMBER:[              ]"
                 AT POS.
            ADD 26 TO POS.
            DISPLAY WS-ORDERNUMBER AT POS.
            MOVE 2910 TO POS.
            DISPLAY "DO YOU WISH TO CHANGE THE ORDER-NUMBER ENTERED.[ ]"
                 AT POS.
            ADD 48 TO POS.

           MOVE "Y"            TO CDA-DATA.
           MOVE 1              TO CDA-DATALEN.
           MOVE 26             TO CDA-ROW.
           MOVE 57             TO CDA-COL.
           MOVE CDA-WHITE      TO CDA-COLOR.
           MOVE 'F'            TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CHANGE-ORDER.

            IF WS-CHANGE-ORDER NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CTCO-010.
               
            IF WS-CHANGE-ORDER = "N"
               GO TO CTCO-999.
               
            IF WS-CHANGE-ORDER = "Y"
               MOVE X"19" TO F-EXIT-CH
               PERFORM CHECK-PASSWORD
             IF WS-PASSWORD-VALID = "Y"
               GO TO CTCO-015
      *         GO TO CTCO-999
             ELSE
               MOVE "PASSWORD INVALID, PLEASE RE-ENTER." TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CTCO-010.
       CTCO-015.
            PERFORM CTCO-005.
            MOVE " " TO WS-ORDERNUMBER.
            DISPLAY " ENTER NEW ORDER NUMBER :[                    ]"
                 AT POS.
            ADD 26 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 35        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ORDERNUMBER.

            IF WS-ORDERNUMBER = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO CTCO-015.
            PERFORM CTCO-005 THRU CTCO-006.
       CTCO-999.
            EXIT.
      *
       READ-DELIVERY-FILE SECTION.
       RDELIV-000.
            OPEN I-O PARAMETER-FILE.
            MOVE 1 TO SUB-1
                      PA-RECORD.
            MOVE 3 TO PA-TYPE.
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
               MOVE "PARAMETER BUSY ON DELIVERY-READ, 'ESC' TO RETRY."
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
               PERFORM ERROR-020
               MOVE 1 TO SUB-1
               GO TO RDELIV-999.
            GO TO RDELIV-010.
       RDELIV-999.
            EXIT.
      *
       PRINT-RETURN-SLIP SECTION.
       PRS-000.
           MOVE 2910 TO POS
           DISPLAY "PRINTING GOODS RETURN SLIP .........               "
           AT POS.
           MOVE WS-DOTPRINTER TO WS-PRINTER-SAVE.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE WS-PRINT-NORMAL TO PRINT-REC
           WRITE PRINT-REC.
      *ABOVE TWO LINES ENTERED TO SET PRINTER TO 10CPI BEFORE PRINTING
           MOVE "** G O O D S   R E T U R N   S L I P  **" TO SO1-NAME
           MOVE ALL "*"             TO SO2-PIC1
           MOVE ALL "*"             TO SO2-PIC2
           MOVE 0 TO WS-COPIES
                     SLIP-CNT.
           MOVE 66 TO SLIP-LINE.
           MOVE 1 TO SUB-1.
       PRS-005.
           IF B-STOCKNUMBER (SUB-1) = "    "
             IF B-QUANTITY (SUB-1) = 0
               GO TO PRS-900.
           IF SLIP-LINE < 61
               GO TO PRS-010.
           ADD 1         TO SLIP-CNT
           MOVE SLIP-CNT TO SO1-PAGE.
           IF SLIP-CNT = 1
               WRITE PRINT-REC FROM SLIP-HEAD1 AFTER 1
           ELSE
               WRITE PRINT-REC FROM SLIP-HEAD1 AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM SLIP-HEAD2 AFTER 1.
           
           MOVE " "                 TO PRINT-REC
           MOVE "RETURNS FROM :"    TO SO3-PIC1
           MOVE "RETURNS TO :"      TO SO3-PIC2
           WRITE PRINT-REC FROM SLIP-HEAD3 AFTER 1
           MOVE " "                 TO PRINT-REC.

           MOVE PA-NAME             TO H4-ADD
           MOVE CR-NAME             TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 2
           MOVE " "                 TO PRINT-REC SLIP-HEAD4.

           MOVE PA-ADD1             TO H4-ADD
           MOVE CR-DEL-ADDRESS1     TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1
           MOVE " "                 TO PRINT-REC SLIP-HEAD4.

           MOVE PA-ADD2             TO H4-ADD
           MOVE CR-DEL-ADDRESS2     TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1
           MOVE " "     TO PRINT-REC SLIP-HEAD4.

           MOVE PA-ADD3             TO H4-ADD
           MOVE CR-DEL-ADDRESS3     TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1
           MOVE " "     TO PRINT-REC SLIP-HEAD4.

           MOVE PA-CODE TO H4-ADD
           MOVE " "     TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1
           MOVE " "     TO PRINT-REC SLIP-HEAD4.

           MOVE PA-DEL1             TO H4-ADD
           MOVE " "                 TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 2
           MOVE " "                 TO PRINT-REC SLIP-HEAD4.

           MOVE PA-DEL2             TO H4-ADD
           MOVE " "                 TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1
           MOVE " "     TO PRINT-REC SLIP-HEAD4.

           MOVE PA-DEL3             TO H4-ADD
           MOVE " "                 TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1
           MOVE " "     TO PRINT-REC SLIP-HEAD4.

           MOVE ALL "*" TO SLIP-HEAD7
           WRITE PRINT-REC FROM SLIP-HEAD7 AFTER 1
           MOVE " "     TO PRINT-REC SLIP-HEAD7
           
           MOVE "*"                TO SO7-FIL1
                                      SO7-FIL2
                                      SO7-FIL3
                                      SO7-FIL4
           MOVE "SUPPLIER:"        TO SO7-SUP-FIL
           MOVE WS-SUPPLIER        TO SO7-SUPPLIER
           MOVE "INV/DNo:"         TO SO7-DEL-FIL
           MOVE WS-INVNO           TO SO7-DEL-VIA
           MOVE "GRV No:"          TO SO7-PO-FIL
           MOVE WS-STTRANSNO       TO WS-GRV-NUMBER
                                     SO7-PO-NUM
           WRITE PRINT-REC FROM SLIP-HEAD7 AFTER 1
           MOVE ALL "*"            TO SLIP-HEAD7
           WRITE PRINT-REC FROM SLIP-HEAD7 AFTER 1
           MOVE " "                TO PRINT-REC SLIP-HEAD7.

           MOVE " " TO SL8-DISC
           WRITE PRINT-REC FROM SLIP-HEAD8 AFTER 2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE 19  TO SLIP-LINE.
       PRS-010.
           MOVE B-STOCKNUMBER (SUB-1)   TO S-STOCKNO
           MOVE B-DESCRIPTION (SUB-1)   TO S-DESC1
           MOVE B-DESCRIPTION2 (SUB-1)  TO S-DESC2
           MOVE B-QUANTITY (SUB-1)      TO S-QTY
           MOVE B-UNITPRICE (SUB-1)     TO S-UNIT
      *     MOVE B-TOTALPRICE (SUB-1)    TO S-TOTAL
           WRITE PRINT-REC FROM SLIP-DETAIL AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO SLIP-LINE
                    SUB-1.
           IF SUB-1 < 81
              GO TO PRS-005.
       PRS-900.
           MOVE " " TO PRINT-REC SLIP-DETAIL
           MOVE "GOODS AMT OF GRV : R" TO SLIP-TOT-COM
           MOVE WS-SUPPLIER-AMOUNT     TO TOT-GRV
           WRITE PRINT-REC FROM SLIP-TOTAL AFTER 1
           MOVE " " TO PRINT-REC.
           
           MOVE " " TO PRINT-REC SLIP-DETAIL SLIP-TOTAL
           MOVE "      VAT AMOUNT :"   TO SLIP-TOT-COM
           COMPUTE WS-VAT-AMT ROUNDED =
                WS-SUPPLIER-AMOUNT * PA-GST-PERCENT / 100
           MOVE WS-VAT-AMT             TO TOT-GRV
           WRITE PRINT-REC           FROM SLIP-TOTAL AFTER 1
           MOVE " "                    TO PRINT-REC.
           
           
           MOVE " " TO PRINT-REC SLIP-DETAIL SLIP-TOTAL
           MOVE "TOTAL GRV. VALUE : R"  TO SLIP-TOT-COM
           COMPUTE WS-SUPPLIER-AMOUNT = WS-SUPPLIER-AMOUNT + WS-VAT-AMT
           MOVE WS-SUPPLIER-AMOUNT      TO TOT-GRV
           WRITE PRINT-REC              FROM SLIP-TOTAL AFTER 1
           MOVE " "                     TO PRINT-REC
           COMPUTE WS-SUPPLIER-AMOUNT = WS-SUPPLIER-AMOUNT - WS-VAT-AMT.
           
                      
           MOVE 1 TO SUB-1.
           MOVE "REASON FOR RETURN:" TO SLIP-FILL.
       PRS-910.
           MOVE WS-COMMENT (SUB-1)   TO SLIP-COMM
           WRITE PRINT-REC            FROM SLIP-COMMENT AFTER 1.
           IF SUB-1 < 3
              ADD 1 TO SUB-1
            IF WS-COMMENT (SUB-1) NOT = "   "
              MOVE SPACES TO SLIP-FILL
              GO TO PRS-910.
           MOVE " "           TO PRINT-REC SLIP-COMMENT
           MOVE "GOODS RECEIVED BY:" TO SLIP-FILL
           WRITE PRINT-REC            FROM SLIP-COMMENT AFTER 2
           MOVE " "           TO PRINT-REC SLIP-COMMENT
           MOVE "       PRINT NAME:" TO SLIP-FILL
           WRITE PRINT-REC            FROM SLIP-COMMENT AFTER 1
           MOVE " "           TO PRINT-REC SLIP-COMMENT
           MOVE "    DATE RECEIVED:" TO SLIP-FILL
           WRITE PRINT-REC            FROM SLIP-COMMENT AFTER 2
           MOVE " "                  TO PRINT-REC.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
           
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
      * SECOND TIME JUST To GET THE TWO COPIES NEEDED FOR THE GRV
           PERFORM SEND-REPORT-TO-PRINTER.
           MOVE " "                  TO PRINT-REC SLIP-DETAIL.
      *     ADD 1 TO WS-COPIES.
      *     IF WS-COPIES NOT = 2
      *        MOVE 0 TO SLIP-CNT
      *        MOVE 1 TO SUB-1
      *        MOVE 66 TO SLIP-LINE
      *        OPEN OUTPUT PRINT-FILE
      *        GO TO PRS-005.
           MOVE 0 TO WS-STTRANSNO.
           MOVE 2910 TO POS
           DISPLAY "                                                   "
           AT POS.
       PRS-999.
           EXIT.
      *
       PRINT-ORDER-SLIP SECTION.
       POS-000.
           MOVE 2910 TO POS
           DISPLAY "PRINTING PURCHASE ORDER FORM.........              "
           AT POS.
           MOVE WS-DOTPRINTER TO WS-PRINTER-SAVE.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE "** P U R C H A S E   O R D E R **" TO SO1-NAME
           MOVE ALL "*"                             TO SO2-PIC1
           MOVE " "                                 TO SO2-PIC2.
           MOVE 0 TO WS-COPIES
                     SLIP-CNT
                     WS-SUPPLIER-AMOUNT.
           MOVE 66 TO SLIP-LINE.
           MOVE 1 TO SUB-1.
       POS-005.
           IF B-STOCKNUMBER (SUB-1) = "    "
             IF B-QUANTITY (SUB-1) = 0
               GO TO POS-900.
           IF SLIP-LINE < 61
               GO TO POS-010.
           ADD 1         TO SLIP-CNT.
           MOVE SLIP-CNT TO SO1-PAGE.
           IF SLIP-CNT = 1
              WRITE PRINT-REC FROM SLIP-HEAD1 AFTER 1
              WRITE PRINT-REC FROM SLIP-HEAD2 AFTER 1
           ELSE
              WRITE PRINT-REC FROM SLIP-HEAD1 AFTER PAGE
              WRITE PRINT-REC FROM SLIP-HEAD2 AFTER 1.
           MOVE "SUPPLY TO :"      TO SO3-PIC1
           MOVE "SUPPLIER  :"      TO SO3-PIC2
           MOVE " "                TO PRINT-REC
           WRITE PRINT-REC FROM SLIP-HEAD3 AFTER 1
           MOVE " "                TO PRINT-REC.

           MOVE PA-NAME             TO H4-ADD
           MOVE CR-NAME             TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 2
           MOVE " "                 TO PRINT-REC SLIP-HEAD4.

           MOVE PA-ADD1             TO H4-ADD
           MOVE CR-DEL-ADDRESS1     TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1
           MOVE " "                 TO PRINT-REC SLIP-HEAD4.

           MOVE PA-ADD2             TO H4-ADD
           MOVE CR-DEL-ADDRESS2     TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1
           MOVE " "                 TO PRINT-REC SLIP-HEAD4.

           MOVE PA-ADD3             TO H4-ADD
           MOVE CR-DEL-ADDRESS3     TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1
           MOVE " "                 TO PRINT-REC SLIP-HEAD4.

           MOVE PA-CODE             TO H4-ADD
           MOVE " "                 TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1
           MOVE " " TO PRINT-REC SLIP-HEAD4.

           MOVE PA-DEL1             TO H4-ADD
           MOVE " "                 TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 2
           MOVE " "                 TO PRINT-REC SLIP-HEAD4.

           MOVE PA-DEL2             TO H4-ADD
           MOVE " "                 TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1
           MOVE " "     TO PRINT-REC SLIP-HEAD4.

           MOVE PA-DEL3             TO H4-ADD
           MOVE " "                 TO H4-DEL
           WRITE PRINT-REC FROM SLIP-HEAD4 AFTER 1
           MOVE " "     TO PRINT-REC SLIP-HEAD4.
           
           MOVE ALL "*" TO SLIP-HEAD7
           WRITE PRINT-REC FROM SLIP-HEAD7 AFTER 1
           MOVE " " TO PRINT-REC SLIP-HEAD7
           
           MOVE " "                TO SLIP-HEAD7
           MOVE "*"                TO SO7-FIL1
                                      SO7-FIL4
           MOVE "V.A.T.#:"         TO SO7-DEL-FIL
           MOVE PA-CO-VAT-NO       TO SO7-DEL-VIA
           WRITE PRINT-REC FROM SLIP-HEAD7 AFTER 1
           MOVE " "                TO SLIP-HEAD7

           MOVE "*"                TO SO7-FIL1
                                      SO7-FIL2
                                      SO7-FIL3
                                      SO7-FIL4
           MOVE "SUPPLIER:"        TO SO7-SUP-FIL
           MOVE WS-SUPPLIER        TO SO7-SUPPLIER
           MOVE "DEL VIA:"         TO SO7-DEL-FIL
           MOVE WS-DELVIA          TO SO7-DEL-VIA
           MOVE WS-ORDERNUMBER     TO SO7-PO-INFO
           WRITE PRINT-REC FROM SLIP-HEAD7 AFTER 1
           
           MOVE ALL "*"            TO SLIP-HEAD7
           WRITE PRINT-REC FROM SLIP-HEAD7 AFTER 1
           MOVE " "                TO PRINT-REC SLIP-HEAD7.
           
           
           MOVE " DISC" TO SL8-DISC
           WRITE PRINT-REC FROM SLIP-HEAD8 AFTER 2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE 19  TO SLIP-LINE.
       POS-010.
           MOVE B-STOCKNUMBER (SUB-1)   TO S-STOCKNO
           MOVE B-DESCRIPTION (SUB-1)   TO S-DESC1
           MOVE B-DESCRIPTION2 (SUB-1)  TO S-DESC2
           MOVE B-QUANTITY (SUB-1)      TO S-QTY.
           IF WS-PRINT-COSTS = "N"
              MOVE 0                    TO B-UNITPRICE (SUB-1)
                                           B-DISC (SUB-1).
           MOVE B-UNITPRICE (SUB-1)     TO S-UNIT
           MOVE B-DISC (SUB-1)          TO S-DISC.
              
           COMPUTE B-TOTALPRICE (SUB-1) =
            (B-UNITPRICE (SUB-1) - 
            ((B-UNITPRICE (SUB-1) * B-DISC (SUB-1)) / 100))
                       * B-QUANTITY (SUB-1).
      *     MOVE B-TOTALPRICE (SUB-1)    TO S-TOTAL.
           COMPUTE WS-SUPPLIER-AMOUNT = 
                   WS-SUPPLIER-AMOUNT + B-TOTALPRICE (SUB-1).
           WRITE PRINT-REC FROM SLIP-DETAIL AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO SLIP-LINE
           ADD 1 TO SUB-1.
           IF SUB-1 < 81
              GO TO POS-005.
       POS-900.
           MOVE " " TO PRINT-REC SLIP-DETAIL SLIP-TOTAL
           MOVE "      ORDER VALUE :"  TO SLIP-TOT-COM
           MOVE WS-SUPPLIER-AMOUNT     TO  TOT-GRV
           WRITE PRINT-REC              FROM SLIP-TOTAL AFTER 1
           MOVE " "                    TO PRINT-REC.
           
           MOVE " " TO PRINT-REC SLIP-DETAIL SLIP-TOTAL
           MOVE "       VAT AMOUNT :"  TO SLIP-TOT-COM
           COMPUTE WS-VAT-AMT ROUNDED =
                WS-SUPPLIER-AMOUNT * PA-GST-PERCENT / 100
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
           
           MOVE 1 TO SUB-1.
       POS-910.
           MOVE WS-COMMENT (SUB-1)     TO SLIP-COMM-LINE.
           WRITE PRINT-REC              FROM SLIP-COMMENT AFTER 1.
           IF SUB-1 < 3
              ADD 1 TO SUB-1
            IF WS-COMMENT (SUB-1) NOT = "   "
              GO TO POS-910.
           MOVE " "             TO PRINT-REC SLIP-COMMENT.
           WRITE PRINT-REC.
           
           MOVE WS-DUEDATE           TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE "EXPECTED DUE-DATE:" TO SLIP-FILL
           MOVE DISPLAY-DATE         TO SLIP-COMM
           WRITE PRINT-REC  FROM SLIP-COMMENT AFTER 1
           MOVE " " TO PRINT-REC SLIP-COMMENT
           PERFORM OPEN-500.
           
           MOVE "**** PLEASE ACKNOWLEDGE RECEIPT OF THIS ORDER ****"
                TO SLIP-COMM-LINE.
           WRITE PRINT-REC  FROM SLIP-COMMENT AFTER 1.
           MOVE " " TO PRINT-REC SLIP-COMMENT.
           ADD 1    TO WS-COPIES.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           IF WS-COPIES NOT = WS-SLIP-COPIES
               CLOSE PRINT-FILE
               PERFORM SEND-REPORT-TO-PRINTER
               GO TO POS-950.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           GO TO POS-999.
       POS-950.
           MOVE 1 TO SUB-1.
           MOVE 0 TO SLIP-CNT
                     WS-SUPPLIER-AMOUNT.
           MOVE 66 TO SLIP-LINE.
           MOVE WS-DOTPRINTER TO WS-PRINTER-SAVE.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           GO TO POS-005.
           MOVE 2910 TO POS
           DISPLAY "                                                   "
           AT POS.
       POS-999.
           EXIT.
      *
       GET-CREDITOR-INFO SECTION.
       GET-CRED-004.
           MOVE "N" TO WS-NEXT.
           PERFORM ERROR1-020
           MOVE 2810 TO POS.
           DISPLAY WS-MESSAGE AT POS
           DISPLAY "ENTER THE SUPPLIER NUMBER: [       ]" AT POS.
           MOVE 2910 TO POS.
           DISPLAY "Enter the ACCOUNT NUMBER and <Return>, OR" AT POS.
           MOVE 3015 TO POS.
           DISPLAY
           "Enter a SHORT NAME and <PgDn> to scroll through Acc'S."
             AT POS.
           MOVE 2838 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 25        TO CDA-ROW.
           MOVE 37        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CREDITOR-ACCEPT.

           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-CRED-005.
           IF W-ESCAPE-KEY = 7
                GO TO GET-CRED-006
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-CRED-004.
       GET-CRED-005.
           IF WS-CREDITOR-ACCEPT = 0 OR = " "
               GO TO GET-CRED-004.
           MOVE WS-CREDITOR-ACCEPT TO WS-CREDITOR
           PERFORM READ-CREDITOR.
           IF CR-NAME = "** UNKNOWN **"
             MOVE 
            "RE-ENTER THE NUMBER OF A VALID SUPPLIER, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO GET-CRED-004.
           GO TO GET-CRED-020.
       GET-CRED-006.
           MOVE WS-CREDITOR-ACCEPT TO F-NAMEFIELD.
       GET-CRED-007.
           PERFORM READ-NEXT-CREDITOR.
           IF CR-NAME = "** UNKNOWN **"
           MOVE 
            "RE-ENTER THE NUMBER OF A VALID SUPPLIER, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO GET-CRED-004.
       GET-CRED-020.
           PERFORM ERROR-020
           PERFORM ERROR1-020
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2810 TO POS
           DISPLAY "SUPPLIER: [       ]" AT POS.
           MOVE 2821 TO POS.
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
           MOVE 2832 TO POS.
           DISPLAY CR-NAME AT POS.
           MOVE 2910 TO POS.
           DISPLAY "Press <Return> to ACCEPT This Account,    " AT POS.
           MOVE 3015 TO POS.
           DISPLAY 
           "OR Press <PgDn> To Scroll Through More Accounts       "
            AT POS.
           MOVE 3075 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 25        TO CDA-ROW.
           MOVE 74        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.
 
           IF W-ESCAPE-KEY = 1 OR = 2 OR = 5
               GO TO GET-CRED-900.
           IF W-ESCAPE-KEY = 7
               MOVE "Y" TO WS-NEXT
               MOVE " " TO WS-CREDITOR-ACCEPT
               GO TO GET-CRED-007.
           IF W-ESCAPE-KEY = 6
               MOVE " " TO WS-CREDITOR-ACCEPT
               GO TO GET-CRED-004.
           GO TO GET-CRED-020.
       GET-CRED-900.
            PERFORM ERROR-020
            PERFORM ERROR1-020
            MOVE 2810 TO POS
            DISPLAY WS-MESSAGE AT POS.
       GET-CRED-999.
            EXIT.
      *
       READ-INVQUES-FILE SECTION.
       RINVQUES-000.
            MOVE 1 TO PA-RECORD.
            MOVE 6 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RINVQUES-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "N" TO WS-ST-PRINT-LABELS
               GO TO RINVQUES-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY ON READ - RINVQUES, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RINVQUES-010.
       RINVQUES-900.
            MOVE INVQUES-ST-PRINT-LABELS TO WS-ST-PRINT-LABELS.
       RINVQUES-999.
            EXIT.
      *
       WRITE-CRJRN-INV-TRANS SECTION.
       WCIT-000.
            MOVE 0     TO WS-FREIGHT.
            
            PERFORM CLEAR-SCREEN.
            MOVE "StCrJnMt" TO F-FORMNAME
            PERFORM OPEN-060 THRU OPEN-900
            PERFORM DISPLAY-FORM.
            
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            MOVE 2815 TO POS
            DISPLAY "PRESS <CODE-F10> TO EXIT THIS PROGRAM." AT POS.
            MOVE 2910 TO POS
            DISPLAY 
            "WHEN ALL INFO'S ENTERED, <GO> AT LAST FIELD TO WRITE TRANS"
                AT POS.
       WCIT-003.
            PERFORM READ-PARAMETER
            MOVE PA-GST-PERCENT TO WS-VAT-PERC.
            MOVE WS-TOTALPRICE  TO CRJRN-LOC-AMT.
            PERFORM WCIT-066.
            PERFORM WCIT-068.
            PERFORM WCIT-150.
            PERFORM RWCR-005.
            
            MOVE CRJRN-LOC-AMT TO WS-INV-AMT.

            MOVE "BATCH-TYPE" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME.
            IF STRE-TRANSACTION-CODE = 1 OR = 2
                MOVE "*INVOICE*" TO F-NAMEFIELD
            ELSE
                MOVE "*C/NOTE *" TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CRJRNBATCH" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-JRN       TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       WCIT-005.
            MOVE X"0A" TO F-EXIT-CH.
            MOVE "                                    " TO F-NAMEFIELD.
            
            IF STRE-TRANSACTION-NUMBER = 4
               MOVE "DNOTE"        TO F-FIELDNAME
               MOVE 5              TO F-CBFIELDNAME
               MOVE WS-GRV-NUMBER  TO F-NAMEFIELD
               PERFORM USER-FILL-FIELD
               MOVE 10             TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.
            
            MOVE "DNOTE" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DNOTE-NO WS-INV-NO.
            IF F-EXIT-CH = X"87" OR = X"9F"
               PERFORM CHECK-PASSWORD.
            IF F-EXIT-CH = X"87" OR = X"9F"
             IF WS-PASSWORD-VALID = "Y"
               PERFORM ERROR-020
               PERFORM ERROR1-020
               MOVE 2810 TO POS
               DISPLAY WS-MESSAGE AT POS
               GO TO WCIT-999
             ELSE
               MOVE 
           "PASSWORD INVALID, PLEASE CONTINUE WITH D/N INFO."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WCIT-005.
            IF WS-DNOTE-NO = " "
               MOVE "THIS ENTRY CANNOT BE BLANK, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO WCIT-005.
            
            MOVE "N" TO WS-NEXT
            GO TO WCIT-080.
       WCIT-061.
            MOVE "DNOTE"     TO F-FIELDNAME
            MOVE 5           TO F-CBFIELDNAME
            MOVE WS-DNOTE-NO TO F-NAMEFIELD
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       WCIT-062.
            IF CR-NAME = "** UNKNOWN **"
               DISPLAY " " AT 3079 WITH BELL.
            MOVE "SUPPLIER"           TO F-FIELDNAME.
            MOVE 8                    TO F-CBFIELDNAME.
            IF WS-CREDITOR-ST1 NOT = 10
               MOVE CR-ACCOUNT-NUMBER TO F-NAMEFIELD
            ELSE
               MOVE " "               TO F-NAMEFIELD.
            MOVE 7                    TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLIERNAME"    TO F-FIELDNAME.
            MOVE 12                TO F-CBFIELDNAME.
            IF WS-CREDITOR-ST1 NOT = 10
               MOVE CR-NAME        TO F-NAMEFIELD
            ELSE
               MOVE " "            TO F-NAMEFIELD.
            MOVE 40                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       WCIT-066.
            MOVE "LOCALAMT"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE CRJRN-LOC-AMT TO F-EDNAMEFIELDNUM6
            MOVE 11            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "FREIGHT"     TO F-FIELDNAME
            MOVE 7             TO F-CBFIELDNAME
            MOVE WS-FREIGHT    TO F-EDNAMEFIELDNUM6
            MOVE 11            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "VATAMT" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME.
            COMPUTE CRJRN-VAT-AMT ROUNDED =
                 (((CRJRN-LOC-AMT + WS-FREIGHT) * WS-VAT-PERC) / 100).
            MOVE CRJRN-VAT-AMT TO F-EDNAMEFIELDNUM6
            MOVE 11            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC6.
        WCIT-067.
            MOVE "INVDATE"      TO F-FIELDNAME.
            MOVE 7              TO F-CBFIELDNAME.
            MOVE CRJRN-INV-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE   TO F-NAMEFIELD.
            MOVE 10             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       WCIT-068.
            MOVE "TOTAL"       TO F-FIELDNAME
            MOVE 5             TO F-CBFIELDNAME.
            COMPUTE WS-TOTAL =
                 CRJRN-LOC-AMT + WS-FREIGHT + CRJRN-VAT-AMT
            MOVE WS-TOTAL      TO F-EDNAMEFIELDNUM6.
            MOVE 11            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
       WCIT-080.
            MOVE 2810 TO POS.
            IF WS-NEXT = "N"
               DISPLAY
            "ENTER A/C NUMBER & <RETURN> OR A SHORT NAME & <NEXT-PAGE>"
               AT POS
            ELSE
              DISPLAY
            "TO END SEARCH AND RE-ENTER SHORT NAME PRESS <ESC>.    "
               AT POS.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO WCIT-005.
            IF F-EXIT-CH  = X"07"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                MOVE " "            TO CR-NAME
                MOVE "SUPPLIERNAME" TO F-FIELDNAME
                MOVE 12             TO F-CBFIELDNAME
                MOVE CR-NAME        TO F-NAMEFIELD
                MOVE 40             TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                MOVE "N" TO WS-NEXT
                GO TO WCIT-080.
            MOVE 7            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            IF F-EXIT-CH = X"0C"
                PERFORM READ-NEXT-CREDITOR
                PERFORM WCIT-062
             IF WS-CREDITOR-ST1 NOT = 88 AND NOT = 10
                MOVE "Y" TO WS-NEXT
                GO TO WCIT-080
             ELSE
                MOVE 0   TO CR-ACCOUNT-NUMBER
                MOVE " " TO CR-NAME
                GO TO WCIT-080.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-CREDITOR.
            PERFORM READ-CREDITOR.

            MOVE "SUPPLIERNAME" TO F-FIELDNAME.
            MOVE 12             TO F-CBFIELDNAME.
            MOVE CR-NAME        TO F-NAMEFIELD.
            MOVE 40             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            
            IF CR-NAME = "** UNKNOWN **"
               DISPLAY " " AT 3079 WITH BELL
               GO TO WCIT-080.
       WCIT-090.
            PERFORM ERROR-020
            MOVE 2810 TO POS
            DISPLAY WS-MESSAGE AT POS.
            
            PERFORM READ-CRJRN-INVOICES.
            IF WS-CRJRN-ST1 = 88
               GO TO WCIT-005.
            IF WS-NEWINVOICE = "N"
               GO TO WCIT-005.
               
            PERFORM WCIT-003.

            MOVE 2910 TO POS
            DISPLAY 
            "WHEN ALL INFO'S ENTERED, <GO> AT LAST FIELD TO WRITE TRANS"
                AT POS.
                
      ****************************************************************
      * THIS ENTRY BELOW WAS ADDED SO THAT THE ORIGINAL STOCK VALUE  *
      * AMOUNT WOULD BE REMEMBERED AFTER THE READ OF CRJRN FOR       *
      * DUPLICATES OTHERWISE THE NEWLY READ AMOUNT SEEMS TO BE HELD  *
      * IN MEMORY FOR FUTURE USE WHICH IS WRONG.                     *
      *                                                              *
      *      MOVE WS-INV-AMT TO CRJRN-LOC-AMT.                       *
      ****************************************************************
                
            IF WS-CRJRN-INPUT-ONLY = "N"
                GO TO WCIT-092.
       WCIT-091.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "LOCALAMT" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE 11         TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO WCIT-080.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRJRN-LOC-AMT
                                 CRJRN-UNAPPLIED-AMT
                                    WS-SUPPLIER-AMOUNT
                                     F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF CRJRN-LOC-AMT NOT > 0
               PERFORM ERROR1-020
               MOVE "THIS AMOUNT MUST BE > ZERO, RE-ENTER" TO WS-MESSAGE
               PERFORM ERROR1-000
               GO TO WCIT-091.
            MOVE 0 TO CRJRN-FOR-AMT CRJRN-EXCHANGE.
            PERFORM WCIT-066.
       WCIT-092.
            MOVE "                                " TO F-NAMEFIELD.
            MOVE "FREIGHT"  TO F-FIELDNAME.
            MOVE 7          TO F-CBFIELDNAME.
            MOVE 11         TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO WCIT-080.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-FREIGHT
                                 F-EDNAMEFIELDNUM6.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
            
            MOVE 0 TO CRJRN-FOR-AMT CRJRN-EXCHANGE.
            
            PERFORM WCIT-066.
            PERFORM WCIT-068.
            
      * F-EXIT-CH = <F8>
            IF WS-CRJRN-INPUT-ONLY = "N"
             IF F-EXIT-CH = X"1D"
                 GO TO WCIT-095.
            IF WS-CRJRN-INPUT-ONLY = "N"
                 GO TO WCIT-153.
       WCIT-095.
            PERFORM ERROR1-020
            MOVE 2910 TO POS
            DISPLAY 
            "WHEN ALL INFO'S ENTERED, <GO> AT LAST FIELD TO WRITE TRANS"
                AT POS.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "VATAMT" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            MOVE 11       TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO WCIT-092.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRJRN-VAT-AMT
                                 F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF CRJRN-VAT-AMT NOT > 0
               PERFORM ERROR1-020
               MOVE "ARE YOU SURE THE VAT AMT SHOULD BE ZERO ????"
               TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               MOVE 2910 TO POS
            DISPLAY 
            "WHEN ALL INFO'S ENTERED, <GO> AT LAST FIELD TO WRITE TRANS"
                AT POS.
            PERFORM WCIT-068.
            GO TO WCIT-153.
       WCIT-150.
            PERFORM OPEN-500.
            MOVE "INVDATE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE WS-DATE      TO SPLIT-DATE WS-PAY-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       WCIT-153.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "INVDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO WCIT-092.
            MOVE 10          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO WCIT-153.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CRJRN-INV-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO WCIT-153.

           IF SPLIT-MM NOT = WS-PAY-MM
           MOVE "YOU CANNOT ENTER A MONTH OTHER THAN THE CURRENT MONTH."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM WCIT-150
               GO TO WCIT-153.
           IF SPLIT-YY NOT = WS-PAY-YY
            MOVE "YOU CANNOT ENTER A YEAR OTHER THAN THE CURRENT YEAR."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM WCIT-150
               GO TO WCIT-153.
           PERFORM WCIT-067.
           IF F-EXIT-CH NOT = X"1B"
               GO TO WCIT-005.
           
           PERFORM ERROR1-020.
           MOVE 2910 TO POS
           DISPLAY "WRITING CREDITOR JRN RECORD......" AT POS.
           PERFORM REWRITE-CRJRN.
           PERFORM ERROR1-020.
       WCIT-999.
            EXIT.
      *
       READ-CRJRN-INVOICES SECTION.
       RCRI-000.
           MOVE "Y" TO WS-NEWINVOICE.
       RCRI-005.
           MOVE WS-INV-NO TO CRJRN-INV-NO.
           START CRJRN-FILE KEY NOT < CRJRN-INV-NO
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RCRI-200.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CR-JRN BUSY ON START READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRJRN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRJRN-ST1
               CLOSE CRJRN-FILE
               PERFORM OPEN-046
               GO TO RCRI-005.
       RCRI-010.
           READ CRJRN-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
              GO TO RCRI-200.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CR-JRN BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRJRN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRJRN-ST1
              GO TO RCRI-010.
              
           IF CRJRN-INV-NO = WS-INV-NO
            IF CRJRN-CRACC-NUMBER NOT = WS-CREDITOR
              GO TO RCRI-010.
              
      * CHECKING INVOICES
           IF CRJRN-INV-NO = WS-INV-NO
            IF CRJRN-CRACC-NUMBER = WS-CREDITOR
             IF STRE-TRANSACTION-CODE = 1 OR = 2
              IF CRJRN-TYPE = 1
              MOVE 88 TO WS-CRJRN-ST1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE "THIS INVOICE APPEARS IN BATCH            ALREADY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE 2945 TO POS
              DISPLAY CRJRN-REFERENCE AT POS
              MOVE "  PRESS <ESC> TO RE-ENTER AN INVOICE NUMBER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE "N" TO WS-NEWINVOICE
              MOVE " " TO WS-DNOTE-NO
              GO TO RCRI-200.
              
      * CHECKING CREDIT NOTES.
           IF CRJRN-INV-NO = WS-INV-NO
            IF CRJRN-CRACC-NUMBER = WS-CREDITOR
             IF STRE-TRANSACTION-CODE = 4
              IF CRJRN-TYPE = 6
              MOVE 88 TO WS-CRJRN-ST1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE "THIS C/NOTE APPEARS IN BATCH             ALREADY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE 2945 TO POS
              DISPLAY CRJRN-REFERENCE AT POS
              MOVE "  PRESS <ESC> TO RE-ENTER A C/NOTE NUMBER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE "N" TO WS-NEWINVOICE
              MOVE " " TO WS-DNOTE-NO
              GO TO RCRI-200.
      * CHECKING GRV. INVOICE ENTERED WITH SAME NUM AS GRV REQUEST.
           IF CRJRN-INV-NO = WS-INV-NO
            IF CRJRN-CRACC-NUMBER = WS-CREDITOR
             IF STRE-TRANSACTION-CODE = 4
              IF CRJRN-TYPE = 1
              MOVE 88 TO WS-CRJRN-ST1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE "AN INVOICE APPEARS IN BATCH              ALREADY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE 2945 TO POS
              DISPLAY CRJRN-REFERENCE AT POS
              MOVE
            "  <ESC> TO RE-ENTER, C/NOTE MUST BE DIFFERENT TO INV#."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE "N" TO WS-NEWINVOICE
              MOVE " " TO WS-DNOTE-NO
              GO TO RCRI-200.
              
           IF CRJRN-INV-NO NOT = WS-INV-NO
              MOVE "Y" TO WS-NEWINVOICE
              PERFORM CLEAR-CRJRN-FIELDS
              GO TO RCRI-200.
           IF CRJRN-COMPLETE = "Y" OR = "P"
            IF CRJRN-TYPE = 1
              MOVE 88 TO WS-CRJRN-ST1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE "THIS INVOICE HAS BEEN PREVIOUSLY ENTERED & POSTED."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM CLEAR-CRJRN-FIELDS
              MOVE " " TO WS-DNOTE-NO
              MOVE "N" TO WS-NEWINVOICE.
           IF CRJRN-COMPLETE = "Y" OR = "P"
            IF CRJRN-TYPE = 6
              MOVE 88 TO WS-CRJRN-ST1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE "THIS C/NOTE HAS BEEN PREVIOUSLY ENTERED & POSTED."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM CLEAR-CRJRN-FIELDS
              MOVE " " TO WS-DNOTE-NO
              MOVE "N" TO WS-NEWINVOICE.
       RCRI-200.
           UNLOCK CRJRN-FILE.
       RCRI-999.
           EXIT.
      *
       READ-CREDITOR SECTION.
       RCR-000.
           MOVE "N"         TO WS-NEXT.
           MOVE WS-CREDITOR TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-KEY
                INVALID KEY NEXT SENTENCE.
       RCR-010.
           READ CREDITOR-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "** UNKNOWN **" TO CR-NAME
                MOVE 88              TO WS-CREDITOR-ST1
                GO TO RCR-999.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITOR BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO RCR-010.
       RCR-999.
             EXIT.
      *
       READ-NEXT-CREDITOR SECTION.
       RNC-000.
           IF WS-NEXT = "Y"
               GO TO RNC-010.
           MOVE F-NAMEFIELD TO CR-NAME
           START CREDITOR-MASTER KEY NOT < CR-ALT-KEY
                  INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE "** UNKNOWN **" TO CR-NAME
                MOVE 88              TO WS-CREDITOR-ST1
                MOVE "N"             TO WS-NEXT
                GO TO RNC-999.
       RNC-010.
           READ CREDITOR-MASTER NEXT
                AT END NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 10
                MOVE "END OF SEARCH, 'ESC' TO RE-ENTER A SHORT NAME."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE "N" TO WS-NEXT
                GO TO RNC-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE "CREDITOR BUSY ON READ-NEXT, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO RNC-010.
           MOVE CR-ACCOUNT-NUMBER TO WS-CREDITOR.
       RNC-999.
           EXIT.
      *
       CHECK-DUE-DATE SECTION.
       CDD-010.
           MOVE CRJRN-INV-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE CONVERT-DATE TO WS-CH-DATE.
           IF CR-TERMS = " " OR = "1" OR = "3" OR = "8"
              MOVE 25 TO WS-CH-DD
              ADD 1 TO WS-CH-MM
              GO TO CDD-020.
           IF CR-TERMS = "4"
              MOVE 25 TO WS-CH-DD
              ADD 2 TO WS-CH-MM
              GO TO CDD-020.
           IF CR-TERMS = "5"
              MOVE 25 TO WS-CH-DD
              ADD 3 TO WS-CH-MM
              GO TO CDD-020.
           IF CR-TERMS = "6"
              MOVE 25 TO WS-CH-DD
              ADD 4 TO WS-CH-MM
              GO TO CDD-020.
      *7=180 DAYS.  USED TO BE LETTER OF CREDIT
           IF CR-TERMS = "7"
              MOVE 25 TO WS-CH-DD
              ADD 6 TO WS-CH-MM.
       CDD-020.
           IF WS-CH-MM > 12
              SUBTRACT 12 FROM WS-CH-MM
              ADD 1 TO WS-CH-YY.

           MOVE WS-CH-DATE TO CONVERT-DATE.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO CRJRN-DUE-DATE.
       CDD-999.
           EXIT.
      *
       REWRITE-CRJRN SECTION.
       RWCR-005.
           MOVE 0 TO WS-GLDISC.
           
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-CRTRANSNO TO CRJRN-TRANS.
           ADD 1               TO GLPA-CRTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           IF GLPA-CURRENT-CRPER NOT = GLPA-CURRENT-SLPER
              MOVE "F"        TO CRJRN-FUTURE
           ELSE
              MOVE " "        TO CRJRN-FUTURE.

           START CRJRN-FILE KEY NOT < CRJRN-KEY.
           
           MOVE WS-MM-DESC (WS-MM) TO WS-JRN-REST
           MOVE PA-CURRENT-PER-WW  TO WS-JRN-WEEK
           MOVE WS-JRN             TO CRJRN-REFERENCE.
       RWCR-006.
           MOVE WS-CURRENTPER TO CRJRN-NO
           MOVE "N"           TO CRJRN-COMPLETE
           PERFORM CHECK-DUE-DATE
           MOVE " "           TO CRJRN-INV-NO
           MOVE WS-DNOTE-NO   TO CRJRN-DNOTE-NO
                                 CRJRN-INV-NO
           MOVE WS-CREDITOR   TO CRJRN-CRACC-NUMBER.
       RWCR-007.
           IF STRE-TRANSACTION-CODE = 4
               GO TO RWCR-008.
      *****************************************************************
      *NEW SECTION SO THAT WHEN ENTERING ONLY CRJRN IT DOESN'T PUT IN *
      *AMT FROM THE PREVIOUS STOCK ENTERED TRANS.                     *
      *****************************************************************
           IF WS-CRJRN-INPUT-ONLY = "N"
              COMPUTE CRJRN-LOC-AMT = 
                   CRJRN-LOC-AMT + CRJRN-VAT-AMT + WS-FREIGHT
           MOVE CRJRN-LOC-AMT TO WS-TOTAL.

           MOVE WS-TOTAL TO CRJRN-LOC-AMT
           MOVE CRJRN-LOC-AMT TO CRJRN-UNAPPLIED-AMT
           MOVE 0             TO CRJRN-SETT-DISC.
           IF CR-SETT-DISC > 0
             COMPUTE CRJRN-SETT-DISC ROUNDED = ((WS-TOTAL
                     * CR-SETT-DISC) / 100).
           MOVE 1 TO CRJRN-TYPE.
           MOVE 1 TO SUB-1.
           GO TO RWCR-011.
       RWCR-008.
           IF STRE-TRANSACTION-CODE = 1 OR = 2
                GO TO RWCR-011.
                
      *     COMPUTE CRJRN-LOC-AMT = 
      *           (CRJRN-LOC-AMT + CRJRN-VAT-AMT + WS-FREIGHT) * -1
           MOVE WS-TOTAL TO CRJRN-LOC-AMT
           COMPUTE CRJRN-LOC-AMT = CRJRN-LOC-AMT * -1
      *     MOVE CRJRN-LOC-AMT TO WS-TOTAL.
           MOVE CRJRN-LOC-AMT TO CRJRN-UNAPPLIED-AMT
           MOVE 0             TO CRJRN-SETT-DISC.
           
           COMPUTE CRJRN-VAT-AMT = CRJRN-VAT-AMT * -1.
           
           IF CR-SETT-DISC > 0
             COMPUTE CRJRN-SETT-DISC ROUNDED = ((WS-TOTAL
                     * CR-SETT-DISC) / 100) * -1.
           MOVE 6 TO CRJRN-TYPE.
           MOVE 1 TO SUB-1.
       RWCR-011.
      *************
      * SUB-1 = 1 *
      *************
           MOVE "50-200-05-00"      TO CRJRN-GLACC (SUB-1)
           MOVE CRJRN-GLACC (SUB-1) TO GL-NUMBER
           PERFORM READ-GLMASTER.
           IF CRJRN-TYPE = 1
              MOVE WS-SUPPLIER-AMOUNT TO CRJRN-GLAMT (SUB-1)
           ELSE
              COMPUTE WS-SUPPLIER-AMOUNT = WS-SUPPLIER-AMOUNT * -1
              MOVE WS-SUPPLIER-AMOUNT TO CRJRN-GLAMT (SUB-1).
              
           MOVE GL-DESCRIPTION        TO CRJRN-GLDESC (SUB-1)
           COMPUTE CRJRN-GLDISC (SUB-1) ROUNDED =
               ((CRJRN-GLAMT (SUB-1) + CRJRN-VAT-AMT)
                     * CR-SETT-DISC) / 100.
           ADD CRJRN-GLDISC (SUB-1) TO WS-GLDISC.
       RWCR-012.
      *************
      * SUB-1 = 2 *
      *************
          MOVE 2 TO SUB-1.
          IF WS-FREIGHT = 0
              GO TO RWCR-013.
              
           MOVE "50-090-05-00"         TO CRJRN-GLACC (SUB-1)
           MOVE CRJRN-GLACC (SUB-1)    TO GL-NUMBER
           PERFORM READ-GLMASTER.
           IF CRJRN-TYPE = 1
              MOVE WS-FREIGHT          TO CRJRN-GLAMT (SUB-1)
           ELSE
              COMPUTE WS-FREIGHT = WS-FREIGHT * -1
              MOVE WS-FREIGHT          TO CRJRN-GLAMT (SUB-1).
              
              MOVE GL-DESCRIPTION      TO CRJRN-GLDESC (SUB-1)

           COMPUTE CRJRN-GLDISC (SUB-1) ROUNDED =
              (CRJRN-GLAMT (SUB-1) * CR-SETT-DISC) / 100.

           ADD CRJRN-GLDISC (SUB-1)    TO WS-GLDISC.
       RWCR-013.
      *************
      * SUB-1 = 3 *
      *************
          IF WS-FREIGHT = 0
              MOVE 2 TO SUB-1
          ELSE
              MOVE 3 TO SUB-1.
              
          MOVE GLPA-GLVAT-ACC      TO CRJRN-GLACC (SUB-1)
          MOVE CRJRN-GLACC (SUB-1) TO GL-NUMBER
          PERFORM READ-GLMASTER
          MOVE GL-DESCRIPTION      TO CRJRN-GLDESC (SUB-1).

          MOVE CRJRN-VAT-AMT       TO CRJRN-GLAMT (SUB-1).
          
          IF WS-GLDISC NOT = CRJRN-SETT-DISC
             COMPUTE WS-GLDISC-DIFF = CRJRN-SETT-DISC - WS-GLDISC
             ADD WS-GLDISC-DIFF TO CRJRN-GLDISC (1).
          
          IF WS-NEWINVOICE = "Y"
             GO TO RWCR-019.
       RWCR-018.
           REWRITE CRJRN-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RWCR-019.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CRJRN REWRITE ERR, RWCR-018. TRANS NOT RE-WRITTEN."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRJRN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRJRN-ST1
               GO TO RWCR-999.
           GO TO RWCR-999.
       RWCR-019.
           WRITE CRJRN-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RWCR-018.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CRJRN WRITE ERR, RWCR-019. TRANS NOT WRITTEN."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       RWCR-999.
           EXIT.
      *
       READ-GLPARAMETER SECTION.
       RGLP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO PARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               STOP RUN.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RGLP-000.
       RGLP-999.
           EXIT.
      *
       READ-GLPARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 1 TO GLPA-RECORD.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RPL-010.
           READ GLPARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE "GLPARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLPARAMETER-ST1
               GO TO RPL-010.
       RPL-999.
           EXIT.
      *
       REWRITE-GLPARAMETER SECTION.
       REWP-000.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE "GLPARAMETER BUSY ON RE-WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLPARAMETER-ST1
               GO TO RP-000.
       REWP-999.
           EXIT.
      *
       READ-GLMASTER SECTION.
       RD-000.
           START GL-MASTER KEY NOT < GL-KEY.
       RD-010.
           READ GL-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE " " TO GL-NUMBER
               MOVE "*** A/C INVALID ***" TO GL-DESCRIPTION
               GO TO RD-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLMASTER  BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO RD-010.
       RD-999.
           EXIT.
      *
       SCROLL-NEXT SECTION.
       NEXT-000.
            ADD 1 TO SUB-1.
            IF SUB-1 > SUB-25
               MOVE SUB-25 TO SUB-1.
            IF SUB-1 < 1
               MOVE 1 TO SUB-1 SUB-25.
            MOVE 1 TO F-INDEX. 
            IF SUB-1 > 71
               MOVE 71 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 80  
                GO TO NEXT-030.
            IF F-INDEX < 11
                GO TO NEXT-010.
       NEXT-030.
            SUBTRACT 10 FROM SUB-1.
            IF SUB-1 > 70
              IF SUB-25 > 70
               COMPUTE F-INDEX = 10 - (80 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 10
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 2910 TO POS
            DISPLAY "    BODY LINE: " AT POS
            ADD 16 TO POS
            MOVE SUB-1 TO WS-BODY-LINE
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-999.
             EXIT.
      *
       SCROLL-NEXT-PAGE SECTION.
       NEXT-PAGE-000.
            ADD 10 TO SUB-1.
            IF SUB-1 > SUB-25 + 1
               MOVE SUB-25 TO SUB-1.
            MOVE 1 TO F-INDEX. 
            IF SUB-1 > 71
               MOVE 71 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 80  
                GO TO NEXT-PAGE-030.
            IF F-INDEX < 11
                GO TO NEXT-PAGE-010.
       NEXT-PAGE-030.
            SUBTRACT 10 FROM SUB-1.
            IF SUB-1 > 70
              IF SUB-25 > 70
               COMPUTE F-INDEX = 10 - (80 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX.
            IF SUB-1 > 80
               MOVE 71 TO SUB-1.
            IF F-INDEX > 10
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 2910 TO POS
            DISPLAY "    BODY LINE: " AT POS
            ADD 16 TO POS
            MOVE SUB-1 TO WS-BODY-LINE
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-PAGE-999.
             EXIT.
      *
       SCROLL-PREVIOUS SECTION.
       PREV-000.
            SUBTRACT 10 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-010.
            PERFORM SCROLLING.
       PREV-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 80   
                GO TO PREV-030.
            IF F-INDEX < 11
                GO TO PREV-010.
       PREV-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 10 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

             MOVE 2910 TO POS
             DISPLAY "    BODY LINE: " AT POS
             ADD 16 TO POS
             MOVE SUB-1 TO WS-BODY-LINE
             DISPLAY WS-BODY-LINE AT POS.
       PREV-999.
             EXIT.
      *
       SCROLL-DOWN SECTION.
       SCROLL-DOWN-000.
            SUBTRACT 1 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       SCROLL-DOWN-010.
            PERFORM SCROLLING.
       SCROLL-DOWN-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 80
                GO TO SCROLL-DOWN-030.
            IF F-INDEX < 11
                GO TO SCROLL-DOWN-010.
       SCROLL-DOWN-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 10 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

             MOVE 2910 TO POS
             DISPLAY "    BODY LINE: " AT POS
             ADD 16 TO POS
             MOVE SUB-1 TO WS-BODY-LINE
             DISPLAY WS-BODY-LINE AT POS.
       SCROLL-DOWN-999.
             EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE 15 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = "   "
                 MOVE "                   " TO F-NAMEFIELD
            ELSE
                 MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-010.
            MOVE "DESCRIPTION1" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE 20 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = "   "
                 MOVE "                   " TO F-NAMEFIELD
            ELSE
                 MOVE B-DESCRIPTION (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESCRIPTION2" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE 20 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = "   "
                 MOVE "                   " TO F-NAMEFIELD
            ELSE
                 MOVE B-DESCRIPTION2 (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-020.
            MOVE "QUANTITY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 6 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = "   "
                 MOVE "                   " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
            ELSE
                MOVE B-QUANTITY (SUB-1) TO F-EDNAMEFIELDNUMNEG
                PERFORM WRITE-FIELD-NUMNEG.

            MOVE "UNITPRICE" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            MOVE 11          TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = "   "
                MOVE "                   " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
            ELSE
                MOVE B-UNITPRICE (SUB-1)   TO F-EDNAMEFIELD99Mil
                PERFORM WRITE-FIELD-99Mil.

            MOVE "TOTALPRICE" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            MOVE 11           TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = "   "
                MOVE "                   " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
            ELSE
                MOVE B-TOTALPRICE (SUB-1)  TO F-EDNAMEFIELD99Mil
                PERFORM WRITE-FIELD-99Mil.

            MOVE "REFERENCENO" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE 20 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = "   "
                 MOVE "                   " TO F-NAMEFIELD
            ELSE
               MOVE B-REFNO (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REFERENCEDATE" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            IF B-STOCKNUMBER (SUB-1) = "   "
                MOVE "                   " TO F-NAMEFIELD
            ELSE
                MOVE B-DATE (SUB-1) TO SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       DISPLAY-LINE-ITEMS SECTION.
       DISPL-000.
            MOVE "STOCKNUMBER"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE ST-STOCKNUMBER TO F-NAMEFIELD
            MOVE 15             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       DISPL-010.
            MOVE "DESCRIPTION1"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE ST-DESCRIPTION1 TO F-NAMEFIELD
            MOVE 20              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESCRIPTION2"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE ST-DESCRIPTION2 TO F-NAMEFIELD
            MOVE 20              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       DISPL-020.
            MOVE "REFERENCENO"   TO F-FIELDNAME
            MOVE 11              TO F-CBFIELDNAME
            MOVE B-REFNO (SUB-1) TO F-NAMEFIELD
            MOVE 20              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REFERENCEDATE" TO F-FIELDNAME
            MOVE 13              TO F-CBFIELDNAME
            MOVE B-DATE (SUB-1)  TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE    TO F-NAMEFIELD
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       DISPL-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 1 TO SUB-1.
             MOVE "          " TO WS-ORDERNUMBER.
       CF-010.
             MOVE " " TO B-STOCKNUMBER (SUB-1)
                         B-DESCRIPTION (SUB-1)
                         B-DESCRIPTION2 (SUB-1)
                         B-REFNO (SUB-1).
             MOVE 0   TO B-QUANTITY (SUB-1)
                         B-UNITPRICE (SUB-1)
                         B-TOTALPRICE (SUB-1)
                         B-DISC (SUB-1)
                         B-DATE (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 81
                 GO TO CF-010.
       CF-999.
             EXIT.
      *
       CANCEL-TRANSACTION SECTION.
       CAN-000.
           COMPUTE SUB-2 = SUB-1 + 1.
       CAN-010.
           IF SUB-2 > 80
              GO TO CAN-090.
           MOVE BODY-LINE (SUB-2) TO BODY-LINE (SUB-1).
           IF B-STOCKNUMBER (SUB-2) = SPACES
               MOVE " " TO B-STOCKNUMBER (SUB-1)
                           B-DESCRIPTION (SUB-1)
                           B-DESCRIPTION2 (SUB-1)
                           B-REFNO (SUB-1)
               MOVE 0   TO B-QUANTITY (SUB-1)
                           B-UNITPRICE (SUB-1)
                           B-TOTALPRICE (SUB-1)
                           B-DISC (SUB-1)
                           B-DATE (SUB-1)
               GO TO CAN-090.
           ADD 1 TO SUB-1 SUB-2
           GO TO CAN-010.
       CAN-090.
           MOVE " " TO B-STOCKNUMBER (SUB-1)
                       B-DESCRIPTION (SUB-1)
                       B-DESCRIPTION2 (SUB-1)
                       B-REFNO (SUB-1).
           MOVE 0   TO B-QUANTITY (SUB-1)
                       B-UNITPRICE (SUB-1)
                       B-TOTALPRICE (SUB-1)
                       B-DISC (SUB-1)
                       B-DATE (SUB-1).
           MOVE 1 TO SUB-1 F-INDEX.
           IF STRE-TRANSACTION-CODE = 1 OR = 2 OR = 4
               PERFORM CHECK-TOTALS.
       CAN-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           START PARAMETER-FILE KEY NOT < PA-KEY.
       RP-010.
           READ PARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO PARAMETER RECORD ON FILE, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "SLPARAMETER BUSY ON READ, 'ESC' TO RE-TRY."
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
       READ-PARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           START PARAMETER-FILE KEY NOT < PA-KEY.
       RPL-010.
           READ PARAMETER-FILE LOCK          
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO PARAMETER RECORD ON FILE, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ-LOCK, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
              GO TO RPL-010.
       RPL-999.
           EXIT.
      *
       REWRITE-PARAMETER SECTION.
       REWP-000.
           REWRITE PARAMETER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "PARAMETER RECORD NOT UPDATED, 'ESC' TO EXIT PROG."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY ON REWRITE, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RP-000.
       REWP-999.
           EXIT.
      *
       CLEAR-CRJRN-FIELDS SECTION.
       CFJRN-000.
           MOVE 1 TO SUB-1.
       CFJRN-010.
           MOVE " " TO CRJRN-GLACC (SUB-1)
                       CRJRN-GLDESC (SUB-1).
           MOVE 0   TO CRJRN-GLAMT (SUB-1)
                       CRJRN-GLDISC (SUB-1).
           ADD 1 TO SUB-1.
           IF SUB-1 < 11
               GO TO CFJRN-010.
       CFJRN-020.
           MOVE " " TO CRJRN-REFERENCE
                       CRJRN-PERIOD
                       CRJRN-INV-NO
                       CRJRN-DNOTE-NO
                       CRJRN-COMPLETE.
           MOVE 0 TO   CRJRN-TRANS
                       CRJRN-NO
                       CRJRN-INV-DATE
                       CRJRN-DUE-DATE
                       CRJRN-CRACC-NUMBER
                       CRJRN-LOC-AMT
                       CRJRN-FOR-AMT
                       CRJRN-SETT-DISC.
       CFJRN-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
            OPEN I-O PARAMETER-FILE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER BUSY ON OPEN, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-010.
            MOVE ALL "X" TO STORE-DEL.
            PERFORM READ-DELIVERY-FILE.
            PERFORM READ-INVQUES-FILE.
       OPEN-011.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0 
              MOVE 0 TO WS-GLMAST-ST1
              MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-011.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-012.
       OPEN-013.
           MOVE " "                TO WS-CRJRN-FUTURE
           PERFORM READ-GLPARAMETER
           MOVE GLPA-CURRENT-SLPER TO WS-CURRENTPER
           IF GLPA-CURRENT-CRPER NOT = GLPA-CURRENT-SLPER
              MOVE "F"        TO WS-CRJRN-FUTURE.
       OPEN-020.
            OPEN I-O OUTSTANDING-ORDERS.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO WS-OUTORD-ST1
               MOVE "SORDERS BUSY ON OPEN, PRESS 'ESC' TO RETRY."
                   TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-020.
       OPEN-040.
            OPEN I-O STKRECEIPTS-FILE.
            IF WS-STKRECEIPT-ST1 NOT = 0
               MOVE 0 TO WS-STKRECEIPT-ST1
               MOVE "ST-RECEIPTS BUSY ON OPEN, PRESS 'ESC' TO RETRY." 
                  TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-040.
       OPEN-045.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-045.
       OPEN-046.
           OPEN I-O CRJRN-FILE.
           IF WS-CRJRN-ST1 NOT = 0 
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CRJRN FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-046.
       OPEN-060.
           MOVE Ws-Forms-Name   TO F-FILENAME.
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-500.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE      TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO SO1-DATE
           MOVE WS-MM        TO WS-ORDER-MM
           MOVE WS-YY        TO WS-ORDER-YY.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER
                 PARAMETER-FILE
                 OUTSTANDING-ORDERS
                 STKRECEIPTS-FILE
                 CREDITOR-MASTER
                 GL-MASTER
                 CRJRN-FILE.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldNumeric".
       Copy "WriteField99Mil".
       Copy "WriteFieldNumeric6".
       Copy "WriteFieldNumNeg".
       Copy "WriteFieldPrice".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "StockPassword".
       Copy "ReadKBD".
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
       Copy "WriteDailyExcep1".
      *
      * END-OF-JOB
