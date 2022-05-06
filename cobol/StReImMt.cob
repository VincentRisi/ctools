        IDENTIFICATION DIVISION.
        PROGRAM-ID. StReImMt.
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
          Copy "SelectStMaster".
          Copy "SelectStChanges".
          Copy "SelectSlParameter".
          Copy "SelectStReceipt".
          Copy "SelectStImports".
          Copy "SelectStOrders".
          Copy "SelectCrMaster".
          Copy "SelectCrJrn".
          Copy "SelectCrCurrency".
          Copy "SelectGlMaster".
          Copy "SelectGlParameter".
          Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LABEL-PRINT ASSIGN TO WS-LABELPRINTER
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SPL-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStockChanges.
           COPY ChlfdDaily.
           COPY ChlfdParam.
           COPY ChlfdImpReceipts.
           COPY ChlfdStkReceipts.
           COPY ChlfdOutOrd.
           COPY ChlfdCrJrn.
           COPY ChlfdCreditor.
           COPY ChlfdCrCurr.
           COPY ChlfdGlParam.
           COPY ChlfdGlMast.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER              PIC X(132).
       FD  LABEL-PRINT.
       01  LABEL-REC.
           03  FILLER              PIC X(250).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-LABELPRINTER      PIC X(100) VALUE " ".
       77  WS-DOTPRINTER        PIC X(100) VALUE " ".
       77  WS-ACCEPT-COPIES     PIC X VALUE " ".
       77  WS-ST-PRINT-LABELS   PIC X VALUE " ".
       77  WS-ITEM-RECOSTED     PIC X VALUE " ".
       77  WS-1STPRINT          PIC X VALUE " ".
       77  WS-YN                PIC X VALUE " ".
       77  WS-VALID             PIC X VALUE " ".
       77  WS-END-OF-FILE       PIC X VALUE " ".
       77  WS-QTY-EACH-ITEM     PIC 9(5) VALUE 0.
       77  WS-START-POS         PIC 9 VALUE 0.
       77  WS-DIVIDE-ACCEPT     PIC X(3) VALUE " ".
       77  WS-DIVIDE-BY         PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  SLLABEL-LINE         PIC 9(2) VALUE 66.
       77  SLLABEL-CNT          PIC 9(2) VALUE 0.
       77  WS-PRICE             PIC 9(8)V99 VALUE 0.
       77  SUB1-SAVE            PIC S9(5) VALUE 0.
       77  WS-QTY               PIC 9(5) VALUE 0.
       77  WS-READ              PIC 99 VALUE 0.
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-INVERSEDISC       PIC 9(2)V99 VALUE 0.
       77  WS-ORDER-DISC        PIC 9(2)V99 VALUE 0.
       77  WS-DISCOUNT-ENTRY    PIC X(5) VALUE " ".
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "StMastIq".
       77  WS-STOCK-INQUIRY     PIC X(8) VALUE "StMastIq".
       77  WS-ORDER-INQUIRY     PIC X(8) VALUE "StOrStIq".
       77  WS-PORDER-INQUIRY    PIC X(8) VALUE "StOrOrIq".
       77  WS-PERCENT           PIC 9(3)V99 VALUE 0.
       77  WS-PERC              PIC S9(4)V99 VALUE 0.
       77  WS-NEW-ONCOSTPERCENT PIC S9(4)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(6)V99 VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-BODY-LINE         PIC Z(2)9.
       77  WS-STOCKNUMBER       PIC X(34) VALUE " ".
       77  WS-BYORD-STOCK       PIC X VALUE " ".
       77  WS-BEG-STOCK         PIC X(15) VALUE " ".
       77  WS-END-STOCK         PIC X(15) VALUE " ".
       77  WS-NOT-THERE         PIC X VALUE " ".
       77  WS-FOREIGN-CHECK-VAL PIC 9(8)V99 VALUE 0.
       77  WS-DUTY-AMOUNT       PIC 9(8)V99 VALUE 0.
       77  WS-FOREIGN-MASK      PIC Z(7)9.99.
       77  WS-FOREIGN-VAL       PIC 9(8)V99.
       77  WS-TOTAL-DUTY        PIC 9(8)V99 VALUE 0.
       77  WS-TOTALPRICE        PIC 9(8)V99 VALUE 0.
       77  WS-TOTAL-SURCHARGE   PIC 9(8)V99 VALUE 0.
       77  WS-RAND-AMT-PAID     PIC 9(8)V99 VALUE 0.
       77  WS-RATEPAID          PIC 9(4)V9(5) VALUE 0.
       77  WS-WORK-DUTY         PIC 9(8)V9999 VALUE 0.
       77  WS-WORKTOTAL         PIC 9(8)V99 VALUE 0.
       77  WS-CHANGE-ORDER      PIC X VALUE " ".
       77  WS-RATE-DIFF-PERC    PIC S9(2)V9(5) VALUE 0.
       77  WS-RATE-DIFF         PIC S9(4)V9(5) VALUE 0.
       77  WS-UPDATE            PIC XX VALUE " ".
       77  WS-UP-SELL           PIC XX VALUE " ".
       77  WS-UP-FORGN          PIC XX VALUE " ".
       77  WS-STOCK-CHANGE      PIC X VALUE " ".
       77  WS-NEXT              PIC X VALUE " ".
       77  WS-TOTAL             PIC S9(8)V99 VALUE 0.
       77  WS-CURRENCY-TEMP     PIC S9(2)V99999 VALUE 0.
       77  WS-EXCHANGE-DIS      PIC Z(1)9.99999.
       77  WS-LOC-COMPUTE       PIC S9(8)V99 VALUE 0.
       77  WS-FOR-RUNNING       PIC S9(8)V99 VALUE 0.
       77  WS-LOC-RUNNING       PIC S9(8)V99 VALUE 0.
       77  WS-LDCOST            PIC S9(8)V99 VALUE 0.
       77  WS-CREDITOR          PIC 9(7).
       77  WS-CREDITOR-ACCEPT   PIC X(7) VALUE " ".
       77  WS-CURRENTGLPER      PIC 99 VALUE 0.
       77  WS-RECOMPUTE-CHECK-VAL PIC X VALUE " ".
       01  W-CRTSTATUS           PIC 9(4) value 0.
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
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1        PIC 99.
       01  WS-STCHANGE-STATUS.
           03  WS-STCHANGE-ST1     PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1  PIC 99.
       01  WS-IMPRECEIPT-STATUS.
           03  WS-IMPRECEIPT-ST1   PIC 99.
       01  WS-STKRECEIPT-STATUS.
           03  WS-STKRECEIPT-ST1   PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1     PIC 99.
       01  WS-CRJRN-STATUS.
           03  WS-CRJRN-ST1        PIC 99.
       01  WS-CURRENCY-STATUS.
           03  WS-CURRENCY-ST1     PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1       PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1       PIC 99.
       01  WS-Spl-STATUS.
           03  WS-Spl-ST1          PIC 99.
       01 WS-ORDERNUMBER01.
         02  WS-ORDERNUMBERS OCCURS 20.
           03  WS-ORDER            PIC X(20).
           03  WS-FOREIGN-VALUE    PIC 9(7)V99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  WS-RECOST-INFO.
           03  WS-RECOST-COMMENT   PIC X(5) VALUE " ".
           03  WS-RECOST-EXRATE    PIC Z(3)9.99999.
       01  WS-ALT-KEY.
           03  WS-SUPPLIER         PIC X(7).
           03  WS-INVOICENUM       PIC X(10).
       01  WS-PRINTER-INFO.
           03  WS-PRN-FIL          PIC X(8) VALUE " ".
           03  WS-PRN-NAME         PIC X(20) VALUE " ".
       01  JOURNAL-DATA.
           03  WS-JRN.
               05  WS-JRN-1STCHAR   PIC X(2) VALUE "PI".
               05  WS-JRN-REST      PIC X(3).
               05  WS-JRN-WEEK      PIC X(2).
               05  WS-FILLER        PIC X(3) VALUE " ".
           03  WS-INV-TOTAL         PIC 9(3).
           03  WS-CURRENTPER        PIC 99 VALUE 0.
           03  WS-FUTURE-BATCH      PIC X VALUE " ".
           03  WS-VAT-PERC          PIC 99V99.
           03  WS-INV-AMT           PIC S9(8)V99.
           03  WS-UNAPPLIED         PIC S9(8)V99.
           03  WS-FORAMT            PIC S9(8)V99.
           03  WS-SETT-DISC         PIC S9(8)V99.
           03  WS-INV-NO            PIC X(10).
           03  WS-DNOTE-NO          PIC X(10).
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 401.
               05  B-ORDERIND         PIC 99.
               05  B-STOCKNUMBER      PIC X(15).
               05  B-LINE-COSTED      PIC X.
               05  B-QUANTITY         PIC 9(5).
               05  B-COST             PIC 9(8)V999.
               05  B-DISCOUNTPERC     PIC S9(2)V99.
               05  B-DUTYPERC         PIC 9(2)V99.
               05  B-SURCHPERC        PIC 9(2)V99.
               05  B-DUTYAMOUNT       PIC 9(8)V99.
               05  B-LANDEDCOST       PIC 9(8)V99.
               05  B-SELLING          PIC 9(8)V99.      
       01  FOREIGN-INV-FIELDS.
           03  FOREIGN-LINES OCCURS 10.
               05  FL-DNOTE           PIC X(10).
               05  FL-FOR-AMT         PIC S9(8)V99.
               05  FL-LOC-AMT         PIC S9(8)V99.
       01  BODY-CHECK-FIELDS.
           03  BODY-CHECK-LINES.
               05  WS-B-QUANTITY      PIC 9(5).
               05  WS-B-COST          PIC 9(8)V999.
               05  WS-B-DISCOUNTPERC  PIC S9(2)V99.
      *ZoomBox Parameters     
       01  AttrArray          Pic x(2240).
       01  CharArray          Pic x(2240).
       01  Title              Pic x(25).
       01  sCols              Pic 9(4) Comp.
       01  sLines             Pic 9(4) Comp.
       01  fFinalBox          Pic 9(2) Comp.
       01  pBoxDesc.
           03  iFrame         Pic 9(3) Comp Value 0.
           03  Reserved       Pic X(4) Value "    ".
           03  nColsStart     Pic 9(4) Comp Value 0.
           03  nLinesStart    Pic 9(4) Comp Value 2.
           03  pBkgFrameChars Pic x(4).
           03  pBkgFrameAttrs Pic x(4).
           03  sBkgFrame      Pic 9(4) Comp Value 2240.
           03  pbTitle        Pic x(4).
           03  cbTitle        Pic 9(4) Comp Value 0.
           03  fCenterTitle   Pic x.
           
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(18) VALUE "I M P O R T E D".
           03  FILLER         PIC X(17) VALUE "C O S T I N G".
           03  FILLER         PIC X(55) VALUE "A U D I T   T R A I L".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(6) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(56) VALUE ALL "*".
           03  FILLER         PIC X(47) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(11) VALUE " ".
           03  FILLER         PIC X(24) VALUE "SUPPLIER          :".
           03  H3-SUPPLIERNO  PIC X(7).
           03  FILLER         PIC X(90) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(11) VALUE " ".
           03  FILLER         PIC X(21) VALUE "INVOICE NO        :".
           03  H3-INVNO       PIC X(14).
           03  FILLER         PIC X(16) VALUE "TOTAL RAND DUTY:".
           03  H5-TOTRANDDUTY PIC Z(7)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(17) VALUE "ORDER NO     :".
           03  H7-ORDERNO     PIC X(22).
           03  FILLER         PIC X(17) VALUE " ".
       01  HEAD5.
           03  FILLER         PIC X(11) VALUE " ".
           03  FILLER         PIC X(27) VALUE "CURRENCY DESC     :".
           03  H4-CURRDESC    PIC X(6).
           03  FILLER         PIC X(2) VALUE " ".
           03  FILLER         PIC X(16) VALUE "RAND FREIGHT   :".
           03  H6-RANDFRGTAMT PIC Z(7)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(17) VALUE "DATE RECEIVED:".
           03  H7-DATERECVD   PIC X(10).
           03  FILLER         PIC X(31) VALUE " ".
       01  HEAD6.
           03  FILLER         PIC X(11) VALUE " ".
           03  FILLER         PIC X(21) VALUE "CURRENCY RATE     :".
           03  H7-EXCHANGERATE PIC Z(3)9.99999.
           03  FILLER         PIC X(4) VALUE " ".
           03  FILLER         PIC X(16) VALUE "WHARFAGE       :".
           03  H6-WHARFAGE    PIC Z(7)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(17) VALUE "RATE PAID    :".
           03  H8-RATEPAID    PIC Z(3)9.99999.
           03  FILLER         PIC X(30) VALUE " ".
       01  HEAD7.
           03  FILLER         PIC X(11) VALUE " ".
           03  FILLER         PIC X(20) VALUE "FOR. INVOICE AMT  :".
           03  H4-FORINVAMT   PIC Z(7)9.99BB.
           03  FILLER         PIC X(2) VALUE " ".
           03  FILLER         PIC X(16) VALUE "CLEARING       :".
           03  H6-CLEARING    PIC Z(7)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(16) VALUE "RAND AMT PAID:".
           03  H8-RANDPAID    PIC Z(7)9.99.
           03  FILLER         PIC X(30) VALUE " ".
       01  HEAD8.
           03  FILLER         PIC X(11) VALUE " ".
           03  FILLER         PIC X(20) VALUE "FOREIGN FRGHT AMT :".
           03  H4-FORFRGTAMT  PIC Z(7)9.99.
           03  FILLER         PIC X(4) VALUE " ".
           03  FILLER         PIC X(16) VALUE "SURCHARGE      :".
           03  H6-RANDSURCH   PIC Z(7)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(21) VALUE "MARK UP %    :".
           03  H7-MARKUPPERC  PIC Z99.99.
           03  FILLER         PIC X(30) VALUE " ".
       01  HEAD8A.
           03  FILLER         PIC X(11) VALUE " ".
           03  FILLER         PIC X(20) VALUE "GROSS RAND AMT    :".
           03  H5-GROSSRANDINVAMT PIC Z(7)9.99.
           03  FILLER         PIC X(4) VALUE " ".
           03  FILLER         PIC X(16) VALUE "TOTAL CHARGES  :".
           03  H6-TOTCHARGES  PIC Z(7)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(21) VALUE "ON COST %    :".
           03  H7-ONCOSTPERC  PIC Z99.99.
           03  FILLER         PIC X(30) VALUE " ".
       01  HEAD9.
           03  FILLER         PIC X(6) VALUE "LINE#".
           03  FILLER         PIC X(25) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(6) VALUE "QTY".
           03  FILLER         PIC X(10) VALUE "COST EACH".
           03  FILLER         PIC X(8) VALUE "DUTY %".
           03  FILLER         PIC X(11) VALUE "DUTY AMT".
           03  FILLER         PIC X(8) VALUE "DISC.%".
           03  FILLER         PIC X(9) VALUE "SURC.%".
           03  FILLER         PIC X(11) VALUE "LD COST".
           03  FILLER         PIC X(11) VALUE "S/PRICE".
           03  FILLER         PIC X(11)  VALUE "O/PRICE".
           03  FILLER         PIC X(10) VALUE "O/PR %".
           03  FILLER         PIC X(6)  VALUE " IND".
       01  DETAIL-LINE.
           03  D-LINE         PIC Z(2)9.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-STOCKNO      PIC X(23).
           03  D-QTY          PIC Z(4)9.
      *    03  FILLER         PIC X(2) VALUE " ".
           03  D-COSTEACH     PIC Z(7)9.999.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-DUTYPERC     PIC Z9.99.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-DUTAMT       PIC Z(4)9.99.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-DISCPERC     PIC ZZ9.99.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-SURCPERC     PIC ZZ9.99.
      *    03  FILLER         PIC X(2) VALUE " ".
           03  D-LDCOST       PIC Z(7)9.99.
      *    03  FILLER         PIC X(2) VALUE " ".
           03  D-SPRICE       PIC Z(7)9.99.
      *    03  FILLER         PIC X(2) VALUE " ".
           03  D-OSPRICE      PIC Z(7)9.99.
           03  FILLER         PIC X(3).
           03  D-PERC         PIC Z(3)9.99-.
           03  FILLER         PIC X(5).
           03  D-IND          PIC 99.
           03  FILLER         PIC X(2).
       01  ORDER-LINE.
           03  O-NAME         PIC X(12) VALUE " ".
           03  O-IND          PIC ZZ.
           03  FILLER         PIC X(4) VALUE " ".
           03  O-NO           PIC X(25) VALUE " ".
           03  O-VALUE        PIC Z(6)9.99.
       01  TOTAL-LINE.
           03  TOT-DESC       PIC X(25) VALUE " ".
           03  TOT-COST       PIC Z(7)9.99.
           03  FILLER         PIC X(6) VALUE " ".
           03  TOT-DUTY-DESC  PIC X(25) VALUE " ".
           03  TOT-DUTY       PIC Z(7)9.99.
           03  FILLER         PIC X(58) VALUE " ".
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
           PERFORM CLEAR-SCREEN
           MOVE 0320 TO POS
           DISPLAY "** IMPORTS COSTING Program  **" AT POS
           MOVE 0420 TO POS
           DISPLAY "******************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
           MOVE WS-PRINTER TO WS-DOTPRINTER.
           MOVE 2510 TO POS.
           DISPLAY "Program Loading...... " AT POS.
           MOVE "StReImMt"      TO F-FORMNAME.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
        CONTROL-010.
           MOVE "StReImMt"      TO F-FORMNAME
           PERFORM OPEN-060 THRU OPEN-900.
           PERFORM CLEAR-FIELDS.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           MOVE 1 TO F-INDEX SUB-1.
           
           GO TO CONTROL-010.
       CONTROL-020.
           Move 9 To Ws-PrinterNumber (21)
           Move 9 To Ws-PrinterType (21).
           Copy "PrinterStSpecial".
           MOVE WS-LABELPRINTER TO WS-PRINTER.
       CONTROL-999.
           EXIT.
      * 
       CHANGE-PRINTER SECTION.
       CPS-005.
            PERFORM CLEAR-SCREEN.
            Copy "PrinterAccept".
            PERFORM DISPLAY-FORM.
       CPS-999.
            EXIT.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO WS-MESSAGE
            MOVE 2610 TO POS
            DISPLAY WS-MESSAGE AT POS
            PERFORM ERROR1-020
            PERFORM ERROR-020.
       GET-001.
            MOVE "Printer:"   TO WS-PRN-FIL
            MOVE Ws-Printer   To WS-PRN-NAME
            MOVE 0449 TO POS
            DISPLAY WS-PRINTER-INFO AT POS.
       GET-002.
            PERFORM CLEAR-ORDER-TOTALS.
            PERFORM CLEAR-CRJRN-RECORD.
            MOVE 0 TO WS-FOREIGN-CHECK-VAL
                      WS-DUTY-AMOUNT
                      IMRE-RATEPAID
                      IMRE-RAND-AMT-PAID
                      WS-ORDER-DISC
                      SUB-25.
            MOVE "N" TO IMRE-UPDATED-YN.
            MOVE "  " TO WS-UPDATE 
                         WS-UP-SELL
                         WS-UP-FORGN
                         WS-DISCOUNT-ENTRY
                         WS-BYORD-STOCK.
            PERFORM CLEAR-ORDER-TOTALS.
       GET-010.
           MOVE 2910 TO POS
           DISPLAY
           "Suppliers-Order Inq By : ALL 'X'=STOCK, PORDER BY ORDER,"
            AT POS
           MOVE 3020 TO POS
           DISPLAY "BLANK = StockInq, OR Enter SUPPLIER NAME." AT POS.

            MOVE " "           TO WS-NOT-THERE
            MOVE 1             TO B-ORDERIND (1).
            MOVE "SUPPLIERNUM" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0B"
                GO TO GET-010.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                PERFORM CLEAR-FIELDS
                MOVE 0 TO WS-FOREIGN-CHECK-VAL
                PERFORM ERROR-020
                PERFORM GET-001
                GO TO GET-010.
            MOVE 7           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO IMRE-SUPPLIER
                                  WS-SUPPLIER.
            IF IMRE-SUPPLIER = SPACES
                CLOSE STOCK-MASTER
                PERFORM CLEAR-SCREEN
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                PERFORM GET-001
                GO TO GET-010.
            IF IMRE-SUPPLIER = "XXXXXXX"
                CLOSE STOCK-MASTER
                CLOSE OUTSTANDING-ORDERS
                PERFORM CLEAR-SCREEN
                CALL WS-ORDER-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-ORDER-INQUIRY
                PERFORM OPEN-000
                PERFORM OPEN-002
                PERFORM DISPLAY-FORM
                PERFORM GET-001
                GO TO GET-010.
            IF IMRE-SUPPLIER = "PORDER"
                CLOSE STOCK-MASTER
                CLOSE OUTSTANDING-ORDERS
                PERFORM CLEAR-SCREEN
                CALL WS-PORDER-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-PORDER-INQUIRY
                PERFORM OPEN-000
                PERFORM OPEN-002
                PERFORM DISPLAY-FORM
                PERFORM GET-001
                GO TO GET-010.
       GET-020.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "INVOICENUM" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-010.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO IMRE-INVOICENUM
                                WS-INVOICENUM.
            IF IMRE-INVOICENUM = SPACES
                GO TO GET-020.
            PERFORM FIND-IMPORT.
            IF WS-NOT-THERE = " "
                GO TO GET-160.
            IF WS-NOT-THERE = "X"
                MOVE " " TO WS-NOT-THERE
                GO TO GET-999.
       GET-030.
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
            MOVE F-NAMEFIELD TO IMRE-CURRENCY.
            
            PERFORM READ-CURRENCY.
            IF WS-CURRENCY-ST1 = 23 OR 35 OR 49
              GO TO GET-030.
            PERFORM ERROR-020.
            PERFORM ERROR1-020.
       GET-040.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "EXCHANGERATE"    TO F-FIELDNAME
            MOVE 12                TO F-CBFIELDNAME
            MOVE 10                TO F-CBFIELDLENGTH
            MOVE IMRE-EXCHANGERATE TO F-EDNAMEFIELDNUMDEC
            MOVE 10                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUM-DEC.
            
            MOVE 2910 TO POS
            DISPLAY "1      = R" AT POS
            COMPUTE WS-CURRENCY-TEMP = 1 / IMRE-EXCHANGERATE
            ADD 2 TO POS 
            DISPLAY IMRE-CURRENCY AT POS
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
                GO TO GET-030.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            
            IF F-EXIT-CH = X"1D"
            COMPUTE NUMERIC-RATE = 1 / NUMERIC-RATE.
            
            MOVE NUMERIC-RATE TO IMRE-EXCHANGERATE
                                 F-EDNAMEFIELDNUMDEC.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUM-DEC.
            IF IMRE-EXCHANGERATE = 0
               MOVE "EXCHANGE RATE CANNOT BE ZERO" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-040.
            
            MOVE 2910 TO POS
            DISPLAY "1      = R" AT POS
            COMPUTE WS-CURRENCY-TEMP = 1 / IMRE-EXCHANGERATE
            ADD 2 TO POS 
            DISPLAY IMRE-CURRENCY AT POS
            ADD 8 TO POS
            MOVE WS-CURRENCY-TEMP TO WS-EXCHANGE-DIS
            DISPLAY WS-EXCHANGE-DIS AT POS.
       GET-050.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "INVAMTFOREIGN" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-040.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO IMRE-INVAMTFOREIGN
                                 F-EDNAMEFIELDFORTOTAL.
            PERFORM WRITE-FIELD-FORTOTAL.

            PERFORM ERROR-020.
            PERFORM ERROR1-020.
       GET-060.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "FREIGHTAMTFOREIGN" TO F-FIELDNAME.
            MOVE 17                  TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-050.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO IMRE-FREIGHTAMTFOREIGN
                                 F-EDNAMEFIELD99mil.
            PERFORM WRITE-FIELD-99Mil.
       GET-070.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "RANDINVAMT" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            MOVE 11           TO F-CBFIELDLENGTH.
            COMPUTE IMRE-RANDINVAMTGROSS ROUNDED =
               IMRE-INVAMTFOREIGN / IMRE-EXCHANGERATE.
            MOVE IMRE-RANDINVAMTGROSS TO F-EDNAMEFIELD99Mil.
            PERFORM WRITE-FIELD-99Mil.
       GET-080.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "TOTALRANDDUTY" TO F-FIELDNAME.
            MOVE 13              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-060.
            MOVE 11              TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD     TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE    TO IMRE-TOTALDUTYRAND
                                       F-EDNAMEFIELD99Mil.
            PERFORM WRITE-FIELD-99Mil.
       GET-090.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "RANDFREIGHTAMT" TO F-FIELDNAME.
            MOVE 14               TO F-CBFIELDNAME.
            MOVE 11               TO F-CBFIELDLENGTH.
            IF IMRE-FREIGHTAMTFOREIGN > 0
                COMPUTE IMRE-FREIGHTRAND ROUNDED =
                   IMRE-FREIGHTAMTFOREIGN / IMRE-EXCHANGERATE
                MOVE IMRE-FREIGHTRAND TO F-EDNAMEFIELD99Mil
                PERFORM WRITE-FIELD-99Mil.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-080.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO IMRE-FREIGHTRAND
                                 F-EDNAMEFIELD99Mil.
            PERFORM WRITE-FIELD-99Mil.
       GET-100.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "WARFAGE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-090.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO IMRE-WARFAGE
                                 F-EDNAMEFIELD99Mil.
            PERFORM WRITE-FIELD-99Mil.
       GET-110.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "CLEARINGCHGS" TO F-FIELDNAME.
            MOVE 12             TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-100.
            MOVE 11             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE   TO IMRE-CLEARING
                                      F-EDNAMEFIELD99Mil.
            PERFORM WRITE-FIELD-99Mil.
       GET-120.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "SURCHARGE" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-110.
            MOVE 11            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD   TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE  TO IMRE-SURCHARGE-RAND
                                     F-EDNAMEFIELD99Mil.
            PERFORM WRITE-FIELD-99Mil.
       GET-130.
            COMPUTE IMRE-TOTALCHARGES = IMRE-FREIGHTRAND +
                IMRE-WARFAGE + IMRE-CLEARING.
            IF IMRE-FREIGHTAMTFOREIGN > 0
            COMPUTE IMRE-NETT-RAND-INV-AMT =
                (IMRE-RANDINVAMTGROSS - (IMRE-FREIGHTAMTFOREIGN / 
                                         IMRE-EXCHANGERATE))
            ELSE
                MOVE IMRE-RANDINVAMTGROSS TO IMRE-NETT-RAND-INV-AMT.
            MOVE "TOTALCHARGES"    TO F-FIELDNAME
            MOVE 12                TO F-CBFIELDNAME
            MOVE 11                TO F-CBFIELDLENGTH
            MOVE IMRE-TOTALCHARGES TO F-EDNAMEFIELD99Mil
            PERFORM WRITE-FIELD-99Mil.
            GO TO GET-150.
       GET-140.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "ORDERNUM" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-120.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ORDER (1).
       GET-150.
            IF WS-NOT-THERE = " "
                GO TO GET-160.

            MOVE "DATERECEIVED" TO F-FIELDNAME.
            MOVE 12             TO F-CBFIELDNAME.
            MOVE 10             TO F-CBFIELDLENGTH.
            MOVE WS-DATE        TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE   TO F-NAMEFIELD.
            MOVE 10             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-155.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATERECEIVED" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-120.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO GET-155.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO IMRE-DATERECEIVED.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO GET-155.

            MOVE "RATEPAID"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE 0           TO IMRE-RATEPAID
                                WS-RATEPAID.
            MOVE WS-RATEPAID TO F-EDNAMEFIELDNUMDEC.
            MOVE 10          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUM-DEC.

            MOVE "RANDAMTPAID"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE 0                TO IMRE-RAND-AMT-PAID
                                       WS-RAND-AMT-PAID.
            MOVE WS-RAND-AMT-PAID TO F-EDNAMEFIELD99Mil
            MOVE 11               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99Mil.

            GO TO GET-180.
       GET-160.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "RATEPAID" TO F-FIELDNAME.
            MOVE 8                     TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01" AND WS-NOT-THERE = " "
                GO TO GET-010
            ELSE
                IF F-EXIT-CH = X"01" AND WS-NOT-THERE NOT = " "
                GO TO GET-150.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO IMRE-RATEPAID
                                 WS-RATEPAID
                                 F-EDNAMEFIELDNUMDEC.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUM-DEC.
       GET-170.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "RANDAMTPAID"         TO F-FIELDNAME.
            MOVE 11                    TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-160.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO IMRE-RAND-AMT-PAID
                                   WS-RAND-AMT-PAID
                                    F-EDNAMEFIELD99Mil.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99mil.
       GET-180.
            MOVE "                   " TO F-NAMEFIELD.
            IF WS-NOT-THERE = " "
                PERFORM UPDATE-SECOND-TIME
                GO TO GET-999.
            MOVE "MARKUP" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-150.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO IMRE-MARKUPPERCENT
                                 F-EDNAMEFIELDPERC.
            PERFORM WRITE-FIELD-PERC.
            IF IMRE-MARKUPPERCENT = 0 
                GO TO GET-180.
       GET-190.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "ONCOSTPERCENT" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE 6 TO F-CBFIELDLENGTH.
            IF IMRE-NETT-RAND-INV-AMT NOT = 0
              COMPUTE IMRE-ONCOSTPERCENT ROUNDED =
              (IMRE-TOTALCHARGES / IMRE-NETT-RAND-INV-AMT) * 100. 
            MOVE IMRE-ONCOSTPERCENT TO F-EDNAMEFIELDPERC.
            PERFORM WRITE-FIELD-PERC.
            MOVE 0 TO F-EDNAMEFIELDPERC.
            PERFORM FILL-BODY.
            IF SUB-1 > 400
                GO TO GET-900.
            IF WS-ABOVE-BODY = "1"
      *          MOVE " " TO WS-ABOVE-BODY
                GO TO GET-180.
       GET-900.
            PERFORM ERROR-020.
            PERFORM ORDER-NUMBER-ENTRY.
            PERFORM COMPUTE-FOREIGN-ORDER-VALUE.
            MOVE 1 TO SUB-1.
            MOVE WS-DOTPRINTER TO WS-PRINTER-SAVE.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            PERFORM PRINT-ROUTINE.
            MOVE 66 TO LINE-CNT.
            CLOSE PRINT-FILE.
            PERFORM SEND-REPORT-TO-PRINTER.

            ADD IMRE-FREIGHTAMTFOREIGN TO WS-FOREIGN-CHECK-VAL.
            IF IMRE-INVAMTFOREIGN = WS-FOREIGN-CHECK-VAL
               GO TO GET-920.
            MOVE "FOREIGN AMOUNTS DO NOT BALANCE!!!!!!" TO WS-MESSAGE.
            PERFORM ERROR-MESSAGE.
       GET-920.
           MOVE " " TO WS-UPDATE.
           PERFORM ERROR-020.
           PERFORM CLEAR-010.
           MOVE 3010 TO POS.
           DISPLAY "IS IT ALRIGHT TO UPDATE?? ENTER Y OR N: [ ]" AT POS.
           ADD 41 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-UPDATE.

      *    ACCEPT WS-UPDATE AT POS.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2
               GO TO GET-925
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-920.
       GET-925.
           IF WS-UPDATE = "Y"
              GO TO GET-930.
           IF WS-UPDATE = "N"
              COMPUTE WS-FOREIGN-CHECK-VAL =
                      WS-FOREIGN-CHECK-VAL - IMRE-FREIGHTAMTFOREIGN
              MOVE "2" TO WS-ABOVE-BODY
              PERFORM ERROR-020
              GO TO GET-190.
           MOVE "WHAT ARE YOU TRYING TO ANSWER??PRINT-LABEL???"
              TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
           GO TO GET-920.
       GET-930.
           MOVE " " TO WS-UP-SELL.
           PERFORM ERROR-020.
           PERFORM CLEAR-010.
           MOVE 3010 TO POS.
           DISPLAY "  UPDATE SELLING PRICE?? ENTER Y OR N:  [ ]" 
               AT POS.
           ADD 41 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-UP-SELL.

      *     ACCEPT WS-UP-SELL AT POS.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2
               GO TO GET-935
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-930.
       GET-935.
            IF WS-UP-SELL = "Y" OR = "N"
               GO TO GET-940.
            MOVE "WHAT ARE YOU TRYING TO ANSWER?????" TO WS-MESSAGE.
            PERFORM ERROR-MESSAGE.
            MOVE " " TO WS-UP-SELL.
            GO TO GET-930.
       GET-940.
           MOVE " " TO WS-UP-FORGN.
           PERFORM ERROR-020.
           PERFORM CLEAR-010.
           MOVE 3010 TO POS.
           DISPLAY "  UPDATE FOREIGN COSTS ? ENTER Y OR N:  [ ]" AT POS.
           ADD 41 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-UP-FORGN.

      *     ACCEPT WS-UP-FORGN AT POS.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2
               GO TO GET-945
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-940.
       GET-945.
            IF WS-UP-FORGN = "Y" OR = "N"
               GO TO GET-950.
            MOVE "WHAT ARE YOU TRYING TO ANSWER?????" TO WS-MESSAGE
            PERFORM ERROR-MESSAGE
            MOVE " " TO WS-UP-FORGN
            GO TO GET-940.
       GET-950.
            MOVE " " TO WS-MESSAGE
            PERFORM ERROR-020
            MOVE 2810 TO POS
            DISPLAY WS-MESSAGE AT POS.
            PERFORM ERROR-020
            PERFORM ERROR1-020.
            PERFORM UPDATE-STOCK.
            MOVE " " TO WS-MESSAGE
            PERFORM ERROR1-020.
            IF WS-ST-PRINT-LABELS = "N"
               GO TO GET-999.

            PERFORM WRITE-FOREIGN-CRJRN-TRANS.
            PERFORM CLEAR-CRJRN-RECORD.
            PERFORM WRITE-FREIGHT-CRJRN-TRANS.
       GET-960.
            MOVE " " TO WS-YN
            PERFORM CLEAR-010
            MOVE 2910 TO POS
            DISPLAY "DO YOU WISH TO PRINT STOCK LABELS: [ ]" AT POS
            ADD 36 TO POS

            MOVE ' '       TO CDA-DATA.
            MOVE 1         TO CDA-DATALEN.
            MOVE 26        TO CDA-ROW.
            MOVE 45        TO CDA-COL.
            MOVE CDA-WHITE TO CDA-COLOR.
            MOVE 'F'       TO CDA-ATTR.
            PERFORM CTOS-ACCEPT.
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
       CALC-POS-OF-CURSOR SECTION.
       CPOC-005.
             IF SUB-1SAVE < 7
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
            IF WS-ABOVE-BODY NOT = " "
               MOVE " " TO WS-ABOVE-BODY
               MOVE 1 TO SUB-1 SUB-2 SUB-3 F-INDEX
               PERFORM SCROLL-NEXT
               PERFORM SCROLL-PREVIOUS
            ELSE
               MOVE 1 TO SUB-1 SUB-2 SUB-3 SUB-25 F-INDEX.
       FILL-005.
            MOVE 2810 TO POS.
            DISPLAY "    BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
            MOVE 0 TO WS-B-QUANTITY
                      WS-B-COST
                      WS-B-DISCOUNTPERC.
            MOVE 2510 TO POS
            DISPLAY
            "TO CHANGE THE PRINTER TO PRINT REPORT TO, PRESS <ALT-P>."
               AT POS.
            MOVE 2615 TO POS
            DISPLAY
            "TO SEARCH FOR P/O NUMBER PRESS <ALT-F8>...."
               AT POS.
            MOVE 2710 TO POS
            DISPLAY
            "PRESS <ALT-Z> TO GO INTO ZOOMBOX MODE TO CALL UP STOCKINQ."
               AT POS.
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ORDERIND" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE SUB-1 TO SUB-6.
            SUBTRACT 1 FROM SUB-6.
            IF B-ORDERIND (SUB-1) > 0
                MOVE B-ORDERIND (SUB-1) TO F-EDNAMEFIELDIND.
            IF SUB-6 = 0
                GO TO FILL-012.
            IF B-ORDERIND (SUB-1) = 0
               MOVE B-ORDERIND (SUB-6) TO B-ORDERIND (SUB-1)
                                          F-EDNAMEFIELDIND.
       FILL-012.
            MOVE 2 TO F-CBFIELDLENGTH.
            IF B-ORDERIND (SUB-1) < 10
               MOVE B-ORDERIND (SUB-1) TO F-EDNAMEFIELDIND1
               PERFORM WRITE-FIELD-INDEX1
            ELSE
               PERFORM WRITE-FIELD-INDEX.
            PERFORM USER-FILL-FIELD.
            
      ***********************************************
      * NEW SECTION TO ALTER THE PRINTER SELECTED.  *
      * <CODE-p> = X"F0"  <CODE-SHIFT-P> = X"D0"    *
      ***********************************************
            IF F-EXIT-CH = X"F0" OR = X"D0"
                MOVE SUB-1   TO SUB-1SAVE
                MOVE F-INDEX TO F-INDEXSAVE
                PERFORM CHANGE-PRINTER
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                PERFORM FI-010 THRU FI-020
                PERFORM GET-001
                PERFORM CALC-POS-OF-CURSOR
                GO TO FILL-005.
      ***********************************************
      *ZOOMBOX MODE                                 *
      * <CODE-z> = X"FA"  <CODE-SHIFT-Z> = X"DA"    *
      ***********************************************
      *IN CTOS: <CODE-Z>; <ALT-Z> IN LINUX
           IF F-EXIT-CH = X"FA" OR = X"DA"
                MOVE SUB-1   TO SUB-1SAVE
                MOVE F-INDEX TO F-INDEXSAVE
                PERFORM CLEAR-SCREEN
                CALL WS-STOCK-INQUIRY USING WS-LINKAGE
                CANCEL WS-STOCK-INQUIRY
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                PERFORM FI-010 THRU FI-020
                PERFORM GET-001
                PERFORM CALC-POS-OF-CURSOR
                GO TO FILL-005.

                
            IF F-EXIT-CH = X"01" AND F-INDEX = 1
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999.
            IF F-EXIT-CH = X"01" AND F-INDEX > 1
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-010.
            IF F-EXIT-CH = X"01" AND SUB-1 > 7
                PERFORM SCROLL-PREVIOUS
                GO TO FILL-010.
            IF F-EXIT-CH = X"0B" AND F-INDEX < 7
                ADD 1 TO F-INDEX SUB-1
                GO TO FILL-010.
            IF F-EXIT-CH = X"0B" AND F-INDEX = 7
                PERFORM SCROLL-NEXT
                GO TO FILL-010.
                
            IF F-EXIT-CH = X"11"
                PERFORM SCROLL-NEXT
                GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
                PERFORM SCROLL-NEXT-PAGE
                GO TO FILL-010.
            IF F-EXIT-CH = X"05"
                PERFORM SCROLL-PREVIOUS
                GO TO FILL-010.
            IF F-EXIT-CH = X"13"
                PERFORM SCROLL-DOWN
                GO TO FILL-010.
       FILL-013.
      ********************
      * TAB CHARACTER    *
      ********************
           IF F-EXIT-CH = X"09"
                MOVE SPACES TO WS-MESSAGE
                MOVE 2510 TO POS
                DISPLAY WS-MESSAGE AT POS
                MOVE 2610 TO POS
                DISPLAY WS-MESSAGE AT POS
                MOVE 2710 TO POS
                DISPLAY WS-MESSAGE AT POS
                GO TO FILL-999.
      *****************************************************
      * <CODE-TAB>=X"89" IN CTOS, <ALT-F8>=X"9D" IN LINUX *
      *****************************************************
           IF F-EXIT-CH = X"89" OR = X"9D"
                MOVE SUB-1 TO SUB1-SAVE
                PERFORM GET-NEW-ORDER-ENTERED
                MOVE SUB1-SAVE TO SUB-1
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                GO TO FILL-010.
      ******************************************************
      * <CODE-ESC>=X"87" IN CTOS, <ALT-F10>=X"9F" IN LINUX *
      ******************************************************
           IF F-EXIT-CH = X"07"
               MOVE 
               "PRESS 'Alt-F10' TO CANCEL A LINE ITEM, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO  FILL-010.
           IF F-EXIT-CH = X"87" OR = X"9F"
             MOVE SUB-1 TO SUB-7
             COMPUTE WS-INVERSEDISC =
             ((100 - B-DISCOUNTPERC (SUB-1)) / 100)
             COMPUTE WS-FOREIGN-CHECK-VAL ROUNDED =
             (WS-FOREIGN-CHECK-VAL -
             ((B-QUANTITY (SUB-1) * B-COST (SUB-1)) * WS-INVERSEDISC))

              PERFORM CANCEL-TRANSACTION
              SUBTRACT 1 FROM SUB-25
              PERFORM COMPUTE-RUNNING-TOTAL
              MOVE 1 TO SUB-1
                        F-INDEX
              PERFORM SCROLL-NEXT
              PERFORM SCROLL-PREVIOUS
              IF SUB-25 > 8
                   SUBTRACT 5 FROM SUB-25
                   MOVE SUB-25 TO SUB-1
                   PERFORM SCROLL-NEXT
                   ADD 5 TO SUB-25
                   MOVE 2910 TO POS
                   DISPLAY "RUNNING TOTAL:              DUTY AMOUNT:"
                    AT POS
                   MOVE 2925 TO POS
                   MOVE WS-FOREIGN-CHECK-VAL TO WS-FOREIGN-MASK
                   DISPLAY WS-FOREIGN-MASK AT POS
                   MOVE 2950 TO POS
                   MOVE WS-DUTY-AMOUNT TO WS-FOREIGN-MASK
                   DISPLAY WS-FOREIGN-MASK AT POS
                   GO TO FILL-005
              ELSE
                   MOVE 2910 TO POS
                   DISPLAY "RUNNING TOTAL:              DUTY AMOUNT:"
                    AT POS
                   MOVE 2925 TO POS
                   MOVE WS-FOREIGN-CHECK-VAL TO WS-FOREIGN-MASK
                   DISPLAY WS-FOREIGN-MASK AT POS
                   MOVE 2950 TO POS
                   MOVE WS-DUTY-AMOUNT TO WS-FOREIGN-MASK
                   DISPLAY WS-FOREIGN-MASK AT POS
                   GO TO FILL-005.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO B-ORDERIND (SUB-1)
                                 F-EDNAMEFIELDIND.
            PERFORM WRITE-FIELD-INDEX.
            IF B-ORDERIND (SUB-1) = 0
                MOVE 2801 TO POS
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-010.
       FILL-015.
            MOVE SPACES TO WS-MESSAGE
            MOVE 2510 TO POS
            DISPLAY WS-MESSAGE AT POS.
            MOVE 2610 TO POS
            DISPLAY WS-MESSAGE AT POS.
            MOVE 2710 TO POS
            DISPLAY WS-MESSAGE AT POS.
            
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            PERFORM FILL-005.
            IF F-EXIT-CH = X"01"
                GO TO FILL-010.    
            IF F-EXIT-CH = X"07"
                GO TO FILL-013.  
      *
      *<CODE-RETURN> TO USE LAST-LINE STOCK-ITEM
           IF F-EXIT-CH = X"8A"
               MOVE WS-STOCKNUMBER TO F-NAMEFIELD
               MOVE 15             TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-015.
      *
      *<CODE-NEXT-PAGE> TO READ NEXT STOCK ITEM.
            IF F-EXIT-CH = X"8C"
             IF SUB-1 NOT < SUB-25
                PERFORM READ-NEXT-STOCK-ITEM
             IF WS-STOCK-ST1 = 0
                PERFORM DISPLAY-LINE-ITEMS
                GO TO FILL-015
             ELSE
                GO TO FILL-015.
            IF F-NAMEFIELD = "    "
                GO TO FILL-015.
            IF B-COST (SUB-1) > 0
                MOVE B-COST (SUB-1) TO WS-B-COST.
            MOVE F-NAMEFIELD TO B-STOCKNUMBER (SUB-1)
                                WS-STOCKNUMBER.
            PERFORM READ-STOCK.
            IF ST-DESCRIPTION1 = "NOT THERE!!!"
                MOVE "INVALID STOCK NUMBER" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-015.

            MOVE "STOCKDESCRIPTION1" TO F-FIELDNAME.
            MOVE 17                  TO F-CBFIELDNAME.
            MOVE 20                  TO F-CBFIELDLENGTH.
            MOVE ST-DESCRIPTION1     TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKDESCRIPTION2" TO F-FIELDNAME.
            MOVE 17                  TO F-CBFIELDNAME.
            MOVE 20                  TO F-CBFIELDLENGTH.
            MOVE ST-DESCRIPTION2     TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

      *      IF B-LINE-COSTED (SUB-1) = "Y"
      *          GO TO FILL-019.

            IF B-LINE-COSTED (SUB-1) NOT = "Y"
                MOVE "COSTEACH"     TO F-FIELDNAME
                MOVE 8              TO F-CBFIELDNAME
                MOVE ST-FOREIGNCOST TO F-EDNAMEFIELDFOREIGN99
                                       B-COST (SUB-1)
                MOVE 12             TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-FOREIGN99
            ELSE
                MOVE "COSTEACH"     TO F-FIELDNAME
                MOVE 8              TO F-CBFIELDNAME
                MOVE B-COST (SUB-1) TO F-EDNAMEFIELDFOREIGN99
                MOVE 12             TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-FOREIGN99.
                
            MOVE "DUTYPERCENT"  TO F-FIELDNAME.
            MOVE 11             TO F-CBFIELDNAME.
            MOVE ST-DUTYPERCENT TO B-DUTYPERC (SUB-1)
                                   F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.

      * REMOVED AS THE FORM COULD NOT HANDLE THE NEW SIZE FOR ALL
      * RAND AMT'S BEING PULLED TO 99,999,999.99
      *      MOVE "SURCHPERCENT" TO F-FIELDNAME.
      *      MOVE 12             TO F-CBFIELDNAME.
      *      MOVE ST-SURCHARGE   TO B-SURCHPERC (SUB-1)
      *                             F-EDNAMEFIELDAMOUNTDIS.
      *      MOVE 5              TO F-CBFIELDLENGTH.
      *      PERFORM WRITE-FIELD-AMOUNTDIS.
       FILL-019.
            MOVE B-QUANTITY (SUB-1)     TO WS-B-QUANTITY.
            MOVE B-DISCOUNTPERC (SUB-1) TO WS-B-DISCOUNTPERC.    
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QUANTITY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO FILL-015.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO B-QUANTITY (SUB-1)
                                 F-EDNAMEFIELDQTY.
            PERFORM WRITE-FIELD-QTY.
            IF B-QUANTITY (SUB-1) = 0
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-020.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COSTEACH" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO FILL-020.
            MOVE 12           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO B-COST (SUB-1).
            MOVE 12             TO F-CBFIELDLENGTH
            MOVE B-COST (SUB-1) TO F-EDNAMEFIELDFOREIGN99
            PERFORM WRITE-FIELD-FOREIGN99.

            MOVE 0 TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE SUB-1 TO SUB-6.
            SUBTRACT 1 FROM SUB-6. 
            IF SUB-6 < 1
               MOVE 1 TO SUB-6.
            IF B-DISCOUNTPERC (SUB-1) > 0
                MOVE B-DISCOUNTPERC (SUB-1) TO F-EDNAMEFIELDAMOUNTDIS.
            IF SUB-6 = 0
                GO TO FILL-040.
            IF B-LINE-COSTED (SUB-1) NOT = "Y"
               MOVE B-DISCOUNTPERC (SUB-6) TO B-DISCOUNTPERC (SUB-1)
                                              F-EDNAMEFIELDAMOUNTDIS.

            IF F-EXIT-CH NOT = X"1D"
             IF SUB-1 > 1
                PERFORM DISPLAY-REST-OF-LINE
                GO TO FILL-090
             ELSE
                GO TO FILL-040.
       FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISCOUNTPERCENT" TO F-FIELDNAME.
            MOVE 15                TO F-CBFIELDNAME.
            MOVE 6                 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                MOVE "NO CHANGES CAN BE MADE AT THIS STAGE"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-040.
            PERFORM READ-FIELD-ALPHA.
            IF F-CBFIRSTLINE = 0
                GO TO FILL-050.
            ADD 2             TO F-CBFIRSTLINE.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO B-DISCOUNTPERC (SUB-1).
       FILL-050.
            IF SUB-25 > 1
               PERFORM COMPUTE-RUNNING-TOTAL.

            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DUTYPERCENT" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                MOVE "NO CHANGES CAN BE MADE AT THIS STAGE"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-050.
            PERFORM READ-FIELD-ALPHA.
            IF F-CBFIRSTLINE = 0
                GO TO FILL-050.
            ADD 5 TO F-CBFIRSTLINE.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO B-DUTYPERC (SUB-1).
       FILL-060.
            COMPUTE B-DUTYAMOUNT (SUB-1) ROUNDED =
                B-COST (SUB-1) / IMRE-EXCHANGERATE.
            COMPUTE B-DUTYAMOUNT (SUB-1) ROUNDED =
                  (B-DUTYAMOUNT (SUB-1) -
                  (B-DUTYAMOUNT (SUB-1) *
                  B-DISCOUNTPERC (SUB-1) / 100)).
            COMPUTE B-DUTYAMOUNT (SUB-1) ROUNDED =
                  (B-DUTYAMOUNT (SUB-1) *
                  B-DUTYPERC (SUB-1) / 100).
       FILL-068.
      *      MOVE "                    " TO F-NAMEFIELD.
      *      MOVE "SURCHPERCENT" TO F-FIELDNAME.
      *      MOVE 12             TO F-CBFIELDNAME.
      *      MOVE 5              TO F-CBFIELDLENGTH.
      *      PERFORM USER-FILL-FIELD.
      *      IF F-EXIT-CH = X"01"
      *          MOVE "NO CHANGES CAN BE MADE AT THIS STAGE"
      *          TO WS-MESSAGE
      *          PERFORM ERROR-MESSAGE
      *          GO TO FILL-068.
      *      PERFORM READ-FIELD-ALPHA.
      *      IF F-CBFIRSTLINE = 0
      *          GO TO FILL-068.
      *      ADD 5             TO F-CBFIRSTLINE.
      *      MOVE F-NAMEFIELD  TO ALPHA-RATE.
      *      PERFORM DECIMALISE-RATE.
      *      MOVE NUMERIC-RATE TO B-SURCHPERC (SUB-1).
      *      
            PERFORM DISPLAY-REST-OF-LINE.
       FILL-070.
            MOVE "LDCOST" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            MOVE 11       TO F-CBFIELDLENGTH.
            COMPUTE B-LANDEDCOST (SUB-1) ROUNDED =
                (B-COST (SUB-1) / IMRE-EXCHANGERATE).
            COMPUTE B-LANDEDCOST (SUB-1) ROUNDED =
                (B-LANDEDCOST (SUB-1) - (B-LANDEDCOST (SUB-1) *
                 B-DISCOUNTPERC (SUB-1) / 100)).
            COMPUTE B-LANDEDCOST (SUB-1) ROUNDED = (B-LANDEDCOST (SUB-1)
                 + (((IMRE-ONCOSTPERCENT + B-DUTYPERC (SUB-1) +
                 B-SURCHPERC (SUB-1)) * B-LANDEDCOST (SUB-1)) / 100)).
            MOVE B-LANDEDCOST (SUB-1) TO F-EDNAMEFIELD99Mil.
            PERFORM WRITE-FIELD-99Mil.
       FILL-080.
            MOVE "SPRICE"   TO F-FIELDNAME.
            MOVE 6          TO F-CBFIELDNAME.
            MOVE 11         TO F-CBFIELDLENGTH.
            COMPUTE B-SELLING (SUB-1) ROUNDED = (B-LANDEDCOST (SUB-1)
              + ((B-LANDEDCOST (SUB-1) * IMRE-MARKUPPERCENT) / 100)).
            MOVE B-SELLING (SUB-1) TO F-EDNAMEFIELD99Mil.
            PERFORM WRITE-FIELD-99Mil.
       FILL-090.
            MOVE 2910 TO POS
            DISPLAY "RUNNING TOTAL:              DUTY AMOUNT:"
            AT POS
            MOVE 2925 TO POS
            MOVE WS-FOREIGN-CHECK-VAL TO WS-FOREIGN-MASK
            DISPLAY WS-FOREIGN-MASK AT POS
            MOVE 2950 TO POS
            MOVE WS-DUTY-AMOUNT TO WS-FOREIGN-MASK
            DISPLAY WS-FOREIGN-MASK AT POS.
            
            MOVE "Y" TO B-LINE-COSTED (SUB-1).
            
            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 > SUB-25
                MOVE SUB-1 TO SUB-25.
            IF SUB-1 = 200 OR = 300 OR = 375
                MOVE "NB! YOU MAY ENTER A MAXIMUM OF 400 LINES ONLY."
                TO WS-MESSAGE
                MOVE 3020 TO POS
                DISPLAY WS-MESSAGE AT POS.
            IF SUB-1 = 201 OR = 276
                MOVE " " TO WS-MESSAGE
                MOVE 3020 TO POS
                DISPLAY WS-MESSAGE AT POS.
            IF SUB-1 > 400
                MOVE "400 LINES ARE UP!! , 'ESC' TO TAB."
                 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-999.
           IF F-INDEX < 8
                GO TO FILL-005.
            SUBTRACT 1 FROM SUB-1.
            PERFORM SCROLL-NEXT.
            MOVE 1 TO F-INDEX.
            GO TO FILL-010.
       FILL-999.
             EXIT.
      *
       DISPLAY-REST-OF-LINE SECTION.
       DROL-010.
           MOVE "COSTEACH"     TO F-FIELDNAME
           MOVE 8              TO F-CBFIELDNAME
           MOVE 12             TO F-CBFIELDLENGTH
           MOVE B-COST (SUB-1) TO F-EDNAMEFIELDFOREIGN99
           PERFORM WRITE-FIELD-FOREIGN99.

           MOVE "DISCOUNTPERCENT"      TO F-FIELDNAME
           MOVE 15                     TO F-CBFIELDNAME
           MOVE B-DISCOUNTPERC (SUB-1) TO F-EDNAMEFIELDAMOUNTDIS
           MOVE 6                      TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-AMOUNTDIS.
           
           PERFORM COMPUTE-RUNNING-TOTAL.

           COMPUTE B-DUTYAMOUNT (SUB-1) ROUNDED =
                B-COST (SUB-1) / IMRE-EXCHANGERATE.
           COMPUTE B-DUTYAMOUNT (SUB-1) ROUNDED =
               (B-DUTYAMOUNT (SUB-1) - (B-DUTYAMOUNT (SUB-1) *
                B-DISCOUNTPERC (SUB-1) / 100)).
           COMPUTE B-DUTYAMOUNT (SUB-1) ROUNDED =
               (B-DUTYAMOUNT (SUB-1) * B-DUTYPERC (SUB-1) / 100).

            MOVE "LDCOST" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            MOVE 11       TO F-CBFIELDLENGTH.
            COMPUTE B-LANDEDCOST (SUB-1) ROUNDED =
                (B-COST (SUB-1) / IMRE-EXCHANGERATE).
            COMPUTE B-LANDEDCOST (SUB-1) ROUNDED =
                (B-LANDEDCOST (SUB-1) - (B-LANDEDCOST (SUB-1) *
                 B-DISCOUNTPERC (SUB-1) / 100)).
            COMPUTE B-LANDEDCOST (SUB-1) ROUNDED = (B-LANDEDCOST (SUB-1)
                 + (((IMRE-ONCOSTPERCENT + B-DUTYPERC (SUB-1) +
                 B-SURCHPERC (SUB-1)) * B-LANDEDCOST (SUB-1)) / 100)).
            MOVE B-LANDEDCOST (SUB-1) TO F-EDNAMEFIELD99Mil.
            PERFORM WRITE-FIELD-99Mil.

            MOVE "SPRICE" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE 11       TO F-CBFIELDLENGTH.
            COMPUTE B-SELLING (SUB-1) ROUNDED = (B-LANDEDCOST (SUB-1)
              + ((B-LANDEDCOST (SUB-1) * IMRE-MARKUPPERCENT) / 100)).
            MOVE B-SELLING (SUB-1) TO F-EDNAMEFIELD99Mil.
            PERFORM WRITE-FIELD-99Mil.
       DROL-999.
            EXIT.
      *
       PRINT-LABELS SECTION.
       PR-000.
      *     MOVE "HERE AT LABEL PRINT SECTION" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

           MOVE WS-LABELPRINTER TO WS-PRINTER-SAVE.
           PERFORM GET-USER-PRINT-NAME.
           MOVE WS-PRINTER TO WS-LABELPRINTER.

      *     MOVE "HERE AT LABEL PRINT OPEN" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           OPEN OUTPUT LABEL-PRINT.
           IF WS-SPL-ST1 NOT = 0
               MOVE "LABEL Print File Open error, 'ESC' To RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               Move Ws-LabelPrinter to ws-MESSAGE
               PERFORM ERROR-MESSAGE
               Move Ws-PRINTER-SAVE to ws-MESSAGE
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
           MOVE 2910 TO POS
           DISPLAY "                                         " AT POS.
           CLOSE LABEL-PRINT.
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
           MOVE IMRE-INVOICENUM   TO P-INVNO (SUB-1).
           
           IF SUB-1 < 4
              ADD 1 TO SUB-1
           ELSE
              PERFORM PR-020
              MOVE 1 TO SUB-1.
           ADD 1 TO SUB-2.
           IF SUB-2 = WS-QTY-EACH-ITEM
              MOVE 0 TO SUB-2
              ADD 1 TO SUB-4
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
       CLEAR-ORDER-TOTALS SECTION.
       COT-005.
           MOVE 1 TO SUB-1.
       COT-010.
           MOVE " " TO WS-ORDER (SUB-1)
           MOVE 0   TO WS-FOREIGN-VALUE (SUB-1).
           IF SUB-1 < 20
             ADD 1 TO SUB-1
             GO TO COT-010.
           MOVE 0 TO SUB-1.
       COT-999.
           EXIT.
      *
       COMPUTE-FOREIGN-ORDER-VALUE SECTION.
       CFOV-000.
            MOVE 1 TO SUB-1 SUB-2.
            MOVE 2910 TO POS
            DISPLAY "COMPUTING FOREIGN VALUE OF ORDERS....    " AT POS.
       CFOV-001.
            MOVE 0 TO WS-FOREIGN-VALUE (SUB-2).
            ADD 1 TO SUB-2.
            IF SUB-2 < 21
               GO TO CFOV-001.
            MOVE 1 TO SUB-2.
       CFOV-010.
            IF B-ORDERIND (SUB-1) = 0
               GO TO CFOV-900.
            MOVE B-ORDERIND (SUB-1) TO SUB-2.
            COMPUTE WS-FOREIGN-VAL =
              (B-COST (SUB-1) * B-QUANTITY (SUB-1)) *
                 ((100 - B-DISCOUNTPERC (SUB-1)) / 100).
            ADD WS-FOREIGN-VAL TO WS-FOREIGN-VALUE (SUB-2).
            IF SUB-1 < 399
               ADD 1 TO SUB-1
               GO TO CFOV-010.
       CFOV-900.
            PERFORM ERROR1-020.
       CFOV-999.
            EXIT.
      *     
       GET-NEW-ORDER-ENTERED SECTION.
       GNOE-001.
            MOVE 0 TO SUB-10.
       GNOE-002.
            IF SUB-10 < 20
               ADD 1 TO SUB-10.
            IF WS-ORDER (SUB-10) NOT = " "
               GO TO GNOE-002.
       GNOE-005.
            PERFORM CLEAR-010.
            PERFORM ERROR1-020
            PERFORM ERROR-020
            MOVE 3010 TO POS
            DISPLAY "                3=STOCKNUMBER & ORDERNUMBER"
            AT POS
            MOVE 2910 TO POS
            DISPLAY "SEARCH ORDER BY 1=STOCKNUMBER, 2=ORDERNUMBER: [ ]"
            AT POS
            ADD 47 TO POS

            MOVE ' '       TO CDA-DATA.
            MOVE 1         TO CDA-DATALEN.
            MOVE 26        TO CDA-ROW.
            MOVE 56        TO CDA-COL.
            MOVE CDA-GREEN TO CDA-COLOR.
            MOVE 'F'       TO CDA-ATTR.
            PERFORM CTOS-ACCEPT.
            MOVE CDA-DATA TO WS-BYORD-STOCK.
            
            IF WS-BYORD-STOCK NOT = "1" AND NOT = "2" AND NOT = "3"
               MOVE "PLEASE RE-ENTER OPTION, MUST BE 1,2 OR 3"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GNOE-005.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            IF WS-BYORD-STOCK = "1" OR = "3"
               MOVE 2910 TO POS
               DISPLAY "PLEASE ENTER BEGINNING STOCK NUMBER." AT POS.
            IF WS-BYORD-STOCK = "2"
               MOVE 2910 TO POS
               DISPLAY "PLEASE ENTER THE FULL P/O NUMBER.   " AT POS.
       GNOE-006.
            MOVE "ORDERNUM" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 20         TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               GO TO GNOE-999.
           IF WS-BYORD-STOCK = "1" OR = "3"
            IF WS-BEG-STOCK = " "
               MOVE F-NAMEFIELD TO WS-BEG-STOCK
               MOVE 2910 TO POS
               DISPLAY "PLEASE ENTER ENDING STOCK NUMBER.   " AT POS
               MOVE "ORDERNUM" TO F-FIELDNAME
               MOVE 8          TO F-CBFIELDNAME
               MOVE " "        TO F-NAMEFIELD
               MOVE 20         TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO GNOE-006
            ELSE
               MOVE F-NAMEFIELD TO WS-END-STOCK.
           IF WS-BYORD-STOCK = "2"
               MOVE F-NAMEFIELD TO WS-ORDER (SUB-10)
               PERFORM ONE-900.
       GNOE-010.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           IF WS-BYORD-STOCK = "1"
               PERFORM ONE-900
               GO TO GNOE-500.
           IF WS-BYORD-STOCK = "3"
               MOVE 2810 TO POS
               DISPLAY "PLEASE ENTER THE FULL P/O NUMBERS YOU REQUIRE.."
                AT POS
              PERFORM ORDER-NUMBER-ENTRY
              GO TO GNOE-800.
      *
      * DONE ONLY IF WS-BYORD-STOCK = "2"
      *
           MOVE WS-ORDER (SUB-10)  TO OO-ORDER-NUMBER
           MOVE " "                TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO OO-QUANTITY
               GO TO GNOE-900.
       GNOE-020.
           READ OUTSTANDING-ORDERS NEXT
               AT END NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 10
               MOVE 0 TO OO-QUANTITY
               GO TO GNOE-900.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "ORDERS BUSY GNOE-020, PRESS 'ESC' TO RE-TRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO GNOE-020.
           IF OO-ORDER-NUMBER NOT = WS-ORDER (SUB-10)
               GO TO GNOE-900.
           IF OO-QUANTITY = 0
               GO TO GNOE-020.
               
           MOVE 2810 TO POS
           DISPLAY "READING STOCK-NUMBER:" AT POS
           ADD 22 TO POS
           DISPLAY OO-STOCK-NUMBER AT POS.
           
           MOVE OO-STOCK-NUMBER TO B-STOCKNUMBER (SUB-1) WS-STOCKNUMBER
           MOVE OO-QUANTITY     TO B-QUANTITY (SUB-1).
           IF WS-ORDER-DISC > 0
             MOVE WS-ORDER-DISC TO B-DISCOUNTPERC (SUB-1).
           MOVE SUB-10          TO B-ORDERIND (SUB-1).
           PERFORM READ-STOCK.
           IF OO-COST = 0
              MOVE ST-FOREIGNCOST  TO B-COST (SUB-1)
           ELSE
              MOVE OO-COST         TO B-COST (SUB-1).
           MOVE ST-DUTYPERCENT  TO B-DUTYPERC (SUB-1)
           MOVE ST-SURCHARGE    TO B-SURCHPERC (SUB-1).
           
           IF SUB-1 < 400
              ADD 1 TO SUB-1
              GO TO GNOE-020
           ELSE
              MOVE "YOU HAVE REACHED 400 LIMIT OF ITEMS" TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              MOVE "YOU WILL HAVE TO <CANCEL> TO ABORT." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
           GO TO GNOE-900.
       GNOE-500.
      *
      * DONE ONLY IF WS-BYORD-STOCK = "1"
      *
           MOVE WS-BEG-STOCK TO OO-STOCK-NUMBER
           START OUTSTANDING-ORDERS KEY NOT < OO-STOCK-NUMBER
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO OO-QUANTITY
               GO TO GNOE-900.
       GNOE-520.
           READ OUTSTANDING-ORDERS NEXT
               AT END NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 10
               MOVE 0 TO OO-QUANTITY
               GO TO GNOE-900.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "ORDERS BUSY GNOE-520, PRESS 'ESC' TO RE-TRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO GNOE-520.
           IF OO-STOCK-NUMBER < WS-BEG-STOCK
               GO TO GNOE-520.
           IF OO-STOCK-NUMBER > WS-END-STOCK
               GO TO GNOE-900.
               
           MOVE 2810 TO POS
           DISPLAY "READING STOCK-NUMBER:" AT POS
           ADD 22 TO POS
           DISPLAY OO-STOCK-NUMBER AT POS.
           
           IF OO-QUANTITY = 0
               GO TO GNOE-520.
           MOVE OO-STOCK-NUMBER TO B-STOCKNUMBER (SUB-1) WS-STOCKNUMBER
           MOVE OO-QUANTITY     TO B-QUANTITY (SUB-1)
           IF WS-ORDER-DISC > 0
             MOVE WS-ORDER-DISC TO B-DISCOUNTPERC (SUB-1).
           PERFORM FIND-WHAT-INDEX.
           PERFORM READ-STOCK.
           IF OO-COST = 0
              MOVE ST-FOREIGNCOST  TO B-COST (SUB-1)
           ELSE
              MOVE OO-COST         TO B-COST (SUB-1).
      *     MOVE ST-FOREIGNCOST  TO B-COST (SUB-1)
           MOVE ST-DUTYPERCENT  TO B-DUTYPERC (SUB-1)
           MOVE ST-SURCHARGE    TO B-SURCHPERC (SUB-1).

           IF SUB-1 < 400
              ADD 1 TO SUB-1
              GO TO GNOE-520
           ELSE
              MOVE "YOU HAVE REACHED 400 LIMIT OF ITEMS" TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              MOVE "YOU WILL HAVE TO <CANCEL> TO ABORT." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       GNOE-800.
      *
      * DONE ONLY IF WS-BYORD-STOCK = "3"
      *
           PERFORM ERROR1-020
           PERFORM ERROR-020
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
           
           MOVE WS-BEG-STOCK TO OO-STOCK-NUMBER
           START OUTSTANDING-ORDERS KEY NOT < OO-STOCK-NUMBER
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO OO-QUANTITY
               GO TO GNOE-900.
       GNOE-820.
           READ OUTSTANDING-ORDERS NEXT
               AT END NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 10
               MOVE 0 TO OO-QUANTITY
               GO TO GNOE-900.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "ORDERS BUSY GNOE-820, PRESS 'ESC' TO RE-TRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO GNOE-820.
           IF OO-STOCK-NUMBER < WS-BEG-STOCK
               GO TO GNOE-820.
           IF OO-STOCK-NUMBER > WS-END-STOCK
               GO TO GNOE-900.
               
           MOVE 2810 TO POS
           DISPLAY "READING STOCK-NUMBER:" AT POS
           ADD 22 TO POS
           DISPLAY OO-STOCK-NUMBER AT POS.
           
           IF OO-QUANTITY = 0
               GO TO GNOE-820.
           
           PERFORM CHECK-IF-ORDER-VALID
           IF WS-VALID = "N"
              GO TO GNOE-820.

           MOVE OO-STOCK-NUMBER TO B-STOCKNUMBER (SUB-1) WS-STOCKNUMBER
           MOVE OO-QUANTITY     TO B-QUANTITY (SUB-1).
           IF WS-ORDER-DISC > 0
             MOVE WS-ORDER-DISC TO B-DISCOUNTPERC (SUB-1).
            
           PERFORM FIND-WHAT-INDEX.
           PERFORM READ-STOCK.
           IF OO-COST = 0
              MOVE ST-FOREIGNCOST  TO B-COST (SUB-1)
           ELSE
              MOVE OO-COST         TO B-COST (SUB-1)
      *     MOVE ST-FOREIGNCOST  TO B-COST (SUB-1)
           MOVE ST-DUTYPERCENT  TO B-DUTYPERC (SUB-1)
           MOVE ST-SURCHARGE    TO B-SURCHPERC (SUB-1).

           IF SUB-1 < 400
              ADD 1 TO SUB-1
              GO TO GNOE-820
           ELSE
              MOVE "YOU HAVE REACHED 400 LIMIT OF ITEMS" TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              MOVE "YOU WILL HAVE TO <CANCEL> TO ABORT." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       GNOE-900.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS.
       GNOE-999.
            EXIT.
      *
       CHECK-IF-ORDER-VALID SECTION.
       CIOV-005.
            MOVE 0 TO SUB-10.
       CIOV-010.
            IF SUB-10 < 20
               ADD 1 TO SUB-10.
            IF WS-ORDER (SUB-10) = " "
                MOVE "N" TO WS-VALID
                GO TO CIOV-999.
            IF WS-ORDER (SUB-10) NOT = " "
             IF WS-ORDER (SUB-10) NOT = OO-ORDER-NUMBER
               GO TO CIOV-010.
             IF WS-ORDER (SUB-10) = OO-ORDER-NUMBER
                MOVE "Y" TO WS-VALID.
       CIOV-999.
            EXIT.
      *
       FIND-WHAT-INDEX SECTION.
       FWI-005.
            MOVE 0 TO SUB-10.
       FWI-010.
            IF SUB-10 < 20
               ADD 1 TO SUB-10.
            IF WS-ORDER (SUB-10) = " "
                MOVE OO-ORDER-NUMBER TO WS-ORDER (SUB-10)
                MOVE SUB-10 TO B-ORDERIND (SUB-1)
                GO TO FWI-999.
            IF WS-ORDER (SUB-10) NOT = " "
             IF WS-ORDER (SUB-10) NOT = OO-ORDER-NUMBER
               GO TO FWI-010.
             IF WS-ORDER (SUB-10) = OO-ORDER-NUMBER
                MOVE SUB-10 TO B-ORDERIND (SUB-1)
                GO TO FWI-999.
       FWI-999.
            EXIT.
      *
       ORDER-NUMBER-ENTRY SECTION.
       ONE-000.
            MOVE 1    TO SUB-9.
            PERFORM ERROR1-020.
       ONE-010.
            MOVE 2710 TO POS.
            DISPLAY "MAXIMUM ORDER NO'S = 20. ORDER NO.:" AT POS.
            MOVE 2746 TO POS.
            DISPLAY SUB-9 AT POS.
            
            IF WS-BYORD-STOCK NOT = " "
                MOVE "ORDERNUM"       TO F-FIELDNAME
                MOVE 8                TO F-CBFIELDNAME
                MOVE WS-ORDER (SUB-9) TO F-NAMEFIELD
                MOVE 20               TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.
            
            MOVE "ORDERNUM" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"01"
             IF SUB-9 > 1
                SUBTRACT 1 FROM SUB-9
                MOVE WS-ORDER (SUB-9) TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO ONE-010
             ELSE
                GO TO ONE-010.
            IF F-NAMEFIELD = " "
                MOVE 2710 TO POS
                DISPLAY "                                              "
                  AT POS
                GO TO ONE-900.
            MOVE F-NAMEFIELD TO WS-ORDER (SUB-9).
            MOVE " " TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.
            ADD 1 TO SUB-9.
            IF SUB-9 = 0
               MOVE 1 TO SUB-9.
            GO TO ONE-010.
       ONE-900.
            PERFORM CLEAR-010.
            PERFORM ERROR1-020.
            MOVE 2910 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 2910 TO POS.
            DISPLAY 
            "ENTER A DISCOUNT IF APPLICABLE TO THESE ITEMS: [     ]"
             AT POS.
            MOVE 2958 TO POS.

            MOVE ' '       TO CDA-DATA.
            MOVE 5         TO CDA-DATALEN.
            MOVE 26        TO CDA-ROW.
            MOVE 57        TO CDA-COL.
            MOVE CDA-GREEN TO CDA-COLOR.
            MOVE 'F'       TO CDA-ATTR.
            PERFORM CTOS-ACCEPT.
            MOVE CDA-DATA TO WS-DISCOUNT-ENTRY.
            
            IF WS-DISCOUNT-ENTRY > 0
                MOVE WS-DISCOUNT-ENTRY TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-ORDER-DISC
                DISPLAY WS-ORDER-DISC AT POS.
            PERFORM ERROR1-020.
       ONE-999.
            EXIT.
      *
       FIND-IMPORT SECTION.
       FI-000.
            START IMPRECEIPTS-FILE KEY NOT < IMRE-ALT-KEY
                INVALID KEY NEXT SENTENCE.
            IF WS-IMPRECEIPT-ST1 = 23 OR 35 OR 49
                MOVE "1" TO WS-NOT-THERE
                GO TO FI-999.
            IF WS-IMPRECEIPT-ST1 NOT = 0
                MOVE "IMPORTS FILE BUSY ON START, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-IMPRECEIPT-ST1
                GO TO FI-000.
       FI-002.
            READ IMPRECEIPTS-FILE NEXT WITH LOCK
                AT END NEXT SENTENCE.
            IF WS-IMPRECEIPT-ST1 = 10
                MOVE "1" TO WS-NOT-THERE
                GO TO FI-999.
            IF WS-IMPRECEIPT-ST1 NOT = 0
                MOVE "IMPORTS-FILE BUSY ON READ NEXT, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-IMPRECEIPT-ST1
                GO TO FI-002.
            IF IMRE-SUPPLIER NOT = WS-SUPPLIER
                MOVE "1"           TO WS-NOT-THERE
                MOVE WS-SUPPLIER   TO IMRE-SUPPLIER
                MOVE WS-INVOICENUM TO IMRE-INVOICENUM
                GO TO FI-999.
            IF IMRE-INVOICENUM NOT = WS-INVOICENUM
                MOVE "1"           TO WS-NOT-THERE
                MOVE WS-SUPPLIER   TO IMRE-SUPPLIER
                MOVE WS-INVOICENUM TO IMRE-INVOICENUM
                GO TO FI-999.
            IF IMRE-UPDATED-YN  = "Y"
                MOVE "THIS INVOICE HAS ALREADY BEEN RECOSTED !!"
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE "X" TO WS-NOT-THERE
                GO TO FI-999.
            IF IMRE-RAND-AMT-PAID NOT = 0
                MOVE "THIS INVOICE HAS ALREADY BEEN RECOSTED !!"
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE "X" TO WS-NOT-THERE
                GO TO FI-999.
            MOVE IMRE-SUPPLIER   TO WS-SUPPLIER.
            MOVE IMRE-INVOICENUM TO WS-INVOICENUM.
       FI-010.
            MOVE "SUPPLIERNUM" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            MOVE 10            TO F-CBFIELDLENGTH.
            MOVE IMRE-SUPPLIER TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICENUM"    TO F-FIELDNAME.
            MOVE 10              TO F-CBFIELDNAME.
            MOVE 10              TO F-CBFIELDLENGTH.
            MOVE IMRE-INVOICENUM TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CURRENCY"    TO F-FIELDNAME.
            MOVE 8             TO F-CBFIELDNAME.
            MOVE 4             TO F-CBFIELDLENGTH.
            MOVE IMRE-CURRENCY TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "EXCHANGERATE"    TO F-FIELDNAME.
            MOVE 12                TO F-CBFIELDNAME.
            MOVE 7                 TO F-CBFIELDLENGTH.
            MOVE IMRE-EXCHANGERATE TO F-EDNAMEFIELDNUMDEC.
            PERFORM WRITE-FIELD-NUM-DEC.

            MOVE "INVAMTFOREIGN"    TO F-FIELDNAME.
            MOVE 13                 TO F-CBFIELDNAME.
            MOVE 11                 TO F-CBFIELDLENGTH.
            MOVE IMRE-INVAMTFOREIGN TO F-EDNAMEFIELDFORTOTAL.
            PERFORM WRITE-FIELD-FORTOTAL.

            MOVE "FREIGHTAMTFOREIGN"    TO F-FIELDNAME
            MOVE 17                     TO F-CBFIELDNAME
            MOVE 11                     TO F-CBFIELDLENGTH
            MOVE IMRE-FREIGHTAMTFOREIGN TO F-EDNAMEFIELD99Mil
            PERFORM WRITE-FIELD-99Mil.

            MOVE "RANDINVAMT"         TO F-FIELDNAME
            MOVE 10                   TO F-CBFIELDNAME
            MOVE 11                   TO F-CBFIELDLENGTH
            MOVE IMRE-RANDINVAMTGROSS TO F-EDNAMEFIELD99Mil
            PERFORM WRITE-FIELD-99Mil.

            MOVE "TOTALRANDDUTY"    TO F-FIELDNAME
            MOVE 13                 TO F-CBFIELDNAME
            MOVE 11                 TO F-CBFIELDLENGTH
            MOVE IMRE-TOTALDUTYRAND TO F-EDNAMEFIELD99Mil
            PERFORM WRITE-FIELD-99Mil.

            MOVE "RANDFREIGHTAMT" TO F-FIELDNAME
            MOVE 14               TO F-CBFIELDNAME
            MOVE 11               TO F-CBFIELDLENGTH
            MOVE IMRE-FREIGHTRAND TO F-EDNAMEFIELD99Mil
            PERFORM WRITE-FIELD-99Mil.

            MOVE "WARFAGE"    TO F-FIELDNAME
            MOVE 7            TO F-CBFIELDNAME
            MOVE 11           TO F-CBFIELDLENGTH
            MOVE IMRE-WARFAGE TO F-EDNAMEFIELD99Mil
            PERFORM WRITE-FIELD-99Mil.

            MOVE "CLEARINGCHGS" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME
            MOVE 11             TO F-CBFIELDLENGTH
            MOVE IMRE-CLEARING  TO F-EDNAMEFIELD99Mil
            PERFORM WRITE-FIELD-99Mil.

            MOVE "SURCHARGE"         TO F-FIELDNAME
            MOVE 9                   TO F-CBFIELDNAME
            MOVE 11                  TO F-CBFIELDLENGTH
            MOVE IMRE-SURCHARGE-RAND TO F-EDNAMEFIELD99mil
            PERFORM WRITE-FIELD-99mil.

            MOVE "TOTALCHARGES"    TO F-FIELDNAME
            MOVE 12                TO F-CBFIELDNAME
            MOVE 11                TO F-CBFIELDLENGTH
            MOVE IMRE-TOTALCHARGES TO F-EDNAMEFIELD99mil
            PERFORM WRITE-FIELD-99mil.

            MOVE "ORDERNUM"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE 10            TO F-CBFIELDLENGTH
            MOVE IMRE-ORDERNUM TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATERECEIVED"    TO F-FIELDNAME
            MOVE 12                TO F-CBFIELDNAME
            MOVE IMRE-DATERECEIVED TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE      TO F-NAMEFIELD
            MOVE 10                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "RATEPAID"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE 10            TO F-CBFIELDLENGTH
            MOVE IMRE-RATEPAID TO F-EDNAMEFIELDNUMDEC
            PERFORM WRITE-FIELD-NUM-DEC.

            MOVE "RANDAMTPAID"      TO F-FIELDNAME
            MOVE 11                 TO F-CBFIELDNAME
            MOVE 9                  TO F-CBFIELDLENGTH
            MOVE IMRE-RAND-AMT-PAID TO F-EDNAMEFIELDAMOUNT
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "MARKUP"           TO F-FIELDNAME
            MOVE 6                  TO F-CBFIELDNAME
            MOVE 6                  TO F-CBFIELDLENGTH
            MOVE IMRE-MARKUPPERCENT TO F-EDNAMEFIELDPERC
            PERFORM WRITE-FIELD-PERC.

            MOVE "ONCOSTPERCENT"    TO F-FIELDNAME
            MOVE 13                 TO F-CBFIELDNAME
            MOVE 6                  TO F-CBFIELDLENGTH
            MOVE IMRE-ONCOSTPERCENT TO F-EDNAMEFIELDPERC
            PERFORM WRITE-FIELD-PERC.
       FI-020.
            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM SCROLL-NEXT
            PERFORM SCROLL-PREVIOUS.
       FI-999.
            EXIT.
      *
       START-FOR-READ-NEXT SECTION.
       SFRN-001.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
       SFRN-999.
             EXIT.
      *
       READ-NEXT-STOCK-ITEM SECTION.
       RNSI-005.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               GO TO RNSI-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
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
       COMPUTE-RUNNING-TOTAL SECTION.
       CRT-005.
           MOVE SUB-1 TO SUB-10.
           MOVE 1 TO SUB-1.
           MOVE 0 TO WS-FOREIGN-CHECK-VAL
                     WS-DUTY-AMOUNT.
       CRT-010.
           IF B-STOCKNUMBER (SUB-1) = " "
                 GO TO CRT-900.
           COMPUTE WS-WORKTOTAL ROUNDED =
                   B-QUANTITY (SUB-1) * B-COST (SUB-1).
           COMPUTE WS-WORKTOTAL ROUNDED = WS-WORKTOTAL - (WS-WORKTOTAL *
                   B-DISCOUNTPERC (SUB-1)) / 100.
           COMPUTE WS-FOREIGN-CHECK-VAL ROUNDED =
                 WS-FOREIGN-CHECK-VAL + WS-WORKTOTAL.

           COMPUTE WS-WORKTOTAL ROUNDED =
                   B-QUANTITY (SUB-1) * B-DUTYAMOUNT (SUB-1).
           ADD WS-WORKTOTAL TO WS-DUTY-AMOUNT.
           
           IF SUB-1 < 400
              ADD 1 TO SUB-1
              GO TO CRT-010.
       CRT-900.
           MOVE SUB-10 TO SUB-1.
           IF WS-FOREIGN-CHECK-VAL =
              (IMRE-INVAMTFOREIGN - IMRE-FREIGHTAMTFOREIGN)
                MOVE "YOU HAVE REACHED THE FOREIGN INVOICE AMOUNT!!" 
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO CRT-999.
           IF WS-FOREIGN-CHECK-VAL >
              (IMRE-INVAMTFOREIGN - IMRE-FREIGHTAMTFOREIGN)
                MOVE
                "YOU HAVE ENTERED ITEMS WITH VALUE > THAN THE INVOICE!" 
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO CRT-999.
           PERFORM ERROR-020.
       CRT-999.
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
            IF SUB-1 > 394
                 MOVE 394 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 400
               GO TO NEXT-030.
            IF F-INDEX < 8
                GO TO NEXT-010.
       NEXT-030.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 > 393
             IF SUB-25 > 393
               COMPUTE F-INDEX = 7 - (400 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX.
            IF F-INDEX > 7
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 2810 TO POS.
            DISPLAY "    BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-999.
             EXIT.
     *
       SCROLL-NEXT-PAGE SECTION.
       NEXT-PAGE-000.
            ADD 7 TO SUB-1.
            IF SUB-1 > SUB-25
                 MOVE SUB-25 TO SUB-1.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1 SUB-25.
            MOVE 1 TO F-INDEX.
            IF SUB-1 > 394
                 MOVE 394 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 400
               GO TO NEXT-PAGE-030.
            IF F-INDEX < 8
                GO TO NEXT-PAGE-010.
       NEXT-PAGE-030.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 > 393
             IF SUB-25 > 393
               COMPUTE F-INDEX = 7 - (400 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX.
            IF SUB-1 > 400
                MOVE 394 TO SUB-1.
            IF F-INDEX > 7
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 2810 TO POS.
            DISPLAY "    BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-PAGE-999.
             EXIT.
      *
       SCROLL-PREVIOUS SECTION.
       PREV-000.
            SUBTRACT 7 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-010.
            PERFORM SCROLLING.
       PREV-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 400
                GO TO PREV-030.
            IF F-INDEX < 8
                GO TO PREV-010.
       PREV-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

             MOVE 2810 TO POS.
             DISPLAY "    BODY LINE: " AT POS.
             ADD 16 TO POS.
             MOVE SUB-1 TO WS-BODY-LINE.
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
            IF SUB-1 > 400
                GO TO SCROLL-DOWN-030.
            IF F-INDEX < 8
                GO TO SCROLL-DOWN-010.
       SCROLL-DOWN-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

             MOVE 2810 TO POS
             DISPLAY "    BODY LINE: " AT POS
             ADD 16 TO POS
             MOVE SUB-1 TO WS-BODY-LINE
             DISPLAY WS-BODY-LINE AT POS.
       SCROLL-DOWN-999.
             EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE "ORDERIND"         TO F-FIELDNAME
            MOVE 8                  TO F-CBFIELDNAME
            MOVE B-ORDERIND (SUB-1) TO F-EDNAMEFIELDIND
            MOVE 2                  TO F-CBFIELDLENGTH.
            IF B-ORDERIND (SUB-1) = 0 OR 
               B-STOCKNUMBER (SUB-1) = " "
                   MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-INDEX.
       SCROLL-010.
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QUANTITY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = " "
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE B-QUANTITY (SUB-1) TO F-EDNAMEFIELDQTY
               PERFORM WRITE-FIELD-QTY.

            MOVE "COSTEACH" TO F-FIELDNAME.  
            MOVE 8          TO F-CBFIELDNAME.
            MOVE 12         TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = " "
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE B-COST (SUB-1) TO F-EDNAMEFIELDFOREIGN99
               PERFORM WRITE-FIELD-FOREIGN99.

            MOVE "DISCOUNTPERCENT" TO F-FIELDNAME.
            MOVE 15                TO F-CBFIELDNAME.
            MOVE 6                 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = " "
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE B-DISCOUNTPERC (SUB-1) TO F-EDNAMEFIELDAMOUNTDIS
               PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "DUTYPERCENT" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = " "
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE B-DUTYPERC (SUB-1) TO F-EDNAMEFIELDAMOUNTDIS
               PERFORM WRITE-FIELD-AMOUNTDIS.

      *      MOVE "SURCHPERCENT" TO F-FIELDNAME.
      *      MOVE 12 TO F-CBFIELDNAME.
      *      MOVE 5 TO F-CBFIELDLENGTH.
      *      IF B-STOCKNUMBER (SUB-1) = " "
      *         MOVE " " TO F-NAMEFIELD
      *         PERFORM WRITE-FIELD-ALPHA
      *      ELSE
      *         MOVE B-SURCHPERC (SUB-1) TO F-EDNAMEFIELDAMOUNTDIS
      *         PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "LDCOST"   TO F-FIELDNAME.
            MOVE 6          TO F-CBFIELDNAME.
            MOVE 11         TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = " "
               MOVE " "     TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE B-LANDEDCOST (SUB-1) TO F-EDNAMEFIELD99Mil
               PERFORM WRITE-FIELD-99Mil.

            MOVE "SPRICE" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE 11       TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = " "
               MOVE " "   TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE B-SELLING (SUB-1) TO F-EDNAMEFIELD99Mil
               PERFORM WRITE-FIELD-99Mil.
       SCROLL-999.
             EXIT.
      *
       DISPLAY-LINE-ITEMS SECTION.
       DIS-L-I-010.
            MOVE "STOCKNUMBER"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE ST-STOCKNUMBER TO F-NAMEFIELD
            MOVE 15             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COSTEACH"     TO F-FIELDNAME
            MOVE 8              TO F-CBFIELDNAME
            MOVE ST-FOREIGNCOST TO F-EDNAMEFIELDFOREIGN99
            MOVE 12             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FOREIGN99.

            MOVE "STOCKDESCRIPTION1" TO F-FIELDNAME
            MOVE 17                  TO F-CBFIELDNAME
            MOVE ST-DESCRIPTION1     TO F-NAMEFIELD
            MOVE 20                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKDESCRIPTION2" TO F-FIELDNAME
            MOVE 17                  TO F-CBFIELDNAME
            MOVE ST-DESCRIPTION2     TO F-NAMEFIELD
            MOVE 20                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DUTYPERCENT"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE ST-DUTYPERCENT TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.

      *      MOVE "SURCHPERCENT" TO F-FIELDNAME
      *      MOVE 12             TO F-CBFIELDNAME
      *      MOVE ST-SURCHARGE   TO F-EDNAMEFIELDAMOUNTDIS
      *      MOVE 5              TO F-CBFIELDLENGTH
      *      PERFORM WRITE-FIELD-AMOUNTDIS.
       DIS-L-I-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 1 TO SUB-1.
       CF-010.
             MOVE " " TO B-STOCKNUMBER (SUB-1)
                         B-LINE-COSTED (SUB-1).
             MOVE 0   TO B-QUANTITY (SUB-1)
                         B-ORDERIND (SUB-1)
                         B-COST (SUB-1)
                         B-DISCOUNTPERC (SUB-1)
                         B-DUTYPERC (SUB-1)
                         B-SURCHPERC (SUB-1)
                         B-DUTYAMOUNT (SUB-1)
                         B-LANDEDCOST (SUB-1)
                         B-SELLING (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 401
                 GO TO CF-010.
             MOVE 1 TO SUB-1.
             UNLOCK STKRECEIPTS-FILE.
       CF-999.
             EXIT.
      *
       CANCEL-TRANSACTION SECTION.
       CAN-000.
             COMPUTE SUB-2 = SUB-1 + 1.
       CAN-010.
             IF SUB-2 > 400
                 GO TO CAN-090.
             IF B-STOCKNUMBER (SUB-2) = " "
                 MOVE " " TO B-STOCKNUMBER (SUB-1)
                 MOVE 0   TO B-QUANTITY (SUB-1)
                             B-ORDERIND (SUB-1)
                             B-COST (SUB-1)
                             B-DISCOUNTPERC (SUB-1)
                             B-DUTYPERC (SUB-1)
                             B-SURCHPERC (SUB-1)
                             B-DUTYAMOUNT (SUB-1)
                             B-LANDEDCOST (SUB-1)
                             B-SELLING (SUB-1)
                 GO TO CAN-090.
             MOVE BODY-LINE (SUB-2) TO BODY-LINE (SUB-1).
             ADD 1 TO SUB-1 SUB-2.
             GO TO CAN-010.
       CAN-090.
             MOVE " " TO B-STOCKNUMBER (SUB-1).
             MOVE 0   TO B-QUANTITY (SUB-1)
                         B-ORDERIND (SUB-1)
                         B-COST (SUB-1)
                         B-DISCOUNTPERC (SUB-1)
                         B-DUTYPERC (SUB-1)
                         B-SURCHPERC (SUB-1)
                         B-DUTYAMOUNT (SUB-1)
                         B-LANDEDCOST (SUB-1)
                         B-SELLING (SUB-1).
             IF B-STOCKNUMBER (1) = " "
                 MOVE 0 TO WS-FOREIGN-CHECK-VAL.
       CAN-999.
             EXIT.
      *
       REWRITE-STOCK SECTION.
       REW-ST-000.
           IF WS-RATE-DIFF = ZERO
                GO TO REW-ST-010.
           IF ST-STOCKNUMBER = " "
                GO TO REW-ST-010.
           REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE "STOCK RECORD:" TO WS-DAILY-1ST
                MOVE ST-STOCKNUMBER  TO WS-DAILY-2ND
                MOVE "NOT UPDATED"   TO WS-DAILY-3RD
                MOVE "ON RE-COST"    TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                GO TO REW-ST-010.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO REW-ST-000.
           IF WS-STOCK-CHANGE = "Y"
            IF WS-UP-SELL = "Y"
               PERFORM WRITE-STOCK-CHANGES
               GO TO REW-ST-010.
           IF WS-STOCK-CHANGE = "Y"
             IF WS-UP-FORGN = "Y"
               PERFORM WRITE-STOCK-CHANGES.
       REW-ST-010.
             MOVE WS-RATEPAID      TO IMRE-RATEPAID
             MOVE WS-RAND-AMT-PAID TO IMRE-RAND-AMT-PAID
             MOVE "Y"              TO IMRE-UPDATED-YN.
             REWRITE IMPORT-RECEIPTS-REC
                INVALID KEY NEXT SENTENCE.
             IF WS-IMPRECEIPT-ST1 = 23 OR 35 OR 49
                MOVE "IMP.RECPT RECORD:" TO WS-DAILY-1ST
                MOVE IMRE-SUPPLIER       TO WS-DAILY-2ND
                MOVE IMRE-INVOICENUM     TO WS-DAILY-3RD
                MOVE "NOT UPDATED"       TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                GO TO REW-ST-999.
             IF WS-IMPRECEIPT-ST1 NOT = 0
               MOVE "IMPRECEIPT RECORD BUSY, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-IMPRECEIPT-ST1
               GO TO REW-ST-010.
       REW-ST-999.
             EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY NEXT SENTENCE.
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
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
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
       R-STL-000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
                MOVE " "            TO ST-STOCKNUMBER
                                       ST-DESCRIPTION2
                MOVE "NOT THERE!!!" TO ST-DESCRIPTION1
                MOVE 0              TO ST-PRICE
                                       ST-AVERAGECOST
                                       ST-DISCOUNT1
               GO TO R-STL-999.
       R-STL-010.
           READ STOCK-MASTER WITH LOCK
                INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE " "            TO ST-STOCKNUMBER
                                       ST-DESCRIPTION2
                MOVE "NOT THERE!!!" TO ST-DESCRIPTION1
                MOVE 0              TO ST-PRICE
                                       ST-AVERAGECOST
                                       ST-DISCOUNT1
                GO TO R-STL-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-STL-010.
       R-STL-999.
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
               MOVE "NO PARAMETER RECORD ON READ, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               CLOSE PARAMETER-FILE
               PERFORM OPEN-001
               GO TO RP-010.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER RECORD BUSY ON READ, 'ESC' TO RETRY." 
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
           READ PARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO PARAMETER RECORD ON READLOCK, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY ON READLOCK, 'ESC' TO RETRY." 
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
               DISPLAY "PARAMETER RECORD NOT UPDATED!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER RECORD BUSY ON REWRITE, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO REWP-000.
       REWP-999.
           EXIT.
      *
       UPDATE-STOCK SECTION.
       UPST-000.
            MOVE 1 TO SUB-1.
       UPST-010.
           MOVE 2910 TO POS
           DISPLAY "STOCK BEING UPDATED FOR LINE     OF     LINES."
                AT POS
           MOVE SUB-25 TO WS-BODY-LINE
           MOVE 2946 TO POS
           DISPLAY WS-BODY-LINE AT POS
           MOVE 2939 TO POS
           MOVE SUB-1 TO WS-BODY-LINE
           DISPLAY WS-BODY-LINE AT POS.

           MOVE 0 TO WS-QTY.
           IF B-STOCKNUMBER (SUB-1) = " "
                GO TO UPST-999.
           MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           PERFORM REAVERAGE-COST.
           IF WS-UP-SELL = "Y"
                MOVE ST-PRICE          TO ST-OLDPRICE
                MOVE B-SELLING (SUB-1) TO ST-PRICE
                MOVE WS-DATE           TO ST-LASTPRICECHANGE.
           ADD B-QUANTITY (SUB-1) TO ST-QTYRECMTD
                                     ST-QTYRECYTD.
           IF ST-QTYONRESERVE < ST-QTYONBORDER
                COMPUTE WS-QTY = (ST-QTYONBORDER - ST-QTYONRESERVE)
            IF WS-QTY > B-QUANTITY (SUB-1)
                ADD B-QUANTITY (SUB-1) TO ST-QTYONRESERVE
                GO TO UPST-015
            ELSE
                ADD WS-QTY TO ST-QTYONRESERVE
                SUBTRACT WS-QTY FROM B-QUANTITY (SUB-1)
                ADD B-QUANTITY (SUB-1) TO ST-QTYONHAND
                ADD WS-QTY TO B-QUANTITY (SUB-1)
                GO TO UPST-015.
           ADD B-QUANTITY (SUB-1) TO ST-QTYONHAND.
       UPST-015.
           MOVE B-ORDERIND (SUB-1)    TO SUB-10
           MOVE WS-ORDER (SUB-10)     TO OO-ORDER-NUMBER
           MOVE B-STOCKNUMBER (SUB-1) TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
             INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO OO-QUANTITY
               GO TO UPST-030.
       UPST-020.
           READ OUTSTANDING-ORDERS
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
               MOVE 0 TO OO-QUANTITY
               GO TO UPST-030.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE 
              "ST-ORDERS BUSY ON READ UPST-020, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO UPST-020.
       UPST-030.
           IF B-QUANTITY (SUB-1) > OO-QUANTITY
               COMPUTE ST-QTYONORDER = ST-QTYONORDER - OO-QUANTITY
               GO TO UPST-700.
            IF ST-QTYONORDER > B-QUANTITY (SUB-1)
                SUBTRACT B-QUANTITY (SUB-1) FROM ST-QTYONORDER
            ELSE
                MOVE 0 TO ST-QTYONORDER.
       UPST-700.
            IF WS-UP-FORGN = "Y"
                MOVE B-COST (SUB-1) TO ST-FOREIGNCOST.
            MOVE WS-DATE            TO ST-LASTRECEIPTDATE
      *      MOVE B-DUTYPERC (SUB-1) TO ST-DUTYPERCENT
            MOVE IMRE-EXCHANGERATE  TO ST-CURRENCY-RATE.
            MOVE IMRE-CURRENCY      TO ST-CURRENCY.
       UPST-900.
            REWRITE STOCK-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE 3010 TO POS
                DISPLAY "STOCK RECORD NOT UPDATED" AT POS
                ADD 25 TO POS
                DISPLAY ST-STOCKNUMBER AT POS
                CALL "LOCKKBD" USING F-FIELDNAME.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO UPST-900.
       UPST-950.
           PERFORM READ-PARAMETER-LOCK
           MOVE PA-STOCK-RECEIPT-NUMBER TO IMRE-TRANSACTION-NUMBER
                                           STRE-TRANSACTION-NUMBER
           ADD 1 TO PA-STOCK-RECEIPT-NUMBER
           PERFORM REWRITE-PARAMETER.

           MOVE B-ORDERIND (SUB-1)     TO SUB-10
           MOVE WS-ORDER (SUB-10)      TO IMRE-ORDERNUM
           MOVE B-STOCKNUMBER (SUB-1)  TO IMRE-STOCK-NUMBER
           MOVE B-QUANTITY (SUB-1)     TO IMRE-QUANTITY
           MOVE B-COST (SUB-1)         TO IMRE-COSTEACH
           MOVE B-DUTYPERC (SUB-1)     TO IMRE-DUTYPERCENT
           MOVE B-DISCOUNTPERC (SUB-1) TO IMRE-DISCOUNTPERCENT
           MOVE B-LANDEDCOST (SUB-1)   TO IMRE-LDCOST
           MOVE B-SELLING (SUB-1)      TO IMRE-SPRICE
           MOVE "N"                    TO IMRE-UPDATED-YN.
       UPST-960.
           WRITE IMPORT-RECEIPTS-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-IMPRECEIPT-ST1 = 23 OR 35 OR 49
               GO TO UPST-950.
           IF WS-IMPRECEIPT-ST1 NOT = 0
               MOVE "IMPORTS RECORD BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-IMPRECEIPT-ST1
               GO TO UPST-960.
       UPST-970.
            MOVE 2                     TO STRE-TRANSACTION-CODE
            MOVE B-STOCKNUMBER (SUB-1) TO STRE-STOCK-NUMBER
            MOVE B-QUANTITY (SUB-1)    TO STRE-QUANTITY
            MOVE B-LANDEDCOST (SUB-1)  TO STRE-UNIT-PRICE
            COMPUTE WS-TOTALPRICE =
                 B-LANDEDCOST (SUB-1) * B-QUANTITY (SUB-1)
            MOVE WS-TOTALPRICE         TO STRE-TOTAL-PRICE
            MOVE IMRE-ALT-KEY          TO STRE-REFERENCE-NO
            MOVE IMRE-DATERECEIVED     TO STRE-REFERENCE-DATE
            MOVE WS-ORDER (SUB-10)     TO STRE-ORDER-NUMBER.
       UPST-975.
            WRITE STOCK-RECEIPTS-REC
                 INVALID KEY NEXT SENTENCE.
            IF WS-STKRECEIPT-ST1 = 23 OR 35 OR 49
                 GO TO UPST-980.
            IF WS-STKRECEIPT-ST1 NOT = 0
               MOVE "ST-RECEIPT BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               
               PERFORM READ-PARAMETER-LOCK
               MOVE PA-STOCK-RECEIPT-NUMBER TO STRE-TRANSACTION-NUMBER
               ADD 1 TO PA-STOCK-RECEIPT-NUMBER
               PERFORM REWRITE-PARAMETER

               MOVE 0 TO WS-STKRECEIPT-ST1
               GO TO UPST-975.
       UPST-980.
           PERFORM UPDATE-OUT-ORDERS.
           
           IF WS-STOCK-CHANGE = "Y"
            IF WS-UP-SELL = "Y"
               PERFORM WRITE-STOCK-CHANGES
               GO TO UPST-985.
           IF WS-STOCK-CHANGE = "Y"
             IF WS-UP-FORGN = "Y"
               PERFORM WRITE-STOCK-CHANGES.
       UPST-985.
           ADD 1 TO SUB-1.
           IF SUB-1 < 401
              GO TO UPST-010.
       UPST-999.
            EXIT.
      *
       REWRITE-RECEIPTS-2ND-TIME SECTION.
       RR2T-970.
            PERFORM READ-PARAMETER-LOCK
            MOVE PA-STOCK-RECEIPT-NUMBER TO STRE-TRANSACTION-NUMBER
            ADD 1 TO PA-STOCK-RECEIPT-NUMBER
            PERFORM REWRITE-PARAMETER.

            MOVE 1                     TO STRE-TRANSACTION-CODE
            MOVE IMRE-STOCK-NUMBER     TO STRE-STOCK-NUMBER
            MOVE 0                     TO STRE-QUANTITY
            MOVE ST-AVERAGECOST        TO STRE-UNIT-PRICE
            COMPUTE WS-TOTALPRICE =
                 ST-AVERAGECOST * IMRE-QUANTITY
            MOVE WS-TOTALPRICE         TO STRE-TOTAL-PRICE
            MOVE IMRE-ALT-KEY          TO STRE-REFERENCE-NO
            MOVE WS-DATE               TO STRE-REFERENCE-DATE
            MOVE "RATE@"               TO WS-RECOST-COMMENT
            MOVE WS-RATEPAID           TO WS-RECOST-EXRATE
            MOVE WS-RECOST-INFO        TO STRE-ORDER-NUMBER.
       RR2T-975.
            WRITE STOCK-RECEIPTS-REC
                 INVALID KEY NEXT SENTENCE.
            IF WS-STKRECEIPT-ST1 NOT = 0
               MOVE "ST-RECEIPT BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-IMPRECEIPT-ST1
               GO TO RR2T-975.
       RR2T-999.
            EXIT.
      *
       REAVERAGE-COST SECTION.
       REAV-000.
            MOVE B-SELLING (SUB-1) TO WS-PRICE.

            IF ST-QTYONHAND > 0
                COMPUTE ST-AVERAGECOST ROUNDED =
               ((ST-QTYONHAND * ST-AVERAGECOST +
               (B-QUANTITY (SUB-1) * B-LANDEDCOST (SUB-1))) /
               (ST-QTYONHAND + B-QUANTITY (SUB-1)))
                MOVE B-LANDEDCOST (SUB-1) TO ST-LASTCOST
            ELSE
                MOVE B-LANDEDCOST (SUB-1) TO ST-AVERAGECOST
                                             ST-LASTCOST.
       REAV-999.
            EXIT.
      *
       CHECK-TO-RE-ENTER-ORDER SECTION.
       CHECK-TREO-000.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            MOVE 2810 TO POS
            DISPLAY WS-MESSAGE AT POS.
       CHECK-TREO-008.
            MOVE 2810 TO POS.
            DISPLAY "THIS IS THE ORDER-NUMBER:[              ]"
                 AT POS.
            ADD 26 TO POS.
            DISPLAY WS-ORDER (SUB-10) AT POS.
            MOVE 2910 TO POS.
            DISPLAY "DO YOU WISH TO CHANGE THE ORDER-NUMBER ENTERED.[ ]"
                 AT POS.
            ADD 48 TO POS.

            MOVE ' '       TO CDA-DATA.
            MOVE 1         TO CDA-DATALEN.
            MOVE 26        TO CDA-ROW.
            MOVE 57        TO CDA-COL.
            MOVE CDA-GREEN TO CDA-COLOR.
            MOVE 'F'       TO CDA-ATTR.
            PERFORM CTOS-ACCEPT.
            MOVE CDA-DATA TO WS-CHANGE-ORDER.

            IF WS-CHANGE-ORDER NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CHECK-TREO-008.
            IF WS-CHANGE-ORDER = "N"
               GO TO CHECK-TREO-900.
       CHECK-TREO-010.
            MOVE "ORDERNUM" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = " "
                MOVE "YOU CANNOT HAVE A BLANK ORDER NUMBER, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO CHECK-TREO-010.
            MOVE F-NAMEFIELD TO WS-ORDER (SUB-10).
       CHECK-TREO-900.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
       CHECK-TREO-999.
            EXIT.
      *
       UPDATE-OUT-ORDERS SECTION.
       UPOO-000.
           MOVE B-ORDERIND (SUB-1)    TO SUB-10
           MOVE WS-ORDER (SUB-10)     TO OO-ORDER-NUMBER
           MOVE B-STOCKNUMBER (SUB-1) TO OO-STOCK-NUMBER.
           READ OUTSTANDING-ORDERS WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
               MOVE 3001 TO POS
               DISPLAY " " AT 3079 WITH BELL
               DISPLAY "ORDER NOT FOUND FOR STOCK NUMBER:" AT POS
               MOVE 3035 TO POS
               DISPLAY B-STOCKNUMBER (SUB-1) AT POS
               MOVE 3051 TO POS
               DISPLAY "ORDER NO:" AT POS
               ADD 10 TO POS
               DISPLAY WS-ORDER (SUB-10) AT POS
               CALL "LOCKKBD" USING F-FIELDNAME
               PERFORM ERROR-020
               PERFORM CHECK-TO-RE-ENTER-ORDER
            IF WS-CHANGE-ORDER = "N"
               GO TO UPOO-900
            ELSE
               GO TO UPOO-000.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "O/S ST-ORDERS BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO UPOO-000.
       UPOO-040.
           SUBTRACT B-QUANTITY (SUB-1) FROM OO-QUANTITY.
           IF OO-QUANTITY < 1
               MOVE 0 TO OO-QUANTITY.
       UPOO-050.
           REWRITE OUT-ORDER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               DISPLAY "ORDER NOT UPDATED" AT POS
               CALL "LOCKKBD" USING F-FIELDNAME
               GO TO UPOO-900.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "ORDERS BUSY ON REWRITE UPOO-050, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO UPOO-050.
       UPOO-900.
           PERFORM ERROR-020
           PERFORM ERROR1-020.
           MOVE 2501 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2601 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2701 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2801 TO POS
           DISPLAY WS-MESSAGE AT POS.
       UPOO-999.
           EXIT.
      *
       UPDATE-SECOND-TIME SECTION.
       UP2ND-000.
           MOVE 0 TO LINE-CNT.
           PERFORM ERROR-020.
           PERFORM CLEAR-010.
           MOVE " " TO WS-UPDATE.
           MOVE 3010 TO POS.
           DISPLAY "IS IT ALRIGHT TO UPDATE?? ENTER Y OR N: [ ]" AT POS.
           ADD 41 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-UPDATE.

      *     ACCEPT WS-UPDATE AT POS.
           IF W-ESCAPE-KEY NOT = 0 AND NOT 1 AND NOT 2 AND NOT 5
              DISPLAY " " AT 3079 WITH BELL
              GO TO UP2ND-000.
           IF WS-UPDATE = "Y"
              GO TO UP2ND-001.
           IF WS-UPDATE = "N"
              GO TO UP2ND-999.
           MOVE "ENTER ONLY Y OR N FOR THE ANSWER." TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           MOVE " " TO WS-UPDATE.
           GO TO UP2ND-000.
       UP2ND-001.
           MOVE " " TO WS-UP-SELL.
           PERFORM ERROR-020.
           MOVE 3010 TO POS.
           DISPLAY "UPDATE PRICE, ENTER Y = YES; N = NO:    [ ]" AT POS.
           ADD 41 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1        TO CDA-DATALEN.
           MOVE 27         TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-UP-SELL.

      *     ACCEPT WS-UP-SELL AT POS.
           IF W-ESCAPE-KEY NOT = 0 AND NOT 1 AND NOT 2 AND NOT 5
              DISPLAY " " AT 3079 WITH BELL
              GO TO UP2ND-001.
           IF WS-UP-SELL = "Y" OR = "N"
              GO TO UP2ND-005.
           MOVE "WHAT ARE YOU TRYING TO ANSWER?????" TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
           GO TO UP2ND-001.
       UP2ND-005.
           MOVE " " TO WS-UP-FORGN.
           PERFORM ERROR-020.
           MOVE 3010 TO POS.
           DISPLAY "  UPDATE FOREIGN COSTS ? ENTER Y OR N:  [ ]" AT POS.
           ADD 41 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-UP-FORGN.

      *     ACCEPT WS-UP-FORGN AT POS.
           IF W-ESCAPE-KEY NOT = 0 AND NOT 1 AND NOT 2 AND NOT 5
              DISPLAY " " AT 3079 WITH BELL
              GO TO UP2ND-005.
           IF WS-UP-FORGN = "Y" OR = "N"
              GO TO UP2ND-010.
           MOVE "WHAT ARE YOU TRYING TO ANSWER?????" TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
           GO TO UP2ND-005.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
       UP2ND-010.
          MOVE "N" TO WS-ITEM-RECOSTED.
          COMPUTE WS-RATE-DIFF = IMRE-EXCHANGERATE - WS-RATEPAID.
          IF WS-RATE-DIFF = ZERO
               GO TO UP2ND-025.
          COMPUTE WS-RATE-DIFF-PERC ROUNDED =
                 ((WS-RATE-DIFF / IMRE-EXCHANGERATE) * 100).
       UP2ND-020.            
      * HERE WE SAVE THE ORIGINAL LANDED COST OF THE SHIPMENT
      * WS-LDCOST IS THE COST ON THE SHIPMENT FOR USE A FEW LINES DOWN.
           MOVE IMRE-LDCOST TO WS-LDCOST.
           COMPUTE WS-RATE-DIFF-PERC = WS-RATE-DIFF-PERC / 100.
                
           COMPUTE IMRE-LDCOST ROUNDED =  IMRE-LDCOST +
              (IMRE-LDCOST * WS-RATE-DIFF-PERC).
               
           MOVE IMRE-STOCK-NUMBER TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           
           MOVE 2920 TO POS
           DISPLAY ST-STOCKNUMBER AT POS
           ADD 1 TO LINE-CNT
           ADD 20 TO POS
           DISPLAY "LINE #" AT POS
           ADD 7 TO POS
           DISPLAY LINE-CNT AT POS.
           
      * IF ST-STOCKNUMBER = " " IT IS BECAUSE THERE IS NO SUCH FILE
      *  WHEN IT DID THE READ-STOCK-LOCK.  ITEM DELETED OR RENAMED.
           IF ST-STOCKNUMBER = " "
              GO TO UP2ND-025.
           IF WS-UP-SELL = "Y"
                MOVE ST-PRICE     TO ST-OLDPRICE
                MOVE IMRE-SPRICE  TO ST-PRICE
                MOVE WS-DATE      TO ST-LASTPRICECHANGE.
           IF WS-UP-FORGN = "Y"
              MOVE IMRE-COSTEACH  TO ST-FOREIGNCOST.
      **************************************************************
      * THE ASSUMPTION HERE IS THAT IF = THEN THE COST ON STOCK IS *
      * FROM THIS SHIPMENT.  IF NOT THEN OTHER SHIPMENTS RECEIVED  *
      * AFTER THIS SHIPMENT - THEREFORE DON'T CHANGE LAST-COST.    *
      * THEREFORE IF = THEN THE AVE-COST SHOULD ALSO BE CHANGED TO *
      * REFLECT THE ACTUAL COST OF THE GOODS.                      *
      **************************************************************
              
      * RE-COSTING HAS GONE UP DUE TO THE RAND FALLING - LIKE JAN 2002
           IF WS-LDCOST = ST-LASTCOST
            IF WS-RATE-DIFF-PERC > 0
             IF IMRE-LDCOST > ST-AVERAGECOST
           COMPUTE ST-AVERAGECOST ROUNDED =  ST-AVERAGECOST  +
              (ST-AVERAGECOST * WS-RATE-DIFF-PERC)
              MOVE "Y" TO WS-ITEM-RECOSTED.
      *        MOVE IMRE-LDCOST    TO ST-AVERAGECOST.
              
      * RE-COSTING HAS GONE DOWN DUE TO THE RAND RISING - LIKE DEC 2002
           IF WS-LDCOST = ST-LASTCOST
            IF WS-RATE-DIFF-PERC < 0
             IF IMRE-LDCOST < ST-AVERAGECOST
           COMPUTE ST-AVERAGECOST  ROUNDED =  ST-AVERAGECOST  +
              (ST-AVERAGECOST  * WS-RATE-DIFF-PERC)
              MOVE "Y" TO WS-ITEM-RECOSTED.
      *        MOVE IMRE-LDCOST    TO ST-AVERAGECOST.

           IF WS-LDCOST = ST-LASTCOST
              MOVE IMRE-LDCOST    TO ST-LASTCOST
              MOVE WS-RATEPAID    TO ST-CURRENCY-RATE.
              
      ****************************************************************
      *THE PROBLEM HERE IS IF YOU RE-COST THE SAME ITEM A NUMBER OF  *
      * TIMES ON DIFFERENT COSTINGS THE AVERAGE COST JUST KEEPS GOING*
      * HIGHER BY THE INCREASED CHANGE IN EXCHANGE-RATE.             *
      * THE ABOVE SOLUTION SHOULD FIX THAT THINKING.                 *
      ****************************************************************
      *
      *     COMPUTE ST-AVERAGECOST ROUNDED = ST-AVERAGECOST +
      *            (ST-AVERAGECOST * WS-RATE-DIFF-PERC).
      *
      * ABOVE TAKEN OUT 21/2/2002 SO THE FULL DIFF. IS WRITTEN OVER
      *             (ST-AVERAGECOST * (WS-RATE-DIFF-PERC / 2)).
           IF WS-ITEM-RECOSTED = "Y"
               PERFORM REWRITE-RECEIPTS-2ND-TIME.
       UP2ND-025.
           PERFORM REWRITE-STOCK.
       UP2ND-030.
           READ IMPRECEIPTS-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-IMPRECEIPT-ST1 = 10
              GO TO UP2ND-999.
           IF WS-IMPRECEIPT-ST1 NOT = 0
              MOVE "IMPORTS BUSY ON READ-NEXT-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-IMPRECEIPT-ST1
              GO TO UP2ND-030.
           IF IMRE-ALT-KEY = WS-ALT-KEY
              GO TO UP2ND-010.
       UP2ND-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE 0 TO PAGE-CNT.
           PERFORM READ-PARAMETER.
       PRR-001.
           IF LINE-CNT < 61
               GO TO PRR-010.
       PRR-005.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
              WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
              WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE IMRE-SUPPLIER TO H3-SUPPLIERNO
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC.
           MOVE IMRE-INVOICENUM    TO H3-INVNO
           MOVE IMRE-TOTALDUTYRAND TO H5-TOTRANDDUTY
           MOVE B-ORDERIND (SUB-1) TO SUB-9
           MOVE WS-ORDER (SUB-9)   TO IMRE-ORDERNUM
           MOVE IMRE-ORDERNUM      TO H7-ORDERNO
           WRITE PRINT-REC FROM HEAD4 AFTER 1.
           MOVE " " TO PRINT-REC
           MOVE IMRE-CURRENCY     TO H4-CURRDESC
           MOVE IMRE-FREIGHTRAND  TO H6-RANDFRGTAMT
           MOVE IMRE-DATERECEIVED TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO H7-DATERECVD
           WRITE PRINT-REC FROM HEAD5 AFTER 1.
           MOVE " " TO PRINT-REC
           MOVE IMRE-EXCHANGERATE TO H7-EXCHANGERATE
           MOVE IMRE-WARFAGE      TO H6-WHARFAGE
           MOVE IMRE-RATEPAID     TO H8-RATEPAID
           WRITE PRINT-REC FROM HEAD6 AFTER 1.
           MOVE " " TO PRINT-REC
           MOVE IMRE-INVAMTFOREIGN TO H4-FORINVAMT
           MOVE IMRE-CLEARING      TO H6-CLEARING
           MOVE IMRE-RAND-AMT-PAID TO H8-RANDPAID
           WRITE PRINT-REC FROM HEAD7 AFTER 1.
           MOVE " " TO PRINT-REC
           MOVE IMRE-FREIGHTAMTFOREIGN TO H4-FORFRGTAMT
           MOVE IMRE-SURCHARGE-RAND    TO H6-RANDSURCH
           MOVE IMRE-MARKUPPERCENT     TO H7-MARKUPPERC
           WRITE PRINT-REC FROM HEAD8 AFTER 1.
           MOVE " " TO PRINT-REC
           MOVE IMRE-RANDINVAMTGROSS TO H5-GROSSRANDINVAMT
           MOVE IMRE-TOTALCHARGES    TO H6-TOTCHARGES
           MOVE IMRE-ONCOSTPERCENT   TO H7-ONCOSTPERC
           WRITE PRINT-REC FROM HEAD8A AFTER 1.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD9 AFTER 2
           MOVE " " TO PRINT-REC
           MOVE 10 TO LINE-CNT.
       PRR-010.
           MOVE SUB-1                 TO D-LINE
           MOVE B-STOCKNUMBER (SUB-1) TO D-STOCKNO
                                         WS-STOCKNUMBER.
           PERFORM READ-STOCK.
           MOVE B-QUANTITY (SUB-1) TO D-QTY
           MOVE B-COST (SUB-1)     TO D-COSTEACH
           MOVE B-DUTYPERC (SUB-1) TO D-DUTYPERC.
           COMPUTE WS-WORK-DUTY =
                B-DUTYAMOUNT (SUB-1) * B-QUANTITY (SUB-1).
           MOVE WS-WORK-DUTY       TO D-DUTAMT
           ADD WS-WORK-DUTY        TO WS-TOTAL-DUTY
           MOVE 0                  TO WS-WORK-DUTY.

           COMPUTE WS-WORK-DUTY =
              (((B-SURCHPERC (SUB-1) * B-COST (SUB-1)) / 100)
                   * B-QUANTITY (SUB-1)) / IMRE-EXCHANGERATE.
           IF B-DISCOUNTPERC (SUB-1) > 0
             COMPUTE WS-WORK-DUTY = WS-WORK-DUTY -
                 ((WS-WORK-DUTY * B-DISCOUNTPERC (SUB-1)) / 100).
           ADD WS-WORK-DUTY            TO WS-TOTAL-SURCHARGE
           MOVE 0                      TO WS-WORK-DUTY
           MOVE B-DISCOUNTPERC (SUB-1) TO D-DISCPERC
           MOVE B-SURCHPERC (SUB-1)    TO D-SURCPERC
           MOVE B-LANDEDCOST (SUB-1)   TO D-LDCOST
           MOVE B-SELLING (SUB-1)      TO D-SPRICE
           MOVE ST-PRICE               TO D-OSPRICE
           COMPUTE WS-MARGIN = ST-PRICE - B-LANDEDCOST (SUB-1).
           COMPUTE WS-PERC = (WS-MARGIN / B-LANDEDCOST (SUB-1)) * 100.
           MOVE WS-PERC                TO D-PERC
           MOVE " " TO PRINT-REC
           MOVE B-ORDERIND (SUB-1) TO D-IND
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT SUB-1.
           IF SUB-1 > 400
               GO TO PRR-900.
           IF B-STOCKNUMBER (SUB-1) = " "
               MOVE 1 TO SUB-1
               GO TO PRR-900.
           GO TO PRR-001.
       PRR-900.
           IF LINE-CNT > 55
                PERFORM PRR-005.
           MOVE 0 TO WS-FOREIGN-VAL.
       PRR-905.
           IF WS-ORDER (SUB-1) = "    "
              MOVE "TOT-FOREIGN"         TO O-NAME
              MOVE " "                   TO O-IND
              MOVE " "                   TO O-NO
              MOVE WS-FOREIGN-VAL        TO O-VALUE
              WRITE PRINT-REC FROM ORDER-LINE AFTER 1
              GO TO PRR-910.
           IF SUB-1 = 1
              MOVE "Order No #:"         TO O-NAME
           ELSE
              MOVE "           "         TO O-NAME.
           MOVE SUB-1                    TO O-IND
           MOVE WS-ORDER (SUB-1)         TO O-NO
           MOVE WS-FOREIGN-VALUE (SUB-1) TO O-VALUE
           ADD WS-FOREIGN-VALUE (SUB-1)  TO WS-FOREIGN-VAL.
           IF SUB-1 = 1
              WRITE PRINT-REC FROM ORDER-LINE AFTER 2
           ELSE
              WRITE PRINT-REC FROM ORDER-LINE AFTER 1.
           ADD 1 TO SUB-1.
           IF SUB-1 < 21
               GO TO PRR-905.
       PRR-910.
           MOVE "** TOTAL GOODS FOREIGN**" TO TOT-DESC
           MOVE WS-FOREIGN-CHECK-VAL       TO TOT-COST
           MOVE "** TOTAL DUTY AMOUNT **"  TO TOT-DUTY-DESC
           MOVE WS-TOTAL-DUTY              TO TOT-DUTY
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.

           MOVE "                        " TO TOT-DESC
           MOVE 0                          TO TOT-COST
           MOVE "* TOTAL SURCHARGE AMT**"  TO TOT-DUTY-DESC
           MOVE WS-TOTAL-SURCHARGE         TO TOT-DUTY
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.

           MOVE 0 TO WS-TOTAL-DUTY
                     WS-TOTAL-SURCHARGE.
           MOVE " " TO PRINT-REC TOTAL-LINE
           MOVE "** BEGINNING TRANS. No**" TO TOT-DESC
           MOVE PA-STOCK-RECEIPT-NUMBER    TO TOT-DUTY-DESC
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       PRR-999.
           EXIT.
      *
       WRITE-FOREIGN-CRJRN-TRANS SECTION.
       WFCT-000.
            PERFORM CLEAR-SCREEN.
            PERFORM CLEAR-CRJRN-FOREIGN-AMTS.
            MOVE "StCrJnIm"      TO F-FORMNAME
            PERFORM OPEN-060 THRU OPEN-900
            PERFORM DISPLAY-FORM.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            
            MOVE 2815 TO POS
            DISPLAY "PRESS <Alt-ESC> TO EXIT THIS PROGRAM, OR"
             AT POS.
            MOVE 2910 TO POS
            DISPLAY 
           "WHEN ALL INFO'S ENTERED, <GO> AT DATE FIELD TO WRITE TRANS."
                AT POS.
            PERFORM READ-PARAMETER
            MOVE PA-GST-PERCENT       TO WS-VAT-PERC.
            MOVE IMRE-RANDINVAMTGROSS TO CRJRN-LOC-AMT.
            MOVE IMRE-INVAMTFOREIGN   TO CRJRN-FOR-AMT.
            PERFORM WFCT-061.
            PERFORM WFCT-066.
            PERFORM WFCT-068.
            PERFORM WFCT-150.
            PERFORM RWCJ-002 THRU RWCJ-005.

            MOVE "CRJRNBATCH" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-JRN       TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       WFCT-005.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "DNOTE" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DNOTE-NO WS-INV-NO.
            IF F-EXIT-CH = X"87"
               PERFORM ERROR-020
               PERFORM ERROR1-020
               MOVE 2810 TO POS
               DISPLAY WS-MESSAGE AT POS
               GO TO WFCT-999.
            IF WS-DNOTE-NO = " "
               MOVE "THIS ENTRY CANNOT BE BLANK, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO WFCT-005.
            
            MOVE "N" TO WS-NEXT
            GO TO WFCT-080.
       WFCT-061.
            MOVE "DNOTE"       TO F-FIELDNAME
            MOVE 5             TO F-CBFIELDNAME
            MOVE WS-INVOICENUM TO F-NAMEFIELD
            MOVE 10            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       WFCT-062.
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
       WFCT-066.
            MOVE "LOCAL-TOT"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME.
            MOVE CRJRN-LOC-AMT TO F-EDNAMEFIELDNUM6.
            MOVE 11            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "FOREIGN-TOT" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME.
            MOVE CRJRN-FOR-AMT TO F-EDNAMEFIELDNUM6.
            MOVE 11            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE 0             TO CRJRN-VAT-AMT.
       WFCT-067.
            MOVE "INVDATE"      TO F-FIELDNAME.
            MOVE 7              TO F-CBFIELDNAME.
            MOVE CRJRN-INV-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE   TO F-NAMEFIELD.
            MOVE 10             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       WFCT-068.
            COMPUTE WS-TOTAL = CRJRN-LOC-AMT + CRJRN-VAT-AMT.
       WFCT-080.
            MOVE 2810 TO POS.
            IF WS-NEXT = "N"
               DISPLAY
            "ENTER A/C NUMBER & <RETURN> OR A SHORT NAME & <PgDn>"
               AT POS
            ELSE
              DISPLAY
            "TO END SEARCH AND RE-ENTER SHORT NAME PRESS <Esc>.    "
               AT POS.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO WFCT-005.
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
                GO TO WFCT-080.
            MOVE 7            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            IF F-EXIT-CH = X"0C"
                PERFORM READ-NEXT-CREDITOR
                PERFORM WFCT-062
             IF WS-CREDITOR-ST1 NOT = 88 AND NOT = 10
                MOVE "Y" TO WS-NEXT
                GO TO WFCT-080
             ELSE
                MOVE 0   TO CR-ACCOUNT-NUMBER
                MOVE " " TO CR-NAME
                GO TO WFCT-080.
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
               GO TO WFCT-080.
       WFCT-090.
            PERFORM ERROR-020
            MOVE 2810 TO POS
            DISPLAY WS-MESSAGE AT POS.
       WFCT-091.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "LOCAL-TOT" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            MOVE 11          TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO WFCT-080.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRJRN-LOC-AMT CRJRN-UNAPPLIED-AMT
                                 F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF CRJRN-LOC-AMT NOT > 0
               PERFORM ERROR1-020
               MOVE "THIS AMOUNT MUST BE > ZERO, RE-ENTER" TO WS-MESSAGE
               PERFORM ERROR1-000
               GO TO WFCT-091.

            PERFORM WFCT-066.
       WFCT-095.
            PERFORM ERROR1-020
            MOVE 2910 TO POS
            DISPLAY 
            "WHEN ALL INFO'S ENTERED, <GO> AT LAST FIELD TO WRITE TRANS"
                AT POS.

            MOVE 0 TO SUB-20 F-INDEX.
            GO TO WFCT-151.
       WFCT-150.
            PERFORM OPEN-950.

            MOVE "INVDATE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE WS-DATE      TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       WFCT-151.
            ADD 1 TO SUB-20 F-INDEX.
            IF SUB-20 > 10
               MOVE 10 TO SUB-20 F-INDEX.
       WFCT-152.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "DNOTES" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO FL-DNOTE (SUB-20).
            IF F-EXIT-CH = X"01"
             IF SUB-20 = 1
                GO TO WFCT-080
             ELSE
                SUBTRACT 1 FROM SUB-20 F-INDEX
                GO TO WFCT-152.
      * TAB-KEY
            IF F-EXIT-CH = X"09"
             IF WS-FOR-RUNNING = CRJRN-FOR-AMT
               GO TO WFCT-160
             ELSE
               MOVE "D/NOTE TOTALS DON'T EQUAL FOREIGN INV AMOUNT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WFCT-152.
            IF FL-DNOTE (SUB-20) = " "
               MOVE "THIS ENTRY CANNOT BE BLANK, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO WFCT-152.
       WFCT-153.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "FOREIGN-AMOUNT" TO F-FIELDNAME.
            MOVE 14               TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11               TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
                GO TO WFCT-152.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO FL-FOR-AMT (SUB-20)
                                 F-EDNAMEFIELDNUM6.

            IF FL-FOR-AMT (SUB-20) = 0
               PERFORM ERROR1-020
               MOVE "THIS AMOUNT CAN'T BE = ZERO, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               GO TO WFCT-153.
               
            COMPUTE WS-LOC-COMPUTE ROUNDED = FL-FOR-AMT (SUB-20)
                 / IMRE-EXCHANGERATE
            MOVE WS-LOC-COMPUTE           TO FL-LOC-AMT (SUB-20).
            
            PERFORM WFCT-155 THRU WFCT-157.

            GO TO WFCT-151.
       WFCT-155.
            MOVE "FOREIGN-AMOUNT"    TO F-FIELDNAME
            MOVE 14                  TO F-CBFIELDNAME
            MOVE FL-FOR-AMT (SUB-20) TO F-EDNAMEFIELDNUM6
            MOVE 11                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC6.
       
            MOVE "LOCAL-AMOUNT"      TO F-FIELDNAME
            MOVE 12                  TO F-CBFIELDNAME
            MOVE FL-LOC-AMT (SUB-20) TO F-EDNAMEFIELDNUM6
            MOVE 11                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE 1 TO SUB-25.
            MOVE 0 TO WS-FOR-RUNNING
                      WS-LOC-RUNNING.
       WFCT-156.
            ADD FL-FOR-AMT (SUB-25) TO WS-FOR-RUNNING 
            ADD FL-LOC-AMT (SUB-25) TO WS-LOC-RUNNING.
            IF SUB-25 < 10
               ADD 1 TO SUB-25
               GO TO WFCT-156.
       WFCT-157.
            MOVE "FOR-TOT-RUNNING"   TO F-FIELDNAME
            MOVE 15                  TO F-CBFIELDNAME
            MOVE WS-FOR-RUNNING      TO F-EDNAMEFIELDNUM6
            MOVE 11                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC6.
       
            MOVE "LOC-TOT-RUNNING"   TO F-FIELDNAME
            MOVE 15                  TO F-CBFIELDNAME
            MOVE WS-LOC-RUNNING      TO F-EDNAMEFIELDNUM6
            MOVE 11                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC6.
       WFCT-160.
            IF WS-LOC-RUNNING = IMRE-RANDINVAMTGROSS
               GO TO WFCT-163.
            COMPUTE WS-LOC-COMPUTE = 
                   (WS-LOC-RUNNING - IMRE-RANDINVAMTGROSS)
            ADD WS-LOC-COMPUTE TO FL-LOC-AMT (SUB-25)
            PERFORM WFCT-157.
       WFCT-163.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "INVDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO WFCT-095.
            MOVE 10          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO WFCT-163.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CRJRN-INV-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO WFCT-163.

           PERFORM WFCT-067.
           IF F-EXIT-CH NOT = X"1B"
               GO TO WFCT-005.
           
           PERFORM ERROR1-020.
           MOVE 2910 TO POS
           DISPLAY "WRITING CREDITOR FOREIGN JRN RECORD......" AT POS.
           PERFORM REWRITE-FOREIGN-CRJRN.
           PERFORM ERROR1-020.
       WFCT-999.
            EXIT.
      *
       CLEAR-CRJRN-FOREIGN-AMTS SECTION.
       CCFA-005.
            MOVE 1 TO SUB-20.
       CCFA-010.
            MOVE SPACES TO FL-DNOTE (SUB-20)
            MOVE 0      TO FL-FOR-AMT (SUB-20)
                           FL-LOC-AMT (SUB-20).
            ADD 1 TO SUB-20.
            IF SUB-20 < 11
               GO TO CCFA-010.
       CCFA-999.
           EXIT.
      *
       CLEAR-CRJRN-RECORD SECTION.
       CCJR-001.
            MOVE " "        TO CRJRN-REFERENCE
                               CRJRN-FUTURE
                               CRJRN-INV-NO
                               CRJRN-DNOTE-NO
                               CRJRN-COMPLETE.
            MOVE 0          TO CRJRN-TRANS
                               CRJRN-TYPE
                               CRJRN-NO
                               CRJRN-CRACC-NUMBER
                               CRJRN-INV-DATE
                               CRJRN-DUE-DATE
                               CRJRN-LOC-AMT
                               CRJRN-VAT-AMT
                               CRJRN-UNAPPLIED-AMT
                               CRJRN-FOR-AMT
                               CRJRN-EXCHANGE
                               CRJRN-SETT-DISC.
       CCJR-005.
            MOVE 1 TO SUB-20.
       CCJR-010.
            MOVE SPACES TO CRJRN-GLACC (SUB-20)
                           CRJRN-GLDESC (SUB-20).
            MOVE 0      TO CRJRN-GLAMT (SUB-20)
                           CRJRN-GLDISC (SUB-20).
            ADD 1 TO SUB-20.
            IF SUB-20 < 11
               GO TO CCJR-010.
       CCJR-999.
           EXIT.
      *
       READ-CREDITOR SECTION.
       RCR-000.
           MOVE "N" TO WS-NEXT.
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
       WRITE-FREIGHT-CRJRN-TRANS SECTION.
       WCIT-000.
            PERFORM CLEAR-SCREEN.
            MOVE "StCrJnMt" TO F-FORMNAME
            PERFORM OPEN-060 THRU OPEN-900
            PERFORM DISPLAY-FORM.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            
            MOVE 2701 TO POS.
            MOVE 
            "                ** THIS SCREEN FOR FREIGHT INVOICE ONLY **"
             TO WS-MESSAGE
            DISPLAY WS-MESSAGE AT POS 
              WITH reverse-video BLINK
                BELL FOREGROUND-COLOR IS 4.
            MOVE 2815 TO POS
            DISPLAY "PRESS <Alt-F10> TO EXIT THIS PROGRAM." AT POS.
            MOVE 2910 TO POS
            DISPLAY 
            "WHEN ALL INFO'S ENTERED, <GO> AT LAST FIELD TO WRITE TRANS"
                AT POS.
       WCIT-003.
            PERFORM READ-PARAMETER
            MOVE PA-GST-PERCENT TO WS-VAT-PERC.
            MOVE WS-TOTALPRICE  TO CRJRN-LOC-AMT.
      *      PERFORM WCIT-066.
      *      PERFORM WCIT-068.
            PERFORM WCIT-150.
            PERFORM RWCR-005.
            
      *      MOVE CRJRN-LOC-AMT TO WS-INV-AMT.

            MOVE "BATCH-TYPE" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME.
            MOVE "*FREIGHT*"  TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CRJRNBATCH" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-JRN       TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       WCIT-005.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "DNOTE" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DNOTE-NO WS-INV-NO.
            IF F-EXIT-CH = X"87" OR = X"9F"
               PERFORM ERROR-020
               PERFORM ERROR1-020
               MOVE 2610 TO POS
               DISPLAY WS-MESSAGE AT POS
               GO TO WCIT-999.
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
            MOVE " "         TO F-NAMEFIELD
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
            MOVE 0             TO F-EDNAMEFIELDNUM6
            MOVE 11            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "VATAMT" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME.
            COMPUTE CRJRN-VAT-AMT ROUNDED =
                 ((CRJRN-LOC-AMT * WS-VAT-PERC) / 100).
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
                 CRJRN-LOC-AMT + CRJRN-VAT-AMT
            MOVE WS-TOTAL      TO F-EDNAMEFIELDNUM6.
            MOVE 11            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
       WCIT-080.
            MOVE 2810 TO POS.
            IF WS-NEXT = "N"
               DISPLAY
            "ENTER A/C NUMBER & <RETURN> OR A SHORT NAME & <PgDn>"
               AT POS
            ELSE
              DISPLAY
            "TO END SEARCH AND RE-ENTER SHORT NAME PRESS <Esc>.    "
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

            MOVE 2910 TO POS
            DISPLAY 
            "WHEN ALL INFO'S ENTERED, <GO> AT LAST FIELD TO WRITE TRANS"
                AT POS.
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
                GO TO WCIT-091.
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
            PERFORM OPEN-950.
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
                GO TO WCIT-091.
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
       REWRITE-CRJRN SECTION.
       RWCR-005.
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
           MOVE WS-CURRENTPER      TO CRJRN-NO
           MOVE "N"                TO CRJRN-COMPLETE
           PERFORM CHECK-DUE-DATE
           MOVE " "                TO CRJRN-INV-NO
           MOVE WS-DNOTE-NO        TO CRJRN-DNOTE-NO
                                      CRJRN-INV-NO
           MOVE WS-CREDITOR        TO CRJRN-CRACC-NUMBER
           COMPUTE CRJRN-LOC-AMT = CRJRN-LOC-AMT + CRJRN-VAT-AMT
           MOVE CRJRN-LOC-AMT      TO CRJRN-UNAPPLIED-AMT
           MOVE IMRE-INVAMTFOREIGN TO CRJRN-FOR-AMT
           MOVE IMRE-EXCHANGERATE  TO CRJRN-EXCHANGE
           MOVE 0                  TO CRJRN-SETT-DISC.
           IF CR-SETT-DISC > 0
             COMPUTE CRJRN-SETT-DISC =
               (CRJRN-LOC-AMT * CR-SETT-DISC) / 100.
           MOVE 1             TO CRJRN-TYPE.
           MOVE 0 TO SUB-1.
       RWCR-010.
           IF SUB-1 < 2
              ADD 1 TO SUB-1
           ELSE
              GO TO RWCR-019.
           IF SUB-1 = 1
              MOVE "50-200-10-00"      TO CRJRN-GLACC (SUB-1)
              MOVE CRJRN-GLACC (SUB-1) TO GL-NUMBER
              PERFORM READ-GLMASTER
              COMPUTE CRJRN-GLAMT (SUB-1) = WS-TOTAL - CRJRN-VAT-AMT
              MOVE GL-DESCRIPTION      TO CRJRN-GLDESC (SUB-1)
              COMPUTE CRJRN-GLDISC (SUB-1) =
                 (CRJRN-GLAMT (SUB-1) * CR-SETT-DISC) / 100
           ELSE
              MOVE GLPA-GLVAT-ACC      TO CRJRN-GLACC (SUB-1)
              MOVE CRJRN-GLACC (SUB-1) TO GL-NUMBER
              PERFORM READ-GLMASTER
              MOVE GL-DESCRIPTION      TO CRJRN-GLDESC (SUB-1)
              MOVE CRJRN-VAT-AMT       TO CRJRN-GLAMT (SUB-1)
              COMPUTE CRJRN-GLDISC (SUB-1) =
                 (CRJRN-SETT-DISC - CRJRN-GLDISC (1)).
      *           (CRJRN-VAT-AMT * CR-SETT-DISC) / 100.
                 
           GO TO RWCR-010.
       RWCR-018.
           REWRITE CRJRN-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RWCR-019.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CRJRN REWRITE ERR, RWCR-018. TRANS NOT RE-WRITTEN"
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
              MOVE "CRJRN WRITE ERR, RWCR-019. TRANS NOT WRITTEN"
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRJRN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRJRN-ST1.
       RWCR-999.
           EXIT.
      *
       REWRITE-FOREIGN-CRJRN SECTION.
       RWCJ-002.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-CRTRANSNO TO CRJRN-TRANS.
           ADD 1               TO GLPA-CRTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
       RWCJ-005.
           IF GLPA-CURRENT-CRPER NOT = GLPA-CURRENT-SLPER
              MOVE "F"        TO CRJRN-FUTURE
           ELSE
              MOVE " "        TO CRJRN-FUTURE.

           START CRJRN-FILE KEY NOT < CRJRN-KEY.
           
           MOVE WS-MM-DESC (WS-MM) TO WS-JRN-REST
           MOVE 99                 TO WS-JRN-WEEK
           MOVE WS-JRN             TO CRJRN-REFERENCE.
           
           MOVE 1 TO SUB-20.
       RWCJ-006.
           IF FL-DNOTE (SUB-20) = " "
               GO TO RWCJ-999.
           MOVE WS-CURRENTPER       TO CRJRN-NO
           MOVE "N"                 TO CRJRN-COMPLETE
           PERFORM CHECK-DUE-DATE
           MOVE FL-DNOTE (SUB-20)   TO CRJRN-DNOTE-NO
                                       CRJRN-INV-NO
           MOVE WS-CREDITOR         TO CRJRN-CRACC-NUMBER
           MOVE FL-LOC-AMT (SUB-20) TO CRJRN-UNAPPLIED-AMT
           MOVE FL-LOC-AMT (SUB-20) TO CRJRN-LOC-AMT
           MOVE FL-FOR-AMT (SUB-20) TO CRJRN-FOR-AMT
           MOVE IMRE-EXCHANGERATE   TO CRJRN-EXCHANGE
           MOVE 0                   TO CRJRN-SETT-DISC.
           IF CR-SETT-DISC > 0
             COMPUTE CRJRN-SETT-DISC =
               (CRJRN-LOC-AMT * CR-SETT-DISC) / 100.
               
           MOVE 1 TO CRJRN-TYPE.
           MOVE 1 TO SUB-1.
       RWCJ-010.
           IF SUB-1 = 1
              MOVE "50-200-10-00"      TO CRJRN-GLACC (SUB-1)
              MOVE CRJRN-GLACC (SUB-1) TO GL-NUMBER
              PERFORM READ-GLMASTER
              MOVE FL-LOC-AMT (SUB-20) TO CRJRN-GLAMT (SUB-1)
              MOVE GL-DESCRIPTION      TO CRJRN-GLDESC (SUB-1)
              COMPUTE CRJRN-GLDISC (SUB-1) =
                 (CRJRN-GLAMT (SUB-1) * CR-SETT-DISC) / 100.
                 
           GO TO RWCJ-019.
       RWCJ-018.
           REWRITE CRJRN-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE "CRJRN ERR, ST=23 TRANS NOT WRITTEN."  TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRJRN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRJRN-ST1
               GO TO RWCJ-019.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 
           "CRJRN FOREIGN REWRITE ERR, RWCJ-018. NOT RE-WRITTEN."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRJRN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRJRN-ST1
               GO TO RWCJ-019.
           GO TO RWCJ-020.
       RWCJ-019.
           WRITE CRJRN-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE "CRJRN ERR, ST=2 TRANS NOT WRITTEN."  TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRJRN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRJRN-ST1
               GO TO RWCJ-018.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 
           "CRJRN FOREIGN WRITE ERR, RWCJ-019. TRANS NOT WRITTEN."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRJRN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRJRN-ST1
               GO TO RWCJ-018.
       RWCJ-020.
           ADD 1 TO SUB-20.
           IF SUB-20 NOT > 10
              PERFORM RWCJ-002
              GO TO RWCJ-006.
       RWCJ-999.
           EXIT.
      *
       READ-GLPARAMETER SECTION.
       RGLP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO PARAMETER RECORD ON FILE, 'ESC' TO EXIT."
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
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
               MOVE "GLPARAMETER BUSY ON READ RPL-010, 'ESC' TO RETRY."
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
               MOVE "GLPARAMETER BUSY REWP-000, 'ESC' TO RETRY."
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
       READ-INVQUES-FILE SECTION.
       RINVQUES-000.
            MOVE 1 TO PA-RECORD
            MOVE 6 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RINVQUES-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "N" TO INVQUES-STOCK-CHANGE
               GO TO RINVQUES-900.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY RINVQUES, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RINVQUES-010.
       RINVQUES-900.
           MOVE INVQUES-STOCK-CHANGE TO WS-STOCK-CHANGE.
       RINVQUES-999.
           EXIT.
      *
       WRITE-STOCK-CHANGES SECTION.
       WSTCH-000.
             MOVE ST-STOCKNUMBER TO STCH-STOCKNUMBER.
             START STOCKCHANGE-MASTER KEY NOT < STCH-KEY
               INVALID KEY NEXT SENTENCE.
       WSTCH-005.
             READ STOCKCHANGE-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STCHANGE-ST1 = 23 OR 35 OR 49
                GO TO WSTCH-006.
             IF WS-STCHANGE-ST1 NOT = 0
                MOVE "STOCKCHANGE BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCHANGE-ST1
               GO TO WSTCH-005.
       WSTCH-006.
          MOVE ST-DESCRIPTION1     TO STCH-DESCRIPTION1
          MOVE ST-DESCRIPTION2     TO STCH-DESCRIPTION2
          MOVE ST-CATEGORY         TO STCH-CATEGORY
          MOVE ST-SUPPLIER         TO STCH-SUPPLIER
          MOVE ST-FOREIGNCOST      TO STCH-FOREIGNCOST
          MOVE ST-PRICE            TO STCH-PRICE
          MOVE ST-UNITOFMEASURE    TO STCH-UNITOFMEASURE
          MOVE ST-DISCOUNT1        TO STCH-DISCOUNT1
          MOVE ST-DISCOUNT2        TO STCH-DISCOUNT2
          MOVE ST-DISCOUNT3        TO STCH-DISCOUNT3
          MOVE ST-DISCOUNT4        TO STCH-DISCOUNT4
          MOVE ST-DISCOUNT5        TO STCH-DISCOUNT5
          MOVE ST-DISCOUNT6        TO STCH-DISCOUNT6
          MOVE ST-DISCOUNT7        TO STCH-DISCOUNT7
          MOVE ST-DISCOUNT8        TO STCH-DISCOUNT8
          MOVE ST-DISCOUNT9        TO STCH-DISCOUNT9
          MOVE ST-MINBUYQTY        TO STCH-MINBUYQTY
          MOVE ST-ANALYSIS         TO STCH-ANALYSIS
          MOVE ST-DUTYPERCENT      TO STCH-DUTYPERCENT
          MOVE ST-DUTYTARIFF       TO STCH-DUTYTARIFF
          MOVE ST-SURCHARGE        TO STCH-SURCHARGE
          MOVE ST-PERMIT           TO STCH-PERMIT
          MOVE "C"                 TO STCH-TYPE-OF-CHANGE.
       WSTCH-010.
           IF WS-STCHANGE-ST1 = 23 OR 35 OR 49
              GO TO WSTCH-020.
           REWRITE STOCKCHANGE-RECORD
              INVALID KEY NEXT SENTENCE.
           IF WS-STCHANGE-ST1 NOT = 0
              MOVE "STOCKCHANGE BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCHANGE-ST1
              GO TO WSTCH-010.
          GO TO WSTCH-999.
       WSTCH-020.
          WRITE STOCKCHANGE-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STCHANGE-ST1 NOT = 0
              MOVE "STOCKCHANGE BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCHANGE-ST1
              GO TO WSTCH-020.
       WSTCH-999.
           EXIT.
      *
       READ-CURRENCY SECTION.
       R-CUR-000.
           MOVE IMRE-CURRENCY TO CU-KEY.
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
           MOVE CU-VALUE TO IMRE-EXCHANGERATE.
       R-CUR-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-001.
            OPEN I-O PARAMETER-FILE.
            IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-001.
           PERFORM READ-PARAMETER.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-002.
            OPEN I-O OUTSTANDING-ORDERS.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO WS-OUTORD-ST1
               MOVE "SORDERS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
       OPEN-003.
            OPEN I-O IMPRECEIPTS-FILE.
            IF WS-IMPRECEIPT-ST1 NOT = 0
               MOVE 0 TO WS-IMPRECEIPT-ST1
               MOVE "IMPORTS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-003.
       OPEN-005.
            OPEN I-O STOCKCHANGE-MASTER.
            IF WS-STCHANGE-ST1 NOT = 0
               MOVE 0 TO WS-STCHANGE-ST1
               MOVE "STCHANGE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
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
           MOVE " "                TO WS-FUTURE-BATCH
           PERFORM READ-GLPARAMETER
           MOVE GLPA-CURRENT-SLPER TO WS-CURRENTPER
           IF GLPA-CURRENT-CRPER NOT = GLPA-CURRENT-SLPER
              MOVE "F"        TO CRJRN-FUTURE.
       OPEN-040.
            OPEN I-O STKRECEIPTS-FILE.
            IF WS-STKRECEIPT-ST1 NOT = 0
               MOVE 0 TO WS-STKRECEIPT-ST1
               MOVE "ST-RECEIPTS BUSY ON OPEN, PRESS 'ESC' TO RETRY." 
                  TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-040.
       OPEN-041.
           PERFORM READ-INVQUES-FILE.
           CLOSE PARAMETER-FILE.
           PERFORM OPEN-001.
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
       OPEN-050.
            OPEN I-O CURRENCY-MASTER.
            IF WS-CURRENCY-ST1 NOT = 0
               MOVE 0 TO WS-CURRENCY-ST1
               MOVE "CURRENCY FILE BUSY ON OPEN, 'CANCEL TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-050.
       OPEN-060.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-950.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER
                 PARAMETER-FILE
                 OUTSTANDING-ORDERS
                 IMPRECEIPTS-FILE
                 STKRECEIPTS-FILE
                 CREDITOR-MASTER
                 GL-MASTER
                 CRJRN-FILE
                 CURRENCY-MASTER.
       END-900.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumeric6".
       Copy "WriteFieldNumDec".
       Copy "WriteFieldIndex".
       Copy "WriteFieldForeign99".
       Copy "WriteFieldForTotal".
       Copy "WriteFieldAmount".
       Copy "WriteFieldAmount1".
       Copy "WriteFieldAmountDis".
       Copy "WriteField99Mil".
       Copy "WriteFieldIndex1".
       Copy "WriteFieldPerc".
       Copy "WriteFieldQty".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      * Copy "ZoomBox".
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
      * END-OF-JOB.
