       IDENTIFICATION DIVISION.
       PROGRAM-ID. SlOrders.
       AUTHOR.  CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrDeliver".
         Copy "SelectStMaster".
         Copy "SelectStSpecPr".
         Copy "SelectSlMaster".
         Copy "SelectSlParameter".
         Copy "SelectStTrans".
         Copy "SelectSlRegister".
         Copy "SelectSlSbRep".
         Copy "SelectStDiscAcc".
         Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS WS-PRINT-STATUS.
           SELECT DOCUBASE-FILE ASSIGN TO WS-DOCUFILE
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdDrDelAddress.
           COPY ChlfdStock.
           COPY ChlfdStPrice.
           COPY ChlfdSales.
           COPY ChlfdParam.
           COPY ChlfdStTrans.
           COPY ChlfdRegister.
           COPY ChlfdSbRep.
           COPY ChlfdDaily.
           COPY ChlfdStDiscAcc.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(255).
      *
       FD  DOCUBASE-FILE.
       01  DB-REC.
           03  FILLER           PIC X(255).
      *
       WORKING-STORAGE SECTION.
       77  WS-DOCUFILE          PIC X(25) VALUE " ".
       77  WS-ACCNO-X           PIC X(7) VALUE " ".
       77  WS-DISCOUNT-CODE     PIC X VALUE " ".
       77  WS-NEWORDER          PIC X VALUE " ".
       77  WS-ORDER-COMPLETE    PIC X VALUE " ".
       77  WS-BORDERS-FOUND     PIC X VALUE " ".
       77  WS-LINECHANGED       PIC X VALUE " ".
       77  WS-SPECIAL-INFO      PIC X VALUE " ".
       77  WS-MUST-PRINT        PIC X VALUE " ".
       77  WS-ADD-TO-OLD-ORDER  PIC X VALUE " ".
       77  WS-AREA              PIC X VALUE " ".
       77  WS-NORM-PRINTER      PIC 99.
       77  WS-REPR-PRINTER      PIC 99.
       77  WS-RUSH-PRINTER      PIC 99.
       77  WS-DIS               PIC XX VALUE " ".
       77  WS-COST-DISPLAY      PIC X VALUE "N".
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-BO-NUMBER         PIC 9(6) VALUE 0.
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-WAS-SUSPENDED     PIC X VALUE " ".
       77  WS-PART-ORDERS       PIC X VALUE " ".
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-P-SLIP            PIC 9(6) VALUE 0.
       77  WS-QUOTE             PIC 9(6) VALUE 0.
       77  WS-REPAIR            PIC 9(6) VALUE 0.
       77  WS-POORDERNO         PIC X(20) VALUE " ".
       77  WS-SALESANALYSIS     PIC X(14) VALUE " ".
       77  WS-SALESANALYSIS-SAVE  PIC X(14) VALUE " ".
       77  WS-ANAL-CODE         PIC XX VALUE " ".
       77  WSAN-CODE-SAVE       PIC XX VALUE " ".
       77  WS-SOLD-BY           PIC XX VALUE " ".
       77  WS-SALESMAN          PIC X(15) VALUE " ".
       77  WS-DELIVERVIA        PIC X(20) VALUE " ".
       77  WS-TERMOFSALE        PIC X(11) VALUE " ".
       77  WS-BINNO             PIC X(6) VALUE " ".
       77  WS-GSTNO             PIC X(13) VALUE " ".
       77  WS-INVOICEDATE       PIC 9(8) VALUE 0.
       77  WS-TODAYS-DATE       PIC 9(8) VALUE 0.
       77  WS-COMMENTLINE       PIC X(30) VALUE " ".
       77  WS-STTR-ORDERQTY     PIC 9(5) VALUE 0.
       77  WS-STTR-SHIPQTY      PIC 9(5) VALUE 0.
       77  WS-ADDONFREIGHT      PIC 9(8)V99 VALUE 0.
       77  WS-POSTADDON         PIC 9(8)V99 VALUE 0.
       77  WS-HANDADDON         PIC 9(8)V99 VALUE 0.
       77  WS-MISCADDON         PIC 9(8)V99 VALUE 0.
       77  WS-SUBTOTAL          PIC 9(8)V99 VALUE 0.
       77  WS-ADDONAMT          PIC 9(8)V99 VALUE 0.
       77  WS-TAXAMT            PIC 9(8)V99 VALUE 0.
       77  WS-INVOICETOTAL      PIC 9(8)V99 VALUE 0.
       77  WS-TAXABLETOTAL      PIC 9(8)V99 VALUE 0.
       77  WS-NONTAXABLETOTAL   PIC 9(8)V99 VALUE 0.
       77  WS-ORDERTOTAL           PIC 9(8)V99 VALUE 0.
       77  WS-QUES-ACC-OVER-LIMIT  PIC X VALUE " ".
       77  WS-LIMIT-EXCEP-WRITE    PIC X VALUE " ".
       77  WS-WORKTOTAL         PIC 9(8)V99 VALUE 0.
       77  WS-WORKTOTAL1        PIC 9(8)V99 VALUE 0.
       77  WS-WORKTOTAL2        PIC 9(9)V99 VALUE 0.
       77  WS-COSTTOTAL         PIC 9(8)V99 VALUE 0.
       77  WS-COSTTOTAL1        PIC 9(8)V99 VALUE 0.
       77  WS-PRICETOTAL        PIC 9(8)V99 VALUE 0.
       77  WS-PRICETOTAL1       PIC 9(8)V99 VALUE 0.
       77  WS-EXPORTTOTAL       PIC 9(8)V99 VALUE 0.
       77  WS-DISCOUNTREG       PIC 9(8)V99 VALUE 0.
       77  WS-DISCOUNTREG1      PIC 9(8)V99 VALUE 0.
       77  WS-DISCOUNT          PIC 9(8)V99 VALUE 0.
       77  WS-DISCOUNT1         PIC 9(8)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(8)V99 VALUE 0.
       77  WS-NEWPRICE          PIC S9(8)V99 VALUE 0.
       77  WS-PRICESAVE         PIC S9(8)V99 VALUE 0.
       77  WS-DISCOUNTSAVE      PIC S9(8)V99 VALUE 0.
       77  WS-PERC              PIC S9(4)V99 VALUE 0.
       77  WS-ITEMS-BELOW-MIN-PERC   PIC X VALUE " ".
       77  WS-GST-PERCENT       PIC 99V99 VALUE 0.
       77  WS-BODY-LINE         PIC ZZ9.
       77  WS-IMM-PR            PIC X VALUE " ".
       77  WS-PAGE              PIC 99 VALUE 0.
       77  WS-DEBTOR-INQUIRY      PIC X(8) VALUE "DrNameIq".
       77  WS-STOCK-INQUIRY       PIC X(8) VALUE "StMastIq".
       77  WS-PORDER-INQUIRY      PIC X(8) VALUE "SlPoShIq".
       77  WS-QUOTE-ACC-INQUIRY   PIC X(8) VALUE "SlQuAcIq".
       77  WS-QUOTE-STK-INQUIRY   PIC X(8) VALUE "SlQuStIq".
       77  WS-QUES-MU-GP-PERC     PIC X VALUE " ".
       77  WS-QUES-CHECK-QUOTES   PIC X VALUE " ".
       77  WS-QUES-PAUSE-ON-PSLIP PIC X VALUE " ".
       77  WS-ZERODIS             PIC X VALUE " ".
       77  WS-PERCENT           PIC 9(2)V99 VALUE 0.
       77  WS-INVOICEDISCOUNT   PIC 9(2)V99 VALUE 0.
       77  WS-BO-QTY            PIC S9(5) VALUE 0.
       77  WS-AVERAGECOST       PIC 9(7)V99 VALUE 0.
       77  WS-AVE-COST-OF-ALLOC-STOCK PIC 9(7)V99 VALUE 0.
       77  WS-AVE-COST-OF-STOCK PIC 9(7)V99 VALUE 0.
       77  PSW-SUB1             PIC S9(5) VALUE 0.
       77  PSW-SUB2             PIC S9(5) VALUE 0.
       77  WS-BEFORE            PIC 9(3) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  CALC-FIELD           PIC 9(7)V99 VALUE 0.
       77  WS-TERM-SUB          PIC 9 VALUE 0.    
       77  WS-DEL-SUB           PIC 9 VALUE 0.    
       77  WSAN-CODE-3          PIC XX VALUE "  ".
       77  HAVE-FOUND-BO        PIC XXX VALUE "   ".
       77  WS-BOFOUND           PIC X VALUE " ".
       77  WS-STTRANS-NO        PIC 9(6).
       77  WS-DRTRANS-NO        PIC 9(6).
       77  WS-DR-DISC           PIC 9(2)V99 VALUE 0.
       77  WS-READS             PIC 99.
       77  WS-REPRINT           PIC X VALUE " ".
       77  WS-ERR               PIC XXX VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-PasswordSaved     Pic X(10).
       01  WTELL-PAUSE.
           03 WTELL-P1          PIC X(3) VALUE X"FF0305".
           03 WTELL-P2          PIC X(5) VALUE "HELLO".
       01  W-READ-KEY           PIC X(20).
       01  WS-STDESC.
           03  WS-DESC1          PIC X(20) VALUE " ".
           03  WS-DESC2          PIC X(20) VALUE " ".
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1      PIC 99.
       01  WS-DRDEL-STATUS.
           03  WS-DRDEL-ST1       PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-STPR-STATUS.
           03  WS-STPR-ST1        PIC 99.
       01  WS-SALES-STATUS.
           03  WS-SALES-ST1       PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1     PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1        PIC 99.
       01  WS-SBREP-STATUS.
           03  WS-SBREP-ST1       PIC 99.
       01  WS-STDISC-STATUS.
           03  WS-STDISC-ST1      PIC 99.
       01  WS-PRINT-STATUS.
           03  WS-PRINT-ST1       PIC 99.
       01 WS-PRINT-MESSAGE.
           03  FILLER         PIC X(8) VALUE "STATUS:".
           03  WS-PM-ST1      PIC X(8) VALUE " ".
           03  FILLER         PIC X(8) VALUE "NUMBER:".
           03  WS-PM-NUM      PIC X(8) VALUE " ".
           03  FILLER         PIC X(13) VALUE "PRINTERNAME:".
           03  WS-PM-NAME     PIC X(12) VALUE " ".
       01  WS-PASSWORD-KEY.
           03  WS-PA-KEY         PIC X OCCURS 11.
       01  WS-NAMEANDADDRESS.
           03  WS-NAME          PIC X(40) VALUE " ".
           03  WS-ADD1          PIC X(25) VALUE " ".
           03  WS-ADD2          PIC X(25) VALUE " ".
           03  WS-ADD3          PIC X(25) VALUE " ".
           03  WS-POSTCODE      PIC 9(4).
           03  WS-DELADD1       PIC X(25) VALUE " ".
           03  WS-DELADD2       PIC X(25) VALUE " ".
           03  WS-DELADD3       PIC X(25) VALUE " ".
           03  WS-PHONE         PIC X(20) VALUE " ".
           03  WS-CONTACT       PIC X(20) VALUE " ".
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X VALUE " ".
           03  SP-REST          PIC X(14) VALUE " ".
       01  SPLIT-ANALYSIS.
           03  WSAN-CODE.
               05  WSAN-CODE-1  PIC X VALUE " ".
               05  WSAN-CODE-2  PIC X VALUE " ".
           03  WSAN-REST        PIC X(12) VALUE " ".
       01  ALPHABET-FIELD.
           03  ALPHA-FIELD      PIC X.
           88  ALPHA-VALUE      VALUES ARE "A" THRU "Z".
       01  NUMBERS-FIELD.
           03  NUMERIC-FIELD    PIC X.
           88  NUMERIC-VALUE    VALUES ARE "0" THRU "9".
       01  SPLIT-DELIVERVIA.
           03  WSDE-CODE        PIC X VALUE " ".
           03  WSDE-REST        PIC X(19) VALUE " ".
       01  SPLIT-TERMOFSALE.
           03  WSTE-CODE        PIC X VALUE " ".
           03  WSTE-REST        PIC X(10) VALUE " ".
       01  STORE-TERM.
         02  WS-TERM-OCCUR OCCURS 10.
           03  WS-ST-TYPE       PIC X.
           03  WS-ST-CODE       PIC X.
           03  WS-ST-TERM       PIC X(11).
       01  STORE-DEL.
         02  WS-DEL-OCCUR OCCURS 10.
           03  WS-DEL-TYPE       PIC X.
           03  WS-DEL-CODE       PIC X.
           03  WS-DEL-TERM       PIC X(20).
       01  WS-BO-MESSAGE.
           03  FILLER           PIC X(7) VALUE " ".
           03  WS-BONUMBER      PIC Z(5)9.
           03  FILLER           PIC X(30) VALUE
              " LINE ITEMS ARE ON B-ORDER.".
           03  FILLER           PIC X(7) VALUE " ".
       01  WS-BO-INVOICE-MESSAGE.
           03  WS-BO-INVOICE    PIC Z(5)9.
           03  WS-BO-DATE       PIC 9(8).
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 201.
               05  B-STOCKNUMBER.
                   07  B-1ST-CHAR      PIC X.
                   07  B-REST          PIC X(14).
               05  B-NEWLINE           PIC X.
               05  B-STTRANS           PIC 9(6).
               05  B-INVOICED          PIC 9(6).
               05  B-PULL              PIC X.
               05  B-REMAINDER.
                   07  B-ORDERQTY          PIC 9(5).
                   07  B-SHIPQTY           PIC 9(5).
                   07  B-SHIPPEDQTY        PIC 9(5).
                   07  B-STOCKDESCRIPTION  PIC X(20).
                   07  B-STOCKDESCRIPTION2 PIC X(20).
                   07  B-STOCKPRICE        PIC 9(8)V99.
                   07  B-STOCKCOST         PIC 9(8)V99.
                   07  B-DISCOUNTPERITEM   PIC 9(2)V99.
                   07  B-TAX               PIC X.
                   07  B-NETT              PIC 9(8)V99.
                   07  B-UNIT              PIC X(4).
                   07  B-STORE             PIC X(5).
                   07  B-MAX-DISC          PIC 9(2)V99.
                   07  B-SPECIAL           PIC X.
                   07  B-MIN-PERC          PIC 9(3)V99.
                   07  B-REPAIR            PIC X.
               05  C-LINE REDEFINES B-REMAINDER.
                   07  C-ORDER            PIC X(5).
                   07  C-SHIP             PIC X(5).
                   07  C-DESC             PIC X(20).
                   07  C-UNIT             PIC X(4).
                   07  C-PRICE            PIC X(9).
                   07  C-COST             PIC X(9).
                   07  C-DISC             PIC X(5).
       01  WS-BODY-LINES.
           03  WS-STOCKNUMBER      PIC X(15).
       01  WS-QUOTE-FOUND-MESSAGE.
           03  WS-FIL1     PIC X(6) VALUE " ".
           03  WS-QU-NUM   PIC Z(5)9.
           03  WS-FIL2     PIC X(6) VALUE " ".
           03  WS-QU-DATE  PIC X(10) VALUE " ".
           03  WS-FIL3     PIC X(5) VALUE " ".
           03  WS-QU-QTY   PIC Z(4)9.
           03  WS-FIL4     PIC X(9) VALUE " ".
           03  WS-QU-PRICE PIC Z(5)9.99.
           03  WS-FIL5     PIC X(8) VALUE " ".
           03  WS-QU-DISC  PIC Z9.99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST.
               05  WS-DAILY-1ST1   PIC X(9) VALUE " ".
               05  WS-DAILY-1ST2   PIC X(4) VALUE " ".
               05  WS-DAILY-1ST3   PIC X(7) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD.
               05  WS-DAILY-3RD1   PIC X(10) VALUE " ".
               05  WS-DAILY-3RD2   PIC X(10) VALUE " ".
           03  WS-DAILY-4TH.
               05  WS-DAILY-4TH1   PIC X(10) VALUE " ".
               05  WS-DAILY-4TH2   PIC X(10) VALUE " ".
       01  WS-REPORT-DATE-STRIP.
           03  WS-STRIP1          PIC X(4).
           03  WS-STRIP2          PIC X(18).
           03  WS-STRIP3          PIC X(3).
       01  WS-TIME-DISPLAY.
           03  SPLIT-TIME-FIL     PIC X(14).
           03  SPLIT-HR           PIC 99.
           03  SPLIT-HR-FIL       PIC X.
           03  SPLIT-MN           PIC 99.
           03  SPLIT-MN-FIL       PIC X.
           03  SPLIT-SC           PIC 99.
       01  WS-ONHAND-LINE.
           03  FILLER-ONHAND          PIC X(8).
           03  WS-QTYONHAND           PIC Z(4)9.
           03  FILLER                 PIC X(5) VALUE " ".
           03  FILLER-ONRES           PIC X(7).
           03  WS-QTYONRESERVE        PIC Z(4)9.
           03  FILLER                 PIC X(5) VALUE " ".
           03  FILLER-ONBO            PIC X(7).
           03  WS-QTYONBORDER         PIC Z(4)9.
           03  FILLER                 PIC X(5) VALUE " ".
           03  FILLER-ONORD           PIC X(9).
           03  WS-QTYONORDER          PIC Z(4)9.
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
           
       01  PLINE1.
           03  FILLER           PIC X(17) VALUE "PICKING SLIP NO:".
           03  P-DIG1           PIC X.
           03  P-SLPRINT-RECORD.
              05  P-SLIP           PIC Z(5)9.
              05  FILLER           PIC X VALUE "-".
              05  P-PRINTNUMBER    PIC 99.
           03  P-DIG2           PIC X.
           03  FILLER           PIC X(3) VALUE " ".
           03  FILLER           PIC X(9) VALUE "ACCOUNT:".
           03  P-DIG3           PIC X.
           03  P-ACCNO          PIC X(10).
           03  P-DIG4           PIC X.
           03  FILLER           PIC X(8) VALUE "VAT NO:".
           03  P-GSTNO          PIC X(39) VALUE " ".
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  P-PAGE           PIC Z9.
           03  FILLER           PIC X(65) VALUE " ".
       01  PLINE2.
           03  P-ADD            PIC X(45) VALUE " ".
           03  P-DEL            PIC X(30) VALUE " ".
           03  P-NAME           PIC X(10) VALUE " ".
           03  P-CONTACT        PIC X(47) VALUE " ".
       01  PLINE3.
           03  P-CODE           PIC 9(4) BLANK WHEN ZERO.
           03  FILLER           PIC X(29) VALUE " ".
           03  P2-DIG1          PIC X.
           03  P2-COMM          PIC X VALUE " ".
           03  P2-DIG2          PIC X.
           03  FILLER           PIC X(10) VALUE " ".
           03  P-TIME           PIC X(81) VALUE " ".
       01  PLINE4-1.
           03  FILLER           PIC X(7) VALUE "*******".
           03  FILLER           PIC X(52) VALUE
           "***TERMS**** ****PURCHASE ORDER*** *SALES* *****SHIP".
           03  FILLER           PIC X(53) VALUE
           " VIA******** **BIN** *REP* **ORDER DATE** **SLIP NO*".
           03  FILLER           PIC X(18) VALUE "*TODAYS DATE******".
       01  PLINE4.
           03  FILLER           PIC X(7) VALUE " ".
           03  P-TERMS          PIC X(11) VALUE " ".
           03  FILLER           PIC X(3) VALUE " ".
           03  P-PO             PIC X(20) VALUE " ".
           03  FILLER           PIC X(4) VALUE " ".
           03  P-SOLD           PIC X(2) VALUE " ".
           03  FILLER           PIC X(4) VALUE " ".
           03  P-VIA            PIC X(20) VALUE " ".
           03  FILLER           PIC X(2) VALUE " ".
           03  P-BIN            PIC X(6) VALUE " ".
           03  FILLER           PIC X(3) VALUE " ".
           03  P-SOLDBY         PIC X(2) VALUE " ".
           03  FILLER           PIC X(4) VALUE " ".
           03  P-DATE           PIC X(10).
           03  FILLER           PIC X(5) VALUE " ".
           03  P-INV            PIC Z(5)9.
           03  FILLER           PIC X(4) VALUE " ".
           03  P-TODAY          PIC X(10).
           03  FILLER           PIC X(9) VALUE " ".
       01  PDET-1.
           03  FILLER           PIC X(41) VALUE
           "*LN**STOCK NUMBER***DESCRIPTION**********".
           03  FILLER           PIC X(20) VALUE ALL "*".
           03  FILLER           PIC X(5) VALUE "UNIT*".
           03  FILLER           PIC X(52) VALUE
           "ORDER**SHIP**SENT***B/O******PRICE*******COST**DISC*".
           03  FILLER           PIC X(27) VALUE
           "*******NETT*TX*STORE*INV NO".
       01  PDET.
           03  P-NO             PIC Z(3).
           03  FILLER           PIC X(1) VALUE " ".
           03  P-STOCK          PIC X(15) VALUE " ".
           03  PDET-REST.
               05  FILLER       PIC X(1) VALUE " ".
               05  P-DESC       PIC X(20) VALUE " ".
               05  P-DESC2      PIC X(20) VALUE " ".
               05  FILLER       PIC X(1) VALUE " ".
               05  P-UNIT       PIC X(5) VALUE " ".
               05  P-ORDER          PIC Z(4)9.
               05  P-ITEM-BELOW-MU  PIC X(1) VALUE " ".
               05  P-SHIPPED.
                   07  P-SHIP       PIC Z(4)9.
                   07  P-PULL       PIC X(1) VALUE " ".
                   07  P-TOTSHIPPED PIC Z(4)9.
                   07  FILLER       PIC X(1) VALUE " ".
                   07  P-BO         PIC Z(4)9.
               05  P-PRICE      PIC Z(7)9.99.
      *         05  FILLER       PIC X(1) VALUE " ".
               05  P-COST       PIC Z(7)9.99.
               05  FILLER       PIC X(1) VALUE " ".
               05  P-DISCOUNT   PIC Z9.99.
               05  P-PERC       PIC X(1).
               05  P-NETT       PIC Z(7)9.99.
               05  FILLER       PIC X(2) VALUE " ".
               05  P-TAX        PIC X(2) VALUE " ".
               05  P-STORE      PIC X(6) VALUE " ".
               05  P-INVOICED   PIC Z(5)9 BLANK WHEN ZERO.
       01  P-COMMENTLINE.
           03  FILLER           PIC X(10) VALUE " ".
           03  P-BO-MESSAGE     PIC X(38) VALUE " ".
           03  P-BO-INVOICE     PIC Z(5)9 BLANK WHEN ZERO.
           03  FILLER           PIC X VALUE " ".
           03  P-BO-DATE        PIC X(10) VALUE " ".
           03  FILLER           PIC X(67) VALUE " ".
       01  P-ADDLINE.
           03  P-DES1           PIC X(12) VALUE " ".
           03  P-SALESMAN.
              05  P-ADD1        PIC Z(7)9.99. 
              05  P-DES2        PIC X(11) VALUE " ".
           03  P-ADD2           PIC Z(7)9.99.
           03  P-DES3           PIC X(11) VALUE " ".
           03  P-ADD3           PIC Z(7)9.99-.
           03  P-ADDDESC        PIC X(27) VALUE " ".
           03  P-DES4           PIC X(11) VALUE " ".
           03  P-ADD4           PIC Z(7)9.99-.
           03  FILLER           PIC X(19) VALUE " ".
       01  P-CONTINUED.
           03  FILLER           PIC X(40) VALUE " ".
           03  FILLER           PIC X(22) VALUE "Continued To.....Page".
           03  P-CONTROL-PAGE   PIC 9.
           03  FILLER           PIC X(69) VALUE " ".
       01  PLINE-PERC.
           03  FILLER           PIC X(10) VALUE " ".
           03  PP2-DIG1         PIC X.
           03  PP2-COMM         PIC X(49) VALUE " ".
           03  PP2-PERC         PIC Z(3)9.99-.
           03  PP2-DIG2         PIC X.
           03  FILLER           PIC X(10) VALUE " ".
       01 P-ALLOCATE-LINE.
           03  P-AL-DIG1          PIC X.
           03  P-AL-COMMENT       PIC X(40).
           03  P-AL-DIG2          PIC X.
       01 P-QUOTE-LINE.
           03  FILLER             PIC X(2) VALUE " ".
           03  P-QU-COMMENT       PIC X(42) VALUE
            "THIS PICKING SLIP IS A CONVERTED QUOTE No:" .
           03  P-QU-DIG1          PIC X.
           03  P-QU-NUMBER        PIC Z(5)9.
           03  P-QU-DIG2          PIC X.
       01 P-REPAIR-LINE.
           03  FILLER             PIC X(2) VALUE " ".
           03  P-RP-COMMENT       PIC X(43) VALUE
            "THIS PICKING SLIP IS COPIED FROM REPAIR No:" .
           03  P-RP-DIG1          PIC X.
           03  P-RP-NUMBER        PIC Z(5)9.
           03  P-RP-DIG2          PIC X.
       Copy "WsDateInfo".
      *
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
           03  F-NAMEFIELDRED.
               05  F-NAMEFIELDRED1 PIC X.
               05  F-NAMEFIELDRED7 PIC X(6).
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-020
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN
           PERFORM CLEAR-FIELDS
           MOVE "ON-HAND:"  TO FILLER-ONHAND
           MOVE "ON-RES:"   TO FILLER-ONRES
           MOVE "ON-B/O:"   TO FILLER-ONBO
           MOVE "ON-ORDER:" TO FILLER-ONORD.
           Move 3 To Ws-PrinterNumber (21)
           Move 9 To Ws-PrinterType (21).
           Copy "PrinterSpecial".
       CONTROL-010.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           IF WS-ABOVE-BODY = "1"
              GO TO CONTROL-010.
           IF F-EXIT-CH = X"17"
              GO TO CONTROL-010.
           PERFORM PRINT-PSLIP
           PERFORM WRITE-DOCUBASE-RECORD
           GO TO CONTROL-010.
       CONTROL-999.
           EXIT.
      *
       CHECK-TO-ENTER-ORDER SECTION.
       CTEO-000.
           IF WS-ABOVE-BODY = "1"
                 GO TO CTEO-999.
           PERFORM CLEAR-010.
           MOVE "N" TO WS-ACCEPT.
           MOVE 2910 TO POS
           DISPLAY "DO YOU WISH TO CONTINUE WITH THIS ORDER: [ ]"
              AT POS
           ADD 42 TO POS

              MOVE 'N'       TO CDA-DATA
              MOVE 1         TO CDA-DATALEN
              MOVE 26        TO CDA-ROW
              MOVE 51        TO CDA-COL
              MOVE CDA-WHITE TO CDA-COLOR
              MOVE 'F'       TO CDA-ATTR
              PERFORM CTOS-ACCEPT
              MOVE CDA-DATA TO WS-ACCEPT
           
      *     ACCEPT WS-ACCEPT AT POS.
           IF WS-ACCEPT NOT = "Y" AND NOT = "N"
              DISPLAY " " AT 3079 WITH BELL
              GO TO CTEO-000.
           IF WS-ACCEPT = "N"
              GO TO CTEO-999.
       CTEO-100.
           IF F-NAMEFIELDRED1 NOT = "Q" AND NOT = "R"
               GO TO CTEO-300.
       CTEO-200.
           MOVE " " TO WS-ADD-TO-OLD-ORDER
           MOVE 3010 TO POS
           IF F-NAMEFIELDRED1 = "Q"
           DISPLAY
             "Do you wish to ADD this QUOTE to a PREVIOUS P/SLIP? [ ]"
               AT POS
           ELSE
           DISPLAY
             "Do you wish to ADD this REPAIR to a PREVIOUS P/SLIP?[ ]"
               AT POS.
           ADD 53 TO POS

              MOVE 'N'       TO CDA-DATA
              MOVE 1         TO CDA-DATALEN
              MOVE 27        TO CDA-ROW
              MOVE 62        TO CDA-COL
              MOVE CDA-WHITE TO CDA-COLOR
              MOVE 'F'       TO CDA-ATTR
              PERFORM CTOS-ACCEPT
              MOVE CDA-DATA TO WS-ADD-TO-OLD-ORDER
           
      *     ACCEPT WS-ADD-TO-OLD-ORDER AT POS.
           IF WS-ADD-TO-OLD-ORDER = "N"
              GO TO CTEO-300.
           IF WS-ADD-TO-OLD-ORDER NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO CTEO-200.
           MOVE 2910 TO POS
           DISPLAY
           "Enter the PICKING SLIP # you want this to be ADDED TO."
               AT POS.
       CTEO-210.
           PERFORM ERROR-020
           MOVE "                        " TO F-NAMEFIELD
           MOVE "INVOICENUM" TO F-FIELDNAME
           MOVE 10           TO F-CBFIELDNAME
           MOVE 6            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA
           MOVE 6            TO F-CBFIELDLENGTH
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
               GO TO CTEO-200.
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD  TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-INVOICE
           MOVE WS-INVOICE   TO F-EDNAMEFIELDNUM P-SLIP WS-P-SLIP
           MOVE 6            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-NUMERIC.
           IF WS-INVOICE NOT > 0
             MOVE "THIS FIELD CANNOT BE BLANK OR ZERO, RE-ENTER."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO CTEO-210.
           PERFORM CHECK-IF-ORDER-VALID.
           IF WS-INCR-ST1 = 88
               GO TO CTEO-200.
           MOVE X"19" TO F-EXIT-CH
           PERFORM ERROR1-020
           MOVE WS-DATE TO WS-INVOICEDATE.
           GO TO CTEO-500.
       CTEO-300.
           PERFORM READ-PARAMETER-LOCK
           MOVE PA-ORDER-NUMBER TO WS-INVOICE P-SLIP WS-P-SLIP.
       CTEO-400.
           COMPUTE PA-ORDER-NUMBER = PA-ORDER-NUMBER + 1.
           PERFORM REWRITE-PARAMETER
           PERFORM READ-PARAMETER-LOCK
           IF WS-INVOICE = PA-ORDER-NUMBER
              GO TO CTEO-300.
           PERFORM REWRITE-PARAMETER.
       CTEO-500.
           MOVE "INVOICENUM" TO F-FIELDNAME
           MOVE 10           TO F-CBFIELDNAME
           MOVE WS-INVOICE   TO F-EDNAMEFIELDNUM
           MOVE 6            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-NUMERIC.
       CTEO-600.
           IF WS-ADD-TO-OLD-ORDER NOT = "Y"
              MOVE "S" TO INCR-PRINTED
              PERFORM WRIC-050
              PERFORM WRIC-055 
              PERFORM WRIC-061
              PERFORM ERROR-020
              PERFORM ERROR1-020
              MOVE 0 TO INCR-COPY-NUMBER.
       CTEO-999.
           EXIT.
      *
       WRITE-DOCUBASE-RECORD SECTION.
       WDBS-001.
          MOVE 2910 TO POS
          DISPLAY "WRITING DOCUBASE RECORD....." AT POS.
          MOVE " " TO WS-DOCUFILE ALPHA-RATE DATA-RATE.
          IF F-EXIT-CH = X"1F"
            IF WS-DIS = "Y"
              MOVE WS-INVOICE    TO P-SLIP
              ADD 1              TO INCR-COPY-NUMBER.
          MOVE INCR-COPY-NUMBER  TO P-PRINTNUMBER.
           
          MOVE "/ctools/ps"      TO ALPHA-RATE
          MOVE WS-CO-NUMBER      TO WS-COMPANY-DIGITS
          MOVE WS-CO-DIG1        TO AL-RATE (11)
          MOVE WS-CO-DIG2        TO AL-RATE (12)
          MOVE "/"               TO AL-RATE (13).
           
      *     MOVE WS-CO-NUMBER     TO AL-RATE (9)
          MOVE P-SLPRINT-RECORD TO DATA-RATE
          MOVE 1  TO SUB-1
          MOVE 14 TO SUB-2.
       WDBS-002.
          IF DAT-RATE (SUB-1) = " "
              ADD 1 TO SUB-1
              GO TO WDBS-002.
       WDBS-003.
           IF SUB-1 > 40
               GO TO WDBS-004.
           MOVE DAT-RATE (SUB-1) TO AL-RATE (SUB-2)
           ADD 1 TO SUB-1 SUB-2
           GO TO WDBS-003.
       WDBS-004.
           MOVE ALPHA-RATE TO WS-DOCUFILE.
           PERFORM PRINT-DOCUBASE-FILE.
           PERFORM ERROR1-020.
       WDBS-999.
           EXIT.
      *
       PRINT-DOCUBASE-FILE SECTION.
       DB-008.
           MOVE " " TO WS-ITEMS-BELOW-MIN-PERC
           MOVE 1 TO WS-PAGE SUB-1 SUB-2
           MOVE " " TO DB-REC
           OPEN OUTPUT DOCUBASE-FILE.
       DB-010.
           MOVE WS-PRINT-COMP TO DB-REC
               WRITE DB-REC
               MOVE " " TO DB-REC.
           
           IF WS-PAGE > 1
               MOVE " " TO DB-REC
               WRITE DB-REC
               MOVE WS-PAGE TO P-CONTROL-PAGE
               WRITE DB-REC FROM P-CONTINUED
               MOVE " " TO DB-REC
               WRITE DB-REC BEFORE PAGE.
       DB-012.
           WRITE DB-REC FROM COMPANY-LINE.
           IF WS-REPRINT = "Y"
               MOVE " " TO DB-REC
               MOVE WS-PRINT-BOLD TO P-AL-DIG1
               MOVE "** THIS IS A REPRINTED ORDER **" TO P-AL-COMMENT
               MOVE WS-PRINT-UNBOLD TO P-AL-DIG2
               WRITE DB-REC FROM P-ALLOCATE-LINE
               MOVE WS-PRINT-COMP TO DB-REC
               WRITE DB-REC
               MOVE " " TO DB-REC
               WRITE DB-REC.
           IF F-EXIT-CH = X"1F"
            IF WS-DIS = "Y"
               MOVE " " TO DB-REC
               MOVE WS-PRINT-BOLD   TO P-AL-DIG1
               MOVE "** THIS ORDER HAS BEEN REVERSED **" TO P-AL-COMMENT
               MOVE WS-PRINT-UNBOLD TO P-AL-DIG2
               WRITE DB-REC FROM P-ALLOCATE-LINE
               MOVE WS-PRINT-COMP   TO DB-REC
               WRITE DB-REC
               MOVE " " TO DB-REC
               WRITE DB-REC.
           IF P-SLIP = 0
              MOVE WS-INVOICE     TO P-SLIP.
           MOVE WS-PRINT-BOLD     TO P-DIG1 P-DIG3 P-QU-DIG1
           MOVE WS-PRINT-UNBOLD   TO P-DIG2 P-DIG4 P-QU-DIG2.
           MOVE WS-ACCOUNT-NUMBER TO P-ACCNO
           MOVE WS-GSTNO          TO P-GSTNO
           MOVE WS-PAGE           TO P-PAGE
           MOVE INCR-COPY-NUMBER  TO P-PRINTNUMBER
           WRITE DB-REC FROM PLINE1.
           MOVE " " TO DB-REC
           MOVE WS-PRINT-COMP TO DB-REC
           WRITE DB-REC
           MOVE " " TO DB-REC.
           MOVE WS-NAME     TO P-ADD
           MOVE "PHONE NO:" TO P-NAME
           MOVE WS-PHONE    TO P-CONTACT
           WRITE DB-REC FROM PLINE2.
           MOVE " "         TO DB-REC PLINE2
           MOVE WS-ADD1     TO P-ADD
           MOVE WS-DELADD1  TO P-DEL
           MOVE "CONTACT :" TO P-NAME
           MOVE WS-CONTACT  TO P-CONTACT
           WRITE DB-REC FROM PLINE2.
           MOVE " "         TO DB-REC PLINE2
           MOVE WS-ADD2     TO P-ADD
           MOVE WS-DELADD2  TO P-DEL
           WRITE DB-REC FROM PLINE2.
           MOVE " "         TO DB-REC PLINE2
           MOVE WS-ADD3     TO P-ADD
           MOVE WS-DELADD3  TO P-DEL
           WRITE DB-REC FROM PLINE2.
           MOVE " "             TO DB-REC PLINE2
           MOVE WS-POSTCODE     TO P-CODE
           MOVE WS-PRINT-BOLD   TO P2-DIG1
           MOVE INCR-AREA       TO P2-COMM
           MOVE WS-PRINT-UNBOLD TO P2-DIG2

           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR TO SPLIT-HR
           MOVE ":" TO SPLIT-HR-FIL SPLIT-MN-FIL
           MOVE WS-MIN TO SPLIT-MN
           MOVE WS-SEC TO SPLIT-SC
           MOVE "Time Printed:" TO SPLIT-TIME-FIL
           MOVE WS-TIME-DISPLAY TO P-TIME.
           WRITE DB-REC FROM PLINE3.

           MOVE " "         TO DB-REC PLINE3
           WRITE DB-REC
           WRITE DB-REC FROM PLINE4-1.
           MOVE WS-TERMOFSALE  TO P-TERMS
           MOVE WS-POORDERNO   TO P-PO
           MOVE WSAN-CODE      TO P-SOLD.
           IF INCR-PRINTED = "R"
              MOVE "*RUSH-JOB, PULL NOW*" TO P-VIA
           ELSE
              MOVE WS-DELIVERVIA          TO P-VIA.
           MOVE WS-BINNO       TO P-BIN
           MOVE WS-SOLD-BY     TO P-SOLDBY.
           IF F-NAMEFIELDRED1 = "Q" OR = "R"
              MOVE WS-DATE     TO WS-INVOICEDATE.
           MOVE WS-INVOICEDATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE   TO P-DATE
           MOVE WS-INVOICE     TO P-INV
           MOVE WS-DATE        TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE   TO P-TODAY
           WRITE DB-REC FROM PLINE4
           MOVE " "            TO DB-REC PLINE4
           WRITE DB-REC
           WRITE DB-REC FROM PDET-1
           MOVE " "            TO DB-REC
           WRITE DB-REC.
       DB-020.
           IF SUB-1 < 199
             IF SUB-1 = SUB-20
               SUBTRACT 1 FROM SUB-2
               GO TO DB-030.
           IF SUB-2 > 40
               MOVE 1 TO SUB-2
               ADD 1 TO WS-PAGE
               GO TO DB-010.
           IF SUB-1 > 200
              GO TO DB-030.
           IF B-STOCKNUMBER (SUB-1) = " "
              AND B-ORDERQTY (SUB-1) = 0
               GO TO DB-030.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               MOVE B-STOCKNUMBER (SUB-1) TO P-STOCK
               MOVE C-LINE (SUB-1) TO PDET-REST
               GO TO DB-025.
           MOVE B-STOCKNUMBER (SUB-1)       TO P-STOCK
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO P-DESC
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO P-DESC2
           MOVE B-ORDERQTY (SUB-1)          TO P-ORDER.

      * NEW SECTION TO FLAG ITEMS THAT ARE BELOW MIN MU PERC
      * ST-MIN-PERC ADDED TO CHLFDSTOCK 17/12/2001
      * ONLY PRINTS FLAG IF SHIPPED-QTY < ORDER-QTY
           IF B-NEWLINE (SUB-1) = "Y" OR = "L" OR = "R"
              GO TO DB-022.
           IF WS-QUES-MU-GP-PERC = "N"
           COMPUTE WS-PERC = 
           (((B-STOCKPRICE (SUB-1) - 
           (B-STOCKPRICE (SUB-1) * B-DISCOUNTPERITEM (SUB-1) / 100))
           - B-STOCKCOST (SUB-1)) / B-STOCKCOST (SUB-1)) * 100.
           IF WS-PERC < B-MIN-PERC (SUB-1)
              MOVE "Y"                      TO WS-ITEMS-BELOW-MIN-PERC
              MOVE "$"                      TO P-ITEM-BELOW-MU
           ELSE
              MOVE " "                      TO P-ITEM-BELOW-MU.
              
           IF WS-QUES-MU-GP-PERC = "Y"
             COMPUTE WS-PERC = 
           (((B-STOCKPRICE (SUB-1) - 
           (B-STOCKPRICE (SUB-1) * B-DISCOUNTPERITEM (SUB-1) / 100))
           - B-STOCKCOST (SUB-1)) / B-STOCKPRICE (SUB-1)) * 100.
           IF WS-QUES-MU-GP-PERC = "Y"
            IF WS-PERC < B-MIN-PERC (SUB-1)
              MOVE "Y"                      TO WS-ITEMS-BELOW-MIN-PERC
              MOVE "$"                      TO P-ITEM-BELOW-MU
            ELSE
              MOVE " "                      TO P-ITEM-BELOW-MU.
       DB-022.
           IF B-NEWLINE (SUB-1) = "Y" OR = "L"
              MOVE "***ALL SHIPPED***"      TO P-SHIPPED
           ELSE
              MOVE B-PULL (SUB-1)           TO P-PULL
              MOVE B-SHIPQTY (SUB-1)        TO P-SHIP
              MOVE B-SHIPPEDQTY (SUB-1)     TO P-TOTSHIPPED
              COMPUTE WS-BO-QTY = B-ORDERQTY (SUB-1) -
                   (B-SHIPQTY (SUB-1) + B-SHIPPEDQTY (SUB-1))
              MOVE WS-BO-QTY                TO P-BO.
           MOVE B-STOCKPRICE (SUB-1)        TO P-PRICE
           MOVE B-STOCKCOST (SUB-1)         TO P-COST
           MOVE B-DISCOUNTPERITEM (SUB-1)   TO P-DISCOUNT
           MOVE "%"                         TO P-PERC
           MOVE B-NETT (SUB-1)              TO P-NETT
           MOVE B-TAX (SUB-1)               TO P-TAX
           MOVE B-UNIT (SUB-1)              TO P-UNIT
           MOVE B-STORE (SUB-1)             TO P-STORE.
              
           IF B-REPAIR (SUB-1) = "R"
              MOVE SPACES                   TO PDET.
      *         MOVE SPACES                   TO P-SHIPPED
      *                                         P-PULL
      *                                         P-SHIP
      *                                         P-TOTSHIPPED
      *                                         P-BO
      *                                         P-PRICE
      *                                         P-COST
      *                                         P-DISCOUNT
      *                                         P-PERC
      *                                         P-NETT
      *                                         P-TAX
      *                                         P-UNIT
      *                                         P-STORE.

           IF INCR-INVOICE NOT = B-INVOICED (SUB-1)
              MOVE B-INVOICED (SUB-1)       TO P-INVOICED.
       DB-025.
           MOVE SUB-1 TO P-NO.
           WRITE DB-REC FROM PDET.
           MOVE " " TO PDET DB-REC.
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 201
              GO TO DB-020.
       DB-030.
            WRITE DB-REC
            WRITE DB-REC.
       DB-035.
           MOVE " "             TO P-COMMENTLINE
           MOVE WS-COMMENTLINE  TO P-BO-MESSAGE
           WRITE DB-REC FROM P-COMMENTLINE
           MOVE " "             TO P-COMMENTLINE
           WRITE DB-REC FROM P-COMMENTLINE
           MOVE " "             TO P-ADDLINE DB-REC
           MOVE "   FREIGHT :"  TO P-DES1
           MOVE "   LABOUR  :"  TO P-DES2
           MOVE "   VAT AMT :"  TO P-DES3
           MOVE WS-ADDONFREIGHT TO P-ADD1
           MOVE WS-HANDADDON    TO P-ADD2
           MOVE WS-TAXAMT       TO P-ADD3
           WRITE DB-REC FROM P-ADDLINE
           MOVE " "             TO P-ADDLINE DB-REC.

           MOVE "   POSTAGE :"  TO P-DES1
           MOVE "   MISC.   :"  TO P-DES2
           MOVE "   SUBTOTAL:"  TO P-DES3
           MOVE "AMT TO INV.:"  TO P-DES4
           MOVE WS-POSTADDON    TO P-ADD1
           MOVE WS-MISCADDON    TO P-ADD2
           MOVE WS-SUBTOTAL     TO P-ADD3
           MOVE WS-INVOICETOTAL TO P-ADD4
           WRITE DB-REC FROM P-ADDLINE
           MOVE " "             TO P-ADDLINE DB-REC.
           WRITE DB-REC.

           MOVE "COMPLETE ORDER AMOUNTS:" TO P-ADDDESC
           MOVE "DISCOUNT: R"             TO P-DES4
           MOVE WS-DISCOUNTREG1           TO P-ADD4
           WRITE DB-REC FROM P-ADDLINE
           MOVE " "                       TO P-ADDLINE DB-REC.

           MOVE "COST AMT: R" TO P-DES4
           MOVE WS-COSTTOTAL1 TO P-ADD4
           WRITE DB-REC FROM P-ADDLINE
           MOVE " "           TO P-ADDLINE DB-REC.

           MOVE "MARGIN  : R" TO P-DES4
           COMPUTE WS-MARGIN = WS-PRICETOTAL1 - WS-COSTTOTAL1
           MOVE WS-MARGIN     TO P-ADD4
           WRITE DB-REC FROM P-ADDLINE
           MOVE " "           TO P-ADDLINE DB-REC.

           MOVE "PROFIT %:  " TO P-DES4
           COMPUTE WS-PERC = (WS-MARGIN / WS-COSTTOTAL1) * 100
           MOVE WS-PERC       TO P-ADD4
           WRITE DB-REC FROM P-ADDLINE
           MOVE " "           TO P-ADDLINE DB-REC.

           MOVE "ORDER TOTAL"  TO P-DES4
           MOVE WS-PRICETOTAL1 TO P-ADD4
           WRITE DB-REC FROM P-ADDLINE.
           MOVE " "            TO P-ADDLINE DB-REC.
       DB-036.
           IF WS-SALESMAN > " "
              MOVE "  SALESMAN:" TO P-DES1
              MOVE WS-SALESMAN   TO P-SALESMAN
           ELSE
              MOVE "  SALESMAN:" TO P-DES1
              MOVE "OPEN A/C"    TO P-SALESMAN.
           WRITE DB-REC FROM P-ADDLINE AFTER 1.
           MOVE " "           TO P-ADDLINE DB-REC.
           
           IF F-NAMEFIELDRED1 = "Q"
              MOVE WS-QUOTE  TO P-QU-NUMBER
              WRITE DB-REC FROM P-QUOTE-LINE.
           IF F-NAMEFIELDRED1 = "R"
              MOVE WS-REPAIR  TO P-RP-NUMBER
              WRITE DB-REC FROM P-REPAIR-LINE.
           
           IF WS-WAS-SUSPENDED = "Y"
             MOVE
           "THIS ORDER WAS ORIGINALLY SUSPENDED, FIRST COPY PRINTED."
              TO DB-REC
              WRITE DB-REC AFTER 2.
           
           IF WS-PART-ORDERS = "N"
            IF WS-BORDERS-FOUND = "Y"
             MOVE
           "THIS ORDER CAN'T BE SUPPLIED AS NO PART-ORDERS ARE ALLOWED."
              TO DB-REC
              WRITE DB-REC AFTER 2.

      **********************************************************
      *THIS SECTION IS FOR THE ORDER AS A WHOLE.  IF ORDER     *
      *BELOW MIN M/U OR G/P THEN THIS COMMENT PRINTS.          *
      **********************************************************
      
           COMPUTE WS-MARGIN = WS-PRICETOTAL1 - WS-COSTTOTAL1.
           IF WS-QUES-MU-GP-PERC = "N"
              COMPUTE WS-PERC = (WS-MARGIN / WS-COSTTOTAL1) * 100
           ELSE
              COMPUTE WS-PERC = (WS-MARGIN / WS-PRICETOTAL1) * 100.
           IF WS-QUES-MU-GP-PERC = "N"
            IF WS-PERC < 20
              MOVE " " TO DB-REC
              WRITE DB-REC AFTER 1
              MOVE WS-PRINT-BOLD   TO PP2-DIG1
              MOVE 
              "** TOTAL ORDER BELOW THE MINIMUM MARK-UP. M/U IS:"
                                   TO PP2-COMM
              MOVE WS-PRINT-UNBOLD TO PP2-DIG2
              MOVE WS-PERC         TO PP2-PERC
              MOVE PLINE-PERC      TO DB-REC
              WRITE DB-REC AFTER 1
              MOVE " " TO PLINE-PERC.
           IF WS-QUES-MU-GP-PERC = "Y"
            IF WS-PERC < 15
              MOVE " " TO DB-REC
              WRITE DB-REC AFTER 1
              MOVE WS-PRINT-BOLD   TO PP2-DIG1
              MOVE 
              "** TOTAL ORDER BELOW THE MINIMUM G/PROFIT. GP IS:"
                                   TO PP2-COMM
              MOVE WS-PRINT-UNBOLD TO PP2-DIG2
              MOVE WS-PERC         TO PP2-PERC
              MOVE PLINE-PERC      TO DB-REC
              WRITE DB-REC AFTER 1
              MOVE " " TO PLINE-PERC.
      *IF INDIVIDUAL ITEMS ARE BELOW THE MIN M/U
           MOVE " " TO PLINE-PERC.
           IF WS-ITEMS-BELOW-MIN-PERC = "Y"
              MOVE " " TO DB-REC
              WRITE DB-REC AFTER 1
              MOVE WS-PRINT-BOLD   TO PP2-DIG1
              MOVE 
              "** SOME ITEMS ARE BELOW THEIR MINIMUM PERCENT. **"
                                   TO PP2-COMM
              MOVE WS-PRINT-UNBOLD TO PP2-DIG2
              MOVE PLINE-PERC      TO DB-REC
              WRITE DB-REC AFTER 1.
              
           
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           MOVE 2 TO WS-PRINT-TYPE.
           PERFORM PRINT-DB-REPORT-INFO.

           MOVE " " TO P-DEL DB-REC.
           MOVE WS-PRINT-NORMAL TO DB-REC
               WRITE DB-REC
               MOVE " " TO DB-REC.
           
           CLOSE DOCUBASE-FILE.
           PERFORM ERROR1-020.
       DB-999.
           EXIT.
      *
       PRINT-PSLIP SECTION.
       PR-000.
           IF F-EXIT-CH = X"17"
               GO TO PR-999.
           IF F-EXIT-CH = X"94"
               PERFORM PR-005 THRU PR-036
               GO TO PR-999.

           IF WS-ADD-TO-OLD-ORDER = "Y"
            IF F-NAMEFIELDRED1 = "Q" OR = "R"
               GO TO PR-060.
       PR-005.
           IF INCR-PRINTED = "S"
               GO TO PR-050.
           MOVE 2910 TO POS
           DISPLAY "PRINTING ORDER FILE.............." AT POS.
           
           IF WS-REPRINT = "Y"
               MOVE WS-REPR-PRINTER To Ws-PrinterNumber (21)
               Move 9               To Ws-PrinterType (21)
               PERFORM CONTROL-100
               GO TO PR-008.
           IF INCR-PRINTED = "R"
               MOVE WS-RUSH-PRINTER To Ws-PrinterNumber (21)
               Move 9               To Ws-PrinterType (21)
               PERFORM CONTROL-100
               GO TO PR-008.
           MOVE WS-NORM-PRINTER To Ws-PrinterNumber (21)
           Move 9               To Ws-PrinterType (21)
           PERFORM CONTROL-100.
       PR-008.
           MOVE " " TO WS-ITEMS-BELOW-MIN-PERC
           MOVE 1 TO WS-PAGE SUB-1 SUB-2
           MOVE " " TO PRINT-REC
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           IF WS-PRINT-ST1 NOT = 0
              MOVE
           "THERE IS A PROBLEM WITH THE PRINTER, NEXT LINE IS STATUS."
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-PRINT-ST1          TO WS-PM-ST1
             MOVE WS-PRINTERNAME (21)   TO WS-PM-NAME
             MOVE WS-PRINTERNUMBER (21) TO WS-PM-NUM
             MOVE WS-PRINT-MESSAGE      TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO PR-008.
       PR-010.
      ******************************************************************
      * ADDED FOR GARRY @ SFJ DUE TO PSLIP'S GOING MISSING.            *
      * HE WILL MANUALLY RE-START THE PRINTER FROM THE 'PRINT MANAGER' *
      ******************************************************************
           IF WS-QUES-PAUSE-ON-PSLIP = "Y"
               MOVE WTELL-PAUSE TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.

           MOVE WS-PRINT-COMP TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           
           IF WS-PAGE > 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE WS-PAGE TO P-CONTROL-PAGE
               WRITE PRINT-REC FROM P-CONTINUED AFTER 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC BEFORE PAGE.
       PR-012.
           WRITE PRINT-REC FROM COMPANY-LINE AFTER 1.
           IF WS-REPRINT = "Y"
               MOVE " " TO PRINT-REC
               MOVE WS-PRINT-BOLD   TO P-AL-DIG1
               MOVE "** THIS IS A REPRINTED ORDER **" TO P-AL-COMMENT
               MOVE WS-PRINT-UNBOLD TO P-AL-DIG2
               WRITE PRINT-REC FROM P-ALLOCATE-LINE AFTER 1
               MOVE WS-PRINT-COMP   TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC AFTER 1.
           IF P-SLIP = 0
              MOVE WS-INVOICE     TO P-SLIP.
           MOVE WS-PRINT-BOLD     TO P-DIG1 P-DIG3 P-QU-DIG1
           MOVE WS-PRINT-UNBOLD   TO P-DIG2 P-DIG4 P-QU-DIG2.
           MOVE WS-ACCOUNT-NUMBER TO P-ACCNO
           MOVE WS-GSTNO          TO P-GSTNO
           MOVE WS-PAGE           TO P-PAGE
           MOVE INCR-COPY-NUMBER  TO P-PRINTNUMBER
           WRITE PRINT-REC FROM PLINE1 AFTER 1.
           MOVE " " TO PRINT-REC
           MOVE WS-PRINT-COMP TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE " " TO PRINT-REC.
           MOVE WS-NAME     TO P-ADD
           MOVE "PHONE NO:" TO P-NAME
           MOVE WS-PHONE    TO P-CONTACT
           WRITE PRINT-REC FROM PLINE2 AFTER 1.
           MOVE " "         TO PRINT-REC PLINE2
           MOVE WS-ADD1     TO P-ADD
           MOVE WS-DELADD1  TO P-DEL
           MOVE "CONTACT :" TO P-NAME
           MOVE WS-CONTACT  TO P-CONTACT
           WRITE PRINT-REC FROM PLINE2 AFTER 1.
           MOVE " "         TO PRINT-REC PLINE2
           MOVE WS-ADD2     TO P-ADD
           MOVE WS-DELADD2  TO P-DEL
           WRITE PRINT-REC FROM PLINE2 AFTER 1.
           MOVE " "         TO PRINT-REC PLINE2
           MOVE WS-ADD3     TO P-ADD
           MOVE WS-DELADD3  TO P-DEL
           WRITE PRINT-REC FROM PLINE2 AFTER 1.
           MOVE " "             TO PRINT-REC PLINE2
           MOVE WS-POSTCODE     TO P-CODE
           MOVE WS-PRINT-BOLD   TO P2-DIG1
           MOVE INCR-AREA       TO P2-COMM
           MOVE WS-PRINT-UNBOLD TO P2-DIG2

           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR TO SPLIT-HR
           MOVE ":" TO SPLIT-HR-FIL SPLIT-MN-FIL
           MOVE WS-MIN TO SPLIT-MN
           MOVE WS-SEC TO SPLIT-SC
           MOVE "Time Printed:" TO SPLIT-TIME-FIL
           MOVE WS-TIME-DISPLAY TO P-TIME.
           WRITE PRINT-REC FROM PLINE3 AFTER 1.

           MOVE " "         TO PRINT-REC PLINE3
           WRITE PRINT-REC AFTER 1
           WRITE PRINT-REC FROM PLINE4-1 AFTER 1.
           MOVE WS-TERMOFSALE  TO P-TERMS
           MOVE WS-POORDERNO   TO P-PO
           MOVE WSAN-CODE      TO P-SOLD.
           IF INCR-PRINTED = "R"
              MOVE "*RUSH-JOB, PULL NOW*" TO P-VIA
           ELSE
              MOVE WS-DELIVERVIA          TO P-VIA.
           MOVE WS-BINNO       TO P-BIN
           MOVE WS-SOLD-BY     TO P-SOLDBY.
           IF F-NAMEFIELDRED1 = "Q" OR = "R"
              MOVE WS-DATE     TO WS-INVOICEDATE.
           MOVE WS-INVOICEDATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE   TO P-DATE
           MOVE WS-INVOICE     TO P-INV
           MOVE WS-DATE        TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE   TO P-TODAY
           WRITE PRINT-REC FROM PLINE4 AFTER 1
           MOVE " "            TO PRINT-REC PLINE4
           WRITE PRINT-REC AFTER 1
           WRITE PRINT-REC FROM PDET-1 AFTER 1
           MOVE " "            TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
       PR-020.
           IF SUB-1 < 199
             IF SUB-1 = SUB-20
               SUBTRACT 1 FROM SUB-2
               GO TO PR-030.
           IF SUB-2 > 40
               MOVE 1 TO SUB-2
               ADD 1 TO WS-PAGE
               GO TO PR-010.
           IF SUB-1 > 200
              GO TO PR-030.
           IF B-STOCKNUMBER (SUB-1) = " "
              AND B-ORDERQTY (SUB-1) = 0
               GO TO PR-030.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               MOVE B-STOCKNUMBER (SUB-1) TO P-STOCK
               MOVE C-LINE (SUB-1) TO PDET-REST
               GO TO PR-025.
           MOVE B-STOCKNUMBER (SUB-1)       TO P-STOCK
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO P-DESC
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO P-DESC2
           MOVE B-ORDERQTY (SUB-1)          TO P-ORDER.
   
      ********************************************************
      * NEW SECTION TO FLAG ITEMS THAT ARE BELOW MIN MU PERC *
      * ST-MIN-PERC ADDED TO CHLFDSTOCK 17/12/2001           *
      * ONLY PRINTS FLAG IF SHIPPED-QTY < ORDER-QTY          *
      ********************************************************
           IF B-NEWLINE (SUB-1) = "Y" OR = "L"
              GO TO PR-022.
           IF B-REPAIR (SUB-1) = "R"
              GO TO PR-022.
           IF WS-QUES-MU-GP-PERC = "N"
             COMPUTE WS-PERC = 
           (((B-STOCKPRICE (SUB-1) - 
           (B-STOCKPRICE (SUB-1) * B-DISCOUNTPERITEM (SUB-1) / 100))
           - B-STOCKCOST (SUB-1)) / B-STOCKCOST (SUB-1)) * 100.
           IF WS-QUES-MU-GP-PERC = "N"
            IF WS-PERC < B-MIN-PERC (SUB-1)
              MOVE "Y"                      TO WS-ITEMS-BELOW-MIN-PERC
              MOVE "$"                      TO P-ITEM-BELOW-MU
            ELSE
              MOVE " "                      TO P-ITEM-BELOW-MU.
              
           IF WS-QUES-MU-GP-PERC = "Y"
             COMPUTE WS-PERC = 
           (((B-STOCKPRICE (SUB-1) - 
           (B-STOCKPRICE (SUB-1) * B-DISCOUNTPERITEM (SUB-1) / 100))
           - B-STOCKCOST (SUB-1)) / B-STOCKPRICE (SUB-1)) * 100.
           IF WS-QUES-MU-GP-PERC = "Y"
            IF WS-PERC < B-MIN-PERC (SUB-1)
              MOVE "Y"                      TO WS-ITEMS-BELOW-MIN-PERC
              MOVE "$"                      TO P-ITEM-BELOW-MU
            ELSE
              MOVE " "                      TO P-ITEM-BELOW-MU.
       PR-022.
      *    IF WS-REPAIR > 0
      *     IF B-REPAIR (SUB-1) = "R"
      *        GO TO PR-024.
           IF B-NEWLINE (SUB-1) = "Y" OR = "L"
              MOVE "***ALL SHIPPED***"      TO P-SHIPPED
           ELSE
              MOVE B-PULL (SUB-1)           TO P-PULL
              MOVE B-SHIPQTY (SUB-1)        TO P-SHIP
              MOVE B-SHIPPEDQTY (SUB-1)     TO P-TOTSHIPPED
              COMPUTE WS-BO-QTY = B-ORDERQTY (SUB-1) -
                   (B-SHIPQTY (SUB-1) + B-SHIPPEDQTY (SUB-1))
              MOVE WS-BO-QTY                TO P-BO.
           MOVE B-STOCKPRICE (SUB-1)        TO P-PRICE
           MOVE B-STOCKCOST (SUB-1)         TO P-COST
           MOVE B-DISCOUNTPERITEM (SUB-1)   TO P-DISCOUNT
           MOVE "%"                         TO P-PERC
           MOVE B-NETT (SUB-1)              TO P-NETT
           MOVE B-TAX (SUB-1)               TO P-TAX
           MOVE B-UNIT (SUB-1)              TO P-UNIT
           MOVE B-STORE (SUB-1)             TO P-STORE.
              
           IF B-REPAIR (SUB-1) = "R"
              MOVE SPACES                   TO PDET.
      *        MOVE SPACES                   TO P-SHIPPED
      *                                         P-PULL
      *                                         P-SHIP
      *                                         P-TOTSHIPPED
      *                                         P-BO
      *                                         P-PRICE
      *                                         P-COST
      *                                         P-DISCOUNT
      *                                         P-PERC
      *                                         P-NETT
      *                                         P-TAX
      *                                         P-UNIT
      *                                         P-STORE.
       PR-024.
           IF INCR-INVOICE NOT = B-INVOICED (SUB-1)
              MOVE B-INVOICED (SUB-1)       TO P-INVOICED.
       PR-025.
           MOVE SUB-1 TO P-NO.
           WRITE PRINT-REC FROM PDET AFTER 1.
           MOVE " " TO PDET PRINT-REC.
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 201
              GO TO PR-020.
       PR-030.
            WRITE PRINT-REC AFTER 2.
       PR-035.
           MOVE " "             TO P-COMMENTLINE
           MOVE WS-COMMENTLINE  TO P-BO-MESSAGE
           WRITE PRINT-REC FROM P-COMMENTLINE AFTER 1
           MOVE " "             TO P-COMMENTLINE
           WRITE PRINT-REC FROM P-COMMENTLINE AFTER 1
           MOVE " "             TO P-ADDLINE PRINT-REC
           MOVE "   FREIGHT :"  TO P-DES1
           MOVE "   LABOUR  :"  TO P-DES2
           MOVE "   VAT AMT :"  TO P-DES3
           MOVE WS-ADDONFREIGHT TO P-ADD1
           MOVE WS-HANDADDON    TO P-ADD2
           MOVE WS-TAXAMT       TO P-ADD3
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " "             TO P-ADDLINE PRINT-REC.

           MOVE "   POSTAGE :"  TO P-DES1
           MOVE "   MISC.   :"  TO P-DES2
           MOVE "   SUBTOTAL:"  TO P-DES3
           MOVE "AMT TO INV.:"  TO P-DES4
           MOVE WS-POSTADDON    TO P-ADD1
           MOVE WS-MISCADDON    TO P-ADD2
           MOVE WS-SUBTOTAL     TO P-ADD3
           MOVE WS-INVOICETOTAL TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " "             TO P-ADDLINE PRINT-REC.
           WRITE PRINT-REC AFTER 1.

           PERFORM CALCULATE-ORDER-TOTAL.
           IF WS-COST-DISPLAY = "N"
              GO TO PR-036.

           MOVE "COMPLETE ORDER AMOUNTS:" TO P-ADDDESC
           MOVE "DISCOUNT: R"             TO P-DES4
           MOVE WS-DISCOUNTREG1           TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " "                       TO P-ADDLINE PRINT-REC.

           MOVE "COST AMT: R" TO P-DES4
           MOVE WS-COSTTOTAL1 TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " "           TO P-ADDLINE PRINT-REC.

           MOVE "MARGIN  : R" TO P-DES4
           COMPUTE WS-MARGIN = WS-PRICETOTAL1 - WS-COSTTOTAL1
           MOVE WS-MARGIN     TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " "           TO P-ADDLINE PRINT-REC.

           MOVE "PROFIT %:  " TO P-DES4
           COMPUTE WS-PERC = (WS-MARGIN / WS-COSTTOTAL1) * 100
           MOVE WS-PERC       TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " "           TO P-ADDLINE PRINT-REC.

           MOVE "ORDER TOTAL"  TO P-DES4
           MOVE WS-PRICETOTAL1 TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1.
           MOVE " "            TO P-ADDLINE PRINT-REC.
       PR-036.
           IF WS-SALESMAN > " "
              MOVE "  SALESMAN:" TO P-DES1
              MOVE WS-SALESMAN   TO P-SALESMAN
           ELSE
              MOVE "  SALESMAN:" TO P-DES1
              MOVE "OPEN A/C"    TO P-SALESMAN.
           WRITE PRINT-REC FROM  P-ADDLINE AFTER 1.
           MOVE " "           TO P-ADDLINE PRINT-REC.
           
           IF F-NAMEFIELDRED1 = "Q"
              MOVE WS-QUOTE  TO P-QU-NUMBER
              WRITE PRINT-REC FROM P-QUOTE-LINE AFTER 1.
           IF F-NAMEFIELDRED1 = "R"
              MOVE WS-REPAIR  TO P-RP-NUMBER
              WRITE PRINT-REC FROM P-REPAIR-LINE AFTER 1.
           
           IF WS-WAS-SUSPENDED = "Y"
             MOVE
           "THIS ORDER WAS ORIGINALLY SUSPENDED, FIRST COPY PRINTED."
              TO PRINT-REC
              WRITE PRINT-REC.
              
           MOVE 2910 TO POS
           DISPLAY "CHECKING FOR BORDERS............." AT POS
           PERFORM CHECK-FOR-BORDERS.
           IF WS-PART-ORDERS = "N"
            IF WS-BORDERS-FOUND = "Y"
             MOVE
           "THIS ORDER CAN'T BE SUPPLIED AS NO PART-ORDERS ARE ALLOWED."
              TO PRINT-REC
              WRITE PRINT-REC AFTER 2.
 
      ********************************************************
      *THIS SECTION IS FOR THE ORDER AS A WHOLE.  IF ORDER   *
      *BELOW MIN M/U OR G/P THEN THIS COMMENT PRINTS.        *
      ********************************************************
           COMPUTE WS-MARGIN = WS-PRICETOTAL1 - WS-COSTTOTAL1.
           IF WS-QUES-MU-GP-PERC = "N"
              COMPUTE WS-PERC = (WS-MARGIN / WS-COSTTOTAL1) * 100
           ELSE
              COMPUTE WS-PERC = (WS-MARGIN / WS-PRICETOTAL1) * 100.
              
           IF WS-QUES-MU-GP-PERC = "N"
            IF WS-PERC < 20
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              MOVE WS-PRINT-BOLD   TO PP2-DIG1
              MOVE 
              "** TOTAL ORDER BELOW THE MINIMUM MARK-UP. M/U IS:"
                                   TO PP2-COMM
              MOVE WS-PRINT-UNBOLD TO PP2-DIG2
              MOVE WS-PERC         TO PP2-PERC
              MOVE PLINE-PERC      TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              MOVE " " TO PLINE-PERC.
           IF WS-QUES-MU-GP-PERC = "Y"
            IF WS-PERC < 15
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              MOVE WS-PRINT-BOLD   TO PP2-DIG1
              MOVE 
              "** TOTAL ORDER BELOW THE MINIMUM G/PROFIT. GP IS:"
                                   TO PP2-COMM
              MOVE WS-PRINT-UNBOLD TO PP2-DIG2
              MOVE WS-PERC         TO PP2-PERC
              MOVE PLINE-PERC      TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              MOVE " " TO PLINE-PERC.
      *IF INDIVIDUAL ITEMS ARE BELOW THE MIN M/U
           MOVE " " TO PLINE-PERC.
           IF WS-ITEMS-BELOW-MIN-PERC = "Y"
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              MOVE WS-PRINT-BOLD   TO PP2-DIG1
              MOVE 
              "** SOME ITEMS ARE BELOW THEIR MINIMUM PERCENT. **"
                                   TO PP2-COMM
              MOVE WS-PRINT-UNBOLD TO PP2-DIG2
              MOVE PLINE-PERC      TO PRINT-REC
              WRITE PRINT-REC AFTER 1.
           
           MOVE " " TO P-DEL PRINT-REC.
           
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
      ********************************************************
      *NEXT LINE ENTERED TO RE-SET PRINTER TO 10CPI FOR GRC  *
      * AS [SPLA] IS A SHARED PRINTER                        *
      ********************************************************
           MOVE 2 TO WS-PRINT-TYPE.
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           PERFORM ERROR1-020.
       PR-050.
           IF F-EXIT-CH = X"94"
               GO TO PR-999.
           
           MOVE 2910 TO POS
           DISPLAY "CHECKING ORDER COMPLETE.........." AT POS
           PERFORM CHECK-ORDER-COMPLETE.

           MOVE 2910 TO POS
           DISPLAY "WRITING ST-ORDER-TRANS..........." AT POS
           PERFORM WRITE-STOCK-TRANSACTIONS
           
           MOVE 2910 TO POS
           DISPLAY "WRITING ORDER-REGISTER..........." AT POS
           PERFORM WRITE-INCR-REGISTER.
           
           IF F-NAMEFIELDRED1 = "Q"
              MOVE 2910 TO POS
              DISPLAY "DELETING QUOTE-REGISTER.........." AT POS
              PERFORM DELETE-QUOTE-REGISTER
              MOVE 2910 TO POS
              DISPLAY "DELETING QUOTE-ST-TRANS.........." AT POS
              PERFORM DELETE-QUOTE-TRANS.

              
           IF F-NAMEFIELDRED1 = "R"
              MOVE 2910 TO POS
              DISPLAY "CHECKING ORDER COMPLETE.........." AT POS
              PERFORM CHECK-REPAIR-COMPLETE

              MOVE 2910 TO POS
              DISPLAY "REWRITING REPAIR-REGISTER........" AT POS
              PERFORM REWRITE-REPAIR-REGISTER
              
              MOVE 2910 TO POS
              DISPLAY "REWRITING REPAIR-STOCK-TRANS....." AT POS
              PERFORM REWRITE-REPAIR-ST-TRANS.
              
           GO TO PR-999.
       PR-060.
           MOVE 2910 TO POS
           DISPLAY "WRITING ST-ORDER-TRANS..........." AT POS
           PERFORM WRITE-STOCK-TRANSACTIONS
           
           MOVE 2910 TO POS
           DISPLAY "REWRITING OLD ORDER REGISTER....." AT POS
           PERFORM REWRITE-OLD-ORDER
           
           MOVE 2910 TO POS
           DISPLAY "DELETING QUOTE-REGISTER.........." AT POS
           PERFORM DELETE-QUOTE-REGISTER
           
           MOVE 2910 TO POS
           DISPLAY "DELETING QUOTE-ST-TRANS.........." AT POS
           PERFORM DELETE-QUOTE-TRANS.
       PR-999.
           EXIT.
      *
       CHANGE-DEL-QTY SECTION.
       CDQ-005.
           PERFORM FILL-BODY.
           IF WS-ABOVE-BODY = "1"
               GO TO CDQ-999.
           PERFORM GET-240 THRU GET-999.
       CDQ-999.
           EXIT.
      *
       CHANGE-PORDER SECTION.
       CPOR-010.
           PERFORM CHECK-PASSWORD.
           IF WS-PASSWORD-VALID = "N"
              GO TO CPOR-999.
           MOVE "          " TO ALPHA-RATE
           MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-INVOICE P-SLIP
           PERFORM READ-INVOICE-REGISTER.
           IF WS-NEWORDER = "N" OR = "P"
             PERFORM READ-STOCK-TRANSACTIONS
             PERFORM FIND-INFO
           ELSE
             MOVE "THIS P/SLIP IS COMPLETE, 'ESC' TO EXIT."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO CPOR-999.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
       CPOR-020.
           MOVE "POORDERNO" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD
           IF F-EXIT-CH = X"07"
               UNLOCK INCR-REGISTER
               MOVE X"17" TO F-EXIT-CH
               GO TO CPOR-999.
           MOVE 20          TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD TO WS-POORDERNO.
           IF WS-POORDERNO NOT = INCR-PORDER
               PERFORM CHECK-REGISTER
           ELSE
               GO TO CPOR-025.
           IF WS-NEWORDER NOT = "Y"
              MOVE
           "THERE IS A P/SLIP WITH THIS P-ORDER FOR THIS ACC, " &
           "'ESC' TO RE-ENTER." TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
              MOVE NUMERIC-RATE TO WS-INVOICE
              PERFORM READ-INVOICE-REGISTER
              GO TO CPOR-020.
           MOVE "N" TO WS-NEWORDER
                       WS-ORDER-COMPLETE.
       CPOR-025.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DEBTORNAME" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO CPOR-020.
            MOVE 40 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-NAME.
       CPOR-030.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO CPOR-025.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ADD1.
       CPOR-040.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO CPOR-030.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ADD2.
       CPOR-050.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO CPOR-040.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ADD3.
       CPOR-060.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "POSTCODE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO CPOR-050.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-POSTCODE.
       CPOR-070.
           IF F-EXIT-CH = X"1B"
                GO TO CPOR-900.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE 25        TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"19"
               MOVE 25          TO F-CBFIELDLENGTH
               PERFORM READ-FIELD-ALPHA
               MOVE F-NAMEFIELD TO F-NAMEFIELDRED
               PERFORM GET-DEL-INFO
               GO TO CPOR-070.
            IF F-EXIT-CH = X"01"
                GO TO CPOR-020.
            MOVE 25          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DELADD1.
            IF WS-DELADD1 = "  "
                GO TO CPOR-070.
           IF F-EXIT-CH = X"1B"
                GO TO CPOR-900.
       CPOR-080.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE 25        TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO CPOR-070.
            MOVE 25          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DELADD2.
            IF WS-DELADD2 = "  "
                GO TO CPOR-080.
           IF F-EXIT-CH = X"1B"
                GO TO CPOR-900.
       CPOR-090.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE 25        TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO CPOR-080.
            MOVE 25          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DELADD3.
           IF F-EXIT-CH = X"1B"
                GO TO CPOR-900.
       CPOR-130.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DELIVERVIA" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            MOVE 20           TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO CPOR-070.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-FIELD NUMERIC-FIELD WS-DELIVERVIA.
            IF NOT NUMERIC-VALUE
                GO TO CPOR-900.
            IF ALPHA-VALUE
                GO TO CPOR-900.
            MOVE WS-DELIVERVIA TO SPLIT-DELIVERVIA.
            MOVE WSDE-CODE TO WS-DEL-SUB.
            IF WS-DEL-SUB = 0
               MOVE 1 TO WS-DEL-SUB.
            MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA.
            IF WS-DELIVERVIA = ALL "X"
                MOVE "INVALID DELIVERY CODE, PLEASE RE-ENTER!"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO CPOR-130.
            MOVE "DELIVERVIA"  TO F-FIELDNAME.
            MOVE 10            TO F-CBFIELDNAME.
            MOVE WS-DELIVERVIA TO F-NAMEFIELD.
            MOVE 20            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
      ****************************************************
      *DEL-CODE = 5 IS FOR 'ROAD FREIGHT VARIOUS'        *
      ****************************************************
            IF WS-DEL-SUB = 5
             IF WS-SPECIAL-INFO NOT = "Y"
               PERFORM GET-DEL-INFO.
               
           IF F-EXIT-CH NOT = X"1B"
                GO TO CPOR-020.
       CPOR-900.
           PERFORM RIR-000 THRU RIR-005.
           MOVE WS-NAME       TO INCR-NAME
           MOVE WS-ADD1       TO INCR-ADD1
           MOVE WS-ADD2       TO INCR-ADD2
           MOVE WS-ADD3       TO INCR-ADD3
           MOVE WS-POSTCODE   TO INCR-CODE
           MOVE WS-DELADD1    TO INCR-DEL1
           MOVE WS-DELADD2    TO INCR-DEL2
           MOVE WS-DELADD3    TO INCR-DEL3
           MOVE WS-DELIVERVIA TO INCR-DELIVERY
           MOVE WS-POORDERNO  TO INCR-PORDER
           PERFORM WRIC-065.
           MOVE X"17" TO F-EXIT-CH.
       CPOR-999.
           EXIT.
      *
       GET-DEL-INFO SECTION.
       GDI-005.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           IF F-EXIT-CH = X"19"
               GO TO GDI-075.
           MOVE
           "ENTER *, A NUMBER & <F5> FOR SPECIAL DELIVERY INSTRUCTIONS,"
              TO WS-MESSAGE
              PERFORM ERROR1-000
           MOVE
           "  AS THIS ACCOUNT IS FLAGGED FOR 'ROAD FREIGHT' DELIVERY."
              TO WS-MESSAGE
              PERFORM ERROR-000.
       GDI-070.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            MOVE SPACES TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                GO TO GDI-900.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO F-NAMEFIELDRED.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
       GDI-075.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 NOT = "*"
               GO TO GDI-070.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "*"
               PERFORM READ-DEBTOR-DELIVERIES.
           IF F-NAMEFIELDRED1 NOT = "*"
              MOVE F-NAMEFIELD TO WS-DELADD1.
           IF WS-DELADD1 = "  "
                GO TO GDI-070.

            MOVE "DELADD1"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD1 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD2"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD2 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD3"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD3 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERVIA"     TO F-FIELDNAME
            MOVE 10               TO F-CBFIELDNAME
            MOVE WS-DELIVERVIA    TO F-NAMEFIELD
            MOVE 20               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            MOVE "Y" TO WS-SPECIAL-INFO.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
       GDI-900.
           IF WS-QUOTE > 0
              MOVE "Q" TO F-NAMEFIELDRED1.
           IF WS-REPAIR > 0
              MOVE "R" TO F-NAMEFIELDRED1.
       GDI-999.
           EXIT.
      *
       READ-DEBTOR-DELIVERIES SECTION.
       RDD-011.
           OPEN I-O DEBTOR-DELIVERY.
           IF WS-DRDEL-ST1 NOT = 0 
              MOVE "DR-DELIVERY FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDD-011.
       RDD-020.
           MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE      TO DRDEL-NUMBER
           MOVE DR-ACCOUNT-NUMBER TO DRDEL-ACCOUNT-NUMBER
           START DEBTOR-DELIVERY KEY NOT < DRDEL-KEY
              INVALID KEY NEXT SENTENCE.
       RDD-030.
           READ DEBTOR-DELIVERY
               INVALID KEY NEXT SENTENCE.
           IF WS-DRDEL-ST1 = 23 OR 35 OR 49
               MOVE "THERE IS NO SPECIAL DELIVERY FILE FOR THIS ACCOUNT"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDD-900.
           IF WS-DRDEL-ST1 NOT = 0
               MOVE "DR-DEL RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRDEL-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRDEL-ST1
               GO TO RDD-030.
       RDD-010.
           MOVE DRDEL-NAME      TO WS-DELIVERVIA
           MOVE DRDEL-ADDRESS1  TO WS-DELADD1
           MOVE DRDEL-ADDRESS2  TO WS-DELADD2
           MOVE DRDEL-ADDRESS3  TO WS-DELADD3.
           MOVE "Y"             TO WS-SPECIAL-INFO.
       RDD-900.
           CLOSE DEBTOR-DELIVERY.
       RDD-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-010.
      *      PERFORM CI-900.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "N" TO WS-ZERODIS
                        WS-PASSWORD-VALID
                        WS-LINECHANGED
                        WS-ACCEPT
                        WS-WAS-SUSPENDED
                        WS-MUST-PRINT.
            MOVE "Y" TO WS-NEWORDER.
            MOVE " " TO WS-MESSAGE
                        WS-ABOVE-BODY
                        WS-SPECIAL-INFO
                        WS-DIS
                        WS-SOLD-BY
                        WS-BO-INVOICE-MESSAGE.
           PERFORM ERROR-020
           PERFORM ERROR1-020.
           MOVE 2910 TO POS
           DISPLAY
           "ENTER A/C # & <RETURN> TO CREATE A P/SLIP, ENTER P/SLIP" &
           " # & <F5>" AT POS
           MOVE 3010 TO POS
           DISPLAY
           " TO CHANGE P/SLIP, OR ENTER P/SLIP # & <F3> TO CHANGE " &
           "P/ORDER #." AT POS.
            MOVE "ACCOUNTNO" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
      *RE-PRINT OF P-SLIP
      *      IF F-EXIT-CH = X"14"
      *       IF WS-IMM-PR = "Y"
      *          MOVE "Press 'ALT-C' To Reprint This Order."
      *          TO WS-MESSAGE
      *          PERFORM ERROR-MESSAGE
      *          GO TO GET-010.
      * <CODE-COPY> CHANGED IN LINUX TO <COPY> WHICH IS <ALT-C>
            IF F-EXIT-CH = X"14"
             IF WS-IMM-PR = "Y"
                PERFORM PRINT-PSLIP
                GO TO GET-010.
            MOVE " " TO WS-NAMEANDADDRESS
                        WS-ACCNO-X
                        WS-ADD-TO-OLD-ORDER
                        WS-BINNO WS-REPRINT INCR-PORDER. 
                        
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            MOVE 0 TO DR-ACCOUNT-NUMBER
                      WS-ADDONFREIGHT WS-POSTADDON
                      WS-HANDADDON WS-MISCADDON
                      WS-POSTCODE WS-BO-NUMBER WS-QUOTE WS-REPAIR
                      WS-INVOICEDISCOUNT WS-STTRANS-NO WS-P-SLIP.
            MOVE 1 TO SUB-20 SUB-25
            PERFORM CLEAR-FIELDS
            MOVE 7 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO F-NAMEFIELDRED.

            IF F-NAMEFIELDRED = "STOCK  "
                CLOSE STOCK-MASTER
                CALL WS-STOCK-INQUIRY USING WS-LINKAGE
                CANCEL WS-STOCK-INQUIRY
                PERFORM CLEAR-SCREEN
                OPEN I-O STOCK-MASTER
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD F-NAMEFIELDRED
                GO TO GET-010.
            IF F-NAMEFIELDRED = "PORDER "
                CLOSE STOCK-MASTER
                CALL WS-PORDER-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-PORDER-INQUIRY
                OPEN I-O STOCK-MASTER
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD F-NAMEFIELDRED
                GO TO GET-010.
            IF F-NAMEFIELDRED = "QUOTEAC"
                CLOSE STOCK-MASTER
                CLOSE DEBTOR-MASTER
                CALL WS-QUOTE-ACC-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-QUOTE-ACC-INQUIRY
                OPEN I-O STOCK-MASTER
                OPEN I-O DEBTOR-MASTER
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD F-NAMEFIELDRED
                GO TO GET-010.
            IF F-NAMEFIELDRED = "QUOTEST"
                CLOSE STOCK-MASTER
                CLOSE DEBTOR-MASTER
                CALL WS-QUOTE-STK-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-QUOTE-STK-INQUIRY
                OPEN I-O STOCK-MASTER
                OPEN I-O DEBTOR-MASTER
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD F-NAMEFIELDRED
                GO TO GET-010.
            IF F-NAMEFIELDRED1 = "S"
                AND F-NAMEFIELDRED7 NOT = "TOCK"
                   DISPLAY " " AT 3079 WITH BELL
                   GO TO GET-010.

      **********************************************************
      * <F3> KEY, TO FIND OLD ORDER AND CHANGE P-ORDER NUMBER  *
      **********************************************************
           IF F-EXIT-CH = X"17"
            IF F-NAMEFIELDRED1 = "*"
               PERFORM CHANGE-PORDER
               GO TO GET-999.
              
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           IF F-EXIT-CH = X"19" OR = X"1F" OR = X"9B" OR = X"C7"
             IF F-NAMEFIELDRED1 NOT = "*" AND NOT = "Q" AND NOT = "R"
                GO TO GET-010.
           IF F-EXIT-CH = X"9B" OR X"C7" OR = X"1F" OR = X"15"
                PERFORM CHECK-PASSWORD
            IF WS-PASSWORD-VALID = "N"
               GO TO GET-010.
      **********************************************************
      * <F1> KEY & 'D', TO CHANGE DELIVERY QTY'S               *
      **********************************************************
           IF F-EXIT-CH = X"15"
            IF F-NAMEFIELDRED1 = "D"
                MOVE "          " TO ALPHA-RATE
                MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-INVOICE P-SLIP WS-P-SLIP
                PERFORM READ-INVOICE-REGISTER
            IF WS-NEWORDER = "N" OR = "P" OR = "T"
                PERFORM READ-STOCK-TRANSACTIONS
                PERFORM FIND-INFO
                MOVE "Y" TO WS-REPRINT
                            WS-ACCEPT
                PERFORM CHANGE-DEL-QTY
                GO TO GET-999.
      **********************************************************
      * 'CODE-GO' KEY, TO FIND OLD ORDER, PENDING STOCK & PULL *
      * <ALT-G> = X"C7"    <ALT-g> = X"9B"                     *
      **********************************************************
           IF F-EXIT-CH = X"9B" OR = X"C7"
                MOVE "          " TO ALPHA-RATE
                MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-INVOICE P-SLIP WS-P-SLIP
                PERFORM READ-INVOICE-REGISTER
            IF WS-NEWORDER = "N" OR = "P" OR = "T"
                PERFORM READ-STOCK-TRANSACTIONS
                PERFORM FIND-INFO
                MOVE "Y" TO WS-REPRINT
                            WS-ACCEPT
                GO TO GET-150.
      ***********************************************************
      * 'F5' KEY, TO FIND A QUOTE, CONVERT TO ORDER AND DISPLAY *
      ***********************************************************
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "Q"
                MOVE "          " TO ALPHA-RATE
                MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-QUOTE
                PERFORM READ-QUOTE-REGISTER.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "Q"
             IF WS-NEWORDER = "N"
                PERFORM CHECK-TO-ENTER-ORDER.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "Q"
             IF WS-NEWORDER = "N"
              IF WS-ACCEPT = "Y"
                PERFORM READ-QUOTE-TRANSACTIONS
                PERFORM RESERVE-STOCK-QUANTITIES
                PERFORM ERROR1-020
                PERFORM ERROR-020
                PERFORM FIND-INFO
                MOVE "Y" TO WS-MUST-PRINT
                MOVE 2910 TO POS
                DISPLAY "DAILY EXCEPTION LOG BEING WRITTEN.....     "
                AT POS
                MOVE "QUOTE CONVERTED  No:" TO WS-DAILY-1ST
                MOVE WS-QUOTE               TO WS-DAILY-2ND
                MOVE "TO P/SLIP NUMBER   :" TO WS-DAILY-3RD
                MOVE WS-INVOICE             TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                PERFORM GET-USER-MAIL-NAME
                PERFORM GET-REPORT-Y2K-DATE
                MOVE "ABOVE CHANGE DONE BY" TO WS-DAILY-1ST
                MOVE WS-pbValue             TO WS-DAILY-2ND
                MOVE "DATE & TIME CHANGED:" TO WS-DAILY-3RD
                MOVE pbRet                  TO WS-REPORT-DATE-STRIP
                MOVE WS-STRIP2              TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO GET-095
             ELSE
                PERFORM CI-950
                MOVE 0 TO DR-ACCOUNT-NUMBER
                          WS-ACCOUNT-NUMBER
                PERFORM CLEAR-FIELDS
                MOVE " " TO WS-BINNO
                GO TO GET-010.
      ************************************************************
      * 'F5' KEY, TO FIND A REPAIR, CONVERT TO ORDER AND DISPLAY *
      ************************************************************
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "R"
                MOVE "          " TO ALPHA-RATE
                MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-REPAIR
                PERFORM READ-REPAIR-REGISTER.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "R"
             IF WS-NEWORDER = "N"
                PERFORM CHECK-TO-ENTER-ORDER.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "R"
             IF WS-NEWORDER = "N"
              IF WS-ACCEPT = "Y"
                PERFORM READ-REPAIR-TRANSACTIONS
                PERFORM RESERVE-STOCK-QUANTITIES
                PERFORM ERROR1-020
                PERFORM ERROR-020
                PERFORM FIND-INFO
                MOVE "Y" TO WS-MUST-PRINT
                MOVE 2910 TO POS
                DISPLAY "DAILY EXCEPTION LOG BEING WRITTEN.....     "
                AT POS
                MOVE "REPAIR CONVERTED No:" TO WS-DAILY-1ST
                MOVE WS-REPAIR              TO WS-DAILY-2ND
                MOVE "TO P/SLIP NUMBER   :" TO WS-DAILY-3RD
                MOVE WS-INVOICE             TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                PERFORM GET-USER-MAIL-NAME
                PERFORM GET-REPORT-Y2K-DATE
                MOVE "ABOVE CHANGE DONE BY" TO WS-DAILY-1ST
                MOVE Ws-pbValue             TO WS-DAILY-2ND
                MOVE "DATE & TIME CHANGED:" TO WS-DAILY-3RD
                MOVE pbRet                  TO WS-REPORT-DATE-STRIP
                MOVE WS-STRIP2              TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO GET-095
             ELSE
                PERFORM CI-950
                MOVE 0 TO DR-ACCOUNT-NUMBER
                          WS-ACCOUNT-NUMBER
                PERFORM CLEAR-FIELDS
                MOVE " " TO WS-BINNO
                GO TO GET-010.
      *******************************************
      * 'F5' KEY, TO FIND OLD ORDER AND DISPLAY *
      *******************************************
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "*"
                MOVE "          " TO ALPHA-RATE
                MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-INVOICE P-SLIP WS-P-SLIP
                PERFORM READ-INVOICE-REGISTER.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "*"
             IF WS-NEWORDER = "N"
                PERFORM READ-STOCK-TRANSACTIONS
                PERFORM FIND-INFO
                MOVE "Y" TO WS-ACCEPT
                            WS-REPRINT
              IF INCR-PRINTED = "T" OR = "S"
                MOVE "Y" TO WS-REPRINT
                GO TO GET-150
              ELSE
                GO TO GET-150.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "Q"
             IF WS-NEWORDER = "Y"
                MOVE "QUOTE NOT FOUND IN THE SYSTEM, CAN'T DISPLAY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-010.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "R"
             IF WS-NEWORDER = "Y"
                MOVE "REPAIR NOT FOUND IN THE SYSTEM, CAN'T DISPLAY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-010.
           IF F-EXIT-CH = X"19" OR = X"9B" OR = X"C7"
            IF F-NAMEFIELDRED1 = "*"
             IF WS-NEWORDER = "Y"
                MOVE "ORDER NOT FOUND IN THE SYSTEM, CAN'T DISPLAY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-010
            ELSE
             IF WS-NEWORDER = "C"
                MOVE " " TO WS-MESSAGE
                UNLOCK INCR-REGISTER
                PERFORM ERROR-020
                PERFORM ERROR1-020
                GO TO GET-010.
      ***************************************
      * 'F10' KEY, TO DELETE COMPLETE ORDER *
      ***************************************
           IF F-EXIT-CH = X"1F"
            IF F-NAMEFIELDRED1 = "*"
              MOVE "          " TO ALPHA-RATE
              MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
              PERFORM DECIMALISE-RATE
              MOVE NUMERIC-RATE TO WS-INVOICE
              PERFORM READ-INVOICE-REGISTER.
           IF F-EXIT-CH = X"1F"
            IF F-NAMEFIELDRED1 = "*"
             IF WS-NEWORDER = "Y"
              MOVE "ORDER NOT FOUND IN THE SYSTEM, CAN'T DELETE."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO GET-010.
           IF F-EXIT-CH = X"1F"
            IF F-NAMEFIELDRED1 = "*"
             IF WS-NEWORDER = "C"
              MOVE " " TO WS-MESSAGE
              PERFORM ERROR-020
              PERFORM ERROR1-020
              GO TO GET-010.
       GET-011.
           IF F-EXIT-CH = X"1F"
            IF F-NAMEFIELDRED1 = "*"
             IF WS-NEWORDER = "N"
              PERFORM READ-STOCK-TRANSACTIONS
              PERFORM FIND-INFO
              PERFORM CLEAR-010
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 2910 TO POS
              DISPLAY "ARE YOU SURE ABOUT DELETING THE ENTIRE ORDER."
              AT POS
              MOVE 3010 TO POS
              DISPLAY "ENTER Y TO DELETE, N TO STOP DELETION. [ ]"
                 AT POS
              ADD 40 TO POS

              MOVE 'N'       TO CDA-DATA
              MOVE 1         TO CDA-DATALEN
              MOVE 27        TO CDA-ROW
              MOVE 49        TO CDA-COL
              MOVE CDA-WHITE TO CDA-COLOR
              MOVE 'F'       TO CDA-ATTR
              PERFORM CTOS-ACCEPT
              MOVE CDA-DATA TO WS-DIS

      *        ACCEPT WS-DIS AT POS
            IF W-ESCAPE-KEY = 1 OR 2
               MOVE X"1F" TO F-EXIT-CH
               GO TO GET-012
            ELSE
               MOVE X"1F" TO F-EXIT-CH
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-011.
       GET-012.
           PERFORM ERROR-020.
           PERFORM ERROR1-020.
           IF F-EXIT-CH = X"1F"
            IF F-NAMEFIELDRED1 = "*"
             IF WS-DIS NOT = "Y" AND NOT = "N"
                GO TO GET-011.
           IF F-EXIT-CH = X"1F"
            IF F-NAMEFIELDRED1 = "*"
             IF WS-NEWORDER = "N"
              IF WS-DIS = "Y"
                PERFORM WRITE-DOCUBASE-RECORD
                PERFORM DELETE-STOCK-TRANS
                PERFORM DELETE-INVOICE-REGISTER
                PERFORM CLEAR-FIELDS
                PERFORM ERROR1-020
                PERFORM ERROR-020
                MOVE 2910 TO POS
                DISPLAY "DAILY EXCEPTION LOG BEING WRITTEN.....     "
                AT POS
                MOVE "P/SLIP  REVERSED No:" TO WS-DAILY-1ST
                MOVE WS-INVOICE             TO WS-DAILY-2ND
                MOVE "DATE OF REVERSAL IS:" TO WS-DAILY-3RD
                MOVE WS-DATE                TO SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE           TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                PERFORM GET-USER-MAIL-NAME
                PERFORM GET-REPORT-Y2K-DATE
                MOVE "ABOVE REVERSED BY  :" TO WS-DAILY-1ST
                MOVE WS-pbValue             TO WS-DAILY-2ND
                MOVE "DATE & TIME CHANGED:" TO WS-DAILY-3RD
                MOVE pbRet                  TO WS-REPORT-DATE-STRIP
                MOVE WS-STRIP2              TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                PERFORM DISPLAY-FORM
                GO TO GET-010
              ELSE
                PERFORM CLEAR-FIELDS
                PERFORM ERROR1-020
                PERFORM ERROR-020
                PERFORM DISPLAY-FORM
                GO TO GET-010.

            IF F-NAMEFIELDRED1 = " "
                MOVE "0" TO F-NAMEFIELDRED1.
            IF F-NAMEFIELDRED1 = "0" OR = "1" OR = "2" OR = "3"
             OR = "4" OR = "5" OR = "6" OR = "7" OR = "8" OR = "9"
                NEXT SENTENCE
                ELSE 
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
            MOVE F-NAMEFIELD TO WS-ACCNO-X.
            MOVE WS-ACCNO-X TO DR-ACCOUNT-NUMBER
                               WS-ACCOUNT-NUMBER.
            IF DR-ACCOUNT-NUMBER = 0
                CLOSE DEBTOR-MASTER
                CALL WS-DEBTOR-INQUIRY USING WS-LINKAGE
                CANCEL WS-DEBTOR-INQUIRY
                PERFORM CLEAR-SCREEN
                OPEN I-O DEBTOR-MASTER
                PERFORM DISPLAY-FORM
                GO TO GET-010.

           PERFORM READ-DEBTORS.
           IF DR-NAME = "UNKNOWN"
                GO TO GET-010.
           IF DR-ACCOUNT-NUMBER = 9999999 OR = 0300150
                             OR = 0300090 OR = 0300087
                             OR = 0300100 OR = 0300200
               MOVE " " TO DR-TELEPHONE
               GO TO GET-017.
           IF DR-BALANCE > DR-CREDIT-LIMIT
               COMPUTE WS-WORK-FIELD = DR-BALANCE - DR-CREDIT-LIMIT
               MOVE WS-WORK-FIELD         TO F-EDNAMEFIELDAMOUNT1
               MOVE "OVER THE CR.LIMIT:"  TO WS-DAILY-1ST
               MOVE F-EDNAMEFIELDAMOUNT1  TO WS-DAILY-2ND
               MOVE " "                   TO WS-DAILY-3RD
                                             WS-DAILY-4TH
               MOVE WS-DAILY-MESSAGE      TO WS-MESSAGE
               PERFORM ERROR-000.
      ****************************************************************
      * NEW SECTION ADDED TO ADVISE SALESMAN IF THE ACCOUNT IS OVER  *
      * THE LIMIT.  THIS WILL ONLY HAPPEN IF THE FLAG IS SET IN THE  *
      * PARAMETER FILE - INVQUES-ACC-OVER-LIMIT = "Y"                *
      ****************************************************************
           IF DR-BALANCE > DR-CREDIT-LIMIT
            IF WS-QUES-ACC-OVER-LIMIT = "Y"
             MOVE
            "ORDERS CANNOT BE INVOICED AS THE ACCOUNT IS OVER THE"
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE
            "CREDIT LIMIT, ADVISE THE ACC'S DEPARTMENT & CUSTOMER."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.

           IF DR-SUPPLY-Y-N = "N"
               MOVE "GOODS CANNOT BE SUPPLIED ON THIS A/C."
               TO WS-MESSAGE
               PERFORM ERROR1-000.
           IF DR-SUPPLY-Y-N = "S"
               MOVE
               "THIS ACCOUNT HAS BEEN SUSPENDED. CHECK WITH A/C'S DEPT."
               TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR-020
               GO TO GET-010.
       GET-017.
            MOVE "DEBTORNAME" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-NAME      TO F-NAMEFIELD
            MOVE 40           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            IF DR-NAME = "UNKNOWN"
                GO TO GET-010.
            MOVE DR-DISCOUNT-CODE  TO WS-DISCOUNT-CODE
            MOVE DR-DELIVERY-CODE  TO WSDE-CODE
            MOVE DR-SALES-ANALYSIS TO WSAN-CODE
            MOVE "POSTADD1"        TO F-FIELDNAME
            MOVE 8                 TO F-CBFIELDNAME
            MOVE WS-ADD1           TO F-NAMEFIELD
            MOVE 25                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD2" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE WS-ADD2    TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD3" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE WS-ADD3    TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTCODE"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-POSTCODE TO F-NAMEFIELD
            MOVE 4           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD1"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD1 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD2"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD2 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD3"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD3 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE WSAN-CODE        TO SA-KEY
            PERFORM READ-SALES-ANALYSIS
            MOVE "SALESANALYSIS"  TO F-FIELDNAME
            MOVE 13               TO F-CBFIELDNAME
            MOVE WS-SALESANALYSIS TO F-NAMEFIELD
            MOVE 14               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM READ-SBREP
            MOVE "SOLDBY"   TO F-FIELDNAME
            MOVE 6          TO F-CBFIELDNAME
            MOVE WS-SOLD-BY TO F-NAMEFIELD
            MOVE 2          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GSTNO"  TO F-FIELDNAME
            MOVE 5        TO F-CBFIELDNAME
            MOVE DR-GSTNO TO F-NAMEFIELD WS-GSTNO
            MOVE 13       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PHONE"      TO F-FIELDNAME
            MOVE 5            TO F-CBFIELDNAME.
            MOVE DR-TELEPHONE TO F-NAMEFIELD WS-PHONE
            MOVE 20           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESMAN"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE DR-SALESMAN  TO F-NAMEFIELD
            MOVE 1            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-AREA" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE INCR-AREA  TO F-NAMEFIELD.
            MOVE 1          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE DR-PART-ORDERS TO WS-PART-ORDERS.
            MOVE "PART-ORDERS"  TO F-FIELDNAME.
            MOVE 11             TO F-CBFIELDNAME.
            MOVE WS-PART-ORDERS TO F-NAMEFIELD.
            MOVE 1              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERVIA"     TO F-FIELDNAME
            MOVE 10               TO F-CBFIELDNAME
            MOVE DR-DELIVERY-CODE TO WS-DEL-SUB
            MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA
            MOVE WS-DELIVERVIA    TO F-NAMEFIELD
            MOVE 20               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TERMOFSALE"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE DR-TERMS-CODE TO WS-TERM-SUB
            MOVE WS-ST-TERM (WS-TERM-SUB) TO WS-TERMOFSALE
            MOVE WS-TERMOFSALE TO F-NAMEFIELD
            MOVE 11            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICEDATE" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE WS-DATE       TO WS-INVOICEDATE SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE  TO F-NAMEFIELD
            MOVE 10            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM ERROR-020
            PERFORM ERROR1-020
            GO TO GET-095.
       GET-020.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DEBTORNAME" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
             IF WS-NEWORDER NOT = "Y"
              IF WS-MUST-PRINT = "Y"
                MOVE "THE ORDER HAS BEEN CHANGED, PRINT THIS COPY."
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-150.
            IF F-EXIT-CH = X"01"
             IF WS-NEWORDER = "N"
              IF WS-MUST-PRINT NOT = "Y"
                PERFORM CANCEL-INVOICE
                PERFORM ERROR1-020
                GO TO GET-010.
            IF F-EXIT-CH = X"01"
              IF WS-NEWORDER = "Y"
               IF WS-ACCEPT = "Y"
                PERFORM DELETE-STOCK-TRANS
                PERFORM RIR-000 THRU RIR-005
                PERFORM DELETE-INVOICE-REGISTER
                PERFORM CI-950
                MOVE 0 TO DR-ACCOUNT-NUMBER
                          WS-ACCOUNT-NUMBER
                PERFORM CLEAR-FIELDS
                MOVE " " TO WS-BINNO
                GO TO GET-010
               ELSE
                PERFORM CI-950
                MOVE 0 TO DR-ACCOUNT-NUMBER
                          WS-ACCOUNT-NUMBER
                PERFORM CLEAR-FIELDS
                MOVE " " TO WS-BINNO
                GO TO GET-010.
            MOVE 40 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-NAME.
       GET-030.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-020.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ADD1.
       GET-040.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-030.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ADD2.
       GET-050.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-040.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ADD3.
       GET-060.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "POSTCODE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-050.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-POSTCODE.
       GET-070.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"19"
               MOVE 25 TO F-CBFIELDLENGTH
               PERFORM READ-FIELD-ALPHA
               MOVE F-NAMEFIELD TO F-NAMEFIELDRED
               PERFORM GET-DEL-INFO
               GO TO GET-070.
            IF F-EXIT-CH = X"01"
                GO TO GET-060.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DELADD1.
            IF WS-DELADD1 = "  "
                GO TO GET-070.
       GET-080.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-070.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DELADD2.
            IF WS-DELADD2 = "  "
                GO TO GET-080.
       GET-090.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-080.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DELADD3.
       GET-095.
            IF WS-DELADD1 = "  "
                GO TO GET-070.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "PHONE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-070.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-PHONE.
            IF WS-PHONE = "    "
               GO TO GET-095.
       GET-096.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "CONTACTNAME" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-095.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-CONTACT.
            IF WS-CONTACT = "    "
               GO TO GET-096.
               
            IF WS-NEWORDER = "N"
             IF F-NAMEFIELDRED1 = "*"
               GO TO GET-115.
       GET-110.
           MOVE "Y"           TO WS-NEWORDER
           MOVE WS-P-SLIP     TO WS-INVOICE
           MOVE SPACES        TO F-NAMEFIELD.
           IF F-NAMEFIELDRED1 NOT = "Q" AND NOT ="R"
              MOVE "COMMENTLINE" TO F-FIELDNAME
              MOVE 11            TO F-CBFIELDNAME
              MOVE 30            TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ALPHA.

           MOVE " "         TO WS-POORDERNO
           MOVE "POORDERNO" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD
           PERFORM ERROR-020.
           IF F-EXIT-CH = X"01"
               GO TO GET-096.
           MOVE 20 TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD TO WS-POORDERNO.
           
           IF WS-NEWORDER = "Y"
      *      IF WS-POORDERNO NOT = INCR-PORDER
               PERFORM CHECK-REGISTER.
       GET-111.
           PERFORM CLEAR-010.
           
           IF WS-NEWORDER = "N" OR = "P"
            IF WS-MUST-PRINT = "Y"
             MOVE "THERE IS AN ORDER WITH THIS P/O NUMBER=" 
                 TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE "  PLEASE RE-ENTER THE P/O NUMBER." TO WS-MESSAGE
             PERFORM ERROR-000
             MOVE 2755 TO POS
             DISPLAY INCR-PORDER  AT POS
             MOVE " " TO INCR-PORDER
             UNLOCK INCR-REGISTER
             MOVE 0 TO INCR-INVOICE
             GO TO GET-110.
           IF WS-NEWORDER = "N" OR = "P"
           IF WS-ABOVE-BODY = "1"
             MOVE "THERE IS AN ORDER WITH THAT P/O NUMBER=" 
                 TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE "  PLEASE RE-ENTER THE P/O NUMBER." TO WS-MESSAGE
             PERFORM ERROR-000
             MOVE " " TO INCR-PORDER
             UNLOCK INCR-REGISTER
             MOVE 0 TO INCR-INVOICE
             GO TO GET-110.
           
           IF WS-NEWORDER = "C"
               MOVE 2910 TO POS
                DISPLAY "ORDER ALREADY INVOICED, INVOICE No:" AT POS
                ADD 35 TO POS
                MOVE INCR-BO-INV-NO TO F-EDNAMEFIELDNUM
                DISPLAY F-EDNAMEFIELDNUM AT POS
                PERFORM ERROR-010
                MOVE " " TO WS-MESSAGE
                MOVE 2910 TO POS
                DISPLAY WS-MESSAGE AT POS
             IF F-NAMEFIELDRED1 NOT = "Q" AND NOT "R"
                UNLOCK INCR-REGISTER
                GO TO GET-110
             ELSE
                GO TO GET-110.
           IF WS-NEWORDER = "Y"
              GO TO GET-115.
           IF WS-NEWORDER = "N"
            IF F-NAMEFIELDRED1 = "Q" OR = "R"
              GO TO GET-115.
           IF INCR-PRINTED = "P"
             MOVE "THE ORDER IS IN THE STORE READY FOR PULLING, DON'T"
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE "CREATE A 2nd SLIP BEFORE THE ORIGINAL IS RETURNED."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             MOVE " " TO INCR-PORDER
             IF F-NAMEFIELDRED1 NOT = "Q" AND NOT = "R"
                UNLOCK INCR-REGISTER
                GO TO GET-110
             ELSE
                GO TO GET-110.
           IF WS-NEWORDER = "N"
              MOVE 2910 TO POS
              DISPLAY "  THIS ORDER HAS ALREADY BEEN ENTERED," AT POS
              MOVE 3010 TO POS
              DISPLAY "DO YOU WISH TO SEE THE OLD ORDER, Y OR N:"
              AT POS.
            ADD 43 TO POS
           
           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DIS.

            IF W-ESCAPE-KEY = 1 OR 2
               GO TO GET-112
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               UNLOCK INCR-REGISTER
               MOVE 0 TO INCR-INVOICE
               GO TO GET-110.
       GET-112.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           IF WS-DIS = "Y"
               PERFORM CHECK-PASSWORD
            IF WS-PASSWORD-VALID = "N"
                MOVE " " TO INCR-PORDER
                UNLOCK INCR-REGISTER
                MOVE 0 TO INCR-INVOICE
                GO TO GET-110.
           IF WS-DIS = "Y"
               PERFORM READ-INVOICE-REGISTER
             IF WS-NEWORDER = "C" OR = "Y"
               GO TO GET-010
             ELSE
               PERFORM READ-STOCK-TRANSACTIONS
               PERFORM FIND-INFO
               MOVE "Y" TO WS-ACCEPT
               MOVE "*" TO F-NAMEFIELDRED1
               GO TO GET-150.
           IF WS-DIS = "N"
                MOVE " " TO INCR-PORDER
                UNLOCK INCR-REGISTER
                MOVE 0 TO INCR-INVOICE
                GO TO GET-110.
           DISPLAY " " AT 3079 WITH BELL
           GO TO GET-111.
       GET-115.
      ****************************************************
      *DEL-CODE = 5 IS FOR 'ROAD FREIGHT VARIOUS'        *
      ****************************************************
            IF DR-DELIVERY-CODE = 5
             IF WS-SPECIAL-INFO NOT = "Y"
               PERFORM GET-DEL-INFO.
               
            IF WS-SOLD-BY NOT = "  "
               GO TO GET-120.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "SOLDBY" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"01"
             IF WS-NEWORDER = "Y"
              IF WS-MUST-PRINT = "N"
                GO TO GET-110
             ELSE
                GO TO GET-090.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-SOLD-BY.
            MOVE 2 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            IF WS-SOLD-BY = "  "
                GO TO GET-115.
            GO TO GET-118.
       GET-117.
            MOVE WS-AREA    TO INCR-AREA.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DEL-AREA" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE INCR-AREA  TO F-NAMEFIELD.
            MOVE 1          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR-AREA WS-AREA.
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            IF INCR-AREA = "  "
                GO TO GET-117.
       GET-118.
            MOVE "PART-ORDERS"  TO F-FIELDNAME.
            MOVE 11             TO F-CBFIELDNAME.
            MOVE WS-PART-ORDERS TO F-NAMEFIELD.
            MOVE 1              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR-PART-ORDERS WS-PART-ORDERS.
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"01"
                GO TO GET-110.
            IF INCR-PART-ORDERS NOT = "N" AND NOT = "Y"
                GO TO GET-118.
       GET-120.
            IF F-EXIT-CH = X"1D"
                GO TO GET-121.
            GO TO GET-150.
       GET-121.
            MOVE WSAN-CODE TO WSAN-CODE-SAVE.
            MOVE WS-SALESANALYSIS TO WS-SALESANALYSIS-SAVE.
       GET-125.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "SALESANALYSIS" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE 0 TO F-CBFIRSTLINE.
            MOVE 14 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"01"
                PERFORM READ-FIELD-ALPHA
                IF F-NAMEFIELD = WS-SALESANALYSIS
                GO TO GET-115
            ELSE
                GO TO GET-125.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = WS-SALESANALYSIS
                IF WSAN-CODE = " 6" OR = "6 " OR = "06" OR = "52"
                AND WS-GSTNO = "EXPORT" OR = "EXPORT      "
                GO TO GET-130.
            IF F-NAMEFIELD = WS-SALESANALYSIS
                MOVE WSAN-CODE-SAVE TO WSAN-CODE
                GO TO GET-130.
            MOVE F-NAMEFIELD TO WS-SALESANALYSIS.
            MOVE WS-SALESANALYSIS TO SPLIT-ANALYSIS.
            MOVE F-NAMEFIELD TO SPLIT-ANALYSIS.
            MOVE WSAN-CODE-2 TO ALPHA-FIELD.
            IF WSAN-CODE-1 NOT = "1" AND NOT = "2" AND NOT = "3"
                       AND NOT = "4" AND NOT = "5" AND NOT = "6"
                       AND NOT = "7" AND NOT = "8" AND NOT = "9"
                       AND NOT = "0"
                MOVE "INVALID SALES ANALYSIS!!!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE WSAN-CODE-SAVE TO WSAN-CODE
                MOVE WS-SALESANALYSIS-SAVE TO WS-SALESANALYSIS
                GO TO GET-125.
            IF ALPHA-VALUE
                MOVE WSAN-CODE-1 TO WSAN-CODE-2
                MOVE "0" TO WSAN-CODE-1.
            PERFORM READ-SALES-ANALYSIS.
            IF WS-SALESANALYSIS = "UNKNOWN"
                MOVE "INVALID SALES ANALYSIS!!!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE WSAN-CODE-SAVE TO WSAN-CODE
                MOVE WS-SALESANALYSIS-SAVE TO WS-SALESANALYSIS
                GO TO GET-125.
            IF WSAN-CODE = " 6" OR = "6 " OR = "06" OR = "52"
               MOVE "GSTNO" TO F-FIELDNAME
               MOVE 5 TO F-CBFIELDNAME
               MOVE "EXPORT" TO F-NAMEFIELD WS-GSTNO
               MOVE 13 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.
            MOVE "SALESANALYSIS" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE WS-SALESANALYSIS TO F-NAMEFIELD.
            MOVE 14 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            IF WS-SALESANALYSIS = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                MOVE WSAN-CODE-SAVE TO WSAN-CODE
                MOVE WS-SALESANALYSIS-SAVE TO WS-SALESANALYSIS
                GO TO GET-125.
       GET-130.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DELIVERVIA" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
             IF WS-NEWORDER = "Y"
                GO TO GET-118
             ELSE
                GO TO GET-096.
            IF F-EXIT-CH = X"1D"
                GO TO GET-120.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-FIELD NUMERIC-FIELD WS-DELIVERVIA.
            IF NOT NUMERIC-VALUE
                GO TO GET-140.
            IF ALPHA-VALUE
                GO TO GET-140.
            MOVE WS-DELIVERVIA TO SPLIT-DELIVERVIA.
            MOVE WSDE-CODE TO WS-DEL-SUB.
            IF WS-DEL-SUB = 0
               MOVE 1 TO WS-DEL-SUB.
            MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA.
            IF WS-DELIVERVIA = ALL "X"
                MOVE "INVALID DELIVERY CODE, PLEASE RE-ENTER!"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-130.
            MOVE "DELIVERVIA" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-DELIVERVIA TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
      ****************************************************
      *DEL-CODE = 5 IS FOR 'ROAD FREIGHT VARIOUS'        *
      ****************************************************
            IF WS-DEL-SUB = 5
             IF WS-SPECIAL-INFO NOT = "Y"
               PERFORM GET-DEL-INFO.
       GET-140.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "TERMOFSALE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-130.
            MOVE 0 TO F-CBFIRSTLINE.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-FIELD NUMERIC-FIELD.
            IF NOT NUMERIC-VALUE
                GO TO GET-150.
            IF ALPHA-VALUE
                GO TO GET-150.
            MOVE F-NAMEFIELD TO WS-TERMOFSALE.
            MOVE WS-TERMOFSALE TO SPLIT-TERMOFSALE.
            MOVE WSTE-CODE TO WS-TERM-SUB.
            IF WS-TERM-SUB = 0
               MOVE 1 TO WS-TERM-SUB.
            MOVE WS-ST-TERM (WS-TERM-SUB) TO WS-TERMOFSALE.
            IF WS-TERMOFSALE = ALL "X"
                MOVE "INVALID TERM CODE, PLEASE RE-ENTER!"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-140.
            MOVE "TERMOFSALE"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-TERMOFSALE TO F-NAMEFIELD
            MOVE 11            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-150.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FIELDS
                PERFORM ERROR1-020
                PERFORM ERROR-020
                PERFORM DISPLAY-FORM
                GO TO GET-010.
      *****************************
      *RE-ENTER THE DEL-AREA CODE *
      *****************************
            PERFORM ERROR1-020.
            PERFORM GET-117.
            PERFORM ERROR1-020.
            IF WS-SALESANALYSIS = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-121.
            IF WS-BO-NUMBER > 0
               MOVE WS-BO-NUMBER  TO WS-BONUMBER
               MOVE WS-BO-MESSAGE TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
            IF DR-SUPPLY-Y-N = "S"
                MOVE 
           "PRESS 'ESC' TO CLEAR THE SCREEN, ACCOUNT SUSPENDED."
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                UNLOCK INCR-REGISTER
            IF WS-NEWORDER = "Y"
                PERFORM DELETE-STOCK-TRANS
                PERFORM ERROR1-020
                PERFORM DISPLAY-FORM
                GO TO GET-010
             ELSE
                PERFORM ERROR1-020
                PERFORM CLEAR-SCREEN
                PERFORM CLEAR-FIELDS
                PERFORM DISPLAY-FORM
                GO TO GET-010.
      ***************************************************************
      *SECTION TO CHECK IF ACCOUNT IS ALREADY OVER THE CREDIT LIMIT.*
      ***************************************************************
           IF DR-BALANCE > DR-CREDIT-LIMIT
               COMPUTE WS-ORDERTOTAL = 
                  DR-BALANCE - DR-CREDIT-LIMIT
            IF WS-QUES-ACC-OVER-LIMIT = "Y"
             MOVE 
            "THE ACCOUNT IS OVER THE CREDIT CR-LIMIT BY R"
             TO WS-MESSAGE
             PERFORM ERROR-000
                MOVE WS-ORDERTOTAL TO P-PRICE
                MOVE 2859 TO POS
                DISPLAY P-PRICE AT POS
                PERFORM ERROR-010
             MOVE 
            "THIS ORDER CAN'T BE INVOICED AS THE ACCOUNT IS OVER THE"
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE
            "CREDIT LIMIT, PLEASE ADVISE ACC'S DEPARTMENT & CUSTOMER."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.

            MOVE "                        " TO F-NAMEFIELD
            MOVE "BINNO" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"01"
                GO TO GET-140.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-BINNO.
            IF WS-GSTNO = "EXPORT"
               AND WSAN-CODE NOT = "6 " AND NOT = " 6"
                         AND NOT = "06" AND NOT = "52"
              MOVE "SALES ANALYSIS MUST BE 6 OR 52 IF GST-NO = EXPORT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-120.
      *F8-KEY
            IF WS-NEWORDER = "Y"
             IF WS-ABOVE-BODY NOT = "1"
                MOVE 0 TO WS-INVOICEDISCOUNT.
            IF F-EXIT-CH NOT = X"1D"
                MOVE 1 TO SUB-1 F-INDEX
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                GO TO GET-180.
       GET-160.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 13      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-GSTNO.
            IF F-EXIT-CH = X"01"
                GO TO GET-150.
       GET-170.
            IF WS-GSTNO = "EXPORT       "
               AND WSAN-CODE NOT = "6 " AND NOT = " 6"
                         AND NOT = "06" AND NOT = "52"
              MOVE "SALES ANALYSIS MUST BE 6 OR 52 IF GST-NO = EXPORT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-120.
            IF WSAN-CODE = "6 " OR = " 6" OR = "06" OR = "52"
             IF WS-GSTNO NOT = "EXPORT       " AND NOT = "EXPORT"
                MOVE
               "SALES ANALYSIS CAN'T BE 6 OR 52 IF GST-NO NOT EXPORT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-120.
            IF F-EXIT-CH NOT = X"1D"
                GO TO GET-172.
       GET-171.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "INVOICEDATE" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-160.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO GET-171.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-INVOICEDATE SPLIT-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO GET-171.
           IF SPLIT-MM NOT = WS-MM
             OR SPLIT-YY NOT = WS-YY
               MOVE
           "YOU MAY NOT ENTER A DATE WHICH IS OUT OF THIS MONTH."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-171.
       GET-172.
            IF F-EXIT-CH NOT = X"1D"
                GO TO GET-177.
            PERFORM CHECK-PASSWORD.
            IF WS-PASSWORD-VALID = "N"
                GO TO GET-177.
       GET-175.
            MOVE "                        " TO F-NAMEFIELD.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
            IF WS-ABOVE-BODY NOT = "1"
               MOVE 0   TO WS-INVOICEDISCOUNT
               MOVE "N" TO WS-ZERODIS.
               
           MOVE 3010 TO POS
           DISPLAY
           "IF YOU WISH TO ZERO THE DISCOUNT ENTER 0 AND PRESS <F8>."
              AT POS.
            MOVE "INVOICEDISCOUNT"  TO F-FIELDNAME.
            MOVE 15                 TO F-CBFIELDNAME.
            MOVE WS-INVOICEDISCOUNT TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5                  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"1D"
               MOVE "Y" TO WS-ZERODIS
               PERFORM CHANGE-DISCOUNT-PER-LINE.
            MOVE 5            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-INVOICEDISCOUNT.
            MOVE WS-INVOICEDISCOUNT TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5                  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF WS-INVOICEDISCOUNT > 0
               PERFORM CHANGE-DISCOUNT-PER-LINE.
       GET-177.
            PERFORM SET-GST.
            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM SCROLL-PREVIOUS.
       GET-180.
            IF WS-NEWORDER = "Y"
             IF F-NAMEFIELDRED1 NOT = "Q" AND NOT ="R"
               PERFORM CHECK-TO-ENTER-ORDER.

            IF WS-ACCEPT = "N"
                PERFORM CI-950
                MOVE 0 TO DR-ACCOUNT-NUMBER
                          WS-ACCOUNT-NUMBER
                PERFORM CLEAR-FIELDS
                GO TO GET-010.
            MOVE "N" TO WS-LINECHANGED.
            PERFORM FILL-BODY.
            IF WS-ABOVE-BODY = "1"
                GO TO GET-150.
       GET-185.
      ****************************************************************
      *SECTION TO CHECK IF THE NEW ORDER TO BE INVOICED WILL PUSH THE*
      *ACCOUNT OVER THE CREDIT LIMIT.                                *
      ****************************************************************
            PERFORM GET-240 THRU GET-270
             IF DR-BALANCE + WS-SUBTOTAL > DR-CREDIT-LIMIT
               COMPUTE WS-ORDERTOTAL = 
                  DR-BALANCE + WS-SUBTOTAL + WS-TAXAMT 
                             - DR-CREDIT-LIMIT
            IF WS-QUES-ACC-OVER-LIMIT = "Y"
             MOVE 
            "NEW ORDER WILL PUSH THE ACC OVER CR-LIMIT BY R"
             TO WS-MESSAGE
             PERFORM ERROR-000
                MOVE WS-ORDERTOTAL TO P-PRICE
                MOVE 2861 TO POS
                DISPLAY P-PRICE AT POS
                PERFORM ERROR-010
             MOVE
            "THE ORDER CANNOT BE INVOICED AS THE ACCOUNT WILL BE OVER"
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE
            "THE CREDIT LIMIT, ADVISE THE ACCS DEPARTMENT & CUSTOMER."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
       GET-190.
            MOVE 1 TO F-INDEX.
            IF F-NAMEFIELDRED1 NOT = "Q" AND NOT = "R"
             IF WS-NEWORDER = "Y"
               PERFORM CLEAR-BOTTOM-FIELDS
               MOVE 0 TO WS-ADDONFREIGHT
                         WS-POSTADDON
                         WS-HANDADDON
                         WS-MISCADDON.
            MOVE 1 TO F-INDEX
            PERFORM GET-240 THRU GET-245
            PERFORM GET-250
            PERFORM GET-260
            PERFORM GET-270
            MOVE "COMMENTLINE" TO F-FIELDNAME
            MOVE 11 TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
             IF WS-NEWORDER = "Y"
                MOVE 1 TO F-INDEX 
                MOVE "1" TO WS-ABOVE-BODY
                PERFORM CLEAR-BOTTOM-FIELDS
                MOVE 0 TO WS-ADDONFREIGHT
                          WS-POSTADDON
                          WS-HANDADDON
                          WS-MISCADDON
                MOVE 1 TO F-INDEX SUB-1
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                GO TO GET-180
              ELSE
                MOVE 1 TO F-INDEX SUB-1
                MOVE "1" TO WS-ABOVE-BODY
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                GO TO GET-180.
            MOVE 30 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-COMMENTLINE.
      *F8-KEY.
            IF F-EXIT-CH = X"1D"
                GO TO GET-200.
            IF WS-IMM-PR = "Y"
                GO TO GET-240.
       GET-200.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "ADDONFREIGHT"        TO F-FIELDNAME
            MOVE 12                    TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-190.
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-ADDONFREIGHT
                                 F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       GET-210.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "POSTADDON"           TO F-FIELDNAME
            MOVE 9                     TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-200.
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-POSTADDON
                                 F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       GET-220.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "HANDADDON"           TO F-FIELDNAME
            MOVE 9                     TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-210.
            MOVE 9 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-HANDADDON
                                 F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       GET-230.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "MISC.ADDON"          TO F-FIELDNAME
            MOVE 10                    TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-220.
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-MISCADDON
                                 F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
            
      *X"55" = POSITION 21 IN THE PSWD TABLE OF 92
      *
            IF F-EXIT-CH = X"1D"
               MOVE X"55" TO F-EXIT-CH
               PERFORM CHECK-PASSWORD
            ELSE
               GO TO GET-240.
            IF WS-PASSWORD-VALID = "N"
                MOVE
            "TO ZERO TAX, TAX NUM MUST = EXPORT AND SALES CODE 6 OR 52."
                  TO WS-MESSAGE
                  PERFORM ERROR-MESSAGE
                GO TO GET-240
            ELSE
                PERFORM TAX-ONLY
                GO TO GET-245.
       GET-240.
            PERFORM CALCULATE-TOTALS.
       GET-245.
            MOVE "SUBTOTAL"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-SUBTOTAL TO F-EDNAMEFIELD9MIL
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.
       GET-250.
            MOVE "ADDONAMT"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-ADDONAMT TO F-EDNAMEFIELD9MIL
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.
       GET-260.
            MOVE "TAXAMT"    TO F-FIELDNAME
            MOVE 6           TO F-CBFIELDNAME
            MOVE WS-TAXAMT   TO F-EDNAMEFIELD9MIL
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.
       GET-270.
            COMPUTE WS-INVOICETOTAL = WS-SUBTOTAL +
                                      WS-TAXAMT + 
                                      WS-ADDONAMT.
            MOVE "INVOICETOTAL"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE WS-INVOICETOTAL TO F-EDNAMEFIELD9MIL
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.
       GET-900.
      *
      ******INCR-PRINTED KEY CONFIGURATIONS.****
      *<F4> =X"18":"S"  =SUSPEND               *
      *<F10>=X"1F":"R"  =RUSH JOB              *
      *<RET>=X"0A":"P"  =PRINT NEW/REPRINT OLD *
      *<GO> =X"1B":"P"  =DITTO                 *
      ******************************************
           MOVE 2701 TO POS
           DISPLAY
           "<F10> = Rush-Job Counter, <Return> / <Go> = Print " &
           "In Batch, & <F4> = Suspend Order" AT POS
           MOVE 2805 TO POS
           DISPLAY
           "NB! Ship Via = 'Collect At Counter' Will Print Immediately"
              AT POS.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
            IF F-NAMEFIELDRED1 NOT = "D"
              GO TO GET-190
            ELSE
              GO TO GET-900.
           IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                    AND NOT = X"18" AND NOT = X"1F"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-900.
       GET-910.
           PERFORM ERROR-020
           PERFORM ERROR1-020
           IF F-NAMEFIELDRED1 NOT = "Q" AND NOT = "R"
              ADD  1 TO INCR-COPY-NUMBER
           ELSE
              MOVE 1 TO INCR-COPY-NUMBER.
           IF F-EXIT-CH = X"18"
            IF INCR-COPY-NUMBER > 1
              PERFORM ERROR1-020
              MOVE
            "Only NEW P/Slip's can be SUSPENDED, <ESC> To Re-Enter."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               SUBTRACT 1 FROM INCR-COPY-NUMBER
               GO TO GET-270.
           IF WS-WAS-SUSPENDED = "Y"
              MOVE 1 TO INCR-COPY-NUMBER.
           IF F-EXIT-CH = X"0A" OR = X"1B"
              MOVE "P" TO INCR-PRINTED
              GO TO GET-999.
           IF F-EXIT-CH = X"18"
      *      IF INCR-PRINTED NOT = "P"
              MOVE "S" TO INCR-PRINTED
              GO TO GET-999.
      *      ELSE
      *        MOVE "T" TO INCR-PRINTED
      *        GO TO GET-999.
           IF F-EXIT-CH = X"1F"
              MOVE "R" TO INCR-PRINTED
              GO TO GET-999.
       GET-999.
            EXIT.
      *
       CHANGE-DISCOUNT-PER-LINE SECTION.
       CDPL-010.
            MOVE 1 TO SUB-1.
       CDPL-020.
            IF B-STOCKNUMBER (SUB-1) = " "
                GO TO CDPL-999.
            
            IF WS-INVOICEDISCOUNT = 0
             IF F-EXIT-CH = X"1D"
               MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1)
               GO TO CDPL-030
             ELSE
               GO TO CDPL-030.
            IF B-SPECIAL (SUB-1) = "Y"
               GO TO CDPL-030.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
             IF SP-1STCHAR = "/"
                MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1)
                GO TO CDPL-030.
               
      *THE NEXT FEW LINES WERE ADDED TO CHECK IF THE DISCOUNT
      * FOR THE ORDER IS > THE MAX ITEM DISCOUNT AND IF SO
      * TO DISALLOW IT.
      
            MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
            PERFORM READ-STOCK.
            PERFORM READ-SPECIAL-DISC-ACC.

            IF WS-INVOICEDISCOUNT < STDISC-PERCENT
               MOVE STDISC-PERCENT TO B-DISCOUNTPERITEM (SUB-1)
               GO TO CDPL-030.
            IF WS-INVOICEDISCOUNT > STDISC-PERCENT
             IF STDISC-PERCENT NOT = 0
               MOVE STDISC-PERCENT TO B-DISCOUNTPERITEM (SUB-1)
               GO TO CDPL-030.
            
            IF STDISC-PERCENT = 0
             IF WS-INVOICEDISCOUNT NOT > B-MAX-DISC (SUB-1)
               MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1)
             ELSE
               MOVE B-MAX-DISC (SUB-1) TO B-DISCOUNTPERITEM (SUB-1).
       CDPL-030.
            ADD 1 TO SUB-1.
            IF SUB-1 < 201
               GO TO CDPL-020.
       CDPL-999.
            EXIT.
      *
       READ-SBREP SECTION.
       RSB-005.
           IF DR-SALESMAN = " " OR = "0"
              MOVE " " TO WS-SALESMAN
              GO TO RSB-999.
       RSB-010.
           OPEN I-O SBREP-MASTER.
           IF WS-SBREP-ST1 NOT = 0 
              MOVE "SBREP FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-SBREP-ST1
              GO TO RSB-010.
       RSB-020.
           MOVE DR-SALESMAN TO SBREP-REP.
           START SBREP-MASTER KEY NOT < SBREP-KEY.
       RSB-030.
           READ SBREP-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-SBREP-ST1 = 23 OR 35 OR 49
              MOVE "  " TO WS-SOLD-BY
                           WS-SALESMAN
              GO TO RSB-040.
           IF WS-SBREP-ST1 NOT = 0 
              MOVE "SBREP FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-SBREP-ST1
              GO TO RSB-030.
           IF WS-SOLD-BY = " "
              MOVE SBREP-INT  TO WS-SOLD-BY.
           MOVE SBREP-REPNAME TO WS-SALESMAN.
       RSB-040.
           CLOSE SBREP-MASTER.
       RSB-999.
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
            IF WS-ABOVE-BODY = " "
                 ADD 1 TO WS-STTRANS-NO.
            MOVE " " TO WS-ABOVE-BODY
            MOVE 1 TO SUB-1 SUB-2 SUB-3.
       FILL-005.
            PERFORM ERROR-020.
            MOVE 2710 TO POS
            DISPLAY
            "PRESS <ALT-Z> TO GO INTO ZOOMBOX MODE TO CALL UP STOCKINQ."
               AT POS.
            MOVE 2810 TO POS
            DISPLAY
             "PRESS <ALT-F12> TO SWITCH BETWEEN SHOWING COSTS OR NOT."
               AT POS.
               
            MOVE 3010 TO POS
            DISPLAY "    BODY LINE: " AT POS
            ADD 15 TO POS
            MOVE SUB-1 TO WS-BODY-LINE
            DISPLAY WS-BODY-LINE AT POS.

            MOVE WS-LASTPASSWORD TO WS-PASSWORDSAVED.
       FILL-010.
            IF WS-LASTPASSWORD = " "  
               MOVE WS-PASSWORDSAVED TO WS-LASTPASSWORD.
            PERFORM RUNNING-TOTAL.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                PERFORM SCROLL-050.
                
            MOVE ALL SPACES    TO F-NAMEFIELD
            MOVE "STOCKNUMBER" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            MOVE 15            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD   TO SPLIT-STOCK
            PERFORM FILL-005.
      ************************************************************
      *X"E3" = <CODE-c>  X"C3"=<CODE-SHIFT-c>   X"63"=<c>        *
      *USED TO MAKE COMMENT COMPLETE, NOT PRINTED ON NEXT INVOICE*
      *************************************************************
           IF F-EXIT-CH = X"E3" OR = X"C3" OR = X"63"
            IF SP-1STCHAR = "*"
             IF B-NEWLINE (SUB-1) NOT = "L"
                MOVE "L" TO B-NEWLINE (SUB-1)
                PERFORM SCCO-010
                MOVE "Y" TO WS-MUST-PRINT
                GO TO FILL-010
             ELSE
                MOVE "N" TO B-NEWLINE (SUB-1)
                PERFORM SCCO-010
                MOVE "Y" TO WS-MUST-PRINT
                GO TO FILL-010.

            IF F-EXIT-CH = X"0B" 
              IF B-STOCKNUMBER (SUB-1) = " "
                GO TO FILL-010.
            IF F-EXIT-CH = X"0B" OR = X"0A"
             IF SP-1STCHAR = "*"
                PERFORM SCCO-010.
            IF F-EXIT-CH = X"0A" 
             IF B-STOCKNUMBER (SUB-1) NOT = " "
              IF F-INDEX = 7
                PERFORM SCROLL-NEXT
                GO TO FILL-010
              ELSE
                ADD 1 TO SUB-1 F-INDEX
                GO TO FILL-010.

      *<CODE-b> = GO TO BEGINNING
           IF F-EXIT-CH = X"E2" OR = X"C2" OR = X"62"
                MOVE 0 TO SUB-1
                PERFORM SCROLL-NEXT
                GO TO FILL-005.
      *<CODE-m> = GO TO MIDDLE
           IF F-EXIT-CH = X"ED" OR = X"CD" OR = X"6D"
                     OR = X"0F" OR = X"8F"
             IF SUB-25 > 10
                MOVE SUB-25 TO SUB-1
                COMPUTE SUB-1 = SUB-1 / 2
                PERFORM SCROLL-NEXT
                GO TO FILL-005
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-005.
      *<CODE-e> = GO TO END
           IF F-EXIT-CH = X"E5" OR = X"C5" OR = X"65"
            IF SUB-25 > 14
                MOVE SUB-25 TO SUB-1
                SUBTRACT 3 FROM SUB-1
                PERFORM SCROLL-NEXT
                GO TO FILL-005
            ELSE
                DISPLAY " " AT 3079 WITH BELL
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
                PERFORM FIND-010 THRU FIND-020
                PERFORM CALC-POS-OF-CURSOR
                GO TO FILL-005.

            IF F-EXIT-CH = X"01" AND F-INDEX = 1
             IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
              IF WS-MUST-PRINT = "Y"
               MOVE
            "YOU SHOULD PRINT THIS ORDER AS A CHANGE HAS BEEN MADE."
               TO WS-MESSAGE
               PERFORM ERROR-000
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999
              ELSE
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999.
           IF F-EXIT-CH = X"01" AND F-INDEX = 1
             IF F-NAMEFIELD NOT = B-STOCKNUMBER (SUB-1)
                MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
      *        IF SP-1STCHAR NOT = "*" AND NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                MOVE "THE ORDER HAS BEEN CHANGED, PRINT THIS COPY."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999
             ELSE
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999.

            IF F-EXIT-CH = X"01"
             IF B-STOCKNUMBER (SUB-1) = " "
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-005.

            IF F-EXIT-CH = X"01" AND F-INDEX > 1
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-005
              ELSE
      *        IF SP-1STCHAR NOT = "*" AND NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15                    TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-005.

            IF F-EXIT-CH = X"0B" AND F-INDEX < 7
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                ADD 1 TO F-INDEX SUB-1
                GO TO FILL-005
              ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15                    TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                ADD 1 TO F-INDEX SUB-1
                GO TO FILL-005.
            IF F-EXIT-CH = X"0B" AND F-INDEX = 7
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                PERFORM SCROLL-NEXT
                GO TO FILL-005
              ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15                    TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM SCROLL-NEXT
                GO TO FILL-005.
      *****************************
      *X"91"= DISPLAY COST ON/OFF *
      *****************************
           IF F-EXIT-CH = X"91"
              MOVE WS-LASTPASSWORD TO WS-PASSWORDSAVED
              PERFORM CHECK-PASSWORD.
           IF F-EXIT-CH = X"91"
            IF WS-PASSWORD-VALID = "Y"
             IF WS-COST-DISPLAY = "N"
                MOVE "Y" TO WS-COST-DISPLAY
                PERFORM SCROLL-NEXT-PAGE
                PERFORM SCROLL-PREVIOUS
                GO TO FILL-010
             ELSE
                MOVE "N" TO WS-COST-DISPLAY
                PERFORM SCROLL-NEXT-PAGE
                PERFORM SCROLL-PREVIOUS
                GO TO FILL-010.
           IF F-EXIT-CH = X"91"
            IF WS-PASSWORD-VALID = "N"
             IF WS-COST-DISPLAY = "Y"
                MOVE "N" TO WS-COST-DISPLAY
                PERFORM SCROLL-NEXT-PAGE
                PERFORM SCROLL-PREVIOUS
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
      ******************
      * TAB CHARACTER  *
      ******************
            IF F-EXIT-CH = X"09"
             IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                PERFORM ERROR-020
                PERFORM CHECK-SUB1-TOTAL
                GO TO FILL-999
             ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM ERROR-020
                PERFORM CHECK-SUB1-TOTAL
                GO TO FILL-999.
      *****************************************************
      * TO ALLOW STORES STAFF TO ONLY CHANGE THE SHIP QTY *
      *****************************************************
            IF F-NAMEFIELDRED1 = "D"
             IF F-EXIT-CH NOT = X"01" AND NOT = X"0B"
                      AND NOT = X"09" AND NOT = X"05"
                      AND NOT = X"11" AND NOT = X"0C"
                      AND NOT = X"19"
                   GO TO FILL-010.
      *****************************************************
      * TO ALLOW CHANGES TO NON STOCK REPAIR ITEMS.       *
      *****************************************************
            IF F-NAMEFIELDRED1 = "R"
             IF SP-1STCHAR NOT = "/"
              IF F-EXIT-CH NOT = X"01" AND NOT = X"0B"
                       AND NOT = X"09" AND NOT = X"05"
                       AND NOT = X"11" AND NOT = X"0C"
                       AND NOT = X"19"
                 GO TO FILL-010.
                
           IF WS-NEWORDER = "N"
            IF B-NEWLINE (SUB-1) = "L" OR ="Y"
             IF F-EXIT-CH NOT = X"01" AND NOT = X"0B"
                       AND NOT = X"09" AND NOT = X"05"
                       AND NOT = X"11" AND NOT = X"0C"
                   GO TO FILL-010.
      ************************************************************
      * 'ESC' = X"07"; 'CODE-CANCEL' = X"87" ; 'ALT-F10' = X"9F" *
      ************************************************************
            IF F-EXIT-CH = X"07" OR = X"87" OR = X"9F"
             IF B-STOCKNUMBER (SUB-1) = "  "
                GO TO FILL-010.
            IF F-EXIT-CH = X"07"
                MOVE "TO DELETE A LINE-ITEM PRESS 'ALT-F10'"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-010.
           IF F-EXIT-CH = X"87" OR = X"9F"
            IF B-STOCKNUMBER (SUB-1) > " "
             IF WS-REPAIR > 0
               GO TO FILL-010.
            IF F-EXIT-CH = X"87" OR = X"9F"
                MOVE "Y" TO WS-MUST-PRINT
                PERFORM CANCEL-STOCK-TRANS.
            IF F-EXIT-CH = X"87" OR = X"9F"
                MOVE SUB-1 TO SUB-7
                MOVE "Y" TO WS-MUST-PRINT
                PERFORM CANCEL-TRANSACTION
                SUBTRACT 1 FROM SUB-25
                MOVE 1 TO SUB-1
                          F-INDEX
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
              IF SUB-25 > 8
                SUBTRACT 5 FROM SUB-25
                MOVE SUB-25 TO SUB-1
                PERFORM SCROLL-NEXT
                ADD 5 TO SUB-25
                GO TO FILL-010
              ELSE
                GO TO FILL-010.
      *************************************************
      * SECTION TO DISALLOW RE-ENTRY OF EXISTING LINE *
      *************************************************
           IF F-NAMEFIELD NOT = B-STOCKNUMBER (SUB-1)
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15                    TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                GO TO FILL-005.
      *******************************************
      *SECTION TO CHANGE QTY'S OF EXISTING LINE *
      *F5      = "19"   CHANGES SHIP-QTY        *
      *CODE-F5 = "99"   CHANGES SHIP & ORDER-QTY*
      *******************************************
           IF F-EXIT-CH = X"19" OR = X"99"
            IF B-STOCKNUMBER (SUB-1) = " "
               GO TO FILL-010.
           IF F-EXIT-CH = X"19" OR = X"99"
            IF B-STOCKNUMBER (SUB-1) > " "
             IF SP-1STCHAR NOT = "/"
              IF WS-REPAIR > 0
                GO TO FILL-010.
           IF F-EXIT-CH = X"19" OR = X"99"
                PERFORM CHANGE-QTY
            IF SP-1STCHAR NOT = "/"
                MOVE ST-BINLOCATION TO B-STORE (SUB-1).
          IF F-EXIT-CH = X"19" OR = X"99"
            IF SP-1STCHAR NOT = "*" AND NOT  = "/"
             IF WS-COST-DISPLAY = "Y"
                MOVE "STOCKCOST"    TO F-FIELDNAME
                MOVE 9              TO F-CBFIELDNAME
                MOVE ST-AVERAGECOST TO F-EDNAMEFIELDAMOUNT
                                       B-STOCKCOST (SUB-1)
                MOVE 9              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNT
            ELSE
                MOVE ST-AVERAGECOST TO B-STOCKCOST (SUB-1).
           IF F-EXIT-CH = X"19" OR = X"99"
            IF WSAN-CODE = "53" 
                PERFORM COMPUTE-SPECIAL-PRICES.
           IF F-EXIT-CH = X"19" OR = X"99"
            IF WSAN-CODE = "57"
                PERFORM COMPUTE-ASSOCIATE-PRICES.
           IF F-EXIT-CH = X"19" OR = X"99"
            IF WSAN-CODE = "53" OR = "57"
              IF SP-1STCHAR NOT = "*" AND NOT  = "/"
                MOVE "STOCKPRICE" TO F-FIELDNAME
                MOVE 10           TO F-CBFIELDNAME
               IF WS-NEWPRICE = ST-PRICE 
                MOVE ST-PRICE     TO F-EDNAMEFIELDAMOUNT
                                     B-STOCKPRICE (SUB-1)
                MOVE 9            TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNT.
             IF F-EXIT-CH = X"19"
                PERFORM DLI-020
                GO TO FILL-025.
             IF F-EXIT-CH = X"99"
                PERFORM DLI-020
                GO TO FILL-020.
      ********************************************
      *<CODE-NEXT-PAGE> TO READ NEXT STOCK ITEM. *
      ********************************************
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

            IF F-NAMEFIELD = " "
                GO TO FILL-010.
           MOVE F-NAMEFIELD TO B-STOCKNUMBER (SUB-1).
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
                MOVE 0 TO WS-QTYONHAND
                          WS-QTYONRESERVE
                          WS-QTYONORDER
                          WS-QTYONBORDER
                MOVE 2110 TO POS
                DISPLAY WS-ONHAND-LINE AT POS
                PERFORM FILL-COMMENT
             IF SUB-1 < SUB-25
                MOVE "Y" TO WS-LINECHANGED
                GO TO FILL-090
             ELSE
                GO TO FILL-090.

            IF SP-1STCHAR NOT = "/"
                AND B-STOCKDESCRIPTION (SUB-1) NOT = " "
                AND B-ORDERQTY (SUB-1) NOT = 0
                GO TO FILL-010.
            IF SP-1STCHAR NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER
                PERFORM READ-STOCK.
            
            IF WS-STOCK-ST1 = 0 
                MOVE ST-QTYONHAND    TO WS-QTYONHAND
                MOVE ST-QTYONRESERVE TO WS-QTYONRESERVE
                MOVE ST-QTYONORDER   TO WS-QTYONORDER
                MOVE ST-QTYONBORDER  TO WS-QTYONBORDER
            ELSE
                MOVE 0               TO WS-QTYONHAND
                                        WS-QTYONRESERVE
                                        WS-QTYONORDER
                                        WS-QTYONBORDER.
            PERFORM CHECK-IF-ENTERED-BEFORE.
            MOVE 2110 TO POS.
            DISPLAY WS-ONHAND-LINE AT POS.

            IF SP-1STCHAR = "/"
             IF SUB-1 < SUB-25
                MOVE "Y" TO WS-LINECHANGED
             ELSE
                MOVE " " TO ST-DESCRIPTION1
                            ST-DESCRIPTION2
                MOVE 0 TO ST-PRICE
                          ST-AVERAGECOST
                          ST-DISCOUNT1
                          ST-DISCOUNT9
                          STDISC-PERCENT
                          WS-QTYONHAND
                          WS-QTYONRESERVE
                          WS-QTYONORDER
                          WS-QTYONBORDER
                MOVE 2110 TO POS
                DISPLAY WS-ONHAND-LINE AT POS.
            IF SP-1STCHAR NOT = "/"
               AND ST-DESCRIPTION1 = " "
                   MOVE "INVALID STOCK ITEM!!!" TO WS-MESSAGE
                   PERFORM ERROR-MESSAGE
                   MOVE " " TO B-STOCKNUMBER (SUB-1)
                   GO TO FILL-005.

            IF SP-1STCHAR = "/"
             OR ST-DESCRIPTION1 = " "
                GO TO FILL-0100.
                
            IF WSAN-CODE = "57"
             IF B-SPECIAL (SUB-1) NOT = "Y"
                PERFORM READ-STDISCOUNT.
            IF WSAN-CODE NOT = "57"
                PERFORM READ-STDISCOUNT.

            MOVE 0 TO STPR-PRICE.
            
      *  53=BRANCHES
      *  57=ASSOCIATE COMPANIES
            
            IF STDISC-PERCENT = 0
             IF WSAN-CODE NOT = "53" AND NOT = "57"
                PERFORM READ-SPECIAL-PRICES.

            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16                 TO F-CBFIELDNAME.
            MOVE ST-DESCRIPTION1    TO F-NAMEFIELD
                                       B-STOCKDESCRIPTION (SUB-1).
            MOVE ST-DESCRIPTION2    TO B-STOCKDESCRIPTION2 (SUB-1).
            MOVE 20                 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
           
            PERFORM SCROLL-050.

            MOVE "PERUNIT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE ST-UNITOFMEASURE TO F-NAMEFIELD
                                     B-UNIT (SUB-1).
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE ST-BINLOCATION TO B-STORE (SUB-1).
       FILL-0100.
            IF SP-1STCHAR = "/"
             OR ST-PRICE = 0
                 GO TO FILL-0110.
                 
            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            MOVE ST-PRICE     TO F-EDNAMEFIELDAMOUNT
                                 B-STOCKPRICE (SUB-1).
            MOVE 9            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
            
            MOVE ST-MIN-PERC TO B-MIN-PERC (SUB-1).
       FILL-0110.
            IF SP-1STCHAR = "/"
             OR ST-AVERAGECOST = 0
                 GO TO FILL-0120.
            MOVE "STOCKCOST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-AVERAGECOST TO F-EDNAMEFIELDAMOUNT
                                   B-STOCKCOST (SUB-1).
            MOVE 9 TO F-CBFIELDLENGTH.
            IF WS-COST-DISPLAY = "N"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
            ELSE
                PERFORM WRITE-FIELD-AMOUNT.
       FILL-0120.
           MOVE "DISCOUNTPERITEM" TO F-FIELDNAME.
           MOVE 15 TO F-CBFIELDNAME.
            IF WSAN-CODE = "57"
             IF B-SPECIAL (SUB-1) = "Y"
                MOVE 0 TO F-EDNAMEFIELDAMOUNTDIS
                          B-DISCOUNTPERITEM (SUB-1)
                MOVE 5 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS
                GO TO FILL-0130.
           IF SP-1STCHAR = "/"
            IF WS-INVOICEDISCOUNT > 0
                MOVE WS-INVOICEDISCOUNT TO F-EDNAMEFIELDAMOUNTDIS
                                           B-DISCOUNTPERITEM (SUB-1)
                MOVE 5 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS
                GO TO FILL-0130.
            IF STDISC-PERCENT > 0
                MOVE STDISC-PERCENT TO F-EDNAMEFIELDAMOUNTDIS
                                       B-DISCOUNTPERITEM (SUB-1)
                MOVE 5              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS
                GO TO FILL-0130.
            IF WS-ZERODIS = "Y"
                MOVE WS-INVOICEDISCOUNT TO F-EDNAMEFIELDAMOUNTDIS
                                           B-DISCOUNTPERITEM (SUB-1)
                MOVE 5 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS
                GO TO FILL-0130.
            IF WS-INVOICEDISCOUNT > 0
             IF WS-INVOICEDISCOUNT NOT > B-MAX-DISC (SUB-1)
                MOVE WS-INVOICEDISCOUNT TO F-EDNAMEFIELDAMOUNTDIS
                                           B-DISCOUNTPERITEM (SUB-1)
                MOVE 5 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS
                GO TO FILL-0125
             ELSE
                MOVE B-MAX-DISC (SUB-1) TO F-EDNAMEFIELDAMOUNTDIS
                                           B-DISCOUNTPERITEM (SUB-1)
                MOVE 5 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS
                GO TO FILL-0125.
            IF WS-DISCOUNT-CODE = "0" OR = " "
                MOVE 0 TO F-EDNAMEFIELDAMOUNTDIS
                          B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "1"
                MOVE ST-DISCOUNT1 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "2"
                MOVE ST-DISCOUNT2 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "3"
                MOVE ST-DISCOUNT3 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "4"
                MOVE ST-DISCOUNT4 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "5"
                MOVE ST-DISCOUNT5 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "6"
                MOVE ST-DISCOUNT6 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "7"
                MOVE ST-DISCOUNT7 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "8"
                MOVE ST-DISCOUNT8 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "9"
                MOVE ST-DISCOUNT9 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
       FILL-0125.
            IF STPR-PRICE > 0
                MOVE B-DISCOUNTPERITEM (SUB-1) TO WS-DISCOUNTSAVE
                MOVE 0 TO F-EDNAMEFIELDAMOUNTDIS
                          B-DISCOUNTPERITEM (SUB-1).
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
       FILL-0130.
      *      MOVE "TAX" TO F-FIELDNAME.
      *      MOVE 3 TO F-CBFIELDNAME.
      *      IF WS-GSTNO = "EXPORT" OR > " "
            IF WS-GSTNO = "EXPORT" OR = "EXPORT      "
                MOVE "N" TO F-NAMEFIELD
                            B-TAX (SUB-1)
            ELSE
                MOVE "Y" TO F-NAMEFIELD
                            B-TAX (SUB-1).
      *      MOVE 1 TO F-CBFIELDLENGTH.
      *      PERFORM WRITE-FIELD-ALPHA.
      
            IF SP-1STCHAR NOT = "/" AND NOT = "*"
                PERFORM READ-QUOTE-BY-ACCOUNT. 
       FILL-020.
           MOVE "                   " TO F-NAMEFIELD.
           MOVE "ORDERQTY" TO F-FIELDNAME.
           MOVE 8 TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
             IF WS-LINECHANGED = "N"
               MOVE 0   TO B-ORDERQTY (SUB-1)
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-010
             ELSE
               GO TO FILL-020.
           IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
           MOVE 5 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           IF SIGN-FOUND = 1
               GO TO FILL-020.
           MOVE NUMERIC-RATE TO B-ORDERQTY (SUB-1).
           IF WS-LINECHANGED = "C"
            IF B-ORDERQTY (SUB-1) < B-SHIPPEDQTY (SUB-1)
                MOVE
           "YOU CANNOT HAVE ON ORDER < THAN QTY PREVIOUSLY SHIPPED OF:"
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE B-SHIPPEDQTY (SUB-1) TO F-EDNAMEFIELDQTY
                MOVE 3070 TO POS
                DISPLAY F-EDNAMEFIELDQTY AT POS
                GO TO FILL-020.
           IF WS-LINECHANGED = "C"
               COMPUTE WS-BO-QTY = B-ORDERQTY (SUB-1) -
                          B-SHIPPEDQTY (SUB-1)
            IF WS-BO-QTY > 0
               MOVE WS-BO-QTY TO F-EDNAMEFIELDQTY
               PERFORM WRITE-FIELD-QTY
            ELSE
               MOVE B-ORDERQTY (SUB-1) TO F-EDNAMEFIELDQTY
               PERFORM WRITE-FIELD-QTY.
           IF B-ORDERQTY (SUB-1) = 0
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-020.
           IF WS-LINECHANGED NOT = "C"
               MOVE B-ORDERQTY (SUB-1) TO F-EDNAMEFIELDQTY
               PERFORM WRITE-FIELD-QTY.
           IF WS-LINECHANGED = "C"
             IF B-ORDERQTY (SUB-1) = B-SHIPPEDQTY (SUB-1)
               MOVE 0         TO B-SHIPQTY (SUB-1)
               PERFORM FILL-027
               MOVE "L"       TO B-NEWLINE (SUB-1)
               MOVE "SHIPQTY" TO F-FIELDNAME
               MOVE 7         TO F-CBFIELDNAME
               MOVE 5         TO F-CBFIELDLENGTH
               MOVE "SHIPD"   TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            IF SP-1STCHAR NOT = "/"
               REWRITE STOCK-RECORD
               GO TO FILL-090
            ELSE
               GO TO FILL-090.
            
           IF SP-1STCHAR NOT = "/"
            IF WS-LINECHANGED = "C"
                COMPUTE B-SHIPQTY (SUB-1) =
                B-ORDERQTY (SUB-1) - B-SHIPPEDQTY (SUB-1)
             IF ST-QTYONHAND NOT < B-SHIPQTY (SUB-1)
                GO TO FILL-027
             ELSE
               MOVE ST-QTYONHAND  TO B-SHIPQTY (SUB-1)
                GO TO FILL-027.
             
            IF SP-1STCHAR NOT = "/"
             IF ST-QTYONHAND NOT < B-ORDERQTY (SUB-1)
                MOVE NUMERIC-RATE TO B-SHIPQTY (SUB-1)
             ELSE
               MOVE ST-QTYONHAND  TO B-SHIPQTY (SUB-1).
            IF SP-1STCHAR = "/"
               MOVE NUMERIC-RATE TO B-SHIPQTY (SUB-1).
            GO TO FILL-027.
       FILL-025.
            MOVE "ORDERQTY" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME.
            COMPUTE WS-BO-QTY =
                  B-ORDERQTY (SUB-1) - B-SHIPPEDQTY (SUB-1).
            MOVE WS-BO-QTY TO F-EDNAMEFIELDQTY
            MOVE 5         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY.
            IF SP-1STCHAR NOT = "/"
             IF WS-BO-QTY NOT > ST-QTYONHAND
               MOVE WS-BO-QTY TO B-SHIPQTY (SUB-1)
               GO TO FILL-027
             ELSE
               MOVE ST-QTYONHAND TO B-SHIPQTY (SUB-1)
               GO TO FILL-027.
            MOVE WS-BO-QTY TO B-SHIPQTY (SUB-1).
       FILL-027.
            MOVE "SHIPQTY"         TO F-FIELDNAME
            MOVE 7                 TO F-CBFIELDNAME
            MOVE B-SHIPQTY (SUB-1) TO F-EDNAMEFIELDQTY
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY.
       FILL-035.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "SHIPQTY" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
             IF F-NAMEFIELDRED1 = "D"
              IF SUB-1 > 1
                 SUBTRACT 1 FROM SUB-1 F-INDEX
                 GO TO FILL-035
              ELSE
                 GO TO FILL-035.
            IF F-EXIT-CH = X"01"
             IF WS-LINECHANGED NOT = "N"
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-035.
            IF F-EXIT-CH = X"01"
                MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
             IF SP-1STCHAR NOT = "/"
                MOVE 0   TO B-SHIPQTY (SUB-1)
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO FILL-020
             ELSE
                GO TO FILL-020.
            MOVE 5 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"1D" OR = X"0A" OR = X"15"
               GO TO FILL-037.
            DISPLAY " " AT 3079 WITH BELL
            GO TO FILL-035.
       FILL-037.
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE.
            IF SIGN-FOUND = 1
                GO TO FILL-035.
            
            MOVE NUMERIC-RATE TO B-SHIPQTY (SUB-1).
            IF B-ORDERQTY (SUB-1) < B-SHIPQTY (SUB-1)
                MOVE "YOU MAY NOT ENTER MORE TO SHIP THAN ON ORDER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-035.
            MOVE B-SHIPQTY (SUB-1) TO F-EDNAMEFIELDQTY
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY.
       FILL-038.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR = "/"
                GO TO FILL-045.
            MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
            PERFORM READ-STOCK-LOCK.
       FILL-040.
            IF B-SHIPQTY (SUB-1) > ST-QTYONHAND
                REWRITE STOCK-RECORD
                DISPLAY " " AT 3079 WITH BELL
                MOVE B-STOCKNUMBER (SUB-1) TO WS-DAILY-1ST
                MOVE "MAXIMUM TO SHIP IS:" TO WS-DAILY-2ND
                MOVE ST-QTYONHAND          TO F-EDNAMEFIELDQTY
                MOVE F-EDNAMEFIELDQTY TO WS-DAILY-3RD
                MOVE " "              TO WS-DAILY-4TH
                MOVE WS-DAILY-MESSAGE TO WS-MESSAGE
                MOVE " " TO WS-DAILY-MESSAGE
                PERFORM ERROR-MESSAGE
            IF WS-LINECHANGED = "Y"
                MOVE "SHIPQTY"    TO F-FIELDNAME
                MOVE 0            TO B-SHIPQTY (SUB-1)
                MOVE 7            TO F-CBFIELDNAME
                MOVE ST-QTYONHAND TO F-EDNAMEFIELDQTY
                MOVE 5            TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-QTY
                GO TO FILL-035
            ELSE
                MOVE "ORDERQTY" TO F-FIELDNAME
                MOVE 8          TO F-CBFIELDNAME
                MOVE 5          TO F-CBFIELDLENGTH
                MOVE 0          TO B-ORDERQTY (SUB-1)
                MOVE " "        TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                MOVE "SHIPQTY"  TO F-FIELDNAME
                MOVE 0          TO B-SHIPQTY (SUB-1)
                MOVE 7          TO F-CBFIELDNAME
                MOVE 5          TO F-CBFIELDLENGTH
                MOVE " "        TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO FILL-020.

           SUBTRACT B-SHIPQTY (SUB-1) FROM ST-QTYONHAND.
           ADD B-SHIPQTY (SUB-1)        TO ST-QTYONRESERVE.
      *     IF WS-LINECHANGED = "N" OR = "C"
           IF WS-LINECHANGED = "N"
             ADD B-ORDERQTY (SUB-1) TO ST-QTYONBORDER
           ELSE
             COMPUTE WS-BO-QTY =
                B-ORDERQTY (SUB-1) - B-SHIPPEDQTY (SUB-1)
             ADD WS-BO-QTY TO ST-QTYONBORDER.

           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
               
            IF F-NAMEFIELDRED1 = "D"
                GO TO FILL-090.
      *F1-KEY
            IF F-EXIT-CH = X"15"
             IF WS-PRICESAVE > 0
                MOVE WS-PRICESAVE         TO ST-PRICE
                MOVE "STOCKPRICE"         TO F-FIELDNAME
                MOVE 10                   TO F-CBFIELDNAME
                MOVE ST-PRICE             TO B-STOCKPRICE (SUB-1)
                MOVE B-STOCKPRICE (SUB-1) TO F-EDNAMEFIELDAMOUNT
                MOVE 9                    TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNT
                
                MOVE "DISCOUNTPERITEM" TO F-FIELDNAME
                MOVE 15                TO F-CBFIELDNAME
                MOVE WS-DISCOUNTSAVE   TO B-DISCOUNTPERITEM (SUB-1)
                                          F-EDNAMEFIELDAMOUNTDIS
                MOVE 5                 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS
                MOVE 0   TO STPR-PRICE
                MOVE "N" TO B-SPECIAL (SUB-1).
      *F8-KEY
            IF SP-1STCHAR NOT = "/"
             IF F-EXIT-CH NOT = X"1D"
                GO TO FILL-090.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR NOT = "/"
                GO TO FILL-048.
       FILL-045.
            IF SP-1STCHAR = "/"
             IF SUB-1 NOT = SUB-25
              IF F-EXIT-CH NOT = X"1D"
                GO TO FILL-090.
       FILL-046.
            PERFORM FILL-005
            MOVE 2920 TO POS
            DISPLAY "DESCRIPTION1" AT POS.

            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE B-STOCKDESCRIPTION (SUB-1) TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                MOVE 2920 TO POS
                DISPLAY "                  " AT POS
                GO TO FILL-035.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-045.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO B-STOCKDESCRIPTION (SUB-1).
       FILL-047.
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE B-STOCKDESCRIPTION2 (SUB-1) TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE 2920 TO POS.
            DISPLAY "DESCRIPTION2" AT POS.
            
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
                GO TO FILL-046.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-047.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO B-STOCKDESCRIPTION2 (SUB-1).
            MOVE 2920 TO POS.
            DISPLAY "                  " AT POS.
      *
      *RE-DISPLAYING DESC 1
      *
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE B-STOCKDESCRIPTION (SUB-1) TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE "HERE AT FILL-048" TO WS-MESSAGE
            PERFORM ERROR-MESSAGE.
            
       FILL-048.
      ***************************************************************
      *NEW SECTION TO CHECK PASSWORD BEFORE ALLOWING CHANGE OF PRICE*
      ***************************************************************
            IF SP-1STCHAR = "/"
                GO TO FILL-049.
            IF F-EXIT-CH NOT = X"1D"
               GO TO FILL-070.
            GO TO FILL-0491.
       FILL-049.
            MOVE "PERUNIT" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
             IF SP-1STCHAR = "/"
                GO TO FILL-045.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1D"
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-049.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO B-UNIT (SUB-1).
            MOVE 4           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       FILL-0491.
            IF SP-1STCHAR = "/"
               GO TO FILL-050.
            IF F-EXIT-CH NOT = X"1D"
               GO TO FILL-070.
       FILL-0492.
            PERFORM CHECK-PASSWORD.
            IF WS-PASSWORD-VALID = "N"
               GO TO FILL-070.
       FILL-050.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
             IF SP-1STCHAR = "/"
                GO TO FILL-049.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1D"
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-050.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF SIGN-FOUND = 1
                GO TO FILL-050.
            
            IF SP-1STCHAR NOT = "/"
             IF NUMERIC-RATE NOT = B-STOCKPRICE (SUB-1)
                MOVE "STOCK ITEM CHANGED"   TO WS-DAILY-1ST
                MOVE WS-STOCKNUMBER         TO WS-DAILY-2ND
                MOVE "P/SLIP #"             TO WS-DAILY-3RD1
                MOVE WS-P-SLIP              TO WS-DAILY-3RD2
                MOVE "NEW PRICE"            TO WS-DAILY-4TH1
                MOVE NUMERIC-RATE           TO F-EDNAMEFIELDAMOUNT
                MOVE F-EDNAMEFIELDAMOUNT    TO WS-DAILY-4TH2
                PERFORM WRITE-DAILY
                PERFORM GET-USER-MAIL-NAME
                PERFORM GET-REPORT-Y2K-DATE
                MOVE "ABOVE CHANGE DONE BY" TO WS-DAILY-1ST
                MOVE WS-pbValue             TO WS-DAILY-2ND
                MOVE "DATE & TIME CHANGED:" TO WS-DAILY-3RD
                MOVE pbRet                  TO WS-REPORT-DATE-STRIP
                MOVE WS-STRIP2              TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.

            MOVE "STOCKPRICE"         TO F-FIELDNAME
            MOVE 10                   TO F-CBFIELDNAME
            MOVE NUMERIC-RATE         TO B-STOCKPRICE (SUB-1)
            MOVE B-STOCKPRICE (SUB-1) TO F-EDNAMEFIELDAMOUNT
            MOVE 9                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            IF WS-PRICESAVE = B-STOCKPRICE (SUB-1)
                MOVE WS-DISCOUNTSAVE   TO B-DISCOUNTPERITEM (SUB-1)
                                          F-EDNAMEFIELDAMOUNTDIS
                MOVE "DISCOUNTPERITEM" TO F-FIELDNAME
                MOVE 15                TO F-CBFIELDNAME
                MOVE 5                 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS.
            
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
      *      IF SP-1STCHAR = "/"
      *      IF B-STOCKPRICE (SUB-1) = 0
      *       IF WS-COST-DISPLAY = "Y"
      *         GO TO FILL-060.
                
            IF SP-1STCHAR = "/"
             IF SUB-1 = SUB-25
                GO TO FILL-060.
            IF SP-1STCHAR = "/"
             IF SUB-1 NOT = SUB-25
              IF WS-COST-DISPLAY = "Y"
                GO TO FILL-060
              ELSE
                GO TO FILL-072.

            IF SP-1STCHAR = "/"
               GO TO FILL-060.
            IF F-EXIT-CH NOT = X"1D"
               GO TO FILL-070.
            IF WS-COST-DISPLAY = "N"
                GO TO FILL-070.
            
            IF SP-1STCHAR NOT = "/"
               MOVE X"16" TO F-EXIT-CH
               PERFORM CHECK-PASSWORD
               MOVE X"1D" TO F-EXIT-CH
             IF WS-PASSWORD-VALID = "N"
               GO TO FILL-070.
       FILL-060.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "STOCKCOST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
             IF SP-1STCHAR = "/"
                GO TO FILL-050
             ELSE
                GO TO FILL-0492.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-060.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF SIGN-FOUND = 1
                GO TO FILL-060.
            
            IF SP-1STCHAR NOT = "/"
             IF NUMERIC-RATE NOT = B-STOCKCOST (SUB-1)
                MOVE "STOCK ITEM CHANGED"   TO WS-DAILY-1ST
                MOVE WS-STOCKNUMBER         TO WS-DAILY-2ND
                MOVE "P/SLIP #"             TO WS-DAILY-3RD1
                MOVE WS-P-SLIP              TO WS-DAILY-3RD2
                MOVE "NEW COST  "           TO WS-DAILY-4TH1
                MOVE NUMERIC-RATE           TO F-EDNAMEFIELDAMOUNT
                MOVE F-EDNAMEFIELDAMOUNT    TO WS-DAILY-4TH2
                PERFORM WRITE-DAILY
                PERFORM GET-USER-MAIL-NAME
                PERFORM GET-REPORT-Y2K-DATE
                MOVE "ABOVE CHANGE DONE BY" TO WS-DAILY-1ST
                MOVE WS-pbValue             TO WS-DAILY-2ND
                MOVE "DATE & TIME CHANGED:" TO WS-DAILY-3RD
                MOVE pbRet                  TO WS-REPORT-DATE-STRIP
                MOVE WS-STRIP2              TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.

            MOVE "STOCKCOST"         TO F-FIELDNAME
            MOVE 9                   TO F-CBFIELDNAME
            MOVE NUMERIC-RATE        TO B-STOCKCOST (SUB-1)
            MOVE B-STOCKCOST (SUB-1) TO F-EDNAMEFIELDAMOUNT
            MOVE 9                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       FILL-070.
            IF SP-1STCHAR = "/"
             IF WS-PRICESAVE = B-STOCKPRICE (SUB-1)
               GO TO FILL-072.
            IF SP-1STCHAR = "/"
               GO TO FILL-072.
            IF F-EXIT-CH NOT = X"1D"
               GO TO FILL-080.
            PERFORM CHECK-PASSWORD.
            IF WS-PASSWORD-VALID = "N"
               GO TO FILL-080.
       FILL-072.
            IF B-SPECIAL (SUB-1) = "Y"
               MOVE "THIS IS A SPECIAL PRICE, DISCOUNT CAN'T BE CHANGED"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-090.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME.
            MOVE 15 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            
            IF F-EXIT-CH = X"01"
             IF SP-1STCHAR = "/"
              IF SUB-1 NOT = SUB-25
               IF WS-COST-DISPLAY = "Y"
                GO TO FILL-060
               ELSE
                GO TO FILL-050.
            IF F-EXIT-CH = X"01"
             IF SP-1STCHAR = "/"
              IF SUB-1 = SUB-25
                GO TO FILL-060.
            IF F-EXIT-CH = X"01"
             IF SP-1STCHAR NOT = "/"
                GO TO FILL-0492.
                
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-072.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF SIGN-FOUND = 1
                GO TO FILL-070.
            
            IF SP-1STCHAR NOT = "/"
             IF NUMERIC-RATE > B-MAX-DISC (SUB-1)
                MOVE B-MAX-DISC (SUB-1) TO NUMERIC-RATE
                MOVE "MAXIMUM DISCOUNT ON THIS ITEM IS :"
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE 3045 TO POS
                MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                DISPLAY F-EDNAMEFIELDAMOUNTDIS AT POS
                GO TO FILL-072.

            IF SP-1STCHAR NOT = "/"
             IF NUMERIC-RATE NOT = B-DISCOUNTPERITEM (SUB-1)
                MOVE "STOCK ITEM CHANGED"   TO WS-DAILY-1ST
                MOVE WS-STOCKNUMBER         TO WS-DAILY-2ND
                MOVE "P/SLIP #"             TO WS-DAILY-3RD1
                MOVE WS-P-SLIP              TO WS-DAILY-3RD2
                MOVE "NEW DISCNT"           TO WS-DAILY-4TH1
                MOVE NUMERIC-RATE           TO F-EDNAMEFIELDAMOUNTDIS
                MOVE F-EDNAMEFIELDAMOUNTDIS TO WS-DAILY-4TH2
                PERFORM WRITE-DAILY
                PERFORM GET-USER-MAIL-NAME
                PERFORM GET-REPORT-Y2K-DATE
                MOVE "ABOVE CHANGE DONE BY" TO WS-DAILY-1ST
                MOVE WS-pbValue             TO WS-DAILY-2ND
                MOVE "DATE & TIME CHANGED:" TO WS-DAILY-3RD
                MOVE pbRet                  TO WS-REPORT-DATE-STRIP
                MOVE WS-STRIP2              TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.

            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME
            MOVE 15                TO F-CBFIELDNAME
            MOVE NUMERIC-RATE      TO B-DISCOUNTPERITEM (SUB-1)
                                      F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.
       FILL-080.
      * 
      * REMOVED. NO SPACE ON THE FORM
      * 
            GO TO FILL-090.
         
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "TAX" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
             IF SP-1STCHAR = "/"
                GO TO FILL-070
             ELSE
                GO TO FILL-0492.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-080.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO B-TAX (SUB-1).
       FILL-090.
            MOVE 0 TO WS-PRICESAVE.
            PERFORM ERROR-020
            PERFORM ERROR1-020
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.

            IF WS-LINECHANGED = "Y" OR = "C"
               PERFORM CHANGE-STOCK-TRANS.
               
            IF WS-LINECHANGED = "N"
               PERFORM WRITE-NEW-TRANS
               ADD 1 TO WS-STTRANS-NO.
            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 > WS-STTRANS-NO
               MOVE SUB-1 TO WS-STTRANS-NO.
            IF WS-LINECHANGED = "N"
               MOVE SUB-1 TO SUB-25.
            MOVE "N" TO WS-LINECHANGED
            MOVE "Y" TO WS-MUST-PRINT.
            IF SUB-1 > 200             
                MOVE 200 TO SUB-1 SUB-25
                MOVE
              "200 LINES ARE UP, PRESS 'ESC' TO TAB & END THE ORDER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-999.
            IF F-INDEX < 8
                GO TO FILL-005.
            SUBTRACT 1 FROM SUB-1
            PERFORM SCROLL-NEXT
            GO TO FILL-010.
       FILL-999.
            EXIT.
      *
       CHECK-IF-ENTERED-BEFORE SECTION.
       CIEB-005.
            IF SUB-1 = 1
               GO TO CIEB-999.
      *      MOVE 2910 TO POS
      *      DISPLAY "CHECKING FOR DUPLICATIONS ON PREVIOUS LINES...."
      *      AT POS
            MOVE 0 TO WS-BEFORE.
       CIEB-010.
            ADD 1 TO WS-BEFORE.
            IF B-STOCKNUMBER (SUB-1) = B-STOCKNUMBER (WS-BEFORE)
             IF SUB-1 NOT = WS-BEFORE
               GO TO CIEB-020.
            IF WS-BEFORE = SUB-1
               GO TO CIEB-900
            ELSE
               GO TO CIEB-010.
       CIEB-020.
            MOVE 3010 TO POS
            DISPLAY "STOCKNUMBER ALREADY ENTERED ON LINE NUMBER:"
            AT POS
            ADD 43 TO POS
            MOVE WS-BEFORE TO WS-BODY-LINE
            DISPLAY WS-BODY-LINE AT POS
            CALL "LOCKKBD" USING F-FIELDNAME.
       CIEB-900.
      *      PERFORM ERROR1-020.
       CIEB-999.
            EXIT.
      *
       WRITE-NEW-TRANS SECTION.
       WNT-TRANS-000.
           IF B-NEWLINE (SUB-1) NOT = " "
               GO TO WNT-TRANS-999.
       WNT-TRANS-002.
           IF WS-STTRANS-NO = 0
              ADD 1 TO WS-STTRANS-NO.
           MOVE WS-INVOICE                  TO STTR-REFERENCE1
           MOVE 4                           TO STTR-TYPE.
           MOVE WS-STTRANS-NO               TO STTR-TRANSACTION-NUMBER
                                               B-STTRANS (SUB-1)
           MOVE B-STOCKNUMBER (SUB-1)       TO SPLIT-STOCK
                                               STTR-STOCK-NUMBER
           MOVE WS-ACCOUNT-NUMBER           TO STTR-ACCOUNT-NUMBER
           MOVE "N"                         TO STTR-COMPLETE
                                               STTR-ST-COMPLETE
                                               STTR-AC-COMPLETE
                                               B-NEWLINE (SUB-1).
           IF WS-REPAIR > 0
            IF B-REPAIR (SUB-1) = "R"
           MOVE "R"                         TO STTR-COMPLETE
                                               STTR-ST-COMPLETE
                                               STTR-AC-COMPLETE.
            
           MOVE 0                           TO STTR-INV-NO
           MOVE WS-INVOICEDATE              TO STTR-DATE
                                               STTR-AC-DATE
                                               STTR-ST-DATE.
            IF SP-1STCHAR = "*"
                GO TO WNT-TRANS-600.
           MOVE B-ORDERQTY (SUB-1)          TO STTR-ORDERQTY
           MOVE B-SHIPQTY (SUB-1)           TO STTR-SHIPQTY
           MOVE B-SHIPPEDQTY (SUB-1)        TO STTR-SHIPPEDQTY
           MOVE B-NETT (SUB-1)              TO STTR-SALES-VALUE
           MOVE B-STOCKPRICE (SUB-1)        TO STTR-PRICE
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO STTR-DESC1
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO STTR-DESC2
           MOVE B-TAX (SUB-1)               TO STTR-TAX
           MOVE B-UNIT (SUB-1)              TO STTR-UNIT
           MOVE B-DISCOUNTPERITEM (SUB-1)   TO STTR-ITEMDISC
           MOVE B-STOCKCOST (SUB-1)         TO STTR-COST-VALUE
           GO TO WNT-TRANS-800.
       WNT-TRANS-600.
           MOVE C-DESC (SUB-1)   TO COM-DESC
           MOVE C-UNIT (SUB-1)   TO COM-UNIT
           MOVE C-ORDER (SUB-1)  TO COM-ORDERQTY
           MOVE C-SHIP (SUB-1)   TO COM-SHIPQTY
           MOVE C-PRICE (SUB-1)  TO COM-PRICE
           MOVE C-COST (SUB-1)   TO COM-COST
           MOVE C-DISC (SUB-1)   TO COM-DISC
           MOVE " "              TO COM-FILLER.
       WNT-TRANS-800.
           WRITE STOCK-TRANS-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "WRITE ERROR AT WNT-TRANS-800, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               ADD 1 TO WS-STTRANS-NO STTR-TRANSACTION-NUMBER
               MOVE 0 TO WS-STTRANS-ST1
               GO TO WNT-TRANS-800.
           IF STTR-TRANSACTION-NUMBER = 0
              MOVE "ST-TRANS-NO = 0 AT WNT-TRANS-800, 'ESC' TO ADD 1."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               ADD 1 TO WS-STTRANS-NO STTR-TRANSACTION-NUMBER
               MOVE 0 TO WS-STTRANS-ST1
               GO TO WNT-TRANS-800.
       WNT-TRANS-999.
            EXIT.
      *
       CHANGE-QTY SECTION.
       CQS-010.
           IF SP-1STCHAR = "*"
              GO TO CQS-030.
           IF SP-1STCHAR = "/"
              GO TO CQS-029.
              
           COMPUTE WS-BO-QTY =
              B-ORDERQTY (SUB-1) - B-SHIPPEDQTY (SUB-1).
           MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           
           IF ST-QTYONRESERVE > ST-QTYONBORDER
              SUBTRACT WS-BO-QTY FROM ST-QTYONRESERVE
              ADD WS-BO-QTY        TO ST-QTYONHAND 
              SUBTRACT WS-BO-QTY FROM ST-QTYONBORDER
              GO TO CQS-020.
           IF ST-QTYONRESERVE = ST-QTYONBORDER
            IF WS-BO-QTY NOT > ST-QTYONRESERVE
              SUBTRACT WS-BO-QTY FROM ST-QTYONRESERVE
              ADD WS-BO-QTY        TO ST-QTYONHAND
              SUBTRACT WS-BO-QTY FROM ST-QTYONBORDER
              GO TO CQS-020
            ELSE
              GO TO CQS-020.

           PERFORM READ-ALL-LINE-ITEMS.
              
           IF WS-STTR-SHIPQTY > ST-QTYONRESERVE
              SUBTRACT WS-BO-QTY FROM ST-QTYONBORDER
              GO TO CQS-020.
           IF WS-STTR-SHIPQTY = ST-QTYONRESERVE
            IF WS-BO-QTY NOT > ST-QTYONBORDER
              SUBTRACT WS-BO-QTY FROM ST-QTYONBORDER
              GO TO CQS-020
            ELSE
              MOVE 0               TO ST-QTYONBORDER
              GO TO CQS-020.
           IF WS-STTR-SHIPQTY < ST-QTYONRESERVE
              SUBTRACT WS-STTR-SHIPQTY FROM ST-QTYONRESERVE.
           IF WS-BO-QTY NOT > ST-QTYONRESERVE
              SUBTRACT WS-BO-QTY FROM ST-QTYONRESERVE
              ADD WS-BO-QTY        TO ST-QTYONHAND
              SUBTRACT WS-BO-QTY FROM ST-QTYONBORDER
              ADD WS-STTR-SHIPQTY  TO ST-QTYONRESERVE
              GO TO CQS-020.
           IF WS-BO-QTY > ST-QTYONRESERVE
              ADD ST-QTYONRESERVE  TO ST-QTYONHAND
              MOVE 0               TO ST-QTYONRESERVE
              SUBTRACT WS-BO-QTY FROM ST-QTYONBORDER
              MOVE WS-STTR-SHIPQTY TO ST-QTYONRESERVE.
       CQS-020.
      ******************************************************************
      *STOCK IS WRITTEN BACK TO ONHAND SO THAT IT CAN BE RE-ALLOCATED  *
      *ON THE SLIP LATER IN THE PROGRAM WHEN THE NEW SHIP-QTY IS       *
      *ENTERED IN THE FILL SECTION.  28/7/2005                         *
      *THIS IS DONE SO ALL STOCK - ON P/SLIP AND ON HAND GETS AVERAGED *
      *OUT INCASE THE P/SLIP IS LATER REVERSED.  THIS SHOULD CHANGE    *
      *THE DIFFERENCE BETWEEN SL19 AND THE G/L GP SHOWN IN THE INCOME  *
      *STATEMENT.  SEE NEXT 4 LINES OF CODE.                           *
      ******************************************************************
           
           COMPUTE WS-AVE-COST-OF-ALLOC-STOCK =
              B-SHIPQTY (SUB-1) * B-STOCKCOST (SUB-1).
           COMPUTE WS-AVE-COST-OF-STOCK = 
              (ST-QTYONHAND - B-SHIPQTY (SUB-1)) * ST-AVERAGECOST.

           IF ST-QTYONHAND > 0
           COMPUTE WS-AVE-COST-OF-STOCK ROUNDED =
             (WS-AVE-COST-OF-STOCK + WS-AVE-COST-OF-ALLOC-STOCK) / 
                ST-QTYONHAND.

           IF ST-QTYONHAND > 0
            IF WS-AVE-COST-OF-STOCK > 0
              MOVE WS-AVE-COST-OF-STOCK TO ST-AVERAGECOST.
              
           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
       CQS-021.
           IF F-EXIT-CH = X"19"
              GO TO CQS-030.
           IF B-SHIPPEDQTY (SUB-1) = 0
              GO TO CQS-029.
           MOVE B-SHIPPEDQTY (SUB-1) TO B-ORDERQTY (SUB-1)
           MOVE 2920 TO POS
           DISPLAY "QTY PREVIOUSLY SHIPPED IS:" AT POS
           ADD 27 TO POS
           MOVE B-SHIPPEDQTY (SUB-1) TO F-EDNAMEFIELDQTY
           DISPLAY F-EDNAMEFIELDQTY AT POS.
       CQS-029.
           MOVE "ORDERQTY" TO F-FIELDNAME.
           MOVE 8          TO F-CBFIELDNAME
           MOVE 5          TO F-CBFIELDLENGTH.
           IF F-EXIT-CH = X"99"
            IF B-SHIPPEDQTY (SUB-1) = 0
              MOVE 0       TO B-ORDERQTY (SUB-1)
                              B-SHIPPEDQTY (SUB-1).
           MOVE 0          TO B-SHIPQTY (SUB-1)
           MOVE " "        TO F-NAMEFIELD
           PERFORM WRITE-FIELD-ALPHA
           MOVE "SHIPQTY"  TO F-FIELDNAME
           MOVE 7          TO F-CBFIELDNAME
           MOVE 5          TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       CQS-030.
           IF F-EXIT-CH = X"99"
              MOVE "C" TO WS-LINECHANGED
           ELSE
              MOVE "Y" TO WS-LINECHANGED.
           MOVE " " TO B-PULL (SUB-1).
       CQS-999.
           EXIT.
      *
       READ-ALL-LINE-ITEMS SECTION.
       RALI-000.
           MOVE 0 TO WS-STTR-ORDERQTY
                     WS-STTR-SHIPQTY.
           MOVE "N"                   TO STTR-ST-COMPLETE.
           MOVE B-STOCKNUMBER (SUB-1) TO STTR-STOCK-NUMBER.
           MOVE 0                     TO STTR-ST-DATE.
           START STOCK-TRANS-FILE KEY NOT < STTR-ST-KEY
              INVALID KEY NEXT SENTENCE.
       RALI-010.
           READ STOCK-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              MOVE 0 TO WS-STTRANS-ST1
              MOVE 0 TO    STTR-TYPE
              GO TO RALI-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "READ-ALL-LINE-ITEMS ST-TRANS BUSY, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RALI-010.
      ******************************************************************
      *NEW SECTION TO TAKE CARE OF HAVING THE SAME STOCKNUMBER         *
      *ENTERED TWICE ON A SINGLE P/SLIP.  THIS CAN HAPPEN PARTICULARLY *
      *IF MERGING QUOTES INTO ONE P/SLIP.          7/2/2000            *
      *ONLY VALID IF <F5> IS PRESSED ON ONE OF THE DUPLICATE NUMBERS.  *
      *THIS CHECKS IF THE ONE READ IS THE ONE IN MEMORY ON THE LINE    *
      *WHERE <F5> IS PRESSED.  IF NUMBER SAME BUT ST-TRANS-NUM NOT THEN*
      *ITEM READ MUST BE THE DUPLICATE.                                *
      ******************************************************************
           IF STTR-STOCK-NUMBER = B-STOCKNUMBER (SUB-1)
            IF STTR-REFERENCE1 = WS-INVOICE
             IF STTR-TRANSACTION-NUMBER = B-STTRANS (SUB-1)
                 GO TO RALI-010
             ELSE
                 GO TO RALI-020.
              
           IF STTR-STOCK-NUMBER NOT = B-STOCKNUMBER (SUB-1)
              GO TO RALI-999.
           IF STTR-REFERENCE1 = WS-INVOICE
              GO TO RALI-010.
           IF STTR-ST-COMPLETE NOT = "N"
              GO TO RALI-999.
           IF STTR-TYPE NOT = 4 AND NOT = 7
              GO TO RALI-010.
       RALI-020.
           ADD STTR-ORDERQTY TO WS-STTR-ORDERQTY
           ADD STTR-SHIPQTY  TO WS-STTR-SHIPQTY
           GO TO RALI-010.
       RALI-999.
           EXIT.
      *
       CANCEL-STOCK-TRANS SECTION.
       CAN-TRANS-000.
           IF B-NEWLINE (SUB-1) = " "
               GO TO CAN-TRANS-999.
           MOVE WS-INVOICE        TO STTR-REFERENCE1.
           MOVE 4                 TO STTR-TYPE.
           MOVE B-STTRANS (SUB-1) TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY.
           READ STOCK-TRANS-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
               MOVE 
          "ST-TRANS BUSY ON CANCEL-TRANS, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-STTRANS-ST1
               GO TO CAN-TRANS-000.
       CAN-TRANS-002.
            DELETE STOCK-TRANS-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
               MOVE 
          "ST-TRANS BUSY ON DELETE, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-STTRANS-ST1
               GO TO CAN-TRANS-002.
       CAN-TRANS-999.
            EXIT.
      *
       CHANGE-STOCK-TRANS SECTION.
       CHGE-TRANS-000.
           IF B-NEWLINE (SUB-1) = " "
               GO TO CHGE-TRANS-999.
           MOVE WS-INVOICE        TO STTR-REFERENCE1
           MOVE 4                 TO STTR-TYPE
           MOVE B-STTRANS (SUB-1) TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
           READ STOCK-TRANS-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "BAD START ON CHANGE-ST-TRANS, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INVOICE TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE B-STTRANS (SUB-1) TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STTRANS-ST1
              GO TO CHGE-TRANS-000.
       CHGE-TRANS-002.
           IF F-NAMEFIELDRED1 = "D"
            IF STTR-SHIPQTY > B-SHIPQTY (SUB-1)
               MOVE WS-DATE                 TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE DISPLAY-DATE            TO WS-DAILY-1ST1
               MOVE "PS#"                   TO WS-DAILY-1ST2
               MOVE WS-INVOICE              TO WS-DAILY-1ST3
               MOVE B-STOCKNUMBER (SUB-1)   TO WS-DAILY-2ND
               MOVE "BEFORE CHG"            TO WS-DAILY-3RD1
               MOVE STTR-SHIPQTY            TO F-EDNAMEFIELDQTY
               MOVE F-EDNAMEFIELDQTY        TO WS-DAILY-3RD2
               MOVE "AFTER CHNG"            TO WS-DAILY-4TH1
               MOVE B-SHIPQTY (SUB-1)       TO F-EDNAMEFIELDQTY
               MOVE F-EDNAMEFIELDQTY        TO WS-DAILY-4TH2
               PERFORM WRITE-DAILY.
               
           MOVE B-ORDERQTY (SUB-1)          TO STTR-ORDERQTY
           MOVE B-SHIPQTY (SUB-1)           TO STTR-SHIPQTY.
           MOVE B-SHIPPEDQTY (SUB-1)        TO STTR-SHIPPEDQTY.
      *      IF WS-LINECHANGED = "C"
      *         MOVE 0                       TO STTR-SHIPPEDQTY.
           MOVE B-NETT (SUB-1)              TO STTR-SALES-VALUE
           MOVE B-STOCKPRICE (SUB-1)        TO STTR-PRICE
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO STTR-DESC1
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO STTR-DESC2
           MOVE B-TAX (SUB-1)               TO STTR-TAX
           MOVE B-UNIT (SUB-1)              TO STTR-UNIT
           MOVE B-DISCOUNTPERITEM (SUB-1)   TO STTR-ITEMDISC
           MOVE B-STOCKCOST (SUB-1)         TO STTR-COST-VALUE.
       CHGE-TRANS-005.
           REWRITE STOCK-TRANS-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "BAD REWRITE ON CHANGE-ST-TRANS, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO CHGE-TRANS-005.
           IF STTR-TRANSACTION-NUMBER = 0
              MOVE
             "ST-TRANS-NO = 0 AT CHGE-TRANS-002, ADD 1 & GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              ADD 1 TO STTR-TRANSACTION-NUMBER
              GO TO CHGE-TRANS-005.
       CHGE-TRANS-999.
            EXIT.
      *
       DISPLAY-LINE-ITEMS SECTION.
       DLI-010.
           IF WS-STOCK-ST1 NOT = 0
                GO TO DLI-999.
           MOVE "STOCKNUMBER"  TO F-FIELDNAME
           MOVE 11             TO F-CBFIELDNAME
           MOVE ST-STOCKNUMBER TO F-NAMEFIELD
           MOVE 15             TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "STOCKDESCRIPTION" TO F-FIELDNAME
           MOVE 16                 TO F-CBFIELDNAME
           MOVE ST-DESCRIPTION1    TO F-NAMEFIELD
           MOVE 20                 TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "PERUNIT"        TO F-FIELDNAME
           MOVE 7                TO F-CBFIELDNAME
           MOVE ST-UNITOFMEASURE TO F-NAMEFIELD
           MOVE 4                TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "STOCKPRICE" TO F-FIELDNAME
           MOVE 10           TO F-CBFIELDNAME
           MOVE ST-PRICE     TO F-EDNAMEFIELDAMOUNT
           MOVE 9            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-AMOUNT.

           MOVE "STOCKCOST"    TO F-FIELDNAME
           MOVE 9              TO F-CBFIELDNAME
           MOVE ST-AVERAGECOST TO F-EDNAMEFIELDAMOUNT
           MOVE 9              TO F-CBFIELDLENGTH.
           IF WS-COST-DISPLAY = "N"
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
           ELSE
               PERFORM WRITE-FIELD-AMOUNT.
           MOVE "DISCOUNTPERITEM" TO F-FIELDNAME
           MOVE 15                TO F-CBFIELDNAME.
           IF WS-ZERODIS = "Y"
               MOVE WS-INVOICEDISCOUNT TO F-EDNAMEFIELDAMOUNTDIS
               MOVE 5 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-AMOUNTDIS
               GO TO DLI-0130.
           IF WS-INVOICEDISCOUNT > 0
               MOVE WS-INVOICEDISCOUNT TO F-EDNAMEFIELDAMOUNTDIS
               MOVE 5 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-AMOUNTDIS
               GO TO DLI-0130.
           IF WS-DISCOUNT-CODE = "0" OR = " "
                MOVE 0 TO F-EDNAMEFIELDAMOUNTDIS.
           IF WS-DISCOUNT-CODE = "1"
                MOVE ST-DISCOUNT1 TO F-EDNAMEFIELDAMOUNTDIS.
           IF WS-DISCOUNT-CODE = "2"
                MOVE ST-DISCOUNT2 TO F-EDNAMEFIELDAMOUNTDIS.
           IF WS-DISCOUNT-CODE = "3"
                MOVE ST-DISCOUNT3 TO F-EDNAMEFIELDAMOUNTDIS.
           IF WS-DISCOUNT-CODE = "4"
                MOVE ST-DISCOUNT4 TO F-EDNAMEFIELDAMOUNTDIS.
           IF WS-DISCOUNT-CODE = "5"
                MOVE ST-DISCOUNT5 TO F-EDNAMEFIELDAMOUNTDIS.
           IF WS-DISCOUNT-CODE = "6"
                MOVE ST-DISCOUNT6 TO F-EDNAMEFIELDAMOUNTDIS.
           IF WS-DISCOUNT-CODE = "7"
                MOVE ST-DISCOUNT7 TO F-EDNAMEFIELDAMOUNTDIS.
           IF WS-DISCOUNT-CODE = "8"
                MOVE ST-DISCOUNT8 TO F-EDNAMEFIELDAMOUNTDIS.
           IF WS-DISCOUNT-CODE = "9"
                MOVE ST-DISCOUNT9 TO F-EDNAMEFIELDAMOUNTDIS.
       DLI-0130.
      *      MOVE "TAX" TO F-FIELDNAME
      *      MOVE 3     TO F-CBFIELDNAME.
            IF WS-GSTNO = "EXPORT" OR = "EXPORT      "
                MOVE "N" TO F-NAMEFIELD
                            B-TAX (SUB-1)
            ELSE
                MOVE "Y" TO F-NAMEFIELD
                            B-TAX (SUB-1).
      *      MOVE 1 TO F-CBFIELDLENGTH
      *      PERFORM WRITE-FIELD-ALPHA.
       DLI-020.
           IF SP-1STCHAR NOT = "/"
              MOVE ST-QTYONHAND    TO WS-QTYONHAND
              MOVE ST-QTYONRESERVE TO WS-QTYONRESERVE
              MOVE ST-QTYONORDER   TO WS-QTYONORDER
              MOVE ST-QTYONBORDER  TO WS-QTYONBORDER
              MOVE 2110 TO POS
              DISPLAY WS-ONHAND-LINE AT POS.

           PERFORM SCROLL-050.
       DLI-999.
           EXIT.
      *
       RUNNING-TOTAL SECTION.
       RUN-000.
           MOVE 1 TO SUB-3.
           MOVE 0 TO WS-WORKTOTAL
                     WS-WORKTOTAL2
                     WS-COSTTOTAL
                     WS-MARGIN
                     WS-PRICETOTAL
                     WS-PERC.
       RUN-010.
           IF B-STOCKNUMBER (SUB-3) = " "
                GO TO RUN-020.
           IF B-NEWLINE (SUB-1) = "Y" OR = "L"
                GO TO RUN-015.
           MOVE B-STOCKNUMBER (SUB-3) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
                GO TO RUN-015.
           COMPUTE WS-WORKTOTAL =
               (B-SHIPQTY (SUB-3) * B-STOCKPRICE (SUB-3)).
           COMPUTE WS-DISCOUNT ROUNDED = ((B-SHIPQTY (SUB-3) * 
             B-STOCKPRICE (SUB-3)) * B-DISCOUNTPERITEM (SUB-3)) / 100.
           SUBTRACT WS-DISCOUNT FROM WS-WORKTOTAL.
           ADD WS-WORKTOTAL TO WS-PRICETOTAL.
           IF B-TAX (SUB-3) = "Y"
               COMPUTE WS-WORKTOTAL ROUNDED = 
               (WS-WORKTOTAL + (WS-WORKTOTAL * WS-GST-PERCENT / 100)).
           ADD WS-WORKTOTAL TO WS-WORKTOTAL2.
           IF B-SHIPQTY (SUB-3) > 0
               COMPUTE WS-COSTTOTAL = (WS-COSTTOTAL +
                 B-STOCKCOST (SUB-3) * B-SHIPQTY (SUB-3)).
           IF WS-WORKTOTAL2 > 9999999.99
              MOVE "** YOUR ORDER VALUE HAS EXCEEDED R9,999,999.99 **"
              TO WS-MESSAGE
              PERFORM ERROR-000
              MOVE " **  YOU MUST CANCEL YOUR LAST STOCK LINE ! **"
              TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR-020
              PERFORM ERROR1-020
              SUBTRACT 1 FROM SUB-1
              IF F-INDEX > 1
                SUBTRACT 1 FROM F-INDEX
                PERFORM SCROLLING
                GO TO RUN-999
              ELSE
                PERFORM SCROLLING
                GO TO RUN-999.
       RUN-015.
           ADD 1 TO SUB-3.
           IF SUB-3 > 200
               GO TO RUN-020.
           GO TO RUN-010.
       RUN-020.
           COMPUTE WS-MARGIN = WS-PRICETOTAL - WS-COSTTOTAL.
           IF WS-QUES-MU-GP-PERC = "N"
            COMPUTE WS-PERC ROUNDED = (WS-MARGIN / WS-COSTTOTAL) * 100
           ELSE
            COMPUTE WS-PERC ROUNDED = (WS-MARGIN / WS-PRICETOTAL) * 100.

           IF WS-QUES-MU-GP-PERC = "N"
              MOVE 2459 TO POS
              DISPLAY "M/U Perc:" AT POS
           ELSE
              MOVE 2459 TO POS
              DISPLAY "G/P Perc:" AT POS.

           MOVE "TOTALCOST"     TO F-FIELDNAME.
           MOVE 9               TO F-CBFIELDNAME.
           IF WS-COST-DISPLAY = "N"
              MOVE 0            TO F-EDNAMEFIELD9MIL
           ELSE
              MOVE WS-COSTTOTAL TO F-EDNAMEFIELD9MIL.
           MOVE 10              TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-9MIL.

           MOVE "TOTALPERCENT" TO F-FIELDNAME.
           MOVE 12             TO F-CBFIELDNAME.
           IF WS-COST-DISPLAY = "N"
              MOVE 0           TO F-EDNAMEFIELD9MIL
           ELSE
              MOVE WS-PERC     TO F-EDNAMEFIELD9MIL.
           MOVE 10             TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-9MIL.

           MOVE "SUBTOTAL"    TO F-FIELDNAME.
           MOVE 8             TO F-CBFIELDNAME.
           MOVE WS-WORKTOTAL2 TO F-EDNAMEFIELD9MIL.
           MOVE 10            TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-9MIL.
       RUN-999.
           EXIT.
      *
       CALCULATE-TOTALS SECTION.
       CT-000.
           MOVE 1 TO SUB-1.
           MOVE 0 TO WS-TAXABLETOTAL WS-NONTAXABLETOTAL
                WS-WORKTOTAL WS-DISCOUNT WS-COSTTOTAL WS-TAXAMT
                WS-PRICETOTAL WS-EXPORTTOTAL WS-DISCOUNTREG.
       CT-010.
           IF B-STOCKNUMBER (SUB-1) = " "
               GO TO CT-020.
           IF B-NEWLINE (SUB-1) = "Y" OR = "L"
               GO TO CT-015.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               GO TO CT-015.
           COMPUTE WS-WORKTOTAL =
               B-SHIPQTY (SUB-1) * B-STOCKPRICE (SUB-1).
           COMPUTE WS-DISCOUNT ROUNDED =
              WS-WORKTOTAL * B-DISCOUNTPERITEM (SUB-1) / 100.
           ADD WS-DISCOUNT TO WS-DISCOUNTREG.
           SUBTRACT WS-DISCOUNT FROM WS-WORKTOTAL.
           COMPUTE WS-COSTTOTAL = (WS-COSTTOTAL +
               B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1)).
           IF WS-GSTNO NOT = "EXPORT"
               ADD WS-WORKTOTAL TO WS-TAXABLETOTAL
            ELSE
               ADD WS-WORKTOTAL TO WS-EXPORTTOTAL.
           ADD WS-WORKTOTAL TO WS-PRICETOTAL.
           MOVE WS-WORKTOTAL TO B-NETT (SUB-1).
       CT-015.
           ADD 1 TO SUB-1.
           IF SUB-1 > 200
               GO TO CT-020.
           IF SUB-1 < 201
               GO TO CT-010.
       CT-020.
           COMPUTE WS-SUBTOTAL = WS-TAXABLETOTAL + 
                                 WS-NONTAXABLETOTAL + 
                                 WS-EXPORTTOTAL.
           COMPUTE WS-ADDONAMT = WS-ADDONFREIGHT + WS-POSTADDON + 
                                 WS-HANDADDON + WS-MISCADDON.
      *     COMPUTE WS-TAXAMT ROUNDED = WS-TAXABLETOTAL * 
      *                                 WS-GST-PERCENT / 100.
           IF WS-GSTNO NOT = "EXPORT"
             COMPUTE WS-TAXAMT ROUNDED =
               (WS-TAXABLETOTAL + WS-ADDONAMT) * WS-GST-PERCENT / 100.
       CT-999.
             EXIT.
      *
       CALCULATE-ORDER-TOTAL SECTION.
       CTOS-000.
           MOVE 1 TO SUB-1.
           MOVE 0 TO WS-WORKTOTAL1 WS-DISCOUNT1 WS-COSTTOTAL1
                     WS-PRICETOTAL1 WS-DISCOUNTREG1.
       CTOS-010.
           IF B-STOCKNUMBER (SUB-1) = " "
                 GO TO CTOS-999.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
                 GO TO CTOS-015.
           COMPUTE WS-WORKTOTAL1 =
                 B-ORDERQTY (SUB-1) * B-STOCKPRICE (SUB-1).
           COMPUTE WS-DISCOUNT1 ROUNDED =
                WS-WORKTOTAL1 * B-DISCOUNTPERITEM (SUB-1) / 100.
           ADD WS-DISCOUNT1 TO WS-DISCOUNTREG1.
           SUBTRACT WS-DISCOUNT1 FROM WS-WORKTOTAL1.
           COMPUTE WS-COSTTOTAL1 = WS-COSTTOTAL1 +
                 (B-STOCKCOST (SUB-1) * B-ORDERQTY (SUB-1)).
           ADD WS-WORKTOTAL1 TO WS-PRICETOTAL1.
       CTOS-015.
           ADD 1 TO SUB-1.
           IF SUB-1 < 201
                 GO TO CTOS-010.
       CTOS-999.
           EXIT.
      *
       SET-GST SECTION.
       SG-000.
            MOVE 1 TO SUB-1.
       SG-010.
            IF WS-GSTNO NOT = "EXPORT" AND NOT = "EXPORT       "
               MOVE "Y" TO B-TAX (SUB-1)
            ELSE
               MOVE "N" TO B-TAX (SUB-1).
      *TAKEN OUT WHEN "CHANGE-DISCOUNT-PER-LINE" SECTION CHANGED
      * FOR NEW DISCOUNT INFO 17/3/1999
      *      IF WS-INVOICEDISCOUNT > 0
      *         MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1).
      *      IF WS-INVOICEDISCOUNT = 0
      *       IF F-EXIT-CH = X"1D"
      *         MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 201
               GO TO SG-010.
       SG-999.
            EXIT.
      *
       CHECK-ORDER-COMPLETE SECTION.
       COC-010.
           MOVE 0 TO SUB-1.
           MOVE "Y" TO WS-ORDER-COMPLETE.
       COC-015.
           ADD 1 TO SUB-1.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
                 GO TO COC-020.
           IF B-STOCKNUMBER (SUB-1) = "    "
              GO TO COC-999.
           IF B-NEWLINE (SUB-1) NOT = "Y" AND NOT = "L"
              MOVE "N" TO WS-ORDER-COMPLETE
              GO TO COC-999.
       COC-020.
           IF SUB-1 < 200
              GO TO COC-015.
       COC-999.
           EXIT.
      *
       CHECK-REPAIR-COMPLETE SECTION.
       CREPC-010.
           MOVE 0 TO SUB-1.
           MOVE "Y" TO WS-ORDER-COMPLETE.
       CREPC-015.
           ADD 1 TO SUB-1.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
                 GO TO CREPC-020.
           IF B-STOCKNUMBER (SUB-1) = "    "
              GO TO CREPC-999.
              
           IF B-REPAIR (SUB-1) = "R"
               GO TO CREPC-020.
           IF B-REPAIR (SUB-1) = "P"
            IF B-ORDERQTY (SUB-1) >
                 B-SHIPPEDQTY (SUB-1) + B-SHIPQTY (SUB-1)
               MOVE "N" TO WS-ORDER-COMPLETE
               GO TO CREPC-999.
       CREPC-020.
           IF SUB-1 < 200
              GO TO CREPC-015.
       CREPC-999.
           EXIT.
      *
       CHECK-FOR-BORDERS SECTION.
       CFBO-010.
           MOVE 0 TO SUB-1.
           MOVE "N" TO WS-BORDERS-FOUND.
       CFBO-015.
           ADD 1 TO SUB-1.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
                 GO TO CFBO-020.
           IF B-STOCKNUMBER (SUB-1) = "    "
              GO TO CFBO-999.
           IF B-NEWLINE (SUB-1) = "Y" OR = "L"
              GO TO CFBO-020.
           IF B-ORDERQTY (SUB-1) NOT = 
               B-SHIPQTY (SUB-1) + B-SHIPPEDQTY (SUB-1)
               MOVE "Y" TO WS-BORDERS-FOUND
               GO TO CFBO-999.
       CFBO-020.
           IF SUB-1 < 200
              GO TO CFBO-015.
       CFBO-999.
           EXIT.
      *
       WRITE-INCR-REGISTER SECTION.
       WRIC-050.
            MOVE WS-INVOICE        TO INCR-INVOICE
            MOVE 4                 TO INCR-TRANS
            MOVE WS-ACCOUNT-NUMBER TO INCR-ACCOUNT
            MOVE WS-POORDERNO      TO INCR-PORDER
            MOVE WS-GSTNO          TO INCR-GSTNO.
            IF WS-NEWORDER = "Y"
               MOVE WS-DATE        TO INCR-DATE.
            IF F-NAMEFIELDRED1 = "Q" OR = "R"
               MOVE WS-DATE        TO INCR-DATE.
            MOVE WSAN-CODE         TO INCR-SALES
            MOVE WS-INVOICETOTAL   TO INCR-INVCRED-AMT
            MOVE WS-TAXAMT         TO INCR-TAX
            MOVE WS-ADDONAMT       TO INCR-ADDONS
            MOVE WS-DISCOUNTREG    TO INCR-DISCOUNT
            MOVE WS-COSTTOTAL      TO INCR-INVCRED-COST
            MOVE WS-SOLD-BY        TO INCR-SB-TYPE
            MOVE WS-DRTRANS-NO     TO INCR-DRTRANS-NO.
           IF WS-PART-ORDERS = "N"
            IF WS-BORDERS-FOUND = "Y"
                MOVE "N" TO INCR-PART-ORDERS.
           IF WS-BORDERS-FOUND = "N"
                MOVE "Y" TO INCR-PART-ORDERS.
       WRIC-051.
            IF WS-ORDER-COMPLETE = "Y"
               MOVE "L"            TO INCR-PRINTED
               GO TO WRIC-055.
            IF WS-ORDER-COMPLETE = "N"
             IF WS-SUBTOTAL = 0
              IF INCR-PRINTED NOT = "S"
               MOVE "N"            TO INCR-PRINTED
               GO TO WRIC-055.
      *      IF INCR-PRINTED NOT = "S" AND NOT = "R"
            IF INCR-PRINTED NOT = "S" AND NOT = "L" AND NOT = "T"
               MOVE "P"            TO INCR-PRINTED
               GO TO WRIC-055.
            IF INCR-PRINTED = "R"
               MOVE "P"            TO INCR-PRINTED.
       WRIC-055.
            MOVE WS-NAME           TO INCR-NAME
            MOVE WS-ADD1           TO INCR-ADD1
            MOVE WS-ADD2           TO INCR-ADD2
            MOVE WS-ADD3           TO INCR-ADD3
            MOVE WS-POSTCODE       TO INCR-CODE
            MOVE WS-DELADD1        TO INCR-DEL1
            MOVE WS-DELADD2        TO INCR-DEL2
            MOVE WS-DELADD3        TO INCR-DEL3
            MOVE WS-TERMOFSALE     TO INCR-TERMS
            MOVE WS-PHONE          TO INCR-PHONE
            MOVE WS-CONTACT        TO INCR-CONTACT
            MOVE 0                 TO INCR-PULL-DATE
                                      INCR-PULL-TIME
            MOVE "  "              TO INCR-PULLBY
            MOVE WS-DELIVERVIA     TO INCR-DELIVERY
            MOVE WS-BINNO          TO INCR-BIN
            MOVE WS-COMMENTLINE    TO INCR-COMMENT.
            IF WS-NEWORDER = "N"
               MOVE WS-BO-INVOICE  TO INCR-BO-INV-NO
               MOVE WS-BO-DATE     TO INCR-BO-DATE
            ELSE
               MOVE INCR-INVOICE   TO INCR-BO-INV-NO
               MOVE WS-DATE        TO INCR-BO-DATE.
            MOVE WS-POSTADDON      TO INCR-ADDPOST
            MOVE WS-ADDONFREIGHT   TO INCR-ADDFREIGHT
            MOVE WS-HANDADDON      TO INCR-ADDLABOUR
            MOVE WS-MISCADDON      TO INCR-ADDMISC.
            MOVE SUB-20            TO INCR-LINENO.
       WRIC-060.
            IF WS-ACCEPT = "Y"
                GO TO WRIC-065.
            IF WS-NEWORDER = "N"
                GO TO WRIC-065.
       WRIC-061.
            WRITE INCR-REC
                  INVALID KEY NEXT SENTENCE
            IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER ERC ON WRITE <WRIC-061>, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO WRIC-061.
       WRIC-064.
            GO TO WRIC-999.
       WRIC-065.
            REWRITE INCR-REC
                  INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 NOT = 0
               MOVE 
               "REGISTER ERC ON REWRITE <WRIC-060>, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO WRIC-065.
       WRIC-999.
              EXIT.
      *
       CHECK-IF-ORDER-VALID SECTION.
       CIOV-000.
           MOVE WS-INVOICE TO INCR-INVOICE.
           MOVE 4          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               UNLOCK INCR-REGISTER
               MOVE 88 TO WS-INCR-ST1
               GO TO CIOV-999.
       CIOV-005.
           READ INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               UNLOCK INCR-REGISTER
               MOVE 88 TO WS-INCR-ST1
               GO TO CIOV-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY <CIOV-005>, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO CIOV-005.
           IF INCR-PRINTED = "Y" OR = "L"
               MOVE "THIS ORDER HAS BEEN INVOICED AND IS COMPLETE."
               TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE "        INVOICED ON:" TO WS-DAILY-1ST
               MOVE INCR-BO-INV-NO         TO F-EDNAMEFIELDNUM
               MOVE F-EDNAMEFIELDNUM       TO WS-DAILY-2ND
               MOVE " " TO WS-DAILY-3RD WS-DAILY-4TH
               MOVE WS-DAILY-MESSAGE       TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR-020
               UNLOCK INCR-REGISTER
               MOVE 88 TO WS-INCR-ST1
               GO TO CIOV-999.
           IF INCR-ACCOUNT NOT = WS-ACCOUNT-NUMBER
               MOVE "THIS ORDER IS NOT FOR THE SAME ACCOUNT NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               UNLOCK INCR-REGISTER
               MOVE 88 TO WS-INCR-ST1
               GO TO CIOV-999.
           MOVE INCR-LINENO TO SUB-30 WS-STTRANS-NO.
       CIOV-999.
           EXIT.
      *
       REWRITE-OLD-ORDER SECTION.
       ROOR-010.
           MOVE WS-INVOICE TO INCR-INVOICE.
           MOVE 4          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
       ROOR-050.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY <ROOR-050>, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO ROOR-050.
       ROOR-055.
           ADD 1 TO SUB-20
           MOVE SUB-20 TO INCR-LINENO.
       ROOR-060.
           REWRITE INCR-REC
                INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY <ROOR-060>, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO ROOR-060.
       ROOR-999.
              EXIT.
      *
       CHECK-REGISTER SECTION.
       CRS-050.
           MOVE " "               TO WS-INCR-ST1
           MOVE WS-ACCOUNT-NUMBER TO INCR-ACCOUNT
           MOVE WS-POORDERNO      TO INCR-PORDER.
           START INCR-REGISTER KEY NOT < INCR-ALT-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE "Y" TO WS-NEWORDER
               GO TO CRS-999.
       CRS-060.
           READ INCR-REGISTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
               MOVE 0 TO WS-INCR-ST1
               GO TO CRS-900.
           IF WS-INCR-ST1 NOT = 0
               MOVE "P/SLIP LOCKED @ ANOTHER TERMINAL, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO CRS-060.
           IF INCR-ACCOUNT = WS-ACCOUNT-NUMBER
            IF INCR-PORDER NOT = WS-POORDERNO
               MOVE "Y" TO WS-NEWORDER
               MOVE 0   TO INCR-COPY-NUMBER
               GO TO CRS-900.
           IF INCR-ACCOUNT NOT = WS-ACCOUNT-NUMBER
            IF INCR-PORDER = WS-POORDERNO
               MOVE "Y" TO WS-NEWORDER
               MOVE 0   TO INCR-COPY-NUMBER
               GO TO CRS-900.
           IF INCR-ACCOUNT NOT = WS-ACCOUNT-NUMBER
            IF INCR-PORDER NOT = WS-POORDERNO
               MOVE "Y" TO WS-NEWORDER
               MOVE 0   TO INCR-COPY-NUMBER
               GO TO CRS-900.
           IF INCR-ACCOUNT = WS-ACCOUNT-NUMBER
            IF INCR-PORDER = WS-POORDERNO
             IF INCR-INVOICE = WS-INVOICE
               MOVE "Y" TO WS-NEWORDER
               MOVE 0   TO INCR-COPY-NUMBER
               GO TO CRS-900.

           IF INCR-PRINTED NOT = "Y" AND NOT = "L"
            IF INCR-PRINTED = "P"
               MOVE "P" TO WS-NEWORDER
               GO TO CRS-999.
           IF INCR-PRINTED NOT = "Y" AND NOT = "L"
            IF INCR-TRANS NOT = 8
               MOVE "N" TO WS-NEWORDER
               MOVE INCR-INVOICE TO WS-INVOICE
               GO TO CRS-999
            ELSE
               MOVE "N" TO WS-NEWORDER
               GO TO CRS-999.
           IF INCR-PRINTED = "Y" OR = "L"
               MOVE "C" TO WS-NEWORDER
               MOVE INCR-INVOICE TO WS-INVOICE
               GO TO CRS-999.
           MOVE "Y" TO WS-NEWORDER.
           MOVE 0   TO INCR-COPY-NUMBER.
       CRS-900.
           UNLOCK INCR-REGISTER.
       CRS-999.
           EXIT.
      *
       READ-INVOICE-REGISTER SECTION.
       RIR-000.
           MOVE WS-INVOICE TO INCR-INVOICE.
           MOVE 4          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE "Y" TO WS-NEWORDER
               GO TO RIR-999.
       RIR-005.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-INCR-ST1
               MOVE "Y" TO WS-NEWORDER
               GO TO RIR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY <RIR-005>, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-005.
       RIR-006.
           IF INCR-PRINTED = "Y" OR = "L"
               MOVE "THIS ORDER HAS BEEN INVOICED AND IS COMPLETE."
               TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE "        INVOICED ON:" TO WS-DAILY-1ST
               MOVE INCR-BO-INV-NO         TO F-EDNAMEFIELDNUM
               MOVE F-EDNAMEFIELDNUM       TO WS-DAILY-2ND
               MOVE " " TO WS-DAILY-3RD WS-DAILY-4TH
               MOVE WS-DAILY-MESSAGE       TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR-020
               MOVE "C" TO WS-NEWORDER
               GO TO RIR-999.
           IF INCR-PRINTED = "P"
            IF F-EXIT-CH NOT = X"9B" AND NOT = X"1F" AND NOT = X"17"
                     AND NOT = X"15" AND NOT = X"C7"
             MOVE "THE ORDER IS IN THE STORE READY FOR PULLING, DON'T"
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE "CREATE A 2nd SLIP BEFORE THE ORIGINAL IS RETURNED."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             MOVE "C" TO WS-NEWORDER
             GO TO RIR-999.
       RIR-010.
           MOVE INCR-ACCOUNT        TO WS-ACCOUNT-NUMBER
           MOVE INCR-GSTNO          TO WS-GSTNO
           MOVE INCR-DATE           TO WS-INVOICEDATE
           MOVE INCR-SALES          TO WSAN-CODE
           MOVE INCR-INVCRED-AMT    TO WS-INVOICETOTAL
           MOVE INCR-TAX            TO WS-TAXAMT
           MOVE INCR-ADDONS         TO WS-ADDONAMT
           MOVE INCR-SB-TYPE        TO WS-SOLD-BY
           MOVE INCR-DRTRANS-NO     TO WS-DRTRANS-NO
           MOVE 1                   TO WS-STTRANS-NO.
           COMPUTE WS-SUBTOTAL =
                WS-INVOICETOTAL - (WS-TAXAMT + WS-ADDONAMT).
           MOVE INCR-NAME       TO WS-NAME
           MOVE INCR-ADD1       TO WS-ADD1
           MOVE INCR-ADD2       TO WS-ADD2
           MOVE INCR-ADD3       TO WS-ADD3
           MOVE INCR-CODE       TO WS-POSTCODE
           MOVE INCR-DEL1       TO WS-DELADD1
           MOVE INCR-DEL2       TO WS-DELADD2
           MOVE INCR-DEL3       TO WS-DELADD3
           MOVE INCR-TERMS      TO WS-TERMOFSALE
           MOVE INCR-PORDER     TO WS-POORDERNO
           MOVE INCR-CONTACT    TO WS-CONTACT
           MOVE INCR-AREA       TO WS-AREA
           MOVE INCR-PHONE      TO WS-PHONE
           MOVE INCR-DELIVERY   TO WS-DELIVERVIA
           MOVE INCR-BIN        TO WS-BINNO
           MOVE INCR-COMMENT    TO WS-COMMENTLINE
           MOVE INCR-ADDPOST    TO WS-POSTADDON
           MOVE INCR-ADDFREIGHT TO WS-ADDONFREIGHT
           MOVE INCR-ADDLABOUR  TO WS-HANDADDON
           MOVE INCR-ADDMISC    TO WS-MISCADDON
           MOVE INCR-BO-INV-NO  TO WS-BO-INVOICE
           MOVE INCR-BO-DATE    TO WS-BO-DATE
           MOVE INCR-LINENO     TO SUB-20 SUB-25.

           MOVE "N" TO WS-NEWORDER.
           IF INCR-PRINTED = "S"
             MOVE "Y" TO WS-WAS-SUSPENDED
           ELSE
             MOVE "N" TO WS-WAS-SUSPENDED.
       RIR-999.
           EXIT.
      *
       DELETE-INVOICE-REGISTER SECTION.
       DIR-010.
           MOVE 2910 TO POS.
           DISPLAY "DELETING ORDER-REGISTER................." AT POS.
           DELETE INCR-REGISTER
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
              MOVE "REGISTER BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-INCR-ST1
              GO TO DIR-010.
       DIR-999.
           EXIT.
      *
       DELETE-QUOTE-REGISTER SECTION.
       DQR-001.
           MOVE WS-QUOTE     TO INCR-INVOICE
           MOVE 8            TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               GO TO DQR-999.
       DQR-005.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               GO TO DQR-999.
           IF WS-INCR-ST1 NOT = 0
              MOVE "REGISTER BUSY READ DQR-010, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO DQR-005.
       DQR-010.
           DELETE INCR-REGISTER
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
              MOVE "REGISTER BUSY <DQR-010>, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO DQR-010.
       DQR-999.
           EXIT.
      *
       READ-QUOTE-BY-ACCOUNT SECTION.
       RQBA-000.
      *****************************************************************
      * WS-QUES-CHECK-QUOTES:  N=NO ACCOUNTS CHECKED AT ALL.          *
      *                        Y=CHECK ALL ACCOUNTS FOR QUOTES.       *
      *                        A=ACCOUNT QUOTES CHECKED, NO INTERNAL  *
      *                          COD A/C'S LIKE 0300150, 0300200 ETC. *
      *****************************************************************
           IF WS-QUES-CHECK-QUOTES = "N"
               GO TO RQBA-999.
           IF WS-QUES-CHECK-QUOTES = "A"
            IF WS-ACCOUNT-NUMBER = 0300087 OR = 0300090 OR = 0300100
                 OR = 0300150 OR = 0300200 OR = 9999999 OR = 0300500
                 OR = 0300501 OR = 0300502 OR = 0300503 OR = 0300504
                 OR = 0300505 OR = 0300506 OR = 0300507 OR = 0300508
                 OR = 0300509 OR = 0300510
               GO TO RQBA-999.
           MOVE 2910 TO POS
           DISPLAY "READING ST-QUOTE-BY-ACCOUNT.........." AT POS.
           MOVE "Q"               TO STTR-AC-COMPLETE
           MOVE WS-ACCOUNT-NUMBER TO STTR-ACCOUNT-NUMBER
           START STOCK-TRANS-FILE KEY NOT < STTR-AC-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE 0 TO STTR-TYPE
              GO TO RQBA-999.
       RQBA-010.
           READ STOCK-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              PERFORM ERROR1-020
              GO TO RQBA-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS RECORD BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RQBA-010.
           IF STTR-ACCOUNT-NUMBER NOT = WS-ACCOUNT-NUMBER
              PERFORM ERROR1-020
              GO TO RQBA-999.
           IF STTR-TYPE NOT = 8
              GO TO RQBA-010.
              
           IF STTR-STOCK-NUMBER NOT = B-STOCKNUMBER (SUB-1)
              GO TO RQBA-010.

           IF STTR-STOCK-NUMBER = B-STOCKNUMBER (SUB-1)
              MOVE "QUOTE#"        TO WS-FIL1
              MOVE STTR-REFERENCE1 TO WS-QU-NUM
              MOVE " DATE:"        TO WS-FIL2
              MOVE STTR-DATE       TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE    TO WS-QU-DATE
              MOVE " QTY:"         TO WS-FIL3
              MOVE STTR-ORDERQTY   TO WS-QU-QTY
              MOVE " PRICE R"      TO WS-FIL4
              MOVE STTR-PRICE      TO WS-QU-PRICE
              MOVE " DISC%:"       TO WS-FIL5
              MOVE STTR-ITEMDISC   TO WS-QU-DISC
              
              PERFORM ERROR1-020
              MOVE WS-QUOTE-FOUND-MESSAGE TO WS-MESSAGE2
              PERFORM ERROR3-MESSAGE.
           PERFORM ERROR1-020.
       RQBA-999.
           EXIT.
      *
       READ-QUOTE-REGISTER SECTION.
       RQR-000.
           MOVE WS-QUOTE   TO INCR-INVOICE.
           MOVE 8          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE "Y" TO WS-NEWORDER
               GO TO RQR-999.
       RQR-005.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-INCR-ST1
               MOVE "Y" TO WS-NEWORDER
               GO TO RQR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ-RQR-005, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RQR-005.
           IF INCR-PRINTED = "Y" OR = "L"
               MOVE "AN ORDER HAS BEEN ENTERED FOR THIS QUOTE."
               TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE "      ORDER NUMBER :" TO WS-DAILY-1ST
               MOVE INCR-BO-INV-NO         TO F-EDNAMEFIELDNUM
               MOVE F-EDNAMEFIELDNUM       TO WS-DAILY-2ND
               MOVE " " TO WS-DAILY-3RD WS-DAILY-4TH
               MOVE WS-DAILY-MESSAGE       TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR-020
               MOVE "C" TO WS-NEWORDER
               GO TO RQR-999.
       RQR-010.
           MOVE INCR-ACCOUNT        TO WS-ACCOUNT-NUMBER
           MOVE INCR-GSTNO          TO WS-GSTNO
           MOVE INCR-DATE           TO WS-INVOICEDATE
           MOVE INCR-SALES          TO WSAN-CODE
           MOVE INCR-INVCRED-AMT    TO WS-INVOICETOTAL
           MOVE INCR-TAX            TO WS-TAXAMT
           MOVE INCR-ADDONS         TO WS-ADDONAMT
           MOVE INCR-SB-TYPE        TO WS-SOLD-BY
           MOVE INCR-DRTRANS-NO     TO WS-DRTRANS-NO
           MOVE 1                   TO WS-STTRANS-NO.
           COMPUTE WS-SUBTOTAL =
                WS-INVOICETOTAL - (WS-TAXAMT + WS-ADDONAMT).
           MOVE INCR-NAME       TO WS-NAME
           MOVE INCR-ADD1       TO WS-ADD1
           MOVE INCR-ADD2       TO WS-ADD2
           MOVE INCR-ADD3       TO WS-ADD3
           MOVE INCR-CODE       TO WS-POSTCODE
           MOVE INCR-DEL1       TO WS-DELADD1
           MOVE INCR-DEL2       TO WS-DELADD2
           MOVE INCR-DEL3       TO WS-DELADD3
           MOVE INCR-TERMS      TO WS-TERMOFSALE
           MOVE INCR-PORDER     TO WS-POORDERNO
           MOVE INCR-CONTACT    TO WS-CONTACT
           MOVE INCR-PHONE      TO WS-PHONE
           MOVE INCR-DELIVERY   TO WS-DELIVERVIA
           MOVE INCR-BIN        TO WS-BINNO
           MOVE INCR-COMMENT    TO WS-COMMENTLINE
           MOVE INCR-ADDPOST    TO WS-POSTADDON
           MOVE INCR-ADDFREIGHT TO WS-ADDONFREIGHT
           MOVE INCR-ADDLABOUR  TO WS-HANDADDON
           MOVE INCR-ADDMISC    TO WS-MISCADDON
           MOVE INCR-BO-INV-NO  TO WS-BO-INVOICE
           MOVE INCR-BO-DATE    TO WS-BO-DATE
           MOVE INCR-LINENO     TO SUB-20 SUB-25.

           MOVE "N" TO WS-NEWORDER.
       RQR-999.
           EXIT.
      *
       READ-QUOTE-TRANSACTIONS SECTION.
       RSQT-000.
           MOVE 2910 TO POS
           DISPLAY "READING QUOTE-ST-TRANSACTIONS..........." AT POS
           MOVE 1          TO SUB-1
           MOVE WS-QUOTE   TO STTR-REFERENCE1
           MOVE 8          TO STTR-TYPE
           MOVE 0          TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
       RSQT-010.
           READ STOCK-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              MOVE 0 TO STTR-TYPE
              GO TO RSQT-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS QUOTE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RSQT-010.
           IF STTR-REFERENCE1 NOT = WS-QUOTE
              GO TO RSQT-999.
           IF STTR-TYPE NOT = 8
              GO TO RSQT-010.
           MOVE STTR-TRANSACTION-NUMBER TO B-STTRANS (SUB-1).
           MOVE STTR-STOCK-NUMBER       TO B-STOCKNUMBER (SUB-1)
                                           SPLIT-STOCK.
           MOVE " "               TO B-NEWLINE (SUB-1).
           MOVE STTR-INV-NO       TO B-INVOICED (SUB-1).
           IF SP-1STCHAR = "*"
               GO TO RSQT-020.
           MOVE STTR-DESC1        TO B-STOCKDESCRIPTION (SUB-1)
           MOVE STTR-DESC2        TO B-STOCKDESCRIPTION2 (SUB-1)
           MOVE STTR-SHIPQTY      TO B-SHIPQTY (SUB-1)
           MOVE STTR-ORDERQTY     TO B-ORDERQTY (SUB-1)
           MOVE STTR-SHIPPEDQTY   TO B-SHIPPEDQTY (SUB-1)
           MOVE STTR-PRICE        TO B-STOCKPRICE (SUB-1)
           MOVE STTR-COST-VALUE   TO B-STOCKCOST (SUB-1)
           MOVE STTR-ITEMDISC     TO B-DISCOUNTPERITEM (SUB-1)
           MOVE STTR-TAX          TO B-TAX (SUB-1)
           MOVE STTR-UNIT         TO B-UNIT (SUB-1)
           MOVE STTR-SALES-VALUE  TO B-NETT (SUB-1)
           COMPUTE WS-COSTTOTAL = WS-COSTTOTAL +
               (B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1))
           GO TO RSQT-050.
       RSQT-020.
           MOVE COMMENT-FIELDS    TO C-LINE (SUB-1).
       RSQT-050.
           ADD 1 TO SUB-1
           MOVE SUB-1 TO SUB-25.
           IF SUB-1 > 200
              MOVE 200 TO SUB-1 SUB-25
              GO TO RSQT-999.
           GO TO RSQT-010.
       RSQT-999.
           EXIT.
      *
       DELETE-QUOTE-TRANS SECTION.
       DQT-000.
           MOVE 2910 TO POS
           DISPLAY "DELETING QUOTE-ST-TRANSACTIONS.........." AT POS
           MOVE 1          TO SUB-1
           MOVE WS-QUOTE   TO STTR-REFERENCE1
           MOVE 8          TO STTR-TYPE
           MOVE 1          TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              GO TO DQT-999.
       DQT-010.
           READ STOCK-TRANS-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              GO TO DQT-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-QUOTE BUSY ON READ-DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO DQT-010.
           IF STTR-REFERENCE1 NOT = WS-QUOTE
              UNLOCK STOCK-TRANS-FILE
              GO TO DQT-999.
           IF STTR-TYPE NOT = 8
              UNLOCK STOCK-TRANS-FILE
              GO TO DQT-999.
       DQT-900.
           DELETE STOCK-TRANS-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-QUOTE-TRANS BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO DQT-900.
       DQT-950.
           GO TO DQT-010.
       DQT-999.
           EXIT.
      *
       READ-REPAIR-REGISTER SECTION.
       RRPR-000.
           MOVE WS-REPAIR  TO INCR-INVOICE.
           MOVE 3          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE "Y" TO WS-NEWORDER
               GO TO RRPR-999.
       RRPR-005.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-INCR-ST1
               MOVE "Y" TO WS-NEWORDER
               GO TO RRPR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY <RRPR-005>, PRESS 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RRPR-005.
           IF INCR-PRINTED = "Y" OR = "L"
               MOVE "AN ORDER HAS BEEN ENTERED FOR THIS REPAIR."
               TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE "      ORDER NUMBER :" TO WS-DAILY-1ST
               MOVE INCR-BO-INV-NO         TO F-EDNAMEFIELDNUM
               MOVE F-EDNAMEFIELDNUM       TO WS-DAILY-2ND
               MOVE " " TO WS-DAILY-3RD WS-DAILY-4TH
               MOVE WS-DAILY-MESSAGE       TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR-020
               MOVE "C" TO WS-NEWORDER
               GO TO RRPR-999.
       RRPR-010.
           MOVE INCR-ACCOUNT        TO WS-ACCOUNT-NUMBER
           MOVE INCR-GSTNO          TO WS-GSTNO
           MOVE INCR-DATE           TO WS-INVOICEDATE
           MOVE INCR-SALES          TO WSAN-CODE
           MOVE INCR-INVCRED-AMT    TO WS-INVOICETOTAL
           MOVE INCR-TAX            TO WS-TAXAMT
           MOVE INCR-ADDONS         TO WS-ADDONAMT
           MOVE INCR-SB-TYPE        TO WS-SOLD-BY
           MOVE INCR-DRTRANS-NO     TO WS-DRTRANS-NO
           MOVE 1                   TO WS-STTRANS-NO.
           COMPUTE WS-SUBTOTAL =
                WS-INVOICETOTAL - (WS-TAXAMT + WS-ADDONAMT).
           MOVE INCR-NAME       TO WS-NAME
           MOVE INCR-ADD1       TO WS-ADD1
           MOVE INCR-ADD2       TO WS-ADD2
           MOVE INCR-ADD3       TO WS-ADD3
           MOVE INCR-CODE       TO WS-POSTCODE
           MOVE INCR-DEL1       TO WS-DELADD1
           MOVE INCR-DEL2       TO WS-DELADD2
           MOVE INCR-DEL3       TO WS-DELADD3
           MOVE INCR-TERMS      TO WS-TERMOFSALE
           MOVE INCR-PORDER     TO WS-POORDERNO
           MOVE INCR-CONTACT    TO WS-CONTACT
           MOVE INCR-PHONE      TO WS-PHONE
           MOVE INCR-DELIVERY   TO WS-DELIVERVIA
           MOVE INCR-BIN        TO WS-BINNO
           MOVE INCR-COMMENT    TO WS-COMMENTLINE
           MOVE INCR-ADDPOST    TO WS-POSTADDON
           MOVE INCR-ADDFREIGHT TO WS-ADDONFREIGHT
           MOVE INCR-ADDLABOUR  TO WS-HANDADDON
           MOVE INCR-ADDMISC    TO WS-MISCADDON
           MOVE INCR-BO-INV-NO  TO WS-BO-INVOICE
           MOVE INCR-BO-DATE    TO WS-BO-DATE
           MOVE INCR-LINENO     TO SUB-20 SUB-25.

           MOVE "N" TO WS-NEWORDER.
       RRPR-999.
           EXIT.
      *
       REWRITE-REPAIR-REGISTER SECTION.
       RWRP-000.
           MOVE WS-REPAIR  TO INCR-INVOICE.
           MOVE 3          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               GO TO RWRP-999.
       RWRP-005.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
       RWRP-050.
           MOVE WS-ACCOUNT-NUMBER TO INCR-ACCOUNT
           MOVE WS-POORDERNO      TO INCR-PORDER
           MOVE WS-GSTNO          TO INCR-GSTNO.
           MOVE WSAN-CODE         TO INCR-SALES
           MOVE WS-INVOICETOTAL   TO INCR-INVCRED-AMT
           MOVE WS-TAXAMT         TO INCR-TAX
           MOVE WS-ADDONAMT       TO INCR-ADDONS
           MOVE WS-DISCOUNTREG    TO INCR-DISCOUNT
           MOVE WS-COSTTOTAL      TO INCR-INVCRED-COST
           MOVE WS-SOLD-BY        TO INCR-SB-TYPE
           MOVE "Y"               TO INCR-PART-ORDERS.
       RWRP-051.
           IF WS-ORDER-COMPLETE = "Y"
               MOVE "Y"           TO INCR-PRINTED
               GO TO RWRP-055.
       RWRP-055.
           MOVE WS-NAME           TO INCR-NAME
           MOVE WS-ADD1           TO INCR-ADD1
           MOVE WS-ADD2           TO INCR-ADD2
           MOVE WS-ADD3           TO INCR-ADD3
           MOVE WS-POSTCODE       TO INCR-CODE
           MOVE WS-DELADD1        TO INCR-DEL1
           MOVE WS-DELADD2        TO INCR-DEL2
           MOVE WS-DELADD3        TO INCR-DEL3
           MOVE WS-TERMOFSALE     TO INCR-TERMS
           MOVE WS-PHONE          TO INCR-PHONE
           MOVE WS-CONTACT        TO INCR-CONTACT
           MOVE 0                 TO INCR-PULL-DATE
                                     INCR-PULL-TIME
           MOVE "  "              TO INCR-PULLBY
           MOVE WS-DELIVERVIA     TO INCR-DELIVERY
           MOVE WS-BINNO          TO INCR-BIN
           MOVE WS-COMMENTLINE    TO INCR-COMMENT.
           MOVE WS-INVOICE        TO INCR-BO-INV-NO
           MOVE WS-DATE           TO INCR-BO-DATE.
           MOVE WS-POSTADDON      TO INCR-ADDPOST
           MOVE WS-ADDONFREIGHT   TO INCR-ADDFREIGHT
           MOVE WS-HANDADDON      TO INCR-ADDLABOUR
           MOVE WS-MISCADDON      TO INCR-ADDMISC.
           MOVE SUB-20            TO INCR-LINENO.
       RWRP-065.
           REWRITE INCR-REC
                INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE
            "REWRITING REPAIR REGISTER ERC <RWRP-065>, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RWRP-065.
       RWRP-999.
           EXIT.
     *
       READ-REPAIR-TRANSACTIONS SECTION.
       RSRPT-000.
           MOVE 2910 TO POS
           DISPLAY "READING REPAIR-ST-TRANSACTIONS..........." AT POS
           MOVE 1          TO SUB-1
           MOVE WS-REPAIR  TO STTR-REFERENCE1
           MOVE 3          TO STTR-TYPE
           MOVE 0          TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
       RSRPT-010.
           READ STOCK-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              MOVE 0 TO STTR-TYPE
              GO TO RSRPT-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS RSRPT-010 BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RSRPT-010.
           IF STTR-REFERENCE1 NOT = WS-REPAIR
              GO TO RSRPT-999.
           IF STTR-TYPE NOT = 3
              GO TO RSRPT-010.
              
           IF STTR-COMPLETE = "L" OR = "Y"
                GO TO RSRPT-010.
                
           MOVE STTR-TRANSACTION-NUMBER TO B-STTRANS (SUB-1).
           MOVE STTR-STOCK-NUMBER       TO B-STOCKNUMBER (SUB-1)
                                           SPLIT-STOCK.
           MOVE " "               TO B-NEWLINE (SUB-1).
           MOVE STTR-INV-NO       TO B-INVOICED (SUB-1).
           MOVE STTR-COMPLETE     TO B-REPAIR (SUB-1).
           IF SP-1STCHAR = "*"
               GO TO RSRPT-020.
           MOVE STTR-DESC1        TO B-STOCKDESCRIPTION (SUB-1)
           MOVE STTR-DESC2        TO B-STOCKDESCRIPTION2 (SUB-1)
           MOVE STTR-SHIPQTY      TO B-SHIPQTY (SUB-1)
           MOVE STTR-ORDERQTY     TO B-ORDERQTY (SUB-1)
           MOVE STTR-SHIPPEDQTY   TO B-SHIPPEDQTY (SUB-1)
           MOVE STTR-PRICE        TO B-STOCKPRICE (SUB-1)
           MOVE STTR-COST-VALUE   TO B-STOCKCOST (SUB-1)
           MOVE STTR-ITEMDISC     TO B-DISCOUNTPERITEM (SUB-1)
           MOVE STTR-TAX          TO B-TAX (SUB-1)
           MOVE STTR-UNIT         TO B-UNIT (SUB-1)
           MOVE STTR-SALES-VALUE  TO B-NETT (SUB-1)
           COMPUTE WS-COSTTOTAL = WS-COSTTOTAL +
               (B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1))
           GO TO RSRPT-050.
       RSRPT-020.
           MOVE COMMENT-FIELDS    TO C-LINE (SUB-1).
       RSRPT-050.
           ADD 1 TO SUB-1
           MOVE SUB-1 TO SUB-25.
           IF SUB-1 > 200
              MOVE 200 TO SUB-1 SUB-25
              GO TO RSRPT-999.
           GO TO RSRPT-010.
       RSRPT-999.
           EXIT.
      *
       REWRITE-REPAIR-ST-TRANS SECTION.
       RWREPST-00000.
           MOVE 1          TO SUB-1.
           MOVE 3          TO STTR-TYPE
           MOVE WS-REPAIR  TO STTR-REFERENCE1
           MOVE 0          TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
               INVALID KEY NEXT SENTENCE.
           IF B-STOCKNUMBER (SUB-1) = " "
              GO TO RWREPST-999.
       RWREPST-005.
           READ STOCK-TRANS-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE 
            "ST-TRANS BUSY REPAIR-READ-FOR WRITE, 'ESC TO SEE INFO."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE B-STOCKNUMBER (SUB-1) TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE STTR-TRANSACTION-NUMBER TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-STTRANS-ST1
              PERFORM ERROR1-020
              GO TO RWREPST-005.
              
           IF STTR-REFERENCE1 NOT = WS-REPAIR
                GO TO RWREPST-999.
       RWREPST-006.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
                                          STTR-STOCK-NUMBER.
            MOVE WS-INVOICE            TO STTR-INV-NO.

            IF WS-ORDER-COMPLETE = "Y"            
             IF B-REPAIR (SUB-1) NOT = "R"
               MOVE "Y"                TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE.
                
            IF WS-ORDER-COMPLETE NOT = "Y"
             IF B-REPAIR (SUB-1) = "R"
               MOVE "R"                TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE.
            IF SP-1STCHAR = "*"
                GO TO RWREPST-015.

            IF WS-ORDER-COMPLETE NOT = "Y"
             IF B-REPAIR (SUB-1) = "P"
              IF STTR-ORDERQTY = B-SHIPQTY (SUB-1) + STTR-SHIPPEDQTY
               MOVE "Y"                TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE
              ELSE
               MOVE "P"                TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE.
            IF B-REPAIR (SUB-1) = "R"
                GO TO RWREPST-018.
       RWREPST-010.
            IF B-NEWLINE (SUB-1) NOT = "Y" AND NOT = "L"
               ADD B-SHIPQTY (SUB-1)         TO STTR-SHIPPEDQTY.
            MOVE B-NETT (SUB-1)              TO STTR-SALES-VALUE
            MOVE B-STOCKPRICE (SUB-1)        TO STTR-PRICE
            MOVE B-STOCKDESCRIPTION (SUB-1)  TO STTR-DESC1
            MOVE B-STOCKDESCRIPTION2 (SUB-1) TO STTR-DESC2
            MOVE B-TAX (SUB-1)               TO STTR-TAX
            MOVE B-UNIT (SUB-1)              TO STTR-UNIT
            MOVE B-DISCOUNTPERITEM (SUB-1)   TO STTR-ITEMDISC
            MOVE B-STOCKCOST (SUB-1)         TO STTR-COST-VALUE
            GO TO RWREPST-018.
       RWREPST-015.
            IF B-REPAIR (SUB-1) NOT = "R"
             IF WS-ORDER-COMPLETE = "Y"
               MOVE "Y"           TO STTR-COMPLETE
                                     STTR-ST-COMPLETE
                                     STTR-AC-COMPLETE.
            IF B-REPAIR (SUB-1) = "R"
             IF WS-ORDER-COMPLETE = "Y"
               MOVE "Y"           TO STTR-COMPLETE
                                     STTR-ST-COMPLETE
                                     STTR-AC-COMPLETE
             ELSE
               MOVE "R"           TO STTR-COMPLETE
                                     STTR-ST-COMPLETE
                                     STTR-AC-COMPLETE.
            MOVE C-DESC (SUB-1)   TO COM-DESC
            MOVE C-UNIT (SUB-1)   TO COM-UNIT
            MOVE C-ORDER (SUB-1)  TO COM-ORDERQTY
            MOVE C-SHIP (SUB-1)   TO COM-SHIPQTY
            MOVE C-PRICE (SUB-1)  TO COM-PRICE
            MOVE C-COST (SUB-1)   TO COM-COST
            MOVE C-DISC (SUB-1)   TO COM-DISC
            MOVE " "              TO COM-FILLER.
       RWREPST-018.
           REWRITE STOCK-TRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "RWREPST-018 ERC ON REPAIR, NOTIFY THE SUPERVISOR."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE B-STOCKNUMBER (SUB-1) TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE STTR-TRANSACTION-NUMBER TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STTRANS-ST1
              PERFORM ERROR1-020.
       RWREPST-020.
            ADD 1 TO SUB-1.
            IF SUB-1 < 201
             IF B-STOCKNUMBER (SUB-1) = " "
                GO TO RWREPST-999.
            IF SUB-1 < 201
                GO TO RWREPST-005.
       RWREPST-999.
            EXIT.
      *
       RESERVE-STOCK-QUANTITIES SECTION.
       RSQ-000.
           IF SUB-30 = 0
                MOVE 1 TO WS-STTRANS-NO.
           MOVE 2910 TO POS
           DISPLAY "RESERVING STOCK QUANTITIES.............." AT POS
           MOVE 1 TO SUB-1
           MOVE 0 TO WS-BO-NUMBER.
       RSQ-002.
           IF B-STOCKNUMBER (SUB-1) = "    "
                 GO TO RSQ-999.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*" OR = "/"
                GO TO RSQ-040.
           IF WS-REPAIR > 0
            IF B-REPAIR (SUB-1) = "R"
                GO TO RSQ-040.
           MOVE B-STOCKNUMBER (SUB-1) TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       RSQ-005.
           READ STOCK-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               GO TO RSQ-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD IN USE <RSQ-005>, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RSQ-005.
       RSQ-010.
           IF B-ORDERQTY (SUB-1) NOT > ST-QTYONHAND
               SUBTRACT B-ORDERQTY (SUB-1) FROM ST-QTYONHAND
               ADD B-ORDERQTY (SUB-1)        TO ST-QTYONRESERVE
                                                ST-QTYONBORDER
               MOVE B-ORDERQTY (SUB-1)        TO B-SHIPQTY (SUB-1)
               GO TO RSQ-020.
           MOVE ST-QTYONHAND             TO B-SHIPQTY (SUB-1)
           ADD  ST-QTYONHAND             TO ST-QTYONRESERVE
           MOVE 0                        TO ST-QTYONHAND
           ADD B-ORDERQTY (SUB-1)        TO ST-QTYONBORDER.
           ADD 1 TO WS-BO-NUMBER.
       RSQ-020.
           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK-REWRITE ERROR <RSQ-020>, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RSQ-020.
       RSQ-040.
           PERFORM WNT-TRANS-002 THRU WNT-TRANS-800.
       RSQ-050.
           ADD 1 TO SUB-1 WS-STTRANS-NO.
           IF SUB-1 < 201
              GO TO RSQ-002.
           MOVE 1 TO SUB-1.
       RSQ-999.
           EXIT.
      *
       DELETE-STOCK-TRANS SECTION.
       DST-000.
           MOVE 2910 TO POS
           DISPLAY "DELETING ST-TRANS FILES................." AT POS
           MOVE 1          TO SUB-1
           MOVE WS-INVOICE TO STTR-REFERENCE1
           MOVE 4          TO STTR-TYPE
           MOVE 0          TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
       DST-010.
           READ STOCK-TRANS-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              MOVE 0 TO STTR-TYPE
              GO TO DST-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS BUSY ON READ-DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO DST-010.
           IF STTR-TYPE NOT = 4
              GO TO DST-999.
           IF STTR-REFERENCE1 NOT = WS-INVOICE
              GO TO DST-999.
           IF STTR-COMPLETE = "Y" OR = "L"
               GO TO DST-900.
               
           MOVE STTR-STOCK-NUMBER TO B-STOCKNUMBER (SUB-1)
                                     SPLIT-STOCK.
           IF SP-1STCHAR = "/" OR = "*"
              GO TO DST-900.
           MOVE STTR-SHIPQTY          TO B-SHIPQTY (SUB-1).
           COMPUTE B-ORDERQTY (SUB-1) = STTR-ORDERQTY - STTR-SHIPPEDQTY.
      *     MOVE STTR-ORDERQTY         TO B-ORDERQTY (SUB-1).
           MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           IF WS-ERR = "ERR"
              GO TO DST-900.

      *********************************************************
      *NEXT 7 LINES NEW FOR RE-AVERAGING THE COST IF ITEM     *
      *DELETED OFF THE P/SLIP JUST IN CASE ST-AVERAGECOST IS  *
      *DIFFERENT TO THE COST ON THE P/SLIP                    *
      *********************************************************
           COMPUTE WS-AVE-COST-OF-ALLOC-STOCK =
              B-SHIPQTY (SUB-1) * B-STOCKCOST (SUB-1).
           COMPUTE WS-AVE-COST-OF-STOCK = 
              ST-QTYONHAND * ST-AVERAGECOST.

           ADD B-SHIPQTY (SUB-1)            TO ST-QTYONHAND.

           COMPUTE ST-AVERAGECOST = 
              (WS-AVE-COST-OF-STOCK + WS-AVE-COST-OF-ALLOC-STOCK)
                  / ST-QTYONHAND.
           
           IF ST-QTYONRESERVE > B-SHIPQTY (SUB-1)
               SUBTRACT B-SHIPQTY (SUB-1) FROM ST-QTYONRESERVE
           ELSE
                MOVE 0                TO ST-QTYONRESERVE.
           IF ST-QTYONBORDER > B-ORDERQTY (SUB-1)
               SUBTRACT B-ORDERQTY (SUB-1) FROM ST-QTYONBORDER
           ELSE
                MOVE 0                TO ST-QTYONBORDER.
           IF ST-QTYONRESERVE = ST-QTYONBORDER
                GO TO DST-850.
           IF ST-QTYONRESERVE < ST-QTYONBORDER
                GO TO DST-850.
           COMPUTE WS-BO-QTY = ST-QTYONRESERVE - ST-QTYONBORDER.
           SUBTRACT WS-BO-QTY FROM ST-QTYONRESERVE.
           ADD WS-BO-QTY        TO ST-QTYONHAND.
       DST-850.
           REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE ST-STOCKNUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO DST-850.
       DST-900.
           DELETE STOCK-TRANS-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE STTR-STOCK-NUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STTRANS-ST1
              GO TO DST-900.
       DST-950.
           ADD 1 TO SUB-1.
           IF SUB-1 > 200
              GO TO DST-999.
           GO TO DST-010.
       DST-999.
           EXIT.
      *
       READ-STOCK-TRANSACTIONS SECTION.
       RSTT-000.
           MOVE 2910 TO POS
           DISPLAY "READING ST-TRANSACTIONS................." AT POS
           MOVE 1 TO SUB-1
           MOVE WS-INVOICE TO STTR-REFERENCE1
           MOVE 4          TO STTR-TYPE
           MOVE 0          TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
       RSTT-010.
           READ STOCK-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              MOVE 0 TO STTR-TYPE
              GO TO RSTT-999.
           IF WS-STTRANS-ST1 NOT = 0
               MOVE 
              "ST-TRANS BUSY ON READ-NEXT, RSTT-010, 'ESC' TO SEE MORE."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 
            "ST-TRANS STOCK ITEM SHOWN BELOW IS LOCKED, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE STTR-STOCK-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
              GO TO RSTT-010.
           IF STTR-REFERENCE1 NOT = WS-INVOICE
              GO TO RSTT-999.
           IF STTR-TYPE NOT = 4
              GO TO RSTT-010.
           IF F-EXIT-CH = X"9B" OR = X"C7"
              MOVE "*" TO B-PULL (SUB-1).
           MOVE STTR-TRANSACTION-NUMBER TO B-STTRANS (SUB-1).
           MOVE STTR-STOCK-NUMBER       TO B-STOCKNUMBER (SUB-1)
                                       SPLIT-STOCK.
           MOVE STTR-COMPLETE     TO B-NEWLINE (SUB-1).
           IF STTR-COMPLETE = " "
               MOVE "N"           TO B-NEWLINE (SUB-1).
           MOVE STTR-INV-NO       TO B-INVOICED (SUB-1).
           IF SP-1STCHAR = "*"
               GO TO RSTT-020.
           MOVE STTR-DESC1        TO B-STOCKDESCRIPTION (SUB-1)
           MOVE STTR-DESC2        TO B-STOCKDESCRIPTION2 (SUB-1)
           MOVE STTR-SHIPQTY      TO B-SHIPQTY (SUB-1)
           MOVE STTR-ORDERQTY     TO B-ORDERQTY (SUB-1)
           MOVE STTR-SHIPPEDQTY   TO B-SHIPPEDQTY (SUB-1)
           MOVE STTR-PRICE        TO B-STOCKPRICE (SUB-1)
           MOVE STTR-COST-VALUE   TO B-STOCKCOST (SUB-1)
           MOVE STTR-ITEMDISC     TO B-DISCOUNTPERITEM (SUB-1)
           MOVE STTR-TAX          TO B-TAX (SUB-1)
           MOVE STTR-UNIT         TO B-UNIT (SUB-1)
           MOVE STTR-SALES-VALUE  TO B-NETT (SUB-1)
           COMPUTE WS-COSTTOTAL = WS-COSTTOTAL +
               (B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1))
           GO TO RSTT-050.
       RSTT-020.
           MOVE COM-ORDERQTY      TO C-ORDER (SUB-1)
           MOVE COM-SHIPQTY       TO C-SHIP (SUB-1)
           MOVE COM-DESC          TO C-DESC (SUB-1)
           MOVE COM-UNIT          TO C-UNIT (SUB-1)
           MOVE COM-PRICE         TO C-PRICE (SUB-1)
           MOVE COM-COST          TO C-COST (SUB-1)
           MOVE COM-DISC          TO C-DISC (SUB-1).
       RSTT-050.
           IF SP-1STCHAR NOT = "*" AND NOT = "/" AND NOT = "R"
              PERFORM READ-STOCK-TEMP.
           IF B-STTRANS (SUB-1) > WS-STTRANS-NO
              MOVE B-STTRANS (SUB-1) TO WS-STTRANS-NO.
           ADD 1 TO SUB-1
           MOVE SUB-1 TO SUB-25.
           IF SUB-1 > 200
              MOVE 200 TO SUB-1 SUB-25
              GO TO RSTT-999.
           GO TO RSTT-010.
       RSTT-999.
           EXIT.
      *
       WRITE-STOCK-TRANSACTIONS SECTION.
       WST-00000.
           IF WS-ADD-TO-OLD-ORDER = "Y"
               MOVE SUB-30 TO WS-STTRANS-NO.
           MOVE 1          TO SUB-1
           MOVE WS-INVOICE TO STTR-REFERENCE1
           MOVE 4          TO STTR-TYPE
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
               INVALID KEY NEXT SENTENCE.
           IF B-STOCKNUMBER (SUB-1) = " "
              GO TO WST-999.
       WST-000.
           IF B-STTRANS (SUB-1) = 0
              ADD 1 TO B-STTRANS (SUB-1).
           MOVE B-STTRANS (SUB-1) TO STTR-TRANSACTION-NUMBER.
           IF B-NEWLINE (SUB-1) = " "
              GO TO WST-006.
       WST-005.
           READ STOCK-TRANS-FILE WITH LOCK
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "ST-TRANS BUSY ON READ-FOR WRITE, 'ESC' TO SEE INFO."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE B-STOCKNUMBER (SUB-1) TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE B-STTRANS (SUB-1) TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO WST-005.
       WST-006.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
                                          STTR-STOCK-NUMBER.
            MOVE WS-ACCOUNT-NUMBER     TO STTR-ACCOUNT-NUMBER
            MOVE WS-INVOICEDATE        TO STTR-DATE
                                          STTR-AC-DATE
                                          STTR-ST-DATE.
            MOVE B-INVOICED (SUB-1)    TO STTR-INV-NO.

            IF B-NEWLINE (SUB-1) NOT = "Y" AND NOT = "L" AND NOT = "R"
               MOVE "N"                TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE
             ELSE
               MOVE B-NEWLINE (SUB-1)  TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE.
            IF B-REPAIR (SUB-1) = "R"
               MOVE B-REPAIR (SUB-1)  TO STTR-COMPLETE
                                         STTR-ST-COMPLETE
                                         STTR-AC-COMPLETE.

            IF SP-1STCHAR = "*"
                GO TO WST-015.
            IF B-REPAIR (SUB-1) = "R"
                GO TO WST-018.
       WST-010.
            IF B-NEWLINE (SUB-1) = " "
               MOVE B-ORDERQTY (SUB-1)       TO STTR-ORDERQTY.
            IF B-NEWLINE (SUB-1) NOT = "Y" AND NOT = "L"
               MOVE B-SHIPQTY (SUB-1)        TO STTR-SHIPQTY.
            MOVE B-SHIPPEDQTY (SUB-1)        TO STTR-SHIPPEDQTY
            MOVE B-NETT (SUB-1)              TO STTR-SALES-VALUE
            MOVE B-STOCKPRICE (SUB-1)        TO STTR-PRICE
            MOVE B-STOCKDESCRIPTION (SUB-1)  TO STTR-DESC1
            MOVE B-STOCKDESCRIPTION2 (SUB-1) TO STTR-DESC2
            MOVE B-TAX (SUB-1)               TO STTR-TAX
            MOVE B-UNIT (SUB-1)              TO STTR-UNIT
            MOVE B-DISCOUNTPERITEM (SUB-1)   TO STTR-ITEMDISC
            MOVE B-STOCKCOST (SUB-1)         TO STTR-COST-VALUE
            GO TO WST-018.
       WST-015.
            IF WS-ORDER-COMPLETE = "Y"
               MOVE "L"           TO STTR-COMPLETE
                                     STTR-ST-COMPLETE
                                     STTR-AC-COMPLETE
             ELSE
               MOVE "N"           TO STTR-COMPLETE
                                     STTR-ST-COMPLETE
                                     STTR-AC-COMPLETE.
            MOVE C-DESC (SUB-1)   TO COM-DESC
            MOVE C-UNIT (SUB-1)   TO COM-UNIT
            MOVE C-ORDER (SUB-1)  TO COM-ORDERQTY
            MOVE C-SHIP (SUB-1)   TO COM-SHIPQTY
            MOVE C-PRICE (SUB-1)  TO COM-PRICE
            MOVE C-COST (SUB-1)   TO COM-COST
            MOVE C-DISC (SUB-1)   TO COM-DISC
            MOVE " "              TO COM-FILLER.
       WST-018.
            IF B-NEWLINE (SUB-1) = " "
               WRITE STOCK-TRANS-REC
                  INVALID KEY NEXT SENTENCE
            ELSE
               REWRITE STOCK-TRANS-REC
                  INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS BUSY AT (WST-018) NOTIFY THE SUPERVISOR."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE B-STOCKNUMBER (SUB-1) TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE B-STTRANS (SUB-1) TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-STTRANS-ST1
              PERFORM ERROR1-020
              GO TO WST-018.
           IF STTR-TRANSACTION-NUMBER = 0
              MOVE
              "ST-TRANS-NO = 0 AT WST-020, 'ESC' TO ADD 1."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              ADD 1 TO STTR-TRANSACTION-NUMBER
              GO TO WST-018.
       WST-020.
            ADD 1 TO SUB-1.
            IF SUB-1 < 201
             IF B-STOCKNUMBER (SUB-1) = " "
                GO TO WST-999.
            IF SUB-1 < 201
                GO TO WST-000.
       WST-999.
            EXIT.
      *
       FIND-INFO SECTION.
       FIND-010.
            MOVE "INVOICENUM" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE 6            TO F-CBFIELDLENGTH
            MOVE WS-INVOICE   TO F-EDNAMEFIELDNUM
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "ACCOUNTNO"       TO F-FIELDNAME
            MOVE 9                 TO F-CBFIELDNAME
            MOVE WS-ACCOUNT-NUMBER TO F-NAMEFIELD
                                      DR-ACCOUNT-NUMBER
                                      WS-ACCOUNT-NUMBER
            MOVE 7                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM RD-000.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE
            "THE A/C No ON THIS P/SLIP DOES NOT EXIST, PLEASE NOTIFY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
               MOVE "YOUR SUPERVISOR IMMEDIATELY, 'ESC' TO EXIT."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO FIND-999.
            MOVE DR-DISCOUNT-CODE  TO WS-DISCOUNT-CODE.
            IF DR-ACCOUNT-NUMBER = 9999999 OR = 0300150
                              OR = 0300090 OR = 0300087
                              OR = 0300100 OR = 0300200
               GO TO FIND-020.
           IF DR-BALANCE > DR-CREDIT-LIMIT
               COMPUTE WS-WORK-FIELD = DR-BALANCE - DR-CREDIT-LIMIT
               MOVE WS-WORK-FIELD TO F-EDNAMEFIELDAMOUNT1
               MOVE "OVER THE CR.LIMIT:"  TO WS-DAILY-1ST
               MOVE F-EDNAMEFIELDAMOUNT1  TO WS-DAILY-2ND
               MOVE " "                   TO WS-DAILY-3RD
                                             WS-DAILY-4TH
               MOVE WS-DAILY-MESSAGE TO WS-MESSAGE
               PERFORM ERROR-000.
           IF DR-SUPPLY-Y-N = "N"
               MOVE "GOODS CANNOT BE SUPPLIED ON THIS ACCOUNT."
               TO WS-MESSAGE
               PERFORM ERROR1-000.
           IF DR-SUPPLY-Y-N = "S"
               MOVE
               "THIS ACCOUNT HAS BEEN SUSPENDED. CHECK WITH A/C'S DEPT."
               TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE.
       FIND-020.
            MOVE "COPYDESC"             TO F-FIELDNAME
            MOVE 8                      TO F-CBFIELDNAME
            MOVE "P/SLIP COPY NUMBER :" TO F-NAMEFIELD
            MOVE 20                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COPYNUMBER"     TO F-FIELDNAME
            MOVE 10               TO F-CBFIELDNAME
            MOVE INCR-COPY-NUMBER TO F-NAMEFIELD
            MOVE 2                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEBTORNAME" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-NAME      TO F-NAMEFIELD
            MOVE 40           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "POSTADD1" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE WS-ADD1    TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "POSTADD2" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE WS-ADD2    TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "POSTADD3" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE WS-ADD3    TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "POSTCODE"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-POSTCODE TO F-NAMEFIELD
            MOVE 4           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DELADD1"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD1 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DELADD2"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD2 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DELADD3"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD3 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "POORDERNO"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-POORDERNO TO F-NAMEFIELD
            MOVE 20           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE WSAN-CODE        TO SA-KEY
            PERFORM READ-SALES-ANALYSIS
            MOVE "SALESANALYSIS"  TO F-FIELDNAME
            MOVE 13               TO F-CBFIELDNAME
            MOVE WS-SALESANALYSIS TO F-NAMEFIELD
            MOVE 14               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DELIVERVIA"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-DELIVERVIA TO F-NAMEFIELD
            MOVE 20            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "TERMOFSALE"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-TERMOFSALE TO F-NAMEFIELD
            MOVE 11            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "GSTNO"       TO F-FIELDNAME
            MOVE 5             TO F-CBFIELDNAME
            MOVE WS-GSTNO      TO F-NAMEFIELD
            MOVE 13            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "BINNO"  TO F-FIELDNAME
            MOVE 5        TO F-CBFIELDNAME
            MOVE WS-BINNO TO F-NAMEFIELD
            MOVE 6        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PHONE"  TO F-FIELDNAME
            MOVE 5        TO F-CBFIELDNAME
            MOVE WS-PHONE TO F-NAMEFIELD
            MOVE 20       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "CONTACTNAME" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE WS-CONTACT    TO F-NAMEFIELD
            MOVE 20            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "SALESMAN"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE DR-SALESMAN  TO F-NAMEFIELD
            MOVE 1            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICEDATE"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE WS-INVOICEDATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE   TO F-NAMEFIELD
            MOVE 10             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            PERFORM READ-SBREP
            MOVE "SOLDBY"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE WS-SOLD-BY TO F-NAMEFIELD
            MOVE 2         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DEL-AREA" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE INCR-AREA  TO F-NAMEFIELD.
            MOVE 1          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE INCR-PART-ORDERS TO WS-PART-ORDERS.
            MOVE "PART-ORDERS"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE WS-PART-ORDERS   TO F-NAMEFIELD.
            MOVE 1                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PULL-DESC"           TO F-FIELDNAME
            MOVE 9                     TO F-CBFIELDNAME.
            IF INCR-PULL-DATE > 0
                MOVE "GOODS PULLED ON" TO F-NAMEFIELD
                MOVE 15                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                
                MOVE "PULL-DATE"       TO F-FIELDNAME
                MOVE 9                 TO F-CBFIELDNAME
                MOVE INCR-PULL-DATE    TO SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE      TO F-NAMEFIELD
                MOVE 10                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                
                MOVE "PULL-TIME"    TO F-FIELDNAME
                MOVE 9              TO F-CBFIELDNAME
                MOVE INCR-PULL-TIME TO ALPHA-RATE
                PERFORM TIME-CHECKING
                MOVE WS-DATE-CHECK  TO F-NAMEFIELD
                MOVE 8              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
             ELSE
                MOVE "GOODS NOT PULLD" TO F-NAMEFIELD
                MOVE 15                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.

            IF SUB-1 NOT > 0
                MOVE 1 TO SUB-1.
            PERFORM SCROLL-NEXT
            PERFORM SCROLL-PREVIOUS
            PERFORM CHECK-DISCOUNT.

            MOVE "COMMENTLINE"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE WS-COMMENTLINE TO F-NAMEFIELD
            MOVE 30             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ADDONFREIGHT"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE WS-ADDONFREIGHT TO F-EDNAMEFIELDAMOUNT
            MOVE 9               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT

            MOVE "POSTADDON"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-POSTADDON TO F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT

            MOVE "HANDADDON"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-HANDADDON TO F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT

            MOVE "MISC.ADDON" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-MISCADDON TO F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT

            MOVE "SUBTOTAL"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-SUBTOTAL TO F-EDNAMEFIELD9MIL
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL

            MOVE "ADDONAMT"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-ADDONAMT TO F-EDNAMEFIELD9MIL
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL

            MOVE "TAXAMT"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE WS-TAXAMT TO F-EDNAMEFIELD9MIL
            MOVE 10        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL

            MOVE "INVOICETOTAL"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE WS-INVOICETOTAL TO F-EDNAMEFIELD9MIL
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.
       FIND-999.  
            EXIT.
      *
       CHECK-DISCOUNT SECTION.
       C-DIS-005.
            MOVE 1 TO SUB-1.
            MOVE B-DISCOUNTPERITEM (SUB-1) TO WS-INVOICEDISCOUNT.
       C-DIS-020.
            ADD 1 TO SUB-1.
            IF SUB-1 > 200
               MOVE 200 TO SUB-1
               GO TO C-DIS-999.
            IF B-STOCKNUMBER (SUB-1) = " "
               GO TO C-DIS-999.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
               GO TO C-DIS-020.
            IF B-DISCOUNTPERITEM (SUB-1) = WS-INVOICEDISCOUNT
               GO TO C-DIS-020
            ELSE
               MOVE 0 TO WS-INVOICEDISCOUNT.
       C-DIS-999.
            EXIT.
      *
       READ-DEBTORS SECTION.
       RD-000.
           MOVE WS-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
           READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE SPACES TO DR-NAME DR-ADDRESS1 DR-ADDRESS2
                              DR-ADDRESS3 DR-DEL-ADDRESS1
                              DR-DEL-ADDRESS2 DR-DEL-ADDRESS3
               MOVE "UNKNOWN" TO DR-NAME
               MOVE 0         TO DR-POST-CODE.
           IF WS-DEBTOR-ST1 NOT = 0 AND NOT = 23 AND NOT = 35
                        AND NOT = 49
               MOVE "DEBTOR RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-000.
       RD-005.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               GO TO RD-999.
       RD-010.
           MOVE DR-NAME         TO WS-NAME
           MOVE DR-ADDRESS1     TO WS-ADD1
           MOVE DR-ADDRESS2     TO WS-ADD2
           MOVE DR-ADDRESS3     TO WS-ADD3
           MOVE DR-DEL-ADDRESS1 TO WS-DELADD1
           MOVE DR-DEL-ADDRESS2 TO WS-DELADD2
           MOVE DR-DEL-ADDRESS3 TO WS-DELADD3
           MOVE DR-POST-CODE    TO WS-POSTCODE.
       RD-999.
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
               MOVE "PARAMETER TERMS BUSY ON READ,  'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
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
             CLOSE PARAMETER-FILE.
       RTERM-9999.
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
            IF SUB-1 = 10
               PERFORM ERROR-020
               GO TO RDELIV-999.
            GO TO RDELIV-010.
       RDELIV-999.
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
               MOVE 1 TO WS-NORM-PRINTER
                         WS-REPR-PRINTER
                         WS-RUSH-PRINTER
               GO TO RINVQUES-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY RINVQUES, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RINVQUES-010.
       RINVQUES-900.
            MOVE INVQUES-PS-NORM-PRINTER TO WS-NORM-PRINTER
            MOVE INVQUES-PS-REPR-PRINTER TO WS-REPR-PRINTER
            MOVE INVQUES-PS-RUSH-PRINTER TO WS-RUSH-PRINTER
            MOVE INVQUES-MU-GP-PERC      TO WS-QUES-MU-GP-PERC
            MOVE INVQUES-CHECK-QUOTES    TO WS-QUES-CHECK-QUOTES
            MOVE INVQUES-PAUSE-ON-PSLIP  TO WS-QUES-PAUSE-ON-PSLIP
            MOVE INVQUES-ACC-OVER-LIMIT  TO WS-QUES-ACC-OVER-LIMIT.
       RINVQUES-999.
            EXIT.
      *
       READ-SALES-ANALYSIS SECTION.
       RSALES-000.
            OPEN I-O SALES-ANALYSIS.
            IF WS-SALES-ST1 NOT = 0
               MOVE "SALES ANALYSIS BUSY ON OPEN, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO RSALES-000.
       RSALES-500.
            MOVE WSAN-CODE TO SA-KEY.
            READ SALES-ANALYSIS
                INVALID KEY NEXT SENTENCE.
            IF WS-SALES-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN" TO SA-NAME
                GO TO RSALES-900.
            IF WS-SALES-ST1 NOT = 0
               MOVE "SALES ANALYSIS BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO RSALES-500.
       RSALES-900.
            PERFORM ERROR-020.
            MOVE 0 TO WS-SALES-ST1.
            MOVE SA-NAME TO WS-SALESANALYSIS.
       RSALES-950.
            CLOSE SALES-ANALYSIS.
            IF WS-SALES-ST1 NOT = 0
               MOVE "SALES-ANALYSIS BUSY ON CLOSE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO RSALES-950.
       RSALES-999.
            EXIT.
      *
       TAX-ONLY SECTION.
       TO-000.
            MOVE "TAXAMT"     TO F-FIELDNAME.
            MOVE 6            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-TAXAMT
                                 F-EDNAMEFIELDAMOUNT.
            PERFORM WRITE-FIELD-AMOUNT.
      *      IF WS-GSTNO = "EXPORT"
      *       IF WS-TAXAMT > 0
      *         MOVE
      *       "AN EXPORT SALE CANNOT HAVE VAT ALLOCATED TO IT, RE-ENTER."
      *           TO WS-MESSAGE
      *           PERFORM ERROR-MESSAGE
      *           GO TO TO-000.
            MOVE X"1D" TO F-EXIT-CH.
       TO-999.
            EXIT.
      *
       READ-SPECIAL-PRICES SECTION.
       SPR-000.
           MOVE ST-STOCKNUMBER TO STPR-STOCKNUMBER.
           START STPR-MASTER KEY NOT < STPR-KEY
               INVALID KEY NEXT SENTENCE.
       SPR-005.
           READ STPR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
              MOVE 0 TO STPR-PRICE WS-PRICESAVE
              GO TO SPR-999.
           IF WS-STPR-ST1 NOT = 0
              MOVE "SPECIAL PRICES BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               GO TO SPR-005.
           
           PERFORM ERROR1-020
           MOVE
            "PRESS <F1> TO RE-INSTATE THE ORIGINAL PRICE & DISCOUNT."
              TO WS-MESSAGE
              PERFORM ERROR1-000.

           MOVE "THIS IS A SPECIAL PRICE, NORMAL PRICE IS R"
              TO WS-MESSAGE.
           PERFORM ERROR-000.
           MOVE 3058       TO POS.
           MOVE ST-PRICE   TO F-EDNAMEFIELDAMOUNT WS-PRICESAVE.
           DISPLAY F-EDNAMEFIELDAMOUNT AT POS.
           MOVE STPR-PRICE TO ST-PRICE.
           MOVE "Y"        TO B-SPECIAL (SUB-1).
       SPR-999.
           EXIT.
      *
       READ-STOCK-TEMP SECTION.
       RST-TEM000.
           MOVE B-STOCKNUMBER (SUB-1) TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       RST-TEM005.
           READ STOCK-MASTER
               INVALID KEY
               MOVE "ERR" TO WS-ERR
               NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "STOCK TEMP INVALID,'ESC' TO IGNORE."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE ST-STOCKNUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RST-TEM99.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK TEMP IN USE ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RST-TEM005.
           MOVE ST-MIN-PERC TO B-MIN-PERC (SUB-1).
       RST-TEM99.
           EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-005.
           READ STOCK-MASTER
               INVALID KEY
               MOVE "ERR" TO WS-ERR
               NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               PERFORM START-FOR-READ-NEXT
               MOVE " " TO ST-DESCRIPTION1
               GO TO R-ST-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD IN USE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-005.
           PERFORM ERROR-020.
           IF ST-ANALYSIS = "D"
               MOVE
            "DON'T ENTER ORDER, ITEM TO BE DELETED WHEN ON HAND = ZERO."
                   TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           MOVE ST-DISCOUNT9 TO B-MAX-DISC (SUB-1).
           MOVE ST-MIN-PERC  TO B-MIN-PERC (SUB-1).
       R-ST-010.
      ***************
      *BRANCH OFFICE*
      ***************
           IF WSAN-CODE = "53"
              PERFORM COMPUTE-SPECIAL-PRICES.
      ******************
      *ASSOCIATE OFFICE*
      ******************
           IF WSAN-CODE = "57"
              PERFORM COMPUTE-ASSOCIATE-PRICES.
       R-ST-999.
           EXIT.
      *
       START-FOR-READ-NEXT SECTION.
       SFRN-001.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON START-10, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               MOVE " " TO ST-STOCKNUMBER
               GO TO SFRN-001.
       SFRN-999.
             EXIT.
      *
       READ-NEXT-STOCK-ITEM SECTION.
       RNSI-005.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               GO TO RNSI-999.
           IF WS-STOCK-ST1 = 91
               MOVE "STOCK RECORD IN USE READ-NEXT-91, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO ST-STOCKNUMBER
               PERFORM START-FOR-READ-NEXT
               GO TO RNSI-005.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD IN USE READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RNSI-005.
           IF ST-ANALYSIS = "D"
               MOVE
            "DON'T ENTER ORDER, ITEM TO BE DELETED WHEN ON HAND = ZERO."
                   TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
       RNSI-999.
           EXIT.
      *
       READ-PREV-STOCK-ITEM SECTION.
       RPREV-005.
           READ STOCK-MASTER PREVIOUS
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               GO TO RPREV-999.
           IF WS-STOCK-ST1 = 91
               MOVE "STOCK RECORD IN USE READ-NEXT-91, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO ST-STOCKNUMBER
               PERFORM START-FOR-READ-NEXT
               GO TO RPREV-005.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD IN USE READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RPREV-005.
           IF ST-ANALYSIS = "D"
               MOVE
            "DON'T ENTER ORDER, ITEM TO BE DELETED WHEN ON HAND = ZERO."
                   TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
       RPREV-999.
           EXIT.
      *
       READ-STOCK-LOCK SECTION.
       R-STL-000.
             MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
             START STOCK-MASTER KEY NOT < ST-KEY.
       R-STL-005.
             READ STOCK-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                 MOVE " "   TO ST-STOCKNUMBER
                               ST-DESCRIPTION1
                               ST-DESCRIPTION2
                 MOVE 0     TO ST-PRICE
                               ST-AVERAGECOST
                               ST-DISCOUNT1
                 MOVE "ERR" TO WS-ERR
                 GO TO R-STL-999.
             IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-STL-005.
           MOVE ST-DISCOUNT9 TO B-MAX-DISC (SUB-1).
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
               MOVE "NO PARAMETER RECORD ON FILE, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               STOP RUN.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY."
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
              MOVE "PARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY."
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
              MOVE "PARAMETER BUSY ON REWRITE, 'ESC' TO RETRY."
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
       CHECK-SUB1-TOTAL SECTION.
       CHK-000.
            MOVE 1 TO SUB-1.
       CHK-010.
           IF SUB-1 < 200
             IF B-STOCKNUMBER (SUB-1) NOT = " "
                ADD 1 TO SUB-1
                GO TO CHK-010.
           MOVE SUB-1 TO SUB-20.
      *     SUBTRACT 1 FROM SUB-1.
      *     IF B-STTRANS (SUB-1) > SUB-20
      *        MOVE B-STTRANS (SUB-1) TO SUB-20
      *        ADD 1 TO SUB-20.
       CHK-EXIT.
           EXIT.
      *
       FILL-COMMENT SECTION.
       COMM-A.
            MOVE " " TO C-ORDER (SUB-1)
                        C-SHIP (SUB-1)
                        C-DESC (SUB-1)
                        C-UNIT (SUB-1)
                        C-PRICE (SUB-1)
                        C-COST (SUB-1)
                        C-DISC (SUB-1).
       COMM-000.
            MOVE "ORDERQTY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-999.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO C-ORDER (SUB-1).
       COMM-010.
            MOVE "SHIPQTY" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-000.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO C-SHIP (SUB-1).
       COMM-020.
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-010.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO C-DESC (SUB-1).
       COMM-025.
            MOVE "PERUNIT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-020.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO C-UNIT (SUB-1).
       COMM-030.
            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-020.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO C-PRICE (SUB-1).
       COMM-040.
            MOVE "STOCKCOST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-030.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO C-COST (SUB-1).
       COMM-050.
            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME.
            MOVE 15 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-040.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO C-DISC (SUB-1).
       COMM-999.
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
            PERFORM CLEAR-BODY.
            MOVE 1 TO F-INDEX. 
            IF SUB-1 > 194
               MOVE 194 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 200  
                GO TO NEXT-030.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO B-TAX (SUB-1)
                MOVE 0 TO B-DISCOUNTPERITEM (SUB-1).
            IF F-INDEX < 8
                GO TO NEXT-010.
       NEXT-030.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 > 193
             IF SUB-25 > 193
               COMPUTE F-INDEX = 7 - (200 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 7
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.
            
            PERFORM FILL-005.
       NEXT-999.
             EXIT.
      *
       SCROLL-NEXT-PAGE SECTION.
       NEXT-PAGE-000.
            ADD 7 TO SUB-1.
            IF SUB-1 > SUB-25
               MOVE SUB-25 TO SUB-1.
            MOVE 1 TO F-INDEX. 
            PERFORM CLEAR-BODY.
            MOVE 1 TO F-INDEX. 
            IF SUB-1 > 194
               MOVE 194 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 200
                GO TO NEXT-PAGE-030.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO B-TAX (SUB-1)
                MOVE 0 TO B-DISCOUNTPERITEM (SUB-1).
            IF F-INDEX < 8
                GO TO NEXT-PAGE-010.
       NEXT-PAGE-030.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 > 193
             IF SUB-25 > 193
               COMPUTE F-INDEX = 7 - (200 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX.
            IF SUB-1 > 200
               MOVE 194 TO SUB-1.
            IF F-INDEX > 7
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            PERFORM FILL-005.
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
            IF SUB-1 > 200   
                GO TO PREV-030.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO B-TAX (SUB-1)
                MOVE 0   TO B-DISCOUNTPERITEM (SUB-1).
            IF F-INDEX < 8
                GO TO PREV-010.
       PREV-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            PERFORM FILL-005.
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
            IF SUB-1 > 200   
                GO TO SCROLL-DOWN-030.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO B-TAX (SUB-1)
                MOVE 0 TO B-DISCOUNTPERITEM (SUB-1).
            IF F-INDEX < 8
                GO TO SCROLL-DOWN-010.
       SCROLL-DOWN-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            PERFORM FILL-005.
       SCROLL-DOWN-999.
             EXIT.
      *
       SCROLLING SECTION.
       SCROLL-010.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.
                
            MOVE "STOCKNUMBER"         TO F-FIELDNAME
            MOVE 11                    TO F-CBFIELDNAME
            MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
            MOVE 15                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
                PERFORM SCROLL-COMMENT
                GO TO SCROLL-999.

            MOVE "ORDERQTY" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE 5          TO F-CBFIELDLENGTH
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-020.
            IF B-NEWLINE (SUB-1) = "L" OR = "Y"
               MOVE B-ORDERQTY (SUB-1) TO F-EDNAMEFIELDQTY
               PERFORM WRITE-FIELD-QTY
            ELSE
               COMPUTE WS-BO-QTY = B-ORDERQTY (SUB-1) -
                          B-SHIPPEDQTY (SUB-1)
               MOVE WS-BO-QTY TO F-EDNAMEFIELDQTY
               PERFORM WRITE-FIELD-QTY.
       SCROLL-020.
            MOVE "SHIPQTY" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE 5         TO F-CBFIELDLENGTH.
            IF B-NEWLINE (SUB-1) = "L" OR = "Y"
                MOVE "SHIPD" TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-030.
            IF B-NEWLINE (SUB-1) = "R"
                MOVE "     " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-030.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE B-SHIPQTY (SUB-1) TO F-EDNAMEFIELDQTY
                PERFORM WRITE-FIELD-QTY
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.
       SCROLL-030.
            MOVE "STOCKDESCRIPTION"         TO F-FIELDNAME
            MOVE 16                         TO F-CBFIELDNAME
            MOVE B-STOCKDESCRIPTION (SUB-1) TO F-NAMEFIELD
            MOVE 20                         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PERUNIT"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME
            MOVE B-UNIT (SUB-1) TO F-NAMEFIELD
            MOVE 4              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            IF B-NEWLINE (SUB-1) = "R"
               GO TO SCROLL-050.
            MOVE "STOCKPRICE" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE 9            TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE B-STOCKPRICE (SUB-1) TO F-EDNAMEFIELDAMOUNT
                PERFORM WRITE-FIELD-AMOUNT
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKCOST" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE 9           TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-040.
            IF WS-COST-DISPLAY = "N"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
            ELSE
                MOVE B-STOCKCOST (SUB-1) TO F-EDNAMEFIELDAMOUNT
                PERFORM WRITE-FIELD-AMOUNT.
       SCROLL-040.
            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME
            MOVE 15                TO F-CBFIELDNAME
            MOVE 5                 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE B-DISCOUNTPERITEM (SUB-1) TO
                           F-EDNAMEFIELDAMOUNTDIS
                PERFORM WRITE-FIELD-AMOUNTDIS
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.

      *      MOVE "TAX"         TO F-FIELDNAME
      *      MOVE 3             TO F-CBFIELDNAME
      *      MOVE B-TAX (SUB-1) TO F-NAMEFIELD
      *      MOVE 1             TO F-CBFIELDLENGTH
      *      PERFORM WRITE-FIELD-ALPHA.
       SCROLL-050.
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            IF F-EXIT-CH = X"8C" OR = X"85"
               MOVE ST-DESCRIPTION1             TO WS-DESC1
               MOVE ST-DESCRIPTION2             TO WS-DESC2
            ELSE
               MOVE B-STOCKDESCRIPTION (SUB-1)  TO WS-DESC1
               MOVE B-STOCKDESCRIPTION2 (SUB-1) TO WS-DESC2.
            
            MOVE "ST-DESC"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-000.
            MOVE 0 TO F-INDEX.
       CLEAR-002.
            ADD 1 TO F-INDEX.
            IF F-INDEX > 7
               GO TO CLEAR-BODY-999.

            MOVE "STOCKNUMBER" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 15            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ORDERQTY" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE 0          TO F-EDNAMEFIELDNUM
            MOVE 5          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC

            MOVE "SHIPQTY" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE 0         TO F-EDNAMEFIELDNUM
            MOVE 5         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC

            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME
            MOVE 16                 TO F-CBFIELDNAME
            MOVE " "                TO F-NAMEFIELD
            MOVE 20                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PERUNIT" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 4         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKPRICE" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE " "          TO F-NAMEFIELD
            MOVE 8            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKCOST" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 8           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME
            MOVE 15                TO F-CBFIELDNAME
            MOVE " "               TO F-NAMEFIELD
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

      *      MOVE "TAX" TO F-FIELDNAME
      *      MOVE 3     TO F-CBFIELDNAME
      *      MOVE " "   TO F-NAMEFIELD
      *      MOVE 1     TO F-CBFIELDLENGTH
      *      PERFORM WRITE-FIELD-ALPHA

            GO TO CLEAR-002.
       CLEAR-BODY-999.
             EXIT.
      *
       SCROLL-COMMENT SECTION.
       SCCO-000.
            MOVE "ORDERQTY"      TO F-FIELDNAME
            MOVE 8               TO F-CBFIELDNAME
            MOVE C-ORDER (SUB-1) TO F-NAMEFIELD
            MOVE 5               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "SHIPQTY"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME
            MOVE C-SHIP (SUB-1) TO F-NAMEFIELD
            MOVE 5              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME
            MOVE 16                 TO F-CBFIELDNAME
            MOVE C-DESC (SUB-1)     TO F-NAMEFIELD
            MOVE 20                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PERUNIT"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME
            MOVE C-UNIT (SUB-1) TO F-NAMEFIELD
            MOVE 4              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKPRICE"    TO F-FIELDNAME
            MOVE 10              TO F-CBFIELDNAME
            MOVE C-PRICE (SUB-1) TO F-NAMEFIELD
            MOVE 9               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKCOST"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE C-COST (SUB-1) TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME
            MOVE 15                TO F-CBFIELDNAME
            MOVE 5                 TO F-CBFIELDLENGTH
            MOVE C-DISC (SUB-1)    TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.
       SCCO-010.
            IF B-NEWLINE (SUB-1) = "L"
               MOVE
           "COMMENT AT POSITION    WILL NOT BE PRINTED ON THE INVOICE."
                TO WS-MESSAGE
           ELSE
               MOVE
           "COMMENT AT POSITION    WILL BE PRINTED ON THE NEXT INVOICE."
                TO WS-MESSAGE.
          PERFORM ERROR-000
          MOVE 3034 TO POS
          MOVE F-INDEX TO WS-BODY-LINE
          DISPLAY WS-BODY-LINE AT POS
          PERFORM ERROR-010.
       SCCO-999.
            EXIT.
      *
       READ-STDISCOUNT SECTION.
       R-STDISC-000.
             MOVE ST-STOCKNUMBER    TO STDISC-STOCKNUMBER.
             MOVE WS-ACCOUNT-NUMBER TO STDISC-ACCOUNT.
             START STDISC-MASTER KEY NOT < STDISC-KEY
                  INVALID KEY NEXT SENTENCE.
       R-STDISC-010.
             READ STDISC-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STDISC-ST1 = 23 OR 35 OR 49
                MOVE 0 TO STDISC-PERCENT
                GO TO R-STDISC-999.
             IF WS-STDISC-ST1 NOT = 0
                MOVE "STDISCOUNT BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STDISC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STDISC-ST1
                GO TO R-STDISC-010.
           PERFORM CHECK-NORMAL-DISCOUNT.
           MOVE "THIS IS A SPECIAL DISCOUNT, NORMAL DISC. IS"
              TO WS-MESSAGE.
           PERFORM ERROR-000.
           MOVE 3059 TO POS.
           MOVE WS-DR-DISC TO F-EDNAMEFIELDAMOUNTDIS.
           DISPLAY F-EDNAMEFIELDAMOUNTDIS AT POS.
       R-STDISC-999.
             EXIT.
      *
       READ-SPECIAL-DISC-ACC SECTION.
       RST-DISC-ACC-000.
             MOVE ST-STOCKNUMBER    TO STDISC-STOCKNUMBER.
             MOVE WS-ACCOUNT-NUMBER TO STDISC-ACCOUNT.
             START STDISC-MASTER KEY NOT < STDISC-KEY
                  INVALID KEY NEXT SENTENCE.
       RST-DISC-ACC-010.
             READ STDISC-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STDISC-ST1 = 23 OR 35 OR 49
                MOVE 0 TO STDISC-PERCENT
                GO TO RST-DISC-ACC-999.
             IF WS-STDISC-ST1 NOT = 0
                MOVE "STDISCOUNT-ACC BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                MOVE WS-STDISC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STDISC-ST1
                GO TO RST-DISC-ACC-010.
       RST-DISC-ACC-999.
             EXIT.
      *
       CHECK-NORMAL-DISCOUNT SECTION.
       CDS-005.
           IF DR-DISCOUNT-CODE = " " OR = "0"
              MOVE 0 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "1"
              MOVE ST-DISCOUNT1 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "2"
              MOVE ST-DISCOUNT2 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "3"
              MOVE ST-DISCOUNT3 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "4"
              MOVE ST-DISCOUNT4 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "5"
              MOVE ST-DISCOUNT5 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "6"
              MOVE ST-DISCOUNT6 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "7"
              MOVE ST-DISCOUNT7 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "8"
              MOVE ST-DISCOUNT8 TO WS-DR-DISC
              GO TO CDS-999.
           IF DR-DISCOUNT-CODE = "9"
              MOVE ST-DISCOUNT9 TO WS-DR-DISC
              GO TO CDS-999.
       CDS-999.
           EXIT.
      *
       CLEAR-BOTTOM-FIELDS SECTION.
       CBF-000.
            MOVE "ADDONFREIGHT" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "POSTADDON"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "HANDADDON"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "MISC.ADDON"   TO F-FIELDNAME
            MOVE 10             TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "SUBTOTAL"     TO F-FIELDNAME
            MOVE 8              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 10             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ADDONAMT"     TO F-FIELDNAME
            MOVE 8              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 10             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "TAXAMT"       TO F-FIELDNAME
            MOVE 6              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 10             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "INVOICETOTAL" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 10             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       CBF-999.
            EXIT.
      *
       CANCEL-INVOICE SECTION.
       CI-000.
             MOVE 2910 TO POS
             DISPLAY "CANCELLING ORDER-TRANSACTIONS ........" AT POS
             MOVE 1 TO SUB-1.
             IF WS-NEWORDER NOT = "Y"
              IF F-NAMEFIELDRED1 NOT = "Q" AND NOT = "R"
                   GO TO CI-900.
       CI-010.
             IF B-STOCKNUMBER (SUB-1) = " "
                IF B-ORDERQTY (SUB-1) = 0
                 GO TO CI-900.
             PERFORM CANCEL-TRANSACTION
             MOVE 1 TO SUB-1
             GO TO CI-010.
       CI-900.
             UNLOCK INCR-REGISTER
             UNLOCK STOCK-TRANS-FILE.
       CI-950.
             PERFORM CLEAR-FIELDS
             PERFORM DISPLAY-FORM
             PERFORM ERROR-020.
       CI-999.
             EXIT.
      *
       CANCEL-TRANSACTION SECTION.
       CAN-005.
           MOVE "   " TO WS-ERR.
           COMPUTE SUB-2 = SUB-1 + 1.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*" OR = "/"
                GO TO CAN-010.
           MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           IF WS-ERR = "ERR"
              MOVE
         "THERE WAS AN ERROR IN READING THAT STOCK ITEM, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STOCK-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STOCK-ST1
              GO TO CAN-999.
      *********************************************************
      *NEXT 7 LINES NEW FOR RE-AVERAGING THE COST IF ITEM     *
      *DELETED OFF THE P/SLIP JUST IN CASE ST-AVERAGECOST IS  *
      *DIFFERENT TO THE COST ON THE P/SLIP                    *
      *********************************************************
           COMPUTE WS-AVE-COST-OF-ALLOC-STOCK =
              B-SHIPQTY (SUB-1) * B-STOCKCOST (SUB-1).
           COMPUTE WS-AVE-COST-OF-STOCK = 
              ST-QTYONHAND * ST-AVERAGECOST.

           ADD B-SHIPQTY (SUB-1)            TO ST-QTYONHAND.

           COMPUTE WS-AVERAGECOST = 
              (WS-AVE-COST-OF-STOCK + WS-AVE-COST-OF-ALLOC-STOCK)
                  / ST-QTYONHAND.
           IF WS-AVERAGECOST > 0 
              MOVE WS-AVERAGECOST TO ST-AVERAGECOST.
           
           IF ST-QTYONRESERVE > B-SHIPQTY (SUB-1)
               SUBTRACT B-SHIPQTY (SUB-1) FROM ST-QTYONRESERVE
           ELSE
               MOVE 0                       TO ST-QTYONRESERVE.
                
           IF ST-QTYONBORDER > B-ORDERQTY (SUB-1)
               SUBTRACT B-ORDERQTY (SUB-1) FROM ST-QTYONBORDER
           ELSE
               MOVE 0                        TO ST-QTYONBORDER.
           IF ST-QTYONRESERVE = ST-QTYONBORDER
                GO TO CAN-008.
           IF ST-QTYONRESERVE < ST-QTYONBORDER
                GO TO CAN-008.
           COMPUTE WS-BO-QTY = ST-QTYONRESERVE - ST-QTYONBORDER.
           SUBTRACT WS-BO-QTY FROM ST-QTYONRESERVE.
           ADD WS-BO-QTY        TO ST-QTYONHAND.
       CAN-008.
           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "STOCK RECORD:"       TO WS-DAILY-1ST
               MOVE B-STOCKNUMBER (SUB-1) TO WS-DAILY-2ND
               MOVE "NOT UPDATED "        TO WS-DAILY-3RD
               MOVE "ON CANCEL OF TRANS." TO WS-DAILY-4TH
               PERFORM WRITE-DAILY
               GO TO CAN-010.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO CAN-008.
       CAN-010.
            IF SUB-2 > 200 
               MOVE 200 TO SUB-1 SUB-2
               GO TO CAN-090.
             IF B-STOCKNUMBER (SUB-2) = " "
                 MOVE " " TO C-LINE (SUB-1)
                 MOVE " " TO B-STOCKNUMBER (SUB-1)
                             B-STOCKDESCRIPTION (SUB-1)
                             B-STOCKDESCRIPTION2 (SUB-1)
                             B-TAX (SUB-1)
                             B-UNIT (SUB-1)
                             B-STORE (SUB-1)
                             B-NEWLINE (SUB-1)
                 MOVE 0   TO B-ORDERQTY (SUB-1)
                             B-SHIPQTY (SUB-1)
                             B-SHIPPEDQTY (SUB-1)
                             B-STOCKPRICE (SUB-1)
                             B-STOCKCOST (SUB-1)
                             B-DISCOUNTPERITEM (SUB-1)
                             B-NETT (SUB-1)
                 GO TO CAN-090.
             MOVE C-LINE (SUB-2)    TO C-LINE (SUB-1).
             MOVE BODY-LINE (SUB-2) TO BODY-LINE (SUB-1).
             ADD 1 TO SUB-1 SUB-2.
             GO TO CAN-010.
       CAN-090.
             MOVE " " TO C-LINE (SUB-1).
             MOVE " " TO B-STOCKNUMBER (SUB-1)
                         B-STOCKDESCRIPTION (SUB-1)
                         B-STOCKDESCRIPTION2 (SUB-1)
                         B-TAX (SUB-1)
                         B-UNIT (SUB-1)
                         B-STORE (SUB-1)
                         B-NEWLINE (SUB-1).
             MOVE 0   TO B-ORDERQTY (SUB-1)
                         B-SHIPQTY (SUB-1)
                         B-SHIPPEDQTY (SUB-1)
                         B-STOCKPRICE (SUB-1)
                         B-STOCKCOST (SUB-1)
                         B-DISCOUNTPERITEM (SUB-1)
                         B-NETT (SUB-1).
       CAN-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 1 TO SUB-1.
       CF-010.
             MOVE " " TO C-LINE (SUB-1).
             MOVE " " TO B-STOCKNUMBER (SUB-1)
                         B-STOCKDESCRIPTION (SUB-1)
                         B-STOCKDESCRIPTION2 (SUB-1)
                         B-TAX (SUB-1)
                         B-UNIT (SUB-1)
                         B-STORE (SUB-1)
                         B-PULL (SUB-1)
                         B-SPECIAL (SUB-1)
                         B-REPAIR (SUB-1)
                         B-NEWLINE (SUB-1).
             MOVE 0   TO B-STTRANS (SUB-1)
                         B-INVOICED (SUB-1)
                         B-ORDERQTY (SUB-1)
                         B-SHIPQTY (SUB-1)
                         B-SHIPPEDQTY (SUB-1)
                         B-STOCKPRICE (SUB-1)
                         B-STOCKCOST (SUB-1)
                         B-DISCOUNTPERITEM (SUB-1)
                         B-NETT (SUB-1)
                         B-MAX-DISC (SUB-1)
                         B-MIN-PERC (SUB-1)
             ADD 1 TO SUB-1.
             IF SUB-1 < 201
                 GO TO CF-010.
       CF-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O PARAMETER-FILE.
           PERFORM READ-PARAMETER.
           MOVE PA-NAME TO CO-NAME.
           MOVE PA-GST-PERCENT TO WS-GST-PERCENT.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           IF WS-MM = PA-CURRENT-PER-MM
             IF WS-YY = PA-CURRENT-PER-YY
               GO TO OPEN-010.
       OPEN-005.
           MOVE 0410 TO POS
           DISPLAY "        *** ORDERS / P-SLIP PROGRAM ***" AT POS
           MOVE 1010 TO POS.
           DISPLAY "THE CURRENT MONTH OR YEAR ON THE PARAMETER FILE"
               AT POS.
           MOVE 1110 TO POS.
           DISPLAY "    DOES NOT CORRESPOND WITH TODAYS DATE!!!!" AT POS
           MOVE 1210 TO POS.
           DISPLAY "         GO AND CHECK THE SYSTEM DATE, " AT POS
           MOVE 1310 TO POS.
           DISPLAY "   AS IT APPEARS YOU'RE IN THE WRONG MONTH." AT POS
           MOVE 1610 TO POS.
           DISPLAY "   PRESS 'GO' OR 'NEXT' TO END THE PROGRAM." AT POS
           MOVE 3010 TO POS.
           
           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 60        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-IMM-PR.

      *     ACCEPT WS-IMM-PR AT POS.
           IF W-ESCAPE-KEY = 1 OR = 2
               CLOSE PARAMETER-FILE
               EXIT PROGRAM
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO OPEN-005.
       OPEN-010.
           MOVE ALL "X" TO STORE-TERM
                           STORE-DEL.
           PERFORM READ-TERMS-FILE.
           PERFORM READ-DELIVERY-FILE.
           PERFORM READ-INVQUES-FILE.
           CLOSE PARAMETER-FILE.
       OPEN-011.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0 
              MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DEBTOR-ST1
              GO TO OPEN-011.
       OPEN-012.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0 
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STOCK-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STOCK-ST1
              GO TO OPEN-012.
       OPEN-013.
           OPEN I-O STPR-MASTER.
           IF WS-STPR-ST1 NOT = 0 
              MOVE "SPECIAL PRICE FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STPR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STPR-ST1
              GO TO OPEN-013.
       OPEN-014.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE "SLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SLPARAMETER-ST1
              GO TO OPEN-014.
       OPEN-015.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0 
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STTRANS-ST1
              GO TO OPEN-015.
       OPEN-016.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0 
              MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-INCR-ST1
              GO TO OPEN-016.
       OPEN-019.
          OPEN I-O STDISC-MASTER.
          IF WS-STDISC-ST1 NOT = 0
              MOVE "STOCK SPEC-DISC BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STDISC-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STDISC-ST1
              GO TO OPEN-019.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlOrders"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE DEBTOR-MASTER
                 STOCK-MASTER
                 STPR-MASTER
                 PARAMETER-FILE
                 INCR-REGISTER
                 STOCK-TRANS-FILE
                 STDISC-MASTER.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ComputeSpecialPrices".
       Copy "ComputeAssociatePrices".
       Copy "OrderPassword".
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAmount".
       Copy "WriteField9Mil".
       Copy "WriteFieldAmount1".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldQty".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "PrintDBReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
       Copy "ZoomBox".
       Copy "CTOSCobolAccept".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "TimeChecking".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "Error3Message".
       Copy "WriteDailyExcep1".
      *
      * END-OF-JOB
