       IDENTIFICATION DIVISION.
       PROGRAM-ID. SlInvoic.
       AUTHOR.  CHRISTENSEN.
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
         Copy "SelectStMaster".
         Copy "SelectStSpecPr".
         Copy "SelectSlMaster".
         Copy "SelectSlParameter". 
         Copy "SelectDrTrans".
         Copy "SelectStTrans".
         Copy "SelectSlRegister".
         Copy "SelectSlDistributions".
         Copy "SelectSlSoldBy".
         Copy "SelectCoPullBy".
         Copy "SelectCoPullers".
         Copy "SelectCoCashSales".
         Copy "SelectSlInvRev".
         Copy "SelectSlSbRep".
         Copy "SelectSlDaily".
         Copy "SelectCoMenu".
         Copy "SelectSlSpecials".
         Copy "SelectStDiscAcc".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LASER-PRINT ASSIGN TO W-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdStock.
           COPY ChlfdStPrice.
           COPY ChlfdSales.
           COPY ChlfdDaily.
           COPY ChlfdParam.
           COPY ChlfdDrTrans.
           COPY ChlfdStTrans.
           COPY ChlfdDisTot.
           COPY ChlfdRegister.
           COPY ChlfdSoldBy.
           COPY ChlfdPullBy.
           COPY ChlfdPullers.
           COPY ChlfdCashSale.
           COPY ChlfdSbRep.
           COPY ChlfdInvRev.
           COPY ChlfdMenu.
           COPY ChlfdSpecialSales.
           COPY ChlfdStDiscAcc.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       FD  LASER-PRINT.
       01  LASER-REC.
           03  FILLER           PIC X(135).
      *
       WORKING-STORAGE SECTION.
       77  WS-STOCKNUMBER       PIC X(15).
       77  WS-HEADING-DISPLAY   PIC X(38) VALUE " ".
       77  WS-ACCNO-X           PIC X(7) VALUE " ".
       77  WS-NEWORDER          PIC X VALUE " ".
       77  WS-LINECHANGED       PIC X VALUE " ".
       77  WS-BORDERS-FOUND     PIC X VALUE " ".
       77  WS-NON-STOCK-FOUND   PIC X VALUE " ".
       77  WS-DIS               PIC X VALUE " ".
       77  WS-COST-DISPLAY      PIC X VALUE "N".
       77  WS-DISCOUNT-CODE     PIC X VALUE " ".
       77  WS-CONTACT           PIC X(20) VALUE " ".
       77  WS-PHONE             PIC X(20) VALUE " ".
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-DISCOUNTREG       PIC 9(7)V99 VALUE 0.
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-ORDER             PIC 9(6) VALUE 0.
       77  WS-POORDERNO         PIC X(20) VALUE " ".
       77  WS-SALESANALYSIS     PIC X(14) VALUE " ".
       77  WS-SALESANALYSIS-SAVE  PIC X(14) VALUE " ".
       77  WS-ANAL-CODE         PIC XX VALUE " ".
       77  WSAN-CODE-SAVE       PIC XX VALUE " ".
       77  Ws-Sold-By           PIC XX VALUE " ".
       77  Ws-PULLBY            PIC XX VALUE " ".
       77  WS-DELIVERVIA        PIC X(20) VALUE " ".
       77  WS-TERMOFSALE        PIC X(11) VALUE " ".
       77  WS-DR-DISC           PIC 9(2)V99 VALUE 0.
       77  WS-BINNO             PIC X(6) VALUE " ".
       77  WS-GSTNO             PIC X(13) VALUE " ".
       77  WS-CO-VATNO          PIC X(15) VALUE " ".
       77  WS-INVOICEDATE       PIC 9(8) VALUE 0.
       77  WS-ORDERDATE         PIC 9(8) VALUE 0.
       77  WS-COMMENTLINE       PIC X(30) VALUE " ".
       77  WS-ADDONFREIGHT      PIC 9(8)V99 VALUE 0.
       77  WS-POSTADDON         PIC 9(8)V99 VALUE 0.
       77  WS-HANDADDON         PIC 9(8)V99 VALUE 0.
       77  WS-MISCADDON         PIC 9(8)V99 VALUE 0.
       77  WS-SUBTOTAL          PIC 9(8)V99 VALUE 0.
       77  WS-ADDONAMT          PIC 9(8)V99 VALUE 0.
       77  WS-TAXAMT            PIC 9(8)V99 VALUE 0.
       77  WS-TAXAMT-SAVE       PIC 9(8)V99 VALUE 0.
       77  WS-INVOICETOTAL      PIC 9(8)V99 VALUE 0.
       77  WS-TAXABLETOTAL      PIC 9(8)V99 VALUE 0.
       77  WS-NONTAXABLETOTAL   PIC 9(8)V99 VALUE 0.
       77  WS-WORKTOTAL         PIC 9(8)V99 VALUE 0.
       77  WS-WORKTOTAL2        PIC 9(9)V99 VALUE 0.
       77  WS-COSTTOTAL         PIC 9(8)V99 VALUE 0.
       77  WS-COSTTOTAL1        PIC 9(8)V99 VALUE 0.
       77  WS-PRICETOTAL        PIC 9(8)V99 VALUE 0.
       77  WS-PRICETOTAL1       PIC 9(8)V99 VALUE 0.
       77  WS-EXPORTTOTAL       PIC 9(8)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(8)V99 VALUE 0.
       77  WS-DISCOUNT          PIC 9(8)V99 VALUE 0.
       77  WS-ORDCOSTTOTAL      PIC 9(8)V99 VALUE 0.
       77  WS-ORDDISCOUNTREG    PIC 9(8)V99 VALUE 0.
       77  WS-ORDWORKTOTAL      PIC 9(8)V99 VALUE 0.
       77  WS-ORDDISCOUNT       PIC 9(8)V99 VALUE 0.
       77  WS-ORDTAXABLETOTAL   PIC 9(8)V99 VALUE 0.
       77  WS-ORDNONTAXABLETOTAL   PIC 9(8)V99 VALUE 0.
       77  WS-ORDERTOTAL           PIC 9(8)V99 VALUE 0.
       77  WS-ORDTAXAMT            PIC 9(8)V99 VALUE 0.
       77  WS-GST-AMT-TAXED        PIC 9(8)V99 VALUE 0.
       77  WS-GST-AMT-TAXABLE      PIC 9(8)V99 VALUE 0.
       77  WS-GST-AMT-NONTAXABLE   PIC 9(8)V99 VALUE 0.
       77  WS-GST-AMT-EXPORT       PIC 9(8)V99 VALUE 0.
       77  WS-GST-PERCENT          PIC 99V99 VALUE 0.
       77  WS-BODY-LINE         PIC ZZ9.
       77  WS-PRINTER-LASER     PIC X(20) VALUE " ".
       77  WS-IMM-PR            PIC X VALUE " ".
       77  WS-MUST-PRINT        PIC X VALUE " ".
       77  WS-TYPE-OF-DOCUMENT  PIC 9.
       77  WS-PAGE              PIC 99 VALUE 0.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "DrNameIq".
       77  WS-STOCK-INQUIRY     PIC X(8) VALUE "StMastIq".
       77  WS-PORDER-INQUIRY    PIC X(8) VALUE "SlPoShIq".
       77  WS-ZERODIS           PIC X VALUE " ".
       77  WS-INVOICEDISCOUNT   PIC 9(2)V99 VALUE 0.
       77  WS-BO-QTY            PIC S9(5) VALUE 0.
       77  WS-PERC              PIC S9(4)V99 VALUE 0.
       77  WS-NEWPRICE          PIC S9(8)V99 VALUE 0.
       77  WS-PRICESAVE         PIC S9(8)V99 VALUE 0.
       77  WS-DISCOUNTSAVE      PIC S9(4)V99 VALUE 0.
       77  PSW-SUB1             PIC S9(5) VALUE 0.
       77  PSW-SUB2             PIC S9(5) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-TERM-SUB          PIC 9 VALUE 0.    
       77  WS-DEL-SUB           PIC 9 VALUE 0.    
       77  WS-BOFOUND           PIC X VALUE " ".
       77  WS-STTRANS-NO        PIC 9(6).
       77  WS-DRTRANS-NO        PIC 9(6).
       77  WS-AREA              PIC X VALUE " ".
       77  WS-QUES-PRINT-PULLERS   PIC X VALUE " ".
       77  WS-QUES-CASH-SALES      PIC X VALUE " ".
       77  WS-QUES-ACC-OVER-LIMIT  PIC X VALUE " ".
       77  WS-LIMIT-EXCEP-WRITE    PIC X VALUE " ".
       77  WS-SPEC-COMMENT      PIC X(60) VALUE " ".
       77  WS-CASH-ACCEPT       PIC X(9) VALUE " ".
       77  WS-READS             PIC 99.
       77  WS-COPIES            PIC 9(2) VALUE 0.
       77  WS-COPY              PIC 9(2) VALUE 0.
       77  WS-COPIES-PRINTED    PIC 9(2) VALUE 0.
       77  WS-PARCEL            PIC 9(2) VALUE 0.
       77  WS-PARCEL-PRINTED    PIC 9(2) VALUE 0.
       77  WS-PasswordSaved     Pic X(10).
       01  W-READ-KEY           PIC X(11).
       01  W-CRTSTATUS           PIC 9(4) value 0.
       01  WS-STDESC.
           03  WS-DESC1          PIC X(20) VALUE " ".
           03  WS-DESC2          PIC X(20) VALUE " ".
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1       PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1        PIC 99.
       01  WS-STPR-STATUS.
           03  WS-STPR-ST1         PIC 99.
       01  WS-SALES-STATUS.
           03  WS-SALES-ST1        PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1  PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1      PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1      PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1         PIC 99.
       01  WS-DISTRIBUTION-STATUS.
           03  WS-DISTRIBUTION-ST1 PIC 99.
       01  WS-SOLDBY-STATUS.
           03  WS-SOLDBY-ST1       PIC 99.
       01  WS-PULLBY-STATUS.
           03  WS-PULLBY-ST1       PIC 99.
       01  WS-PULLERS-STATUS.
           03  WS-PU-ST1           PIC 99.
       01  WS-CASHSALE-STATUS.
           03  WS-CASHSALE-ST1     PIC 99.
       01  WS-INVREV-STATUS.
           03  WS-INVREV-ST1       PIC 99.
       01  WS-SBREP-STATUS.
           03  WS-SBREP-ST1        PIC 99.
       01  WS-MENU-STATUS.
           03  WS-MENU-ST1         PIC 99.
       01  WS-SPECIALS-STATUS.
           03  WS-SPECIALS-ST1     PIC 99.
       01  WS-STDISC-STATUS.
           03  WS-STDISC-ST1       PIC 99.
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
       01  WS-DIST-TOTALS.
           03  WS-DIST-INVOICE  PIC 9(8)V99 VALUE 0.
           03  WS-DIST-ADDON    PIC 9(8)V99 VALUE 0.
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
           03  NUMERIC-FIELD      PIC X.
           88  NUMERIC-VALUE      VALUES ARE "0" THRU "9".
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
       01  WS-BO-REDUCED-MESSAGE.
           03  FILLER           PIC X(7) VALUE " ".
           03  WS-BO-MESSAGE    PIC X(38) VALUE " ".
           03  WS-BO-INVOICE    PIC Z(5)9 BLANK WHEN ZERO.
           03  FILLER           PIC X VALUE " ".
           03  WS-BO-DATE       PIC 99/99/9999.
           03  FILLER           PIC X(72) VALUE " ".
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 200.
               05  B-STOCKNUMBER.
                   07  B-1ST-CHAR      PIC X.
                   07  B-REST          PIC X(14).
               05  B-STTRANS           PIC 9(6).
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
                   07  B-SPECIAL           PIC X.
                   07  B-COMPLETE          PIC X.
               05  C-LINE REDEFINES B-REMAINDER.
                   07  C-ORDER            PIC X(5).
                   07  C-SHIP             PIC X(5).
                   07  C-DESC             PIC X(20).
                   07  C-UNIT             PIC X(4).
                   07  C-PRICE            PIC X(9).
                   07  C-COST             PIC X(9).
                   07  C-DISC             PIC X(5).
       01  WS-TIMES.
           03  WS-HRS           PIC 99.
           03  WS-MINS          PIC 99.
           03  WS-SECS          PIC 99.
           03  WS-100S          PIC 99.
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
           03  SPLIT-TIME-FIL     PIC X(6) VALUE "Time:".
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
           
       01  PCREDITLINE.
           03  P-PRINT8         PIC X(4).
           03  FILLER           PIC X(46) VALUE " ".
           03  P-XES            PIC X(20) VALUE " ".
           03  FILLER           PIC X(8) VALUE " ".
           03  P-NAME           PIC X(54).
       01  PLINE1.
           03  FILLER           PIC X(13) VALUE " ".
           03  P-GSTNO          PIC X(18) VALUE " ".
           03  P-ACCNO          PIC X(7).
           03  FILLER           PIC X(12) VALUE " ".
           03  P-TYPE           PIC X(20) VALUE " ".
           03  FILLER           PIC X(8) VALUE " ".
           03  P-ADDNAME        PIC X(54).
       01  PLINE2.
           03  FILLER           PIC X(13) VALUE " ".
           03  P-ADD.
               05  P-ADDRESS    PIC X(50) VALUE " ".
               05  P2-DIG1      PIC X.
               05  P2-COMM      PIC X VALUE " ".
               05  P2-DIG2      PIC X.
               05  FILLER       PIC X(12) VALUE " ".
           03  SUPP-ADD.
               05  SUPP-DIG10   PIC X(7) VALUE " ".
               05  SUPP-DIG30.
                   07  SUPP-DIG-BLANK   PIC X VALUE " ".
                   07  SUPP-DIG-VAT     PIC X(22) VALUE " ".
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
           03  FILLER           PIC X VALUE " ".
           03  P-SLIP           PIC Z(5)9.
           03  P-SLIP-SLASH     PIC X.
           03  P-SLIP-COPY      PIC 99.
           03  FILLER           PIC X VALUE " ".
           03  P-DATE           PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  P-INV            PIC Z(5)9.
           03  FILLER           PIC X(2) VALUE " ".
           03  P-PAGE           PIC Z9.
           03  FILLER           PIC X(15) VALUE " ".
       01  PDET.
           03  FILLER               PIC X(2).
           03  P-NO                 PIC Z(2)9.
           03  FILLER               PIC X(1).
           03  P-STOCK              PIC X(15).
           03  PDET-REST.
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
      *        05  FILLER           PIC X(1).
               05  P-PRICE          PIC Z(7)9.99.
               05  FILLER           PIC X(1).
               05  P-DISCOUNT       PIC Z9.99.
      *        05  FILLER           PIC X(1).
               05  P-NETT           PIC Z(7)9.99.
               05  FILLER           PIC X(14).
       01  P-COMMENTLINE.
           03  FILLER           PIC X(11) VALUE " ".
           03  P-BO-MESSAGE     PIC X(38) VALUE " ".
           03  P-REST-OF-LINE.
               05  FILLER       PIC X(5).
               05  P-DIG1       PIC X.
               05  P-COMM       PIC X(20) VALUE " ".
               05  P-DIG2       PIC X.
               05  FILLER       PIC X(50).
       01  P-ADDLINE.
           03  P-PULLBY         PIC X(11) VALUE " ".
           03  P-PHONE          PIC X(20) VALUE " ".
           03  P-ADD1           PIC Z(7)9.99. 
           03  FILLER           PIC X(13) VALUE " ".
           03  P-ADD2           PIC Z(7)9.99.
           03  FILLER           PIC X(12) VALUE " ".
           03  P-ADD3           PIC Z(7)9.99.
           03  FILLER           PIC X(17) VALUE " ".
           03  P-ADD4           PIC Z(7)9.99.
           03  FILLER           PIC X(14) VALUE " ".
       01  P-CONTINUED.
           03  FILLER           PIC X(40) VALUE " ".
           03  FILLER           PIC X(22) VALUE "Continued To.....Page".
           03  P-CONT-PAGE      PIC 9.
           03  FILLER           PIC X(63) VALUE " ".
       01  PARCEL-LINE.
           03  PCL-DIG1              PIC X.
           03  PCL-REST.
              05  PCL-DESC           PIC X(12) VALUE " ".
              05  PCL-REST-DIG1      PIC X.
              05  PCL-NO1            PIC XX.
              05  PCL-FILLER         PIC X VALUE "/".
              05  PCL-NO2            PIC XX.
              05  PCL-DIG2           PIC X.
       01  PARCEL2.
           03  PL2-DIG1               PIC X.
           03  PL2-REST.
              05  PL2-DESC            PIC X(12) VALUE " ".
              05  PL2-REST-DIG1       PIC X.
              05  PL2-ADD             PIC X(30) VALUE " ".
              05  PL2-DIG2            PIC X.
       01 WS-FST-LINE.
          05  WS-DELIM-F             PIC  X(2).
          05  WS-DATA-F              PIC  X(98).
       01 WS-OTH-LINE-1.
          05  WS-O-L                 PIC  X(8).
          05  WS-O-LINE              PIC  99.
          05  FILLER                 PIC  X(89).
       01 WS-OTH-LINE.
          05  WS-DELIM-O             PIC  X.
          05  WS-DATA-O              PIC  X(99).
       01  LASER-PCREDITLINE.
           03  PLCR-CHAR1       PIC X(2).
           03  FILLER           PIC X(2) VALUE " ".
           03  PL-TYPE          PIC X(22) VALUE " ".
           03  FILLER           PIC X(14) VALUE " ".
           03  PL-NAME          PIC X(98).
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
           03  PL2-CHAR         PIC X(2) VALUE " ".
           03  FILLER           PIC X(3) VALUE " ".
           03  PL-ADD.
               05  PL-ADDRESS    PIC X(47) VALUE " ".
               05  SUPPL-TIME    PIC X(21).
               05  PL-PULLBY     PIC X(9) VALUE " ".
               05  PL-AREA       PIC X(7).
           03  SUPPL-ADD.
               05  SUPPL-DIG10   PIC X(13) VALUE " ".
               05  SUPPL-DIG30   PIC X(23) VALUE " ".
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
           03  FILLER            PIC X(15) VALUE " ".
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
               05  FILLER            PIC X(1).
               05  PL-PRICE          PIC Z(7)9.99.
               05  FILLER            PIC X(2).
               05  PL-DISCOUNT       PIC Z9.99.
               05  FILLER            PIC X(1).
               05  PL-NETT           PIC Z(7)9.99.
       01  LASERPL-COMMENTLINE.
           03  PLCOM-CHAR        PIC X(2) VALUE " ".
           03  FILLER            PIC X(11) VALUE " ".
           03  PL-BO-MESSAGE     PIC X(38) VALUE " ".
           03  PL-REST-OF-LINE.
               05  FILLER        PIC X(6).
               05  PL-COMM       PIC X(21) VALUE " ".
               05  FILLER        PIC X(50).
       01  LASERPL-ADDLINE.
           03  PLADD-CHAR        PIC X(2) VALUE " ".
           03  FILLER            PIC X(11) VALUE " ".
           03  PL-PHONE          PIC X(27) VALUE " ".
           03  PL-ADD1           PIC Z(7)9.99. 
           03  FILLER            PIC X(15) VALUE " ".
           03  PL-ADD2           PIC Z(7)9.99.
           03  FILLER            PIC X(16) VALUE " ".
           03  PL-ADD3           PIC Z(7)9.99.
           03  FILLER            PIC X(13) VALUE " ".
           03  PL-CURRENCY       PIC X(5) VALUE " ".
           03  PL-ADD4           PIC Z(7)9.99.
       01  PL-CONTINUED.
           03  PLCONT-CHAR     PIC X(2) VALUE " ".
           03  FILLER          PIC X(38) VALUE " ".
           03  FILLER          PIC X(22) VALUE "Continued To.....Page".
           03  PL-CONT-PAGE    PIC 9.
           03  FILLER          PIC X(63) VALUE " ".
       Copy "WsDateInfo".
       Copy "WStore".
       Copy "FServer".
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

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           PERFORM INITIAL-MESSAGE
           MOVE 2910 TO POS
           DISPLAY "The Invoice Program Is Being Loaded !!" AT POS.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN
           MOVE "ON-HAND:"  TO FILLER-ONHAND
           MOVE "ON-RES:"   TO FILLER-ONRES
           MOVE "ON-B/O:"   TO FILLER-ONBO
           MOVE "ON-ORDER:" TO FILLER-ONORD.
       CONT-010.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           PERFORM DISPLAY-FORM
           PERFORM GET-DATA
           PERFORM PRINT-INVOICE
           PERFORM ERROR1-020
           IF WS-IMM-PR = "S" OR = "H"
            IF WS-DIS = "Y"
               PERFORM PRINT-LABEL.
           GO TO CONT-010.
      *     Copy "<Copy>PrinterSpecial".
       CONT-999.
           EXIT.
      *
       INITIAL-MESSAGE SECTION.
       IM-000.
           PERFORM CLEAR-SCREEN.
       IM-005.
           MOVE 0430 TO POS
           DISPLAY "** INVOICE PROGRAM **" AT POS
           MOVE 0510 TO POS
           DISPLAY
           "Is This Batch run, Counter Print, Stores Print OR HP" &
           " Laser Print ?" AT POS
           MOVE 0710 TO POS
           DISPLAY
           "B=Batch Print; C=Counter Print; S=Stores Print; H=HP: [ ]"
                AT POS
           MOVE 0765 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 04        TO CDA-ROW.
           MOVE 64        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-IMM-PR.
           
      *      ACCEPT WS-IMM-PR AT POS.
           IF W-ESCAPE-KEY = 3
               EXIT PROGRAM.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO IM-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO IM-005.
       IM-010.
           IF WS-IMM-PR = "B" OR = "C" OR = "H" OR = "S"
              GO TO IM-040.
       IM-020.
           MOVE 1001 TO POS
           DISPLAY "YOUR SELECTION MUST BE EITHER B OR C OR H OR S."
            AT POS
           DISPLAY " " AT 3079 WITH BELL
           GO TO IM-000.
       IM-040.
           IF WS-IMM-PR = "B"
               GO TO IM-999.
           MOVE 1 TO SUB-1.
       IM-045.
           IF WS-IMM-PR = "C"
            IF WS-PRINTERNUMBER (SUB-1) = 7
               MOVE WS-PRINTERNAME (SUB-1)  TO WS-PRINTER
               MOVE WS-PRINTERCHARS (SUB-1) TO WS-PRINT-CHARS
               GO TO IM-999.
           IF WS-IMM-PR = "S"
            IF WS-PRINTERNUMBER (SUB-1) = 4
               MOVE WS-PRINTERNAME (SUB-1)  TO WS-PRINTER
               MOVE WS-PRINTERCHARS (SUB-1) TO WS-PRINT-CHARS
               GO TO IM-999.
           IF WS-IMM-PR = "H"
            IF WS-PRINTERNUMBER (SUB-1) = 15
               MOVE WS-PRINTERNAME (SUB-1)  TO WS-PRINTER-SAVE
                                               WS-PRINTER-LASER
               MOVE 3 TO WS-PROG-TYPE
               GO TO IM-999.
           IF SUB-1 < 11
             ADD 1 TO SUB-1
             GO TO IM-045.
           MOVE "CAN'T FIND A PRINTERNUMBER, PRN PARAMETER NOT SET UP."
             TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
      *     MOVE 3               TO WS-PRINTERTYPE (11).
       IM-050.
           GO TO IM-999.
       IM-600.
      * SECTION REMOVED 6/9/2000.  A/C'S WANT ALL THREE COPIES
      *
      *     IF WS-IMM-PR = "H"
      *      IF WS-ACCOUNT-NUMBER NOT = 0300087 AND NOT = 0300090
      *     AND NOT = 0300100 AND NOT = 0300150 AND NOT = 0300200
      *     AND NOT = 9999999
      *         MOVE 3 TO WS-PROG-TYPE
      *     ELSE
      *         MOVE 2 TO WS-PROG-TYPE.
               MOVE 3 TO WS-PROG-TYPE.
       IM-640.
           GO TO IM-999.
       IM-650.
           PERFORM CDNVD-005
           ACCEPT WS-TIMES FROM TIME
           MOVE "/ctools/spl/" To ALPHA-RATE
           PERFORM CDNVD-015
           MOVE WS-TIMES TO DATA-RATE
           MOVE 1 TO SUB-2
           PERFORM CDNVD-025
           SUBTRACT 1 FROM SUB-1
           MOVE ALPHA-RATE TO W-FILENAME.
           MOVE SUB-1      TO FS-CBDATAFILE.
       IM-999.
           EXIT.
      *
       WORK-OUT-PRINT-FILE-NAME SECTION.
       WOPFN-001.
           MOVE WS-PRINTER-LASER TO WS-PRINTER-SAVE.
           
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

           MOVE SPACES      TO DATA-RATE
           MOVE "InPrintCo" TO DATA-RATE
           MOVE 1           TO SUB-2.
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
      *     MOVE WS-PRINTER-SAVE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

      *     MOVE W-FILENAME TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       WOPFN-999.
            EXIT.
      *
       PRINT-INVOICE SECTION.
       PR-000.
           IF F-EXIT-CH = X"94"
               PERFORM PR-005 THRU PR-036
               GO TO PR-999.
       PR-001.
           OPEN I-O INV-REV.
           IF WS-INVREV-ST1 NOT = 0
              MOVE 0 TO WS-INVREV-ST1
                GO TO PR-002.
       PR-0015.
           READ INV-REV NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-INVREV-ST1 = 10
              MOVE 0 TO WS-INVREV-ST1
              CLOSE INV-REV
              GO TO PR-002.
           IF WS-INVREV-ST1 NOT = 0
              MOVE "INV RE RECORD BUSY ON READ PR-0015, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INVREV-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INVREV-ST1
               GO TO PR-0015.
           MOVE INV-NO TO WS-INVOICE.
       PR-0017.
           DELETE INV-REV
              INVALID KEY NEXT SENTENCE.
           IF WS-INVREV-ST1 NOT = 0
              MOVE "INV RE RECORD BUSY ON DEL PR-0017, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INVREV-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INVREV-ST1
               GO TO PR-0017.
           CLOSE INV-REV.
           GO TO PR-004.
       PR-002.
           PERFORM READ-PARAMETER-LOCK
           MOVE PA-INVOICE-NUMBER TO WS-INVOICE.
       PR-003.         
           COMPUTE PA-INVOICE-NUMBER = PA-INVOICE-NUMBER + 1
           PERFORM REWRITE-PARAMETER
           PERFORM READ-PARAMETER-LOCK.
           IF WS-INVOICE = PA-INVOICE-NUMBER
              GO TO PR-003.      
           PERFORM REWRITE-PARAMETER.
       PR-004.
           MOVE "INVOICENUM" TO F-FIELDNAME
           MOVE 10 TO F-CBFIELDNAME
           MOVE 6  TO F-CBFIELDLENGTH
           MOVE WS-INVOICE TO F-EDNAMEFIELDNUM
           PERFORM WRITE-FIELD-NUMERIC.
           
           PERFORM QUES-PULL-CASH.
       PR-005.
           MOVE 1 TO WS-PAGE SUB-1 SUB-2.
           MOVE " " TO PRINT-REC.
           IF WS-IMM-PR = "B"
               GO TO PR-050. 
           
           IF WS-IMM-PR = "H"
      *        PERFORM IM-600
      *        PERFORM IM-650
      *        PERFORM GET-USER-PRINT-NAME
              MOVE WS-PRINTER-LASER TO WS-PRINTER
              PERFORM WORK-OUT-PRINT-FILE-NAME
              OPEN OUTPUT LASER-PRINT
              PERFORM ZL1-LASER-HEADINGS
              MOVE 1 TO WS-TYPE-OF-DOCUMENT
              PERFORM LASER-PRINT-INVOICE
      *        PERFORM A995-QUEUE-FSD
              GO TO PR-050.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       PR-010.
           IF WS-PAGE > 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE WS-PAGE TO P-CONT-PAGE
               WRITE PRINT-REC FROM P-CONTINUED
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC BEFORE PAGE.
           MOVE PA-NAME TO P-NAME.
           WRITE PRINT-REC FROM PCREDITLINE.
           MOVE " "       TO PRINT-REC.
       PR-012.
      **********************************************
      * TEST FOR CHANGING PAGE LENGTH TO 8INCH     *
      **********************************************
           MOVE WS-PRINT-8            TO P-PRINT8.
           
           MOVE WS-GSTNO              TO P-GSTNO
           MOVE WS-ACCOUNT-NUMBER     TO P-ACCNO
           MOVE PA-ADD1               TO P-ADDNAME
           WRITE PRINT-REC FROM PLINE1
           MOVE " "       TO PRINT-REC PLINE2
           MOVE PA-ADD2   TO SUPP-ADD
           WRITE PRINT-REC FROM PLINE2
           MOVE " "        TO PRINT-REC PLINE2
           MOVE WS-DELADD1 TO P-ADD
           MOVE PA-ADD3    TO SUPP-ADD
           WRITE PRINT-REC  FROM PLINE2
           MOVE " "        TO PRINT-REC PLINE2
           MOVE WS-DELADD2 TO P-ADD
           MOVE PA-DEL1    TO SUPP-ADD
           WRITE PRINT-REC  FROM PLINE2
           MOVE " "        TO PRINT-REC PLINE2
           MOVE WS-DELADD3 TO P-ADD
           MOVE PA-DEL2    TO SUPP-ADD
           WRITE PRINT-REC  FROM PLINE2
           MOVE " "        TO PRINT-REC PLINE2
           MOVE PA-DEL3    TO SUPP-ADD
           WRITE PRINT-REC FROM PLINE2
           MOVE " "        TO PRINT-REC PLINE2.

           MOVE WS-NAME         TO P-ADD
           WRITE PRINT-REC FROM PLINE2
           MOVE " "             TO PRINT-REC PLINE2
           MOVE WS-ADD1         TO P-ADD
           MOVE PA-PHONE        TO SUPP-DIG30
           WRITE PRINT-REC FROM PLINE2
           MOVE " "             TO PRINT-REC PLINE2
           MOVE WS-ADD2         TO P-ADDRESS
           MOVE PA-FAX       TO SUPP-DIG30
           WRITE PRINT-REC FROM PLINE2
           MOVE " "          TO PRINT-REC PLINE2
           MOVE WS-ADD3      TO P-ADD
           MOVE PA-CO-REG-NO TO SUPP-DIG30
           WRITE PRINT-REC      FROM PLINE2
           MOVE " "          TO PRINT-REC PLINE2.

           MOVE WS-POSTCODE     TO P-ADD
           MOVE WS-PRINT-BOLD   TO P2-DIG1
           MOVE INCR-AREA       TO P2-COMM
           MOVE WS-PRINT-UNBOLD TO P2-DIG2
           MOVE PA-CO-VAT-NO    TO SUPP-DIG-VAT
           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR           TO SPLIT-HR
           MOVE ":"             TO SPLIT-HR-FIL
                                   SPLIT-MN-FIL
           MOVE WS-MIN          TO SPLIT-MN
           MOVE WS-SEC          TO SPLIT-SC
           MOVE WS-TIME-DISPLAY TO SUPP-TIME
           WRITE PRINT-REC      FROM PLINE2
           MOVE " "             TO PRINT-REC PLINE2 PLINE4.

           WRITE PRINT-REC
           WRITE PRINT-REC
           MOVE WS-TERMOFSALE    TO P-TERMS
           MOVE WS-POORDERNO     TO P-PO
           MOVE WSAN-CODE        TO P-SOLD
           MOVE WS-DELIVERVIA    TO P-VIA
           MOVE WS-BINNO         TO P-BIN
           MOVE Ws-Sold-By        TO P-SOLDBY.
           IF WS-NEWORDER NOT = "Y"
              MOVE WS-ORDERDATE  TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE  TO P-ORDERDATE
              MOVE WS-ORDER      TO P-SLIP
              MOVE "/"           TO P-SLIP-SLASH
              MOVE WS-COPY       TO P-SLIP-COPY.
           MOVE WS-INVOICEDATE   TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE     TO P-DATE
           MOVE WS-INVOICE       TO P-INV
           MOVE WS-PAGE          TO P-PAGE
           WRITE PRINT-REC FROM PLINE4
           MOVE " "              TO PRINT-REC PLINE4
           WRITE PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PDET.
       PR-020.
           IF SUB-1 < 199
             IF SUB-1 = SUB-20
               SUBTRACT 1 FROM SUB-2
               GO TO PR-030.
           IF SUB-2 > 20
               MOVE 1 TO SUB-2
               ADD 1 TO WS-PAGE
               GO TO PR-010.
           IF SUB-1 > 200
              GO TO PR-030.
           IF B-STOCKNUMBER (SUB-1) = " "
              AND B-ORDERQTY (SUB-1) = 0
               GO TO PR-030.
           MOVE B-STOCKNUMBER (SUB-1)       TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               MOVE B-STOCKNUMBER (SUB-1)   TO P-STOCK
               MOVE C-LINE (SUB-1)          TO PDET-REST
               GO TO PR-025.
           MOVE B-STOCKNUMBER (SUB-1)       TO P-STOCK
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO P-DESC
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO P-DESC2
           MOVE B-UNIT (SUB-1)              TO P-UNIT
           MOVE B-ORDERQTY (SUB-1)          TO P-ORDER
           MOVE B-SHIPQTY (SUB-1)           TO P-SHIP.
           IF B-COMPLETE (SUB-1) NOT = "R"
             COMPUTE WS-BO-QTY = B-ORDERQTY (SUB-1) -
               (B-SHIPQTY (SUB-1) + B-SHIPPEDQTY (SUB-1))
             MOVE WS-BO-QTY                 TO P-BO
             MOVE B-SHIPPEDQTY (SUB-1)      TO P-SHIPPED.
           IF B-COMPLETE (SUB-1) = "R"
              MOVE 0                        TO P-BO
              MOVE 0                        TO P-SHIPPED
              MOVE 0                        TO P-SHIP.
           MOVE B-STOCKPRICE (SUB-1)        TO P-PRICE
           MOVE B-DISCOUNTPERITEM (SUB-1)   TO P-DISCOUNT
           MOVE B-NETT (SUB-1)              TO P-NETT.
       PR-025.
           MOVE SUB-1 TO P-NO
           WRITE PRINT-REC FROM PDET
           MOVE " " TO PDET PRINT-REC.
           ADD 1 TO SUB-1
                    SUB-2.
           IF SUB-1 < 201
              GO TO PR-020.
       PR-030.
           IF SUB-2 < 20
              WRITE PRINT-REC
              ADD 1 TO SUB-2
              GO TO PR-030.
       PR-035.
           MOVE " "              TO P-COMMENTLINE
           MOVE WS-COMMENTLINE   TO P-BO-MESSAGE
           MOVE WS-SPEC-COMMENT  TO P-REST-OF-LINE
           WRITE PRINT-REC     FROM P-COMMENTLINE
           MOVE " "              TO P-COMMENTLINE.

           MOVE WS-CONTACT      TO P-BO-MESSAGE
           MOVE WS-PRINT-BOLD   TO P-DIG1
           MOVE PA-COMMENT      TO P-COMM
           MOVE WS-PRINT-UNBOLD TO P-DIG2
           WRITE PRINT-REC    FROM P-COMMENTLINE.

           MOVE " " TO P-ADDLINE P-COMMENTLINE PRINT-REC
           MOVE INCR-PULLBY      TO P-PULLBY
           MOVE WS-PHONE         TO P-PHONE
           MOVE WS-ADDONFREIGHT  TO P-ADD1
           MOVE WS-HANDADDON     TO P-ADD2
           MOVE WS-TAXAMT        TO P-ADD3
           WRITE PRINT-REC     FROM P-ADDLINE
           MOVE " "              TO P-ADDLINE PRINT-REC.

           MOVE WS-POSTADDON     TO P-ADD1
           MOVE WS-MISCADDON     TO P-ADD2
           MOVE WS-SUBTOTAL      TO P-ADD3
           MOVE WS-INVOICETOTAL  TO P-ADD4
           WRITE PRINT-REC     FROM P-ADDLINE
           MOVE " "              TO P-ADDLINE PRINT-REC.
       PR-036.
           WRITE PRINT-REC.
           MOVE " " TO PRINT-REC
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.

           MOVE 2958 TO POS
           DISPLAY WS-MESSAGE AT POS.
       PR-050.
           IF F-EXIT-CH = X"94"
               GO TO PR-999.
               
           MOVE 2910 TO POS
           DISPLAY "1. DEBTOR-TRANS BEING WRITTEN..........    " AT POS
           PERFORM WRITE-DEBTOR-TRANSACTIONS
           
           MOVE 2910 TO POS
           DISPLAY "2. DEBTOR FILE BEING UPDATED....           " AT POS
           PERFORM UPDATE-DEBTOR
           
           MOVE 2910 TO POS
           DISPLAY "3. SALES ANALYSIS FILE BEING UPDATED....   " AT POS
           PERFORM UPDATE-SALES
           
           MOVE 2910 TO POS
           DISPLAY "4. DISTRIBUTION FILE BEING UPDATED......   " AT POS
           PERFORM UPDATE-DISTRIBUTION
           
           MOVE 2910 TO POS
           DISPLAY "5. STOCK FILES BEING UPDATED..........     " AT POS
           PERFORM UPDATE-STOCK
           
           MOVE 2910 TO POS
           DISPLAY "6. ORDERS FILE BEING UPDATED..........     " AT POS
           PERFORM REWRITE-ORDER-TRANS
           
           MOVE 2910 TO POS
           DISPLAY "7. INVOICE REGISTER BEING WRITTEN.......   " AT POS
           PERFORM WRITE-INCR-REGISTER
           
           MOVE 2910 TO POS
           DISPLAY "8. STOCK-TRANS BEING WRITTEN..........     " AT POS
           PERFORM WRITE-STOCK-TRANSACTIONS.  
           
           IF WS-NEWORDER = "N"
               MOVE 2910 TO POS
               DISPLAY "9. STOCK-TRANS (ORDER) BEING UPDATED..." AT POS
               PERFORM REWRITE-STOCK-TRANSACTIONS.
           MOVE 2910 TO POS
           DISPLAY "10. SOLDBY FILE BEING WRITTEN..........    " AT POS
           PERFORM WRITE-SOLD-BY.
       PR-999.
           EXIT.
      *
       LASER-PRINT-INVOICE SECTION.
       LR-005.
           MOVE 1 TO WS-PAGE SUB-1 SUB-2.
           MOVE " " TO PRINT-REC.
       LR-010.
      ***************************************
      *INVOICE BEGIN CHAR = HEXB4 �         *
      *NEXT LINE BEGIN CHAR = HEXB6 �       *
      ***************************************
           IF WS-PAGE > 1
               MOVE " " TO LASER-REC
               MOVE "�"     TO PLCONT-CHAR
               MOVE WS-PAGE TO PL-CONT-PAGE
               WRITE LASER-REC FROM PL-CONTINUED
               MOVE " "     TO LASER-REC
               MOVE "�"     TO LASER-REC
               WRITE LASER-REC
               WRITE LASER-REC
               WRITE LASER-REC
               WRITE LASER-REC.
               
      *      MOVE "�"         TO LASER-REC
      *      WRITE LASER-REC.
  
           MOVE " "     TO LASER-REC
           MOVE "뉜"    TO PLCR-CHAR1
           IF WS-TYPE-OF-DOCUMENT = 1
              MOVE "      TAX INVOICE     " TO PL-TYPE
              GO TO LP-011.
           IF WS-TYPE-OF-DOCUMENT = 2
              MOVE "SUPPLIER DELIVERY NOTE" TO PL-TYPE
              GO TO LP-011.
           IF WS-TYPE-OF-DOCUMENT = 3
              MOVE "CUSTOMER DELIVERY NOTE" TO PL-TYPE.
       LP-011.
           MOVE " "     TO LASER-REC
           MOVE "�"     TO PL2-CHAR
           MOVE PA-NAME TO PL-NAME.
           WRITE LASER-REC FROM LASER-PCREDITLINE.
           MOVE " "     TO LASER-REC.
       LR-012.
           MOVE "�"                   TO PL1-CHAR
           MOVE WS-GSTNO              TO PL-GSTNO
           MOVE WS-ACCOUNT-NUMBER     TO PL-ACCNO
           MOVE PA-ADD1               TO PL-ADDNAME
           WRITE LASER-REC FROM LASER-PLINE1.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "�"        TO PL2-CHAR
           MOVE PA-ADD2    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "�"        TO PL2-CHAR
           MOVE WS-DELADD1 TO PL-ADD
           MOVE PA-ADD3    TO SUPPL-ADD
           WRITE LASER-REC  FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "�"        TO PL2-CHAR
           MOVE WS-DELADD2 TO PL-ADD
           MOVE PA-DEL1    TO SUPPL-ADD
           WRITE LASER-REC  FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "�"        TO PL2-CHAR
           MOVE WS-DELADD3 TO PL-ADD
           MOVE PA-DEL2    TO SUPPL-ADD
           WRITE LASER-REC  FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "�"        TO PL2-CHAR
           MOVE PA-DEL3    TO SUPPL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2.
           MOVE "�"        TO PL2-CHAR
           MOVE WS-NAME    TO PL-ADD
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "�"        TO PL2-CHAR
           MOVE WS-ADD1    TO PL-ADD
           MOVE PA-PHONE   TO SUPPL-DIG30
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "        TO LASER-REC LASER-PLINE2
           MOVE "�"        TO PL2-CHAR
           MOVE WS-ADD2    TO PL-ADD
           MOVE PA-FAX     TO SUPPL-DIG30
           WRITE LASER-REC FROM LASER-PLINE2.
           
           MOVE " "          TO LASER-REC LASER-PLINE2
           MOVE "�"          TO PL2-CHAR
           MOVE WS-ADD3      TO PL-ADD
           MOVE PA-CO-REG-NO TO SUPPL-DIG30
           WRITE LASER-REC      FROM LASER-PLINE2.
           
           MOVE " "             TO LASER-REC LASER-PLINE2.
           MOVE "�"             TO PL2-CHAR
           MOVE WS-POSTCODE     TO PL-ADD
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
           MOVE "�"              TO PL4-CHAR
           MOVE WS-TERMOFSALE    TO PL-TERMS
           MOVE WS-POORDERNO     TO PL-PO
           MOVE WSAN-CODE        TO PL-SOLD
           MOVE WS-DELIVERVIA    TO PL-VIA
           MOVE WS-BINNO         TO PL-BIN
           MOVE WS-SOLD-BY       TO PL-SOLDBY.
           IF WS-NEWORDER NOT = "Y"
              MOVE WS-ORDERDATE  TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE  TO PL-ORDERDATE
              MOVE WS-ORDER      TO PL-SLIP
              MOVE "/"           TO PL-SLIP-SLASH
              MOVE WS-COPY       TO PL-SLIP-COPY.
           MOVE WS-INVOICEDATE   TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE     TO PL-DATE
           MOVE WS-INVOICE       TO PL-INV
           MOVE WS-PAGE          TO PL-PAGE
           WRITE LASER-REC FROM LASER-PLINE4.
           
           MOVE " "              TO LASER-REC LASER-PLINE4
                                    LASER-PDET.
       LR-020.
           IF SUB-1 < 199
             IF SUB-1 = SUB-20
               SUBTRACT 1 FROM SUB-2
               GO TO LR-030.
           IF SUB-2 > 20
               MOVE 1 TO SUB-2
               ADD 1  TO WS-PAGE
               GO TO LR-010.
           IF SUB-1 > 200
              GO TO LR-030.
           IF B-STOCKNUMBER (SUB-1) = " "
              AND B-ORDERQTY (SUB-1) = 0
               GO TO LR-030.
           MOVE "�"                         TO PLDET-CHAR
           MOVE B-STOCKNUMBER (SUB-1)       TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               MOVE B-STOCKNUMBER (SUB-1)   TO PL-STOCK
               MOVE C-LINE (SUB-1)          TO PLDET-REST
               GO TO LR-025.
           MOVE B-STOCKNUMBER (SUB-1)       TO PL-STOCK
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO PL-DESC
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO PL-DESC2
           MOVE B-UNIT (SUB-1)              TO PL-UNIT
           MOVE B-ORDERQTY (SUB-1)          TO PL-ORDER
           MOVE B-SHIPQTY (SUB-1)           TO PL-SHIP.
           IF B-COMPLETE (SUB-1) NOT = "R"
             COMPUTE WS-BO-QTY = B-ORDERQTY (SUB-1) -
               (B-SHIPQTY (SUB-1) + B-SHIPPEDQTY (SUB-1))
             MOVE WS-BO-QTY                 TO PL-BO
             MOVE B-SHIPPEDQTY (SUB-1)      TO PL-SHIPPED.
           IF B-COMPLETE (SUB-1) = "R"
              MOVE 0                        TO P-BO
              MOVE 0                        TO P-SHIPPED
              MOVE 0                        TO P-SHIP.
           MOVE B-STOCKPRICE (SUB-1)        TO PL-PRICE
           MOVE B-DISCOUNTPERITEM (SUB-1)   TO PL-DISCOUNT
           MOVE B-NETT (SUB-1)              TO PL-NETT.
       LR-025.
           MOVE SUB-1 TO PL-NO
           WRITE LASER-REC FROM LASER-PDET
           MOVE " " TO LASER-REC LASER-PDET.
           ADD 1 TO SUB-1
                    SUB-2.
           IF SUB-1 < 201
              GO TO LR-020.
       LR-030.
           IF SUB-2 < 20
              MOVE "�" TO PLDET-CHAR
              WRITE LASER-REC FROM LASER-PDET
              ADD 1 TO SUB-2
              GO TO LR-030.
       LR-035.
           MOVE " "              TO LASERPL-COMMENTLINE
           MOVE "�"              TO PLCOM-CHAR
           MOVE WS-COMMENTLINE   TO PL-BO-MESSAGE
           MOVE WS-SPEC-COMMENT  TO PL-REST-OF-LINE
           WRITE LASER-REC        FROM LASERPL-COMMENTLINE.
           
           MOVE " "              TO LASERPL-COMMENTLINE.
           MOVE "�"              TO PLCOM-CHAR
           MOVE WS-CONTACT       TO PL-BO-MESSAGE
           MOVE PA-COMMENT       TO PL-COMM
           WRITE LASER-REC       FROM LASERPL-COMMENTLINE.

           MOVE " " TO LASER-REC LASERPL-ADDLINE LASERPL-COMMENTLINE.
           MOVE "�"              TO PLADD-CHAR
           MOVE WS-PHONE         TO PL-PHONE
           WRITE LASER-REC        FROM LASERPL-ADDLINE.

           MOVE " " TO LASER-REC LASERPL-ADDLINE LASERPL-COMMENTLINE.
           MOVE "�"              TO PLADD-CHAR
           MOVE WS-ADDONFREIGHT  TO PL-ADD1
           MOVE WS-HANDADDON     TO PL-ADD2
           MOVE WS-TAXAMT        TO PL-ADD3
           WRITE LASER-REC        FROM LASERPL-ADDLINE.
           
           MOVE " "              TO LASERPL-ADDLINE LASER-REC.
           MOVE "�"              TO PLADD-CHAR
           MOVE WS-POSTADDON     TO PL-ADD1
           MOVE WS-MISCADDON     TO PL-ADD2
           MOVE WS-SUBTOTAL      TO PL-ADD3
           MOVE "ZAR"            TO PL-CURRENCY
           MOVE WS-INVOICETOTAL  TO PL-ADD4
           WRITE LASER-REC        FROM LASERPL-ADDLINE.
       LR-036.
           
      * TO DETERMINE WHAT HEADING FOR THE DOCUMENT TO PRINT SEE BELOW:
      * WS-PROG-TYPE: 1=INVOICE ONLY
      *               2=D/NOTES ONLY
      *               3=INVOICE & D/NOTES
      *               4=CREDIT NOTES
      
      *PRINT INVOICE ONLY
           IF WS-PROG-TYPE = 1
               GO TO LR-900.
               
      *PRINT DELIVERY NOTES ONLY
           IF WS-PROG-TYPE = 2
            IF WS-TYPE-OF-DOCUMENT = 2
               MOVE 3 TO WS-TYPE-OF-DOCUMENT
               GO TO LR-005.
               
           IF WS-PROG-TYPE = 2
            IF WS-TYPE-OF-DOCUMENT = 3
               GO TO LR-900.
               
      *PRINT INVOICE & DELIVERY NOTES
           IF WS-PROG-TYPE = 3
            IF WS-TYPE-OF-DOCUMENT < 3
               ADD 1 TO WS-TYPE-OF-DOCUMENT
               GO TO LR-005.
           IF WS-PROG-TYPE = 2 OR = 3
            IF WS-TYPE-OF-DOCUMENT = 3
               GO TO LR-900.
       LR-900.
           MOVE " " TO LASERPL-ADDLINE LASER-REC.
           CLOSE LASER-PRINT.
           PERFORM SETUP-INVOICE-FOR-PDF

           MOVE 2958 TO POS
           DISPLAY WS-MESSAGE AT POS.
       LR-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-010.
            PERFORM CI-900.
            UNLOCK INCR-REGISTER
            UNLOCK STOCK-TRANS-FILE.
                        
            MOVE "INVOICEDATE" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            MOVE WS-DATE       TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE  TO F-NAMEFIELD.
            MOVE 10            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            IF WS-IMM-PR = "B"
             MOVE "* INVOICE ENTRY - BATCH PRINTING ** " 
                 TO WS-HEADING-DISPLAY.
            IF WS-IMM-PR = "C"
             MOVE "* INVOICE ENTRY - COUNTER PRINTING **" 
                 TO WS-HEADING-DISPLAY.
            IF WS-IMM-PR = "H"
             MOVE "* INVOICE ENTRY - PDF LASER PRINTING *" 
                 TO WS-HEADING-DISPLAY.
            IF WS-IMM-PR = "S"
             MOVE "* INVOICE ENTRY - STORES PRINTING **"
                 TO WS-HEADING-DISPLAY.
            MOVE 320 TO POS.
            DISPLAY WS-HEADING-DISPLAY AT POS.
 
            MOVE " " TO WS-MESSAGE.
            MOVE "ACCOUNTNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"14"
             IF WS-IMM-PR = "C" OR = "S" OR = "H"
                MOVE "Press 'CODE-COPY' To Reprint This Invoice"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-010.
            IF F-EXIT-CH = X"94"
             IF WS-IMM-PR = "C" OR = "S" OR = "H"
                PERFORM PRINT-INVOICE
                GO TO GET-010.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            MOVE "  " TO Ws-Sold-By.
            MOVE "N"  TO WS-ZERODIS 
                         WS-MUST-PRINT
                         WS-LIMIT-EXCEP-WRITE
                         WS-PASSWORD-VALID.
            MOVE "Y"  TO WS-NEWORDER.
            MOVE WS-DATE    TO WS-INVOICEDATE.
            MOVE 7           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO F-NAMEFIELDRED.
      *********************************************
      * 'F5' KEY, TO FIND OLD ORDER AND DISPLAY   *
      *********************************************
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 NOT = "*"
                GO TO GET-010.
           IF F-EXIT-CH = X"19"
                MOVE " " TO WS-BO-REDUCED-MESSAGE
                PERFORM CLEAR-FIELDS
                MOVE "          "    TO ALPHA-RATE
                MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE    TO WS-INVOICE
                PERFORM READ-INVOICE-REGISTER.
           IF F-EXIT-CH = X"19"
            IF WS-NEWORDER = "N"
                PERFORM READ-STOCK-TRANSACTIONS
                PERFORM READ-DEBTORS
                PERFORM FIND-INFO
                PERFORM CHECK-FOR-BORDERS.
           IF F-EXIT-CH = X"19"
            IF WS-NEWORDER = "N"
             IF DR-NAME = "UNKNOWN"
                MOVE
           "THE ACCOUNT NUMBER IS INVALID, ADVISE YOUR SUPERVISOR."
                TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                GO TO GET-010.
           IF F-EXIT-CH = X"19"
            IF WS-NEWORDER = "N"
             IF DR-SUPPLY-Y-N = "N"
                MOVE "GOODS CANNOT BE SUPPLIED ON THIS ACCOUNT."
                TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                GO TO GET-010.
           IF F-EXIT-CH = X"19"
            IF WS-NEWORDER = "N"
             IF DR-SUPPLY-Y-N = "S"
                MOVE
               "THIS ACCOUNT HAS BEEN SUSPENDED, CHECK WITH A/C'S DEPT."
                TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                GO TO GET-010.
           IF F-EXIT-CH = X"19"
            IF WS-NEWORDER = "N"
                PERFORM READ-DEBTORS
                GO TO GET-150.

           IF F-EXIT-CH = X"19"
            IF WS-NEWORDER = "C"
                PERFORM ERROR1-020
                PERFORM ERROR-020
                PERFORM DISPLAY-FORM
                GO TO GET-010.
            IF F-NAMEFIELDRED = "STOCK  "
                CLOSE STOCK-MASTER
                CLOSE PARAMETER-FILE
                CALL WS-STOCK-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-STOCK-INQUIRY
                PERFORM OPEN-012
                PERFORM OPEN-014
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
            IF F-NAMEFIELDRED1 = "S"
                AND F-NAMEFIELDRED7 NOT = "TOCK"
                   DISPLAY " " AT 3079 WITH BELL
                   GO TO GET-010.
            IF F-NAMEFIELDRED1 = " "
                MOVE "0" TO F-NAMEFIELDRED1.
            IF F-NAMEFIELDRED1 = "0" OR = "1" OR = "2" OR = "3"
                   OR = "4" OR = "5" OR = "6" OR = "7" OR = "8"
                   OR = "9"
                NEXT SENTENCE
                ELSE 
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
            MOVE " " TO WS-NAMEANDADDRESS
                        WS-ACCNO-X
                        WS-BO-REDUCED-MESSAGE
                        WS-BOFOUND
                        WS-BINNO
                        WS-CONTACT
                        WS-AREA
                        WS-PULLBY
                        WS-PHONE.
            MOVE 0 TO WS-POSTCODE
                      DR-ACCOUNT-NUMBER
                      WS-ADDONFREIGHT WS-POSTADDON
                      WS-HANDADDON WS-MISCADDON WS-COPY.
            MOVE 1 TO SUB-20 SUB-25.
            PERFORM CLEAR-FIELDS.
            MOVE F-NAMEFIELD TO WS-ACCNO-X.
            MOVE WS-ACCNO-X TO DR-ACCOUNT-NUMBER
                               WS-ACCOUNT-NUMBER.
            IF DR-ACCOUNT-NUMBER = 0
                CLOSE DEBTOR-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM CLEAR-SCREEN
                OPEN I-O DEBTOR-MASTER
                PERFORM DISPLAY-FORM
                GO TO GET-010.

           PERFORM READ-DEBTORS.
           IF DR-ACCOUNT-NUMBER = 9999999 OR = 0300150
                             OR = 0300090 OR = 0300087
                             OR = 0300100 OR = 0300200
               MOVE " " TO DR-TELEPHONE
               GO TO GET-012.
      ***************************************************************
      * SECTION TO CHECK IF ACCOUNT IS ALREADY OVER THE CREDIT LIMIT*
      * THIS SECTION USED WHEN ENTERING A NEW INVOICE, NOT CALLING  *
      * UP A P/SLIP TO INVOICE.  FOR CHECKING ON P/SLIP CALLED UP   *
      * PRESSING <F5> SEE GET-150 SECTION FOR LIMIT CHECKING        *
      ***************************************************************
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
      * NEW SECTION ADDED TO ASK FOR PASSWORD IF THE ACCOUNT IS OVER *
      * THE LIMIT.  THIS WILL ONLY HAPPEN IF THE FLAG IS SET IN THE  *
      * PARAMETER FILE - INVQUES-ACC-OVER-LIMIT = "Y"                *
      ****************************************************************
           IF DR-BALANCE > DR-CREDIT-LIMIT
            IF WS-QUES-ACC-OVER-LIMIT = "Y"
             MOVE 
            "THIS ACCOUNT IS OVER THE CREDIT LIMIT, ENTER A PASSWORD."
             TO WS-MESSAGE
             PERFORM ERROR-000
               MOVE X"85" TO F-EXIT-CH
               PERFORM CHECK-PASSWORD
             IF WS-PASSWORD-VALID = "N"
             MOVE
            "NEW ORDERS CANNOT BE ENTERED AS THE ACCOUNT IS OVER THE"
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE
            "CREDIT LIMIT AND THE PASSWORD ENTERED IN INCORRECT."
             TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM CLEAR-SCREEN
                PERFORM ERROR1-020
                PERFORM ERROR-020
                PERFORM DISPLAY-FORM
                UNLOCK INCR-REGISTER
                GO TO GET-010.

           IF WS-PASSWORD-VALID = "Y"
            IF WS-LIMIT-EXCEP-WRITE = "N"
                MOVE "Y" TO WS-LIMIT-EXCEP-WRITE
                MOVE "ACCOUNT OVER LIMIT  " TO WS-DAILY-1ST
                MOVE WS-ACCOUNT-NUMBER      TO WS-DAILY-2ND
                MOVE "NEW INVOICE START TO" TO WS-DAILY-3RD
                MOVE "MAKE ON THE FLY.    " TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                PERFORM GET-USER-MAIL-NAME
                PERFORM GET-REPORT-Y2K-DATE
                MOVE "ABOVE CHANGE DONE BY" TO WS-DAILY-1ST
                MOVE WS-pbValue             TO WS-DAILY-2ND
                MOVE "DATE & TIME CHANGED:" TO WS-DAILY-3RD
                MOVE pbRet                  TO WS-REPORT-DATE-STRIP
                MOVE WS-STRIP2              TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
             

           IF DR-SUPPLY-Y-N = "N"
               MOVE "GOODS CANNOT BE SUPPLIED ON THIS ACCOUNT."
               TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM CLEAR-SCREEN
               PERFORM ERROR1-020
               PERFORM ERROR-020
               PERFORM DISPLAY-FORM
               UNLOCK INCR-REGISTER
               GO TO GET-010.
             
           IF DR-SUPPLY-Y-N = "S"
                MOVE
               "THIS ACCOUNT HAS BEEN SUSPENDED, CHECK WITH A/C'S DEPT."
                TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM CLEAR-SCREEN
                PERFORM ERROR1-020
                PERFORM ERROR-020
                PERFORM DISPLAY-FORM
                UNLOCK INCR-REGISTER
                GO TO GET-010.
            MOVE " " TO INCR-AREA.
       GET-012.
            MOVE "DEBTORNAME" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            IF DR-NAME = "UNKNOWN"
                GO TO GET-010.
            MOVE DR-DISCOUNT-CODE  TO WS-DISCOUNT-CODE
            MOVE DR-DELIVERY-CODE  TO WSDE-CODE.
            MOVE DR-SALES-ANALYSIS TO WSAN-CODE.
            MOVE "POSTADD1"        TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-ADD1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-ADD2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-ADD3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTCODE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-POSTCODE TO F-NAMEFIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-DELADD1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-DELADD2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-DELADD3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE WSAN-CODE TO SA-KEY.
            PERFORM READ-SALES-ANALYSIS.
            MOVE "SALESANALYSIS" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE WS-SALESANALYSIS TO F-NAMEFIELD.
            MOVE 14 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM READ-SBREP.
            MOVE "SOLDBY" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE Ws-Sold-By TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
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

            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DR-GSTNO TO F-NAMEFIELD WS-GSTNO.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERVIA" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DR-DELIVERY-CODE TO WS-DEL-SUB.
            MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA.
            MOVE WS-DELIVERVIA TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TERMOFSALE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DR-TERMS-CODE TO WS-TERM-SUB.
            MOVE WS-ST-TERM (WS-TERM-SUB) TO WS-TERMOFSALE.
            MOVE WS-TERMOFSALE TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICEDATE" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            MOVE WS-DATE       TO WS-INVOICEDATE SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE  TO F-NAMEFIELD.
            MOVE 10            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            GO TO GET-095.
       GET-020.
            MOVE "DEBTORNAME" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                PERFORM CANCEL-INVOICE.
            IF F-EXIT-CH = X"01"
                MOVE 0 TO DR-ACCOUNT-NUMBER
                          WS-ACCOUNT-NUMBER
                PERFORM CLEAR-FIELDS
                PERFORM DISPLAY-FORM
                MOVE " " TO WS-BINNO
                GO TO GET-010.
            MOVE 40          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-NAME.
       GET-030.
            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8  TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-020.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ADD1.
       GET-040.
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
            MOVE "                    " TO F-NAMEFIELD.
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
            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-060.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DELADD1.
            IF WS-DELADD1 = "  "
                GO TO GET-070.
       GET-080.
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
            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-080.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DELADD3.
            IF WS-NEWORDER = "N"
               GO TO GET-150.
       GET-095.
            IF WS-DELADD1 = "  "
                GO TO GET-070.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "PHONE"      TO F-FIELDNAME
            MOVE 5            TO F-CBFIELDNAME.
            IF WS-NEWORDER = "Y"
               MOVE DR-TELEPHONE TO WS-PHONE F-NAMEFIELD
            ELSE
               MOVE WS-PHONE  TO F-NAMEFIELD.
            MOVE 20           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-090.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-PHONE.
            IF WS-PHONE = "    "
               GO TO GET-095.
       GET-096.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "CONTACTNAME" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            IF WS-NEWORDER = "N"
               MOVE WS-CONTACT TO F-NAMEFIELD
               MOVE 20         TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.
            MOVE 20 TO F-CBFIELDLENGTH.
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
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "COMMENTLINE" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            
            MOVE 30 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE " " TO WS-POORDERNO.
            MOVE "POORDERNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"01"
      *          MOVE "Y" TO WS-NEWORDER
                GO TO GET-096.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-POORDERNO.
            PERFORM CHECK-REGISTER.
       GET-111.
            IF WS-NEWORDER = "N"
              MOVE 2910 TO POS
              DISPLAY "THIS ORDER HAS ALREADY BEEN ENTERED," AT POS
              MOVE 3010 TO POS
              DISPLAY "THE INTERNAL ORDER No. =" AT POS
              ADD 25 TO POS
              MOVE INCR-INVOICE TO F-EDNAMEFIELDNUM
              DISPLAY F-EDNAMEFIELDNUM AT POS
              PERFORM ERROR-010
              MOVE " " TO WS-MESSAGE
              MOVE 2910 TO POS
              DISPLAY WS-MESSAGE AT POS
              MOVE 3010 TO POS
              DISPLAY WS-MESSAGE AT POS
              PERFORM DISPLAY-FORM
              UNLOCK INCR-REGISTER
              GO TO GET-010.
           IF WS-NEWORDER = "P"
             MOVE "THE ORDER IS IN THE STORE READY FOR PULLING, DON'T"
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE "CREATE A 2nd SLIP BEFORE THE ORIGINAL IS RETURNED."
             TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
               UNLOCK INCR-REGISTER
               GO TO GET-010.
           IF WS-NEWORDER = "C"
               MOVE "THIS ORDER HAS BEEN INVOICED AND IS COMPLETE."
               TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE "        INVOICED ON:" TO WS-DAILY-1ST
               MOVE INCR-BO-INV-NO         TO F-EDNAMEFIELDNUM
               MOVE F-EDNAMEFIELDNUM       TO WS-DAILY-2ND
               MOVE " " TO                    WS-DAILY-3RD
                                              WS-DAILY-4TH
               MOVE WS-DAILY-MESSAGE       TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR-020
               UNLOCK INCR-REGISTER
               GO TO GET-010.
       GET-115.
            IF Ws-Sold-By NOT = "  "
               GO TO GET-117.
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
                GO TO GET-096.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO Ws-Sold-By.
            MOVE 2 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            IF Ws-Sold-By = "  "
                GO TO GET-115.
            GO TO GET-120.
       GET-117.
            MOVE WS-AREA   TO INCR-AREA.
            MOVE WS-PULLBY TO INCR-PULLBY.
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
               MOVE "GSTNO"  TO F-FIELDNAME
               MOVE 5
       TO F-CBFIELDNAME
               MOVE "EXPORT" TO F-NAMEFIELD WS-GSTNO
               MOVE 13       TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.
            MOVE "SALESANALYSIS" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE WS-SALESANALYSIS TO F-NAMEFIELD.
            MOVE 14 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            IF WS-SALESANALYSIS = "UNKNOWN"
                MOVE 2801 TO POS
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
                GO TO GET-110
              ELSE
                GO TO GET-090.
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
                MOVE "INVALID DELIVERY CODE!!!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-130.
            MOVE "DELIVERVIA" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-DELIVERVIA TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-140.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "TERMOFSALE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
      *DELETE KEY
            IF F-EXIT-CH = X"7F"
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
               GO TO GET-140.
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
                MOVE "INVALID TERM CODE!!!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-140.
            MOVE "TERMOFSALE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-TERMOFSALE TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-150.
            IF WS-SALESANALYSIS = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-125.
      ***************************************************************
      *SECTION TO CHECK IF ACCOUNT IS ALREADY OVER THE CREDIT LIMIT.*
      ***************************************************************
      * NEW SECTION ADDED TO ASK FOR PASSWORD IF THE ACCOUNT IS OVER*
      * THE LIMIT.  THIS WILL ONLY HAPPEN IF THE FLAG IS SET IN THE *
      * PARAMETER FILE - INVQUES-ACC-OVER-LIMIT = "Y"               *
      ***************************************************************
          IF WS-NEWORDER NOT = "Y"
           IF DR-BALANCE > DR-CREDIT-LIMIT
               COMPUTE WS-ORDERTOTAL = 
                  DR-BALANCE - DR-CREDIT-LIMIT
            IF WS-QUES-ACC-OVER-LIMIT = "Y"
             MOVE 
            "THE ACCOUNT IS OVER THE CREDIT CR-LIMIT BY R"
             TO WS-MESSAGE
             PERFORM ERROR-000
                MOVE WS-ORDERTOTAL TO P-PRICE
                MOVE 3059 TO POS
                DISPLAY P-PRICE AT POS
               MOVE X"85" TO F-EXIT-CH
               PERFORM CHECK-PASSWORD
               PERFORM ERROR1-020
               PERFORM ERROR-020
             IF WS-PASSWORD-VALID = "N"
             MOVE 
            "THIS ORDER CAN'T BE INVOICED AS THE ACCOUNT IS OVER THE"
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE
            "CREDIT LIMIT AND THE PASSWORD ENTERED IS INCORRECT."
             TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM CLEAR-SCREEN
                PERFORM ERROR1-020
                PERFORM ERROR-020
                PERFORM DISPLAY-FORM
                UNLOCK INCR-REGISTER
                GO TO GET-010.
      ****************************************************************
      *SECTION TO CHECK IF THE NEW ORDER TO BE INVOICED WILL PUSH THE*
      *ACCOUNT OVER THE CREDIT LIMIT.                                *
      ****************************************************************
          IF WS-NEWORDER NOT = "Y"
           IF WS-PASSWORD-VALID = "N"
            PERFORM CALCULATE-TOTALS
            PERFORM GET-245 THRU GET-270
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
                MOVE 3061 TO POS
                DISPLAY P-PRICE AT POS
               MOVE X"85" TO F-EXIT-CH
               PERFORM CHECK-PASSWORD
             PERFORM ERROR1-020
             PERFORM ERROR-020
             IF WS-PASSWORD-VALID = "N"
             MOVE
            "NEW ORDER CANNOT BE INVOICED AS THE ACCOUNT WILL BE OVER"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE
             "THE CREDIT LIMIT, AND THE PASSWORD ENTERED IS INCORRECT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM CLEAR-SCREEN
              PERFORM ERROR1-020
              PERFORM ERROR-020
              PERFORM DISPLAY-FORM
              UNLOCK INCR-REGISTER
                GO TO GET-010.

           IF WS-PASSWORD-VALID = "Y"
            IF WS-LIMIT-EXCEP-WRITE = "N"
                MOVE "Y" TO WS-LIMIT-EXCEP-WRITE
                MOVE "PSWD LIMIT ACCOUNT  " TO WS-DAILY-1ST
                MOVE INCR-ACCOUNT           TO WS-DAILY-2ND
                MOVE "TO INV P/SLIP NUMBER" TO WS-DAILY-3RD
                MOVE WS-ORDER               TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                PERFORM GET-USER-MAIL-NAME
                PERFORM GET-REPORT-Y2K-DATE
                MOVE "ABOVE CHANGE DONE BY" TO WS-DAILY-1ST
                MOVE WS-pbValue             TO WS-DAILY-2ND
                MOVE "DATE & TIME CHANGED:" TO WS-DAILY-3RD
                MOVE pbRet                  TO WS-REPORT-DATE-STRIP
                MOVE WS-STRIP2              TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
             
      *****************************
      *RE-ENTER THE DEL-AREA CODE *
      *****************************
            PERFORM GET-117.
               
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "BINNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
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
            IF F-EXIT-CH NOT = X"1D"
                MOVE 1 TO SUB-1 F-INDEX
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
             IF WS-NEWORDER = "Y"
                MOVE 0 TO WS-INVOICEDISCOUNT
                GO TO GET-180
             ELSE
                GO TO GET-180.
       GET-160.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "GSTNO" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            MOVE 13      TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-GSTNO.
            IF F-EXIT-CH = X"01"
                GO TO GET-150.
       GET-170.
            IF WS-ABOVE-BODY NOT = "1"
               MOVE " " TO WS-ABOVE-BODY.
            IF WS-GSTNO = "EXPORT"
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
            MOVE "INVOICEDATE" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-160.
            MOVE 10            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO GET-171.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
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
            MOVE "N" TO WS-ZERODIS.
               
           MOVE 3010 TO POS
           DISPLAY
           "IF YOU WISH TO ZERO THE DISCOUNT ENTER 0 AND PRESS <F8>."
              AT POS.
            MOVE "INVOICEDISCOUNT" TO F-FIELDNAME.
            MOVE 15 TO F-CBFIELDNAME.
            MOVE 0  TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 0  TO WS-INVOICEDISCOUNT.
            MOVE 5  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"1D"
               MOVE "Y" TO WS-ZERODIS
               PERFORM CHANGE-DISCOUNT-PER-LINE.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            ADD 5 TO F-CBFIRSTLINE.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-INVOICEDISCOUNT.
            IF WS-INVOICEDISCOUNT > 0
               PERFORM CHANGE-DISCOUNT-PER-LINE.
       GET-177.
            PERFORM SET-GST.
            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM SCROLL-PREVIOUS.
       GET-180.
            MOVE "N" TO WS-LINECHANGED.
            PERFORM FILL-BODY.
            IF WS-ABOVE-BODY = "1"
                GO TO GET-150.
       GET-185.
            PERFORM ERROR-020.
            MOVE 2710 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 2810 TO POS
            DISPLAY WS-MESSAGE AT POS.
      ****************************************************************
      *SECTION TO CHECK IF THE NEW ORDER TO BE INVOICED WILL PUSH THE*
      *ACCOUNT OVER THE CREDIT LIMIT.                                *
      ****************************************************************
          PERFORM CALCULATE-TOTALS.
          IF WS-NEWORDER = "Y"
           IF DR-BALANCE + WS-SUBTOTAL > DR-CREDIT-LIMIT
               COMPUTE WS-WORK-FIELD = 
                  DR-BALANCE + WS-SUBTOTAL +WS-TAXAMT 
                             - DR-CREDIT-LIMIT
            IF WS-QUES-ACC-OVER-LIMIT = "Y"
             MOVE 
            "NEW INVOICE WILL PUSH THE ACC OVER CR-LIMIT BY R"
             TO WS-MESSAGE
             PERFORM ERROR-000
                MOVE WS-ORDERTOTAL TO P-PRICE
                MOVE 3063 TO POS
                DISPLAY P-PRICE AT POS
               MOVE X"85" TO F-EXIT-CH
               PERFORM CHECK-PASSWORD
             PERFORM ERROR1-020
             PERFORM ERROR-020
             IF WS-PASSWORD-VALID = "N"
              MOVE
            "THIS ORDER CAN'T BE INVOICED AS THE ACCOUNT WILL BE OVER"
                TO WS-MESSAGE
             PERFORM ERROR1-000
              MOVE
            "THE CREDIT LIMIT, AND THE PASSWORD ENTERED IS INCORRECT."
                TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             PERFORM ERROR-020
             MOVE 1 TO F-INDEX
             GO TO GET-180.

           IF WS-PASSWORD-VALID = "Y"
            IF WS-LIMIT-EXCEP-WRITE = "N"
                MOVE "Y" TO WS-LIMIT-EXCEP-WRITE
                MOVE "ACCOUNT OVER LIMIT  " TO WS-DAILY-1ST
                MOVE INCR-ACCOUNT           TO WS-DAILY-2ND
                MOVE "ITEMS OVER VALUE OF:" TO WS-DAILY-3RD
                MOVE WS-WORK-FIELD          TO P-NETT
                MOVE P-NETT                 TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                PERFORM GET-USER-MAIL-NAME
                PERFORM GET-REPORT-Y2K-DATE
                MOVE "ABOVE CHANGE DONE BY" TO WS-DAILY-1ST
                MOVE WS-pbValue             TO WS-DAILY-2ND
                MOVE "DATE & TIME CHANGED:" TO WS-DAILY-3RD
                MOVE pbRet                  TO WS-REPORT-DATE-STRIP
                MOVE WS-STRIP2              TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
       GET-190.
            MOVE 1 TO F-INDEX.
            IF WS-NEWORDER = "Y"
               PERFORM CLEAR-BOTTOM-FIELDS
               MOVE 0 TO WS-ADDONFREIGHT
                         WS-POSTADDON
                         WS-HANDADDON
                         WS-MISCADDON.
            MOVE 1 TO F-INDEX.
            PERFORM GET-240.
            PERFORM GET-245.
            PERFORM GET-250.
            PERFORM GET-260.
            PERFORM GET-270.
            MOVE "COMMENTLINE" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
              IF WS-NEWORDER = "Y"
                MOVE 1 TO F-INDEX 
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
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                GO TO GET-180.
            IF F-EXIT-CH = X"07"
                GO TO GET-170.
            MOVE 30 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-COMMENTLINE.
      *F8-KEY.
            IF F-EXIT-CH = X"1D"
                GO TO GET-200.
            IF WS-IMM-PR = "C" OR = "S" OR = "H"
                GO TO GET-240.
       GET-200.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "ADDONFREIGHT"        TO F-FIELDNAME
            MOVE 12                    TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-190.
            MOVE 11           TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-ADDONFREIGHT F-EDNAMEFIELD99MIL
            MOVE 11           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.
       GET-210.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "POSTADDON"           TO F-FIELDNAME
            MOVE 9                     TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-200.
            MOVE 11             TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD   TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE  TO WS-POSTADDON F-EDNAMEFIELD99MIL
            MOVE 11             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.
       GET-220.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "HANDADDON"           TO F-FIELDNAME
            MOVE 9                     TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            IF F-EXIT-CH = X"01"
                GO TO GET-210.
            MOVE 11 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-HANDADDON F-EDNAMEFIELD99MIL
            MOVE 11           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.
       GET-230.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "MISC.ADDON"          TO F-FIELDNAME
            MOVE 10                    TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-220.
            MOVE 11 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-MISCADDON F-EDNAMEFIELD99MIL
            MOVE 11           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.
            
      *X"55" = POSITION 21 IN THE PSWD TABLE OF 92
      *
            IF F-EXIT-CH = X"1D"
               MOVE X"57" TO F-EXIT-CH
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
            MOVE WS-SUBTOTAL TO F-EDNAMEFIELD99MIL
            MOVE 11          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.
       GET-250.
            MOVE "ADDONAMT"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-ADDONAMT TO F-EDNAMEFIELD99MIL
            MOVE 11          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.
       GET-260.
            MOVE "TAXAMT"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE WS-TAXAMT TO F-EDNAMEFIELD99MIL
            MOVE 11        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.
       GET-270.
            IF F-EXIT-CH = X"1D"
               COMPUTE WS-ADDONAMT = WS-ADDONFREIGHT + WS-POSTADDON + 
                                     WS-HANDADDON + WS-MISCADDON
               PERFORM GET-250.
               
            COMPUTE WS-INVOICETOTAL = WS-SUBTOTAL +
                                      WS-TAXAMT + 
                                      WS-ADDONAMT.
            MOVE "INVOICETOTAL"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE WS-INVOICETOTAL TO F-EDNAMEFIELD99MIL
            MOVE 11              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.
       GET-900.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-190.
            ADD WS-INVOICETOTAL    TO WS-DIST-INVOICE
            ADD WS-ADDONAMT        TO WS-DIST-ADDON
            ADD WS-TAXAMT          TO WS-GST-AMT-TAXED
            ADD WS-TAXABLETOTAL    TO WS-GST-AMT-TAXABLE
            ADD WS-NONTAXABLETOTAL TO WS-GST-AMT-NONTAXABLE
            ADD WS-EXPORTTOTAL     TO WS-GST-AMT-EXPORT.
       GET-999.
            EXIT.
      *
       CHANGE-DISCOUNT-PER-LINE SECTION.
       CDPL-010.
            MOVE 1 TO SUB-1.
       CDPL-020.
            IF B-STOCKNUMBER (SUB-1) = " "
                GO TO CDPL-999.
            MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 201
               GO TO CDPL-020.
       CDPL-999.
            EXIT.
      *
       READ-SBREP SECTION.
       RSB-005.
           IF DR-SALESMAN = " " OR = "0"
              GO TO RSB-999.
       RSB-010.
           OPEN I-O SBREP-MASTER.
           IF WS-SBREP-ST1 NOT = 0 
              MOVE "SBREP FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SBREP-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
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
              GO TO RSB-040.
           MOVE SBREP-INT TO WS-SOLD-BY.
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
            MOVE " " TO WS-ABOVE-BODY.
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
            ADD 16 TO POS
            MOVE SUB-1 TO WS-BODY-LINE
            DISPLAY WS-BODY-LINE AT POS.
       FILL-010.
            IF WS-LASTPASSWORD = " "  
               MOVE WS-PASSWORDSAVED TO WS-LASTPASSWORD.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                PERFORM SCROLL-050.
            MOVE "                    " TO F-NAMEFIELD
            PERFORM RUNNING-TOTAL
            MOVE "STOCKNUMBER" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.

            IF F-EXIT-CH = X"0B" 
              IF B-STOCKNUMBER (SUB-1) = " "
                GO TO FILL-010.

      *<CODE-B> = GO TO BEGINNING
           IF F-EXIT-CH = X"E2" OR = X"C2" OR = X"62"
                MOVE 0 TO SUB-1
                PERFORM SCROLL-NEXT
                GO TO FILL-005.
      *<CODE-M> = GO TO MIDDLE
           IF F-EXIT-CH = X"ED" OR = X"CD" OR = X"6D"
             IF SUB-25 > 10
                MOVE SUB-25 TO SUB-1
                COMPUTE SUB-1 = SUB-1 / 2
                PERFORM SCROLL-NEXT
                GO TO FILL-005.
      *<CODE-E> = GO TO END
           IF F-EXIT-CH = X"E5" OR = X"C5" OR = X"65"
            IF SUB-25 > 14
                MOVE SUB-25 TO SUB-1
                SUBTRACT 3 FROM SUB-1
                PERFORM SCROLL-NEXT
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
                PERFORM FIND-010
                PERFORM CALC-POS-OF-CURSOR
                GO TO FILL-005.

            IF F-EXIT-CH = X"01" AND F-INDEX = 1
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM READ-FIELD-ALPHA
             IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999
             ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
               IF SP-1STCHAR NOT = "*" AND NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999.

            IF WS-NEWORDER = "N"
             IF F-EXIT-CH NOT = X"01" AND NOT = X"0B" AND NOT = X"11"
                      AND NOT = X"0C" AND NOT = X"05" AND NOT = X"09"
                      AND NOT = X"91"
                 GO TO FILL-010.

            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            PERFORM FILL-005.
            IF F-EXIT-CH = X"01"
             IF B-STOCKNUMBER (SUB-1) = "  "
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-005.

            IF F-EXIT-CH = X"01" AND F-INDEX > 1
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-010
              ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
               IF SP-1STCHAR NOT = "*" AND NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-010.

            IF F-EXIT-CH = X"0B" AND F-INDEX < 7
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                ADD 1 TO F-INDEX SUB-1
                GO TO FILL-005
              ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
               IF SP-1STCHAR NOT = "*" AND NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                ADD 1 TO F-INDEX SUB-1
                GO TO FILL-005.
            IF F-EXIT-CH = X"0B" AND F-INDEX = 7
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                PERFORM SCROLL-NEXT
                GO TO FILL-010
              ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
               IF SP-1STCHAR NOT = "*" AND NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM SCROLL-NEXT
                GO TO FILL-010.

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
      ****************
      * TAB CHARACTER*
      ****************
            IF F-EXIT-CH = X"09"
             IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                MOVE " " TO WS-ABOVE-BODY
                PERFORM ERROR-020
                PERFORM CHECK-SUB1-TOTAL
                GO TO FILL-999
             ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                MOVE " " TO WS-ABOVE-BODY
                PERFORM ERROR-020
                PERFORM CHECK-SUB1-TOTAL
                GO TO FILL-999.
      **********************************************************
      * 'ESC' = X"07"; 'CODE-CANCEL' = X"87" ; 'Alt-F10'=X"9F" *
      **********************************************************
            IF F-EXIT-CH = X"07"
                AND B-STOCKNUMBER (SUB-1) = " "
                GO TO FILL-010.
            IF F-EXIT-CH = X"07"
                MOVE "TO DELETE A LINE-ITEM PRESS 'ALT-F10'"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-010.
            IF F-EXIT-CH = X"87" OR = X"9F"
                MOVE SUB-1 TO SUB-7
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
      *
      * NEW SECTION TO DISALLOW RE-ENTRY OF EXISTING LINE
      *
           IF F-NAMEFIELD NOT = B-STOCKNUMBER (SUB-1)
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
            IF SP-1STCHAR NOT = "*"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                GO TO FILL-010.
      *
      *<CODE-NEXT-PAGE> TO READ NEXT STOCK ITEM.
            IF F-EXIT-CH = X"8C"
             IF SUB-1 NOT < SUB-25
                PERFORM READ-NEXT-STOCK-ITEM
             IF WS-STOCK-ST1 = 0
                PERFORM DISPLAY-LINE-ITEMS
                GO TO FILL-010
             ELSE
                GO TO FILL-010.
      *
      *<CODE-PREV-PAGE> TO READ PREVIOUS STOCK ITEM.
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
            IF WS-STOCK-ST1 NOT = 51
                MOVE ST-QTYONHAND    TO WS-QTYONHAND
                MOVE ST-QTYONRESERVE TO WS-QTYONRESERVE
                MOVE ST-QTYONORDER   TO WS-QTYONORDER
                MOVE ST-QTYONBORDER  TO WS-QTYONBORDER
            ELSE
                MOVE 0               TO WS-QTYONHAND
                                        WS-QTYONRESERVE
                                        WS-QTYONORDER
                                        WS-QTYONBORDER.
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
            
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME
            MOVE 16                 TO F-CBFIELDNAME
            MOVE ST-DESCRIPTION1    TO F-NAMEFIELD
                                       B-STOCKDESCRIPTION (SUB-1)
            MOVE ST-DESCRIPTION2    TO B-STOCKDESCRIPTION2 (SUB-1)
            MOVE 20                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA
            PERFORM SCROLL-050.

            MOVE "PERUNIT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE ST-UNITOFMEASURE TO F-NAMEFIELD
                                     B-UNIT (SUB-1).
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       FILL-0100.
            IF SP-1STCHAR = "/"
             OR ST-PRICE = 0
                 GO TO FILL-0110.
            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE ST-PRICE TO F-EDNAMEFIELDAMOUNT
                             B-STOCKPRICE (SUB-1).
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
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
                MOVE WS-INVOICEDISCOUNT TO F-EDNAMEFIELDAMOUNTDIS
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
            IF WS-GSTNO = "EXPORT"
                MOVE "N" TO F-NAMEFIELD
                            B-TAX (SUB-1)
            ELSE
                MOVE "Y" TO F-NAMEFIELD
                            B-TAX (SUB-1).
      *      MOVE 1 TO F-CBFIELDLENGTH.
      *      PERFORM WRITE-FIELD-ALPHA.
       FILL-020.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "ORDERQTY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
             IF WS-LINECHANGED = "N"
               MOVE 0 TO B-ORDERQTY (SUB-1)
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-010
             ELSE
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
            MOVE "              " TO F-NAMEFIELD.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF SIGN-FOUND = 1
                GO TO FILL-020.
            MOVE NUMERIC-RATE TO B-ORDERQTY (SUB-1).
            IF B-ORDERQTY (SUB-1) = 0
                MOVE 2801 TO POS
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-020.
            MOVE B-ORDERQTY (SUB-1) TO F-EDNAMEFIELDQTY.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-QTY.
            IF SP-1STCHAR NOT = "/"
             IF ST-QTYONHAND NOT < B-ORDERQTY (SUB-1)
                MOVE NUMERIC-RATE TO B-SHIPQTY (SUB-1)
             ELSE
               MOVE ST-QTYONHAND  TO B-SHIPQTY (SUB-1).
            IF SP-1STCHAR = "/"
               MOVE NUMERIC-RATE TO B-SHIPQTY (SUB-1).
       FILL-025.
            MOVE "SHIPQTY"         TO F-FIELDNAME
            MOVE 7                 TO F-CBFIELDNAME
            MOVE B-SHIPQTY (SUB-1) TO F-EDNAMEFIELDQTY
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY.
       FILL-035.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "SHIPQTY" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
             IF WS-LINECHANGED NOT = "N"
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-035.
            IF F-EXIT-CH = X"01"
                MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
             IF SP-1STCHAR NOT = "/"
                MOVE 0 TO B-SHIPQTY (SUB-1)
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO FILL-020
             ELSE
                GO TO FILL-020.
            MOVE 5 TO F-CBFIELDLENGTH.
            MOVE "              " TO F-NAMEFIELD.
            IF F-EXIT-CH = X"1D" OR = X"0A" OR = X"15"
               GO TO FILL-037.
            DISPLAY " " AT 3079 WITH BELL.
            GO TO FILL-035.
       FILL-037.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF SIGN-FOUND = 1
                GO TO FILL-035.
            MOVE NUMERIC-RATE TO B-SHIPQTY (SUB-1).
            IF B-ORDERQTY (SUB-1) NOT = B-SHIPQTY (SUB-1)
                MOVE "NO BACK-ORDERS TO BE ENTERED ON A NEW INVOICE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-035.
            MOVE B-SHIPQTY (SUB-1) TO F-EDNAMEFIELDQTY.
            MOVE 5 TO F-CBFIELDLENGTH.
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
                MOVE " "              TO WS-DAILY-MESSAGE
                PERFORM ERROR-MESSAGE
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
           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
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
                MOVE 0                 TO STPR-PRICE
                MOVE "N"               TO B-SPECIAL (SUB-1).
      *F8-KEY
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
            PERFORM FILL-005.
            DISPLAY "DESCRIPTION1" AT POS.

            MOVE "                        " TO F-NAMEFIELD.
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE B-STOCKDESCRIPTION (SUB-1) TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                PERFORM ERROR1-020
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
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1D"
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
       FILL-048.
      ***************************************************************
      *NEW SECTION TO CHECK PASSWORD BEFORE ALLOWING CHANGE OF PRICE*
      ***************************************************************
            IF SP-1STCHAR = "/"
               GO TO FILL-050.
            IF F-EXIT-CH NOT = X"1D"
               GO TO FILL-070.
            
            PERFORM CHECK-PASSWORD
            IF WS-PASSWORD-VALID = "N"
               GO TO FILL-070.
       FILL-050.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
              IF SP-1STCHAR = "/"
                GO TO FILL-045.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-050.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF SIGN-FOUND = 1
                GO TO FILL-050.
            MOVE NUMERIC-RATE TO B-STOCKPRICE (SUB-1).
            MOVE B-STOCKPRICE (SUB-1) TO F-EDNAMEFIELDAMOUNT.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
            
            IF WS-PRICESAVE = B-STOCKPRICE (SUB-1)
                MOVE WS-DISCOUNTSAVE   TO B-DISCOUNTPERITEM (SUB-1)
                                          F-EDNAMEFIELDAMOUNTDIS
                MOVE "DISCOUNTPERITEM" TO F-FIELDNAME
                MOVE 15                TO F-CBFIELDNAME
                MOVE 5                 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS.
                
      *      IF B-STOCKPRICE (SUB-1) = 0
      *       IF WS-COST-DISPLAY = "Y"
      *         GO TO FILL-060.
               
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR = "/"
             IF SUB-1 = SUB-25
                GO TO FILL-060.
            IF SP-1STCHAR = "/"
             IF SUB-1 NOT = SUB-25
              IF WS-COST-DISPLAY = "Y"
                GO TO FILL-060
              ELSE
                GO TO FILL-090.

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
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "STOCKCOST" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            MOVE 9           TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO FILL-050.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-060.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF SIGN-FOUND = 1
                GO TO FILL-060.
            MOVE NUMERIC-RATE        TO B-STOCKCOST (SUB-1)
            MOVE B-STOCKCOST (SUB-1) TO F-EDNAMEFIELDAMOUNT
            MOVE 9                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       FILL-070.
            IF SP-1STCHAR = "/"
               GO TO FILL-071.
            IF F-EXIT-CH NOT = X"1D"
               GO TO FILL-080.
            PERFORM CHECK-PASSWORD.
            IF WS-PASSWORD-VALID = "N"
               GO TO FILL-080.
       FILL-071.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME.
            MOVE 15                TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5                 TO F-CBFIELDLENGTH.
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
                GO TO FILL-050.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-070.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF SIGN-FOUND = 1
                GO TO FILL-070.
            MOVE NUMERIC-RATE TO B-DISCOUNTPERITEM (SUB-1)
                                 F-EDNAMEFIELDAMOUNTDIS.
            PERFORM WRITE-FIELD-AMOUNTDIS.
       FILL-080.
      *
      * THIS FIELD HAS BEEN REMOVED AS THE FORM CANNOT HANDLE AN
      * EXTRA 2 CHARS.
      *
            GO TO FILL-090.
            
      *      MOVE "TAX" TO F-FIELDNAME.
      *      MOVE 3 TO F-CBFIELDNAME.
      *      PERFORM USER-FILL-FIELD.
      *      MOVE 1 TO F-CBFIELDLENGTH.
      *      IF F-EXIT-CH = X"01"
      *       IF SP-1STCHAR = "/"
      *          GO TO FILL-070
      *       ELSE
      *          GO TO FILL-050.
      *      IF F-EXIT-CH NOT = X"0A"
      *         DISPLAY " " AT 3079 WITH BELL
      *         GO TO FILL-080.
      *      PERFORM READ-FIELD-ALPHA.
      *      MOVE F-NAMEFIELD TO B-TAX (SUB-1).
       FILL-090.
            MOVE 0 TO WS-PRICESAVE.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            ADD 1 TO SUB-1 F-INDEX.
            IF WS-LINECHANGED = "N"
               MOVE SUB-1 TO SUB-25.
            MOVE "N" TO WS-LINECHANGED.
            MOVE "Y" TO WS-MUST-PRINT.
            IF SUB-1 > 200             
                MOVE 200 TO SUB-1 SUB-25
                MOVE "200 LINES ARE UP, PRESS 'ESC' TO TAB."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-999.
            IF F-INDEX < 8
                GO TO FILL-005.
            SUBTRACT 1 FROM SUB-1. 
            PERFORM SCROLL-NEXT.
            GO TO FILL-010.
       FILL-999.
             EXIT.
      *
       DISPLAY-LINE-ITEMS SECTION.
       DLI-005.
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
       CHECK-SUB1-TOTAL SECTION.
       CHK-000.
            MOVE 1 TO SUB-1.
       CHK-010.
           IF SUB-1 < 200
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                ADD 1 TO SUB-1
                GO TO CHK-010.
           MOVE SUB-1 TO SUB-20.
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
            MOVE 8 TO F-CBFIELDLENGTH.
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
              MOVE " " TO WS-MESSAGE
              PERFORM ERROR1-020
              PERFORM ERROR-020
              SUBTRACT 1 FROM SUB-1
              IF F-INDEX > 1
                SUBTRACT 1 FROM F-INDEX
                PERFORM SCROLLING
              ELSE
                PERFORM SCROLLING.
       RUN-015.
           ADD 1 TO SUB-3.
           IF SUB-3 > 200
               GO TO RUN-020.
           GO TO RUN-010.
       RUN-020.
           COMPUTE WS-MARGIN = WS-PRICETOTAL - WS-COSTTOTAL.
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / WS-COSTTOTAL) * 100.

           MOVE "TOTALCOST"     TO F-FIELDNAME.
           MOVE 9               TO F-CBFIELDNAME.
           IF WS-COST-DISPLAY = "N"
              MOVE 0            TO F-EDNAMEFIELD99MIL
           ELSE
              MOVE WS-COSTTOTAL TO F-EDNAMEFIELD99MIL.
           MOVE 11              TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-99MIL.

           MOVE "TOTALPERCENT" TO F-FIELDNAME.
           MOVE 12             TO F-CBFIELDNAME.
           IF WS-COST-DISPLAY = "N"
              MOVE 0           TO F-EDNAMEFIELD99MIL
           ELSE
              MOVE WS-PERC     TO F-EDNAMEFIELD99MIL.
           MOVE 11             TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-99MIL.

           MOVE "SUBTOTAL"    TO F-FIELDNAME.
           MOVE 8             TO F-CBFIELDNAME.
           MOVE WS-WORKTOTAL2 TO F-EDNAMEFIELD99MIL.
           MOVE 11            TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-99MIL.
       RUN-999.
           EXIT.
      *
       CALCULATE-TOTALS SECTION.
       CT-000.
           MOVE 1 TO SUB-1.
           MOVE 0 TO WS-TAXABLETOTAL WS-NONTAXABLETOTAL
                  WS-WORKTOTAL WS-DISCOUNT WS-COSTTOTAL WS-TAXAMT
                  WS-PRICETOTAL WS-EXPORTTOTAL WS-DISCOUNTREG.
           MOVE 0 TO WS-DIST-INVOICE
                     WS-DIST-ADDON
                     WS-SUBTOTAL
                     WS-GST-AMT-TAXED
                     WS-GST-AMT-TAXABLE
                     WS-GST-AMT-NONTAXABLE
                     WS-GST-AMT-EXPORT. 
       CT-010.
           IF B-STOCKNUMBER (SUB-1) = " "
               GO TO CT-020.
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
           IF SUB-1 < 201
               GO TO CT-010.
       CT-020.
           COMPUTE WS-SUBTOTAL = WS-TAXABLETOTAL + 
                                 WS-NONTAXABLETOTAL + 
                                 WS-EXPORTTOTAL.
      *     COMPUTE WS-TAXAMT ROUNDED = WS-TAXABLETOTAL * 
      *                                 WS-GST-PERCENT / 100.
           COMPUTE WS-ADDONAMT = WS-ADDONFREIGHT + WS-POSTADDON + 
                                 WS-HANDADDON + WS-MISCADDON.
           IF WS-GSTNO NOT = "EXPORT"
             COMPUTE WS-TAXAMT ROUNDED =
              (WS-TAXABLETOTAL + WS-ADDONAMT) * WS-GST-PERCENT / 100.
              
           MOVE WS-TAXAMT TO WS-TAXAMT-SAVE.
       CT-999.
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
            IF WS-INVOICEDISCOUNT > 0
               MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1).
            IF WS-INVOICEDISCOUNT = 0
             IF F-EXIT-CH = X"1D"
               MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 201
               GO TO SG-010.
       SG-999.
            EXIT.
      *
       CHECK-FOR-BORDERS SECTION.
       CFBO-010.
           MOVE 0   TO SUB-1.
           MOVE "N" TO WS-BORDERS-FOUND.
       CFBO-015.
           ADD 1 TO SUB-1.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
                 GO TO CFBO-020.
           IF B-STOCKNUMBER (SUB-1) = SPACES
              GO TO CFBO-950.
           IF B-COMPLETE (SUB-1) = "R"
               GO TO CFBO-020.
           IF B-ORDERQTY (SUB-1) NOT = 
               B-SHIPQTY (SUB-1) + B-SHIPPEDQTY (SUB-1)
               MOVE "Y" TO WS-BORDERS-FOUND
               GO TO CFBO-900.
       CFBO-020.
           IF SUB-1 < 200
              GO TO CFBO-015.
              
           GO TO CFBO-950.
       CFBO-900.
           IF INCR-PART-ORDERS = "Y"
               GO TO CFBO-950.
           MOVE "THIS ORDER CANNOT BE INVOICED AS THERE ARE B/O'S"
           TO WS-MESSAGE
           PERFORM ERROR1-000
           MOVE "AND THIS ORDER IS FLAGGED FOR NO PART DELIVERIES."
           TO WS-MESSAGE
           PERFORM ERROR-MESSAGE
           MOVE "C" TO WS-NEWORDER.
       CFBO-950.
           MOVE X"99" TO F-EXIT-CH
           PERFORM CHECK-FOR-NON-STOCK.
           MOVE X"19" TO F-EXIT-CH.
       CFBO-999.
           EXIT.
      *
       CHECK-FOR-NON-STOCK SECTION.
       CFNS-010.
           MOVE 0   TO SUB-1.
           MOVE "N" TO WS-NON-STOCK-FOUND.
       CFNS-015.
           ADD 1 TO SUB-1.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR NOT = "/"
                 GO TO CFNS-020.
           IF B-STOCKNUMBER (SUB-1) = "    "
              GO TO CFNS-999.
           IF SP-1STCHAR = "/"
               MOVE "Y" TO WS-NON-STOCK-FOUND
               GO TO CFNS-900.
       CFNS-020.
           IF SUB-1 < 200
              GO TO CFNS-015.
              
           GO TO CFNS-999.
       CFNS-900.
           PERFORM CHECK-PASSWORD.
           IF WS-PASSWORD-VALID = "Y"
              GO TO CFNS-999.
              
           MOVE
            "THIS ORDER CANNOT BE INVOICED AS THERE ARE NON-STOCK ITEMS"
           TO WS-MESSAGE
           PERFORM ERROR1-000
           MOVE "PLEASE CREATE A STOCK ITEM AND CHANGE THE ORDER."
           TO WS-MESSAGE
           PERFORM ERROR-MESSAGE
           MOVE "C" TO WS-NEWORDER.
       CFNS-999.
           EXIT.
      *
       WRITE-INCR-REGISTER SECTION.
       WRIC-050.
            MOVE WS-INVOICE        TO INCR-INVOICE
            MOVE 1                 TO INCR-TRANS
            MOVE WS-ACCOUNT-NUMBER TO INCR-ACCOUNT
            MOVE WS-POORDERNO      TO INCR-PORDER
            MOVE WS-GSTNO          TO INCR-GSTNO
            MOVE WS-DATE           TO INCR-DATE
            MOVE WSAN-CODE         TO INCR-SALES
            MOVE WS-INVOICETOTAL   TO INCR-INVCRED-AMT
            MOVE WS-TAXAMT         TO INCR-TAX
            MOVE WS-ADDONAMT       TO INCR-ADDONS
            MOVE WS-DISCOUNTREG    TO INCR-DISCOUNT
            MOVE WS-COSTTOTAL      TO INCR-INVCRED-COST
            MOVE Ws-Sold-By        TO INCR-SB-TYPE
            MOVE WS-DRTRANS-NO     TO INCR-DRTRANS-NO
      *      MOVE 1                 TO INCR-STTRANS-NO.
            IF WS-IMM-PR = "B"
               MOVE "N"            TO INCR-PRINTED
            ELSE
               MOVE "Y"            TO INCR-PRINTED.
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
            MOVE WS-DELIVERVIA     TO INCR-DELIVERY
            MOVE WS-BINNO          TO INCR-BIN
            MOVE WS-COMMENTLINE    TO INCR-COMMENT.
            IF WS-NEWORDER = "N"
               MOVE WS-ORDER       TO INCR-BO-INV-NO
               MOVE WS-ORDERDATE   TO INCR-BO-DATE
            ELSE
               MOVE INCR-INVOICE   TO INCR-BO-INV-NO
               MOVE WS-DATE        TO INCR-BO-DATE.
            MOVE WS-POSTADDON      TO INCR-ADDPOST
            MOVE WS-ADDONFREIGHT   TO INCR-ADDFREIGHT
            MOVE WS-HANDADDON      TO INCR-ADDLABOUR
            MOVE WS-MISCADDON      TO INCR-ADDMISC
            MOVE SUB-20            TO INCR-LINENO.
       WRIC-060.
            WRITE INCR-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 NOT = 0
                MOVE "INV-REG BUSY ON INVOICE WRITE, 'ESC' TO RETRY"
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO WRIC-060.
       WRIC-999.
              EXIT.
      *
       REWRITE-ORDER-TRANS SECTION.
       ROT-010.
            PERFORM CHECK-ORDER-COMPLETE.
            MOVE 4                 TO INCR-TRANS.
            IF WS-NEWORDER = "N"
                GO TO ROT-040.
       ROT-015.
            MOVE WS-INVOICE        TO INCR-INVOICE
            MOVE WS-ACCOUNT-NUMBER TO INCR-ACCOUNT
            MOVE WS-POORDERNO      TO INCR-PORDER
            MOVE WS-GSTNO          TO INCR-GSTNO
            MOVE WS-DATE           TO INCR-DATE
            MOVE WSAN-CODE         TO INCR-SALES
            MOVE WS-ORDERTOTAL     TO INCR-INVCRED-AMT
            MOVE WS-ORDTAXAMT      TO INCR-TAX
            MOVE WS-ADDONAMT       TO INCR-ADDONS
            MOVE WS-ORDDISCOUNTREG TO INCR-DISCOUNT
            MOVE WS-ORDCOSTTOTAL   TO INCR-INVCRED-COST
            MOVE Ws-Sold-By        TO INCR-SB-TYPE
            MOVE WS-DRTRANS-NO     TO INCR-DRTRANS-NO
            MOVE "Y"               TO INCR-PRINTED
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
            MOVE WS-DATE           TO INCR-PULL-DATE
            MOVE WS-TIME           TO INCR-PULL-TIME
            MOVE WS-DELIVERVIA     TO INCR-DELIVERY
            MOVE "      "          TO INCR-BIN
            MOVE WS-COMMENTLINE    TO INCR-COMMENT
            MOVE WS-INVOICE        TO INCR-BO-INV-NO
            MOVE WS-DATE           TO INCR-BO-DATE
            MOVE WS-POSTADDON      TO INCR-ADDPOST
            MOVE WS-ADDONFREIGHT   TO INCR-ADDFREIGHT
            MOVE WS-HANDADDON      TO INCR-ADDLABOUR
            MOVE WS-MISCADDON      TO INCR-ADDMISC
            MOVE SUB-20            TO INCR-LINENO
            GO TO ROT-060.
       ROT-040.
            MOVE "      "          TO INCR-BIN.
            MOVE WS-INVOICE        TO INCR-BO-INV-NO.
            MOVE WS-DATE           TO INCR-BO-DATE.
            IF WS-BOFOUND = "N"
               MOVE "L"            TO INCR-PRINTED
            ELSE
               MOVE "N"            TO INCR-PRINTED.
            IF INCR-PRINTED = "N"
               MOVE 0 TO INCR-PULL-DATE
                         INCR-PULL-TIME.
       ROT-060.
            IF WS-NEWORDER = "Y"
              WRITE INCR-REC
                  INVALID KEY NEXT SENTENCE
            ELSE
              REWRITE INCR-REC
                  INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 NOT = 0
              IF WS-NEWORDER = "N"
               MOVE "ORDER-REG BUSY - ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO ROT-060.
       ROT-999.
              EXIT.
      *
       CHECK-REGISTER SECTION.
       CRS-050.
           MOVE "Y" TO WS-NEWORDER.
           MOVE 0 TO WS-INCR-ST1.
           MOVE WS-ACCOUNT-NUMBER TO INCR-ACCOUNT.
           MOVE WS-POORDERNO      TO INCR-PORDER.
           START INCR-REGISTER KEY NOT < INCR-ALT-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               GO TO CRS-999.
       CRS-060.
           READ INCR-REGISTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
               MOVE 0 TO WS-INCR-ST1
               GO TO CRS-999.
           IF WS-INCR-ST1 = 91
               CLOSE INCR-REGISTER
               PERFORM OPEN-016
               GO TO CRS-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "CHECKING REGISTER BUSY CRS-060, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO CRS-060.
           IF INCR-ACCOUNT = WS-ACCOUNT-NUMBER
            IF INCR-PORDER NOT = WS-POORDERNO
               GO TO CRS-900.
           IF INCR-ACCOUNT NOT = WS-ACCOUNT-NUMBER
            IF INCR-PORDER = WS-POORDERNO
               GO TO CRS-900.
           IF INCR-ACCOUNT NOT = WS-ACCOUNT-NUMBER
            IF INCR-PORDER NOT = WS-POORDERNO
               GO TO CRS-900.


           IF INCR-PRINTED NOT = "Y" AND NOT = "L"
            IF INCR-PRINTED = "P"
               MOVE "P" TO WS-NEWORDER
               GO TO CRS-999
            ELSE
               MOVE "N" TO WS-NEWORDER
               MOVE INCR-INVOICE TO WS-INVOICE
               GO TO CRS-999.
           IF INCR-PRINTED = "Y" OR = "L"
               MOVE "C" TO WS-NEWORDER
               MOVE INCR-INVOICE TO WS-INVOICE
               GO TO CRS-999.
       CRS-900.
           UNLOCK INCR-REGISTER.
       CRS-999.
           EXIT.
      *
       READ-INVOICE-REGISTER SECTION.
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
               MOVE "INV/CR. REGISTER BUSY, RIR-005, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-005.
           IF INCR-PRINTED = "Y" OR = "L"
               MOVE "THIS ORDER HAS BEEN INVOICED AND IS COMPLETE."
               TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE "        INVOICED ON:" TO WS-DAILY-1ST
               MOVE INCR-BO-INV-NO         TO F-EDNAMEFIELDNUM
               MOVE F-EDNAMEFIELDNUM       TO WS-DAILY-2ND
               MOVE " "                    TO WS-DAILY-3RD WS-DAILY-4TH
               MOVE WS-DAILY-MESSAGE       TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR-020
               MOVE "C" TO WS-NEWORDER
               GO TO RIR-999.
           IF INCR-PULLBY = "  "
               MOVE
           "PLEASE CONFIRM PULLING THE ORDER BEFORE INVOICING IT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "C" TO WS-NEWORDER
               GO TO RIR-999.
       RIR-010.
           MOVE INCR-INVOICE        TO WS-ORDER
           MOVE INCR-ACCOUNT        TO WS-ACCOUNT-NUMBER
           MOVE INCR-GSTNO          TO WS-GSTNO
           MOVE INCR-DATE           TO WS-ORDERDATE
           MOVE INCR-SALES          TO WSAN-CODE
           MOVE INCR-INVCRED-AMT    TO WS-INVOICETOTAL
           MOVE INCR-TAX            TO WS-TAXAMT
           MOVE INCR-ADDONS         TO WS-ADDONAMT
           MOVE INCR-SB-TYPE        TO Ws-Sold-By
           MOVE INCR-PULLBY         TO Ws-PULLBY
           MOVE INCR-DRTRANS-NO     TO WS-DRTRANS-NO.
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
           MOVE INCR-AREA       TO WS-AREA
           MOVE INCR-DELIVERY   TO WS-DELIVERVIA
           MOVE INCR-BIN        TO WS-BINNO
           MOVE INCR-COMMENT    TO WS-COMMENTLINE
           MOVE INCR-ADDPOST    TO WS-POSTADDON
           MOVE INCR-ADDFREIGHT TO WS-ADDONFREIGHT
           MOVE INCR-ADDLABOUR  TO WS-HANDADDON
           MOVE INCR-ADDMISC    TO WS-MISCADDON
           MOVE INCR-LINENO     TO SUB-20 SUB-25.

           MOVE "N" TO WS-NEWORDER.
       RIR-999.
           EXIT.
      *
       CHECK-ORDER-COMPLETE SECTION.
       OCS-005.
           MOVE 1 TO SUB-1.
           MOVE "N" TO WS-BOFOUND.
       OCS-010.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
              GO TO OCS-020.
           IF B-COMPLETE (SUB-1) = "R"
              GO TO OCS-020.
           IF B-ORDERQTY (SUB-1) -
                (B-SHIPQTY (SUB-1) + B-SHIPPEDQTY (SUB-1)) = 0
              GO TO OCS-020
           ELSE
              MOVE "Y" TO WS-BOFOUND
              GO TO OCS-999.
       OCS-020.
           ADD 1 TO SUB-1.
           IF SUB-1 > 200
               MOVE 200 TO SUB-1
               GO TO OCS-999.
           IF B-STOCKNUMBER (SUB-1) = "   "
               GO TO OCS-999.
           GO TO OCS-010.
       OCS-999.
           EXIT.
      *
       READ-STOCK-TRANSACTIONS SECTION.
       RSTT-000.
           MOVE 1          TO SUB-1.
           MOVE WS-INVOICE TO STTR-REFERENCE1
           MOVE 4          TO STTR-TYPE
           MOVE 0          TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE 0 TO STTR-TYPE
              GO TO RSTT-999.
        RSTT-010.
           READ STOCK-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              MOVE 0 TO STTR-TYPE
              GO TO RSTT-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS BUSY ON READ AT RSTT-010, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
             IF WS-STTRANS-ST1 = 91
              GO TO RSTT-999
             ELSE
              GO TO RSTT-010.
           IF STTR-REFERENCE1 NOT = WS-INVOICE
              GO TO RSTT-999.
           IF STTR-TYPE NOT = 4
              GO TO RSTT-010.
           IF STTR-COMPLETE = "Y" OR = "L"
              GO TO RSTT-010.
           MOVE STTR-TRANSACTION-NUMBER TO B-STTRANS (SUB-1).
           MOVE STTR-STOCK-NUMBER       TO B-STOCKNUMBER (SUB-1)
                                           SPLIT-STOCK.
           MOVE STTR-COMPLETE     TO B-COMPLETE (SUB-1).
           IF SP-1STCHAR = "*"
               GO TO RSTT-020.
           MOVE STTR-SHIPQTY      TO B-SHIPQTY (SUB-1).
           MOVE STTR-ORDERQTY     TO B-ORDERQTY (SUB-1).
           MOVE STTR-SHIPPEDQTY   TO B-SHIPPEDQTY (SUB-1).
           MOVE STTR-DESC1        TO B-STOCKDESCRIPTION (SUB-1).
           MOVE STTR-DESC2        TO B-STOCKDESCRIPTION2 (SUB-1).
           MOVE STTR-PRICE        TO B-STOCKPRICE (SUB-1).
           MOVE STTR-COST-VALUE   TO B-STOCKCOST (SUB-1).
           MOVE STTR-ITEMDISC     TO B-DISCOUNTPERITEM (SUB-1).
           MOVE STTR-TAX          TO B-TAX (SUB-1).
           MOVE STTR-UNIT         TO B-UNIT (SUB-1).
           MOVE STTR-SALES-VALUE  TO B-NETT (SUB-1).
           COMPUTE WS-COSTTOTAL = WS-COSTTOTAL +
               (B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1)).
           GO TO RSTT-050.
       RSTT-020.
           MOVE COM-ORDERQTY      TO C-ORDER (SUB-1).
           MOVE COM-SHIPQTY       TO C-SHIP (SUB-1).
           MOVE COM-DESC          TO C-DESC (SUB-1).
           MOVE COM-UNIT          TO C-UNIT (SUB-1).
           MOVE COM-PRICE         TO C-PRICE (SUB-1).
           MOVE COM-COST          TO C-COST (SUB-1).
           MOVE COM-DISC          TO C-DISC (SUB-1).
       RSTT-050.
           IF SP-1STCHAR = "*" OR = "/"
              GO TO RSTT-055.
           PERFORM CHECK-FOR-SPECIALS.
       RSTT-055.
           ADD 1 TO SUB-1.
           MOVE SUB-1 TO SUB-25.
           IF SUB-1 > 200
              MOVE 200 TO SUB-1 SUB-25
              GO TO RSTT-999.
           GO TO RSTT-010.
       RSTT-999.
           EXIT.
      *
       REWRITE-STOCK-TRANSACTIONS SECTION.
       RWST-000.
            MOVE 1        TO SUB-1.
            MOVE WS-ORDER TO STTR-REFERENCE1.
            MOVE 4        TO STTR-TYPE.
            MOVE 1        TO STTR-TRANSACTION-NUMBER.
            START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
       RWST-001.
            IF B-STOCKNUMBER (SUB-1) = " "
               GO TO RWST-999.
            MOVE B-STTRANS (SUB-1) TO STTR-TRANSACTION-NUMBER.
       RWST-005.
            READ STOCK-TRANS-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS BUSY ON READ - RWST-005, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RWST-005.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
                                          STTR-STOCK-NUMBER.
            MOVE WS-ACCOUNT-NUMBER     TO STTR-ACCOUNT-NUMBER.
      ******************************************************************
      * DATE REWRITE REMOVED AS THIS GIVES THE DATE OF INVOICE AND NOT *
      *  THE DATE OF THE ORDER BEING ENTERED WHEN DOING B-ORDER INQUIRY*
      *  NOW DATE REFLECTED WILL BE DATE OF ORDER PUT ON SYSTEM.       *
      *      MOVE WS-INVOICEDATE        TO STTR-DATE.                  *
      ******************************************************************
            MOVE WS-INVOICE            TO STTR-INV-NO.
            IF SP-1STCHAR = "*"
                GO TO RWST-015.
            IF B-ORDERQTY (SUB-1) -
                 (B-SHIPQTY (SUB-1) + B-SHIPPEDQTY (SUB-1)) = 0
               MOVE "L"                TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE
            ELSE
               MOVE "N"                TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE.
           IF B-COMPLETE (SUB-1) = "R"
               MOVE "R"                TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE.
       RWST-010.
            MOVE B-ORDERQTY (SUB-1)          TO STTR-ORDERQTY.
            ADD B-SHIPQTY (SUB-1)            TO STTR-SHIPPEDQTY.
            MOVE 0                           TO STTR-SHIPQTY.
            MOVE B-NETT (SUB-1)              TO STTR-SALES-VALUE.
            MOVE B-STOCKPRICE (SUB-1)        TO STTR-PRICE.
            MOVE B-STOCKDESCRIPTION (SUB-1)  TO STTR-DESC1.
            MOVE B-STOCKDESCRIPTION2 (SUB-1) TO STTR-DESC2.
            MOVE B-TAX (SUB-1)               TO STTR-TAX.
            MOVE B-UNIT (SUB-1)              TO STTR-UNIT.
            MOVE B-DISCOUNTPERITEM (SUB-1)   TO STTR-ITEMDISC.
            MOVE B-STOCKCOST (SUB-1)         TO STTR-COST-VALUE.
            GO TO RWST-018.
       RWST-015.
            IF WS-BOFOUND = "Y"
               MOVE "N"          TO STTR-COMPLETE
                                    STTR-ST-COMPLETE
                                    STTR-AC-COMPLETE
            ELSE
               MOVE "L"          TO STTR-COMPLETE
                                    STTR-ST-COMPLETE
                                    STTR-AC-COMPLETE.
            MOVE C-DESC (SUB-1)  TO COM-DESC.
            MOVE C-UNIT (SUB-1)  TO COM-UNIT.
            MOVE C-ORDER (SUB-1) TO COM-ORDERQTY.
            MOVE C-SHIP (SUB-1)  TO COM-SHIPQTY.
            MOVE C-PRICE (SUB-1) TO COM-PRICE.
            MOVE C-COST (SUB-1)  TO COM-COST.
            MOVE C-DISC (SUB-1)  TO COM-DISC.
            MOVE " "             TO COM-FILLER.
       RWST-018.
            REWRITE STOCK-TRANS-REC
                INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS BUSY ON REWRITE RWST-018, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RWST-018.
       RWST-020.
            ADD 1 TO SUB-1.
            IF SUB-1 < 201
                GO TO RWST-001.
       RWST-999.
            EXIT.
      *
       WRITE-STOCK-TRANSACTIONS SECTION.
       WST-00000.
            MOVE 1 TO SUB-1.
            MOVE 0 TO WS-STTRANS-NO.
            MOVE WS-INVOICE TO STTR-REFERENCE1.
            MOVE 1          TO STTR-TYPE.
            START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
       WST-000.
            IF B-STOCKNUMBER (SUB-1) = " "
               GO TO WST-999.
             ADD 1              TO WS-STTRANS-NO.
             MOVE WS-STTRANS-NO TO STTR-TRANSACTION-NUMBER.
       WST-005.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
                                          STTR-STOCK-NUMBER.
            MOVE WS-ACCOUNT-NUMBER     TO STTR-ACCOUNT-NUMBER.
            MOVE WS-INVOICE            TO STTR-INV-NO.
            MOVE WS-INVOICEDATE        TO STTR-DATE
                                          STTR-AC-DATE
                                          STTR-ST-DATE.
            MOVE "Y"                   TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE.
            IF SP-1STCHAR = "*"
                GO TO WST-015.
       WST-010.
            MOVE B-ORDERQTY (SUB-1)          TO STTR-ORDERQTY.
            MOVE B-SHIPQTY (SUB-1)           TO STTR-SHIPQTY.
            MOVE B-SHIPPEDQTY (SUB-1)        TO STTR-SHIPPEDQTY.
            MOVE B-NETT (SUB-1)              TO STTR-SALES-VALUE.
            MOVE B-STOCKCOST (SUB-1)         TO STTR-COST-VALUE.
            MOVE B-STOCKPRICE (SUB-1)        TO STTR-PRICE.
            MOVE B-STOCKDESCRIPTION (SUB-1)  TO STTR-DESC1.
            MOVE B-STOCKDESCRIPTION2 (SUB-1) TO STTR-DESC2.
            MOVE B-DISCOUNTPERITEM (SUB-1)   TO STTR-ITEMDISC.
            MOVE B-TAX (SUB-1)               TO STTR-TAX.
            MOVE B-UNIT (SUB-1)              TO STTR-UNIT.
            GO TO WST-018.
       WST-015.
            MOVE C-ORDER (SUB-1) TO COM-ORDERQTY.
            MOVE C-SHIP (SUB-1)  TO COM-SHIPQTY.
            MOVE C-DESC (SUB-1)  TO COM-DESC.
            MOVE C-UNIT (SUB-1)  TO COM-UNIT.
            MOVE C-PRICE (SUB-1) TO COM-PRICE.
            MOVE C-COST (SUB-1)  TO COM-COST.
            MOVE C-DISC (SUB-1)  TO COM-DISC.
            MOVE " "             TO COM-FILLER.
       WST-018.
            WRITE STOCK-TRANS-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS BUSY ON WRITE WST-018, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO WST-000.
       WST-020.
            ADD 1 TO SUB-1.
            IF SUB-1 < 201
                GO TO WST-000.
       WST-999.
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
           START DEBTOR-MASTER KEY NOT < DR-KEY.
       RD-010.
           READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE " " TO DR-NAME DR-ADDRESS1 DR-ADDRESS2
                        DR-ADDRESS3 DR-DEL-ADDRESS1
                        DR-DEL-ADDRESS2 DR-DEL-ADDRESS3
               MOVE "UNKNOWN" TO DR-NAME
               MOVE 0 TO DR-POST-CODE
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
           IF WS-NEWORDER = "N"
                GO TO RD-999.
       RD-050.
           MOVE DR-NAME         TO WS-NAME.
           MOVE DR-ADDRESS1     TO WS-ADD1.
           MOVE DR-ADDRESS2     TO WS-ADD2.
           MOVE DR-ADDRESS3     TO WS-ADD3.
           MOVE DR-DEL-ADDRESS1 TO WS-DELADD1.
           MOVE DR-DEL-ADDRESS2 TO WS-DELADD2.
           MOVE DR-DEL-ADDRESS3 TO WS-DELADD3.
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
               MOVE "PARAMETER TERM BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER DELV BUSY ON READ,'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
       READ-COMM-FILE SECTION.
       RCOMM-000.
            MOVE 1 TO PA-RECORD.
            MOVE 4 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RCOMM-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE " " TO WS-SPEC-COMMENT
               GO TO RCOMM-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER COMM FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RCOMM-010.
       RCOMM-900.
            MOVE COMM-DESC TO WS-SPEC-COMMENT.
       RCOMM-999.
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
               MOVE "N" TO WS-QUES-PRINT-PULLERS
                           WS-QUES-CASH-SALES
               GO TO RINVQUES-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER RINVQUES BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RINVQUES-010.
       RINVQUES-900.
            MOVE INVQUES-PRT-PULLERS    TO WS-QUES-PRINT-PULLERS
            MOVE INVQUES-CASH-SALES     TO WS-QUES-CASH-SALES.
            MOVE INVQUES-ACC-OVER-LIMIT TO WS-QUES-ACC-OVER-LIMIT.
       RINVQUES-999.
            EXIT.
      *
       READ-SALES-ANALYSIS SECTION.
       RSALES-010.
            MOVE WSAN-CODE TO SA-KEY.
       RSALES-500.
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
       RSALES-999.
            EXIT.
      *
       UPDATE-SALES SECTION.
       UPSA-500.
            MOVE WSAN-CODE TO SA-KEY.
            READ SALES-ANALYSIS WITH LOCK
                INVALID KEY NEXT SENTENCE.
            IF WS-SALES-ST1 = 23 OR 35 OR 49
                GO TO UPSA-999.
            IF WS-SALES-ST1 NOT = 0
               MOVE "SALES BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO UPSA-500.
           ADD WS-COSTTOTAL TO SA-COST-WEEK
                               SA-COST-PTD
                               SA-COST-YTD.
           ADD WS-PRICETOTAL TO SA-SALES-WEEK
                                SA-SALES-PTD
                                SA-SALES-YTD.
       UPSA-600.
           REWRITE SALES-ANALYSIS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-SALES-ST1 = 23 OR 35 OR 49
              GO TO UPSA-999.
           IF WS-SALES-ST1 NOT = 0
               MOVE "SALES BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO UPSA-600.
       UPSA-999.
           EXIT.
      *
       TAX-ONLY SECTION.
       TO-000.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "TAXAMT"     TO F-FIELDNAME.
            MOVE 6            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-TAXAMT
                                 F-EDNAMEFIELDAMOUNT.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
            
      *      IF WS-GSTNO = "EXPORT"
      *       IF WS-TAXAMT > 0
      *         MOVE
      *     "AN EXPORT SALE CANNOT HAVE VAT ALLOCATED TO IT, RE-ENTER."
      *           TO WS-MESSAGE
      *           PERFORM ERROR-MESSAGE
      *           GO TO TO-000.
      *      IF WS-GSTNO NOT = "EXPORT"
      *       IF WS-TAXAMT = 0
      *         MOVE
      *     "TO ZERO TAX, TAX NUM MUST = EXPORT AND SALES CODE 6 OR 52."
      *           TO WS-MESSAGE
      *           PERFORM ERROR-MESSAGE
      *           GO TO TO-000.
      *           
      * WANTED TO USE THE SECTION BELOW TO STOP FOLK CHANGING THE VAT
      * AMT ON INVOICES BUT DIDN'T AS THE ACCOUNTS STAFF NEED ACCESS.
      *      IF WS-TAXAMT NOT = WS-TAXAMT-SAVE
      *         MOVE
      *       "YOU MAY NOT CHANGE THE CALCULATED TAX FOR THE INVOICE."
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
               MOVE "N" TO B-SPECIAL (SUB-1)
               GO TO SPR-999.
           IF WS-STPR-ST1 NOT = 0
              MOVE "SPECIAL PRICES BUSY ON READ, 'ESC' TO RE-TRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               GO TO SPR-005.
           
           PERFORM ERROR1-020.
           MOVE
            "PRESS <F1> TO RE-INSTATE THE ORIGINAL PRICE & DISCOUNT."
              TO WS-MESSAGE
              PERFORM ERROR1-000.
           MOVE "Y" TO B-SPECIAL (SUB-1)
           MOVE "THIS IS A SPECIAL PRICE, NORMAL PRICE IS R"
              TO WS-MESSAGE.
           PERFORM ERROR-000.
           MOVE 3058 TO POS.
           MOVE ST-PRICE TO F-EDNAMEFIELDAMOUNT WS-PRICESAVE
           DISPLAY F-EDNAMEFIELDAMOUNT AT POS.
           MOVE STPR-PRICE TO ST-PRICE.
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
       SPR-999.
           EXIT.
      *
       CHECK-FOR-SPECIALS SECTION.
       CHFSP-000.
           MOVE STTR-STOCK-NUMBER TO STPR-STOCKNUMBER.
           START STPR-MASTER KEY NOT < STPR-KEY
               INVALID KEY NEXT SENTENCE.
       CHFSP-005.
           READ STPR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
               MOVE "N" TO B-SPECIAL (SUB-1)
               GO TO CHFSP-999.
           IF WS-STPR-ST1 NOT = 0
              MOVE "SPECIAL PRICES BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               GO TO CHFSP-005.
           MOVE "Y" TO B-SPECIAL (SUB-1).
       CHFSP-999.
           EXIT.
      *
       DELETE-SPECIAL-PRICE SECTION.
       DSPEC-000.
           MOVE ST-STOCKNUMBER TO STPR-STOCKNUMBER.
           START STPR-MASTER KEY NOT < STPR-KEY
               INVALID KEY NEXT SENTENCE.
       DSPEC-005.
           READ STPR-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
               GO TO DSPEC-999.
           IF WS-STPR-ST1 NOT = 0
              MOVE "SPECIAL PRICES BUSY ON READ-DEL, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               GO TO DSPEC-005.
       DSPEC-500.
           DELETE STPR-MASTER
             INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 NOT = 0
              MOVE "SPECIAL PRICES BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               GO TO DSPEC-500.
       DSPEC-999.
           EXIT.
      *
       WRITE-SPECIALS-FILE SECTION.
       W-SPEC-005.
           MOVE WS-SOLD-BY            TO SP-INITIALS
           MOVE WS-INVOICE            TO SP-INVOICE-NUMBER
           MOVE B-STOCKNUMBER (SUB-1) TO SP-STOCK
           MOVE 1                     TO SP-TRANS
           MOVE WS-ACCOUNT-NUMBER     TO SP-ACCOUNT-NUMBER
           MOVE WS-NAME               TO SP-ACCOUNT-NAME
           MOVE WS-DATE               TO SP-DATE-OF-INVOICE
           MOVE B-NETT (SUB-1)        TO SP-SALE-AMOUNT
           COMPUTE SP-COST-AMOUNT =
                 B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1).
                 
           GO TO W-SPEC-010.
       W-SPEC-006.
           READ SPECIALS-FILE
              INVALID KEY NEXT SENTENCE.
           ADD B-NETT (SUB-1) TO SP-SALE-AMOUNT.
           COMPUTE SP-COST-AMOUNT = SP-COST-AMOUNT + 
                 (B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1)).

           REWRITE SPECIALS-RECORD
              INVALID KEY NEXT SENTENCE.
           IF WS-SPECIALS-ST1 NOT = 0
              MOVE "ERROR IN SPECIALS RE-WRITE, 'ESC' TO SEE CODES"
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
              MOVE "INFORM YOUR SUPERVISOR THAT NO SPECIAL IS WRITTEN."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
              
           GO TO W-SPEC-999.
       W-SPEC-010.
           WRITE SPECIALS-RECORD
              INVALID KEY NEXT SENTENCE.
           IF WS-SPECIALS-ST1 = 23 OR 35 OR 49
              GO TO W-SPEC-006.
           IF WS-SPECIALS-ST1 NOT = 0
              MOVE "ERROR IN SPECIALS WRITE, 'ESC' TO SEE CODES"
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               MOVE "INFORM YOUR SUPERVISOR THAT NO SPECIAL IS WRITTEN."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
       W-SPEC-999.
           EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
             MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
             START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
       R-ST-005.
             READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                 PERFORM START-FOR-READ-NEXT
                 MOVE " " TO ST-DESCRIPTION1
                 GO TO R-ST-999.
             IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-005.
      ***************
      *BRANCH OFFICE*
      ***************
           IF WSAN-CODE = "53"
              PERFORM COMPUTE-SPECIAL-PRICES.
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
           IF WS-STOCK-ST1 NOT = 0
               MOVE "READ-NEXT-STOCK ERROR, 'ESC' TO RETRY."
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
       R-ST-PREV-005.
           READ STOCK-MASTER PREVIOUS
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               GO TO R-ST-PREV-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "READ-PREVIOUS-STOCK ERROR, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-PREV-005.
           IF ST-ANALYSIS = "D"
               MOVE
            "DON'T ENTER ORDER, ITEM TO BE DELETED WHEN ON HAND = ZERO."
                   TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
       R-ST-PREV-999.
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
                 MOVE " " TO ST-STOCKNUMBER
                             ST-DESCRIPTION1
                             ST-DESCRIPTION2
                 MOVE 0 TO ST-PRICE
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
               GO TO R-STL-005.
             PERFORM ERROR-020.
       R-STL-999.
             EXIT.
      *
       UPDATE-STOCK SECTION.
       UPST-000.
           MOVE 1 TO SUB-1.
       UPST-001.
           IF B-STOCKNUMBER (SUB-1) = " "
               GO TO UPST-999.
           IF B-SHIPQTY (SUB-1) = 0
                GO TO UPST-050.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*" OR = "/"
                GO TO UPST-050.
           MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               GO TO UPST-050.
       UPST-010.
           IF WS-NEWORDER = "N"
              SUBTRACT B-SHIPQTY (SUB-1) FROM ST-QTYONRESERVE
                                              ST-QTYONBORDER.
           ADD B-SHIPQTY (SUB-1) TO ST-SALESUNITMTD
                                    ST-SALESUNITSYTD.
           ADD B-NETT (SUB-1) TO ST-SALESRANDSMTD
                                 ST-SALESRANDSYTD.
           COMPUTE ST-SALESCOSTMTD = ST-SALESCOSTMTD + 
               (B-SHIPQTY (SUB-1) * B-STOCKCOST (SUB-1)).
           COMPUTE ST-SALESCOSTYTD = ST-SALESCOSTYTD + 
               (B-SHIPQTY (SUB-1) * B-STOCKCOST (SUB-1)).
           MOVE WS-INVOICEDATE TO ST-LASTSALEDATE.
           
      *********************************************************
      *DELETE SPECIAL FROM STOCK FILE  AND WRITE SPECIALS-FILE*
      *********************************************************
           IF B-SPECIAL (Sub-1) = "N"
              GO TO UPST-112.
           IF ST-QTYONRESERVE = 0
            IF ST-QTYONHAND = 0
      *        MOVE "S" TO ST-ANALYSIS
              PERFORM DELETE-SPECIAL-PRICE.
           PERFORM WRITE-SPECIALS-FILE.
       UPST-112.
           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               GO TO UPST-050.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON REWRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO UPST-112.
       UPST-050.
           ADD 1 TO SUB-1.
           IF SUB-1 < 201
               GO TO UPST-001.
       UPST-999.
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
              MOVE "PARAMETER ON REWRITE DOESN'T EXIST, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
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
       WRITE-DEBTOR-TRANSACTIONS SECTION.
       WRTR-010.
            PERFORM READ-PARAMETER-LOCK.
            MOVE PA-DRTRANS-NUMBER TO DRTR-TRANSACTION-NUMBER
                                      WS-DRTRANS-NO.
            ADD 1 TO PA-DRTRANS-NUMBER.
            PERFORM REWRITE-PARAMETER.
            MOVE 1                 TO DRTR-TYPE.
            MOVE WS-ACCOUNT-NUMBER TO DRTR-ACCOUNT-NUMBER.
            MOVE WS-POORDERNO      TO DRTR-REFERENCE1.
            MOVE WS-INVOICE        TO DRTR-REFERENCE2.
            MOVE WS-INVOICEDATE    TO DRTR-DATE.
            MOVE 0                 TO DRTR-DEL-DATE.
            MOVE WS-INVOICETOTAL   TO DRTR-AMT-OF-INVOICE
                                      DRTR-AMT-OUTSTANDING.
       WRTR-005.
            WRITE DEBTOR-TRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                MOVE "DR.TRANS, NO FILE FOR RWRITE, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO WRTR-010.
            IF WS-DRTRANS-ST1 NOT = 0
                MOVE "DR.TRANS FILE BUSY ON WRITE, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO WRTR-005.
       WRTR-999.
            EXIT.
      *
       WRITE-SOLD-BY SECTION.
       WSB-010.
           MOVE WS-INVOICE        TO SB-INVOICE-NUMBER.
           MOVE Ws-Sold-By        TO SB-TYPE.
           MOVE 1                 TO SB-TRANS.
           MOVE WS-ACCOUNT-NUMBER TO SB-ACCOUNT-NUMBER.
           MOVE WS-NAME           TO SB-ACCOUNT-NAME.
           MOVE WS-DATE           TO SB-DATE-OF-INVOICE.
           MOVE WS-PRICETOTAL     TO SB-SALE-AMOUNT.
           MOVE WS-COSTTOTAL      TO SB-COST-AMOUNT.
       WSB-020.
           WRITE SOLDBY-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-SOLDBY-ST1 NOT = 0
               MOVE "SOLDBY RECORD NOT WRITTEN, ADVISE YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SOLDBY-ST1.
       WSB-999.
           EXIT.
      *
       UPDATE-DEBTOR SECTION.
       UPDR-000.
           MOVE WS-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
       UPDR-010.
           READ DEBTOR-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "DEBTOR FILE DOES NOT EXIST, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO UPDR-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DR-RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO UPDR-010.
            ADD WS-INVOICETOTAL TO DR-BALANCE
                                   DR-CURRENT.
            ADD WS-COSTTOTAL TO DR-COST-PTD
                                DR-COST-YTD.
            ADD WS-PRICETOTAL TO DR-SALES-PTD
                                 DR-SALES-YTD.
            MOVE WS-INVOICEDATE TO DR-DATE-LAST-SALE.
       UPDR-900.
            REWRITE DEBTOR-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                GO TO UPDR-999.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DR-RECORD BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO UPDR-900.
       UPDR-999.
           EXIT.
      *
       UPDATE-DISTRIBUTION SECTION.
       UPDIS-000.
           MOVE "1" TO DIST-KEY.
       UPDIS-010.
           READ DISTRIBUTIONS WITH LOCK
              INVALID KEY NEXT SENTENCE.
           IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
              MOVE "DISTRIBUTION NOT FOUND, CALL YOUR SUPERVISOR" 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO UPDIS-999.
           IF WS-DISTRIBUTION-ST1 NOT = 0 
              MOVE "DIST.RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DISTRIBUTION-ST1
              GO TO UPDIS-010.
           ADD WS-SUBTOTAL TO DIST-INVOICEWEEK
                              DIST-INVOICEPTD
                              DIST-INVOICEYTD.
           ADD WS-DIST-INVOICE TO DIST-ACCRECWEEK
                                  DIST-ACCRECPTD
                                  DIST-ACCRECYTD.
           ADD WS-DIST-ADDON TO DIST-ADDONWEEK
                                DIST-ADDONPTD
                                DIST-ADDONYTD.
       UPDIS-400.
           ADD WS-GST-AMT-TAXED TO GST-AMT-TAXED-WEEK
                                   GST-AMT-TAXED-PTD
                                   GST-AMT-TAXED-YTD.
           ADD WS-GST-AMT-TAXABLE TO GST-AMT-TAXABLE-WEEK
                                     GST-AMT-TAXABLE-PTD
                                     GST-AMT-TAXABLE-YTD.
           ADD WS-GST-AMT-NONTAXABLE TO GST-AMT-NONTAXABLE-WEEK
                                        GST-AMT-NONTAXABLE-PTD
                                        GST-AMT-NONTAXABLE-YTD.
           ADD WS-GST-AMT-EXPORT TO GST-AMT-EXPORT-WEEK
                                    GST-AMT-EXPORT-PTD
                                    GST-AMT-EXPORT-YTD.
       UPDIS-500.
           REWRITE DIST-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
              MOVE "DISTRIBUTIONS NOT UPDATED, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO UPDIS-999.
           IF WS-DISTRIBUTION-ST1 NOT = 0 
              MOVE "DISTRIBUTION BUSY ON UPDATE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DISTRIBUTION-ST1
              GO TO UPDIS-500.
       UPDIS-999.
           EXIT.
      *
       FIND-INFO SECTION.
       FIND-010.
            MOVE "INVOICENUM" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-INVOICE   TO F-EDNAMEFIELDNUM
            MOVE 6            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "ACCOUNTNO"       TO F-FIELDNAME
            MOVE 9                 TO F-CBFIELDNAME
            MOVE WS-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 7                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEBTORNAME" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-NAME      TO F-NAMEFIELD
            MOVE 40           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD1" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE WS-ADD1    TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
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

            MOVE "DELADD3"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            MOVE WS-DELADD3  TO F-NAMEFIELD
            MOVE 25          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PHONE"     TO F-FIELDNAME
            MOVE 5           TO F-CBFIELDNAME
            MOVE WS-PHONE    TO F-NAMEFIELD
            MOVE 20          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CONTACTNAME" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE WS-CONTACT    TO F-NAMEFIELD
            MOVE 20            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESMAN"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE DR-SALESMAN  TO F-NAMEFIELD
            MOVE 1            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POORDERNO"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-POORDERNO TO F-NAMEFIELD
            MOVE 20           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE WSAN-CODE        TO SA-KEY
            PERFORM READ-SALES-ANALYSIS
            MOVE "SALESANALYSIS"  TO F-FIELDNAME
            MOVE 13               TO F-CBFIELDNAME
            MOVE WS-SALESANALYSIS TO F-NAMEFIELD
            MOVE 14               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERVIA"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-DELIVERVIA TO F-NAMEFIELD
            MOVE 20            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TERMOFSALE"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-TERMOFSALE TO F-NAMEFIELD
            MOVE 11            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GSTNO"  TO F-FIELDNAME
            MOVE 5        TO F-CBFIELDNAME
            MOVE WS-GSTNO TO F-NAMEFIELD
            MOVE 13       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BINNO"  TO F-FIELDNAME
            MOVE 5        TO F-CBFIELDNAME
            MOVE WS-BINNO TO F-NAMEFIELD
            MOVE 6        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICEDATE"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE WS-INVOICEDATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE   TO F-NAMEFIELD
            MOVE 10             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SOLDBY"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE Ws-Sold-By TO F-NAMEFIELD
            MOVE 2         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-AREA" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE INCR-AREA  TO F-NAMEFIELD.
            MOVE 1          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COPYDESC"             TO F-FIELDNAME
            MOVE 8                      TO F-CBFIELDNAME
            MOVE "P/SLIP COPY NUMBER :" TO F-NAMEFIELD
            MOVE 20                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COPYNUMBER"     TO F-FIELDNAME
            MOVE 10               TO F-CBFIELDNAME
            MOVE INCR-COPY-NUMBER TO F-NAMEFIELD WS-COPY
            MOVE 2                TO F-CBFIELDLENGTH
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
                MOVE 1 TO SUB-1 F-INDEX.
            PERFORM SCROLL-NEXT
            PERFORM SCROLL-PREVIOUS
            PERFORM CHECK-DISCOUNT.

            MOVE "COMMENTLINE"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE WS-COMMENTLINE TO F-NAMEFIELD
            MOVE 30             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDONFREIGHT"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE WS-ADDONFREIGHT TO F-EDNAMEFIELD99MIL
            MOVE 11              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.

            MOVE "POSTADDON"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-POSTADDON TO F-EDNAMEFIELD99MIL
            MOVE 11           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.

            MOVE "HANDADDON"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-HANDADDON TO F-EDNAMEFIELD99MIL
            MOVE 11           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL

            MOVE "MISC.ADDON" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-MISCADDON TO F-EDNAMEFIELD99MIL
            MOVE 11           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.

            MOVE "SUBTOTAL"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-SUBTOTAL TO F-EDNAMEFIELD99MIL
            MOVE 11          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.

            MOVE "ADDONAMT"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-ADDONAMT TO F-EDNAMEFIELD99MIL
            MOVE 11          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.

            MOVE "TAXAMT"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE WS-TAXAMT TO F-EDNAMEFIELD99MIL
            MOVE 11        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.

            MOVE "INVOICETOTAL"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE WS-INVOICETOTAL TO F-EDNAMEFIELD99MIL
            MOVE 11              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.
       FIND-999.  
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

            MOVE 3010 TO POS.
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

            MOVE 3010 TO POS.
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
            IF SUB-1 > 200   
                GO TO PREV-030.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO B-TAX (SUB-1)
                MOVE 0 TO B-DISCOUNTPERITEM (SUB-1).
            IF F-INDEX < 8
                GO TO PREV-010.
       PREV-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

             MOVE 3010 TO POS.
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
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
                PERFORM SCROLL-COMMENT
                GO TO SCROLL-999.

            MOVE "ORDERQTY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-020.
            MOVE B-ORDERQTY (SUB-1) TO F-EDNAMEFIELDQTY.
            PERFORM WRITE-FIELD-QTY.
       SCROLL-020.
            MOVE "SHIPQTY" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE B-SHIPQTY (SUB-1) TO F-EDNAMEFIELDQTY
                PERFORM WRITE-FIELD-QTY
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.
            IF B-COMPLETE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.
       SCROLL-030.
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE B-STOCKDESCRIPTION (SUB-1) TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PERUNIT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE B-UNIT (SUB-1) TO F-NAMEFIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE 9 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE B-STOCKPRICE (SUB-1) TO F-EDNAMEFIELDAMOUNT
                PERFORM WRITE-FIELD-AMOUNT
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.
            IF B-COMPLETE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKCOST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 9 TO F-CBFIELDLENGTH.
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
            IF B-COMPLETE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.
       SCROLL-040.
            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME.
            MOVE 15 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE B-DISCOUNTPERITEM (SUB-1) TO
                           F-EDNAMEFIELDAMOUNTDIS
                PERFORM WRITE-FIELD-AMOUNTDIS
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.
            IF B-COMPLETE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.

      *      MOVE "TAX" TO F-FIELDNAME.
      *      MOVE 3 TO F-CBFIELDNAME.
      *      MOVE B-TAX (SUB-1) TO F-NAMEFIELD.
      *      MOVE 1 TO F-CBFIELDLENGTH.
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
       SCROLL-COMMENT SECTION.
       SCCO-000.
            MOVE "ORDERQTY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            MOVE C-ORDER (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SHIPQTY" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            MOVE C-SHIP (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE 20 TO F-CBFIELDLENGTH.
            MOVE C-DESC (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PERUNIT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 4 TO F-CBFIELDLENGTH.
            MOVE C-UNIT (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE 8 TO F-CBFIELDLENGTH.
            MOVE C-PRICE (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKCOST"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE 9              TO F-CBFIELDLENGTH
            MOVE C-COST (SUB-1) TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME

            MOVE 15                TO F-CBFIELDNAME
            MOVE 5                 TO F-CBFIELDLENGTH
            MOVE C-DISC (SUB-1)    TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.
       SCCO-999.
            EXIT.
      *
       CANCEL-INVOICE SECTION.
       CI-000.
             MOVE 1 TO SUB-1.
             IF WS-NEWORDER NOT = "Y"
                 GO TO CI-900.
       CI-010.
             IF B-STOCKNUMBER (SUB-1) = " "
                IF B-ORDERQTY (SUB-1) = 0
                 GO TO CI-900.
             PERFORM CANCEL-TRANSACTION.
             MOVE 1 TO SUB-1.
             GO TO CI-010.
       CI-900.
             UNLOCK INCR-REGISTER.
             UNLOCK STOCK-TRANS-FILE.
       CI-950.
             PERFORM CLEAR-FIELDS.
             PERFORM DISPLAY-FORM.
             MOVE 2801 TO POS.
             MOVE " " TO WS-MESSAGE.
             DISPLAY WS-MESSAGE AT POS.
       CI-999.
             EXIT.
      *
       CANCEL-TRANSACTION SECTION.
       CAN-005.
           COMPUTE SUB-2 = SUB-1 + 1.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*" OR = "/"
                GO TO CAN-010.
           MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
              GO TO CAN-010.
           ADD B-SHIPQTY (SUB-1) TO ST-QTYONHAND.
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
               MOVE "STOCK BUSY ON CANCEL-REWRITE, 'ESC' TO RETRY."
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
                         B-UNIT (SUB-1).
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
            MOVE 9           TO F-CBFIELDLENGTH
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
       CLEAR-BOTTOM-FIELDS SECTION.
       CBF-000.
            MOVE "ADDONFREIGHT" TO F-FIELDNAME.
            MOVE 12             TO F-CBFIELDNAME.
            MOVE 11             TO F-CBFIELDLENGTH.
            MOVE SPACES         TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADDON" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            MOVE 11          TO F-CBFIELDLENGTH.
            MOVE SPACES      TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "HANDADDON" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            MOVE 11          TO F-CBFIELDLENGTH.
            MOVE SPACES      TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "MISC.ADDON" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            MOVE 11           TO F-CBFIELDLENGTH.
            MOVE SPACES       TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUBTOTAL" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE 11         TO F-CBFIELDLENGTH.
            MOVE SPACES     TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDONAMT" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE 11         TO F-CBFIELDLENGTH.
            MOVE SPACES     TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TAXAMT"   TO F-FIELDNAME.
            MOVE 6          TO F-CBFIELDNAME.
            MOVE 11         TO F-CBFIELDLENGTH.
            MOVE SPACES     TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICETOTAL" TO F-FIELDNAME.
            MOVE 12             TO F-CBFIELDNAME.
            MOVE 11             TO F-CBFIELDLENGTH.
            MOVE SPACES         TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
       CBF-999.
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
                         B-SPECIAL (SUB-1)
                         B-COMPLETE (SUB-1).
             MOVE 0   TO B-ORDERQTY (SUB-1)
                         B-SHIPQTY (SUB-1)
                         B-SHIPPEDQTY (SUB-1)
                         B-STOCKPRICE (SUB-1)
                         B-STOCKCOST (SUB-1)
                         B-DISCOUNTPERITEM (SUB-1)
                         B-NETT (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 201
                 GO TO CF-010.
       CF-999.
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
                MOVE "STDISCOUNT BUSY ON READ, 'ESC' TO RE-TRY."
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
           MOVE 2859 TO POS.
           MOVE WS-DR-DISC TO F-EDNAMEFIELDAMOUNTDIS.
           DISPLAY F-EDNAMEFIELDAMOUNTDIS AT POS.
       R-STDISC-999.
             EXIT.
      *
       QUES-PULL-CASH SECTION.
       QPC-005.
           IF WS-QUES-PRINT-PULLERS = "N"
               GO TO QPC-500.
           PERFORM CLEAR-010.
        QPC-006.
            OPEN I-O PULLER-MASTER.
            IF WS-PU-ST1 NOT = 0
               MOVE "PULLER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-PU-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-PU-ST1
               GO TO QPC-006.
       QPC-010.
           OPEN I-O PULL-BY.
           IF WS-PULLBY-ST1 NOT = 0
              MOVE "PULLBY ERROR ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-PULLBY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-PULLBY-ST1
              GO TO QPC-010.
           MOVE " " TO PB-INITIAL.
       QPC-015.
           MOVE INCR-PULLBY TO PB-INITIAL.
           MOVE 2910 TO POS
           DISPLAY
           "ENTER THE INITIALS OF THE PERSON THAT PULLED THE STOCK [  ]"
             AT POS
           ADD 56 TO POS

           MOVE PB-INITIAL TO CDA-DATA.
           MOVE 2          TO CDA-DATALEN.
           MOVE 26         TO CDA-ROW.
           MOVE 65         TO CDA-COL.
           MOVE CDA-WHITE  TO CDA-COLOR.
           MOVE 'F'        TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO PB-INITIAL.

           IF PB-INITIAL = "  "
              DISPLAY " " AT 3079 WITH BELL
              GO TO QPC-015.
           MOVE PB-INITIAL TO PU-INITIAL INCR-PULLBY
           PERFORM READ-PULLERS.
           IF WS-PU-ST1 = 23 OR 35 OR 49
              MOVE
           "PLEASE ENTER A VALID INITIAL FOR A CURRENT STORES PULLER"
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO QPC-015.
           
           MOVE WS-INVOICE      TO PB-INVOICE
           MOVE Ws-InvoiceDate  TO PB-INV-DATE
           MOVE SUB-20          TO PB-LINE-CNT
           SUBTRACT 1         FROM PB-LINE-CNT
           MOVE WS-INVOICETOTAL TO PB-SALE-AMOUNT.
       QPC-020.
           WRITE PULLBY-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-PULLBY-ST1 NOT = 0
               MOVE "PULLBY RECORD NOT WRITTEN, ADVISE YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-PULLBY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-PULLBY-ST1.
           CLOSE PULL-BY
                 PULLER-MASTER.
       QPC-500.
           PERFORM ERROR-020.
           PERFORM ERROR1-020.
           IF WS-QUES-CASH-SALES = "N"
               GO TO QPC-900.
           PERFORM CLEAR-010.
       QPC-505.
           MOVE " " TO WS-DIS
           MOVE 3010 TO POS
           DISPLAY
           "IS THIS A CASH TRANSACTION, ENTER Y OR N : [ ]" AT POS
           ADD 44 TO POS
           
           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DIS.

           IF WS-DIS NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO QPC-505.
           IF WS-DIS = "N"
              GO TO QPC-900.
           PERFORM ERROR-020.
           IF WS-CASHSALE-ST1 = 88
              GO TO QPC-515.
      *************************************************************
      *ST1=8; CASH-SALE OPEN, SO WE DON'T OPEN AGAIN IF UP-ARROW  *
      *       FROM QPC-515 INCASE OPERATOR CHANGES MIND ON YES/NO *
      *************************************************************
       QPC-510.
           OPEN I-O CASH-SALE.
           IF WS-CASHSALE-ST1 NOT = 0
              MOVE "CASHSALE ERROR ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CASHSALE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CASHSALE-ST1
               GO TO QPC-510.
           MOVE " " TO WS-CASH-ACCEPT.
           MOVE 88 TO WS-CASHSALE-ST1.
       QPC-515.
           MOVE 2910 TO POS
           DISPLAY
           "ENTER THE AMOUNT PAID BY CASH : [         ]" AT POS
           ADD 33 TO POS
           MOVE WS-INVOICETOTAL TO F-EDNAMEFIELDAMOUNT
           MOVE F-EDNAMEFIELDAMOUNT TO WS-CASH-ACCEPT CDA-DATA
           DISPLAY F-EDNAMEFIELDAMOUNT AT POS.

           MOVE 9         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CASH-ACCEPT.

           IF W-ESCAPE-KEY = 4
              GO TO QPC-505.
           MOVE WS-CASH-ACCEPT TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO CS-SALE-AMOUNT F-EDNAMEFIELDAMOUNT
           MOVE 2943 TO POS
           DISPLAY F-EDNAMEFIELDAMOUNT AT POS.
           IF SIGN-FOUND = 1
              GO TO QPC-515.
           IF CS-SALE-AMOUNT = 0
              DISPLAY " " AT 3079 WITH BELL
              GO TO QPC-515.
       QPC-517.
           MOVE 3010 TO POS
           DISPLAY "IS THE AMOUNT ENTERED CORRECT : [ ]" AT POS
           ADD 33 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DIS.

           IF WS-DIS NOT = "Y" AND NOT = "N"
              DISPLAY " " AT 3079 WITH BELL
              GO TO QPC-517.
           IF WS-DIS = "N"
              GO TO QPC-515.
           MOVE WS-INVOICE        TO CS-INVOICE
           MOVE WS-ACCOUNT-NUMBER TO CS-ACCOUNT
           MOVE WS-INVOICEDATE    TO CS-INV-DATE
           MOVE WS-SOLD-BY        TO CS-INITIAL.
       QPC-520.
           WRITE CASHSALE-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-CASHSALE-ST1 NOT = 0
               MOVE
                "CASHSALE RECORD NOT WRITTEN, ADVISE YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CASHSALE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CASHSALE-ST1.
           CLOSE CASH-SALE.
       QPC-900.
           PERFORM ERROR-020.
           PERFORM ERROR1-020.
           IF WS-IMM-PR NOT = "S" AND NOT = "H"
               GO TO QPC-999.
       QPC-910.
           MOVE " " TO WS-DIS
           MOVE 3010 TO POS
           DISPLAY
           "DO YOU WISH TO PRINT A PACKING LABEL, ENTER Y OR N : [ ]"
            AT POS
           ADD 54 TO POS

           MOVE 'N'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 63        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DIS.

.
           IF WS-DIS NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO QPC-910.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
       QPC-999.
           EXIT.
      *
       PRINT-LABEL SECTION.
       PL-001.
           MOVE SPACES TO WS-PRINTER.
           MOVE 5      TO WS-PRINTERNUMBER (21).
           MOVE 4      TO WS-PRINTERTYPE (21).
           Copy "PrinterSpecial".
       PL-002.
           MOVE "  " TO WS-SOLD-BY.
           MOVE 2920 TO POS.
           DISPLAY "IN PRINTING PARCEL LABELS," AT POS
           MOVE 3010 TO POS
           DISPLAY
           "HOW MANY COPIES DO YOU WANT TO PRINT : [  ]" AT POS
           ADD 40 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SOLD-BY.

           IF WS-SOLDBY = " "
              DISPLAY " " AT 3079 WITH BELL
              GO TO PL-002.
            MOVE WS-SOLD-BY TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-COPIES
                                 F-EDNAMEFIELDANAL.
            DISPLAY F-EDNAMEFIELDANAL AT POS.
            IF NUMERIC-RATE > 1
               MOVE 2910 TO POS
               DISPLAY "ARE YOU SURE YOU WANT SO MANY COPIES? [ ]"
               AT POS
               ADD 39 TO POS
               MOVE ' '       TO CDA-DATA
               MOVE 1         TO CDA-DATALEN
               MOVE 26        TO CDA-ROW
               MOVE 48        TO CDA-COL
               MOVE CDA-WHITE TO CDA-COLOR
               MOVE 'F'       TO CDA-ATTR
               PERFORM CTOS-ACCEPT
               MOVE CDA-DATA TO WS-DIS.

            IF WS-COPIES = 1
               GO TO PL-003.
            IF WS-COPIES NOT > 1
               GO TO PL-002.
            IF WS-DIS = "Y"
               GO TO PL-003
            ELSE
               GO TO PL-002.
       PL-003.
           MOVE "  " TO WS-SOLD-BY.
           MOVE 3010 TO POS
           DISPLAY
           "HOW MANY PARCELS ARE THERE TO PRINT FOR: [  ]" AT POS
           ADD 42 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SOLD-BY.

           IF WS-SOLDBY = " "
              DISPLAY " " AT 3079 WITH BELL
              GO TO PL-003.
            MOVE WS-SOLD-BY TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-PARCEL
                                 F-EDNAMEFIELDANAL.
            DISPLAY F-EDNAMEFIELDANAL AT POS.
            IF WS-PARCEL = 1
               GO TO PL-005.
            IF WS-PARCEL < 1
               GO TO PL-003.
            IF WS-PARCEL > 1
               MOVE 2910 TO POS
               DISPLAY "ARE YOU SURE YOU WANT SO MANY PARCELS? [ ]"
               AT POS
               ADD 40 TO POS
               MOVE ' '       TO CDA-DATA
               MOVE 1         TO CDA-DATALEN
               MOVE 26        TO CDA-ROW
               MOVE 49        TO CDA-COL
               MOVE CDA-WHITE TO CDA-COLOR
               MOVE 'F'       TO CDA-ATTR
               PERFORM CTOS-ACCEPT
               MOVE CDA-DATA TO WS-DIS.

            IF WS-DIS = "Y"
               GO TO PL-005
            ELSE
               GO TO PL-003.
       PL-005.
           PERFORM ERROR-020
           MOVE 3010 TO POS
           DISPLAY "LABELS BEING PRINTED, PLEASE BE PATIENT...." AT POS
           MOVE 0  TO WS-COPIES-PRINTED
                      WS-PARCEL-PRINTED.
           CALL "C$SLEEP" USING 1.
      *          MOVE 10 TO  W-DELAY.
       PL-012.
           PERFORM PL-001.
           
      *     MOVE "IN PRINT-LABEL SECTION" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           
           MOVE WS-PRINT-COMP   TO PRINT-REC
           WRITE PRINT-REC.
           MOVE " "             TO PARCEL2 PRINT-REC
           MOVE INCR-NAME       TO PL2-REST
           MOVE WS-PRINT-BOLD   TO PL2-DIG1
           WRITE PRINT-REC FROM PARCEL2.
           MOVE WS-PRINT-NORMAL TO PRINT-REC
           WRITE PRINT-REC.

           MOVE " "             TO PARCEL2 PRINT-REC
           MOVE INCR-DEL1       TO PL2-REST
           WRITE PRINT-REC FROM PARCEL2
           MOVE " "             TO PARCEL2 PRINT-REC
           MOVE INCR-DEL2       TO PL2-REST
           WRITE PRINT-REC FROM PARCEL2
           MOVE " "             TO PARCEL2 PRINT-REC
           MOVE INCR-DEL3       TO PL2-REST
           MOVE WS-PRINT-UNBOLD TO PL2-DIG2
           WRITE PRINT-REC FROM PARCEL2
           MOVE " "             TO PARCEL2 PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC.

           MOVE WS-PRINT-BOLD   TO PL2-REST-DIG1
           MOVE "ORDER     #:"  TO PL2-DESC
           MOVE INCR-PORDER     TO PL2-ADD
           MOVE WS-PRINT-UNBOLD TO PL2-DIG2
           WRITE PRINT-REC FROM PARCEL2
           MOVE " "             TO PARCEL2 PRINT-REC
           MOVE WS-PRINT-BOLD   TO PL2-REST-DIG1
           MOVE "INVOICE   #:"  TO PL2-DESC
           MOVE INCR-INVOICE    TO PL2-ADD
           MOVE WS-PRINT-UNBOLD TO PL2-DIG2
           WRITE PRINT-REC FROM PARCEL2
           MOVE " "             TO PARCEL2 PRINT-REC
           MOVE WS-PRINT-BOLD   TO PL2-REST-DIG1
           MOVE "INTERNAL  #:"  TO PL2-DESC
           MOVE INCR-BO-INV-NO  TO PL2-ADD
           MOVE WS-PRINT-UNBOLD TO PL2-DIG2
           WRITE PRINT-REC FROM PARCEL2
           MOVE " "             TO PARCEL2 PRINT-REC.

           MOVE WS-PRINT-BOLD   TO PL2-REST-DIG1
           MOVE "DELIVERY   :"  TO PL2-DESC
           MOVE INCR-DELIVERY   TO PL2-ADD
           MOVE WS-PRINT-UNBOLD TO PL2-DIG2
           WRITE PRINT-REC FROM PARCEL2
           MOVE " "             TO PARCEL2 PRINT-REC
           MOVE WS-PRINT-BOLD   TO PL2-REST-DIG1
           MOVE "CONTACT    :"  TO PL2-DESC
           MOVE WS-PRINT-UNBOLD TO PL2-DIG2
           MOVE INCR-CONTACT    TO PL2-ADD
           WRITE PRINT-REC FROM PARCEL2
           MOVE " "             TO PARCEL2 PRINT-REC
           ADD 1                TO WS-PARCEL-PRINTED
           MOVE WS-PRINT-BOLD     TO PCL-REST-DIG1
           MOVE "PARCEL    #:"    TO PCL-DESC
           MOVE WS-PARCEL-PRINTED TO PCL-NO1
           MOVE WS-PARCEL         TO PCL-NO2
           MOVE WS-PRINT-UNBOLD   TO PCL-DIG2
           WRITE PRINT-REC FROM PARCEL-LINE
           MOVE " "               TO PRINT-REC
           SUBTRACT 1 FROM WS-PARCEL-PRINTED
           WRITE PRINT-REC
           WRITE PRINT-REC
      *     MOVE WS-PRINT-BOLD   TO PL2-DIG1
      *     MOVE WS-COMMENT      TO PL2-REST
      *     WRITE PRINT-REC FROM PARCEL2.

           MOVE " "             TO PARCEL2 PRINT-REC
           WRITE PRINT-REC
           MOVE "SUPPLIED BY:"  TO PL2-DESC
           MOVE WS-PRINT-UNBOLD TO PL2-DIG2
           WRITE PRINT-REC FROM PARCEL2.

           MOVE " "             TO PARCEL2 PRINT-REC
           WRITE PRINT-REC BEFORE PAGE.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.

           CALL "C$SLEEP" USING 1.
      *     CALL "&DELAY" USING W-ERROR W-DELAY.

           ADD 1 TO WS-PARCEL-PRINTED.
           IF WS-PARCEL-PRINTED NOT = WS-PARCEL
               GO TO PL-012
           ELSE
               ADD 1 TO WS-COPIES-PRINTED
               MOVE 0 TO WS-PARCEL-PRINTED.

           IF WS-COPIES NOT = WS-COPIES-PRINTED
               GO TO PL-012.
           IF WS-IMM-PR = "B"
               GO TO PL-999.
           IF WS-IMM-PR = "H"
               MOVE 3 TO WS-PROG-TYPE
               GO TO PL-999.
           MOVE 1 TO SUB-1.
       PL-900.
           IF WS-IMM-PR = "C"
            IF WS-PRINTERNUMBER (SUB-1) = 7
               MOVE WS-PRINTERNAME (SUB-1) TO WS-PRINTER
               MOVE WS-PRINTERCHARS (SUB-1) TO WS-PRINT-CHARS
               GO TO PL-999.
           IF WS-IMM-PR = "S"
            IF WS-PRINTERNUMBER (SUB-1) = 4
               MOVE WS-PRINTERNAME (SUB-1) TO WS-PRINTER
               MOVE WS-PRINTERCHARS (SUB-1) TO WS-PRINT-CHARS
               GO TO PL-999.
           IF SUB-1 < 11
             ADD 1 TO SUB-1
             GO TO PL-900.
           MOVE "CAN'T FIND A PRINTERNUMBER, PRN PARAMETER NOT SET UP."
            TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
       PL-999.
           EXIT.
      *
       READ-PULLERS SECTION.
       RPULL-000.
           START PULLER-MASTER KEY NOT < PU-KEY.
       RPULL-010.
           READ PULLER-MASTER
                 INVALID KEY NEXT SENTENCE.
           IF WS-PU-ST1 = 23 OR 35 OR 49
                GO TO RPULL-999.
           IF WS-PU-ST1 NOT = 0
                MOVE "PULLER RECORD BUSY ON READ, 'ESC' TO RETRY"
                  TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-PU-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-PU-ST1
                GO TO RPULL-010.
           MOVE PU-INITIAL TO PB-INITIAL.
       RPULL-999.
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
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O PARAMETER-FILE.
           PERFORM READ-PARAMETER.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           IF WS-MM = PA-CURRENT-PER-MM
             IF WS-YY = PA-CURRENT-PER-YY
               GO TO OPEN-010.
       OPEN-005.
           MOVE 0610 TO POS
           DISPLAY "THE CURRENT MONTH OR YEAR ON THE PARAMETER FILE"
           AT POS
           MOVE 0710 TO POS
           DISPLAY "DOES NOT CORRESPOND WITH TODAYS DATE!!!!"
           AT POS
           MOVE 0810 TO POS
           DISPLAY "GO AND CHECK THE SYSTEM DATE, "
           AT POS
           MOVE 0910 TO POS
           DISPLAY "OR ELSE RUN THE MONTH-END ROUTINE."
           AT POS
           MOVE 1010 TO POS
           DISPLAY "PRESS 'GO' OR 'NEXT' TO END THE PROGRAM."
           AT POS
           MOVE 1110 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-IMM-PR.

           IF W-ESCAPE-KEY = 1 OR 2
               CLOSE PARAMETER-FILE
               EXIT PROGRAM
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO OPEN-005.
       OPEN-010.
           MOVE ALL "X" TO STORE-TERM
                           STORE-DEL.
           PERFORM READ-TERMS-FILE
           PERFORM READ-DELIVERY-FILE
           PERFORM READ-COMM-FILE
           PERFORM READ-INVQUES-FILE
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
              MOVE "SPECIAL PRICE ON OPEN, 'ESC' TO RETRY."
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
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
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
              MOVE "ST-TRANS. FILE BUSY ON OPEN, 'ESC' TO RETRY."
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
              MOVE "REGISTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-INCR-ST1
              GO TO OPEN-016.
       OPEN-017.
           OPEN I-O SPECIALS-FILE.
           IF WS-SPECIALS-ST1 NOT = 0 
              MOVE "SPECIALS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SPECIALS-ST1
              GO TO OPEN-017.
       OPEN-018.
           OPEN I-O DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0 
              MOVE "DR-TRANS. BUSY ON OPEN,  'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DRTRANS-ST1
              GO TO OPEN-018.
       OPEN-019.
            OPEN I-O SALES-ANALYSIS.
            IF WS-SALES-ST1 NOT = 0
               MOVE "SALES ANALYSIS BUSY, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO OPEN-019.
       OPEN-020. 
           OPEN I-O SOLD-BY.
           IF WS-SOLDBY-ST1 NOT = 0
              MOVE "SOLDBY BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SOLDBY-ST1
              GO TO OPEN-020.
       OPEN-021.
           OPEN I-O DISTRIBUTIONS.
           IF WS-DISTRIBUTION-ST1 NOT = 0
              MOVE "DISTRIBUTION FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DISTRIBUTION-ST1
              GO TO OPEN-021.
       OPEN-022.
            OPEN I-O STDISC-MASTER.
            IF WS-STDISC-ST1 NOT = 0
               MOVE "STOCK SPEC-DISC BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STDISC-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STDISC-ST1
               GO TO OPEN-022.
       OPEN-030.
           MOVE "ACCOUNTNO"    TO F-FIELDNAME
           PERFORM READ-PARAMETER-LOCK
           MOVE PA-CO-VAT-NO   TO WS-CO-VATNO
           MOVE PA-GST-PERCENT TO WS-GST-PERCENT
           PERFORM REWRITE-PARAMETER.
       OPEN-050.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlInvoic"      TO F-FORMNAME
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
                 STOCK-TRANS-FILE
                 DEBTOR-TRANS-FILE
                 SALES-ANALYSIS
                 SPECIALS-FILE
                 DISTRIBUTIONS
                 SOLD-BY
                 INCR-REGISTER
                 STDISC-MASTER.
           EXIT PROGRAM.
      *      STOP RUN.
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
       Copy "WriteField99Mil".
       Copy "WriteFieldAmount1".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldPrice".
       Copy "WriteFieldQty". 
       Copy "CheckDataNames".
       Copy "QueuePrintLaserInvoice".
       Copy "Z1LaserHeadings".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserPrintName".
       Copy "SetupInvoiceForPDF".
       Copy "SendReportToPrinter".
       Copy "GetUserMailName".
       Copy "ZoomBox".
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
       Copy "CTOSCobolAccept".
       Copy "WriteDailyExcep1".
      * END-OF-JOB
