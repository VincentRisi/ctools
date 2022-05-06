       IDENTIFICATION DIVISION.
       PROGRAM-ID. SlQuote.
       AUTHOR.  CHRISTENSEN.
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
         Copy "SelectDrMaster".
         Copy "SelectStMaster".
         Copy "SelectStSpecPr".
         Copy "SelectSlMaster".
         Copy "SelectSlParameter".
         Copy "SelectStTrans".
         Copy "SelectSlRegister".
         Copy "SelectSlSbRep".
         Copy "SelectSlDaily".
         Copy "SelectCoFaxParam".
         Copy "SelectStDiscAcc".
         Copy "SelectBmMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdStock.
           COPY ChlfdStPrice.
           COPY ChlfdSales.
           COPY ChlfdParam.
           COPY ChlfdStTrans.
           COPY ChlfdRegister.
           COPY ChlfdSbRep.
           COPY ChlfdDaily.
           COPY ChlfdFaxParam.
           COPY ChlfdStDiscAcc.
           COPY ChlfdToolkit.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(90).
      *
       WORKING-STORAGE SECTION.
       77  WS-ACCNO-X           PIC X(7) VALUE " ".
       77  WS-DISCOUNT-CODE     PIC X VALUE " ".
       77  WS-NEWORDER          PIC X VALUE " ".
       77  WS-ORDER-COMPLETE    PIC X VALUE " ".
       77  WS-LINECHANGED       PIC X VALUE " ".
       77  WS-MUST-PRINT        PIC X VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-PRINT-ANSWER      PIC X VALUE " ".
       77  WS-DIS               PIC XX VALUE " ".
       77  WS-COST-DISPLAY      PIC X VALUE "N".
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-BELOWF-BODY          PIC X VALUE " ".
       77  WS-PRINT-TOTALS         PIC X VALUE " ".
       77  WS-QUES-MU-GP-PERC      PIC X VALUE " ".
       77  WS-QUES-CHECK-QUOTES    PIC X VALUE " ".
       77  WS-QUES-PAUSE-ON-PSLIP  PIC X VALUE " ".
       77  WS-ORDERTOTAL           PIC 9(8)V99 VALUE 0.
       77  WS-QUES-ACC-OVER-LIMIT  PIC X VALUE " ".
       77  WS-LIMIT-EXCEP-WRITE    PIC X VALUE " ".
       77  WS-NORM-PRINTER      PIC 99.
       77  WS-REPR-PRINTER      PIC 99.
       77  WS-RUSH-PRINTER      PIC 99.
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-TOOLKIT-NUMBER    PIC X(15) VALUE " ".
       77  WS-POORDERNO         PIC X(20) VALUE " ".
       77  WS-SALESANALYSIS     PIC X(14) VALUE " ".
       77  WS-SALESANALYSIS-SAVE  PIC X(14) VALUE " ".
       77  WS-ANAL-CODE           PIC XX VALUE " ".
       77  WSAN-CODE-SAVE         PIC XX VALUE " ".
       77  WS-SOLD-BY             PIC XX VALUE " ".
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
       77  WS-WORKTOTAL         PIC 9(8)V99 VALUE 0.
       77  WS-WORKTOTAL1        PIC 9(8)V99 VALUE 0.
       77  WS-WORKTOTAL2        PIC 9(8)V99 VALUE 0.
       77  WS-COSTTOTAL         PIC 9(8)V99 VALUE 0.
       77  WS-COSTTOTAL1        PIC 9(8)V99 VALUE 0.
       77  WS-PRICETOTAL        PIC 9(8)V99 VALUE 0.
       77  WS-PRICETOTAL1       PIC 9(8)V99 VALUE 0.
       77  WSF-XPORTTOTAL       PIC 9(8)V99 VALUE 0.
       77  WS-DISCOUNTREG       PIC 9(8)V99 VALUE 0.
       77  WS-DISCOUNTREG1      PIC 9(8)V99 VALUE 0.
       77  WS-DISCOUNT-ACCEPT   PIC X(10) VALUE " ".
       77  WS-DISCOUNT          PIC 9(8)V99 VALUE 0.
       77  WS-DISCOUNT1         PIC 9(8)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(8)V99 VALUE 0.
       77  WS-NEWPRICE          PIC S9(6)V99 VALUE 0.
       77  WS-PRICESAVE         PIC S9(6)V99 VALUE 0.
       77  WS-DISCOUNTSAVE      PIC S9(6)V99 VALUE 0.
       77  WS-PERC              PIC S9(8)V99 VALUE 0.
       77  WS-GST-PERCENT       PIC 99V99 VALUE 0.
       77  WS-BODY-LINE         PIC ZZ9.
       77  WS-IMM-PR            PIC X VALUE " ".
       77  WS-FAX-NUMBER        PIC X(20) VALUE " ".
       77  WSF-MAIL-NUMBER      PIC X(50) VALUE " ".
       77  PAGE-CNT             PIC 99 VALUE 0.
       77  LINE-CNT             PIC 99 VALUE 0.
       77  COPY-CNT             PIC 9 VALUE 0.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "DrNameIq".
       77  WS-STOCK-INQUIRY     PIC X(8) VALUE "StMastIq".
       77  WS-PORDER-INQUIRY    PIC X(8) VALUE "SlPoShIq".
       77  WS-QUOTE-ACINQ       PIC X(8) VALUE "SlQuAcIq".
       77  WS-QUOTE-STINQ       PIC X(8) VALUE "SlQuStIq".
       77  WS-CODETAB           PIC X VALUE " ".
       77  WS-ZERODIS           PIC X VALUE " ".
       77  WS-PRINT-SUF-COMMENT PIC X VALUE "Y".
       77  WS-DR-DISC           PIC 9(2)V99 VALUE 0.
       77  WS-INVOICEDISCOUNT   PIC 9(2)V99 VALUE 0.
       77  WS-BO-QTY            PIC S9(5) VALUE 0.
       77  PSW-SUB1             PIC S9(5) VALUE 0.
       77  PSW-SUB2             PIC S9(5) VALUE 0.
       77  WS-BEFORE            PIC 9(3) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-TERM-SUB          PIC 9 VALUE 0.    
       77  WS-DEL-SUB           PIC 9 VALUE 0.    
       77  WSAN-CODE-3          PIC XX VALUE "  ".
       77  WS-COMMENT1          PIC X(60) VALUE " ".
       77  WS-COMMENT2          PIC X(60) VALUE " ".
       77  WS-SALESMAN          PIC X(20) VALUE " ".
       77  HAVE-FOUND-BO        PIC XXX VALUE "   ".
       77  WS-BOFOUND           PIC X VALUE " ".
       77  WS-STTRANS-NO        PIC 9(6).
       77  WS-DRTRANS-NO        PIC 9(6).
       77  WS-READS             PIC 99.
       77  WS-REPRINT           PIC X VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WSF-RR               PIC XXX VALUE " ".
       77  WS-PASSWORDSAVED     PIC X(10).
       77  WS-PRINTER-PAGE1     PIC X(100) VALUE " ".
       77  WS-PRINTER-PAGE2     PIC X(100) VALUE " ".
       77  WS-PRINTER-DOT       PIC X(100) VALUE " ".
       77  WS-PRINTER-PDF       PIC X(100) VALUE " ".
       77  WS-PRINTER-FAX       PIC X(100) VALUE " ".
       77  WS-ACC-ERROR         PIC X VALUE " ".      
       77  WS-QUOTE-NAME        PIC X(25) VALUE " ".
       01  WS-EMAIL             PIC X(50).
       01  WS-TEMP-EMAIL-FILE   PIC X(50).
       01  WS-SPACE-CNT         PIC 9(2) VALUE ZEROES.
       01  W-READ-KEY           PIC X(11).
       01  W-CRTSTATUS          PIC 9(4) value 0.
       01  F-RC                 BINARY-SHORT VALUE 0.
       01  WS-STDESC.
           03  WS-DESC1         PIC X(20) VALUE " ".
           03  WS-DESC2         PIC X(20) VALUE " ".
       01  WS-PASSWORD-KEY.
           03  WS-PA-KEY        PIC X OCCURS 11.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1          PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1           PIC 99.
       01  WS-STPR-STATUS.
           03  WS-STPR-ST1            PIC 99.
       01  WS-SALES-STATUS.
           03  WS-SALES-ST1           PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1           PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1     PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1         PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1            PIC 99.
       01  WS-SBREP-STATUS.
           03  WS-SBREP-ST1           PIC 99.
       01  WS-FAX-STATUS.
           03  WS-FAX-ST1             PIC 99.
       01  WS-STDISC-STATUS.
           03  WS-STDISC-ST1          PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-TOOLKIT-ST1         PIC 99.
       01  WS-QUOTE-CHECK.
           03  WS-O-C           PIC X OCCURS 25.
       01  WS-NAME-LENGTH.
           03  WS-NL            PIC X OCCURS 40.
       01  WS-XQS-FAX.
           03  WS-XQS-BEG       PIC X(9) VALUE "(:(FaxTo=".
           03  WS-XQS-END       PIC X(3) VALUE "):)".
           03  WS-XQS-COVERS.
               05  WS-XQS1      PIC X(6) VALUE "Cover=".
               05  WS-XQS2      PIC X(2) VALUE "  ".
               05  WS-XQS3      PIC X(12) VALUE "Quote\Other=".
               05  WS-XQS4      PIC X(2) VALUE "  ".
               05  WS-XQS5      PIC X(6) VALUE "Quote2".
           03  WS-XQS-FROM.
               05  WS-XQS-FROMID    PIC X(6) VALUE "\From=".
               05  WS-XQS-FROM-NAME PIC X(25) VALUE " ".
           03  WS-XQS-COMMENT-LINE.
               05 WS-XQS-COMM-DESC PIC X(34) VALUE
                "(:(Comment=OUR QUOTE REFERENCE #: ".
               05 WS-XQS-COMMENT   PIC X(17) VALUE " ".
           03  WS-XQS-PRIORITY     PIC X(26) VALUE
               "Pri=B\OurRef=*QUOTATION*\".
           03  WS-XQS-USERNAME.
               05  WS-XQS-SENT   PIC X(6) VALUE "NSent=".
               05  WS-XQS-SNAME  PIC X(25) VALUE " ".
               05  WS-XQS-ERROR  PIC X(8) VALUE "\NError=".
               05  WS-XQS-ENAME  PIC X(25) VALUE " ".
       01  WS-XQS-FAX-LINES.
           03  WS-XQS-LINE OCCURS 5 PIC X(100) VALUE " ".
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
       01  WS-QUOTATION.
           03  WS-QUOTE         PIC 9(6).
           03  WS-FIL-M         PIC X.
           03  WS-QUOTE-MM      PIC 99.
           03  WS-FIL-Y         PIC X.
           03  WS-QUOTE-YY      PIC 99.
       01  WSF-MAIL-QUOTE.
           03  WSF-Q-FIL        PIC X(15) VALUE "/ctools/eimage/".
           03  WSF-QUOTE        PIC X(7).
           03  FILLER           PIC X VALUE "-".
           03  WSF-PRINTNUMBER  PIC 99.
       01  WS-EMAIL-FINAL.
           03  WS-EF-FIL        PIC X(15) VALUE " ".
           03  WS-BAL-OF-NAME   PIC X(35).
       01  WSF-FAX-QUOTE.
           03  WSF-F-FIL        PIC X(12) VALUE "/ctools/fax/".
           03  WSF-FAX          PIC X(10).
       01  WS-SP-PRINT.
           03  WS-1ST-11CHAR    PIC X(11).
           03  WS-REST          PIC X(20).
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
           03  SP-REST          PIC X(15) VALUE " ".
       01  SPLIT-ANALYSIS.
           03  WSAN-CODE.
               05  WSAN-CODE-1  PIC X VALUE " ".
               05  WSAN-CODE-2  PIC X VALUE " ".
           03  WSAN-REST        PIC X(12) VALUE " ".
       01  ALPHABET-FIELD.
           03  ALPHA-FIELD      PIC X.
           88  ALPHA-VALUE      VALUES ARE "A" THRU "Z".
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
       01  STORE-QUOTE.
         02  WS-QUOTE-OCCUR OCCURS 10.
           03  WS-QUOTE-TYPE       PIC X.
           03  WS-QUOTE-CODE       PIC X.
           03  WS-QUOTE-TERM       PIC X(60).
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 151.
               05  B-STOCKNUMBER.
                   07  B-1ST-CHAR      PIC X.
                   07  B-REST          PIC X(14).
               05  B-NEWLINE           PIC X.
               05  B-STTRANS           PIC 9(6).
               05  B-INVOICED          PIC 9(6).
               05  B-INSTOCK           PIC X.
               05  B-REMAINDER.
                   07  B-ORDERQTY          PIC 9(5).
                   07  B-SHIPQTY           PIC 9(5).
                   07  B-SHIPPEDQTY        PIC 9(5).
                   07  B-STOCKDESCRIPTION  PIC X(20).
                   07  B-STOCKDESCRIPTION2 PIC X(20).
                   07  B-STOCKPRICE        PIC 9(6)V99.
                   07  B-STOCKCOST         PIC 9(6)V99.
                   07  B-DISCOUNTPERITEM   PIC 9(2)V99.
                   07  B-TAX               PIC X.
                   07  B-NETT              PIC 9(6)V99.
                   07  B-UNIT              PIC X(4).
                   07  B-STORE             PIC X(5).
                   07  B-MAX-DISC          PIC 9(2)V99.
                   07  B-SPECIAL           PIC X.
               05  C-LINE REDEFINES B-REMAINDER.
                   07  C-ORDER            PIC X(5).
                   07  C-DESC             PIC X(20).
                   07  C-UNIT             PIC X(4).
                   07  C-PRICE            PIC X(9).
                   07  C-COST             PIC X(9).
                   07  C-DISC             PIC X(5).
                   07  FILLER             PIC X(36).
       01  WS-BODY-LINES.
           03  WS-STOCKNUMBER      PIC X(15).
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
       01  WS-TIME-DISPLAY.
           03  SPLIT-TIME-FIL     PIC X(14).
           03  SPLIT-HR           PIC 99.
           03  SPLIT-HR-FIL       PIC X.
           03  SPLIT-MN           PIC 99.
           03  SPLIT-MN-FIL       PIC X.
           03  SPLIT-SC           PIC 99.
       01  WS-HYLA-TO-LINE.
           03  FILLER             PIC X(15) VALUE " ".
           03  WS-HYLA-TO-NAME    PIC X(25) VALUE " ".
       01 WS-HYLA-FROM-LINE.
           03  FILLER             PIC X(15) VALUE " ".
           03  WS-HYLA-FROM-NAME  PIC X(25) VALUE " ".
           03  FILLER             PIC X(28) VALUE " ".
           03  WS-HYLA-PAGE       PIC Z9 VALUE " ".
       01  WS-HYLA-TYPE-LINE.
           03  FILLER             PIC X(15) VALUE " ".
           03  WS-HYLA-TYPE       PIC X(30) VALUE "*QUOTATION*".
           03  WS-HYLA-DATE       PIC X(30) VALUE " ".
       01  WS-HYLA-COMMENT-LINE.
           03  FILLER             PIC X(15) VALUE " ".
           03 WS-HYLA-COMM-DESC   PIC X(23) VALUE
              "OUR QUOTE REFERENCE #: ".
           03 WS-HYLA-COMMENT     PIC X(17) VALUE " ".
       01  WS-HYLA-TYPE-LINE2.
           03  FILLER             PIC X(15) VALUE " ".
           03  WS-HYLA-TYPE2      PIC X(30) VALUE "*QUOTATION*".
       01 WS-HYLA-FROM-LINE2.
           03  FILLER             PIC X(7) VALUE " ".
           03  WS-HYLA-PAGE2      PIC Z9 VALUE " ".
       01  HEAD1.
           03  H1-1                   PIC XX.
           03  HEAD1-D1               PIC X(19).
           03  HEAD1-NAME             PIC X(40).
           03  HEAD1-D2               PIC X(20).
           03  FILLER                 PIC X(7).
           03  H1-2                   PIC X.
       01  HEAD2.
           03  H2-1                   PIC XX.
           03  HEAD2-D1               PIC X(2).
           03  HEAD2-ADD.
               05  HEAD2-PH-DESC      PIC X(6).
               05  HEAD2-PHONE        PIC X(22).
               05  HEAD2-PH-REQU      PIC X(21).
           03  HEAD2-DEL.
               05  HEAD2-FX-DESC      PIC X(4).
               05  HEAD2-FAX          PIC X(22).
           03  HEAD2-D2               PIC X(2).
           03  FILLER                 PIC X(7).
           03  H2-2                   PIC X.
       01  HEAD3.
           03  H3-1                   PIC XX.
           03  HEAD3-ADD.
               05  HEAD3-ACC          PIC X(11).
               05  HEAD3-NUMBER       PIC X(39).
           03  HEAD3-DEL              PIC X(25).
           03  FILLER                 PIC X(11).
           03  H3-2                   PIC X.
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
       01  WS-STOCK-LINE.
           03  H4-1                   PIC XX.
           03  WSW-STOCKNUMBER        PIC X(15).
           03  WSW-REMAINDER.
               05  WSW-INSTOCK        PIC X.
               05  WSW-DESCRIPTION    PIC X(20).
               05  WSW-DESCRIPTION2   PIC X(20).
               05  WSW-QUANTITY       PIC Z(4)9.
               05  WSW-RAND           PIC X.
               05  WSW-PRICE          PIC Z(6)9.99.
               05  FILLER             PIC X(1).
               05  WSW-DISCOUNT       PIC Z9.99 BLANK WHEN ZERO.
               05  FILLER             PIC X(8).
           03  H4-2                   PIC X.
       01  WSF-STOCK-LINE.
           03  WSF-STOCKNUMBER        PIC X(15).
           03  WSF-REMAINDER.
               05  WSF-INSTOCK        PIC X.
               05  WSF-DESCRIPTION    PIC X(20).
               05  WSF-DESCRIPTION2   PIC X(20).
               05  WSF-QUANTITY       PIC Z(4)9.
               05  WSF-RAND           PIC X.
               05  WSF-PRICE          PIC Z(6)9.99.
               05  FILLER             PIC X(1).
               05  WSF-DISCOUNT       PIC Z9.99 BLANK WHEN ZERO.
               05  FILLER             PIC X(8).
       01  WS-FILE-NAME-FOR-FAX.
           03  WS-FOLDER-NAME         PIC X(12) VALUE "/ctools/fax/".
           03  WS-QUOTE-REFERENCE     PIC X(15) VALUE " ".
       01  WS-DATE-LINE.
      *     03  H5-1                   PIC XX.
           03  FILLER                 PIC X(2) VALUE "*".
           03  FILLER                 PIC X(6) VALUE "DATE:".
           03  WSD-DATE               PIC X(10).
           03  FILLER                 PIC X(12) VALUE " ".
           03  FILLER                 PIC X(22) VALUE
            "OUR QUOTE REFERENCE#:".
           03  WS-QUOTEREF            PIC X(15).
           03  FILLER                 PIC X(2) VALUE " ".
           03  FILLER                 PIC X(5) VALUE "PAGE:".
           03  WSD-PAGE               PIC Z9.
           03  FILLER                 PIC X(2) VALUE " *".
           03  FILLER                 PIC X(8).
      *     03  H5-2                   PIC X.
       01  WSF-DATE-LINE.
           03  H5-1                   PIC XX.
           03  FILLER                 PIC X(2) VALUE "*".
           03  FILLER                 PIC X(6) VALUE "DATE:".
           03  WSF-DATE               PIC X(10).
           03  FILLER                 PIC X(12) VALUE " ".
           03  FILLER                 PIC X(22) VALUE
            "OUR QUOTE REFERENCE#:".
           03  WSF-QUOTEREF           PIC X(15).
           03  FILLER                 PIC X(2) VALUE " ".
           03  FILLER                 PIC X(5) VALUE "PAGE:".
           03  WSF-PAGE               PIC Z9.
           03  FILLER                 PIC X(2) VALUE " *".
           03  FILLER                 PIC X(8).
           03  H5-2                   PIC X.
       01  WS-SALESMAN-LINE.
           03  H6-1                   PIC XX.
           03  WSS-SALES-DESC         PIC X(20).
           03  WSS-SALESMAN           PIC X(30).
           03  WSS-DIS                PIC X(10).
           03  WSS-DISCOUNT           PIC X(20).
           03  FILLER                 PIC X(6).
           03  H6-2                   PIC X.
       01  W-UNDERLINE.
           03  H7E-1            PIC XX.
           03  H7E-UNDER.
              05  FILLER        PIC X(61).
              05  W-UNDER-DESC  PIC X(11).
              05  FILLER        PIC X(6).
           03  FILLER           PIC X(8).
           03  H7E-2            PIC X.
       01  WF-UNDERLINE.
           03  H7-UNDER.
              05  FILLER         PIC X(61).
              05  WF-UNDER-DESC  PIC X(11).
              05  FILLER         PIC X(6).
           03  FILLER            PIC X(8).
       01  WF-CONTINUE-LINE.
           03  H8-1            PIC XX.
           03  FILLER          PIC X(36).
           03  FILLER          PIC X(20) VALUE "Continued to Page #".
           03  WF-NEWF-PAGE    PIC Z9.
           03  FILLER          PIC X(28).
           03  H8-2            PIC X.
       01  WF-COMMENT-LINE.
           03  H9-1            PIC XX.
           03  WF-COMMENT-DESC PIC X(86).
           03  H9-2            PIC X.
       Copy "WsDateInfo".
       Copy "FaxInfo".
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
           PERFORM CLEAR-SCREEN
           PERFORM CLEAR-020.
           MOVE 0320 TO POS
           DISPLAY "** QUOTATION PROGRAM **" AT POS
           MOVE 0420 TO POS
           DISPLAY "***********************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
           MOVE WS-PRINTER TO WS-PRINTER-DOT.
       CONTROL-008.
           MOVE "Y" TO WS-PRINT-SUF-COMMENT
           MOVE 1010 TO POS
           DISPLAY "PRINT COMMENT OF SUFFICIENT STOCK ON HAND, Y/N: [ ]"
           AT POS
           ADD 49 TO POS

           MOVE 'Y'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 58        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PRINT-SUF-COMMENT.

           IF WS-PRINT-SUF-COMMENT NOT = "N" AND NOT = "Y"
              GO TO CONTROL-008.
           MOVE 2910 TO POS
           DISPLAY "Program loading, please be patient ...." AT POS.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN
           PERFORM CLEAR-FIELDS
           MOVE "ON-HAND:"  TO FILLER-ONHAND
           MOVE "ON-RES:"   TO FILLER-ONRES
           MOVE "ON-B/O:"   TO FILLER-ONBO
           MOVE "ON-ORDER:" TO FILLER-ONORD.
       CONT-010.
           MOVE 0 TO FAX-JOBNUMBER
           MOVE " " TO WS-MESSAGE
           MOVE 2601 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2901 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2720 TO POS
           DISPLAY WS-MESSAGE AT POS
           PERFORM DISPLAY-FORM
           PERFORM GET-DATA.
      *<F3> CHANGE OF P-ORDER NUMBER.
           IF F-EXIT-CH = X"17"
              GO TO CONT-020.
           
           MOVE 3010 TO POS
           DISPLAY "WRITING QUOTE REGISTER...............      " AT POS
           PERFORM WRITE-INCR-REGISTER.
           
           MOVE 3010 TO POS
           DISPLAY "WRITING STOCK TRANSACTIONS...........      " AT POS
           PERFORM WRITE-STOCK-TRANSACTIONS.
       CONT-020.
           MOVE " " TO WS-QUOTATION WS-QUOTE-NAME
           PERFORM OPEN-050
           GO TO CONT-010.
       CONT-999.
           EXIT.
      *
       REMOVE-SPACES-IN-FAX-NAME SECTION.
       RSIFN-005.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
           MOVE WS-FILE-NAME-FOR-FAX TO ALPHA-RATE.
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
           MOVE ALPHA-RATE TO WS-PRINTER.
       RSIFN-999.
           EXIT.
      *
       WRITE-ROUTINE SECTION.
       WR-000.
           MOVE 1 TO SUB-1 SUB-2
                     PAGE-CNT.
           MOVE 0 TO COPY-CNT
                     WS-PRICETOTAL.
           MOVE " " TO PRINT-REC.
           PERFORM READ-PARAMETER.
           IF WS-ANSWER = "1" OR = "2"
               MOVE WS-PRINTER-DOT TO WS-MESSAGE
               PERFORM GET-USER-PRINT-NAME.
           
      *     MOVE "IN WRITE SECTION" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     MOVE WS-PRINTER TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE WS-PRINTER-SAVE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     PERFORM ERROR1-020.
      *     MOVE WS-PRINTER-DOT TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR1-020.
               
           OPEN OUTPUT PRINT-FILE.
       WR-001.
           MOVE " " TO WSF-STOCK-LINE HEAD1 PRINT-REC
           MOVE ALL "*" TO HEAD1-D1 HEAD1-D2

           PERFORM CHECK-NAME-LENGTH.
           
           MOVE PA-NAME TO HEAD1-NAME
           WRITE PRINT-REC FROM HEAD1
           MOVE " "     TO PRINT-REC.

           MOVE "*"     TO HEAD2-D1
           MOVE " *"    TO HEAD2-D2
           MOVE PA-ADD1 TO HEAD2-ADD
           MOVE PA-DEL1 TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2
           MOVE " "     TO PRINT-REC.

           MOVE PA-ADD2 TO HEAD2-ADD
           MOVE PA-DEL2 TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2
           MOVE " "     TO PRINT-REC.

           MOVE PA-ADD3 TO HEAD2-ADD
           MOVE PA-DEL3 TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2
           MOVE " "     TO PRINT-REC.

           MOVE PA-CODE TO HEAD2-ADD
           MOVE " "     TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2
           MOVE " "     TO PRINT-REC.

           MOVE "PHONE:" TO HEAD2-PH-DESC
           MOVE "FAX:"   TO HEAD2-FX-DESC
           MOVE PA-PHONE TO HEAD2-PHONE
           MOVE PA-FAX   TO HEAD2-FAX
           WRITE PRINT-REC FROM HEAD2
           MOVE " "      TO HEAD2-ADD HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2.

           MOVE " " TO PRINT-REC HEAD2
           MOVE "********** Q U O T A T I O N ***********" TO HEAD1-NAME
           WRITE PRINT-REC FROM HEAD1
           MOVE " " TO PRINT-REC HEAD1
           WRITE PRINT-REC.

           MOVE "POSTAL ADDRESS"   TO HEAD3-ADD
           MOVE "DELIVERY ADDRESS" TO HEAD3-DEL
           WRITE PRINT-REC FROM HEAD3
           MOVE " "                TO HEAD3 PRINT-REC.

           MOVE "ACCOUNT #:"      TO HEAD3-ACC
           MOVE WS-ACCOUNT-NUMBER TO HEAD3-NUMBER
           WRITE PRINT-REC FROM HEAD3
           MOVE " "               TO HEAD3 PRINT-REC.

           MOVE WS-NAME     TO HEAD3-ADD
           MOVE WS-DELADD1  TO HEAD3-DEL
           WRITE PRINT-REC FROM HEAD3
           MOVE " "         TO HEAD3 PRINT-REC.

           MOVE WS-ADD1     TO HEAD3-ADD
           MOVE WS-DELADD2  TO HEAD3-DEL
           WRITE PRINT-REC FROM HEAD3
           MOVE " "         TO HEAD3 PRINT-REC.

           MOVE WS-ADD2     TO HEAD3-ADD
           MOVE WS-DELADD3  TO HEAD3-DEL
           WRITE PRINT-REC FROM HEAD3
           MOVE " "         TO HEAD3 PRINT-REC.

           IF WS-ADD3 NOT = " "
               MOVE WS-ADD3     TO HEAD3-ADD
               WRITE PRINT-REC FROM HEAD3
               MOVE " "         TO HEAD3 PRINT-REC.

           MOVE WS-POSTCODE TO HEAD3-ADD
           WRITE PRINT-REC FROM HEAD3
           MOVE " "         TO HEAD3 PRINT-REC.
           MOVE "FOR THE ATTENTION OF:" TO HEAD2-PH-REQU
           MOVE WS-CONTACT              TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2
           MOVE " "                     TO PRINT-REC HEAD2.

           MOVE "TELEPHONE NUMBER    :" TO HEAD2-PH-REQU
           MOVE WS-PHONE                TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2
           MOVE " "                     TO PRINT-REC HEAD2.

           MOVE "TENDER/QUOTE REQUEST:" TO HEAD2-PH-REQU
           MOVE WS-POORDERNO            TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2
           MOVE " "                     TO PRINT-REC HEAD2.

           MOVE "CUSTOMER FAX NUMBER :" TO HEAD2-PH-REQU
           MOVE WS-FAX-NUMBER           TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2
           MOVE " "                     TO PRINT-REC HEAD2.
       WR-002.
           MOVE " "     TO PRINT-REC WF-UNDERLINE
           WRITE PRINT-REC
           MOVE ALL "*" TO H7-UNDER
           WRITE PRINT-REC FROM WF-UNDERLINE
           MOVE " "     TO PRINT-REC.
           
           MOVE PAGE-CNT       TO WSD-PAGE
           MOVE WS-INVOICEDATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE   TO WSD-DATE
           WRITE PRINT-REC FROM WS-DATE-LINE.
           
           MOVE " "     TO PRINT-REC WF-UNDERLINE
           MOVE ALL "*" TO H7-UNDER
           WRITE PRINT-REC FROM WF-UNDERLINE
           MOVE " "     TO PRINT-REC WF-UNDERLINE.
           WRITE PRINT-REC.
       WR-003.
           MOVE
           "STOCKNUMBER     DESCRIPTION                          " &
           "     QTY LIST PRICE DISC%" TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           MOVE 7 TO LINE-CNT.
       WR-005.
           ADD 21 TO LINE-CNT
           MOVE 1 TO SUB-1.
       WR-010.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
              MOVE " "                   TO PRINT-REC WSF-STOCK-LINE
              MOVE B-STOCKNUMBER (SUB-1) TO WSF-STOCKNUMBER
              MOVE C-LINE (SUB-1)        TO WSF-REMAINDER
              GO TO WR-015.
           IF WS-NEWORDER = "N"
            IF SP-1STCHAR NOT = "/"
              MOVE B-STOCKNUMBER (SUB-1)    TO ST-STOCKNUMBER
              PERFORM CHECK-STOCK
            IF B-ORDERQTY (SUB-1) NOT > ST-QTYONHAND
                 MOVE "*"                   TO B-INSTOCK (SUB-1).
           MOVE B-STOCKNUMBER (SUB-1)       TO WSF-STOCKNUMBER
           MOVE B-INSTOCK (SUB-1)           TO WSF-INSTOCK
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO WSF-DESCRIPTION
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO WSF-DESCRIPTION2
           MOVE B-ORDERQTY (SUB-1)          TO WSF-QUANTITY
           MOVE B-STOCKPRICE (SUB-1)        TO WSF-PRICE
           MOVE B-DISCOUNTPERITEM (SUB-1)   TO WSF-DISCOUNT
              
           ADD B-NETT (SUB-1)               TO WS-PRICETOTAL.
       WR-015.
           WRITE PRINT-REC FROM WSF-STOCK-LINE
           MOVE " " TO PRINT-REC WSF-STOCK-LINE.
       WR-020.
           ADD 1 TO SUB-1 LINE-CNT.
              
           IF WS-ANSWER = "1" OR = "2" OR = "W"
            IF LINE-CNT > 58
              ADD 1 TO PAGE-CNT
              MOVE PAGE-CNT TO WF-NEWF-PAGE
              WRITE PRINT-REC FROM WF-CONTINUE-LINE
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC BEFORE PAGE
              PERFORM WR-002 THRU WR-003.
              
           IF SUB-1 > 150
              MOVE 
              "150 LINES ARE UP, WE CANNOT PRINT MORE, 'ESC' TO EXIT"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
              GO TO WR-500.
           IF B-STOCKNUMBER (SUB-1) = " "
              GO TO WR-500.
           GO TO WR-010.
       WR-500.
           IF WS-ANSWER = "1" OR = "2" OR = "W"
            IF LINE-CNT > 52
              ADD 1 TO PAGE-CNT
              MOVE PAGE-CNT TO WF-NEWF-PAGE
              WRITE PRINT-REC FROM WF-CONTINUE-LINE
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC BEFORE PAGE
              PERFORM WR-002.
           MOVE " " TO WS-STOCK-LINE PRINT-REC
           WRITE PRINT-REC
           MOVE WS-COMMENTLINE TO PRINT-REC
           WRITE PRINT-REC.
           IF WS-COMMENT1 > " "
              MOVE WS-COMMENT1 TO PRINT-REC
              WRITE PRINT-REC.
           IF WS-COMMENT2 > " "
              MOVE WS-COMMENT2 TO PRINT-REC
              WRITE PRINT-REC.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           IF WS-PRINT-TOTALS = "N"
              MOVE 1 TO SUB-1
              GO TO WR-600.
       WR-550.
           MOVE "      *GROSS QUOTE T" TO WSF-DESCRIPTION
           MOVE "OTAL EXCLUDING VAT *" TO WSF-DESCRIPTION2
           COMPUTE WS-WORKTOTAL = WS-SUBTOTAL + WS-DISCOUNTREG
           MOVE WS-WORKTOTAL           TO WSF-PRICE
           MOVE "R"                    TO WSF-RAND
           WRITE PRINT-REC FROM WSF-STOCK-LINE

           MOVE "      ***** LESS TOT" TO WSF-DESCRIPTION
           MOVE "AL DISCOUNT AMOUNT *" TO WSF-DESCRIPTION2
           MOVE WS-DISCOUNTREG         TO WSF-PRICE
           MOVE "R"                    TO WSF-RAND
           WRITE PRINT-REC FROM WSF-STOCK-LINE
           MOVE " "                    TO WF-UNDERLINE
           MOVE ALL "-"                TO WF-UNDER-DESC
           WRITE PRINT-REC FROM WF-UNDERLINE
           
           MOVE "      * NETT QUOTE T" TO WSF-DESCRIPTION
           MOVE "OTAL EXCLUDING VAT *" TO WSF-DESCRIPTION2
           MOVE WS-SUBTOTAL            TO WSF-PRICE
           MOVE "R"                    TO WSF-RAND
           WRITE PRINT-REC FROM WSF-STOCK-LINE.
           
           IF WS-MISCADDON > 0
               MOVE "      ************MI" TO WSF-DESCRIPTION
               MOVE "SCELLANEOUS ADD ON *" TO WSF-DESCRIPTION2
               MOVE WS-MISCADDON           TO WSF-PRICE
               MOVE "R"                    TO WSF-RAND
               WRITE PRINT-REC FROM WSF-STOCK-LINE.
           
           IF WS-POSTADDON > 0
               MOVE "      **************" TO WSF-DESCRIPTION
               MOVE "**POST & PACKAGING *" TO WSF-DESCRIPTION2
               MOVE WS-POSTADDON           TO WSF-PRICE
               MOVE "R"                    TO WSF-RAND
               WRITE PRINT-REC FROM WSF-STOCK-LINE.
           
           IF WS-HANDADDON > 0
               MOVE "      **************" TO WSF-DESCRIPTION
               MOVE "******LABOUR ADDON *" TO WSF-DESCRIPTION2
               MOVE WS-HANDADDON           TO WSF-PRICE
               MOVE "R"                    TO WSF-RAND
               WRITE PRINT-REC FROM WSF-STOCK-LINE.
           
           IF WS-ADDONFREIGHT > 0
               MOVE "      **************" TO WSF-DESCRIPTION
               MOVE "***FREIGHT CHARGES *" TO WSF-DESCRIPTION2
               MOVE WS-ADDONFREIGHT        TO WSF-PRICE
               MOVE "R"                    TO WSF-RAND
               WRITE PRINT-REC FROM WSF-STOCK-LINE.
           
            MOVE " " TO WS-STOCK-LINE PRINT-REC
            MOVE "      **************" TO WSF-DESCRIPTION
            MOVE "PLUS V.A.T. AMOUNT *" TO WSF-DESCRIPTION2
            MOVE WS-TAXAMT              TO WSF-PRICE
            MOVE "R"                    TO WSF-RAND
            WRITE PRINT-REC FROM WSF-STOCK-LINE.
           
            MOVE " "                    TO WF-UNDERLINE
            MOVE ALL "-"                TO WF-UNDER-DESC
            WRITE PRINT-REC FROM WF-UNDERLINE.
               
            MOVE " " TO WS-STOCK-LINE PRINT-REC
            MOVE "      **************" TO WSF-DESCRIPTION
            MOVE "TOTAL QUOTE AMOUNT *" TO WSF-DESCRIPTION2
            MOVE WS-INVOICETOTAL        TO WSF-PRICE
            MOVE "R"                    TO WSF-RAND
            WRITE PRINT-REC FROM WSF-STOCK-LINE.
            
            MOVE " "                    TO WF-UNDERLINE
            MOVE ALL "="                TO WF-UNDER-DESC
            WRITE PRINT-REC FROM WF-UNDERLINE.

           MOVE " " TO WS-STOCK-LINE WSF-STOCK-LINE PRINT-REC
           WRITE PRINT-REC.
           MOVE 1 TO SUB-1.
       WR-600.
           IF WS-QUOTE-TERM (SUB-1) = " "
              GO TO WR-605.
           MOVE WS-QUOTE-TERM (SUB-1) TO PRINT-REC
           WRITE PRINT-REC.
           IF SUB-1 < 10
             ADD 1 TO SUB-1
             GO TO WR-600.
           MOVE 1 TO SUB-1.
       WR-605.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           IF WS-PRINT-SUF-COMMENT = "Y"
              MOVE
              "'*' IN FRONT OF DESCRIPTION = SUFFICIENT STOCK ON HAND"
              TO PRINT-REC
              WRITE PRINT-REC
              MOVE
              "    AT PRESENT TO FULFILL QUOTE, SUBJECT TO PRIOR SALE."
              TO PRINT-REC
              WRITE PRINT-REC
              MOVE " " TO PRINT-REC.
           WRITE PRINT-REC
           MOVE "QUOTE PREPARED BY:" TO WSS-SALES-DESC
           MOVE WS-SALESMAN          TO WSS-SALESMAN
           WRITE PRINT-REC FROM WS-SALESMAN-LINE
           MOVE " " TO PRINT-REC WS-SALESMAN-LINE
           WRITE PRINT-REC.
           ADD  1 TO COPY-CNT
           MOVE 0 TO WS-PRICETOTAL.
           IF WS-ANSWER = "1" OR = "N" OR = "W"
              GO TO WR-700.
           IF WS-ANSWER = "2"
            IF COPY-CNT = 1
              WRITE PRINT-REC BEFORE PAGE
              MOVE 1 TO PAGE-CNT
              GO TO WR-001.
       WR-700.
           CLOSE PRINT-FILE.

           IF WS-ANSWER = "1" OR = "2"
               PERFORM SEND-REPORT-TO-PRINTER.

      * In the PrintQuote1 shell script:
      * #1 = WS-CO-NUMBER
      * #2 = WS-PRINTER-SAVE (THE PRINTER NAME) E.G. MP140
      * #3 = WS-REFERENCE-NUMBER E.G. Q90555.05.15-1
       WR-999.
           EXIT.
      *
       WRITE-FAX-ROUTINE SECTION.
       WR-FAX-000.
           MOVE 1 TO SUB-1 SUB-2
                     PAGE-CNT.
           MOVE 0 TO COPY-CNT
                     WS-PRICETOTAL.
           MOVE " " TO PRINT-REC.
           PERFORM READ-PARAMETER.
      * Fax-PaNumber = 1 is CTOS Fax SLICE - OLD STYLE
      * Fax-PaNumber = 2 is Murata CTOS Fax
      * Fax-PaNumber = 3 is XQS CTOS fax
      * Fax-PaNumber = 4 is Hylafax Linux Fax
           IF Fax-PaNumber = 3
               MOVE "[QFax]" TO WS-PRINTER.
               
           IF Fax-PaNumber = 4
               MOVE WS-QUOTEREF TO WS-QUOTE-REFERENCE
               PERFORM REMOVE-SPACES-IN-FAX-NAME
               MOVE WS-PRINTER TO WS-PRINTER-PAGE1.
           
           OPEN OUTPUT PRINT-FILE.
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
               WRITE PRINT-REC
               WRITE PRINT-REC.
       WRFAX-001.
           MOVE "POSTAL ADDRESS"   TO HEAD3-ADD
           MOVE "DELIVERY ADDRESS" TO HEAD3-DEL
           WRITE PRINT-REC FROM HEAD3
           MOVE " "                TO HEAD3 PRINT-REC.

           MOVE "ACCOUNT #:"      TO HEAD3-ACC
           MOVE WS-ACCOUNT-NUMBER TO HEAD3-NUMBER
           WRITE PRINT-REC FROM HEAD3
           MOVE " "               TO HEAD3 PRINT-REC.

           MOVE WS-NAME     TO HEAD3-ADD
           MOVE WS-DELADD1  TO HEAD3-DEL
           WRITE PRINT-REC FROM HEAD3
           MOVE " "         TO HEAD3 PRINT-REC.

           MOVE WS-ADD1     TO HEAD3-ADD
           MOVE WS-DELADD2  TO HEAD3-DEL
           WRITE PRINT-REC FROM HEAD3
           MOVE " "         TO HEAD3 PRINT-REC.

           MOVE WS-ADD2     TO HEAD3-ADD
           MOVE WS-DELADD3  TO HEAD3-DEL
           WRITE PRINT-REC FROM HEAD3
           MOVE " "         TO HEAD3 PRINT-REC.

           IF WS-ADD3 NOT = " "
               MOVE WS-ADD3     TO HEAD3-ADD
               WRITE PRINT-REC FROM HEAD3
               MOVE " "         TO HEAD3 PRINT-REC.

           MOVE WS-POSTCODE TO HEAD3-ADD
           WRITE PRINT-REC FROM HEAD3
           MOVE " "         TO HEAD3 PRINT-REC.
 
           MOVE "TELEPHONE NUMBER    :" TO HEAD2-PH-REQU
           MOVE WS-PHONE                TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2
           MOVE " "                     TO PRINT-REC HEAD2.

           MOVE "TENDER/QUOTE REQUEST:" TO HEAD2-PH-REQU
           MOVE WS-POORDERNO            TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2
           MOVE " "                     TO PRINT-REC HEAD2.

           MOVE "CUSTOMER FAX NUMBER :" TO HEAD2-PH-REQU
           MOVE WS-FAX-NUMBER           TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2
           MOVE " "                     TO PRINT-REC HEAD2.
       WRFAX-003.
           MOVE
           "STOCKNUMBER     DESCRIPTION                          " &
           "     QTY LIST PRICE DISC%" TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           MOVE 14 TO LINE-CNT.
       WRFAX-005.
           ADD 17 TO LINE-CNT
           MOVE 1 TO SUB-1.
       WRFAX-010.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
              MOVE " "                   TO PRINT-REC WSF-STOCK-LINE
              MOVE B-STOCKNUMBER (SUB-1) TO WSF-STOCKNUMBER
              MOVE C-LINE (SUB-1)        TO WSF-REMAINDER
              GO TO WRFAX-015.
           IF WS-NEWORDER = "N"
            IF SP-1STCHAR NOT = "/"
              MOVE B-STOCKNUMBER (SUB-1)    TO ST-STOCKNUMBER
              PERFORM CHECK-STOCK
            IF B-ORDERQTY (SUB-1) NOT > ST-QTYONHAND
                 MOVE "*"                   TO B-INSTOCK (SUB-1).
           MOVE B-STOCKNUMBER (SUB-1)       TO WSF-STOCKNUMBER
           MOVE B-INSTOCK (SUB-1)           TO WSF-INSTOCK
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO WSF-DESCRIPTION
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO WSF-DESCRIPTION2
           MOVE B-ORDERQTY (SUB-1)          TO WSF-QUANTITY
           MOVE B-STOCKPRICE (SUB-1)        TO WSF-PRICE
           MOVE B-DISCOUNTPERITEM (SUB-1)   TO WSF-DISCOUNT
              
           ADD B-NETT (SUB-1)               TO WS-PRICETOTAL.
       WRFAX-015.
           WRITE PRINT-REC FROM WSF-STOCK-LINE
           MOVE " " TO PRINT-REC WSF-STOCK-LINE.
       WRFAX-020.
           ADD 1 TO SUB-1 LINE-CNT.
           
           IF Fax-PaNumber = 3
            IF LINE-CNT > 55
              ADD 1 TO PAGE-CNT
              MOVE PAGE-CNT TO WF-NEWF-PAGE
              WRITE PRINT-REC FROM WF-CONTINUE-LINE
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC BEFORE PAGE
              GO TO WRFAX-003.
              
           IF Fax-PaNumber = 4
            IF LINE-CNT > 59
             IF PAGE-CNT = 1
      * THIS NEXT SECTION FOR END OF PAGE 1 GOING TO 2
               ADD 1 TO PAGE-CNT
               MOVE PAGE-CNT TO WF-NEWF-PAGE
               WRITE PRINT-REC FROM WF-CONTINUE-LINE
               MOVE " " TO PRINT-REC
                 CLOSE PRINT-FILE
                 PERFORM REMOVE-SPACES-IN-FAX-NAME
                 MOVE WS-PRINTER TO WS-PRINTER-PAGE2
                 OPEN OUTPUT PRINT-FILE
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM WS-HYLA-TYPE-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 MOVE PAGE-CNT       TO WS-HYLA-PAGE2
                 WRITE PRINT-REC FROM WS-HYLA-FROM-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 PERFORM WRFAX-003
             ELSE
      * THIS NEXT SECTION FOR END OF PAGE 2 ONWARDS........
            IF LINE-CNT > 65
             IF PAGE-CNT > 1
               ADD 1 TO PAGE-CNT
               MOVE PAGE-CNT TO WF-NEWF-PAGE
               WRITE PRINT-REC FROM WF-CONTINUE-LINE
               MOVE " " TO PRINT-REC
                 MOVE " " TO PRINT-REC
                 WRITE PRINT-REC BEFORE PAGE
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM WS-HYLA-TYPE-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 MOVE PAGE-CNT TO WS-HYLA-PAGE2
                 WRITE PRINT-REC FROM WS-HYLA-FROM-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 PERFORM WRFAX-003.
              
           IF SUB-1 > 150
              MOVE 
              "150 LINES ARE UP, WE CANNOT PRINT MORE, 'ESC' TO EXIT"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WRFAX-500.
           IF B-STOCKNUMBER (SUB-1) = " "
               GO TO WRFAX-500.
           GO TO WRFAX-010.
       WRFAX-500.
           IF Fax-PaNumber = 3
            IF LINE-CNT > 45
              ADD 1 TO PAGE-CNT
              MOVE PAGE-CNT TO WF-NEWF-PAGE
              WRITE PRINT-REC FROM WF-CONTINUE-LINE
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC BEFORE PAGE.
              
           IF Fax-PaNumber = 4
      * THIS NEXT SECTION FOR END OF ST-TRANS AND BEGINNING OF THE TOTAL
      * SECTION WHICH REQUIRES 20 LINES TO FINISH
            IF LINE-CNT > 40
               ADD 1 TO PAGE-CNT
               MOVE PAGE-CNT TO WF-NEWF-PAGE
               WRITE PRINT-REC FROM WF-CONTINUE-LINE
               MOVE " " TO PRINT-REC
             IF PAGE-CNT = 2
                 CLOSE PRINT-FILE
                 PERFORM REMOVE-SPACES-IN-FAX-NAME
                 MOVE WS-PRINTER TO WS-PRINTER-PAGE2
                 OPEN OUTPUT PRINT-FILE
                 WRITE PRINT-REC FROM WS-HYLA-TYPE-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 MOVE PAGE-CNT TO WS-HYLA-PAGE2
                 WRITE PRINT-REC FROM WS-HYLA-FROM-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 PERFORM WRFAX-003
             ELSE
      *     IF Fax-PaNumber = 4
      *      IF LINE-CNT > 50
      *         ADD 1 TO PAGE-CNT
      *         MOVE PAGE-CNT TO WF-NEWF-PAGE
      *         WRITE PRINT-REC FROM WF-CONTINUE-LINE
      *         MOVE " " TO PRINT-REC
             IF PAGE-CNT > 2
                 MOVE " " TO PRINT-REC
                 WRITE PRINT-REC BEFORE PAGE
                 WRITE PRINT-REC FROM WS-HYLA-TYPE-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC AFTER 2
                 MOVE PAGE-CNT TO WS-HYLA-PAGE2
                 WRITE PRINT-REC FROM WS-HYLA-FROM-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC.
              
           MOVE " " TO WS-STOCK-LINE PRINT-REC
           WRITE PRINT-REC
           MOVE WS-COMMENTLINE TO PRINT-REC
           WRITE PRINT-REC.
           IF WS-COMMENT1 > " "
              MOVE WS-COMMENT1 TO PRINT-REC
              WRITE PRINT-REC.
           IF WS-COMMENT2 > " "
              MOVE WS-COMMENT2 TO PRINT-REC
              WRITE PRINT-REC.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           IF WS-PRINT-TOTALS = "N"
              MOVE 1 TO SUB-1
              GO TO WRFAX-600.
       WRFAX-550.
           MOVE "      *GROSS QUOTE T" TO WSF-DESCRIPTION
           MOVE "OTAL EXCLUDING VAT *" TO WSF-DESCRIPTION2
           COMPUTE WS-WORKTOTAL = WS-SUBTOTAL + WS-DISCOUNTREG
           MOVE WS-WORKTOTAL           TO WSF-PRICE
           MOVE "R"                    TO WSF-RAND
           WRITE PRINT-REC FROM WSF-STOCK-LINE

           MOVE "      ***** LESS TOT" TO WSF-DESCRIPTION
           MOVE "AL DISCOUNT AMOUNT *" TO WSF-DESCRIPTION2
           MOVE WS-DISCOUNTREG         TO WSF-PRICE
           MOVE "R"                    TO WSF-RAND
           WRITE PRINT-REC FROM WSF-STOCK-LINE
           MOVE " "                    TO WF-UNDERLINE
           MOVE ALL "-"                TO WF-UNDER-DESC
           WRITE PRINT-REC FROM WF-UNDERLINE
           
           MOVE "      * NETT QUOTE T" TO WSF-DESCRIPTION
           MOVE "OTAL EXCLUDING VAT *" TO WSF-DESCRIPTION2
           MOVE WS-SUBTOTAL            TO WSF-PRICE
           MOVE "R"                    TO WSF-RAND
           WRITE PRINT-REC FROM WSF-STOCK-LINE.
           
           IF WS-MISCADDON > 0
               MOVE "      ************MI" TO WSF-DESCRIPTION
               MOVE "SCELLANEOUS ADD ON *" TO WSF-DESCRIPTION2
               MOVE WS-MISCADDON           TO WSF-PRICE
               MOVE "R"                    TO WSF-RAND
               WRITE PRINT-REC FROM WSF-STOCK-LINE.
           
           IF WS-POSTADDON > 0
               MOVE "      **************" TO WSF-DESCRIPTION
               MOVE "**POST & PACKAGING *" TO WSF-DESCRIPTION2
               MOVE WS-POSTADDON           TO WSF-PRICE
               MOVE "R"                    TO WSF-RAND
               WRITE PRINT-REC FROM WSF-STOCK-LINE.
           
           IF WS-HANDADDON > 0
               MOVE "      **************" TO WSF-DESCRIPTION
               MOVE "******LABOUR ADDON *" TO WSF-DESCRIPTION2
               MOVE WS-HANDADDON           TO WSF-PRICE
               MOVE "R"                    TO WSF-RAND
               WRITE PRINT-REC FROM WSF-STOCK-LINE.
           
           IF WS-ADDONFREIGHT > 0
               MOVE "      **************" TO WSF-DESCRIPTION
               MOVE "***FREIGHT CHARGES *" TO WSF-DESCRIPTION2
               MOVE WS-ADDONFREIGHT        TO WSF-PRICE
               MOVE "R"                    TO WSF-RAND
               WRITE PRINT-REC FROM WSF-STOCK-LINE.
           
            MOVE " " TO WS-STOCK-LINE PRINT-REC
            MOVE "      **************" TO WSF-DESCRIPTION
            MOVE "PLUS V.A.T. AMOUNT *" TO WSF-DESCRIPTION2
            MOVE WS-TAXAMT              TO WSF-PRICE
            MOVE "R"                    TO WSF-RAND
            WRITE PRINT-REC FROM WSF-STOCK-LINE.
           
            MOVE " "                    TO WF-UNDERLINE
            MOVE ALL "-"                TO WF-UNDER-DESC
            WRITE PRINT-REC FROM WF-UNDERLINE.
               
            MOVE " " TO WS-STOCK-LINE PRINT-REC
            MOVE "      **************" TO WSF-DESCRIPTION
            MOVE "TOTAL QUOTE AMOUNT *" TO WSF-DESCRIPTION2
            MOVE WS-INVOICETOTAL        TO WSF-PRICE
            MOVE "R"                    TO WSF-RAND
            WRITE PRINT-REC FROM WSF-STOCK-LINE.
            
            MOVE " "                    TO WF-UNDERLINE
            MOVE ALL "="                TO WF-UNDER-DESC
            WRITE PRINT-REC FROM WF-UNDERLINE.

           MOVE " " TO WS-STOCK-LINE WSF-STOCK-LINE PRINT-REC
           WRITE PRINT-REC.
           MOVE 1 TO SUB-1.
       WRFAX-600.
           IF WS-QUOTE-TERM (SUB-1) = " "
              GO TO WRFAX-605.
           MOVE WS-QUOTE-TERM (SUB-1) TO PRINT-REC
           WRITE PRINT-REC.
           IF SUB-1 < 10
             ADD 1 TO SUB-1
             GO TO WRFAX-600.
           MOVE 1 TO SUB-1.
       WRFAX-605.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           IF WS-PRINT-SUF-COMMENT = "Y"
              MOVE
              "'*' IN FRONT OF DESCRIPTION = SUFFICIENT STOCK ON HAND"
              TO PRINT-REC
              WRITE PRINT-REC
              MOVE
              "    AT PRESENT TO FULFILL QUOTE, SUBJECT TO PRIOR SALE."
              TO PRINT-REC
              WRITE PRINT-REC
              MOVE " " TO PRINT-REC.
           WRITE PRINT-REC
           MOVE "QUOTE PREPARED BY:" TO WSS-SALES-DESC
           MOVE WS-SALESMAN          TO WSS-SALESMAN
           WRITE PRINT-REC FROM WS-SALESMAN-LINE
           MOVE " " TO PRINT-REC WS-SALESMAN-LINE
           WRITE PRINT-REC.
      *********************************************************
      * FAX-JOBNUMBER IS ONLY FOR EPS TYPE FAXES - CODE 1 & 2 *
      *********************************************************
           IF FAX-JOBNUMBER NOT = 0
               MOVE " JOB No#:"      TO WSS-DIS
               MOVE F-NAMEFIELDJOB   TO WSS-DISCOUNT
               WRITE PRINT-REC FROM  WS-SALESMAN-LINE
               MOVE " " TO PRINT-REC WS-SALESMAN-LINE.
           ADD  1 TO COPY-CNT
           MOVE 0 TO WS-PRICETOTAL.
       WRFAX-700.
           IF Fax-PaNumber = 3 OR = 4
                PERFORM GET-USER-MAIL-NAME
                PERFORM GET-REPORT-Y2K-DATE
                PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
      * IF PAGE-CNT > 2 WE MOVE 2 TO PAGE-CNT AS THERE ARE ONLY 
      * TWO FILES CREATED - 1 AND 2.  2 HAS ALL THE SUBSEQUENT PAGES
      * INSIDE IT.
           IF PAGE-CNT > 2 
              MOVE 2 TO PAGE-CNT.

      * In the PrintQuote1 shell script:
      * #1 = WS-CO-NUMBER
      * #2 = WS-PRINTER-SAVE (THE PRINTER NAME) E.G. MP140
      * #3 = WS-REFERENCE-NUMBER E.G. Q90555.05.15-1

           IF Fax-PaNumber = 4
            IF PAGE-CNT = 1
                 PERFORM WORK-OUT-PDF-FILE-NAMES
                 MOVE WS-PRINTER-PAGE1   TO WS-PRINTER
                 PERFORM FIND-PDF-TYPE-PRINTER
                 PERFORM SETUP-QUOTE-FOR-PDF
            ELSE
                 PERFORM WORK-OUT-PDF-FILE-NAMES
                 MOVE WS-PRINTER-PAGE1   TO WS-PRINTER
                 PERFORM FIND-PDF-TYPE-PRINTER
                 PERFORM SETUP-QUOTE-FOR-PDF
                 
                 MOVE WS-PRINTER-PAGE2   TO WS-PRINTER
                 PERFORM SETUP-QUOTE2-FOR-PDF
                 PERFORM SETUP-MERGE-QUOTE-FOR-PDF.
       WRFAX-999.
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
                                                WS-PRINTER-PDF
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
       WRITE-EMAIL-ROUTINE SECTION.
       WEMR-000.
           MOVE 1 TO SUB-1 
                     SUB-2
                     LINE-CNT
                     PAGE-CNT.
           MOVE 0 TO COPY-CNT
                     WS-PRICETOTAL.
           MOVE " " TO PRINT-REC.
           OPEN OUTPUT PRINT-FILE.
           
      *     MOVE "IN WEMR WRITE MAIL SECTION" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     MOVE WS-PRINTER TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE WS-PRINTER-SAVE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR1-020.

           PERFORM Z1-HEADINGS.
           
           PERFORM READ-PARAMETER.
       WEMR-00010.
           MOVE "뉜"         TO WS-DELIM-F
           MOVE "�"          TO H1-1
           MOVE "�"          TO H1-2.
       WEMR-00011.
           MOVE "�"          TO H9-1
           MOVE "�"          TO H9-2.
           IF PAGE-CNT = 1
                MOVE WSF-MAIL-NUMBER TO WS-DATA-F
           ELSE
                MOVE SPACES          TO WS-DATA-F.
           WRITE PRINT-REC FROM WS-FST-LINE AFTER 1.
       WEMR-001.
           MOVE " " TO WS-STOCK-LINE HEAD1 PRINT-REC
           MOVE ALL "*" TO HEAD1-D1 HEAD1-D2.

           IF PAGE-CNT = 1
               PERFORM CHECK-NAME-LENGTH.
           
           MOVE "�"          TO H1-1
           MOVE "�"          TO H1-2.
           MOVE PA-NAME TO HEAD1-NAME
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC.

           MOVE "�"          TO H2-1
           MOVE "�"          TO H2-2.
           MOVE "*"  TO HEAD2-D1
           MOVE " *" TO HEAD2-D2
           MOVE PA-ADD1 TO HEAD2-ADD
           MOVE PA-DEL1 TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC.

           MOVE PA-ADD2 TO HEAD2-ADD
           MOVE PA-DEL2 TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC.

           MOVE PA-ADD3 TO HEAD2-ADD
           MOVE PA-DEL3 TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC.

           MOVE PA-CODE TO HEAD2-ADD
           MOVE " "     TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC.

           MOVE "PHONE:" TO HEAD2-PH-DESC
           MOVE "FAX:"   TO HEAD2-FX-DESC
           MOVE PA-PHONE TO HEAD2-PHONE
           MOVE PA-FAX   TO HEAD2-FAX
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO HEAD2-ADD HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2 AFTER 1.

           MOVE "�"          TO H1-1
           MOVE "�"          TO H1-2.
           MOVE " " TO PRINT-REC HEAD2
           MOVE "********** Q U O T A T I O N ***********" TO HEAD1-NAME
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC HEAD1.
       WEMR-001X.
           MOVE " "          TO PRINT-REC
           MOVE "�"          TO H2-1
           MOVE "�"          TO H2-2
           WRITE PRINT-REC FROM HEAD2 AFTER 1

           MOVE "�"                TO H3-1
           MOVE "�"                TO H3-2
           MOVE "POSTAL ADDRESS"   TO HEAD3-ADD
           MOVE "DELIVERY ADDRESS" TO HEAD3-DEL
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO HEAD3 PRINT-REC.

           MOVE "�"                TO H3-1
           MOVE "�"                TO H3-2
           MOVE "ACCOUNT #:"       TO HEAD3-ACC
           MOVE WS-ACCOUNT-NUMBER  TO HEAD3-NUMBER
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO HEAD3 PRINT-REC.

           MOVE "�"                TO H3-1
           MOVE "�"                TO H3-2
           MOVE WS-NAME            TO HEAD3-ADD
           MOVE WS-DELADD1         TO HEAD3-DEL
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO HEAD3 PRINT-REC.

           MOVE "�"                TO H3-1
           MOVE "�"                TO H3-2
           MOVE WS-ADD1            TO HEAD3-ADD
           MOVE WS-DELADD2         TO HEAD3-DEL
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO HEAD3 PRINT-REC.

           MOVE "�"                TO H3-1
           MOVE "�"                TO H3-2
           MOVE WS-ADD2            TO HEAD3-ADD
           MOVE WS-DELADD3         TO HEAD3-DEL
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO HEAD3 PRINT-REC.

           IF WS-ADD3 NOT = " "
               MOVE "�"                TO H3-1
               MOVE "�"                TO H3-2
               MOVE WS-ADD3            TO HEAD3-ADD
               WRITE PRINT-REC FROM HEAD3 AFTER 1
               MOVE " " TO HEAD3 PRINT-REC.

           MOVE "�"                TO H3-1
           MOVE "�"                TO H3-2
           MOVE WS-POSTCODE        TO HEAD3-ADD
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO HEAD3 PRINT-REC.

           IF WS-ADD3 = " "
              MOVE " "          TO PRINT-REC
              MOVE "�"          TO H2-1
              MOVE "�"          TO H2-2
              WRITE PRINT-REC FROM HEAD2 AFTER 1.

           MOVE "�"                     TO H2-1
           MOVE "�"                     TO H2-2
           MOVE "FOR THE ATTENTION OF:" TO HEAD2-PH-REQU
           MOVE WS-CONTACT              TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC HEAD2.
       WEMR-001X2.
           MOVE "�"                     TO H2-1
           MOVE "�"                     TO H2-2
           MOVE "TELEPHONE NUMBER    :" TO HEAD2-PH-REQU
           MOVE WS-PHONE                TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC HEAD2.

           MOVE "�"                     TO H2-1
           MOVE "�"                     TO H2-2
           MOVE "TENDER/QUOTE REQUEST:" TO HEAD2-PH-REQU
           MOVE WS-POORDERNO            TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC HEAD2.

           MOVE "�"                     TO H2-1
           MOVE "�"                     TO H2-2
           MOVE "CUSTOMER FAX NUMBER :" TO HEAD2-PH-REQU
           MOVE WS-FAX-NUMBER           TO HEAD2-DEL
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC HEAD2.
       WEMR-002.
           MOVE " "          TO PRINT-REC
           MOVE "�"          TO H2-1
           MOVE "�"          TO H2-2
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           
           MOVE ALL "*"      TO H7E-UNDER
           MOVE "� "         TO H7E-1
           MOVE "�"          TO H7E-2
           WRITE PRINT-REC FROM W-UNDERLINE AFTER 1
           MOVE " " TO PRINT-REC.
           
           MOVE "�"            TO H5-1
           MOVE "�"            TO H5-2
           MOVE PAGE-CNT       TO WSF-PAGE
           MOVE WS-INVOICEDATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE   TO WSF-DATE.
           WRITE PRINT-REC FROM WSF-DATE-LINE AFTER 1
           
           MOVE " "            TO PRINT-REC
           MOVE ALL "*"        TO H7-UNDER
           MOVE "� "           TO H7E-1
           MOVE "�"            TO H7E-2
           WRITE PRINT-REC FROM W-UNDERLINE AFTER 1
           
           MOVE " " TO PRINT-REC W-UNDERLINE.
           MOVE "�"            TO H7E-1
           MOVE "�"            TO H7E-2
           WRITE PRINT-REC FROM W-UNDERLINE AFTER 1.
       WEMR-003.
           MOVE 0 TO LINE-CNT.
           
           MOVE " " TO PRINT-REC WF-COMMENT-LINE.
           MOVE "�"            TO H9-1
           MOVE "�"            TO H9-2
           MOVE
           "STOCKNUMBER     DESCRIPTION                          " &
           "     QTY LIST PRICE DISC%" TO WF-COMMENT-DESC
           WRITE PRINT-REC FROM WF-COMMENT-LINE AFTER 1
           MOVE " " TO PRINT-REC W-UNDERLINE.
       WEMR-005.
           MOVE 0 TO LINE-CNT.
           MOVE 1 TO SUB-1.
       WEMR-010.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
              MOVE " "                    TO PRINT-REC WS-STOCK-LINE
              MOVE B-STOCKNUMBER (SUB-1)  TO WSW-STOCKNUMBER
              MOVE C-LINE (SUB-1)         TO WSW-REMAINDER
              GO TO WEMR-015.
           IF WS-NEWORDER = "N"
            IF SP-1STCHAR NOT = "/"
              MOVE B-STOCKNUMBER (SUB-1) TO ST-STOCKNUMBER
              PERFORM CHECK-STOCK
            IF B-ORDERQTY (SUB-1) NOT > ST-QTYONHAND
                 MOVE "*" TO B-INSTOCK (SUB-1).
           MOVE B-STOCKNUMBER (SUB-1)       TO WSW-STOCKNUMBER
           MOVE B-INSTOCK (SUB-1)           TO WSW-INSTOCK
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO WSW-DESCRIPTION
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO WSW-DESCRIPTION2
           MOVE B-ORDERQTY (SUB-1)          TO WSW-QUANTITY
           MOVE B-STOCKPRICE (SUB-1)        TO WSW-PRICE
           MOVE B-DISCOUNTPERITEM (SUB-1)   TO WSW-DISCOUNT
           ADD B-NETT (SUB-1)               TO WS-PRICETOTAL.
       WEMR-015.
           MOVE "�"            TO H4-1
           MOVE "�"            TO H4-2
           WRITE PRINT-REC FROM WS-STOCK-LINE AFTER 1
           MOVE " " TO WS-STOCK-LINE PRINT-REC.
       WEMR-020.
           ADD 1 TO SUB-1 LINE-CNT.
              
           IF LINE-CNT > 30
              ADD 1               TO PAGE-CNT
              PERFORM WEMR-00010 THRU WEMR-003
              MOVE 0 TO LINE-CNT.
              
           IF SUB-1 > 150
              GO TO WEMR-400.
           IF B-STOCKNUMBER (SUB-1) = " "
              GO TO WEMR-400.
           GO TO WEMR-010.
       WEMR-400.
           IF LINE-CNT < 12
              GO TO WEMR-500.
           IF LINE-CNT NOT > 30
              MOVE " " TO PRINT-REC W-UNDERLINE
              MOVE "�"            TO H7E-1
              MOVE "�"            TO H7E-2
              WRITE PRINT-REC FROM W-UNDERLINE AFTER 1
              ADD 1 TO LINE-CNT  
              GO TO WEMR-400.
       WEMR-500.
           IF LINE-CNT > 30
              ADD 1            TO PAGE-CNT
              PERFORM WEMR-00010 THRU WEMR-002
              MOVE " " TO PRINT-REC W-UNDERLINE
              MOVE "�"         TO H7E-1
              MOVE "�"         TO H7E-2
              WRITE PRINT-REC FROM W-UNDERLINE AFTER 1
              MOVE 0 TO LINE-CNT.
           
           MOVE "�*"           TO H9-1
           MOVE "�"            TO H9-2
           MOVE WS-COMMENTLINE TO WF-COMMENT-DESC
           WRITE PRINT-REC FROM WF-COMMENT-LINE AFTER 1
           
           MOVE WS-COMMENT1 TO WF-COMMENT-DESC
           WRITE PRINT-REC FROM WF-COMMENT-LINE AFTER 1
           
           MOVE WS-COMMENT2 TO WF-COMMENT-DESC
           WRITE PRINT-REC FROM WF-COMMENT-LINE AFTER 1.
           
           MOVE " " TO PRINT-REC W-UNDERLINE
           MOVE "�"            TO H7E-1
           MOVE "�"            TO H7E-2
           WRITE PRINT-REC FROM W-UNDERLINE AFTER 1.
           IF WS-PRINT-TOTALS = "N"
              MOVE 1 TO SUB-1
              GO TO WEMR-600.
           
           MOVE "�"                    TO H4-1
           MOVE "�"                    TO H4-2
           MOVE "      *GROSS QUOTE T" TO WSW-DESCRIPTION
           MOVE "OTAL EXCLUDING VAT *" TO WSW-DESCRIPTION2
           COMPUTE WS-WORKTOTAL = WS-SUBTOTAL + WS-DISCOUNTREG
           MOVE WS-WORKTOTAL           TO WSW-PRICE
           MOVE "R"                    TO WSW-RAND
           WRITE PRINT-REC FROM WS-STOCK-LINE AFTER 1.
           
           MOVE "      ***** LESS TOT" TO WSW-DESCRIPTION
           MOVE "AL DISCOUNT AMOUNT *" TO WSW-DESCRIPTION2
           MOVE WS-DISCOUNTREG         TO WSW-PRICE
           MOVE "R"                    TO WSW-RAND
           WRITE PRINT-REC FROM WS-STOCK-LINE AFTER 1
           
           MOVE " "                    TO W-UNDERLINE
           MOVE "�"                    TO H7E-1
           MOVE "�"                    TO H7E-2
           MOVE ALL "-"                TO W-UNDER-DESC
           WRITE PRINT-REC FROM W-UNDERLINE AFTER 1.
           
           MOVE "�"                    TO H4-1
           MOVE "�"                    TO H4-2
           MOVE "      * NETT QUOTE T" TO WSW-DESCRIPTION
           MOVE "OTAL EXCLUDING VAT *" TO WSW-DESCRIPTION2
           MOVE WS-SUBTOTAL            TO WSW-PRICE
           MOVE "R"                    TO WSW-RAND
           WRITE PRINT-REC FROM WS-STOCK-LINE AFTER 1.
           
           IF WS-MISCADDON > 0
           MOVE "      ************MI" TO WSW-DESCRIPTION
           MOVE "SCELLANEOUS ADD ON *" TO WSW-DESCRIPTION2
           MOVE WS-MISCADDON           TO WSW-PRICE
           MOVE "R"                    TO WSW-RAND
           WRITE PRINT-REC FROM WS-STOCK-LINE AFTER 1.
           
           IF WS-POSTADDON > 0
           MOVE "      **************" TO WSW-DESCRIPTION
           MOVE "**POST & PACKAGING *" TO WSW-DESCRIPTION2
           MOVE WS-POSTADDON           TO WSW-PRICE
           MOVE "R"                    TO WSW-RAND
           WRITE PRINT-REC FROM WS-STOCK-LINE AFTER 1.
           
           IF WS-HANDADDON > 0
           MOVE "      **************" TO WSW-DESCRIPTION
           MOVE "******LABOUR ADDON *" TO WSW-DESCRIPTION2
           MOVE WS-HANDADDON           TO WSW-PRICE
           MOVE "R"                    TO WSW-RAND
           WRITE PRINT-REC FROM WS-STOCK-LINE AFTER 1.
           
           IF WS-ADDONFREIGHT > 0
           MOVE "      **************" TO WSW-DESCRIPTION
           MOVE "***FREIGHT CHARGES *" TO WSW-DESCRIPTION2
           MOVE WS-ADDONFREIGHT        TO WSW-PRICE
           MOVE "R"                    TO WSW-RAND
           WRITE PRINT-REC FROM WS-STOCK-LINE AFTER 1.
           
           MOVE " " TO WS-STOCK-LINE PRINT-REC
           MOVE "�"                    TO H4-1
           MOVE "�"                    TO H4-2
           MOVE "      **************" TO WSW-DESCRIPTION
           MOVE "PLUS V.A.T. AMOUNT *" TO WSW-DESCRIPTION2
           MOVE WS-TAXAMT              TO WSW-PRICE
           MOVE "R"                    TO WSW-RAND
           WRITE PRINT-REC FROM WS-STOCK-LINE AFTER 1.
           
           MOVE " "                    TO W-UNDERLINE
           MOVE "�"                    TO H7E-1
           MOVE "�"                    TO H7E-2
           MOVE ALL "-"                TO W-UNDER-DESC
           WRITE PRINT-REC FROM W-UNDERLINE AFTER 1.
           
           MOVE " " TO WS-STOCK-LINE PRINT-REC
           MOVE "�"                    TO H4-1
           MOVE "�"                    TO H4-2
           MOVE "      **************" TO WSW-DESCRIPTION
           MOVE "TOTAL QUOTE AMOUNT *" TO WSW-DESCRIPTION2
           MOVE WS-INVOICETOTAL        TO WSW-PRICE
           MOVE "R"                    TO WSW-RAND
           WRITE PRINT-REC FROM WS-STOCK-LINE AFTER 1.
           
           MOVE " "                    TO W-UNDERLINE
           MOVE "�"                    TO H7E-1
           MOVE "�"                    TO H7E-2
           MOVE ALL "="                TO W-UNDER-DESC
           WRITE PRINT-REC FROM W-UNDERLINE AFTER 1.
           
           MOVE " " TO PRINT-REC W-UNDERLINE
           MOVE "�"            TO H7E-1
           MOVE "�"            TO H7E-2
           WRITE PRINT-REC FROM W-UNDERLINE AFTER 1.
           MOVE 1 TO SUB-1.
       WEMR-600.
           IF WS-QUOTE-TERM (SUB-1) = " "
              GO TO WEMR-605.
           MOVE "�"                    TO H9-1
           MOVE "�"                    TO H9-2
           MOVE WS-QUOTE-TERM (SUB-1) TO WF-COMMENT-DESC
           WRITE PRINT-REC FROM WF-COMMENT-LINE AFTER 1.
           IF SUB-1 < 10
             ADD 1 TO SUB-1 LINE-CNT
             GO TO WEMR-600.
           MOVE 1 TO SUB-1.
       WEMR-605.
           MOVE " " TO PRINT-REC W-UNDERLINE
           MOVE "�"            TO H7E-1
           MOVE "�"            TO H7E-2
           WRITE PRINT-REC FROM W-UNDERLINE AFTER 1.

           MOVE " " TO PRINT-REC
           MOVE "�"                    TO H9-1
           MOVE "�"                    TO H9-2.
           IF WS-PRINT-SUF-COMMENT = "Y"
              MOVE
              "'*' IN FRONT OF DESCRIPTION = SUFFICIENT STOCK ON HAND"
              TO WF-COMMENT-DESC
              WRITE PRINT-REC FROM WF-COMMENT-LINE AFTER 1
              MOVE
              "    AT PRESENT TO FULFILL QUOTE, SUBJECT TO PRIOR SALE."
              TO WF-COMMENT-DESC
              WRITE PRINT-REC FROM WF-COMMENT-LINE AFTER 1
              ADD 2    TO LINE-CNT
              MOVE " " TO PRINT-REC.

           MOVE "�"                  TO H6-1
           MOVE "�"                  TO H6-2.
           MOVE "QUOTE PREPARED BY:" TO WSS-SALES-DESC
           MOVE WS-SALESMAN          TO WSS-SALESMAN
           WRITE PRINT-REC FROM WS-SALESMAN-LINE AFTER 1
           MOVE " " TO PRINT-REC WS-SALESMAN-LINE.
           ADD 16 TO LINE-CNT.
       WEMR-650.
           IF LINE-CNT NOT > 30
              MOVE " " TO PRINT-REC W-UNDERLINE
              MOVE "�"            TO H7E-1
              MOVE "�"            TO H7E-2
              WRITE PRINT-REC FROM W-UNDERLINE AFTER 1
              ADD 1 TO LINE-CNT  
              GO TO WEMR-650.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM W-UNDERLINE.
       WEMR-700.
           CLOSE PRINT-FILE.
       WEMR-999.
           EXIT.
      *
       CHANGE-PORDER SECTION.
       CPOR-010.
           PERFORM CHECK-PASSWORD.
           IF WS-PASSWORD-VALID = "N"
              MOVE "INCORRECT PASSWORD ENTERED, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE X"17" TO F-EXIT-CH
              GO TO CPOR-999.
           MOVE "          " TO ALPHA-RATE
           MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-INVOICE
           PERFORM READ-INVOICE-REGISTER.
           IF WS-NEWORDER = "N" OR = "P"
             PERFORM READ-STOCK-TRANSACTIONS
             PERFORM FIND-INFO
           ELSE
             MOVE "THIS QUOTE IS COMPLETE, 'ESC' TO EXIT."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO CPOR-999.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
       CPOR-020.
           MOVE "POORDERNO" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD
           MOVE 20          TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD TO WS-POORDERNO.
           IF WS-POORDERNO NOT = INCR-PORDER
               PERFORM CHECK-REGISTER
           ELSE
               PERFORM RIR-005
               GO TO CPOR-900.
           IF WS-NEWORDER = "N"
              MOVE
           "THERE IS A P/SLIP WITH THIS P-ORDER FOR THIS ACC, " &
           "'ESC' TO RE-ENTER." TO WS-MESSAGE
           PERFORM ERROR-MESSAGE
           MOVE " " TO INCR-PORDER
              GO TO CPOR-020.
           MOVE "N" TO WS-NEWORDER
                       WS-ORDER-COMPLETE.
       CPOR-900.
           PERFORM WRITE-INCR-REGISTER.
           MOVE X"17" TO F-EXIT-CH.
       CPOR-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-010.
      *      PERFORM CI-900.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "N" TO WS-ZERODIS
                        WS-LINECHANGED
                        WS-CODETAB
                        WS-MUST-PRINT.
            MOVE "Y" TO WS-NEWORDER.
            MOVE " " TO WS-MESSAGE
                        WS-ABOVE-BODY
                        WS-BELOWF-BODY
                        WS-DIS
                        WS-SOLD-BY.
            PERFORM ERROR-020
            PERFORM ERROR1-020.
           MOVE 2910 TO POS
           DISPLAY
           "ENTER A/C # & <RETURN> TO CREATE A QUOTE, ENTER QUOTE " &
           "# & <F5>" AT POS
           MOVE 3010 TO POS
           DISPLAY
           " TO CHANGE QUOTE, OR ENTER QUOTE # & <F3> TO CHANGE " &
           "P/ORDER #." AT POS.
            MOVE "ACCOUNTNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"14"
             IF WS-IMM-PR = "Y"
                MOVE "Press 'ALT-C' To Reprint This Quote."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-010.
            IF F-EXIT-CH = X"94"
             IF WS-IMM-PR = "Y"
                PERFORM WRITE-ROUTINE
                GO TO GET-010.
            MOVE " " TO WS-NAMEANDADDRESS
                        WS-ACCNO-X
                        WS-BINNO WS-REPRINT WS-QUOTEREF.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            MOVE 0 TO DR-ACCOUNT-NUMBER
                      WS-ADDONFREIGHT WS-POSTADDON
                      WS-HANDADDON WS-MISCADDON
                      WS-POSTCODE
                      WS-INVOICEDISCOUNT.
            MOVE 1 TO SUB-20 SUB-25.
            PERFORM CLEAR-FIELDS.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO F-NAMEFIELDRED.

            IF F-NAMEFIELDRED = "STOCK  "
                CLOSE STOCK-MASTER
                CALL WS-STOCK-INQUIRY USING WS-LINKAGE
                CANCEL WS-STOCK-INQUIRY
                PERFORM CLEAR-SCREEN
                PERFORM OPEN-012
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD F-NAMEFIELDRED
                GO TO GET-010.
            IF F-NAMEFIELDRED = "PORDER "
                CLOSE STOCK-MASTER
                CALL WS-PORDER-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-PORDER-INQUIRY
                PERFORM OPEN-012
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD F-NAMEFIELDRED
                GO TO GET-010.
            IF F-NAMEFIELDRED = "QUOTEAC"
                CLOSE DEBTOR-MASTER
                CLOSE STOCK-TRANS-FILE
                CLOSE INCR-REGISTER
                CALL WS-QUOTE-ACINQ USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-QUOTE-ACINQ
                PERFORM OPEN-011
                PERFORM OPEN-015
                PERFORM OPEN-016
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD F-NAMEFIELDRED
                GO TO GET-010.
            IF F-NAMEFIELDRED = "QUOTEST"
                CLOSE DEBTOR-MASTER
                CLOSE STOCK-TRANS-FILE
                CLOSE INCR-REGISTER
                CALL WS-QUOTE-STINQ USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-QUOTE-STINQ
                PERFORM OPEN-011
                PERFORM OPEN-015
                PERFORM OPEN-016
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD F-NAMEFIELDRED
                GO TO GET-010.
            IF F-NAMEFIELDRED1 = "S"
                AND F-NAMEFIELDRED7 NOT = "TOCK"
                   DISPLAY " " AT 3079 WITH BELL
                   GO TO GET-010.
      *****************************************************
      * 'F3' KEY, TO FIND QUOTE & CHANGE P/ORDER NUMBER   *
      *****************************************************
           IF F-EXIT-CH = X"17"
            IF F-NAMEFIELDRED1 = "Q"
               PERFORM CHANGE-PORDER
               GO TO GET-999.
              
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           IF F-EXIT-CH = X"19" OR = X"1F"
            IF F-NAMEFIELDRED1 NOT = "Q"
                GO TO GET-010.
           IF F-EXIT-CH = X"1F"
                MOVE X"11" TO F-EXIT-CH
                PERFORM CHECK-PASSWORD
                MOVE X"1F" TO F-EXIT-CH
            IF WS-PASSWORD-VALID = "N"
               GO TO GET-010.
      * 'F5' KEY, TO FIND OLD QUOTE AND DISPLAY
           IF F-EXIT-CH = X"19"
                MOVE "          " TO ALPHA-RATE
                MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-INVOICE
                PERFORM READ-INVOICE-REGISTER.
           IF F-EXIT-CH = X"19"
            IF WS-NEWORDER = "N"
                PERFORM READ-STOCK-TRANSACTIONS
                PERFORM ERROR1-020
                PERFORM FIND-INFO
                GO TO GET-150.
           IF F-EXIT-CH = X"19" OR = X"9B" OR = X"C7"
            IF WS-NEWORDER = "P"
                MOVE "AN ORDER WITH THIS REF. HAS BEEN ENTERED ALREADY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-010.
           IF F-EXIT-CH = X"19" OR = X"9B" OR = X"C7"
            IF WS-NEWORDER = "Y"
                MOVE "QUOTE NOT FOUND IN THE SYSTEM, CAN'T DISPLAY."
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
      ****************************************
      * 'F10' KEY, TO DELETE COMPLETE QUOTE  *
      ****************************************
           IF F-EXIT-CH = X"1F"
              MOVE "          " TO ALPHA-RATE
              MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
              PERFORM DECIMALISE-RATE
              MOVE NUMERIC-RATE TO WS-INVOICE
              PERFORM READ-INVOICE-REGISTER.
           IF F-EXIT-CH = X"1F"
            IF WS-NEWORDER = "Y"
              MOVE "QUOTE NOT FOUND IN THE SYSTEM, CAN'T DELETE."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO GET-010.
           IF F-EXIT-CH = X"1F"
            IF WS-NEWORDER = "C"
              MOVE " " TO WS-MESSAGE
              PERFORM ERROR-020
              PERFORM ERROR1-020
              GO TO GET-010.
       GET-011.
           IF F-EXIT-CH = X"1F"
            IF WS-NEWORDER = "N"
              PERFORM READ-STOCK-TRANSACTIONS
              PERFORM FIND-INFO
              PERFORM CLEAR-010
              MOVE 2910 TO POS
              DISPLAY "ARE YOU SURE ABOUT DELETING THE ENTIRE QUOTE."
              AT POS
              MOVE 3010 TO POS
              DISPLAY "ENTER Y TO DELETE, N TO STOP DELETION." AT POS
              ADD 40 TO POS
              MOVE 'N'       TO CDA-DATA
              MOVE 1         TO CDA-DATALEN
              MOVE 26        TO CDA-ROW
              MOVE 49        TO CDA-COL
              MOVE CDA-WHITE TO CDA-COLOR
              MOVE 'F'       TO CDA-ATTR
              PERFORM CTOS-ACCEPT
              MOVE CDA-DATA TO WS-DIS
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
            IF WS-DIS NOT = "Y" AND NOT = "N"
                GO TO GET-011.
           IF F-EXIT-CH = X"1F"
            IF WS-NEWORDER = "N"
             IF WS-DIS = "Y"
                PERFORM DELETE-STOCK-TRANS
                PERFORM DELETE-INVOICE-REGISTER
                PERFORM CLEAR-FIELDS
                PERFORM ERROR1-020
                PERFORM ERROR-020
                MOVE 2910 TO POS
                DISPLAY "DAILY EXCEPTION LOG BEING WRITTEN.....     "
                AT POS
                MOVE "QUOTE REVERSED   No:" TO WS-DAILY-1ST
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
                GO TO GET-010.

            IF F-NAMEFIELDRED1 = " "
                MOVE "0" TO F-NAMEFIELDRED1.
            IF F-NAMEFIELDRED1 = "0" OR = "1" OR = "2" OR = "3"
             OR = "4" OR = "5" OR = "6" OR = "7" OR = 88 OR = "9"
                NEXT SENTENCE
                ELSE 
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
            MOVE F-NAMEFIELD TO WS-ACCNO-X.
            MOVE WS-ACCNO-X TO DR-ACCOUNT-NUMBER
                               WS-ACCOUNT-NUMBER.
            IF DR-ACCOUNT-NUMBER = 0
                CLOSE DEBTOR-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM CLEAR-SCREEN
                PERFORM OPEN-011
                PERFORM DISPLAY-FORM
                GO TO GET-010.

           PERFORM READ-DEBTORS.
           IF DR-ACCOUNT-NUMBER = 9999999 OR = 0300150
                             OR = 0300090 OR = 0300087
                             OR = 0300100 OR = 0300200
               MOVE " " TO DR-TELEPHONE
               GO TO GET-017.
           IF DR-BALANCE > DR-CREDIT-LIMIT
               COMPUTE WS-WORK-FIELD = DR-BALANCE - DR-CREDIT-LIMIT
               MOVE WS-WORK-FIELD TO F-EDNAMEFIELDAMOUNT1
               MOVE "OVER THE CR.LIMIT:" TO WS-DAILY-1ST
               MOVE F-EDNAMEFIELDAMOUNT1 TO WS-DAILY-2ND
               MOVE " "                  TO WS-DAILY-3RD
                                            WS-DAILY-4TH
               MOVE WS-DAILY-MESSAGE     TO WS-MESSAGE
               PERFORM ERROR-000.
      ****************************************************************
      * NEW SECTION ADDED TO ADVISE SALESMAN IF THE ACCOUNT IS OVER  *
      * THE LIMIT.  THIS WILL ONLY HAPPEN IF THE FLAG IS SET IN THE  *
      * PARAMETER FILE - INVQUES-ACC-OVER-LIMIT = "Y"                *
      ****************************************************************
           IF DR-BALANCE > DR-CREDIT-LIMIT
            IF WS-QUES-ACC-OVER-LIMIT = "Y"
             MOVE
            "QUOTES CANNOT BE INVOICED AS THE ACCOUNT IS OVER THE"
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE
            "CREDIT LIMIT, ADVISE THE ACC'S DEPARTMENT & CUSTOMER."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
           IF DR-SUPPLY-Y-N = "N"
               MOVE 
               "GOODS CANNOT BE SUPPLIED ON THIS A/C, SUPPLY ON HOLD." 
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
            MOVE "DEBTORNAME" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            MOVE WS-NAME      TO F-NAMEFIELD.
            MOVE 40           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            IF DR-NAME = "UNKNOWN"
                GO TO GET-010.
            MOVE DR-DISCOUNT-CODE  TO WS-DISCOUNT-CODE
            MOVE DR-DELIVERY-CODE  TO WSDE-CODE
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
            MOVE WS-SOLD-BY TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DR-GSTNO TO F-NAMEFIELD WS-GSTNO.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PHONE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DR-TELEPHONE TO F-NAMEFIELD WS-PHONE.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESMAN"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE DR-SALESMAN  TO F-NAMEFIELD
            MOVE 1            TO F-CBFIELDLENGTH
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
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DEBTORNAME" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
             IF WS-NEWORDER NOT = "Y"
              IF WS-MUST-PRINT = "Y"
                MOVE "THE QUOTE HAS BEEN CHANGED, PRINT THIS COPY."
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-150
              ELSE
                PERFORM CANCEL-INVOICE
                GO TO GET-010.
            IF F-EXIT-CH = X"01"
              IF WS-NEWORDER = "Y"
                PERFORM CANCEL-INVOICE
                MOVE 0 TO DR-ACCOUNT-NUMBER
                          WS-ACCOUNT-NUMBER
                PERFORM CLEAR-FIELDS
                MOVE " " TO WS-BINNO
                GO TO GET-010
              ELSE
                UNLOCK INCR-REGISTER
                MOVE 0 TO DR-ACCOUNT-NUMBER
                          WS-ACCOUNT-NUMBER
                PERFORM CLEAR-FIELDS
                PERFORM DISPLAY-FORM
                MOVE " " TO WS-BINNO
                GO TO GET-010.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
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
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "CONTACTNAME" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-095.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-CONTACT.
            IF WS-CONTACT = "    "
               GO TO GET-096.
            IF WS-NEWORDER = "N"
               GO TO GET-115.
       GET-110.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "COMMENTLINE"              TO F-FIELDNAME.
            MOVE 11                         TO F-CBFIELDNAME.
            MOVE 30                         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE " " TO WS-POORDERNO.
            MOVE "POORDERNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"01"
             IF WS-INVOICE = 0
                MOVE "N" TO WS-MUST-PRINT
                GO TO GET-096
             ELSE
                GO TO GET-096.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-POORDERNO.
            PERFORM CHECK-REGISTER.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
       GET-111.
           IF WS-NEWORDER = "C"
                MOVE 2910 TO POS
                DISPLAY "QUOTE INVOICED ON INVOICE No:" AT POS
                ADD 35 TO POS
            IF INCR-TRANS = 1
                MOVE INCR-INVOICE TO F-EDNAMEFIELDNUM
                DISPLAY F-EDNAMEFIELDNUM AT POS
                PERFORM ERROR-010
                MOVE " " TO WS-MESSAGE
                MOVE 2910 TO POS
                DISPLAY WS-MESSAGE AT POS
                UNLOCK INCR-REGISTER
                GO TO GET-110
            ELSE
                MOVE INCR-BO-INV-NO TO F-EDNAMEFIELDNUM
                DISPLAY F-EDNAMEFIELDNUM AT POS
                PERFORM ERROR-010
                MOVE " " TO WS-MESSAGE
                MOVE 2910 TO POS
                DISPLAY WS-MESSAGE AT POS
                UNLOCK INCR-REGISTER
                GO TO GET-110.
           IF WS-NEWORDER = "Y"
              GO TO GET-115.
           IF WS-NEWORDER = "P"
            IF INCR-TRANS = 4
                MOVE "AN ORDER WITH THIS REF. HAS BEEN ENTERED ALREADY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                UNLOCK INCR-REGISTER
                GO TO GET-110.
           IF WS-NEWORDER = "P" OR = "N"
            IF WS-ABOVE-BODY = "1"
             IF INCR-TRANS = 8
                MOVE "A QUOTE WITH THIS REF. HAS BEEN ENTERED ALREADY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE "  PLEASE RE-ENTER THE PO/NUMBER." TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE " " TO INCR-PORDER
                UNLOCK INCR-REGISTER
                GO TO GET-110.
           IF WS-NEWORDER = "N"
            IF WS-ABOVE-BODY NOT = "1"
              MOVE 2910 TO POS
              DISPLAY "  THIS QUOTE HAS ALREADY BEEN ENTERED," AT POS
              MOVE 3010 TO POS
              DISPLAY "DO YOU WISH TO SEE THIS OLD QUOTE, Y OR N:"
              AT POS
              ADD 43 TO POS
              MOVE 'N'       TO CDA-DATA
              MOVE 1         TO CDA-DATALEN
              MOVE 27        TO CDA-ROW
              MOVE 52        TO CDA-COL
              MOVE CDA-WHITE TO CDA-COLOR
              MOVE 'F'       TO CDA-ATTR
              PERFORM CTOS-ACCEPT
              MOVE CDA-DATA TO WS-DIS
             IF W-ESCAPE-KEY = 1 OR 2
                 GO TO GET-112
              ELSE
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-111.
       GET-112.
            MOVE " " TO WS-MESSAGE.
            MOVE 2910 TO POS.
            DISPLAY WS-MESSAGE AT POS.
            MOVE 3010 TO POS.
            DISPLAY WS-MESSAGE AT POS.
            IF WS-DIS = "Y"
               PERFORM READ-INVOICE-REGISTER
             IF WS-NEWORDER = "C" OR = "Y"
               MOVE
         "THIS QUOTE IS COMPLETE AND CANNOT BE DISLAYED, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM DISPLAY-FORM
               GO TO GET-010
             ELSE
               PERFORM READ-STOCK-TRANSACTIONS
               PERFORM FIND-INFO
               GO TO GET-150.
            IF WS-DIS = "N"
                MOVE 0 TO DR-ACCOUNT-NUMBER
                          WS-ACCOUNT-NUMBER
                MOVE " " TO WS-BINNO
                PERFORM CLEAR-FIELDS
                PERFORM DISPLAY-FORM
                UNLOCK INCR-REGISTER
                GO TO GET-010.
            DISPLAY " " AT 3079 WITH BELL.
            GO TO GET-111.
       GET-115.
            IF WS-SOLD-BY NOT = "  "
               GO TO GET-120.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "SOLDBY" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"01"
             IF WS-NEWORDER = "Y"
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
                       AND NOT = "7" AND NOT = 88 AND NOT = "9"
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
                GO TO GET-110
              ELSE
                GO TO GET-096.
            IF F-EXIT-CH = X"1D"
                GO TO GET-120.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-FIELD WS-DELIVERVIA.
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
            MOVE F-NAMEFIELD TO ALPHA-FIELD.
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
            MOVE "TERMOFSALE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-TERMOFSALE TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-150.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FIELDS
                PERFORM ERROR1-020
                PERFORM ERROR-020
                PERFORM DISPLAY-FORM
                GO TO GET-010.
            IF WS-SALESANALYSIS = "UNKNOWN"
                MOVE "SALES ANALYSIS CODE IS WRONG, RE-ENTER." 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-125.
            IF DR-SUPPLY-Y-N = "S"
                MOVE "PRESS 'ESC' TO CLEAR THE SCREEN, CAN'T ENTER."
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
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
                MOVE WS-ORDERTOTAL TO WSF-PRICE
                MOVE 2859 TO POS
                DISPLAY WSF-PRICE AT POS
                PERFORM ERROR-010
            MOVE 
            "THIS QUOTE CAN'T BE INVOICED AS THE ACCOUNT IS OVER THE"
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE
            "CREDIT LIMIT, PLEASE ADVISE ACC'S DEPARTMENT & CUSTOMER."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.

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
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-GSTNO.
            IF F-EXIT-CH = X"01"
                GO TO GET-150.
       GET-170.
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
            MOVE 5            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF WS-INVOICEDISCOUNT > 0
               PERFORM CHANGE-DISCOUNT-PER-LINE.
       GET-177.
            PERFORM SET-GST.
            MOVE 1 TO SUB-1 F-INDEX.
      *      PERFORM SCROLL-NEXT.
            PERFORM SCROLL-PREVIOUS.
       GET-180.
            MOVE "N" TO WS-LINECHANGED.
            PERFORM ERROR1-020
            PERFORM ERROR-020
            PERFORM FILL-BODY.
            IF WS-ABOVE-BODY = "1"
                GO TO GET-150.
            PERFORM ERROR-020
            MOVE 2701 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 2801 TO POS
            DISPLAY WS-MESSAGE AT POS.
       GET-185.
      ****************************************************************
      *SECTION TO CHECK IF THE NEW ORDER TO BE INVOICED WILL PUSH THE*
      *ACCOUNT OVER THE CREDIT LIMIT.                                *
      ****************************************************************
            PERFORM GET-240 THRU GET-270.
            IF DR-BALANCE + WS-SUBTOTAL > DR-CREDIT-LIMIT
               COMPUTE WS-ORDERTOTAL = 
                  DR-BALANCE + WS-SUBTOTAL + WS-TAXAMT 
                             - DR-CREDIT-LIMIT
            IF WS-QUES-ACC-OVER-LIMIT = "Y"
             MOVE 
            "NEW ORDER WILL PUSH THE ACC OVER CR-LIMIT BY R"
             TO WS-MESSAGE
             PERFORM ERROR-000
                MOVE WS-ORDERTOTAL TO WSF-PRICE
                MOVE 3061 TO POS
                DISPLAY WSF-PRICE AT POS
                PERFORM ERROR-010
             MOVE
            "THE QUOTE CANNOT BE INVOICED AS THE ACCOUNT WILL BE OVER"
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE
            "THE CREDIT LIMIT, ADVISE THE ACCS DEPARTMENT & CUSTOMER."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
       GET-190.
            MOVE 1 TO F-INDEX.
            IF WS-NEWORDER = "Y"
               PERFORM CLEAR-BOTTOM-FIELDS
               MOVE 0 TO WS-ADDONFREIGHT
                         WS-POSTADDON
                         WS-HANDADDON
                         WS-MISCADDON.
            MOVE 1 TO F-INDEX.
            PERFORM GET-240 THRU GET-245
            PERFORM GET-250
            PERFORM GET-260
            PERFORM GET-270.
            MOVE "COMMENTLINE" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
             IF WS-NEWORDER = "Y"
                MOVE 1 TO F-INDEX 
                PERFORM CLEAR-BOTTOM-FIELDS
                MOVE 0 TO WS-ADDONFREIGHT
                MOVE 0 TO WS-POSTADDON
                MOVE 0 TO WS-HANDADDON
                MOVE 0 TO WS-MISCADDON
                MOVE 1 TO F-INDEX SUB-1
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                GO TO GET-180
              ELSE
                MOVE 1 TO F-INDEX SUB-1
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                GO TO GET-180.
            MOVE 30 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-COMMENTLINE.
      *F8-KEY.
            IF F-EXIT-CH = X"1D"
                GO TO GET-200.
            IF WS-IMM-PR = "Y"
                GO TO GET-240.
       GET-200.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "ADDONFREIGHT" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
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
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "POSTADDON" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
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
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "HANDADDON" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
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
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "MISC.ADDON" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
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
            MOVE "SUBTOTAL"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE WS-SUBTOTAL TO F-EDNAMEFIELD9MIL.
            MOVE 10          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-9MIL.
       GET-250.
            MOVE "ADDONAMT"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE WS-ADDONAMT TO F-EDNAMEFIELD9MIL.
            MOVE 10          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-9MIL.
       GET-260.
            MOVE "TAXAMT"  TO F-FIELDNAME.
            MOVE 6         TO F-CBFIELDNAME.
            MOVE WS-TAXAMT TO F-EDNAMEFIELD9MIL.
            MOVE 10        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-9MIL.
       GET-270.
            COMPUTE WS-INVOICETOTAL = WS-SUBTOTAL +
                                      WS-TAXAMT + 
                                      WS-ADDONAMT.
            MOVE "INVOICETOTAL"  TO F-FIELDNAME.
            MOVE 12              TO F-CBFIELDNAME.
            MOVE WS-INVOICETOTAL TO F-EDNAMEFIELD9MIL.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-9MIL.
       GET-300.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-190.
       FINAL-ENTRY-001.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
       FINAL-ENTRY-055.
           IF WS-BELOWF-BODY NOT = "Y"
            IF WS-NEWORDER NOT = "N"
               PERFORM READ-PARAMETER-LOCK
               MOVE PA-QUOTE-NUMBER TO WS-QUOTE WS-INVOICE
               ADD 1 TO PA-QUOTE-NUMBER
               PERFORM REWRITE-PARAMETER.

           IF WS-BELOWF-BODY NOT = "Y"
               PERFORM REMOVE-LEADING-ZEROS.
           MOVE "INVOICENUM" TO F-FIELDNAME.
           MOVE 10           TO F-CBFIELDNAME.
           MOVE WS-INVOICE   TO F-EDNAMEFIELDNUM.
           MOVE 6            TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-NUMERIC.

           MOVE WS-QUOTE-NAME    TO WS-QUOTEREF WSF-QUOTEREF.
           MOVE INCR-COPY-NUMBER TO WSF-PRINTNUMBER
           ADD 1                 TO WSF-PRINTNUMBER.
           MOVE "Y" TO WS-BELOWF-BODY.
       FINAL-ENTRY-056.
            PERFORM CLEAR-010.
            PERFORM FINAL-ENTRY-001.
            PERFORM ERROR1-020
            MOVE 2910 TO POS.
            DISPLAY
            "To SUSPEND the Quote = 'S', For WordProcessing = 'W' OR"
                AT POS.
            MOVE "Z" TO WS-ANSWER.
            MOVE 3010 TO POS.
            DISPLAY 
            "Print: DotMatrix Y=2, N=1, Z=Zero. Or P=PDF: [ ]" AT POS.
            ADD 46 TO POS.

           MOVE 'Z'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

            IF W-ESCAPE-KEY = 4
               GO TO GET-270.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO FINAL-ENTRY-060
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO FINAL-ENTRY-055.
       FINAL-ENTRY-060.
            IF WS-ANSWER NOT = "Y" AND NOT = "N" AND NOT = "P"
                     AND NOT = "S" AND NOT = "W" AND NOT = "Z"
               MOVE " " TO WS-ANSWER
               GO TO FINAL-ENTRY-055.
            PERFORM ERROR1-020
            PERFORM ERROR-020
            IF WS-ANSWER = "S"
               GO TO GET-999.
            MOVE "                    " TO WS-SALESMAN.
            MOVE 2910 TO POS.
            DISPLAY "What Is The Salesmans name:[                    ]"
                 AT POS.
            ADD 28 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 37        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SALESMAN.

            IF W-ESCAPE-KEY = 4
               MOVE "1" TO WS-ABOVE-BODY
               GO TO FINAL-ENTRY-055.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO FINAL-ENTRY-067
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO FINAL-ENTRY-060.
       FINAL-ENTRY-067.
            IF WS-SALESMAN = "          "
               GO TO FINAL-ENTRY-060.
            PERFORM ERROR1-020
            PERFORM ERROR-020
            MOVE 2901 TO POS.
            MOVE " " TO WS-COMMENT1 WS-COMMENT2.
            DISPLAY "Comment:[" AT POS.
            ADD 69 TO POS.
            DISPLAY "]" AT POS.
            MOVE 2910 TO POS.
 
           MOVE ' '       TO CDA-DATA.
           MOVE 60        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 09        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMMENT1.

            IF W-ESCAPE-KEY = 4
               MOVE "1" TO WS-ABOVE-BODY
               GO TO FINAL-ENTRY-060.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO FINAL-ENTRY-070
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO FINAL-ENTRY-067.
       FINAL-ENTRY-070.
            PERFORM ERROR-020.
            MOVE 2901 TO POS.
            DISPLAY "        [" AT POS.
            ADD 69 TO POS.
            DISPLAY "]" AT POS.
            MOVE 2910 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 60        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 09        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMMENT2.

            IF W-ESCAPE-KEY = 4
               MOVE "1" TO WS-ABOVE-BODY
               GO TO FINAL-ENTRY-067.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               MOVE 1 TO SUB-1
               GO TO FINAL-ENTRY-075
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO FINAL-ENTRY-070.
       FINAL-ENTRY-075.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            IF WS-QUOTE-TERM (SUB-1) = " "
               MOVE 1 TO SUB-1
               GO TO FINAL-ENTRY-078.
            MOVE 3010 TO POS
            DISPLAY "[" AT POS
            ADD 61 TO POS
            DISPLAY "]" AT POS.
            MOVE 3011 TO POS
            DISPLAY WS-QUOTE-TERM (SUB-1) AT POS

           MOVE WS-QUOTE-TERM (SUB-1) TO CDA-DATA.
           MOVE 60        TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 10        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-QUOTE-TERM (SUB-1).

            IF W-ESCAPE-KEY = 4
             IF SUB-1 > 1
               SUBTRACT 1 FROM SUB-1
               GO TO FINAL-ENTRY-075
             ELSE
               GO TO FINAL-ENTRY-070.
            IF W-ESCAPE-KEY NOT = 0 AND NOT = 1 AND NOT = 2 AND NOT = 5
               DISPLAY " " AT 3079 WITH BELL
               GO TO FINAL-ENTRY-075.
            IF SUB-1 < 10
               ADD 1 TO SUB-1
               GO TO FINAL-ENTRY-075.
            MOVE 1 TO SUB-1.
       FINAL-ENTRY-078.
            PERFORM ERROR1-020
            PERFORM ERROR-020
            MOVE 2910 TO POS
            MOVE " " TO WS-AUTO-FAX
            DISPLAY "Print Totals Y / N    :[ ]" AT POS
            ADD 24 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 33        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PRINT-TOTALS.

            IF W-ESCAPE-KEY = 4
               MOVE 1 TO SUB-1
               GO TO FINAL-ENTRY-075.
            IF WS-PRINT-TOTALS NOT = "Y" AND NOT = "N"
               GO TO FINAL-ENTRY-078.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO FINAL-ENTRY-080
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO FINAL-ENTRY-078.
       FINAL-ENTRY-080.
            IF WS-ANSWER = "W"
               GO TO FINAL-ENTRY-100.
            PERFORM ERROR1-020
            PERFORM ERROR-020
            MOVE 2910 TO POS
            MOVE " " TO WS-AUTO-FAX
            DISPLAY "Send By: F=Fax, E=Email, N=Neither :[ ]" AT POS
            ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-AUTO-FAX.

            IF W-ESCAPE-KEY = 4
               MOVE "1" TO WS-ABOVE-BODY
               GO TO FINAL-ENTRY-070.
            IF WS-AUTO-FAX NOT = "E" AND NOT = "F" AND NOT = "N"
               GO TO FINAL-ENTRY-080.
            IF WS-AUTO-FAX  = "N"
               MOVE DR-TELEX TO WS-FAX-NUMBER
               GO TO FINAL-ENTRY-100.
            IF WS-AUTO-FAX  = "E"
               MOVE DR-SALES-EMAIL TO WSF-MAIL-NUMBER
               GO TO FINAL-ENTRY-095.
            IF WS-AUTO-FAX  = "F"
               MOVE DR-SALES-EMAIL TO WS-FAX-NUMBER
               GO TO FINAL-ENTRY-090.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO FINAL-ENTRY-090
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO FINAL-ENTRY-080.
       FINAL-ENTRY-090.
            MOVE SPACES TO WS-FAX-NUMBER
            MOVE DR-TELEX TO WS-FAX-NUMBER.
            PERFORM ERROR1-020
            PERFORM ERROR-020
            MOVE 3010 TO POS.
            DISPLAY
            "Enter Fax Number Only With 'NUMBERS & A DASH' No Brackets."
                AT POS.
            MOVE 2910 TO POS.
            DISPLAY
            "Fax Number, Re-Enter If Not Correct:[                    ]"
                AT POS.
            ADD 37 TO POS.
            DISPLAY WS-FAX-NUMBER AT POS.

           MOVE WS-FAX-NUMBER TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FAX-NUMBER.

            IF W-ESCAPE-KEY = 4
               MOVE "1" TO WS-ABOVE-BODY
               GO TO FINAL-ENTRY-080.
            IF WS-FAX-NUMBER = " "
               MOVE "THIS FIELD CANNOT BE BLANK, RE-ENTER" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FINAL-ENTRY-090.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO FINAL-ENTRY-100
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO FINAL-ENTRY-090.
       FINAL-ENTRY-095.
            MOVE 0 TO WS-SPACE-CNT.
            MOVE SPACES TO WSF-MAIL-NUMBER
            MOVE DR-SALES-EMAIL TO WSF-MAIL-NUMBER.
            PERFORM ERROR1-020
            PERFORM ERROR-020
            MOVE 3010 TO POS.
            DISPLAY
            "ENTER EMail ADDRESS IN lower case ONLY, NO SPACES."
                AT POS.
            MOVE 2910 TO POS.
            DISPLAY
            "EMail:[                                                  ]"
                AT POS.
            ADD 7 TO POS.
            DISPLAY WSF-MAIL-NUMBER AT POS.

           MOVE WSF-MAIL-NUMBER TO CDA-DATA.
           MOVE 50        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 16        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WSF-MAIL-NUMBER.

            IF W-ESCAPE-KEY = 4
               MOVE "1" TO WS-ABOVE-BODY
               GO TO FINAL-ENTRY-080.
            IF WSF-MAIL-NUMBER = " "
               MOVE "THIS FIELD CANNOT BE BLANK, RE-ENTER" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FINAL-ENTRY-095.

            MOVE FUNCTION LOWER-CASE(WSF-MAIL-NUMBER) TO WSF-MAIL-NUMBER 
                                                          WS-EMAIL
                                                           F-NAMEFIELD.
            INSPECT WS-EMAIL TALLYING WS-SPACE-CNT FOR CHARACTERS
                BEFORE INITIAL SPACE.
            IF WS-EMAIL(1:(WS-SPACE-CNT)) IS NOT WS-VALID-EMAIL
                MOVE "EMAIL ADDRESS HAS AN INVALID CHARACTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FINAL-ENTRY-095.
 
            MOVE 2910 TO POS.
            DISPLAY
            "EMail:[                                                  ]"
                AT POS.
            ADD 7 TO POS.
            DISPLAY WSF-MAIL-NUMBER AT POS.
 
            PERFORM CHECK-EMAIL-FOR-VALIDITY.
            IF WS-ACC-ERROR = "Y"
                GO TO FINAL-ENTRY-095.
            IF WS-SPACE-CNT < 10
                MOVE 
            "EMAIL ADDRESS INVALID AS IT'S TOO SHORT, 'ESC' TO RETRY." 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FINAL-ENTRY-095.

            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO FINAL-ENTRY-100
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO FINAL-ENTRY-095.
       FINAL-ENTRY-100.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
       FINAL-ENTRY-800.
           IF WS-ANSWER = "W"
              GO TO FINAL-ENTRY-905.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           MOVE 2610 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           IF WS-AUTO-FAX  = "E"
              GO TO FINAL-ENTRY-950.
           IF WS-AUTO-FAX  = "N"
            IF WS-ANSWER = "P"
              GO TO FINAL-ENTRY-901.
            IF WS-AUTO-FAX  = "F"
              PERFORM CHECK-FAX-NUMBER.
       FINAL-ENTRY-900.
           IF WS-AUTO-FAX = "N" OR = "E"
              GO TO FINAL-ENTRY-950.
       FINAL-ENTRY-901.
      **********************************
      *fax routine for XQS FAX SYSTEM **
      **********************************
           MOVE 3010 TO POS.
           IF Fax-PaNumber = 3
            IF WS-ANSWER NOT = "W"
              DISPLAY "SENDING QUOTE BY FAX...........         " AT POS.
           IF Fax-PaNumber = 3
              PERFORM ERROR1-020
              MOVE 2910 TO POS
              DISPLAY "GETTING XQS-FAX DETAILS...." AT POS
              PERFORM ENTER-XQS-DETAILS
              PERFORM ERROR1-020
              MOVE 2910 TO POS
              DISPLAY "PERFORMING WRITE-ROUTINE..." AT POS
              PERFORM WRITE-FAX-ROUTINE
              PERFORM ERROR1-020
              GO TO FINAL-ENTRY-950.
      *******************************************
      *fax routine for Linux Hylfax FAX SYSTEM **
      *******************************************
           MOVE 3010 TO POS.
          IF Fax-PaNumber = 4
           IF WS-ANSWER NOT = "W"
              DISPLAY "SENDING QUOTE BY HylaFax......         " AT POS.
           IF Fax-PaNumber = 4
              PERFORM ERROR1-020
              MOVE 2910 TO POS
              DISPLAY "GETTING HYLAFAX DETAILS...." AT POS
              PERFORM ENTER-XQS-DETAILS
              PERFORM ERROR1-020
              MOVE 2910 TO POS
              DISPLAY "PERFORMING WRITE-ROUTINE..." AT POS
              PERFORM WRITE-FAX-ROUTINE
              PERFORM ERROR1-020
              GO TO FINAL-ENTRY-950.
      *
      **************************************************************
      *fax routine for MURATA SYSTEM & WRITE TO <FX> FOR WORD-PRO **
      **************************************************************
       FINAL-ENTRY-905.
           MOVE 3010 TO POS.
           IF WS-ANSWER NOT = "W"
              DISPLAY "SENDING QUOTE BY FAX...........         " AT POS
           ELSE
              DISPLAY "WRITING QUOTE TO <Fx> DIRECTORY...      " AT POS.
           MOVE "/ctools/fx/"  TO WS-1ST-11CHAR
           MOVE "/ctools/fax/" TO WS-FAX-12CHAR
           PERFORM CHECK-QUOTE-REF.
           IF WS-ANSWER = "Y"
               MOVE "2" TO WS-ANSWER.
           PERFORM WRITE-FAX-ROUTINE.
           IF WS-ANSWER = "2"
               MOVE "Y" TO WS-ANSWER.
           IF WS-ANSWER = "W"
               GO TO GET-999.
           PERFORM PREPARE-FAX-SENDING.
           MOVE 2910 TO POS
           DISPLAY "The Job No For This Fax Is:                " AT POS
           ADD 28 TO POS
           MOVE FAX-JOBNUMBER TO F-NAMEFIELDJOB
           DISPLAY F-NAMEFIELDJOB AT POS
           MOVE 3020 TO POS
           DISPLAY "Press <RETURN> To Finish The Fax." AT POS
           ADD 35 TO POS
           ACCEPT WS-ACCEPT AT POS.
       FINAL-ENTRY-950.
      ********************************************
      *HARD COPY DOT MATRIX PRINT-OUT SECTION   **
      ********************************************
           MOVE Spaces              TO WS-PRINTER.
           MOVE Ws-PrinterName (21) To Ws-Printer.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS
           IF WS-ANSWER = "Z"
              GO TO FINAL-ENTRY-960.
           IF WS-ANSWER = "Y"
              MOVE "2" TO WS-ANSWER
              MOVE 3010 TO POS
              DISPLAY "PRINTING OF QUOTE IN PROGRESS........      "
              AT POS
              PERFORM WRITE-ROUTINE.
           IF WS-ANSWER = "N"
              MOVE "1" TO WS-ANSWER
              MOVE 3010 TO POS
              DISPLAY "PRINTING OF QUOTE IN PROGRESS........      "
              AT POS
              PERFORM WRITE-ROUTINE.
       FINAL-ENTRY-960.
      *********************************************
      *COPY WRITTEN TO /ctools/eimage/ SECTION   **
      *********************************************
           IF WS-AUTO-FAX NOT = "E" 
               GO TO GET-999.
               
           PERFORM GET-EMAIL-QUOTE-NAME.
           MOVE Spaces              TO WS-PRINTER.
           MOVE WSF-mail-Quote      To Ws-Printer.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS

           MOVE "1" TO WS-ANSWER
           MOVE 3010 TO POS
           DISPLAY "WRITING EMAIL DISK FILE .............      "
           AT POS
           PERFORM WRITE-EMAIL-ROUTINE.

           PERFORM MOVE-EMAIL-FROM-EIMAGE-SETUP
           PERFORM MOVE-EMAIL-RECORD-FROM-EIMAGE.
       GET-999.
            EXIT.
      *
       CHECK-EMAIL-FOR-VALIDITY SECTION.
       CEFV-005.
             MOVE 0 TO SUB-1.
             MOVE SPACES TO ALPHA-RATE
             MOVE F-NAMEFIELD TO ALPHA-RATE.
             MOVE "N" TO WS-ACC-ERROR.
       CEFV-010.
             ADD 1 TO SUB-1.
             IF SUB-1 > 42
                MOVE "Y" TO WS-ACC-ERROR
                GO TO CEFV-900.
             IF AL-RATE (SUB-1) = "@"
                MOVE 0 TO SUB-1
                GO TO CEFV-020.
             GO TO CEFV-010.
       CEFV-020.
             ADD 1 TO SUB-1.
             IF SUB-1 > 42
                MOVE "Y" TO WS-ACC-ERROR
                GO TO CEFV-900.
             IF AL-RATE (SUB-1) = "."
                GO TO CEFV-025.
             GO TO CEFV-020.
       CEFV-025.
      *ADDED THIS NEXT LINE SO THAT WE DON'T CHECK FOR AN EXTRA . OR COM
             GO TO CEFV-999.
       
             ADD 1 TO SUB-1.
             IF AL-RATE (SUB-1) = "c"
                GO TO CEFV-026
             ELSE
                SUBTRACT 1 FROM SUB-1
                GO TO CEFV-030.
             MOVE "Y" TO WS-ACC-ERROR.
       CEFV-026.
             ADD 1 TO SUB-1.
             IF AL-RATE (SUB-1) = "o"
                GO TO CEFV-027.
             SUBTRACT 2 FROM SUB-1
             GO TO CEFV-030.
       CEFV-027.
             ADD 1 TO SUB-1.
             IF AL-RATE (SUB-1) = "m"
                GO TO CEFV-040.
             SUBTRACT 3 FROM SUB-1.
       CEFV-030.
             ADD 1 TO SUB-1.
             IF SUB-1 > 42
                MOVE "Y" TO WS-ACC-ERROR
                GO TO CEFV-900.
             IF AL-RATE (SUB-1) = "."
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
            MOVE " " TO WS-ABOVE-BODY
            MOVE 1 TO SUB-1 SUB-2 SUB-3.
       FILL-005.
            PERFORM ERROR-020.
            MOVE 2610 TO POS
            DISPLAY
             "PRESS <ALT-Z> TO ENTER ZOOMBOX TO DO A STOCK ENQUIRY."
               AT POS.
            MOVE 2710 TO POS
            DISPLAY
             "PRESS <ALT-G> TO ENTER A TOOLKIT LISTING INTO THE QUOTE."
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
            PERFORM RUNNING-TOTAL
            IF B-STOCKNUMBER (SUB-1) NOT = " "
               PERFORM SCROLL-050.

            MOVE "                    " TO F-NAMEFIELD
            MOVE "STOCKNUMBER" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            MOVE 15 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.

            IF F-EXIT-CH = X"0B" 
             IF B-STOCKNUMBER (SUB-1) = " "
                GO TO FILL-010.
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
            IF SUB-25 > 10
                MOVE SUB-25 TO SUB-1
                COMPUTE SUB-1 = SUB-1 / 2
                PERFORM SCROLL-NEXT
                GO TO FILL-005.
      *<CODE-e> = GO TO END
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
                PERFORM FIND-010 THRU FIND-020
                PERFORM CALC-POS-OF-CURSOR
                GO TO FILL-005.

      *****************************************************************
      *CREATE A BLANK SPACE INTO WHICH A NEW LINE OF STOCK IS ENTERED *
      *****************************************************************
      * <CODE-TAB> (CTOS); <ALT-F8> =X"9D" IN LINUX
      * CTOS SECTION
           IF F-EXIT-CH = X"89" AND SUB-25 < 150
                PERFORM EMPTY-LINE
                MOVE SUB-3 TO SUB-1
                ADD 1 TO SUB-25
                MOVE "E" TO WS-LINECHANGED
                MOVE "Y" TO WS-CODETAB
              IF SUB-1 > 3
                SUBTRACT 3 FROM SUB-1
                PERFORM SCROLL-NEXT
                ADD 1 TO SUB-25
                GO TO FILL-005 
              ELSE
                PERFORM SCROLL-NEXT
                ADD 1 TO SUB-25
                GO TO FILL-005.
      * LINUX SECTION
           IF F-EXIT-CH = X"9D" AND SUB-25 < 150
                PERFORM EMPTY-LINE
                MOVE SUB-3 TO SUB-1
                ADD 1 TO SUB-25
                MOVE "E" TO WS-LINECHANGED
                MOVE "Y" TO WS-CODETAB
              IF SUB-1 > 3
                SUBTRACT 3 FROM SUB-1
                PERFORM SCROLL-NEXT
                ADD 1 TO SUB-25
                GO TO FILL-005 
              ELSE
                PERFORM SCROLL-NEXT
                ADD 1 TO SUB-25
                GO TO FILL-005.
      * <UP-ARROW>
            IF F-EXIT-CH = X"01" AND F-INDEX = 1
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
              IF WS-MUST-PRINT = "Y"
               MOVE
            "YOU SHOULD PRINT THIS QUOTE AS A CHANGE HAS BEEN MADE."
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
              IF SP-1STCHAR NOT = "*" AND NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                MOVE
           "YOU SHOULD PRINT THIS QUOTE AS A CHANGE HAS BEEN MADE."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999
             ELSE
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999.

            MOVE F-NAMEFIELD TO SPLIT-STOCK.
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
      * <DOWN-ARROW>
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

      **************************************
      *X"91"=<ALT-F8>  DISPLAY COST ON/OFF *
      **************************************
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
      * TAB CHARACTER
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
      * 'ESC' = X"07"; 'CODE-CANCEL' = X"87"; 'ALT-F10' = X"9F"
            IF F-EXIT-CH = X"07" OR = X"87" OR = X"9F"
             IF B-STOCKNUMBER (SUB-1) = "  "
                GO TO FILL-010.
            IF F-EXIT-CH = X"07"
                MOVE "TO DELETE A LINE-ITEM PRESS 'ALT-F10'"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-010.
            IF F-EXIT-CH = X"87"
              IF WS-NEWORDER = "N"
               IF WS-CODETAB = "N"
                MOVE "Y" TO WS-MUST-PRINT
                PERFORM CANCEL-STOCK-TRANS
               ELSE
                MOVE
            "BY ADDING A LINE YOU MUST SAVE THIS VERSION 1ST, THEN YOU"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE 
            " MAY DELETE A LINE.  NO DELETE & ADD IN THE SAME SESSION."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO FILL-010.
            IF F-EXIT-CH = X"87" OR = X"9F"
                MOVE SUB-1 TO SUB-7
                MOVE "Y" TO WS-MUST-PRINT
                PERFORM CANCEL-STOCK-TRANS
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
      * SECTION TO DISALLOW RE-ENTRY OF EXISTING LINE
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
      *SECTION TO CHANGE QTY'S OF EXISTING LINE
      *F5      = "19"
           IF F-EXIT-CH = X"19"
            IF B-STOCKNUMBER (SUB-1) = " "
               GO TO FILL-010.
           IF F-EXIT-CH = X"19"
            IF SP-1STCHAR = "*" OR = "/"
               GO TO FILL-020.

           IF F-EXIT-CH = X"19"
            IF SP-1STCHAR NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER
                PERFORM READ-STOCK
                MOVE ST-BINLOCATION TO B-STORE (SUB-1)
                MOVE "Y" TO WS-LINECHANGED
            IF WS-COST-DISPLAY = "Y"
                MOVE "STOCKCOST" TO F-FIELDNAME
                MOVE 9 TO F-CBFIELDNAME
                MOVE ST-AVERAGECOST TO F-EDNAMEFIELDAMOUNT
                                       B-STOCKCOST (SUB-1)
                MOVE 9 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNT
            ELSE
                MOVE ST-AVERAGECOST TO B-STOCKCOST (SUB-1).
            IF F-EXIT-CH = X"19"
                PERFORM DLI-020
                GO TO FILL-020.
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
                
      ***********************************************************
      * NEW SECTION TO CALL UP TOOLKIT LIST AND QUOTE. <CODE-GO>*
      ***********************************************************
            IF F-EXIT-CH = X"9B" or = x"C7"
             IF SP-1STCHAR NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER
                                              WS-TOOLKIT-NUMBER
                PERFORM READ-STOCK
                MOVE 2910 TO POS
                DISPLAY
                "TOOLKIT LIST BEING READ, PLEASE BE PATIENT....." AT POS
                PERFORM ENTER-STOCK-DETAILS
                MOVE 1 TO B-ORDERQTY (SUB-1)
                MOVE 0 TO B-STOCKPRICE (SUB-1)
                          B-STOCKCOST (SUB-1)
                          B-DISCOUNTPERITEM (SUB-1)
                ADD 1  TO SUB-1
                MOVE "*** TOOLKIT AS "  TO B-STOCKNUMBER (SUB-1)
                MOVE "LISTED BELOW ***" TO C-LINE (SUB-1)
                ADD 1  TO SUB-1
                PERFORM READ-TOOLKITS
                MOVE SUB-1 TO SUB-25
                MOVE 1 TO SUB-1
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                PERFORM ERROR1-020
                GO TO FILL-005.

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
            PERFORM CHECK-IF-ENTERED-BEFORE.
            MOVE 2110 TO POS.
            DISPLAY WS-ONHAND-LINE AT POS.

           IF SP-1STCHAR = "/"
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
                DISPLAY WS-ONHAND-LINE AT POS
             IF SUB-1 < SUB-25
                MOVE "Y" TO WS-LINECHANGED.
            IF SP-1STCHAR NOT = "/"
               AND ST-DESCRIPTION1 = " "
                   MOVE "INVALID STOCK ITEM!!!" TO WS-MESSAGE
                   PERFORM ERROR-MESSAGE
                   MOVE " " TO B-STOCKNUMBER (SUB-1)
                   GO TO FILL-005.

            IF ST-DESCRIPTION1 = " "
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

            MOVE "PERUNIT"        TO F-FIELDNAME.
            MOVE 7                TO F-CBFIELDNAME.
            MOVE ST-UNITOFMEASURE TO F-NAMEFIELD
                                     B-UNIT (SUB-1).
            MOVE 4                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE ST-BINLOCATION TO B-STORE (SUB-1).
       FILL-0100.
            IF ST-PRICE = 0
                 GO TO FILL-0110.
            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE ST-PRICE TO F-EDNAMEFIELDAMOUNT
                             B-STOCKPRICE (SUB-1).
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
       FILL-0110.
            IF ST-AVERAGECOST = 0
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
                MOVE 5                  TO F-CBFIELDLENGTH
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
            MOVE "TAX" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
      *      IF WS-GSTNO = "EXPORT" OR > " "
            IF WS-GSTNO = "EXPORT" OR = "EXPORT      "
                MOVE "N" TO F-NAMEFIELD
                            B-TAX (SUB-1)
            ELSE
                MOVE "Y" TO F-NAMEFIELD
                            B-TAX (SUB-1).
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
      
            IF SP-1STCHAR NOT = "/" AND NOT = "*"
                PERFORM READ-QUOTE-BY-ACCOUNT. 
            
            PERFORM CHECK-DATE-LAST-SOLD.
       FILL-020.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "ORDERQTY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
             IF WS-LINECHANGED = "N"
               MOVE 0 TO B-ORDERQTY (SUB-1)
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-010
             ELSE
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1D" AND NOT = X"15"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF SIGN-FOUND = 1
                GO TO FILL-020.
            MOVE NUMERIC-RATE TO B-ORDERQTY (SUB-1).
            IF B-ORDERQTY (SUB-1) = 0
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-020.
            MOVE B-ORDERQTY (SUB-1) TO F-EDNAMEFIELDQTY.
            PERFORM WRITE-FIELD-QTY.

            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR = "/"
                GO TO FILL-045.
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
            IF F-EXIT-CH = X"1D"
                GO TO FILL-048.
            GO TO FILL-090.
       FILL-045.
            IF SP-1STCHAR = "/"
             IF SUB-1 NOT = SUB-25
              IF F-EXIT-CH NOT = X"1D"
                GO TO FILL-090.
       FILL-046.
            MOVE 3010 TO POS.
            DISPLAY "    BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
            MOVE 2920 TO POS.
            DISPLAY "DESCRIPTION1" AT POS.

            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE B-STOCKDESCRIPTION (SUB-1) TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
                MOVE 2920 TO POS
                DISPLAY "                  " AT POS
                GO TO FILL-020.
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
            MOVE 2720 TO POS.
            DISPLAY "DESCRIPTION2" AT POS.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
                GO TO FILL-045.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-047.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO B-STOCKDESCRIPTION2 (SUB-1).
            MOVE 2720 TO POS.
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
            PERFORM CHECK-PASSWORD
            IF WS-PASSWORD-VALID = "N"
               GO TO FILL-070.
       FILL-050.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9            TO F-CBFIELDLENGTH.
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
                MOVE "QUOTE AC"             TO WS-DAILY-3RD1
                MOVE WS-ACCOUNT-NUMBER      TO WS-DAILY-3RD2
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
                MOVE "DISCOUNTPERITEM" TO F-FIELDNAME
                MOVE 15                TO F-CBFIELDNAME
                MOVE WS-DISCOUNTSAVE   TO B-DISCOUNTPERITEM (SUB-1)
                                          F-EDNAMEFIELDAMOUNTDIS
                MOVE 5                 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS
                MOVE 0 TO STPR-PRICE.
            
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
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
            MOVE "STOCKCOST"           TO F-FIELDNAME.
            MOVE 9                     TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9                     TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
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
                MOVE "REPAR AC"             TO WS-DAILY-3RD1
                MOVE WS-ACCOUNT-NUMBER      TO WS-DAILY-3RD2
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
                MOVE 2845 TO POS
                MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                DISPLAY F-EDNAMEFIELDAMOUNTDIS AT POS
                GO TO FILL-072.

            IF SP-1STCHAR NOT = "/"
             IF NUMERIC-RATE NOT = B-DISCOUNTPERITEM (SUB-1)
                MOVE "STOCK ITEM CHANGED"   TO WS-DAILY-1ST
                MOVE WS-STOCKNUMBER         TO WS-DAILY-2ND
                MOVE "QUOTE AC"             TO WS-DAILY-3RD1
                MOVE WS-ACCOUNT-NUMBER      TO WS-DAILY-3RD2
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
                                      F-EDNAMEFIELDAMOUNTDIS
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.
       FILL-080.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "TAX" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
             IF SP-1STCHAR = "/"
                GO TO FILL-070
             ELSE
                GO TO FILL-048.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-080.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO B-TAX (SUB-1).
       FILL-090.
            PERFORM ERROR-020
            PERFORM ERROR1-020
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR = "/" OR = "*"
                 MOVE " " TO B-INSTOCK (SUB-1)
                 GO TO FILL-095.
            IF B-ORDERQTY (SUB-1) NOT > ST-QTYONHAND
                 MOVE "*" TO B-INSTOCK (SUB-1)
            ELSE
                 MOVE " " TO B-INSTOCK (SUB-1).
       FILL-095.
            MOVE 0 TO WS-PRICESAVE.
            PERFORM ERROR1-020.
            PERFORM ERROR-020.
            MOVE "Y" TO WS-MUST-PRINT.
            ADD 1    TO SUB-1 F-INDEX.
            IF WS-LINECHANGED = "N"
                MOVE SUB-1 TO SUB-25.
            MOVE "N" TO WS-LINECHANGED.
            MOVE 0 TO SUB-3.
            IF SUB-1 > 150             
                MOVE 150 TO SUB-1 SUB-25
                MOVE "150 LINES ARE UP, PRESS 'ESC' TO TAB."
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
              MOVE "STTRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
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
       ENTER-STOCK-DETAILS SECTION.
       ESTD-005.
           MOVE ST-DESCRIPTION1   TO B-STOCKDESCRIPTION (SUB-1)
           MOVE ST-DESCRIPTION2   TO B-STOCKDESCRIPTION2 (SUB-1)
           MOVE ST-UNITOFMEASURE  TO B-UNIT (SUB-1).
           MOVE ST-PRICE          TO B-STOCKPRICE (SUB-1).
           MOVE ST-AVERAGECOST    TO B-STOCKCOST (SUB-1).
           IF WS-GSTNO = "EXPORT" OR = "EXPORT      "
                MOVE "N"          TO B-TAX (SUB-1)
           ELSE
                MOVE "Y"          TO B-TAX (SUB-1).
                
           IF STDISC-PERCENT > 0
               MOVE STDISC-PERCENT TO B-DISCOUNTPERITEM (SUB-1)
               GO TO ESTD-999.
           IF WS-ZERODIS = "Y"
               MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1)
               GO TO ESTD-999.
           IF WS-INVOICEDISCOUNT > 0
               MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1)
               GO TO ESTD-999.
                
           IF WS-DISCOUNT-CODE = "0" OR = " "
               MOVE 0 TO B-DISCOUNTPERITEM (SUB-1).
               GO TO ESTD-999.
           IF WS-DISCOUNT-CODE = "1"
               MOVE ST-DISCOUNT1 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO ESTD-999.
           IF WS-DISCOUNT-CODE = "2"
               MOVE ST-DISCOUNT2 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO ESTD-999.
           IF WS-DISCOUNT-CODE = "3"
               MOVE ST-DISCOUNT3 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO ESTD-999.
           IF WS-DISCOUNT-CODE = "4"
               MOVE ST-DISCOUNT4 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO ESTD-999.
           IF WS-DISCOUNT-CODE = "5"
               MOVE ST-DISCOUNT5 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO ESTD-999.
           IF WS-DISCOUNT-CODE = "6"
               MOVE ST-DISCOUNT6 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO ESTD-999.
           IF WS-DISCOUNT-CODE = "7"
               MOVE ST-DISCOUNT7 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO ESTD-999.
           IF WS-DISCOUNT-CODE = "8"
               MOVE ST-DISCOUNT8 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO ESTD-999.
           IF WS-DISCOUNT-CODE = "9"
               MOVE ST-DISCOUNT9 TO B-DISCOUNTPERITEM (SUB-1).
       ESTD-999.
           EXIT.
      *
       READ-TOOLKITS SECTION.
       R-KIT-005.
           MOVE ST-STOCKNUMBER TO TO-TOOLKIT-NUMBER.
           MOVE " "            TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "THERE IS NO SUCH TOOLKIT LISTED, 'ESC' RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
              GO TO R-KIT-999.
       R-KIT-010.
           READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               GO TO R-KIT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKIT FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO R-KIT-010.
           IF TO-COMPONENT-NUMBER = " "
               GO TO R-KIT-010.
           IF TO-TOOLKIT-NUMBER NOT = WS-TOOLKIT-NUMBER
               MOVE 1 TO WS-TOOLKIT-ST1
               GO TO R-KIT-999.
       R-KIT-020.
           MOVE TO-COMPONENT-NUMBER TO B-STOCKNUMBER (SUB-1)
                                       WS-STOCKNUMBER.
           PERFORM READ-STOCK.
           MOVE TO-QUANTITY       TO B-ORDERQTY (SUB-1).
           MOVE ST-DESCRIPTION1   TO B-STOCKDESCRIPTION (SUB-1)
           MOVE ST-DESCRIPTION2   TO B-STOCKDESCRIPTION2 (SUB-1)
           MOVE ST-UNITOFMEASURE  TO B-UNIT (SUB-1).
           MOVE ST-PRICE          TO B-STOCKPRICE (SUB-1).
           MOVE ST-AVERAGECOST    TO B-STOCKCOST (SUB-1).
           IF WS-GSTNO = "EXPORT" OR = "EXPORT      "
                MOVE "N"          TO B-TAX (SUB-1)
           ELSE
                MOVE "Y"          TO B-TAX (SUB-1).
           
           IF STDISC-PERCENT > 0
               MOVE STDISC-PERCENT TO B-DISCOUNTPERITEM (SUB-1)
               GO TO R-KIT-900.
           IF WS-ZERODIS = "Y"
               MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1)
               GO TO R-KIT-900.
           IF WS-INVOICEDISCOUNT > 0
               MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1)
               GO TO R-KIT-900.
               
           IF WS-DISCOUNT-CODE = "1"
               MOVE ST-DISCOUNT1 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO R-KIT-900.
           IF WS-DISCOUNT-CODE = "2"
               MOVE ST-DISCOUNT2 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO R-KIT-900.
           IF WS-DISCOUNT-CODE = "3"
               MOVE ST-DISCOUNT3 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO R-KIT-900.
           IF WS-DISCOUNT-CODE = "4"
               MOVE ST-DISCOUNT4 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO R-KIT-900.
           IF WS-DISCOUNT-CODE = "5"
               MOVE ST-DISCOUNT5 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO R-KIT-900.
           IF WS-DISCOUNT-CODE = "6"
               MOVE ST-DISCOUNT6 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO R-KIT-900.
           IF WS-DISCOUNT-CODE = "7"
               MOVE ST-DISCOUNT7 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO R-KIT-900.
           IF WS-DISCOUNT-CODE = "8"
               MOVE ST-DISCOUNT8 TO B-DISCOUNTPERITEM (SUB-1)
               GO TO R-KIT-900.
           IF WS-DISCOUNT-CODE = "9"
               MOVE ST-DISCOUNT9 TO B-DISCOUNTPERITEM (SUB-1).
       R-KIT-900.
           IF SUB-1 < 150
              ADD 1 TO SUB-1
              GO TO R-KIT-010.
       R-KIT-999.
           EXIT.
      *
       CHECK-IF-ENTERED-BEFORE SECTION.
       CIEB-005.
            IF SUB-1 = 1
               GO TO CIEB-999.
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
            DISPLAY "STOCKNUMBER ALREADY ENTERED ON LINE NUMBER"
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
       REMOVE-LEADING-ZEROS SECTION.
       RLZ-000.
           MOVE " " TO ALPHA-RATE
                       WS-QUOTE-CHECK
                       WS-QUOTE-NAME.
       RLZ-002.
           MOVE WS-QUOTATION TO WS-QUOTE-CHECK.

           MOVE 1 TO SUB-1.
           MOVE "Q" TO AL-RATE (SUB-1).
           MOVE 2 TO SUB-2.
      * NEW LINE BELOW TO NOT REMOVE LEADING ZEROS AS THIS LEADS TO 
      * QUOTE NUMBERS BELOW 100000 LOOKING LIKE Q67890 -03
           GO TO RLZ-005.
       RLZ-003.
           IF SUB-1 < 7
            IF WS-O-C (SUB-1) = 0
               ADD 1 TO SUB-1
               GO TO RLZ-003.
       RLZ-005.
           MOVE WS-O-C (SUB-1) TO AL-RATE (SUB-2).
           IF SUB-2 NOT > 25
              ADD 1 TO SUB-1 SUB-2
              GO TO RLZ-005.
      *     MOVE " " TO WS-QUOTATION.
           MOVE ALPHA-RATE TO WS-QUOTE-NAME.
           MOVE 1 TO SUB-1 SUB-2.
       RLZ-999.
           EXIT.
      *
       MOVE-EMAIL-FROM-EIMAGE-SETUP SECTION.
       MERFES-005.
             MOVE WS-TEMP-EMAIL-FILE TO WS-EMAIL-FINAL.
             
             MOVE "/ctools/equote/" TO WS-EF-FIL.
       MERFES-999.
            EXIT.
      *
       GET-EMAIL-QUOTE-NAME SECTION.
       GEQN-000.
           MOVE " " TO ALPHA-RATE
                       WS-QUOTE-CHECK.
       GEQN-002.
           MOVE WS-QUOTE-NAME TO WS-QUOTE-CHECK.

           MOVE 1 TO SUB-1
                     SUB-2.
       GEQN-005.
           MOVE WS-O-C (SUB-1) TO AL-RATE (SUB-2).
           IF SUB-2 NOT > 20
              ADD 1 TO SUB-1 SUB-2.
              
           IF WS-O-C (SUB-1) NOT = "."
              GO TO GEQN-005.

           MOVE " "        TO WSF-QUOTE.
           MOVE ALPHA-RATE TO WSF-QUOTE.

           MOVE WSF-MAIL-QUOTE TO WS-TEMP-EMAIL-FILE
           MOVE 1 TO SUB-1 SUB-2.
       GEQN-999.
           EXIT.
      *
       CHECK-NAME-LENGTH SECTION.
       CNL-000.
           MOVE SPACES TO ALPHA-RATE
                          WS-NAME-LENGTH.
           MOVE ALL "*" TO ALPHA-RATE.
       CNL-002.
           MOVE PA-NAME TO WS-NAME-LENGTH.
           MOVE 40 TO SUB-1.
       CNL-003.
           IF WS-NL (SUB-1) = " "
               SUBTRACT 1 FROM SUB-1
               GO TO CNL-003.
           IF SUB-1 = 39 OR = 40
              MOVE WS-NAME-LENGTH TO ALPHA-RATE
              GO TO CNL-015.
           COMPUTE SUB-2 = 40 - ((40 - SUB-1) / 2).
           
           IF SUB-2 = 0 OR = 40
              MOVE WS-NAME-LENGTH TO ALPHA-RATE
              GO TO CNL-015.
           ADD 1 TO SUB-1 SUB-2
           MOVE " " TO AL-RATE (SUB-2)
           SUBTRACT 1 FROM SUB-1 SUB-2.
       CNL-005.
           MOVE WS-NL (SUB-1) TO AL-RATE (SUB-2).
           IF SUB-1 > 1
              SUBTRACT 1 FROM SUB-1 SUB-2
              GO TO CNL-005.
       CNL-010.
           IF SUB-2 > 1
              SUBTRACT 1 FROM SUB-2.
           MOVE " " TO AL-RATE (SUB-2)
           MOVE SPACES TO PA-NAME.
       CNL-015.
           MOVE ALPHA-RATE TO PA-NAME.
           MOVE 1 TO SUB-1 SUB-2.
       CNL-999.
           EXIT.
      *
       ENTER-XQS-DETAILS SECTION.
       XQS-000.
           PERFORM GET-USER-MAIL-NAME.
           MOVE WS-pbValue TO WS-XQS-SNAME
                              WS-XQS-ENAME
                              WS-XQS-FROM-NAME
                              WS-HYLA-FROM-NAME.
       XQS-001.
           MOVE " "           TO ALPHA-RATE WS-FAX-CHECK
           MOVE WS-XQS-BEG    TO ALPHA-RATE
           MOVE 10            TO SUB-1
           MOVE WS-CONTACT    TO WS-FAX-CHECK WS-HYLA-TO-NAME
           MOVE 40            TO SUB-2.
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
           MOVE WS-QUOTATION        TO WS-XQS-COMMENT
                                       WS-HYLA-COMMENT
           MOVE " "                 TO WS-FAX-CHECK
           MOVE WS-XQS-COMMENT      TO WS-FAX-CHECK
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
      *     MOVE 1 TO SUB-1.
       XQS-9991.
      *     MOVE WS-XQS-LINE (SUB-1) TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     IF SUB-1 < 5
      *        ADD 1 TO SUB-1
      *        GO TO XQS-9991.
       XQS-9999.
           EXIT.
      *
       CHECK-QUOTE-REF SECTION.
      *ONLY USED FOR MURATA FAX SENDING
       OC-001.
           MOVE " "          TO WS-QUOTE-CHECK
           MOVE WS-QUOTATION TO WS-QUOTE-CHECK
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
           MOVE WS-QUOTE-CHECK TO WS-REST.
           MOVE WS-SP-PRINT    TO WS-PRINTER.
           MOVE SUB-1          TO SUB-5.
           SUBTRACT 1 FROM SUB-1.
           ADD 9               TO SUB-1.
           MOVE WS-SP-PRINT    TO FAX-ASCIIFILENAME.
           MOVE SUB-1          TO FAX-CBASCIIFILENAME.
           ADD 5               TO SUB-1.
           MOVE SUB-1          TO FAX-CBFAXFILENAME.
           MOVE SUB-5          TO SUB-1.
       OC-017.
           MOVE "." TO WS-O-C (SUB-1)
           ADD 1 TO SUB-1
           MOVE "F" TO WS-O-C (SUB-1)
           ADD 1 TO SUB-1
           MOVE "a" TO WS-O-C (SUB-1)
           ADD 1 TO SUB-1
           MOVE "x" TO WS-O-C (SUB-1).
           MOVE WS-QUOTE-CHECK TO WS-FAX-REST.
           MOVE WS-FAX-SEND    TO FAX-FAXFILENAME.
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
           MOVE 2910 TO POS.
           DISPLAY "THERE IS AN ERROR IN THE FAX PHONE NUMBER," AT POS.
           MOVE 3010 TO POS.
           DISPLAY "  PRESS <RETURN> TO RE-ENTER A FAX NUMBER." AT POS.
           MOVE 3065 TO POS.
           ACCEPT WS-ACCEPT AT POS.
       FNC-905.
           MOVE " " TO WS-MESSAGE.
           MOVE 2910 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 3010 TO POS.
           DISPLAY WS-MESSAGE AT POS.
       FNC-910.
           MOVE 2910 TO POS.
           DISPLAY "ENTER A NEW FAX NUMBER: [                   ]"
               AT POS.
           ADD 25 TO POS.
           ACCEPT WS-FAX-NUMBER AT POS.
           IF WS-FAX-NUMBER = " "
               GO TO FNC-010.
           MOVE 2910 TO POS.
           DISPLAY "                                             "
               AT POS.
           GO TO FNC-010.
       FNC-999.
           EXIT.
      *
       DISPLAY-LINE-ITEMS SECTION.
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
            MOVE "TAX" TO F-FIELDNAME
            MOVE 3     TO F-CBFIELDNAME.
            IF WS-GSTNO = "EXPORT" OR = "EXPORT      "
                MOVE "N" TO F-NAMEFIELD
                            B-TAX (SUB-1)
            ELSE
                MOVE "Y" TO F-NAMEFIELD
                            B-TAX (SUB-1).
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       DLI-020.
           MOVE ST-QTYONHAND    TO WS-QTYONHAND
           MOVE ST-QTYONRESERVE TO WS-QTYONRESERVE
           MOVE ST-QTYONORDER   TO WS-QTYONORDER
           MOVE ST-QTYONBORDER  TO WS-QTYONBORDER
           MOVE 2110 TO POS
           DISPLAY WS-ONHAND-LINE AT POS.

           PERFORM CHECK-DATE-LAST-SOLD.
           PERFORM SCROLL-050.
       DLI-999.
           EXIT.
      *
       CHECK-DATE-LAST-SOLD SECTION.
       CDLS-005.
           IF ST-QTYONHAND > 0
           OR ST-QTYONRESERVE > 0
               GO TO CDLS-999.
                      
           MOVE WS-DATE TO    WS-AGE-DATE.
           IF WS-AGE-MM > 3
              SUBTRACT 3 FROM WS-AGE-MM
           ELSE
              ADD 12       TO WS-AGE-MM
              SUBTRACT 1 FROM WS-AGE-YY.
           IF ST-LASTPRICECHANGE NOT > WS-AGE-DATE
              GO TO CDLS-010
           ELSE
              GO TO CDLS-999.
       CDLS-010.
           MOVE WS-DATE TO    WS-AGE-DATE.
           IF WS-AGE-MM > 3
              SUBTRACT 3 FROM WS-AGE-MM
           ELSE
              ADD 12       TO WS-AGE-MM
              SUBTRACT 1 FROM WS-AGE-YY.

           IF ST-LASTSALEDATE NOT > WS-AGE-DATE
             MOVE "THIS ITEM WAS PREV SOLD OVER 3 MONTHS AGO & PRICES" &
             "/COSTS MAY HAVE CHANGED." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE 
             "PLEASE CHECK PRICES & COSTS BEFORE QUOTING NEW CUSTOMER."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
       CDLS-999.
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
      * FOR THE QUOTE IS > THE MAX ITEM DISCOUNT AND IF SO
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
            IF SUB-1 < 151
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
           IF WS-SBREP-ST1 NOT = 0 
              MOVE "SBREP FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SBREP-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SBREP-ST1
               GO TO RSB-030.
           MOVE SBREP-INT TO WS-SOLD-BY.
       RSB-040.
           CLOSE SBREP-MASTER.
       RSB-999.
            EXIT.
      *
       CANCEL-STOCK-TRANS SECTION.
       CAN-TRANS-000.
           IF B-NEWLINE (SUB-1) = " "
      *         MOVE "B-NEWLINE (SUB-1) = ' ' " TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               GO TO CAN-TRANS-999.
           MOVE WS-INVOICE        TO STTR-REFERENCE1.
           MOVE 8                 TO STTR-TYPE.
           MOVE B-STTRANS (SUB-1) TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY.
           READ STOCK-TRANS-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
               MOVE 
             "CANCEL ST-TRANS BUSY ON READ, IN 2 SEC GOING TO RETRY."
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
             "DELETE ST-TRANS BUSY ON READ, IN 2 SEC GOING TO RETRY."
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
       CANCEL-TRANSACTION SECTION.
       CAN-005.
            COMPUTE SUB-2 = SUB-1 + 1.
       CAN-010.
            IF SUB-2 > 150 
               MOVE 150 TO SUB-1 SUB-2
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
             MOVE C-LINE (SUB-2)    TO C-LINE (SUB-1)
             MOVE BODY-LINE (SUB-2) TO BODY-LINE (SUB-1)
             ADD 1 TO SUB-1 SUB-2
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
       EMPTY-LINE SECTION.
       EMPTY-LINE-005.
           MOVE SUB-1 TO SUB-3
           COMPUTE SUB-3 = SUB-3 + 1
           MOVE SUB-25 TO SUB-2
           COMPUTE SUB-1 = SUB-2 - 1.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1.
           MOVE " " TO B-NEWLINE (SUB-1)
           MOVE 0   TO B-STTRANS (SUB-1).
       EMPTY-LINE-010.
           MOVE C-LINE (SUB-1)    TO C-LINE (SUB-2)
           MOVE BODY-LINE (SUB-1) TO BODY-LINE (SUB-2)
           IF B-STTRANS (SUB-2) NOT = 0
           ADD 1                  TO B-STTRANS (SUB-2).
           IF SUB-1 NOT > SUB-3
               GO TO EMPTY-LINE-090.
           SUBTRACT 1 FROM SUB-1 SUB-2
           GO TO EMPTY-LINE-010.
       EMPTY-LINE-090.
           MOVE " " TO C-LINE (SUB-1).
           MOVE " " TO B-STOCKNUMBER (SUB-1)
                       B-STOCKDESCRIPTION (SUB-1)
                       B-STOCKDESCRIPTION2 (SUB-1)
                       B-INSTOCK (SUB-1)
                       B-TAX (SUB-1)
                       B-UNIT (SUB-1)
                       B-STORE (SUB-1).
      *                 B-NEWLINE (SUB-1).
           MOVE 0   TO B-ORDERQTY (SUB-1)
                       B-SHIPQTY (SUB-1)
                       B-SHIPPEDQTY (SUB-1)
                       B-STOCKPRICE (SUB-1)
                       B-STOCKCOST (SUB-1)
                       B-STTRANS (SUB-1)
                       B-INVOICED (SUB-1)
                       B-DISCOUNTPERITEM (SUB-1)
                       B-NETT (SUB-1).
       EMPTY-LINE-999.
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
               (B-ORDERQTY (SUB-3) * B-STOCKPRICE (SUB-3)).
           COMPUTE WS-DISCOUNT ROUNDED = ((B-ORDERQTY (SUB-3) * 
             B-STOCKPRICE (SUB-3)) * B-DISCOUNTPERITEM (SUB-3)) / 100.
           SUBTRACT WS-DISCOUNT FROM WS-WORKTOTAL.
           ADD WS-WORKTOTAL TO WS-PRICETOTAL.
           IF B-TAX (SUB-3) = "Y"
               COMPUTE WS-WORKTOTAL ROUNDED = 
               (WS-WORKTOTAL + (WS-WORKTOTAL * WS-GST-PERCENT / 100)).
           ADD WS-WORKTOTAL TO WS-WORKTOTAL2.
           COMPUTE WS-COSTTOTAL = (WS-COSTTOTAL +
               B-STOCKCOST (SUB-3) * B-ORDERQTY (SUB-3)).
           IF WS-WORKTOTAL2 > 9999999.99
              MOVE "** YOUR QUOTE VALUE HAS EXCEEDED R9,999,999.99 **"
              TO WS-MESSAGE
              PERFORM ERROR-000
              MOVE " **  YOU MUST CANCEL YOUR LAST STOCK LINE ! **"
              TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              MOVE " " TO WS-MESSAGE
              PERFORM ERROR-020
              SUBTRACT 1 FROM SUB-1
              IF F-INDEX > 1
                SUBTRACT 1 FROM F-INDEX
                PERFORM SCROLLING
              ELSE
                PERFORM SCROLLING.
       RUN-015.
           ADD 1 TO SUB-3.
           IF SUB-3 > 150
               GO TO RUN-020.
           GO TO RUN-010.
       RUN-020.
           COMPUTE WS-MARGIN = WS-PRICETOTAL - WS-COSTTOTAL.
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / WS-COSTTOTAL) * 100.

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
           MOVE 0 TO SUB-3.
       RUN-999.
           EXIT.
      *
       CALCULATE-TOTALS SECTION.
       CT-000.
           MOVE 1 TO SUB-1.
           MOVE 0 TO WS-TAXABLETOTAL WS-NONTAXABLETOTAL
                     WS-WORKTOTAL    WS-DISCOUNT    WS-COSTTOTAL
                     WS-PRICETOTAL   WSF-XPORTTOTAL WS-DISCOUNTREG.
       CT-010.
           IF B-STOCKNUMBER (SUB-1) = " "
               GO TO CT-020.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               GO TO CT-015.
           COMPUTE WS-WORKTOTAL =
               B-ORDERQTY (SUB-1) * B-STOCKPRICE (SUB-1).
           COMPUTE WS-DISCOUNT ROUNDED =
              WS-WORKTOTAL * B-DISCOUNTPERITEM (SUB-1) / 100.
           ADD WS-DISCOUNT TO WS-DISCOUNTREG.
           SUBTRACT WS-DISCOUNT FROM WS-WORKTOTAL.
           COMPUTE WS-COSTTOTAL = (WS-COSTTOTAL +
               B-STOCKCOST (SUB-1) * B-ORDERQTY (SUB-1)).
           IF WS-GSTNO NOT = "EXPORT"
               ADD WS-WORKTOTAL TO WS-TAXABLETOTAL
            ELSE
               ADD WS-WORKTOTAL TO WSF-XPORTTOTAL.
           ADD WS-WORKTOTAL TO WS-PRICETOTAL.
           MOVE WS-WORKTOTAL TO B-NETT (SUB-1).
       CT-015.
           ADD 1 TO SUB-1.
           IF SUB-1 > 150
               GO TO CT-020.
           IF SUB-1 < 151
               GO TO CT-010.
       CT-020.
           COMPUTE WS-SUBTOTAL = WS-TAXABLETOTAL + 
                                 WS-NONTAXABLETOTAL + 
                                 WSF-XPORTTOTAL.
           COMPUTE WS-ADDONAMT = WS-ADDONFREIGHT + WS-POSTADDON + 
                                 WS-HANDADDON + WS-MISCADDON.
           COMPUTE WS-TAXAMT ROUNDED =
             (WS-TAXABLETOTAL + WS-ADDONAMT) * WS-GST-PERCENT / 100.
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
      *
      *TAKEN OUT WHEN "CHANGE-DISCOUNT-PER-LINE" SECTION CHANGED
      * FOR NEW DISCOUNT INFO 17/3/1999
      *      IF WS-INVOICEDISCOUNT > 0
      *         MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1).
      *      IF WS-INVOICEDISCOUNT = 0
      *       IF F-EXIT-CH = X"1D"
      *         MOVE WS-INVOICEDISCOUNT TO B-DISCOUNTPERITEM (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 151
               GO TO SG-010.
       SG-999.
            EXIT.
      *
       WRITE-INCR-REGISTER SECTION.
       WRIC-050.
            MOVE WS-INVOICE        TO INCR-INVOICE
            MOVE 8                 TO INCR-TRANS
            MOVE WS-ACCOUNT-NUMBER TO INCR-ACCOUNT
            MOVE WS-POORDERNO      TO INCR-PORDER
            MOVE WS-GSTNO          TO INCR-GSTNO
            IF WS-NEWORDER = "Y"
               MOVE WS-DATE        TO INCR-DATE.
            MOVE WSAN-CODE         TO INCR-SALES
            MOVE WS-INVOICETOTAL   TO INCR-INVCRED-AMT
            MOVE WS-TAXAMT         TO INCR-TAX
            MOVE WS-ADDONAMT       TO INCR-ADDONS
            MOVE WS-DISCOUNTREG    TO INCR-DISCOUNT
            MOVE WS-COSTTOTAL      TO INCR-INVCRED-COST
            MOVE WS-SOLD-BY         TO INCR-SB-TYPE
            MOVE WS-DRTRANS-NO     TO INCR-DRTRANS-NO
      *      MOVE WS-STTRANS-NO     TO INCR-STTRANS-NO
            MOVE "N"               TO INCR-PRINTED
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
            MOVE WS-DELIVERVIA     TO INCR-DELIVERY
            MOVE WS-BINNO          TO INCR-BIN
            MOVE WS-COMMENTLINE    TO INCR-COMMENT
            MOVE INCR-INVOICE      TO INCR-BO-INV-NO
            MOVE WS-DATE           TO INCR-BO-DATE
            MOVE WS-POSTADDON      TO INCR-ADDPOST
            MOVE WS-ADDONFREIGHT   TO INCR-ADDFREIGHT
            MOVE WS-HANDADDON      TO INCR-ADDLABOUR
            MOVE WS-MISCADDON      TO INCR-ADDMISC
            MOVE SUB-20            TO INCR-LINENO.
            IF WS-NEWORDER = "Y"
               MOVE 1 TO INCR-COPY-NUMBER
            ELSE
               ADD 1  TO INCR-COPY-NUMBER.
       WRIC-060.
            IF WS-NEWORDER = "N"
                GO TO WRIC-065.
            WRITE INCR-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 NOT = 0
                MOVE "REGISTER BUSY ON WRITE, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-INCR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-INCR-ST1
                GO TO WRIC-065.
       WRIC-065.
            REWRITE INCR-REC
                  INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 NOT = 0
                MOVE "REGISTER BUSY ON REWRITE, 'ESC' TO RETRY"
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
       CHECK-REGISTER SECTION.
       CRS-050.
           MOVE WS-ACCOUNT-NUMBER TO INCR-ACCOUNT.
           MOVE WS-POORDERNO      TO INCR-PORDER.
           START INCR-REGISTER KEY NOT < INCR-ALT-KEY.
           IF WS-INCR-ST1 NOT = 0
               MOVE "Y" TO WS-NEWORDER
               GO TO CRS-999.
       CRS-060.
           READ INCR-REGISTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
               MOVE 0 TO WS-INCR-ST1
               GO TO CRS-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ-NEXT-LOCK, 'ESC' TO RETRY."
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
               GO TO CRS-900.
           IF INCR-ACCOUNT NOT = WS-ACCOUNT-NUMBER
            IF INCR-PORDER = WS-POORDERNO
               MOVE "Y" TO WS-NEWORDER
               GO TO CRS-900.
           IF INCR-ACCOUNT NOT = WS-ACCOUNT-NUMBER
            IF INCR-PORDER NOT = WS-POORDERNO
               MOVE "Y" TO WS-NEWORDER
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
               MOVE "C"          TO WS-NEWORDER
               MOVE INCR-INVOICE TO WS-INVOICE
               GO TO CRS-999.
           MOVE "Y" TO WS-NEWORDER.
       CRS-900.
           UNLOCK INCR-REGISTER.
       CRS-999.
           EXIT.
      *
       READ-INVOICE-REGISTER SECTION.
       RIR-000.
           MOVE WS-INVOICE TO INCR-INVOICE WS-QUOTE.
           MOVE 8          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-INCR-ST1
               MOVE "Y" TO WS-NEWORDER
               GO TO RIR-999.
       RIR-005.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
       RIR-006.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-INCR-ST1
               MOVE "Y" TO WS-NEWORDER
               GO TO RIR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-INCR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-INCR-ST1
                GO TO RIR-005.
           IF INCR-PRINTED = "Y" OR = "L"
               MOVE "THIS QUOTE HAS BEEN ENTERED AS AN ORDER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE "           ORDER NO:" TO WS-DAILY-1ST
               MOVE INCR-BO-INV-NO         TO F-EDNAMEFIELDNUM
               MOVE F-EDNAMEFIELDNUM       TO WS-DAILY-2ND
               MOVE " "                    TO WS-DAILY-3RD WS-DAILY-4TH
               MOVE WS-DAILY-MESSAGE       TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR-020
               MOVE "C" TO WS-NEWORDER
               GO TO RIR-999.
       RIR-010.
           MOVE INCR-ACCOUNT        TO WS-ACCOUNT-NUMBER.
           MOVE INCR-GSTNO          TO WS-GSTNO.
           MOVE INCR-DATE           TO WS-INVOICEDATE.
           MOVE INCR-SALES          TO WSAN-CODE.
           MOVE INCR-INVCRED-AMT    TO WS-INVOICETOTAL.
           MOVE INCR-TAX            TO WS-TAXAMT.
           MOVE INCR-ADDONS         TO WS-ADDONAMT.
           MOVE INCR-SB-TYPE        TO WS-SOLD-BY.
           MOVE INCR-DRTRANS-NO     TO WS-DRTRANS-NO.
      *     MOVE INCR-STTRANS-NO     TO WS-STTRANS-NO.
           COMPUTE WS-SUBTOTAL =
                WS-INVOICETOTAL - (WS-TAXAMT + WS-ADDONAMT).
           MOVE INCR-NAME      TO WS-NAME.
           MOVE INCR-ADD1      TO WS-ADD1.
           MOVE INCR-ADD2      TO WS-ADD2.
           MOVE INCR-ADD3      TO WS-ADD3.
           MOVE INCR-CODE      TO WS-POSTCODE.
           MOVE INCR-DEL1      TO WS-DELADD1.
           MOVE INCR-DEL2      TO WS-DELADD2.
           MOVE INCR-DEL3      TO WS-DELADD3.
           MOVE INCR-TERMS     TO WS-TERMOFSALE.
           MOVE INCR-PORDER    TO WS-POORDERNO.
           MOVE INCR-CONTACT   TO WS-CONTACT.
           MOVE INCR-PHONE     TO WS-PHONE.
           MOVE INCR-DELIVERY  TO WS-DELIVERVIA.
           MOVE INCR-BIN        TO WS-BINNO.
           MOVE INCR-COMMENT    TO WS-COMMENTLINE.
           MOVE INCR-ADDPOST    TO WS-POSTADDON.
           MOVE INCR-ADDFREIGHT TO WS-ADDONFREIGHT.
           MOVE INCR-ADDLABOUR  TO WS-HANDADDON.
           MOVE INCR-ADDMISC    TO WS-MISCADDON.
           MOVE INCR-LINENO     TO SUB-20 SUB-25.

           MOVE "N" TO WS-NEWORDER.
       RIR-999.
           EXIT.
      *
       DELETE-INVOICE-REGISTER SECTION.
       DIR-010.
           MOVE 2910 TO POS.
           DISPLAY "DELETING QUOTE-REGISTER................" AT POS.
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
       DELETE-STOCK-TRANS SECTION.
       DST-000.
           MOVE 2910 TO POS.
           DISPLAY "DELETING ST-QUOTE-TRANSACTIONS........." AT POS.
           MOVE 1 TO SUB-1.
           MOVE WS-INVOICE TO STTR-REFERENCE1.
           MOVE 8          TO STTR-TYPE.
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
              MOVE 0 TO WS-STTRANS-ST1
               MOVE 
             "DST ST-TRANS BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
              GO TO DST-010.
           IF STTR-REFERENCE1 NOT = WS-INVOICE
              UNLOCK STOCK-TRANS-FILE
              GO TO DST-999.
           IF STTR-TYPE NOT = 8
              UNLOCK STOCK-TRANS-FILE
              GO TO DST-999.
       DST-900.
           DELETE STOCK-TRANS-FILE
               INVALID KEY NEXT SENTENCE.
       DST-950.
           ADD 1 TO SUB-1.
           IF SUB-1 > 150
              GO TO DST-999.
           GO TO DST-010.
       DST-999.
           EXIT.
      *
       READ-STOCK-TRANSACTIONS SECTION.
       RSTT-000.
           MOVE 2910 TO POS
           DISPLAY "READING ST-QUOTE-TRANSACTIONS.........." AT POS.
           MOVE 1          TO SUB-1
           MOVE 8          TO STTR-TYPE
           MOVE WS-INVOICE TO STTR-REFERENCE1
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
              MOVE 0 TO WS-STTRANS-ST1
              MOVE 
             "ST-TRANS BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               GO TO RSTT-010.
           IF STTR-REFERENCE1 NOT = WS-INVOICE
              GO TO RSTT-999.
           IF STTR-TYPE NOT = 8
              GO TO RSTT-010.
           MOVE STTR-TRANSACTION-NUMBER TO B-STTRANS (SUB-1).
           MOVE STTR-STOCK-NUMBER       TO B-STOCKNUMBER (SUB-1)
                                           SPLIT-STOCK.
           MOVE "N"               TO B-NEWLINE (SUB-1).
           MOVE STTR-INV-NO       TO B-INVOICED (SUB-1).
           IF SP-1STCHAR = "*"
               GO TO RSTT-020.
           MOVE STTR-DESC1        TO B-STOCKDESCRIPTION (SUB-1).
           MOVE STTR-DESC2        TO B-STOCKDESCRIPTION2 (SUB-1).
           MOVE STTR-ORDERQTY     TO B-ORDERQTY (SUB-1).
           MOVE STTR-PRICE        TO B-STOCKPRICE (SUB-1).
           MOVE STTR-COST-VALUE   TO B-STOCKCOST (SUB-1).
           MOVE STTR-ITEMDISC     TO B-DISCOUNTPERITEM (SUB-1).
           MOVE STTR-TAX          TO B-TAX (SUB-1).
           MOVE STTR-UNIT         TO B-UNIT (SUB-1).
           MOVE STTR-SALES-VALUE  TO B-NETT (SUB-1).
           COMPUTE WS-COSTTOTAL = WS-COSTTOTAL +
               (B-STOCKCOST (SUB-1) * B-ORDERQTY (SUB-1)).
           GO TO RSTT-050.
       RSTT-020.
           MOVE COMMENT-FIELDS      TO C-LINE (SUB-1).
       RSTT-050.
           ADD 1 TO SUB-1.
           MOVE SUB-1 TO SUB-25.
           IF SUB-1 > 150
              MOVE 150 TO SUB-1 SUB-25
              GO TO RSTT-999.
           GO TO RSTT-010.
       RSTT-999.
           EXIT.
      *
       WRITE-STOCK-TRANSACTIONS SECTION.
       WST-00000.
            MOVE 1          TO SUB-1
            MOVE 0          TO WS-STTRANS-NO
            MOVE 1          TO STTR-TRANSACTION-NUMBER
            MOVE WS-INVOICE TO STTR-REFERENCE1
            MOVE 8          TO STTR-TYPE.
            START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
            IF B-STOCKNUMBER (SUB-1) = " "
               GO TO WST-999.
       WST-000.
            IF B-NEWLINE (SUB-1) NOT = " "
             IF B-STTRANS (SUB-1) NOT = 0
                 MOVE B-STTRANS (SUB-1) TO STTR-TRANSACTION-NUMBER
                                           WS-STTRANS-NO
                 GO TO WST-005.
            ADD 1              TO WS-STTRANS-NO.
            MOVE WS-STTRANS-NO TO STTR-TRANSACTION-NUMBER.
       WST-005.
            IF B-NEWLINE (SUB-1) NOT = " "
                READ STOCK-TRANS-FILE WITH LOCK
                   INVALID KEY NEXT SENTENCE.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
                                          STTR-STOCK-NUMBER.
            MOVE WS-ACCOUNT-NUMBER     TO STTR-ACCOUNT-NUMBER
            MOVE WS-INVOICEDATE        TO STTR-DATE
                                          STTR-AC-DATE
                                          STTR-ST-DATE
            MOVE B-INVOICED (SUB-1)    TO STTR-INV-NO
            MOVE "Q"                   TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE.
            IF SP-1STCHAR = "*"
                GO TO WST-015.
       WST-010.
            MOVE B-ORDERQTY (SUB-1)          TO STTR-ORDERQTY
            MOVE 0                           TO STTR-SHIPQTY
                                                STTR-SHIPPEDQTY
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
            MOVE C-LINE (SUB-1)         TO COMMENT-FIELDS.
       WST-018.
            IF B-NEWLINE (SUB-1) = " "
               WRITE STOCK-TRANS-REC
                  INVALID KEY NEXT SENTENCE
            ELSE
               REWRITE STOCK-TRANS-REC
                  INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 = 0
                GO TO WST-020.
            IF B-NEWLINE (SUB-1) = " "
               MOVE "N" TO B-NEWLINE (SUB-1)
            ELSE
               MOVE " " TO B-NEWLINE (SUB-1).
            GO TO WST-018.
       WST-020.
           IF STTR-TRANSACTION-NUMBER = 0
              MOVE "ST-TRANS-NO = 0 AT WST-020, ADVISE THE BOSS."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
            ADD 1 TO SUB-1.
            IF SUB-1 < 151
              IF B-STOCKNUMBER (SUB-1) = " "
                GO TO WST-999.
            IF SUB-1 < 151
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
            MOVE 7                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA
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
               MOVE 
               "GOODS CANNOT BE SUPPLIED AS THIS ACCOUNT IS ON HOLD."
               TO WS-MESSAGE
               PERFORM ERROR1-000.
           IF DR-SUPPLY-Y-N = "S"
               MOVE
               "THIS ACCOUNT HAS BEEN SUSPENDED. CHECK WITH A/C'S DEPT."
               TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE.
       FIND-020.
            MOVE DR-DISCOUNT-CODE  TO WS-DISCOUNT-CODE.

            MOVE "COPYDESC"             TO F-FIELDNAME
            MOVE 8                      TO F-CBFIELDNAME
            MOVE " QUOTE COPY NUMBER :" TO F-NAMEFIELD
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

            MOVE "DELADD3"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD3 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POORDERNO"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE 20           TO F-CBFIELDLENGTH
            MOVE WS-POORDERNO TO F-NAMEFIELD
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

            MOVE "PHONE"  TO F-FIELDNAME
            MOVE 5        TO F-CBFIELDNAME
            MOVE WS-PHONE TO F-NAMEFIELD
            MOVE 13       TO F-CBFIELDLENGTH
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

            MOVE "INVOICEDATE"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE WS-INVOICEDATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE   TO F-NAMEFIELD
            MOVE 10             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SOLDBY"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE WS-SOLD-BY TO F-NAMEFIELD
            MOVE 2         TO F-CBFIELDLENGTH
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
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDONFREIGHT"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE WS-ADDONFREIGHT TO F-EDNAMEFIELDADDON
            MOVE 7               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ADDON.

            MOVE "POSTADDON"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE 7            TO F-CBFIELDLENGTH
            MOVE WS-POSTADDON TO F-EDNAMEFIELDADDON
            PERFORM WRITE-FIELD-ADDON.

            MOVE "HANDADDON"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE 7            TO F-CBFIELDLENGTH
            MOVE WS-HANDADDON TO F-EDNAMEFIELDADDON
            PERFORM WRITE-FIELD-ADDON.

            MOVE "MISC.ADDON" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE 7            TO F-CBFIELDLENGTH
            MOVE WS-MISCADDON TO F-EDNAMEFIELDADDON
            PERFORM WRITE-FIELD-ADDON.

            MOVE "SUBTOTAL"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-SUBTOTAL TO F-EDNAMEFIELD9MIL
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.

            MOVE "ADDONAMT"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-ADDONAMT TO F-EDNAMEFIELD9MIL
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.

            MOVE "TAXAMT"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE WS-TAXAMT TO F-EDNAMEFIELD9MIL
            MOVE 10        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.

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
            IF SUB-1 > 150
               MOVE 150 TO SUB-1
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
               MOVE "UNKNOWN" TO DR-NAME
               GO TO RD-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTORS BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-000.
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
               MOVE "PARAMETER TERMS BUSY ON READ, 'ESC' TO RETRY."
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
            EXIT.
      *
       READ-DELIVERY-FILE SECTION.
       RDELIV-000.
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
               MOVE "PARAMETER DELV BUSY ON READ, 'ESC' TO RETRY."
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
       READ-QUOTE-FILE SECTION.
       RQUOTE-000.
            MOVE 1 TO SUB-1
                      PA-RECORD.
            MOVE 5 TO PA-TYPE.
       RQUOTE-010.
            READ PARAMETER-FILE NEXT
                AT END NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 10
                GO TO RQUOTE-999.
            IF PA-TYPE < 5
                GO TO RQUOTE-010.
            IF PA-TYPE > 5
                GO TO RQUOTE-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER QUOTE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RQUOTE-010.
            IF PARAMETER-REC = "           "
               GO TO RQUOTE-010.           
            MOVE PARAMETER-REC TO WS-QUOTE-OCCUR (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 = 10
               PERFORM ERROR-020
               GO TO RQUOTE-999.
            GO TO RQUOTE-010.
       RQUOTE-999.
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
               MOVE "SALES-ANALYSIS BUSY ON CLOSE" TO WS-MESSAGE
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
            MOVE "TAXAMT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-TAXAMT
                                 F-EDNAMEFIELDAMOUNT.
            PERFORM WRITE-FIELD-AMOUNT.
      *      IF WS-GSTNO = "EXPORT"
      *       IF WS-TAXAMT > 0
      *         MOVE
      *      "AN EXPORT SALE CANNOT HAVE VAT ALLOCATED TO IT, RE-ENTER."
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
           MOVE 3058 TO POS.
           MOVE ST-PRICE TO F-EDNAMEFIELDAMOUNT WS-PRICESAVE.
           DISPLAY F-EDNAMEFIELDAMOUNT AT POS.
           MOVE STPR-PRICE TO ST-PRICE.
           MOVE "Y"        TO B-SPECIAL (SUB-1).
       SPR-999.
           EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-005.
           READ STOCK-MASTER
               INVALID KEY
               MOVE "ERR" TO WSF-RR
               NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               PERFORM START-FOR-READ-NEXT
               MOVE " " TO ST-DESCRIPTION1
               GO TO R-ST-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK IN USE ON READ,'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-005.
           PERFORM ERROR-020.
       R-ST-010.
         IF ST-ANALYSIS = "D"
               MOVE
            "DON'T ENTER ORDER, ITEM TO BE DELETED WHEN ON HAND = ZERO."
                   TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           MOVE ST-DISCOUNT9 TO B-MAX-DISC (SUB-1).
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
       Copy "ComputeSpecialPrices".
       Copy "ComputeAssociatePrices".
      *
       CHECK-STOCK SECTION.
       CH-ST-005.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               PERFORM START-FOR-READ-NEXT
               MOVE " " TO ST-DESCRIPTION1
               GO TO CH-ST-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK IN USE ON CHECK-STOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO CH-ST-005.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       CH-ST-999.
           EXIT.
      *
       START-FOR-READ-NEXT SECTION.
       SFRN-001.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
             MOVE "STOCK BUSY ON STOCK-START-FOR-READ, 'ESC' TO RETRY."
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
               MOVE " " TO ST-STOCKNUMBER
               PERFORM START-FOR-READ-NEXT
               GO TO RNSI-005.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON READ-NEXT, 'ESC' TO RETRY."
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
               MOVE " " TO ST-STOCKNUMBER
               PERFORM START-FOR-READ-NEXT
               GO TO RPREV-005.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON READ-PREVIOUS, 'ESC' TO RETRY."
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
           IF SUB-1 < 150
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                ADD 1 TO SUB-1
                GO TO CHK-010.
           MOVE SUB-1 TO SUB-20.
       CHK-EXIT.
           EXIT.
      *
       FILL-COMMENT SECTION.
       COMM-A.
            MOVE " " TO C-LINE (SUB-1).
       COMM-000.
            MOVE "ORDERQTY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-999.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO C-ORDER (SUB-1).
       COMM-020.
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-000.
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
                GO TO COMM-025.
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
            IF SUB-1 > 150  
                GO TO NEXT-030.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO B-TAX (SUB-1)
                MOVE 0 TO B-DISCOUNTPERITEM (SUB-1).
            IF F-INDEX < 8
                GO TO NEXT-010.
       NEXT-030.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 > 143
             IF SUB-25 > 143
               COMPUTE F-INDEX = 7 - (150 - SUB-25)
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
            IF SUB-1 > 150  
                GO TO NEXT-PAGE-030.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO B-TAX (SUB-1)
                MOVE 0 TO B-DISCOUNTPERITEM (SUB-1).
            IF F-INDEX < 8
                GO TO NEXT-PAGE-010.
       NEXT-PAGE-030.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 > 143
             IF SUB-25 > 143
               COMPUTE F-INDEX = 7 - (150 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX.
            IF SUB-1 > 150
               MOVE 144 TO SUB-1.
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
            IF SUB-1 > 150
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
                GO TO SCROLL-030.
            MOVE B-ORDERQTY (SUB-1) TO F-EDNAMEFIELDQTY.
            PERFORM WRITE-FIELD-QTY.
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

            MOVE "TAX" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE B-TAX (SUB-1) TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
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
       CLEAR-005.
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDERQTY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PERUNIT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE 8 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKCOST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 8 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME.
            MOVE 15 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TAX" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE " "           TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            GO TO CLEAR-002.
       CLEAR-BODY-999.
             EXIT.
      *
       SCROLL-COMMENT SECTION.
       SCCO-000.
            MOVE "ORDERQTY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            MOVE C-ORDER (SUB-1) TO F-NAMEFIELD.
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
            MOVE 9 TO F-CBFIELDLENGTH.
            MOVE C-PRICE (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKCOST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 9 TO F-CBFIELDLENGTH.
            MOVE C-COST (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME.
            MOVE 15 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            MOVE C-DISC (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
       SCCO-999.
            EXIT.
      *
       CLEAR-BOTTOM-FIELDS SECTION.
       CBF-000.
            MOVE "ADDONFREIGHT" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE 7 TO F-CBFIELDLENGTH.
            MOVE "                    " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADDON" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 7 TO F-CBFIELDLENGTH.
            MOVE "                    " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "HANDADDON" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 7 TO F-CBFIELDLENGTH.
            MOVE "                    " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "MISC.ADDON" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE 7 TO F-CBFIELDLENGTH.
            MOVE "                    " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUBTOTAL" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE 10         TO F-CBFIELDLENGTH.
            MOVE SPACES     TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDONAMT" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE 10         TO F-CBFIELDLENGTH.
            MOVE SPACES     TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TAXAMT"   TO F-FIELDNAME.
            MOVE 6          TO F-CBFIELDNAME.
            MOVE 10         TO F-CBFIELDLENGTH.
            MOVE SPACES     TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICETOTAL" TO F-FIELDNAME.
            MOVE 12             TO F-CBFIELDNAME.
            MOVE 10             TO F-CBFIELDLENGTH.
            MOVE SPACES         TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
       CBF-999.
            EXIT.
      *
       CANCEL-INVOICE SECTION.
       CI-000.
             MOVE 2910 TO POS.
             DISPLAY "CANCELLING QUOTE TRANSACTIONS ......." AT POS.
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
             MOVE 3001 TO POS.
             MOVE " " TO WS-MESSAGE.
             DISPLAY WS-MESSAGE AT POS.
       CI-999.
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
                         B-INSTOCK (SUB-1)
                         B-NEWLINE (SUB-1).
             MOVE 0   TO B-ORDERQTY (SUB-1)
                         B-STOCKPRICE (SUB-1)
                         B-STOCKCOST (SUB-1)
                         B-DISCOUNTPERITEM (SUB-1)
                         B-INVOICED (SUB-1)
                         B-NETT (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 151
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
                MOVE "ST-DISCOUNT BUSY ON READ, 'ESC' TO RETRY."
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
               MOVE "STDISCOUNT BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
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
               MOVE "PARAMETER BUSY READ INVQUES, 'ESC' TO RETRY."
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
       READ-FAXPARAM SECTION.
       RDFXPRM-000.
           MOVE 1 TO FAX-PARMKEY.
           START FAX-PARAMETER KEY NOT < FAX-PAKEY.
       RDFXPRM-010.
           READ FAX-PARAMETER
                 INVALID KEY NEXT SENTENCE.
           IF WS-FAX-ST1 = 23 OR 35 OR 49
                MOVE
            "WE HAVE A PROBLEM IN FAX-PARAMETER ON READ, 'ESC' TO RETRY"
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-FAX-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-FAX-ST1
                 GO TO RDFXPRM-010.
           IF WS-FAX-ST1 NOT = 0
                MOVE "FAX RECORD BUSY ON READ, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-FAX-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-FAX-ST1
                 GO TO RDFXPRM-010.
       RDFXPRM-999.
             EXIT.
      *-----------------------------------------------------------*
       Z1-HEADINGS SECTION.
      *-----------------------------------------------------------*
       Z1-50.
            MOVE ALL SPACES TO WS-FST-LINE WS-OTH-LINE-1.
            MOVE "뉜" TO WS-DELIM-F.
            MOVE "�"  TO WS-DELIM-O
            MOVE "�"  TO WS-DELIM-END1
                         WS-DELIM-END2.
            
            MOVE 1             TO SUB-1
            MOVE SUB-1         TO WS-O-LINE
            MOVE "CompLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-F
            WRITE PRINT-REC FROM WS-FST-LINE AFTER 0.
       Z1-51.
            ADD 1              TO SUB-1
            IF SUB-1 > 10
               MOVE 0 TO SUB-1
               GO TO Z1-52.
            MOVE SUB-1         TO WS-O-LINE
            MOVE "CompLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-51.
       Z1-52.
            ADD 1              TO SUB-1
            IF SUB-1 > 11
               MOVE 0 TO SUB-1
               GO TO Z1-53.
            MOVE SUB-1         TO WS-O-LINE
            MOVE "SuppLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-52.
       Z1-53.
            ADD 1              TO SUB-1
            IF SUB-1 > 6
               MOVE 0 TO SUB-1
               GO TO Z1-54.
            MOVE SUB-1         TO WS-O-LINE
            MOVE "QuotLine"    TO WS-O-L
            MOVE WS-OTH-LINE-1 TO WS-DATA-O
            WRITE PRINT-REC FROM WS-OTH-LINE AFTER 1.
            GO TO Z1-53.
       Z1-54.
            ADD 1              TO SUB-1
            IF SUB-1 > 31
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
       OPEN-000.
           OPEN I-O PARAMETER-FILE.
           PERFORM READ-PARAMETER.
           MOVE PA-GST-PERCENT TO WS-GST-PERCENT.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO WSD-DATE.
           IF WS-MM = PA-CURRENT-PER-MM
             IF WS-YY = PA-CURRENT-PER-YY
               GO TO OPEN-010.
       OPEN-005.
           MOVE 1510 TO POS.
           DISPLAY "THE CURRENT MONTH OR YEAR ON THE PARAMETER FILE"
               AT POS.
           MOVE 1610 TO POS.
           DISPLAY " " AT POS.

           MOVE 1710 TO POS.
           DISPLAY "DOES NOT CORRESPOND WITH TODAYS DATE!!!!" AT POS.
           MOVE 1810 TO POS.
           DISPLAY " " AT POS.
           MOVE 1910 TO POS.
           DISPLAY "GO AND CHECK THE SYSTEM DATE, " AT POS.
           MOVE 2010 TO POS.
           DISPLAY " " AT POS.
           MOVE 2110 TO POS.
           DISPLAY "AS IT APPEARS YOU'RE IN THE WRONG MONTH." AT POS.
           MOVE 2210 TO POS.
           DISPLAY " " AT POS.
           MOVE 2310 TO POS.
           DISPLAY "PRESS 'GO' OR 'NEXT' TO END THE PROGRAM" AT POS.
           MOVE 2350 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-IMM-PR.

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
           PERFORM READ-QUOTE-FILE.
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
       OPEN-018.
           OPEN I-O Fax-Parameter.
           IF WS-FAX-ST1 NOT = 0 
              MOVE "FaxParam FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-FAX-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-FAX-ST1
              GO TO OPEN-018.
           Perform Read-FaxParam.
           CLOSE Fax-Parameter.
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
          OPEN I-O TOOLKITS.
          IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "TOOLKIT FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-TOOLKIT-ST1
              GO TO OPEN-020.
       OPEN-021.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlQuote"       TO F-FORMNAME.
           MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-050.
            MOVE "."   TO WS-FIL-M
                          WS-FIL-Y.
            MOVE WS-MM TO WS-QUOTE-MM.
            MOVE WS-YY TO WS-QUOTE-YY.
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
                 TOOLKITS
                 STDISC-MASTER.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAddOn".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAmount".
       Copy "WriteField9Mil".
       Copy "WriteFieldAmount1".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldQty".
       Copy "PrepareFaxSending".
       Copy "OrderPassword".
       Copy "GetSystemY2KDate".
       Copy "GetUserMailName".
       Copy "GetReportY2KDate".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
       Copy "SetupQuoteForPDF".
       Copy "SetupQuote2ForPDF".
       Copy "SetupMergeQuoteForPDF".
       Copy "MoveEmailRecordFromEimage".
       Copy "ZoomBox".
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
       Copy "Error3Message".
       Copy "WriteDailyExcep1".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
