       IDENTIFICATION DIVISION.
       PROGRAM-ID. SlOrChRp.
       AUTHOR.  CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
         Copy "SelectStTrans".
         Copy "SelectSlRegister".
         Copy "SelectSlDaily".
         Copy "SelectSlParameter".
         Copy "SelectDrMaster".
         Copy "SelectStMaster".
         Copy "SelectSlSbRep".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DOCUBASE-PRINT ASSIGN TO WS-DOCUFILE
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStTrans.
           COPY ChlfdRegister.
           COPY ChlfdDaily.
           COPY ChlfdParam.
           COPY ChlfdDebtor.
           COPY ChlfdStock.
           COPY ChlfdSbRep.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(126).
           03  PRINT-RECINV        PIC X(6).
      *
       FD  DOCUBASE-PRINT.
       01  DB-REC.
           03  FILLER           PIC X(126).
           03  DB-RECINV        PIC X(6).
      *
       WORKING-STORAGE SECTION.
       77  WS-DOCUFILE          PIC X(25) VALUE " ".
       77  WS-CHANGE-SOLDBY     PIC X VALUE " ".
       77  WS-SALESMAN          PIC X(15) VALUE " ".
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-BO-QTY            PIC 9(5) VALUE 0.
       77  WS-POORDERNO         PIC X(20) VALUE " ".
       77  WSAN-CODE            PIC XX VALUE " ".
       77  WS-TYPE              PIC X VALUE " ".
       77  WS-PRINT-AMTS        PIC X VALUE " ".
       77  WS-SOLD-BY           PIC XX VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-DOCPRINTED        PIC X VALUE " ".
       77  WS-DELIVERVIA        PIC X(20) VALUE " ".
       77  WS-TERMOFSALE        PIC X(11) VALUE " ".
       77  WS-BINNO             PIC X(6) VALUE " ".
       77  WS-GSTNO             PIC X(13) VALUE " ".
       77  WS-INVOICEDATE       PIC 9(8) VALUE 0.
       77  WS-TODAYS-DATE       PIC 9(8) VALUE 0.
       77  WS-COMMENTLINE       PIC X(30) VALUE " ".
       77  WS-ADDONFREIGHT      PIC 9(4)V99 VALUE 0.
       77  WS-POSTADDON         PIC 9(4)V99 VALUE 0.
       77  WS-HANDADDON         PIC 9(4)V99 VALUE 0.
       77  WS-MISCADDON         PIC 9(4)V99 VALUE 0.
       77  WS-SUBTOTAL          PIC 9(6)V99 VALUE 0.
       77  WS-ADDONAMT          PIC 9(6)V99 VALUE 0.
       77  WS-TAXAMT            PIC 9(6)V99 VALUE 0.
       77  WS-INVOICETOTAL      PIC 9(6)V99 VALUE 0.
       77  WS-TAXABLETOTAL      PIC 9(6)V99 VALUE 0.
       77  WS-NONTAXABLETOTAL   PIC 9(6)V99 VALUE 0.
       77  WS-WORKTOTAL         PIC 9(6)V99 VALUE 0.
       77  WS-WORKTOTAL1        PIC 9(6)V99 VALUE 0.
       77  WS-WORKTOTAL2        PIC 9(7)V99 VALUE 0.
       77  WS-COSTTOTAL         PIC 9(6)V99 VALUE 0.
       77  WS-COSTTOTAL1        PIC 9(6)V99 VALUE 0.
       77  WS-PRICETOTAL        PIC 9(6)V99 VALUE 0.
       77  WS-PRICETOTAL1       PIC 9(6)V99 VALUE 0.
       77  WS-EXPORTTOTAL       PIC 9(6)V99 VALUE 0.
       77  WS-DISCOUNTREG       PIC 9(6)V99 VALUE 0.
       77  WS-DISCOUNTREG1      PIC 9(6)V99 VALUE 0.
       77  WS-DISCOUNT          PIC 9(6)V99 VALUE 0.
       77  WS-DISCOUNT1         PIC 9(6)V99 VALUE 0.
       77  WS-MARGIN            PIC 9(6)V99 VALUE 0.
       77  WS-ITEMS-BELOW-MIN-PERC   PIC X VALUE " ".
       77  WS-PERC              PIC S9(4)V99 VALUE 0.
       77  WS-GST-PERCENT       PIC 99V99 VALUE 0.
       77  WS-PARAM             PIC X(34) VALUE " ".
       77  WS-QUES-MU-GP-PERC   PIC X VALUE " ".
       77  WS-PAGE              PIC 99 VALUE 0.
       77  WS-INVOICEDISCOUNT   PIC 9(2)V99 VALUE 0.
       77  WS-NORM-PRINTER      PIC 9.
       77  WS-REPR-PRINTER      PIC 9.
       77  WS-RUSH-PRINTER      PIC 9.
       77  WS-ORDER-NO          PIC 9(5) VALUE 0.
       01  WS-ORDER-NO-DIS      PIC Z(4)9.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1   PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1     PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-SBREP-STATUS.
           03  WS-SBREP-ST1     PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1     PIC 99.
       01  WS-NAMEANDADDRESS.
           03  WS-NAME          PIC X(40) VALUE " ".
           03  WS-ADD1          PIC X(25) VALUE " ".
           03  WS-ADD2          PIC X(25) VALUE " ".
           03  WS-ADD3          PIC X(25) VALUE " ".
           03  WS-POSTCODE      PIC 9(4).
           03  WS-DELADD1       PIC X(25) VALUE " ".
           03  WS-DELADD2       PIC X(25) VALUE " ".
           03  WS-DELADD3       PIC X(25) VALUE " ".
           03  WS-PHONE         PIC X(13) VALUE " ".
           03  WS-CONTACT       PIC X(20) VALUE " ".
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X VALUE " ".
           03  SP-REST          PIC X(14) VALUE " ".
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 301.
               05  B-STOCKNUMBER.
                   07  B-1ST-CHAR      PIC X.
                   07  B-REST          PIC X(14).
               05  B-INVOICED          PIC 9(6).
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
                   07  B-PULL              PIC X.
                   07  B-NEWLINE           PIC X.
                   07  B-NETT              PIC 9(6)V99.
                   07  B-UNIT              PIC X(4).
                   07  B-STORE             PIC X(5).
                   07  B-MIN-PERC          PIC 9(3)V99.
               05  C-LINE REDEFINES B-REMAINDER.
                   07  C-ORDER            PIC X(5).
                   07  C-SHIP             PIC X(5).
                   07  C-DESC             PIC X(20).
                   07  C-UNIT             PIC X(4).
                   07  C-PRICE            PIC X(9).
                   07  C-COST             PIC X(9).
                   07  C-DISC             PIC X(5).
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  WS-TIME-DISPLAY.
           03  SPLIT-TIME-FIL     PIC X(14).
           03  SPLIT-HR           PIC 99.
           03  SPLIT-HR-FIL       PIC X.
           03  SPLIT-MN           PIC 99.
           03  SPLIT-MN-FIL       PIC X.
           03  SPLIT-SC           PIC 99.
       01  PLINE1.
           03  FILLER           PIC X(17) VALUE "PICKING SLIP NO:".
           03  P-DIG1           PIC X.
           03  P-SLPRINT-RECORD.
              05  P-SLIP           PIC Z(5)9.
              05  FILLER           PIC X VALUE "-".
              05  P-COPYNUMBER     PIC 99.
           03  P-DIG2           PIC X.
           03  FILLER           PIC X(3) VALUE " ".
           03  FILLER           PIC X(9) VALUE "ACCOUNT:".
           03  P-DIG3           PIC X.
           03  P-ACCNO          PIC X(10).
           03  P-DIG4           PIC X.
           03  FILLER           PIC X(8) VALUE "GST NO:".
           03  P-GSTNO          PIC X(39) VALUE " ".
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  P-PAGE           PIC Z9.
           03  FILLER           PIC X(42) VALUE " ".
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
           03  P-TIME           PIC X(100) VALUE " ".
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
           03  FILLER           PIC X(5) VALUE " ".
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
           03  P-BO-DATE        PIC X(8) VALUE " ".
           03  FILLER           PIC X(69) VALUE " ".
       01  P-ADDLINE.
           03  P-DES1           PIC X(12) VALUE " ".
           03  P-SALESMAN.
              05  P-ADD1        PIC Z(5)9.99. 
              05  P-DES2        PIC X(13) VALUE " ".
           03  P-ADD2           PIC Z(5)9.99.
           03  P-DES3           PIC X(12) VALUE " ".
           03  P-ADD3           PIC Z(5)9.99.
           03  P-ADDDESC        PIC X(28) VALUE " ".
           03  P-DES4           PIC X(12) VALUE " ".
           03  P-ADD4           PIC Z(5)9.99-.
           03  FILLER           PIC X(19) VALUE " ".
       01  P-CONTINUED.
           03  FILLER           PIC X(40) VALUE " ".
           03  FILLER           PIC X(22) VALUE "Continued To.....Page".
           03  P-CONTROL-PAGE      PIC 9.
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
       01  HEADER1.
           03  FILLER          PIC X(12) VALUE "TODAYS DATE:".
           03  H1-TODAYS-DATE  PIC X(10).
           03  FILLER          PIC X(5) VALUE " ".
           03  FILLER          PIC X(13) VALUE "CREATED DATE:".
           03  H1-CREATE-DATE  PIC X(10).
           03  FILLER          PIC X(5) VALUE " ".
           03  FILLER          PIC X(38) VALUE
           "** TOOLKIT ASSEMBLY PICKING SLIP **".
           03  H1-P-TYPE1      PIC X.
           03  FILLER          PIC X(8) VALUE "SLIP NO:".
           03  H1-NO           PIC Z(5)9.
           03  FILLER          PIC X VALUE "-".
           03  H1-COPY         PIC 99.
           03  FILLER          PIC X VALUE " ".
           03  H1-P-TYPE2      PIC X.
           03  FILLER          PIC X(5) VALUE "Page:".
           03  H1-PAGE         PIC Z9.
       01  HEADER2.
           03  FILLER          PIC X(10) VALUE "KIT NAME:".
           03  H2-KIT          PIC X(20) VALUE " ".
           03  FILLER          PIC X(13) VALUE "DESCRIPTION:".
           03  H2-DESC1        PIC X(20) VALUE " ".
           03  H2-DESC2        PIC X(30) VALUE " ".
           03  FILLER          PIC X(10) VALUE "KIT PRICE:".
           03  H2-PRICE        PIC Z(5)9.99.
       01  HEADER3.
           03  FILLER          PIC X(10) VALUE "ORDER QTY:".
           03  H3-QTY          PIC Z(2)9.
           03  FILLER          PIC X(4) VALUE " ".
           03  FILLER          PIC X(10) VALUE "  MFG QTY:".
           03  H3-MFGQTY       PIC Z(2)9.
           03  FILLER          PIC X(4) VALUE " ".
           03  FILLER          PIC X(17) VALUE "PREVIOUS MFG QTY:".
           03  H3-SHPDQTY      PIC Z(2)9.
           03  FILLER          PIC X(4) VALUE " ".
           03  FILLER          PIC X(9) VALUE "COMMENT:".
           03  H3-COMMENT      PIC X(65) VALUE " ".
       01  HEADER4.
           03  FILLER          PIC X(60) VALUE
           "STOCKNUMBER     DESCRIPTION".
           03  FILLER          PIC X(60) VALUE
           "PRICE      COST        ORDER  MFG NOW  B-ORDER   USED PREV".
       01  DETAIL-LINE.
           03  D-STOCK         PIC X(16) VALUE " ".
           03  D-DESC1         PIC X(20) VALUE " ".
           03  D-DESC2         PIC X(21) VALUE " ".
           03  D-PRICE         PIC Z(4)9.99.
           03  FILLER          PIC X(2) VALUE " ".
           03  D-COST          PIC Z(4)9.99.
           03  FILLER          PIC X(8) VALUE " ".
           03  D-ORDER         PIC Z(4)9.
           03  FILLER          PIC X(4) VALUE " ".
           03  D-MFG           PIC Z(4)9.
           03  D-PULL          PIC X(1) VALUE " ".
           03  FILLER          PIC X(3) VALUE " ".
           03  D-BO            PIC Z(4)9.
           03  FILLER          PIC X(7) VALUE " ".
           03  D-SHPD          PIC Z(4)9.
           03  FILLER          PIC X(9) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER          PIC X(19) VALUE "TOTAL ITEM PRICE: R".
           03  TOT-PRICE       PIC Z(5)9.99.
           03  FILLER          PIC X(9) VALUE " ".
           03  FILLER          PIC X(19) VALUE "TOTAL ITEM COSTS: R".
           03  TOT-COST        PIC Z(5)9.99.
       01  PERC-LINE.
           03  FILLER          PIC X(15) VALUE "PERCENTAGE MFG:".
           03  PERC-AMT        PIC Z(2)9.
       01  P-MESSAGE-LINE.
           03  P-MES-TYPE      PIC X(2) VALUE " ".
           03  P-MESSAGE       PIC X(130) VALUE " ".

       Copy "WsDateInfo".
       Copy "FormsInfo".
      *
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 315 TO POS.
           DISPLAY "** PRINTING OF AUTO-ALLOCATED ORDERS **" AT POS.
           MOVE 415 TO POS.
           DISPLAY "***************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1210 TO POS.
           DISPLAY "B=Print BILLS OF MATERIAL Changed." AT POS
           MOVE 1310 TO POS.
           DISPLAY "C=Print Allocated Orders ready to print," AT POS.
           MOVE 1410 TO POS.
           DISPLAY "D=Print Allocated Orders Already in store.:[ ]"
            AT POS.
           MOVE 1454 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TYPE.

           IF W-ESCAPE-KEY = 4
                GO TO CONTROL-005.
           IF WS-TYPE NOT = "B" AND NOT = "C" AND NOT = "D"
                DISPLAY " " AT 3079 WITH BELL
                GO TO CONTROL-010.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO CONTROL-015
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO CONTROL-010.
       CONTROL-015.
           MOVE 1510 TO POS.
           DISPLAY "Change SOLDBY To XX, Y=Yes, N=No.        : [ ]"
              AT POS.
           ADD 44 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CHANGE-SOLDBY.
           
           IF W-ESCAPE-KEY = 4
                GO TO CONTROL-010.
           IF WS-CHANGE-SOLDBY NOT = "Y" AND NOT = "N"
                DISPLAY " " AT 3079 WITH BELL
                GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO CONTROL-020
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO CONTROL-015.
       CONTROL-020.
           MOVE 2810 TO POS.
           DISPLAY "Run in progress, please be patient......" AT POS.
           PERFORM OPEN-FILES.
           IF WS-TYPE NOT = "B"
               PERFORM GET-DATA
           ELSE
               PERFORM GET-BM-DATA.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-005.
           MOVE WS-TYPE TO INCR-PRINTED.
           START INCR-REGISTER KEY NOT < INCR-PRINTED
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE
            "REGISTER BUSY ON START GET-005, IN 2 SEC GOING TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-INCR-ST1
               MOVE 0 TO INCR-INVOICE
               GO TO GET-020.
       GET-010.
           PERFORM READ-REGISTER.
       GET-020.
           IF INCR-INVOICE = 0
               GO TO GET-999
           ELSE
               PERFORM READ-STOCK-TRANSACTIONS.
           PERFORM CALCULATE-TOTALS.
           PERFORM CALCULATE-ORDER-TOTAL.
           PERFORM PRINT-INVOICE.
           PERFORM WRITE-DOCUBASE-RECORD
           ADD 1 TO WS-ORDER-NO.
           MOVE WS-ORDER-NO TO WS-ORDER-NO-DIS.
           MOVE 2510 TO POS.
           DISPLAY "Number of Orders processed so far :" AT POS.
           MOVE 2548 TO POS.
           DISPLAY WS-ORDER-NO-DIS AT POS.
           PERFORM CLEAR-FIELDS.
           GO TO GET-010.
       GET-999.
           EXIT.
      *
       GET-BM-DATA SECTION.
       GET-BM-005.
           MOVE WS-TYPE TO INCR-PRINTED.
           START INCR-REGISTER KEY NOT < INCR-PRINTED
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE
            "REGISTER BUSY ON START GET-BM005, IN 2 SEC GOING TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO INCR-INVOICE
               GO TO GET-BM-020.
       GET-BM-010.
           PERFORM READ-BM-REGISTER.
       GET-BM-020.
           IF INCR-INVOICE = 0
               GO TO GET-BM-999
           ELSE
               PERFORM READ-STOCK-TRANSACTIONS.
           PERFORM PRINT-TOOLKIT.
           ADD 1 TO WS-ORDER-NO.
           MOVE WS-ORDER-NO TO WS-ORDER-NO-DIS.
           MOVE 2510 TO POS.
           DISPLAY "Number of B/M'S processed so far :" AT POS.
           MOVE 2548 TO POS.
           DISPLAY WS-ORDER-NO-DIS AT POS.
           PERFORM CLEAR-FIELDS.
           GO TO GET-BM-010.
       GET-BM-999.
           EXIT.
      *
       PRINT-INVOICE SECTION.
       PR-000.
           MOVE WS-INVOICE TO P-SLIP.
       PR-005.
           MOVE " " TO WS-ITEMS-BELOW-MIN-PERC
           MOVE 1   TO WS-PAGE SUB-1 SUB-2.
           MOVE " " TO PRINT-REC.
       PR-010.
           MOVE WS-PRINT-COMP TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE " " TO PRINT-REC.
           IF WS-PAGE > 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE WS-PAGE TO P-CONTROL-PAGE
               WRITE PRINT-REC FROM P-CONTINUED AFTER 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC AFTER PAGE.
       PR-012.
           WRITE PRINT-REC FROM COMPANY-LINE.
           IF WS-TYPE = "C"
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE Ws-Print-Bold   TO P-AL-DIG1
               MOVE "** AUTO-ALLOCATED ORDER PRINT **" TO P-AL-COMMENT
               MOVE Ws-Print-UnBold TO P-AL-DIG2
               WRITE PRINT-REC FROM P-ALLOCATE-LINE AFTER 1
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE " " TO PRINT-REC
               GO TO PR-015.
           IF WS-TYPE = "D"
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE Ws-Print-Bold   TO P-AL-DIG1
               MOVE "** AUTO-ALLOCATED PRINT - 2ND COPY **"
                   TO P-AL-COMMENT
               MOVE Ws-Print-UnBold TO P-AL-DIG2
               WRITE PRINT-REC FROM P-ALLOCATE-LINE AFTER 1
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE " " TO PRINT-REC.
       PR-015.
           MOVE Ws-Print-Bold     TO P-DIG1 P-DIG3
           MOVE Ws-Print-UnBold   TO P-DIG2 P-DIG4.
           MOVE WS-ACCOUNT-NUMBER TO P-ACCNO
           MOVE WS-GSTNO          TO P-GSTNO
           MOVE WS-PAGE           TO P-PAGE
           MOVE INCR-COPY-NUMBER  TO P-COPYNUMBER.
           WRITE PRINT-REC FROM PLINE1 AFTER 1.
           
           MOVE WS-PRINT-COMP TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           
           MOVE WS-NAME     TO P-ADD
           MOVE "PHONE NO:" TO P-NAME
           MOVE WS-PHONE    TO P-CONTACT
           WRITE PRINT-REC FROM PLINE2 AFTER 1.
           MOVE " "         TO PRINT-REC PLINE2
           MOVE WS-ADD1     TO P-ADD
           MOVE WS-DELADD1  TO P-DEL
           MOVE "CONTACT :" TO P-NAME
           MOVE WS-CONTACT  TO P-CONTACT
           WRITE PRINT-REC FROM PLINE2 AFTER 1
           MOVE " "        TO PRINT-REC PLINE2.
           MOVE WS-ADD2    TO P-ADD
           MOVE WS-DELADD2  TO P-DEL
           WRITE PRINT-REC FROM PLINE2 AFTER 1
           MOVE " "        TO PRINT-REC PLINE2.
           MOVE WS-ADD3    TO P-ADD
           MOVE WS-DELADD3 TO P-DEL
           WRITE PRINT-REC FROM PLINE2 AFTER 1
           MOVE " "         TO PRINT-REC PLINE2.

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
           MOVE WSAN-CODE      TO P-SOLD
           MOVE WS-DELIVERVIA  TO P-VIA
           MOVE WS-BINNO       TO P-BIN
           MOVE WS-SOLD-BY     TO P-SOLDBY.
           
           MOVE WS-INVOICEDATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE   TO P-DATE
           
           MOVE WS-INVOICE     TO P-INV
           
           MOVE WS-DATE        TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE   TO P-TODAY
           
           WRITE PRINT-REC FROM PLINE4 AFTER 1.
           MOVE " "            TO PRINT-REC PLINE4
           WRITE PRINT-REC AFTER 1
           WRITE PRINT-REC FROM PDET-1 AFTER 1
           MOVE " "            TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
       PR-020.
           IF SUB-1 < 299
             IF SUB-1 = SUB-20
               SUBTRACT 1 FROM SUB-2
               GO TO PR-030.
           IF SUB-2 > 40
               MOVE 1 TO SUB-2
               ADD 1 TO WS-PAGE
               GO TO PR-010.
           IF SUB-1 > 300
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
           
      * NEW SECTION TO FLAG ITEMS THAT ARE BELOW MIN MU PERC
      * ST-MIN-PERC ADDED TO CHLFDSTOCK 17/12/2001
      * ONLY PRINTS FLAG IF SHIPPED-QTY < ORDER-QTY
      
           IF B-NEWLINE (SUB-1) = "Y" OR = "L" OR = "R"
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
           IF B-NEWLINE (SUB-1) = "R"
               GO TO PR-025.
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
           MOVE B-INVOICED (SUB-1)          TO P-INVOICED.
       PR-025.
           MOVE SUB-1 TO P-NO
           WRITE PRINT-REC FROM PDET AFTER 1
           MOVE " " TO PDET PRINT-REC
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 301
              GO TO PR-020.
       PR-030.
           WRITE PRINT-REC AFTER 2.
       PR-035.
           MOVE " " TO P-COMMENTLINE
           MOVE WS-COMMENTLINE   TO P-BO-MESSAGE
           WRITE PRINT-REC FROM P-COMMENTLINE AFTER 1
           MOVE " " TO P-COMMENTLINE
           WRITE PRINT-REC FROM P-COMMENTLINE AFTER 1.

           MOVE " " TO P-ADDLINE PRINT-REC
           MOVE "   FREIGHT :" TO P-DES1
           MOVE "   LABOUR  :" TO P-DES2
           MOVE "   VAT AMT :" TO P-DES3
           MOVE WS-ADDONFREIGHT  TO P-ADD1
           MOVE WS-HANDADDON     TO P-ADD2
           MOVE WS-TAXAMT        TO P-ADD3
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " " TO P-ADDLINE PRINT-REC.

           MOVE " " TO P-ADDLINE PRINT-REC
           MOVE "   POSTAGE :" TO P-DES1
           MOVE "   MISC.   :" TO P-DES2
           MOVE "   SUBTOTAL:" TO P-DES3
           MOVE "AMT TO INV.:" TO P-DES4
           MOVE WS-POSTADDON     TO P-ADD1
           MOVE WS-MISCADDON     TO P-ADD2
           MOVE WS-SUBTOTAL      TO P-ADD3
           MOVE WS-INVOICETOTAL  TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1.
           MOVE " " TO P-ADDLINE PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE " " TO P-ADDLINE PRINT-REC.

      *****************************************************************
      *  THE FOLLOWING SECTION WAS TAKEN OUT SO THAT CUSTOMERS WOULD  *
      *  NOT SEE PROFIT & COSTS WHEN COLLECTING GOODS.   4/8/95       *
      *****************************************************************
           GO TO PR-036.

           MOVE "COMPLETE ORDER AMOUNTS:" TO P-ADDDESC
           MOVE "DISCOUNT: R"   TO P-DES4
           MOVE WS-DISCOUNTREG1 TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " " TO P-ADDLINE PRINT-REC.

           MOVE "COST AMT: R" TO P-DES4
           MOVE WS-COSTTOTAL1 TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " " TO P-ADDLINE PRINT-REC.

           MOVE "MARGIN  : R" TO P-DES4
           COMPUTE WS-MARGIN = WS-PRICETOTAL1 - WS-COSTTOTAL1
           MOVE WS-MARGIN     TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " " TO P-ADDLINE PRINT-REC.

           MOVE "PROFIT %:  " TO P-DES4
           COMPUTE WS-PERC = (WS-MARGIN / WS-COSTTOTAL1) * 100
           MOVE WS-PERC TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " " TO P-ADDLINE PRINT-REC.

           MOVE "ORDER TOTAL"  TO P-DES4
           MOVE WS-PRICETOTAL1 TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1.
       PR-036.
           PERFORM READ-DEBTORS.
           IF WS-SALESMAN > " "
              MOVE "  SALESMAN:" TO P-DES1
              MOVE WS-SALESMAN   TO P-SALESMAN
           ELSE
              MOVE "  SALESMAN:" TO P-DES1
              MOVE "OPEN A/C"    TO P-SALESMAN.
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1.

              
      *THIS SECTION IS FOR THE ORDER AS A WHOLE.  IF ORDER 
      *BELOW MIN M/U OR G/P THEN THIS COMMENT PRINTS.
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
              WRITE PRINT-REC AFTER 1.
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
              WRITE PRINT-REC AFTER 1.
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
           
           MOVE " "           TO P-ADDLINE DB-REC.
           MOVE " " TO P-DEL PRINT-REC
           MOVE WS-PRINT-NORMAL TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
           
      *     WRITE PRINT-REC BEFORE PAGE.
       PR-999.
           EXIT.
      *
       PRINT-TOOLKIT SECTION.
       BMPR-004.
           MOVE "Y" TO WS-PRINT-AMTS.
       BMPR-005.
           MOVE 1   TO WS-PAGE SUB-1 SUB-2.
           MOVE " " TO PRINT-REC.
      *     OPEN OUTPUT PRINT-FILE.
       BMPR-010.
           MOVE WS-PRINT-COMP TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE " " TO PRINT-REC.
           IF WS-PAGE > 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE WS-PAGE TO P-CONTROL-PAGE
               WRITE PRINT-REC FROM P-CONTINUED
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC BEFORE PAGE.
       BMPR-012.
           MOVE INCR-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO H1-CREATE-DATE.
           MOVE WS-DATE           TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE      TO H1-TODAYS-DATE.
           MOVE Ws-Print-Bold     TO H1-P-TYPE1
           MOVE Ws-Print-UnBold   TO H1-P-TYPE2
           MOVE INCR-INVOICE      TO H1-NO
           MOVE WS-PAGE           TO H1-PAGE
           MOVE INCR-COPY-NUMBER  TO H1-COPY.
           WRITE PRINT-REC      FROM COMPANY-LINE
           MOVE " "            TO PRINT-REC
           WRITE PRINT-REC      FROM HEADER1
           MOVE " "            TO PRINT-REC
           WRITE PRINT-REC
           MOVE INCR-KITNAME    TO H2-KIT
           MOVE INCR-KITDESC1   TO H2-DESC1
           MOVE INCR-KITDESC2   TO H2-DESC2.
           IF WS-PRINT-AMTS = "Y"
             MOVE INCR-KITPRICE TO H2-PRICE.
           WRITE PRINT-REC FROM HEADER2
           MOVE " " TO PRINT-REC
           MOVE INCR-KITQTY     TO H3-QTY
           MOVE 0               TO H3-MFGQTY
           MOVE INCR-KITSHPDQTY TO H3-SHPDQTY
           MOVE INCR-KITCOMMENT TO H3-COMMENT
           WRITE PRINT-REC FROM HEADER3
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEADER4
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE 8 TO SUB-2.
       BMPR-020.
           IF SUB-1 < 149
            IF SUB-1 = SUB-25
               SUBTRACT 1 FROM SUB-2
               GO TO BMPR-030.
           IF SUB-2 > 58
               MOVE 1 TO SUB-2
               ADD 1 TO WS-PAGE
               GO TO BMPR-010.
           IF SUB-1 > 125
               GO TO BMPR-030.
           IF B-STOCKNUMBER (SUB-1) = " "
             AND B-ORDERQTY (SUB-1) = 0
             GO TO BMPR-030.
           MOVE B-STOCKNUMBER (SUB-1)       TO D-STOCK
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO D-DESC1
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO D-DESC2
           IF WS-PRINT-AMTS = "Y"
               MOVE B-STOCKPRICE (SUB-1)    TO D-PRICE
               MOVE B-STOCKCOST (SUB-1)     TO D-COST.
           MOVE B-PULL (SUB-1)              TO D-PULL
           MOVE B-ORDERQTY (SUB-1)          TO D-ORDER
           MOVE B-SHIPQTY (SUB-1)           TO D-MFG
           MOVE B-SHIPPEDQTY (SUB-1)        TO D-SHPD
           COMPUTE WS-BO-QTY = B-ORDERQTY (SUB-1) -
               (B-SHIPQTY (SUB-1) + B-SHIPPEDQTY (SUB-1))
           MOVE WS-BO-QTY                   TO D-BO.
       BMPR-025.
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO DETAIL-LINE PRINT-REC.
           ADD 1 TO SUB-1
                    SUB-2.
           IF SUB-1 < 150
              GO TO BMPR-020.
       BMPR-030.
           WRITE PRINT-REC.
      *     IF WS-PRINT-AMTS = "Y"
      *         MOVE WS-PRICES TO TOT-PRICE
      *         MOVE WS-COSTS  TO TOT-COST
      *         WRITE PRINT-REC FROM TOTAL-LINE
      *         MOVE " " TO PRINT-REC
      *         WRITE PRINT-REC.

      *     COMPUTE WS-PERC = ((WS-MFGQTY + WS-SHPDQTY) / WS-QTY) * 100.
      *     MOVE WS-PERC TO PERC-AMT.
      *     WRITE PRINT-REC FROM PERC-LINE AFTER 1.
      *     MOVE " " TO PRINT-REC.

           MOVE WS-PRINT-BOLD TO P-MES-TYPE.
              MOVE
           "** THIS IS AN AUTO ALLOCATION, ITEMS ONLY RESERVED **"
              TO P-MESSAGE
              GO TO BMPR-036.
       BMPR-036.
           WRITE PRINT-REC FROM P-MESSAGE-LINE
           MOVE " " TO PRINT-REC P-MESSAGE-LINE
           MOVE WS-PRINT-UNBOLD TO P-MES-TYPE
           WRITE PRINT-REC FROM P-MESSAGE-LINE.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
           
      *     CLOSE PRINT-FILE
           MOVE 2758 TO POS
           DISPLAY WS-MESSAGE AT POS.
       BMPR-999.
           EXIT.
      *
       WRITE-DOCUBASE-RECORD SECTION.
       WDBS-001.
           MOVE 2710 TO POS
           DISPLAY "WRITING DOCUBASE RECORD....." AT POS.
           MOVE " " TO WS-DOCUFILE ALPHA-RATE DATA-RATE.
           MOVE "/ctools/ps"     TO ALPHA-RATE
           MOVE WS-CO-NUMBER     TO WS-COMPANY-DIGITS
           MOVE WS-CO-DIG1       TO AL-RATE (11)
           MOVE WS-CO-DIG2       TO AL-RATE (12)
           MOVE "/"              TO AL-RATE (13).
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
           OPEN OUTPUT DOCUBASE-PRINT.
           MOVE WS-INVOICE TO P-SLIP.
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
           IF WS-TYPE = "C"
               MOVE " " TO DB-REC
               WRITE DB-REC AFTER 1
               MOVE Ws-Print-Bold   TO P-AL-DIG1
               MOVE "** AUTO-ALLOCATED ORDER PRINT **" TO P-AL-COMMENT
               MOVE Ws-Print-UnBold TO P-AL-DIG2
               WRITE DB-REC FROM P-ALLOCATE-LINE AFTER 1
               MOVE WS-PRINT-COMP TO DB-REC
               WRITE DB-REC AFTER 1
               MOVE " " TO DB-REC
               GO TO DB-015.
           IF WS-TYPE = "D"
               MOVE " " TO DB-REC
               WRITE DB-REC AFTER 1
               MOVE Ws-Print-Bold   TO P-AL-DIG1
               MOVE "** AUTO-ALLOCATED PRINT - 2ND COPY **"
                   TO P-AL-COMMENT
               MOVE Ws-Print-UnBold TO P-AL-DIG2
               WRITE DB-REC FROM P-ALLOCATE-LINE AFTER 1
               MOVE WS-PRINT-COMP TO DB-REC
               WRITE DB-REC AFTER 1
               MOVE " " TO DB-REC.
       DB-015.
           MOVE WS-PRINT-BOLD     TO P-DIG1 P-DIG3
           MOVE WS-PRINT-UNBOLD   TO P-DIG2 P-DIG4.
           MOVE WS-ACCOUNT-NUMBER TO P-ACCNO
           MOVE WS-GSTNO          TO P-GSTNO
           MOVE WS-PAGE           TO P-PAGE
           MOVE INCR-COPY-NUMBER  TO P-COPYNUMBER
           WRITE DB-REC FROM PLINE1 AFTER 1.
           MOVE " " TO DB-REC
           MOVE WS-PRINT-COMP TO DB-REC
           WRITE DB-REC AFTER 1.
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
           MOVE WS-DELIVERVIA  TO P-VIA.
           MOVE WS-BINNO       TO P-BIN
           MOVE WS-SOLD-BY     TO P-SOLDBY.
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
           IF SUB-1 < 299
             IF SUB-1 = SUB-20
               SUBTRACT 1 FROM SUB-2
               GO TO DB-030.
           IF SUB-2 > 40
               MOVE 1 TO SUB-2
               ADD 1 TO WS-PAGE
               GO TO DB-010.
           IF SUB-1 > 300
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
           IF B-NEWLINE (SUB-1) = "R"
               GO TO DB-025.
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
           MOVE B-INVOICED (SUB-1)          TO P-INVOICED.
       DB-025.
           MOVE SUB-1 TO P-NO.
           WRITE DB-REC FROM PDET.
           MOVE " " TO PDET DB-REC.
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 301
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
           MOVE "  SALESMAN:" TO P-DES1
           MOVE WS-SALESMAN   TO P-SALESMAN.
           WRITE DB-REC FROM P-ADDLINE.
           MOVE " "           TO P-ADDLINE DB-REC.
      *THIS SECTION IS FOR THE ORDER AS A WHOLE.  IF ORDER 
      *BELOW MIN M/U OR G/P THEN THIS COMMENT PRINTS.
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
              WRITE DB-REC AFTER 1.
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
              WRITE DB-REC AFTER 1.
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
           
           MOVE " " TO P-DEL DB-REC.
           MOVE WS-PRINT-NORMAL TO DB-REC
           WRITE DB-REC
           MOVE " " TO DB-REC.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-DB-REPORT-INFO.
           
           CLOSE DOCUBASE-PRINT.
           PERFORM ERROR1-020.
       DB-999.
           EXIT.
      *
       READ-REGISTER SECTION.
       RIR-005.
           READ INCR-REGISTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
               MOVE 0 TO INCR-INVOICE
               GO TO RIR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE
             "NEXT REGISTER BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-005.
           IF INCR-TRANS NOT = 4
               GO TO RIR-005.
           IF INCR-PRINTED NOT = WS-TYPE
               MOVE 0 TO INCR-INVOICE
               GO TO RIR-999.
           PERFORM ERROR-020.
       RIR-010.
           MOVE INCR-INVOICE     TO WS-INVOICE.
           MOVE INCR-ACCOUNT     TO WS-ACCOUNT-NUMBER.
           MOVE INCR-GSTNO       TO WS-GSTNO.
           MOVE INCR-DATE        TO WS-INVOICEDATE.
           MOVE INCR-SALES       TO WSAN-CODE.
           MOVE INCR-INVCRED-AMT TO WS-INVOICETOTAL.
           MOVE INCR-TAX         TO WS-TAXAMT.
           MOVE INCR-ADDONS      TO WS-ADDONAMT.
           IF WS-CHANGE-SOLDBY = "Y"
              MOVE "XX"          TO INCR-SB-TYPE.
           MOVE INCR-SB-TYPE     TO WS-SOLD-BY.
           COMPUTE WS-SUBTOTAL =
                WS-INVOICETOTAL - (WS-TAXAMT + WS-ADDONAMT).
           MOVE INCR-NAME        TO WS-NAME.
           MOVE INCR-ADD1        TO WS-ADD1.
           MOVE INCR-ADD2        TO WS-ADD2.
           MOVE INCR-ADD3        TO WS-ADD3.
           MOVE INCR-CODE        TO WS-POSTCODE.
           MOVE INCR-DEL1        TO WS-DELADD1.
           MOVE INCR-DEL2        TO WS-DELADD2.
           MOVE INCR-DEL3        TO WS-DELADD3.
           MOVE INCR-TERMS       TO WS-TERMOFSALE.
           MOVE INCR-PORDER      TO WS-POORDERNO.
           MOVE INCR-CONTACT     TO WS-CONTACT.
           MOVE INCR-PHONE       TO WS-PHONE.
           MOVE INCR-DELIVERY    TO WS-DELIVERVIA.
           MOVE INCR-BIN         TO WS-BINNO.
           MOVE INCR-COMMENT     TO WS-COMMENTLINE.
           MOVE INCR-ADDPOST     TO WS-POSTADDON.
           MOVE INCR-ADDFREIGHT  TO WS-ADDONFREIGHT.
           MOVE INCR-ADDLABOUR   TO WS-HANDADDON.
           MOVE INCR-ADDMISC     TO WS-MISCADDON.

           IF WS-DOCPRINTED NOT = "Y"
               MOVE "Y" TO WS-DOCPRINTED.
           MOVE "P" TO INCR-PRINTED.
           ADD 1    TO INCR-COPY-NUMBER.
           MOVE " " TO INCR-PULLBY
           MOVE 0   TO INCR-PULL-DATE.
        RIR-900.
           REWRITE INCR-REC
                  INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE
            "REGISTER BUSY ON RE-WRITE RIR900, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-900.
       RIR-999.
           EXIT.
      *
       READ-BM-REGISTER SECTION.
       RBM-005.
           READ INCR-REGISTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
               MOVE 0 TO INCR-INVOICE
               GO TO RBM-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE 
          "NEXT BM-REGISTER BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-INCR-ST1
               GO TO RBM-005.

           IF INCR-PRINTED NOT = WS-TYPE
               MOVE 0 TO INCR-INVOICE
               GO TO RBM-999.

           IF INCR-TRANS NOT = 7
               MOVE 
          "NEXT BM-REG BUSY INCR-TRANS NOT=7, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               GO TO RBM-005.

           PERFORM ERROR-020.
           MOVE INCR-INVOICE     TO WS-INVOICE.

           IF WS-DOCPRINTED NOT = "Y"
               MOVE "Y" TO WS-DOCPRINTED.
           MOVE "N"     TO INCR-PRINTED.
           ADD 1        TO INCR-COPY-NUMBER.
           MOVE " "     TO INCR-PULLBY
           MOVE 0       TO INCR-PULL-DATE
                           INCR-PULL-TIME.
       RBM-900.
           REWRITE INCR-REC
                  INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE
            "REGISTER BUSY ON RE-WRITE RBM900, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-INCR-ST1
               GO TO RBM-900.
       RBM-999.
           EXIT.
      *
       READ-DEBTORS SECTION.
       RD-000.
           MOVE INCR-ACCOUNT TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY
               INVALID KEY NEXT SENTENCE.
       RD-005.
           READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE " " TO WS-SALESMAN
               GO TO RD-999.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE
                  "DEBTOR BUSY ON READ, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-000.
       RD-010.
           PERFORM READ-SBREP.
       RD-999.
           EXIT.
      *
       READ-SBREP SECTION.
       RSB-005.
           IF DR-SALESMAN = " " OR = "0"
              MOVE " " TO WS-SALESMAN
              GO TO RSB-999.
       RSB-020.
           MOVE DR-SALESMAN TO SBREP-REP.
           START SBREP-MASTER KEY NOT < SBREP-KEY
             INVALID KEY NEXT SENTENCE.
       RSB-030.
           READ SBREP-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-SBREP-ST1 = 23 OR 35 OR 49
              MOVE " " TO WS-SALESMAN
              GO TO RSB-999.
           MOVE SBREP-REPNAME TO WS-SALESMAN.
       RSB-999.
            EXIT.
      *
       READ-STOCK-TRANSACTIONS SECTION.
       RSTT-000.
           MOVE 1          TO SUB-1
           MOVE WS-INVOICE TO STTR-REFERENCE1
           MOVE INCR-TRANS TO STTR-TYPE
           MOVE 0          TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
       RSTT-010.
           READ STOCK-TRANS-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              MOVE 0 TO STTR-TYPE
              GO TO RSTT-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE
             "NEXT ST-TRANS BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RSTT-010.
           IF STTR-REFERENCE1 NOT = WS-INVOICE
              GO TO RSTT-999.
           IF STTR-TYPE NOT = INCR-TRANS
              MOVE
             "STTR-TYPE NOT=INCR-TRANS, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-STTRANS-ST1
              GO TO RSTT-010.
           MOVE STTR-STOCK-NUMBER TO B-STOCKNUMBER (SUB-1)
                                     SPLIT-STOCK.
           MOVE STTR-INV-NO       TO B-INVOICED (SUB-1).
           MOVE STTR-COMPLETE     TO B-NEWLINE (SUB-1).
           
           IF STTR-COMPLETE NOT = "B" AND NOT = "C" AND NOT = "D" 
                MOVE " "          TO B-PULL (SUB-1)
           ELSE
                MOVE "*"          TO B-PULL (SUB-1).
           IF SP-1STCHAR = "*"
               GO TO RSTT-020.

           MOVE STTR-DESC1        TO B-STOCKDESCRIPTION (SUB-1)
           MOVE STTR-DESC2        TO B-STOCKDESCRIPTION2 (SUB-1)
           MOVE STTR-ORDERQTY     TO B-ORDERQTY (SUB-1)
           MOVE STTR-SHIPQTY      TO B-SHIPQTY (SUB-1)
           MOVE STTR-SHIPPEDQTY   TO B-SHIPPEDQTY (SUB-1)
           MOVE STTR-PRICE        TO B-STOCKPRICE (SUB-1)
           MOVE STTR-COST-VALUE   TO B-STOCKCOST (SUB-1)
           MOVE STTR-ITEMDISC     TO B-DISCOUNTPERITEM (SUB-1)
           MOVE STTR-TAX          TO B-TAX (SUB-1)
           MOVE STTR-UNIT         TO B-UNIT (SUB-1)
           MOVE STTR-SALES-VALUE  TO B-NETT (SUB-1)
           GO TO RSTT-025.
       RSTT-020.
           MOVE COM-ORDERQTY      TO C-ORDER (SUB-1)
           MOVE COM-SHIPQTY       TO C-SHIP (SUB-1)
           MOVE COM-DESC          TO C-DESC (SUB-1)
           MOVE COM-UNIT          TO C-UNIT (SUB-1)
           MOVE COM-PRICE         TO C-PRICE (SUB-1)
           MOVE COM-COST          TO C-COST (SUB-1)
           MOVE COM-DISC          TO C-DISC (SUB-1).
       RSTT-025.
           IF STTR-COMPLETE = "B" OR = "C" OR = "D"
              GO TO RSTT-030
           ELSE
              GO TO RSTT-050.
       RSTT-030.
           MOVE "N" TO STTR-COMPLETE
                       STTR-AC-COMPLETE
                       STTR-ST-COMPLETE.
           REWRITE STOCK-TRANS-REC
                  INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE
             "ST-TRANS BUSY READ-NEXT RSTT030, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RSTT-030.
       RSTT-050.
           IF SP-1STCHAR NOT = "*" AND NOT = "/"
              PERFORM READ-STOCK.
           ADD 1 TO SUB-1.
           MOVE SUB-1 TO SUB-20.
           IF SUB-1 > 300
              GO TO RSTT-999.
           GO TO RSTT-010.
       RSTT-999.
           EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
           MOVE B-STOCKNUMBER (SUB-1) TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-005.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE " " TO B-STORE (SUB-1)
               GO TO R-ST-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-005.
           MOVE ST-BINLOCATION TO B-STORE (SUB-1)
           MOVE ST-MIN-PERC    TO B-MIN-PERC (SUB-1)
           PERFORM ERROR-020.
       R-ST-999.
           EXIT.
      *
       CALCULATE-TOTALS SECTION.
       CT-000.
             MOVE 1 TO SUB-1.
             MOVE 0 TO WS-TAXABLETOTAL WS-NONTAXABLETOTAL
                  WS-WORKTOTAL WS-DISCOUNT WS-COSTTOTAL 
                  WS-PRICETOTAL WS-EXPORTTOTAL WS-DISCOUNTREG.
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
             IF B-TAX (SUB-1) = "Y"
                 ADD WS-WORKTOTAL TO WS-TAXABLETOTAL
             ELSE
                 ADD WS-WORKTOTAL TO WS-NONTAXABLETOTAL.
             IF WS-GSTNO = "EXPORT"
                 ADD WS-WORKTOTAL TO WS-EXPORTTOTAL.
             ADD WS-WORKTOTAL TO WS-PRICETOTAL.
             MOVE WS-WORKTOTAL TO B-NETT (SUB-1).
       CT-015.
             ADD 1 TO SUB-1.
             IF SUB-1 > 300
                 GO TO CT-020.
             IF SUB-1 < 301
                 GO TO CT-010.
       CT-020.
             COMPUTE WS-SUBTOTAL = WS-TAXABLETOTAL + 
                                   WS-NONTAXABLETOTAL + 
                                   WS-EXPORTTOTAL.
             COMPUTE WS-ADDONAMT = WS-ADDONFREIGHT + WS-POSTADDON + 
                                   WS-HANDADDON + WS-MISCADDON.
             COMPUTE WS-TAXAMT ROUNDED = (WS-TAXABLETOTAL + WS-ADDONAMT)
                                 * WS-GST-PERCENT / 100.
             COMPUTE WS-INVOICETOTAL = WS-SUBTOTAL +
                                       WS-TAXAMT +
                                       WS-ADDONAMT.
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
           IF SUB-1 < 301
                 GO TO CTOS-010.
       CTOS-999.
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
                         B-UNIT (SUB-1).
             MOVE 0   TO B-ORDERQTY (SUB-1)
                         B-SHIPQTY (SUB-1)
                         B-SHIPPEDQTY (SUB-1)
                         B-STOCKPRICE (SUB-1)
                         B-STOCKCOST (SUB-1)
                         B-DISCOUNTPERITEM (SUB-1)
                         B-INVOICED (SUB-1)
                         B-MIN-PERC (SUB-1)
                         B-NETT (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 301
                 GO TO CF-010.
       CF-999.
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
               MOVE "PARAMETER BUSY RINVQUES, PRESS 'ESC' TO RETRY."
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
            MOVE INVQUES-MU-GP-PERC      TO WS-QUES-MU-GP-PERC.
       RINVQUES-999.
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
               MOVE 2710 TO POS
               DISPLAY "NO PARAMETER RECORD!!!!" AT POS
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY RP-010, PRESS 'ESC' TO RETRY."
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
       END-OFF SECTION.
       END-000.
           MOVE " " TO WS-TYPE.
           MOVE 2810 TO POS.
           DISPLAY "Press <RETURN> To Exit Program.           " AT POS.
           ADD 50 TO POS.
           
           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 25        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TYPE

            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO END-500
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO END-000.
      *     ACCEPT WS-TYPE AT POS.
       END-500.
      *     PERFORM GET-USER-MAIL-NAME
      *     PERFORM GET-REPORT-Y2K-DATE
      *     PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           IF WS-DOCPRINTED = "Y"
               PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           CLOSE STOCK-TRANS-FILE
                 INCR-REGISTER
                 SBREP-MASTER
                 STOCK-MASTER
                 DEBTOR-MASTER.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           PERFORM GET-SYSTEM-Y2K-DATE.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       OPEN-005.
           OPEN I-O SBREP-MASTER.
           IF WS-SBREP-ST1 NOT = 0 
              MOVE "SB-REP BUSY ON OPEN, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-SBREP-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-SBREP-ST1
               GO TO OPEN-005.
       OPEN-010.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0 
              MOVE "REGISTER BUSY ON OPEN, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-INCR-ST1
               GO TO OPEN-010.
       OPEN-011.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0 
              MOVE "DEBTOR BUSY ON OPEN, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO OPEN-011.
       OPEN-012.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0 
              MOVE "STOCK BUSY ON OPEN, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-012.
       OPEN-014.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE "PARAMATER BUSY ON OPEN, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO OPEN-014.
           PERFORM READ-INVQUES-FILE.
           PERFORM READ-PARAMETER.
           MOVE PA-GST-PERCENT TO WS-GST-PERCENT.
           MOVE PA-NAME        TO CO-NAME.
           CLOSE PARAMETER-FILE.
       OPEN-015.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0 
              MOVE "ST-TRANS BUSY ON OPEN, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000 
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020 
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 1
               MOVE 0 TO WS-STTRANS-ST1
               GO TO OPEN-015.
       OPEN-999.
           EXIT.
      *      
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "PrintDBReportInfo".
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
       Copy "WriteDailyExcep1".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
