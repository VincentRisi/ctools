       IDENTIFICATION DIVISION.
       PROGRAM-ID. SlOrFiRp.
       AUTHOR.  CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
        Copy "SelectStTrans".
        Copy "SelectSlRegister".
        Copy "SelectSlParameter".
        Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
              ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStTrans.
           COPY ChlfdRegister.
           COPY ChlfdDaily.
           COPY ChlfdParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-POORDERNO         PIC X(20) VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WSAN-CODE            PIC XX VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
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
       77  WS-PERC              PIC 9(4)V99 VALUE 0.
       77  WS-GST-PERCENT       PIC 99V99 VALUE 0.
       77  WS-PARAM             PIC X(34) VALUE " ".
       77  WS-PAGE              PIC 99 VALUE 0.
       77  WS-PERCENT           PIC 9(2)V99 VALUE 0.
       77  WS-INVOICEDISCOUNT   PIC 9(2)V99 VALUE 0.
       77  WS-ORDER-NO          PIC 9(5) VALUE 0.
       01  WS-ORDER-NO-DIS      PIC Z(4)9.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1     PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1        PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
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
           03  BODY-LINE OCCURS 300.
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
                   07  B-NETT              PIC 9(6)V99.
                   07  B-UNIT              PIC X(4).
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
       01  PLINE1.
           03  FILLER           PIC X(17) VALUE "PICKING SLIP NO:".
           03  P-DIG1           PIC X.
           03  P-SLIP           PIC Z(5)9.
           03  FILLER           PIC X VALUE "-".
           03  P-PRINTNUMBER    PIC 99.
           03  P-DIG2           PIC X.
           03  FILLER           PIC X(3) VALUE " ".
           03  FILLER           PIC X(9) VALUE "ACCOUNT:".
           03  P-DIG3           PIC X.
           03  P-ACCNO          PIC X(10).
           03  P-GSTNO          PIC X(17) VALUE " ".
           03  P-DIG4           PIC X.
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  P-PAGE           PIC Z9.
           03  FILLER           PIC X(76) VALUE " ".
       01  PLINE2.
           03  P-ADD            PIC X(45) VALUE " ".
           03  P-DEL            PIC X(30) VALUE " ".
           03  P-NAME           PIC X(10) VALUE " ".
           03  P-CONTACT        PIC X(47) VALUE " ".
       01  PLINE3.
           03  P-CODE           PIC 9(4) BLANK WHEN ZERO.
           03  FILLER           PIC X(26) VALUE " ".
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
           03  P-ADD1           PIC Z(5)9.99. 
           03  P-DES2           PIC X(13) VALUE " ".
           03  P-ADD2           PIC Z(5)9.99.
           03  P-DES3           PIC X(12) VALUE " ".
           03  P-ADD3           PIC Z(5)9.99.
           03  P-ADDDESC        PIC X(28) VALUE " ".
           03  P-DES4           PIC X(12) VALUE " ".
           03  P-ADD4           PIC Z(5)9.99.
           03  FILLER           PIC X(19) VALUE " ".
       01  P-CONTINUED.
           03  FILLER           PIC X(40) VALUE " ".
           03  FILLER           PIC X(22) VALUE "Continued To.....Page".
           03  P-CONTROL-PAGE      PIC 9.
           03  FILLER           PIC X(69) VALUE " ".
       01 P-ALLOCATE-LINE.
           03  P-AL-DIG1          PIC X.
           03  P-AL-COMMENT       PIC X(40).
           03  P-AL-DIG2          PIC X.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "** FINAL PRINT OF COMPLETE ORDERS **" AT POS
           MOVE 415 TO POS
           DISPLAY "************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1310 TO POS.
           DISPLAY "PRESS 'RETURN' TO CONTINUE OR 'END' TO EXIT"
              AT POS.
           ADD 45 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-003.
           IF W-ESCAPE-KEY = 3 OR = 6
               EXIT PROGRAM.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-500
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-500.
           MOVE 3010 TO POS.
           DISPLAY "Run in progress, please be patient......" AT POS.
           
           PERFORM OPEN-FILES.
           PERFORM GET-DATA.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       PRINT-INVOICE SECTION.
       PR-000.
           MOVE WS-INVOICE TO P-SLIP.
       PR-005.
           MOVE 1 TO WS-PAGE SUB-1 SUB-2.
           MOVE " " TO PRINT-REC.
       PR-010.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF WS-PAGE > 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE WS-PAGE TO P-CONTROL-PAGE
               WRITE PRINT-REC FROM P-CONTINUED AFTER 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC AFTER PAGE.
       PR-012.
           WRITE PRINT-REC FROM COMPANY-LINE AFTER 1
           MOVE " " TO PRINT-REC
           MOVE Ws-Print-Bold   TO P-AL-DIG1
           MOVE "** FINAL PRINT, ORDER COMPLETE **" TO P-AL-COMMENT
           MOVE Ws-Print-Unbold TO P-AL-DIG2
           WRITE PRINT-REC FROM P-ALLOCATE-LINE AFTER 1
           MOVE WS-PRINT-COMP TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE " " TO PRINT-REC.
       PRR-014.
           MOVE Ws-Print-Bold     TO P-DIG1 P-DIG3
           MOVE Ws-Print-Unbold   TO P-DIG2 P-DIG4
           MOVE WS-ACCOUNT-NUMBER TO P-ACCNO
           MOVE INCR-COPY-NUMBER  TO P-PRINTNUMBER
           MOVE WS-GSTNO          TO P-GSTNO
           MOVE WS-PAGE           TO P-PAGE
           WRITE PRINT-REC FROM PLINE1 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE WS-PRINT-COMP     TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE " " TO PRINT-REC.
           MOVE WS-NAME     TO P-ADD
           MOVE "PHONE NO:" TO P-NAME
           MOVE WS-PHONE    TO P-CONTACT
           WRITE PRINT-REC FROM PLINE2 AFTER 1
           MOVE " "         TO PRINT-REC PLINE2
           MOVE WS-ADD1     TO P-ADD
           MOVE WS-DELADD1  TO P-DEL
           MOVE "CONTACT :" TO P-NAME
           MOVE WS-CONTACT  TO P-CONTACT
           WRITE PRINT-REC FROM PLINE2 AFTER 1
           MOVE " "         TO PRINT-REC PLINE2
           MOVE WS-ADD2     TO P-ADD
           MOVE WS-DELADD2  TO P-DEL
           WRITE PRINT-REC FROM PLINE2 AFTER 1
           MOVE " "         TO PRINT-REC PLINE2
           MOVE WS-ADD3     TO P-ADD
           MOVE WS-DELADD3  TO P-DEL
           MOVE " "         TO P-DEL
           WRITE PRINT-REC FROM PLINE2 AFTER 1
           MOVE " "         TO PRINT-REC PLINE2
           MOVE WS-POSTCODE TO P-CODE
           WRITE PRINT-REC FROM PLINE3 AFTER 1
           MOVE " "         TO PRINT-REC PLINE3
           WRITE PRINT-REC AFTER 1
           WRITE PRINT-REC FROM PLINE4-1 AFTER 1
           MOVE WS-TERMOFSALE  TO P-TERMS
           MOVE WS-POORDERNO   TO P-PO
           MOVE WSAN-CODE      TO P-SOLD
           MOVE WS-DELIVERVIA  TO P-VIA
           MOVE WS-BINNO       TO P-BIN
           MOVE WS-SOLDBY      TO P-SOLDBY
           MOVE WS-INVOICEDATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE   TO P-DATE
           MOVE WS-INVOICE     TO P-INV
           MOVE WS-DATE        TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE   TO P-TODAY
           WRITE PRINT-REC FROM PLINE4 AFTER 1
           MOVE " "            TO PRINT-REC PLINE4.
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
           MOVE B-ORDERQTY (SUB-1)          TO P-ORDER
           MOVE "***ALL SHIPPED***"         TO P-SHIPPED
           MOVE B-STOCKPRICE (SUB-1)        TO P-PRICE
           MOVE B-STOCKCOST (SUB-1)         TO P-COST
           MOVE B-DISCOUNTPERITEM (SUB-1)   TO P-DISCOUNT
           MOVE "%"                         TO P-PERC
           MOVE B-NETT (SUB-1)              TO P-NETT
           MOVE B-TAX (SUB-1)               TO P-TAX
           MOVE B-UNIT (SUB-1)              TO P-UNIT
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
           MOVE "   GST AMT :" TO P-DES3
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
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " " TO P-ADDLINE PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE " " TO P-ADDLINE PRINT-REC

           MOVE "COMPLETE ORDER AMOUNTS:" TO P-ADDDESC
           MOVE "DISCOUNT: R"   TO P-DES4
           MOVE WS-DISCOUNTREG1 TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " " TO P-ADDLINE PRINT-REC

           MOVE "COST AMT: R" TO P-DES4
           MOVE WS-COSTTOTAL1 TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " " TO P-ADDLINE PRINT-REC

           MOVE "MARGIN  : R" TO P-DES4
           COMPUTE WS-MARGIN = WS-PRICETOTAL1 - WS-COSTTOTAL1
           MOVE WS-MARGIN TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " " TO P-ADDLINE PRINT-REC

           MOVE "PROFIT %:  " TO P-DES4
           COMPUTE WS-PERC = (WS-MARGIN / WS-COSTTOTAL1) * 100
           MOVE WS-PERC TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1
           MOVE " " TO P-ADDLINE PRINT-REC

           MOVE "ORDER TOTAL"  TO P-DES4
           MOVE WS-PRICETOTAL1 TO P-ADD4
           WRITE PRINT-REC FROM P-ADDLINE AFTER 1.
       PR-036.
           MOVE " " TO P-DEL PRINT-REC
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           WRITE PRINT-REC AFTER PAGE.
       PR-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-005.
           MOVE "L" TO INCR-PRINTED.
           START INCR-REGISTER KEY NOT < INCR-PRINTED
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE "NO ORDERS TO PRINT ON START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-999.
       GET-010.
           PERFORM READ-REGISTER.
           IF INCR-INVOICE = 0
               GO TO GET-999
           ELSE
               PERFORM READ-STOCK-TRANSACTIONS.
           PERFORM CALCULATE-TOTALS.
           PERFORM CALCULATE-ORDER-TOTAL.
           PERFORM PRINT-INVOICE.
           ADD 1 TO WS-ORDER-NO.
           MOVE WS-ORDER-NO TO WS-ORDER-NO-DIS.
           MOVE 2510 TO POS.
           DISPLAY "Number of orders processed so far :" AT POS.
           MOVE 2548 TO POS.
           DISPLAY WS-ORDER-NO-DIS AT POS.
           PERFORM CLEAR-FIELDS.
           GO TO GET-010.
       GET-999.
           EXIT.
      *
       READ-REGISTER SECTION.
       RIR-005.
           READ INCR-REGISTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10 OR = 23
               MOVE 0 TO INCR-INVOICE
               GO TO RIR-999.
           IF WS-INCR-ST1 NOT = 0
             MOVE "REGISTER BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-005.
           IF INCR-TRANS NOT = 4
               GO TO RIR-005.
           IF INCR-PRINTED NOT = "L"
               MOVE 0 TO INCR-INVOICE
               GO TO RIR-999.
       RIR-010.
           MOVE INCR-INVOICE        TO WS-INVOICE
           MOVE INCR-ACCOUNT        TO WS-ACCOUNT-NUMBER
           MOVE INCR-GSTNO          TO WS-GSTNO
           MOVE INCR-DATE           TO WS-INVOICEDATE
           MOVE INCR-SALES          TO WSAN-CODE
           MOVE INCR-INVCRED-AMT    TO WS-INVOICETOTAL
           MOVE INCR-TAX            TO WS-TAXAMT
           MOVE INCR-ADDONS         TO WS-ADDONAMT
           MOVE INCR-SB-TYPE        TO WS-SOLDBY
           COMPUTE WS-SUBTOTAL =
                WS-INVOICETOTAL - (WS-TAXAMT + WS-ADDONAMT)
           MOVE INCR-NAME      TO WS-NAME
           MOVE INCR-ADD1      TO WS-ADD1
           MOVE INCR-ADD2      TO WS-ADD2
           MOVE INCR-ADD3      TO WS-ADD3
           MOVE INCR-CODE      TO WS-POSTCODE
           MOVE INCR-DEL1      TO WS-DELADD1
           MOVE INCR-DEL2      TO WS-DELADD2
           MOVE INCR-DEL3      TO WS-DELADD3
           MOVE INCR-TERMS     TO WS-TERMOFSALE
           MOVE INCR-PORDER    TO WS-POORDERNO
           MOVE INCR-CONTACT   TO WS-CONTACT
           MOVE INCR-PHONE     TO WS-PHONE
           MOVE INCR-DELIVERY  TO WS-DELIVERVIA
           MOVE INCR-BIN       TO WS-BINNO
           MOVE INCR-COMMENT   TO WS-COMMENTLINE
           MOVE INCR-ADDPOST    TO WS-POSTADDON
           MOVE INCR-ADDFREIGHT TO WS-ADDONFREIGHT
           MOVE INCR-ADDLABOUR  TO WS-HANDADDON
           MOVE INCR-ADDMISC    TO WS-MISCADDON.
       RIR-900.
           ADD 1    TO INCR-COPY-NUMBER
           MOVE "Y" TO INCR-PRINTED.
           REWRITE INCR-REC
                  INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON REWRIT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-900.
       RIR-999.
           EXIT.
      *
       READ-STOCK-TRANSACTIONS SECTION.
       RSTT-000.
           MOVE 1 TO SUB-1.
           MOVE WS-INVOICE TO STTR-REFERENCE1.
           MOVE 4          TO STTR-TYPE.
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
              MOVE "STTRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RSTT-010.
           IF STTR-REFERENCE1 NOT = WS-INVOICE
              GO TO RSTT-999.
           IF STTR-TYPE NOT = 4
              GO TO RSTT-010.
           MOVE STTR-STOCK-NUMBER TO B-STOCKNUMBER (SUB-1)
                                     SPLIT-STOCK.
           MOVE STTR-INV-NO       TO B-INVOICED (SUB-1).
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
           GO TO RSTT-030.
       RSTT-020.
           MOVE COM-ORDERQTY      TO C-ORDER (SUB-1)
           MOVE COM-SHIPQTY       TO C-SHIP (SUB-1)
           MOVE COM-DESC          TO C-DESC (SUB-1)
           MOVE COM-UNIT          TO C-UNIT (SUB-1)
           MOVE COM-PRICE         TO C-PRICE (SUB-1)
           MOVE COM-COST          TO C-COST (SUB-1)
           MOVE COM-DISC          TO C-DISC (SUB-1).
       RSTT-030.
           IF STTR-COMPLETE NOT = "R"
              MOVE "Y" TO STTR-COMPLETE STTR-ST-COMPLETE 
                          STTR-AC-COMPLETE.
           REWRITE STOCK-TRANS-REC
                  INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
               MOVE "ST-TRANS FILE BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RSTT-030.
       RSTT-050.
           ADD 1 TO SUB-1.
           MOVE SUB-1 TO SUB-20.
           IF SUB-1 > 300
              GO TO RSTT-999.
           GO TO RSTT-010.
       RSTT-999.
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
             COMPUTE WS-TAXAMT ROUNDED = WS-TAXABLETOTAL * 
                                         WS-GST-PERCENT / 100.
             COMPUTE WS-ADDONAMT = WS-ADDONFREIGHT + WS-POSTADDON + 
                                   WS-HANDADDON + WS-MISCADDON.
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
                         B-NETT (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 301
                 GO TO CF-010.
       CF-999.
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
              MOVE "SLPARAMETER NO SUCH FILE @ READ, 'ESC' TO EXIT." 
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "SLPARAMETER FILE BUSY ON READ, 'ESC' TO RETRY." 
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
       OPEN-FILES SECTION.
       OPEN-000.
           PERFORM GET-SYSTEM-Y2K-DATE.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       OPEN-010.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0 
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-010.
       OPEN-015.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-015.
       OPEN-014.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "SLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
           PERFORM READ-PARAMETER.
           MOVE PA-GST-PERCENT TO WS-GST-PERCENT.
           MOVE PA-NAME        TO CO-NAME.
           CLOSE PARAMETER-FILE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           MOVE " " TO WS-SOLDBY
           PERFORM ERROR-020
           MOVE 3010 TO POS
           DISPLAY
            "Run Finished, Press <RETURN> to EXIT The Program.    "
             AT POS
           ADD 50 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 60        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SOLDBY.
           
      *     ACCEPT WS-SOLDBY AT POS
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           CLOSE STOCK-TRANS-FILE
                 INCR-REGISTER.
            EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *      
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
       Copy "WriteDailyExcep1".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
