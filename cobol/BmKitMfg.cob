       IDENTIFICATION DIVISION.
       PROGRAM-ID. BmKitMfg.
       AUTHOR.  CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       Copy "SelectStMaster".
       Copy "SelectBmMaster".
       Copy "SelectSlParameter".
       Copy "SelectStTrans".
       Copy "SelectSlRegister".
       Copy "SelectStOrders".
       Copy "SelectStReceipt".
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
           COPY ChlfdParam.
           COPY ChlfdToolkit.
           COPY ChlfdStTrans.
           COPY ChlfdOutOrd.
           COPY ChlfdRegister.
           COPY ChlfdStkReceipts.
           COPY ChlfdDaily.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
       FD  LABEL-PRINT.
       01  LP-REC               PIC X(250).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-LABELPRINTER      PIC X(100) VALUE " ".
       77  WS-DOTPRINTER        PIC X(100) VALUE " ".
       77  WS-NEWORDER          PIC X VALUE " ".
       77  WS-SUFFICIENT-STOCK  PIC X VALUE " ".
       77  WS-STOCKNUMBER       PIC X(15).
       77  WS-DIS               PIC X VALUE " ".
       77  WS-ORDER-COMPLETE    PIC X VALUE " ".
       77  WS-TYPE-OF-FINISH    PIC X VALUE " ".
       77  WS-LINECHANGED       PIC X VALUE " ".
       77  WS-MUST-PRINT        PIC X VALUE " ".
       77  WS-PRINT-AMTS        PIC X VALUE " ".
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-INVOICEDATE       PIC 9(8) VALUE 0.
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-ORDER             PIC 9(6) VALUE 0.
       77  WS-TODAYS-DATE       PIC 9(8) VALUE 0.
       77  WS-ORDERDATE         PIC 9(8) VALUE 0.
       77  WS-STTRANS-NO        PIC 9(6) VALUE 0.
       77  WS-BODY-LINE         PIC ZZ9.
       77  WS-PARAM             PIC X(34) VALUE " ".
       77  WS-IMM-PR            PIC X VALUE " ".
       77  WS-PAGE              PIC 99 VALUE 0.
       77  WS-STOCK-INQUIRY     PIC X(8) VALUE "StMastIq".
       77  WS-ORDER-INQUIRY     PIC X(8) VALUE "StOrStIq".
       77  WS-BO-QTY            PIC S9(5) VALUE 0.
       77  WS-TOT-ONHAND        PIC S9(5) VALUE 0.
       77  WS-PRICES            PIC 9(6)V99 VALUE 0.
       77  WS-FACTOR            PIC 9(3) VALUE 0.
       77  WS-PERC              PIC 9(3) VALUE 0.
       77  WS-NEW-COST          PIC 9(8)V99 VALUE 0.
       77  WS-COSTS             PIC 9(6)V99 VALUE 0.
       77  WS-KITQTY            PIC 9(3) VALUE 0.
       77  WS-RESQTY            PIC 9(3) VALUE 0.
       77  WS-MFGQTY            PIC 9(3) VALUE 0.
       77  WS-SHPDQTY           PIC 9(3) VALUE 0.
       77  WS-TEMPQTY           PIC S9(5) VALUE 0.
       77  WS-ORDERQTY          PIC 9(5) VALUE 0.
       77  WS-TOTALRESQTY       PIC 9(5) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-WRITE             PIC X VALUE " ".
       77  WS-STTR-SHIPQTY      PIC 9(5) VALUE 0.
       77  WS-STTR-ORDERQTY     PIC 9(5) VALUE 0.
       77  WS-ERR               PIC XXX VALUE " ".
       77  WS-YN                PIC X VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-ST-PRINT-LABELS   PIC X VALUE " ".
       77  WS-1STPRINT          PIC X VALUE " ".
       77  WS-END-OF-FILE       PIC X VALUE " ".
       77  WS-KITQTY-EACH-ITEM  PIC 99 VALUE 0.
       77  WS-START-POS         PIC 9 VALUE 0.
       01  WS-STDESC.
           03  WS-DESC1         PIC X(20) VALUE " ".
           03  WS-DESC2         PIC X(20) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1        PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1  PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1      PIC 99.
       01  WS-STKRECEIPT-STATUS.
           03  WS-STKRECEIPT-ST1   PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1         PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-KIT-ST1          PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1       PIC 99.
       01  WS-Spl-STATUS.
           03  WS-Spl-ST1          PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X VALUE " ".
           03  SP-REST          PIC X(14) VALUE " ".
       01  ALPHABET-FIELD.
           03  ALPHA-FIELD      PIC X.
           88  ALPHA-VALUE      VALUES ARE "A" THRU "Z".
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 151.
               05  B-STOCKNUMBER.
                   07  B-1ST-CHAR      PIC X.
                   07  B-REST          PIC X(14).
               05  B-NEWLINE           PIC X.
               05  B-STTRANS           PIC 9(6).
               05  B-REMAINDER.
                   07  B-ORDERQTY          PIC 9(5).
                   07  B-SHIPQTY           PIC 9(5).
                   07  B-MFGQTY            PIC 9(5).
                   07  B-SHIPPEDQTY        PIC 9(5).
                   07  B-STOCKDESCRIPTION  PIC X(20).
                   07  B-STOCKDESCRIPTION2 PIC X(20).
                   07  B-STOCKPRICE        PIC 9(6)V99.
                   07  B-STOCKCOST         PIC 9(6)V99.
                   07  B-UNIT              PIC X(4).
       01  WS-RECEIPT-LINE.
           03  WS-BM-DESC           PIC X(4).
           03  WS-BM-NUMBER         PIC Z(5)9.
           03  FILLER               PIC X.
           03  WS-BM-COPY-DESC      PIC X(5).
           03  WS-BM-COPY           PIC Z9.
       01  WS-KITS.
           03  WS-KIT-SAVE          PIC X(15).
           03  WS-KIT-DESC1         PIC X(20).
           03  WS-KIT-DESC2         PIC X(20).
           03  WS-KIT-PRICE         PIC 9(6)V99.
           03  WS-KIT-COMMENT       PIC X(40).
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
       01  WS-ONHAND-LINE.
           03  FILLER-ONHAND          PIC X(8).
           03  WS-KITQTYONHAND           PIC Z(4)9.
           03  FILLER                 PIC X(5) VALUE " ".
           03  FILLER-ONRES           PIC X(7).
           03  WS-KITQTYONRESERVE        PIC Z(4)9.
           03  FILLER                 PIC X(5) VALUE " ".
           03  FILLER-ONBO            PIC X(7).
           03  WS-KITQTYONBORDER         PIC Z(4)9.
           03  FILLER                 PIC X(5) VALUE " ".
           03  FILLER-ONORD           PIC X(9).
           03  WS-KITQTYONORDER          PIC Z(4)9.
       01  HEADER1.
           03  FILLER          PIC X(12) VALUE "TODAYS DATE:".
           03  H1-TODAYS-DATE  PIC X(10).
           03  FILLER          PIC X(3) VALUE " ".
           03  FILLER          PIC X(13) VALUE "CREATED DATE:".
           03  H1-CREATE-DATE  PIC X(10).
           03  FILLER          PIC X(3) VALUE " ".
           03  FILLER          PIC X(38) VALUE
           "** TOOLKIT ASSEMBLY PICKING SLIP **".
           03  H1-P-TYPE1      PIC X.
           03  FILLER          PIC X(8) VALUE "SLIP NO:".
           03  H1-NO           PIC Z(5)9.
           03  FILLER          PIC X VALUE "/".
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
           03  FILLER          PIC X(50) VALUE
           "PRICE      COST  ORDER MFG NOW B-ORDER  USED PREV".
           03  FILLER          PIC X(15) VALUE "ON HAND    RES".
       01  DETAIL-LINE.
           03  D-STOCK         PIC X(16) VALUE " ".
           03  D-DESC1         PIC X(20) VALUE " ".
           03  D-DESC2         PIC X(21) VALUE " ".
           03  D-PRICE         PIC Z(4)9.99.
           03  FILLER          PIC X(2) VALUE " ".
           03  D-COST          PIC Z(4)9.99.
           03  FILLER          PIC X(2) VALUE " ".
           03  D-ORDER         PIC Z(4)9.
           03  FILLER          PIC X(3) VALUE " ".
           03  D-MFG           PIC Z(4)9.
           03  FILLER          PIC X(3) VALUE " ".
           03  D-BO            PIC Z(4)9.
           03  FILLER          PIC X(6) VALUE " ".
           03  D-SHPD          PIC Z(4)9.
           03  FILLER          PIC X(2) VALUE " ".
           03  D-ONHAND        PIC Z(5)9.
           03  D-BLANK         PIC X(1) VALUE " ".
           03  D-RESERVE       PIC Z(5)9.
       01  TOTAL-LINE.
           03  FILLER          PIC X(19) VALUE "TOTAL ITEM PRICE: R".
           03  TOT-PRICE       PIC Z(5)9.99.
           03  FILLER          PIC X(9) VALUE " ".
           03  FILLER          PIC X(19) VALUE "TOTAL ITEM COSTS: R".
           03  TOT-COST        PIC Z(5)9.99.
       01  P-CONTINUED.
           03  FILLER          PIC X(15) VALUE " ".
           03  FILLER          PIC X(17) VALUE "Continued to Page".
           03  P-CONTROL-PAGE     PIC Z9.
       01  PERC-LINE.
           03  FILLER          PIC X(15) VALUE "PERCENTAGE MFG:".
           03  PERC-AMT        PIC Z(2)9.
       01  P-MESSAGE-LINE.
           03  P-MES-TYPE      PIC X(2) VALUE " ".
           03  P-MESSAGE       PIC X(130) VALUE " ".
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
           03  P-BIN            PIC X(10) VALUE " ".
           03  P-UNITDESC       PIC X(5) VALUE " ".
           03  P-UNIT           PIC X(10) VALUE " ".
           03  P-BMDESC         PIC X(5) VALUE " ".
           03  P-BM-NUM         PIC Z(5)9.
           03  FILLER           PIC X(7) VALUE " ".
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
           PERFORM CLEAR-SCREEN
           MOVE 320 TO POS
           DISPLAY "** KIT MANUFACTURING PROGRAM **" AT POS
           MOVE 420 TO POS
           DISPLAY "*******************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
           MOVE WS-PRINTER TO WS-DOTPRINTER.
           MOVE 2510 TO POS
           DISPLAY "Program loading....." AT POS.
       CONTROL-010.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN
           PERFORM CLEAR-FIELDS.
           PERFORM DISPLAY-FORM.
       CONTROL-050.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           MOVE WS-DATE     TO WS-INVOICEDATE.
           MOVE "ON-HAND:"  TO FILLER-ONHAND
           MOVE "ON-RES:"   TO FILLER-ONRES
           MOVE "ON-B/O:"   TO FILLER-ONBO
           MOVE "ON-ORDER:" TO FILLER-ONORD.
       CONTROL-060.
           PERFORM GET-DATA.
           MOVE 2910 TO POS.
           DISPLAY "PRINTING BILL-OF-MATERIALS...." AT POS.
           PERFORM PRINT-TOOLKIT.
      ********************************************************
      *     WS-TYPE-OF-FINISH                                *
      * 'F5' =X"19"   TYPE=1     UPDATE & PRINT              *
      * 'F10'=X"1F"   TYPE=2     PRINT ONLY                  *
      ********************************************************
           IF WS-TYPE-OF-FINISH = "2"
              MOVE 2910 TO POS
              DISPLAY "ENTERING KIT ON ORDER........." AT POS
              PERFORM ENTER-KIT-ON-ORDER.
           IF WS-TYPE-OF-FINISH = "1"
            IF WS-NEWORDER = "Y"
              MOVE 2910 TO POS
              DISPLAY "ENTERING KIT ON ORDER........." AT POS
              PERFORM ENTER-KIT-ON-ORDER.
           IF WS-TYPE-OF-FINISH = "1"
              MOVE 2910 TO POS
              DISPLAY "UPDATING STOCK FILE..........." AT POS
              PERFORM UPDATE-STOCK.
           MOVE 2910 TO POS.
           DISPLAY "UPDATING REGISTER FILE........" AT POS.
           MOVE "N" TO WS-NEWORDER.
           PERFORM REWRITE-ORDER-TRANS.
           MOVE 2910 TO POS.
           DISPLAY "UPDATING ST-TRANS FILE........" AT POS.
           PERFORM REWRITE-STOCK-TRANSACTIONS.
       CONTROL-960.
           IF WS-MFGQTY = 0
               GO TO CONTROL-050.
           IF WS-TYPE-OF-FINISH = "2"
               GO TO CONTROL-050.
           MOVE " " TO WS-YN
           PERFORM CLEAR-010
           MOVE 2910 TO POS
           DISPLAY "DO YOU WISH TO PRINT STOCK LABELS: [ ]" AT POS
           ADD 36 TO POS

           MOVE 'N'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 45        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-YN.

           IF WS-YN NOT = "Y" AND NOT = "N"
                GO TO CONTROL-960.
           IF WS-YN = "N"
              PERFORM ERROR1-020
              GO TO CONTROL-970.
           IF WS-YN = "Y"
              PERFORM ERROR1-020
              PERFORM CONTROL-020
              PERFORM CONTROL-100
              PERFORM PRINT-LABELS.
       CONTROL-970.
           GO TO CONTROL-050.
       CONTROL-020.
           Move 9 To Ws-PrinterNumber (21)
           Move 9 To Ws-PrinterType (21).
           Copy "PrinterStSpecial".
       CONTROL-999.
           EXIT.
      *
       PRINT-TOOLKIT SECTION.
       PR-004.
           MOVE "REFNO"    TO F-FIELDNAME
           MOVE 5          TO F-CBFIELDNAME
           MOVE WS-INVOICE TO F-EDNAMEFIELDNUM
           MOVE 7          TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-NUMERIC.
           PERFORM CLEAR-010.
           MOVE 2610 TO POS
           DISPLAY "PRINT PRICES, COSTS ETC, Y OR N : [ ]" AT POS
           ADD 35 TO POS

           MOVE WS-PRINT-AMTS TO CDA-DATA.
           MOVE 1             TO CDA-DATALEN.
           MOVE 23            TO CDA-ROW.
           MOVE 44            TO CDA-COL.
           MOVE CDA-WHITE     TO CDA-COLOR.
           MOVE 'F'           TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA      TO WS-PRINT-AMTS.

           IF WS-PRINT-AMTS NOT = "Y" AND NOT = "N"
              MOVE "ONLY Y OR N ARE VALID ENTRIES, RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO PR-004.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
       PR-005.
           MOVE 1 TO WS-PAGE SUB-1 SUB-2.
           MOVE " " TO PRINT-REC.
           MOVE WS-DOTPRINTER TO WS-PRINTER-SAVE.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       PR-010.
      *     IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF WS-PAGE > 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE WS-PAGE TO P-CONTROL-PAGE
               WRITE PRINT-REC FROM P-CONTINUED
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC BEFORE PAGE.
       PR-012.
           IF WS-NEWORDER = "Y"
              MOVE WS-INVOICEDATE TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE   TO H1-CREATE-DATE
           ELSE
              MOVE WS-ORDERDATE   TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE   TO H1-CREATE-DATE.
           MOVE WS-INVOICEDATE    TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE      TO H1-TODAYS-DATE.
           MOVE Ws-Print-Bold     TO H1-P-TYPE1
           MOVE Ws-Print-UnBold   TO H1-P-TYPE2
           MOVE WS-INVOICE        TO H1-NO
           MOVE WS-PAGE           TO H1-PAGE
           MOVE INCR-COPY-NUMBER  TO H1-COPY.
           WRITE PRINT-REC      FROM COMPANY-LINE
           MOVE " "               TO PRINT-REC
           WRITE PRINT-REC      FROM HEADER1
           MOVE " "               TO PRINT-REC
           WRITE PRINT-REC
           MOVE WS-KIT-SAVE       TO H2-KIT
           MOVE WS-KIT-DESC1      TO H2-DESC1
           MOVE WS-KIT-DESC2      TO H2-DESC2
           IF WS-PRINT-AMTS = "Y"
             MOVE WS-KIT-PRICE    TO H2-PRICE.
           WRITE PRINT-REC      FROM HEADER2
           MOVE " "               TO PRINT-REC
           MOVE WS-KITQTY         TO H3-QTY
           MOVE WS-MFGQTY         TO H3-MFGQTY
           MOVE WS-SHPDQTY        TO H3-SHPDQTY
           MOVE WS-KIT-COMMENT    TO H3-COMMENT
           WRITE PRINT-REC      FROM HEADER3
           MOVE " "               TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC      FROM HEADER4
           MOVE " "               TO PRINT-REC
           WRITE PRINT-REC
           MOVE 8 TO SUB-2.
           MOVE " " TO DETAIL-LINE.
       PR-020.
           IF SUB-1 < 149
            IF SUB-1 = SUB-25
               SUBTRACT 1 FROM SUB-2
               GO TO PR-030.
           IF SUB-2 > 58
               MOVE 1 TO SUB-2
               ADD 1 TO WS-PAGE
               GO TO PR-010.
           IF SUB-1 > 125
               GO TO PR-030.
           IF B-STOCKNUMBER (SUB-1) = " "
             AND B-ORDERQTY (SUB-1) = 0
             GO TO PR-030.
      *5 NEW LINES ENTERED 19/5/2001 
           IF B-STOCKNUMBER (SUB-1) NOT = "/" AND NOT = "*"
               MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER
               PERFORM READ-STOCK-QTY.
           MOVE ST-QTYONHAND                TO D-ONHAND
           MOVE ST-QTYONRESERVE             TO D-RESERVE.
             
           MOVE B-STOCKNUMBER (SUB-1)       TO D-STOCK
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO D-DESC1
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO D-DESC2
           IF WS-PRINT-AMTS = "Y"
               MOVE B-STOCKPRICE (SUB-1)    TO D-PRICE
               MOVE B-STOCKCOST (SUB-1)     TO D-COST.
           MOVE B-ORDERQTY (SUB-1)          TO D-ORDER
           MOVE B-MFGQTY (SUB-1)            TO D-MFG
           MOVE B-SHIPPEDQTY (SUB-1)        TO D-SHPD
           COMPUTE WS-BO-QTY = B-ORDERQTY (SUB-1) -
               (B-SHIPQTY (SUB-1) + B-SHIPPEDQTY (SUB-1))
           MOVE WS-BO-QTY                   TO D-BO.
       PR-025.
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO DETAIL-LINE PRINT-REC.
           ADD 1 TO SUB-1
                    SUB-2.
           IF SUB-1 < 150
              GO TO PR-020.
       PR-030.
           WRITE PRINT-REC.
           IF WS-PRINT-AMTS = "Y"
               MOVE WS-PRICES TO TOT-PRICE
               MOVE WS-COSTS  TO TOT-COST
               WRITE PRINT-REC FROM TOTAL-LINE
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC.

           COMPUTE WS-PERC = ((WS-MFGQTY + WS-SHPDQTY) / WS-KITQTY) * 100.
           MOVE WS-PERC TO PERC-AMT.
           WRITE PRINT-REC FROM PERC-LINE AFTER 1.
           MOVE " " TO PRINT-REC.

           MOVE Ws-Print-Bold TO P-MES-TYPE.
           IF WS-TYPE-OF-FINISH = "2"
            IF INCR-PRINTED NOT = "P"
              MOVE "** ITEMS ONLY RESERVED FOR MANUFACTURE **"
              TO P-MESSAGE
              GO TO PR-036
            ELSE
               MOVE
               "** KIT PREVIOUSLY ENTERED, ITEMS STILL ON BACK-ORDER **"
                TO P-MESSAGE
              GO TO PR-036.
           IF WS-TYPE-OF-FINISH = "1"
            IF WS-ORDER-COMPLETE = "Y"
               MOVE "** ITEMS ENTERED, KIT NOW COMPLETE **"
               TO P-MESSAGE
            ELSE
               MOVE "** KIT ENTERED, AND ITEMS ARE ON BACK-ORDER **"
                TO P-MESSAGE.
       PR-036.
           WRITE PRINT-REC FROM P-MESSAGE-LINE
           MOVE " " TO PRINT-REC P-MESSAGE-LINE
           MOVE Ws-Print-UnBold TO P-MES-TYPE
           WRITE PRINT-REC FROM P-MESSAGE-LINE
           IF WS-PRINT-TYPE NOT = 2
               MOVE WS-PRINT-NORMAL TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           MOVE 2758 TO POS
           DISPLAY WS-MESSAGE AT POS.
       PR-999.
           EXIT.
      *
       PRINT-LABELS SECTION.
       PRLB-000.
           MOVE WS-LABELPRINTER TO WS-PRINTER-SAVE.
           PERFORM GET-USER-PRINT-NAME.
           MOVE WS-PRINTER TO WS-LABELPRINTER.
           OPEN OUTPUT LABEL-PRINT.
           IF WS-Spl-ST1 NOT = 0
               MOVE "Print File Open error, 'ESC' To RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               move ws-LabelPrinter to ws-MESSAGE
               PERFORM ERROR-MESSAGE
               move ws-Printer to ws-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-SPL-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PRLB-000.
       PRLB-001.
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
               GO TO PRLB-001.
           MOVE WS-YN TO WS-START-POS.
           
           MOVE 2910 TO POS
           DISPLAY "The LABEL REPORT IS being compiled........." AT POS.
           MOVE WS-KIT-SAVE TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
           PERFORM ERROR-020.
       PRLB-010.
           IF WS-END-OF-FILE = " "
               PERFORM READ-MASTER.
       PRLB-020.
           WRITE LP-REC FROM  PLINE1 AFTER 1
           MOVE " " TO LP-REC PLINE1
           WRITE LP-REC FROM  PLINE2 AFTER 1
           MOVE " " TO LP-REC PLINE2
           WRITE LP-REC FROM  PLINE3 AFTER 1
           MOVE " " TO LP-REC PLINE3
           WRITE LP-REC FROM  PLINE4 AFTER 1
           MOVE " " TO LP-REC PLINE4
           WRITE LP-REC AFTER 2.
       PRLB-030.
           IF WS-END-OF-FILE = " "
              GO TO PRLB-010.
       PRLB-900.
           MOVE 2510 TO POS
           DISPLAY "                                         " AT POS.
           CLOSE LABEL-PRINT.
           PERFORM SEND-REPORT-TO-PRINTER.
       PRLB-999.
           EXIT.
      *
       READ-MASTER SECTION.
       RM-000.
           IF WS-START-POS NOT = 1
              PERFORM FILL-IN-BLANKS.
           MOVE WS-START-POS TO SUB-1
           MOVE 0            TO SUB-2.
       RM-010.
           READ STOCK-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
              MOVE "1" TO WS-END-OF-FILE
              GO TO RM-999.
           IF WS-STOCK-ST1 NOT = 0
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCK RECORD BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RM-010.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           MOVE WS-MFGQTY         TO WS-KITQTY-EACH-ITEM.
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
           MOVE "BM No"           TO P-BMDESC (SUB-1)
           MOVE WS-INVOICE        TO P-BM-NUM (SUB-1).
           
           IF SUB-1 < 4
              ADD 1 TO SUB-1
           ELSE
              PERFORM PRLB-020
              MOVE 1 TO SUB-1.
           ADD 1 TO SUB-2.
           IF SUB-2 = WS-KITQTY-EACH-ITEM
              MOVE 0 TO SUB-2
              MOVE "1" TO WS-END-OF-FILE
              GO TO RM-999
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
       CHECK-TO-ENTER-ORDER SECTION.
       CTEO-000.
           IF WS-ABOVE-BODY = "1"
                 GO TO CTEO-999.
           PERFORM CLEAR-010.
           MOVE 2910 TO POS
           DISPLAY "DO YOU WISH TO CONTINUE WITH THIS B/M  : [ ]"
              AT POS
           ADD 42 TO POS

           MOVE 'N'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 51        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

           IF WS-ACCEPT NOT = "Y" AND NOT = "N"
              DISPLAY " " AT 3079 WITH BELL
              GO TO CTEO-000.
           IF WS-ACCEPT = "N"
              GO TO CTEO-999.
       CTEO-300.
           PERFORM READ-PARAMETER-LOCK
           MOVE PA-ORDER-NUMBER TO WS-INVOICE.
       CTEO-400.
           COMPUTE PA-ORDER-NUMBER = PA-ORDER-NUMBER + 1.
           PERFORM REWRITE-PARAMETER
           PERFORM READ-PARAMETER-LOCK
           IF WS-INVOICE = PA-ORDER-NUMBER
              GO TO CTEO-300.
           PERFORM REWRITE-PARAMETER.
       CTEO-500.
           MOVE "REFNO"    TO F-FIELDNAME
           MOVE 5          TO F-CBFIELDNAME
           MOVE WS-INVOICE TO F-EDNAMEFIELDNUM
           MOVE 7          TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-NUMERIC.
       CTEO-600.
           PERFORM ROT-050
           MOVE "N" TO INCR-PRINTED
           PERFORM ROT-055 
           PERFORM ROT-060
           PERFORM ERROR-020
           PERFORM ERROR1-020
           PERFORM READ-INVOICE-REGISTER
           MOVE "Y" TO WS-NEWORDER
           MOVE 0 TO INCR-COPY-NUMBER.
       CTEO-999.
           EXIT.
      *
       CHANGE-DEL-QTY SECTION.
       CDQ-005.
            MOVE "N" TO WS-LINECHANGED.
           PERFORM FILL-BODY.
           IF WS-ABOVE-BODY = "1"
               GO TO CDQ-999.
           PERFORM GET-290 THRU GET-999.
       CDQ-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-010.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "N" TO WS-LINECHANGED
                        WS-MUST-PRINT.
            MOVE "Y" TO WS-NEWORDER.
            MOVE " " TO WS-MESSAGE
                        WS-STOCKNUMBER
                        WS-KITS
                        WS-ACCEPT
                        WS-ABOVE-BODY
                        WS-DIS
                        WS-TYPE-OF-FINISH.
            MOVE 0 TO WS-KITQTY
                      WS-INVOICE
                      WS-MFGQTY
                      WS-SHPDQTY
                      SUB-1
                      SUB-25.
            PERFORM ERROR-020.
            PERFORM ERROR1-020.
            PERFORM CLEAR-FIELDS.
            PERFORM DISPLAY-FORM.
       GET-0110.
            PERFORM CONTROL-050.
            MOVE 2901 TO POS
            DISPLAY
           "ENTER ALL 'X' FOR SUPPLIER ORDER ENQUIRY, 'STOCK' FOR" &
           " STOCK ENQUIRY," AT POS
            MOVE 3001 TO POS
            DISPLAY
           " '*' & NUMBER FOR EXISTING BILL OR LEAVE BLANK TO CREATE" &
           " A NEW BILL OF MATERIAL." AT POS.
      
            MOVE "REFNO" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            MOVE 7       TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO F-NAMEFIELDRED.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            PERFORM ERROR-020.
            PERFORM ERROR1-020.
            IF F-NAMEFIELDRED = "       "
                GO TO GET-220.
            IF F-NAMEFIELDRED = "STOCK  "
                CLOSE STOCK-MASTER
                PERFORM CLEAR-SCREEN
                CALL WS-STOCK-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-STOCK-INQUIRY
                PERFORM OPEN-014
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD F-NAMEFIELDRED
                GO TO GET-010.
            IF F-NAMEFIELDRED = "XXXXXXX"
                CLOSE STOCK-MASTER
                CLOSE OUTSTANDING-ORDERS
                CLOSE PARAMETER-FILE
                PERFORM CLEAR-SCREEN
                CALL WS-ORDER-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-ORDER-INQUIRY
                PERFORM OPEN-014
                PERFORM OPEN-018
                PERFORM OPEN-017
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD F-NAMEFIELDRED
                GO TO GET-010.
      **********************************************************
      * <F1> KEY & 'D', TO CHANGE MANUFACTURE QTY'S            *
      **********************************************************
           IF F-EXIT-CH = X"15"
            IF F-NAMEFIELDRED1 = "D"
                MOVE "          " TO ALPHA-RATE
                MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-INVOICE
                PERFORM READ-INVOICE-REGISTER
            IF WS-NEWORDER = "N" OR = "P" OR = "T"
                PERFORM READ-STOCK-TRANSACTIONS
                PERFORM GET-150
                PERFORM GET-270
                MOVE "Y" TO WS-MUST-PRINT
                            WS-ACCEPT
                PERFORM CHANGE-DEL-QTY
                GO TO GET-999.
      ********************************************
      * 'F5' KEY, TO FIND OLD ORDER AND DISPLAY  *
      ********************************************
           IF F-EXIT-CH = X"19"
                MOVE "          " TO ALPHA-RATE
                MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-INVOICE
                PERFORM READ-INVOICE-REGISTER.
           IF F-EXIT-CH = X"19"
            IF WS-NEWORDER = "C" OR = "Y"
              GO TO GET-010.
           IF F-EXIT-CH = X"19"
            IF WS-NEWORDER = "N"
                PERFORM READ-STOCK-TRANSACTIONS
                GO TO GET-150.
      ********************************************
      * 'F10' KEY, TO DELETE COMPLETE ORDER      *
      ********************************************
           IF F-EXIT-CH = X"1F"
              MOVE "          " TO ALPHA-RATE
              MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
              PERFORM DECIMALISE-RATE
              MOVE NUMERIC-RATE TO WS-INVOICE
              PERFORM READ-INVOICE-REGISTER.
           IF F-EXIT-CH = X"1F"
            IF WS-NEWORDER = "Y"
              MOVE "ORDER NOT FOUND IN THE SYSTEM, CAN'T DELETE."
              TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
              GO TO GET-010.
           IF F-EXIT-CH = X"1F"
            IF WS-NEWORDER = "C"
              GO TO GET-010.
       GET-011.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
      *     PERFORM CLEAR-010
      *     PERFORM CLEAR-020.
           IF F-EXIT-CH = X"1F"
            IF WS-NEWORDER = "N"
              MOVE 2910 TO POS
              DISPLAY "ARE YOU SURE ABOUT DELETING THE ENTIRE ORDER."
              AT POS
              MOVE 3010 TO POS
              DISPLAY "ENTER Y TO DELETE, N TO STOP DELETION. [ ]"
               AT POS
              ADD 40 TO POS

              MOVE ' '       TO CDA-DATA
              MOVE 1         TO CDA-DATALEN
              MOVE 27        TO CDA-ROW
              MOVE 49        TO CDA-COL
              MOVE CDA-WHITE TO CDA-COLOR
              MOVE 'F'       TO CDA-ATTR
              PERFORM CTOS-ACCEPT
              MOVE CDA-DATA TO WS-DIS

            IF W-ESCAPE-KEY = 0 OR 1 OR 2
               MOVE X"1F" TO F-EXIT-CH
               GO TO GET-012
            ELSE
               MOVE X"1F" TO F-EXIT-CH
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-011.
       GET-012.
           IF F-EXIT-CH = X"1F"
            IF WS-DIS NOT = "Y" AND NOT = "N"
                GO TO GET-011.
           PERFORM ERROR-020.
           PERFORM ERROR1-020.
           IF F-EXIT-CH = X"1F"
            IF WS-NEWORDER = "N"
             IF WS-DIS = "Y"
                 PERFORM DELETE-STOCK-TRANS
                 PERFORM DELETE-INVOICE-REGISTER
                 PERFORM CANCEL-KIT-ORDER
                 PERFORM CLEAR-FIELDS
                 PERFORM ERROR1-020
                 GO TO GET-010.
           DISPLAY " " AT 3079 WITH BELL.
           GO TO GET-0110.
       GET-150.
            IF WS-NEWORDER = "Y"
               GO TO GET-220.
            MOVE "REFNO"    TO F-FIELDNAME.
            MOVE 5          TO F-CBFIELDNAME.
            MOVE WS-INVOICE TO F-EDNAMEFIELDNUM.
            MOVE 7          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "KITDATE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE WS-ORDERDATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "KITORD" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            MOVE WS-KITQTY   TO F-EDNAMEFIELDKITQTY.
            MOVE 3        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-KITQTY.

            MOVE "KITSHPD"  TO F-FIELDNAME.
            MOVE 7          TO F-CBFIELDNAME.
            MOVE WS-SHPDQTY TO F-EDNAMEFIELDKITQTY.
            MOVE 3          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-KITQTY.

            MOVE "KITNAME"   TO F-FIELDNAME.
            MOVE 7           TO F-CBFIELDNAME.
            MOVE WS-KIT-SAVE TO F-NAMEFIELD.
            MOVE 15          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE SPACES       TO WS-STDESC
            MOVE WS-KIT-DESC1 TO WS-DESC1
            MOVE WS-KIT-DESC2 TO WS-DESC2.

            MOVE "KT-DESC"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE WS-STDESC    TO F-NAMEFIELD.
            MOVE 40           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "KITPRICE"   TO F-FIELDNAME.
            MOVE 8            TO F-CBFIELDNAME.
            MOVE WS-KIT-PRICE TO F-EDNAMEFIELDAMOUNT.
            MOVE 9            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "KITCOMMENT"   TO F-FIELDNAME.
            MOVE 10             TO F-CBFIELDNAME.
            MOVE WS-KIT-COMMENT TO F-NAMEFIELD.
            MOVE 40             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COPYDESC"             TO F-FIELDNAME
            MOVE 8                      TO F-CBFIELDNAME
            MOVE "   B/M COPY NUMBER :" TO F-NAMEFIELD
            MOVE 20                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COPYNUMBER"     TO F-FIELDNAME
            MOVE 10               TO F-CBFIELDNAME
            MOVE INCR-COPY-NUMBER TO F-NAMEFIELD
            MOVE 2                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-219.
            IF WS-NEWORDER = "N"
                GO TO GET-240.
       GET-220.
            IF WS-NEWORDER = "Y"
              MOVE 2910 TO POS
              DISPLAY
            "ENTER QTY, PRESS <Alt-F10> TO REVERSE ENGINEER A KIT,"
                 AT POS
              MOVE 3020 TO POS
              DISPLAY "OR <RETURN> TO CREATE A BILL OF MATERIAL."
                 AT POS
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "KITORD"    TO F-FIELDNAME.
            MOVE 6           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 3           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-KITQTY F-EDNAMEFIELDKITQTY.
            IF F-EXIT-CH = X"01"
             IF WS-MUST-PRINT = "Y"
              MOVE "YOU MUST PRINT THIS BILL AS CHANGES HAVE BEEN MADE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-260.
            IF F-EXIT-CH = X"01"
             IF WS-KITQTY = 0
                GO TO GET-010
             ELSE
                PERFORM CANCEL-KIT
                GO TO GET-010.
            IF WS-NEWORDER = "N"
             IF F-EXIT-CH NOT = X"01"
                GO TO GET-222.
            IF WS-KITQTY NOT > 0
              MOVE "QTY MUST BE > 0" TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO GET-220.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-KITQTY.
            Perform Error-020
            Perform Error1-020.
            IF F-EXIT-CH = X"87" OR = x"9F"
               PERFORM REVERSE-ENGINEER
               GO TO GET-010.
       GET-222.
            MOVE "KITNAME" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
                GO TO GET-220.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-STOCKNUMBER.
            PERFORM READ-STOCK.
            MOVE WS-STOCKNUMBER TO WS-KIT-SAVE.
            IF ST-DESCRIPTION1 = "   "
               MOVE "INVALID TOOLKIT ENTRY, PRESS 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-222.
            
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1 WS-KIT-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2 WS-KIT-DESC2.

            MOVE "KT-DESC"      TO F-FIELDNAME.
            MOVE 7               TO F-CBFIELDNAME.
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "KITPRICE" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE ST-PRICE   TO F-EDNAMEFIELDAMOUNT WS-KIT-PRICE.
            MOVE 9          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "KITDATE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE WS-DATE      TO WS-ORDERDATE.
            MOVE WS-ORDERDATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-225.
            MOVE "KITCOMMENT" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-222.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-KIT-COMMENT.
            IF WS-MUST-PRINT = "Y"
                GO TO GET-260.
       GET-240.
            IF F-NAMEFIELDRED1 = "D"
                GO TO GET-270.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "RESQTY"    TO F-FIELDNAME.
            MOVE 6           TO F-CBFIELDNAME.
            COMPUTE WS-RESQTY = WS-KITQTY - WS-SHPDQTY.
            MOVE WS-RESQTY   TO F-EDNAMEFIELDKITQTY.
            MOVE 3           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-KITQTY.
            
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-225.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF NUMERIC-RATE > WS-RESQTY
                MOVE 
               "YOU CAN'T RESERVE MORE THAN ORDER-QTY - USED-QTY."
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-240.
            MOVE NUMERIC-RATE TO WS-RESQTY F-EDNAMEFIELDKITQTY.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-KITQTY.
       GET-242.
            IF WS-NEWORDER = "Y"
                PERFORM CHECK-TO-ENTER-ORDER
             IF WS-ACCEPT = "N"
                PERFORM CI-950
                GO TO GET-010.
            IF WS-NEWORDER = "N"
                PERFORM RESERVE-STOCK.
       GET-250.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
            IF WS-NEWORDER = "Y"
             IF WS-ABOVE-BODY NOT = "1"
                PERFORM READ-TOOLKIT
                PERFORM SUBTOTALS.
       GET-260.
            MOVE "TOTALPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-PRICES TO F-EDNAMEFIELDAMOUNT.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "TOTALCOST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE WS-COSTS TO F-EDNAMEFIELDAMOUNT.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
       GET-270.
            MOVE 1 TO SUB-1 F-INDEX
            PERFORM SCROLL-NEXT
            PERFORM SCROLL-PREVIOUS.
       GET-280.
            MOVE "N" TO WS-LINECHANGED.
            PERFORM FILL-BODY.
            IF WS-ABOVE-BODY = "1"
                GO TO GET-225.

            PERFORM ERROR1-020
            MOVE 2710 TO POS
            DISPLAY WS-MESSAGE AT POS.
       GET-290.
           MOVE
           "PRESS 'F5' TO COMPLETE THE KIT-ASSEMBLY & ENTER TO ON-HAND,"
              TO WS-MESSAGE.
           PERFORM ERROR1-000.
           MOVE " OR PRESS 'F10' TO RESERVE STOCK ONLY & PRINT."
               TO WS-MESSAGE.
           PERFORM ERROR-000.

           MOVE "TOTALCOST" TO F-FIELDNAME.
           MOVE 9           TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           MOVE 8 TO F-CBFIELDLENGTH.
           IF F-EXIT-CH = X"01"
              MOVE "2" TO WS-ABOVE-BODY
              GO TO GET-270.
       GET-300.
      ******************************
      * 'F5'=X"19"  'F10'=X"1F"    *
      ******************************
           IF F-EXIT-CH NOT = X"19" AND NOT = X"1F"
              GO TO GET-290.
           IF F-EXIT-CH = X"19"
              MOVE "1" TO WS-TYPE-OF-FINISH
           ELSE
              MOVE "2" TO WS-TYPE-OF-FINISH.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
       GET-350.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "ENTERQTY" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE WS-RESQTY  TO WS-MFGQTY F-EDNAMEFIELDKITQTY.
            MOVE 3          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-KITQTY.

            PERFORM USER-FILL-FIELD.

            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                GO TO GET-350.
            MOVE 3           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE < 0
               MOVE "QTY MUST BE ZERO OR HIGHER." TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO GET-350.
           IF NUMERIC-RATE > WS-MFGQTY
               MOVE "QTY CANNOT BE > QTY-RESERVED."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-350.
           MOVE NUMERIC-RATE TO WS-MFGQTY F-EDNAMEFIELDKITQTY.
           MOVE 3            TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-KITQTY.
            
           PERFORM CHECK-ORDER-COMPLETE.
           
      *     MOVE "WS-ORDER-COMPLETE = " TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE WS-ORDER-COMPLETE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR-020.
           
           IF WS-TYPE-OF-FINISH = "2"
                 GO TO GET-400.
           IF WS-KITQTY = WS-SHPDQTY + WS-MFGQTY
            IF WS-ORDER-COMPLETE = "Y"
                 GO TO GET-400.
           IF WS-KITQTY NOT = WS-SHPDQTY + WS-MFGQTY
                 GO TO GET-400.

           IF WS-TYPE-OF-FINISH = "1"
            IF WS-KITQTY = WS-SHPDQTY + WS-MFGQTY
              IF WS-ORDER-COMPLETE = "N"
                 MOVE 
           "YOU ARE COMPLETING THE BM AND THERE ARE B/O'S, PRESS <F7>"
              TO WS-MESSAGE
              PERFORM ERROR1-000
           MOVE 
           "     TO AMEND THE BM OR PRESS 'ESC' TO CONTINUE."
               TO WS-MESSAGE
           PERFORM ERROR-000.

           MOVE "ENTERQTY" TO F-FIELDNAME.
           MOVE 8          TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.

           IF F-EXIT-CH NOT = X"07" AND NOT = X"1C"
              GO TO GET-350.
           IF F-EXIT-CH = X"1C"
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO GET-280.
       GET-400.
            IF WS-NEWORDER = "Y"
               MOVE 1 TO INCR-COPY-NUMBER
            ELSE
               ADD 1  TO INCR-COPY-NUMBER.
            IF WS-MESSAGE NOT = " "
              PERFORM ERROR1-020
              PERFORM ERROR-020.
       GET-999.
           EXIT.
      *
       CALC-POS-OF-CURSOR SECTION.
       CPOC-005.
             IF SUB-1SAVE < 13
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
      *     IF INCR-PRINTED NOT = "P"
            IF WS-SUFFICIENT-STOCK = "Y"
             IF SUB-25 NOT = 1
              IF WS-ABOVE-BODY = " "
               MOVE "THERE IS SUFFICIENT STOCK TO COMPLETE THE KITS."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE "   **  NO ITEMS HAVE BEEN BACK-ORDERED. **"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
            PERFORM ERROR1-020.
            MOVE " " TO WS-ABOVE-BODY.
            MOVE 1 TO SUB-1 SUB-2.
       FILL-005.
            MOVE 2710 TO POS
            DISPLAY
            "PRESS <ALT-Z> TO GO INTO ZOOMBOX MODE TO CALL UP STOCKINQ."
               AT POS.

            PERFORM ERROR-020
            MOVE 3010 TO POS
            DISPLAY "    BODY LINE: " AT POS
            ADD 16 TO POS
            MOVE SUB-1 TO WS-BODY-LINE
            DISPLAY WS-BODY-LINE AT POS.
       FILL-010.
            PERFORM SUBTOTALS
            PERFORM SCROLL-500
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 15 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.

            IF F-EXIT-CH = X"0B" OR = X"0A"
             IF F-NAMEFIELD = SPACES
                GO TO FILL-010.

            IF F-EXIT-CH = X"01" AND F-INDEX = 1
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM READ-FIELD-ALPHA
             IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
              IF WS-MUST-PRINT = "Y"
               MOVE
            "YOU SHOULD PRINT THIS ORDER AS A CHANGE HAS BEEN MADE"
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999
              ELSE
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999.
           IF F-EXIT-CH = X"01" AND F-INDEX = 1
            IF SP-1STCHAR NOT = "/"
             IF F-NAMEFIELD NOT = B-STOCKNUMBER (SUB-1)
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

            MOVE 15          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO SPLIT-STOCK.
            PERFORM FILL-005.
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

            IF F-EXIT-CH = X"01"
             IF B-STOCKNUMBER (SUB-1) = "  "
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-005.

            IF F-EXIT-CH = X"01" AND F-INDEX > 1
             IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-010
             ELSE
              IF SP-1STCHAR NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-010.

            IF F-EXIT-CH = X"0B" AND F-INDEX < 12
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                ADD 1 TO F-INDEX SUB-1
                GO TO FILL-005
              ELSE
               IF SP-1STCHAR NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                ADD 1 TO F-INDEX SUB-1
                GO TO FILL-005.
            IF F-EXIT-CH = X"0B" AND F-INDEX = 12
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

            IF F-EXIT-CH = X"0A" AND F-INDEX < 12
             IF B-STOCKNUMBER (SUB-1) NOT = SPACES
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                ADD 1 TO F-INDEX SUB-1
                GO TO FILL-005
              ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                ADD 1 TO F-INDEX SUB-1
                GO TO FILL-005.
            IF F-EXIT-CH = X"0A" AND F-INDEX = 12
             IF B-STOCKNUMBER (SUB-1) NOT = SPACES
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                PERFORM SCROLL-NEXT
                GO TO FILL-010
              ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
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
      *****************
      * TAB CHARACTER *
      *****************
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
            IF B-NEWLINE (SUB-1) = "L" OR ="Y"
              IF F-EXIT-CH NOT = X"01" AND NOT = X"0B"
                       AND NOT = X"09" AND NOT = X"05"
                       AND NOT = X"11" AND NOT = X"0C"
                   GO TO FILL-010.
                   
      * 'ESC' = X"07"; 'CODE-CANCEL' = X"87"; 'ALT-F10'=X"9F"
            IF F-EXIT-CH = X"07" OR = X"87" OR X"9F"
             IF B-STOCKNUMBER (SUB-1) = "  "
                GO TO FILL-010.
            IF F-EXIT-CH = X"07"
                MOVE "TO DELETE A LINE-ITEM PRESS 'ALT-F10'"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-010.
            IF F-EXIT-CH = X"87" OR = X"9F"
      *       IF WS-NEWORDER = "N"
                MOVE "Y" TO WS-MUST-PRINT
                PERFORM CANCEL-STOCK-TRANS.
            IF F-EXIT-CH = X"87" OR = X"9F"
                MOVE SUB-1 TO SUB-7
                MOVE "Y" TO WS-MUST-PRINT
                PERFORM CANCEL-TRANSACTION
                MOVE 1 TO SUB-1 F-INDEX
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                SUBTRACT 1 FROM SUB-25
              IF SUB-25 > 13
                SUBTRACT 5 FROM SUB-25
                MOVE SUB-25 TO SUB-1
                PERFORM SCROLL-NEXT
                ADD 5 TO SUB-25
                GO TO FILL-010
              ELSE
                GO TO FILL-010.
      **********************************************
      *SECTION TO CHANGE QTY'S OF EXISTING LINE    *
      *F5      = "19"                              *
      *CODE-F5 = "99"                              *
      **********************************************
           IF F-EXIT-CH = X"19" OR = X"99"
            IF B-STOCKNUMBER (SUB-1) = " "
               GO TO FILL-010.
           IF F-EXIT-CH = X"19" OR = X"99"
            IF SP-1STCHAR = "/"
               GO TO FILL-010.
           IF F-EXIT-CH = X"19" OR = X"99"
                PERFORM CHANGE-QTY
                MOVE ST-QTYONHAND    TO WS-KITQTYONHAND
                MOVE ST-QTYONRESERVE TO WS-KITQTYONRESERVE
                MOVE ST-QTYONORDER   TO WS-KITQTYONORDER
                MOVE ST-QTYONBORDER  TO WS-KITQTYONBORDER
                MOVE 2410 TO POS
                DISPLAY WS-ONHAND-LINE AT POS
                
                MOVE "STOCKCOST"    TO F-FIELDNAME
                MOVE 9              TO F-CBFIELDNAME
                MOVE ST-AVERAGECOST TO F-EDNAMEFIELDAMOUNT
                                       B-STOCKCOST (SUB-1)
                MOVE 9 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNT
             IF F-EXIT-CH = X"19"
                GO TO FILL-025
             ELSE
                GO TO FILL-020.

      *
      *<CODE-NEXT-PAGE> TO READ NEXT STOCK ITEM.
            IF F-EXIT-CH = X"8C"
             IF SUB-1 NOT < SUB-25
                PERFORM READ-NEXT-STOCK-ITEM
             IF WS-STOCK-ST1 = 0
                MOVE ST-DESCRIPTION1 TO B-STOCKDESCRIPTION (SUB-1)
                MOVE ST-DESCRIPTION2 TO B-STOCKDESCRIPTION2 (SUB-1)
                PERFORM DISPLAY-LINE-ITEMS
                GO TO FILL-010
             ELSE
                GO TO FILL-010.
            IF F-NAMEFIELD = " "
                GO TO FILL-010.
           MOVE F-NAMEFIELD TO B-STOCKNUMBER (SUB-1).
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.

            IF SP-1STCHAR NOT = "/"
                AND B-STOCKDESCRIPTION (SUB-1) NOT = " "
                AND B-ORDERQTY (SUB-1) NOT = 0
                GO TO FILL-010.
                
            IF SP-1STCHAR NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER
                PERFORM READ-STOCK.
                
            IF WS-STOCK-ST1 NOT = 51
                MOVE ST-QTYONHAND    TO WS-KITQTYONHAND
                MOVE ST-QTYONRESERVE TO WS-KITQTYONRESERVE
                MOVE ST-QTYONORDER   TO WS-KITQTYONORDER
                MOVE ST-QTYONBORDER  TO WS-KITQTYONBORDER
            ELSE
                MOVE 0               TO WS-KITQTYONHAND
                                        WS-KITQTYONRESERVE
                                        WS-KITQTYONORDER
                                        WS-KITQTYONBORDER.
            MOVE 2410 TO POS.
            DISPLAY WS-ONHAND-LINE AT POS.
            IF SP-1STCHAR NOT = "/"
               AND ST-DESCRIPTION1 = " "
                   MOVE "INVALID STOCK ITEM!!!" TO WS-MESSAGE
                   PERFORM ERROR-MESSAGE
                   MOVE " " TO B-STOCKNUMBER (SUB-1)
                   GO TO FILL-005.
            IF SP-1STCHAR = "/"
                MOVE " " TO ST-DESCRIPTION1
                            ST-DESCRIPTION2
                MOVE 0 TO ST-PRICE
                          ST-AVERAGECOST
                          ST-DISCOUNT1
             IF SUB-1 < SUB-25
                MOVE "Y" TO WS-LINECHANGED.
       FILL-0150.
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
                 GO TO FILL-020.
            MOVE "STOCKCOST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-AVERAGECOST TO F-EDNAMEFIELDAMOUNT
                                   B-STOCKCOST (SUB-1).
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE ST-DESCRIPTION1 TO F-NAMEFIELD
                                    B-STOCKDESCRIPTION (SUB-1).
            MOVE ST-DESCRIPTION2 TO B-STOCKDESCRIPTION2 (SUB-1).
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM SCROLL-500.
       FILL-020.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "TOTALQTY" TO F-FIELDNAME.
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
            IF B-ORDERQTY (SUB-1) = 0
                MOVE 2801 TO POS
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-020.
            MOVE B-ORDERQTY (SUB-1) TO F-EDNAMEFIELDQTY.
            PERFORM WRITE-FIELD-QTY.
            IF SP-1STCHAR NOT = "/"
             IF ST-QTYONHAND NOT < B-ORDERQTY (SUB-1)
                MOVE NUMERIC-RATE TO B-SHIPQTY (SUB-1)
            ELSE
               MOVE ST-QTYONHAND  TO B-SHIPQTY (SUB-1).
            IF SP-1STCHAR = "/"
               MOVE NUMERIC-RATE TO B-SHIPQTY (SUB-1).
            GO TO FILL-027.
       FILL-025.
            MOVE "TOTALQTY" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
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
            MOVE "MFGQTY"          TO F-FIELDNAME
            MOVE 6                 TO F-CBFIELDNAME
            MOVE B-SHIPQTY (SUB-1) TO F-EDNAMEFIELDQTY
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY.
       FILL-035.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "MFGQTY" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
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
            MOVE "                      " TO F-NAMEFIELD.
            IF F-EXIT-CH = X"1D" OR = X"0A"
               GO TO FILL-037.
            DISPLAY " " AT 3079 WITH BELL
            GO TO FILL-035.
       FILL-037.
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE.
            IF SIGN-FOUND = 1
                GO TO FILL-035.
            MOVE NUMERIC-RATE TO B-SHIPQTY (SUB-1).
            IF B-ORDERQTY (SUB-1) < B-SHIPQTY (SUB-1)
                MOVE "YOU MAY NOT ENTER MORE TO MNFG THAN ON ORDER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-035.
            MOVE B-SHIPQTY (SUB-1) TO F-EDNAMEFIELDQTY
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY.
       FILL-038.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
              IF SP-1STCHAR = "/"
                GO TO FILL-060.
            MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
            PERFORM READ-STOCK-LOCK.
       FILL-040.
            IF B-SHIPQTY (SUB-1) > ST-QTYONHAND
                REWRITE STOCK-RECORD
                MOVE 3078 TO POS
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
                MOVE "MFGQTY"     TO F-FIELDNAME
                MOVE 0            TO B-SHIPQTY (SUB-1)
                MOVE 6            TO F-CBFIELDNAME
                MOVE ST-QTYONHAND TO F-EDNAMEFIELDQTY
                MOVE 5            TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-QTY
                GO TO FILL-035
            ELSE
                MOVE "TOTALQTY" TO F-FIELDNAME
                MOVE 8          TO F-CBFIELDNAME
                MOVE 5          TO F-CBFIELDLENGTH
                MOVE 0          TO B-ORDERQTY (SUB-1)
                MOVE " "        TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                MOVE "MFGQTY"   TO F-FIELDNAME
                MOVE 0          TO B-SHIPQTY (SUB-1)
                MOVE 6          TO F-CBFIELDNAME
                MOVE 5          TO F-CBFIELDLENGTH
                MOVE " "        TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO FILL-020.

           SUBTRACT B-SHIPQTY (SUB-1) FROM ST-QTYONHAND.
           ADD B-SHIPQTY (SUB-1)        TO ST-QTYONRESERVE.
           IF WS-LINECHANGED = "N" OR = "C"
             ADD B-ORDERQTY (SUB-1) TO ST-QTYONBORDER
           ELSE
             COMPUTE WS-BO-QTY =
                B-ORDERQTY (SUB-1) - B-SHIPPEDQTY (SUB-1)
             ADD WS-BO-QTY TO ST-QTYONBORDER.

           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
       FILL-060.
            IF F-EXIT-CH NOT = X"1D"
                GO TO FILL-090.
            MOVE "STOCKCOST" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
                GO TO FILL-035.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-060.
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD         TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE        TO B-STOCKCOST (SUB-1)
            MOVE B-STOCKCOST (SUB-1) TO F-EDNAMEFIELDAMOUNT
            PERFORM WRITE-FIELD-AMOUNT.
       FILL-090.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF WS-LINECHANGED = "Y" OR = "C"
               PERFORM CHANGE-STOCK-TRANS.
               
            IF WS-LINECHANGED = "N"
               PERFORM WRITE-NEW-TRANS
               ADD 1 TO WS-STTRANS-NO.

            ADD 1 TO SUB-1 F-INDEX.
            IF WS-LINECHANGED = "N"
               MOVE SUB-1 TO SUB-25.
            MOVE "N" TO WS-LINECHANGED.
            MOVE "Y" TO WS-MUST-PRINT.
            IF SUB-1 > 150
                MOVE 150 TO SUB-1 SUB-25
                MOVE "150 LINES ARE UP, PRESS 'ESC' TO TAB."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-999.
            IF F-INDEX < 13
                GO TO FILL-005.
            SUBTRACT 1 FROM SUB-1. 
            PERFORM SCROLL-NEXT.
            GO TO FILL-010.
       FILL-999.
             EXIT.
      *
       FIND-INFO SECTION.
       FIND-010.
            MOVE "REFNO"    TO F-FIELDNAME.
            MOVE 5          TO F-CBFIELDNAME.
            MOVE WS-INVOICE TO F-EDNAMEFIELDNUM.
            MOVE 7          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "KITDATE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE WS-ORDERDATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "KITORD" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            MOVE WS-KITQTY   TO F-EDNAMEFIELDKITQTY.
            MOVE 3        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-KITQTY.

            MOVE "KITSHPD"  TO F-FIELDNAME.
            MOVE 7          TO F-CBFIELDNAME.
            MOVE WS-SHPDQTY TO F-EDNAMEFIELDKITQTY.
            MOVE 3          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-KITQTY.

            MOVE "KITNAME"   TO F-FIELDNAME.
            MOVE 7           TO F-CBFIELDNAME.
            MOVE WS-KIT-SAVE TO F-NAMEFIELD.
            MOVE 15          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE SPACES       TO WS-STDESC
            MOVE WS-KIT-DESC1 TO WS-DESC1
            MOVE WS-KIT-DESC2 TO WS-DESC2.

            MOVE "KT-DESC"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE WS-STDESC    TO F-NAMEFIELD.
            MOVE 40           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "KITPRICE"   TO F-FIELDNAME.
            MOVE 8            TO F-CBFIELDNAME.
            MOVE WS-KIT-PRICE TO F-EDNAMEFIELDAMOUNT.
            MOVE 9            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "KITCOMMENT"   TO F-FIELDNAME.
            MOVE 10             TO F-CBFIELDNAME.
            MOVE WS-KIT-COMMENT TO F-NAMEFIELD.
            MOVE 40             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COPYDESC"             TO F-FIELDNAME
            MOVE 8                      TO F-CBFIELDNAME
            MOVE "   B/M COPY NUMBER :" TO F-NAMEFIELD
            MOVE 20                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COPYNUMBER"     TO F-FIELDNAME
            MOVE 10               TO F-CBFIELDNAME
            MOVE INCR-COPY-NUMBER TO F-NAMEFIELD
            MOVE 2                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            IF SUB-1 NOT > 0
                MOVE 1 TO SUB-1 F-INDEX.
            PERFORM SCROLL-NEXT
            PERFORM SCROLL-PREVIOUS.

       FIND-999.
            EXIT.
      *
       CHANGE-QTY SECTION.
       CQS-010.
           MOVE 2910 TO POS
           DISPLAY "READING ALL B/O'S TO RE-CALC RESERVE..." AT POS.
           
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

      *     IF ST-QTYONRESERVE < ST-QTYONBORDER
              PERFORM READ-ALL-LINE-ITEMS.
           IF WS-STTR-SHIPQTY > ST-QTYONRESERVE
              SUBTRACT WS-BO-QTY FROM ST-QTYONBORDER
              GO TO CQS-020.
           IF WS-STTR-SHIPQTY = ST-QTYONRESERVE
            IF WS-BO-QTY NOT > ST-QTYONBORDER
              SUBTRACT WS-BO-QTY FROM ST-QTYONBORDER
              GO TO CQS-020
            ELSE
              MOVE 0 TO ST-QTYONBORDER
              GO TO CQS-020.
           IF WS-STTR-SHIPQTY < ST-QTYONRESERVE
              SUBTRACT WS-STTR-SHIPQTY FROM ST-QTYONRESERVE.
           IF WS-BO-QTY NOT > ST-QTYONRESERVE
              SUBTRACT WS-BO-QTY FROM ST-QTYONRESERVE
              ADD WS-BO-QTY        TO ST-QTYONHAND
              SUBTRACT WS-BO-QTY FROM ST-QTYONBORDER
              ADD WS-STTR-SHIPQTY TO ST-QTYONRESERVE
              GO TO CQS-020.
           IF WS-BO-QTY > ST-QTYONRESERVE
              ADD ST-QTYONRESERVE  TO ST-QTYONHAND
              MOVE 0               TO ST-QTYONRESERVE
              SUBTRACT WS-BO-QTY FROM ST-QTYONBORDER
              MOVE WS-STTR-SHIPQTY TO ST-QTYONRESERVE.
       CQS-020.
           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
           MOVE "TOTALQTY" TO F-FIELDNAME
           MOVE 8          TO F-CBFIELDNAME
           MOVE 5          TO F-CBFIELDLENGTH.
           IF F-EXIT-CH = X"99"
              MOVE 0       TO B-ORDERQTY (SUB-1)
                              B-SHIPPEDQTY (SUB-1).
           MOVE 0          TO B-SHIPQTY (SUB-1)
           MOVE " "        TO F-NAMEFIELD
           PERFORM WRITE-FIELD-ALPHA
           MOVE "MFGQTY" TO F-FIELDNAME
           MOVE 6        TO F-CBFIELDNAME
           MOVE 5        TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       CQS-030.
           IF F-EXIT-CH = X"99"
              MOVE "C" TO WS-LINECHANGED
           ELSE
              MOVE "Y" TO WS-LINECHANGED.
              
           PERFORM ERROR1-020.
       CQS-999.
           EXIT.
      *
       READ-ALL-LINE-ITEMS SECTION.
       RALI-000.
           MOVE 0 TO WS-STTR-ORDERQTY
                     WS-STTR-SHIPQTY.
           MOVE "N"                   TO STTR-ST-COMPLETE
           MOVE B-STOCKNUMBER (SUB-1) TO STTR-STOCK-NUMBER
           MOVE 0                     TO STTR-ST-DATE.
           START STOCK-TRANS-FILE KEY NOT < STTR-ST-KEY
              INVALID KEY NEXT SENTENCE.
       RALI-010.
           READ STOCK-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              MOVE 0 TO   WS-STTRANS-ST1
              MOVE 0 TO STTR-TYPE
              GO TO RALI-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "STOCK TRANS BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STTRANS-ST1
              GO TO RALI-010.
           IF STTR-STOCK-NUMBER NOT = B-STOCKNUMBER (SUB-1)
              GO TO RALI-999.
           IF STTR-REFERENCE1 = WS-INVOICE
              GO TO RALI-010.
           IF STTR-COMPLETE NOT = "N"
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
       DISPLAY-LINE-ITEMS SECTION.
       DLI-005.
           IF WS-STOCK-ST1 NOT = 0
                GO TO DLI-999.
            MOVE "STOCKNUMBER"  TO F-FIELDNAME.
            MOVE 11             TO F-CBFIELDNAME.
            MOVE ST-STOCKNUMBER TO F-NAMEFIELD
            MOVE 15             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKDESCRIPTION"         TO F-FIELDNAME.
            MOVE 16                         TO F-CBFIELDNAME.
            MOVE B-STOCKDESCRIPTION (SUB-1) TO F-NAMEFIELD.
            MOVE 20                         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            MOVE ST-PRICE     TO F-EDNAMEFIELDAMOUNT.
            MOVE 9            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "STOCKCOST"    TO F-FIELDNAME.
            MOVE 9              TO F-CBFIELDNAME.
            MOVE ST-AVERAGECOST TO F-EDNAMEFIELDAMOUNT.
            MOVE 9              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
       DLI-020.
           MOVE ST-QTYONHAND    TO WS-KITQTYONHAND
           MOVE ST-QTYONRESERVE TO WS-KITQTYONRESERVE
           MOVE ST-QTYONORDER   TO WS-KITQTYONORDER
           MOVE ST-QTYONBORDER  TO WS-KITQTYONBORDER
           MOVE 2410 TO POS
           DISPLAY WS-ONHAND-LINE AT POS.
           
           PERFORM SCROLL-500.
       DLI-999.
           EXIT.
      *
       CANCEL-STOCK-TRANS SECTION.
       CAN-TRANS-000.
           IF B-NEWLINE (SUB-1) = " "
               GO TO CAN-TRANS-999.
           MOVE WS-INVOICE        TO STTR-REFERENCE1.
           MOVE 7                 TO STTR-TYPE.
           MOVE B-STTRANS (SUB-1) TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY.
           READ STOCK-TRANS-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "CAN-TRANS-000 ST1 NOT = 0, RE-READING IN 1 SECOND." 
              TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              GO TO CAN-TRANS-000.
       CAN-TRANS-002.
            DELETE STOCK-TRANS-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "CAN-TRANS-002 ST1 NOT = 0, RE-DELETE IN 1 SECOND."
               TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              GO TO CAN-TRANS-002.
       CAN-TRANS-999.
            EXIT.
      *
       CHANGE-STOCK-TRANS SECTION.
       RES-ST-000.
           IF B-NEWLINE (SUB-1) = " "
               GO TO RES-ST-999.
           MOVE WS-INVOICE        TO STTR-REFERENCE1.
           MOVE 7                 TO STTR-TYPE.
           MOVE B-STTRANS (SUB-1) TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY.
           READ STOCK-TRANS-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "RES-ST-000 ST1 NOT = 0, RE-READ IN 1 SECOND."
               TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              GO TO RES-ST-000.
       RES-ST-002.
           IF F-NAMEFIELDRED1 = "D"
            IF STTR-SHIPQTY > B-SHIPQTY (SUB-1)
               MOVE WS-DATE                 TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE DISPLAY-DATE            TO WS-DAILY-1ST1
               MOVE "BM#"                   TO WS-DAILY-1ST2
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
           IF WS-LINECHANGED = "C"
               MOVE 0                       TO STTR-SHIPPEDQTY.
           MOVE B-STOCKPRICE (SUB-1)        TO STTR-PRICE
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO STTR-DESC1
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO STTR-DESC2
           MOVE B-STOCKCOST (SUB-1)         TO STTR-COST-VALUE.
           REWRITE STOCK-TRANS-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "BAD REWRITE ON CHANGE-ST-TRANS, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RES-ST-002.
       RES-ST-999.
            EXIT.
      *
       READ-TOOLKIT SECTION.
       RTK-010.
             MOVE 1   TO SUB-1 SUB-25
             MOVE "Y" TO WS-SUFFICIENT-STOCK
             MOVE ST-STOCKNUMBER TO TO-TOOLKIT-NUMBER
             MOVE " "            TO TO-COMPONENT-NUMBER.
             START TOOLKITS KEY NOT < TO-KEY
                 INVALID KEY NEXT SENTENCE.
             IF WS-KIT-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-KIT-ST1
                MOVE "N" TO WS-SUFFICIENT-STOCK
                GO TO RTK-900.
             MOVE 2910 TO POS
             DISPLAY "Reading Toolkit Items, Be patient....." AT POS.
       RTK-015.
           READ TOOLKITS NEXT
              AT END NEXT SENTENCE.
           IF WS-KIT-ST1 = 10
              MOVE 0 TO WS-KIT-ST1
              GO TO RTK-900.
           IF TO-COMPONENT-NUMBER = "  "
              GO TO RTK-015.
           IF WS-KIT-ST1 NOT = 0
              MOVE 0 TO WS-KIT-ST1
              MOVE "TOOLKITS BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RTK-015.
           IF TO-TOOLKIT-NUMBER NOT = WS-KIT-SAVE
              GO TO RTK-900.
           MOVE TO-COMPONENT-NUMBER TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           IF ST-DESCRIPTION1 = " "
              MOVE WS-STOCKNUMBER TO WS-MESSAGE
              PERFORM ERROR-000
              MOVE "NOT FOUND IN STOCK-MASTER FILE" TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              GO TO RTK-015.
           MOVE ST-STOCKNUMBER      TO B-STOCKNUMBER (SUB-1)
           MOVE ST-DESCRIPTION1     TO B-STOCKDESCRIPTION (SUB-1)
           MOVE ST-DESCRIPTION2     TO B-STOCKDESCRIPTION2 (SUB-1)
           MOVE ST-PRICE            TO B-STOCKPRICE (SUB-1)
           MOVE ST-AVERAGECOST      TO B-STOCKCOST (SUB-1)
           MOVE ST-UNITOFMEASURE    TO B-UNIT (SUB-1).
           MOVE " "                 TO B-NEWLINE (SUB-1).
           COMPUTE WS-ORDERQTY =    TO-QUANTITY * WS-KITQTY
           COMPUTE WS-TOTALRESQTY = TO-QUANTITY * WS-RESQTY
           MOVE WS-ORDERQTY         TO B-ORDERQTY (SUB-1)
           ADD WS-ORDERQTY          TO ST-QTYONBORDER.
           IF WS-TOTALRESQTY NOT > ST-QTYONHAND
              SUBTRACT WS-TOTALRESQTY FROM ST-QTYONHAND
              ADD WS-TOTALRESQTY     TO ST-QTYONRESERVE
              MOVE WS-TOTALRESQTY    TO B-SHIPQTY (SUB-1)
           ELSE
              MOVE "N"               TO WS-SUFFICIENT-STOCK
              MOVE ST-QTYONHAND      TO B-SHIPQTY (SUB-1)
              ADD ST-QTYONHAND       TO ST-QTYONRESERVE
              MOVE 0                 TO ST-QTYONHAND.
       RTK-050.
           REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
       RTK-060.
           PERFORM WNT-TRANS-002 THRU WNT-TRANS-800.

           MOVE 2610 TO POS
           DISPLAY "UPDATING LINE #:" AT POS
           ADD 17 TO POS
           MOVE SUB-1 TO WS-BODY-LINE
           DISPLAY WS-BODY-LINE AT POS.

           ADD 1 TO SUB-1 WS-STTRANS-NO.
           IF SUB-1 > SUB-25
              MOVE SUB-1 TO SUB-25.
           IF SUB-1 < 150
              GO TO RTK-015.
           MOVE "THIS KIT HAS MORE THAN 150 ITEMS, CAN'T COMPLETE."
              TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
       RTK-900.
           PERFORM ERROR1-020.
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
       RTK-999.
           EXIT.
      *
       WRITE-NEW-TRANS SECTION.
       WNT-TRANS-000.
           IF B-NEWLINE (SUB-1) NOT = " "
               GO TO WNT-TRANS-999.
           COMPUTE SUB-2 = SUB-1 - 1.
           IF SUB-2 = 0
              MOVE 1 TO SUB-2.
           MOVE B-STTRANS (SUB-2) TO WS-STTRANS-NO
           ADD 1                  TO WS-STTRANS-NO.
           IF SUB-1 > WS-STTRANS-NO
               MOVE SUB-1 TO WS-STTRANS-NO.
       WNT-TRANS-002.
           IF WS-STTRANS-NO = 0
              ADD 1 TO WS-STTRANS-NO.
           MOVE WS-INVOICE                  TO STTR-REFERENCE1
           MOVE 7                           TO STTR-TYPE.
           MOVE WS-STTRANS-NO               TO STTR-TRANSACTION-NUMBER
                                               B-STTRANS (SUB-1)
           MOVE B-STOCKNUMBER (SUB-1)       TO SPLIT-STOCK
                                               STTR-STOCK-NUMBER
           MOVE 7777777                     TO STTR-ACCOUNT-NUMBER
           MOVE "N"                         TO STTR-COMPLETE
                                               STTR-ST-COMPLETE
                                               STTR-AC-COMPLETE
                                               B-NEWLINE (SUB-1)
           MOVE 0                           TO STTR-INV-NO
                                               STTR-SALES-VALUE
                                               STTR-ITEMDISC
           MOVE WS-INVOICEDATE              TO STTR-DATE
                                               STTR-AC-DATE
                                               STTR-ST-DATE.
           MOVE B-ORDERQTY (SUB-1)          TO STTR-ORDERQTY
           MOVE B-SHIPQTY (SUB-1)           TO STTR-SHIPQTY
           MOVE B-SHIPPEDQTY (SUB-1)        TO STTR-SHIPPEDQTY
           MOVE B-STOCKPRICE (SUB-1)        TO STTR-PRICE
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO STTR-DESC1
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO STTR-DESC2
           MOVE B-STOCKCOST (SUB-1)         TO STTR-COST-VALUE
           MOVE "Y"                         TO STTR-TAX
           MOVE B-UNIT (SUB-1)              TO STTR-UNIT.
       WNT-TRANS-800.
           WRITE STOCK-TRANS-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "WRITE ERROR AT WNT-TRANS-800, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE STTR-TRANSACTION-NUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              ADD 1 TO WS-STTRANS-NO STTR-TRANSACTION-NUMBER
              GO TO WNT-TRANS-800.
           IF STTR-TRANSACTION-NUMBER = 0
              MOVE "ST-TRANS-NO = 0 AT WNT-TRANS-800, ADVISE THE BOSS."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       WNT-TRANS-999.
            EXIT.
      *
       REVERSE-ENGINEER SECTION.
       REV-ENG-001.
            MOVE "KITNAME"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 15          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-STOCKNUMBER.
            PERFORM READ-STOCK-LOCK.
            MOVE WS-STOCKNUMBER TO WS-KIT-SAVE.
            IF ST-DESCRIPTION1 = "   "
               MOVE "INVALID TOOLKIT ENTRY, PRESS 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO REV-ENG-001.
            IF WS-KITQTY > ST-QTYONHAND
               MOVE
              "YOU MAY NOT REVERSE ENGINEER MORE KITS THAN ARE ON HAND."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE "      PRESS 'ESC' TO EXIT AND RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO REV-ENG-999.
               
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1 WS-KIT-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2 WS-KIT-DESC2.
            
            MOVE "KT-DESC"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "KITPRICE" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE ST-PRICE   TO F-EDNAMEFIELDAMOUNT WS-KIT-PRICE.
            MOVE 9          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
       REV-ENG-002.
            PERFORM ERROR1-020
            PERFORM CLEAR-010
            MOVE 2910 TO POS
            DISPLAY "DO YOU WISH TO CONTINUE : Y/N     [ ]" AT POS
            ADD 35 TO POS

           MOVE 'N'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 44        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DIS.

            IF WS-DIS NOT = "N" AND NOT = "Y"
               MOVE "ANSWER MUST BE Y OR N, RE-ENTER" TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO REV-ENG-002.
            IF WS-DIS = "N"
               UNLOCK STOCK-MASTER
               GO TO REV-ENG-999.
       REV-ENG-003.
             MOVE 0 TO SUB-1.
             SUBTRACT WS-KITQTY FROM ST-QTYONHAND
             SUBTRACT WS-KITQTY FROM ST-QTYRECMTD
             SUBTRACT WS-KITQTY FROM ST-QTYRECYTD.
             PERFORM REV-ENG-050.
             PERFORM WRITE-REV-STOCK-RECEIPT.
       REV-ENG-010.
             IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
             MOVE ST-STOCKNUMBER TO TO-TOOLKIT-NUMBER.
             MOVE " "            TO TO-COMPONENT-NUMBER.
             START TOOLKITS KEY NOT < TO-KEY
                 INVALID KEY NEXT SENTENCE.
             IF WS-KIT-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-KIT-ST1
                GO TO REV-ENG-900.
             MOVE 2910 TO POS.
             DISPLAY "Reverse Engineering of above Bill Of Material..."
                   AT POS.
       REV-ENG-015.
             READ TOOLKITS NEXT
                AT END NEXT SENTENCE.
             IF WS-KIT-ST1 = 10
                MOVE 0 TO WS-KIT-ST1
                GO TO REV-ENG-900.
             IF TO-COMPONENT-NUMBER = "  "
                GO TO REV-ENG-015.
             IF WS-KIT-ST1 NOT = 0
                MOVE 0 TO WS-KIT-ST1
                MOVE "TOOLKITS BUSY AT REV-ENG-015, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO REV-ENG-015.
             IF TO-TOOLKIT-NUMBER NOT = WS-KIT-SAVE
                GO TO REV-ENG-900.
             MOVE TO-COMPONENT-NUMBER TO WS-STOCKNUMBER.
             PERFORM READ-STOCK-LOCK.
             IF WS-ERR = "ERR"
                MOVE WS-STOCKNUMBER TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "NOT FOUND IN STOCK-MASTER FILE" TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                GO TO REV-ENG-015.
             COMPUTE WS-ORDERQTY = TO-QUANTITY * WS-KITQTY
             ADD WS-ORDERQTY        TO ST-QTYONHAND.
             SUBTRACT WS-ORDERQTY FROM ST-SALESUNITMTD
             SUBTRACT WS-ORDERQTY FROM ST-SALESUNITSYTD.
       REV-ENG-050.
             REWRITE STOCK-RECORD
                INVALID KEY NEXT SENTENCE.
       REV-ENG-060.
           ADD 1 TO SUB-1
           MOVE 2610 TO POS
           DISPLAY "UPDATING LINE #:" AT POS
           ADD 17 TO POS
           MOVE SUB-1 TO WS-BODY-LINE
           DISPLAY WS-BODY-LINE AT POS.

             GO TO REV-ENG-015.
       REV-ENG-900.
             PERFORM ERROR1-020.
       REV-ENG-999.
             EXIT.
      *
       RESERVE-STOCK SECTION.
       RES-ST-010.
             IF WS-RESQTY = 0
                 GO TO RES-ST-950.
             MOVE 1    TO SUB-1
             MOVE "Y"  TO WS-SUFFICIENT-STOCK
             MOVE 2910 TO POS
             DISPLAY "Reserving Stock For Qty Entered......." AT POS.
       RES-ST-015.
           IF B-STOCKNUMBER (SUB-1) = " "
              GO TO RES-ST-950.
           MOVE 2610 TO POS
           DISPLAY "Reading All B-Orders for Line     Of     Lines."
               AT POS
           MOVE SUB-1 TO WS-BODY-LINE
           ADD 30 TO POS
           DISPLAY WS-BODY-LINE AT POS
           MOVE SUB-25 TO WS-BODY-LINE
           ADD 7 TO POS
           DISPLAY WS-BODY-LINE AT POS.

           IF B-ORDERQTY (SUB-1) =
               B-SHIPQTY (SUB-1) + B-SHIPPEDQTY (SUB-1)
               GO TO RES-ST-900.
           MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           IF WS-ERR = "ERR"
              MOVE WS-STOCKNUMBER TO WS-MESSAGE
              PERFORM ERROR-000
              MOVE "NOT FOUND IN STOCK-MASTER FILE" TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              MOVE SUB-1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RES-ST-060.
           MOVE ST-PRICE         TO B-STOCKPRICE (SUB-1).
           MOVE ST-AVERAGECOST   TO B-STOCKCOST (SUB-1).
           MOVE ST-UNITOFMEASURE TO B-UNIT (SUB-1).

           COMPUTE WS-TOTALRESQTY =
              (B-ORDERQTY (SUB-1) / WS-KITQTY) * WS-RESQTY.
           COMPUTE WS-BO-QTY =
               B-ORDERQTY (SUB-1) - B-SHIPPEDQTY (SUB-1).
           IF WS-TOTALRESQTY > WS-BO-QTY
                 MOVE WS-BO-QTY TO WS-TOTALRESQTY.

           PERFORM READ-ALL-LINE-ITEMS.

           ADD B-SHIPQTY (SUB-1)        TO ST-QTYONHAND
           SUBTRACT B-SHIPQTY (SUB-1) FROM ST-QTYONRESERVE
           MOVE 0                        TO B-SHIPQTY (SUB-1).

           IF WS-STTR-SHIPQTY < ST-QTYONRESERVE
              COMPUTE WS-TEMPQTY = ST-QTYONRESERVE - WS-STTR-SHIPQTY
              SUBTRACT WS-TEMPQTY FROM ST-QTYONRESERVE
              ADD WS-TEMPQTY TO ST-QTYONHAND.

           IF WS-TOTALRESQTY NOT > ST-QTYONHAND
              SUBTRACT WS-TOTALRESQTY FROM ST-QTYONHAND
                                           WS-TEMPQTY
              ADD WS-TOTALRESQTY     TO ST-QTYONRESERVE
              MOVE WS-TOTALRESQTY    TO B-SHIPQTY (SUB-1)
           ELSE
              MOVE "N"               TO WS-SUFFICIENT-STOCK
              MOVE ST-QTYONHAND      TO B-SHIPQTY (SUB-1)
              ADD ST-QTYONHAND       TO ST-QTYONRESERVE
              MOVE 0                 TO ST-QTYONHAND
                                        WS-TEMPQTY.
           IF ST-QTYONHAND = 0
              GO TO RES-ST-050.
           ADD WS-BO-QTY         TO WS-STTR-ORDERQTY
           ADD B-SHIPQTY (SUB-1) TO WS-STTR-SHIPQTY
           COMPUTE WS-TEMPQTY = WS-STTR-ORDERQTY - WS-STTR-SHIPQTY.
           IF WS-TEMPQTY > 0
            IF WS-TEMPQTY NOT > ST-QTYONHAND
                SUBTRACT WS-TEMPQTY FROM ST-QTYONHAND
                ADD WS-TEMPQTY        TO ST-QTYONRESERVE
            ELSE
                ADD ST-QTYONHAND      TO ST-QTYONRESERVE
                MOVE 0                TO ST-QTYONHAND.
       RES-ST-050.
           REWRITE STOCK-RECORD
                INVALID KEY NEXT SENTENCE.
       RES-ST-060.
           MOVE WS-INVOICE        TO STTR-REFERENCE1
           MOVE 7                 TO STTR-TYPE
           MOVE B-STTRANS (SUB-1) TO STTR-TRANSACTION-NUMBER.
           READ STOCK-TRANS-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE 0 TO WS-STTRANS-ST1
              MOVE 
              "REWRITING ST-TRANS ERROR, RES-ST-060, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE B-STOCKNUMBER (SUB-1) TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RES-ST-900.
       RES-ST-065.
           MOVE B-ORDERQTY (SUB-1)          TO STTR-ORDERQTY
           MOVE B-SHIPPEDQTY (SUB-1)        TO STTR-SHIPPEDQTY
           MOVE B-SHIPQTY (SUB-1)           TO STTR-SHIPQTY
           MOVE B-STOCKPRICE (SUB-1)        TO STTR-PRICE
           MOVE B-STOCKCOST (SUB-1)         TO STTR-COST-VALUE.
           REWRITE STOCK-TRANS-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "BAD REWRITE ON CHANGE-ST-TRANS, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RES-ST-065.
       RES-ST-900.
             ADD 1 TO SUB-1.
             IF SUB-1 > SUB-25
                MOVE SUB-1 TO SUB-25.
             IF SUB-1 < 150
                GO TO RES-ST-015.
       RES-ST-950.
             MOVE "Y" TO WS-MUST-PRINT
             PERFORM ERROR1-020
             MOVE 2610 TO POS
             DISPLAY WS-MESSAGE AT POS.
       RES-ST-999.
             EXIT.
      *
       SUBTOTALS SECTION.
       CT-000.
           MOVE SUB-1 TO SUB-2
           MOVE 1     TO SUB-1
           MOVE 0     TO WS-PRICES WS-COSTS.
       CT-010.
           IF B-STOCKNUMBER (SUB-1) = " "
                 GO TO CT-900.
           COMPUTE WS-PRICES = WS-PRICES +
               (B-STOCKPRICE (SUB-1) * (B-ORDERQTY (SUB-1) / WS-KITQTY)).
           COMPUTE WS-COSTS = WS-COSTS +
               (B-STOCKCOST (SUB-1) * (B-ORDERQTY (SUB-1) / WS-KITQTY)).
       CT-015.
           ADD 1 TO SUB-1.
           IF SUB-1 < 150
               GO TO CT-010.
       CT-900.
           PERFORM GET-260.
           MOVE SUB-2 TO SUB-1.
       CT-999.
           EXIT.
      *
       CHECK-ORDER-COMPLETE SECTION.
       COC-010.
           MOVE 0 TO SUB-1.
           
      *     MOVE WS-KITQTY TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE WS-SHPDQTY TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR1-020
      *     MOVE WS-MFGQTY TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
           IF WS-KITQTY = WS-SHPDQTY + WS-MFGQTY
              MOVE "Y" TO WS-ORDER-COMPLETE
              GO TO COC-015
           ELSE
              MOVE "N" TO WS-ORDER-COMPLETE
              GO TO COC-030.
       COC-015.
           ADD 1 TO SUB-1.
           IF B-STOCKNUMBER (SUB-1) = "    "
              GO TO COC-030.
           IF WS-TYPE-OF-FINISH = "1"
            IF B-ORDERQTY (SUB-1) =
              B-SHIPQTY (SUB-1) + B-SHIPPEDQTY (SUB-1)
              GO TO COC-020.
           IF B-NEWLINE (SUB-1) NOT = "Y" AND NOT = "L"
              MOVE "N" TO WS-ORDER-COMPLETE
              GO TO COC-030.
       COC-020.
           IF SUB-1 < 150
              GO TO COC-015.
       COC-030.
           MOVE 0 TO SUB-1.
      *     IF WS-TYPE-OF-FINISH = "2"
      *        GO TO COC-999.
       COC-040.
           ADD 1 TO SUB-1.
           IF B-STOCKNUMBER (SUB-1) = " "
              GO TO COC-999.
           IF B-NEWLINE (SUB-1) = "Y" OR = "L"
               GO TO COC-045.
           COMPUTE B-MFGQTY (SUB-1) ROUNDED =
                ((B-ORDERQTY (SUB-1) / WS-KITQTY) * WS-MFGQTY).
           IF B-MFGQTY (SUB-1) > B-SHIPQTY (SUB-1)
                MOVE B-SHIPQTY (SUB-1) TO B-MFGQTY (SUB-1).
           IF INCR-KITSHPDQTY = INCR-KITQTY
                MOVE B-SHIPQTY (SUB-1) TO B-MFGQTY (SUB-1).
       COC-045.
           IF SUB-1 < 150
              GO TO COC-040.
       COC-999.
           EXIT.
      *
       REWRITE-ORDER-TRANS SECTION.
       ROT-050.
            IF WS-NEWORDER = "Y"
                MOVE WS-INVOICE     TO INCR-INVOICE
                MOVE WS-DATE        TO INCR-DATE
            ELSE
                MOVE WS-ORDER       TO INCR-INVOICE.
       ROT-051.
            IF WS-TYPE-OF-FINISH = "1"
             IF WS-ORDER-COMPLETE = "Y"
              IF WS-KITQTY = WS-SHPDQTY + WS-MFGQTY
               MOVE "Y"             TO INCR-PRINTED
               GO TO ROT-055
             ELSE
               MOVE "P"             TO INCR-PRINTED
               GO TO ROT-055.

            IF INCR-PRINTED NOT = "P"
               MOVE "N"             TO INCR-PRINTED.
       ROT-055.
            MOVE 7                  TO INCR-TRANS
            MOVE WS-KIT-COMMENT     TO INCR-KITCOMMENT
            MOVE SUB-25             TO INCR-LINENO
            MOVE 7777777            TO INCR-ACCOUNT
            MOVE INCR-INVOICE       TO INCR-PORDER
            MOVE WS-KITQTY          TO INCR-KITQTY.
            
            IF INCR-KITSHPDQTY NOT > 0
            MOVE 0                  TO INCR-KITSHPDQTY.
            IF WS-TYPE-OF-FINISH = "1"
               ADD WS-MFGQTY        TO INCR-KITSHPDQTY.
               
            IF WS-TYPE-OF-FINISH = "2"
             IF INCR-KITSHPDQTY NOT > 0
               MOVE 0               TO INCR-KITSHPDQTY.
            MOVE WS-KIT-SAVE        TO INCR-KITNAME
            MOVE WS-KIT-PRICE       TO INCR-KITPRICE
            MOVE WS-KIT-DESC1       TO INCR-KITDESC1
            MOVE WS-KIT-DESC2       TO INCR-KITDESC2.
            MOVE " "                TO INCR-PULLBY
            MOVE 0                  TO INCR-PULL-DATE
                                       INCR-PULL-TIME.
            MOVE "Y"                TO INCR-PART-ORDERS.
       ROT-060.
            IF WS-NEWORDER = "Y"
              WRITE INCR-REC
                  INVALID KEY NEXT SENTENCE
            ELSE
              REWRITE INCR-REC
                  INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 NOT = 0
              IF WS-NEWORDER = "N"
                MOVE 0 TO WS-INCR-ST1
                MOVE "ORDER-REG BUSY - ON REWRITE, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO ROT-060.
       ROT-999.
              EXIT.
      *
       READ-INVOICE-REGISTER SECTION.
           MOVE WS-INVOICE TO INCR-INVOICE.
           MOVE 7          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
       RIR-005.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-INCR-ST1
               MOVE "Y" TO WS-NEWORDER
               MOVE "THIS KIT ORDER CANNOT BE FOUND, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RIR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE 0 TO WS-INCR-ST1
               MOVE "INV/CR. REGISTER BUSY, RIR-005, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RIR-005.
           IF INCR-PRINTED = "Y" OR = "L"
               MOVE "THIS KIT ORDER IS COMPLETE." TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "C" TO WS-NEWORDER
               GO TO RIR-999.
       RIR-010.
           MOVE INCR-INVOICE     TO WS-ORDER.
           MOVE INCR-DATE        TO WS-ORDERDATE.
           MOVE INCR-KITNAME     TO WS-KIT-SAVE.
           MOVE INCR-KITQTY      TO WS-KITQTY.
           MOVE 0                TO WS-MFGQTY.
           IF INCR-KITSHPDQTY > 0
             MOVE INCR-KITSHPDQTY  TO WS-SHPDQTY
           ELSE
             MOVE 0              TO WS-SHPDQTY.  
           MOVE INCR-KITPRICE    TO WS-KIT-PRICE.
           MOVE INCR-KITDESC1    TO WS-KIT-DESC1.
           MOVE INCR-KITDESC2    TO WS-KIT-DESC2. 
           MOVE INCR-KITCOMMENT  TO WS-KIT-COMMENT.

           MOVE "N" TO WS-NEWORDER.
       RIR-999.
           EXIT.
      *
       DELETE-INVOICE-REGISTER SECTION.
       DIR-010.
           IF INCR-INVOICE = 0
              GO TO DIR-999.
           MOVE 2910 TO POS.
           DISPLAY "DELETING HEADER DETAILS....." AT POS.
           DELETE INCR-REGISTER
              INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER ST1 NOT =0 ON DELETION, 'ESC' TO RE-TRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO DIR-010.
       DIR-999.
           EXIT.
      *
       DELETE-STOCK-TRANS SECTION.
       DST-000.
           MOVE 2910 TO POS.
           DISPLAY "DELETING ST-TRANS FILES....." AT POS.
           MOVE 1 TO SUB-1.
           MOVE WS-INVOICE        TO STTR-REFERENCE1.
           MOVE 7                 TO STTR-TYPE.
           MOVE 1                 TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "BAD START ON ST-TRANS." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO DST-999.
       DST-010.
           READ STOCK-TRANS-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10 OR = 91
              MOVE 0 TO STTR-TYPE
              GO TO DST-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS RECORD ERROR ON DST-010, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO DST-010.
           IF STTR-REFERENCE1 NOT = WS-INVOICE
              GO TO DST-999.
           IF STTR-COMPLETE = "Y" OR = "L"
               GO TO DST-900.
           MOVE STTR-STOCK-NUMBER TO B-STOCKNUMBER (SUB-1)
                                     SPLIT-STOCK.
           IF SP-1STCHAR = "/"
              GO TO DST-900.
           MOVE STTR-SHIPQTY          TO B-SHIPQTY (SUB-1).
           MOVE STTR-ORDERQTY         TO B-ORDERQTY (SUB-1).
           MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           ADD B-SHIPQTY (SUB-1)         TO ST-QTYONHAND.
           SUBTRACT B-SHIPQTY (SUB-1)  FROM ST-QTYONRESERVE.
           SUBTRACT B-ORDERQTY (SUB-1) FROM ST-QTYONBORDER.
           REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
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
           MOVE "Y"        TO WS-SUFFICIENT-STOCK
           MOVE 1          TO SUB-1 SUB-25
           MOVE WS-INVOICE TO STTR-REFERENCE1
           MOVE 7          TO STTR-TYPE
           MOVE 1          TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "BAD START ON ST-TRANS, 'ESC' TO SEE ERC."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSTT-999.
       RSTT-010.
           READ STOCK-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10 OR = 91
              MOVE 0 TO STTR-TYPE
              GO TO RSTT-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS RECORD ERROR ON RSTT-010, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RSTT-010.
           IF STTR-REFERENCE1 NOT = WS-INVOICE
              GO TO RSTT-999.
           IF STTR-TYPE NOT = 7
              GO TO RSTT-010.
           MOVE STTR-TRANSACTION-NUMBER TO B-STTRANS (SUB-1).
           MOVE STTR-STOCK-NUMBER TO B-STOCKNUMBER (SUB-1)
                                     SPLIT-STOCK.
           IF STTR-COMPLETE = " "
               MOVE "N"           TO B-NEWLINE (SUB-1)
           ELSE
               MOVE STTR-COMPLETE TO B-NEWLINE (SUB-1).
           MOVE STTR-DESC1        TO B-STOCKDESCRIPTION (SUB-1)
           MOVE STTR-DESC2        TO B-STOCKDESCRIPTION2 (SUB-1)
           MOVE STTR-ORDERQTY     TO B-ORDERQTY (SUB-1)
           MOVE STTR-SHIPQTY      TO B-SHIPQTY (SUB-1)
           MOVE 0                 TO B-MFGQTY (SUB-1)
           MOVE STTR-SHIPPEDQTY   TO B-SHIPPEDQTY (SUB-1)
           MOVE STTR-PRICE        TO B-STOCKPRICE (SUB-1)
           MOVE STTR-COST-VALUE   TO B-STOCKCOST (SUB-1).
           IF STTR-ORDERQTY NOT = STTR-SHIPQTY + STTR-SHIPPEDQTY
               MOVE "N" TO WS-SUFFICIENT-STOCK.
       RSTT-050.
           ADD 1 TO SUB-1.
           MOVE SUB-1 TO SUB-25.
           IF SUB-1 > 150
              GO TO RSTT-999.
           GO TO RSTT-010.
       RSTT-999.
           EXIT.
      *
       REWRITE-STOCK-TRANSACTIONS SECTION.
       WST-00000.
            MOVE " "        TO DATA-FIELDS
            MOVE 0          TO WS-STTRANS-NO
            MOVE 1          TO SUB-1
            MOVE WS-INVOICE TO STTR-REFERENCE1
            MOVE 7          TO STTR-TYPE.
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
           ADD 1              TO WS-STTRANS-NO
           MOVE WS-STTRANS-NO TO STTR-TRANSACTION-NUMBER
           GO TO WST-006.
       WST-005.
           READ STOCK-TRANS-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
       WST-006.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
           MOVE B-STOCKNUMBER (SUB-1) TO STTR-STOCK-NUMBER
           MOVE WS-INVOICEDATE        TO STTR-DATE
                                         STTR-AC-DATE
                                         STTR-ST-DATE
           MOVE 7777777               TO STTR-ACCOUNT-NUMBER.
       WST-010.
      *************************
      * 'F5' =X"19"   TYPE=1  *
      * 'F10'=X"1F"   TYPE=2  *
      *************************
            IF B-NEWLINE (SUB-1) = "L"
               GO TO WST-018.
            IF WS-TYPE-OF-FINISH = "1"
               MOVE B-ORDERQTY (SUB-1) TO STTR-ORDERQTY
             IF B-ORDERQTY (SUB-1) =
                 B-MFGQTY (SUB-1) + B-SHIPPEDQTY (SUB-1)
               MOVE "L"                TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE
               MOVE 0                  TO STTR-SHIPQTY
               MOVE B-ORDERQTY (SUB-1) TO STTR-SHIPPEDQTY
               GO TO WST-015
             ELSE
               MOVE "N"                TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE
               COMPUTE STTR-SHIPPEDQTY =
                   STTR-SHIPPEDQTY + B-MFGQTY (SUB-1)
               COMPUTE STTR-SHIPQTY =
                   B-SHIPQTY (SUB-1) - B-MFGQTY (SUB-1)
               GO TO WST-015.
            MOVE "N"                   TO STTR-COMPLETE
                                          STTR-ST-COMPLETE
                                          STTR-AC-COMPLETE.
            MOVE B-ORDERQTY (SUB-1)    TO STTR-ORDERQTY
            MOVE B-SHIPQTY (SUB-1)     TO STTR-SHIPQTY
            MOVE B-SHIPPEDQTY (SUB-1)  TO STTR-SHIPPEDQTY.
       WST-015.
           MOVE B-STOCKPRICE (SUB-1)        TO STTR-PRICE
           MOVE B-STOCKDESCRIPTION (SUB-1)  TO STTR-DESC1
           MOVE B-STOCKDESCRIPTION2 (SUB-1) TO STTR-DESC2
           MOVE B-STOCKCOST (SUB-1)         TO STTR-COST-VALUE.
       WST-018.
            IF WS-ORDER-COMPLETE = "Y"
               MOVE "Y" TO STTR-COMPLETE
                           STTR-ST-COMPLETE
                           STTR-AC-COMPLETE.
            IF B-NEWLINE (SUB-1) = " "
               WRITE STOCK-TRANS-REC
                  INVALID KEY NEXT SENTENCE
            ELSE
               REWRITE STOCK-TRANS-REC
                  INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 = 0
                GO TO WST-020.

            CALL "C$SLEEP" USING 1
            GO TO WST-000.
       WST-020.
            ADD 1 TO SUB-1.
            IF B-STOCKNUMBER (SUB-1) = " "
                GO TO WST-999.
            IF SUB-1 < 150
                MOVE " " TO DATA-FIELDS
                GO TO WST-000.
       WST-999.
            EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
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
               MOVE 
               "STOCK RECORD BUSY ON READ R-ST-005, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-005.
           PERFORM ERROR-020.
           IF ST-ANALYSIS = "D"
               MOVE
            "DON'T ENTER ORDER, ITEM TO BE DELETED WHEN ON HAND = ZERO."
                   TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
       R-ST-999.
           EXIT.
      *
       READ-STOCK-QTY SECTION.
       R-ST-Q-000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE 0  TO ST-QTYONHAND
                          ST-QTYONRESERVE
               MOVE "*" TO D-BLANK 
               GO TO R-ST-Q-999.
       R-ST-Q-005.
           READ STOCK-MASTER
               INVALID KEY 
               MOVE "ERR" TO WS-ERR
               NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE 0  TO ST-QTYONHAND
                          ST-QTYONRESERVE
               MOVE "*" TO D-BLANK 
               GO TO R-ST-Q-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY IN READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-Q-005.
           PERFORM ERROR-020.
       R-ST-Q-999.
           EXIT.
      *
       READ-STOCK-LOCK SECTION.
       R-STL-000.
           MOVE "   " TO WS-ERR.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
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
               MOVE "ERR" TO WS-ERR
               GO TO R-STL-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY IN READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-STL-005.
           PERFORM ERROR-020.
       R-STL-999.
           EXIT.
      *
       WRITE-SUPPLIER-ORDER SECTION.
       RSQ-000.
            START OUTSTANDING-ORDERS KEY NOT < OO-KEY.
       RSQ-002.
            MOVE WS-INVOICE     TO OO-ORDER-NUMBER.
            MOVE WS-KIT-SAVE    TO OO-STOCK-NUMBER.
            MOVE WS-KITQTY         TO OO-QUANTITY
                                   OO-ORIG-QTY.
            MOVE WS-DATE        TO OO-ORDERDATE OO-DUEDATE.
            MOVE 4              TO OO-DELIVERY-METHOD.
            MOVE 7777777        TO OO-SUPPLIER-NUMBER.
            MOVE "Y"            TO OO-UPDATED.
            MOVE "L"            TO OO-FOR-LOC.
            MOVE WS-COSTS       TO OO-COST.
       RSQ-005.
            WRITE OUT-ORDER-REC
               INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE "SUPPLIERS FILE NOT WRITTEN, 'ESC' TO PROCEED"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
       RSQ-999.
           EXIT.
      *
       DELETE-SUPPLIER-ORDER SECTION.
       DSO-000.
            PERFORM ERROR1-020.
            MOVE 2910 TO POS.
            DISPLAY "REMOVING KIT ORDER FROM STOCK-FILE...." AT POS.
            MOVE WS-INVOICE  TO OO-ORDER-NUMBER
            MOVE WS-KIT-SAVE TO OO-STOCK-NUMBER.
            START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE 88 TO WS-OUTORD-ST1
               MOVE "SUPPLIER ORDER FILE BAD START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DSO-999.
       DSO-002.
            READ OUTSTANDING-ORDERS WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 = 23 OR 35 OR 49
      *         MOVE "SUPPLIER ORDER FILE DOES'NT EXIST, CANCEL TO EXIT"
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               GO TO DSO-999.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO WS-OUTORD-ST1
               MOVE "SUPPLIER ORDERS BUSY ON READ-LOCK, 'ESC' TO RE-TRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DSO-002.
       DSO-005.
            DELETE OUTSTANDING-ORDERS
               INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE "SUPPLIER ORDER NOT DELETED, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
       DSO-999.
           EXIT.
      *
       UPDATE-SUPPLIER-ORDER SECTION.
       UPSO-000.
            MOVE WS-INVOICE  TO OO-ORDER-NUMBER.
            MOVE WS-KIT-SAVE TO OO-STOCK-NUMBER.
            START OUTSTANDING-ORDERS KEY NOT < OO-KEY.
       UPSO-002.
            READ OUTSTANDING-ORDERS WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 = 23 OR 35 OR 49
      *         MOVE "SUPPLIER ORDER FILE DOES'NT EXIST, CANCEL TO EXIT"
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         MOVE WS-INVOICE TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         MOVE WS-KIT-SAVE TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               GO TO UPSO-999.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO WS-OUTORD-ST1
               MOVE "SUPPLIER ORDERS BUSY ON READ-LOCK, 'ESC' TO RE-TRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPSO-002.
            SUBTRACT WS-MFGQTY FROM OO-QUANTITY.
       UPSO-005.
            REWRITE OUT-ORDER-REC
               INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE "SUPPLIER ORDER NOT RE-WRITTEN, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
       UPSO-999.
           EXIT.
      *
       ENTER-KIT-ON-ORDER SECTION.
       EKOO-010.
           IF WS-NEWORDER = "N"
               GO TO EKOO-999.
           MOVE WS-KIT-SAVE TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           IF WS-ERR = "ERR"
               GO TO EKOO-900.
           ADD WS-KITQTY   TO ST-QTYONORDER.
           MOVE WS-DATE TO ST-LASTORDERDATE.
           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
       EKOO-900.
           PERFORM WRITE-SUPPLIER-ORDER.
       EKOO-999.
           EXIT.
      *
       CANCEL-KIT-ORDER SECTION.
       CKO-010.
           MOVE 2910 TO POS.
           DISPLAY "REMOVING KIT-ORDER FROM STOCK-FILE...." AT POS.
           MOVE WS-KIT-SAVE TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           IF WS-ERR = "ERR"
               MOVE
                "KIT BUSY ON READ-STOCK-LOCK, CAN'T CANCEL KIT ORDER"
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CKO-900.
           COMPUTE WS-MFGQTY = WS-KITQTY - WS-SHPDQTY.
           SUBTRACT WS-MFGQTY FROM ST-QTYONORDER.
           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
       CKO-900.
           PERFORM DELETE-SUPPLIER-ORDER.
       CKO-999.
           EXIT.
      *
       START-FOR-READ-NEXT SECTION.
       SFRN-001.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               MOVE " " TO ST-STOCKNUMBER
               MOVE 
               "STOCK RECORD AT END ON START, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
               CALL "C$SLEEP" USING 1
               GO TO RNSI-005.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD IN USE, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
       UPDATE-STOCK SECTION.
       USS-010.
           MOVE 0 TO WS-BO-QTY
                     WS-NEW-COST
                     WS-TOT-ONHAND.
           IF INCR-PRINTED = "P" OR = "L" OR = "Y"
            IF WS-NEWORDER = "N"
             IF WS-MFGQTY = 0
              GO TO USS-020.
      *
      *UPDATING HEADER STOCK ITEM - KIT BEING MADE
      *
           MOVE WS-KIT-SAVE TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           IF WS-ERR = "ERR"
              MOVE "USS-010, ITEM NOT THERE, NOT ON-HAND, 'ESC' TO EXIT" 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE ST-STOCKNUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO USS-020.
           COMPUTE WS-TOT-ONHAND = ST-QTYONHAND + WS-MFGQTY.
           IF ST-QTYONHAND > 0
              COMPUTE WS-NEW-COST = ((ST-QTYONHAND * ST-AVERAGECOST) +
                     (WS-COSTS * WS-MFGQTY)) / WS-TOT-ONHAND
              MOVE WS-NEW-COST TO ST-AVERAGECOST
           ELSE
              MOVE WS-COSTS TO    ST-AVERAGECOST.

           IF ST-QTYONRESERVE NOT < ST-QTYONBORDER
              ADD WS-MFGQTY TO ST-QTYONHAND
              GO TO USS-015.

           IF ST-QTYONRESERVE < ST-QTYONBORDER
              COMPUTE WS-BO-QTY = ST-QTYONBORDER - ST-QTYONRESERVE
            IF WS-BO-QTY > WS-MFGQTY
              ADD WS-MFGQTY TO ST-QTYONRESERVE
              GO TO USS-015
            ELSE
              ADD WS-BO-QTY TO        ST-QTYONRESERVE
              SUBTRACT WS-BO-QTY FROM WS-MFGQTY
              ADD WS-MFGQTY TO        ST-QTYONHAND
              ADD WS-BO-QTY TO        WS-MFGQTY.
       USS-015.
           ADD WS-MFGQTY TO ST-QTYRECMTD
                            ST-QTYRECYTD.
           IF WS-MFGQTY NOT > ST-QTYONORDER
              SUBTRACT WS-MFGQTY FROM ST-QTYONORDER
           ELSE
              MOVE 0               TO ST-QTYONORDER.
       USS-016.
           MOVE WS-DATE  TO ST-LASTRECEIPTDATE.
           MOVE WS-COSTS TO ST-LASTCOST.
           REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
           PERFORM UPDATE-SUPPLIER-ORDER.
           IF WS-MFGQTY > 0
               PERFORM WRITE-STOCK-RECEIPT.
       USS-020.
      *
      *UPDATING STOCK LINE ITEMS
      *
           MOVE 1 TO SUB-1.
       USS-025.
           IF B-NEWLINE (SUB-1) = "L" OR = "Y"
              GO TO USS-850.
           MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER
                                         SPLIT-STOCK.
           IF SP-1STCHAR = "/"
              GO TO USS-850.
           PERFORM READ-STOCK-LOCK.
           IF B-MFGQTY (SUB-1) NOT >         ST-QTYONRESERVE
              SUBTRACT B-MFGQTY (SUB-1) FROM ST-QTYONRESERVE
           ELSE
              MOVE 0                      TO ST-QTYONRESERVE
              MOVE WS-KIT-SAVE            TO WS-DAILY-1ST
              MOVE ST-STOCKNUMBER         TO WS-DAILY-2ND
              MOVE "ERR.SH-QTY > RES-QTY" TO WS-DAILY-3RD
              MOVE INCR-INVOICE           TO WS-DAILY-4TH
              PERFORM WRITE-DAILY.
           IF B-MFGQTY (SUB-1) NOT >         ST-QTYONBORDER
              SUBTRACT B-MFGQTY (SUB-1) FROM ST-QTYONBORDER
           ELSE
              MOVE 0 TO                      ST-QTYONBORDER.
           ADD B-MFGQTY (SUB-1) TO ST-SALESUNITMTD
                                   ST-SALESUNITSYTD.
           MOVE WS-DATE TO         ST-LASTSALEDATE.
           REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
       USS-850.
           ADD 1 TO SUB-1.
           IF SUB-1 < 150
            IF B-STOCKNUMBER (SUB-1) NOT = " "
              GO TO USS-025.
       USS-999.
           EXIT.
      *
       WRITE-STOCK-RECEIPT SECTION.
       WSR-010.
            PERFORM READ-PARAMETER-LOCK.
            MOVE PA-STOCK-RECEIPT-NUMBER TO STRE-TRANSACTION-NUMBER.
            ADD 1                        TO PA-STOCK-RECEIPT-NUMBER
            PERFORM REWRITE-PARAMETER
            MOVE 7                     TO STRE-TRANSACTION-CODE
            MOVE WS-KIT-SAVE           TO STRE-STOCK-NUMBER
            MOVE WS-MFGQTY             TO STRE-QUANTITY
            MOVE WS-COSTS              TO STRE-UNIT-PRICE
            COMPUTE STRE-TOTAL-PRICE = WS-COSTS * WS-MFGQTY
            
            MOVE "B/M"                 TO WS-BM-DESC
            MOVE WS-INVOICE            TO WS-BM-NUMBER
            MOVE "COPY"                TO WS-BM-COPY-DESC
            MOVE INCR-COPY-NUMBER      TO WS-BM-COPY
            MOVE WS-RECEIPT-LINE       TO STRE-REFERENCE-NO
            
            MOVE WS-INVOICEDATE        TO STRE-REFERENCE-DATE
            MOVE WS-KIT-COMMENT        TO STRE-ORDER-NUMBER.
       WSR-960.
            WRITE STOCK-RECEIPTS-REC
                 INVALID KEY NEXT SENTENCE.
            IF WS-STKRECEIPT-ST1 = 23 OR 35 OR 49
                 GO TO WSR-010.
            IF WS-STKRECEIPT-ST1 NOT = 0
                MOVE "ST-RECEIPT BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WSR-960.
       WSR-999.
           EXIT.
      *
       WRITE-REV-STOCK-RECEIPT SECTION.
       WRSR-010.
            PERFORM READ-PARAMETER-LOCK.
            MOVE PA-STOCK-RECEIPT-NUMBER TO STRE-TRANSACTION-NUMBER.
            ADD 1                        TO PA-STOCK-RECEIPT-NUMBER
            PERFORM REWRITE-PARAMETER
            MOVE 7                     TO STRE-TRANSACTION-CODE
            MOVE WS-KIT-SAVE           TO STRE-STOCK-NUMBER
            MOVE WS-KITQTY                TO STRE-QUANTITY
            COMPUTE STRE-QUANTITY = STRE-QUANTITY * -1
            MOVE ST-AVERAGECOST        TO STRE-UNIT-PRICE
            COMPUTE STRE-TOTAL-PRICE = ST-AVERAGECOST * WS-KITQTY
            MOVE "REVERSE MFG OF KITS" TO STRE-REFERENCE-NO.
            MOVE WS-DATE               TO STRE-REFERENCE-DATE
            MOVE " "                   TO STRE-ORDER-NUMBER.
       WRSR-960.
            WRITE STOCK-RECEIPTS-REC
                 INVALID KEY NEXT SENTENCE.
            IF WS-STKRECEIPT-ST1 = 23 OR 35 OR 49
                 GO TO WRSR-010.
            IF WS-STKRECEIPT-ST1 NOT = 0
                MOVE "ST-RECEIPT BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WRSR-960.
       WRSR-999.
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
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       READ-PARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           READ PARAMETER-FILE LOCK          
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO PARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RPL-000.
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
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
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
                MOVE SUB-1 TO SUB-20
                GO TO CHK-010.
       CHK-EXIT.
           EXIT.
      *
       RELEASE-INVOICE-REGISTER SECTION.
       RNIR-005.
           UNLOCK INCR-REGISTER.
       RNIR-999.
           EXIT.
      *
       CANCEL-KIT SECTION.
       CI-000.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           IF WS-INVOICE = 0
              GO TO CI-950.
           MOVE 1 TO SUB-1.
           IF WS-NEWORDER = "N"
              GO TO CI-900.
       CI-010.
           IF B-STOCKNUMBER (SUB-1) = " "
            IF B-ORDERQTY (SUB-1) = 0
               GO TO CI-900.
      *     PERFORM CANCEL-TRANSACTION.
      *     GO TO CI-000.
       CI-900.
           IF WS-NEWORDER = "Y"
               PERFORM DELETE-INVOICE-REGISTER
               PERFORM DELETE-STOCK-TRANS.
           PERFORM RELEASE-INVOICE-REGISTER
           PERFORM ERROR-020
           PERFORM CLEAR-FIELDS.
       CI-950.
           PERFORM DISPLAY-FORM.
       CI-999.
           EXIT.
      *
       CANCEL-TRANSACTION SECTION.
       CAN-005.
           MOVE " " TO WS-ERR.
           COMPUTE SUB-2 = SUB-1 + 1.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "/"
                GO TO CAN-010.
           MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           IF WS-ERR = "ERR" 
              GO TO CAN-010.
           ADD B-SHIPQTY (SUB-1)         TO ST-QTYONHAND
           SUBTRACT B-SHIPQTY (SUB-1)  FROM ST-QTYONRESERVE
           SUBTRACT B-ORDERQTY (SUB-1) FROM ST-QTYONBORDER.
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
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CAN-008.
       CAN-010.
            IF SUB-2 > 150 
               GO TO CAN-090.
            IF B-STOCKNUMBER (SUB-2) = " "
               MOVE " " TO B-STOCKNUMBER (SUB-1)
                           B-STOCKDESCRIPTION (SUB-1)
                           B-STOCKDESCRIPTION2 (SUB-1)
                           B-NEWLINE (SUB-1)
               MOVE 0   TO B-ORDERQTY (SUB-1)
                           B-SHIPQTY (SUB-1)
                           B-STOCKPRICE (SUB-1)
                           B-STOCKCOST (SUB-1)
               GO TO CAN-090.
             MOVE BODY-LINE (SUB-2) TO BODY-LINE (SUB-1).
             ADD 1 TO SUB-1 SUB-2.
             GO TO CAN-010.
       CAN-090.
             MOVE " " TO B-STOCKNUMBER (SUB-1)
                         B-STOCKDESCRIPTION (SUB-1)
                         B-STOCKDESCRIPTION2 (SUB-1)
                         B-NEWLINE (SUB-1).
             MOVE 0   TO B-ORDERQTY (SUB-1)
                         B-SHIPQTY (SUB-1)
                         B-MFGQTY (SUB-1)
                         B-SHIPPEDQTY (SUB-1)
                         B-STOCKPRICE (SUB-1)
                         B-STOCKCOST (SUB-1).
       CAN-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 1 TO SUB-1.
       CF-010.
             MOVE " " TO B-STOCKNUMBER (SUB-1)
                         B-STOCKDESCRIPTION (SUB-1)
                         B-STOCKDESCRIPTION2 (SUB-1)
                         B-NEWLINE (SUB-1).
             MOVE 0   TO B-ORDERQTY (SUB-1)
                         B-SHIPQTY (SUB-1)
                         B-MFGQTY (SUB-1)
                         B-SHIPPEDQTY (SUB-1)
                         B-STOCKPRICE (SUB-1)
                         B-STOCKCOST (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 150
                 GO TO CF-010.
       CF-020.
             MOVE " " TO INCR-PORDER
                         INCR-GSTNO
                         INCR-SB-TYPE
                         INCR-PRINTED
                         INCR-KIT-FIELDS.
             MOVE 0 TO INCR-KEY
                       INCR-ACCOUNT
                       INCR-DATE
                       INCR-SALES
                       INCR-INVCRED-AMT
                       INCR-TAX
                       INCR-ADDONS
                       INCR-DISCOUNT
                       INCR-INVCRED-COST
                       INCR-DRTRANS-NO.
      *                 INCR-STTRANS-NO.
       CF-999.
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
            IF SUB-1 > 139
               MOVE 139 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 139  
                GO TO NEXT-030.
            IF F-INDEX < 13
                GO TO NEXT-010.
       NEXT-030.
            SUBTRACT 12 FROM SUB-1.
            IF SUB-1 > 139
              IF SUB-25 > 139
               COMPUTE F-INDEX = 12 - (151 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 12
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3010 TO POS
            DISPLAY "    BODY LINE: " AT POS
            ADD 16 TO POS
            MOVE SUB-1 TO WS-BODY-LINE
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-999.
             EXIT.
      *
       SCROLL-NEXT-PAGE SECTION.
       NEXT-PAGE-000.
            ADD 12 TO SUB-1.
            IF SUB-1 > SUB-25
               MOVE SUB-25 TO SUB-1.
            MOVE 1 TO F-INDEX. 
            PERFORM CLEAR-BODY.
            MOVE 1 TO F-INDEX. 
            IF SUB-1 > 139
               MOVE 139 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 139
                GO TO NEXT-PAGE-030.
            IF F-INDEX < 13
                GO TO NEXT-PAGE-010.
       NEXT-PAGE-030.
            SUBTRACT 12 FROM SUB-1.
            IF SUB-1 > 139
              IF SUB-25 > 139
               COMPUTE F-INDEX = 12 - (151 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX.
            IF SUB-1 > 139
               MOVE 139 TO SUB-1.
            IF F-INDEX > 12
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3010 TO POS
            DISPLAY "    BODY LINE: " AT POS
            ADD 16 TO POS
            MOVE SUB-1 TO WS-BODY-LINE
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-PAGE-999.
             EXIT.
      *
       SCROLL-PREVIOUS SECTION.
       PREV-000.
            SUBTRACT 12 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-010.
            PERFORM SCROLLING.
       PREV-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 125   
                GO TO PREV-030.
            IF F-INDEX < 13
                GO TO PREV-010.
       PREV-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 12 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

             MOVE 3010 TO POS
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
            IF SUB-1 > 139
                GO TO SCROLL-DOWN-030.
            IF F-INDEX < 12
                GO TO SCROLL-DOWN-010.
       SCROLL-DOWN-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 12 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3010 TO POS
            DISPLAY "    BODY LINE: " AT POS
            ADD 16 TO POS
            MOVE SUB-1 TO WS-BODY-LINE
            DISPLAY WS-BODY-LINE AT POS.
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
       SCROLL-020.
            MOVE "TOTALQTY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-025.
            IF B-NEWLINE (SUB-1) = "L" OR = "Y"
               MOVE B-ORDERQTY (SUB-1) TO F-EDNAMEFIELDQTY
               PERFORM WRITE-FIELD-QTY
            ELSE
               COMPUTE WS-BO-QTY = B-ORDERQTY (SUB-1) -
                          B-SHIPPEDQTY (SUB-1)
               MOVE WS-BO-QTY TO F-EDNAMEFIELDQTY
               PERFORM WRITE-FIELD-QTY.
       SCROLL-025.
            MOVE "MFGQTY" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            IF B-NEWLINE (SUB-1) = "L" OR = "Y"
                MOVE "SHIPD" TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-030.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE B-SHIPQTY (SUB-1) TO F-EDNAMEFIELDQTY
                PERFORM WRITE-FIELD-QTY
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.
       SCROLL-030.
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE B-STOCKDESCRIPTION (SUB-1) TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
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
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE B-STOCKCOST (SUB-1) TO F-EDNAMEFIELDAMOUNT
                PERFORM WRITE-FIELD-AMOUNT
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.
       SCROLL-500.
            MOVE SPACES                      TO WS-STDESC
            MOVE B-STOCKDESCRIPTION (SUB-1)  TO WS-DESC1
            MOVE B-STOCKDESCRIPTION2 (SUB-1) TO WS-DESC2.

            MOVE "ST-DESC"     TO F-FIELDNAME
            MOVE 7             TO F-CBFIELDNAME
            MOVE WS-STDESC     TO F-NAMEFIELD
            MOVE 40            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-000.
            MOVE 0 TO F-INDEX.
       CLEAR-002.
            ADD 1 TO F-INDEX.
            IF F-INDEX > 12
               GO TO CLEAR-BODY-999.

            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TOTALQTY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 0 TO F-EDNAMEFIELDNUM.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "MFGQTY" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE 0 TO F-EDNAMEFIELDNUM.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
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

            GO TO CLEAR-002.
       CLEAR-BODY-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' FOR STATUS."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
           PERFORM READ-PARAMETER.
           PERFORM GET-SYSTEM-Y2K-DATE
      *     ACCEPT WS-DATE FROM DATE.
           IF WS-MM = PA-CURRENT-PER-MM
             IF WS-YY = PA-CURRENT-PER-YY
               GO TO OPEN-010.
       OPEN-005.
           DISPLAY "THE CURRENT MONTH OR YEAR ON THE PARAMETER FILE".
           DISPLAY " "
           DISPLAY "DOES NOT CORRESPOND WITH TODAYS DATE!!!!".
           DISPLAY " ".
           DISPLAY "GO AND CHECK THE SYSTEM DATE, ".
           DISPLAY " ".
           DISPLAY "AS IT APPEARS YOU'RE IN THE WRONG MONTH.".
           DISPLAY " ".
           DISPLAY "PRESS 'GO' OR 'NEXT' TO END THE PROGRAM".

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-IMM-PR.

      *     ACCEPT WS-IMM-PR AT POS.
           IF W-ESCAPE-KEY = 1 OR 2
               CLOSE PARAMETER-FILE
               EXIT PROGRAM
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO OPEN-005.
       OPEN-010.
           MOVE Ws-Co-Name TO CO-NAME.
           CLOSE PARAMETER-FILE.
       OPEN-011.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-011.
       OPEN-014.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
       OPEN-015.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0 
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-015.
       OPEN-016.
           OPEN I-O TOOLKITS.
           IF WS-KIT-ST1 NOT = 0 
              MOVE 0 TO WS-KIT-ST1
              MOVE "TOOLKITS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-016.
       OPEN-017.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-017.
       OPEN-018.
           OPEN I-O OUTSTANDING-ORDERS.
           IF WS-OUTORD-ST1 NOT = 0 
              MOVE 0 TO WS-OUTORD-ST1
              MOVE "SUPPLIERS FILE BUSY, BE PATIENT!" TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-018.
       OPEN-040.
            OPEN I-O STKRECEIPTS-FILE.
            IF WS-STKRECEIPT-ST1 NOT = 0
               MOVE 0 TO WS-STKRECEIPT-ST1
               MOVE "ST-RECEIPTS BUSY ON OPEN, 'ESC' TO RETRY." 
                  TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-040.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "BmKitMfg"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER
                 STOCK-TRANS-FILE
                 INCR-REGISTER
                 PARAMETER-FILE
                 OUTSTANDING-ORDERS
                 TOOLKITS.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldAmount".
       Copy "WriteFieldKitQty".
       Copy "WriteFieldQty".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "PrintReportInfo".
       Copy "GetUserMailName".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      *
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
       Copy "WriteDailyExcep1".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
