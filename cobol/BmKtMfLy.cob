       IDENTIFICATION DIVISION.
       PROGRAM-ID. BmKtMfLy.
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
       Copy "SelectStTransLy".
       Copy "SelectSlRegLy".
       Copy "SelectStOrders".
       Copy "SelectSlDaily".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdParam.
           COPY ChlfdToolkit.
           COPY ChlfdStTransLy.
           COPY ChlfdOutOrd.
           COPY ChlfdRegisterLy.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-NEWORDER          PIC X VALUE " ".
       77  WS-SUFFICIENT-STOCK  PIC X VALUE " ".
       77  WS-STOCKNUMBER       PIC X(15).
       77  WS-END               PIC X VALUE " ".
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
       77  WS-QTY               PIC 9(3) VALUE 0.
       77  WS-RESQTY            PIC 9(3) VALUE 0.
       77  WS-MFGQTY            PIC 9(3) VALUE 0.
       77  WS-SHPDQTY           PIC 9(3) VALUE 0.
       77  WS-TEMPQTY           PIC S9(5) VALUE 0.
       77  WS-ORDERQTY          PIC 9(5) VALUE 0.
       77  WS-TOTALRESQTY       PIC 9(5) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-WRITE             PIC X VALUE " ".
       77  WS-STTR-LY-SHIPQTY      PIC 9(5) VALUE 0.
       77  WS-STTR-LY-ORDERQTY     PIC 9(5) VALUE 0.
       77  WS-ERR               PIC XXX VALUE " ".
       01  WS-STDESC.
           03  WS-DESC1          PIC X(20) VALUE " ".
           03  WS-DESC2          PIC X(20) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-STTRANSLY-STATUS.
           03  WS-STTRANSLY-ST1   PIC 99.
       01  WS-INCR-LY-STATUS.
           03  WS-INCR-LY-ST1     PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-KIT-ST1         PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1      PIC 99.
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
       01  WS-KITS.
           03  WS-KIT-SAVE          PIC X(15).
           03  WS-KIT-DESC1         PIC X(20).
           03  WS-KIT-DESC2         PIC X(20).
           03  WS-KIT-PRICE         PIC 9(6)V99.
           03  WS-KIT-COMMENT       PIC X(40).
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
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
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN
           PERFORM CLEAR-FIELDS.
       CONTROL-050.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           PERFORM DISPLAY-FORM.
           MOVE WS-DATE     TO WS-INVOICEDATE.
           MOVE "ON-HAND:"  TO FILLER-ONHAND
           MOVE "ON-RES:"   TO FILLER-ONRES
           MOVE "ON-B/O:"   TO FILLER-ONBO
           MOVE "ON-ORDER:" TO FILLER-ONORD.
       CONTROL-060.
           PERFORM GET-DATA.
           GO TO CONTROL-050.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-010.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "N" TO WS-LINECHANGED
                        WS-MUST-PRINT.
            MOVE "Y" TO WS-NEWORDER.
            MOVE " " TO WS-MESSAGE
                        WS-KITS
                        WS-ABOVE-BODY
                        WS-DIS
                        WS-TYPE-OF-FINISH.
            MOVE 0 TO WS-QTY
                      WS-MFGQTY
                      WS-SHPDQTY.
            PERFORM ERROR-020.
            PERFORM ERROR1-020.
            PERFORM CLEAR-FIELDS.
            PERFORM CONTROL-050.
            MOVE 0420 TO POS
            DISPLAY "** LAST YEAR INFO **" AT POS.
            
            MOVE 2907 TO POS
            DISPLAY
           "ENTER ALL 'X' FOR SUPPLIER ORDER ENQUIRY, 'STOCK' FOR" &
           " STOCK ENQUIRY," AT POS
              MOVE 3010 TO POS
              DISPLAY
           " '*' & NUMBER FOR EXISTING BILL OF MATERIAL." AT POS.
           
            MOVE "REFNO" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            MOVE 7       TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO F-NAMEFIELDRED.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM READ-NEXT-INVOICE-REGISTER
                PERFORM READ-STOCK-TRANSACTIONS
             IF WS-END NOT = "Y"
               GO TO GET-150
             ELSE
               GO TO GET-010.
            IF F-EXIT-CH = X"05"
                PERFORM READ-PREVIOUS-INVOICE-REGISTER
                PERFORM READ-STOCK-TRANSACTIONS
             IF WS-END NOT = "Y"
               GO TO GET-150
             ELSE
               GO TO GET-010.
            PERFORM ERROR-020.
            PERFORM ERROR1-020.
            IF F-NAMEFIELDRED = "STOCK  " OR = "       "
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
               MOVE "COPYDESC"             TO F-FIELDNAME
               MOVE 8                      TO F-CBFIELDNAME
               MOVE "* COMPLETE - COPY  *" TO F-NAMEFIELD
               MOVE 20                     TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.
           IF F-EXIT-CH = X"19"
            IF WS-NEWORDER = "N"
               MOVE "COPYDESC"             TO F-FIELDNAME
               MOVE 8                      TO F-CBFIELDNAME
               MOVE "* INCOMPLETE - COPY*" TO F-NAMEFIELD
               MOVE 20                     TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.

            MOVE "COPYNUMBER"     TO F-FIELDNAME
            MOVE 10               TO F-CBFIELDNAME
            MOVE INCR-LY-COPY-NUMBER TO F-NAMEFIELD
            MOVE 2                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
           IF F-EXIT-CH = X"19"
      *      IF WS-NEWORDER = "N"
                PERFORM READ-STOCK-TRANSACTIONS
                GO TO GET-150.
       GET-150.
            IF WS-NEWORDER = "Y"
               GO TO GET-999.
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
            MOVE WS-QTY   TO F-EDNAMEFIELDKITQTY.
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

            MOVE SPACES          TO WS-STDESC
            MOVE WS-KIT-DESC1    TO WS-DESC1
            MOVE WS-KIT-DESC2    TO WS-DESC2.

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
       GET-250.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
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
            PERFORM FILL-BODY.
       GET-999.
           EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
            PERFORM ERROR1-020.
            MOVE " " TO WS-ABOVE-BODY.
            MOVE 1 TO SUB-1 SUB-2.
            PERFORM FILL-011.
       FILL-005.
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
                GO TO FILL-999.

            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO SPLIT-STOCK.
            PERFORM FILL-005.
            IF F-EXIT-CH = X"01"
             IF B-STOCKNUMBER (SUB-1) = "  "
                SUBTRACT 1 FROM F-INDEX SUB-1
                PERFORM FILL-011
                GO TO FILL-005.

            IF F-EXIT-CH = X"01" AND F-INDEX > 1
             IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                SUBTRACT 1 FROM F-INDEX SUB-1
                PERFORM FILL-011
                GO TO FILL-010
             ELSE
              IF SP-1STCHAR NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                SUBTRACT 1 FROM F-INDEX SUB-1
                PERFORM FILL-011
                GO TO FILL-010.

            IF F-EXIT-CH = X"0B" AND F-INDEX < 12
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                ADD 1 TO F-INDEX SUB-1
                PERFORM FILL-011
                GO TO FILL-005
              ELSE
               IF SP-1STCHAR NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                ADD 1 TO F-INDEX SUB-1
                PERFORM FILL-011
                GO TO FILL-005.
            IF F-EXIT-CH = X"0B" AND F-INDEX = 12
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                PERFORM SCROLL-NEXT
                PERFORM FILL-011
                GO TO FILL-010
              ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
               IF SP-1STCHAR NOT = "*" AND NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM SCROLL-NEXT
                PERFORM FILL-011
                GO TO FILL-010.

            IF F-EXIT-CH = X"0A" AND F-INDEX < 12
             IF B-STOCKNUMBER (SUB-1) NOT = SPACES
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                ADD 1 TO F-INDEX SUB-1
                PERFORM FILL-011
                GO TO FILL-005
              ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                ADD 1 TO F-INDEX SUB-1
                PERFORM FILL-011
                GO TO FILL-005.
            IF F-EXIT-CH = X"0A" AND F-INDEX = 12
             IF B-STOCKNUMBER (SUB-1) NOT = SPACES
              IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                PERFORM SCROLL-NEXT
                PERFORM FILL-011
                GO TO FILL-010
              ELSE
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM SCROLL-NEXT
                PERFORM FILL-011
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
                GO TO FILL-010.
            IF F-EXIT-CH NOT = X"01" AND NOT = X"0B" AND NOT = X"11"
                     AND NOT = X"0C" AND NOT = X"05" AND NOT = X"13"
                     AND NOT = X"09" AND NOT = X"04" AND NOT = X"19"
                     AND NOT = X"99" AND NOT = X"07"
                GO TO FILL-010.
      * TAB CHARACTER
            IF F-EXIT-CH = X"09" OR = X"07"
                GO TO FILL-999.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
       FILL-011.
            MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER
                PERFORM READ-STOCK.
            IF WS-STOCK-ST1 NOT = "2"
                MOVE ST-QTYONHAND    TO WS-QTYONHAND
                MOVE ST-QTYONRESERVE TO WS-QTYONRESERVE
                MOVE ST-QTYONORDER   TO WS-QTYONORDER
                MOVE ST-QTYONBORDER  TO WS-QTYONBORDER
            ELSE
                MOVE 0               TO WS-QTYONHAND
                                        WS-QTYONRESERVE
                                        WS-QTYONORDER
                                        WS-QTYONBORDER.
            MOVE 2410 TO POS.
            DISPLAY WS-ONHAND-LINE AT POS.
       FILL-0150.
            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE ST-PRICE TO F-EDNAMEFIELDAMOUNT
                             B-STOCKPRICE (SUB-1).
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.

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
       FILL-999.
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
               (B-STOCKPRICE (SUB-1) * (B-ORDERQTY (SUB-1) / WS-QTY)).
           COMPUTE WS-COSTS = WS-COSTS +
               (B-STOCKCOST (SUB-1) * (B-ORDERQTY (SUB-1) / WS-QTY)).
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
       READ-INVOICE-REGISTER SECTION.
           MOVE WS-INVOICE TO INCR-LY-INVOICE.
           MOVE 7          TO INCR-LY-TRANS.
           START INCR-LY-REGISTER KEY NOT < INCR-LY-KEY
               INVALID KEY NEXT SENTENCE.
       RIR-005.
           READ INCR-LY-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-LY-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-INCR-LY-ST1
               MOVE "Y" TO WS-NEWORDER
               MOVE "THIS KIT ORDER CANNOT BE FOUND, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RIR-999.
           IF WS-INCR-LY-ST1 NOT = 0
               MOVE 0 TO WS-INCR-LY-ST1
               MOVE "REGISTER BUSY ON READ, RIR-005, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RIR-005.
           IF INCR-LY-PRINTED = "Y" OR = "L"
      *         MOVE "THIS KIT ORDER IS COMPLETE." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               PERFORM RIR-010
               MOVE "C" TO WS-NEWORDER
               GO TO RIR-999.
       RIR-010.
           MOVE INCR-LY-INVOICE     TO WS-ORDER.
           MOVE INCR-LY-DATE        TO WS-ORDERDATE.
           MOVE INCR-LY-KITNAME     TO WS-KIT-SAVE.
           MOVE INCR-LY-KITQTY      TO WS-QTY.
           MOVE 0                TO WS-MFGQTY.
           MOVE INCR-LY-KITSHPDQTY  TO WS-SHPDQTY.
           MOVE INCR-LY-KITPRICE    TO WS-KIT-PRICE.
           MOVE INCR-LY-KITDESC1    TO WS-KIT-DESC1.
           MOVE INCR-LY-KITDESC2    TO WS-KIT-DESC2. 
           MOVE INCR-LY-KITCOMMENT  TO WS-KIT-COMMENT.
       RIR-020.
           MOVE "N" TO WS-NEWORDER.
       RIR-999.
           EXIT.
      *
       READ-NEXT-INVOICE-REGISTER SECTION.
           MOVE WS-ORDER   TO INCR-LY-INVOICE.
           MOVE 7          TO INCR-LY-TRANS.
           START INCR-LY-REGISTER KEY > INCR-LY-KEY
               INVALID KEY NEXT SENTENCE.
       RDNX-005.
           READ INCR-LY-REGISTER NEXT
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-LY-ST1 = 10
               GO TO RDNX-999.
           IF WS-INCR-LY-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-INCR-LY-ST1
               MOVE "Y" TO WS-NEWORDER
               MOVE "THIS KIT ORDER CANNOT BE FOUND, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDNX-999.
           IF WS-INCR-LY-ST1 NOT = 0
               MOVE 0 TO WS-INCR-LY-ST1
               MOVE "REGISTER BUSY ON READ, RDNX-005, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDNX-005.
           IF INCR-LY-PRINTED = "Y" OR = "L"
               PERFORM RDNX-010
               MOVE "C" TO WS-NEWORDER
               GO TO RDNX-999.
       RDNX-010.
           MOVE INCR-LY-INVOICE     TO WS-ORDER WS-INVOICE.
           MOVE INCR-LY-DATE        TO WS-ORDERDATE.
           MOVE INCR-LY-KITNAME     TO WS-KIT-SAVE.
           MOVE INCR-LY-KITQTY      TO WS-QTY.
           MOVE 0                TO WS-MFGQTY.
           MOVE INCR-LY-KITSHPDQTY  TO WS-SHPDQTY.
           MOVE INCR-LY-KITPRICE    TO WS-KIT-PRICE.
           MOVE INCR-LY-KITDESC1    TO WS-KIT-DESC1.
           MOVE INCR-LY-KITDESC2    TO WS-KIT-DESC2. 
           MOVE INCR-LY-KITCOMMENT  TO WS-KIT-COMMENT.
       RDNX-020.
           MOVE "N" TO WS-NEWORDER.
       RDNX-999.
           EXIT.
      *
       READ-PREVIOUS-INVOICE-REGISTER SECTION.
           MOVE WS-ORDER   TO INCR-LY-INVOICE.
           MOVE 7          TO INCR-LY-TRANS.
           START INCR-LY-REGISTER KEY < INCR-LY-KEY
               INVALID KEY NEXT SENTENCE.
       RDPREV-005.
           READ INCR-LY-REGISTER PREVIOUS
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-LY-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-INCR-LY-ST1
               MOVE "Y" TO WS-NEWORDER
               MOVE "THIS KIT ORDER CANNOT BE FOUND, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDPREV-999.
           IF INCR-LY-TRANS NOT = 7
               MOVE 0 TO WS-INCR-LY-ST1
               MOVE "Y" TO WS-NEWORDER
               MOVE "NO MORE KITS IN READ-PREVIOUS, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDPREV-999.
           IF WS-INCR-LY-ST1 NOT = 0
               MOVE 0 TO WS-INCR-LY-ST1
               MOVE "REGISTER BUSY ON READ, RDPREV-005, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDPREV-005.
           IF INCR-LY-PRINTED = "Y" OR = "L"
               PERFORM RDPREV-010
               MOVE "C" TO WS-NEWORDER
               GO TO RDPREV-999.
       RDPREV-010.
           MOVE INCR-LY-INVOICE     TO WS-ORDER WS-INVOICE.
           MOVE INCR-LY-DATE        TO WS-ORDERDATE.
           MOVE INCR-LY-KITNAME     TO WS-KIT-SAVE.
           MOVE INCR-LY-KITQTY      TO WS-QTY.
           MOVE 0                TO WS-MFGQTY.
           MOVE INCR-LY-KITSHPDQTY  TO WS-SHPDQTY.
           MOVE INCR-LY-KITPRICE    TO WS-KIT-PRICE.
           MOVE INCR-LY-KITDESC1    TO WS-KIT-DESC1.
           MOVE INCR-LY-KITDESC2    TO WS-KIT-DESC2. 
           MOVE INCR-LY-KITCOMMENT  TO WS-KIT-COMMENT.
       RDPREV-020.
           MOVE "N" TO WS-NEWORDER.
       RDPREV-999.
           EXIT.
      *
       READ-STOCK-TRANSACTIONS SECTION.
       RSTT-000.
           MOVE "Y"        TO WS-SUFFICIENT-STOCK
           MOVE 1          TO SUB-1 SUB-25
           MOVE WS-INVOICE TO STTR-LY-REFERENCE1
           MOVE 7          TO STTR-LY-TYPE
           MOVE 1          TO STTR-LY-TRANSACTION-NUMBER.
           START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 NOT = 0
              MOVE "BAD START ON ST-TRANS, ESC' TO EXIT." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSTT-999.
       RSTT-010.
           READ STOCK-TRANSLY-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 = 10 OR = 91
              MOVE 0 TO STTR-LY-TYPE
              GO TO RSTT-999.
           IF WS-STTRANSLY-ST1 NOT = 0
              MOVE "ST-TRANS RECORD ERROR ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSTT-010.
           IF STTR-LY-REFERENCE1 NOT = WS-INVOICE
              GO TO RSTT-999.
           IF STTR-LY-TYPE NOT = 7
              GO TO RSTT-010.
           MOVE STTR-LY-TRANSACTION-NUMBER TO B-STTRANS (SUB-1).
           MOVE STTR-LY-STOCK-NUMBER TO B-STOCKNUMBER (SUB-1)
                                     SPLIT-STOCK.
           IF STTR-LY-COMPLETE = " "
               MOVE "N"           TO B-NEWLINE (SUB-1)
           ELSE
               MOVE STTR-LY-COMPLETE TO B-NEWLINE (SUB-1).
           MOVE STTR-LY-DESC1        TO B-STOCKDESCRIPTION (SUB-1)
           MOVE STTR-LY-DESC2        TO B-STOCKDESCRIPTION2 (SUB-1)
           MOVE STTR-LY-ORDERQTY     TO B-ORDERQTY (SUB-1)
           MOVE STTR-LY-SHIPQTY      TO B-SHIPQTY (SUB-1)
           MOVE 0                    TO B-MFGQTY (SUB-1)
           MOVE STTR-LY-SHIPPEDQTY   TO B-SHIPPEDQTY (SUB-1)
           MOVE STTR-LY-PRICE        TO B-STOCKPRICE (SUB-1)
           MOVE STTR-LY-COST-VALUE   TO B-STOCKCOST (SUB-1).
           IF STTR-LY-ORDERQTY NOT =
                STTR-LY-SHIPQTY + STTR-LY-SHIPPEDQTY
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
       READ-STOCK SECTION.
       R-ST-000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-005.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE " " TO ST-STOCKNUMBER
                           ST-DESCRIPTION1
                           ST-DESCRIPTION2
               MOVE 0   TO ST-PRICE
                           ST-AVERAGECOST
                           ST-DISCOUNT1
               GO TO R-ST-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-005.
           PERFORM ERROR-020.
       R-ST-999.
           EXIT.
      *
       START-FOR-READ-NEXT SECTION.
       SFRN-001.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               MOVE "STOCK BUSY ON START FOR READ, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
           UNLOCK INCR-LY-REGISTER.
       RNIR-999.
           EXIT.
      *
       CANCEL-KIT SECTION.
       CI-000.
             MOVE 1 TO SUB-1.
             IF WS-NEWORDER = "N"
                GO TO CI-900.
       CI-010.
             IF B-STOCKNUMBER (SUB-1) = " "
              IF B-ORDERQTY (SUB-1) = 0
                 GO TO CI-900.
             GO TO CI-000.
       CI-900.
             PERFORM RELEASE-INVOICE-REGISTER
             PERFORM ERROR-020
             PERFORM CLEAR-FIELDS.
       CI-999.
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
             MOVE " " TO INCR-LY-PORDER
                         INCR-LY-GSTNO
                         INCR-LY-SB-TYPE
                         INCR-LY-PRINTED
                         INCR-LY-KIT-FIELDS.
             MOVE 0 TO INCR-LY-KEY
                       INCR-LY-ACCOUNT
                       INCR-LY-DATE
                       INCR-LY-SALES
                       INCR-LY-INVCRED-AMT
                       INCR-LY-TAX
                       INCR-LY-ADDONS
                       INCR-LY-DISCOUNT
                       INCR-LY-INVCRED-COST
                       INCR-LY-DRTRANS-NO.
      *                 INCR-LY-STTRANS-NO.
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
            MOVE SPACES          TO WS-STDESC
            MOVE B-STOCKDESCRIPTION (SUB-1)  TO WS-DESC1
            MOVE B-STOCKDESCRIPTION2 (SUB-1) TO WS-DESC2.
            
            MOVE "ST-DESC"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-STDESC  TO F-NAMEFIELD
            MOVE 40         TO F-CBFIELDLENGTH
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
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
           PERFORM READ-PARAMETER.
           PERFORM GET-SYSTEM-Y2K-DATE.
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
           ACCEPT WS-IMM-PR AT POS.
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
           OPEN I-O INCR-LY-REGISTER.
           IF WS-INCR-LY-ST1 NOT = 0
              MOVE "REGISTER-LY FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
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
           OPEN I-O STOCK-TRANSLY-FILE.
           IF WS-STTRANSLY-ST1 NOT = 0 
              MOVE "ST-TRANS-LY FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
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
              MOVE "SUPPLIERS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-018.
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
                 STOCK-TRANSLY-FILE
                 INCR-LY-REGISTER
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
       Copy "CTOSCobolAccept".
      * END-OF-JOB
