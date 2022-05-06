        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlBrOrdr.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectDrMaster".
          Copy "SelectStMaster".
          Copy "SelectStOrders".
          Copy "SelectSlParameter".
          Copy "SelectSlDaily".
          Copy "SelectCoDataName".
          Copy "SelectStTrans".
          Copy "SelectSlRegister".
          Copy "SelectStDiscAcc".
           SELECT COMPANY-MENU ASSIGN TO "CoCompany"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-MENU-STATUS
               RECORD KEY IS PTY-KEY.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDataName.
           COPY ChlfdDebtor.
           COPY ChlfdStock.
           COPY ChlfdCompany.
           COPY ChlfdDaily.
           COPY ChlfdOutOrd.
           COPY ChlfdStTrans.
           COPY ChlfdParam.
           COPY ChlfdRegister.
           COPY ChlfdStDiscAcc.
      *
       WORKING-STORAGE SECTION.
       77  Ws-cbDebtor        PIC 9(3) VALUE 1.
       77  Ws-cbStock         PIC 9(3) VALUE 11.
       77  Ws-cbStTrans       PIC 9(3) VALUE 12.
       77  Ws-cbStOrders      PIC 9(3) VALUE 16.
       77  Ws-cbDailyEx       PIC 9(3) VALUE 21.
       77  Ws-cbParameter     PIC 9(3) VALUE 23.
       77  Ws-cbSales         PIC 9(3) VALUE 26.
       77  Ws-cbRegister      PIC 9(3) VALUE 28.
       77  Ws-cbStDiscAcc     PIC 9(3) VALUE 95.
       77  WS-ACCEPT          PIC X VALUE " ".
       77  WS-DEBTOR-ACCEPT   PIC X(7) VALUE " ".
       77  WS-COMPANY-UPDATE  PIC XX VALUE " ".      
       77  WS-UPDATE          PIC X VALUE " ".      
       77  NEW-STOCKNO        PIC X VALUE " ".      
       77  WS-NEWSTOCKNUMBER  PIC X(15) VALUE " ".
       77  WS-OLDSTOCKNUMBER  PIC X(15) VALUE " ".
       77  WS-TOOL-VALID      PIC X VALUE " ".
       77  WS-TOOLKIT-INVALID PIC X VALUE " ".
       77  WS-TOOLKITNUMBER   PIC X(15) VALUE " ".
       77  WS-NEW-KIT         PIC X(15) VALUE " ".
       77  WS-QTY             PIC 9(3) VALUE 0.
       77  WS-PORDER          PIC X(20) VALUE " ".      
       77  WS-RANGE3          PIC X VALUE " ".      
       77  WS-RANGE4          PIC X VALUE " ".      
       77  WS-RANGE5          PIC X VALUE " ".      
       77  WS-CATEGORY        PIC X(3) VALUE " ".      
       77  WS-NUMBER          PIC 9(4) VALUE 0.
       77  WS-LEAP-YEAR       PIC X VALUE " ".
       77  WS-AMOUNT          PIC 9(3)V99 VALUE 0.
       77  WS-PART-ORDERS       PIC X VALUE " ".
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-P-SLIP            PIC 9(6) VALUE 0.
       77  WS-QUOTE             PIC 9(6) VALUE 0.
       77  WS-REPAIR            PIC 9(6) VALUE 0.
       77  WS-POORDERNO         PIC X(25) VALUE " ".
       77  WS-SALESANALYSIS     PIC X(14) VALUE " ".
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-ORDER-COMPLETE    PIC X VALUE " ".
       77  WS-NEXT              PIC X VALUE " ".
       77  WS-DELIVERVIA        PIC X(20) VALUE " ".
       77  WS-TERMOFSALE        PIC X(11) VALUE " ".
       77  WS-BINNO             PIC X(6) VALUE " ".
       77  WS-GSTNO             PIC X(13) VALUE " ".
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
       77  WS-NEWORDER          PIC X VALUE " ".
       77  WS-BOFOUND           PIC X VALUE " ".
       77  WS-BORDERS-FOUND     PIC X VALUE " ".
       77  WS-STTRANS-NO        PIC 9(6).
       77  WS-DRTRANS-NO        PIC 9(6).
       77  WS-DR-DISC           PIC 9(2)V99 VALUE 0.
       77  WS-QUES-MU-GP-PERC   PIC X VALUE " ".
       77  WS-READS             PIC 99.
       77  WS-TERM-SUB          PIC 9 VALUE 0.    
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
       01  SPLIT-ANALYSIS.
           03  WSAN-CODE.
               05  WSAN-CODE-1  PIC X VALUE " ".
               05  WSAN-CODE-2  PIC X VALUE " ".
           03  WSAN-REST        PIC X(12) VALUE " ".
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1      PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1      PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1     PIC 99.
       01  WS-MENU-STATUS.
           03  WS-MENU-ST1        PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1       PIC 99.
       01  WS-SALES-STATUS.
           03  WS-SALES-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1        PIC 99.
       01  WS-STDISC-STATUS.
           03  WS-STDISC-ST1      PIC 99.
       01  WS-DATA-STATUS.
           03  WS-DATA-ST1        PIC 99.
       01  COMPANIES-LIST-NAMES.
         02  COMPANIES-LIST OCCURS 20.
           03  LIST-NAME        PIC X(40).
           03  LIST-NUMBER      PIC 99.
           03  LIST-VOL-DIR     PIC X(25).
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 201.
               05  B-STOCKNUMBER.
                   07  B-1ST-CHAR      PIC X.
                   07  B-REST          PIC X(14).
               05  B-NEWLINE           PIC X.
               05  B-STTRANS           PIC 9(6).
               05  B-INVOICED          PIC 9(6).
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
       Copy "WsDateInfo".
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
           MOVE 0317 TO POS
           DISPLAY "*** BRANCH STOCK ORDER PLACING PROGRAM ***" AT POS
           MOVE 0417 TO POS
           DISPLAY "******************************************" AT POS.
       CONT-010.
           Move 0 to F-Error5.
           Move 2610 to Pos
           Display "Checking Data-Names........." At Pos.
           Perform Check-Data-Names.
           
           Move 2610 to Pos
           Display "Reading Next-Company........" At Pos.
           Perform Read-Next-Company.
       CONT-015.
           Move 2610 to Pos
           Display "Selecting Branch to Update..." At Pos.
           Perform Select-Branch.
           
           Move 2610 to Pos
           Display "Opening Branch Files........" At Pos.
           Perform Check-Branch-Data-Names.
           IF F-ERROR5 NOT = 0 AND NOT = 220
               GO TO CONT-015.
           
           Perform Open-Files.
           Move 2610 to Pos
           Display "                             " At Pos.
           
           Perform Get-Data.
           
           IF W-ESCAPE-KEY = 4
              GO TO CONT-060.
           
           PERFORM GET-DEBTOR-INFO.
       CONT-018.
           MOVE 2410 TO POS
           DISPLAY "ARE YOU SURE YOU WISH TO CONTINUE : [ ]" AT POS
           ADD 37 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 21         TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE4.

      *     ACCEPT WS-RANGE4 AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO CONT-060.
           IF WS-RANGE4 NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONT-018.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONT-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONT-018.
       CONT-020.
           PERFORM READ-PARAMETER-LOCK.
           MOVE PA-ORDER-NUMBER TO WS-INVOICE.
       CONT-025.
           COMPUTE PA-ORDER-NUMBER = PA-ORDER-NUMBER + 1.
           PERFORM REWRITE-PARAMETER
           PERFORM READ-PARAMETER-LOCK
           IF WS-INVOICE = PA-ORDER-NUMBER
              GO TO CONT-025.
           PERFORM REWRITE-PARAMETER.
           
           MOVE 2110 TO POS
           DISPLAY "YOUR P/SLIP NUMBER IS :" AT POS
           ADD 24 TO POS
           DISPLAY WS-INVOICE AT POS.
           
           PERFORM WRIC-050
           PERFORM WRIC-055
           PERFORM WRIC-061.
           
           PERFORM READ-NEXT-ORDER-LINE.

           PERFORM WRIC-085.
           PERFORM END-000.
       CONT-060.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
           
           MOVE 2610 TO POS
           DISPLAY "OPENING H/OFFICE FILES........" AT POS.
           PERFORM CHECK-DATA-NAMES.
           MOVE WS-VOL-DIR TO LIST-VOL-DIR (SUB-40).
           PERFORM CHECK-BRANCH-DATA-NAMES.
           MOVE 2610 TO POS
           DISPLAY "OPENING H/OFFICE FILES......DONE." AT POS.

           MOVE 
           "THE ORDER HAS BEEN PLACED ON THE SUPPLIER, 'ESC' TO EXIT."
            TO WS-MESSAGE
            PERFORM ERROR1-MESSAGE.     
           PERFORM END-OFF.
       CONT-999.
           Exit.
      *
       GET-DATA SECTION.
       GET-005.
           MOVE 1210 TO POS
           DISPLAY "P/ORDER NUMBER   : [                    ]" AT POS
           ADD 20 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PORDER.

           IF W-ESCAPE-KEY = 4
               GO TO GET-999.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-005.
       GET-020.
           MOVE 2910 TO POS
           DISPLAY "Reading Number of Lines On The P/Order......  "
               AT POS.
           PERFORM READ-ALL-ORDER-LINES.
           IF WS-OUTORD-ST1 = 88
               GO TO GET-005.
               
           PERFORM ERROR-020
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS.
       GET-030.
           MOVE "N" TO WS-RANGE3.
           MOVE 1610 TO POS
           DISPLAY "ENTER N=NEW P/SLIP, A=ADD TO P/SLIP:    [ ]" AT POS
           MOVE 1710 TO POS
           DISPLAY "NB! A=ONLY IF THE PROCESS IS NOT COMPLETE." AT POS
           ADD 41 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 50        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE3.

           IF W-ESCAPE-KEY = 4
              GO TO GET-005.
           IF WS-RANGE3 NOT = "N" AND NOT = "A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-040.
           IF WS-RANGE4 = "N"
              PERFORM END-000
              PERFORM CHECK-DATA-NAMES
              MOVE WS-VOL-DIR TO LIST-VOL-DIR (SUB-40)
              PERFORM CHECK-BRANCH-DATA-NAMES
              PERFORM END-900.
       GET-999.
           EXIT.
      *
       READ-NEXT-ORDER-LINE SECTION.
       RNSC-000.
           PERFORM ERROR1-020.
           MOVE 2610 TO POS.
           DISPLAY "Reading Order Stock Lines........." AT POS.
           MOVE 0         TO WS-NUMBER
                             WS-STTRANS-NO.
           MOVE WS-PORDER TO OO-ORDER-NUMBER
           MOVE " "       TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
       RNSC-005.
           READ OUTSTANDING-ORDERS NEXT WITH LOCK
                 AT END NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 10
               GO TO RNSC-900.
           IF WS-OUTORD-ST1 NOT = 0
                MOVE "STOCK-ORDER BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-OUTORD-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE OO-STOCK-NUMBER TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-OUTORD-ST1
                GO TO RNSC-005.
           IF OO-ORDER-NUMBER < WS-PORDER
               GO TO RNSC-005.
           IF OO-ORDER-NUMBER > WS-PORDER
               GO TO RNSC-900.
               
           ADD 1 TO WS-NUMBER
           MOVE 2510 TO POS
           DISPLAY "Stock Order #      Being Added To P/Slip, Item:"
           AT POS
           ADD 13 TO POS
           MOVE WS-NUMBER TO F-EDNAMEFIELDCRED
           DISPLAY F-EDNAMEFIELDCRED AT POS
           MOVE 2558 TO POS
           DISPLAY OO-STOCK-NUMBER AT POS.
           
           PERFORM GET-STOCK-INFO
           PERFORM ERROR1-020
           PERFORM ERROR-020
           
           GO TO RNSC-005.
       RNSC-900. 
           PERFORM ERROR1-020.
           MOVE 2610 TO POS.
           DISPLAY WS-MESSAGE AT POS.
       RNSC-999.
           EXIT.
      *
       GET-STOCK-INFO SECTION.
       RSQT-000.
           MOVE 2910 TO POS
           DISPLAY "GETTING STOCK INFO FOR ORDER............" AT POS.
           MOVE WS-NUMBER TO SUB-1.
       RSQT-010.
           PERFORM READ-STOCK.
           IF NEW-STOCKNO = "Y"
              MOVE OO-STOCK-NUMBER TO SP-REST
              MOVE "/"             TO SP-1STCHAR
              MOVE SPLIT-STOCK     TO ST-STOCKNUMBER.
           
           MOVE ST-STOCKNUMBER     TO B-STOCKNUMBER (SUB-1)
           MOVE " "                TO B-NEWLINE (SUB-1)
           MOVE 0                  TO B-INVOICED (SUB-1)
           MOVE ST-DESCRIPTION1    TO B-STOCKDESCRIPTION (SUB-1)
           MOVE ST-DESCRIPTION2    TO B-STOCKDESCRIPTION2 (SUB-1)
           MOVE OO-QUANTITY        TO B-ORDERQTY (SUB-1).
           IF NEW-STOCKNO = "Y" 
              MOVE 0               TO B-SHIPQTY (SUB-1)
           ELSE
              PERFORM RESERVE-STOCK-QUANTITIES.
              
           MOVE 0                  TO B-SHIPPEDQTY (SUB-1).
           
           PERFORM READ-SPECIAL-DISC-ACC.
            
           IF STDISC-PERCENT > 0
               GO TO RSQT-020.

      ******************************            
      *  53=BRANCHES               *
      *  57=ASSOCIATE COMPANIES    *
      ******************************
           IF WSAN-CODE NOT = "53" AND NOT = "57"
               GO TO RSQT-015.
           
           IF WSAN-CODE = "53"
              PERFORM COMPUTE-SPECIAL-PRICES.
           IF WSAN-CODE = "57"
              PERFORM COMPUTE-ASSOCIATE-PRICES.
              
           MOVE ST-PRICE          TO STTR-PRICE
           MOVE 0                 TO STTR-ITEMDISC
                                        B-DISCOUNTPERITEM (SUB-1).
           
           GO TO RSQT-020.
       RSQT-015.
      * CHECK FOR NORMAL DISCOUNT ON THE ITEM AS SPECIAL DISC NOT VALID
      * AND NOT CODE 53 OR CODE 57.  AS IN CTJ ORDERING FROM ORX.
           MOVE WS-NUMBER TO SUB-1.
            IF DR-DISCOUNT-CODE = "0" OR = " "
                MOVE 0            TO B-DISCOUNTPERITEM (SUB-1).
            IF DR-DISCOUNT-CODE = "1"
                MOVE ST-DISCOUNT1 TO B-DISCOUNTPERITEM (SUB-1).
            IF DR-DISCOUNT-CODE = "2"
                MOVE ST-DISCOUNT2 TO B-DISCOUNTPERITEM (SUB-1).
            IF DR-DISCOUNT-CODE = "3"
                MOVE ST-DISCOUNT3 TO B-DISCOUNTPERITEM (SUB-1).
            IF DR-DISCOUNT-CODE = "4"
                MOVE ST-DISCOUNT4 TO B-DISCOUNTPERITEM (SUB-1).
            IF DR-DISCOUNT-CODE = "5"
                MOVE ST-DISCOUNT5 TO B-DISCOUNTPERITEM (SUB-1).
            IF DR-DISCOUNT-CODE = "6"
                MOVE ST-DISCOUNT6 TO B-DISCOUNTPERITEM (SUB-1).
            IF DR-DISCOUNT-CODE = "7"
                MOVE ST-DISCOUNT7 TO B-DISCOUNTPERITEM (SUB-1).
            IF DR-DISCOUNT-CODE = "8"
                MOVE ST-DISCOUNT8 TO B-DISCOUNTPERITEM (SUB-1).
            IF DR-DISCOUNT-CODE = "9"
                MOVE ST-DISCOUNT9 TO B-DISCOUNTPERITEM (SUB-1).
            
            MOVE B-DISCOUNTPERITEM (SUB-1) TO STTR-ITEMDISC.
       RSQT-020.
           MOVE ST-PRICE          TO B-STOCKPRICE (SUB-1)
           MOVE ST-AVERAGECOST    TO B-STOCKCOST (SUB-1)
           MOVE STTR-ITEMDISC     TO B-DISCOUNTPERITEM (SUB-1)
           MOVE "Y"               TO B-TAX (SUB-1)
           MOVE ST-UNITOFMEASURE  TO B-UNIT (SUB-1).
           COMPUTE WS-PRICETOTAL = (B-STOCKPRICE (SUB-1) -
            ((B-STOCKPRICE (SUB-1) * B-DISCOUNTPERITEM (SUB-1))
                  / 100)) * B-SHIPQTY (SUB-1).
           MOVE WS-PRICETOTAL     TO B-NETT (SUB-1)
           COMPUTE WS-COSTTOTAL = WS-COSTTOTAL +
               (B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1)).
       RSQT-040.
           PERFORM WNT-TRANS-002 THRU WNT-TRANS-800.
       RSQT-050.
           ADD 1 TO SUB-1
           MOVE SUB-1 TO SUB-25.
           IF SUB-1 > 200
               MOVE
            "TOO MANY LINE ITEMS ON ORDER, 200 REACHED. GOING TO END."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSQT-999.
               
           PERFORM ERROR1-020.
      *     GO TO RSQT-010.
       RSQT-999.
           EXIT.
      *
       Copy "ComputeSpecialPrices".
       Copy "ComputeAssociatePrices".
      *
       RESERVE-STOCK-QUANTITIES SECTION.
       RSQ-000.
           MOVE 2910 TO POS
           DISPLAY "RESERVING STOCK QUANTITIES.............." AT POS.
       RSQ-002.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "/"
                GO TO RSQ-999.
           MOVE B-STOCKNUMBER (SUB-1) TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
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
               MOVE B-ORDERQTY (SUB-1)       TO  B-SHIPQTY (SUB-1)
               GO TO RSQ-020.
           MOVE ST-QTYONHAND             TO B-SHIPQTY (SUB-1)
           ADD  ST-QTYONHAND             TO ST-QTYONRESERVE
           MOVE 0                        TO ST-QTYONHAND
           ADD B-ORDERQTY (SUB-1)        TO ST-QTYONBORDER.
       RSQ-020.
           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON REWRITE <RSQ-020>, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RSQ-020.
               
           PERFORM ERROR1-020.
       RSQ-999.
           EXIT.
      *
       WRITE-NEW-TRANS SECTION.
       WNT-TRANS-000.
           IF B-NEWLINE (SUB-1) NOT = " "
               GO TO WNT-TRANS-999.
       WNT-TRANS-002.
           MOVE WS-INVOICE                  TO STTR-REFERENCE1
           MOVE 4                           TO STTR-TYPE
           MOVE WS-NUMBER                   TO STTR-TRANSACTION-NUMBER
                                                  B-STTRANS (SUB-1).
                                                  
           MOVE B-STOCKNUMBER (SUB-1)       TO SPLIT-STOCK
                                               STTR-STOCK-NUMBER
           MOVE DR-ACCOUNT-NUMBER           TO STTR-ACCOUNT-NUMBER
           MOVE "N"                         TO STTR-COMPLETE
                                               STTR-ST-COMPLETE
                                               STTR-AC-COMPLETE
                                                  B-NEWLINE (SUB-1).
            
           MOVE 0                           TO STTR-INV-NO
           MOVE WS-DATE                     TO STTR-DATE
                                               STTR-AC-DATE
                                               STTR-ST-DATE.
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
           MOVE B-STOCKCOST (SUB-1)         TO STTR-COST-VALUE.
       WNT-TRANS-800.
           WRITE STOCK-TRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS BUSY ON WRITE <WNT-800>, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              ADD 1 TO WS-STTRANS-NO STTR-TRANSACTION-NUMBER
              MOVE 0 TO WS-STTRANS-ST1
              GO TO WNT-TRANS-800.
           IF STTR-TRANSACTION-NUMBER = 0
              MOVE "ST-TRANS-NO = 0 AT WNT-TRANS-800, 'ESC' TO RETRY."
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
       READ-STOCK SECTION.
       R-ST-000.
           MOVE OO-STOCK-NUMBER TO ST-STOCKNUMBER
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       R-ST-010.
           READ STOCK-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE SPACES TO STOCK-RECORD
               MOVE "Y" TO NEW-STOCKNO
               GO TO R-ST-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-010.
           MOVE "N" TO NEW-STOCKNO.
       R-ST-999.
           EXIT.
      *
       READ-STOCK-VALID SECTION.
       RSTV-000.
             START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY NEXT SENTENCE.
       RSTV-010.
             READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE SPACES TO STOCK-RECORD
                MOVE "Y" TO NEW-STOCKNO
                GO TO RSTV-999.
             IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK BUSY ON READ,<RSTV-010> 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK-ST1
                GO TO RSTV-010.
             MOVE "N" TO NEW-STOCKNO.
       RSTV-999.
             EXIT.
      *
       READ-ALL-ORDER-LINES SECTION.
       RSN-000.
           MOVE 0         TO WS-NUMBER.
           MOVE WS-PORDER TO OO-ORDER-NUMBER
           MOVE " "       TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
              MOVE "NO SUCH P/ORDER IN THE SYSTEM, RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 88 TO WS-OUTORD-ST1
              GO TO RSN-999.
       RSN-005. 
           READ OUTSTANDING-ORDERS NEXT
             AT END 
               GO TO RSN-900.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "STOCKCHANGE FILE BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO RSN-005.
           IF OO-ORDER-NUMBER < WS-PORDER
               GO TO RSN-005.
           IF OO-ORDER-NUMBER > WS-PORDER
               GO TO RSN-900.
           ADD 1 TO WS-NUMBER.
           GO TO RSN-005.
       RSN-900.
           MOVE 2910 TO POS
           DISPLAY "                                              "
              AT POS.
           MOVE 1310 TO POS
           DISPLAY "There are      Stock Lines on the P/Order.    "
              AT POS.
           ADD 9 TO POS
           MOVE WS-NUMBER TO F-EDNAMEFIELDCRED
           DISPLAY F-EDNAMEFIELDCRED AT POS.
       RSN-999.
           EXIT.
      *
       WRITE-INCR-REGISTER SECTION.
       WRIC-050.
            MOVE WS-INVOICE        TO INCR-INVOICE
            MOVE 4                 TO INCR-TRANS
            MOVE DR-ACCOUNT-NUMBER TO INCR-ACCOUNT
            MOVE WS-PORDER         TO INCR-PORDER
            MOVE DR-GSTNO          TO INCR-GSTNO
            MOVE WS-DATE           TO INCR-DATE
            MOVE DR-SALES-ANALYSIS TO INCR-SALES
            MOVE WS-INVOICETOTAL   TO INCR-INVCRED-AMT
            MOVE WS-TAXAMT         TO INCR-TAX
            MOVE WS-ADDONAMT       TO INCR-ADDONS
            MOVE WS-DISCOUNTREG    TO INCR-DISCOUNT
            MOVE WS-COSTTOTAL      TO INCR-INVCRED-COST
            MOVE "YY"              TO INCR-SB-TYPE
            MOVE WS-DRTRANS-NO     TO INCR-DRTRANS-NO
            MOVE "Y"               TO INCR-PART-ORDERS
            MOVE "P"               TO INCR-PRINTED
            MOVE "G"               TO INCR-AREA
            MOVE 1                 TO INCR-COPY-NUMBER.
       WRIC-055.
            MOVE DR-NAME           TO INCR-NAME
            MOVE DR-ADDRESS1       TO INCR-ADD1
            MOVE DR-ADDRESS2       TO INCR-ADD2
            MOVE DR-ADDRESS3       TO INCR-ADD3
            MOVE DR-POST-CODE      TO INCR-CODE
            MOVE DR-DEL-ADDRESS1   TO INCR-DEL1
            MOVE DR-DEL-ADDRESS2   TO INCR-DEL2
            MOVE DR-DEL-ADDRESS3   TO INCR-DEL3.
            
            MOVE DR-TERMS-CODE            TO WS-TERM-SUB
            MOVE WS-ST-TERM (WS-TERM-SUB) TO INCR-TERMS.
            
            MOVE DR-TELEPHONE      TO INCR-PHONE.
            
            PERFORM GET-USER-MAIL-NAME
            MOVE WS-pbValue        TO INCR-CONTACT
            MOVE 0                 TO INCR-PULL-DATE
                                      INCR-PULL-TIME
            MOVE "  "                          TO INCR-PULLBY
            MOVE "**DELIVER AS NORMAL*"        TO INCR-DELIVERY
            MOVE "  "                          TO INCR-BIN
            MOVE "**GROUP AUTO SYSTEM ORDER**" TO INCR-COMMENT.
            MOVE INCR-INVOICE                  TO INCR-BO-INV-NO
            MOVE WS-DATE                       TO INCR-BO-DATE.
            
            MOVE 0                 TO INCR-ADDPOST
                                      INCR-ADDFREIGHT
                                      INCR-ADDLABOUR
                                      INCR-ADDMISC.
            MOVE WS-NUMBER         TO INCR-LINENO.
       WRIC-061.
            WRITE INCR-REC
                  INVALID KEY NEXT SENTENCE
            IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO WRIC-061.
       WRIC-075.
            GO TO WRIC-999.
       WRIC-085.
            PERFORM READ-INVOICE-REGISTER.
            PERFORM CALCULATE-ORDER-TOTAL.

            MOVE WS-PRICETOTAL1    TO INCR-INVCRED-AMT
            MOVE WS-TAXAMT         TO INCR-TAX
            MOVE WS-ADDONAMT       TO INCR-ADDONS
            MOVE WS-DISCOUNTREG1   TO INCR-DISCOUNT
            MOVE WS-COSTTOTAL1     TO INCR-INVCRED-COST.

            REWRITE INCR-REC
                  INVALID KEY NEXT SENTENCE
            IF WS-INCR-ST1 NOT = 0
               MOVE
                "REGISTER BUSY ON REWRITE <WRIC-085>, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-INCR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-INCR-ST1
                GO TO WRIC-085.
       WRIC-999.
              EXIT.
      *
       READ-INVOICE-REGISTER SECTION.
       RIR-000.
           MOVE WS-INVOICE TO INCR-INVOICE.
           MOVE 4          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
             MOVE "BAD START ON REGISTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-999.
       RIR-005.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE "REGISTER BAD READ ERC23, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RIR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE 
               "REGISTER BUSY ON READ-LOCK <RIR-005>, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-005.
       RIR-999.
           EXIT.
      *
       CALCULATE-ORDER-TOTAL SECTION.
       CTOS-000.
           MOVE 1 TO SUB-1.
           MOVE 0 TO WS-WORKTOTAL1 WS-DISCOUNT1 WS-COSTTOTAL1
                     WS-PRICETOTAL1 WS-DISCOUNTREG1.
       CTOS-010.
           IF B-STOCKNUMBER (SUB-1) = " "
                 GO TO CTOS-900.
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
       CTOS-900.
           IF WS-GSTNO NOT = "EXPORT"
             COMPUTE WS-TAXAMT ROUNDED =
               (WS-PRICETOTAL1 * WS-GST-PERCENT) / 100.
       CTOS-999.
           EXIT.
      *
       GET-DEBTOR-INFO SECTION.
       GET-DEBT-004.
           MOVE "N" TO WS-NEXT.
           PERFORM ERROR1-020
           MOVE 1810 TO POS.
           DISPLAY WS-MESSAGE AT POS
           DISPLAY "ENTER YOUR DEBTOR NUMBER : [       ]" AT POS.
           MOVE 1910 TO POS.
           DISPLAY "Enter the ACCOUNT NUMBER and <Return>, OR" AT POS.
           MOVE 2015 TO POS.
           DISPLAY "Enter a SHORT NAME and <PgDn> to scroll through" &
           " Accounts." AT POS.
           MOVE 1838 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 37        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DEBTOR-ACCEPT.

      *     ACCEPT WS-DEBTOR-ACCEPT AT POS.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-DEBT-005.
           IF W-ESCAPE-KEY = 7
                GO TO GET-DEBT-006
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-DEBT-004.
       GET-DEBT-005.
           IF WS-DEBTOR-ACCEPT = 0 OR = " "
               GO TO GET-DEBT-004.
           PERFORM READ-DEBTOR.
           IF DR-NAME = "** UNKNOWN **"
             MOVE
           "RE-ENTER THE DEBTOR NUMBER IN THE SUPPLIER'S DATABASE."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO GET-DEBT-004.
           GO TO GET-DEBT-020.
       GET-DEBT-006.
           MOVE WS-DEBTOR-ACCEPT TO F-NAMEFIELD.
       GET-DEBT-007.
           PERFORM READ-NEXT-DEBTOR.
       GET-DEBT-020.
           PERFORM ERROR-020
           PERFORM ERROR1-020
           MOVE 1810 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1810 TO POS
           DISPLAY "YOUR NUM: [       ]" AT POS.
           MOVE 1821 TO POS.
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
           MOVE 1832 TO POS.
           DISPLAY DR-NAME AT POS.
           MOVE 1910 TO POS.
           DISPLAY "Press <Return> to ACCEPT This Account,    " AT POS.
           MOVE 2015 TO POS.
           DISPLAY "OR Press <PgDn> To Scroll Through More Accounts" &
           ".                  " AT POS.
           MOVE 2075 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 74        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

      *     ACCEPT WS-ACCEPT AT POS.
           IF W-ESCAPE-KEY = 1 OR = 2 OR = 5
               GO TO GET-DEBT-900.
           IF W-ESCAPE-KEY = 7
               MOVE "Y" TO WS-NEXT
               MOVE " " TO WS-DEBTOR-ACCEPT
               GO TO GET-DEBT-007.
           IF W-ESCAPE-KEY = 6
               MOVE " " TO WS-DEBTOR-ACCEPT
               GO TO GET-DEBT-004.
           GO TO GET-DEBT-020.
       GET-DEBT-900.
            PERFORM ERROR-020
            PERFORM ERROR1-020
            MOVE 1910 TO POS
            DISPLAY WS-MESSAGE AT POS.
            MOVE 2010 TO POS
            DISPLAY WS-MESSAGE AT POS.
            MOVE 2610 TO POS
            DISPLAY WS-MESSAGE AT POS.
       GET-DEBT-999.
            EXIT.
      *
       READ-DEBTOR SECTION.
       RDR-000.
           MOVE "N"       TO WS-NEXT.
           MOVE WS-DEBTOR-ACCEPT TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY
                INVALID KEY NEXT SENTENCE.
       RDR-010.
           READ DEBTOR-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE "** UNKNOWN **" TO DR-NAME
                MOVE 88              TO WS-DEBTOR-ST1
                GO TO RDR-999.
           IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RDR-010.
           MOVE DR-SALES-ANALYSIS TO WSAN-CODE.
       RDR-999.
             EXIT.
      *
       READ-NEXT-DEBTOR SECTION.
       RNC-000.
           IF WS-NEXT = "Y"
               GO TO RNC-010.
           MOVE F-NAMEFIELD TO DR-NAME
           START DEBTOR-MASTER KEY NOT < DR-ALT-KEY
                  INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
                MOVE "** UNKNOWN **" TO DR-NAME
                MOVE 88              TO WS-DEBTOR-ST1
                MOVE "N"             TO WS-NEXT
                GO TO RNC-999.
       RNC-010.
           READ DEBTOR-MASTER NEXT
                AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
                MOVE "END OF SEARCH, 'ESC' TO RE-ENTER A SHORT NAME."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE "N" TO WS-NEXT
                GO TO RNC-999.
           IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR BUSY ON READ-NEXT, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RNC-010.
           MOVE DR-SALES-ANALYSIS TO WSAN-CODE.
       RNC-999.
           EXIT.
      *
       READ-SPECIAL-DISC-ACC SECTION.
       RST-DISC-ACC-000.
             MOVE 2910 TO POS
             DISPLAY "READING SPECIAL DISCOUNT FOR THE ACCOUNT.." AT POS
             
             MOVE ST-STOCKNUMBER    TO STDISC-STOCKNUMBER.
             MOVE DR-ACCOUNT-NUMBER TO STDISC-ACCOUNT.
             START STDISC-MASTER KEY NOT < STDISC-KEY
                  INVALID KEY NEXT SENTENCE.
       RST-DISC-ACC-010.
             READ STDISC-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STDISC-ST1 = 23 OR 35 OR 49
                MOVE 0 TO STDISC-PERCENT
                GO TO RST-DISC-ACC-999.
             IF WS-STDISC-ST1 NOT = 0
                MOVE "STDISCOUNT BUSY ON READ, 'ESC' TO RE-TRY."
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
       CNDIS-005.
           IF DR-DISCOUNT-CODE = " " OR = "0"
              MOVE 0 TO WS-DR-DISC
              GO TO CNDIS-999.
           IF DR-DISCOUNT-CODE = "1"
              MOVE ST-DISCOUNT1 TO WS-DR-DISC
              GO TO CNDIS-999.
           IF DR-DISCOUNT-CODE = "2"
              MOVE ST-DISCOUNT2 TO WS-DR-DISC
              GO TO CNDIS-999.
           IF DR-DISCOUNT-CODE = "3"
              MOVE ST-DISCOUNT3 TO WS-DR-DISC
              GO TO CNDIS-999.
           IF DR-DISCOUNT-CODE = "4"
              MOVE ST-DISCOUNT4 TO WS-DR-DISC
              GO TO CNDIS-999.
           IF DR-DISCOUNT-CODE = "5"
              MOVE ST-DISCOUNT5 TO WS-DR-DISC
              GO TO CNDIS-999.
           IF DR-DISCOUNT-CODE = "6"
              MOVE ST-DISCOUNT6 TO WS-DR-DISC
              GO TO CNDIS-999.
           IF DR-DISCOUNT-CODE = "7"
              MOVE ST-DISCOUNT7 TO WS-DR-DISC
              GO TO CNDIS-999.
           IF DR-DISCOUNT-CODE = "8"
              MOVE ST-DISCOUNT8 TO WS-DR-DISC
              GO TO CNDIS-999.
           IF DR-DISCOUNT-CODE = "9"
              MOVE ST-DISCOUNT9 TO WS-DR-DISC
              GO TO CNDIS-999.
       CNDIS-999.
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
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER TERMS BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
               MOVE "PARAMETER DELV BUSY ON READ, 'ESC' TO RETRY."
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
       READ-INVQUES-FILE SECTION.
       RINVQUES-000.
            MOVE 1 TO PA-RECORD.
            MOVE 6 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RINVQUES-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               GO TO RINVQUES-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER BUSY RINVQUES ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RINVQUES-010.
       RINVQUES-900.
            MOVE INVQUES-MU-GP-PERC      TO WS-QUES-MU-GP-PERC.
       RINVQUES-999.
            EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 0 TO WS-READS
                     PA-TYPE.
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
              ADD 1 TO WS-READS
            IF WS-READS > 10
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-READS
              GO TO RP-010
            ELSE
              GO TO RP-010.
       RP-999.
           EXIT.
      *
       READ-PARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 0 TO WS-READS
                     PA-TYPE.
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
              ADD 1 TO WS-READS
            IF WS-READS > 10
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RPL-010
              MOVE 0 TO WS-READS
            ELSE
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
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO REWP-000.
       REWP-999.
           EXIT.
      *
       CHECK-DATA-NAMES SECTION.
       CDN-005.
          MOVE " " TO ALPHA-RATE.
          MOVE 0   TO SUB-1.
       CDN-010.
          MOVE WS-VOL-DIR TO ALPHA-RATE.
       CDN-015.
          ADD 1 TO SUB-1.
          IF SUB-1 NOT > 60
           IF AL-RATE (SUB-1) NOT = " "
            GO TO CDN-015.
       CDN-020.
          MOVE WS-DATA-FILE TO DATA-RATE.
          MOVE 1            TO SUB-2.
       CDN-025.
          MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
          ADD 1 TO SUB-1 SUB-2.
          IF DAT-RATE (SUB-2) NOT = " "
           IF SUB-1 NOT > 60
             GO TO CDN-025.
       CDN-030.
          MOVE ALPHA-RATE TO WS-DATA
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE WS-VOL-DIR TO ALPHA-RATE.
          
          PERFORM OPEN-DATA-005
          PERFORM READ-DATAFILE.
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE WS-VOL-DIR TO ALPHA-RATE

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-STORDERS TO DATA-RATE
          PERFORM CDN-025
          MOVE ALPHA-RATE TO WS-STORDERS
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE WS-VOL-DIR TO ALPHA-RATE.
          
          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-DAILYEX TO DATA-RATE
          PERFORM CDN-025
          MOVE ALPHA-RATE TO WS-DAILYEX
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE WS-VOL-DIR TO ALPHA-RATE.
       CDN-900.
          CLOSE DATA-FILE.
       CDN-999.
          EXIT.
      *
       CHECK-BRANCH-DATA-NAMES SECTION.
       CBDN-005.
          MOVE " " TO ALPHA-RATE.
          MOVE 0   TO SUB-1.
       CBDN-010.
          MOVE LIST-VOL-DIR (SUB-40) TO ALPHA-RATE.
       CBDN-015.
          ADD 1 TO SUB-1.
          IF SUB-1 NOT > 60
           IF AL-RATE (SUB-1) NOT = " "
            GO TO CBDN-015.
       CBDN-020.
          MOVE WS-DATA-FILE TO DATA-RATE.
          MOVE 1            TO SUB-2.
       CBDN-025.
          MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
          ADD 1 TO SUB-1 SUB-2.
          IF DAT-RATE (SUB-2) NOT = " "
           IF SUB-1 NOT > 60
             GO TO CBDN-025.
       CBDN-030.
          MOVE ALPHA-RATE TO WS-DATA
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-40) TO ALPHA-RATE.

          PERFORM OPEN-DATA
          IF F-ERROR5 NOT = 0 AND NOT = 220
             GO TO CBDN-999.
             
          PERFORM READ-BRANCH-DATAFILE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-STOCK TO DATA-RATE
          PERFORM CBDN-025
          MOVE ALPHA-RATE TO WS-STOCK
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-40) TO ALPHA-RATE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-STTRANS TO DATA-RATE
          PERFORM CBDN-025
          MOVE ALPHA-RATE TO WS-STTRANS
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-40) TO ALPHA-RATE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-DEBTOR TO DATA-RATE
          PERFORM CBDN-025
          MOVE ALPHA-RATE TO WS-DEBTOR
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-40) TO ALPHA-RATE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-PARAMETER TO DATA-RATE
          PERFORM CBDN-025
          MOVE ALPHA-RATE TO WS-PARAMETER
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-40) TO ALPHA-RATE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-REGISTER TO DATA-RATE
          PERFORM CBDN-025
          MOVE ALPHA-RATE TO WS-REGISTER
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-40) TO ALPHA-RATE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-STDISCACC TO DATA-RATE
          PERFORM CBDN-025
          MOVE ALPHA-RATE TO WS-STDISCACC
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-40) TO ALPHA-RATE.
       CBDN-999.
          EXIT.
      *
       READ-DATAFILE SECTION.
       RC-005.
           MOVE WS-CBSTORDERS TO DATA-NUMBER.
           START DATA-FILE KEY NOT < DATA-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DATA-ST1 NOT = 0
               GO TO RC-999.
       RC-010.
           READ DATA-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-DATA-ST1 NOT = 0
               MOVE "DATAFILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE DATA-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RC-010.
       RC-020.
           MOVE DATA-NAME        TO WS-STORDERS.
           
           MOVE WS-CBDAILYEX     TO DATA-NUMBER
           PERFORM RC-010
           MOVE DATA-NAME        TO WS-DAILYEX.
       RC-999.
           EXIT.
      *
       READ-BRANCH-DATAFILE SECTION.
       RBC-005.
           MOVE WS-CBSTOCK TO DATA-NUMBER.
           START DATA-FILE KEY NOT < DATA-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DATA-ST1 NOT = 0
               GO TO RBC-999.
       RBC-010.
           READ DATA-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-DATA-ST1 NOT = 0
               MOVE "BRANCH DATAFILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DATA-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RBC-010.
       RBC-020.
           MOVE DATA-NAME      TO WS-STOCK.
           
           MOVE WS-CBSTTRANS TO DATA-NUMBER
           PERFORM RBC-010
           MOVE DATA-NAME  TO WS-STTRANS.
           
           MOVE WS-CBDEBTOR TO DATA-NUMBER
           PERFORM RBC-010
           MOVE DATA-NAME  TO WS-DEBTOR.
           
           MOVE WS-CBPARAMETER TO DATA-NUMBER
           PERFORM RBC-010
           MOVE DATA-NAME  TO WS-PARAMETER.
           
      *     MOVE WS-CBSALES TO DATA-NUMBER
      *     PERFORM RBC-010
      *     MOVE DATA-NAME  TO WS-SALES.
           
           MOVE WS-CBREGISTER TO DATA-NUMBER
           PERFORM RBC-010
           MOVE DATA-NAME  TO WS-REGISTER.
           
           MOVE WS-CBSTDISCACC TO DATA-NUMBER
           PERFORM RBC-010
           MOVE DATA-NAME  TO WS-STDISCACC.
       RBC-900.
           CLOSE DATA-FILE.
       RBC-999.
           EXIT.
      *
       CHECK-DATA-SIZE SECTION.         
       CDS-005.
           MOVE " " TO DATA-RATE.
           MOVE 0   TO SUB-10.
       CDS-015.
           ADD 1 TO SUB-10.
           IF DAT-RATE (SUB-10) NOT = " "
            IF SUB-10 NOT > 60
            GO TO CDS-015.
          SUBTRACT 1 FROM SUB-10.
       CDS-999.
          EXIT.
      *
       OPEN-NODE-FILE SECTION.
       ONF-001.
           PERFORM CDS-005.
           MOVE WS-DATA TO DATA-RATE.
           PERFORM CDS-015.

           IF DAT-RATE (1) NOT = "{"
                MOVE 0 TO F-ERROR5
                GO TO ONF-999.

           MOVE WS-DATA          TO F-FILENAME
           MOVE SUB-10           TO F-CBFILENAME.
           CALL "OPENFILE" USING   F-ERROR5
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE.

           IF F-ERROR5 NOT = 0
            IF F-ERROR5 NOT = 220
               GO TO ONF-999.

           IF F-ERROR5 NOT = 210
               CALL "CLOSEFILE" USING  F-ERROR5
                                       F-FH.
       ONF-999.
            EXIT.
      *
       OPEN-DATA SECTION.
       OPEN-DATA-001.
            PERFORM OPEN-NODE-FILE.
            IF F-ERROR5 NOT = 0 AND NOT = 220
                PERFORM ERROR1-020
                MOVE
          "THE DIGINET LINK IS NOT CURRENTLY RUNNING, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE "B" TO WS-PASSWORD-VALID.
       OPEN-DATA-005.
           IF F-ERROR5 = 0 OR = 220
            OPEN I-O DATA-FILE
           IF WS-DATA-ST1 NOT = 0
               MOVE "DATAFILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DATA-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE WS-DATA TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-DATA-005.
       OPEN-DATA-999.
           EXIT.
      *
       SELECT-BRANCH SECTION.
       SB-005.
           MOVE 0610 TO POS
           DISPLAY "STOCK ORDER PLACED ON COMPANY No ? : [  ]"
           AT POS
           ADD 38 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 3         TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMPANY-UPDATE.

      *     ACCEPT WS-COMPANY-UPDATE AT POS.
           IF W-ESCAPE-KEY = 3
              EXIT PROGRAM.
           IF WS-COMPANY-UPDATE = " "
               GO TO SB-005.
           MOVE WS-COMPANY-UPDATE TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO SUB-40.
           DISPLAY SUB-40 AT POS.
           IF LIST-NAME (SUB-40) = " "
               MOVE "PLEASE ENTER A COMPANY THAT EXISTS, TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO SB-005.
           PERFORM ERROR-020.
           IF LIST-NAME (SUB-40) = WS-CO-NUMBER
               MOVE "YOU MAY NOT UPDATE YOUR OWN COMPANY, TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO SB-005.
           MOVE 0810 TO POS
           DISPLAY"Company Selected :" AT POS
           ADD 19 TO POS
           DISPLAY LIST-NAME (SUB-40) AT POS
           MOVE 0929 TO POS
           DISPLAY LIST-VOL-DIR (SUB-40) AT POS.
       SB-020.
           MOVE 1010 TO POS
           DISPLAY "ARE YOU SURE THIS IS THE COMPANY TO UPDATE [ ]"
           AT POS
           ADD 44 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-UPDATE.

      *  ACCEPT WS-UPDATE AT POS.
        IF W-ESCAPE-KEY = 4
               GO TO SB-005.
        IF WS-UPDATE NOT = "N" AND NOT = "Y"
              MOVE "THIS ENTRY MUST BE EITHER Y OR N, RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO SB-020.
           PERFORM ERROR-020.
           IF WS-UPDATE = "N"
              GO TO SB-005.
       SB-999.
           EXIT.
      *
       READ-NEXT-COMPANY SECTION.
       RNC-005.
           OPEN I-O COMPANY-MENU.
           IF WS-MENU-ST1 NOT = 0
               MOVE "COMPANY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-MENU-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-MENU-ST1
               GO TO RNC-005.
           MOVE 1 TO PTY-NUMBER.
           START COMPANY-MENU KEY NOT < PTY-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-MENU-ST1 NOT = 0
               GO TO RNC-900.
           MOVE 1 TO SUB-40.
       RNC-010.
           READ COMPANY-MENU NEXT
               AT END NEXT SENTENCE.
           IF WS-MENU-ST1 = 10
               GO TO RNC-900.
           IF WS-MENU-ST1 NOT = 0
               MOVE "COMPANY FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-MENU-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-MENU-ST1
               GO TO RNC-010.
           MOVE PTY-VOL-DIR TO LIST-VOL-DIR (SUB-40)
           MOVE PTY-NUMBER  TO LIST-NUMBER (SUB-40)
           MOVE PTY-CO-NAME TO LIST-NAME (SUB-40).
           
           IF SUB-40 < 20
              ADD 1 TO SUB-40
              GO TO RNC-010.
       RNC-900.
           CLOSE COMPANY-MENU.
       RNC-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O PARAMETER-FILE.
           PERFORM READ-PARAMETER.
           MOVE PA-NAME TO CO-NAME.
           MOVE PA-GST-PERCENT TO WS-GST-PERCENT.
           PERFORM GET-SYSTEM-Y2K-DATE.
       OPEN-010.
           MOVE ALL "X" TO STORE-TERM
                           STORE-DEL.
           PERFORM READ-TERMS-FILE.
           PERFORM READ-DELIVERY-FILE.
           PERFORM READ-INVQUES-FILE.
           CLOSE PARAMETER-FILE.
       OPEN-0000.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-0000.
      *      MOVE WS-STOCK TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-002.
            OPEN I-O OUTSTANDING-ORDERS.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO WS-OUTORD-ST1
               MOVE "ST-ORDERS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
      *      MOVE Ws-StOrders TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-004.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-004.
      *         MOVE Ws-Debtor TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE.
       OPEN-005.
            OPEN I-O STOCK-TRANS-FILE.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE 0 TO WS-STTRANS-ST1
               MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
      *      MOVE WS-STTRANS TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-014.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
      *      MOVE WS-PARAMETER TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-016.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0 
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-016.
      *      MOVE WS-REGISTER TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-019.
            OPEN I-O STDISC-MASTER.
            IF WS-STDISC-ST1 NOT = 0
               MOVE 0 TO WS-STDISC-ST1
               MOVE "STOCK SPEC-DISC BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-019.
      *      MOVE WS-STDISCACC TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER
                  PARAMETER-FILE
                  DEBTOR-MASTER
                  INCR-REGISTER
                  STDISC-MASTER
                  STOCK-TRANS-FILE
                  OUTSTANDING-ORDERS.
       END-900.
            EXIT PROGRAM.
       END-999.
           EXIT.
      *      
       Copy "GetSystemY2KDate".
       Copy "GetUserMailName".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
