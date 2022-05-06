        IDENTIFICATION DIVISION.
        PROGRAM-ID. StOrGnRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStOrders".
         Copy "SelectStMaster".
         Copy "SelectSlParameter".
         Copy "SelectStOrderGen".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdOutOrd.
           COPY ChlfdStock.
           COPY ChlfdParam.
           COPY ChlfdOrderGen.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(255).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-SUPPLIER          PIC X(7) VALUE " ".
       77  WS-LINE              PIC 9(3) VALUE 66.
       77  WS-LINE-ANSWER       PIC X VALUE " ".
       77  WS-CHECK-ORDERS      PIC X VALUE " ".
       77  WS-DEL               PIC X VALUE " ".
       77  WS-LINECNT           PIC 9(3) VALUE 0.
       77  WS-ANSWER            PIC X(20) VALUE " ".
       77  WS-SEA-AIR-ORDER     PIC X VALUE " ".
       77  WS-VALIDORDER        PIC X VALUE " ".
       77  WS-VALID-LINES-ONLY  PIC X VALUE " ".
       77  WS-USE-MIN-BUY-QTY   PIC X VALUE " ".
       77  WS-TEMPORD           PIC X VALUE " ".
       77  WS-RANGE1            PIC X(10) VALUE " ".
       77  WS-RANGE2            PIC X(10) VALUE " ".
       77  WS-BOX-ORDERED       PIC 9(5) VALUE 0.
       77  WS-QTY               PIC S9(5) VALUE 0.
       77  WS-ORDERVALUE        PIC 9(7)V99 VALUE 0.
       77  WS-ORDERSEA          PIC 9(7)V99 VALUE 0.
       77  WS-AVE               PIC 9(5)V99 VALUE 0.
       77  WS-ORDER-URGENT      PIC S9(5) VALUE 0.
       77  WS-ORDER-SEA         PIC S9(5) VALUE 0.
       77  WS-RECEIVE-SOON      PIC 9(5) VALUE 0.
       77  WS-RECEIVE-LATER     PIC 9(5) VALUE 0.
       77  WS-TODAY             PIC 9(4) VALUE 0.
       77  WS-DUEDAY            PIC 9(4) VALUE 0.
       77  WS-TERM-SUB          PIC 9 VALUE 0.    
       77  WS-DEL-SUB           PIC 9 VALUE 0.    
       01  SPLIT-DELIVERVIA.
           03  WSDE-CODE        PIC X VALUE " ".
           03  WSDE-REST        PIC X(19) VALUE " ".
       01  STORE-DEL.
         02  WS-DEL-OCCUR OCCURS 10.
           03  WS-DEL-TYPE       PIC X.
           03  WS-DEL-CODE       PIC X.
           03  WS-DEL-TERM       PIC X(20).
       01  WS-BOX-WHOLE.
           03  WS-BOX-ORDER     PIC 9(6)V99.
           03  FILLER REDEFINES WS-BOX-ORDER.
               05  B-RATE       PIC 9 OCCURS 8.
       01  WS-WORKFIELD-NAMES.
         02  WS-WORKFIELDS OCCURS 20.
           03  WS-ORDER        PIC X(20).
           03  WS-ORDERQTY     PIC 9(5).
           03  WS-SHIPVIA      PIC X(20).
           03  WS-DELVIA       PIC 9.
           03  WS-DUEDATE      PIC 9(8).
           03  WS-SUPPLIED-BY  PIC X(7).
           03  WS-SUPPLY-NAME  PIC X(7).
           03  WS-SUPPLY-COUNT PIC 999.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1      PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-GEN-STATUS.
           03  WS-GEN-ST1         PIC 99.
       01  SPLIT1-DATE.
           03  SPLIT1-YY        PIC 9999.
           03  SPLIT1-MM        PIC 99.
           03  SPLIT1-DD        PIC 99.
       01  WS1-DATE.
           03  WS1-DD           PIC 99.
           03  WS1-MM           PIC 99.
           03  WS1-YY           PIC 9999.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE "  DATE".
           03  H-DATE           PIC X(10).
           03  FILLER           PIC X(22) VALUE " ".
           03  FILLER           PIC X(40) VALUE 
           "S T O C K   O R D E R   R E P O R T".
           03  H-TYPE           PIC X(42) VALUE " ".
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC ZZ9.
           03  FILLER           PIC X(13) VALUE " ".
       01  HEAD2.
           03  FILLER           PIC X(39) VALUE " ".
           03  FILLER           PIC X(40) VALUE 
           "***********************************".
           03  FILLER           PIC X(53) VALUE " ".
       01  HEAD3.
           03  FILLER           PIC X(86) VALUE " ".
           03  H3-ORDER1        PIC X(5) VALUE "PO-1".
           03  H3-ORDER2        PIC X(5) VALUE "PO-2".
           03  H3-ORDER3        PIC X(5) VALUE "PO-3".
           03  H3-ORDER4        PIC X(5) VALUE "PO-4".
           03  H3-ORDER5        PIC X(5) VALUE "PO-5".
           03  H3-ORDER6        PIC X(5) VALUE "PO-6".
           03  H3-ORDER7        PIC X(5) VALUE "PO-7".
           03  H3-ORDER8        PIC X(5) VALUE "PO-8".
           03  H3-ORDER9        PIC X(5) VALUE "PO-9".
           03  H3-ORDER10       PIC X(5) VALUE "PO10".
           03  H3-ORDER11       PIC X(5) VALUE "PO11".
           03  H3-ORDER12       PIC X(5) VALUE "PO12".
           03  H3-ORDER13       PIC X(5) VALUE "PO13".
           03  H3-ORDER14       PIC X(5) VALUE "PO14".
           03  H3-ORDER15       PIC X(5) VALUE "PO15".
           03  H3-ORDER16       PIC X(5) VALUE "PO16".
           03  H3-ORDER17       PIC X(5) VALUE "PO17".
           03  H3-ORDER18       PIC X(5) VALUE "PO18".
           03  H3-ORDER19       PIC X(5) VALUE "PO19".
           03  H3-ORDER20       PIC X(5) VALUE "PO20".
       01  HEAD4.
           03  FILLER           PIC X(29) VALUE " ".
           03  FILLER           PIC X(103) VALUE
           "Level        Stock Qty On         Sales     O r d e r".
       01  HEAD5.
           03  FILLER           PIC X(2) VALUE " ".
           03  FILLER           PIC X(17) VALUE "Stock Number".
           03  FILLER           PIC X(41) VALUE
           "MinBuy  Max   Min   Hand  Ordr     BO".
           03  FILLER           PIC X(28) VALUE
           "Ptd    Ytd  Urgnt  Sea".
           03  FILLER           PIC X(54) VALUE " ".
       01  DETAIL-LINE.
           03  D-DELETE         PIC X VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  D-STOCK          PIC X(15) VALUE " ".
           03  FILLER           PIC X(1) VALUE " ".
           03  D-MINBUY         PIC Z(5)9.
           03  D-MAX            PIC Z(5)9.
           03  D-MIN            PIC Z(5)9.
           03  FILLER           PIC X VALUE " ".
           03  D-ONHAND         PIC Z(5)9.
           03  D-ORDER          PIC Z(5)9.
           03  D-ORD-ERR        PIC X.
           03  D-BO             PIC Z(5)9.
           03  FILLER           PIC X VALUE " ".
           03  D-PTD            PIC Z(5)9-.
           03  D-YTD            PIC Z(5)9-.
           03  FILLER           PIC X VALUE " ".
           03  D-URGENT-ORDER   PIC Z(4)9.
           03  D-SEA-ORDER      PIC Z(4)9.
           03  FILLER           PIC X(3) VALUE " ".
           03  D-ORDER1         PIC Z(4)9.
           03  D-ORDER2         PIC Z(4)9.
           03  D-ORDER3         PIC Z(4)9.
           03  D-ORDER4         PIC Z(4)9.
           03  D-ORDER5         PIC Z(4)9.
           03  D-ORDER6         PIC Z(4)9.
           03  D-ORDER7         PIC Z(4)9.
           03  D-ORDER8         PIC Z(4)9.
           03  D-ORDER9         PIC Z(4)9.
           03  D-ORDER10        PIC Z(4)9.
           03  D-ORDER11        PIC Z(4)9.
           03  D-ORDER12        PIC Z(4)9.
           03  D-ORDER13        PIC Z(4)9.
           03  D-ORDER14        PIC Z(4)9.
           03  D-ORDER15        PIC Z(4)9.
           03  D-ORDER16        PIC Z(4)9.
           03  D-ORDER17        PIC Z(4)9.
           03  D-ORDER18        PIC Z(4)9.
           03  D-ORDER19        PIC Z(4)9.
           03  D-ORDER20        PIC Z(4)9.
       01  SUB-TOTAL-LINE.
           03  FILLER           PIC X(20) VALUE " ".
           03  FILLER           PIC X(33) VALUE
           " Approx. Value Of Urgent Order: R".
           03  P-SUB-CNT        PIC Z(7)9.99.
           03  FILLER           PIC X(68) VALUE " ".
       01  SUB1-TOTAL-LINE.
           03  FILLER           PIC X(24) VALUE " ".
           03  FILLER           PIC X(29) VALUE
           "Approx. Value Of Sea Order: R".
           03  P-SUB1-CNT       PIC Z(7)9.99.
           03  FILLER           PIC X(68) VALUE " ".
       01  ORDER-NO-LINE.
           03  FILLER              PIC X(10) VALUE "Order No:".
           03  H-05LINE.
               05  H-ORDERNO       PIC Z9.
               05  FILLER          PIC X(5) VALUE " ".
               05  H-ORDER         PIC X(25) VALUE " ".
               05  H-DUEDATE       PIC X(10).
               05  FILLER          PIC X(5) VALUE " ".
               05  H-SHIPVIA       PIC X(20) VALUE " ".
               05  FILLER          PIC X(6) VALUE " ".
               05  H-SUPPLIER      PIC X(20) VALUE " ".
               05  FILLER          PIC X(31) VALUE " ".
       01  SUPPLIER-LINE.
           03  FILLER              PIC X(25) VALUE
               "ONLY ITEMS FOR SUPPLIER:".
           03  S-SUPPLIER          PIC X(107) VALUE " ".
       01  SUPPLIER-NAME-LINE.
           03  S-SUPPLY-NAME       PIC X(20).
           03  S-SUPPLY-COUNT      PIC Z(2)9.
           03  FILLER              PIC X(113) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 320 TO POS
           DISPLAY "***** Stock Order Report *****" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H-DATE.
           PERFORM SDFO-010.
       CONTROL-030.
           MOVE 810 TO POS.    
           DISPLAY "Enter The Beginning Stock No To Print [          ]"
                     AT POS.
           MOVE 849 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 48        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-030.
       CONTROL-040.
           MOVE 910 TO POS.    
           DISPLAY "   Enter The Ending Stock No To Print [          ]"
                     AT POS.
           MOVE 949 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 6         TO CDA-ROW.
           MOVE 48        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-030.
           IF WS-RANGE2 = " "
               GO TO CONTROL-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-041
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-040.
       CONTROL-041.
           MOVE 1010 TO POS.    
           DISPLAY "E=Enter Orders, A=Auto-Checking, B=Branch:[ ]"
                AT POS.
           MOVE 1053 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CHECK-ORDERS.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-040.
           IF WS-CHECK-ORDERS NOT = "A" AND NOT = "E" AND NOT = "B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-041.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-045
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-041.
       CONTROL-045.
           IF WS-CHECK-ORDERS = "A" OR = "B"
                GO TO CONTROL-900.
           MOVE 1110 TO POS.
           DISPLAY "You may enter a MAXIMUM of 12 SUPPLIER ORDERS."
               AT POS.
       CONTROL-050.
           MOVE 1210 TO POS.
           DISPLAY "Enter Order No  1. [                    ]" AT POS.
           MOVE 1230 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           ACCEPT WS-ANSWER AT POS.
           IF W-ESCAPE-KEY = 4
               MOVE 0 TO SUB-1 SUB-2
               GO TO CONTROL-040.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-055
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-050.
       CONTROL-055.
           IF WS-ANSWER = " "
             MOVE 1 TO SUB-1
             MOVE 0 TO SUB-2
             MOVE WS-ANSWER TO WS-ORDER (SUB-1)
             GO TO CONTROL-900.
           MOVE 1 TO SUB-1 SUB-2.
           MOVE WS-ANSWER TO WS-ORDER (SUB-1).
           MOVE " " TO WS-ANSWER.
       CONTROL-060.
           MOVE 1310 TO POS.
           DISPLAY "Enter Order No  2. [                    ]" AT POS.
           MOVE 1330 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               MOVE 1 TO SUB-1 SUB-2
               GO TO CONTROL-050.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-065
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-060.
       CONTROL-065.
           IF WS-ANSWER = " "
             GO TO CONTROL-900.
           ADD 1 TO SUB-1 SUB-2.
           MOVE WS-ANSWER TO WS-ORDER (SUB-1)
           MOVE " " TO WS-ANSWER.
       CONTROL-070.
           MOVE 1410 TO POS.
           DISPLAY "Enter Order No  3. [                    ]" AT POS.
           MOVE 1430 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               MOVE 1 TO SUB-1 SUB-2
               GO TO CONTROL-060.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-075
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-070.
       CONTROL-075.
           IF WS-ANSWER = " "
             GO TO CONTROL-900.
           ADD 1 TO SUB-1 SUB-2.
           MOVE WS-ANSWER TO WS-ORDER (SUB-1)
           MOVE " " TO WS-ANSWER.
       CONTROL-080.
           MOVE 1510 TO POS.
           DISPLAY "Enter Order No  4. [                    ]" AT POS.
           MOVE 1530 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               MOVE 2 TO SUB-1 SUB-2
               GO TO CONTROL-070.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-085
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-080.
       CONTROL-085.
           IF WS-ANSWER = " "
             GO TO CONTROL-900.
           ADD 1 TO SUB-1 SUB-2.
           MOVE WS-ANSWER TO WS-ORDER (SUB-1)
           MOVE " " TO WS-ANSWER.
       CONTROL-090.
           MOVE 1610 TO POS.
           DISPLAY "Enter Order No  5. [                    ]" AT POS.
           MOVE 1630 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               MOVE 3 TO SUB-1 SUB-2
               GO TO CONTROL-080.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-095
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-090.
       CONTROL-095.
           IF WS-ANSWER = " "
             GO TO CONTROL-900.
           ADD 1 TO SUB-1 SUB-2.
           MOVE WS-ANSWER TO WS-ORDER (SUB-1)
           MOVE " " TO WS-ANSWER.
       CONTROL-100.
           MOVE 1710 TO POS.
           DISPLAY "Enter Order No  6. [                    ]" AT POS.
           MOVE 1730 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               MOVE 4 TO SUB-1 SUB-2
               GO TO CONTROL-090.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-105
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-100.
       CONTROL-105.
           IF WS-ANSWER = " "
             GO TO CONTROL-900.
           ADD 1 TO SUB-1 SUB-2.
           MOVE WS-ANSWER TO WS-ORDER (SUB-1)
           MOVE " " TO WS-ANSWER.
       CONTROL-150.
           MOVE 1810 TO POS.
           DISPLAY "Enter Order No  7. [                    ]" AT POS.
           MOVE 1830 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               MOVE 5 TO SUB-1 SUB-2
               GO TO CONTROL-100.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-155
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-150.
       CONTROL-155.
           IF WS-ANSWER = " "
             GO TO CONTROL-900.
           ADD 1 TO SUB-1 SUB-2.
           MOVE WS-ANSWER TO WS-ORDER (SUB-1)
           MOVE " " TO WS-ANSWER.
       CONTROL-160.
           MOVE 1910 TO POS.
           DISPLAY "Enter Order No  8. [                    ]" AT POS.
           MOVE 1930 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               MOVE 6 TO SUB-1 SUB-2
               GO TO CONTROL-150.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-165
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-160.
       CONTROL-165.
           IF WS-ANSWER = " "
             GO TO CONTROL-900.
           ADD 1 TO SUB-1 SUB-2.
           MOVE WS-ANSWER TO WS-ORDER (SUB-1)
           MOVE " " TO WS-ANSWER.
       CONTROL-170.
           MOVE 2010 TO POS.
           DISPLAY "Enter Order No  9. [                    ]" AT POS.
           MOVE 2030 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               MOVE 7 TO SUB-1 SUB-2
               GO TO CONTROL-160.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-175
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-170.
       CONTROL-175.
           IF WS-ANSWER = " "
             GO TO CONTROL-900.
           ADD 1 TO SUB-1 SUB-2.
           MOVE WS-ANSWER TO WS-ORDER (SUB-1)
           MOVE " " TO WS-ANSWER.
       CONTROL-180.
           MOVE 2110 TO POS.
           DISPLAY "Enter Order No 10. [                    ]" AT POS.
           MOVE 2130 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 18        TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               MOVE 8 TO SUB-1 SUB-2
               GO TO CONTROL-170.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-185
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-180.
       CONTROL-185.
           IF WS-ANSWER = " "
             GO TO CONTROL-900.
           ADD 1 TO SUB-1 SUB-2.
           MOVE WS-ANSWER TO WS-ORDER (SUB-1)
           MOVE " " TO WS-ANSWER.
       CONTROL-190.
           MOVE 2210 TO POS.
           DISPLAY "Enter Order No 11. [                    ]" AT POS.
           MOVE 2230 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 19        TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               MOVE 9 TO SUB-1 SUB-2
               GO TO CONTROL-180.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-195
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-190.
       CONTROL-195.
           IF WS-ANSWER = " "
             GO TO CONTROL-900.
           ADD 1 TO SUB-1 SUB-2.
           MOVE WS-ANSWER TO WS-ORDER (SUB-1)
           MOVE " " TO WS-ANSWER.
       CONTROL-200.
           MOVE 2310 TO POS.
           DISPLAY "Enter Order No 12. [                    ]" AT POS.
           MOVE 2330 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 20        TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               MOVE 10 TO SUB-1 SUB-2
               GO TO CONTROL-190.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-205
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-200.
       CONTROL-205.
           IF WS-ANSWER = " "
             GO TO CONTROL-900.
           ADD 1 TO SUB-1 SUB-2.
           MOVE WS-ANSWER TO WS-ORDER (SUB-1)
           MOVE " " TO WS-ANSWER.
       CONTROL-900.
           IF WS-CHECK-ORDERS = "B"
               MOVE "L" TO WS-SEA-AIR-ORDER
               GO TO CONTROL-1000.
           MOVE 2410 TO POS.
           DISPLAY "S=Sea, A=Air, B=Air + Sea, L=Local Supplier: [ ]"
             AT POS.
           ADD 46 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 21        TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SEA-AIR-ORDER.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-200.
           IF WS-SEA-AIR-ORDER NOT = "A" AND NOT = "B"
                           AND NOT = "L" AND NOT = "S"
               MOVE 3010 TO POS
               DISPLAY "YOUR ANSWER MUST BE EITHER A, B, L OR S."
               AT POS
               GO TO CONTROL-900.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-1000
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-900.
       CONTROL-1000.
           MOVE 2510 TO POS.
           DISPLAY "Enter a SUPPLIER NAME, leave BLANK to print ALL."
              AT POS.
           MOVE 2558 TO POS.
           DISPLAY " : [       ]" AT POS.
           MOVE 2562 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 22        TO CDA-ROW.
           MOVE 61        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SUPPLIER.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-900.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-1005
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-1000.
       CONTROL-1005.
           MOVE 2610 TO POS.
           DISPLAY "Do you wish to create a TEMPORARY Order-File."
              AT POS.
           MOVE 2658 TO POS.
           DISPLAY " : [ ]" AT POS.
           MOVE 2662 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 23        TO CDA-ROW.
           MOVE 61        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TEMPORD.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-1000.
           IF WS-TEMPORD NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-1005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO CONTROL-1020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-1005.
       CONTROL-1020.
           MOVE 2710 TO POS.
           DISPLAY "Print ONLY items placed ON-ORDER, Y or N:    "
              AT POS.
           MOVE 2758 TO POS.
           DISPLAY " : [ ]" AT POS.
           MOVE 2762 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 24        TO CDA-ROW.
           MOVE 61        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-VALID-LINES-ONLY.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-1005.
           IF WS-VALID-LINES-ONLY NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-1020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO CONTROL-1030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-1020.
       CONTROL-1030.
           MOVE 2810 TO POS.
           DISPLAY "If ST-ANALYSIS=S, Use MIN-BUY-QTY FOR ORDER-QTY?"
              AT POS.
           MOVE 2858 TO POS.
           DISPLAY " : [ ]" AT POS.
           MOVE 2862 TO POS.

           MOVE 'N'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 25        TO CDA-ROW.
           MOVE 61        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-USE-MIN-BUY-QTY.
           
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-1020.
           IF WS-USE-MIN-BUY-QTY NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-1030.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO CONTROL-1050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-1030.
       CONTROL-1050.
           PERFORM OPEN-FILES.
           IF WS-CHECK-ORDERS = "A"
               PERFORM FIND-ORDERS-OFF-SYSTEM.
           PERFORM ERROR-020.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM GET-HEADING-DATA.
           IF WS-CHECK-ORDERS = "B"
              PERFORM PRINT-BRANCH-ROUTINE
           ELSE
              PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-HEADING-DATA SECTION.
       GET-000.
           IF WS-SEA-AIR-ORDER = "A"
              MOVE "AIR ORDER ONLY CALCULATED " TO H-TYPE.
           IF WS-SEA-AIR-ORDER = "B"
              MOVE "AIR & SEA ORDER CALCULATED" TO H-TYPE.
           IF WS-SEA-AIR-ORDER = "L"
              MOVE "**LOCAL ORDER CALCULATED**" TO H-TYPE.
           IF WS-SEA-AIR-ORDER = "S"
              MOVE "SEA ORDER ONLY CALCULATED"  TO H-TYPE.
           MOVE 0 TO SUB-1.
       GET-001.
           ADD 1 TO SUB-1.
           IF WS-ORDER (SUB-1) = " "
              PERFORM PRINT-HEADINGS
              GO TO GET-999.
       GET-007.
           PERFORM READ-OUTSTANDING-ORDERS.
           MOVE OO-DUEDATE         TO WS-DUEDATE (SUB-1).
           MOVE OO-DELIVERY-METHOD TO WS-DEL
                                      WS-DELVIA (SUB-1).
           MOVE WS-DEL    TO SPLIT-DELIVERVIA.
           MOVE WSDE-CODE TO WS-DEL-SUB.
           IF WS-DEL-SUB = 0
              MOVE "ERROR IN P/O NUMBER" TO WS-SHIPVIA (SUB-1)
              GO TO GET-010.
           MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-SHIPVIA (SUB-1).
           MOVE OO-SUPPLIER-NUMBER       TO WS-SUPPLIED-BY (SUB-1).
        GET-010.
           IF SUB-1 = 20
              PERFORM PRINT-HEADINGS
              GO TO GET-999.
           GO TO GET-001.
           MOVE 0 TO WS-DEL.
       GET-999.
           EXIT.
      *
       READ-OUTSTANDING-ORDERS SECTION.
       ROO-005.
           MOVE WS-ORDER (SUB-1) TO OO-ORDER-NUMBER.
           MOVE WS-RANGE1        TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
              MOVE 0 TO OO-QUANTITY
                        OO-DUEDATE
                        OO-DELIVERY-METHOD
              GO TO ROO-999.
       ROO-020.
           READ OUTSTANDING-ORDERS NEXT
                AT END NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 10
              MOVE 0 TO WS-OUTORD-ST1
              MOVE 0 TO OO-QUANTITY
                        OO-DUEDATE
                        OO-DELIVERY-METHOD
              GO TO ROO-999.
           IF WS-OUTORD-ST1 NOT = 0
              MOVE "S-ORDER FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO ROO-020.
           IF OO-ORDER-NUMBER NOT = WS-ORDER (SUB-1)
              GO TO ROO-020.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
       ROO-999.
           EXIT.
      *
       FIND-ORDERS-OFF-SYSTEM SECTION.
       CFOOS-005.
           MOVE 2310 TO POS.
           DISPLAY "Getting Order Numbers Off The System..." AT POS.
           MOVE WS-RANGE1 TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-STOCK-NUMBER
              INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
              GO TO CFOOS-900.
       CFOOS-020.
           READ OUTSTANDING-ORDERS NEXT
                AT END NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 10
              MOVE 0 TO WS-OUTORD-ST1
              MOVE 0 TO OO-QUANTITY
                        OO-DUEDATE
                        OO-DELIVERY-METHOD
              GO TO CFOOS-900.
           IF WS-OUTORD-ST1 NOT = 0
              MOVE "S/ORDER FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO CFOOS-020.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           IF OO-STOCK-NUMBER < WS-RANGE1
              GO TO CFOOS-020.
           IF OO-STOCK-NUMBER > WS-RANGE2
              GO TO CFOOS-900.
           IF OO-QUANTITY = 0
              GO TO CFOOS-020.
           IF WS-SUPPLIER NOT = " "
            IF OO-SUPPLIER-NUMBER NOT = WS-SUPPLIER
              GO TO CFOOS-020.
       CFOOS-030.
           MOVE 1 TO SUB-3.
       CFOOS-040.
           IF SUB-3 > 20
              GO TO CFOOS-900.
           IF OO-ORDER-NUMBER = WS-ORDER (SUB-3)
              GO TO CFOOS-020.
           IF OO-ORDER-NUMBER NOT = WS-ORDER (SUB-3)
            IF WS-ORDER (SUB-3) NOT = " "
              ADD 1 TO SUB-3
              GO TO CFOOS-040.
           MOVE OO-ORDER-NUMBER TO WS-ORDER (SUB-3).
       CFOOS-050.
           IF SUB-3 = 1
              MOVE 1110 TO POS
              DISPLAY "Order # 1. [                    ]" AT POS
              MOVE 1122 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 2
              MOVE 1210 TO POS
              DISPLAY "Order # 2. [                    ]" AT POS
              MOVE 1222 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 3
              MOVE 1310 TO POS
              DISPLAY "Order # 3. [                    ]" AT POS
              MOVE 1322 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 4
              MOVE 1410 TO POS
              DISPLAY "Order # 4. [                    ]" AT POS
              MOVE 1422 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 5
              MOVE 1510 TO POS
              DISPLAY "Order # 5. [                    ]" AT POS
              MOVE 1522 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 6
              MOVE 1610 TO POS
              DISPLAY "Order # 6. [                    ]" AT POS
              MOVE 1622 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 7
              MOVE 1710 TO POS
              DISPLAY "Order # 7. [                    ]" AT POS
              MOVE 1722 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 8
              MOVE 1810 TO POS
              DISPLAY "Order # 8. [                    ]" AT POS
              MOVE 1822 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 9
              MOVE 1910 TO POS
              DISPLAY "Order # 9. [                    ]" AT POS
              MOVE 1922 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 10
              MOVE 2010 TO POS
              DISPLAY "Order #10. [                    ]" AT POS
              MOVE 2022 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 11
              MOVE 1145 TO POS
              DISPLAY "Order #11. [                    ]" AT POS
              MOVE 1157 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 12
              MOVE 1245 TO POS
              DISPLAY "Order #12. [                    ]" AT POS
              MOVE 1257 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 13
              MOVE 1345 TO POS
              DISPLAY "Order #13. [                    ]" AT POS
              MOVE 1357 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 14
              MOVE 1445 TO POS
              DISPLAY "Order #14. [                    ]" AT POS
              MOVE 1457 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 15
              MOVE 1545 TO POS
              DISPLAY "Order #15. [                    ]" AT POS
              MOVE 1557 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 16
              MOVE 1645 TO POS
              DISPLAY "Order #16. [                    ]" AT POS
              MOVE 1657 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 17
              MOVE 1745 TO POS
              DISPLAY "Order #17. [                    ]" AT POS
              MOVE 1757 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 18
              MOVE 1845 TO POS
              DISPLAY "Order #18. [                    ]" AT POS
              MOVE 1857 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 19
              MOVE 1945 TO POS
              DISPLAY "Order #19. [                    ]" AT POS
              MOVE 1957 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 = 20
              MOVE 2045 TO POS
              DISPLAY "Order #20. [                    ]" AT POS
              MOVE 2057 TO POS
              DISPLAY WS-ORDER (SUB-3) AT POS.
           IF SUB-3 > 20
              MOVE 2252 TO POS
              DISPLAY "+ More Orders found.." AT POS.

           GO TO CFOOS-020.
       CFOOS-900.
           CLOSE OUTSTANDING-ORDERS.
           PERFORM OPEN-001.
           MOVE 1 TO SUB-3.
       CFOOS-950.
           IF WS-ORDER (SUB-3) = " "
               GO TO CFOOS-960.
           ADD 1 TO SUB-3.
           IF SUB-3 < 20
               GO TO CFOOS-950.
       CFOOS-960.
           SUBTRACT 1 FROM SUB-3.
           MOVE SUB-3 TO SUB-2.
           PERFORM ERROR-020.
           IF SUB-3 < 10
              MOVE " " TO H3-ORDER10 H3-ORDER11 H3-ORDER12 H3-ORDER13
               H3-ORDER14 H3-ORDER15 H3-ORDER16 H3-ORDER17 H3-ORDER18
               H3-ORDER19 H3-ORDER20.
           MOVE 2310 TO POS.
           DISPLAY WS-MESSAGE AT POS.
       CFOOS-999.
           EXIT.
      *
       READ-OUT-ORDERS SECTION.
       ROS-005.
           MOVE WS-ORDER (SUB-1) TO OO-ORDER-NUMBER.
           MOVE ST-STOCKNUMBER   TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY.
       ROS-010.
           READ OUTSTANDING-ORDERS
                INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-OUTORD-ST1
              MOVE 0 TO OO-QUANTITY
                        WS-QTY
                        WS-ORDERQTY (SUB-1)
              GO TO ROS-999.
           IF WS-OUTORD-ST1 NOT = 0
              MOVE "ORDERS FILE BUSY ON READ, 'ESC' TO RETRY."
                 TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO ROS-010.
              
           MOVE OO-QUANTITY TO WS-QTY
                               WS-ORDERQTY (SUB-1).
       ROS-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           MOVE WS-RANGE1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       PR-001.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               PERFORM SUB-LINE
               GO TO PR-900.
           IF WS-STOCK-ST1 NOT = 0
             MOVE "STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO PR-001.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           MOVE 2910 TO POS.
           DISPLAY "Stock-Number Being Read: " AT POS.
           ADD 25 TO POS.
           DISPLAY ST-STOCKNUMBER AT POS.
           MOVE " " TO D-DELETE.
       PR-005.
           IF ST-STOCKNUMBER < WS-RANGE1
               GO TO PR-001.
           IF ST-STOCKNUMBER > WS-RANGE2
               PERFORM SUB-LINE
               GO TO PR-900.
           IF WS-SUPPLIER = "       "
               GO TO PR-010.
           IF ST-SUPPLIER NOT = WS-SUPPLIER
               GO TO PR-001.
       PR-010.
           IF WS-LINE > 60
               PERFORM PRINT-HEADINGS.
       PR-020.
           MOVE ST-STOCKNUMBER    TO D-STOCK
           MOVE ST-MINBUYQTY      TO D-MINBUY
           MOVE ST-MAXIMUMLEVEL   TO D-MAX
           MOVE ST-MINIMUMLEVEL   TO D-MIN
           COMPUTE ST-QTYONHAND = ST-QTYONHAND + ST-QTYONRESERVE
           MOVE ST-QTYONHAND      TO D-ONHAND
           MOVE ST-QTYONORDER     TO D-ORDER
           MOVE ST-QTYONBORDER    TO D-BO
           MOVE ST-SALESUNITMTD   TO D-PTD
           MOVE ST-SALESUNITSYTD  TO D-YTD
           MOVE 0                 TO SUB-1.
       PR-021.
           ADD 1 TO SUB-1.
           IF SUB-1 > 20
              MOVE 0 TO SUB-1
              GO TO PR-025.
           MOVE 0 TO WS-ORDERQTY (SUB-1).
           GO TO PR-021.
       PR-025.
           ADD 1 TO SUB-1.
           IF SUB-1 > SUB-2
              GO TO PR-030.
           IF SUB-1 > 20
              MOVE 20 TO SUB-1
              GO TO PR-030.
           PERFORM READ-OUT-ORDERS.
           
           IF SUB-1 = 1
              MOVE WS-QTY TO D-ORDER1.
           IF SUB-1 = 2
              MOVE WS-QTY TO D-ORDER2.
           IF SUB-1 = 3
              MOVE WS-QTY TO D-ORDER3.
           IF SUB-1 = 4
              MOVE WS-QTY TO D-ORDER4.
           IF SUB-1 = 5
              MOVE WS-QTY TO D-ORDER5.
           IF SUB-1 = 6
              MOVE WS-QTY TO D-ORDER6.
           IF SUB-1 = 7
              MOVE WS-QTY TO D-ORDER7.
           IF SUB-1 = 8
              MOVE WS-QTY TO D-ORDER8.
           IF SUB-1 = 9
              MOVE WS-QTY TO D-ORDER9.
           IF SUB-1 = 10
              MOVE WS-QTY TO D-ORDER10.
           IF SUB-1 = 11
              MOVE WS-QTY TO D-ORDER11.
           IF SUB-1 = 12
              MOVE WS-QTY TO D-ORDER12.
           IF SUB-1 = 13
              MOVE WS-QTY TO D-ORDER13.
           IF SUB-1 = 14
              MOVE WS-QTY TO D-ORDER14.
           IF SUB-1 = 15
              MOVE WS-QTY TO D-ORDER15.
           IF SUB-1 = 16
              MOVE WS-QTY TO D-ORDER16.
           IF SUB-1 = 17
              MOVE WS-QTY TO D-ORDER17.
           IF SUB-1 = 18
              MOVE WS-QTY TO D-ORDER18.
           IF SUB-1 = 19
              MOVE WS-QTY TO D-ORDER19.
           IF SUB-1 = 20
              MOVE WS-QTY TO D-ORDER20
              MOVE 0 TO WS-QTY
              GO TO PR-030.

           MOVE 0 TO WS-QTY.
           GO TO PR-025.
       PR-030.
           IF SUB-1 > 20
              MOVE 20 TO SUB-1.
           IF ST-ANALYSIS = "T" OR = "B"
              MOVE "T" TO D-DELETE.
           IF ST-ANALYSIS = "S"
              MOVE "S" TO D-DELETE.
           IF ST-ANALYSIS = "D"
              MOVE "D" TO D-DELETE
              MOVE 0 TO WS-ORDER-URGENT
                        WS-ORDER-SEA
                        D-URGENT-ORDER
                        D-SEA-ORDER
              GO TO PR-050.
           IF ST-ANALYSIS = "N"
              MOVE "N" TO D-DELETE
              MOVE 0   TO WS-ORDER-URGENT
                          WS-ORDER-SEA
                           D-URGENT-ORDER
                           D-SEA-ORDER
              GO TO PR-050.
           PERFORM QUESTION-TO-ORDER.
           MOVE WS-ORDER-URGENT TO D-URGENT-ORDER.
           MOVE WS-ORDER-SEA    TO D-SEA-ORDER.
           PERFORM ADD-TO-VALUE.
       PR-050.
           IF WS-VALID-LINES-ONLY = "Y"
            IF WS-ORDER-URGENT = 0
             IF WS-ORDER-SEA = 0
               GO TO PR-001.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           IF WS-TEMPORD = "Y"
              PERFORM WRITE-TEMP-ORDER.
           ADD 1 TO WS-LINE.
           GO TO PR-001.
       PR-900.
           IF SUB-3 > 9
              MOVE WS-PRINT-NORMAL TO PRINT-REC
              WRITE PRINT-REC.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       PR-999.
           EXIT.
      *
       PRINT-BRANCH-ROUTINE SECTION.
       PRBR-000.
           MOVE WS-RANGE1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       PRBR-001.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               PERFORM SUB-LINE
               GO TO PRBR-900.
           IF WS-STOCK-ST1 NOT = 0
             MOVE "STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRBR-001.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.

           MOVE 2310 TO POS.
           DISPLAY "Stock-Number Being Read: " AT POS.
           ADD 25 TO POS.
           DISPLAY ST-STOCKNUMBER AT POS.
           MOVE " " TO D-DELETE.
       PRBR-005.
           IF ST-STOCKNUMBER < WS-RANGE1
               GO TO PRBR-001.
           IF ST-STOCKNUMBER > WS-RANGE2
               PERFORM SUB-LINE
               GO TO PRBR-900.
           IF WS-SUPPLIER = "    "
               GO TO PRBR-010.
           IF ST-SUPPLIER NOT = WS-SUPPLIER
               GO TO PRBR-001.
       PRBR-010.
           IF WS-LINE > 60
               PERFORM PRINT-HEADINGS.
       PRBR-020.
           MOVE ST-STOCKNUMBER    TO D-STOCK
           MOVE ST-MINBUYQTY      TO D-MINBUY
           MOVE ST-MAXIMUMLEVEL   TO D-MAX
           MOVE ST-MINIMUMLEVEL   TO D-MIN
           COMPUTE ST-QTYONHAND = ST-QTYONHAND + ST-QTYONRESERVE
           MOVE ST-QTYONHAND      TO D-ONHAND
           MOVE ST-QTYONORDER     TO D-ORDER
           MOVE ST-QTYONBORDER    TO D-BO
           MOVE ST-SALESUNITMTD   TO D-PTD
           MOVE ST-SALESUNITSYTD  TO D-YTD.
           
           COMPUTE WS-QTY =
            ((ST-QTYONHAND + ST-QTYONORDER) - ST-QTYONBORDER).
           COMPUTE WS-RECEIVE-SOON ROUNDED =
                       ((ST-MAXIMUMLEVEL * 60) / 100).
                       
      * CHANGED 28/11/2006 SO THAT BRANCHES DON'T CARRY A LOT OF STOCK
      *                 ((ST-MAXIMUMLEVEL * 60) / 100).
           COMPUTE WS-ORDER-URGENT = WS-RECEIVE-SOON - WS-QTY.
           IF WS-ORDER-URGENT < 0
              MOVE 0 TO WS-ORDER-URGENT.
       PRBR-030.
           IF ST-ANALYSIS = "T" OR = "B"
              MOVE "T" TO D-DELETE.
           IF ST-ANALYSIS = "S"
              MOVE "S" TO D-DELETE.
           IF ST-ANALYSIS = "D"
              MOVE "D" TO D-DELETE
              MOVE 0 TO WS-ORDER-URGENT
                        WS-ORDER-SEA
                        D-URGENT-ORDER
                        D-SEA-ORDER
              GO TO PRBR-050.
           IF ST-ANALYSIS = "N"
              MOVE "N" TO D-DELETE
              MOVE 0   TO WS-ORDER-URGENT
                          WS-ORDER-SEA
                           D-URGENT-ORDER
                           D-SEA-ORDER
              GO TO PRBR-050.
           MOVE WS-ORDER-URGENT TO D-URGENT-ORDER.
           MOVE WS-ORDER-SEA    TO D-SEA-ORDER.
           PERFORM ADD-TO-VALUE.
       PRBR-050.
           IF WS-VALID-LINES-ONLY = "Y"
            IF WS-ORDER-URGENT = 0
             IF WS-ORDER-SEA = 0
               GO TO PRBR-001.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           IF WS-TEMPORD = "Y"
              PERFORM WRITE-TEMP-ORDER.
           ADD 1 TO WS-LINE.
           GO TO PRBR-001.
       PRBR-900.
           IF SUB-3 > 9
              MOVE WS-PRINT-NORMAL TO PRINT-REC
              WRITE PRINT-REC.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       PRBR-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           ADD 1        TO WS-PAGE.
           MOVE WS-PAGE TO H-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC
               GO TO PH-010.
           IF SUB-3 > 9
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC.
       PH-010.
           IF WS-PAGE = 1
              WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
              WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           WRITE PRINT-REC FROM HEAD5 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE 7 TO WS-LINE
           WRITE PRINT-REC AFTER 1.
       PH-999.
           EXIT.
      *
       WRITE-TEMP-ORDER SECTION.
       WTO-010.
           IF WS-ORDER-URGENT > 0
              MOVE ST-SUPPLIER     TO OG-SUPPLIER
              MOVE "A"             TO OG-SEA-AIR
              MOVE ST-STOCKNUMBER  TO OG-STOCK-NUMBER
              MOVE WS-ORDER-URGENT TO OG-QUANTITY
              PERFORM WTO-500.
           IF WS-ORDER-SEA > 0
              MOVE ST-SUPPLIER     TO OG-SUPPLIER
              MOVE "S"             TO OG-SEA-AIR
              MOVE ST-STOCKNUMBER  TO OG-STOCK-NUMBER
              MOVE WS-ORDER-SEA    TO OG-QUANTITY
              PERFORM WTO-500.
           GO TO WTO-999.
       WTO-500.
           WRITE ORDER-GEN-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-GEN-ST1 = 23 OR 35 OR 49
              REWRITE ORDER-GEN-REC.
           IF WS-GEN-ST1 NOT = 0
              MOVE "ORDER-GEN-REC BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GEN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GEN-ST1
               GO TO WTO-500.
       WTO-999.
           EXIT.
      *
       QUESTION-TO-ORDER SECTION.
       QTO-005.
           MOVE 0 TO WS-ORDER-URGENT
                     WS-ORDER-SEA
                     WS-RECEIVE-SOON
                     WS-RECEIVE-LATER
                     SUB-1.
       QTO-007.
           ADD 1 TO SUB-1.

           IF WS-ORDER (SUB-1) = " "
              GO TO QTO-0071.
           IF WS-ORDERQTY (SUB-1) = 0
            IF SUB-1 < 20
              GO TO QTO-007.
       QTO-0071.
      * NEW SECTION ADDED WHEN DUEDATE COULD BE = ZERO
      * THIS DECIDED 23/6/2001
           IF WS-DUEDATE (SUB-1) = 0
              MOVE 999 TO WS-DUEDAY
           ELSE
              PERFORM SDFO-020.

           IF WS-DELVIA (SUB-1) = 9 OR = 6
            IF WS-DUEDAY NOT = 999
              ADD WS-ORDERQTY (SUB-1) TO WS-RECEIVE-SOON
              GO TO QTO-008
            ELSE
              ADD WS-ORDERQTY (SUB-1) TO WS-RECEIVE-LATER
              GO TO QTO-008.
           IF WS-DUEDAY < WS-TODAY
              ADD WS-ORDERQTY (SUB-1) TO WS-RECEIVE-SOON
              GO TO QTO-008.
           IF WS-SEA-AIR-ORDER = "L"
              ADD WS-ORDERQTY (SUB-1) TO WS-RECEIVE-SOON
              GO TO QTO-008.
           IF WS-DELVIA (SUB-1) NOT = 9 AND NOT = 6 
            IF WS-DUEDAY > WS-TODAY
              ADD WS-ORDERQTY (SUB-1) TO WS-RECEIVE-LATER.
       QTO-008.
           IF SUB-1 > 19
              GO TO QTO-010.
           GO TO QTO-007.
       QTO-010.
      *********************************************************
      *NEXT TWO LINES TO CHECK TRANS WITH STOCK FILE AMT'S    *
      *********************************************************
           IF ST-QTYONORDER NOT = WS-RECEIVE-SOON + WS-RECEIVE-LATER
                MOVE "*" TO D-ORD-ERR.
           COMPUTE WS-QTY =
              (ST-QTYONHAND + WS-RECEIVE-SOON) - ST-QTYONBORDER.
           IF WS-QTY < ST-MINIMUMLEVEL
               PERFORM COU-010.
           IF WS-QTY + WS-RECEIVE-LATER <
                    ((ST-MAXIMUMLEVEL * 60) / 100)
              PERFORM COU-040.
       QTO-900.
           IF WS-QTY + WS-RECEIVE-LATER <
                    ((ST-MAXIMUMLEVEL * 60) / 100)
            IF WS-SEA-AIR-ORDER = "A"
             IF D-DELETE = "T"
              IF INVQUES-STOCK-TO-MAX = "Y"
                 COMPUTE WS-ORDER-SEA ROUNDED = ST-MAXIMUMLEVEL -
                   (WS-QTY + WS-RECEIVE-LATER)
               IF WS-ORDER-SEA > 0
                 MOVE WS-ORDER-SEA TO WS-ORDER-URGENT
                 MOVE 0            TO WS-ORDER-SEA
                 GO TO QTO-950
               ELSE
                 MOVE 0 TO WS-ORDER-SEA
                           WS-ORDER-URGENT
                 GO TO QTO-950.
           IF WS-QTY + WS-RECEIVE-LATER <
                    ((ST-MAXIMUMLEVEL * 60) / 100)
            IF WS-SEA-AIR-ORDER = "L"
              COMPUTE WS-ORDER-SEA ROUNDED = ST-MAXIMUMLEVEL -
                 (WS-QTY + WS-RECEIVE-LATER)
             IF WS-ORDER-SEA > 0
              MOVE WS-ORDER-SEA TO WS-ORDER-URGENT
              MOVE 0            TO WS-ORDER-SEA
              GO TO QTO-950
             ELSE
              MOVE 0 TO WS-ORDER-SEA
                        WS-ORDER-URGENT
              GO TO QTO-950.
           IF WS-SEA-AIR-ORDER = "A"
            IF WS-ORDER-URGENT = 0
              COMPUTE WS-ORDER-SEA ROUNDED = WS-ORDER-SEA / 3
              ADD WS-ORDER-SEA TO WS-ORDER-URGENT
              MOVE 0           TO WS-ORDER-SEA
            ELSE
              COMPUTE WS-ORDER-SEA ROUNDED = WS-ORDER-SEA / 4
              ADD WS-ORDER-SEA TO WS-ORDER-URGENT
              MOVE 0           TO WS-ORDER-SEA.
            IF WS-SEA-AIR-ORDER = "S"
                MOVE 0 TO WS-ORDER-URGENT.
       QTO-950.
              IF WS-ORDER-URGENT > 0
                  MOVE WS-ORDER-URGENT TO WS-BOX-ORDERED
                  PERFORM COMPUTE-BOX-QTY
                  MOVE WS-BOX-ORDERED TO WS-ORDER-URGENT.
              IF WS-ORDER-SEA > 0
                  MOVE WS-ORDER-SEA TO WS-BOX-ORDERED
                  PERFORM COMPUTE-BOX-QTY
                  MOVE WS-BOX-ORDERED TO WS-ORDER-SEA.
           COMPUTE WS-QTY = ST-QTYONHAND + WS-RECEIVE-SOON.
       QTO-955.
           IF WS-SEA-AIR-ORDER = "A"
            IF WS-ORDER-URGENT + WS-QTY < ST-QTYONBORDER
                ADD ST-MINBUYQTY TO WS-ORDER-URGENT
                GO TO QTO-955.
       QTO-960.
           IF WS-ORDER-URGENT > 0
              OR WS-ORDER-SEA > 0
                PERFORM CHECK-SUPPLIER-NAME.
       QTO-999.
           EXIT.
      *
       CHECK-SUPPLIER-NAME SECTION.
       CSN-005.
           MOVE 1 TO SUB-5.
       CSN-010.
           IF SUB-5 > 20
               MOVE 20 TO SUB-5
               GO TO CSN-999.
           IF WS-SUPPLY-NAME (SUB-5) = " "
               GO TO CSN-020.
           IF WS-SUPPLY-NAME (SUB-5) NOT = ST-SUPPLIER
               ADD 1 TO SUB-5
               GO TO CSN-010.
           ADD 1 TO WS-SUPPLY-COUNT (SUB-5)
              GO TO CSN-999.
       CSN-020.
           MOVE ST-SUPPLIER TO WS-SUPPLY-NAME (SUB-5)
           ADD 1            TO WS-SUPPLY-COUNT (SUB-5).
       CSN-999.
           EXIT.
      *
       CALCULATE-ORDER-URGENT SECTION.
       COU-010.
           COMPUTE WS-ORDER-URGENT = ST-MINIMUMLEVEL - WS-QTY.
       COU-040.
           COMPUTE WS-ORDER-SEA = ST-MAXIMUMLEVEL -
                (WS-QTY + WS-RECEIVE-LATER + WS-ORDER-URGENT).
           COMPUTE WS-ORDER-SEA = WS-ORDER-SEA + (WS-ORDER-SEA / 2).
       COU-999.
           EXIT.           
      *
       COMPUTE-BOX-QTY SECTION.
       CBQ-010.
           IF ST-ANALYSIS = "S"
            IF WS-USE-MIN-BUY-QTY = "N"
              GO TO CBQ-999.
           MOVE 0 TO WS-BOX-ORDER.
           IF WS-BOX-ORDERED = 0
               GO TO CBQ-999.
           IF WS-BOX-ORDERED < ST-MINBUYQTY
               MOVE ST-MINBUYQTY TO WS-BOX-ORDERED
            IF D-ORD-ERR = "*"
               MOVE "!"          TO D-ORD-ERR
               GO TO CBQ-999
            ELSE
               MOVE "$"          TO D-ORD-ERR
               GO TO CBQ-999.
       CBQ-020.
      * NEW IF STATEMENT BELOW.  ADDED 23/1/2016
      *  THERE SEEMS TO BE AN ERROR IN THE GNUCOBOL FOR THE B-RATE
      *  SECTION WHERE BOX ORDER SOMETIMES COMES BACK AS 0.
           IF ST-MINBUYQTY = 1
               MOVE WS-BOX-ORDERED TO WS-BOX-ORDER
               GO TO CBQ-040.
           COMPUTE WS-BOX-ORDER = WS-BOX-ORDERED / ST-MINBUYQTY.
           IF B-RATE (6) > 3
            IF B-RATE (7) > 1
              ADD 1 TO WS-BOX-ORDER
              GO TO CBQ-030.
       CBQ-030.
           MOVE 5 TO SUB-6.
       CBQ-035.
           ADD 1 TO SUB-6.
           MOVE 0 TO B-RATE (SUB-6).
           IF SUB-6 = 8
              GO TO CBQ-040
           ELSE
              GO TO CBQ-035.
       CBQ-040.
           COMPUTE WS-BOX-ORDERED = WS-BOX-ORDER * ST-MINBUYQTY.
       CBQ-999.
           EXIT.
      *
       SPLIT-DATE-FOR-ORDER SECTION.
       SDFO-010.
           COMPUTE WS-TODAY = (WS-MM * 30) + WS-DD.
       SDFO-020.
      *NEW LINE ADDED TO SEE IF DUE DATE BEFORE TODAY, THEN USE TODAY
      *     IF WS-DUEDATE (SUB-1) NOT > WS-TODAY
      *        MOVE WS-TODAY TO WS-DUEDATE (SUB-1).
           MOVE WS-DUEDATE (SUB-1) TO WS1-DATE.
           IF WS1-YY NOT = WS-YY
              COMPUTE WS-DUEDAY = ((((WS1-MM + 12) * 30)
                      + WS1-DD) - 21)
           ELSE
              COMPUTE WS-DUEDAY = (((WS1-MM * 30) + WS1-DD) - 21).
       SDFO-999.
           EXIT.
      *
       ADD-TO-VALUE SECTION.
       ATV-000.
           IF WS-ORDER-URGENT > 0
             MOVE ST-AVERAGECOST TO WS-AVE
             COMPUTE WS-ORDERVALUE = WS-ORDERVALUE
                          + (WS-AVE * WS-ORDER-URGENT).
       ATV-010.
           IF WS-ORDER-SEA > 0
             MOVE ST-AVERAGECOST TO WS-AVE
             COMPUTE WS-ORDERSEA = WS-ORDERSEA
                           + (WS-AVE * WS-ORDER-SEA).
       ATV-999.
           EXIT.
      *
       SUB-LINE SECTION.
       SL-000.
           MOVE WS-ORDERVALUE TO P-SUB-CNT.
           WRITE PRINT-REC FROM SUB-TOTAL-LINE AFTER 2.
       SL-010.
           MOVE WS-ORDERSEA TO P-SUB1-CNT.
           WRITE PRINT-REC FROM SUB1-TOTAL-LINE AFTER 1.
       SL-015.
           MOVE 1 TO SUB-1.
           IF WS-ORDER (SUB-1) NOT = "    "
            MOVE " " TO PRINT-REC
            MOVE "                 ORDER NO                 DUE DATE" &
            "       SHIP VIA                  SUPPLIER" TO PRINT-REC
            WRITE PRINT-REC AFTER 2.
           MOVE " " TO PRINT-REC.
       SL-020.
           IF SUB-1 > SUB-2
              GO TO SL-900.
           MOVE SUB-1                  TO H-ORDERNO.
           MOVE WS-ORDER (SUB-1)       TO H-ORDER.
           MOVE WS-DUEDATE (SUB-1)     TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE           TO H-DUEDATE.
           MOVE WS-SHIPVIA (SUB-1)     TO H-SHIPVIA.
           MOVE WS-SUPPLIED-BY (SUB-1) TO H-SUPPLIER.
           WRITE PRINT-REC FROM ORDER-NO-LINE AFTER 1.
           MOVE " " TO PRINT-REC H-05LINE.
           ADD 1 TO SUB-1.
           GO TO SL-020.
       SL-900.
           WRITE PRINT-REC.
           WRITE PRINT-REC.
           MOVE "'D' = STOCK TO BE DELETED WHEN ON HAND = ZERO."
              TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE " " TO PRINT-REC.
           MOVE 
           "'S' = STOCK ITEM WAS ON SPECIAL, ORDER ONLY WHEN NEEDED."
              TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE " " TO PRINT-REC.
           MOVE "'N' = STOCK NOT TO BE RE-ORDERED, INTERNAL NUMBER."
              TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE " " TO PRINT-REC.
           MOVE "'T' = STOCK ITEM FOUND IN TOOLKITS." TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE " " TO PRINT-REC.
           MOVE "'*' NEXT TO QTY ON ORDER: QTY ON ORDER NOT = ORDERS"
           TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE "'!' NEXT TO QTY ON ORDER: QTY ON ORDER NOT = ORDERS" &
           "    AND QTY NEEDED < MINBUY-QTY."
           TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE "'$' NEXT TO QTY ON ORDER: QTY NEEDED < MINBUY-QTY."
           TO PRINT-REC.
           WRITE PRINT-REC.
           IF WS-SUPPLIER = "    "
               GO TO SL-910.
           MOVE WS-SUPPLIER TO S-SUPPLIER.
           WRITE PRINT-REC FROM SUPPLIER-LINE AFTER 2.
           MOVE " " TO PRINT-REC.
       SL-910.
           IF WS-TEMPORD = "N"
               GO TO SL-930.
           MOVE 1 TO SUB-1.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE "TEMPORARY FILE CREATED FOR:" TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE "SUPPLIER            QTY" TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE 1 TO SUB-1.
       SL-920.
           IF WS-SUPPLY-NAME (SUB-1) = " "
              GO TO SL-999.
           MOVE WS-SUPPLY-NAME (SUB-1)  TO S-SUPPLY-NAME.
           MOVE WS-SUPPLY-COUNT (SUB-1) TO S-SUPPLY-COUNT.
           WRITE PRINT-REC FROM SUPPLIER-NAME-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           ADD 1 TO SUB-1.
           IF SUB-1 < 20
              GO TO SL-920.
       SL-930.
           IF WS-VALID-LINES-ONLY = "N"
              GO TO SL-999.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
           WRITE PRINT-REC.
           MOVE "ONLY ITEMS TO BE ORDERED HAVE BEEN PRINTED."
            TO PRINT-REC.
           WRITE PRINT-REC.
           IF WS-USE-MIN-BUY-QTY = "Y"
              MOVE 
          "** MIN-BUY-QTY USED FOR ST-ANALYSIS='S' CALCULATIONS. **"
             TO PRINT-REC
           ELSE
              MOVE
          "** MIN-BUY-QTY NOT USED FOR ST-ANALYSIS='S' CALCULATIONS. **"
             TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
       SL-999.
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
       READ-INVQUES-FILE SECTION.
       RINVQUES-000.
            MOVE 1 TO PA-RECORD.
            MOVE 6 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RINVQUES-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "N" TO INVQUES-STOCK-TO-MAX
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
       RINVQUES-999.
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
           OPEN I-O OUTSTANDING-ORDERS.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "ORDERS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-001.
       OPEN-002.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
           MOVE ALL "X" TO STORE-DEL.
           PERFORM READ-DELIVERY-FILE.
           PERFORM READ-INVQUES-FILE.
           CLOSE PARAMETER-FILE.
           MOVE WS-CO-NAME TO CO-NAME.
       OPEN-003.
           OPEN I-O ORDER-GEN-FILE.
           IF WS-GEN-ST1 NOT = 0
               MOVE "ORDER-GEN-FILE BUSY ON OPEN I-O, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GEN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GEN-ST1
               GO TO OPEN-004.
            GO TO OPEN-999.
       OPEN-004.
           OPEN OUTPUT ORDER-GEN-FILE.
           IF WS-GEN-ST1 NOT = 0
               MOVE "ORDER-GEN BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GEN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GEN-ST1
               GO TO OPEN-003.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE PRINT-FILE
                 STOCK-MASTER
                 OUTSTANDING-ORDERS
                 ORDER-GEN-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
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
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
