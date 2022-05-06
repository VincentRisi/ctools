       IDENTIFICATION DIVISION.
       PROGRAM-ID. SlInOrIq.
       AUTHOR.  CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
         Copy "SelectStMaster".
         Copy "SelectStTrans".
         Copy "SelectSlRegister".
         Copy "SelectDrTrans".
         Copy "SelectDrMaster".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStTrans.
           COPY ChlfdRegister.
           COPY ChlfdDrTrans.
           COPY ChlfdDebtor.
      *
       WORKING-STORAGE SECTION.
       77  WS-ACCNO-X           PIC 9(7) VALUE 0.
       77  WS-NEWORDER          PIC X VALUE " ".
       77  WS-AREA              PIC X VALUE " ".
       77  WS-DIS               PIC XX VALUE " ".
       77  WS-COST-DISPLAY      PIC X VALUE "N".
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-BO-QTY            PIC 9(5) VALUE 0.
       77  WS-ERR               PIC XXX VALUE " ".
       77  WS-SUBTOTAL          PIC 9(7)V99 VALUE 0.
       77  WS-BODY-LINE         PIC ZZ9.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "DrNameIq".
       77  WS-STOCK-INQUIRY     PIC X(8) VALUE "StMastIq".
       77  WS-PORDER-INQUIRY    PIC X(8) VALUE "SlPoShIq".
       77  WS-LINKED-NUMBER     PIC X(7) VALUE " ".
       77  WS-PAYED             PIC 9(8)V99 VALUE 0.
       77  WS-WORKTOTAL         PIC 9(8)V99 VALUE 0.
       77  WS-WORKTOTAL2        PIC 9(8)V99 VALUE 0.
       77  WS-DISCOUNT          PIC 9(8)V99 VALUE 0.
       77  WS-COSTTOTAL         PIC 9(8)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(8)V99 VALUE 0.
       77  WS-PERC              PIC S9(4)V99 VALUE 0.
       77  WS-PRICETOTAL        PIC 9(8)V99 VALUE 0.
       77  WS-GST-PERCENT       PIC 99V99 VALUE 0.
       77  WS-PART-ORDERS       PIC X VALUE " ".
       01  WS-STDESC.
           03  WS-DESC1         PIC X(20) VALUE " ".
           03  WS-DESC2         PIC X(20) VALUE " ".
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1   PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1   PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X VALUE " ".
           03  SP-REST          PIC X(14) VALUE " ".
       01  WS-COMMENT-DISPLAY.
           03  COMMENT-STOCK    PIC X(15).
           03  COMMENT-BALANCE  PIC X(60).
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
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 301.
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
               05  C-LINE REDEFINES B-REMAINDER.
                   07  C-ORDER            PIC X(5).
                   07  C-SHIP             PIC X(5).
                   07  C-SENT             PIC X(5).
                   07  C-BORDER           PIC X(5).
                   07  C-UNIT             PIC X(4).
                   07  C-PRICE            PIC X(9).
                   07  C-COST             PIC X(9).
                   07  C-DISC             PIC X(5).
                   07  C-REST             PIC X(10).
       01  WS-BODY-LINES.
           03  WS-STOCKNUMBER      PIC X(15).
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
       CONTROL-010.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           PERFORM DISPLAY-FORM
           MOVE 1 TO SUB-20 SUB-25
           PERFORM CLEAR-FIELDS
           PERFORM GET-DATA.
           GO TO CONTROL-010.
       CONTROL-999.
           EXIT.
      *
       GET-NUMBER-TO-DISPLAY SECTION.
       GNTD-005.
           MOVE SPACES TO ALPHA-RATE.
           MOVE WS-LINK-ACCOUNT TO ALPHA-RATE.
           IF AL-RATE (1) = "1"
              MOVE "I" TO AL-RATE (1).
           IF AL-RATE (1) = "3"
              MOVE "Q" TO AL-RATE (1).
           IF AL-RATE (1) = "4"
              MOVE "O" TO AL-RATE (1).
           IF AL-RATE (1) = "6"
              MOVE "C" TO AL-RATE (1).
           IF AL-RATE (1) = "8"
              MOVE "Q" TO AL-RATE (1).
       GNTD-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-010.
           MOVE "TYPE-OF-READ" TO F-FIELDNAME
           MOVE 12             TO F-CBFIELDNAME
           MOVE "**INVOICE/C-NOTE/P-SLIP/QUOTE INQUIRY**" TO F-NAMEFIELD
           MOVE 40             TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
           MOVE 2901 TO POS.
           DISPLAY
           "I=Inv, C=C/N, O=P/Slip, Q=Quote, R=Repair; Enter " &
           "the #, Then PRESS <F5> to View." AT POS.

            IF WS-LINK-ACCOUNT > 0
                PERFORM GET-NUMBER-TO-DISPLAY
                MOVE "ACCOUNTNO"      TO F-FIELDNAME
                MOVE 9                TO F-CBFIELDNAME
                MOVE ALPHA-RATE       TO F-NAMEFIELD
                MOVE 7                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.

            MOVE "                        " TO F-NAMEFIELD.
            MOVE " " TO WS-DIS.
            MOVE "ACCOUNTNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO F-NAMEFIELDRED.

            IF F-NAMEFIELDRED = "STOCK  "
                CALL WS-STOCK-INQUIRY USING WS-LINKAGE
                CANCEL WS-STOCK-INQUIRY
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD F-NAMEFIELDRED
                GO TO GET-010.
            IF F-NAMEFIELDRED = "PORDER "
                CALL WS-PORDER-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-PORDER-INQUIRY
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD F-NAMEFIELDRED
                GO TO GET-010.
            IF F-NAMEFIELDRED1 = "S"
                AND F-NAMEFIELDRED7 NOT = "TOCK"
                   DISPLAY " " AT 3079 WITH BELL
                   GO TO GET-010.

           IF F-EXIT-CH = X"19"
             IF F-NAMEFIELDRED1 NOT = "I" AND NOT = "O" AND NOT = "C"
                            AND NOT = "Q" AND NOT = "R"
                GO TO GET-010.
      * 'F5' KEY, TO FIND OLD ORDER AND DISPLAY
           IF F-EXIT-CH = X"19"
                MOVE "          " TO ALPHA-RATE
                MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-INVOICE
                PERFORM READ-INVOICE-REGISTER.
           IF F-EXIT-CH = X"19"
             IF WS-NEWORDER = "N"
                PERFORM READ-STOCK-TRANSACTIONS
                PERFORM READ-DEBTORS
                PERFORM FIND-INFO
                GO TO GET-150.
           IF F-EXIT-CH = X"19"
             IF WS-NEWORDER = "Y"
                MOVE "RECORD NOT FOUND IN THE SYSTEM, CAN'T DISPLAY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-010.
       GET-012.
            IF F-NAMEFIELDRED1 = " "
                MOVE "0" TO F-NAMEFIELDRED1.
            IF F-NAMEFIELDRED1 = "0" OR = "1" OR = "2" OR = "3"
             OR = "4" OR = "5" OR = "6" OR = "7" OR = "8" OR = "9"
                NEXT SENTENCE
                ELSE 
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
            MOVE F-NAMEFIELD TO WS-ACCNO-X.
            IF WS-ACCNO-X = 0
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                GO TO GET-010.
            GO TO GET-010.
            PERFORM ERROR-020.
            PERFORM ERROR1-020.
       GET-150.
            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM SCROLL-PREVIOUS.
       GET-180.
            PERFORM FILL-BODY.
            PERFORM CLEAR-SCREEN.
       GET-999.
            EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
           MOVE " " TO WS-ABOVE-BODY.
           MOVE 1   TO SUB-1 SUB-2 SUB-3.
           PERFORM ERROR1-020.
           MOVE 2910 TO POS
           DISPLAY
            "Press <ESC> to Clear Screen, OR <END> to EXIT."
               AT POS.
       FILL-005.
           PERFORM ERROR-020.
           MOVE 3010 TO POS.
           DISPLAY "    BODY LINE: " AT POS.
           ADD 16 TO POS.
           MOVE SUB-1 TO WS-BODY-LINE.
           DISPLAY WS-BODY-LINE AT POS.
       FILL-010.
           PERFORM RUNNING-TOTAL.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF B-STOCKNUMBER (SUB-1) NOT = " "
            IF SP-1STCHAR NOT = "*"
               PERFORM SCROLL-500.
               
           MOVE 2601 TO POS
           MOVE " " TO WS-MESSAGE
           DISPLAY WS-MESSAGE AT POS
           MOVE 2620 TO POS
           DISPLAY WS-MESSAGE AT POS.
           
           IF F-NAMEFIELDRED1 = "I" OR = "O" OR = "Q"
            IF SP-1STCHAR = "/"
               MOVE 0 TO WS-QTYONHAND
                         WS-QTYONRESERVE
                         WS-QTYONORDER
                         WS-QTYONBORDER
               MOVE 2610 TO POS
               DISPLAY WS-ONHAND-LINE AT POS.
            IF F-NAMEFIELDRED1 = "I" OR = "O" OR = "Q"
               MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER
               PERFORM READ-STOCK
             IF WS-STOCK-ST1 = 0
                MOVE ST-QTYONHAND    TO WS-QTYONHAND
                MOVE ST-QTYONRESERVE TO WS-QTYONRESERVE
                MOVE ST-QTYONORDER   TO WS-QTYONORDER
                MOVE ST-QTYONBORDER  TO WS-QTYONBORDER
                MOVE 2610 TO POS
                DISPLAY WS-ONHAND-LINE AT POS
             ELSE
                MOVE 0               TO WS-QTYONHAND
                                        WS-QTYONRESERVE
                                        WS-QTYONORDER
                                        WS-QTYONBORDER
                MOVE 2610 TO POS
                DISPLAY WS-ONHAND-LINE AT POS.
           IF F-NAMEFIELDRED1 = "I" OR = "O" OR = "Q"
            IF SP-1STCHAR = "*"
               MOVE 2601 TO POS
               DISPLAY WS-MESSAGE AT POS
               MOVE 2620 TO POS
               DISPLAY WS-MESSAGE AT POS
               MOVE B-STOCKNUMBER (SUB-1) TO COMMENT-STOCK
               MOVE C-LINE (SUB-1)        TO COMMENT-BALANCE
               MOVE 2605 TO POS
               DISPLAY WS-COMMENT-DISPLAY AT POS.
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.

            IF F-EXIT-CH = X"0B" 
              IF B-STOCKNUMBER (SUB-1) = " "
                GO TO FILL-010.
            IF F-EXIT-CH = X"01" AND F-INDEX = 1
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999.
            IF F-EXIT-CH = X"07"
                PERFORM ERROR-020
                GO TO FILL-999.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"01"
              IF B-STOCKNUMBER (SUB-1) = "  "
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-005.
            IF F-EXIT-CH = X"01" AND F-INDEX > 1
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-010.
            IF F-EXIT-CH = X"0B" AND F-INDEX < 7
                ADD 1 TO F-INDEX SUB-1
                GO TO FILL-005.
            IF F-EXIT-CH = X"0B" AND F-INDEX = 7
                PERFORM SCROLL-NEXT
                GO TO FILL-010.
            IF F-EXIT-CH = X"91"
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
                PERFORM ERROR-020
                GO TO FILL-999.
            GO TO FILL-010.
       FILL-999.
             EXIT.
      *
       READ-INVOICE-REGISTER SECTION.
       RIR-000.
           MOVE WS-INVOICE TO INCR-INVOICE.
           IF F-NAMEFIELDRED1 = "I"
                MOVE 1 TO INCR-TRANS.
           IF F-NAMEFIELDRED1 = "R"
                MOVE 3 TO INCR-TRANS.
           IF F-NAMEFIELDRED1 = "O"
                MOVE 4 TO INCR-TRANS.
           IF F-NAMEFIELDRED1 = "C"
                MOVE 6 TO INCR-TRANS.
           IF F-NAMEFIELDRED1 = "Q"
                MOVE 8 TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE "Y" TO WS-NEWORDER
               GO TO RIR-999.
       RIR-005.
           READ INCR-REGISTER
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
       RIR-010.
           MOVE INCR-LINENO TO SUB-20 SUB-25.
           MOVE "N"         TO WS-NEWORDER.
           IF INCR-TRANS = 1
              PERFORM READ-DRTRANS.
       RIR-999.
           EXIT.
      *
       READ-DRTRANS SECTION.
       RDTR-005.
           MOVE 1               TO DRTR-TYPE
           MOVE INCR-DRTRANS-NO TO DRTR-TRANSACTION-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                GO TO RDTR-999.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "BAD START ON DR-TRANS FILE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO RDTR-005.
       RDTR-010.
           READ DEBTOR-TRANS-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
               GO TO RDTR-999.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DR-TRANS FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO RDTR-010.
       RDTR-999.
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
               GO TO R-ST-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD IN USE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE ST-STOCKNUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-005.
       R-ST-999.
           EXIT.
      *
       READ-STOCK-TRANSACTIONS SECTION.
       RSTT-000.
           PERFORM ERROR1-020.
           MOVE 2910 TO POS.
           DISPLAY "READING ST-TRANSACTIONS................." AT POS.
           MOVE 1 TO SUB-1.
           MOVE WS-INVOICE TO STTR-REFERENCE1.
           IF F-NAMEFIELDRED1 = "I"
                MOVE 1 TO STTR-TYPE.
           IF F-NAMEFIELDRED1 = "R"
                MOVE 3 TO STTR-TYPE.
           IF F-NAMEFIELDRED1 = "O"
                MOVE 4 TO STTR-TYPE.
           IF F-NAMEFIELDRED1 = "C"
                MOVE 6 TO STTR-TYPE.
           IF F-NAMEFIELDRED1 = "Q"
                MOVE 8 TO STTR-TYPE.
           MOVE 1      TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
       RSTT-010.
           READ STOCK-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              MOVE 0 TO STTR-TYPE
              GO TO RSTT-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS BUSY ON READ-NEXT, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE STTR-STOCK-NUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STTRANS-ST1
              GO TO RSTT-010.
           IF STTR-REFERENCE1 NOT = WS-INVOICE
              GO TO RSTT-999.
           IF F-NAMEFIELDRED1 = "I"
            IF STTR-TYPE NOT = 1
              GO TO RSTT-999.
           IF F-NAMEFIELDRED1 = "R"
            IF STTR-TYPE NOT = 3
              GO TO RSTT-999.
           IF F-NAMEFIELDRED1 = "O"
            IF STTR-TYPE NOT = 4
              GO TO RSTT-999.
           IF F-NAMEFIELDRED1 = "C"
            IF STTR-TYPE NOT = 6
              GO TO RSTT-999.
           IF F-NAMEFIELDRED1 = "Q"
            IF STTR-TYPE NOT = 8
              GO TO RSTT-999.
              
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
           MOVE STTR-SALES-VALUE  TO B-NETT (SUB-1).
           GO TO RSTT-050.
       RSTT-020.
           MOVE COMMENT-FIELDS    TO C-LINE (SUB-1).
       RSTT-050.
           ADD 1 TO SUB-1.
           MOVE SUB-1 TO SUB-25.
           IF SUB-1 > 300
              MOVE 300 TO SUB-1 SUB-25
              GO TO RSTT-999.
           GO TO RSTT-010.
       RSTT-999.
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
       FIND-INFO SECTION.
       FIND-010.
            MOVE "TYPE-OF-READ" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            IF F-NAMEFIELDRED1 = "I"
               MOVE "** INVOICE INQUIRY PROGRAM BY INVOICE **"
               TO F-NAMEFIELD.
            IF F-NAMEFIELDRED1 = "R"
               MOVE "** REPAIRS INQUIRY PROGRAM BY REPAIR **"
               TO F-NAMEFIELD.
            IF F-NAMEFIELDRED1 = "C"
               MOVE "** C/NOTES INQUIRY PROGRAM BY C/NOTE  **"
               TO F-NAMEFIELD.
            IF F-NAMEFIELDRED1 = "O"
               MOVE " ** P/SLIP INQUIRY PROGRAM BY P/SLIP **"
               TO F-NAMEFIELD.
            IF F-NAMEFIELDRED1 = "Q"
               MOVE "  ** QUOTE INQUIRY PROGRAM BY QUOTE **"
               TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            IF F-NAMEFIELDRED1 NOT = "O" AND NOT = "R"
                 GO TO FIND-020.
            MOVE "ORDER-DESC" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            
            IF INCR-PRINTED = "Y" OR = "L"
               MOVE "**COMPLETE**" TO F-NAMEFIELD
               GO TO FIND-019.
            IF INCR-PRINTED = "S"
               MOVE "*SUSPENDED* " TO F-NAMEFIELD
            ELSE
               MOVE "*INCOMPLETE*" TO F-NAMEFIELD.
       FIND-019.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       FIND-020.
            MOVE "INVOICENUM" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            MOVE 6            TO F-CBFIELDLENGTH.
            MOVE WS-INVOICE   TO F-EDNAMEFIELDNUM.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "TRANS2ND"     TO F-FIELDNAME
            MOVE 8              TO F-CBFIELDNAME
            MOVE 6              TO F-CBFIELDLENGTH
            MOVE INCR-BO-INV-NO TO F-EDNAMEFIELDNUM
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "ACCOUNTNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE INCR-ACCOUNT TO F-NAMEFIELD
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEBTORNAME" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE INCR-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE INCR-ADD1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE INCR-ADD2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE INCR-ADD3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTCODE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE INCR-CODE TO F-NAMEFIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-DEL1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-DEL2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-DEL3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POORDERNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 20 TO F-CBFIELDLENGTH.
            MOVE INCR-PORDER TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESANALYSIS" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE INCR-SALES TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERVIA" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE INCR-DELIVERY TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TERMOFSALE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE INCR-TERMS TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-GSTNO TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BINNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-BIN TO F-NAMEFIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PHONE"    TO F-FIELDNAME
            MOVE 5          TO F-CBFIELDNAME
            MOVE INCR-PHONE TO F-NAMEFIELD
            MOVE 20         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CONTACTNAME" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE INCR-CONTACT TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICEDATE" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            MOVE INCR-DATE     TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE  TO F-NAMEFIELD.
            MOVE 10            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SOLDBY" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR-SB-TYPE TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

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

            MOVE "PULL-DESC"           TO F-FIELDNAME
            MOVE 9                     TO F-CBFIELDNAME.
            IF F-NAMEFIELDRED1 = "O"
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
                
                MOVE "PULLDESC" TO F-FIELDNAME
                MOVE 8          TO F-CBFIELDNAME
                MOVE "BY:"      TO F-NAMEFIELD
                MOVE 3          TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA

                MOVE "PULLER"    TO F-FIELDNAME
                MOVE 6           TO F-CBFIELDNAME
                MOVE INCR-PULLBY TO F-NAMEFIELD
                MOVE 2           TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
             ELSE
                MOVE "GOODS NOT PULLD" TO F-NAMEFIELD
                MOVE 15                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.
            IF F-NAMEFIELDRED1 = "I"
             IF INCR-PULL-DATE > 0
                MOVE "GOODS DELIV. ON" TO F-NAMEFIELD
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
                MOVE "GOODS NOT DELV." TO F-NAMEFIELD
                MOVE 15                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.

            IF F-NAMEFIELDRED1 = "I"
             IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                MOVE INCR-INVCRED-AMT TO WS-PAYED
             ELSE
                COMPUTE WS-PAYED = 
                   DRTR-AMT-OF-INVOICE - DRTR-AMT-OUTSTANDING.

            MOVE "PAY-DESC"            TO F-FIELDNAME
            MOVE 8                     TO F-CBFIELDNAME.
            IF F-NAMEFIELDRED1 = "I"
                MOVE "INVOICE AMT PD:" TO F-NAMEFIELD
                MOVE 15                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                
                MOVE "PAY-AMT"         TO F-FIELDNAME
                MOVE 7                 TO F-CBFIELDNAME
                MOVE WS-PAYED          TO F-EDNAMEFIELD99Mil
                MOVE 11                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-99Mil.
                
            IF F-NAMEFIELDRED1 = "O"
             IF DR-SUPPLY-Y-N = "N"
                MOVE "ACCOUNT ON HOLD" TO F-NAMEFIELD
                MOVE 15                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                GO TO FIND-025.
            IF F-NAMEFIELDRED1 = "O" OR = "R"
             IF DR-SUPPLY-Y-N = "S"
                MOVE "ACCOUNT SUSPEND" TO F-NAMEFIELD
                MOVE 15                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                GO TO FIND-025.
            IF F-NAMEFIELDRED1 = "O" OR = "R"
             IF DR-SUPPLY-Y-N = "Z"
                MOVE "UNKNOWN ACCOUNT" TO F-NAMEFIELD
                MOVE 15                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                GO TO FIND-025.
                
            IF F-NAMEFIELDRED1 = "O" OR = "R"
                MOVE SPACES            TO F-NAMEFIELD
                MOVE 15                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                
                MOVE "PAY-AMT"         TO F-FIELDNAME
                MOVE 7                 TO F-CBFIELDNAME
                MOVE SPACES            TO F-NAMEFIELD
                MOVE 15                TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.
       FIND-025.
            MOVE 1 TO SUB-1.

            PERFORM SCROLL-NEXT.
            PERFORM SCROLL-PREVIOUS.

            MOVE "COMMENTLINE" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE INCR-COMMENT TO F-NAMEFIELD.
            MOVE 30 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDONFREIGHT"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE INCR-ADDFREIGHT TO F-EDNAMEFIELD99Mil
            MOVE 11              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "POSTADDON"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE INCR-ADDPOST TO F-EDNAMEFIELD99Mil
            MOVE 11           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "HANDADDON"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE INCR-ADDLABOUR TO F-EDNAMEFIELD99Mil
            MOVE 11             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "MISC.ADDON" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE INCR-ADDMISC TO F-EDNAMEFIELD99Mil
            MOVE 11           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "SUBTOTAL"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            COMPUTE WS-SUBTOTAL =
                 INCR-INVCRED-AMT - INCR-TAX - INCR-ADDONS.
            MOVE WS-SUBTOTAL TO F-EDNAMEFIELD99Mil.
            MOVE 11          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99MIL.

            MOVE "ADDONAMT"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE INCR-ADDONS TO F-EDNAMEFIELD99MIL.
            MOVE 11          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99MIL.

            MOVE "TAXAMT" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            MOVE INCR-TAX TO F-EDNAMEFIELD99MIL.
            MOVE 11       TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99MIL.

            MOVE "INVOICETOTAL"   TO F-FIELDNAME.
            MOVE 12               TO F-CBFIELDNAME.
            MOVE INCR-INVCRED-AMT TO F-EDNAMEFIELD99MIL.
            MOVE 11               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99MIL.
       FIND-999.  
            EXIT.
      *
       READ-DEBTORS SECTION.
       RD-000.
           MOVE INCR-ACCOUNT TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY
               INVALID KEY NEXT SENTENCE.
       RD-010.
           READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "Z" TO DR-SUPPLY-Y-N
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
       RD-999.
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
            IF SUB-1 > 294
               MOVE 294 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 300  
                GO TO NEXT-030.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO B-TAX (SUB-1)
                MOVE 0 TO B-DISCOUNTPERITEM (SUB-1).
            IF F-INDEX < 8
                GO TO NEXT-010.
       NEXT-030.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 > 293
             IF SUB-25 > 293
               COMPUTE F-INDEX = 7 - (300 - SUB-25)
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
            IF SUB-1 > 294
               MOVE 294 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 300  
                GO TO NEXT-PAGE-030.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO B-TAX (SUB-1)
                MOVE 0 TO B-DISCOUNTPERITEM (SUB-1).
            IF F-INDEX < 8
                GO TO NEXT-PAGE-010.
       NEXT-PAGE-030.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 > 293
             IF SUB-25 > 293
               COMPUTE F-INDEX = 7 - (300 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX.
            IF SUB-1 > 300
               MOVE 294 TO SUB-1.
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
            IF SUB-1 > 300   
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
            MOVE "STOCKNUMBER"         TO F-FIELDNAME.
            MOVE 11                    TO F-CBFIELDNAME.
            MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD.
            MOVE 15                    TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
                PERFORM SCROLL-COMMENT
                GO TO SCROLL-999.

            MOVE "ORDERQTY" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE 5          TO F-CBFIELDLENGTH.
            IF F-NAMEFIELDRED1 = "C"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-020.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-020.
            MOVE B-ORDERQTY (SUB-1) TO F-EDNAMEFIELDQTY.
            PERFORM WRITE-FIELD-QTY.
       SCROLL-020.
            MOVE "SHIPQTY" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE 5         TO F-CBFIELDLENGTH.
            IF F-NAMEFIELDRED1 = "I"
                MOVE " "   TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-025.
            IF B-NEWLINE (SUB-1) = "R"
                MOVE " "   TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-025.
            IF F-NAMEFIELDRED1 = "Q" OR = "R"
                MOVE " "   TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-025.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE B-SHIPQTY (SUB-1) TO F-EDNAMEFIELDQTY
                PERFORM WRITE-FIELD-QTY
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.
       SCROLL-025.
            MOVE "SHIPPED" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE 5         TO F-CBFIELDLENGTH.
            IF F-NAMEFIELDRED1 = "C"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-030.
                
            IF F-NAMEFIELDRED1 = "R"
             IF B-NEWLINE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-028.
            IF B-NEWLINE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-028.

            IF F-NAMEFIELDRED1 = "I"
             IF B-STOCKNUMBER (SUB-1) NOT = " "
               MOVE B-SHIPQTY (SUB-1) TO F-EDNAMEFIELDQTY
               PERFORM WRITE-FIELD-QTY
               GO TO SCROLL-030
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-030.

      *      IF B-NEWLINE (SUB-1) = "R"
      *      IF F-NAMEFIELDRED1 = "O" OR = "R"
            IF F-NAMEFIELDRED1 = "O"
             IF B-STOCKNUMBER (SUB-1) NOT = " "
               MOVE B-SHIPPEDQTY (SUB-1) TO F-EDNAMEFIELDQTY
               MOVE 5                    TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-QTY
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.

            COMPUTE WS-BO-QTY = B-ORDERQTY (SUB-1) -
                  (B-SHIPPEDQTY (SUB-1) + B-SHIPQTY (SUB-1)).
       SCROLL-028.
            MOVE "B-ORDER" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE 5         TO F-CBFIELDLENGTH.
            IF F-NAMEFIELDRED1 = "C"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-030.
             IF B-NEWLINE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-030.
            IF F-NAMEFIELDRED1 = "R"
             IF B-NEWLINE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-030.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE WS-BO-QTY TO F-EDNAMEFIELDQTY
                PERFORM WRITE-FIELD-QTY
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.
       SCROLL-030.
            MOVE "PERUNIT"      TO F-FIELDNAME.
            MOVE 7              TO F-CBFIELDNAME.
            MOVE B-UNIT (SUB-1) TO F-NAMEFIELD.
            MOVE 4              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            MOVE 11           TO F-CBFIELDLENGTH.
             IF B-NEWLINE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-035.
            IF F-NAMEFIELDRED1 = "R"
             IF B-NEWLINE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-035.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE B-STOCKPRICE (SUB-1) TO F-EDNAMEFIELD99Mil
                PERFORM WRITE-FIELD-99Mil
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.
       SCROLL-035.
            MOVE "STOCKCOST" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            MOVE 11          TO F-CBFIELDLENGTH.
             IF B-NEWLINE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-040.
            IF F-NAMEFIELDRED1 = "R"
             IF B-NEWLINE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-040.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-040.
            IF WS-COST-DISPLAY = "N"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
            ELSE
                MOVE B-STOCKCOST (SUB-1) TO F-EDNAMEFIELD99Mil
                PERFORM WRITE-FIELD-99Mil.
       SCROLL-040.
            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME.
            MOVE 15                TO F-CBFIELDNAME.
            MOVE 5                 TO F-CBFIELDLENGTH.
             IF B-NEWLINE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-050.
            IF F-NAMEFIELDRED1 = "R"
             IF B-NEWLINE (SUB-1) = "R"
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-050.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
                MOVE B-DISCOUNTPERITEM (SUB-1) TO
                           F-EDNAMEFIELDAMOUNTDIS
                PERFORM WRITE-FIELD-AMOUNTDIS
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.
       SCROLL-050.
            MOVE "TAX"         TO F-FIELDNAME.
            MOVE 3             TO F-CBFIELDNAME.
            MOVE B-TAX (SUB-1) TO F-NAMEFIELD.
            MOVE 1             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            GO TO SCROLL-999.
       SCROLL-500.
            MOVE SPACES          TO WS-STDESC
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

            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDERQTY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 0 TO F-EDNAMEFIELDNUM.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "SHIPQTY" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 0 TO F-EDNAMEFIELDNUM.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "SHIPPED" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 0 TO F-EDNAMEFIELDNUM.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "B-ORDER" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 0 TO F-EDNAMEFIELDNUM.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "PERUNIT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKPRICE" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE 11           TO F-CBFIELDLENGTH
            MOVE " "          TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKCOST" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE 11          TO F-CBFIELDLENGTH
            MOVE " "         TO F-NAMEFIELD
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

      *      IF F-NAMEFIELDRED1 = "C"
      *           GO TO SCCO-010.
            MOVE "SHIPQTY" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            MOVE C-SHIP (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
       SCCO-010.
            MOVE "SHIPPED" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE C-SENT (SUB-1) TO F-NAMEFIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "B-ORDER" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE C-BORDER (SUB-1) TO F-NAMEFIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PERUNIT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 4 TO F-CBFIELDLENGTH.
            MOVE C-UNIT (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKPRICE"    TO F-FIELDNAME
            MOVE 10              TO F-CBFIELDNAME
            MOVE 11              TO F-CBFIELDLENGTH
            MOVE C-PRICE (SUB-1) TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKCOST"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE 11             TO F-CBFIELDLENGTH
            MOVE C-COST (SUB-1) TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME.
            MOVE 15 TO F-CBFIELDNAME.
            MOVE 5 TO F-CBFIELDLENGTH.
            MOVE C-DISC (SUB-1) TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
       SCCO-999.
            EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 1 TO SUB-1.
       CF-010.
             IF B-STOCKNUMBER (SUB-1) = " "
              IF B-ORDERQTY (SUB-1) = 0
                  GO TO CF-999.
             MOVE " " TO C-LINE (SUB-1).
             MOVE " " TO B-STOCKNUMBER (SUB-1)
                         B-STOCKDESCRIPTION (SUB-1)
                         B-STOCKDESCRIPTION2 (SUB-1)
                         B-TAX (SUB-1)
                         B-UNIT (SUB-1)
                         B-PULL (SUB-1)
                         B-NEWLINE (SUB-1).
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
       OPEN-FILES SECTION.
       OPEN-011.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0 
              MOVE 0 TO WS-DEBTOR-ST1
              MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-011.
       OPEN-012.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0 
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-012.
       OPEN-015.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-STTRANS-ST1
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-015.
       OPEN-016.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0 
              MOVE 0 TO WS-INCR-ST1
              MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-016.
       OPEN-018.
           OPEN I-O DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-DRTRANS-ST1
              MOVE "DR-TRANS. BUSY ON OPEN,  'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-018.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlInOrIq"      TO F-FORMNAME.
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE INCR-REGISTER
                 DEBTOR-MASTER
                 STOCK-MASTER
                 DEBTOR-TRANS-FILE
                 STOCK-TRANS-FILE.
           EXIT PROGRAM.
      *      STOP RUN.
       END-999.
           EXIT.
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAddOn".
       Copy "WriteFieldAmount".
       Copy "WriteField99Mil".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldQty".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "TimeChecking".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB
