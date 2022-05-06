        IDENTIFICATION DIVISION.
        PROGRAM-ID. StReLoIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStReceipt".
          Copy "SelectStReceiptLy".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStkReceipts.
           COPY ChlfdStkReceiptsLy.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "StMastIq".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-LY                PIC X VALUE " ".
       77  WS-CODE-TYPE         PIC 9 VALUE 0.
       77  WS-TRANS-NUM         PIC 9(6) VALUE 0.
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-STOCKNUMBER       PIC X(16) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-BODY-LINE         PIC Z(5)9.
       01  WS-SCROLL-NUMBERS.
           03  WS-SCROLL-NUM OCCURS 10000.
             05  WS-STTR-TRANS  PIC 9(6).
             05  WS-STTR-CODE   PIC 9.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1         PIC 99.
       01  WS-STKRECEIPT-STATUS.
           03  WS-STKRECEIPT-ST1    PIC 99.
       01  WS-STKRECEIPTSLY-STATUS.
           03  WS-STKRECEIPTSLY-ST1 PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR     PIC X.
           03  SP-REST        PIC X(15).
       01  SPLIT-PORDER.
           03  SP-1ST3CHAR    PIC X(3).
           03  SP-3REST       PIC X(13).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(29) VALUE
              "** STOCK RECEIPTS ENQUIRY BY".
           03  H1-TYPE        PIC X(30) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(1) VALUE " ".
           03  H2-TYPE        PIC X(7) VALUE " ".
           03  H2-STOCK       PIC X(20).
           03  H2-STOCKDESC   PIC X(13) VALUE " ".
           03  H2-DESC1       PIC X(20).
           03  H2-DESC2       PIC X(71).
       01  HEAD3.
           03  FILLER         PIC X(10) VALUE "TRANS No".
           03  FILLER         PIC X(10) VALUE "CODE".
           03  FILLER         PIC X(6) VALUE "QTY".
           03  FILLER         PIC X(28) VALUE "UNIT PRICE TOTAL PRICE".
           03  FILLER         PIC X(25) VALUE "TRANSACTION COMMENT".
           03  FILLER         PIC X(9) VALUE " DATE".
           03  H3-DESC        PIC X(44) VALUE " ".
       01  DETAIL-LINE.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-TRANSNO      PIC Z(5)9.
           03  FILLER         PIC X(5) VALUE " ".
           03  D-CODE         PIC X.
           03  FILLER         PIC X(5) VALUE " ".
           03  D-QTY          PIC Z(4)9-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-UNIT-PRICE   PIC Z(7)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-TOTAL-PRICE  PIC Z(7)9.99.
           03  FILLER         PIC X(6) VALUE " ".
           03  D-REFNO        PIC X(22).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-DATE         PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PORDER       PIC X(20).
           03  FILLER         PIC X(24) VALUE " ".
       01  TOTAL-LINE.
           03  T-TYPE.
             05  T-DESC1        PIC X(18) VALUE " ".
             05  T-CODE         PIC 9.
             05  FILLER         PIC X.
             05  T-DESC2        PIC X(20) VALUE " ".
       Copy "WsDateInfo".
      *
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
           MOVE 0310 TO POS
           DISPLAY "** STOCK RECEIPTS (LOCAL) INQUIRY **" AT POS
           MOVE 0410 TO POS
           DISPLAY "************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
           MOVE 2510 TO POS
           DISPLAY "Program loading ....." AT POS.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "N" TO WS-ANSWER.
            MOVE 0   TO WS-CODE-TYPE.
            MOVE SPACES TO ST-DESCRIPTION1
                           ST-DESCRIPTION2.
            PERFORM CLEAR-MEMORY.

           MOVE 2910 TO POS
           DISPLAY
           "Enter Stocknumber OR # & Trans Number To View By Trans #,"
              AT POS
           MOVE 3012 TO POS
           DISPLAY
           "OR P/O.... To View Transactions By P/Order Number." AT POS.
       GET-001.            
            MOVE "STOCK" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"07"
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 PERFORM READ-STOCK-NEXT
                 GO TO GET-010.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-STOCK-PREVIOUS
                 GO TO GET-010.
            MOVE 16 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            
            MOVE F-NAMEFIELD TO SPLIT-STOCK
                                SPLIT-PORDER.
            IF SP-1STCHAR = "#"
               MOVE SP-REST TO ALPHA-RATE
               PERFORM DECIMALISE-RATE
               MOVE NUMERIC-RATE TO WS-TRANS-NUM
               MOVE WS-TRANS-NUM TO ST-STOCKNUMBER 
               PERFORM GET-010
               GO TO GET-030.
            IF SP-1STCHAR = "P"
             IF SP-1ST3CHAR = "P/O"
               PERFORM GET-010
               GO TO GET-030.
            
            MOVE F-NAMEFIELD TO ST-STOCKNUMBER.
            IF ST-STOCKNUMBER = 0 OR = "   "
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-003
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            PERFORM READ-STOCK.
      *      GO TO GET-020.
       GET-010.
            MOVE "STOCK"        TO F-FIELDNAME
            MOVE 5              TO F-CBFIELDNAME.
            IF SP-1ST3CHAR = "P/O"
               MOVE SPLIT-PORDER   TO F-NAMEFIELD
            ELSE
               MOVE ST-STOCKNUMBER TO F-NAMEFIELD.
            MOVE 16             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCK-PORDER" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME.
            IF SP-1STCHAR = "#"
               MOVE "Stock Number   " TO F-NAMEFIELD.
            IF SP-1STCHAR = "P"
             IF SP-1ST3CHAR = "P/O"
               MOVE "Stock Number   " TO F-NAMEFIELD.

            IF SP-1STCHAR NOT = "#"
             IF SP-1ST3CHAR NOT = "P/O"
            MOVE "P/Order Number "    TO F-NAMEFIELD.
            MOVE 15                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            PERFORM ERROR-020
            PERFORM ERROR1-020
            MOVE 2610 TO POS
            DISPLAY WS-MESSAGE AT POS.
       GET-020.
            MOVE "DESC"          TO F-FIELDNAME.
            MOVE 4               TO F-CBFIELDNAME.
            MOVE ST-DESCRIPTION1 TO F-NAMEFIELD.
            MOVE 20              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESC2"         TO F-FIELDNAME.
            MOVE 5               TO F-CBFIELDNAME.
            MOVE ST-DESCRIPTION2 TO F-NAMEFIELD.
            MOVE 20              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE "ONHAND"     TO F-FIELDNAME
            MOVE 6            TO F-CBFIELDNAME
            MOVE ST-QTYONHAND TO F-EDNAMEFIELDINV
            MOVE 6            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-INV.
            
            MOVE "RESERVE"       TO F-FIELDNAME
            MOVE 7               TO F-CBFIELDNAME
            MOVE ST-QTYONRESERVE TO F-EDNAMEFIELDINV
            MOVE 6               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-INV.
       GET-030.
            IF ST-DESCRIPTION1 = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-001.
            MOVE SPACES      TO F-NAMEFIELD.
            MOVE "CODE-TYPE" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-001.
            IF F-EXIT-CH = X"07"
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            MOVE 1            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-CODE-TYPE.
       GET-040.
            MOVE "TYPE-LY" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE "N"       TO F-NAMEFIELD
            MOVE 1         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE "TYPE-LY" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-030.
            IF F-EXIT-CH = X"07"
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-LY.
            MOVE " " TO F-EXIT-CH.

            IF WS-LY NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
       GET-045.
            MOVE " " TO F-EXIT-CH.
      *      CLOSE STKRECEIPTS-FILE.


            IF WS-LY = "N"
      *         PERFORM OPEN-005
               PERFORM READ-ALL-TRANSACTIONS
               PERFORM FILL-BODY
            ELSE
      *         PERFORM OPEN-004
               PERFORM READ-ALL-LY-TRANSACTIONS
               PERFORM FILL-LY-BODY.

            IF F-EXIT-CH = X"07" OR = X"09" OR = X"1F"
                PERFORM CLEAR-TRANSACTIONS
                MOVE 1   TO F-INDEX SUB-1
                MOVE "Y" TO WS-ANSWER
                GO TO GET-999.

            GO TO GET-999.

            IF WS-LY = "N"
               PERFORM READ-TRANSACTIONS
            ELSE
               PERFORM READ-LY-TRANSACTIONS.
       GET-900.
            IF WS-ANSWER = "Y"
             IF WS-LY = "N"
                CLOSE STKRECEIPTS-FILE
                GO TO GET-999
             ELSE
                CLOSE STKRECEIPTSLY-FILE
                GO TO GET-999.
            IF F-INDEX < 15
             IF F-INDEX NOT = "07"
               MOVE 2905 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3005 TO POS
               DISPLAY " Or Press 'F10' To Print All Receipts"
                  AT POS
               ADD 38 TO POS
               DISPLAY "For This Stock Number." AT POS
               MOVE "PORDER" TO F-FIELDNAME
               MOVE 6        TO F-CBFIELDNAME
               MOVE 15       TO F-INDEX
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
                
            PERFORM ERROR1-020
            PERFORM ERROR-020.
      *      MOVE 2910 TO POS.
      *      DISPLAY "                                   " AT POS.
      *      MOVE 3010 TO POS.
      *      DISPLAY "                                   " AT POS.
      *      ADD 30 TO POS.
      *      DISPLAY "                                   " AT POS.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
             IF WS-LY = "N"
                OPEN I-O STKRECEIPTS-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE STKRECEIPTS-FILE
                PERFORM ERROR-020
             ELSE
                OPEN I-O STKRECEIPTSLY-FILE
                PERFORM PRINT-LY-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE STKRECEIPTSLY-FILE
                PERFORM ERROR-020.
       GET-999.
           EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
           PERFORM OPEN-005.
             
           MOVE 1 TO F-INDEX.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
           PERFORM SCROLL-PREVIOUS-PAGE.

           MOVE 2702 TO POS
           DISPLAY "Press 'PgDn' For More, 'PgUp' For Prev,"
           AT POS
           ADD 40 TO POS
           DISPLAY "'F12' OR 'F11' to Scroll Up/Down," AT POS
           MOVE 2803 TO POS
           DISPLAY 
        "'ESC' OR 'TAB' To Clear The Screen, 'F10' To Print All" &
           " Transactions." AT POS.
       FILL-010.
           MOVE 3015 TO POS 
           DISPLAY "Current Line#: " AT POS
           ADD 16 TO POS
           MOVE SUB-1 TO WS-BODY-LINE
           DISPLAY WS-BODY-LINE AT POS.

           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.

            MOVE "CODE"  TO F-FIELDNAME
            MOVE 4       TO F-CBFIELDNAME
            MOVE 1       TO F-CBFIELDLENGTH
            PERFORM USER-FILL-FIELD.
            PERFORM READ-FIELD-ALPHA.

      *UP-ARROW
           IF F-EXIT-CH = X"01" AND F-INDEX = 1
            IF SUB-1 = 1
              GO TO FILL-010
            ELSE
              PERFORM SCROLL-PREVIOUS
              MOVE 15 TO F-INDEX
              GO TO FILL-010.
           IF F-EXIT-CH = X"01" AND F-INDEX > 1
              SUBTRACT 1 FROM F-INDEX 
                              SUB-1
            IF F-INDEX > 0
              GO TO FILL-010
            ELSE
              MOVE 1 TO F-INDEX
              PERFORM SCROLL-PREVIOUS
              MOVE 1 TO F-INDEX
              GO TO FILL-010.
      *DOWN-ARROW
           IF F-EXIT-CH = X"0B" AND F-INDEX < 15
            IF SUB-1 NOT = SUB-9
              ADD 1 TO F-INDEX SUB-1
              GO TO FILL-010
            ELSE
              GO TO FILL-010.
      *SCROLL-UP
           IF F-EXIT-CH = X"11"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-NEXT
              MOVE 1 TO F-INDEX
              GO TO FILL-010
            ELSE
              GO TO FILL-010.
      *SCROLL-DOWN
            IF F-EXIT-CH = X"13"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-PREVIOUS
              MOVE 1 TO F-INDEX
              COMPUTE SUB-1 = SUB-1 - 14
             IF SUB-1 NOT > 1
                 MOVE 1 TO SUB-1
                 GO TO FILL-010
             ELSE 
                 GO TO FILL-010.
      *NEXT-PAGE
           IF F-EXIT-CH = X"0C"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-NEXT-PAGE
              MOVE 1 TO F-INDEX
              GO TO FILL-010
            ELSE
              GO TO FILL-010.
      *PREV-PAGE
           IF F-EXIT-CH = X"05"
              PERFORM SCROLL-PREVIOUS-PAGE
              MOVE 1 TO F-INDEX
              GO TO FILL-010.
      *TAB 
           IF F-EXIT-CH = X"09"
              GO TO FILL-900.
      *FINISH - <End>
           IF F-EXIT-CH = X"04"
              PERFORM END-OFF.
      *ESC
           IF F-EXIT-CH = X"07"
              GO TO FILL-900.
      * <f10> to print
           IF F-EXIT-CH = X"1F"
                MOVE "Printing In Progress, Please Be Patient."
                   TO WS-MESSAGE
                PERFORM ERROR-000
                CLOSE STKRECEIPTS-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO FILL-900.
           MOVE 7 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
      *RETURN
           IF F-EXIT-CH = X"0A" AND F-INDEX < 15
             IF SUB-1 NOT = SUB-9
              ADD 1 TO F-INDEX SUB-1
              GO TO FILL-010
             ELSE
              GO TO FILL-010.
       FILL-050.
           ADD 1 TO SUB-1 F-INDEX.
           IF SUB-1 > 10000
               MOVE "10,000 LINES ARE UP, 'ESC' TO <TAB>."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-900.
           IF F-INDEX < 16
               GO TO FILL-010.
           SUBTRACT 1 FROM SUB-1.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1.
           PERFORM SCROLL-NEXT.
           MOVE 1 TO F-INDEX.
           GO TO FILL-010.
       FILL-900.
           CLOSE STKRECEIPTS-FILE.
       FILL-999.
           EXIT.
      *
       FILL-LY-BODY SECTION.
       FILL-LY-000.
           PERFORM OPEN-004.
              
           MOVE 1 TO F-INDEX.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
           PERFORM SCROLL-PREVIOUS-PAGE.

           MOVE 2702 TO POS
           DISPLAY "Press 'PgDn' For More, 'PgUp' For Prev,"
           AT POS
           ADD 40 TO POS
           DISPLAY "'F12' OR 'F11' to Scroll Up/Down," AT POS
           MOVE 2803 TO POS
           DISPLAY 
        "'ESC' OR 'TAB' To Clear The Screen, 'F10' To Print All" &
           " Transactions." AT POS.
       FILL-LY-010.
           MOVE 3015 TO POS 
           DISPLAY "Current Line#: " AT POS
           ADD 16 TO POS
           MOVE SUB-1 TO WS-BODY-LINE
           DISPLAY WS-BODY-LINE AT POS.

           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.

            MOVE "CODE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE 1      TO F-CBFIELDLENGTH
            PERFORM USER-FILL-FIELD.
            PERFORM READ-FIELD-ALPHA.

      *UP-ARROW
           IF F-EXIT-CH = X"01" AND F-INDEX = 1
            IF SUB-1 = 1
              GO TO FILL-LY-010
            ELSE
              PERFORM SCROLL-PREVIOUS
              MOVE 15 TO F-INDEX
              GO TO FILL-LY-010.
           IF F-EXIT-CH = X"01" AND F-INDEX > 1
              SUBTRACT 1 FROM F-INDEX 
                              SUB-1
            IF F-INDEX > 0
              GO TO FILL-LY-010
            ELSE
              MOVE 1 TO F-INDEX
              PERFORM SCROLL-PREVIOUS
              MOVE 1 TO F-INDEX
              GO TO FILL-LY-010.
      *DOWN-ARROW
           IF F-EXIT-CH = X"0B" AND F-INDEX < 15
            IF SUB-1 NOT = SUB-9
              ADD 1 TO F-INDEX SUB-1
              GO TO FILL-LY-010
            ELSE
              GO TO FILL-LY-010.
      *SCROLL-UP
           IF F-EXIT-CH = X"11"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-NEXT
              MOVE 1 TO F-INDEX
              GO TO FILL-LY-010
            ELSE
              GO TO FILL-LY-010.
      *SCROLL-DOWN
            IF F-EXIT-CH = X"13"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-PREVIOUS
              MOVE 1 TO F-INDEX
              COMPUTE SUB-1 = SUB-1 - 14
             IF SUB-1 NOT > 1
                 MOVE 1 TO SUB-1
                 GO TO FILL-LY-010
             ELSE 
                 GO TO FILL-LY-010.
      *NEXT-PAGE
           IF F-EXIT-CH = X"0C"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-NEXT-PAGE
              MOVE 1 TO F-INDEX
              GO TO FILL-LY-010
            ELSE
              GO TO FILL-LY-010.
      *PREV-PAGE
           IF F-EXIT-CH = X"05"
              PERFORM SCROLL-PREVIOUS-PAGE
              MOVE 1 TO F-INDEX
              GO TO FILL-LY-010.
      *TAB 
           IF F-EXIT-CH = X"09"
              GO TO FILL-LY-900.
      *FINISH - <End>
           IF F-EXIT-CH = X"04"
              PERFORM END-OFF.
      *ESC
           IF F-EXIT-CH = X"07"
              GO TO FILL-LY-900.
      * <f10> to print
           IF F-EXIT-CH = X"1F"
                MOVE "Printing In Progress, Please Be Patient."
                   TO WS-MESSAGE
                PERFORM ERROR-000
                CLOSE STKRECEIPTSLY-FILE
                PERFORM PRINT-LY-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO FILL-LY-900.
           MOVE 7 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
      *RETURN
           IF F-EXIT-CH = X"0A" AND F-INDEX < 15
             IF SUB-1 NOT = SUB-9
              ADD 1 TO F-INDEX SUB-1
              GO TO FILL-LY-010
             ELSE
              GO TO FILL-LY-010.
       FILL-LY-050.
           ADD 1 TO SUB-1 F-INDEX.
           IF SUB-1 > 10000
               MOVE "10,000 LINES ARE UP, 'ESC' TO <TAB>."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-LY-900.
           IF F-INDEX < 16
               GO TO FILL-LY-010.
           SUBTRACT 1 FROM SUB-1.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1.
           PERFORM SCROLL-NEXT.
           MOVE 1 TO F-INDEX.
           GO TO FILL-LY-010.
       FILL-LY-900.
           CLOSE STKRECEIPTSLY-FILE.
       FILL-LY-999.
           EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           PERFORM OPEN-005.
       RDTR-001.
           MOVE 1   TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
       RDTR-005.
           IF SP-1STCHAR = "#"
               MOVE WS-TRANS-NUM   TO STRE-TRANSACTION-NUMBER
               MOVE WS-CODE-TYPE   TO STRE-TRANSACTION-CODE
               START STKRECEIPTS-FILE KEY NOT < STRE-KEY
                   GO TO RDTR-007.
                   
           IF SP-1STCHAR = "P"
            IF SP-1ST3CHAR = "P/O"
               MOVE SPLIT-PORDER TO STRE-ORDER-NUMBER
               START STKRECEIPTS-FILE KEY NOT < STRE-ORDER-NUMBER
                   GO TO RDTR-007.

            MOVE ST-STOCKNUMBER TO STRE-STOCK-NUMBER
             START STKRECEIPTS-FILE KEY NOT < STRE-STOCK-NUMBER.
       RDTR-007.
           IF WS-STKRECEIPT-ST1 NOT = 0
              MOVE "BAD START " TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              CLOSE STKRECEIPTS-FILE
              MOVE 8 TO WS-STKRECEIPT-ST1
              GO TO RDTR-999.
            MOVE " " TO F-EXIT-CH.
       RDTR-010.
           IF F-EXIT-CH = " "
            READ STKRECEIPTS-FILE NEXT
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
            READ STKRECEIPTS-FILE PREVIOUS
               AT END NEXT SENTENCE.
           IF WS-STKRECEIPT-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTS-FILE
               GO TO RDTR-999.
           IF WS-STKRECEIPT-ST1 NOT = 0
             MOVE "RECEIPTS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STKRECEIPT-ST1
               GO TO RDTR-010.
               
           IF SP-1STCHAR NOT = "#"
            IF SP-1ST3CHAR NOT = "P/O"
             IF STRE-STOCK-NUMBER NOT = ST-STOCKNUMBER
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTS-FILE
               GO TO RDTR-999.
           IF SP-1STCHAR = "P"
            IF SP-1ST3CHAR = "P/O"
             IF STRE-ORDER-NUMBER > SPLIT-PORDER
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTS-FILE
               GO TO RDTR-999.
           IF WS-CODE-TYPE NOT = 0
            IF STRE-TRANSACTION-CODE NOT = WS-CODE-TYPE
               MOVE 2910 TO POS
               MOVE "Be Patient, Reading Next Receipt Record...."
               TO WS-MESSAGE
               DISPLAY WS-MESSAGE AT POS
                GO TO RDTR-010.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR1-020.
       RDTR-020. 
           IF F-INDEX > 15
                MOVE 2905 TO POS
                DISPLAY "Press 'PgDn' For More, 'PgUp' for Previous,"
                 AT POS
                ADD 44 TO POS
                DISPLAY "Or 'Esc' To Clear The Screen !" AT POS
                MOVE 3005 TO POS
                DISPLAY " Or Press 'F10' To Print All Receipts"
                   AT POS
                ADD 38 TO POS
                DISPLAY "For This Stock Number." AT POS
                MOVE 15 TO F-INDEX
                PERFORM USER-FILL-FIELD.
                
                PERFORM ERROR1-020
                PERFORM ERROR-020.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-TRANSACTIONS
                MOVE " " TO F-EXIT-CH
                MOVE 1 TO F-INDEX.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-TRANSACTIONS
                MOVE 1 TO F-EXIT-CH
                MOVE 1 TO F-INDEX.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                GO TO RDTR-999.
           IF F-EXIT-CH = X"1F"
                PERFORM PRINT-ROUTINE.
           IF F-EXIT-CH = X"1F"
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                PERFORM ERROR-020
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                    AND NOT = X"07" AND NOT = " "   AND NOT = X"1F"
                    AND NOT = 1
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTR-010.
       RDTR-999.
           EXIT.
      *
       READ-LY-TRANSACTIONS SECTION.
       RDTRLY-0000.
           PERFORM OPEN-004.
       RDTRLY-001.
           MOVE 1   TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
       RDTRLY-005.
           IF SP-1STCHAR = "#"
               MOVE WS-TRANS-NUM   TO STRELY-TRANSACTION-NUMBER
               MOVE WS-CODE-TYPE   TO STRELY-TRANSACTION-CODE
               START STKRECEIPTSLY-FILE KEY NOT < STRELY-KEY
                  GO TO RDTRLY-007.
                  
           IF SP-1STCHAR = "P"
            IF SP-1ST3CHAR = "P/O"
               MOVE ST-STOCKNUMBER TO STRELY-ORDER-NUMBER
               START STKRECEIPTSLY-FILE KEY NOT < STRELY-ORDER-NUMBER
                  GO TO RDTRLY-007.
                  
           MOVE ST-STOCKNUMBER TO STRELY-STOCK-NUMBER
            START STKRECEIPTSLY-FILE KEY NOT < STRELY-STOCK-NUMBER.
       RDTRLY-007.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
              CLOSE STKRECEIPTSLY-FILE
              GO TO RDTRLY-999.
            MOVE " " TO F-EXIT-CH.
       RDTRLY-010.
           IF F-EXIT-CH = " "
            READ STKRECEIPTSLY-FILE NEXT
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
            READ STKRECEIPTSLY-FILE PREVIOUS
               AT END NEXT SENTENCE.
           IF WS-STKRECEIPTSLY-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTSLY-FILE
               GO TO RDTRLY-999.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
           MOVE "RECEIPTLY BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPTSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STKRECEIPTSLY-ST1
               GO TO RDTRLY-010.
               
           IF SP-1STCHAR NOT = "#"
            IF SP-1ST3CHAR NOT = "P/O"
             IF STRELY-STOCK-NUMBER NOT = ST-STOCKNUMBER
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTSLY-FILE
               GO TO RDTRLY-999.
           IF SP-1STCHAR = "P"
            IF SP-1ST3CHAR = "P/O"
             IF STRELY-ORDER-NUMBER > ST-STOCKNUMBER
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTS-FILE
               GO TO RDTRLY-999.

           IF WS-CODE-TYPE NOT = 0
            IF STRELY-TRANSACTION-CODE NOT = WS-CODE-TYPE
               MOVE 2910 TO POS
               MOVE "Be Patient, Reading Next Receipt Record...."
               TO WS-MESSAGE
               DISPLAY WS-MESSAGE AT POS
                GO TO RDTRLY-010.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR1-020.
       RDTRLY-020. 
           IF F-INDEX > 15
                MOVE 2910 TO POS
              DISPLAY "Press 'NEXT PAGE' For More, 'PgUp' for Previous,"
                 AT POS
                ADD 44 TO POS
                DISPLAY "OR 'ESC' To Clear The Screen !" AT POS
                MOVE 3010 TO POS
                DISPLAY "Or Press 'F10' To Print All Receipts"
                   AT POS
                ADD 37 TO POS
                DISPLAY "For This Stock Number." AT POS
                MOVE 15 TO F-INDEX
                PERFORM USER-FILL-FIELD.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-TRANSACTIONS
                MOVE " " TO F-EXIT-CH
                MOVE 1 TO F-INDEX.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-TRANSACTIONS
                MOVE 1 TO F-EXIT-CH
                MOVE 1 TO F-INDEX.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                GO TO RDTRLY-999.
           IF F-EXIT-CH = X"1F"
                PERFORM PRINT-LY-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                PERFORM ERROR-020
                GO TO RDTRLY-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                    AND NOT = X"07" AND NOT = " "   AND NOT = X"1F"
                    AND NOT = 1
                MOVE 16 TO F-INDEX
                GO TO RDTRLY-020.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTRLY-010.
       RDTRLY-999.
           EXIT.
      *
       READ-ALL-TRANSACTIONS SECTION.
       RDALL-000.
           PERFORM OPEN-005.
       RDALL-001.
           MOVE 1   TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
       RDALL-005.
           IF SP-1STCHAR = "#"
               MOVE WS-TRANS-NUM   TO STRE-TRANSACTION-NUMBER
               MOVE WS-CODE-TYPE   TO STRE-TRANSACTION-CODE
               START STKRECEIPTS-FILE KEY NOT < STRE-KEY
                   GO TO RDALL-007.
                   
           IF SP-1STCHAR = "P"
            IF SP-1ST3CHAR = "P/O"
               MOVE SPLIT-PORDER TO STRE-ORDER-NUMBER
               START STKRECEIPTS-FILE KEY NOT < STRE-ORDER-NUMBER
                   GO TO RDALL-007.

           MOVE ST-STOCKNUMBER TO STRE-STOCK-NUMBER
           START STKRECEIPTS-FILE KEY NOT < STRE-STOCK-NUMBER.
       RDALL-007.
           IF WS-STKRECEIPT-ST1 NOT = 0
              MOVE "BAD START " TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              CLOSE STKRECEIPTS-FILE
              MOVE 8 TO WS-STKRECEIPT-ST1
              GO TO RDALL-999.
            MOVE " " TO F-EXIT-CH.
       RDALL-010.
           READ STKRECEIPTS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-STKRECEIPT-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTS-FILE
               GO TO RDALL-999.
           IF WS-STKRECEIPT-ST1 NOT = 0
             MOVE "RECEIPTS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STKRECEIPT-ST1
               GO TO RDALL-010.
               
           IF SP-1STCHAR NOT = "#"
            IF SP-1ST3CHAR NOT = "P/O"
             IF STRE-STOCK-NUMBER NOT = ST-STOCKNUMBER
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTS-FILE
               GO TO RDALL-900.
           IF SP-1STCHAR = "P"
            IF SP-1ST3CHAR = "P/O"
             IF STRE-ORDER-NUMBER > SPLIT-PORDER
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTS-FILE
               GO TO RDALL-900.
           IF WS-CODE-TYPE NOT = 0
            IF STRE-TRANSACTION-CODE NOT = WS-CODE-TYPE
               MOVE 2910 TO POS
               MOVE "Be Patient, Reading Next Receipt Record...."
               TO WS-MESSAGE
               DISPLAY WS-MESSAGE AT POS
                GO TO RDALL-010.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR1-020.
       RDALL-020.
           MOVE STRE-TRANSACTION-NUMBER TO WS-STTR-TRANS (SUB-1).
           MOVE STRE-TRANSACTION-CODE   TO WS-STTR-CODE (SUB-1).
           
           IF SUB-1 < 10000
              ADD 1 TO SUB-1
              PERFORM RDALL-910
              GO TO RDALL-010.
              
           MOVE "THERE ARE MORE THAN 10,000 ITEMS ON THIS ORDER."
             TO WS-MESSAGE
             PERFORM ERROR1-000
           MOVE "PRESS 'Esc' TO EXIT THE READ-ALL SECTION."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
       RDALL-900.
           SUBTRACT 1 FROM SUB-1
           MOVE SUB-1 TO SUB-9.
           IF SUB-9 < 0
               MOVE 0 TO SUB-9.
           GO TO RDALL-910.
       RDALL-910.
           MOVE 2912 TO POS.
           DISPLAY "Total # of Lines:" AT POS
           ADD 19 TO POS.
           MOVE SUB-9 TO WS-BODY-LINE.
           DISPLAY WS-BODY-LINE AT POS.
           ADD 1 TO SUB-9.
       RDALL-950.
           CLOSE STKRECEIPTS-FILE.
       RDALL-999.
           EXIT.
      *
       READ-ORDER-ONLY SECTION.
       RDONLY-000.
           IF SUB-1 > SUB-9
               GO TO RDONLY-999.
               
           MOVE WS-STTR-TRANS (SUB-1)  TO STRE-TRANSACTION-NUMBER
           MOVE WS-STTR-CODE (SUB-1)   TO STRE-TRANSACTION-CODE
           START STKRECEIPTS-FILE KEY NOT < STRE-KEY
               INVALID KEY NEXT SENTENCE.

           IF WS-STKRECEIPT-ST1 NOT = 0
                CLOSE STKRECEIPTS-FILE
                GO TO RDONLY-999.
           MOVE " " TO F-EXIT-CH.
       RDONLY-010.
           READ STKRECEIPTS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-STKRECEIPT-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTS-FILE
               GO TO RDONLY-999.
           IF WS-STKRECEIPT-ST1 NOT = 0
             MOVE "RECEIPTS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STKRECEIPT-ST1
               GO TO RDONLY-010.
       RDONLY-999.
           EXIT.
      *
       READ-ORDERLY-ONLY SECTION.
       RDONLYLY-000.
           IF SUB-1 > SUB-9
               GO TO RDONLYLY-999.
               
           MOVE WS-STTR-TRANS (SUB-1)  TO STRELY-TRANSACTION-NUMBER
           MOVE WS-STTR-CODE (SUB-1)   TO STRELY-TRANSACTION-CODE
           START STKRECEIPTSLY-FILE KEY NOT < STRELY-KEY
               INVALID KEY NEXT SENTENCE.

           IF WS-STKRECEIPTSLY-ST1 NOT = 0
                CLOSE STKRECEIPTSLY-FILE
                GO TO RDONLYLY-999.
           MOVE " " TO F-EXIT-CH.
       RDONLYLY-010.
           READ STKRECEIPTSLY-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-STKRECEIPT-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTSLY-FILE
               GO TO RDONLYLY-999.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
           MOVE "RECEIPTSLY BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPTSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STKRECEIPTSLY-ST1
               GO TO RDONLYLY-010.
       RDONLYLY-999.
           EXIT.
      *
       READ-ALL-LY-TRANSACTIONS SECTION.
       RDALLLY-0000.
           PERFORM OPEN-004.
       RDALLLY-001.
           MOVE 1   TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
       RDALLLY-005.
           IF SP-1STCHAR = "#"
               MOVE WS-TRANS-NUM   TO STRELY-TRANSACTION-NUMBER
               MOVE WS-CODE-TYPE   TO STRELY-TRANSACTION-CODE
               START STKRECEIPTSLY-FILE KEY NOT < STRELY-KEY
                  GO TO RDALLLY-007.
                  
           IF SP-1STCHAR = "P"
            IF SP-1ST3CHAR = "P/O"
               MOVE ST-STOCKNUMBER TO STRELY-ORDER-NUMBER
               START STKRECEIPTSLY-FILE KEY NOT < STRELY-ORDER-NUMBER
                  GO TO RDALLLY-007.
                  
           MOVE ST-STOCKNUMBER TO STRELY-STOCK-NUMBER
            START STKRECEIPTSLY-FILE KEY NOT < STRELY-STOCK-NUMBER.
       RDALLLY-007.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
              CLOSE STKRECEIPTSLY-FILE
              GO TO RDALLLY-999.
            MOVE " " TO F-EXIT-CH.
       RDALLLY-010.
           READ STKRECEIPTSLY-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-STKRECEIPTSLY-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTSLY-FILE
               GO TO RDALLLY-900.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
           MOVE "RECEIPTLY BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPTSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STKRECEIPTSLY-ST1
               GO TO RDALLLY-010.
               
           IF SP-1STCHAR NOT = "#"
            IF SP-1ST3CHAR NOT = "P/O"
             IF STRELY-STOCK-NUMBER NOT = ST-STOCKNUMBER
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTSLY-FILE
               GO TO RDALLLY-900.
           IF SP-1STCHAR = "P"
            IF SP-1ST3CHAR = "P/O"
             IF STRELY-ORDER-NUMBER > ST-STOCKNUMBER
               MOVE 1 TO F-INDEX
               CLOSE STKRECEIPTS-FILE
               GO TO RDALLLY-900.

           IF WS-CODE-TYPE NOT = 0
            IF STRELY-TRANSACTION-CODE NOT = WS-CODE-TYPE
               MOVE 2910 TO POS
               MOVE "Be Patient, Reading Next Receipt Record...."
               TO WS-MESSAGE
               DISPLAY WS-MESSAGE AT POS
                GO TO RDALLLY-010.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR1-020.
       RDALLLY-020.
           MOVE STRELY-TRANSACTION-NUMBER TO WS-STTR-TRANS (SUB-1).
           MOVE STRELY-TRANSACTION-CODE   TO WS-STTR-CODE (SUB-1).
           
           IF SUB-1 < 10000
              ADD 1 TO SUB-1
              PERFORM RDALLLY-910
              GO TO RDALLLY-010.
              
           MOVE "THERE ARE MORE THAN 10,000 ITEMS ON THIS ORDER."
             TO WS-MESSAGE
             PERFORM ERROR1-000
           MOVE "PRESS 'Esc' TO EXIT THE READ-ALL SECTION."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
       RDALLLY-900.
           SUBTRACT 1 FROM SUB-1
           MOVE SUB-1 TO SUB-9.
           IF SUB-9 < 0
               MOVE 0 TO SUB-9.
           GO TO RDALLLY-910.
       RDALLLY-910.
           MOVE 2912 TO POS.
           DISPLAY "Total # of Lines:" AT POS
           ADD 19 TO POS.
           MOVE SUB-9 TO WS-BODY-LINE.
           DISPLAY WS-BODY-LINE AT POS.
           ADD 1 TO SUB-9.
       RDALLLY-950.
           CLOSE STKRECEIPTSLY-FILE.
       RDALLLY-999.
           EXIT.
      *
       READ-STOCK SECTION.
       RS-000.
            MOVE ST-STOCKNUMBER TO WS-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY.
       RS-010.
            READ STOCK-MASTER
                INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER
                MOVE "UNKNOWN" TO ST-DESCRIPTION1
                GO TO RS-999.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RS-010.
       RS-999.
            EXIT.
      *
       START-STOCK SECTION.
       ST-ST-000.
              MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
              START STOCK-MASTER KEY NOT LESS ST-STOCKNUMBER.
       ST-ST-999.
             EXIT.
      *
       READ-STOCK-NEXT SECTION.
       R-ST-NX-000.
             MOVE 0 TO WS-STOCK-ST1.
       R-ST-NX-005. 
             READ STOCK-MASTER NEXT
                 AT END NEXT SENTENCE.
             IF WS-STOCK-ST1 = 10
               MOVE "END OF NEXT-PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
                  TO WS-MESSAGE
                  PERFORM ERROR-MESSAGE
                  PERFORM START-STOCK
                  GO TO R-ST-NX-005.
             IF WS-STOCK-ST1 = 0
                 GO TO R-ST-NX-999
             ELSE
                 MOVE "STOCK FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-STOCK-ST1
                 PERFORM START-STOCK
                 GO TO R-ST-NX-005.
       R-ST-NX-999.
             EXIT.
      *
       READ-STOCK-PREVIOUS SECTION.
       RPREV-000.
             MOVE 0 TO WS-STOCK-ST1.
       RPREV-005. 
             READ STOCK-MASTER PREVIOUS
                 AT END NEXT SENTENCE.
             IF WS-STOCK-ST1 = 10
               MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
                  TO WS-MESSAGE
                  PERFORM ERROR-MESSAGE
                  PERFORM START-STOCK
                  GO TO R-ST-NX-005.
             IF WS-STOCK-ST1 = 0
                 GO TO RPREV-999
             ELSE
                 MOVE "STOCK FILE BUSY ON READ-PREV, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-STOCK-ST1
                 PERFORM START-STOCK
                 GO TO RPREV-005.
       RPREV-999.
             EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-0000.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
           MOVE 3010 TO POS
           DISPLAY "Printing In Progress, Please Be Patient." AT POS.
           MOVE 0 TO PAGE-CNT.
           MOVE 66 TO LINE-CNT.
           PERFORM GET-USER-PRINT-NAME.
           PERFORM OPEN-005.
           OPEN OUTPUT PRINT-FILE.
           
           IF SP-1STCHAR = "#"
               MOVE WS-TRANS-NUM   TO STRE-TRANSACTION-NUMBER
               MOVE WS-CODE-TYPE   TO STRE-TRANSACTION-CODE
               START STKRECEIPTS-FILE KEY NOT < STRE-KEY
               GO TO PRR-001.
               
           IF SP-1STCHAR = "P"
            IF SP-1ST3CHAR = "P/O"
               MOVE SPLIT-PORDER TO STRE-ORDER-NUMBER
               START STKRECEIPTS-FILE KEY NOT < STRE-ORDER-NUMBER
               GO TO PRR-001.

           MOVE ST-STOCKNUMBER TO STRE-STOCK-NUMBER
            START STKRECEIPTS-FILE KEY NOT < STRE-STOCK-NUMBER
               INVALID KEY NEXT SENTENCE.
       PRR-001.
            IF WS-STKRECEIPT-ST1 NOT = 0
               MOVE 0 TO WS-STKRECEIPT-ST1
               GO TO PRR-900.
       PRR-002.
            READ STKRECEIPTS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-STKRECEIPT-ST1 = 10
               MOVE 0 TO WS-STKRECEIPT-ST1
               GO TO PRR-900.
            IF WS-STKRECEIPT-ST1 NOT = 0
             MOVE "RECEIPTS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STKRECEIPT-ST1
               GO TO PRR-002.
               
           IF SP-1STCHAR NOT = "#"
            IF SP-1ST3CHAR NOT = "P/O"
             IF STRE-STOCK-NUMBER < ST-STOCKNUMBER
               GO TO PRR-002.
           IF SP-1STCHAR NOT = "#"
            IF SP-1ST3CHAR NOT = "P/O"
             IF STRE-STOCK-NUMBER > ST-STOCKNUMBER
               GO TO PRR-900.
               
           IF SP-1STCHAR  = "P"
            IF SP-1ST3CHAR = "P/O"
             IF STRE-ORDER-NUMBER < ST-STOCKNUMBER
               GO TO PRR-002.
           IF SP-1STCHAR = "P"
            IF SP-1ST3CHAR = "P/O"
             IF STRE-ORDER-NUMBER > SPLIT-STOCK
               GO TO PRR-900.
               
           IF WS-CODE-TYPE NOT = 0
            IF STRE-TRANSACTION-CODE NOT = WS-CODE-TYPE
                GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060 THRU PRR-070.
       PRR-020.
           MOVE STRE-TRANSACTION-NUMBER TO D-TRANSNO
           MOVE STRE-TRANSACTION-CODE   TO D-CODE
           MOVE STRE-QUANTITY           TO D-QTY
           MOVE STRE-UNIT-PRICE         TO D-UNIT-PRICE
           MOVE STRE-TOTAL-PRICE        TO D-TOTAL-PRICE
           MOVE STRE-REFERENCE-NO       TO D-REFNO
           MOVE STRE-REFERENCE-DATE     TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-DATE.
           IF SP-1STCHAR = "#"
              MOVE STRE-STOCK-NUMBER    TO D-PORDER
           ELSE
              MOVE STRE-ORDER-NUMBER    TO D-PORDER.
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC.
           ADD 1 TO LINE-CNT.
           GO TO PRR-002.
       PRR-060.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
       PRR-065.
           IF SP-1STCHAR = "#"
              MOVE "TRANSACTION NUMBER **" TO H1-TYPE
              MOVE "TRANS#:"               TO H2-TYPE
              MOVE SPACES                  TO H2-STOCKDESC
           ELSE
              MOVE "P/ORDER NUMBER **    " TO H1-TYPE
              MOVE SPACES                  TO H2-TYPE
              MOVE SPACES                  TO H2-STOCKDESC.
       PRR-066.
           IF SP-1STCHAR NOT = "#"
            IF SP-1ST3CHAR NOT = "P/O"
              MOVE "STOCK NUMBER **      " TO H1-TYPE
              MOVE "STOCK :"               TO H2-TYPE
              MOVE "DESCRIPTION:"          TO H2-STOCKDESC.
       PRR-070.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " " TO PRINT-REC
                         
            MOVE ST-STOCKNUMBER   TO H2-STOCK
            MOVE ST-DESCRIPTION1  TO H2-DESC1
            MOVE ST-DESCRIPTION2  TO H2-DESC2
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC.
           IF SP-1STCHAR = "#"
              MOVE "STOCK NUMBER" TO H3-DESC
           ELSE
              MOVE "P/O NUMBER  " TO H3-DESC.
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE " " TO PRINT-REC
            MOVE 6 TO LINE-CNT.
       PRR-900.
           IF LINE-CNT > 60
                PERFORM PRR-060
                PERFORM PRR-066 THRU PRR-070.
                
           MOVE "CODE TYPES ARE:" TO PRINT-REC
           WRITE PRINT-REC AFTER 2
           MOVE "1=RECEIPTS NOT ORDERED" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "2=RECEIPTS ORDERED" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "3=ORDERS ON SUPPLIERS" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "4=GRV'S TO SUPPLIERS" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "5=STOCK ADJUSTMENT" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "6=STOCK SALES" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "7=STOCK LABEL PRINTING" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "8=TRANSFERS OF STOCK FROM RESERVE TO ON-HAND"
            TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "9=B/MATERIAL ADJUSTMENTS" TO PRINT-REC
           WRITE PRINT-REC AFTER 1.

           IF WS-CODE-TYPE = 0
              MOVE "** ALL CODE TYPES PRINTED **" TO T-TYPE
              WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
           ELSE
              MOVE "** ONLY CODE TYPE " TO T-DESC1
              MOVE WS-CODE-TYPE         TO T-CODE
              MOVE "PRINTED **"         TO T-DESC2
              WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.

           MOVE "** THIS YEAR INFO PRINTED **" TO T-TYPE
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           
           MOVE " " TO TOTAL-LINE PRINT-REC.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE STKRECEIPTS-FILE.
       PRR-999.
           EXIT.
      *
       PRINT-LY-ROUTINE SECTION.
       PRRLY-0000.
           PERFORM ERROR-020.
           MOVE 2910 TO POS.
           DISPLAY WS-MESSAGE AT POS.
           MOVE 3010 TO POS
           DISPLAY "Printing In Progress, Please Be Patient." AT POS.
           MOVE 0 TO PAGE-CNT.
           MOVE 66 TO LINE-CNT.
           PERFORM GET-USER-PRINT-NAME.
           PERFORM OPEN-004.
           OPEN OUTPUT PRINT-FILE.
           
           IF SP-1STCHAR = "#"
               MOVE WS-TRANS-NUM   TO STRELY-TRANSACTION-NUMBER
               MOVE WS-CODE-TYPE   TO STRELY-TRANSACTION-CODE
               START STKRECEIPTSLY-FILE KEY NOT < STRELY-KEY
               GO TO PRRLY-001.
               
           IF SP-1STCHAR = "P"
            IF SP-1ST3CHAR = "P/O"
               MOVE SPLIT-PORDER TO STRELY-ORDER-NUMBER
               START STKRECEIPTSLY-FILE KEY NOT < STRELY-ORDER-NUMBER
               GO TO PRRLY-001.

           MOVE ST-STOCKNUMBER TO STRELY-STOCK-NUMBER
           START STKRECEIPTSLY-FILE KEY NOT < STRELY-STOCK-NUMBER
              INVALID KEY NEXT SENTENCE.
       PRRLY-001.
            IF WS-STKRECEIPTSLY-ST1 NOT = 0
               MOVE 0 TO WS-STKRECEIPTSLY-ST1
               GO TO PRRLY-900.
       PRRLY-002.
            READ STKRECEIPTSLY-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-STKRECEIPTSLY-ST1 = 10
               MOVE 0 TO WS-STKRECEIPTSLY-ST1
               GO TO PRRLY-900.
            IF WS-STKRECEIPTSLY-ST1 NOT = 0
           MOVE "RECEIPTSLY BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPTSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STKRECEIPTSLY-ST1
               GO TO PRRLY-002.
               
           IF SP-1STCHAR NOT = "#"
            IF SP-1ST3CHAR NOT = "P/O"
             IF STRELY-STOCK-NUMBER < ST-STOCKNUMBER
               GO TO PRRLY-002.
           IF SP-1STCHAR NOT = "#"
            IF SP-1ST3CHAR NOT = "P/O"
             IF STRELY-STOCK-NUMBER > ST-STOCKNUMBER
               GO TO PRRLY-900.
               
           IF SP-1STCHAR = "P"
            IF SP-1ST3CHAR = "P/O"
             IF STRELY-ORDER-NUMBER < ST-STOCKNUMBER
               GO TO PRRLY-002.
           IF SP-1STCHAR = "P"
            IF SP-1ST3CHAR = "P/O"
             IF STRELY-ORDER-NUMBER > SPLIT-STOCK
               GO TO PRRLY-900.

           IF WS-CODE-TYPE NOT = 0
            IF STRELY-TRANSACTION-CODE NOT = WS-CODE-TYPE
                GO TO PRRLY-002.
       PRRLY-010.
            IF LINE-CNT < 61
               GO TO PRRLY-020.
           PERFORM PRRLY-060 THRU PRRLY-070.
       PRRLY-020.
           MOVE STRELY-TRANSACTION-NUMBER TO D-TRANSNO
           MOVE STRELY-TRANSACTION-CODE   TO D-CODE
           MOVE STRELY-QUANTITY           TO D-QTY
           MOVE STRELY-UNIT-PRICE         TO D-UNIT-PRICE
           MOVE STRELY-TOTAL-PRICE        TO D-TOTAL-PRICE
           MOVE STRELY-REFERENCE-NO       TO D-REFNO
           MOVE STRELY-REFERENCE-DATE     TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE              TO D-DATE
           IF SP-1STCHAR = "#"
              MOVE STRELY-STOCK-NUMBER    TO D-PORDER
           ELSE
              MOVE STRELY-ORDER-NUMBER    TO D-PORDER.
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC.
           ADD 1 TO LINE-CNT.
           GO TO PRRLY-002.
       PRRLY-060.
            ADD 1         TO PAGE-CNT.
            MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
       PRRLY-065.
           IF SP-1STCHAR = "#"
              MOVE "TRANSACTION NUMBER **" TO H1-TYPE
              MOVE "TRANS#:"               TO H2-TYPE
              MOVE SPACES                  TO H2-STOCKDESC
           ELSE
              MOVE "P/ORDER NUMBER **    " TO H1-TYPE
              MOVE SPACES                  TO H2-TYPE
              MOVE SPACES                  TO H2-STOCKDESC.
       PRRLY-066.
           IF SP-1STCHAR NOT = "#"
            IF SP-1ST3CHAR NOT = "P/O"
               MOVE "STOCK NUMBER **      " TO H1-TYPE
               MOVE "STOCK :"               TO H2-TYPE
               MOVE "DESCRIPTION:"          TO H2-STOCKDESC.
       PRRLY-070.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " " TO PRINT-REC

            MOVE ST-STOCKNUMBER   TO H2-STOCK
            MOVE ST-DESCRIPTION1  TO H2-DESC1
            MOVE ST-DESCRIPTION2  TO H2-DESC2
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
           IF SP-1STCHAR = "#"
              MOVE "STOCK NUMBER" TO H3-DESC
           ELSE
              MOVE "P/O NUMBER  " TO H3-DESC.
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE " " TO PRINT-REC
            MOVE 6 TO LINE-CNT.
       PRRLY-900.
           IF LINE-CNT > 60
                PERFORM PRRLY-060
                PERFORM PRRLY-066 THRU PRRLY-070.
                
           MOVE "CODE TYPES ARE:" TO PRINT-REC
           WRITE PRINT-REC AFTER 2
           MOVE "1=RECEIPTS NOT ORDERED" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "2=RECEIPTS ORDERED" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "3=ORDERS ON SUPPLIERS" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "4=GRV'S TO SUPPLIERS" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "5=STOCK ADJUSTMENT" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "6=STOCK SALES" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "7=STOCK LABEL PRINTING" TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "8=TRANSFERS OF STOCK FROM RESERVE TO ON-HAND"
            TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE "9=B/MATERIAL ADJUSTMENTS" TO PRINT-REC
           WRITE PRINT-REC AFTER 1.

           IF WS-CODE-TYPE = 0
              MOVE "** ALL CODE TYPES PRINTED **" TO T-TYPE
              WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
           ELSE
              MOVE "** ONLY CODE TYPE " TO T-DESC1
              MOVE WS-CODE-TYPE         TO T-CODE
              MOVE "PRINTED **"         TO T-DESC2
              WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.
              
           MOVE "** LAST YEAR INFO PRINTED **" TO T-TYPE
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.

           MOVE " " TO TOTAL-LINE PRINT-REC.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE STKRECEIPTSLY-FILE.
       PRRLY-999.
           EXIT.
      *
       SCROLL-NEXT SECTION.
       NEXT-000.
            ADD 1  TO SUB-1.
            IF SUB-1 > SUB-9
               MOVE SUB-9 TO SUB-1.
            IF SUB-1 < 1
               MOVE 1 TO SUB-1 F-INDEX.
            MOVE 1 TO F-INDEX.
            PERFORM CLEAR-TRANSACTIONS.
            MOVE 1 TO F-INDEX.
            IF SUB-1 > 9985
                MOVE 9985 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 16
                GO TO NEXT-010.
            IF SUB-1 > 9985  
                GO TO NEXT-025.
            MOVE 1 TO F-INDEX.
       NEXT-025.
            SUBTRACT 15 FROM SUB-1.
            IF SUB-1 > 9985
              IF SUB-25 > 9985
               COMPUTE F-INDEX = 15 - (10001 - SUB-9)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 15
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3015 TO POS.
            DISPLAY "Current Line#: " AT POS
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-999.
             EXIT.
      *
       SCROLL-NEXT-PAGE SECTION.
       NEXT-PAGE-000.
            ADD 15  TO SUB-1.
            IF SUB-1 > SUB-9
               MOVE SUB-9 TO SUB-1.
            IF SUB-1 < 1
               MOVE 1 TO SUB-1 F-INDEX.
            MOVE 1 TO F-INDEX.
            PERFORM CLEAR-TRANSACTIONS.
            MOVE 1 TO F-INDEX.
            IF SUB-1 > 9985
                 MOVE 9985 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 16
                GO TO NEXT-PAGE-010.
            IF SUB-1 > 9985 
                GO TO NEXT-PAGE-025.
            MOVE 1 TO F-INDEX.
       NEXT-PAGE-025.
            SUBTRACT 15 FROM SUB-1.
            IF SUB-1 > 9985
              IF SUB-25 > 9985
               COMPUTE F-INDEX = 15 - (10001 - SUB-9)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 15
               MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3015 TO POS.
            DISPLAY "Current Line#: " AT POS
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-PAGE-999.
             EXIT.
      *
       SCROLL-PREVIOUS-PAGE SECTION.
       PREV-PAGE-000.
            PERFORM CLEAR-TRANSACTIONS.
            SUBTRACT 15 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-PAGE-010.
            PERFORM SCROLLING.
       PREV-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 16
                GO TO PREV-PAGE-010.
            MOVE 1 TO F-INDEX.
            SUBTRACT 15 FROM SUB-1.
       PREV-PAGE-025.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.
            MOVE 3015 TO POS.
            DISPLAY "Current Line#: " AT POS
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       PREV-PAGE-999.
            EXIT.
      *
       SCROLL-PREVIOUS SECTION.
       PREV-000.
            PERFORM CLEAR-TRANSACTIONS.
            IF F-EXIT-CH = X"01"
               SUBTRACT 15 FROM SUB-1
            ELSE
               SUBTRACT 1 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-010.
            PERFORM SCROLLING.
       PREV-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 16
                GO TO PREV-010.
            MOVE 1 TO F-INDEX.
            SUBTRACT 1 FROM SUB-1.
       PREV-025.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.
            MOVE 3015 TO POS.
            DISPLAY "Current Line#: " AT POS
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       PREV-999.
            EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            IF WS-LY = "N"
             IF SUB-1 < SUB-9
               PERFORM READ-ORDER-ONLY
             ELSE
               GO TO SCROLL-999.
            IF WS-LY = "Y"
             IF SUB-1 < SUB-9
               PERFORM READ-ORDERLY-ONLY
             ELSE
               GO TO SCROLL-999.

            MOVE "CODE"                TO F-FIELDNAME
            MOVE 4                     TO F-CBFIELDNAME
            IF WS-LY = "N"
               MOVE STRE-TRANSACTION-CODE TO F-NAMEFIELD
            ELSE
               MOVE STRELY-TRANSACTION-CODE TO F-NAMEFIELD.
            MOVE 1                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"              TO F-FIELDNAME
            MOVE 4                   TO F-CBFIELDNAME
            IF WS-LY = "N"
               MOVE STRE-REFERENCE-DATE TO SPLIT-DATE
            ELSE
               MOVE STRELY-REFERENCE-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE        TO F-NAMEFIELD
            MOVE 10                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QTY"         TO F-FIELDNAME
            MOVE 3             TO F-CBFIELDNAME
            IF WS-LY = "N"
               MOVE STRE-QUANTITY TO F-EDNAMEFIELDNUMNEG
            ELSE
               MOVE STRELY-QUANTITY TO F-EDNAMEFIELDNUMNEG.
            MOVE 6             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMNEG.

            MOVE "UPRICE"        TO F-FIELDNAME
            MOVE 6               TO F-CBFIELDNAME
            IF WS-LY = "N"
                MOVE STRE-UNIT-PRICE TO F-EDNAMEFIELD99Mil
            ELSE
                MOVE STRELY-UNIT-PRICE TO F-EDNAMEFIELD99Mil.
            MOVE 11              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.

            MOVE "TRANS"          TO F-FIELDNAME
            MOVE 5                TO F-CBFIELDNAME
            IF WS-LY = "N"
                 MOVE STRE-TRANSACTION-NUMBER   TO F-NAMEFIELD
            ELSE
                 MOVE STRELY-TRANSACTION-NUMBER TO F-NAMEFIELD.
            MOVE 11                             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REFNO"           TO F-FIELDNAME
            MOVE 5                 TO F-CBFIELDNAME
            IF WS-LY = "N"
                MOVE STRE-REFERENCE-NO   TO F-NAMEFIELD
            ELSE
                MOVE STRELY-REFERENCE-NO TO F-NAMEFIELD.
            MOVE 20                      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

           IF SP-1STCHAR = "#"
              GO TO SCROLL-500.
           IF SP-1ST3CHAR = "P/O"
              GO TO SCROLL-500.
            MOVE "PORDER"          TO F-FIELDNAME
            MOVE 6                 TO F-CBFIELDNAME
            IF WS-LY = "N"
                MOVE STRE-ORDER-NUMBER   TO F-NAMEFIELD
            ELSE
                MOVE STRELY-ORDER-NUMBER TO F-NAMEFIELD.
            MOVE 16                      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            GO TO SCROLL-999.
       SCROLL-500.
            MOVE "PORDER"          TO F-FIELDNAME
            MOVE 6                 TO F-CBFIELDNAME
            IF WS-LY = "N"
                MOVE STRE-STOCK-NUMBER   TO F-NAMEFIELD
            ELSE
                MOVE STRELY-STOCK-NUMBER TO F-NAMEFIELD.
            MOVE 16                      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO F-INDEX.
       CLTR-010.
            IF F-INDEX > 15
                GO TO CLTR-999.
            MOVE "CODE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 1      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 10     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QTY" TO F-FIELDNAME
            MOVE 3     TO F-CBFIELDNAME
            MOVE " "   TO F-NAMEFIELD
            MOVE 6     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "UPRICE" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 11       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TRANS"  TO F-FIELDNAME
            MOVE 5        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 11       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REFNO" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 20      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PORDER" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 16       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO F-INDEX.
            GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       CLEAR-MEMORY SECTION.
       CMS-005.
            MOVE 1 TO SUB-1.
            MOVE 0 TO SUB-9.
       CMS-010.
            IF WS-STTR-TRANS (SUB-1) NOT = 0
                MOVE 0 TO WS-STTR-TRANS (SUB-1)
                          WS-STTR-CODE (SUB-1)
            ELSE
                GO TO CMS-900.
            IF SUB-1 < 10000
               ADD 1 TO SUB-1
               GO TO CMS-010.
       CMS-900.
            MOVE 1 TO SUB-1.
       CMS-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-003.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-003.
           MOVE Ws-Co-Name TO CO-NAME.
           GO TO OPEN-006.
       OPEN-004.
           OPEN I-O STKRECEIPTSLY-FILE.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
              MOVE "ST-RECEIPTSLY FILE BUSY ON OPEN, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STKRECEIPTSLY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              CLOSE STKRECEIPTSLY-FILE
              GO TO OPEN-004.
       OPEN-005.
           OPEN I-O STKRECEIPTS-FILE.
           IF WS-STKRECEIPT-ST1 NOT = 0
              MOVE "ST-RECEIPTS BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STKRECEIPT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              CLOSE STKRECEIPTS-FILE
              GO TO OPEN-005.
       OPEN-006.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StReLoIq"      TO F-FORMNAME.
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER.
       END-900.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *      
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumNeg".
       Copy "WriteField99Mil".
       Copy "WriteFieldInv".
       Copy "DisplayForm".
       Copy "UserFillField".
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
      *
      * END-OF-JOB.
