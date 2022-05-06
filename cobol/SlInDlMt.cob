        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlInDlMt.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectSlRegister".
          Copy "SelectDrTrans".
          Copy "SelectCoCashSales".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdRegister.
           COPY ChlfdDrTrans.
           COPY ChlfdCashSale.
      *
       WORKING-STORAGE SECTION.
       77  WS-INQ-PROGRAM       PIC X(8) VALUE "SlInOrIq".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-PSDIS             PIC Z9.
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-PRINTER           PIC X(12) VALUE " ".
       77  WS-TYPE-OF-KEY       PIC X(20) VALUE " ".
       77  WS-KEY               PIC X VALUE " ".
       77  WS-DIS               PIC X VALUE " ".
       77  WS-CASH-ACCEPT       PIC X(11) VALUE " ".
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-NO-CH             PIC 9 VALUE 0.
       77  W-INVALID-TYPE       PIC X VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       01  WS-PB-FIELDS-NAME.
         02  WS-PB-FIELDS OCCURS 15.
           03  WS-INV-NO       PIC 9(6).
           03  WS-DEL-DATE     PIC 9(8).
           03  WS-TRAN-DATE    PIC 9(8).
           03  WS-ACCOUNT      PIC 9(7).
           03  WS-NAME         PIC X(20).
           03  WS-PS-AMT       PIC 9(8)V99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1     PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1  PIC 99.
       01  Ws-CashSale-Status.
           03  Ws-CashSale-St1 PIC 99.
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
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-020.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-020.
      *
       QUES-CASH SECTION.
       QPC-005.
           PERFORM CLEAR-010.
           PERFORM ERROR-020
           PERFORM ERROR1-020.
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
       QPC-505.
           MOVE 2610 TO POS
           DISPLAY "INVOICE BEING UPDATED :" AT POS
           ADD 25 TO POS
           DISPLAY INCR-INVOICE AT POS.
           
           MOVE "N" TO WS-DIS
           MOVE 3010 TO POS
           DISPLAY
           "IS THIS A CASH TRANSACTION, ENTER Y,N OR D:[ ]" AT POS
           ADD 44 TO POS

           MOVE WS-DIS    TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DIS.

           IF WS-DIS NOT = "N" AND NOT = "Y" AND NOT = "D"
              DISPLAY " " AT 3079 WITH BELL
              GO TO QPC-505.
           IF WS-DIS = "N"
              GO TO QPC-900.
           IF WS-DIS = "D"
              GO TO QPC-600.
           PERFORM ERROR-020.
       QPC-515.
           MOVE 2910 TO POS
           DISPLAY
           "ENTER THE AMOUNT PAID BY CASH : [           ]" AT POS
           ADD 33 TO POS
           MOVE INCR-INVCRED-AMT   TO F-EDNAMEFIELD99MIL
           MOVE F-EDNAMEFIELD99MIL TO WS-CASH-ACCEPT
           DISPLAY F-EDNAMEFIELD99MIL AT POS.

           MOVE F-EDNAMEFIELD99MIL TO CDA-DATA.
           MOVE 11        TO CDA-DATALEN.
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
           MOVE NUMERIC-RATE   TO CS-SALE-AMOUNT F-EDNAMEFIELD99MIL
           DISPLAY F-EDNAMEFIELD99MIL AT POS.
           IF SIGN-FOUND = 1
              GO TO QPC-515.
           IF CS-SALE-AMOUNT = 0
              DISPLAY " " AT 3079 WITH BELL
              GO TO QPC-515.
       QPC-517.
           MOVE 3010 TO POS
           DISPLAY "IS THE AMOUNT ENTERED CORRECT : [ ]" AT POS
           ADD 33 TO POS

           MOVE 'Y'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DIS.

           IF W-ESCAPE-KEY = 4
              GO TO QPC-515.
           IF WS-DIS NOT = "Y" AND NOT = "N"
              DISPLAY " " AT 3079 WITH BELL
              GO TO QPC-517.
           IF WS-DIS = "N"
              GO TO QPC-515.
           MOVE INCR-INVOICE   TO CS-INVOICE
           MOVE INCR-ACCOUNT   TO CS-ACCOUNT
           MOVE INCR-DATE      TO CS-INV-DATE
           MOVE INCR-SB-TYPE   TO CS-INITIAL.
       QPC-520.
           WRITE CashSale-Rec
              INVALID KEY NEXT SENTENCE.
           IF Ws-CashSale-ST1 = 22 OR = 23 OR = 35 OR = 49
      *         MOVE
      *          "CASHSALE RECORD ALREADY EXISTS, 'ESC' TO EXIT."
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               GO TO QPC-530.
           IF Ws-CashSale-ST1 NOT = 0
               MOVE
                "CASHSALE RECORD NOT WRITTEN, ADVISE YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CASHSALE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020.
            GO TO QPC-900.
       QPC-530.
           REWRITE CashSale-Rec
              INVALID KEY NEXT SENTENCE.
           IF Ws-CashSale-ST1 = 22 OR = 23 OR = 35 OR = 49
      *          MOVE
      *          "CASHSALE RECORD ALREADY EXISTS, 'ESC' TO EXIT."
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               GO TO QPC-900.
           IF Ws-CashSale-ST1 NOT = 0
               MOVE
                "CASHSALE RECORD NOT REWRITTEN, ADVISE YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CASHSALE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020.
            GO TO QPC-900.
       QPC-600.
           MOVE INCR-INVOICE TO CS-INVOICE.
           START CASH-SALE KEY NOT < CS-KEY
              INVALID KEY NEXT SENTENCE.
       QPC-605.
           READ CASH-SALE WITH LOCK
              INVALID KEY NEXT SENTENCE.
           IF WS-CASHSALE-ST1 = 23 OR = 35 OR = 49
               MOVE
          "THIS CASHSALE RECORD DOESN'T EXIST TO DELETE, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO QPC-900.
           IF WS-CASHSALE-ST1 NOT = 0
               MOVE "CASHSALE BUSY ON READ TO DELETE, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CASHSALE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO QPC-605.
           MOVE 2650 TO POS
           DISPLAY "** FOUND **" AT POS.
       QPC-610.
           DELETE CASH-SALE
              INVALID KEY NEXT SENTENCE.
           IF WS-CASHSALE-ST1 NOT = 0
               MOVE "CASHSALE BUSY ON DELETE, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CASHSALE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO QPC-610.
       QPC-900.
           PERFORM ERROR-020.
           PERFORM ERROR1-020.
       QPC-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000. 
            MOVE 2710 TO POS
            DISPLAY
           "ENTER THE INVOICE# AND <PgDn> TO READ NUMBERS ONWARDS"
              AT POS
            MOVE 2810 TO POS
            DISPLAY 
           "OR ENTER INVOICE # & <ALT-G> TO RECALL A FLAGGED INVOICE."
              AT POS.
            MOVE "N" TO WS-ANSWER
                        W-INVALID-TYPE.
            MOVE " " TO WS-NEWINPUT.
            MOVE 1   TO F-INDEX.

            PERFORM ERROR1-020
            PERFORM ERROR-020.
            PERFORM CLEAR-MEMORY.
            
            MOVE "TRANS" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            MOVE 6       TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO INCR-INVOICE
                                 WS-INVOICE.
            
            IF INCR-INVOICE = 0
                CLOSE INCR-REGISTER
                CALL WS-INQ-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQ-PROGRAM
                PERFORM CLEAR-SCREEN
                OPEN I-O INCR-REGISTER
                PERFORM DISPLAY-FORM
                GO TO GET-000.
      * X"9B" = <alt-g>  X"c7" = <alt-G>
           IF F-EXIT-CH NOT = X"0A" AND NOT = X"0C"
                    AND NOT = X"9B" AND NOT = X"C7"
               MOVE
           "YOU MAY ONLY PRESS <RETURN>, <PgDn> OR <ALT-G>"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-000.
            IF F-EXIT-CH = X"0A" OR = X"9B" OR = X"C7"
               MOVE "1" TO WS-NEWINPUT
               PERFORM READ-ONE-TRANSACTION
               GO TO GET-920.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            MOVE 2810 TO POS
            DISPLAY WS-MESSAGE AT POS.
       GET-900.
            PERFORM READ-TRANSACTIONS.
            IF WS-NEWINPUT = "8"
                GO TO GET-950.
            PERFORM ERROR1-020.
            MOVE SUB-1 TO SUB-2.
            IF SUB-2 NOT > 1
              MOVE "NO FURTHAR TRANSACTIONS, 'ESC' TO END."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO GET-999.
       GET-920.
            IF WS-NEWINPUT = "1"
             IF WS-INCR-ST1 = 88
               GO TO GET-950.
            PERFORM SCROLLING.
            PERFORM FILL-DATA.
       GET-950.
            PERFORM CLEAR-TRANSACTIONS.
            IF WS-ABOVE-BODY = "8"
               GO TO GET-900.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-000.
           PERFORM ERROR-020
           MOVE 2701 TO POS
           DISPLAY  WS-MESSAGE AT POS
           MOVE 2801 TO POS
           DISPLAY  WS-MESSAGE AT POS.
           
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE 1 TO SUB-1 F-INDEX.
           MOVE " " TO WS-ABOVE-BODY.
       FILL-005.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.
          IF WS-INV-NO (SUB-1) = 0 OR = " "
              GO TO FILL-060.
       FILL-013.
           MOVE "DEL-DATE"   TO F-FIELDNAME
           MOVE 8            TO F-CBFIELDNAME.
           IF F-EXIT-CH NOT = X"9B" AND NOT = X"C7"
               MOVE WS-DATE        TO SPLIT-DATE
           ELSE
               MOVE INCR-PULL-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO F-NAMEFIELD.
           MOVE 10           TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-ALPHA.
       FILL-014.           
           MOVE "                    " TO F-NAMEFIELD.
           MOVE "DEL-DATE" TO F-FIELDNAME
           MOVE 8          TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           MOVE 10          TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA.
           IF F-NAMEFIELD = " "
              GO TO FILL-020.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO FILL-014.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE DISPLAY-DATE TO F-NAMEFIELD.
           PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-DEL-DATE (SUB-1) WS-DATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO FILL-014.
           PERFORM ERROR-020.
       FILL-020.
           IF F-EXIT-CH = X"01" AND F-INDEX = 1
              MOVE "1" TO WS-ABOVE-BODY
              GO TO FILL-999.
           IF F-EXIT-CH = X"01" AND F-INDEX > 1
              SUBTRACT 1 FROM F-INDEX SUB-1 SUB-2
              GO TO FILL-005.
      *****************
      *TAB KEY = X"09"*
      *****************
           IF F-EXIT-CH = X"09"
              GO TO FILL-060.
              
           IF F-EXIT-CH = X"12" AND F-INDEX < 15
              ADD 1 TO F-INDEX SUB-1 SUB-2
              GO TO FILL-005.
           IF F-EXIT-CH = X"0B" AND F-INDEX < 15
              ADD 1 TO F-INDEX SUB-1 SUB-2
              GO TO FILL-005.
           IF F-EXIT-CH = X"0A" AND F-INDEX < 15
              ADD 1 TO F-INDEX SUB-1 SUB-2
              GO TO FILL-005.
           IF F-EXIT-CH = X"0B" AND F-INDEX = 15
              MOVE "8" TO WS-ABOVE-BODY
              GO TO FILL-060.
           IF F-EXIT-CH = X"0A" AND F-INDEX = 15
              MOVE "8" TO WS-ABOVE-BODY
              GO TO FILL-060.
           IF F-EXIT-CH = X"12" AND F-INDEX = 15
              MOVE "8" TO WS-ABOVE-BODY
              GO TO FILL-060.
           IF F-EXIT-CH = X"07"
              MOVE 0 TO INCR-PULL-DATE
              PERFORM FILL-060
              GO TO FILL-999.
       FILL-050.
            IF SUB-1 > 14
               GO TO FILL-060.
            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 > SUB-2
               MOVE SUB-1 TO SUB-2.
            GO TO FILL-005.
       FILL-060.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
            MOVE 2910 TO POS
            DISPLAY "WRITING CHANGES TO INVOICE-FILE....." AT POS
            PERFORM REWRITE-REGISTER.
       FILL-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-005.
           PERFORM ERROR1-020
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE WS-INVOICE TO INCR-INVOICE.
           MOVE 1          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
                INVALID KEY NEXT SENTENCE.
           MOVE 1 TO SUB-1 F-INDEX.
       RDTR-010.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10 OR = 91
               GO TO RDTR-999.
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
               GO TO RDTR-010.
            
           IF INCR-TRANS NOT = 1
               GO TO RDTR-999.

           IF INCR-PULL-DATE NOT = 0
               MOVE 2910 TO POS
               DISPLAY "Be Patient, Reading Next Valid INVOICE..."
                AT POS
               GO TO RDTR-010.
               
           MOVE INCR-INVOICE     TO WS-INV-NO (SUB-1)
           MOVE INCR-DATE        TO WS-TRAN-DATE (SUB-1)
           MOVE INCR-ACCOUNT     TO WS-ACCOUNT (SUB-1)
           MOVE INCR-NAME        TO WS-NAME (SUB-1)
           MOVE INCR-INVCRED-AMT TO WS-PS-AMT (SUB-1).
           
           IF SUB-1 < 15
              ADD 1 TO SUB-1
              MOVE 2810 TO POS
              DISPLAY "Number Of Invoices Found :" AT POS
              ADD 26 TO POS
              MOVE SUB-1 TO WS-PSDIS
              DISPLAY WS-PSDIS AT POS
              GO TO RDTR-010.
           PERFORM ERROR-020.
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS.
       RDTR-999.
           EXIT.
      *
       READ-ONE-TRANSACTION SECTION.
       ROT-005.
           MOVE WS-INVOICE TO INCR-INVOICE.
           MOVE 1          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
                INVALID KEY NEXT SENTENCE.
           MOVE 1 TO SUB-1 F-INDEX.
       ROT-010.
           READ INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49 OR = 91
               MOVE
             "THIS INVOICE NUMBER DOES NOT EXIT, PLEASE RE-ENTER."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 88 TO WS-INCR-ST1
               GO TO ROT-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO ROT-010.
               
           IF F-EXIT-CH NOT = X"9B" AND NOT = X"C7"
            IF INCR-PULL-DATE NOT = 0
               MOVE
             "THIS INVOICE NUMBER HAS ALREADY BEEN ALLOCATED, RE-ENTER."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 88 TO WS-INCR-ST1
               GO TO ROT-999.
               
           MOVE INCR-INVOICE     TO WS-INV-NO (SUB-1)
           MOVE INCR-DATE        TO WS-TRAN-DATE (SUB-1)
           MOVE INCR-ACCOUNT     TO WS-ACCOUNT (SUB-1)
           MOVE INCR-NAME        TO WS-NAME (SUB-1)
           MOVE INCR-INVCRED-AMT TO WS-PS-AMT (SUB-1).
           
           ADD 1 TO SUB-1.
       ROT-999.
           EXIT.
      *
       REWRITE-REGISTER SECTION.
       RWR-005.
           MOVE 1 TO SUB-1.
           
           MOVE 1 TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
                INVALID KEY NEXT SENTENCE.
       RWR-008.
           IF WS-DEL-DATE (SUB-1) = 0
                GO TO RWR-070.
           MOVE WS-INV-NO (SUB-1) TO INCR-INVOICE WS-INVOICE.
       RWR-010.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49 OR 91
               GO TO RWR-999.
           IF WS-INCR-ST1 NOT = 0
             MOVE "REGISTER FILE BUSY ON READ-LOCK, 'ESC' TO RETRY."
             TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RWR-010.
           ACCEPT WS-TIME FROM TIME.
           MOVE WS-DEL-DATE (SUB-1) TO INCR-PULL-DATE
           MOVE WS-TIME             TO INCR-PULL-TIME.
       RWR-060.
           REWRITE INCR-REC
                INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
                MOVE "REGISTER BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-INCR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-INCR-ST1
                GO TO RWR-060.
           PERFORM UPDATE-DEBTOR-TRANSACTION.
           PERFORM QUES-CASH.
       RWR-070.
           IF SUB-1 < 15
              ADD 1 TO SUB-1
            IF WS-INV-NO (SUB-1) NOT = 0
              GO TO RWR-008.
       RWR-999.
           EXIT.
      *
       UPDATE-DEBTOR-TRANSACTION SECTION.
       UDT-000. 
           MOVE 1               TO DRTR-TYPE.
           MOVE INCR-DRTRANS-NO TO DRTR-TRANSACTION-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY
               INVALID KEY NEXT SENTENCE.
       UDT-010.
           READ DEBTOR-TRANS-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
               MOVE "DR-TRANS NOT THERE ON READ-LOCK, 'ESC' TO EXIT."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UDT-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DEBTOR TRANS BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO UDT-010.
           IF DRTR-REFERENCE2 = INCR-INVOICE
               MOVE WS-DEL-DATE (SUB-1) TO DRTR-DEL-DATE
               GO TO UDT-020.
           GO TO UDT-999.
       UDT-020.
           REWRITE DEBTOR-TRANS-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
               MOVE "NO SUCH DR-TRANS TO UPDATE, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UDT-999.
           IF WS-DRTRANS-ST1 = 91
               MOVE "DR-TRANS NOT UPDATED, ST1=91, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO UDT-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DR-TRANS BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO UDT-020.
       UDT-999.
           EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE 1 TO SUB-1 F-INDEX.
       SCROLL-005.
            IF WS-INV-NO (SUB-1) = 0 OR = " "
                GO TO SCROLL-999.
                
            MOVE "PSLIP"           TO F-FIELDNAME.
            MOVE 5                 TO F-CBFIELDNAME.
            MOVE WS-INV-NO (SUB-1) TO F-EDNAMEFIELDNUM.
            MOVE 6                 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "DEL-DATE" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE " "        TO F-NAMEFIELD.
            MOVE 10         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"               TO F-FIELDNAME
            MOVE 4                    TO F-CBFIELDNAME
            MOVE WS-TRAN-DATE (SUB-1) TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE         TO F-NAMEFIELD
            MOVE 10                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNO"            TO F-FIELDNAME.
            MOVE 5                  TO F-CBFIELDNAME.
            MOVE WS-ACCOUNT (SUB-1) TO F-NAMEFIELD.
            MOVE 7                  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNAME"       TO F-FIELDNAME.
            MOVE 7               TO F-CBFIELDNAME.
            MOVE WS-NAME (SUB-1) TO F-NAMEFIELD.
            MOVE 20              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT"          TO F-FIELDNAME
            MOVE 6                 TO F-CBFIELDNAME
            MOVE WS-PS-AMT (SUB-1) TO F-EDNAMEFIELD99MIL
            MOVE 11                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.
            
            IF SUB-1 < 15
               ADD 1 TO SUB-1 F-INDEX
               GO TO SCROLL-005.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO SUB-1 F-INDEX.
       CLTR-010.
            MOVE "PSLIP" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            MOVE " "     TO F-NAMEFIELD.
            MOVE 6       TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-DATE" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE " "        TO F-NAMEFIELD.
            MOVE 10         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            MOVE " "    TO F-NAMEFIELD.
            MOVE 10     TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            MOVE " "     TO F-NAMEFIELD.
            MOVE 7       TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNAME" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 20        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 11       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            IF SUB-1 < 15
                ADD 1 TO SUB-1 F-INDEX
                MOVE " " TO WS-PB-FIELDS (SUB-1)
                GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       CLEAR-MEMORY SECTION.
       CMS-005.
            MOVE 1 TO SUB-1.
       CMS-010.
            MOVE SPACES TO WS-PB-FIELDS (SUB-1).
            IF SUB-1 < 15
              ADD 1 TO SUB-1
              GO TO CMS-010.
            MOVE 1 TO SUB-1.
       CMS-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-003.
           OPEN I-O CASH-SALE.
           IF WS-CASHSALE-ST1 NOT = 0
              MOVE 0 TO WS-CASHSALE-ST1
              MOVE "CASHSALE ERROR ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-003.
           MOVE " " TO WS-CASH-ACCEPT.
           MOVE 88  TO WS-CASHSALE-ST1.
       OPEN-005.
           OPEN I-O DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-DRTRANS-ST1
              MOVE "DR-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.
       OPEN-006.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-008.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlInDlMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-900.
            CLOSE INCR-REGISTER
                  DEBTOR-TRANS-FILE
                  Cash-Sale.
            EXIT PROGRAM.
      *       STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteField99Mil".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldDate".
       Copy "WriteFieldPrice".
       Copy "WriteFieldQty".
       Copy "GetSystemY2KDate".
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
      *
      * END-OF-JOB.
