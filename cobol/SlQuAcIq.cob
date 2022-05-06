        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlQuAcIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectStTrans".
         Copy "SelectSlRegister".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdRegister.
           COPY ChlfdStTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "DrNameIq".
       77  WS-INV-INQ-PROGRAM   PIC X(8) VALUE "SlInOrIq".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ACCOUNTNUMBER     PIC X(7) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-BODY-LINE         PIC Z(5)9.
       01  WS-SCROLL-NUMBERS.
           03  WS-SCROLL-NUM OCCURS 50000.
             05  WS-STTR-TYPE   PIC 99.
             05  WS-STTR-REF    PIC 9(6).
             05  WS-STTR-TRANS  PIC 9(6).
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1    PIC 99.
       01  WS-SALEDATE.
           03 WS-SALEYY         PIC 9999.
           03 WS-SALEMM         PIC 99.
           03 WS-SALEDD         PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(75) VALUE
           "** QUOTE ITEM INQUIRY BY ACCOUNT **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(1) VALUE " ".
           03  FILLER         PIC X(10) VALUE "ACCOUNT :".
           03  H3-ACCOUNT     PIC X(10).
           03  FILLER         PIC X(7) VALUE "NAME :".
           03  H3-NAME        PIC X(52).
       01  HEAD3.
           03  FILLER         PIC X(16) VALUE "STOCK".
           03  FILLER         PIC X(30) VALUE "DESCRIPTION".
           03  FILLER         PIC X(23) VALUE "  QTY".
           03  FILLER         PIC X(28) VALUE 
           "PRICE QUOTE REQUEST NUMBER".
           03  FILLER         PIC X(15) VALUE "QUOTE".
           03  FILLER         PIC X(16) VALUE " DATE".
           03  FILLER         PIC X(6) VALUE "TRANS".
       01  DETAIL-LINE.
           03  D-STOCKNO      PIC X(16).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(10).
           03  D-ORDERQTY     PIC Z(4)9.
           03  FILLER         PIC X(12) VALUE " ".
           03  D-PRICE        PIC Z(7)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PONO         PIC X(21).
           03  D-1STINV       PIC Z(5)9.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-1STDATE      PIC X(10).
           03  FILLER         PIC X(9) VALUE " ".
           03  D-TRANS        PIC Z(5)9.
       01  DATE-LINE.
           03  FILLER          PIC X(24).
           03  DT-NAME         PIC X(29) VALUE 
           "** REPORT ONLY PRINTED FROM:".
           03  DT-ENTER-DATE   PIC 99/99/9999.
           03  FILLER          PIC X(3) VALUE " **".
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
           DISPLAY "** QUOTATION INQUIRY BY ACCOUNT NUMBER **" AT POS
           MOVE 0410 TO POS
           DISPLAY "*****************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-020.
           MOVE 2910 TO POS
           DISPLAY "Program loading, please be patient...." AT POS.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-050.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-050.
       CONTROL-999.
           Exit.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "N" TO WS-ANSWER.
            PERFORM OPEN-005.
            PERFORM CLEAR-MEMORY.
       GET-001.
            MOVE 2905 TO POS
            DISPLAY 
           "Press 'PgDn' For Next Account, 'PgUp' For Previous Account,"
            AT POS
            MOVE 3005 TO POS
            DISPLAY " Or Enter Account Number." AT POS

            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
               PERFORM READ-DEBTOR-NEXT
               GO TO GET-010.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO DR-ACCOUNT-NUMBER.
            IF DR-ACCOUNT-NUMBER = 0
                CLOSE DEBTOR-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-000
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            PERFORM READ-DEBTORS.
            GO TO GET-020.
       GET-010.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DR-ACCOUNT-NUMBER TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-020.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
        
            MOVE "NAME"  TO F-FIELDNAME.
            MOVE 4       TO F-CBFIELDNAME.
            MOVE DR-NAME TO F-NAMEFIELD.
            MOVE 40      TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            IF DR-NAME = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                CLOSE STOCK-TRANS-FILE
                GO TO GET-000.
       GET-035.
           MOVE "                        " TO F-NAMEFIELD
           MOVE "DATE" TO F-FIELDNAME
           MOVE 4      TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
               GO TO GET-001.
           IF F-EXIT-CH = X"07"
               PERFORM DISPLAY-FORM
               CLOSE STOCK-TRANS-FILE
               GO TO GET-000.
           MOVE 10        TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           IF F-NAMEFIELD = "  "
               MOVE 0 TO WS-SALEDATE
               GO TO GET-040.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO GET-035.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE DT-ENTER-DATE.
           MOVE DISPLAY-DATE    TO F-NAMEFIELD.
           PERFORM WRITE-FIELD-ALPHA.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-SALEDATE WS-DATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO GET-035.
           PERFORM ERROR-020.
            
           MOVE "DATE"       TO F-FIELDNAME
           MOVE 4            TO F-CBFIELDNAME
           MOVE WS-SALEDATE  TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO F-NAMEFIELD.
           MOVE 10            TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-ALPHA.
       GET-040.
            MOVE " " TO F-EXIT-CH.
            CLOSE STOCK-TRANS-FILE.

            PERFORM READ-ALL-TRANSACTIONS.
            PERFORM FILL-BODY.
            IF F-EXIT-CH = X"07" OR = X"09" OR = X"1F"
                PERFORM CLEAR-TRANSACTIONS
                MOVE 1   TO F-INDEX SUB-1
                MOVE "Y" TO WS-ANSWER
                GO TO GET-999.

            GO TO GET-999.

      *      MOVE " " TO F-EXIT-CH.
      *      PERFORM READ-TRANSACTIONS.
       GET-900.
            PERFORM ERROR1-020
            PERFORM ERROR-020.

            IF WS-ANSWER = "Y"
                CLOSE STOCK-TRANS-FILE
                GO TO GET-999.
            IF F-INDEX < 15
             IF F-INDEX NOT = "07"
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3010 TO POS
               DISPLAY "Or Press 'F10' To Print All Quote Items" AT POS
               ADD 40 TO POS
               DISPLAY "For This Account." AT POS
               MOVE 15 TO F-INDEX
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F" AND NOT = X"04"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE "Printing In Progress, Please Be Patient." 
                   TO WS-MESSAGE
                PERFORM ERROR-000
                OPEN I-O STOCK-TRANS-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE STOCK-TRANS-FILE
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO GET-999.
       GET-999.
            EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
           PERFORM OPEN-005.

           MOVE 1 TO F-INDEX.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
           PERFORM SCROLL-PREVIOUS-PAGE.
       FILL-001.
           MOVE 2702 TO POS
           DISPLAY "Press 'PgDn' For More, 'PgUp' For Prev,"
           AT POS
           ADD 40 TO POS
           DISPLAY "'F12' OR 'F11' to Scroll Up/Down," AT POS
           MOVE 2803 TO POS
           DISPLAY 
        "'ESC' OR 'TAB' To Clear The Screen, 'F10' To Print All" &
           " Transactions." AT POS.
           MOVE 2950 TO POS
           DISPLAY "'Alt-Z' = Zoom on trans." AT POS.
       FILL-010.
           MOVE 3015 TO POS 
           DISPLAY "Current Line#: " AT POS
           ADD 16 TO POS
           MOVE SUB-1 TO WS-BODY-LINE
           DISPLAY WS-BODY-LINE AT POS.

           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.

            MOVE "STOCKNO"         TO F-FIELDNAME.
            MOVE 7                 TO F-CBFIELDNAME.
            MOVE 15                TO F-CBFIELDLENGTH.
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
             IF SUB-1 < 16
              MOVE F-INDEX TO SUB-1
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
      *TAB - <ALT-F8>
           IF F-EXIT-CH = X"09"
              GO TO FILL-900.
      *FINISH - <End>
           IF F-EXIT-CH = X"04"
              CLOSE STOCK-TRANS-FILE
              PERFORM END-OFF.
      *ESC
           IF F-EXIT-CH = X"07"
              GO TO FILL-900.
      * <f10> to print
           IF F-EXIT-CH = X"1F"
                CLOSE STOCK-TRANS-FILE
                MOVE "Printing In Progress, Please Be Patient."
                   TO WS-MESSAGE
                PERFORM ERROR-000
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO FILL-900.
      ***********************************************
      *ZOOMBOX MODE                                 *
      * <CODE-z> = X"FA"  <CODE-SHIFT-Z> = X"DA"    *
      ***********************************************
      *IN CTOS: <CODE-Z>;  IN LINUX: <ALT-Z>
           IF F-EXIT-CH = X"FA" OR = X"DA"
            IF WS-STTR-TYPE (SUB-1) = 8
                PERFORM ADD-TYPE-TO-NUMBER
                MOVE ALPHA-RATE TO WS-LINK-ACCOUNT
            ELSE
                MOVE 0          TO WS-LINK-ACCOUNT.
                
           IF F-EXIT-CH = X"FA" OR = X"DA"
                MOVE SUB-1        TO SUB-1SAVE
                MOVE F-INDEX      TO F-INDEXSAVE
                PERFORM CLEAR-SCREEN
                CALL WS-INV-INQ-PROGRAM USING WS-LINKAGE
                CANCEL WS-INV-INQ-PROGRAM
                MOVE 0 TO WS-LINK-ACCOUNT
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                PERFORM GET-010 THRU GET-020
                PERFORM FILL-001
                PERFORM CALC-POS-OF-CURSOR
                SUBTRACT 1 FROM SUB-9
                PERFORM RDALL-920
                GO TO FILL-010.

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
           CLOSE STOCK-TRANS-FILE.
       FILL-999.
           EXIT.
      *
       ADD-TYPE-TO-NUMBER SECTION.
       ATTN-005.
           MOVE SPACES TO ALPHA-RATE
                           DATA-RATE.
           MOVE WS-STTR-REF (SUB-1) TO DATA-RATE
           MOVE 8 TO AL-RATE (1)
           GO TO ATTN-006.

           MOVE "0000000" TO ALPHA-RATE
           GO TO ATTN-999.
       ATTN-006.
           MOVE 1 TO SUB-10
           MOVE 2 TO SUB-15.
       ATTN-010.
           IF SUB-15 < 8
              MOVE DAT-RATE (SUB-10) TO AL-RATE (SUB-15)
              ADD 1 TO SUB-10 SUB-15
              GO TO ATTN-010.
       ATTN-999.
           EXIT.
      *
       CALC-POS-OF-CURSOR SECTION.
       CPOC-010.
            COMPUTE SUB-1 = SUB-1SAVE - F-INDEXSAVE.
            IF SUB-1 < 0
               MOVE 1 TO SUB-1.
            PERFORM SCROLL-NEXT.
       CPOC-500.
            MOVE SUB-1SAVE   TO SUB-1
            MOVE F-INDEXSAVE TO F-INDEX.
       CPOC-999.
           EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0
               MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-000.
       RDTR-001.
           MOVE 1 TO F-INDEX.
       RDTR-005.
           MOVE "Q"               TO STTR-AC-COMPLETE
           MOVE DR-ACCOUNT-NUMBER TO STTR-ACCOUNT-NUMBER
           MOVE WS-SALEDATE       TO STTR-AC-DATE.
           START STOCK-TRANS-FILE KEY NOT < STTR-AC-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              CLOSE STOCK-TRANS-FILE
              PERFORM RDTR-999.
           MOVE " " TO F-EXIT-CH.
       RDTR-010.
           IF F-EXIT-CH = " "
             READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
             READ STOCK-TRANS-FILE PREVIOUS
               AT END NEXT SENTENCE.
          IF WS-STTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-999.
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
               GO TO RDTR-010.
           IF STTR-TYPE NOT = 8
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid Transaction...." AT POS
               GO TO RDTR-010.
           IF STTR-AC-COMPLETE NOT = "Q"
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-999.
           IF STTR-AC-COMPLETE NOT = "Q"
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-005.
           MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid Transaction...." AT POS
               GO TO RDTR-010.
           IF WS-SALEDATE NOT = 0
            IF STTR-DATE < WS-SALEDATE
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid Transaction...." AT POS
               GO TO RDTR-010.
           IF F-EXIT-CH = " "
            IF STTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-999.
           IF F-EXIT-CH = 1
            IF STTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-000.
       RDTR-020.
           IF F-INDEX > 15
                MOVE 2905 TO POS
                DISPLAY "Press 'PgDn' For More, 'PgUp' For Previous,"
                 AT POS
                ADD 44 TO POS
                DISPLAY "Or 'Esc' To Clear The Screen !" AT POS
                MOVE 3005 TO POS
                DISPLAY "Or Press 'F10' To Print All Quote Items"
                   AT POS
                ADD 40 TO POS
                DISPLAY "For This Account." AT POS
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
                MOVE "Printing In Progress, Please Be Patient."
                   TO WS-MESSAGE
                PERFORM ERROR-000
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR-020
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                    AND NOT = X"07" AND NOT = " "   AND NOT = X"1F"
                    AND NOT = 1
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           IF STTR-REFERENCE1 NOT = INCR-INVOICE
              PERFORM READ-ORDER-REGISTER.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTR-010.
       RDTR-999.
           EXIT.
      *
       READ-ALL-TRANSACTIONS SECTION.
       RDALL-000.
           PERFORM OPEN-005.
           MOVE 1 TO F-INDEX.
           MOVE 0 TO SUB-2.
       RDALL-005.
           MOVE "Q"               TO STTR-AC-COMPLETE
           MOVE DR-ACCOUNT-NUMBER TO STTR-ACCOUNT-NUMBER
           MOVE WS-SALEDATE       TO STTR-AC-DATE.
           START STOCK-TRANS-FILE KEY NOT < STTR-AC-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              CLOSE STOCK-TRANS-FILE
              GO TO RDALL-999.
       RDALL-010.
           READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
          IF WS-STTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDALL-900.
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
               GO TO RDALL-010.
           IF STTR-TYPE NOT = 8
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid Type Transaction...." AT POS
               GO TO RDALL-010.
           IF STTR-AC-COMPLETE NOT = "Q"
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDALL-900.
           IF STTR-AC-COMPLETE NOT = "Q"
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDALL-005.
           MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid * Transaction...." AT POS
               GO TO RDALL-010.
           IF WS-SALEDATE NOT = 0
            IF STTR-DATE < WS-SALEDATE
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid DateTransaction...." AT POS
               GO TO RDALL-010.
           IF STTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDALL-900.
       RDALL-020.
           MOVE STTR-TYPE               TO WS-STTR-TYPE (SUB-1)
           MOVE STTR-REFERENCE1         TO WS-STTR-REF (SUB-1).
           MOVE STTR-TRANSACTION-NUMBER TO WS-STTR-TRANS (SUB-1).
           
           IF SUB-1 < 50000
              ADD 1 TO SUB-1
              PERFORM RDALL-920
              GO TO RDALL-010.
              
           MOVE "THERE ARE MORE THAN 50,000 ITEMS ON THIS ORDER."
             TO WS-MESSAGE
             PERFORM ERROR1-000
           MOVE "PRESS 'Esc' TO EXIT THE READ-ALL SECTION."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
       RDALL-900.
           PERFORM ERROR1-020.
           
           SUBTRACT 1 FROM SUB-1
           MOVE SUB-1 TO SUB-9.
           IF SUB-9 < 0
               MOVE 0 TO SUB-9.
       RDALL-920.          
           MOVE 2912 TO POS.
           DISPLAY "Total # of Lines:" AT POS
           ADD 19 TO POS.
           MOVE SUB-9 TO WS-BODY-LINE.
           DISPLAY WS-BODY-LINE AT POS.
           ADD 1 TO SUB-9.
       RDALL-950.
           CLOSE STOCK-TRANS-FILE.
       RDALL-999.
           EXIT.
      *
       READ-ORDER-ONLY SECTION.
       RDONLY-005.
           IF SUB-1 > SUB-9
               GO TO RDONLY-999.
               
           MOVE WS-STTR-TYPE (SUB-1)   TO STTR-TYPE
           MOVE WS-STTR-REF  (SUB-1)   TO STTR-REFERENCE1
           MOVE WS-STTR-TRANS  (SUB-1) TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
                CLOSE STOCK-TRANS-FILE
                GO TO RDONLY-999.
           MOVE " " TO F-EXIT-CH.
       RDONLY-010.
            READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
          IF WS-STTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               GO TO RDONLY-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "STTRANS BUSY ON READ-ONLY, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RDONLY-010.

           IF STTR-REFERENCE1 NOT = INCR-INVOICE
              PERFORM READ-ORDER-REGISTER.
       RDONLY-999.
           EXIT.
      *
       READ-DEBTORS SECTION.
       RD-000.
            MOVE DR-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER.
            START DEBTOR-MASTER KEY NOT < DR-KEY.
       RD-010.
            READ DEBTOR-MASTER
                INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE WS-ACCOUNTNUMBER TO DR-ACCOUNT-NUMBER
                MOVE " " TO DR-NAME DR-ADDRESS1 DR-ADDRESS2
                         DR-ADDRESS3
                MOVE "UNKNOWN" TO DR-NAME
                MOVE 0 TO DR-POST-CODE
                GO TO RD-999.
            IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DR RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO RD-010.
       RD-999.
            EXIT.
      *
       START-DEBTOR SECTION.
       DR-DR-000.
              MOVE WS-ACCOUNTNUMBER TO DR-ACCOUNT-NUMBER.
              START DEBTOR-MASTER KEY NOT LESS DR-KEY.
       DR-DR-999.
             EXIT.
      *
       READ-DEBTOR-NEXT SECTION.
       R-DR-NX-005. 
             READ DEBTOR-MASTER NEXT
                 AT END NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 0
                 GO TO R-DR-NX-999
             ELSE
               MOVE 
            "DEBTOR BUSY ON READ NEXT, SEE NEXT LINE, <ESC> TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               PERFORM START-DEBTOR
               GO TO R-DR-NX-005.
       R-DR-NX-999.
             EXIT.
      *
       READ-ORDER-REGISTER SECTION.
       ROR-000.
            MOVE STTR-REFERENCE1 TO INCR-INVOICE.
            MOVE STTR-TYPE       TO INCR-TRANS.
            START INCR-REGISTER KEY NOT < INCR-KEY.
       ROR-010.
            READ INCR-REGISTER
                INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 = 23 OR 35 OR 49
                MOVE 0 TO INCR-INVOICE
                          INCR-DATE
                          INCR-BO-INV-NO
                          INCR-BO-DATE
                MOVE "*REGISTER NOT FOUND*" TO INCR-PORDER
                GO TO ROR-999.
            IF WS-INCR-ST1 NOT = 0
                MOVE 0 TO WS-INCR-ST1
                MOVE "*P/O NOT AVAILABLE*" TO INCR-PORDER.
       ROR-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE 0  TO PAGE-CNT
           MOVE 66 TO LINE-CNT.
           PERFORM OPEN-005.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE "Q"               TO STTR-AC-COMPLETE
           MOVE DR-ACCOUNT-NUMBER TO STTR-ACCOUNT-NUMBER
           MOVE WS-SALEDATE       TO STTR-AC-DATE.
           START STOCK-TRANS-FILE KEY NOT < STTR-AC-KEY
                INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PRR-900.
       PRR-002.
            READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-STTRANS-ST1 = 10 OR = 23
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PRR-900.
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
               GO TO PRR-002.
            IF STTR-AC-COMPLETE NOT = "Q"
               GO TO PRR-900.
            IF STTR-TYPE NOT = 8
               GO TO PRR-002.
            MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
               GO TO PRR-002.
            IF STTR-ACCOUNT-NUMBER < DR-ACCOUNT-NUMBER
               GO TO PRR-002.
            IF STTR-ACCOUNT-NUMBER > DR-ACCOUNT-NUMBER
               GO TO PRR-900.
           IF WS-SALEDATE NOT = 0
            IF STTR-DATE < WS-SALEDATE
               GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           IF STTR-REFERENCE1 NOT = INCR-INVOICE
              PERFORM READ-ORDER-REGISTER.
           MOVE STTR-STOCK-NUMBER       TO D-STOCKNO
           MOVE STTR-DESC1              TO D-DESC1
           MOVE STTR-DESC2              TO D-DESC2
           MOVE INCR-PORDER             TO D-PONO
           MOVE STTR-ORDERQTY           TO D-ORDERQTY
           MOVE STTR-PRICE              TO D-PRICE
           MOVE STTR-REFERENCE1         TO D-1STINV
           MOVE STTR-DATE               TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-1STDATE
           MOVE STTR-TRANSACTION-NUMBER TO D-TRANS
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           ADD 1 TO LINE-CNT
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
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " " TO PRINT-REC
            MOVE DR-ACCOUNT-NUMBER TO H3-ACCOUNT
            MOVE DR-NAME           TO H3-NAME
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE " " TO PRINT-REC
            MOVE 5 TO LINE-CNT.
       PRR-900.
           
           IF WS-SALEDATE NOT = 0
              WRITE PRINT-REC FROM DATE-LINE AFTER 1.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PRR-999.
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
            IF SUB-1 > 49985
                MOVE 49985 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 16
                GO TO NEXT-010.
            IF SUB-1 > 49985  
                GO TO NEXT-025.
            MOVE 1 TO F-INDEX.
       NEXT-025.
            SUBTRACT 15 FROM SUB-1.
            IF SUB-1 > 49985
              IF SUB-25 > 49985
               COMPUTE F-INDEX = 15 - (50001 - SUB-9)
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
            IF SUB-1 > 49985
                 MOVE 49985 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 16
                GO TO NEXT-PAGE-010.
            IF SUB-1 > 49985 
                GO TO NEXT-PAGE-025.
            MOVE 1 TO F-INDEX.
       NEXT-PAGE-025.
            SUBTRACT 15 FROM SUB-1.
            IF SUB-1 > 49985
              IF SUB-25 > 49985
               COMPUTE F-INDEX = 15 - (50001 - SUB-9)
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
            SUBTRACT 15 FROM SUB-1.
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
            IF SUB-1 < SUB-9
               PERFORM READ-ORDER-ONLY
            ELSE
               GO TO SCROLL-999.

            MOVE "STOCKNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE STTR-STOCK-NUMBER TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PORDERNO"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE INCR-PORDER TO F-NAMEFIELD.
            MOVE 15          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QTY" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE STTR-ORDERQTY TO F-EDNAMEFIELDQTY.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-QTY.

            MOVE "PRICE"    TO F-FIELDNAME
            MOVE 5          TO F-CBFIELDNAME
            MOVE STTR-PRICE TO F-EDNAMEFIELD99Mil
            MOVE 11         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "ORDER" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-INVOICE TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "ORDDATE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE INCR-DATE    TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       CLEAR-MEMORY SECTION.
       CMS-005.
            MOVE 1 TO SUB-1.
            MOVE 0 TO SUB-9.
       CMS-010.
            IF WS-STTR-TYPE (SUB-1) NOT = 0
                MOVE 0 TO WS-STTR-TYPE (SUB-1)
                          WS-STTR-REF (SUB-1)
                          WS-STTR-TRANS (SUB-1)
            ELSE
                GO TO CMS-900.
            IF SUB-1 < 50000
               ADD 1 TO SUB-1
               GO TO CMS-010.
       CMS-900.
            MOVE 1 TO SUB-1.
       CMS-999.
            EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO F-INDEX.
       CLTR-010.
            IF F-INDEX > 15
                GO TO CLTR-999.
            MOVE "STOCKNO" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 15        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PORDERNO" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE " "        TO F-NAMEFIELD.
            MOVE 15         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QTY"  TO F-FIELDNAME.
            MOVE 3      TO F-CBFIELDNAME.
            MOVE " "    TO F-NAMEFIELD.
            MOVE 5      TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PRICE" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 11      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDER" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            MOVE " "     TO F-NAMEFIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 10        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO F-INDEX.
            GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR-MASTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-003.
            OPEN I-O INCR-REGISTER.
            IF WS-INCR-ST1 NOT = 0
               MOVE 0 TO WS-INCR-ST1
               MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-003.
            GO TO OPEN-006.
       OPEN-005.
            OPEN I-O STOCK-TRANS-FILE.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STTRANS-ST1
               GO TO OPEN-005.
       OPEN-006.
           Move Ws-Co-Name to Co-Name.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME
           MOVE "SlQuAcIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE DEBTOR-MASTER
                 INCR-REGISTER.
       END-900.
           EXIT PROGRAM.
      *      STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldDate".
       Copy "WriteField99Mil".
       Copy "WriteFieldQty".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
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
      *                     
      * END-OF-JOB.
