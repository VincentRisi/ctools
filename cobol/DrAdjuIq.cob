        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrAdjuIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrTrans".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdDrTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "DrAcStIq".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-ANSWER1           PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-TYPE              PIC 99 VALUE 0.
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-ORIG-AMT          PIC S9(7)V99 VALUE 0.
       77  WS-REMAIN-AMT        PIC S9(7)V99 VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-NO-OF-TRANS       PIC 9(5) VALUE 0.
       77  LINES-DISPLAYED      PIC 9(5) VALUE 0.
       77  WS-TRANS-AMT         PIC S9(7)V99 VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-BODY-LINE         PIC Z(4)9.
       01  WS-SCROLL-NUMBERS.
           03  WS-SCROLL-NUM OCCURS 10000.
             05  WS-DRTR-TYPE   PIC 99.
             05  WS-DRTR-TRANS  PIC 9(6).
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1   PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  WS-SALEDATE.
           03 WS-SALEYY         PIC 9999.
           03 WS-SALEMM         PIC 99.
           03 WS-SALEDD         PIC 99.
       01 WS-CODE-NAME.
           03 CODE1-1           PIC 9(7)V99.
           03 CODE1-2           PIC 9(7)V99.
           03 CODE2-1           PIC 9(7)V99.
           03 CODE2-2           PIC 9(7)V99.
           03 CODE3-1           PIC 9(7)V99.
           03 CODE3-2           PIC 9(7)V99.
           03 CODE4-1           PIC 9(7)V99.
           03 CODE4-2           PIC 9(7)V99.
           03 CODE5-1           PIC 9(7)V99.
           03 CODE5-2           PIC 9(7)V99.
           03 CODE6-1           PIC 9(7)V99.
           03 CODE6-2           PIC 9(7)V99.
           03 CODE7-1           PIC 9(7)V99.
           03 CODE7-2           PIC 9(7)V99.
           03 CODE8-1           PIC 9(7)V99.
           03 CODE8-2           PIC 9(7)V99.
           03 CODE9-1           PIC 9(7)V99.
           03 CODE9-2           PIC 9(7)V99.
           03 CODE10-1          PIC 9(7)V99.
           03 CODE10-2          PIC 9(7)V99.
           03 CODE11-1          PIC 9(7)V99.
           03 CODE11-2          PIC 9(7)V99.
       01  WS-TRANSACTION-DESCRIPTIONS.
           03  FILLER          PIC X(10) VALUE "INVOICE +".
           03  FILLER          PIC X(10) VALUE "PAYMENT -".
           03  FILLER          PIC X(10) VALUE "R/D CHQ +".
           03  FILLER          PIC X(10) VALUE "DR JRN  +".
           03  FILLER          PIC X(10) VALUE "CR JRN  -".
           03  FILLER          PIC X(10) VALUE "C/NOTES -".
           03  FILLER          PIC X(10) VALUE "INTEREST+".
           03  FILLER          PIC X(10) VALUE "DISCOUNT-".
           03  FILLER          PIC X(10) VALUE "B/DEBTS -".
           03  FILLER          PIC X(10) VALUE "CHEQU RF+".
           03  FILLER          PIC X(10) VALUE "INT.REV.-".
       01  WS-TRANS-DESCRED REDEFINES WS-TRANSACTION-DESCRIPTIONS.
           03  WS-TRANS-DESC   PIC X(10) OCCURS 11.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(12).
           03  FILLER         PIC X(45) VALUE
           "** DEBTOR TRANSACTION INQUIRY BY CODE **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(50) VALUE 
           "TY  TRANS DESCRIPTION  ACCOUNT NAME" .
           03  FILLER         PIC X(39) VALUE 
           "REF1   REF2    DATE     DEL-DATE".
           03  FILLER         PIC X(20) VALUE 
           "BEGIN     REMAIN".
       01  DETAIL-LINE.
           03  D-TYPE         PIC 99.
           03  FILLER         PIC X VALUE " ".
           03  D-TRANS-NO     PIC 9(6).
           03  FILLER         PIC X VALUE " ".
           03  D-DESC         PIC X(13).
           03  D-ACC-NUM      PIC 9(7).
           03  FILLER         PIC X VALUE " ".
           03  D-ACC-NAME     PIC X(16) VALUE " ".
           03  FILLER         PIC X VALUE " ".
           03  D-REF1         PIC X(6) VALUE " ".
           03  FILLER         PIC X VALUE " ".
           03  D-REF2         PIC X(7) VALUE " ".
           03  D-DATE         PIC X(11) VALUE " ".
           03  D-DEL-DATE     PIC X(10) VALUE " ".
           03  D-ORIG-AMT     PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-REMAIN-AMT   PIC Z(6)9.99-.
       01  TOTAL-LINE.
           03  FILLER         PIC X(3).
           03  T-NAME         PIC X(10) VALUE "TOTALS :".
           03  T-ORIG-AMT     PIC Z(7)9.99-.
           03  FILLER         PIC X VALUE " ".
           03  T-REMAIN-AMT   PIC Z(7)9.99-.
       01  CODE-LINE.
           03  FILLER         PIC X(3).
           03  C-NAME         PIC X(10) VALUE " ".
           03  C-CODE1-1      PIC Z(7)9.99-.
           03  FILLER         PIC X VALUE " ".
           03  C-CODE1-2      PIC Z(7)9.99-.
       Copy "WsDateInfo".
      *
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 0310 TO POS
           DISPLAY "** Debtor transaction Enquiry by Type **"
           AT POS
           MOVE 0410 TO POS
           DISPLAY "****************************************"
           AT POS.
       CONTROL-003.
           Copy "PrinterAcceptDr".
       CONTROL-010.
           MOVE 2510 TO POS
           DISPLAY "Program Loading, Please be patient..." AT POS.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN.
       CONTROL-020.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-020.
      *
       GET-DATA SECTION.
       GET-000.
            PERFORM OPEN-006.
            MOVE 0 TO WS-ORIG-AMT
                      WS-REMAIN-AMT.
            MOVE 0   TO WS-ACCOUNT-NUMBER
                        WS-TYPE.
       GET-001.
            MOVE "N" TO WS-ANSWER WS-ANSWER1.
            PERFORM ERROR1-020.

            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            MOVE 2      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF F-NAMEFIELD = "XX"
               MOVE 0 TO WS-TYPE
               GO TO GET-010.
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-TYPE DRTR-TYPE.
            IF DRTR-TYPE > 11
                MOVE "TRANS. TYPES CANNOT BE > 11, PLEASE RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-000.
            IF DRTR-TYPE = 0
                CLOSE DEBTOR-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                OPEN I-O DEBTOR-MASTER
                CLOSE DEBTOR-TRANS-FILE
                PERFORM DISPLAY-FORM
                GO TO GET-000.
       GET-010.
           MOVE "TYPE"         TO F-FIELDNAME
           MOVE 4              TO F-CBFIELDNAME.
           IF WS-TYPE NOT = 0
              MOVE DRTR-TYPE   TO F-NAMEFIELD.
           MOVE 2              TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       GET-020.
           MOVE "TRANSDESC" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME.
           IF WS-TYPE NOT = 0
              MOVE WS-TRANS-DESC (DRTR-TYPE) TO F-NAMEFIELD
           ELSE
              MOVE "ALL TYPES"               TO F-NAMEFIELD.
           MOVE 10          TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       GET-031.
           MOVE "                        " TO F-NAMEFIELD
           MOVE "ACCNUM" TO F-FIELDNAME
           MOVE 6        TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
                GO TO GET-001.
           MOVE 7        TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           IF F-NAMEFIELD = "  "
               MOVE 0 TO DR-ACCOUNT-NUMBER WS-ACCOUNT-NUMBER
               GO TO GET-035.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO DR-ACCOUNT-NUMBER WS-ACCOUNT-NUMBER.
           PERFORM READ-DEBTOR.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
              MOVE "INVALID" TO F-NAMEFIELD
              MOVE "ACCNUM"  TO F-FIELDNAME
              MOVE 6         TO F-CBFIELDNAME
              MOVE 7         TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ALPHA
              CALL "&LOCKKBD" USING F-FIELDNAME
              GO TO GET-031.

           MOVE "ACCNAME" TO F-FIELDNAME
           MOVE 7         TO F-CBFIELDNAME
           MOVE DR-NAME   TO F-NAMEFIELD
           MOVE 40        TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       GET-035.
           MOVE "                        " TO F-NAMEFIELD
           MOVE "DATE" TO F-FIELDNAME
           MOVE 4      TO F-CBFIELDNAME.
           IF WS-TYPE = 0
               MOVE 01           TO WS-DD
               MOVE WS-DATE      TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE DISPLAY-DATE TO F-NAMEFIELD
               MOVE 10           TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
               GO TO GET-031.
           MOVE 10       TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           IF F-NAMEFIELD = "  "
               MOVE 0 TO WS-SALEDATE
               GO TO GET-040.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO GET-035.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
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
           MOVE 10           TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-ALPHA.
       GET-040.
           MOVE " " TO F-EXIT-CH.
            CLOSE DEBTOR-TRANS-FILE.

            PERFORM READ-ALL-TRANSACTIONS.
            PERFORM FILL-BODY.
            IF F-EXIT-CH = X"07" OR = X"09" OR = X"1F"
                PERFORM CLEAR-TRANSACTIONS
                MOVE 1   TO F-INDEX SUB-1
                MOVE "Y" TO WS-ANSWER
                GO TO GET-999.

            GO TO GET-999.

      *     PERFORM READ-TRANSACTIONS.
       GET-900.
            IF WS-ANSWER = "Y"
               CLOSE DEBTOR-TRANS-FILE
               GO TO GET-999.
            IF F-INDEX < 15
             IF F-INDEX NOT = "07"
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3010 TO POS
               DISPLAY "Or Press 'F10' To Print All Debtor Trans."
                  AT POS
               MOVE 15 TO F-INDEX
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
                
            PERFORM ERROR1-020
            PERFORM ERROR-020
            
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS

            PERFORM ERROR1-020
            PERFORM ERROR-020
                GO TO GET-999.
       GET-999.
            EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
           PERFORM OPEN-006.

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

           MOVE "DRTYPE"       TO F-FIELDNAME
           MOVE 6              TO F-CBFIELDNAME
           MOVE 2              TO F-CBFIELDLENGTH
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
      *ESC
           IF F-EXIT-CH = X"07"
              GO TO FILL-900.
      * <f10> to print
           IF F-EXIT-CH = X"1F"
                CLOSE DEBTOR-TRANS-FILE
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
           CLOSE DEBTOR-TRANS-FILE.
       FILL-999.
           EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           PERFORM OPEN-006.
           MOVE 1   TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
           MOVE 0   TO SUB-2.
       RDTR-005.
           IF WS-TYPE NOT = 0
              MOVE WS-TYPE TO DRTR-TYPE
           ELSE
              MOVE 01      TO DRTR-TYPE.
           MOVE 1          TO DRTR-TRANSACTION-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                GO TO RDTR-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DRTRANS BUSY ON START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               GO TO RDTR-005.
       RDTR-010.
           READ DEBTOR-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE DEBTOR-TRANS-FILE
               GO TO RDTR-999.
           IF WS-DRTRANS-ST1 NOT = 0
            IF SUB-2 = 10
               MOVE 
               "DRTRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO SUB-2
               GO TO RDTR-010
            ELSE
               ADD 1 TO SUB-2
               GO TO RDTR-010.
               
           IF WS-TYPE > 0
            IF DRTR-TYPE NOT = WS-TYPE
               MOVE 1 TO F-INDEX
               CLOSE DEBTOR-TRANS-FILE
               GO TO RDTR-999.
           IF WS-ACCOUNT-NUMBER NOT = 0
            IF DRTR-ACCOUNT-NUMBER NOT = WS-ACCOUNT-NUMBER
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid Transaction...." AT POS
               GO TO RDTR-010.
           IF WS-SALEDATE NOT = 0
            IF DRTR-DATE < WS-SALEDATE
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid Transaction...." AT POS
               GO TO RDTR-010.
           PERFORM ERROR-020.
       RDTR-020. 
           IF F-INDEX > 15
                MOVE 2910 TO POS
                DISPLAY "Press 'PgDn' For More, Or" AT POS
                ADD 31 TO POS
                DISPLAY "'ESC' To Clear The Screen !" AT POS
                MOVE 3010 TO POS
                DISPLAY "Or Press 'F10' To Print All Debtor Trans."
                   AT POS
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
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                GO TO RDTR-999.
           IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient..."
                   AT POS
                CLOSE DEBTOR-TRANS-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                PERFORM ERROR-020
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C"
                    AND NOT = X"07" AND NOT = " " AND NOT = X"1F"
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTR-010.
       RDTR-999.
           EXIT.
      *
       READ-ALL-TRANSACTIONS SECTION.
       RDALL-000.
           PERFORM OPEN-006.
           MOVE 1   TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
           MOVE 0   TO SUB-2.
       RDALL-005.
           IF WS-TYPE NOT = 0
              MOVE WS-TYPE TO DRTR-TYPE
           ELSE
              MOVE 01      TO DRTR-TYPE.
           MOVE 1          TO DRTR-TRANSACTION-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                GO TO RDALL-900.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DRTRANS BUSY ON START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               GO TO RDALL-005.
       RDALL-010.
           READ DEBTOR-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               GO TO RDALL-900.
           IF WS-DRTRANS-ST1 NOT = 0
            IF SUB-2 = 10
               MOVE 
               "DRTRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO SUB-2
               GO TO RDALL-010
            ELSE
               ADD 1 TO SUB-2
               GO TO RDALL-010.
               
           IF WS-TYPE > 0
            IF DRTR-TYPE NOT = WS-TYPE
               MOVE 1 TO F-INDEX
               CLOSE DEBTOR-TRANS-FILE
               GO TO RDALL-900.
           IF WS-ACCOUNT-NUMBER NOT = 0
            IF DRTR-ACCOUNT-NUMBER NOT = WS-ACCOUNT-NUMBER
               GO TO RDALL-010.
           IF WS-SALEDATE NOT = 0
            IF DRTR-DATE < WS-SALEDATE
               GO TO RDALL-010.
           PERFORM ERROR-020.

           MOVE DRTR-TYPE               TO WS-DRTR-TYPE (SUB-1).
           MOVE DRTR-TRANSACTION-NUMBER TO WS-DRTR-TRANS (SUB-1).
           
           ADD 1                    TO WS-NO-OF-TRANS.
           ADD DRTR-AMT-OUTSTANDING TO WS-TRANS-AMT.

           IF SUB-1 < 10000
              ADD 1 TO SUB-1
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
       RDALL-920.
           PERFORM ERROR1-020.

           MOVE 2912 TO POS.
           DISPLAY "Total # of Lines:" AT POS
           ADD 19 TO POS.
           MOVE SUB-9 TO WS-BODY-LINE.
           DISPLAY WS-BODY-LINE AT POS.
           ADD 1 TO SUB-9.
       RDALL-950.
           CLOSE DEBTOR-TRANS-FILE.
       RDALL-999.
           EXIT.
      *
       READ-ORDER-ONLY SECTION.
       RDONLY-000.
           IF SUB-1 > SUB-9
               GO TO RDONLY-999.
       RDONLY-005.
           MOVE WS-DRTR-TYPE (SUB-1)   TO DRTR-TYPE.
           MOVE WS-DRTR-TRANS (SUB-1)  TO DRTR-TRANSACTION-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 NOT = 0
               CLOSE DEBTOR-TRANS-FILE
               GO TO RDONLY-999.
       RDONLY-010.
           READ DEBTOR-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE DEBTOR-TRANS-FILE
               GO TO RDONLY-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DRTRANS BUSY @ READ-ONLY, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               GO TO RDONLY-010.
       RDONLY-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           MOVE 3010 TO POS
           DISPLAY "Printing In Progress, Please Be Patient..." AT POS
           MOVE 0  TO PAGE-CNT SUB-2
                     WS-ORIG-AMT
                     WS-REMAIN-AMT.
           MOVE 66 TO LINE-CNT.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           
           PERFORM OPEN-006.
           
           IF WS-TYPE NOT = 0
              MOVE WS-TYPE TO DRTR-TYPE
           ELSE
              MOVE 0       TO DRTR-TYPE.
           MOVE 1          TO DRTR-TRANSACTION-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO PRR-900.
       PRR-002.
            READ DEBTOR-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO PRR-900.
           IF WS-DRTRANS-ST1 NOT = 0
            IF SUB-2 = 10
               MOVE "DRTRANS BUSY ON READ-PRN, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO SUB-2
               GO TO PRR-002
            ELSE
               ADD 1 TO SUB-2
               GO TO PRR-002.
               
           IF WS-TYPE > 0
            IF DRTR-TYPE NOT = WS-TYPE
               GO TO PRR-900.
           IF WS-ACCOUNT-NUMBER NOT = 0
            IF DRTR-ACCOUNT-NUMBER NOT = WS-ACCOUNT-NUMBER
               GO TO PRR-002.
           IF WS-SALEDATE NOT = 0
            IF DRTR-DATE < WS-SALEDATE
               GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
           MOVE DRTR-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER
              READ DEBTOR-MASTER
                  INVALID KEY
                  MOVE "UNKNOWN"          TO DR-NAME.
           MOVE DRTR-TYPE                 TO D-TYPE
           MOVE DRTR-TRANSACTION-NUMBER   TO D-TRANS-NO
           MOVE WS-TRANS-DESC (DRTR-TYPE) TO D-DESC
           MOVE DR-ACCOUNT-NUMBER         TO D-ACC-NUM
           MOVE DR-NAME                   TO D-ACC-NAME
           MOVE DRTR-REFERENCE1           TO D-REF1
           MOVE DRTR-REFERENCE2           TO D-REF2
           MOVE DRTR-DATE                 TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE              TO D-DATE
           
           MOVE DRTR-DEL-DATE             TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE              TO D-DEL-DATE
           MOVE DRTR-AMT-OF-INVOICE       TO D-ORIG-AMT
           MOVE DRTR-AMT-OUTSTANDING      TO D-REMAIN-AMT.
           
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC.
           IF DRTR-TYPE = 1 OR = 3 OR = 4 OR = 7 OR = 10
              ADD DRTR-AMT-OF-INVOICE   TO WS-ORIG-AMT
              ADD DRTR-AMT-OUTSTANDING  TO WS-REMAIN-AMT
           ELSE
              SUBTRACT DRTR-AMT-OF-INVOICE  FROM WS-ORIG-AMT
              SUBTRACT DRTR-AMT-OUTSTANDING FROM WS-REMAIN-AMT.

           IF DRTR-TYPE = 1
              ADD DRTR-AMT-OF-INVOICE   TO CODE1-1
              ADD DRTR-AMT-OUTSTANDING  TO CODE1-2.
           IF DRTR-TYPE = 2
              ADD DRTR-AMT-OF-INVOICE   TO CODE2-1
              ADD DRTR-AMT-OUTSTANDING  TO CODE2-2.
           IF DRTR-TYPE = 3
              ADD DRTR-AMT-OF-INVOICE   TO CODE3-1
              ADD DRTR-AMT-OUTSTANDING  TO CODE3-2.
           IF DRTR-TYPE = 4
              ADD DRTR-AMT-OF-INVOICE   TO CODE4-1
              ADD DRTR-AMT-OUTSTANDING  TO CODE4-2.
           IF DRTR-TYPE = 5
              ADD DRTR-AMT-OF-INVOICE   TO CODE5-1
              ADD DRTR-AMT-OUTSTANDING  TO CODE5-2.
           IF DRTR-TYPE = 6
              ADD DRTR-AMT-OF-INVOICE   TO CODE6-1
              ADD DRTR-AMT-OUTSTANDING  TO CODE6-2.
           IF DRTR-TYPE = 7
              ADD DRTR-AMT-OF-INVOICE   TO CODE7-1
              ADD DRTR-AMT-OUTSTANDING  TO CODE7-2.
           IF DRTR-TYPE = 8
              ADD DRTR-AMT-OF-INVOICE   TO CODE8-1
              ADD DRTR-AMT-OUTSTANDING  TO CODE8-2.
           IF DRTR-TYPE = 9
              ADD DRTR-AMT-OF-INVOICE   TO CODE9-1
              ADD DRTR-AMT-OUTSTANDING  TO CODE9-2.
           IF DRTR-TYPE = 10
              ADD DRTR-AMT-OF-INVOICE   TO CODE10-1
              ADD DRTR-AMT-OUTSTANDING  TO CODE10-2.
           IF DRTR-TYPE = 11
              ADD DRTR-AMT-OF-INVOICE   TO CODE11-1
              ADD DRTR-AMT-OUTSTANDING  TO CODE11-2.
              
           ADD 1 TO LINE-CNT.
           GO TO PRR-002.
       PRR-060.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC.

            IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
           WRITE PRINT-REC FROM HEAD1
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC
           MOVE 3 TO LINE-CNT.
       PRR-900.
           IF LINE-CNT = 66
               PERFORM PRR-060.
           MOVE "** SUMMARY OF CODES FOR THIS PRINT **" TO PRINT-REC
           WRITE PRINT-REC AFTER 2.
           MOVE "    TYPE           BEGIN       REMAIN" TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           
           MOVE "INVOICE +"  TO C-NAME
           MOVE CODE1-1       TO C-CODE1-1
           MOVE CODE1-2       TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 2.
           
           MOVE "PAYMENT -"   TO C-NAME
           MOVE CODE2-1       TO C-CODE1-1
           MOVE CODE2-2       TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "R/D CHQ +"  TO C-NAME
           MOVE CODE3-1      TO C-CODE1-1
           MOVE CODE3-2      TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "DR JRN  +"   TO C-NAME
           MOVE CODE4-1       TO C-CODE1-1
           MOVE CODE4-2       TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "CR JRN  -"   TO C-NAME
           MOVE CODE5-1       TO C-CODE1-1
           MOVE CODE5-2       TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "C/NOTES -"  TO C-NAME
           MOVE CODE6-1      TO C-CODE1-1
           MOVE CODE6-2      TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "INTEREST+" TO C-NAME
           MOVE CODE7-1     TO C-CODE1-1
           MOVE CODE7-2     TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "DISCOUNT-" TO C-NAME
           MOVE CODE8-1     TO C-CODE1-1
           MOVE CODE8-2     TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "B/DEBTS -" TO C-NAME
           MOVE CODE9-1     TO C-CODE1-1
           MOVE CODE9-2     TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "CH. REF +"  TO C-NAME
           MOVE CODE10-1     TO C-CODE1-1
           MOVE CODE10-2     TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "INT.REV.-"  TO C-NAME
           MOVE CODE11-1     TO C-CODE1-1
           MOVE CODE11-2     TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE WS-ORIG-AMT    TO T-ORIG-AMT
           MOVE WS-REMAIN-AMT  TO T-REMAIN-AMT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.
           
           IF WS-TYPE = 0
               MOVE "** CODES FROM 1 TO 11 PRINTED. **" TO PRINT-REC
               WRITE PRINT-REC AFTER 2.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE DEBTOR-TRANS-FILE
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PRR-999.
           EXIT.
      *
       READ-DEBTOR SECTION.
       RD-010.
           MOVE 0 TO SUB-2.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
       RD-015.
           READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               GO TO RD-999.
           IF WS-DEBTOR-ST1 NOT = 0
            IF SUB-2 = 10
               MOVE "DEBTOR RECORD BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO SUB-2
               GO TO RD-015
            ELSE
               ADD 1 TO SUB-2
               GO TO RD-015.
       RD-999.
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

           MOVE "DRTYPE"       TO F-FIELDNAME
           MOVE 6              TO F-CBFIELDNAME
           MOVE DRTR-TYPE      TO F-NAMEFIELD
           MOVE 2              TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "TRANS"                 TO F-FIELDNAME
           MOVE 5                       TO F-CBFIELDNAME
           MOVE DRTR-TRANSACTION-NUMBER TO F-NAMEFIELD
           MOVE 6                       TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

      *     MOVE "DESC"      TO F-FIELDNAME.
      *     MOVE 4           TO F-CBFIELDNAME.
      *     MOVE WS-TRANS-DESC (DRTR-TYPE) TO F-NAMEFIELD.
      *     MOVE 10          TO F-CBFIELDLENGTH.
      *     PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNO" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE DRTR-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 7       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REF1"          TO F-FIELDNAME
            MOVE 4               TO F-CBFIELDNAME
            MOVE DRTR-REFERENCE1 TO F-NAMEFIELD
            MOVE 6               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REF2"          TO F-FIELDNAME
            MOVE 4               TO F-CBFIELDNAME
            MOVE DRTR-REFERENCE2 TO F-NAMEFIELD
            MOVE 6               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TRANS-DATE" TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE 10           TO F-CBFIELDLENGTH
            MOVE DRTR-DATE    TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-DATE"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE 10            TO F-CBFIELDLENGTH
            MOVE DRTR-DEL-DATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORIG"              TO F-FIELDNAME
            MOVE 4                   TO F-CBFIELDNAME
            MOVE DRTR-AMT-OF-INVOICE TO F-EDNAMEFIELD9MIL
            MOVE 10                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.

            MOVE "REMAIN"             TO F-FIELDNAME
            MOVE 6                    TO F-CBFIELDNAME
            MOVE DRTR-AMT-OUTSTANDING TO F-EDNAMEFIELD9MIL
            MOVE 10                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.
            
            IF WS-TYPE NOT = 0
               ADD DRTR-AMT-OF-INVOICE  TO WS-ORIG-AMT
               ADD DRTR-AMT-OUTSTANDING TO WS-REMAIN-AMT
               MOVE "TOT-ORIG"  TO F-FIELDNAME
               MOVE 8           TO F-CBFIELDNAME
               MOVE WS-ORIG-AMT TO F-EDNAMEFIELD9MIL
               MOVE 10          TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-9MIL

               MOVE "TOT-REMAIN"  TO F-FIELDNAME
               MOVE 10            TO F-CBFIELDNAME
               MOVE WS-REMAIN-AMT TO F-EDNAMEFIELD9MIL
               MOVE 10            TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-9MIL.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO F-INDEX.
       CLTR-010.
            IF F-INDEX > 15
                GO TO CLTR-999.
            MOVE "DRTYPE" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 2        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TRANS"  TO F-FIELDNAME
            MOVE 5        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 6        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
           
            MOVE "ACCNO" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 7       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REF1"     TO F-FIELDNAME
            MOVE 4          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 6          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REF2"     TO F-FIELDNAME
            MOVE 4          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 6          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TRANS-DATE" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE " "          TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-DATE" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 10         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORIG" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            MOVE " "    TO F-NAMEFIELD.
            MOVE 10     TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REMAIN"  TO F-FIELDNAME.
            MOVE 6         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 10        TO F-CBFIELDLENGTH.
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
            IF WS-DRTR-TYPE (SUB-1) NOT = 0
                MOVE 0   TO WS-DRTR-TYPE (SUB-1)
                            WS-DRTR-TRANS (SUB-1)
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
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR-MASTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO OPEN-000.
           GO TO OPEN-009.
       OPEN-006.
           OPEN I-O DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DR-TRANS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO OPEN-006.
       OPEN-009.
           MOVE Ws-Co-Name to CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "DrAdjuIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE DEBTOR-MASTER.
       END-900.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteField9Mil".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldPrice".
       Copy "WriteFieldQty".
       Copy "WriteFieldQtyCred".
       Copy "WriteFieldNumNeg".
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
