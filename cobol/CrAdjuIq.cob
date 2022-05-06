        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrAdjuIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrMaster".
           Copy "SelectCrTrans".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "CrNameIq".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-ANSWER1           PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-TYPE              PIC 99 VALUE 0.
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-ORIG-AMT          PIC S9(7)V99 VALUE 0.
       77  WS-REMAIN-AMT        PIC S9(7)V99 VALUE 0.
       77  WS-TRANS-AMT         PIC S9(7)V99 VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-NO-OF-TRANS       PIC 9(5) VALUE 0.
       77  LINES-DISPLAYED      PIC 9(5) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-BODY-LINE         PIC Z(4)9.
       01  WS-SCROLL-NUMBERS.
           03  WS-SCROLL-NUM OCCURS 10000.
             05  WS-CRTR-TYPE   PIC 99.
             05  WS-CRTR-TRANS  PIC 9(6).
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1  PIC 99.
       01  WS-CRTRANS-STATUS.
           03  WS-CRTRANS-ST1   PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  WS-SALEDATE.
           03 WS-SALEYY         PIC 9999.
           03 WS-SALEMM         PIC 99.
           03 WS-SALEDD         PIC 99.
       01 WS-CODE-NAME.
           03 CODE1-1           PIC S9(7)V99.
           03 CODE1-2           PIC S9(7)V99.
           03 CODE2-1           PIC S9(7)V99.
           03 CODE2-2           PIC S9(7)V99.
           03 CODE3-1           PIC S9(7)V99.
           03 CODE3-2           PIC S9(7)V99.
           03 CODE4-1           PIC S9(7)V99.
           03 CODE4-2           PIC S9(7)V99.
           03 CODE5-1           PIC S9(7)V99.
           03 CODE5-2           PIC S9(7)V99.
           03 CODE6-1           PIC S9(7)V99.
           03 CODE6-2           PIC S9(7)V99.
           03 CODE7-1           PIC S9(7)V99.
           03 CODE7-2           PIC S9(7)V99.
           03 CODE8-1           PIC S9(7)V99.
           03 CODE8-2           PIC S9(7)V99.
           03 CODE9-1           PIC S9(7)V99.
           03 CODE9-2           PIC S9(7)V99.
           03 CODE10-1          PIC S9(7)V99.
           03 CODE10-2          PIC S9(7)V99.
       01  WS-TRANSACTION-DESCRIPTIONS.
           03  FILLER          PIC X(10) VALUE "INVOICE +".
           03  FILLER          PIC X(10) VALUE "PAYMENT -".
           03  FILLER          PIC X(10) VALUE "R/D CHQ +".
           03  FILLER          PIC X(10) VALUE "DR JRN  +".
           03  FILLER          PIC X(10) VALUE "CR JRN  -".
           03  FILLER          PIC X(10) VALUE "C/NOTES -".
           03  FILLER          PIC X(10) VALUE "INTEREST+".
           03  FILLER          PIC X(10) VALUE "DISCOUNT-".
           03  FILLER          PIC X(10) VALUE "FOREX +/-".
           03  FILLER          PIC X(10) VALUE "UN-USED  ".
       01  WS-TRANS-DESCRED REDEFINES WS-TRANSACTION-DESCRIPTIONS.
           03  WS-TRANS-DESC   PIC X(10) OCCURS 10.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(12).
           03  FILLER         PIC X(45) VALUE
           "** CREDITOR TRANSACTION INQUIRY BY CODE **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(48) VALUE 
           "TY  TRANS DESCRIPTION  ACCOUNT NAME" .
           03  FILLER         PIC X(49) VALUE 
           "BATCH      INV-NO        DATE     DEL-DATE".
           03  FILLER         PIC X(20) VALUE 
           "BEGIN      REMAIN".
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
           03  D-REFERENCE    PIC X(11) VALUE " ".
           03  D-INV-NO       PIC X(11) VALUE " ".
           03  D-DATE         PIC X(11) VALUE " ".
           03  D-DUE-DATE     PIC X(11) VALUE " ".
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
           DISPLAY "** Creditor Transaction Enquiry by Type **"
           AT POS
           MOVE 0410 TO POS
           DISPLAY "******************************************"
           AT POS.
       CONTROL-003.
           Copy "PrinterAcceptCr".
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
            PERFORM OPEN-009.
            MOVE 0 TO WS-ORIG-AMT
                      WS-REMAIN-AMT.
            MOVE 0   TO WS-ACCOUNT-NUMBER
                        WS-TYPE.

            PERFORM CLEAR-MEMORY.

       GET-001.
            MOVE "N" TO WS-ANSWER WS-ANSWER1.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            MOVE 2910 TO POS
            DISPLAY "ENTER 'XX' TO SEE AND PRINT ALL CODES." AT POS.
      
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
            MOVE NUMERIC-RATE TO WS-TYPE CRTR-TYPE.
            IF CRTR-TYPE > 10
                MOVE "TRANS. TYPES CANNOT BE > 10, PLEASE RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-000.
            IF CRTR-TYPE = 0
                CLOSE CREDITOR-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                OPEN I-O CREDITOR-MASTER
                CLOSE CRTR-FILE
                PERFORM DISPLAY-FORM
                GO TO GET-000.
       GET-010.
           MOVE "TYPE"         TO F-FIELDNAME
           MOVE 4              TO F-CBFIELDNAME.
           IF WS-TYPE NOT = 0
              MOVE CRTR-TYPE   TO F-NAMEFIELD.
           MOVE 2              TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       GET-020.
           MOVE "TRANSDESC" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME.
           IF WS-TYPE NOT = 0
              MOVE WS-TRANS-DESC (CRTR-TYPE) TO F-NAMEFIELD
           ELSE
              MOVE "ALL TYPES"               TO F-NAMEFIELD.
           MOVE 10          TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       GET-031.
           PERFORM ERROR1-020.
           
           MOVE "                        " TO F-NAMEFIELD
           MOVE "ACCNUM" TO F-FIELDNAME
           MOVE 6        TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
                GO TO GET-001.
           MOVE 7        TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           IF F-NAMEFIELD = "  "
               MOVE 0 TO CR-ACCOUNT-NUMBER WS-ACCOUNT-NUMBER
               GO TO GET-035.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO CR-ACCOUNT-NUMBER WS-ACCOUNT-NUMBER.
           PERFORM READ-CREDITOR.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49 OR 51
              MOVE "INVALID" TO F-NAMEFIELD
              MOVE "ACCNUM"  TO F-FIELDNAME
              MOVE 6         TO F-CBFIELDNAME
              MOVE 7         TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ALPHA
              MOVE "INVALID ACCOUNT NUMBER ENTERED, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO GET-031.

           MOVE "ACCNAME" TO F-FIELDNAME
           MOVE 7         TO F-CBFIELDNAME
           MOVE CR-NAME   TO F-NAMEFIELD
           MOVE 40        TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       GET-035.
           MOVE 2910 TO POS
           DISPLAY "LEAVE BLANK TO SEE & PRINT ALL DATES." AT POS.
       
           MOVE "                        " TO F-NAMEFIELD
           MOVE "DATE" TO F-FIELDNAME
           MOVE 4      TO F-CBFIELDNAME.
           IF WS-TYPE = 0
               MOVE 01           TO WS-DD
               MOVE WS-DATE      TO SPLIT-DATE
               SUBTRACT 1 FROM SPLIT-MM
            IF SPLIT-MM < 1
               MOVE 1 TO SPLIT-MM
               SUBTRACT 1 FROM SPLIT-YY.
           IF WS-TYPE = 0
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
           MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE DISPLAY-DATE    TO F-NAMEFIELD.
           PERFORM WRITE-FIELD-ALPHA.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE      TO WS-SALEDATE WS-DATE.
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
            CLOSE CRTR-FILE.

            PERFORM READ-ALL-TRANSACTIONS.
            PERFORM FILL-BODY.
            IF F-EXIT-CH = X"07" OR = X"09" OR = X"1F"
                PERFORM CLEAR-TRANSACTIONS
                MOVE 1   TO F-INDEX SUB-1
                MOVE "Y" TO WS-ANSWER
                GO TO GET-999.

            GO TO GET-999.

      *      PERFORM READ-TRANSACTIONS.
       GET-900.
            IF WS-ANSWER = "Y"
               CLOSE CRTR-FILE
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
                MOVE 3010 TO POS
                DISPLAY "                                        "
                   AT POS
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
                CLOSE CRTR-FILE
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
           CLOSE CRTR-FILE.
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
           IF WS-TYPE > 0
              MOVE WS-TYPE TO CRTR-TYPE
           ELSE
              MOVE 1       TO CRTR-TYPE.
           MOVE 1          TO CRTR-TRANS.
           START CRTR-FILE KEY NOT < CRTR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
                GO TO RDTR-999.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE "CR-TRANS NOT = 0 ON START, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
              GO TO RDTR-005.
       RDTR-010.
           READ CRTR-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE CRTR-FILE
               GO TO RDTR-999.
           IF WS-CRTRANS-ST1 NOT = 0
            IF SUB-2 = 10
               MOVE "WS-TRANS-ST1 NOT = 0 ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO SUB-2
               GO TO RDTR-010
            ELSE
               ADD 1 TO SUB-2
               GO TO RDTR-010.
               
           IF WS-TYPE > 0
            IF CRTR-TYPE NOT = WS-TYPE
               MOVE 1 TO F-INDEX
               CLOSE CRTR-FILE
               GO TO RDTR-999.
           IF WS-ACCOUNT-NUMBER NOT = 0
            IF CRTR-ACC-NUMBER NOT = WS-ACCOUNT-NUMBER
               GO TO RDTR-010.
           IF WS-SALEDATE NOT = 0
            IF CRTR-DATE < WS-SALEDATE
               GO TO RDTR-010.
           PERFORM ERROR-020.
       RDTR-020. 
           IF F-INDEX > 15
                MOVE 2910 TO POS
                DISPLAY "Press 'PgDn' For More, Or" AT POS
                ADD 27 TO POS
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
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                CLOSE CRTR-FILE
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
           IF WS-TYPE > 0
              MOVE WS-TYPE TO CRTR-TYPE
           ELSE
              MOVE 1       TO CRTR-TYPE.
           MOVE 1          TO CRTR-TRANS.
           START CRTR-FILE KEY NOT < CRTR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
                GO TO RDALL-999.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE "CR-TRANS NOT = 0 ON START, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDALL-005.
       RDALL-010.
           READ CRTR-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               GO TO RDALL-900.
           IF WS-CRTRANS-ST1 NOT = 0 AND NOT = 23
              MOVE "CR-TRANS BUSY ON READ-ALL, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO RDALL-010.
             
           IF WS-TYPE > 0
            IF CRTR-TYPE NOT = WS-TYPE
               MOVE 1 TO F-INDEX
               GO TO RDALL-900.
           IF WS-ACCOUNT-NUMBER NOT = 0
            IF CRTR-ACC-NUMBER NOT = WS-ACCOUNT-NUMBER
               GO TO RDALL-010.
           IF WS-SALEDATE NOT = 0
            IF CRTR-DATE < WS-SALEDATE
               GO TO RDALL-010.
           PERFORM ERROR-020.

           MOVE CRTR-TYPE         TO WS-CRTR-TYPE (SUB-1).
           MOVE CRTR-TRANS        TO WS-CRTR-TRANS (SUB-1).
           
           ADD 1                  TO WS-NO-OF-TRANS.
           ADD CRTR-UNAPPLIED-AMT TO WS-TRANS-AMT.

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
           CLOSE CRTR-FILE.
       RDALL-999.
           EXIT.
      *
       READ-ORDER-ONLY SECTION.
       RDONLY-000.
           IF SUB-1 > SUB-9
               GO TO RDONLY-999.
       RDONLY-005.
           MOVE WS-CRTR-TYPE (SUB-1)   TO CRTR-TYPE.
           MOVE WS-CRTR-TRANS (SUB-1)  TO CRTR-TRANS.
           START CRTR-FILE KEY NOT < CRTR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 NOT = 0
               CLOSE CRTR-FILE
               GO TO RDONLY-999.
       RDONLY-010.
           READ CRTR-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE CRTR-FILE
               GO TO RDONLY-999.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE "CRTRANS BUSY @ READ-ONLY, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               GO TO RDONLY-010.
       RDONLY-999.
           EXIT.
      *
       CLEAR-CODES SECTION.
       CC-010.
           MOVE 0 TO CODE1-1  CODE1-2
                     CODE2-1  CODE2-2
                     CODE3-1  CODE3-2
                     CODE4-1  CODE4-2
                     CODE5-1  CODE5-2
                     CODE6-1  CODE6-2
                     CODE7-1  CODE7-2
                     CODE8-1  CODE8-2
                     CODE9-1  CODE9-2
                     CODE10-1 CODE10-2.
       CC-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           PERFORM CLEAR-CODES.
           MOVE 3010 TO POS
           DISPLAY "Printing In Progress, Please Be Patient." AT POS
           MOVE 0  TO PAGE-CNT SUB-2
                     WS-ORIG-AMT
                     WS-REMAIN-AMT.
           MOVE 66 TO LINE-CNT.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           
           PERFORM OPEN-006.
           
           IF WS-TYPE NOT = 0
              MOVE WS-TYPE TO CRTR-TYPE
           ELSE
              MOVE 01      TO CRTR-TYPE.
           MOVE 1          TO CRTR-TRANS.
           START CRTR-FILE KEY NOT < CRTR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               GO TO PRR-900.
       PRR-002.
            READ CRTR-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 10
               MOVE 0 TO WS-CRTRANS-ST1
               GO TO PRR-900.
           IF WS-CRTRANS-ST1 NOT = 0
            IF SUB-2 = 10
               MOVE "ST-TRANS-ST1 NOT = 0 ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO SUB-2
               GO TO PRR-002
            ELSE
               ADD 1 TO SUB-2
               GO TO PRR-002.
               
           IF WS-TYPE > 0
            IF CRTR-TYPE NOT = WS-TYPE
               GO TO PRR-900.
           IF WS-ACCOUNT-NUMBER NOT = 0
            IF CRTR-ACC-NUMBER NOT = WS-ACCOUNT-NUMBER
               GO TO PRR-002.
           IF WS-SALEDATE NOT = 0
            IF CRTR-DATE < WS-SALEDATE
               GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           IF CRTR-ACC-NUMBER NOT = CR-ACCOUNT-NUMBER
           MOVE CRTR-ACC-NUMBER TO CR-ACCOUNT-NUMBER
              READ CREDITOR-MASTER
                  INVALID KEY
                  MOVE "UNKNOWN"          TO CR-NAME.
           MOVE CRTR-TYPE                 TO D-TYPE
           MOVE CRTR-TRANS                TO D-TRANS-NO
           MOVE WS-TRANS-DESC (CRTR-TYPE) TO D-DESC
           MOVE CR-ACCOUNT-NUMBER         TO D-ACC-NUM
           MOVE CR-NAME                   TO D-ACC-NAME
           MOVE CRTR-REFERENCE            TO D-REFERENCE
           MOVE CRTR-INV-NO               TO D-INV-NO
           MOVE CRTR-DATE                 TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE              TO D-DATE
           
           MOVE CRTR-DUE-DATE             TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE              TO D-DUE-DATE
           MOVE CRTR-LOC-AMT              TO D-ORIG-AMT
           MOVE CRTR-UNAPPLIED-AMT        TO D-REMAIN-AMT.
           
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC.
              ADD CRTR-LOC-AMT        TO WS-ORIG-AMT
              ADD CRTR-UNAPPLIED-AMT  TO WS-REMAIN-AMT.

           IF CRTR-TYPE = 1
              ADD CRTR-LOC-AMT        TO CODE1-1
              ADD CRTR-UNAPPLIED-AMT  TO CODE1-2
              GO TO PRR-055.
           IF CRTR-TYPE = 2
              ADD CRTR-LOC-AMT        TO CODE2-1
              ADD CRTR-UNAPPLIED-AMT  TO CODE2-2
              GO TO PRR-055.
           IF CRTR-TYPE = 3
              ADD CRTR-LOC-AMT        TO CODE3-1
              ADD CRTR-UNAPPLIED-AMT  TO CODE3-2
              GO TO PRR-055.
           IF CRTR-TYPE = 4
              ADD CRTR-LOC-AMT        TO CODE4-1
              ADD CRTR-UNAPPLIED-AMT  TO CODE4-2
              GO TO PRR-055.
           IF CRTR-TYPE = 5
              ADD CRTR-LOC-AMT        TO CODE5-1
              ADD CRTR-UNAPPLIED-AMT  TO CODE5-2
              GO TO PRR-055.
           IF CRTR-TYPE = 6
              ADD CRTR-LOC-AMT        TO CODE6-1
              ADD CRTR-UNAPPLIED-AMT  TO CODE6-2
              GO TO PRR-055.
           IF CRTR-TYPE = 7
              ADD CRTR-LOC-AMT        TO CODE7-1
              ADD CRTR-UNAPPLIED-AMT  TO CODE7-2
              GO TO PRR-055.
           IF CRTR-TYPE = 8
              ADD CRTR-LOC-AMT        TO CODE8-1
              ADD CRTR-UNAPPLIED-AMT  TO CODE8-2
              GO TO PRR-055.
           IF CRTR-TYPE = 9
              ADD CRTR-LOC-AMT        TO CODE9-1
              ADD CRTR-UNAPPLIED-AMT  TO CODE9-2
              GO TO PRR-055.
           IF CRTR-TYPE = 10
              ADD CRTR-LOC-AMT        TO CODE10-1
              ADD CRTR-UNAPPLIED-AMT  TO CODE10-2.
       PRR-055.
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
           WRITE PRINT-REC AFTER 2.
           
           MOVE "INVOICE +"   TO C-NAME
           MOVE CODE1-1       TO C-CODE1-1
           MOVE CODE1-2       TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "PAYMENT -"   TO C-NAME
           MOVE CODE2-1       TO C-CODE1-1
           MOVE CODE2-2       TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "R/D CHQ +"   TO C-NAME
           MOVE CODE3-1       TO C-CODE1-1
           MOVE CODE3-2       TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "DR JRN  +"   TO C-NAME
           MOVE CODE4-1       TO C-CODE1-1
           MOVE CODE4-2       TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "CR JRN  -"   TO C-NAME
           MOVE CODE5-1       TO C-CODE1-1
           MOVE CODE5-2       TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "C/NOTES -"   TO C-NAME
           MOVE CODE6-1       TO C-CODE1-1
           MOVE CODE6-2       TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "INTEREST+"   TO C-NAME
           MOVE CODE7-1       TO C-CODE1-1
           MOVE CODE7-2       TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "DISCOUNT-"   TO C-NAME
           MOVE CODE8-1       TO C-CODE1-1
           MOVE CODE8-2       TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "FOREX +/-"   TO C-NAME
           MOVE CODE9-1       TO C-CODE1-1
           MOVE CODE9-2       TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE "UN-USED  "   TO C-NAME
           MOVE CODE10-1      TO C-CODE1-1
           MOVE CODE10-2      TO C-CODE1-2
           WRITE PRINT-REC FROM CODE-LINE AFTER 1.
           
           MOVE WS-ORIG-AMT   TO T-ORIG-AMT
           MOVE WS-REMAIN-AMT TO T-REMAIN-AMT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.
           
           IF WS-TYPE = 0
               MOVE "** CODES FROM 1 TO 10 PRINTED. **" TO PRINT-REC
               WRITE PRINT-REC AFTER 2.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE CRTR-FILE
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PRR-999.
           EXIT.
      *
       READ-CREDITOR SECTION.
       CR-010.
           MOVE 0 TO SUB-2.
           START CREDITOR-MASTER KEY NOT < CR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 NOT = 0
               GO TO CR-999.
       CR-015.
           READ CREDITOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
               GO TO CR-999.
           IF WS-CREDITOR-ST1 NOT = 0
            IF SUB-2 = 10
               MOVE "CREDITOR RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO SUB-2
               GO TO CR-015
            ELSE
               ADD 1 TO SUB-2
               GO TO CR-015.
       CR-999.
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
           MOVE CRTR-TYPE      TO F-NAMEFIELD
           MOVE 2              TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "TRANS"                 TO F-FIELDNAME
           MOVE 5                       TO F-CBFIELDNAME
           MOVE CRTR-TRANS TO F-NAMEFIELD
           MOVE 6                       TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

      *     MOVE "DESC"      TO F-FIELDNAME.
      *     MOVE 4           TO F-CBFIELDNAME.
      *     MOVE WS-TRANS-DESC (CRTR-TYPE) TO F-NAMEFIELD.
      *     MOVE 10          TO F-CBFIELDLENGTH.
      *     PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNO" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE CRTR-ACC-NUMBER TO F-NAMEFIELD
            MOVE 7       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BATCH"         TO F-FIELDNAME
            MOVE 5               TO F-CBFIELDNAME
            MOVE CRTR-REFERENCE  TO F-NAMEFIELD
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INV-NO"        TO F-FIELDNAME
            MOVE 6               TO F-CBFIELDNAME
            MOVE CRTR-INV-NO     TO F-NAMEFIELD
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TRANS-DATE" TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE 10           TO F-CBFIELDLENGTH
            MOVE CRTR-DATE    TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORIG"              TO F-FIELDNAME
            MOVE 4                   TO F-CBFIELDNAME
            MOVE CRTR-LOC-AMT TO F-EDNAMEFIELD9MIL
            MOVE 10                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.

            MOVE "REMAIN"             TO F-FIELDNAME
            MOVE 6                    TO F-CBFIELDNAME
            MOVE CRTR-UNAPPLIED-AMT TO F-EDNAMEFIELD9MIL
            MOVE 10                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.
            
            IF WS-TYPE NOT = 0
               ADD CRTR-LOC-AMT       TO WS-ORIG-AMT
               ADD CRTR-UNAPPLIED-AMT TO WS-REMAIN-AMT
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

            MOVE "BATCH"    TO F-FIELDNAME
            MOVE 5          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 10         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INV-NO"   TO F-FIELDNAME
            MOVE 6          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 10         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TRANS-DATE" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE " "          TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH
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
            IF WS-CRTR-TYPE (SUB-1) NOT = 0
                MOVE 0   TO WS-CRTR-TYPE (SUB-1)
                            WS-CRTR-TRANS (SUB-1)
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
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITOR-MASTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO OPEN-000.
           GO TO OPEN-009.
       OPEN-006.
           OPEN I-O CRTR-FILE.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE "CR-TRANS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CRTRANS-ST1
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
           MOVE "CrAdjuIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE CREDITOR-MASTER.
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
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
