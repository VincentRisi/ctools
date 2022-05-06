        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrJrnIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrMaster".
           Copy "SelectCrJrn".
           Copy "SelectGlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrJrn.
           COPY ChlfdGlParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(135).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "CrMastIq".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-NO-OF-TRANS       PIC 9(5) VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-CREDITORNUMBER    PIC X(7) VALUE " ".
       77  WS-TRANS-AMT         PIC S9(8)V99 VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  LINES-DISPLAYED      PIC 9(5) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-BODY-LINE         PIC Z(4)9.
       01  WS-SCROLL-NUMBERS.
           03  WS-SCROLL-NUM OCCURS 10000.
             05  WS-CRJRN-REF   PIC X(10).
             05  WS-CRJRN-TRANS PIC 9(6).
             05  WS-CRJRN-TYPE  PIC 9.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1    PIC 99.
       01  WS-CRJRN-STATUS.
           03  WS-CRJRN-ST1       PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-PERIOD.
           03  WS-1ST-CHAR        PIC X.
           03  WS-PER             PIC 99.
       01  WS-TRANSACTIONS.
           03  FILLER              PIC X(3) VALUE "INV".
           03  FILLER              PIC X(3) VALUE "PAY".
           03  FILLER              PIC X(3) VALUE "   ".
           03  FILLER              PIC X(3) VALUE "JDR".
           03  FILLER              PIC X(3) VALUE "JCR".
           03  FILLER              PIC X(3) VALUE "CRD".
           03  FILLER              PIC X(3) VALUE "INT".
           03  FILLER              PIC X(3) VALUE "DIS".
           03  FILLER              PIC X(3) VALUE "FRX".
       01  WS-TRANS-TYPERED REDEFINES WS-TRANSACTIONS.
           03  WS-TRANS-DESC    PIC X(3) OCCURS 9.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(45) VALUE
           "** CREDITOR UNPOSTED JRN INQUIRY **".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(23).
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(13) VALUE "ACCOUNT    :".
           03  H3-ACC         PIC X(119).
       01  HEAD2-1.
           03  FILLER         PIC X(13) VALUE "NAME       :".
           03  H3-NAME        PIC X(119).
       01  HEAD2-2.
           03  FILLER         PIC X(13) VALUE "BALANCE    :".
           03  H3-BALANCE     PIC Z(6)9.99-.
           03  FILLER         PIC X(108) VALUE " ".
       01  HEAD2-3.
           03  FILLER         PIC X(13) VALUE "PERIOD READ:".
           03  H3-PERIOD-READ PIC X(119).
       01  HEAD3.
           03  FILLER         PIC X(6) VALUE "PER".
           03  FILLER         PIC X(11) VALUE "JRN No:".
           03  FILLER         PIC X(5) VALUE "TYPE".
           03  FILLER         PIC X(22) VALUE "INVOICE       DATE".
           03  FILLER         PIC X(24) VALUE "D/NOTE        DUE".
           03  FILLER         PIC X(22) VALUE
              "RAND AMT  UNAPP. AMT".
           03  FILLER         PIC X(26) VALUE
              "FOREIGN AMT  EXCHANGE".
           03  FILLER         PIC X(16) VALUE "S/DISC ST  TRANS".
       01  DETAIL-LINE.
           03  D-PERIOD       PIC X(6).
           03  D-JRNNO        PIC X(11).
           03  D-TYPE         PIC X(5) VALUE " ".
           03  D-INVNO        PIC X(10).
           03  FILLER         PIC X VALUE " ".
           03  D-INVDATE      PIC X(10).
           03  FILLER         PIC X VALUE " ".
           03  D-DNOTE        PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-DUEDATE      PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-RANDAMT      PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-UNAPPLIED    PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-FORAMT       PIC Z(6)9.99-.
           03  D-EXCHANGE     PIC Z(2)9.9(5).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SETTDISC     PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-POSTED       PIC X(2).
           03  D-TRANS        PIC Z(5)9.
        01  TOTAL-LINE.
           05  FILLER         PIC X(40) VALUE " ".
           05  FILLER         PIC X(26) VALUE
              "TOTAL AMOUNT FOR PERIOD:".
           05  T-RANDAMT      PIC Z(6)9.99-.
           05  FILLER         PIC X(77) VALUE " ".
       01  WS-TEMP-LINE.
           03  FILLER           PIC X(10) VALUE " ".
           03  T-TRANS-DATE     PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
           03  T-TRANS-DUEDATE  PIC X(10).
           03  FILLER           PIC X(1) VALUE " ".
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
           DISPLAY "** CREDITOR JOURNAL INQUIRY PROGRAM **" AT POS
           MOVE 0410 TO POS
           DISPLAY "**************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAcceptCr".
       CONTROL-010.
           MOVE 2910 TO POS
           DISPLAY "Program now loading, please be patient..." AT POS.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-020.
           PERFORM DISPLAY-FORM.
           PERFORM DISPLAY-FORM-CR-TOP-INFO.
           PERFORM GET-DATA.
           GO TO CONTROL-020.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "                               " TO F-NAMEFIELD.
            MOVE "N" TO WS-ANSWER.

            PERFORM OPEN-006. 
            PERFORM CLEAR-MEMORY.

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
            IF F-EXIT-CH = X"07"
                 CLOSE CRJRN-FILE
                 GO TO GET-999.
            IF F-EXIT-CH = X"0C"
                 PERFORM READ-CREDITOR-NEXT
                 GO TO GET-010.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-CREDITOR-PREVIOUS
                 GO TO GET-010.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CR-ACCOUNT-NUMBER.
            IF F-NAMEFIELD = "   "
                CLOSE CREDITOR-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                OPEN I-O CREDITOR-MASTER
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-FORM-CR-TOP-INFO
                CLOSE CRJRN-FILE
                GO TO GET-000.
            PERFORM READ-CREDITOR.
            GO TO GET-020.
       GET-010.
            MOVE "ACCNO"           TO F-FIELDNAME.
            MOVE 5                 TO F-CBFIELDNAME.
            MOVE CR-ACCOUNT-NUMBER TO F-NAMEFIELD.
            MOVE 15                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-020.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
        
            MOVE "NAME"  TO F-FIELDNAME.
            MOVE 4       TO F-CBFIELDNAME.
            MOVE CR-NAME TO F-NAMEFIELD.
            MOVE 40      TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            IF CR-NAME = "UNKNOWN"
                MOVE 2801 TO POS
                DISPLAY " " AT 3079 WITH BELL
                CLOSE CRJRN-FILE
                GO TO GET-000.

            MOVE "BALANCE"  TO F-FIELDNAME.
            MOVE 7          TO F-CBFIELDNAME.
            MOVE CR-BALANCE TO F-EDNAMEFIELDSALE.
            MOVE 11         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
        GET-030.
            MOVE "                               " TO F-NAMEFIELD.
            MOVE "READPERIOD" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 CLOSE CRJRN-FILE
                 GO TO GET-000.
            MOVE 2 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"07"
                 CLOSE CRJRN-FILE
                 GO TO GET-999.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0C"
                  DISPLAY " " AT 3079 WITH BELL
                  GO TO GET-030.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-PER.
            IF WS-PER = 0
               MOVE "  " TO F-NAMEFIELD
            ELSE
               MOVE WS-PER TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            MOVE " " TO WS-1ST-CHAR.
       GET-040.
            MOVE " " TO F-EXIT-CH.
            CLOSE CRJRN-FILE.

            PERFORM READ-ALL-TRANSACTIONS.
            PERFORM FILL-BODY.
            IF F-EXIT-CH = X"07" OR = X"09" OR = X"1F"
                PERFORM CLEAR-TRANSACTIONS
                MOVE 1   TO F-INDEX SUB-1
                MOVE "Y" TO WS-ANSWER
                GO TO GET-999.

            GO TO GET-999.

      *      MOVE 0 TO LINES-DISPLAYED.
      *      IF WS-NO-OF-TRANS = 0
      *            GO TO GET-900.
      *      PERFORM READ-TRANSACTIONS.
       GET-900.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
        
            IF WS-ANSWER = "Y"
               CLOSE CRJRN-FILE
               GO TO GET-999.
            IF F-INDEX < 15
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3010 TO POS
               DISPLAY "Or Press 'F10' To Print All Transactions" AT POS
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
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                OPEN I-O CRJRN-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE CRJRN-FILE
                PERFORM ERROR1-020
                PERFORM ERROR-020.
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
      *     MOVE 3015 TO POS 
      *     DISPLAY "Current Line#: " AT POS
      *     ADD 16 TO POS
           MOVE SUB-1 TO WS-BODY-LINE
      *     DISPLAY WS-BODY-LINE AT POS.

           MOVE WS-BODY-LINE    TO LINES-DISPLAYED.
           MOVE "LINES"         TO F-FIELDNAME.
           MOVE 5               TO F-CBFIELDNAME.
           MOVE LINES-DISPLAYED TO F-EDNAMEFIELDCRED.
           MOVE 5               TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-CRED.
            
           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.

            MOVE "PERIOD" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            MOVE 2        TO F-CBFIELDLENGTH.
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
      *     IF F-EXIT-CH = X"13"
      *      IF SUB-1 NOT > SUB-9
      *        PERFORM SCROLL-PREVIOUS
      *       IF SUB-1 < 16
      *        MOVE F-INDEX TO SUB-1
      *        GO TO FILL-010
      *      ELSE
      *        GO TO FILL-010.
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
      *FINISH
           IF F-EXIT-CH = X"04"
              PERFORM END-OFF.
      * <f10> to print
           IF F-EXIT-CH = X"1F"
                CLOSE CRJRN-FILE
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
           CLOSE CRJRN-FILE.
       FILL-999.
           EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           OPEN I-O CRJRN-FILE.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CRJRN-FILE BUSY ON OPEN RDTR-000 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              CLOSE CRJRN-FILE
              GO TO RDTR-000.
           MOVE 1 TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
       RDTR-005.
           MOVE CR-ACCOUNT-NUMBER TO CRJRN-CRACC-NUMBER.
           START CRJRN-FILE KEY NOT < CRJRN-CRACC-NUMBER
               INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
               MOVE "NAME" TO F-FIELDNAME
               MOVE 4 TO F-CBFIELDNAME
               MOVE 25 TO F-CBFIELDLENGTH
               MOVE 1 TO F-INDEX
               CLOSE CRJRN-FILE
               GO TO RDTR-999.
       RDTR-010.
           IF F-EXIT-CH NOT = 1
             READ CRJRN-FILE NEXT
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
             READ CRJRN-FILE PREVIOUS
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
            IF WS-CRJRN-ST1 = 10
               MOVE 1 TO F-INDEX
               MOVE " " TO F-EXIT-CH
               CLOSE CRJRN-FILE
               GO TO RDTR-000.
           IF F-EXIT-CH NOT = 1
            IF WS-CRJRN-ST1 = 10
               MOVE 1 TO F-INDEX
               MOVE " " TO F-EXIT-CH
               CLOSE CRJRN-FILE
               GO TO RDTR-999.
           IF WS-CRJRN-ST1 NOT = 0
               MOVE
               "CRJRN BUSY ON READ-NEXT, IN 1 SECOND GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               GO TO RDTR-010.
           IF F-EXIT-CH = 1
            IF CRJRN-CRACC-NUMBER NOT = CR-ACCOUNT-NUMBER
               MOVE 1 TO F-INDEX
               MOVE " " TO F-EXIT-CH
               CLOSE CRJRN-FILE
               GO TO RDTR-000.
           IF F-EXIT-CH NOT = 1
            IF CRJRN-CRACC-NUMBER NOT = CR-ACCOUNT-NUMBER
               MOVE 1 TO F-INDEX
               MOVE " " TO F-EXIT-CH
               CLOSE CRJRN-FILE
               GO TO RDTR-999.
           IF WS-PER NOT = 0
            IF CRJRN-NO NOT = WS-PER
               GO TO RDTR-010.
      *    IF CRJRN-COMPLETE = "P" OR = "Y"
      *        GO TO RDTR-010.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
       RDTR-020. 
           IF F-INDEX > 15
                MOVE 2905 TO POS
                DISPLAY "Press 'PgDn' For More, OR 'PgUp' For Previous,"
                 AT POS
                ADD 47 TO POS
                DISPLAY "'ESC' To Clear The Screen !" AT POS
                MOVE 3010 TO POS
                DISPLAY "Or Press 'F10' To Print All Transactions"
                   AT POS
                ADD 41 TO POS
                DISPLAY "For This Account Number." AT POS
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
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                    AND NOT = X"07" AND NOT = " "   AND NOT = X"1F"
                    AND NOT = 1 
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           ADD 1 TO LINES-DISPLAYED.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTR-010.
       RDTR-999.
           EXIT.
      *
       READ-ALL-TRANSACTIONS SECTION.
       RDALL-000.
           PERFORM OPEN-006.
       RDALL-005.
           MOVE 0 TO WS-NO-OF-TRANS WS-TRANS-AMT.
           MOVE CR-ACCOUNT-NUMBER TO CRJRN-CRACC-NUMBER.
           START CRJRN-FILE KEY NOT < CRJRN-CRACC-NUMBER
               INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
               GO TO RDALL-900.
       RDALL-010.
           READ CRJRN-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
               GO TO RDALL-900.
           IF WS-CRJRN-ST1 NOT = 0
               MOVE "CRJRN-ST1 ERROR IN READ-NEXT 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRJRN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RDALL-010.
           IF CRJRN-CRACC-NUMBER NOT = CR-ACCOUNT-NUMBER
               GO TO RDALL-900.
           IF WS-PER NOT = 0
            IF CRJRN-NO NOT = WS-PER
               GO TO RDALL-010.
      *     IF CRJRN-COMPLETE = "P" OR = "Y"
      *        GO TO RDALL-010.

           MOVE CRJRN-REFERENCE         TO WS-CRJRN-REF (SUB-1).
           MOVE CRJRN-TRANS             TO WS-CRJRN-TRANS (SUB-1).
           MOVE CRJRN-TYPE              TO WS-CRJRN-TYPE (SUB-1).
           
           ADD 1             TO WS-NO-OF-TRANS.
           ADD CRJRN-LOC-AMT TO WS-TRANS-AMT.

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
       RDALL-910.
            MOVE "TOTTRANS"     TO F-FIELDNAME.
            MOVE 8              TO F-CBFIELDNAME.
            MOVE WS-NO-OF-TRANS TO F-EDNAMEFIELDCRED.
            MOVE 5              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-CRED.

            MOVE "TRANSAMT"   TO F-FIELDNAME.
            MOVE 8            TO F-CBFIELDNAME.
            MOVE WS-TRANS-AMT TO F-EDNAMEFIELDSALE.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
       RDALL-920.
      *     MOVE 2912 TO POS.
      *     DISPLAY "Total # of Lines:" AT POS
      *     ADD 19 TO POS.
      *     MOVE SUB-9 TO WS-BODY-LINE.
      *     DISPLAY WS-BODY-LINE AT POS.
           ADD 1 TO SUB-9.
       RDALL-950.
           CLOSE CRJRN-FILE.
       RDALL-999.
           EXIT.
      *
       READ-ORDER-ONLY SECTION.
       RDONLY-000.
           IF SUB-1 > SUB-9
               GO TO RDONLY-999.
       RDONLY-005.
           MOVE WS-CRJRN-REF (SUB-1)    TO CRJRN-REFERENCE.
           MOVE WS-CRJRN-TRANS (SUB-1)  TO CRJRN-TRANS.
           MOVE WS-CRJRN-TYPE (SUB-1)   TO CRJRN-TYPE.
           START CRJRN-FILE KEY NOT < CRJRN-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 NOT = 0
               CLOSE CRJRN-FILE
               GO TO RDONLY-999.
       RDONLY-010.
           READ CRJRN-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE CRJRN-FILE
               GO TO RDONLY-999.
           IF WS-CRJRN-ST1 NOT = 0
               MOVE "CRJRN BUSY ON READ-ONLY, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               GO TO RDONLY-010.
       RDONLY-999.
           EXIT.
      *
       READ-CREDITOR SECTION.
       RD-010.
           START CREDITOR-MASTER KEY NOT < CR-KEY.
       RD-015.
           READ CREDITOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
               MOVE "UNKNOWN" TO CR-NAME
               GO TO RD-999.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE"CREDITORS BUSY ON READ, RD-015 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RD-015.
       RD-999.
           EXIT.
      *
       START-CREDITOR SECTION.
       ST-ST-000.
              MOVE WS-CREDITORNUMBER TO CR-ACCOUNT-NUMBER.
              START CREDITOR-MASTER KEY NOT < CR-ACCOUNT-NUMBER.
       ST-ST-999.
             EXIT.
      *
       READ-CREDITOR-NEXT SECTION.
       R-ST-NX-000.
             MOVE 0 TO WS-CREDITOR-ST1.
             MOVE " " TO WS-MESSAGE
             MOVE 3010 TO POS
             DISPLAY WS-MESSAGE AT POS.
       R-ST-NX-005. 
             READ CREDITOR-MASTER NEXT
                 AT END NEXT SENTENCE.
             IF WS-CREDITOR-ST1 = 0
                 GO TO R-ST-NX-999
             ELSE
                 MOVE 0 TO WS-CREDITOR-ST1
                 PERFORM START-CREDITOR
                 MOVE 
               "CREDITOR READ-NEXT ON START, IN 1 SEC GOING TO RESTART"
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 CALL "C$SLEEP" USING 1
                 PERFORM ERROR1-020
                 GO TO R-ST-NX-005.
       R-ST-NX-999.
             EXIT.
      *
       READ-CREDITOR-PREVIOUS SECTION.
       RDPREV-000.
             MOVE 0 TO WS-CREDITOR-ST1.
             MOVE " " TO WS-MESSAGE
             MOVE 3010 TO POS
             DISPLAY WS-MESSAGE AT POS.
       RDPREV-005. 
             READ CREDITOR-MASTER PREVIOUS
                 AT END NEXT SENTENCE.
             IF WS-CREDITOR-ST1 = 0
                 GO TO RDPREV-999
             ELSE
                 MOVE 0 TO WS-CREDITOR-ST1
                 PERFORM START-CREDITOR
                 GO TO RDPREV-005.
       RDPREV-999.
             EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            PERFORM OPEN-006.
            MOVE 0  TO PAGE-CNT WS-TRANS-AMT.
            MOVE 66 TO LINE-CNT.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            MOVE CR-ACCOUNT-NUMBER TO CRJRN-CRACC-NUMBER.
            START CRJRN-FILE KEY NOT < CRJRN-CRACC-NUMBER
               INVALID KEY NEXT SENTENCE.
            IF WS-CRJRN-ST1 NOT = 0
               MOVE "BAD START ON PRINT SECTION, GOING TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PRR-900.
       PRR-002.
           READ CRJRN-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
               MOVE 0 TO WS-CRJRN-ST1
               GO TO PRR-900.
           IF WS-CRJRN-ST1 NOT = 0
               MOVE 
                 "CRJRN BUSY ON READ-NEXT PRINT, 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRJRN-ST1
               GO TO PRR-002.
           IF CRJRN-CRACC-NUMBER < CR-ACCOUNT-NUMBER
               GO TO PRR-002.
           IF CRJRN-CRACC-NUMBER > CR-ACCOUNT-NUMBER
               GO TO PRR-900.
           IF WS-PER NOT = 0
            IF CRJRN-NO NOT = WS-PER
               GO TO PRR-002.
      *    IF CRJRN-COMPLETE = "P" OR = "Y"
      *        GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           MOVE CRJRN-PERIOD               TO D-PERIOD
           MOVE CRJRN-REFERENCE            TO D-JRNNO
           MOVE WS-TRANS-DESC (CRJRN-TYPE) TO D-TYPE
           MOVE CRJRN-INV-NO               TO D-INVNO
           MOVE CRJRN-INV-DATE             TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE               TO D-INVDATE
           MOVE CRJRN-DNOTE-NO             TO D-DNOTE
           MOVE CRJRN-DUE-DATE             TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE               TO D-DUEDATE
           MOVE CRJRN-LOC-AMT              TO D-RANDAMT
           MOVE CRJRN-UNAPPLIED-AMT        TO D-UNAPPLIED
           MOVE CRJRN-FOR-AMT              TO D-FORAMT
           MOVE CRJRN-EXCHANGE             TO D-EXCHANGE
           MOVE CRJRN-SETT-DISC            TO D-SETTDISC
           MOVE CRJRN-COMPLETE             TO D-POSTED
           MOVE CRJRN-TRANS                TO D-TRANS.
           WRITE PRINT-REC FROM DETAIL-LINE.
           ADD CRJRN-LOC-AMT TO WS-TRANS-AMT
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRR-002.
       PRR-060.
            ADD 1                   TO PAGE-CNT.
            MOVE PAGE-CNT           TO H1-PAGE.
            MOVE GLPA-CURRENT-CRPER TO H1-PERIOD.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
            MOVE " "               TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD1 AFTER 1.
            MOVE " "               TO PRINT-REC.
            MOVE CR-ACCOUNT-NUMBER TO H3-ACC.
            WRITE PRINT-REC FROM HEAD2 AFTER 2.
            MOVE " " TO PRINT-REC.
            MOVE CR-NAME           TO H3-NAME.
            WRITE PRINT-REC FROM HEAD2-1 AFTER 1.
            MOVE CR-BALANCE        TO H3-BALANCE.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD2-2 AFTER 1.
            IF WS-PER = 0
                MOVE "ALL PERIODS" TO H3-PERIOD-READ
            ELSE
                MOVE WS-PER        TO H3-PERIOD-READ.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD2-3 AFTER 1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD3 AFTER 1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE " " TO PRINT-REC.
            MOVE 8 TO LINE-CNT.
       PRR-900.
           MOVE WS-TRANS-AMT TO T-RANDAMT.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " "              TO PRINT-REC.
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

      *      MOVE 3015 TO POS.
      *      DISPLAY "Current Line#: " AT POS
      *      ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
      *      DISPLAY WS-BODY-LINE AT POS.
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

      *      MOVE 3015 TO POS.
      *      DISPLAY "Current Line#: " AT POS
      *      ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
      *      DISPLAY WS-BODY-LINE AT POS.
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
      *       MOVE 3015 TO POS.
      *      DISPLAY "Current Line#: " AT POS
      *      ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
      *      DISPLAY WS-BODY-LINE AT POS.
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
      *        MOVE 3015 TO POS.
      *        DISPLAY "Current Line#: " AT POS
      *        ADD 16 TO POS.
              MOVE SUB-1 TO WS-BODY-LINE.
      *        DISPLAY WS-BODY-LINE AT POS.
       PREV-999.
            EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            IF SUB-1 < SUB-9
               PERFORM READ-ORDER-ONLY
            ELSE
               GO TO SCROLL-999.

            MOVE "PERIOD" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            MOVE CRJRN-NO TO F-NAMEFIELD.
            MOVE 2        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "JRN"           TO F-FIELDNAME.
            MOVE 3               TO F-CBFIELDNAME.
            MOVE CRJRN-REFERENCE TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE"                     TO F-FIELDNAME.
            MOVE 4                          TO F-CBFIELDNAME.
            MOVE WS-TRANS-DESC (CRJRN-TYPE) TO F-NAMEFIELD.
            MOVE 3                          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE CRJRN-INV-NO TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVDATE"       TO F-FIELDNAME.
            MOVE 7               TO F-CBFIELDNAME.
            MOVE CRJRN-INV-DATE  TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE WS-CONVERT-DATE TO F-NAMEFIELD.
            MOVE 8               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DUEDATE"       TO F-FIELDNAME.
            MOVE 7               TO F-CBFIELDNAME.
            MOVE CRJRN-DUE-DATE  TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE WS-CONVERT-DATE TO F-NAMEFIELD.
            MOVE 8               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "RAND"        TO F-FIELDNAME.
            MOVE 4             TO F-CBFIELDNAME.
            MOVE CRJRN-LOC-AMT TO F-EDNAMEFIELDSALE.
            MOVE 11            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.

            MOVE "UNAPPLIED"         TO F-FIELDNAME.
            MOVE 9                   TO F-CBFIELDNAME.
            MOVE CRJRN-UNAPPLIED-AMT TO F-EDNAMEFIELDSALE.
            MOVE 11                  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.

            MOVE "SETTDISC"      TO F-FIELDNAME.
            MOVE 8               TO F-CBFIELDNAME.
            MOVE CRJRN-SETT-DISC TO F-EDNAMEFIELDADDON.
            MOVE 7               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ADDON.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO F-INDEX.

            MOVE "LINES" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            MOVE " "     TO F-NAMEFIELD.
            MOVE 5       TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       CLTR-010.
            IF F-INDEX > 15
                GO TO CLTR-999.
            MOVE "PERIOD" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            MOVE " "      TO F-NAMEFIELD.
            MOVE 2        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "JRN" TO F-FIELDNAME.
            MOVE 3     TO F-CBFIELDNAME.
            MOVE " "   TO F-NAMEFIELD.
            MOVE 10    TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            MOVE " "    TO F-NAMEFIELD.
            MOVE 3      TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 10        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 8         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DUEDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 8         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "RAND" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            MOVE " "    TO F-NAMEFIELD.
            MOVE 11     TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "UNAPPLIED" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            MOVE " "         TO F-NAMEFIELD.
            MOVE 11          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SETTDISC" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE " "        TO F-NAMEFIELD.
            MOVE 11         TO F-CBFIELDLENGTH.
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
            IF WS-CRJRN-TYPE (SUB-1) NOT = 0
                MOVE 0   TO WS-CRJRN-TYPE (SUB-1)
                            WS-CRJRN-TRANS (SUB-1)
                MOVE " " TO WS-CRJRN-REF (SUB-1)
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
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO GLPARAMETER RECORD FOUND!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE " CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO OPEN-000.
       OPEN-005.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-005.
            GO TO OPEN-007.
       OPEN-006.
           OPEN I-O CRJRN-FILE.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CRJRN-ST1 ERROR IN OPEN-006, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO OPEN-006.
       OPEN-007.
           PERFORM READ-PARAMETER.
           MOVE GLPA-NAME TO CO-NAME.
           PERFORM ENTER-PERIOD-DATES.
      *     PERFORM OPEN-006.
           CLOSE GLPARAMETER-FILE.
       OPEN-008.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrJrnIq"       TO F-FORMNAME
           MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-900.
            CLOSE CREDITOR-MASTER.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldChange".
       Copy "WriteFieldCred".
       Copy "WriteFieldSale".
       Copy "WriteFieldAddOn".
       
      * Copy "<Copy>ComputeCrDatePeriod".
      * Copy "<Copy>EnterCrPeriodDates".
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "DisplayFormCRTopInfo".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
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
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      * END-OF-JOB.
