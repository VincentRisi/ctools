        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlPoShIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectSlRegister".
           SELECT PRINT-FILE ASSIGN TO Ws-Printer
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdRegister.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-STOCK-PROGRAM     PIC X(8) VALUE "StMastIq".
       77  WS-ACCOUNT-PROGRAM   PIC X(8) VALUE "DrNameIq".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-TYPE-OF-KEY       PIC X(20) VALUE " ".
       77  WS-KEY               PIC X VALUE " ".
       77  WS-PORDER            PIC X(5).
       77  WS-ACCOUNT           PIC 9(7).
       77  WS-NO-CH             PIC 9 VALUE 0.
       77  W-PO1                PIC X(1) VALUE " ".
       77  W-PO2                PIC X(2) VALUE " ".
       77  W-PO3                PIC X(3) VALUE " ".
       77  W-PO4                PIC X(4) VALUE " ".
       77  W-PO5                PIC X(5) VALUE " ".
       77  W-PO-CH1             PIC X(1) VALUE " ".
       77  W-PO-CH2             PIC X(2) VALUE " ".
       77  W-PO-CH3             PIC X(3) VALUE " ".
       77  W-PO-CH4             PIC X(4) VALUE " ".
       77  W-PO-CH5             PIC X(5) VALUE " ".
       77  W-INVALID-TYPE       PIC X VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-BODY-LINE         PIC Z(5)9.
       01  WS-SCROLL-NUMBERS.
           03  WS-SCROLL-NUM OCCURS 10000.
             05  WS-INCR-TRANS   PIC 9.
             05  WS-INCR-INVOICE PIC 9(6).
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1  PIC 99.
       01  SPLIT-ORDER.
           03  SP-1ST       PIC X.
           03  SP-2ND       PIC X.
           03  SP-3RD       PIC X.
           03  SP-4TH       PIC X.
           03  SP-5TH       PIC X.
           03  SP-REST      PIC X(15).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(60) VALUE
           "** SHORT PURCHASE ORDERS INQUIRY **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(11) VALUE " SHORT PO:".
           03  H3-PORDER      PIC X(20).
           03  FILLER         PIC X(17) VALUE "TYPE OF ENQUIRY:".
           03  H3-TYPE        PIC X(84) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(8) VALUE "ACCOUNT".
           03  FILLER         PIC X(22) VALUE "PURCHASE ORDER".
           03  FILLER         PIC X(22) VALUE "NUMBER    DATE".
           03  FILLER         PIC X(26) VALUE "STATUS".
           03  FILLER         PIC X(57) VALUE "AMOUNT".
       01  DETAIL-LINE.
           03  D-ACCOUNT      PIC X(8).
           03  D-PONO         PIC X(22).
           03  D-1STINV       PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-1STDATE      PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-COMPLETE     PIC X(26) VALUE " ".
           03  D-AMOUNT       PIC Z(7)9.99.
           03  FILLER         PIC X(46) VALUE " ".
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
           DISPLAY "** SHORT PURCHASE ORDER INQUIRY BY ORDER **" AT POS
           MOVE 0410 TO POS
           DISPLAY "*******************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 2910 TO POS
           DISPLAY "Program now Loading, please be patient...." AT POS.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-020.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-020.
      *
       GET-DATA SECTION.
       GET-000. 
            MOVE "     " TO WS-PORDER
                            W-PO1
                            W-PO2
                            W-PO3
                            W-PO4
                            W-PO5
                            W-PO-CH1
                            W-PO-CH2
                            W-PO-CH3
                            W-PO-CH4
                            W-PO-CH5.
            MOVE "N" TO WS-ANSWER
                         W-INVALID-TYPE.
            MOVE 1   TO  F-INDEX.
            PERFORM CLEAR-MEMORY.

            PERFORM OPEN-006.
       GET-010. 
            MOVE 2705 TO POS.
            DISPLAY
            "Press F4=B-ORDERS, F5=PENDING REPAIRS,F6=COMPLETE" &
            " REPAIRS, F7=INVOICES." AT POS.
            MOVE "SHORTORDER" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR-PORDER
                                WS-PORDER.
            IF INCR-PORDER = "STOCK"
                CLOSE INCR-REGISTER
                CALL WS-STOCK-PROGRAM USING WS-LINKAGE
                CANCEL WS-STOCK-PROGRAM
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            IF INCR-PORDER = " " OR = "0"
                CLOSE INCR-REGISTER
                CALL WS-ACCOUNT-PROGRAM USING WS-LINKAGE
                CANCEL WS-ACCOUNT-PROGRAM
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                GO TO GET-000.
                
      *          MOVE F-CBFIRSTLINE TO WS-MESSAGE
      *          PERFORM ERROR-MESSAGE.
                
            IF F-CBFIRSTLINE = 1
                MOVE WS-PORDER TO W-PO1
                MOVE 1 TO WS-NO-CH
                GO TO GET-020.
            IF F-CBFIRSTLINE = 2
                MOVE WS-PORDER TO W-PO2
                MOVE 2 TO WS-NO-CH
                GO TO GET-020.
            IF F-CBFIRSTLINE = 3
                MOVE WS-PORDER TO W-PO3
                MOVE 3 TO WS-NO-CH
                GO TO GET-020.
            IF F-CBFIRSTLINE = 4
                MOVE WS-PORDER TO W-PO4
                MOVE 4 TO WS-NO-CH
                GO TO GET-020.
            IF F-CBFIRSTLINE = 5
                MOVE WS-PORDER TO W-PO5
                MOVE 5 TO WS-NO-CH.
       GET-020.
            IF F-EXIT-CH = X"18"
                MOVE " *CHECKING ORDERS*" TO WS-TYPE-OF-KEY
                MOVE "O" TO WS-KEY.
            IF F-EXIT-CH = X"19"
                MOVE " *PENDING REPAIRS*" TO WS-TYPE-OF-KEY
                MOVE "R" TO WS-KEY.
            IF F-EXIT-CH = X"1A"
                MOVE " *COMPLETE REPAIRS*" TO WS-TYPE-OF-KEY
                MOVE "Y" TO WS-KEY.
            IF F-EXIT-CH = X"1C"
                MOVE "*CHECKING INVOICES*" TO WS-TYPE-OF-KEY
                MOVE "I" TO WS-KEY.

            IF F-EXIT-CH NOT = X"18" AND NOT = X"19" AND NOT = X"1C"
                     AND NOT = X"1A"
                GO TO GET-010.

            MOVE "PODESC"       TO F-FIELDNAME.
            MOVE 6              TO F-CBFIELDNAME.
            MOVE WS-TYPE-OF-KEY TO F-NAMEFIELD.
            MOVE 20             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-850.
            MOVE 2705 TO POS.
            DISPLAY
            "                                                       " &
            "                                          " AT POS.
            MOVE "                         " TO F-NAMEFIELD.
            MOVE "ACCNUM" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-010.
           IF F-EXIT-CH = X"07"
               PERFORM DISPLAY-FORM
               CLOSE INCR-REGISTER
               GO TO GET-000.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-ACCOUNT.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
       GET-040.
            MOVE " " TO F-EXIT-CH.
            CLOSE INCR-REGISTER.
            
            PERFORM READ-ALL-TRANSACTIONS.
      *      IF WS-ANSWER =  "N"
      *         GO TO GET-999.
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
            IF WS-ANSWER = "Y"
                CLOSE INCR-REGISTER
                GO TO GET-999.
            IF F-INDEX < 15
               MOVE 2905 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen," AT POS
               MOVE 3005 TO POS
               DISPLAY "Or Press 'F10' To Print All Information." AT POS
               MOVE 15 TO F-INDEX
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                OPEN I-O INCR-REGISTER
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE INCR-REGISTER
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
           MOVE 3015 TO POS 
           DISPLAY "Current Line#: " AT POS
           ADD 16 TO POS
           MOVE SUB-1 TO WS-BODY-LINE
           DISPLAY WS-BODY-LINE AT POS.

           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.


            MOVE "ACCNO"      TO F-FIELDNAME.
            MOVE 5            TO F-CBFIELDNAME.
            MOVE 7            TO F-CBFIELDLENGTH.
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
      *TAB - <ALT-F8>
           IF F-EXIT-CH = X"09"
              GO TO FILL-900.
      *ESC
           IF F-EXIT-CH = X"07"
              GO TO FILL-900.
      * <f10> to print
           IF F-EXIT-CH = X"1F"
                CLOSE INCR-REGISTER
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
           MOVE " " TO WS-MESSAGE
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           CLOSE INCR-REGISTER.
       FILL-999.
           EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           PERFORM OPEN-006.
       RDTR-005.
           MOVE WS-PORDER TO INCR-PORDER.
           START INCR-REGISTER KEY NOT < INCR-PORDER
                INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
      *         MOVE INCR-PORDER TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               CLOSE INCR-REGISTER
               MOVE "N" TO WS-ANSWER
               GO TO RDTR-999.
           MOVE "Y" TO WS-NEWINPUT.
           MOVE 1 TO SUB-1 F-INDEX.
           MOVE " " TO F-EXIT-CH.
       RDTR-010.
           IF F-EXIT-CH = " "
             READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
             READ INCR-REGISTER PREVIOUS
               AT END NEXT SENTENCE.
           IF WS-NEWINPUT = "Y"
               PERFORM CLEAR-TRANSACTIONS
               MOVE " " TO F-EXIT-CH
               MOVE "N" TO WS-NEWINPUT
               MOVE 1 TO F-INDEX.

           IF F-EXIT-CH = " "
            IF WS-INCR-ST1 = 10 OR = 91
               MOVE 1 TO F-INDEX
               CLOSE INCR-REGISTER
               GO TO RDTR-999.
           IF F-EXIT-CH = 1
            IF WS-INCR-ST1 = 10 OR = 91
               MOVE 1 TO F-INDEX
               CLOSE INCR-REGISTER
               GO TO RDTR-000.
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
           IF WS-KEY = "O"
            IF INCR-TRANS NOT = 4 AND NOT = 7
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next Back-Order...." AT POS
               GO TO RDTR-010.
           IF WS-KEY = "R"
            IF INCR-TRANS NOT = 3
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next Repair........" AT POS
               GO TO RDTR-010.
           IF WS-KEY = "R"
            IF INCR-PRINTED NOT = "R"
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next Repair........" AT POS
               GO TO RDTR-010.
           IF WS-KEY = "Y"
            IF INCR-TRANS NOT = 3
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next Repair........" AT POS
               GO TO RDTR-010.
           IF WS-KEY = "Y"
            IF INCR-PRINTED NOT = "Y"
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next Repair........" AT POS
               GO TO RDTR-010.
           IF WS-KEY = "I"
            IF INCR-TRANS NOT = 1 AND NOT = 6
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next Invoice......." AT POS
               GO TO RDTR-010.

           PERFORM CHECK-ORDER-TYPE.
           PERFORM ERROR1-020
           PERFORM ERROR-020.

           IF W-INVALID-TYPE = "Y"
               MOVE 1 TO F-INDEX
               CLOSE INCR-REGISTER
               GO TO RDTR-999.
           IF WS-ACCOUNT = 0
               GO TO RDTR-020.
           IF INCR-ACCOUNT NOT = WS-ACCOUNT
            IF WS-ACCOUNT NOT = 0
               GO TO RDTR-010.
       RDTR-020. 
           IF F-INDEX > 15
             MOVE 2905 TO POS
             DISPLAY "Press 'PgDn' For More, 'PgUp' For Previous,"
              AT POS
             ADD 44 TO POS
             DISPLAY "OR 'ESC' To Clear The Screen !" AT POS
             MOVE 3010 TO POS
             DISPLAY "Or Press 'F10' To Print All Information." AT POS
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
       RDTR-500.
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
           MOVE WS-PORDER TO INCR-PORDER.
           START INCR-REGISTER KEY NOT < INCR-PORDER
                INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON START, 'ESC' TO EXIT AND RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
      *         MOVE INCR-PORDER TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               CLOSE INCR-REGISTER
               MOVE "Y" TO WS-ANSWER
               GO TO RDALL-999.
           MOVE "Y" TO WS-NEWINPUT.
           MOVE 1 TO SUB-1 F-INDEX.
       RDALL-010.
            READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
            IF WS-INCR-ST1 = 10 OR = 91
               MOVE 1 TO F-INDEX
               CLOSE INCR-REGISTER
               GO TO RDALL-900.
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
               GO TO RDALL-010.
           IF WS-KEY = "O"
            IF INCR-TRANS NOT = 4 AND NOT = 7
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next Back-Order...." AT POS
               GO TO RDALL-010.
           IF WS-KEY = "R"
            IF INCR-TRANS NOT = 3
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next Repair........" AT POS
               GO TO RDALL-010.
           IF WS-KEY = "R"
            IF INCR-PRINTED NOT = "R"
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next Repair........" AT POS
               GO TO RDALL-010.
           IF WS-KEY = "Y"
            IF INCR-TRANS NOT = 3
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next Repair........" AT POS
               GO TO RDALL-010.
           IF WS-KEY = "Y"
            IF INCR-PRINTED NOT = "Y"
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next Repair........" AT POS
               GO TO RDALL-010.
           IF WS-KEY = "I"
            IF INCR-TRANS NOT = 1 AND NOT = 6
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next Invoice......." AT POS
               GO TO RDALL-010.

           PERFORM CHECK-ORDER-TYPE.
           PERFORM ERROR1-020
           PERFORM ERROR-020.

           IF W-INVALID-TYPE = "Y"
               MOVE 1 TO F-INDEX
               CLOSE INCR-REGISTER
               GO TO RDALL-900.
           IF WS-ACCOUNT = 0
               GO TO RDALL-020.
           IF WS-ACCOUNT NOT = 0
            IF INCR-ACCOUNT NOT = WS-ACCOUNT
               GO TO RDALL-010.
       RDALL-020.
           MOVE INCR-TRANS      TO WS-INCR-TRANS (SUB-1)
           MOVE INCR-INVOICE    TO WS-INCR-INVOICE (SUB-1).
           
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
           PERFORM ERROR1-020
           PERFORM ERROR-020.

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
           CLOSE INCR-REGISTER.
       RDALL-999.
           EXIT.
      *
       READ-ORDER-ONLY SECTION.
       RDONLY-005.
           IF SUB-1 > SUB-9
               GO TO RDONLY-999.
               
           MOVE WS-INCR-TRANS (SUB-1)     TO INCR-TRANS
           MOVE WS-INCR-INVOICE (SUB-1)   TO INCR-INVOICE
           START INCR-REGISTER KEY NOT < INCR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
                CLOSE INCR-REGISTER
                GO TO RDONLY-999.
       RDONLY-010.
            READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
               MOVE 1 TO F-INDEX
               GO TO RDONLY-999.
           IF WS-INCR-ST1 NOT = 0
             MOVE "REGISTER BUSY ON READ-ONLY, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RDONLY-010.
       RDONLY-999.
           EXIT.
      *
       CHECK-ORDER-TYPE SECTION.
       CH-O-T-005.
           MOVE "N" TO W-INVALID-TYPE.
       CH-O-T-010.
           IF WS-NO-CH = 1
              MOVE INCR-PORDER TO W-PO-CH1
              GO TO CH-O-T-100.
           IF WS-NO-CH = 2
              MOVE INCR-PORDER TO W-PO-CH2
              GO TO CH-O-T-200.
           IF WS-NO-CH = 3
              MOVE INCR-PORDER TO W-PO-CH3
              GO TO CH-O-T-300.
           IF WS-NO-CH = 4
              MOVE INCR-PORDER TO W-PO-CH4
              GO TO CH-O-T-400.
           IF WS-NO-CH = 5
              MOVE INCR-PORDER TO W-PO-CH5
              GO TO CH-O-T-500.
       CH-O-T-100.
           IF W-PO-CH1 NOT = W-PO1
              MOVE "Y" TO W-INVALID-TYPE.
           GO TO CH-O-T-999.
       CH-O-T-200.
           IF W-PO-CH2 NOT = W-PO2
              MOVE "Y" TO W-INVALID-TYPE.
           GO TO CH-O-T-999.
       CH-O-T-300.
           IF W-PO-CH3 NOT = W-PO3
              MOVE "Y" TO W-INVALID-TYPE.
           GO TO CH-O-T-999.
       CH-O-T-400.
           IF W-PO-CH4 NOT = W-PO4
              MOVE "Y" TO W-INVALID-TYPE.
           GO TO CH-O-T-999.
       CH-O-T-500.
           IF W-PO-CH5 NOT = W-PO5
              MOVE "Y" TO W-INVALID-TYPE.
       CH-O-T-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE 0  TO PAGE-CNT
            MOVE 66 TO LINE-CNT.
            PERFORM OPEN-006.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            MOVE WS-PORDER TO INCR-PORDER.
            START INCR-REGISTER KEY NOT < INCR-PORDER
                 INVALID KEY NEXT SENTENCE.
       PRR-002.
            READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
            IF WS-INCR-ST1 = 10 OR = 91
               MOVE 0 TO WS-INCR-ST1
               GO TO PRR-900.
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
               GO TO PRR-002.
            IF INCR-PORDER < WS-PORDER
               GO TO PRR-002.
            IF WS-KEY = "O"
             IF INCR-TRANS NOT = 4 AND NOT = 7
               GO TO PRR-002.
            IF WS-KEY = "R"
             IF INCR-TRANS NOT = 3
               GO TO PRR-002.
            IF WS-KEY = "Y"
             IF INCR-TRANS NOT = 3
               GO TO PRR-002.
            IF WS-KEY = "Y"
             IF INCR-PRINTED NOT = "Y"
               GO TO PRR-002.
            IF WS-KEY = "I"
             IF INCR-TRANS NOT = 1 AND NOT = 6
               GO TO PRR-002.
      *     PERFORM CHECK-ORDER-TYPE.
           IF W-INVALID-TYPE = "Y"
               GO TO PRR-900.
           IF WS-ACCOUNT = 0
               GO TO PRR-010.
           IF INCR-ACCOUNT NOT = WS-ACCOUNT
               GO TO PRR-002.
        PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
        PRR-020.
           MOVE INCR-PORDER       TO D-PONO
           MOVE INCR-ACCOUNT      TO D-ACCOUNT
           MOVE INCR-INVOICE      TO D-1STINV
           MOVE INCR-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-1STDATE
           MOVE INCR-INVCRED-AMT  TO D-AMOUNT.
           IF WS-KEY = "O"
            IF INCR-TRANS = 4
             IF INCR-PRINTED = "Y" OR = "L"
               MOVE "**COMPLETE ORDER**        " TO D-COMPLETE.
           IF WS-KEY = "O"
            IF INCR-TRANS = 4
             IF INCR-PRINTED = "N"
               MOVE " **PART ORDER**           " TO D-COMPLETE.
           IF WS-KEY = "O"
            IF INCR-TRANS = 4
             IF INCR-PRINTED = "S"
               MOVE " **SUSPENDED**            " TO D-COMPLETE.
           IF WS-KEY = "O"
            IF INCR-TRANS = 7
             IF INCR-PRINTED = "Y" OR = "L"
               MOVE "**COMPLETED KIT **        " TO D-COMPLETE
             ELSE
               MOVE "** KIT NOT ASSEMBLED **   " TO D-COMPLETE.

           IF WS-KEY = "R"
            IF INCR-TRANS = 3
             IF INCR-PRINTED = "Y" OR = "L"
               MOVE "**COMPLETED REPAIR**      " TO D-COMPLETE.
           IF WS-KEY = "R"
            IF INCR-TRANS = 3
             IF INCR-PRINTED = "S"
               MOVE "**SUSPENDED REPAIR**      " TO D-COMPLETE.
           IF WS-KEY = "R"
            IF INCR-TRANS = 3
             IF INCR-PRINTED = "R"
               MOVE "**INCOMPLETE REPAIR **    " TO D-COMPLETE.
               
           IF WS-KEY = "I"
            IF INCR-TRANS = 1
               MOVE "**COMPLETE INVOICE**      " TO D-COMPLETE.
           IF WS-KEY = "I"
            IF INCR-TRANS = 6
               MOVE "**COMPLETE CREDIT**       " TO D-COMPLETE.
           
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC.
           ADD 1 TO LINE-CNT.
           GO TO PRR-002.
       PRR-060.
            ADD 1         TO PAGE-CNT.
            MOVE PAGE-CNT TO H1-PAGE.

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF WS-KEY = "O"
                MOVE "** ENQUIRY BY ORDER/KIT **"       TO H3-TYPE.
            IF WS-KEY = "R" OR = "Y"
                MOVE "** ENQUIRY BY REPAIR    **"       TO H3-TYPE.
            IF WS-KEY = "I"
                 MOVE "** ENQUIRY BY INVOICE/CREDIT **" TO H3-TYPE.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD1 AFTER 1.
            MOVE " " TO PRINT-REC.
            MOVE WS-PORDER       TO H3-PORDER.
            WRITE PRINT-REC FROM HEAD2 AFTER 1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD3 AFTER 1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE " " TO PRINT-REC.
            MOVE 5 TO LINE-CNT.
       PRR-900.
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
            IF SUB-1 < SUB-9
               PERFORM READ-ORDER-ONLY
            ELSE
               GO TO SCROLL-999.

            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-ACCOUNT TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PORDERNO" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE INCR-PORDER TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INV" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE INCR-INVOICE TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "INVDATE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE INCR-DATE    TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT"         TO F-FIELDNAME
            MOVE 6                TO F-CBFIELDNAME
            MOVE INCR-INVCRED-AMT TO F-EDNAMEFIELD99Mil
            MOVE 11               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "COMPLETE" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            IF WS-KEY = "O"
             IF INCR-TRANS = 4
              IF INCR-PRINTED = "Y" OR = "L"
                 MOVE "ORDER COMP" TO F-NAMEFIELD.
            IF WS-KEY = "O"
             IF INCR-TRANS = 4
              IF INCR-PRINTED = "N"
                 MOVE "PART ORDER" TO F-NAMEFIELD.
            IF WS-KEY = "O"
             IF INCR-TRANS = 4
              IF INCR-PRINTED = "S"
                 MOVE "SUSPENDED " TO F-NAMEFIELD.
            IF WS-KEY = "O"
             IF INCR-TRANS = 7
              IF INCR-PRINTED = "Y" OR = "L"
                 MOVE "KIT COMP. " TO F-NAMEFIELD
             ELSE
                 MOVE "KIT/ASSEMB" TO F-NAMEFIELD.
                 
            IF WS-KEY = "R"
             IF INCR-TRANS = 3
              IF INCR-PRINTED = "Y" OR = "L"
                 MOVE "COMPL REPR" TO F-NAMEFIELD.
            IF WS-KEY = "R"
             IF INCR-TRANS = 3
              IF INCR-PRINTED = "R"
                 MOVE "PART REPR." TO F-NAMEFIELD.
            IF WS-KEY = "R"
             IF INCR-TRANS = 3
              IF INCR-PRINTED = "S"
                 MOVE "SUSP. REPR" TO F-NAMEFIELD.
                 
            IF WS-KEY = "I"
             IF INCR-TRANS = 1
              IF INCR-PRINTED = "Y"
                 MOVE "INVOICED  " TO F-NAMEFIELD
             ELSE
                 MOVE "INV PRINT "TO F-NAMEFIELD.
            IF WS-KEY = "I"
             IF INCR-TRANS = 6
              IF INCR-PRINTED = "Y"
                 MOVE "CRD NOTE  " TO F-NAMEFIELD
             ELSE
                 MOVE "CR TO PRNT"TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO F-INDEX.
       CLTR-010.
            IF F-INDEX > 15
                MOVE 1 TO F-INDEX
                GO TO CLTR-999.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PORDERNO" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE " "TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INV" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 10        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 11       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COMPLETE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
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
            IF WS-INCR-INVOICE (SUB-1) NOT = 0
                MOVE 0 TO WS-INCR-TRANS (SUB-1)
                          WS-INCR-INVOICE (SUB-1)
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
       OPEN-001.
           MOVE Ws-Co-Name TO CO-NAME.
           GO TO OPEN-008.
       OPEN-006.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO OPEN-006.
       OPEN-008.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlPoShIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-900.
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
