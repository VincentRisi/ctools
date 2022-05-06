        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlPSlpIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectDrMaster".
          Copy "SelectSlRegister".
          Copy "SelectSlRegLy".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdRegister.
           COPY ChlfdRegisterLy.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-STOCK-PROGRAM     PIC X(8) VALUE "StMastIq".
       77  WS-ACCOUNT-PROGRAM   PIC X(8) VALUE "DrNameIq".
       77  POS-DOT              PIC 9(4) VALUE 0.
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-COLLECT           PIC X VALUE " ".
       77  WS-DELVIA            PIC X(7) VALUE " ".
       77  WS-INTERNAL          PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(100) VALUE " ".
       77  WS-TYPE-OF-KEY       PIC X(20) VALUE " ".
       77  WS-PRINT-AMOUNT      PIC S9(8)V99.
       77  WS-KEY               PIC X VALUE " ".
       77  WS-INVOICE           PIC 9(6).
       77  WS-ACCOUNT           PIC 9(7).
       77  WS-NO-CH             PIC 9 VALUE 0.
       77  W-INVALID-TYPE       PIC X VALUE " ".
       77  WS-INVOICEDATE       PIC 9(8) VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-LYR               PIC X VALUE " ".
      * 77  WS-LYR               PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-INCR-LY-STATUS.
           03  WS-INCR-LY-ST1   PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-ACCOUNT-CHECK.
           03  WS-1ST-3      PIC 9(3).
           03  WS-LAST-4     PIC 9(4).
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
           03  FILLER         PIC X(3) VALUE " ".
           03  H1-TYPE        PIC X(25) VALUE " ".
           03  FILLER         PIC X(75) VALUE
           "** P/SLIP-INVOICE STATUS INQUIRY BY NUMBER **".
             03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(45) VALUE " ".
           03  FILLER         PIC X(45) VALUE ALL "*".
       01  HEAD3.
           03  FILLER         PIC X(8) VALUE "TRANS#".
           03  FILLER         PIC X(34) VALUE "ACC.#   NAME".
           03  FILLER         PIC X(25) VALUE "PURCHASE ORDER".
           03  FILLER         PIC X(19) VALUE " DATE".
           03  FILLER         PIC X(22) VALUE "STATUS".
           03  FILLER         PIC X(57) VALUE "AMOUNT".
       01  DETAIL-LINE.
           03  D-PSLIP        PIC Z(5)9.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-ACCOUNT      PIC X(8).
           03  D-NAME         PIC X(25).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PONO         PIC X(22).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-DATE         PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-STATUS       PIC X(27) VALUE " ".
           03  D-AMOUNT       PIC Z(7)9.99.
           03  FILLER         PIC X(46) VALUE " ".
       01  FLAG-LINE.
           03  F-COMMENT      PIC X(26) VALUE " ".
           03  F-DATE         PIC X(10) VALUE " ".
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
           DISPLAY "** P/SLIP / INVOICE, QUOTE, CREDIT INQ BY NUM **"
            AT POS
           MOVE 0410 TO POS
           DISPLAY "************************************************"
            AT POS.
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
           MOVE "N" TO WS-ANSWER
                       W-INVALID-TYPE.
           MOVE 1 TO F-INDEX.
           MOVE 2805 TO POS
           DISPLAY
           "<F1> FOR DEL-INVS, <F3> FOR UN-INVOICED P/SLIP'S, <F5>" &
           " FOR UN-DEL INVS," AT POS
           MOVE 2903 TO POS.
           DISPLAY
           "<F6> FOR SUSPENDED P/SLIPS, <F7> FOR UN-PULLED ORDERS," &
           " <F8> FOR B/O P/SLIPS." AT POS
           MOVE 3010 TO POS.
           DISPLAY
           "OR ENTER A DATE INVOICES WERE FLAGGED AS DELIVERED & <F9>."
                AT POS
      
           MOVE "TRANS" TO F-FIELDNAME.
           MOVE 5       TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
           MOVE 10           TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD  TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO INCR-INVOICE
                                WS-INVOICE.
      *<F9>
           IF F-EXIT-CH NOT = X"1E"
               GO TO GET-010.
                
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO GET-000.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-INVOICEDATE SPLIT-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO GET-000.
           MOVE DISPLAY-DATE TO F-DATE.
           MOVE "TRANS"      TO F-FIELDNAME
           MOVE 5            TO F-CBFIELDNAME
           MOVE DISPLAY-DATE TO F-NAMEFIELD
           MOVE 10           TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       GET-010.
           IF INCR-INVOICE = " " OR = "0"
               CLOSE INCR-REGISTER
                     INCR-LY-REGISTER
               CALL WS-ACCOUNT-PROGRAM USING WS-LINKAGE
               CANCEL WS-ACCOUNT-PROGRAM
               PERFORM CLEAR-SCREEN
               OPEN I-O INCR-REGISTER
               OPEN I-O INCR-LY-REGISTER
               PERFORM DISPLAY-FORM
               GO TO GET-000.
       GET-020.
      * <F1> DELIVERED INVOICES
           IF F-EXIT-CH = X"15"
               MOVE "*CHECK DELVRD-INVCS*" TO WS-TYPE-OF-KEY
               MOVE "C" TO WS-KEY.
      * <F3> UN-INVOICED P/SLIPS
           IF F-EXIT-CH = X"17"
               MOVE "* CHECK UN-INVOICED*" TO WS-TYPE-OF-KEY
               MOVE "I" TO WS-KEY.
      * <F5> UN-DELIVERED INVOICES
           IF F-EXIT-CH = X"19"
               MOVE "*CHECK UN-DELIVERED*" TO WS-TYPE-OF-KEY
               MOVE "D" TO WS-KEY.
      * <F6> SUSPENDED P/SLIPS
           IF F-EXIT-CH = X"1A"
               MOVE "* CHECK SUSPENDED  *" TO WS-TYPE-OF-KEY
               MOVE "S" TO WS-KEY.
      * <F7> UN-PULLED P/SLIPS
           IF F-EXIT-CH = X"1C"
               MOVE "* CHECK UN-PULLED  *" TO WS-TYPE-OF-KEY
               MOVE "P" TO WS-KEY.
      * <F8> B/O P/SLIPS
           IF F-EXIT-CH = X"1D"
               MOVE "* CHECK B-ORDERS   *" TO WS-TYPE-OF-KEY
               MOVE "N" TO WS-KEY.
      * <F9> DELIVERY DATE
           IF F-EXIT-CH = X"1E"
               MOVE "*FLAG INVOICE DATE *" TO WS-TYPE-OF-KEY
               MOVE "R" TO WS-KEY.

           IF F-EXIT-CH NOT = X"15" AND NOT = X"17"
                    AND NOT = X"19" AND NOT = X"1A"
                    AND NOT = X"1C" AND NOT = X"1D"
                    AND NOT = X"1E"
               GO TO GET-000.
       GET-050.
            MOVE "PODESC"       TO F-FIELDNAME
            MOVE 6              TO F-CBFIELDNAME
            MOVE WS-TYPE-OF-KEY TO F-NAMEFIELD
            MOVE 20             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-850.
            PERFORM ERROR1-020
            PERFORM ERROR-020
            MOVE 2801 TO POS
            DISPLAY WS-MESSAGE AT POS.
            MOVE 2910 TO POS.
            DISPLAY
            "<F10> TO PRINT LIST NOW, <RETURN> TO DISPLAY." AT POS.
            
            MOVE "                         " TO F-NAMEFIELD.
            MOVE "ACCNUM" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-000.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-ACCOUNT.
            IF WS-ACCOUNT > 0
               MOVE WS-ACCOUNT TO INCR-ACCOUNT
                                  INCR-LY-ACCOUNT
               PERFORM READ-DEBTOR.
            IF WS-ACCOUNT > 0
             IF DR-NAME = "UNKNOWN"
               MOVE "INVALID DEBTOR NUMBER ENTERED, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-850.
       GET-860.
      ****************************************
      * SECTION TO SELECT : COLLECTION Y/N   *
      ****************************************
           PERFORM CLEAR-010.
           PERFORM ERROR1-020
           MOVE 2910 TO POS
           DISPLAY
           "ENTER: C=COLLECT ONLY, A=ALL TYPES, N=NO COLLECTS [ ]"
              AT POS
              ADD 51 TO POS

           MOVE WS-COLLECT TO CDA-DATA.
           MOVE 1          TO CDA-DATALEN.
           MOVE 26         TO CDA-ROW.
           MOVE 60         TO CDA-COL.
           MOVE CDA-WHITE  TO CDA-COLOR.
           MOVE 'F'        TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COLLECT.
              
           IF WS-COLLECT NOT = "A" AND NOT = "C" AND NOT = "N"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-860.
       GET-865.
      *********************************************************
      * SECTION TO SELECT LYR OR THIS YEAR. FOR F5 & F9 ONLY  *
      *********************************************************
           MOVE "T" TO WS-LYR.
           IF WS-KEY NOT = "R" AND NOT = "C" AND NOT = "D"
               GO TO GET-870.
           PERFORM CLEAR-010.
           PERFORM ERROR1-020
           MOVE 2910 TO POS
           DISPLAY "ENTER: L=LYR, T=THIS YEAR     :[ ]" AT POS
              ADD 32 TO POS

           MOVE WS-LYR    TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-LYR.
              
           IF WS-LYR NOT = "L" AND NOT = "T"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-865.
       GET-870.
           IF F-EXIT-CH = X"1F"
            IF WS-LYR = "T"
                PERFORM ERROR1-020
                PERFORM ERROR-020
                MOVE 2810 TO POS
                DISPLAY WS-MESSAGE AT POS
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient..."
                   AT POS
                MOVE 3051 TO POS-DOT
                OPEN I-O INCR-REGISTER
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE INCR-REGISTER
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO GET-999.
           IF F-EXIT-CH = X"1F"
            IF WS-LYR = "L"
                PERFORM ERROR1-020
                PERFORM ERROR-020
                MOVE 2810 TO POS
                DISPLAY WS-MESSAGE AT POS
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient..."
                   AT POS
                MOVE 3051 TO POS-DOT
                OPEN I-O INCR-LY-REGISTER
                PERFORM PRINT-LY-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE INCR-LY-REGISTER
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO GET-999.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            MOVE 2810 TO POS
            DISPLAY WS-MESSAGE AT POS.
            
      *LYR SECTION
           IF WS-LYR = "T"
              GO TO GET-880.
      * DELIVERED INVOICES
           IF WS-KEY = "C"
               MOVE "** LYR DELVRD-INVCS*" TO WS-TYPE-OF-KEY.
      * UN-DELIVERED INVOICES
           IF WS-KEY = "D"
               MOVE "** LYR UN-DELIVERED*" TO WS-TYPE-OF-KEY.
      * DELIVERY DATE
           IF WS-KEY = "R"
               MOVE "*LYR INV FLAG DATE *" TO WS-TYPE-OF-KEY.
           PERFORM GET-050.
       GET-880.
           IF WS-LYR = "T"
               PERFORM READ-TRANSACTIONS
           ELSE
               PERFORM READ-LY-TRANSACTIONS.
       GET-900.
            IF WS-LYR = "T"
             IF WS-ANSWER = "Y"
                CLOSE INCR-REGISTER
                GO TO GET-999.
            IF WS-LYR = "L"
             IF WS-ANSWER = "Y"
                CLOSE INCR-LY-REGISTER
                GO TO GET-999.
            IF F-INDEX < 15
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen," AT POS
               MOVE 3010 TO POS
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
             IF WS-LYR = "T"
                PERFORM CLEAR-TRANSACTIONS
                CLOSE INCR-REGISTER
                GO TO GET-999.
            IF F-EXIT-CH = X"07"
             IF WS-LYR = "L"
                PERFORM CLEAR-TRANSACTIONS
                CLOSE INCR-LY-REGISTER
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
             IF WS-LYR = "T"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient..."
                   AT POS
                MOVE 3051 TO POS-DOT
                CLOSE INCR-REGISTER
                PERFORM RDTR-000
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE INCR-REGISTER
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"1F"
             IF WS-LYR = "L"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient..."
                   AT POS
                MOVE 3051 TO POS-DOT
                CLOSE INCR-LY-REGISTER
                PERFORM RDTR-LY-000
                PERFORM PRINT-LY-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE INCR-LY-REGISTER
            PERFORM ERROR1-020
            PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
              MOVE "REGISTER RECORD USY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              CLOSE INCR-REGISTER
              GO TO RDTR-000.
       RDTR-005.
           IF WS-KEY = "R"
               MOVE WS-INVOICEDATE TO INCR-PULL-DATE
               START INCR-REGISTER KEY NOT < INCR-PULL-DATE
               GO TO RDTR-008.
                
           MOVE WS-INVOICE      TO INCR-INVOICE.
           IF WS-KEY = "D" OR = "C"
                MOVE 1          TO INCR-TRANS
                GO TO RDTR-007.
                
           IF WS-KEY = "I" OR = "P"
                MOVE "P"        TO INCR-PRINTED
                GO TO RDTR-007.
                
           IF WS-KEY = "S"
                MOVE "S"        TO INCR-PRINTED
                GO TO RDTR-007.
                
           IF WS-KEY = "N"
                MOVE "N"        TO INCR-PRINTED.
       RDTR-007.
           IF WS-KEY = "D" OR = "C"
             START INCR-REGISTER KEY NOT < INCR-KEY
                INVALID KEY NEXT SENTENCE
           ELSE
             START INCR-REGISTER KEY NOT < INCR-PRINTED
                INVALID KEY NEXT SENTENCE.
       RDTR-008.
           IF WS-INCR-ST1 NOT = 0
              MOVE "BAD START ON INCR-REGISTER, 'ESC' TO SEE ERC."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RDTR-999.
           
           MOVE "Y" TO WS-NEWINPUT.
           MOVE 1 TO SUB-1 F-INDEX.
       RDTR-010.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
               GO TO RDTR-999.
           IF WS-INCR-ST1 = 23
               MOVE "REGISTER ST1=23, 'ESC' TO EXIT." TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDTR-999.
           IF WS-NEWINPUT = "Y"
               PERFORM CLEAR-TRANSACTIONS
               MOVE " " TO F-EXIT-CH
               MOVE "N" TO WS-NEWINPUT
               MOVE 1 TO F-INDEX.
               
           IF WS-INCR-ST1 = 91
               MOVE 1 TO F-INDEX
               MOVE "N" TO WS-ANSWER              
               CLOSE INCR-REGISTER
               GO TO RDTR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "RECORD LOCKED BY ANOTHER USER, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RDTR-010.
           PERFORM ERROR1-020.
               
           IF WS-KEY = "R"
            IF INCR-TRANS NOT = 1
               GO TO RDTR-010.
           IF WS-KEY = "I" OR = "P" OR = "S"
            IF INCR-TRANS = 7 OR = 8
               GO TO RDTR-010.
           IF WS-KEY = "N"
            IF INCR-TRANS NOT = 4 AND NOT = 7
               GO TO RDTR-010.
           IF WS-KEY = "I" OR = "P" OR = "S" OR = "N"
            IF INCR-TRANS NOT = 4 AND NOT = 7
               MOVE 1 TO F-INDEX
               CLOSE INCR-REGISTER
               GO TO RDTR-999.
           IF WS-KEY = "D" OR = "C"
            IF INCR-TRANS NOT = 1
               MOVE 1 TO F-INDEX
               CLOSE INCR-REGISTER
               GO TO RDTR-999.
               
           IF WS-KEY = "R"
            IF INCR-PULL-DATE NOT = WS-INVOICEDATE
               MOVE 1 TO F-INDEX
               CLOSE INCR-REGISTER
               MOVE "INCR-DATE NOT =" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDTR-999.
           IF WS-KEY = "R"
            IF INCR-PULL-DATE = WS-INVOICEDATE
               GO TO RDTR-017.

           IF WS-KEY = "C"
            IF INCR-PRINTED = "Y"
             IF INCR-PULL-DATE > 0
               GO TO RDTR-017.

           PERFORM ERROR1-020.
           MOVE INCR-DELIVERY TO WS-DELVIA.

           IF WS-COLLECT = "A"
               GO TO RDTR-013.
               
           IF WS-COLLECT = "N"
             IF INCR-AREA = "C"
               GO TO RDTR-010.
           IF WS-COLLECT = "N"
             IF WS-DELVIA = "COLLECT AT COUNTER  "
               GO TO RDTR-010.
               
           IF WS-COLLECT = "C"
             IF INCR-AREA = "C"
               GO TO RDTR-013.
           IF WS-COLLECT = "C"
             IF WS-DELVIA = "COLLECT AT COUNTER  "
               GO TO RDTR-013.
               
           GO TO RDTR-010.
       RDTR-013.
           IF WS-KEY = "I"
            IF INCR-PRINTED = "P"
             IF INCR-PULL-DATE > 0
               GO TO RDTR-017.
           IF WS-KEY = "I"
            IF INCR-PRINTED NOT = "P"
               CLOSE INCR-REGISTER
               GO TO RDTR-999.
               
           IF WS-KEY = "D"
            IF INCR-PRINTED = "Y"
             IF INCR-PULL-DATE = 0
               GO TO RDTR-017.
           IF WS-KEY = "D"
            IF INCR-PRINTED NOT = "Y"
               CLOSE INCR-REGISTER
               GO TO RDTR-999.

           IF WS-KEY = "S"
            IF INCR-PRINTED = "S"
               GO TO RDTR-017
            ELSE
               CLOSE INCR-REGISTER
               GO TO RDTR-999.
               
           IF WS-KEY = "P"
            IF INCR-PRINTED = "P"
             IF INCR-PULL-DATE = 0 OR = " "
               GO TO RDTR-017.
           IF WS-KEY = "P"
            IF INCR-PRINTED NOT = "P"
      *         CLOSE INCR-REGISTER
           MOVE 2910 TO POS
           DISPLAY "Reading NEXT Transaction, Please be patient." AT POS
               GO TO RDTR-010.
      *         GO TO RDTR-999.

           IF WS-KEY = "N"
            IF INCR-PRINTED = "N"
               GO TO RDTR-017
            ELSE
               CLOSE INCR-REGISTER
               GO TO RDTR-999.

           MOVE 2910 TO POS
           DISPLAY "Reading NEXT Transaction, Please be patient." AT POS
           GO TO RDTR-010.
       RDTR-017.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           IF WS-ACCOUNT = 0
               GO TO RDTR-020.
           IF INCR-ACCOUNT NOT = WS-ACCOUNT
            IF WS-ACCOUNT NOT = 0
               GO TO RDTR-010.
       RDTR-020.
           IF F-INDEX > 15
             MOVE 2910 TO POS
             DISPLAY "Press 'PgDn' For More, Or" AT POS
             ADD 27 TO POS
             DISPLAY "'ESC' To Clear The Screen !" AT POS
             MOVE 3020 TO POS
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
           IF F-EXIT-CH = X"07"
               PERFORM CLEAR-TRANSACTIONS
               MOVE "Y" TO WS-ANSWER
               GO TO RDTR-999.
           IF F-EXIT-CH = X"1F"
               MOVE 3010 TO POS
               DISPLAY "Printing In Progress, Please Be Patient..."
                  AT POS
               MOVE 3051 TO POS-DOT
               CLOSE INCR-REGISTER
               PERFORM RDTR-000
               PERFORM PRINT-ROUTINE
               PERFORM CLEAR-TRANSACTIONS
               MOVE "Y" TO WS-ANSWER
               PERFORM ERROR1-020
               PERFORM ERROR-020
               GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C"
                    AND NOT = X"07" AND NOT = " " AND NOT = X"1F"
               MOVE 16 TO F-INDEX
               GO TO RDTR-020.
       RDTR-500.
           IF INCR-ACCOUNT NOT = DR-ACCOUNT-NUMBER
              PERFORM READ-DEBTOR.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTR-010.
       RDTR-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            CLOSE INCR-REGISTER.
            PERFORM RDTR-000.
            
            MOVE 0  TO PAGE-CNT
                       WS-PRINT-AMOUNT.
            MOVE 66 TO LINE-CNT.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            MOVE WS-INVOICE TO INCR-INVOICE.
            
           IF WS-KEY = "R"
               MOVE WS-INVOICEDATE TO INCR-PULL-DATE
               START INCR-REGISTER KEY NOT < INCR-PULL-DATE
               GO TO PRR-002.
            IF WS-KEY = "D" OR = "C"
               MOVE 1          TO INCR-TRANS
            START INCR-REGISTER KEY NOT < INCR-KEY
                 INVALID KEY NEXT SENTENCE
                 GO TO PRR-002.
           IF WS-KEY = "I" OR = "P"
               MOVE "P"        TO INCR-PRINTED
           START INCR-REGISTER KEY NOT < INCR-PRINTED
                INVALID KEY NEXT SENTENCE
                GO TO PRR-002.
                
           IF WS-KEY = "S"
               MOVE "S"        TO INCR-PRINTED
           START INCR-REGISTER KEY NOT < INCR-PRINTED
                INVALID KEY NEXT SENTENCE
                GO TO PRR-002.
                
           IF WS-KEY = "N"
               MOVE "N"        TO INCR-PRINTED
           START INCR-REGISTER KEY NOT < INCR-PRINTED
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
               
           IF WS-KEY = "R"
            IF INCR-TRANS NOT = 1
               GO TO PRR-002.
           IF WS-KEY = "I" OR = "P" OR = "S"
            IF INCR-TRANS = 7 OR = 8
               GO TO PRR-002.
           IF WS-KEY = "N"
            IF INCR-TRANS NOT = 4 AND NOT = 7
               GO TO PRR-002.
           IF WS-KEY = "I" OR = "P" OR = "S" OR = "N"
            IF INCR-TRANS NOT = 4 AND NOT = 7 
               MOVE 1 TO F-INDEX
               CLOSE INCR-REGISTER
               GO TO PRR-900.
           IF WS-KEY = "D" OR = "C"
            IF INCR-TRANS NOT = 1
               MOVE 1 TO F-INDEX
               CLOSE INCR-REGISTER
               GO TO PRR-900.
           IF WS-KEY = "R"
            IF INCR-PULL-DATE NOT = WS-INVOICEDATE
               MOVE 1 TO F-INDEX
               CLOSE INCR-REGISTER
               GO TO PRR-900.
               
           IF WS-KEY NOT = "R"
            IF INCR-INVOICE < WS-INVOICE
               GO TO PRR-002.
               
           IF POS-DOT > 3078
               PERFORM ERROR-020
               MOVE 3010 TO POS
               DISPLAY "Printing In Progress, Please Be Patient..."
               AT POS
               MOVE 3051 TO POS-DOT
          ELSE
              ADD 1 TO POS-DOT
              DISPLAY "." AT POS-DOT.
               
           IF WS-KEY = "R"
            IF INCR-PULL-DATE = WS-INVOICEDATE
               GO TO PRR-007.

           IF WS-KEY = "C"
            IF INCR-PRINTED = "Y"
             IF INCR-PULL-DATE  > 0
               GO TO PRR-007.

            IF WS-COLLECT = "A"
               GO TO PRR-005.

           IF WS-COLLECT = "N"
             IF WS-DELVIA = "COLLECT AT COUNTER  "
               GO TO PRR-002.
            IF WS-COLLECT = "N"
             IF INCR-AREA = "C"
               GO TO PRR-002.
            IF WS-COLLECT = "C"
             IF INCR-AREA NOT = "C"
               GO TO PRR-002.
       PRR-005.
           IF WS-KEY = "I"
            IF INCR-PRINTED = "P" 
             IF INCR-PULL-DATE > 0
               GO TO PRR-007.
           IF WS-KEY = "I"
            IF INCR-PRINTED NOT = "P"
               GO TO PRR-900.
               
           IF WS-KEY = "D"
            IF INCR-PRINTED = "Y"
             IF INCR-PULL-DATE = 0
               GO TO PRR-007.
           IF WS-KEY = "D"
            IF INCR-PRINTED NOT = "Y"
               GO TO PRR-900.
               
           IF WS-KEY = "S"
            IF INCR-PRINTED = "S"
               GO TO PRR-007
            ELSE
               GO TO PRR-900.
               
           IF WS-KEY = "P"
            IF INCR-PRINTED = "P"
             IF INCR-PULL-DATE = 0 OR = " "
               GO TO PRR-007.
           IF WS-KEY = "P"
            IF INCR-PRINTED NOT = "P"
               GO TO PRR-007.

           IF WS-KEY = "N"
            IF INCR-PRINTED = "N"
               GO TO PRR-007
            ELSE
               GO TO PRR-900.
               
           GO TO PRR-002.
        PRR-007.
           IF WS-ACCOUNT = 0
               GO TO PRR-010.
           IF INCR-ACCOUNT NOT = WS-ACCOUNT
               GO TO PRR-002.
        PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
        PRR-020.
           IF INCR-ACCOUNT NOT = DR-ACCOUNT-NUMBER
              PERFORM READ-DEBTOR.
           MOVE INCR-INVOICE      TO D-PSLIP
           MOVE INCR-ACCOUNT      TO D-ACCOUNT
           MOVE DR-NAME           TO D-NAME
           MOVE INCR-PORDER       TO D-PONO
           MOVE INCR-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO D-DATE
           MOVE INCR-INVCRED-AMT  TO D-AMOUNT.
           IF WS-KEY = "C"
            IF INCR-PRINTED = "Y"
             IF INCR-PULL-DATE > 0
               MOVE "**DELIVERED INV**        " TO D-STATUS.
               
           IF WS-KEY = "I"
            IF INCR-PRINTED = "P"
              IF INCR-PULL-DATE > 0
               MOVE "* PULLD/UN-INVOICED ORDER *" TO D-STATUS.
           IF WS-KEY = "I"
            IF INCR-PRINTED = "P"
              IF INCR-PULL-DATE = 0 
               MOVE "*UNPULLD/UN-INVOICED ORDER*" TO D-STATUS.

           IF WS-KEY = "D"
            IF INCR-PRINTED = "Y"
             IF INCR-PULL-DATE = 0
               MOVE "* UN-DELIVERED INVOICE    *" TO D-STATUS.

           IF WS-KEY = "S"
            IF INCR-PRINTED = "S"
               MOVE "* SUSPENDED ORDERS ONLY   *" TO D-STATUS.

           IF WS-KEY = "P"
            IF INCR-PRINTED = "P"
             IF INCR-PULL-DATE = 0
               MOVE "* UN-PULLED ORDERS ONLY   *" TO D-STATUS.

           IF WS-KEY = "R"
               MOVE "* INVOICE FLAGGED ON DATE *" TO D-STATUS.
               
           IF WS-KEY = "N"
            IF INCR-PRINTED = "N"
               MOVE "*B/O ONLY, NOTHING TO PULL*" TO D-STATUS.
           
           WRITE PRINT-REC FROM DETAIL-LINE.
           
           ADD INCR-INVCRED-AMT TO WS-PRINT-AMOUNT.
           
           MOVE " " TO PRINT-REC.
           ADD 1 TO LINE-CNT.
           GO TO PRR-002.
       PRR-060.
           ADD 1         TO PAGE-CNT.
           MOVE PAGE-CNT TO H1-PAGE.

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE " " TO PRINT-REC.

            IF WS-KEY = "C"
                MOVE "** DEL-INVOICES ** " TO H1-TYPE.
            IF WS-KEY = "I"
                MOVE "** UN-INVOICED **  " TO H1-TYPE.
            IF WS-KEY = "D"
                MOVE "** UN-DELIVERED ** " TO H1-TYPE.
            IF WS-KEY = "S"
                MOVE "** SUSPENDED **    " TO H1-TYPE.
            IF WS-KEY = "P"
                MOVE "** UN-PULLED **    " TO H1-TYPE.
            IF WS-KEY = "N"
                MOVE "** B/O'S ONLY **   " TO H1-TYPE.
            IF WS-KEY = "R"
                MOVE "*INVOICE FLAG DATE*" TO H1-TYPE.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE " " TO PRINT-REC
            MOVE 5 TO LINE-CNT.
       PRR-900.
            MOVE " " TO PRINT-REC DETAIL-LINE.
            MOVE "TOTAL OF AMOUNT PRINTED :" TO D-STATUS
            MOVE WS-PRINT-AMOUNT             TO D-AMOUNT
            WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
            
            IF WS-COLLECT = "A"
                MOVE "ALL TRANSACTIONS PRINTED" TO PRINT-REC
                WRITE PRINT-REC AFTER 1.
            IF WS-COLLECT = "C"
                MOVE "ONLY COLLECT TRANSACTIONS PRINTED"
                 TO PRINT-REC
                WRITE PRINT-REC AFTER 1.
            IF WS-COLLECT = "N"
                MOVE "NO COLLECT TRANSACTIONS PRINTED" TO PRINT-REC
                WRITE PRINT-REC AFTER 1.

            IF WS-KEY = "R"
                MOVE "INVOICES FLAGGED ON DATE :" TO F-COMMENT
                WRITE PRINT-REC FROM FLAG-LINE AFTER 1.
                
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PRR-999.
           EXIT.
      *
       READ-LY-TRANSACTIONS SECTION.
       RDTR-LY-000.
           OPEN I-O INCR-LY-REGISTER.
           IF WS-INCR-LY-ST1 NOT = 0
               MOVE "REG-LY BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-LY-ST1
              CLOSE INCR-LY-REGISTER
              GO TO RDTR-LY-000.
       RDTR-LY-005.
           IF WS-KEY = "R"
               MOVE WS-INVOICEDATE TO INCR-LY-PULL-DATE
               START INCR-LY-REGISTER KEY NOT < INCR-LY-PULL-DATE
                  GO TO RDTR-LY-008.
                  
           MOVE WS-INVOICE TO INCR-LY-INVOICE
           MOVE 1          TO INCR-LY-TRANS
           START INCR-LY-REGISTER KEY NOT < INCR-LY-KEY
                INVALID KEY NEXT SENTENCE.
       RDTR-LY-008.
           IF WS-INCR-LY-ST1 NOT = 0
               MOVE "REG-LY BUSY ON START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-LY-ST1
              GO TO RDTR-LY-999.
           
           MOVE "Y" TO WS-NEWINPUT.
           MOVE 1 TO SUB-1 F-INDEX.
       RDTR-LY-010.
           READ INCR-LY-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-NEWINPUT = "Y"
               PERFORM CLEAR-TRANSACTIONS
               MOVE " " TO F-EXIT-CH
               MOVE "N" TO WS-NEWINPUT
               MOVE 1 TO F-INDEX.
               
           IF WS-INCR-LY-ST1 = 91
               MOVE 1 TO F-INDEX
               MOVE "N" TO WS-ANSWER              
               CLOSE INCR-LY-REGISTER
               GO TO RDTR-LY-999.
           IF WS-INCR-LY-ST1 NOT = 0
               MOVE "RECORD LOCKED BY ANOTHER USER, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RDTR-LY-010.
           PERFORM ERROR1-020.
               
           IF WS-KEY = "D" OR = "C"
            IF INCR-LY-TRANS NOT = 1
               MOVE 1 TO F-INDEX
               CLOSE INCR-REGISTER
               GO TO RDTR-LY-999.
           IF WS-KEY = "R"
            IF INCR-LY-TRANS NOT = 1
               GO TO RDTR-LY-010.
           IF WS-KEY = "R"
            IF INCR-LY-PULL-DATE NOT = WS-INVOICEDATE
               MOVE 1 TO F-INDEX
               CLOSE INCR-LY-REGISTER
               GO TO RDTR-LY-999.
               
           IF WS-KEY NOT = "R"
            IF INCR-LY-INVOICE < WS-INVOICE
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next INVOICE..." AT POS
               GO TO RDTR-LY-010.
               
           IF WS-KEY = "R"
            IF INCR-LY-PULL-DATE = WS-INVOICEDATE
               GO TO RDTR-LY-017.

           IF WS-KEY = "C"
            IF INCR-LY-PRINTED = "Y"
             IF INCR-LY-PULL-DATE > 0
               GO TO RDTR-LY-017
             ELSE
               GO TO RDTR-LY-010.

           MOVE INCR-LY-DELIVERY TO WS-DELVIA.

           IF WS-COLLECT = "A"
               GO TO RDTR-LY-013.
               
           IF WS-COLLECT = "N"
             IF INCR-LY-AREA = "C"
               GO TO RDTR-LY-010.
           IF WS-COLLECT = "N"
             IF WS-DELVIA = "COLLECT"
               GO TO RDTR-LY-010.
               
           IF WS-COLLECT = "C"
             IF INCR-LY-AREA = "C"
               GO TO RDTR-LY-013.
           IF WS-COLLECT = "C"
             IF WS-DELVIA = "COLLECT"
               GO TO RDTR-LY-013.

           MOVE 2910 TO POS
           DISPLAY "Area Not the Same, Reading next trans......." AT POS
           GO TO RDTR-LY-010.
       RDTR-LY-013.
           IF WS-KEY = "D"
            IF INCR-LY-PRINTED = "Y"
             IF INCR-LY-PULL-DATE = 0
               GO TO RDTR-LY-017.
           IF WS-KEY = "D"
            IF INCR-LY-PRINTED NOT = "Y"
               CLOSE INCR-LY-REGISTER
               GO TO RDTR-LY-999.

           MOVE 2910 TO POS
           DISPLAY "Reading NEXT Transaction, Please be patient." AT POS
           GO TO RDTR-LY-010.
       RDTR-LY-017.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           IF WS-ACCOUNT = 0
               GO TO RDTR-LY-020.
           IF INCR-LY-ACCOUNT NOT = WS-ACCOUNT
            IF WS-ACCOUNT NOT = 0
               MOVE 2910 TO POS
               DISPLAY
                "Reading NEXT Trans as Acc Not =, be patient." AT POS
               GO TO RDTR-LY-010.
       RDTR-LY-020.
           IF F-INDEX > 15
             MOVE 2910 TO POS
             DISPLAY "Press 'PgDn' For More, Or" AT POS
             ADD 31 TO POS
             DISPLAY "'ESC' To Clear The Screen !" AT POS
             MOVE 3020 TO POS
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
           IF F-EXIT-CH = X"07"
               PERFORM CLEAR-TRANSACTIONS
               MOVE "Y" TO WS-ANSWER
               GO TO RDTR-LY-999.
           IF F-EXIT-CH = X"1F"
               MOVE 3010 TO POS
               DISPLAY "Printing In Progress, Please Be Patient..."
                  AT POS
               MOVE 3051 TO POS-DOT
               CLOSE INCR-LY-REGISTER
               PERFORM RDTR-LY-000
               PERFORM PRINT-LY-ROUTINE
               PERFORM CLEAR-TRANSACTIONS
               MOVE "Y" TO WS-ANSWER
               PERFORM ERROR1-020
               PERFORM ERROR-020
               GO TO RDTR-LY-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C"
                    AND NOT = X"07" AND NOT = " " AND NOT = X"1F"
               MOVE 16 TO F-INDEX
               GO TO RDTR-LY-020.
       RDTR-LY-500.
           IF INCR-LY-ACCOUNT NOT = DR-ACCOUNT-NUMBER
              PERFORM READ-LY-DEBTOR.
           PERFORM SCROLLING-LYR.
           ADD 1 TO F-INDEX.
           GO TO RDTR-LY-010.
       RDTR-LY-999.
           EXIT.
      *
       READ-DEBTOR SECTION.
       RD-000.
           MOVE INCR-ACCOUNT TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
       RD-010.
            READ DEBTOR-MASTER
                INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE WS-ACCOUNT TO DR-ACCOUNT-NUMBER
                MOVE "UNKNOWN" TO DR-NAME
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
       READ-LY-DEBTOR SECTION.
       RDLY-000.
           MOVE INCR-LY-ACCOUNT TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
       RDLY-010.
            READ DEBTOR-MASTER
                INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE WS-ACCOUNT TO DR-ACCOUNT-NUMBER
                MOVE "UNKNOWN" TO DR-NAME
                GO TO RDLY-999.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE "DEBTOR RECORD BUSY ON READ-RDLY010, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RDLY-010.
       RDLY-999.
            EXIT.
      *
       PRINT-LY-ROUTINE SECTION.
       PRR-LY-000.
            MOVE 0  TO PAGE-CNT
                       WS-PRINT-AMOUNT.
            MOVE 66 TO LINE-CNT.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            MOVE WS-INVOICE TO INCR-LY-INVOICE.

            IF WS-KEY = "D" OR = "C"
               MOVE 1          TO INCR-LY-TRANS
            START INCR-LY-REGISTER KEY NOT < INCR-LY-KEY
                 INVALID KEY NEXT SENTENCE
                 GO TO PRR-002.
            
           IF WS-KEY = "R"
               MOVE WS-INVOICEDATE TO INCR-LY-PULL-DATE
               START INCR-LY-REGISTER KEY NOT < INCR-LY-PULL-DATE.
       PRR-LY-002.
            READ INCR-LY-REGISTER NEXT
               AT END NEXT SENTENCE.
            IF WS-INCR-LY-ST1 = 10 OR = 91
               MOVE 0 TO WS-INCR-LY-ST1
               GO TO PRR-LY-900.
            IF WS-INCR-LY-ST1 NOT = 0
             MOVE "REG-LY BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-INCR-LY-ST1
               GO TO PRR-LY-002.
               
           IF WS-KEY = "R"
            IF INCR-LY-TRANS NOT = 1
               GO TO PRR-LY-002.

           IF WS-KEY = "D" OR = "C"
            IF INCR-LY-TRANS NOT = 1
               MOVE 1 TO F-INDEX
               CLOSE INCR-LY-REGISTER
               GO TO PRR-LY-900.

           IF WS-KEY = "R"
            IF INCR-LY-PULL-DATE NOT = WS-INVOICEDATE
               MOVE 1 TO F-INDEX
               CLOSE INCR-LY-REGISTER
               GO TO PRR-LY-900.
               
           IF WS-KEY NOT = "R"
            IF INCR-LY-INVOICE < WS-INVOICE
               GO TO PRR-LY-002.
               
           IF POS-DOT > 3078
               PERFORM ERROR-020
               MOVE 3010 TO POS
               DISPLAY "Printing In Progress, Please Be Patient..."
               AT POS
               MOVE 3051 TO POS-DOT
          ELSE
              ADD 1 TO POS-DOT
              DISPLAY "." AT POS-DOT.
               
           IF WS-KEY = "R"
            IF INCR-LY-PULL-DATE = WS-INVOICEDATE
               GO TO PRR-LY-007.

           IF WS-KEY = "C"
            IF INCR-LY-PRINTED = "Y"
             IF INCR-LY-PULL-DATE  > 0
               GO TO PRR-LY-007.

            IF WS-COLLECT = "A"
               GO TO PRR-LY-005.
            IF WS-COLLECT = "N"
             IF INCR-LY-AREA = "C"
               GO TO PRR-LY-002.
            IF WS-COLLECT = "C"
             IF INCR-LY-AREA NOT = "C"
               GO TO PRR-LY-002.
       PRR-LY-005.
           IF WS-KEY = "D"
            IF INCR-LY-PRINTED = "Y"
             IF INCR-LY-PULL-DATE = 0
               GO TO PRR-LY-007.
           IF WS-KEY = "D"
            IF INCR-LY-PRINTED NOT = "Y"
               GO TO PRR-LY-900.
               
           GO TO PRR-LY-002.
       PRR-LY-007.
           IF WS-ACCOUNT = 0
               GO TO PRR-LY-010.
           IF INCR-LY-ACCOUNT NOT = WS-ACCOUNT
               GO TO PRR-LY-002.
       PRR-LY-010.
            IF LINE-CNT < 61
               GO TO PRR-LY-020.
           PERFORM PRR-LY-060.
       PRR-LY-020.
           IF INCR-LY-ACCOUNT NOT = DR-ACCOUNT-NUMBER
              PERFORM READ-LY-DEBTOR.
           MOVE INCR-LY-INVOICE      TO D-PSLIP
           MOVE INCR-LY-ACCOUNT      TO D-ACCOUNT
           MOVE DR-NAME              TO D-NAME
           MOVE INCR-LY-PORDER       TO D-PONO
           MOVE INCR-LY-DATE         TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE         TO D-DATE
           MOVE INCR-LY-INVCRED-AMT  TO D-AMOUNT.

           IF WS-KEY = "C"
               MOVE "**DELIVERED INV**          " TO D-STATUS.
           IF WS-KEY = "D"
               MOVE "* UN-DELIVERED INVOICE    *" TO D-STATUS.
           IF WS-KEY = "R"
               MOVE "* INVOICE FLAGGED ON DATE *" TO D-STATUS.
               
           WRITE PRINT-REC FROM DETAIL-LINE.
           
           ADD INCR-LY-INVCRED-AMT TO WS-PRINT-AMOUNT.
           
           MOVE " " TO PRINT-REC.
           ADD 1 TO LINE-CNT.
           GO TO PRR-LY-002.
       PRR-LY-060.
           ADD 1         TO PAGE-CNT.
           MOVE PAGE-CNT TO H1-PAGE.

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC AFTER 1
               MOVE " " TO PRINT-REC.

            IF WS-KEY = "C"
                MOVE "*DEL-INVOICES LYR *" TO H1-TYPE.
            IF WS-KEY = "D"
                MOVE "*UN-DEL INVOICE LYR" TO H1-TYPE.
            IF WS-KEY = "R"
                MOVE "*INVOICE FLAG DATE*" TO H1-TYPE.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE " " TO PRINT-REC
            MOVE 5 TO LINE-CNT.
       PRR-LY-900.
            MOVE " " TO PRINT-REC DETAIL-LINE.
            MOVE "TOTAL OF AMOUNT PRINTED :" TO D-STATUS
            MOVE WS-PRINT-AMOUNT             TO D-AMOUNT
            WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
            
            IF WS-COLLECT = "A"
                MOVE "ALL TRANSACTIONS PRINTED" TO PRINT-REC
                WRITE PRINT-REC AFTER 1.
            IF WS-COLLECT = "C"
                MOVE "ONLY COLLECT TRANSACTIONS PRINTED"
                 TO PRINT-REC
                WRITE PRINT-REC AFTER 1.
            IF WS-COLLECT = "N"
                MOVE "NO COLLECT TRANSACTIONS PRINTED" TO PRINT-REC
                WRITE PRINT-REC AFTER 1.

            IF WS-KEY = "D"
                MOVE "UNDELIVERED INVOICES FROM LYR PRINTED."
                 TO PRINT-REC
                WRITE PRINT-REC AFTER 1.
            IF WS-KEY = "C"
                MOVE "DELIVERED INVOICES FROM LYR PRINTED."
                 TO PRINT-REC
                WRITE PRINT-REC AFTER 1.

            IF WS-KEY = "R"
                MOVE "INVOICES FLAGGED ON DATE :" TO F-COMMENT
                WRITE PRINT-REC FROM FLAG-LINE AFTER 1.
                
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PRR-LY-999.
           EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE "PSLIP"      TO F-FIELDNAME
            MOVE 5            TO F-CBFIELDNAME
            MOVE INCR-INVOICE TO F-EDNAMEFIELDNUM
            MOVE 6            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "ACCNO" TO F-FIELDNAME
            MOVE 5 TO F-CBFIELDNAME
            MOVE INCR-ACCOUNT TO F-NAMEFIELD
            MOVE 7 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PORDERNO"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE INCR-PORDER TO F-NAMEFIELD
            MOVE 18          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"       TO F-FIELDNAME
            MOVE 4            TO F-CBFIELDNAME
            MOVE INCR-DATE    TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE-DEL" TO F-FIELDNAME
            MOVE 8 TO F-CBFIELDNAME.
            IF INCR-PULL-DATE NOT > 960401
                MOVE " " TO F-NAMEFIELD
            ELSE
                MOVE INCR-PULL-DATE TO SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-TIME" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            IF INCR-PULL-TIME NOT > 0
                MOVE " " TO F-NAMEFIELD
            ELSE
                MOVE INCR-PULL-TIME TO ALPHA-RATE
                PERFORM TIME-CHECKING
                MOVE WS-DATE-CHECK TO F-NAMEFIELD.
            MOVE 8 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT"         TO F-FIELDNAME
            MOVE 6                TO F-CBFIELDNAME
            MOVE INCR-INVCRED-AMT TO F-EDNAMEFIELD99Mil
            MOVE 11               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.
       SCROLL-999.
             EXIT.
      *
       SCROLLING-LYR SECTION.
       SCROLL-LY-000.
            MOVE "PSLIP"         TO F-FIELDNAME
            MOVE 5               TO F-CBFIELDNAME
            MOVE INCR-LY-INVOICE TO F-EDNAMEFIELDNUM
            MOVE 6               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "ACCNO"         TO F-FIELDNAME
            MOVE 5               TO F-CBFIELDNAME
            MOVE INCR-LY-ACCOUNT TO F-NAMEFIELD
            MOVE 7               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PORDERNO"     TO F-FIELDNAME
            MOVE 8              TO F-CBFIELDNAME
            MOVE INCR-LY-PORDER TO F-NAMEFIELD
            MOVE 18             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"          TO F-FIELDNAME
            MOVE 4               TO F-CBFIELDNAME
            MOVE INCR-LY-DATE    TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE    TO F-NAMEFIELD
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE-DEL"        TO F-FIELDNAME
            MOVE 8                 TO F-CBFIELDNAME
            MOVE INCR-LY-PULL-DATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE      TO F-NAMEFIELD.
            MOVE 10                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-TIME"        TO F-FIELDNAME
            MOVE 8                 TO F-CBFIELDNAME
            MOVE INCR-LY-PULL-TIME TO ALPHA-RATE
            PERFORM TIME-CHECKING
            MOVE WS-DATE-CHECK     TO F-NAMEFIELD
            MOVE 8                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT"            TO F-FIELDNAME
            MOVE 6                   TO F-CBFIELDNAME
            MOVE INCR-LY-INVCRED-AMT TO F-EDNAMEFIELD99Mil
            MOVE 11                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.
       SCROLL-LY-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO SUB-1 F-INDEX.
       CLTR-010.
            IF SUB-1 > 15
                MOVE 1 TO SUB-1 F-INDEX
                GO TO CLTR-999.
            MOVE "PSLIP" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PORDERNO" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE " "        TO F-NAMEFIELD.
            MOVE 18         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            MOVE " "    TO F-NAMEFIELD.
            MOVE 10     TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE-DEL" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE " "        TO F-NAMEFIELD.
            MOVE 10         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-TIME" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 8 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 11       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO SUB-1 F-INDEX.
            GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTORS BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO OPEN-000.
       OPEN-001.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-006.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlPSlpIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-900.
            CLOSE DEBTOR-MASTER.
            EXIT PROGRAM.
      *       STOP RUN.
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
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
       Copy "PrintReportInfo".
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
